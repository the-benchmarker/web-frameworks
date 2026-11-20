use std::{borrow::Cow, env, fmt, io, net::SocketAddr};

use axum::{
    Router,
    body::Bytes,
    extract::{FromRequest, Multipart, Path, Request},
    http::{HeaderMap, StatusCode, Uri, header::CONTENT_TYPE},
    response::IntoResponse,
    routing::{get, post},
};
use hyper::server::conn::http1;
use hyper_util::{rt::TokioIo, service::TowerToHyperService};
use proa::axum::{Route, RouteContextLayer, router_from_spec};
use serde::{
    Deserialize, Deserializer, Serialize,
    de::{Error, IgnoredAny, MapAccess, SeqAccess, Visitor},
};
use sha2::{Digest, Sha256};
use socket2::{Domain, Protocol, Socket, Type};
use tokio::net::TcpListener;

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

const ITEM_COUNT: usize = 100;
const BAD_REQUEST: StatusCode = StatusCode::BAD_REQUEST;

fn reuseport_listener(addr: SocketAddr) -> io::Result<std::net::TcpListener> {
    let sock = Socket::new(Domain::IPV4, Type::STREAM, Some(Protocol::TCP))?;
    sock.set_reuse_address(true)?;
    #[cfg(unix)]
    sock.set_reuse_port(true)?;
    sock.set_nonblocking(true)?;
    sock.bind(&addr.into())?;
    sock.listen(4096)?;
    Ok(sock.into())
}

fn main() -> io::Result<()> {
    let port = env::var("PORT")
        .ok()
        .map(|port| port.parse::<u16>())
        .transpose()
        .map_err(|error| io::Error::new(io::ErrorKind::InvalidInput, error))?
        .unwrap_or(3000);
    let addr = SocketAddr::from(([0, 0, 0, 0], port));
    let workers = std::thread::available_parallelism().map_or(1, |n| n.get());

    let mut handles = Vec::with_capacity(workers);
    for _ in 0..workers {
        let std_listener = reuseport_listener(addr)?;
        handles.push(std::thread::spawn(move || -> io::Result<()> {
            let rt = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()?;
            rt.block_on(async move {
                let app = application();
                let listener = TcpListener::from_std(std_listener)?;
                loop {
                    let Ok((stream, _)) = listener.accept().await else {
                        continue;
                    };
                    let _ = stream.set_nodelay(true);
                    let service = TowerToHyperService::new(app.clone());
                    tokio::spawn(async move {
                        let _ = http1::Builder::new()
                            .serve_connection(TokioIo::new(stream), service)
                            .await;
                    });
                }
            })
        }));
    }

    for handle in handles {
        handle.join().expect("worker thread panicked")?;
    }
    Ok(())
}

fn application() -> Router {
    let (routes, metadata) = router_from_spec(vec![
        Route::endpoint("/", get(empty)),
        Route::endpoint("/user/:id", get(user)),
        Route::endpoint("/user", post(empty)),
        Route::endpoint("/serialization", get(serialization)),
        Route::endpoint("/deserialization", post(deserialization)),
        Route::endpoint("/upload", post(upload)),
    ]);
    routes.layer(RouteContextLayer::new(metadata))
}

async fn empty() -> StatusCode {
    StatusCode::OK
}

#[derive(Serialize)]
struct User<'a> {
    id: &'a str,
}

async fn user(Path(id): Path<String>) -> Result<impl IntoResponse, StatusCode> {
    if id.len() != 8 || !id.bytes().all(|byte| byte.is_ascii_digit()) {
        return Err(BAD_REQUEST);
    }
    let mut body = Vec::with_capacity(17);
    serde_json::to_writer(&mut body, &User { id: &id })
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    Ok(([(CONTENT_TYPE, "application/json")], body))
}

#[derive(Clone, Copy, Serialize)]
struct Seed<'a> {
    seed: &'a str,
}

async fn serialization(uri: Uri) -> Result<impl IntoResponse, StatusCode> {
    let query = uri.query().unwrap_or_default();
    let owned_seed;
    let seed = if query.len() == 13
        && query.starts_with("seed=")
        && query.as_bytes()[5..]
            .iter()
            .all(|byte| byte.is_ascii_lowercase())
    {
        &query[5..]
    } else {
        let mut params = form_urlencoded::parse(query.as_bytes());
        let (name, parsed) = params.next().ok_or(BAD_REQUEST)?;
        if name != "seed"
            || params.next().is_some()
            || parsed.len() != 8
            || !parsed.bytes().all(|byte| byte.is_ascii_lowercase())
        {
            return Err(BAD_REQUEST);
        }
        owned_seed = parsed;
        &owned_seed
    };

    let items = [Seed { seed }; ITEM_COUNT];
    // The validated ASCII seed needs no escapes. Reserve room for the brackets,
    // separators, and every object so the serializer never has to grow its buffer.
    let object_size = br#"{"seed":""}"#.len() + seed.len();
    let capacity = 2 + ITEM_COUNT * object_size + (ITEM_COUNT - 1);
    let mut body = Vec::with_capacity(capacity);
    serde_json::to_writer(&mut body, items.as_slice())
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    Ok(([(CONTENT_TYPE, "application/json")], body))
}

struct ItemId(u32);

impl<'de> Deserialize<'de> for ItemId {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct IdVisitor;

        impl Visitor<'_> for IdVisitor {
            type Value = ItemId;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str("an integral ID between 0 and 1000000")
            }

            fn visit_u64<E: Error>(self, value: u64) -> Result<Self::Value, E> {
                if value <= 1_000_000 {
                    Ok(ItemId(value as u32))
                } else {
                    Err(E::custom("invalid item ID"))
                }
            }

            fn visit_i64<E: Error>(self, value: i64) -> Result<Self::Value, E> {
                if (0..=1_000_000).contains(&value) {
                    Ok(ItemId(value as u32))
                } else {
                    Err(E::custom("invalid item ID"))
                }
            }

            fn visit_f64<E: Error>(self, value: f64) -> Result<Self::Value, E> {
                if (0.0..=1_000_000.0).contains(&value) && value.fract() == 0.0 {
                    Ok(ItemId(value as u32))
                } else {
                    Err(E::custom("invalid item ID"))
                }
            }
        }

        deserializer.deserialize_any(IdVisitor)
    }
}

struct ItemObject(u32);

impl<'de> Deserialize<'de> for ItemObject {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct ItemVisitor;

        impl<'de> Visitor<'de> for ItemVisitor {
            type Value = ItemObject;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str("a JSON object with an integral id field")
            }

            fn visit_map<M: MapAccess<'de>>(self, mut map: M) -> Result<Self::Value, M::Error> {
                let mut id = None;
                while let Some(key) = map.next_key::<Cow<'de, str>>()? {
                    if key == "id" {
                        if id.is_some() {
                            return Err(M::Error::duplicate_field("id"));
                        }
                        id = Some(map.next_value::<ItemId>()?.0);
                    } else {
                        let _ = map.next_value::<IgnoredAny>()?;
                    }
                }
                id.map(ItemObject)
                    .ok_or_else(|| M::Error::missing_field("id"))
            }
        }

        deserializer.deserialize_map(ItemVisitor)
    }
}

#[derive(Serialize)]
struct Summary {
    count: usize,
    checksum: u32,
}

struct InputObject(Summary);

impl<'de> Deserialize<'de> for InputObject {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct RootVisitor;

        impl<'de> Visitor<'de> for RootVisitor {
            type Value = InputObject;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str("a JSON object with an items array")
            }

            fn visit_map<M: MapAccess<'de>>(self, mut map: M) -> Result<Self::Value, M::Error> {
                struct ItemsSeed;

                impl<'de> serde::de::DeserializeSeed<'de> for ItemsSeed {
                    type Value = Summary;

                    fn deserialize<D: Deserializer<'de>>(
                        self,
                        deserializer: D,
                    ) -> Result<Self::Value, D::Error> {
                        struct ItemsVisitor;

                        impl<'de> Visitor<'de> for ItemsVisitor {
                            type Value = Summary;

                            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                                formatter.write_str("100 objects with integral IDs")
                            }

                            fn visit_seq<S: SeqAccess<'de>>(
                                self,
                                mut items: S,
                            ) -> Result<Summary, S::Error> {
                                let mut checksum = 0u32;
                                for count in 0..ITEM_COUNT {
                                    let ItemObject(id) = items
                                        .next_element()?
                                        .ok_or_else(|| S::Error::invalid_length(count, &self))?;
                                    checksum += id;
                                }
                                if items.next_element::<IgnoredAny>()?.is_some() {
                                    return Err(S::Error::invalid_length(ITEM_COUNT + 1, &self));
                                }
                                Ok(Summary {
                                    count: ITEM_COUNT,
                                    checksum,
                                })
                            }
                        }

                        deserializer.deserialize_seq(ItemsVisitor)
                    }
                }

                let mut summary = None;
                while let Some(key) = map.next_key::<Cow<'de, str>>()? {
                    if key == "items" {
                        if summary.is_some() {
                            return Err(M::Error::duplicate_field("items"));
                        }
                        summary = Some(map.next_value_seed(ItemsSeed)?);
                    } else {
                        let _ = map.next_value::<IgnoredAny>()?;
                    }
                }
                summary
                    .map(InputObject)
                    .ok_or_else(|| M::Error::missing_field("items"))
            }
        }

        deserializer.deserialize_map(RootVisitor)
    }
}

fn is_json_content_type(headers: &HeaderMap) -> bool {
    let Some(raw) = headers.get(CONTENT_TYPE).and_then(|v| v.to_str().ok()) else {
        return false;
    };
    if raw.eq_ignore_ascii_case("application/json") {
        return true;
    }
    if let Some((essence, params)) = raw.split_once(';')
        && essence.trim().eq_ignore_ascii_case("application/json")
        && params.trim().eq_ignore_ascii_case("charset=utf-8")
    {
        return true;
    }
    raw.parse::<mime::Mime>().is_ok_and(|content_type| {
        content_type.essence_str() == "application/json"
            || content_type.suffix() == Some(mime::JSON)
            || matches!(
                content_type.essence_str(),
                "text/x-json" | "application/jsonrequest"
            )
    })
}

async fn deserialization(request: Request) -> Result<impl IntoResponse, StatusCode> {
    if !is_json_content_type(request.headers()) {
        return Err(StatusCode::UNSUPPORTED_MEDIA_TYPE);
    }
    if request.uri().query().is_some_and(|query| !query.is_empty()) {
        return Err(BAD_REQUEST);
    }
    let body = Bytes::from_request(request, &())
        .await
        .map_err(|error| error.status())?;
    let InputObject(summary) = serde_json::from_slice(&body).map_err(|_| BAD_REQUEST)?;
    let mut out = Vec::with_capacity(32);
    serde_json::to_writer(&mut out, &summary).map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    Ok(([(CONTENT_TYPE, "application/json")], out))
}

#[derive(Serialize)]
struct Upload<'a> {
    first_byte: Option<u8>,
    last_byte: Option<u8>,
    checksum: &'a str,
}

async fn upload(request: Request) -> Result<impl IntoResponse, StatusCode> {
    let is_multipart = request
        .headers()
        .get(CONTENT_TYPE)
        .and_then(|value| value.to_str().ok())
        .and_then(|value| value.split(';').next())
        .is_some_and(|essence| essence.trim().eq_ignore_ascii_case("multipart/form-data"));
    if !is_multipart {
        return Err(StatusCode::UNSUPPORTED_MEDIA_TYPE);
    }

    let mut multipart = Multipart::from_request(request, &())
        .await
        .map_err(|_| BAD_REQUEST)?;
    let mut file = None;
    while let Some(mut part) = multipart
        .next_field()
        .await
        .map_err(|error| error.status())?
    {
        if part.name() != Some("file") || part.file_name() == Some("") {
            continue;
        }
        if part.file_name().is_none() || file.is_some() {
            return Err(BAD_REQUEST);
        }

        let mut first_byte = None;
        let mut last_byte = None;
        let mut digest = Sha256::new();
        while let Some(chunk) = part.chunk().await.map_err(|error| error.status())? {
            if let Some(&first) = chunk.first() {
                first_byte.get_or_insert(first);
                last_byte = chunk.last().copied();
            }
            digest.update(chunk);
        }
        file = Some((first_byte, last_byte, digest.finalize()));
    }

    let (first_byte, last_byte, digest) = file.ok_or(BAD_REQUEST)?;
    let mut digest_hex = [0; 64];
    let checksum = base16ct::lower::encode_str(&digest, &mut digest_hex)
        .expect("a SHA-256 digest always encodes to 64 hexadecimal bytes");
    let mut out = Vec::with_capacity(128);
    serde_json::to_writer(
        &mut out,
        &Upload {
            first_byte,
            last_byte,
            checksum,
        },
    )
    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    Ok(([(CONTENT_TYPE, "application/json")], out))
}

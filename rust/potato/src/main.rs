// The Benchmarker v2 六路由：/ 与 /user 系列、/serialization、
// /deserialization、/upload，纯 HTTP，监听 3000

use potato::{HttpRequest, HttpResponse};
use sha2::{Digest, Sha256};

fn port() -> u16 {
    std::env::var("PORT")
        .ok()
        .and_then(|p| p.parse().ok())
        .unwrap_or(3000)
}

#[potato::http_get("/")]
fn index() -> HttpResponse {
    HttpResponse::empty()
}

#[potato::http_post("/user")]
fn create_user() -> HttpResponse {
    HttpResponse::empty()
}

#[potato::http_get("/user/{id}")]
fn get_user(id: String) -> HttpResponse {
    if id.len() == 8 && id.bytes().all(|b| b.is_ascii_digit()) {
        HttpResponse::json(format!(r#"{{"id":"{id}"}}"#))
    } else {
        HttpResponse::bad_request("invalid user id")
    }
}

fn decode_query_component(raw: &str) -> String {
    let bytes = raw.as_bytes();
    let mut out = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            b'+' => {
                out.push(b' ');
                i += 1;
            }
            b'%' if i + 2 < bytes.len() => {
                let hi = (bytes[i + 1] as char).to_digit(16);
                let lo = (bytes[i + 2] as char).to_digit(16);
                if let (Some(hi), Some(lo)) = (hi, lo) {
                    out.push((hi << 4 | lo) as u8);
                    i += 3;
                } else {
                    out.push(b'%');
                    i += 1;
                }
            }
            b => {
                out.push(b);
                i += 1;
            }
        }
    }
    String::from_utf8_lossy(&out).into_owned()
}

fn is_valid_seed(s: &str) -> bool {
    s.len() == 8 && s.bytes().all(|b| b.is_ascii_lowercase())
}

#[potato::http_get("/serialization")]
fn serialization(req: &mut HttpRequest) -> HttpResponse {
    // 重复 seed 参数检测须基于原始查询串（url_query 会折叠同名参数）
    let Some(raw) = req.raw_query() else {
        return HttpResponse::bad_request("missing query");
    };
    let mut seed: Option<String> = None;
    for pair in raw.split('&') {
        let (name, value) = pair.split_once('=').unwrap_or((pair, ""));
        if decode_query_component(name) == "seed" {
            if seed.is_some() {
                return HttpResponse::bad_request("repeated seed");
            }
            seed = Some(decode_query_component(value));
        } else {
            return HttpResponse::bad_request("unexpected query parameter");
        }
    }
    let Some(seed) = seed.filter(|s| is_valid_seed(s)) else {
        return HttpResponse::bad_request("invalid seed");
    };
    let mut body = String::with_capacity(18 * 100 + 1);
    body.push('[');
    for _ in 0..100 {
        body.push_str(r#"{"seed":""#);
        body.push_str(&seed);
        body.push_str(r#""},"#);
    }
    body.pop();
    body.push(']');
    HttpResponse::json(body)
}

fn is_json_media_type(content_type: &str) -> bool {
    let media = content_type.split(';').next().unwrap_or("").trim();
    let media = media.to_ascii_lowercase();
    media == "application/json"
        || media == "text/x-json"
        || media == "application/jsonrequest"
        || (media.starts_with("application/") && media.ends_with("+json"))
}

fn integral_id(n: &serde_json::Number) -> Option<u64> {
    let v = if let Some(v) = n.as_u64() {
        v
    } else if let Some(v) = n.as_i64() {
        if v < 0 {
            return None;
        }
        v as u64
    } else {
        let v = n.as_f64()?;
        if !(0.0..=1_000_000.0).contains(&v) || v.fract() != 0.0 {
            return None;
        }
        v as u64
    };
    (v <= 1_000_000).then_some(v)
}

#[potato::http_post("/deserialization")]
fn deserialization(req: &mut HttpRequest) -> HttpResponse {
    let content_type_ok = req
        .get_header("Content-Type")
        .map(is_json_media_type)
        .unwrap_or(false);
    if !content_type_ok {
        let mut res = HttpResponse::error("unsupported media type");
        res.http_code = 415;
        return res;
    }
    if req.raw_query().is_some_and(|q| !q.is_empty()) {
        return HttpResponse::bad_request("unexpected query parameter");
    }
    let parsed: serde_json::Value = match serde_json::from_slice(&req.body[..]) {
        Ok(v) => v,
        Err(_) => return HttpResponse::bad_request("malformed JSON"),
    };
    let Some(items) = parsed.get("items").and_then(|v| v.as_array()) else {
        return HttpResponse::bad_request("missing items array");
    };
    if items.len() != 100 {
        return HttpResponse::bad_request("items length must be 100");
    }
    let mut checksum: u64 = 0;
    for item in items {
        let Some(id) = item
            .get("id")
            .and_then(|v| v.as_number())
            .and_then(integral_id)
        else {
            return HttpResponse::bad_request("invalid item id");
        };
        checksum += id;
    }
    HttpResponse::json(format!(
        r#"{{"count":{},"checksum":{checksum}}}"#,
        items.len()
    ))
}

fn strip_quotes(v: &str) -> &str {
    v.strip_prefix('"')
        .and_then(|rest| rest.strip_suffix('"'))
        .unwrap_or(v)
}

#[potato::http_post("/upload")]
fn upload(req: &mut HttpRequest) -> HttpResponse {
    let unsupported = || {
        let mut res = HttpResponse::error("unsupported media type");
        res.http_code = 415;
        res
    };
    let Some(content_type) = req.get_header("Content-Type") else {
        return unsupported();
    };
    let media = content_type.split(';').next().unwrap_or("").trim();
    if !media.eq_ignore_ascii_case("multipart/form-data") {
        return unsupported();
    }
    let mut boundary = None;
    for param in content_type.split(';').skip(1) {
        if let Some((k, v)) = param.split_once('=') {
            if k.trim().eq_ignore_ascii_case("boundary") {
                let v = strip_quotes(v.trim());
                if v.is_empty() {
                    return HttpResponse::bad_request("empty boundary");
                }
                boundary = Some(v);
            }
        }
    }
    if boundary.is_none() {
        return HttpResponse::bad_request("missing boundary");
    }
    if req.body_malformed_multipart {
        return HttpResponse::bad_request("malformed multipart body");
    }
    let unselected = req.body_unselected_files.iter().any(|n| &n[..] == "file");
    let dup = req.body_dup_files.iter().any(|n| &n[..] == "file");
    let text_field = req.body_pairs.keys().any(|k| &k[..] == "file") && !unselected;
    if dup || text_field {
        return HttpResponse::bad_request("repeated file part");
    }
    let Some(file) = req.body_files.iter().find(|(k, _)| &k[..] == "file") else {
        return HttpResponse::bad_request("missing file part");
    };
    let bytes = &file.1.data[..];
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    let mut checksum = String::with_capacity(64);
    for b in hasher.finalize() {
        checksum.push_str(&format!("{b:02x}"));
    }
    let (first, last) = match (bytes.first(), bytes.last()) {
        (Some(f), Some(l)) => (f.to_string(), l.to_string()),
        _ => ("null".to_string(), "null".to_string()),
    };
    HttpResponse::json(format!(
        r#"{{"first_byte":{first},"last_byte":{last},"checksum":"{checksum}"}}"#
    ))
}

#[tokio::main]
async fn main() {
    potato::ServerConfig::set_ultraspeed(true);
    let mut server = potato::HttpServer::new(format!("0.0.0.0:{}", port()));
    server
        .configure(|ctx| {
            ctx.use_handlers();
            ctx.set_disable_compress(true);
        })
        .expect("configure server");
    _ = server.serve_http().await;
}

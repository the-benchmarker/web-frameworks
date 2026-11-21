mod r#const;

use hyperlane::*;
use sha2::{Digest, Sha256};

use r#const::*;

/// Builds the server configuration.
///
/// `TCP_NODELAY` is enabled: the responses are small and are written one
/// after the other, so the Nagle algorithm would delay the second write for
/// no gain.
///
/// # Returns
///
/// - `ServerConfig` - The configuration of the server.
fn init_server_config() -> ServerConfig {
    let mut server_config: ServerConfig = ServerConfig::default();
    server_config.set_address(Server::format_bind_address(DEFAULT_HOST, 3000));
    server_config.set_nodelay(Some(true));
    server_config
}

/// Builds the request configuration.
///
/// The body limit is raised because the load payloads are larger than the
/// default: a multipart upload carries a 256 KiB file plus its framing.
///
/// # Returns
///
/// - `RequestConfig` - The configuration of the request parser.
fn init_request_config() -> RequestConfig {
    let mut request_config: RequestConfig = RequestConfig::low_security();
    request_config.set_max_body_size(16 * 1024 * 1024);
    request_config
}

/// Writes a JSON answer with the given status code.
///
/// # Arguments
///
/// - `&mut Context` - The context of the request.
/// - `usize` - The HTTP status code of the answer.
/// - `&str` - The serialized JSON body.
fn respond_json(ctx: &mut Context, status_code: usize, body: &str) {
    ctx.response.set_status_code(status_code);
    ctx.response.set_header("content-type", JSON_CONTENT_TYPE);
    ctx.response.set_body(body.as_bytes());
}

/// Writes an answer that carries no body.
///
/// The empty responses of the contract have a zero-length body, so nothing
/// is written into the response body at all.
///
/// # Arguments
///
/// - `&mut Context` - The context of the request.
/// - `usize` - The HTTP status code of the answer.
fn respond_empty(ctx: &mut Context, status_code: usize) {
    ctx.response.set_status_code(status_code);
    ctx.response.set_body("");
}

/// Decodes the percent-encoded octets of a path or query segment.
///
/// The contract requires the path ID to be decoded before it is validated,
/// and the query names to be compared after they are decoded, so both are
/// decoded here.
///
/// # Arguments
///
/// - `&str` - The raw segment as it arrived on the wire.
///
/// # Returns
///
/// - `Option<String>` - The decoded segment, or `None` when the segment holds
///   an invalid escape or a non-UTF-8 sequence.
fn percent_decode(input: &str) -> Option<String> {
    if !input.contains('%') {
        return Some(input.to_owned());
    }
    let bytes: &[u8] = input.as_bytes();
    let mut decoded: Vec<u8> = Vec::with_capacity(bytes.len());
    let mut index: usize = 0;
    while index < bytes.len() {
        if bytes[index] == b'%' {
            let high: u8 = *bytes.get(index + 1)?;
            let low: u8 = *bytes.get(index + 2)?;
            let high_value: u8 = (high as char).to_digit(16)? as u8;
            let low_value: u8 = (low as char).to_digit(16)? as u8;
            decoded.push((high_value << 4) | low_value);
            index += 3;
        } else {
            decoded.push(bytes[index]);
            index += 1;
        }
    }
    String::from_utf8(decoded).ok()
}

/// Checks whether a value is exactly eight ASCII digits.
///
/// The contract requires eight characters of `0` through `9`, and it accepts
/// `00000000`, so the value is never parsed as a number.
///
/// # Arguments
///
/// - `&str` - The value to check.
///
/// # Returns
///
/// - `bool` - Whether the value is a valid ID.
fn is_valid_id(id: &str) -> bool {
    id.len() == 8 && id.bytes().all(|byte: u8| byte.is_ascii_digit())
}

/// The route that answers the index path.
#[route("/")]
struct Index;

/// Implements [`ServerHook`] for [`Index`].
impl ServerHook for Index {
    /// Builds the route.
    ///
    /// # Arguments
    ///
    /// - `&mut Stream` - The stream the request arrived on.
    /// - `&mut Context` - The context of the request.
    ///
    /// # Returns
    ///
    /// - `Self` - The route.
    async fn new(_: &mut Stream, _: &mut Context) -> Self {
        Self
    }

    /// Answers the request.
    ///
    /// The response is only filled here; the bytes leave the process in the
    /// response middleware, so every route pays for one send and not one
    /// send per handler.
    ///
    /// # Arguments
    ///
    /// - `Self` - The route.
    /// - `&mut Stream` - The stream the request arrived on.
    /// - `&mut Context` - The context of the request.
    ///
    /// # Returns
    ///
    /// - `Status` - Whether the pipeline continues.
    async fn handle(self, _: &mut Stream, ctx: &mut Context) -> Status {
        if ctx.request.method.is_get() {
            respond_empty(ctx, 200);
        } else {
            respond_empty(ctx, 404);
        }
        Status::Continue
    }
}

/// The route that answers the user path.
#[route("/user")]
struct User;

/// Implements [`ServerHook`] for [`User`].
impl ServerHook for User {
    /// Builds the route.
    ///
    /// # Arguments
    ///
    /// - `&mut Stream` - The stream the request arrived on.
    /// - `&mut Context` - The context of the request.
    ///
    /// # Returns
    ///
    /// - `Self` - The route.
    async fn new(_: &mut Stream, _: &mut Context) -> Self {
        Self
    }

    /// Answers the request.
    ///
    /// # Arguments
    ///
    /// - `Self` - The route.
    /// - `&mut Stream` - The stream the request arrived on.
    /// - `&mut Context` - The context of the request.
    ///
    /// # Returns
    ///
    /// - `Status` - Whether the pipeline continues.
    async fn handle(self, _: &mut Stream, ctx: &mut Context) -> Status {
        if ctx.request.method.is_post() {
            respond_empty(ctx, 200);
        } else {
            respond_empty(ctx, 404);
        }
        Status::Continue
    }
}

/// The route that answers the user path with a parameter.
#[route("/user/{id}")]
struct UserId;

/// Implements [`ServerHook`] for [`UserId`].
impl ServerHook for UserId {
    /// Builds the route.
    ///
    /// # Arguments
    ///
    /// - `&mut Stream` - The stream the request arrived on.
    /// - `&mut Context` - The context of the request.
    ///
    /// # Returns
    ///
    /// - `Self` - The route.
    async fn new(_: &mut Stream, _: &mut Context) -> Self {
        Self
    }

    /// Answers the request.
    ///
    /// The ID comes from the path parameter and is validated after it is
    /// decoded, so a percent-encoded ID is accepted and an ID that is not
    /// eight digits is rejected. The ID stays a string, so the leading zeros
    /// survive into the answer.
    ///
    /// # Arguments
    ///
    /// - `Self` - The route.
    /// - `&mut Stream` - The stream the request arrived on.
    /// - `&mut Context` - The context of the request.
    ///
    /// # Returns
    ///
    /// - `Status` - Whether the pipeline continues.
    async fn handle(self, _: &mut Stream, ctx: &mut Context) -> Status {
        if !ctx.request.method.is_get() {
            respond_empty(ctx, 404);
            return Status::Continue;
        }
        let Some(raw_id) = ctx.route_params.get("id") else {
            respond_empty(ctx, 404);
            return Status::Continue;
        };
        let Some(id) = percent_decode(raw_id) else {
            respond_empty(ctx, 400);
            return Status::Continue;
        };
        if !is_valid_id(&id) {
            respond_empty(ctx, 400);
            return Status::Continue;
        }
        let body: String = format!("{{\"id\":\"{id}\"}}");
        respond_json(ctx, 200, &body);
        Status::Continue
    }
}

/// The route that answers the serialization path.
#[route("/serialization")]
struct Serialization;

/// Implements [`ServerHook`] for [`Serialization`].
impl ServerHook for Serialization {
    /// Builds the route.
    ///
    /// # Arguments
    ///
    /// - `&mut Stream` - The stream the request arrived on.
    /// - `&mut Context` - The context of the request.
    ///
    /// # Returns
    ///
    /// - `Self` - The route.
    async fn new(_: &mut Stream, _: &mut Context) -> Self {
        Self
    }

    /// Answers the request.
    ///
    /// The query must carry exactly one parameter named `seed` whose value is
    /// eight lowercase ASCII letters. The raw target is read instead of the
    /// parsed query map, because a repeated name has to be rejected before
    /// the framework collapses the values, and because the name itself has to
    /// be compared after it is decoded.
    ///
    /// # Arguments
    ///
    /// - `Self` - The route.
    /// - `&mut Stream` - The stream the request arrived on.
    /// - `&mut Context` - The context of the request.
    ///
    /// # Returns
    ///
    /// - `Status` - Whether the pipeline continues.
    async fn handle(self, _: &mut Stream, ctx: &mut Context) -> Status {
        if !ctx.request.method.is_get() {
            respond_empty(ctx, 404);
            return Status::Continue;
        }
        let Some(target) = ctx.get_request_target() else {
            respond_empty(ctx, 400);
            return Status::Continue;
        };
        let Some((_, query)) = target.split_once('?') else {
            respond_empty(ctx, 400);
            return Status::Continue;
        };
        let mut seed: Option<String> = None;
        for pair in query.split('&') {
            let Some((name, value)) = pair.split_once('=') else {
                respond_empty(ctx, 400);
                return Status::Continue;
            };
            let Some(decoded_name) = percent_decode(name) else {
                respond_empty(ctx, 400);
                return Status::Continue;
            };
            if decoded_name != "seed" {
                respond_empty(ctx, 400);
                return Status::Continue;
            }
            if seed.is_some() {
                respond_empty(ctx, 400);
                return Status::Continue;
            }
            let Some(decoded_value) = percent_decode(value) else {
                respond_empty(ctx, 400);
                return Status::Continue;
            };
            seed = Some(decoded_value);
        }
        let Some(seed) = seed else {
            respond_empty(ctx, 400);
            return Status::Continue;
        };
        if seed.len() != 8 || !seed.bytes().all(|byte: u8| byte.is_ascii_lowercase()) {
            respond_empty(ctx, 400);
            return Status::Continue;
        }
        let mut body: String = String::with_capacity(100 * 20);
        body.push('[');
        for index in 0..100 {
            if index > 0 {
                body.push(',');
            }
            body.push_str("{\"seed\":\"");
            body.push_str(&seed);
            body.push_str("\"}");
        }
        body.push(']');
        respond_json(ctx, 200, &body);
        Status::Continue
    }
}

/// Checks whether the request carries a query string.
///
/// The deserialization route accepts no query parameters, and the raw target
/// is read so that a query is seen even when it carries no value.
///
/// # Arguments
///
/// - `&Context` - The context of the request.
///
/// # Returns
///
/// - `bool` - Whether the request carries a query string.
fn has_query(ctx: &Context) -> bool {
    ctx.get_request_target()
        .is_some_and(|target: String| target.contains('?'))
}

/// Checks whether a media type names a JSON body.
///
/// The contract accepts the JSON media types, the media types that use the
/// `+json` structured syntax suffix, and the two legacy aliases.
///
/// # Arguments
///
/// - `&str` - The media type of the request.
///
/// # Returns
///
/// - `bool` - Whether the media type names a JSON body.
fn is_json_media_type(content_type: &str) -> bool {
    let media_type: String = content_type
        .split(';')
        .next()
        .unwrap_or(content_type)
        .trim()
        .to_ascii_lowercase();
    media_type == JSON_MEDIA_TYPE
        || media_type == JSON_LEGACY_MEDIA_TYPE
        || media_type == JSON_REQUEST_MEDIA_TYPE
        || media_type.ends_with("+json")
}

/// The route that answers the deserialization path.
#[route("/deserialization")]
struct Deserialization;

/// Implements [`ServerHook`] for [`Deserialization`].
impl ServerHook for Deserialization {
    /// Builds the route.
    ///
    /// # Arguments
    ///
    /// - `&mut Stream` - The stream the request arrived on.
    /// - `&mut Context` - The context of the request.
    ///
    /// # Returns
    ///
    /// - `Self` - The route.
    async fn new(_: &mut Stream, _: &mut Context) -> Self {
        Self
    }

    /// Answers the request.
    ///
    /// The media type is checked before the body is read, so a request whose
    /// media type is unsupported is rejected with `415` even when its body is
    /// malformed.
    ///
    /// # Arguments
    ///
    /// - `Self` - The route.
    /// - `&mut Stream` - The stream the request arrived on.
    /// - `&mut Context` - The context of the request.
    ///
    /// # Returns
    ///
    /// - `Status` - Whether the pipeline continues.
    async fn handle(self, _: &mut Stream, ctx: &mut Context) -> Status {
        if !ctx.request.method.is_post() {
            respond_empty(ctx, 404);
            return Status::Continue;
        }
        if has_query(ctx) {
            respond_empty(ctx, 400);
            return Status::Continue;
        }
        let Some(content_type) = ctx.request.try_get_content_type() else {
            respond_empty(ctx, 415);
            return Status::Continue;
        };
        if !is_json_media_type(&content_type) {
            respond_empty(ctx, 415);
            return Status::Continue;
        }
        let body: &[u8] = &ctx.request.body;
        let Ok(parsed) = serde_json::from_slice::<serde_json::Value>(body) else {
            respond_empty(ctx, 400);
            return Status::Continue;
        };
        let Some(items) = parsed
            .get("items")
            .and_then(|value: &serde_json::Value| value.as_array())
        else {
            respond_empty(ctx, 400);
            return Status::Continue;
        };
        if items.len() != 100 {
            respond_empty(ctx, 400);
            return Status::Continue;
        }
        let mut checksum: u64 = 0;
        for item in items {
            let Some(id) = item.get("id") else {
                respond_empty(ctx, 400);
                return Status::Continue;
            };
            let Some(id) = id.as_f64() else {
                respond_empty(ctx, 400);
                return Status::Continue;
            };
            if !id.is_finite() || id.fract() != 0.0 {
                respond_empty(ctx, 400);
                return Status::Continue;
            }
            if !(0.0..=1_000_000.0).contains(&id) {
                respond_empty(ctx, 400);
                return Status::Continue;
            }
            checksum += id as u64;
        }
        let body: String = format!("{{\"count\":100,\"checksum\":{checksum}}}");
        respond_json(ctx, 200, &body);
        Status::Continue
    }
}

/// Checks whether a part declares a file name at all.
///
/// The part carries its raw `Content-Disposition` header, and that header is
/// the only place where an unselected file input (`filename=""`) differs from
/// a plain text field, because the parsed file name of both is empty.
///
/// # Arguments
///
/// - `&BodyPart` - The part to inspect.
///
/// # Returns
///
/// - `bool` - Whether the part declares a `filename` parameter.
fn declares_file_name(part: &BodyPart) -> bool {
    let Some(disposition) = part.try_get_header("content-disposition") else {
        return false;
    };
    for parameter in disposition.split(';').skip(1) {
        let Some((key, _)) = parameter.trim().split_once('=') else {
            continue;
        };
        if key.trim().eq_ignore_ascii_case("filename") {
            return true;
        }
    }
    false
}

/// The route that answers the upload path.
#[route("/upload")]
struct Upload;

/// Implements [`ServerHook`] for [`Upload`].
impl ServerHook for Upload {
    /// Builds the route.
    ///
    /// # Arguments
    ///
    /// - `&mut Stream` - The stream the request arrived on.
    /// - `&mut Context` - The context of the request.
    ///
    /// # Returns
    ///
    /// - `Self` - The route.
    async fn new(_: &mut Stream, _: &mut Context) -> Self {
        Self
    }

    /// Answers the request.
    ///
    /// The media type is checked before the body is read, so a request whose
    /// media type is unsupported is rejected with `415` even when its body is
    /// malformed. Exactly one uploaded file part named `file` must be
    /// present: a part that carries no file name is an unselected file input
    /// and is skipped, and a part that repeats the name makes the request
    /// invalid.
    ///
    /// # Arguments
    ///
    /// - `Self` - The route.
    /// - `&mut Stream` - The stream the request arrived on.
    /// - `&mut Context` - The context of the request.
    ///
    /// # Returns
    ///
    /// - `Status` - Whether the pipeline continues.
    async fn handle(self, _: &mut Stream, ctx: &mut Context) -> Status {
        if !ctx.request.method.is_post() {
            respond_empty(ctx, 404);
            return Status::Continue;
        }
        let Some(content_type) = ctx.request.try_get_content_type() else {
            respond_empty(ctx, 415);
            return Status::Continue;
        };
        let media_type: String = content_type
            .split(';')
            .next()
            .unwrap_or(&content_type)
            .trim()
            .to_ascii_lowercase();
        if media_type != MULTIPART_FORM_DATA_MEDIA_TYPE {
            respond_empty(ctx, 415);
            return Status::Continue;
        }
        let Some(parts) = ctx.request.try_get_body_form_data() else {
            respond_empty(ctx, 400);
            return Status::Continue;
        };
        let mut uploaded: Option<&[u8]> = None;
        for part in &parts {
            let Some(name) = part.try_get_name() else {
                continue;
            };
            if name != "file" {
                continue;
            }
            if !declares_file_name(part) {
                // A part named `file` that declares no file name is a plain
                // text field, not an upload.
                respond_empty(ctx, 400);
                return Status::Continue;
            }
            let Some(file_name) = part.try_get_file_name() else {
                // A part that declares an empty file name is an unselected
                // file input and does not count as an upload.
                continue;
            };
            if file_name.is_empty() {
                continue;
            }
            if uploaded.is_some() {
                respond_empty(ctx, 400);
                return Status::Continue;
            }
            let data: &[u8] = part.try_get_data().unwrap_or_default();
            uploaded = Some(data);
        }
        let Some(uploaded) = uploaded else {
            respond_empty(ctx, 400);
            return Status::Continue;
        };
        let first_byte: Option<u8> = uploaded.first().copied();
        let last_byte: Option<u8> = uploaded.last().copied();
        let mut hasher: Sha256 = Sha256::new();
        hasher.update(uploaded);
        let checksum: String = hasher
            .finalize()
            .iter()
            .map(|byte: &u8| format!("{byte:02x}"))
            .collect();
        let body: String = format!(
            "{{\"first_byte\":{},\"last_byte\":{},\"checksum\":\"{checksum}\"}}",
            first_byte.map_or(JSON_NULL.to_owned(), |byte: u8| byte.to_string()),
            last_byte.map_or(JSON_NULL.to_owned(), |byte: u8| byte.to_string())
        );
        respond_json(ctx, 200, &body);
        Status::Continue
    }
}

/// The response middleware that writes the response onto the stream.
///
/// Every route fills the response and leaves the write to this middleware, so
/// the response is built and sent exactly once per request.
#[response_middleware]
struct Send;

/// Implements [`ServerHook`] for [`Send`].
impl ServerHook for Send {
    /// Builds the middleware.
    ///
    /// # Arguments
    ///
    /// - `&mut Stream` - The stream the request arrived on.
    /// - `&mut Context` - The context of the request.
    ///
    /// # Returns
    ///
    /// - `Self` - The middleware.
    async fn new(_: &mut Stream, _: &mut Context) -> Self {
        Self
    }

    /// Writes the response onto the stream.
    ///
    /// # Arguments
    ///
    /// - `Self` - The middleware.
    /// - `&mut Stream` - The stream the request arrived on.
    /// - `&mut Context` - The context of the request.
    ///
    /// # Returns
    ///
    /// - `Status` - Whether the pipeline continues.
    async fn handle(self, stream: &mut Stream, ctx: &mut Context) -> Status {
        let data: Vec<u8> = ctx.response.build();
        if stream.try_send(data).await.is_err() {
            stream.set_closed(true);
            return Status::Reject;
        }
        Status::Continue
    }
}

/// Starts the server and serves until it is shut down.
#[hyperlane(server: Server)]
#[tokio::main]
async fn main() {
    server
        .server_config(init_server_config())
        .request_config(init_request_config())
        .run()
        .await
        .unwrap()
        .wait()
        .await;
}

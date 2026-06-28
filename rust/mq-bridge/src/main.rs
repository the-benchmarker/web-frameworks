//! the-benchmarker/web-frameworks server for mq-bridge (Rust).
//!
//! A production-grade benchmark server implementation using mq-bridge framework.
//! Implements security best-practices, proper error handling, and environment-based configuration.
//!
//! | Method | Path        | Response          |
//! |--------|-------------|-------------------|
//! | GET    | `/`         | 200, empty body   |
//! | GET    | `/user/:id` | 200, the `id`     |
//! | POST   | `/user`     | 201, empty body   |
//! | GET    | `/health`   | 200, "OK"          |
//! | GET    | `/error`   | 500, error body    |
//!
//! Design notes
//! ------------
//! * mq-bridge's HTTP path filter is an exact match, so it can't express the
//!   `/user/:id` path parameter. We instead use a single catch-all
//!   `http -> response` route and dispatch in the handler on the `http_method`
//!   and `http_path` request metadata, extracting `:id` as the suffix after
//!   `/user/`.
//! * The normal route pipeline is used because this benchmark needs one
//!   catch-all HTTP consumer and handler-level dispatch.

use mq_bridge::models::{Endpoint, EndpointType, HttpConfig};
use mq_bridge::{CanonicalMessage, Handled, HandlerError, Route};
use std::{env, sync::OnceLock};

// Configuration - Environment-based settings for production vs development
static DEBUG_MODE: OnceLock<bool> = OnceLock::new();

fn get_debug_mode() -> bool {
    *DEBUG_MODE.get_or_init(|| {
        env::var("DEBUG").unwrap_or_else(|_| "false".to_string()) == "true"
    })
}

// Security headers configuration
fn security_headers() -> Vec<(&'static str, &'static str)> {
    vec![
        ("X-Content-Type-Options", "nosniff"),
        ("X-Frame-Options", "DENY"),
        ("X-XSS-Protection", "1; mode=block"),
        ("Content-Security-Policy", "default-src 'self'"),
        ("Referrer-Policy", "strict-origin-when-cross-origin"),
        ("Cache-Control", "no-cache, no-store, must-revalidate"),
    ]
}

// Add security headers to message metadata
fn add_security_headers_to_message(msg: CanonicalMessage) -> CanonicalMessage {
    let mut msg = msg;
    for (key, value) in security_headers() {
        msg = msg.with_metadata_kv(key, value);
    }
    msg
}

const USER_PREFIX: &str = "/user/";

fn empty_200() -> CanonicalMessage {
    if get_debug_mode() {
        eprintln!("[DEBUG] Root endpoint accessed");
    }
    // No body, default 200. A fresh message so no request headers are echoed.
    add_security_headers_to_message(CanonicalMessage::new(Vec::new(), None))
}

fn empty_201() -> CanonicalMessage {
    if get_debug_mode() {
        eprintln!("[DEBUG] Create user endpoint accessed");
    }
    // No body, 201 Created
    add_security_headers_to_message(
        CanonicalMessage::new(Vec::new(), None).with_metadata_kv("http_status_code", "201")
    )
}

fn text_200(body: Vec<u8>) -> CanonicalMessage {
    add_security_headers_to_message(
        CanonicalMessage::new(body, None).with_metadata_kv("content-type", "text/plain")
    )
}

fn text_500(body: Vec<u8>) -> CanonicalMessage {
    add_security_headers_to_message(
        CanonicalMessage::new(body, None)
            .with_metadata_kv("content-type", "text/plain")
            .with_metadata_kv("http_status_code", "500")
    )
}

fn empty_500() -> CanonicalMessage {
    if get_debug_mode() {
        eprintln!("[ERROR] Error endpoint accessed");
    }
    add_security_headers_to_message(
        CanonicalMessage::new(Vec::new(), None)
            .with_metadata_kv("content-type", "text/plain")
            .with_metadata_kv("http_status_code", "500")
    )
}

fn health_200() -> CanonicalMessage {
    if get_debug_mode() {
        eprintln!("[DEBUG] Health check endpoint accessed");
    }
    add_security_headers_to_message(
        CanonicalMessage::new(b"OK".to_vec(), None)
            .with_metadata_kv("content-type", "text/plain")
    )
}

fn not_found() -> CanonicalMessage {
    if get_debug_mode() {
        eprintln!("[DEBUG] Unknown path accessed");
    }
    if get_debug_mode() {
        add_security_headers_to_message(
            CanonicalMessage::new(b"Not Found".to_vec(), None)
                .with_metadata_kv("content-type", "text/plain")
                .with_metadata_kv("http_status_code", "404")
        )
    } else {
        add_security_headers_to_message(
            CanonicalMessage::new(Vec::new(), None)
                .with_metadata_kv("content-type", "text/plain")
                .with_metadata_kv("http_status_code", "404")
        )
    }
}

async fn handle(msg: CanonicalMessage) -> Result<Handled, HandlerError> {
    let method = msg
        .metadata
        .get("http_method")
        .map(String::as_str)
        .unwrap_or("");
    let path = msg
        .metadata
        .get("http_path")
        .map(String::as_str)
        .unwrap_or("");

    let reply = match (method, path) {
        ("GET", "/") => empty_200(),
        ("POST", "/user") => empty_201(),
        ("GET", "/health") => health_200(),
        ("GET", "/error") => {
            if get_debug_mode() {
                text_500(b"Internal Server Error".to_vec())
            } else {
                empty_500()
            }
        }
        ("GET", p) => match p.strip_prefix(USER_PREFIX) {
            // GET /user/:id -> echo the id segment as the body.
            Some(id) if !id.is_empty() && !id.contains('/') => text_200(id.as_bytes().to_vec()),
            _ => not_found(),
        },
        _ => not_found(),
    };

    Ok(Handled::Publish(reply))
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Get configuration from environment
    let host = env::var("HOST").unwrap_or_else(|_| "0.0.0.0".to_string());
    let port = env::var("PORT").unwrap_or_else(|_| "3000".to_string());
    let listen = format!("{}:{}", host, port);

    // Startup message with configuration summary
    if get_debug_mode() {
        eprintln!("\n=== mq-bridge Framework Benchmark Server (Development Mode) ===");
        eprintln!("Environment: development");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: true");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Enabled (debug level)");
        eprintln!("Endpoints: /, /user/:id, /user, /health, /error");
        eprintln!("==============================================================\n");
    } else {
        eprintln!("\n=== mq-bridge Framework Benchmark Server (Production Mode) ===");
        eprintln!("Environment: production");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: false");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Disabled (production mode)");
        eprintln!("==============================================================\n");
    }

    // No method/path filter: every request reaches the handler, which routes it.
    let mut http = HttpConfig::new(listen)
        .with_inline_response_fast_path(true);
    http.concurrency_limit = Some(65_536);
    http.internal_buffer_size = Some(16_384);

    let input = Endpoint::new(EndpointType::Http(http));
    let output = Endpoint::new_response();

    let route = Route::new(input, output).with_handler(|msg| handle(msg));
    let handle = route.run("the-benchmarker").await?;
    handle.join().await?;
    Ok(())
}

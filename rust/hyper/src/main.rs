#![allow(clippy::unused_unit)]

use http_body_util::{combinators::BoxBody, BodyExt, Empty, Full};
use hyper::{body::Bytes, http::{HeaderValue, Method, StatusCode}, server, service::service_fn, Request, Response};
use hyper_util::rt::TokioIo;
use once_cell::sync::OnceCell;
use std::{env, net::SocketAddr};
use tokio::net::TcpListener;

// Configuration - Environment-based settings for production vs development
static DEBUG_MODE: OnceCell<bool> = OnceCell::new();
static HOST: OnceCell<String> = OnceCell::new();
static PORT: OnceCell<u16> = OnceCell::new();

#[inline]
fn get_debug_mode() -> bool {
    *DEBUG_MODE.get_or_init(|| {
        env::var("DEBUG").unwrap_or_else(|_| "false".to_string()) == "true"
    })
}

#[inline]
fn get_host() -> &'static str {
    HOST.get_or_init(|| {
        env::var("HOST").unwrap_or_else(|_| "0.0.0.0".to_string())
    })
}

#[inline]
fn get_port() -> u16 {
    *PORT.get_or_init(|| {
        env::var("PORT").unwrap_or_else(|_| "3000".to_string()).parse().unwrap_or(3000)
    })
}

// Security headers configuration - pre-allocated for performance
static SECURITY_HEADERS: &[(&'static str, &'static str)] = &[
    ("X-Content-Type-Options", "nosniff"),
    ("X-Frame-Options", "DENY"),
    ("X-XSS-Protection", "1; mode=block"),
    ("Content-Security-Policy", "default-src 'self'"),
    ("Referrer-Policy", "strict-origin-when-cross-origin"),
    ("Cache-Control", "no-cache, no-store, must-revalidate"),
];

static PATH_PREFIX: &str = "/user/";

static PATH_PREFIX: &'static str = "/user/";

// Apply security headers to response
#[inline]
fn apply_security_headers(mut response: Response<BoxBody<Bytes, hyper::Error>>) -> Response<BoxBody<Bytes, hyper::Error>> {
    for &(key, value) in SECURITY_HEADERS {
        if let Ok(header_value) = HeaderValue::from_str(value) {
            if let Ok(header_name) = hyper::header::HeaderName::from_bytes(key.as_bytes()) {
                response.headers_mut().insert(header_name, header_value);
            }
        }
    }
    response
}

#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    // Initialize configuration
    let debug_mode = get_debug_mode();
    let host = get_host();
    let port = get_port();
    let addr: SocketAddr = format!("{}:{}", host, port).parse().unwrap();
    
    // Startup message with configuration summary
    let mode = if debug_mode { "Development" } else { "Production" };
    let log_status = if debug_mode { "Enabled" } else { "Disabled" };
    
    eprintln!("\n=== Hyper Framework Benchmark Server ({} Mode) ===", mode);
    eprintln!("Environment: {}", if debug_mode { "development" } else { "production" });
    eprintln!("Host: {}, Port: {}", host, port);
    eprintln!("Debug: {}, Security headers: Enabled", debug_mode);
    eprintln!("Logging: {} ({} level)", log_status, if debug_mode { "debug" } else { "warn" });
    eprintln!("Endpoints: /, /user/:id, /user, /health, /error");
    eprintln!("========================================================\n");

    let listener = TcpListener::bind(addr).await?;

    loop {
        let (stream, _) = listener.accept().await?;
        let io = TokioIo::new(stream);
        tokio::task::spawn(async move {
            let _ = server::conn::http1::Builder::new()
                .pipeline_flush(true)
                .serve_connection(io, service_fn(handle))
                .await;
        });
    }
}

async fn handle(
    req: Request<hyper::body::Incoming>,
) -> Result<Response<BoxBody<hyper::body::Bytes, hyper::Error>>, hyper::Error> {
    let debug_mode = get_debug_mode();
    
    // Debug logging
    if debug_mode {
        eprintln!("[DEBUG] {} {}", req.method(), req.uri().path());
    }
    
    let (status, body_text, content_type) = match (req.method(), req.uri().path()) {
        (&Method::GET, "/") => {
            if debug_mode {
                eprintln!("[DEBUG] Root endpoint accessed");
            }
            (StatusCode::OK, "", "text/plain")
        }
        (&Method::POST, "/user") => {
            if debug_mode {
                eprintln!("[DEBUG] Create user endpoint accessed");
            }
            (StatusCode::CREATED, "", "text/plain")
        }
        (&Method::GET, "/health") => {
            if debug_mode {
                eprintln!("[DEBUG] Health check endpoint accessed");
            }
            (StatusCode::OK, "OK", "text/plain")
        }
        (&Method::GET, "/error") => {
            if debug_mode {
                eprintln!("[ERROR] Error endpoint accessed");
            }
            (StatusCode::INTERNAL_SERVER_ERROR, if debug_mode { "Internal Server Error" } else { "" }, "text/plain")
        }
        (&Method::GET, x) => {
            if let Some(user_id) = x.strip_prefix(PATH_PREFIX) {
                if debug_mode {
                    eprintln!("[DEBUG] User endpoint accessed with ID: {}", user_id);
                }
                return Ok(apply_security_headers(
                    Response::builder()
                        .status(StatusCode::OK)
                        .body(full(user_id.to_owned()))
                        .unwrap()
                        .header("Content-Type", "text/plain")
                ));
            }
            // Fallback for unknown paths
            if debug_mode {
                eprintln!("[DEBUG] Unknown path accessed: {}", x);
            }
            (StatusCode::NOT_FOUND, if debug_mode { "Not Found" } else { "" }, "text/plain")
        }
        _ => {
            if debug_mode {
                eprintln!("[DEBUG] Method not allowed for path: {}", req.uri().path());
            }
            (StatusCode::METHOD_NOT_ALLOWED, if debug_mode { "Method Not Allowed" } else { "" }, "text/plain")
        }
    };
    
    let mut res = if body_text.is_empty() {
        Response::new(empty())
    } else {
        Response::new(full(body_text))
    };
    res.headers_mut().insert("Content-Type", HeaderValue::from_static(content_type));
    
    Ok(apply_security_headers(res))
}

#[inline]
fn full<T: Into<Bytes>>(chunk: T) -> BoxBody<Bytes, hyper::Error> {
    Full::new(chunk.into())
        .map_err(|never| match never {})
        .boxed()
}

#[inline]
fn empty() -> BoxBody<Bytes, hyper::Error> {
    Empty::<Bytes>::new()
        .map_err(|never| match never {})
        .boxed()
}

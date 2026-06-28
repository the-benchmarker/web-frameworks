use http_body_util::{combinators::BoxBody, BodyExt, Empty, Full};
use hyper::{body::Bytes, http::{HeaderValue, Method, StatusCode}, server, service::service_fn, Request, Response};
use hyper_util::rt::TokioIo;
use std::{env, net::SocketAddr, sync::OnceLock};
use tokio::net::TcpListener;

// Configuration - Environment-based settings for production vs development
static DEBUG_MODE: OnceLock<bool> = OnceLock::new();
static HOST: OnceLock<String> = OnceLock::new();
static PORT: OnceLock<u16> = OnceLock::new();

fn get_debug_mode() -> bool {
    *DEBUG_MODE.get_or_init(|| {
        env::var("DEBUG").unwrap_or_else(|_| "false".to_string()) == "true"
    })
}

fn get_host() -> String {
    HOST.get_or_init(|| {
        env::var("HOST").unwrap_or_else(|_| "0.0.0.0".to_string())
    }).clone()
}

fn get_port() -> u16 {
    *PORT.get_or_init(|| {
        env::var("PORT").unwrap_or_else(|_| "3000".to_string()).parse().unwrap_or(3000)
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

static PATH_PREFIX: &'static str = "/user/";

// Apply security headers to response
fn apply_security_headers(mut response: Response<BoxBody<Bytes, hyper::Error>>) -> Response<BoxBody<Bytes, hyper::Error>> {
    for (key, value) in security_headers() {
        if let Ok(header_value) = HeaderValue::from_str(value) {
            response.headers_mut().insert(key, header_value);
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
    if debug_mode {
        eprintln!("\n=== Hyper Framework Benchmark Server (Development Mode) ===");
        eprintln!("Environment: development");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: true");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Enabled (debug level)");
        eprintln!("Endpoints: /, /user/:id, /user, /health, /error");
        eprintln!("========================================================\n");
    } else {
        eprintln!("\n=== Hyper Framework Benchmark Server (Production Mode) ===");
        eprintln!("Environment: production");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: false");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Disabled (production mode)");
        eprintln!("========================================================\n");
    }

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
    
    let response = match (req.method(), req.uri().path()) {
        (&Method::GET, "/") => {
            if debug_mode {
                eprintln!("[DEBUG] Root endpoint accessed");
            }
            let mut res = Response::new(empty());
            res.headers_mut().insert("Content-Type", HeaderValue::from_static("text/plain"));
            apply_security_headers(res)
        }
        (&Method::POST, "/user") => {
            if debug_mode {
                eprintln!("[DEBUG] Create user endpoint accessed");
            }
            let mut res = Response::builder().status(StatusCode::CREATED).body(empty()).unwrap();
            res.headers_mut().insert("Content-Type", HeaderValue::from_static("text/plain"));
            apply_security_headers(res)
        }
        (&Method::GET, "/health") => {
            if debug_mode {
                eprintln!("[DEBUG] Health check endpoint accessed");
            }
            let mut res = Response::new(full("OK"));
            res.headers_mut().insert("Content-Type", HeaderValue::from_static("text/plain"));
            apply_security_headers(res)
        }
        (&Method::GET, "/error") => {
            if debug_mode {
                eprintln!("[ERROR] Error endpoint accessed");
            }
            let body = if debug_mode { full("Internal Server Error") } else { empty() };
            let mut res = Response::builder().status(StatusCode::INTERNAL_SERVER_ERROR).body(body).unwrap();
            res.headers_mut().insert("Content-Type", HeaderValue::from_static("text/plain"));
            apply_security_headers(res)
        }
        (&Method::GET, x) => {
            if let Some((_, user_id)) = x.split_once(PATH_PREFIX) {
                if debug_mode {
                    eprintln!("[DEBUG] User endpoint accessed with ID: {}", user_id);
                }
                let mut res = Response::new(full(user_id.to_owned()));
                res.headers_mut().insert("Content-Type", HeaderValue::from_static("text/plain"));
                return Ok(apply_security_headers(res));
            }
            // Fallback for unknown paths
            if debug_mode {
                eprintln!("[DEBUG] Unknown path accessed: {}", x);
            }
            let body = if debug_mode { full("Not Found") } else { empty() };
            let mut res = Response::builder().status(StatusCode::NOT_FOUND).body(body).unwrap();
            res.headers_mut().insert("Content-Type", HeaderValue::from_static("text/plain"));
            apply_security_headers(res)
        }
        _ => {
            if debug_mode {
                eprintln!("[DEBUG] Method not allowed for path: {}", req.uri().path());
            }
            let body = if debug_mode { full("Method Not Allowed") } else { empty() };
            let mut res = Response::builder().status(StatusCode::METHOD_NOT_ALLOWED).body(body).unwrap();
            res.headers_mut().insert("Content-Type", HeaderValue::from_static("text/plain"));
            apply_security_headers(res)
        }
    };
    
    Ok(response)
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

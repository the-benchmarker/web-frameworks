#![allow(clippy::unused_unit)]

use once_cell::sync::Lazy;
use poem::listener::TcpListener;
use poem::web::{header, Path};
use poem::{get, handler, post, IntoResponse, Response, Route, Server};
use std::env;

// Configuration - Environment-based settings for production vs development
static DEBUG_MODE: Lazy<bool> = Lazy::new(|| {
    env::var("DEBUG").unwrap_or_else(|_| "false".to_string()) == "true"
});

// Security headers configuration - pre-allocated for performance
static SECURITY_HEADERS: &[(&'static str, &'static str)] = &[
    ("X-Content-Type-Options", "nosniff"),
    ("X-Frame-Options", "DENY"),
    ("X-XSS-Protection", "1; mode=block"),
    ("Content-Security-Policy", "default-src 'self'"),
    ("Referrer-Policy", "strict-origin-when-cross-origin"),
    ("Cache-Control", "no-cache, no-store, must-revalidate"),
];

// Apply security headers middleware
struct SecurityHeaders;

#[poem::async_trait]
impl poem::Middleware for SecurityHeaders {
    async fn transform(&self, req: poem::Request, next: poem::Next) -> poem::Result<poem::Response> {
        let mut resp = next.run(req).await;
        for &(key, value) in SECURITY_HEADERS {
            resp.headers.insert(
                header::HeaderName::from_static(key),
                header::HeaderValue::from_static(value)
            );
        }
        resp.headers.insert(
            header::CONTENT_TYPE,
            header::HeaderValue::from_static("text/plain")
        );
        Ok(resp)
    }
}

#[handler]
async fn index() {
    if *DEBUG_MODE {
        eprintln!("[DEBUG] Root endpoint accessed");
    }
}

#[handler]
async fn get_user(Path(id): Path<String>) -> impl IntoResponse {
    if *DEBUG_MODE {
        eprintln!("[DEBUG] User endpoint accessed with ID: {}", id);
    }
    id
}

#[handler]
async fn post_user() {
    if *DEBUG_MODE {
        eprintln!("[DEBUG] Create user endpoint accessed");
    }
}

#[handler]
async fn health() {
    if *DEBUG_MODE {
        eprintln!("[DEBUG] Health check endpoint accessed");
    }
    "OK"
}

#[handler]
async fn error() -> impl IntoResponse {
    if *DEBUG_MODE {
        eprintln!("[ERROR] Error endpoint accessed");
    }
    Response::builder()
        .status(500)
        .body(if *DEBUG_MODE { "Internal Server Error" } else { "" })
}

#[tokio::main]
async fn main() -> std::io::Result<()> {
    // Get configuration from environment
    let host = env::var("HOST").unwrap_or_else(|_| "0.0.0.0".to_string());
    let port = env::var("PORT").unwrap_or_else(|_| "3000".to_string());
    let addr = format!("{}:{}", host, port);

    // Startup message with configuration summary
    let mode = if *DEBUG_MODE { "Development" } else { "Production" };
    let log_status = if *DEBUG_MODE { "Enabled" } else { "Disabled" };
    
    eprintln!("\n=== Poem Framework Benchmark Server ({} Mode) ===", mode);
    eprintln!("Environment: {}", if *DEBUG_MODE { "development" } else { "production" });
    eprintln!("Host: {}, Port: {}", host, port);
    eprintln!("Debug: {}, Security headers: Enabled", *DEBUG_MODE);
    eprintln!("Logging: {} ({} level)", log_status, if *DEBUG_MODE { "debug" } else { "warn" });
    eprintln!("Endpoints: /, /user/:id, /user, /health, /error");
    eprintln!("==========================================================\n");

    let app = Route::new()
        .at("/", get(index))
        .at("/user", post(post_user))
        .at("/user/:id", get(get_user))
        .at("/health", get(health))
        .at("/error", get(error))
        .with(SecurityHeaders);

    Server::new(TcpListener::bind(addr))
        .run(app)
        .await
}

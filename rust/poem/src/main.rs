use poem::listener::TcpListener;
use poem::web::{header, Path};
use poem::{get, handler, post, IntoResponse, Route, Server};
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

// Apply security headers middleware
struct SecurityHeaders;

#[poem::async_trait]
impl poem::Middleware for SecurityHeaders {
    async fn transform(&self, req: poem::Request, next: poem::Next) -> poem::Result<poem::Response> {
        let mut resp = next.run(req).await;
        for (key, value) in security_headers() {
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
    if get_debug_mode() {
        eprintln!("[DEBUG] Root endpoint accessed");
    }
}

#[handler]
async fn get_user(Path(id): Path<String>) -> impl IntoResponse {
    if get_debug_mode() {
        eprintln!("[DEBUG] User endpoint accessed with ID: {}", id);
    }
    id
}

#[handler]
async fn post_user() {
    if get_debug_mode() {
        eprintln!("[DEBUG] Create user endpoint accessed");
    }
}

#[handler]
async fn health() {
    if get_debug_mode() {
        eprintln!("[DEBUG] Health check endpoint accessed");
    }
    "OK"
}

#[handler]
async fn error() -> impl IntoResponse {
    if get_debug_mode() {
        eprintln!("[ERROR] Error endpoint accessed");
    }
    if get_debug_mode() {
        poem::Response::builder()
            .status(500)
            .body("Internal Server Error")
    } else {
        poem::Response::builder()
            .status(500)
            .body("")
    }
}

#[tokio::main]
async fn main() -> std::io::Result<()> {
    // Get configuration from environment
    let host = env::var("HOST").unwrap_or_else(|_| "0.0.0.0".to_string());
    let port = env::var("PORT").unwrap_or_else(|_| "3000".to_string());
    let addr = format!("{}:{}", host, port);

    // Startup message with configuration summary
    if get_debug_mode() {
        eprintln!("\n=== Poem Framework Benchmark Server (Development Mode) ===");
        eprintln!("Environment: development");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: true");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Enabled (debug level)");
        eprintln!("Endpoints: /, /user/:id, /user, /health, /error");
        eprintln!("==========================================================\n");
    } else {
        eprintln!("\n=== Poem Framework Benchmark Server (Production Mode) ===");
        eprintln!("Environment: production");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: false");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Disabled (production mode)");
        eprintln!("==========================================================\n");
    }

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

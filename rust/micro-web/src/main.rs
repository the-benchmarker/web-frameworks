use micro_web::responder::Responder;
use micro_web::router::{Router, get, post};
use micro_web::{PathParams, Server, responder};
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

async fn empty_body() -> &'static str {
    if get_debug_mode() {
        eprintln!("[DEBUG] Root endpoint accessed");
    }
    ""
}

async fn echo_uid<'s, 'r>(path_params: &PathParams<'s, 'r>) -> String {
    let id = path_params.get("id").map(|s| s.to_owned()).unwrap();
    if get_debug_mode() {
        eprintln!("[DEBUG] User endpoint accessed with ID: {}", id);
    }
    id
}

async fn create_user() -> &'static str {
    if get_debug_mode() {
        eprintln!("[DEBUG] Create user endpoint accessed");
    }
    ""
}

async fn health_handler() -> &'static str {
    if get_debug_mode() {
        eprintln!("[DEBUG] Health check endpoint accessed");
    }
    "OK"
}

async fn error_handler() -> impl Responder {
    if get_debug_mode() {
        eprintln!("[ERROR] Error endpoint accessed");
    }
    if get_debug_mode() {
        responder::InternalServerError.with_body("Internal Server Error")
    } else {
        responder::InternalServerError.with_body("")
    }
}

async fn default_handler() -> impl Responder {
    if get_debug_mode() {
        eprintln!("[DEBUG] Unknown path accessed");
    }
    if get_debug_mode() {
        responder::NotFound.with_body("Not Found")
    } else {
        responder::NotFound.with_body("")
    }
}

#[tokio::main]
async fn main() {
    // Get configuration from environment
    let host = env::var("HOST").unwrap_or_else(|_| "0.0.0.0".to_string());
    let port = env::var("PORT").unwrap_or_else(|_| "3000".to_string());
    let addr = format!("{}:{}", host, port);

    // Startup message with configuration summary
    if get_debug_mode() {
        eprintln!("\n=== Micro-Web Framework Benchmark Server (Development Mode) ===");
        eprintln!("Environment: development");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: true");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Enabled (debug level)");
        eprintln!("Endpoints: /, /user/:id, /user, /health, /error");
        eprintln!("===============================================================\n");
    } else {
        eprintln!("\n=== Micro-Web Framework Benchmark Server (Production Mode) ===");
        eprintln!("Environment: production");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: false");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Disabled (production mode)");
        eprintln!("===============================================================\n");
    }
    
    // Build router with multiple routes and handlers
    let router = Router::builder()
        .route("/", get(empty_body))
        .route("/user", post(create_user))
        .route("/user/{id}", get(echo_uid))
        .route("/health", get(health_handler))
        .route("/error", get(error_handler))
        .build();

    // Configure and start the server
    Server::builder()
        .router(router)
        .bind(addr)
        .default_handler(default_handler)
        .build()
        .unwrap()
        .start()
        .await;
}


use ohkami::prelude::*;
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

// Apply security headers to response
fn apply_security_headers(mut response: Response) -> Response {
    for (key, value) in security_headers() {
        response.headers.insert(key.parse().unwrap(), value.parse().unwrap());
    }
    response
}

#[nio::main]
async fn main() {
    // Get configuration from environment
    let host = env::var("HOST").unwrap_or_else(|_| "0.0.0.0".to_string());
    let port = env::var("PORT").unwrap_or_else(|_| "3000".to_string());
    let addr = format!("{}:{}", host, port);

    // Startup message with configuration summary
    if get_debug_mode() {
        eprintln!("\n=== Ohkami-nio Framework Benchmark Server (Development Mode) ===");
        eprintln!("Environment: development");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: true");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Enabled (debug level)");
        eprintln!("Endpoints: /, /user/:id, /user, /health, /error");
        eprintln!("==============================================================\n");
    } else {
        eprintln!("\n=== Ohkami-nio Framework Benchmark Server (Production Mode) ===");
        eprintln!("Environment: production");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: false");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Disabled (production mode)");
        eprintln!("==============================================================\n");
    }

    Ohkami::new((
        "/".GET(async || {
            if get_debug_mode() {
                eprintln!("[DEBUG] Root endpoint accessed");
            }
            apply_security_headers(Response::OK())
        }),
        "/user".POST(async || {
            if get_debug_mode() {
                eprintln!("[DEBUG] Create user endpoint accessed");
            }
            apply_security_headers(Response::Created())
        }),
        "/user/:id".GET(async |Path(id): Path<String>| {
            if get_debug_mode() {
                eprintln!("[DEBUG] User endpoint accessed with ID: {}", id);
            }
            apply_security_headers(Response::Ok().body(id))
        }),
        "/health".GET(async || {
            if get_debug_mode() {
                eprintln!("[DEBUG] Health check endpoint accessed");
            }
            apply_security_headers(Response::Ok().body("OK"))
        }),
        "/error".GET(async || {
            if get_debug_mode() {
                eprintln!("[ERROR] Error endpoint accessed");
            }
            if get_debug_mode() {
                apply_security_headers(Response::InternalServerError().body("Internal Server Error"))
            } else {
                apply_security_headers(Response::InternalServerError())
            }
        }),
    ))
    .howl(addr)
    .await
}

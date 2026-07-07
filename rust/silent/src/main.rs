use silent::prelude::*;
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
    response.headers.insert("Content-Type".parse().unwrap(), "text/plain".parse().unwrap());
    response
}

async fn index(_req: Request) -> Result<Response> {
    if get_debug_mode() {
        eprintln!("[DEBUG] Root endpoint accessed");
    }
    Ok(apply_security_headers(Response::empty()))
}

async fn create_user(_req: Request) -> Result<Response> {
    if get_debug_mode() {
        eprintln!("[DEBUG] Create user endpoint accessed");
    }
    Ok(apply_security_headers(Response::empty().with_status(201)))
}

async fn user_handler(req: Request) -> Result<Response> {
    let id = req.get_path_params::<String>("id");
    if get_debug_mode() {
        eprintln!("[DEBUG] User endpoint accessed with ID: {}", id);
    }
    Ok(apply_security_headers(Response::new().with_body(id)))
}

async fn health_handler(_req: Request) -> Result<Response> {
    if get_debug_mode() {
        eprintln!("[DEBUG] Health check endpoint accessed");
    }
    Ok(apply_security_headers(Response::new().with_body("OK")))
}

async fn error_handler(_req: Request) -> Result<Response> {
    if get_debug_mode() {
        eprintln!("[ERROR] Error endpoint accessed");
    }
    if get_debug_mode() {
        Ok(apply_security_headers(Response::new().with_status(500).with_body("Internal Server Error")))
    } else {
        Ok(apply_security_headers(Response::empty().with_status(500)))
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
        eprintln!("\n=== Silent Framework Benchmark Server (Development Mode) ===");
        eprintln!("Environment: development");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: true");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Enabled (debug level)");
        eprintln!("Endpoints: /, /user/:id, /user, /health, /error");
        eprintln!("==========================================================\n");
    } else {
        eprintln!("\n=== Silent Framework Benchmark Server (Production Mode) ===");
        eprintln!("Environment: production");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: false");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Disabled (production mode)");
        eprintln!("==========================================================\n");
    }

    let route = Route::new("").get(index).append(
        Route::new("user")
            .append(
                Route::new("<id>")
                    .get(user_handler),
            )
            .post(create_user),
    )
    .get("health", health_handler)
    .get("error", error_handler);

    Server::new()
        .bind(addr.parse().unwrap())
        .serve(route)
        .await;
}

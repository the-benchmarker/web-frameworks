#![warn(rust_2018_idioms)]

use iron::{prelude::*, status, headers, modifier::Set};
use router::Router;
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
fn apply_security_headers(response: Response) -> Response {
    let mut res = response;
    for (key, value) in security_headers() {
        res.headers.set_raw(key, vec![value.as_bytes().to_vec()]);
    }
    res.headers.set(headers::ContentType::plaintext());
    res
}

fn index_handler(_: &mut Request<'_, '_>) -> IronResult<Response> {
    if get_debug_mode() {
        eprintln!("[DEBUG] Root endpoint accessed");
    }
    Ok(apply_security_headers(Response::with(status::Ok)))
}

fn user_handler(req: &mut Request<'_, '_>) -> IronResult<Response> {
    let ref id = req
        .extensions
        .get::<Router>()
        .unwrap()
        .find("id")
        .unwrap_or("");
    
    if get_debug_mode() {
        eprintln!("[DEBUG] User endpoint accessed with ID: {}", id);
    }
    
    let mut response = Response::with((status::Ok, id.to_string()));
    apply_security_headers(response)
    Ok(response)
}

fn user_register_handler(_: &mut Request<'_, '_>) -> IronResult<Response> {
    if get_debug_mode() {
        eprintln!("[DEBUG] Create user endpoint accessed");
    }
    Ok(apply_security_headers(Response::with(status::Created)))
}

fn health_handler(_: &mut Request<'_, '_>) -> IronResult<Response> {
    if get_debug_mode() {
        eprintln!("[DEBUG] Health check endpoint accessed");
    }
    Ok(apply_security_headers(Response::with((status::Ok, "OK"))))
}

fn error_handler(_: &mut Request<'_, '_>) -> IronResult<Response> {
    if get_debug_mode() {
        eprintln!("[ERROR] Error endpoint accessed");
    }
    if get_debug_mode() {
        Ok(apply_security_headers(Response::with((status::InternalServerError, "Internal Server Error"))))
    } else {
        Ok(apply_security_headers(Response::with(status::InternalServerError)))
    }
}

fn main() {
    // Get configuration from environment
    let host = env::var("HOST").unwrap_or_else(|_| "0.0.0.0".to_string());
    let port = env::var("PORT").unwrap_or_else(|_| "3000".to_string());
    let addr = format!("{}:{}", host, port);

    // Startup message with configuration summary
    if get_debug_mode() {
        eprintln!("\n=== Iron Framework Benchmark Server (Development Mode) ===");
        eprintln!("Environment: development");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: true");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Enabled (debug level)");
        eprintln!("Endpoints: /, /user/:id, /user, /health, /error");
        eprintln!("=======================================================\n");
    } else {
        eprintln!("\n=== Iron Framework Benchmark Server (Production Mode) ===");
        eprintln!("Environment: production");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: false");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Disabled (production mode)");
        eprintln!("=======================================================\n");
    }

    let mut router = Router::new();
    router.get("/", index_handler, "index");
    router.get("/user/:id", user_handler, "user");
    router.post("/user", user_register_handler, "user_register");
    router.get("/health", health_handler, "health");
    router.get("/error", error_handler, "error");

    Iron::new(router).http(addr.as_str()).unwrap();
}

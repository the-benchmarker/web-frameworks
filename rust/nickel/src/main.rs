#![warn(rust_2018_idioms)]

// nickel macros are declarative macros that use private macros
// from the nickel namespace without specifying a path, therefore
// assuming that we import private stuff from nickel
use nickel::*;
use std::{env, sync::OnceLock};

// Configuration - Environment-based settings for production vs development
static DEBUG_MODE: OnceLock<bool> = OnceLock::new();

fn get_debug_mode() -> bool {
    *DEBUG_MODE.get_or_init(|| {
        env::var("DEBUG").unwrap_or_else(|_| "false".to_string()) == "true"
    })
}

// Security headers middleware
fn security_headers_middleware() -> MiddlewareResult<()> {
    let mut response = try_with!(Response::new(""));
    response.headers_mut().set_raw("X-Content-Type-Options", vec![b"nosniff".to_vec()]);
    response.headers_mut().set_raw("X-Frame-Options", vec![b"DENY".to_vec()]);
    response.headers_mut().set_raw("X-XSS-Protection", vec![b"1; mode=block".to_vec()]);
    response.headers_mut().set_raw("Content-Security-Policy", vec![b"default-src 'self'".to_vec()]);
    response.headers_mut().set_raw("Referrer-Policy", vec![b"strict-origin-when-cross-origin".to_vec()]);
    response.headers_mut().set_raw("Cache-Control", vec![b"no-cache, no-store, must-revalidate".to_vec()]);
    Ok(response)
}

// Logging middleware
fn logging_middleware() -> MiddlewareResult<()> {
    if get_debug_mode() {
        eprintln!("[DEBUG] Request received");
    }
    Ok(Response::new(""))
}

// Debug logging for specific endpoints
fn debug_log(endpoint: &str) {
    if get_debug_mode() {
        eprintln!("[DEBUG] {} endpoint accessed", endpoint);
    }
}

fn main() {
    // Get configuration from environment
    let host = env::var("HOST").unwrap_or_else(|_| "0.0.0.0".to_string());
    let port = env::var("PORT").unwrap_or_else(|_| "3000".to_string());
    let addr = format!("{}:{}", host, port);

    // Startup message with configuration summary
    if get_debug_mode() {
        eprintln!("\n=== Nickel Framework Benchmark Server (Development Mode) ===");
        eprintln!("Environment: development");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: true");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Enabled (debug level)");
        eprintln!("Endpoints: /, /user/:id, /user, /health, /error");
        eprintln!("==========================================================\n");
    } else {
        eprintln!("\n=== Nickel Framework Benchmark Server (Production Mode) ===");
        eprintln!("Environment: production");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: false");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Disabled (production mode)");
        eprintln!("==========================================================\n");
    }

    let mut server = Nickel::new();
    
    // Apply security headers middleware to all routes
    server.utilize(security_headers_middleware);
    server.utilize(logging_middleware);
    
    server.get("/", middleware! {
        debug_log("/");
        ""
    });
    
    server.get(
        "/user/:id",
        middleware! {|request|
            debug_log("/user/:id");
            request.param("id").unwrap()
        },
    );
    
    server.post("/user", middleware! {
        debug_log("/user (POST)");
        ""
    });
    
    server.get("/health", middleware! {
        debug_log("/health");
        "OK"
    });
    
    server.get("/error", middleware! {
        if get_debug_mode() {
            eprintln!("[ERROR] Error endpoint accessed");
            "Internal Server Error"
        } else {
            ""
        }
    });

    server.listen(addr).unwrap();
}

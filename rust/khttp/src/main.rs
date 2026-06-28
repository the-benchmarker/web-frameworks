use khttp::{Headers, Method::*, Server};
use std::{env, sync::LazyLock};

// Configuration - Environment-based settings for production vs development
static DEBUG_MODE: LazyLock<bool> = LazyLock::new(|| {
    env::var("DEBUG").unwrap_or_else(|_| "false".to_string()) == "true"
});

// Security headers configuration
static SECURITY_HEADERS: LazyLock<Headers<'static>> = LazyLock::new(|| {
    let mut headers = Headers::new();
    headers.add(Headers::X_CONTENT_TYPE_OPTIONS, b"nosniff");
    headers.add(Headers::X_FRAME_OPTIONS, b"DENY");
    headers.add(Headers::X_XSS_PROTECTION, b"1; mode=block");
    headers.add("Content-Security-Policy", b"default-src 'self'");
    headers.add("Referrer-Policy", b"strict-origin-when-cross-origin");
    headers.add("Cache-Control", b"no-cache, no-store, must-revalidate");
    headers
});

static BASE_HEADERS: LazyLock<Headers<'static>> = LazyLock::new(|| {
    let mut headers = SECURITY_HEADERS.clone();
    headers.add(Headers::CONTENT_TYPE, b"text/plain");
    headers
});

fn debug_log(message: &str) {
    if *DEBUG_MODE {
        eprintln!("[DEBUG] {}", message);
    }
}

fn error_log(message: &str) {
    if *DEBUG_MODE {
        eprintln!("[ERROR] {}", message);
    }
}

fn main() {
    // Get configuration from environment
    let host = env::var("HOST").unwrap_or_else(|_| "0.0.0.0".to_string());
    let port = env::var("PORT").unwrap_or_else(|_| "3000".to_string());
    let addr = format!("{}:{}", host, port);

    // Startup message with configuration summary
    if *DEBUG_MODE {
        eprintln!("\n=== khttp Framework Benchmark Server (Development Mode) ===");
        eprintln!("Environment: development");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: true");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Enabled (debug level)");
        eprintln!("Endpoints: /, /user/:id, /user, /health, /error");
        eprintln!("==========================================================\n");
    } else {
        eprintln!("\n=== khttp Framework Benchmark Server (Production Mode) ===");
        eprintln!("Environment: production");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: false");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Disabled (production mode)");
        eprintln!("==========================================================\n");
    }

    let mut app = Server::builder(addr).unwrap();

    // routes
    app.route(Get, "/", |_, res| {
        debug_log("Root endpoint accessed");
        res.ok0(&BASE_HEADERS)
    });
    
    app.route(Post, "/user", |_, res| {
        debug_log("Create user endpoint accessed");
        res.created0(&BASE_HEADERS)
    });
    
    app.route(Get, "/user/:id", |ctx, res| {
        let id = ctx.params.get("id").unwrap();
        debug_log(&format!("User endpoint accessed with ID: {}", id));
        res.ok(&BASE_HEADERS, id.as_bytes())
    });
    
    // Additional endpoints for production-grade
    app.route(Get, "/health", |_, res| {
        debug_log("Health check endpoint accessed");
        res.ok(&BASE_HEADERS, b"OK")
    });
    
    app.route(Get, "/error", |_, res| {
        error_log("Error endpoint accessed");
        if *DEBUG_MODE {
            res.server_error(&BASE_HEADERS, b"Internal Server Error")
        } else {
            res.server_error0(&BASE_HEADERS)
        }
    });

    // serve
    app.build().serve().unwrap();
}

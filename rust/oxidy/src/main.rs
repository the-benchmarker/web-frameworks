use oxidy::{route, Context, Returns, Server};
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

// Apply security headers to context
fn apply_security_headers(mut c: Context) -> Context {
    for (key, value) in security_headers() {
        c.response.headers.insert(key.to_string(), value.to_string());
    }
    c.response.headers.insert("Content-Type".to_string(), "text/plain".to_string());
    c
}

async fn index(mut c: Context) -> Returns {
    if get_debug_mode() {
        eprintln!("[DEBUG] Root endpoint accessed");
    }
    c = apply_security_headers(c);
    c.response.body = String::new();
    (c, None)
}

async fn user(mut c: Context) -> Returns {
    if get_debug_mode() {
        let id = c.request.param("id").await;
        eprintln!("[DEBUG] User endpoint accessed with ID: {}", id);
    }
    c = apply_security_headers(c);
    c.response.body = c.request.param("id").await;
    (c, None)
}

async fn user_post(mut c: Context) -> Returns {
    if get_debug_mode() {
        eprintln!("[DEBUG] Create user endpoint accessed");
    }
    c = apply_security_headers(c);
    c.response.status_code = 201;
    c.response.body = String::new();
    (c, None)
}

async fn health(mut c: Context) -> Returns {
    if get_debug_mode() {
        eprintln!("[DEBUG] Health check endpoint accessed");
    }
    c = apply_security_headers(c);
    c.response.body = "OK".to_string();
    (c, None)
}

async fn error(mut c: Context) -> Returns {
    if get_debug_mode() {
        eprintln!("[ERROR] Error endpoint accessed");
    }
    c = apply_security_headers(c);
    c.response.status_code = 500;
    if get_debug_mode() {
        c.response.body = "Internal Server Error".to_string();
    } else {
        c.response.body = String::new();
    }
    (c, None)
}

#[tokio::main]
async fn main() {
    // Get configuration from environment
    let host = env::var("HOST").unwrap_or_else(|_| "0.0.0.0".to_string());
    let port = env::var("PORT").unwrap_or_else(|_| "3000".to_string());
    let addr = format!("{}:{}", host, port);

    // Startup message with configuration summary
    if get_debug_mode() {
        eprintln!("\n=== Oxidy Framework Benchmark Server (Development Mode) ===");
        eprintln!("Environment: development");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: true");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Enabled (debug level)");
        eprintln!("Endpoints: /, /user/:id, /user, /health, /error");
        eprintln!("==========================================================\n");
    } else {
        eprintln!("\n=== Oxidy Framework Benchmark Server (Production Mode) ===");
        eprintln!("Environment: production");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: false");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Disabled (production mode)");
        eprintln!("==========================================================\n");
    }

    let mut app = Server::new();
    app.add(route!("get /", index));
    app.add(route!("get /user/:id", user));
    app.add(route!("post /user", user_post));
    app.add(route!("get /health", health));
    app.add(route!("get /error", error));
    app.run(addr).await;
}

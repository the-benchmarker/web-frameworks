use std::{env, io, sync::OnceLock};

use may_minihttp::{HttpService, HttpServiceFactory, Request, Response};

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

// Routes
/*
GET /
GET /user/:id
POST /user
GET /health
GET /error
*/

fn debug_log(message: &str) {
    if get_debug_mode() {
        eprintln!("[DEBUG] {}", message);
    }
}

fn error_log(message: &str) {
    if get_debug_mode() {
        eprintln!("[ERROR] {}", message);
    }
}

#[derive(Clone)]
struct WebFramework;

impl HttpService for WebFramework {
    fn call(&mut self, req: Request, rsp: &mut Response) -> io::Result<()> {
        let method = req.method();
        let path = req.path();

        // Apply security headers to all responses
        for (key, value) in security_headers() {
            rsp.header(&format!("{}: {}", key, value));
        }

        match (method, path) {
            ("GET", "/") => {
                debug_log("Root endpoint accessed");
                rsp.header("Content-Type: text/plain");
                rsp.status_code(200, "OK");
            }
            ("GET", "/health") => {
                debug_log("Health check endpoint accessed");
                rsp.header("Content-Type: text/plain");
                rsp.body_mut().extend_from_slice(b"OK");
                rsp.status_code(200, "OK");
            }
            ("GET", "/error") => {
                error_log("Error endpoint accessed");
                rsp.header("Content-Type: text/plain");
                if get_debug_mode() {
                    rsp.body_mut().extend_from_slice(b"Internal Server Error");
                }
                rsp.status_code(500, "Internal Server Error");
            }
            (method_, path_) if path_.starts_with("/user") => {
                if method_ == "GET" && path_.len() > 6 {
                    let id = path_.split("/").last().unwrap();
                    debug_log(&format!("User endpoint accessed with ID: {}", id));
                    rsp.header("Content-Type: text/plain");
                    rsp.body_mut().extend_from_slice(id.as_bytes());
                    rsp.status_code(200, "OK");
                } else if method_ == "POST" {
                    debug_log("Create user endpoint accessed");
                    rsp.header("Content-Type: text/plain");
                    rsp.status_code(201, "Created");
                } else {
                    if get_debug_mode() {
                        rsp.header("Content-Type: text/plain");
                        rsp.body_mut().extend_from_slice(b"Method Not Allowed");
                    }
                    rsp.status_code(405, "Method Not Allowed");
                }
            }
            _ => {
                if get_debug_mode() {
                    debug_log(&format!("Unknown path accessed: {}", path));
                    rsp.header("Content-Type: text/plain");
                    rsp.body_mut().extend_from_slice(b"Not Found");
                }
                rsp.status_code(404, "Not Found");
            }
        }

        Ok(())
    }
}

struct HttpServer {}

impl HttpServiceFactory for HttpServer {
    type Service = WebFramework;

    fn new_service(&self, _: usize) -> Self::Service {
        WebFramework {}
    }
}

fn main() {
    // Get configuration from environment
    let host = env::var("HOST").unwrap_or_else(|_| "0.0.0.0".to_string());
    let port = env::var("PORT").unwrap_or_else(|_| "3000".to_string());
    let addr = format!("{}:{}", host, port);

    // Startup message with configuration summary
    if get_debug_mode() {
        eprintln!("\n=== may_minihttp Framework Benchmark Server (Development Mode) ===");
        eprintln!("Environment: development");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: true");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Enabled (debug level)");
        eprintln!("Endpoints: /, /user/:id, /user, /health, /error");
        eprintln!("==============================================================\n");
    } else {
        eprintln!("\n=== may_minihttp Framework Benchmark Server (Production Mode) ===");
        eprintln!("Environment: production");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: false");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Disabled (production mode)");
        eprintln!("==============================================================\n");
    }

    let http_server = HttpServer {};
    let server = http_server.start(addr).unwrap();
    server.join().unwrap();
}

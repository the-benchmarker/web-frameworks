use rocket::fairing::{Fairing, Info, Kind};
use rocket::http::{ContentType, Header};
use rocket::{Request, Response};
use std::{env, net::Ipv4Addr, sync::OnceLock};

#[macro_use]
extern crate rocket;

// Configuration - Environment-based settings for production vs development
static DEBUG_MODE: OnceLock<bool> = OnceLock::new();

fn get_debug_mode() -> bool {
    *DEBUG_MODE.get_or_init(|| {
        env::var("DEBUG").unwrap_or_else(|_| "false".to_string()) == "true"
    })
}

// Security headers configuration
struct SecurityHeaders;

#[rocket::async_trait]
impl Fairing for SecurityHeaders {
    fn info(&self) -> Info {
        Info {
            name: "Security Headers",
            kind: Kind::Response
        }
    }

    async fn on_response<'r>(&self, request: &'r Request<'_>, response: &mut Response<'r>) {
        response.set_header(Header::new("X-Content-Type-Options", "nosniff"));
        response.set_header(Header::new("X-Frame-Options", "DENY"));
        response.set_header(Header::new("X-XSS-Protection", "1; mode=block"));
        response.set_header(Header::new("Content-Security-Policy", "default-src 'self'"));
        response.set_header(Header::new("Referrer-Policy", "strict-origin-when-cross-origin"));
        response.set_header(Header::new("Cache-Control", "no-cache, no-store, must-revalidate"));
        response.set_header(ContentType::Plain);
    }
}

#[get("/")]
fn index() -> &'static str {
    if get_debug_mode() {
        eprintln!("[DEBUG] Root endpoint accessed");
    }
    ""
}

#[get("/user/<id>")]
fn get_user(id: &str) -> &str {
    if get_debug_mode() {
        eprintln!("[DEBUG] User endpoint accessed with ID: {}", id);
    }
    id
}

#[post("/user")]
fn post_user() -> &'static str {
    if get_debug_mode() {
        eprintln!("[DEBUG] Create user endpoint accessed");
    }
    ""
}

#[get("/health")]
fn health() -> &'static str {
    if get_debug_mode() {
        eprintln!("[DEBUG] Health check endpoint accessed");
    }
    "OK"
}

#[get("/error")]
fn error() -> String {
    if get_debug_mode() {
        eprintln!("[ERROR] Error endpoint accessed");
    }
    if get_debug_mode() {
        "Internal Server Error".to_string()
    } else {
        String::new()
    }
}

#[launch]
fn rocket() -> _ {
    // Get configuration from environment
    let host = env::var("HOST").unwrap_or_else(|_| "0.0.0.0".to_string());
    let port_str = env::var("PORT").unwrap_or_else(|_| "3000".to_string());
    let port = port_str.parse().unwrap_or(3000);
    
    // Startup message with configuration summary
    if get_debug_mode() {
        eprintln!("\n=== Rocket Framework Benchmark Server (Development Mode) ===");
        eprintln!("Environment: development");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: true");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Enabled (debug level)");
        eprintln!("Endpoints: /, /user/:id, /user, /health, /error");
        eprintln!("==========================================================\n");
    } else {
        eprintln!("\n=== Rocket Framework Benchmark Server (Production Mode) ===");
        eprintln!("Environment: production");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: false");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Disabled (production mode)");
        eprintln!("==========================================================\n");
    }

    let mut config = rocket::config::Config::default();
    config.address = host.parse().unwrap_or_else(|_| Ipv4Addr::new(0, 0, 0, 0).into());
    config.port = port;
    rocket::build()
        .configure(config)
        .attach(SecurityHeaders)
        .mount("/", routes![index, get_user, post_user, health, error])
}

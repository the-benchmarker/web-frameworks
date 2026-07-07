#![allow(clippy::unused_unit)]

use once_cell::sync::Lazy;
use rocket::fairing::{Fairing, Info, Kind};
use rocket::http::{ContentType, Header};
use rocket::{Request, Response};
use std::{env, net::Ipv4Addr};

#[macro_use]
extern crate rocket;

// Configuration - Environment-based settings for production vs development
static DEBUG_MODE: Lazy<bool> = Lazy::new(|| {
    env::var("DEBUG").unwrap_or_else(|_| "false".to_string()) == "true"
});

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

    #[inline]
    async fn on_response<'r>(&self, _request: &'r Request<'_>, response: &mut Response<'r>) {
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
    if *DEBUG_MODE {
        eprintln!("[DEBUG] Root endpoint accessed");
    }
    ""
}

#[get("/user/<id>")]
fn get_user(id: &str) -> &str {
    if *DEBUG_MODE {
        eprintln!("[DEBUG] User endpoint accessed with ID: {}", id);
    }
    id
}

#[post("/user")]
fn post_user() -> &'static str {
    if *DEBUG_MODE {
        eprintln!("[DEBUG] Create user endpoint accessed");
    }
    ""
}

#[get("/health")]
fn health() -> &'static str {
    if *DEBUG_MODE {
        eprintln!("[DEBUG] Health check endpoint accessed");
    }
    "OK"
}

#[get("/error")]
fn error() -> String {
    if *DEBUG_MODE {
        eprintln!("[ERROR] Error endpoint accessed");
    }
    if *DEBUG_MODE { "Internal Server Error" } else { "" }.to_string()
}

#[launch]
fn rocket() -> _ {
    // Get configuration from environment
    let host = env::var("HOST").unwrap_or_else(|_| "0.0.0.0".to_string());
    let port = env::var("PORT").unwrap_or_else(|_| "3000".to_string()).parse().unwrap_or(3000);
    
    // Startup message with configuration summary
    let mode = if *DEBUG_MODE { "Development" } else { "Production" };
    let log_status = if *DEBUG_MODE { "Enabled" } else { "Disabled" };
    
    eprintln!("\n=== Rocket Framework Benchmark Server ({} Mode) ===", mode);
    eprintln!("Environment: {}", if *DEBUG_MODE { "development" } else { "production" });
    eprintln!("Host: {}, Port: {}", host, port);
    eprintln!("Debug: {}, Security headers: Enabled", *DEBUG_MODE);
    eprintln!("Logging: {} ({} level)", log_status, if *DEBUG_MODE { "debug" } else { "warn" });
    eprintln!("Endpoints: /, /user/:id, /user, /health, /error");
    eprintln!("==========================================================\n");

    let mut config = rocket::config::Config::default();
    config.address = host.parse().unwrap_or_else(|_| Ipv4Addr::new(0, 0, 0, 0).into());
    config.port = port;
    rocket::build()
        .configure(config)
        .attach(SecurityHeaders)
        .mount("/", routes![index, get_user, post_user, health, error])
}

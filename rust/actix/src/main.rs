//! Actix-Web Benchmark Server
//!
//! A production-grade benchmark server implementation using Actix-Web framework.
//! Implements security best-practices, proper error handling, and environment-based configuration.

#![allow(clippy::unused_unit)]

use actix_web::{
    error, get, http::header, post, web, App, HttpRequest,
    HttpResponse, HttpServer, Responder,
};
use log::{debug, error, info, LevelFilter};
use once_cell::sync::Lazy;
use std::env;

// Configuration - Environment-based settings for production vs development
static DEBUG_MODE: Lazy<bool> = Lazy::new(|| {
    env::var("DEBUG").unwrap_or_else(|_| "false".to_string()) == "true"
});

// Security headers configuration - pre-allocated for performance
static SECURITY_HEADERS: &[(&'static str, &'static str)] = &[
    ("X-Content-Type-Options", "nosniff"),
    ("X-Frame-Options", "DENY"),
    ("X-XSS-Protection", "1; mode=block"),
    ("Content-Security-Policy", "default-src 'self'"),
    ("Referrer-Policy", "strict-origin-when-cross-origin"),
    ("Cache-Control", "no-cache, no-store, must-revalidate"),
];

/// Apply security headers middleware - optimized with pre-allocated headers
#[inline]
fn apply_security_headers(mut res: HttpResponse) -> HttpResponse {
    for &(key, value) in SECURITY_HEADERS {
        res.insert_header((key, value));
    }
    res
}

/// Root endpoint handler
#[get("/")]
async fn root() -> impl Responder {
    if *DEBUG_MODE {
        debug!("Root endpoint accessed");
    }
    apply_security_headers(HttpResponse::Ok().content_type("text/plain").body(""))
}

/// Get user by ID endpoint
#[get("/user/{id}")]
async fn get_user(id: web::Path<String>) -> impl Responder {
    if *DEBUG_MODE {
        debug!("User endpoint accessed with ID: {}", id);
    }
    apply_security_headers(HttpResponse::Ok().content_type("text/plain").body(id.into_inner()))
}

/// Create user endpoint
#[post("/user")]
async fn create_user() -> impl Responder {
    if *DEBUG_MODE {
        debug!("Create user endpoint accessed");
    }
    apply_security_headers(HttpResponse::Created().content_type("text/plain").body(""))
}

/// Health check endpoint for monitoring
#[get("/health")]
async fn health_check() -> impl Responder {
    if *DEBUG_MODE {
        debug!("Health check endpoint accessed");
    }
    apply_security_headers(HttpResponse::Ok().content_type("text/plain").body("OK"))
}

/// Error test endpoint for verifying error handling
#[get("/error")]
async fn error_test() -> impl Responder {
    if *DEBUG_MODE {
        error!("Error endpoint accessed");
        apply_security_headers(HttpResponse::InternalServerError().content_type("text/plain").body("Internal Server Error"))
    } else {
        apply_security_headers(HttpResponse::InternalServerError().content_type("text/plain").body(""))
    }
}

/// Custom error handler
#[inline]
fn custom_error_handler(err: error::ResponseError, _req: &HttpRequest) -> error::InternalError<&'static str> {
    error!("Unhandled error: {}", err);
    error::InternalError::new("", 500)
}

/// Configure the Actix-Web application
#[inline]
fn configure_app(cfg: &mut web::ServiceConfig) {
    cfg
        .service(root)
        .service(get_user)
        .service(create_user)
        .service(health_check)
        .service(error_test)
        .app_data(web::JsonConfig::default().error_handler(|err, _req| {
            if *DEBUG_MODE {
                error!("JSON error: {}", err);
            }
            apply_security_headers(HttpResponse::BadRequest().content_type("text/plain").body(""))
        }));
}



/// Main application entry point
#[actix_web::main]
async fn main() -> std::io::Result<()> {
    // Get configuration from environment
    let host = env::var("HOST").unwrap_or_else(|_| "0.0.0.0".to_string());
    let port = env::var("PORT").unwrap_or_else(|_| "3000".to_string())
        .parse::<u16>().expect("PORT must be a valid port number");

    // Initialize logging with environment-based level
    env_logger::Builder::new()
        .filter(None, if *DEBUG_MODE { LevelFilter::Debug } else { LevelFilter::Warn })
        .format_timestamp(None)
        .format_target(false)
        .init();

    // Startup message with configuration summary
    let mode = if *DEBUG_MODE { "Development" } else { "Production" };
    let log_status = if *DEBUG_MODE { "Enabled" } else { "Disabled" };
    
    info!("\n=== Actix-Web Framework Benchmark Server ({} Mode) ===", mode);
    info!("Environment: {}", if *DEBUG_MODE { "development" } else { "production" });
    info!("Host: {}, Port: {}", host, port);
    info!("Debug: {}, Security headers: Enabled", *DEBUG_MODE);
    info!("Logging: {} ({} level)", log_status, if *DEBUG_MODE { "debug" } else { "warn" });
    info!("Endpoints: /, /user/:id, /user, /health, /error");
    info!("===============================================================\n");

    info!("Starting Actix-Web benchmark server");

    // Create HTTP server with optimized configuration
    let server = HttpServer::new(|| {
        App::new()
            .configure(configure_app)
            .app_data(web::PayloadConfig::new(16 * 1024 * 1024)) // 16 MB
            .error_handler(custom_error_handler)
            .default_service(web::route().to(|| {
                if *DEBUG_MODE {
                    apply_security_headers(HttpResponse::NotFound().content_type("text/plain").body("Not Found"))
                } else {
                    apply_security_headers(HttpResponse::NotFound().content_type("text/plain").body(""))
                }
            }))
            .shutdown_timeout(60)
    })
    .bind((host, port))?
    .workers(num_cpus::get() * 2)
    .backlog(8192)
    .max_connections(100000)
    .client_timeout(60000)
    .client_disconnect_timeout(5000);

    info!("Server listening on {}:{}", host, port);

    server.run().await?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use actix_web::{test, web, App};

    #[actix_web::test]
    async fn test_root_endpoint() {
        let app = test::init_service(App::new().configure(configure_app)).await;
        let req = test::TestRequest::get().uri("/").to_request();
        let resp = test::call_service(&app, req).await;
        assert!(resp.status().is_success());
    }

    #[actix_web::test]
    async fn test_get_user_endpoint() {
        let app = test::init_service(App::new().configure(configure_app)).await;
        let req = test::TestRequest::get().uri("/user/123").to_request();
        let resp = test::call_service(&app, req).await;
        assert!(resp.status().is_success());
    }

    #[actix_web::test]
    async fn test_create_user_endpoint() {
        let app = test::init_service(App::new().configure(configure_app)).await;
        let req = test::TestRequest::post().uri("/user").to_request();
        let resp = test::call_service(&app, req).await;
        assert!(resp.status().is_success());
    }

    #[actix_web::test]
    async fn test_health_check_endpoint() {
        let app = test::init_service(App::new().configure(configure_app)).await;
        let req = test::TestRequest::get().uri("/health").to_request();
        let resp = test::call_service(&app, req).await;
        assert!(resp.status().is_success());
    }

    #[actix_web::test]
    async fn test_error_endpoint() {
        let app = test::init_service(App::new().configure(configure_app)).await;
        let req = test::TestRequest::get().uri("/error").to_request();
        let resp = test::call_service(&app, req).await;
        assert_eq!(resp.status(), 500);
    }
}

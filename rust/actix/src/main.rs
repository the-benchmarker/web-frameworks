//! Actix-Web Benchmark Server
//!
//! A production-grade benchmark server implementation using Actix-Web framework.
//! Implements security best-practices, proper error handling, and environment-based configuration.

use actix_web::{
    dev::ServiceRequest, error, get, guard, http::header, post, web, App, HttpRequest,
    HttpResponse, HttpServer, Responder,
};
use log::{debug, error, info, LevelFilter};
use serde::Serialize;
use std::env;

// Configuration - Environment-based settings for production vs development
static DEBUG_MODE: once_cell::sync::Lazy<bool> = once_cell::sync::Lazy::new(|| {
    env::var("DEBUG").unwrap_or_else(|_| "false".to_string()) == "true"
});

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

/// User ID path parameter for GET /user/{id} endpoint
#[derive(Debug, Serialize)]
struct UserId(String);

/// Apply security headers middleware
fn apply_security_headers(mut res: HttpResponse) -> HttpResponse {
    for (key, value) in security_headers() {
        res.insert_header((key, value));
    }
    res
}

/// Root endpoint handler
/// 
/// # Returns
/// Empty response for benchmarking
#[get("/")]
async fn root() -> impl Responder {
    if *DEBUG_MODE {
        debug!("Root endpoint accessed");
    }
    apply_security_headers(HttpResponse::Ok().content_type("text/plain").body(""))
}

/// Get user by ID endpoint
/// 
/// # Arguments
/// * `id` - User identifier from path
/// 
/// # Returns
/// User ID as plain text
#[get("/user/{id}")]
async fn get_user(id: web::Path<String>) -> impl Responder {
    let user_id = id.into_inner();
    if *DEBUG_MODE {
        debug!("User endpoint accessed with ID: {}", user_id);
    }
    apply_security_headers(HttpResponse::Ok().content_type("text/plain").body(user_id))
}

/// Create user endpoint
/// 
/// # Returns
/// Empty response for benchmarking
#[post("/user")]
async fn create_user() -> impl Responder {
    if *DEBUG_MODE {
        debug!("Create user endpoint accessed");
    }
    apply_security_headers(HttpResponse::Created().content_type("text/plain").body(""))
}

/// Health check endpoint for monitoring
/// 
/// # Returns
/// Health status
#[get("/health")]
async fn health_check() -> impl Responder {
    if *DEBUG_MODE {
        debug!("Health check endpoint accessed");
    }
    apply_security_headers(HttpResponse::Ok().content_type("text/plain").body("OK"))
}

/// Error test endpoint for verifying error handling
/// 
/// # Returns
/// Error response
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
/// 
/// # Arguments
/// * `err` - The error that occurred
/// * `_req` - The HTTP request
/// 
/// # Returns
/// Error response
fn custom_error_handler(err: actix_web::error::ResponseError, _req: &HttpRequest) -> error::InternalError<&'static str> {
    error!("Unhandled error: {}", err);
    error::InternalError::new("", 500)
}

/// Configure the Actix-Web application
/// 
/// # Returns
/// Configured App instance
fn configure_app(cfg: &mut web::ServiceConfig) {
    cfg
        // Benchmark endpoints
        .service(root)
        .service(get_user)
        .service(create_user)
        // Health check endpoint
        .service(health_check)
        // Error test endpoint
        .service(error_test)
        // Apply default headers
        .app_data(web::JsonConfig::default().error_handler(|err, _req| {
            if *DEBUG_MODE {
                error!("JSON error: {}", err);
            }
            apply_security_headers(HttpResponse::BadRequest().content_type("text/plain").body(""))
        }));
}

/// Custom error handler
/// 
/// # Arguments
/// * `err` - The error that occurred
/// * `_req` - The HTTP request
/// 
/// # Returns
/// Error response
fn custom_error_handler(err: actix_web::error::ResponseError, _req: &HttpRequest) -> error::InternalError<&'static str> {
    if *DEBUG_MODE {
        error!("Unhandled error: {}", err);
    }
    error::InternalError::new("", 500)
}

/// Main application entry point
/// 
/// # Returns
/// Result indicating success or failure
/// 
/// # Panics
/// Panics if server cannot be started
#[actix_web::main]
async fn main() -> std::io::Result<()> {
    // Get configuration from environment
    let host = env::var("HOST").unwrap_or_else(|_| "0.0.0.0".to_string());
    let port_str = env::var("PORT").unwrap_or_else(|_| "3000".to_string());
    let port = port_str.parse::<u16>().expect("PORT must be a valid port number");

    // Initialize logging with environment-based level
    let log_level = if *DEBUG_MODE { LevelFilter::Debug } else { LevelFilter::Warn };
    
    env_logger::Builder::new()
        .filter(None, log_level)
        .format_timestamp(None)
        .format_target(false)
        .init();

    // Startup message with configuration summary
    if *DEBUG_MODE {
        info!("\n=== Actix-Web Framework Benchmark Server (Development Mode) ===");
        info!("Environment: development");
        info!("Host: {}", host);
        info!("Port: {}", port);
        info!("Debug: true");
        info!("Security headers: Enabled");
        info!("Logging: Enabled (debug level)");
        info!("Endpoints: /, /user/:id, /user, /health, /error");
        info!("===============================================================\n");
    } else {
        info!("\n=== Actix-Web Framework Benchmark Server (Production Mode) ===");
        info!("Environment: production");
        info!("Host: {}", host);
        info!("Port: {}", port);
        info!("Debug: false");
        info!("Security headers: Enabled");
        info!("Logging: Disabled (production mode)");
        info!("===============================================================\n");
    }

    info!("Starting Actix-Web benchmark server");

    // Create HTTP server
    let server = HttpServer::new(|| {
        App::new()
            // Configure application
            .configure(configure_app)
            // Set client request content length limit
            .app_data(web::PayloadConfig::new(16 * 1024 * 1024)) // 16 MB
            // Configure response error handler
            .error_handler(custom_error_handler)
            // Configure default service for 404
            .default_service(web::route().to(|| {
                if *DEBUG_MODE {
                    apply_security_headers(HttpResponse::NotFound().content_type("text/plain").body("Not Found"))
                } else {
                    apply_security_headers(HttpResponse::NotFound().content_type("text/plain").body(""))
                }
            }))
            // Configure connection settings
            .shutdown_timeout(60) // 60 seconds graceful shutdown
    })
    .bind((host.as_str(), port))?
    .workers(num_cpus::get() * 2) // Use 2x CPU cores for workers
    .backlog(8192) // Connection backlog
    .max_connections(100000) // Maximum concurrent connections
    .client_timeout(60000) // 60 seconds client timeout
    .client_disconnect_timeout(5000); // 5 seconds disconnect timeout

    info!("Server listening on {}:{}", host, port);

    // Start server
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

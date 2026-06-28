//! Actix-Web Benchmark Server
//!
//! A high-performance benchmark server implementation using Actix-Web framework.
//! Follows Rust best practices including proper error handling, logging, and async/await.

use actix_web::{
    dev::ServiceRequest, error, get, guard, http::header, post, web, App, HttpRequest,
    HttpResponse, HttpServer, Responder,
};
use log::{debug, error, info};
use serde::Serialize;
use std::env;

/// User ID path parameter for GET /user/{id} endpoint
#[derive(Debug, Serialize)]
struct UserId(String);

/// Root endpoint handler
/// 
/// # Returns
/// Empty response for benchmarking
#[get("/")]
async fn root() -> impl Responder {
    debug!("Root endpoint accessed");
    HttpResponse::Ok().content_type("text/plain").body("")
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
    debug!("User endpoint accessed with ID: {}", user_id);
    HttpResponse::Ok().content_type("text/plain").body(user_id)
}

/// Create user endpoint
/// 
/// # Returns
/// Empty response for benchmarking
#[post("/user")]
async fn create_user() -> impl Responder {
    debug!("Create user endpoint accessed");
    HttpResponse::Ok().content_type("text/plain").body("")
}

/// Health check endpoint for monitoring
/// 
/// # Returns
/// Health status
#[get("/health")]
async fn health_check() -> impl Responder {
    HttpResponse::Ok().content_type("text/plain").body("OK")
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
        // Apply default headers
        .app_data(web::JsonConfig::default().error_handler(|err, _req| {
            error!("JSON error: {}", err);
            HttpResponse::BadRequest().content_type("text/plain").body("")
        }));
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
    // Initialize logging
    env_logger::Builder::from_default_env()
        .format_timestamp(None)
        .format_target(false)
        .init();

    info!("Starting Actix-Web benchmark server");

    // Get port from environment or use default
    let port = env::var("PORT").unwrap_or_else(|_| "3000".to_string());
    let port = port.parse::<u16>().expect("PORT must be a valid port number");

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
                HttpResponse::NotFound().content_type("text/plain").body("Not Found")
            }))
            // Configure connection settings
            .shutdown_timeout(60) // 60 seconds graceful shutdown
    })
    .bind(("0.0.0.0", port))?
    .workers(num_cpus::get() * 2) // Use 2x CPU cores for workers
    .backlog(8192) // Connection backlog
    .max_connections(100000) // Maximum concurrent connections
    .client_timeout(60000) // 60 seconds client timeout
    .client_disconnect_timeout(5000); // 5 seconds disconnect timeout

    info!("Server listening on 0.0.0.0:{}", port);

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
}

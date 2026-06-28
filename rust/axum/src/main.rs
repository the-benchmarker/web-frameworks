//! Axum Benchmark Server
//!
//! A production-grade benchmark server implementation using Axum framework.
//! Implements security best-practices, proper error handling, and environment-based configuration.

use axum::{
    extract::Path,
    http::{HeaderMap, HeaderValue, StatusCode},
    response::IntoResponse,
    routing::{get, post},
    Router,
};
use std::{env, net::SocketAddr, time::Duration};
use thiserror::Error;
use tracing::{debug, error, info, Level};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt, EnvFilter};

// Configuration - Environment-based settings for production vs development
static DEBUG_MODE: once_cell::sync::Lazy<bool> = once_cell::sync::Lazy::new(|| {
    env::var("DEBUG").unwrap_or_else(|_| "false".to_string()) == "true"
});

// Security headers configuration
fn security_headers() -> HeaderMap {
    let mut headers = HeaderMap::new();
    headers.insert("X-Content-Type-Options", HeaderValue::from_static("nosniff"));
    headers.insert("X-Frame-Options", HeaderValue::from_static("DENY"));
    headers.insert("X-XSS-Protection", HeaderValue::from_static("1; mode=block"));
    headers.insert("Content-Security-Policy", HeaderValue::from_static("default-src 'self'"));
    headers.insert("Referrer-Policy", HeaderValue::from_static("strict-origin-when-cross-origin"));
    headers.insert("Cache-Control", HeaderValue::from_static("no-cache, no-store, must-revalidate"));
    headers
}

/// Server configuration
#[derive(Debug)]
struct Config {
    host: String,
    port: u16,
    workers: usize,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            host: "0.0.0.0".to_string(),
            port: 3000,
            workers: num_cpus::get() * 2,
        }
    }
}

impl Config {
    fn from_env() -> Self {
        let mut config = Self::default();
        
        if let Ok(port) = env::var("PORT").and_then(|p| p.parse().ok()) {
            config.port = port;
        }
        
        if let Ok(host) = env::var("HOST") {
            config.host = host;
        }
        
        config
    }

    fn socket_addr(&self) -> SocketAddr {
        format!("{}:{}", self.host, self.port).parse().unwrap()
    }
}

/// Custom error type for benchmark server
#[derive(Error, Debug)]
pub enum ServerError {
    #[error("Internal server error")]
    InternalServerError,
    
    #[error("Not found")]
    NotFound,
}

impl IntoResponse for ServerError {
    fn into_response(self) -> axum::response::Response {
        let status = match self {
            ServerError::InternalServerError => StatusCode::INTERNAL_SERVER_ERROR,
            ServerError::NotFound => StatusCode::NOT_FOUND,
        };
        
        error!("{}", self);
        (status, self.to_string()).into_response()
    }
}

/// Apply security headers to response
fn apply_security_headers(mut response: axum::response::Response) -> axum::response::Response {
    for (key, value) in security_headers() {
        response.headers_mut().insert(key, value);
    }
    response
}

/// Root endpoint handler
/// 
/// # Returns
/// Empty response for benchmarking
#[get("/")]
async fn root_handler() -> impl IntoResponse {
    if *DEBUG_MODE {
        debug!("Root endpoint accessed");
    }
    let response = (StatusCode::OK, "").into_response();
    apply_security_headers(response)
}

/// Get user by ID endpoint
/// 
/// # Arguments
/// * `Path(id)` - User identifier from path
/// 
/// # Returns
/// User ID as plain text
#[get("/user/{id}")]
async fn get_user_handler(Path(id): Path<String>) -> impl IntoResponse {
    if *DEBUG_MODE {
        debug!("User endpoint accessed with ID: {}", id);
    }
    let response = (StatusCode::OK, id).into_response();
    apply_security_headers(response)
}

/// Create user endpoint
/// 
/// # Returns
/// Empty response for benchmarking
#[post("/user")]
async fn create_user_handler() -> impl IntoResponse {
    if *DEBUG_MODE {
        debug!("Create user endpoint accessed");
    }
    let response = (StatusCode::CREATED, "").into_response();
    apply_security_headers(response)
}

/// Health check endpoint for monitoring
/// 
/// # Returns
/// Health status
#[get("/health")]
async fn health_check_handler() -> impl IntoResponse {
    if *DEBUG_MODE {
        debug!("Health check endpoint accessed");
    }
    let response = (StatusCode::OK, "OK").into_response();
    apply_security_headers(response)
}

/// Error test endpoint for verifying error handling
/// 
/// # Returns
/// Error response
#[get("/error")]
async fn error_test_handler() -> impl IntoResponse {
    if *DEBUG_MODE {
        error!("Error endpoint accessed");
        let response = (StatusCode::INTERNAL_SERVER_ERROR, "Internal Server Error").into_response();
        apply_security_headers(response)
    } else {
        let response = (StatusCode::INTERNAL_SERVER_ERROR, "").into_response();
        apply_security_headers(response)
    }
}

/// Create the Axum router with all benchmark endpoints
/// 
/// # Returns
/// Configured Router instance
fn create_router() -> Router {
    Router::new()
        // Benchmark endpoints
        .route("/", get(root_handler))
        .route("/user/{id}", get(get_user_handler))
        .route("/user", post(create_user_handler))
        // Health check endpoint
        .route("/health", get(health_check_handler))
        // Error test endpoint
        .route("/error", get(error_test_handler))
        // Fallback for 404
        .fallback(|| async {
            if *DEBUG_MODE {
                error!("Route not found");
            }
            let response = (StatusCode::NOT_FOUND, if *DEBUG_MODE { "Not Found" } else { "" }).into_response();
            apply_security_headers(response)
        })
}

/// Initialize tracing subscriber
/// 
/// # Returns
/// Result indicating success or failure
fn init_tracing() -> Result<(), Box<dyn std::error::Error>> {
    // Configure log level based on environment
    let log_level = if *DEBUG_MODE { Level::DEBUG } else { Level::WARN };
    
    tracing_subscriber::registry()
        .with(EnvFilter::from_default_env().add_directive(log_level.into()))
        .with(tracing_subscriber::fmt::layer().pretty().without_time())
        .init();
    
    Ok(())
}

/// Main application entry point
/// 
/// # Returns
/// Result indicating success or failure
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing
    init_tracing()?;

    // Load configuration
    let config = Config::from_env();

    // Startup message with configuration summary
    if *DEBUG_MODE {
        info!("\n=== Axum Framework Benchmark Server (Development Mode) ===");
        info!("Environment: development");
        info!("Host: {}", config.host);
        info!("Port: {}", config.port);
        info!("Workers: {}", config.workers);
        info!("Debug: true");
        info!("Security headers: Enabled");
        info!("Logging: Enabled (debug level)");
        info!("Endpoints: /, /user/:id, /user, /health, /error");
        info!("========================================================\n");
    } else {
        info!("\n=== Axum Framework Benchmark Server (Production Mode) ===");
        info!("Environment: production");
        info!("Host: {}", config.host);
        info!("Port: {}", config.port);
        info!("Workers: {}", config.workers);
        info!("Debug: false");
        info!("Security headers: Enabled");
        info!("Logging: Disabled (production mode)");
        info!("========================================================\n");
    }
    
    info!("Starting Axum benchmark server");

    info!("Configuration: host={}, port={}, workers={}", 
        config.host, config.port, config.workers);

    // Create router
    let app = create_router();

    // Build listener
    let listener = tokio::net::TcpListener::bind(config.socket_addr()).await?;
    
    info!("Server listening on {}", listener.local_addr()?);

    // Start server
    axum::serve(
        listener,
        app.into_make_service(),
    )
    .with_graceful_shutdown(async {
        // Handle graceful shutdown on SIGINT or SIGTERM
        tokio::signal::ctrl_c().await.expect("Failed to install CTRL+C handler");
        info!("Shutting down gracefully...");
    })
    .await?;

    info!("Server stopped");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use axum::{
        body::Body,
        http::{Request, StatusCode},
    };
    use tower::ServiceExt;

    #[tokio::test]
    async fn test_root_endpoint() {
        let app = create_router();
        let response = app.oneshot(
            Request::builder()
                .uri("/")
                .method("GET")
                .body(Body::empty())
                .unwrap()
        ).await.unwrap();
        
        assert_eq!(response.status(), StatusCode::OK);
    }

    #[tokio::test]
    async fn test_get_user_endpoint() {
        let app = create_router();
        let response = app.oneshot(
            Request::builder()
                .uri("/user/123")
                .method("GET")
                .body(Body::empty())
                .unwrap()
        ).await.unwrap();
        
        assert_eq!(response.status(), StatusCode::OK);
    }

    #[tokio::test]
    async fn test_create_user_endpoint() {
        let app = create_router();
        let response = app.oneshot(
            Request::builder()
                .uri("/user")
                .method("POST")
                .body(Body::empty())
                .unwrap()
        ).await.unwrap();
        
        assert_eq!(response.status(), StatusCode::OK);
    }

    #[tokio::test]
    async fn test_health_check_endpoint() {
        let app = create_router();
        let response = app.oneshot(
            Request::builder()
                .uri("/health")
                .method("GET")
                .body(Body::empty())
                .unwrap()
        ).await.unwrap();
        
        assert_eq!(response.status(), StatusCode::OK);
    }

    #[tokio::test]
    async fn test_not_found_endpoint() {
        let app = create_router();
        let response = app.oneshot(
            Request::builder()
                .uri("/nonexistent")
                .method("GET")
                .body(Body::empty())
                .unwrap()
        ).await.unwrap();
        
        assert_eq!(response.status(), StatusCode::NOT_FOUND);
    }

    #[tokio::test]
    async fn test_error_endpoint() {
        let app = create_router();
        let response = app.oneshot(
            Request::builder()
                .uri("/error")
                .method("GET")
                .body(Body::empty())
                .unwrap()
        ).await.unwrap();
        
        assert_eq!(response.status(), StatusCode::INTERNAL_SERVER_ERROR);
    }
}

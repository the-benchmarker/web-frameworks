//! Axum Benchmark Server
//!
//! A high-performance benchmark server implementation using Axum framework.
//! Follows Rust best practices including proper error handling, logging, and async/await.

use axum::{
    extract::Path,
    http::StatusCode,
    response::IntoResponse,
    routing::{get, post},
    Router,
};
use std::{env, net::SocketAddr, time::Duration};
use thiserror::Error;
use tracing::{debug, error, info, Level};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt, EnvFilter};

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

/// Root endpoint handler
/// 
/// # Returns
/// Empty response for benchmarking
#[get("/")]
async fn root_handler() -> impl IntoResponse {
    debug!("Root endpoint accessed");
    (StatusCode::OK, "")
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
    debug!("User endpoint accessed with ID: {}", id);
    (StatusCode::OK, id)
}

/// Create user endpoint
/// 
/// # Returns
/// Empty response for benchmarking
#[post("/user")]
async fn create_user_handler() -> impl IntoResponse {
    debug!("Create user endpoint accessed");
    (StatusCode::OK, "")
}

/// Health check endpoint for monitoring
/// 
/// # Returns
/// Health status
#[get("/health")]
async fn health_check_handler() -> impl IntoResponse {
    (StatusCode::OK, "OK")
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
        // Fallback for 404
        .fallback(|| async {
            error!("Route not found");
            (StatusCode::NOT_FOUND, "Not Found")
        })
}

/// Initialize tracing subscriber
/// 
/// # Returns
/// Result indicating success or failure
fn init_tracing() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::registry()
        .with(EnvFilter::from_default_env().add_directive(Level::DEBUG.into()))
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
    
    info!("Starting Axum benchmark server");

    // Load configuration
    let config = Config::from_env();
    
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
}

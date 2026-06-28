//! Warp Benchmark Server
//!
//! A high-performance benchmark server implementation using Warp framework.
//! Follows Rust best practices including proper error handling, logging, and async/await.

use std::{convert::Infallible, env, net::SocketAddr};
use thiserror::Error;
use tracing::{debug, error, info, Level};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt, EnvFilter};
use warp::{
    Filter, Rejection, Reply,
};

/// Server configuration
#[derive(Debug, Clone)]
struct Config {
    host: String,
    port: u16,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            host: "0.0.0.0".to_string(),
            port: 3000,
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

impl Reply for ServerError {
    fn into_response(self) -> warp::reply::Response {
        let status = match self {
            ServerError::InternalServerError => warp::http::StatusCode::INTERNAL_SERVER_ERROR,
            ServerError::NotFound => warp::http::StatusCode::NOT_FOUND,
        };
        
        error!("{}", self);
        warp::reply::with_status(self.to_string(), status)
    }
}

/// Convert Rejection to ServerError
async fn handle_rejection(err: Rejection) -> Result<impl Reply, Infallible> {
    error!("Request rejection: {:?}", err);
    Ok(warp::reply::with_status(
        "Not Found",
        warp::http::StatusCode::NOT_FOUND,
    ))
}

/// Root endpoint handler
/// 
/// # Returns
/// Empty response for benchmarking
async fn root_handler() -> Result<impl Reply, Infallible> {
    debug!("Root endpoint accessed");
    Ok("")
}

/// Get user by ID endpoint
/// 
/// # Arguments
/// * `id` - User identifier from path (as u32)
/// 
/// # Returns
/// User ID as plain text
async fn get_user_handler(id: u32) -> Result<impl Reply, Infallible> {
    debug!("User endpoint accessed with ID: {}", id);
    Ok(id.to_string())
}

/// Create user endpoint
/// 
/// # Returns
/// Empty response for benchmarking
async fn create_user_handler() -> Result<impl Reply, Infallible> {
    debug!("Create user endpoint accessed");
    Ok("")
}

/// Health check endpoint for monitoring
/// 
/// # Returns
/// Health status
async fn health_check_handler() -> Result<impl Reply, Infallible> {
    Ok("OK")
}

/// Create the Warp router with all benchmark endpoints
/// 
/// # Returns
/// Warp Filter for all routes
fn create_routes() -> impl Filter<Extract = impl Reply, Error = Rejection> + Clone {
    // Root endpoint
    let index = warp::path::end()
        .and(warp::get())
        .and_then(root_handler);

    // Get user by ID
    let user = warp::path!("user" / u32)
        .and(warp::get())
        .and_then(|id: u32| async move { get_user_handler(id).await });

    // Create user
    let user_post = warp::path("user")
        .and(warp::post())
        .and_then(create_user_handler);

    // Health check
    let health = warp::path("health")
        .and(warp::get())
        .and_then(health_check_handler);

    // Combine all routes
    index.or(user).or(user_post).or(health)
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
    
    info!("Starting Warp benchmark server");

    // Load configuration
    let config = Config::from_env();
    
    info!("Configuration: host={}, port={}", 
        config.host, config.port);

    // Create routes
    let routes = create_routes()
        .recover(handle_rejection);

    // Start server
    info!("Server listening on {}", config.socket_addr());
    
    warp::serve(routes)
        .run(config.socket_addr())
        .await;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use warp::test::request;

    #[tokio::test]
    async fn test_root_endpoint() {
        let routes = create_routes();
        let response = request()
            .path("/")
            .method("GET")
            .reply(&routes)
            .await;
        
        assert_eq!(response.status(), 200);
    }

    #[tokio::test]
    async fn test_get_user_endpoint() {
        let routes = create_routes();
        let response = request()
            .path("/user/123")
            .method("GET")
            .reply(&routes)
            .await;
        
        assert_eq!(response.status(), 200);
    }

    #[tokio::test]
    async fn test_create_user_endpoint() {
        let routes = create_routes();
        let response = request()
            .path("/user")
            .method("POST")
            .reply(&routes)
            .await;
        
        assert_eq!(response.status(), 200);
    }

    #[tokio::test]
    async fn test_health_check_endpoint() {
        let routes = create_routes();
        let response = request()
            .path("/health")
            .method("GET")
            .reply(&routes)
            .await;
        
        assert_eq!(response.status(), 200);
    }

    #[tokio::test]
    async fn test_not_found_endpoint() {
        let routes = create_routes();
        let response = request()
            .path("/nonexistent")
            .method("GET")
            .reply(&routes)
            .await;
        
        assert_eq!(response.status(), 404);
    }
}

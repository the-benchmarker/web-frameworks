//! Axum Benchmark Server
//!
//! A production-grade benchmark server implementation using Axum framework.
//! Implements security best-practices, proper error handling, and environment-based configuration.

use axum::{
    extract::Path,
    http::{header, HeaderMap, HeaderValue, StatusCode},
    response::IntoResponse,
    routing::{get, post},
    Router,
};
use once_cell::sync::Lazy;
use std::{env, net::SocketAddr};
use thiserror::Error;
use tracing::{debug, error, info, Level};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt, EnvFilter};

// Configuration - Environment-based settings for production vs development
static DEBUG_MODE: Lazy<bool> = Lazy::new(|| {
    env::var("DEBUG").unwrap_or_else(|_| "false".to_string()) == "true"
});

// Security headers configuration - pre-parsed for maximum performance
static SECURITY_HEADERS: Lazy<Vec<(axum::http::HeaderName, HeaderValue)>> = Lazy::new(|| {
    vec![
        (header::HeaderName::from_static("x-content-type-options"), HeaderValue::from_static("nosniff")),
        (header::HeaderName::from_static("x-frame-options"), HeaderValue::from_static("DENY")),
        (header::HeaderName::from_static("x-xss-protection"), HeaderValue::from_static("1; mode=block")),
        (header::HeaderName::from_static("content-security-policy"), HeaderValue::from_static("default-src 'self'")),
        (header::HeaderName::from_static("referrer-policy"), HeaderValue::from_static("strict-origin-when-cross-origin")),
        (header::HeaderName::from_static("cache-control"), HeaderValue::from_static("no-cache, no-store, must-revalidate")),
    ]
});

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
#[inline]
fn apply_security_headers(mut response: axum::response::Response) -> axum::response::Response {
    for (name, value) in SECURITY_HEADERS.iter() {
        response.headers_mut().insert(name.clone(), value.clone());
    }
    response
}

/// Root endpoint handler
#[get("/")]
async fn root_handler() -> impl IntoResponse {
    if *DEBUG_MODE {
        debug!("Root endpoint accessed");
    }
    apply_security_headers((StatusCode::OK, "").into_response())
}

/// Get user by ID endpoint
#[get("/user/{id}")]
async fn get_user_handler(Path(id): Path<String>) -> impl IntoResponse {
    if *DEBUG_MODE {
        debug!("User endpoint accessed with ID: {}", id);
    }
    apply_security_headers((StatusCode::OK, id).into_response())
}

/// Create user endpoint
#[post("/user")]
async fn create_user_handler() -> impl IntoResponse {
    if *DEBUG_MODE {
        debug!("Create user endpoint accessed");
    }
    apply_security_headers((StatusCode::CREATED, "").into_response())
}

/// Health check endpoint for monitoring
#[get("/health")]
async fn health_check_handler() -> impl IntoResponse {
    if *DEBUG_MODE {
        debug!("Health check endpoint accessed");
    }
    apply_security_headers((StatusCode::OK, "OK").into_response())
}

/// Error test endpoint for verifying error handling
#[get("/error")]
async fn error_test_handler() -> impl IntoResponse {
    if *DEBUG_MODE {
        error!("Error endpoint accessed");
        apply_security_headers((StatusCode::INTERNAL_SERVER_ERROR, "Internal Server Error").into_response())
    } else {
        apply_security_headers((StatusCode::INTERNAL_SERVER_ERROR, "").into_response())
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
            apply_security_headers((StatusCode::NOT_FOUND, if *DEBUG_MODE { "Not Found" } else { "" }).into_response())
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
    let mode = if *DEBUG_MODE { "Development" } else { "Production" };
    let log_status = if *DEBUG_MODE { "Enabled" } else { "Disabled" };
    
    info!("\n=== Axum Framework Benchmark Server ({} Mode) ===", mode);
    info!("Environment: {}", if *DEBUG_MODE { "development" } else { "production" });
    info!("Host: {}, Port: {}, Workers: {}", config.host, config.port, config.workers);
    info!("Debug: {}, Security headers: Enabled", *DEBUG_MODE);
    info!("Logging: {} ({} level)", log_status, if *DEBUG_MODE { "debug" } else { "warn" });
    info!("Endpoints: /, /user/:id, /user, /health, /error");
    info!("========================================================\n");
    
    info!("Starting Axum benchmark server");

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

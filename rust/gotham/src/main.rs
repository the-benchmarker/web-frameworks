#![warn(rust_2018_idioms)]

use gotham::{
    handler::HandlerError, helpers::http::response::{create_empty_response, create_response}, hyper::StatusCode,
    prelude::*, router::build_simple_router, state::State,
};
use hyper::{HeaderMap, HeaderValue};
use serde::Deserialize;
use std::{env, sync::OnceLock};

// Configuration - Environment-based settings for production vs development
static DEBUG_MODE: OnceLock<bool> = OnceLock::new();

fn get_debug_mode() -> bool {
    *DEBUG_MODE.get_or_init(|| {
        env::var("DEBUG").unwrap_or_else(|_| "false".to_string()) == "true"
    })
}

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

// Create response with security headers
fn create_secure_response(state: &State, status: StatusCode, body: String) -> impl IntoResponse {
    let mut response = create_response(state, status, body);
    for (key, value) in security_headers() {
        response.headers_mut().insert(key, value);
    }
    response.headers_mut().insert("Content-Type", HeaderValue::from_static("text/plain"));
    response
}

// Create empty response with security headers
fn create_secure_empty_response(state: &State, status: StatusCode) -> impl IntoResponse {
    let mut response = create_empty_response(state, status);
    for (key, value) in security_headers() {
        response.headers_mut().insert(key, value);
    }
    response.headers_mut().insert("Content-Type", HeaderValue::from_static("text/plain"));
    response
}

fn main() {
    // Get configuration from environment
    let host = env::var("HOST").unwrap_or_else(|_| "0.0.0.0".to_string());
    let port = env::var("PORT").unwrap_or_else(|_| "3000".to_string());
    let addr = format!("{}:{}", host, port);

    // Startup message with configuration summary
    if get_debug_mode() {
        println!("\n=== Gotham Framework Benchmark Server (Development Mode) ===");
        println!("Environment: development");
        println!("Host: {}", host);
        println!("Port: {}", port);
        println!("Debug: true");
        println!("Security headers: Enabled");
        println!("Logging: Enabled (debug level)");
        println!("Endpoints: /, /user/:id, /user, /health, /error");
        println!("========================================================\n");
    } else {
        println!("\n=== Gotham Framework Benchmark Server (Production Mode) ===");
        println!("Environment: production");
        println!("Host: {}", host);
        println!("Port: {}", port);
        println!("Debug: false");
        println!("Security headers: Enabled");
        println!("Logging: Disabled (production mode)");
        println!("========================================================\n");
    }

    println!("Listening for requests at http://{}", addr);

    let router = build_simple_router(|route| {
        route.get("/").to_async_borrowing(root_handler);
        route.post("/user").to_async_borrowing(create_user_handler);
        route
            .get("/user/:id")
            .with_path_extractor::<PathExtractor>()
            .to_async_borrowing(user_handler);
        route.get("/health").to_async_borrowing(health_handler);
        route.get("/error").to_async_borrowing(error_handler);
    });

    gotham::start(addr, router).expect("Failed to start gotham");
}

async fn root_handler(state: &mut State) -> Result<impl IntoResponse, HandlerError> {
    if get_debug_mode() {
        eprintln!("[DEBUG] Root endpoint accessed");
    }
    Ok(create_secure_empty_response(state, StatusCode::OK))
}

async fn create_user_handler(state: &mut State) -> Result<impl IntoResponse, HandlerError> {
    if get_debug_mode() {
        eprintln!("[DEBUG] Create user endpoint accessed");
    }
    Ok(create_secure_empty_response(state, StatusCode::CREATED))
}

#[derive(Deserialize, StateData, StaticResponseExtender)]
struct PathExtractor {
    id: String,
}

async fn user_handler(state: &mut State) -> Result<impl IntoResponse, HandlerError> {
    let id = PathExtractor::take_from(state).id;
    if get_debug_mode() {
        eprintln!("[DEBUG] User endpoint accessed with ID: {}", id);
    }
    Ok(create_secure_response(state, StatusCode::OK, id))
}

async fn health_handler(state: &mut State) -> Result<impl IntoResponse, HandlerError> {
    if get_debug_mode() {
        eprintln!("[DEBUG] Health check endpoint accessed");
    }
    Ok(create_secure_response(state, StatusCode::OK, "OK".to_string()))
}

async fn error_handler(state: &mut State) -> Result<impl IntoResponse, HandlerError> {
    if get_debug_mode() {
        eprintln!("[ERROR] Error endpoint accessed");
    }
    if get_debug_mode() {
        Ok(create_secure_response(state, StatusCode::INTERNAL_SERVER_ERROR, "Internal Server Error".to_string()))
    } else {
        Ok(create_secure_empty_response(state, StatusCode::INTERNAL_SERVER_ERROR))
    }
}

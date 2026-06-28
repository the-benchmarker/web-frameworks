use salvo::prelude::*;
use salvo::routing::*;
use std::{env, sync::OnceLock};

// Configuration - Environment-based settings for production vs development
static DEBUG_MODE: OnceLock<bool> = OnceLock::new();

fn get_debug_mode() -> bool {
    *DEBUG_MODE.get_or_init(|| {
        env::var("DEBUG").unwrap_or_else(|_| "false".to_string()) == "true"
    })
}

// Security headers middleware
#[handler]
async fn security_headers(req: &mut Request, res: &mut Response) {
    res.headers_mut().insert("X-Content-Type-Options", "nosniff".parse().unwrap());
    res.headers_mut().insert("X-Frame-Options", "DENY".parse().unwrap());
    res.headers_mut().insert("X-XSS-Protection", "1; mode=block".parse().unwrap());
    res.headers_mut().insert("Content-Security-Policy", "default-src 'self'".parse().unwrap());
    res.headers_mut().insert("Referrer-Policy", "strict-origin-when-cross-origin".parse().unwrap());
    res.headers_mut().insert("Cache-Control", "no-cache, no-store, must-revalidate".parse().unwrap());
    res.headers_mut().insert("Content-Type", "text/plain".parse().unwrap());
}

#[handler]
fn index(res: &mut Response) {
    if get_debug_mode() {
        eprintln!("[DEBUG] Root endpoint accessed");
    }
    res.status_code(StatusCode::OK);
}

#[handler]
fn create_user(res: &mut Response) {
    if get_debug_mode() {
        eprintln!("[DEBUG] Create user endpoint accessed");
    }
    res.status_code(StatusCode::CREATED);
}

#[handler]
fn get_user(req: &mut Request, res: &mut Response) {
    if get_debug_mode() {
        eprintln!("[DEBUG] User endpoint accessed with ID: {}", req.params().get("id").unwrap());
    }
    res.status_code(StatusCode::OK);
    res.render(req.params().get("id").unwrap());
}

#[handler]
fn health(res: &mut Response) {
    if get_debug_mode() {
        eprintln!("[DEBUG] Health check endpoint accessed");
    }
    res.status_code(StatusCode::OK);
    res.render("OK");
}

#[handler]
fn error(res: &mut Response) {
    if get_debug_mode() {
        eprintln!("[ERROR] Error endpoint accessed");
    }
    res.status_code(StatusCode::INTERNAL_SERVER_ERROR);
    if get_debug_mode() {
        res.render("Internal Server Error");
    }
}

#[tokio::main]
async fn main() {
    // Get configuration from environment
    let host = env::var("HOST").unwrap_or_else(|_| "0.0.0.0".to_string());
    let port = env::var("PORT").unwrap_or_else(|_| "3000".to_string());
    let addr = format!("{}:{}", host, port);

    // Startup message with configuration summary
    if get_debug_mode() {
        eprintln!("\n=== Salvo Framework Benchmark Server (Development Mode) ===");
        eprintln!("Environment: development");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: true");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Enabled (debug level)");
        eprintln!("Endpoints: /, /user/:id, /user, /health, /error");
        eprintln!("==========================================================\n");
    } else {
        eprintln!("\n=== Salvo Framework Benchmark Server (Production Mode) ===");
        eprintln!("Environment: production");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: false");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Disabled (production mode)");
        eprintln!("==========================================================\n");
    }

    let router = Router::new()
        .hoop(security_headers)
        .get("/", index)
        .push(
            Router::with_path("user")
                .post("/", create_user)
                .push(Router::with_path("{id}").filter(get()).goal(get_user)),
        )
        .get("/health", health)
        .get("/error", error);

    let acceptor = TcpListener::new(addr).bind().await;
    Server::new(acceptor).serve(router).await
}

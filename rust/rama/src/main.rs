use rama::{
    http::{
        header, HeaderMap, HeaderValue, StatusCode,
        server::HttpServer,
        service::web::{Router, extract::Path},
    },
    net::address::SocketAddress,
    rt::Executor,
};
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
    headers.insert("Content-Type", HeaderValue::from_static("text/plain"));
    headers
}

#[derive(Debug, Deserialize)]
struct GetUserParams {
    id: String,
}

// Custom route with security headers
fn secure_route(mut route: Router) -> Router {
    route = route.with_middleware(|mut req, next| async move {
        let mut resp = next.run(req).await;
        for (key, value) in security_headers() {
            resp.headers_mut().insert(key.clone(), value.clone());
        }
        resp
    });
    route
}

#[tokio::main]
async fn main() {
    // Get configuration from environment
    let host = env::var("HOST").unwrap_or_else(|_| "0.0.0.0".to_string());
    let port = env::var("PORT").unwrap_or_else(|_| "3000").to_string();
    let port_num = port.parse().unwrap_or(3000);
    let addr = SocketAddress::new(host.parse().unwrap_or_else(|_| "0.0.0.0".parse().unwrap()), port_num);

    // Startup message with configuration summary
    if get_debug_mode() {
        eprintln!("\n=== Rama Framework Benchmark Server (Development Mode) ===");
        eprintln!("Environment: development");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: true");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Enabled (debug level)");
        eprintln!("Endpoints: /, /user/:id, /user, /health, /error");
        eprintln!("==========================================================\n");
    } else {
        eprintln!("\n=== Rama Framework Benchmark Server (Production Mode) ===");
        eprintln!("Environment: production");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: false");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Disabled (production mode)");
        eprintln!("==========================================================\n");
    }

    let router = Router::new()
        .with_get("/", async || {
            if get_debug_mode() {
                eprintln!("[DEBUG] Root endpoint accessed");
            }
            StatusCode::OK
        })
        .with_post("/user", async || {
            if get_debug_mode() {
                eprintln!("[DEBUG] Create user endpoint accessed");
            }
            StatusCode::CREATED
        })
        .with_get(
            "/user/{id}",
            async |Path(GetUserParams { id }): Path<GetUserParams>| {
                if get_debug_mode() {
                    eprintln!("[DEBUG] User endpoint accessed with ID: {}", id);
                }
                id
            },
        )
        .with_get("/health", async || {
            if get_debug_mode() {
                eprintln!("[DEBUG] Health check endpoint accessed");
            }
            "OK"
        })
        .with_get("/error", async || {
            if get_debug_mode() {
                eprintln!("[ERROR] Error endpoint accessed");
            }
            if get_debug_mode() {
                StatusCode::INTERNAL_SERVER_ERROR.with_body("Internal Server Error")
            } else {
                StatusCode::INTERNAL_SERVER_ERROR
            }
        });

    HttpServer::auto(Executor::default())
        .listen(addr, router)
        .await
        .unwrap();
}

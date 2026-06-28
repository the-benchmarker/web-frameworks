use argan::{handler::HandlerSetter, http::Method, request::RequestHead, Resource, Server};
use hyper::{header, HeaderValue, StatusCode};
use hyper_util::{rt::TokioExecutor, server::conn::auto::Builder};
use std::{env, sync::OnceLock};

// Configuration - Environment-based settings for production vs development
static DEBUG_MODE: OnceLock<bool> = OnceLock::new();

fn get_debug_mode() -> bool {
    *DEBUG_MODE.get_or_init(|| {
        env::var("DEBUG").unwrap_or_else(|_| "false".to_string()) == "true"
    })
}

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

// Custom response type that includes security headers
struct SecureResponse {
    status: StatusCode,
    body: String,
    headers: Vec<(&'static str, &'static str)>,
}

impl SecureResponse {
    fn new(status: StatusCode, body: String) -> Self {
        Self {
            status,
            body,
            headers: security_headers(),
        }
    }
    
    fn with_extra_headers(status: StatusCode, body: String, extra_headers: Vec<(&'static str, &'static str)>) -> Self {
        let mut headers = security_headers();
        headers.extend(extra_headers);
        Self { status, body, headers }
    }
}

impl From<SecureResponse> for hyper::Response<hyper::Body> {
    fn from(response: SecureResponse) -> hyper::Response<hyper::Body> {
        let mut res = hyper::Response::builder()
            .status(response.status);
        
        for (key, value) in response.headers {
            if let Ok(header_value) = HeaderValue::from_str(value) {
                res = res.header(key, header_value);
            }
        }
        
        res.body(hyper::Body::from(response.body))
            .expect("Failed to build response")
    }
}

async fn user_id(request_head: RequestHead) -> String {
    if get_debug_mode() {
        eprintln!("[DEBUG] User endpoint accessed with ID: {}", request_head.path_params_as::<String>().unwrap());
    }
    request_head.path_params_as::<String>().unwrap()
}

async fn root_handler() -> SecureResponse {
    if get_debug_mode() {
        eprintln!("[DEBUG] Root endpoint accessed");
    }
    SecureResponse::new(StatusCode::OK, "".to_string())
}

async fn create_user_handler() -> SecureResponse {
    if get_debug_mode() {
        eprintln!("[DEBUG] Create user endpoint accessed");
    }
    SecureResponse::with_extra_headers(
        StatusCode::CREATED,
        "".to_string(),
        vec![("Content-Type", "text/plain")]
    )
}

async fn health_handler() -> SecureResponse {
    if get_debug_mode() {
        eprintln!("[DEBUG] Health check endpoint accessed");
    }
    SecureResponse::with_extra_headers(
        StatusCode::OK,
        "OK".to_string(),
        vec![("Content-Type", "text/plain")]
    )
}

async fn error_handler() -> SecureResponse {
    if get_debug_mode() {
        eprintln!("[ERROR] Error endpoint accessed");
    }
    if get_debug_mode() {
        SecureResponse::with_extra_headers(
            StatusCode::INTERNAL_SERVER_ERROR,
            "Internal Server Error".to_string(),
            vec![("Content-Type", "text/plain")]
        )
    } else {
        SecureResponse::with_extra_headers(
            StatusCode::INTERNAL_SERVER_ERROR,
            "".to_string(),
            vec![("Content-Type", "text/plain")]
        )
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
        eprintln!("\n=== Argan Framework Benchmark Server (Development Mode) ===");
        eprintln!("Environment: development");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: true");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Enabled (debug level)");
        eprintln!("Endpoints: /, /user/:id, /user, /health, /error");
        eprintln!("========================================================\n");
    } else {
        eprintln!("\n=== Argan Framework Benchmark Server (Production Mode) ===");
        eprintln!("Environment: production");
        eprintln!("Host: {}", host);
        eprintln!("Port: {}", port);
        eprintln!("Debug: false");
        eprintln!("Security headers: Enabled");
        eprintln!("Logging: Disabled (production mode)");
        eprintln!("========================================================\n");
    }

    let mut root = Resource::new("/");
    root.set_handler_for(Method::GET.to(root_handler));

    let user = root.subresource_mut("/user");
    user.set_handler_for(Method::POST.to(create_user_handler));

    user.subresource_mut("/{id}")
        .set_handler_for(Method::GET.to(user_id));

    // Add health and error endpoints
    root.subresource_mut("/health")
        .set_handler_for(Method::GET.to(health_handler));
    
    root.subresource_mut("/error")
        .set_handler_for(Method::GET.to(error_handler));

    let arc_service = root.into_arc_service();
    let connection_builder = Builder::new(TokioExecutor::new());

    Server::new(connection_builder)
        .serve(arc_service, addr)
        .await
        .unwrap();
}

use http::StatusCode;
use micro_web::router::{get, post, Router};
use micro_web::{handler_fn, PathParams, Server};

async fn empty_body() -> &'static str {
    ""
}

async fn echo_uid<'s, 'r>(path_params: &PathParams<'s, 'r>) -> String {
    path_params.get("id").map(|s| s.to_owned()).unwrap()
}

async fn default_handler() -> (&'static str, StatusCode) {
    ("404 not found", StatusCode::NOT_FOUND)
}

#[tokio::main]
async fn main() {
    // Build router with multiple routes and handlers
    let router = Router::builder()
        .route("/", get(handler_fn(empty_body)))
        .route("/user", post(handler_fn(empty_body)))
        // POST route for JSON data with content-type filter
        .route("/user/{id}", get(handler_fn(echo_uid)))
        // Add response encoding wrapper
        .build();

    // Configure and start the server
    Server::builder()
        .router(router)
        .bind("0.0.0.0:3000")
        .default_handler(handler_fn(default_handler))
        .build()
        .unwrap()
        .start()
        .await;
}


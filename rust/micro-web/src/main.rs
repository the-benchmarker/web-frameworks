use micro_web::date::DateServiceDecorator;
use micro_web::responder::Responder;
use micro_web::router::{Router, get, post};
use micro_web::{PathParams, Server, handler_fn, responder};

async fn empty_body() -> &'static str {
    ""
}

async fn echo_uid<'s, 'r>(path_params: &PathParams<'s, 'r>) -> String {
    path_params.get("id").map(|s| s.to_owned()).unwrap()
}

async fn default_handler() -> impl Responder {
    responder::NotFound
}



#[tokio::main]
async fn main() {
    
    // Build router with multiple routes and handlers
    let router = Router::builder()
        .route("/", get(handler_fn(empty_body)))
        .route("/user", post(handler_fn(empty_body)))
        .route("/user/{id}", get(handler_fn(echo_uid)))
        .with_global_decorator(DateServiceDecorator)
        .build();

    // Configure and start the server
    Server::builder().router(router).bind("0.0.0.0:3000").default_handler(handler_fn(default_handler)).build().unwrap().start().await;
}


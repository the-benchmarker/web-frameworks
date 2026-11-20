#![warn(rust_2018_idioms)]

use gotham::{
    handler::HandlerError, helpers::http::response::create_empty_response, hyper::StatusCode,
    prelude::*, router::build_simple_router, state::State,
};
use serde::Deserialize;

fn main() {
    let addr = "0.0.0.0:3000";
    println!("Listening for requests at http://{}", addr);

    let router = build_simple_router(|route| {
        route.get("/").to_async_borrowing(say_ok);
        route.post("/user").to_async_borrowing(say_ok);
        route
            .get("/user/:id")
            .with_path_extractor::<PathExtractor>()
            .to_async_borrowing(log_user);
    });

    gotham::start(addr, router).expect("Failed to start gotham");
}

async fn say_ok(state: &mut State) -> Result<impl IntoResponse, HandlerError> {
    Ok(create_empty_response(state, StatusCode::OK))
}

#[derive(Deserialize, StateData, StaticResponseExtender)]
struct PathExtractor {
    id: String,
}

async fn log_user(state: &mut State) -> Result<impl IntoResponse, HandlerError> {
    Ok(PathExtractor::take_from(state).id)
}

#![warn(rust_2018_idioms)]

use gotham::{
    helpers::http::response,
    hyper::{Body, Response, StatusCode},
    router::builder::*,
    state::*,
};
use gotham_derive::{StateData, StaticResponseExtender};
use serde::Deserialize;

fn main() {
    let addr = "0.0.0.0:3000";
    println!("Listening for requests at http://{}", addr);

    let router = build_simple_router(|route| {
        route.get("/").to(say_ok);
        route.post("/user").to(say_ok);
        route
            .get("/user/:id")
            .with_path_extractor::<PathExtractor>()
            .to(log_user);
    });

    gotham::start(addr, router)
}

fn say_ok(state: State) -> (State, Response<Body>) {
    let res = response::create_empty_response(&state, StatusCode::OK);
    (state, res)
}

#[derive(Deserialize, StateData, StaticResponseExtender)]
struct PathExtractor {
    id: String,
}

fn log_user(mut state: State) -> (State, String) {
    let id = PathExtractor::take_from(&mut state).id;
    (state, id)
}

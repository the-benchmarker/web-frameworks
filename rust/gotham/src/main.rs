extern crate gotham;
#[macro_use]
extern crate gotham_derive;
extern crate hyper;
extern crate serde;
#[macro_use]
extern crate serde_derive;

use gotham::helpers::http::response;
use gotham::router::builder::*;
use gotham::state::*;
use hyper::{Body, Response, StatusCode};

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

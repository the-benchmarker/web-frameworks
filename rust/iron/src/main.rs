#![warn(rust_2018_idioms)]

use iron::{prelude::*, status};
use router::Router;

fn index_handler(_: &mut Request<'_, '_>) -> IronResult<Response> {
    Ok(Response::with(status::Ok))
}

fn user_handler(req: &mut Request<'_, '_>) -> IronResult<Response> {
    let ref id = req
        .extensions
        .get::<Router>()
        .unwrap()
        .find("id")
        .unwrap_or("");
    Ok(Response::with((status::Ok, *id)))
}

fn user_register_handler(_: &mut Request<'_, '_>) -> IronResult<Response> {
    Ok(Response::with(status::Ok))
}

fn main() {
    let mut router = Router::new();
    router.get("/", index_handler, "index");
    router.get("/user/:id", user_handler, "user");
    router.post("/user", user_register_handler, "user_register");

    Iron::new(router).http("0.0.0.0:3000").unwrap();
}

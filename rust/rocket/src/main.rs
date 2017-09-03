#![feature(plugin)]
#![plugin(rocket_codegen)]

extern crate rocket;

use rocket::config::{Config, Environment};

#[get("/")]
fn index_route() {}

#[get("/user/<id>")]
fn user_route(id: String) -> String {
    id
}

#[post("/user")]
fn user_register_route() {}

fn main() {
    let config = Config::build(Environment::Production)
        .address("127.0.0.1")
        .port(3000)
        .finalize()
        .unwrap();
    let app = rocket::custom(config, false);
    app.mount("/", routes![index_route, user_route, user_register_route]).launch();
}

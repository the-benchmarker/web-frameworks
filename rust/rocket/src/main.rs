#![feature(proc_macro_hygiene, decl_macro)]

#[macro_use] extern crate rocket;

use rocket::config::{Config, Environment,LoggingLevel};

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
        .address("0.0.0.0")
        .port(3000)
        .log_level(LoggingLevel::Off)
        .finalize()
        .unwrap();
    let app = rocket::custom(config);
    app
        .mount("/", routes![index_route, user_route, user_register_route])
        .launch();
}

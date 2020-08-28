#[macro_use]
extern crate rocket;

use rocket::{get, launch, post};

#[get("/user/<id>")]
fn user_id(id: String) -> String {
    id
}

#[post("/user")]
fn user<'a>() -> &'a str {
    ""
}

#[get("/")]
fn empty<'a>() -> &'a str {
    ""
}

#[launch]
fn rocket() -> rocket::Rocket {
    rocket::ignite().mount("/", routes![user, user_id, empty])
}

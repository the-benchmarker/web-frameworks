use std::net::Ipv4Addr;

#[macro_use]
extern crate rocket;

#[get("/")]
fn index() -> &'static str {
    ""
}

#[get("/user/<id>")]
fn get_user(id: &str) -> &str {
    id
}

#[post("/user")]
fn post_user() -> &'static str {
    ""
}

#[launch]
fn rocket() -> _ {
    let mut config = rocket::config::Config::default();
    config.address = Ipv4Addr::new(0, 0, 0, 0).into();
    config.port = 3000;
    rocket::build()
        .configure(config)
        .mount("/", routes![index, get_user, post_user])
}

extern crate actix_web;

use actix_web::{web, App, HttpServer, Responder};

fn empty() -> impl Responder {
    println!("")
}

fn show(id: web::Path<u32>) -> impl Responder {
    format!("{}", id)
}

fn main() -> std::io::Result<()> {
    HttpServer::new(|| {
        App::new()
            .service(web::resource("/").to(empty))
            .service(web::resource("/user/{id}").to(show))
            .service(web::resource("/user").to(empty))
    })
    .bind("0.0.0.0:3000")?
    .run()
}

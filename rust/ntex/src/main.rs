use ntex::http::Response;
use ntex::web::{self, App, HttpServer, Responder};

#[web::get("/user/{id}")]
async fn get_user(id: web::types::Path<String>) -> String {
    id.into_inner()
}

#[web::post("/user")]
async fn post_user() -> impl Responder {
    Response::Ok()
}

#[web::get("/")]
async fn index() -> impl Responder {
    Response::Ok()
}

#[ntex::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| {
        App::new()
            .service(get_user)
            .service(post_user)
            .service(index)
    })
    .bind(("0.0.0.0", 3000))?
    .run()
    .await
}

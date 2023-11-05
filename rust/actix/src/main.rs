use actix_web::{get, web, App, HttpServer, Responder, HttpResponse};

#[get("/user/{id}")]
async fn get_user(id: web::Path<String>) -> String {
    id.into_inner()
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| {
        App::new()
            .route("/", web::get().to(|| HttpResponse::Ok()))
            .route("/user", web::post().to(|| HttpResponse::Ok()))
            .service(get_user)
    })
    .bind(("0.0.0.0", 3000))?
    .run()
    .await
}

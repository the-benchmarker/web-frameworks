use actix_web::{get, web, App, HttpResponse, HttpServer, Responder};

#[get("/user/{id}")]
async fn get_user(web::Path(id): web::Path<String>) -> impl Responder {
    id.to_string()
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| {
        App::new()
            .route("/", web::get().to(|| HttpResponse::Ok()))
            .route("/user", web::post().to(|| HttpResponse::Ok()))
            .service(get_user)
    })
    .bind("0.0.0.0:3000")?
    .run()
    .await
}

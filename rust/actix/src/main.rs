use actix_web::{web, App, HttpResponse, HttpServer, Responder};

async fn get_user(id: web::Path<String>) -> impl Responder {
    id.to_string()
}

#[actix_rt::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| {
        App::new()
            .route("/", web::get().to(|| HttpResponse::Ok()))
            .route("/user", web::post().to(|| HttpResponse::Ok()))
            .route("/user/{id}", web::get().to(get_user))
    })
    .bind("127.0.0.1:3000")?
    .run()
    .await
}

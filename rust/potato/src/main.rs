use potato::HttpResponse;

#[potato::http_get("/")]
fn index() -> HttpResponse {
    HttpResponse::empty()
}

#[potato::http_post("/user")]
fn create_user() -> HttpResponse {
    HttpResponse::empty()
}

#[potato::http_get("/user/{id}")]
fn get_user(id: String) -> String {
    id
}

#[tokio::main]
async fn main() {
    potato::ServerConfig::set_ultraspeed(true);
    let port: u16 = std::env::var("PORT")
        .ok()
        .and_then(|p| p.parse().ok())
        .unwrap_or(3000);
    let mut server = potato::HttpServer::new(format!("0.0.0.0:{port}"));
    server
        .configure(|ctx| {
            ctx.use_handlers();
            ctx.set_disable_compress(true);
        })
        .expect("configure server");
    _ = server.serve_http().await;
}

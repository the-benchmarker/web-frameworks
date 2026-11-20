use poem::listener::TcpListener;
use poem::web::Path;
use poem::{get, handler, post, IntoResponse, Route, Server};

#[handler]
async fn index() {}

#[handler]
async fn get_user(Path(id): Path<String>) -> impl IntoResponse {
    id
}

#[handler]
async fn post_user() {}

#[tokio::main]
async fn main() -> std::io::Result<()> {
    let app = Route::new()
        .at("/", get(index))
        .at("/user", post(post_user))
        .at("/user/:id", get(get_user));

    Server::new(TcpListener::bind("0.0.0.0:3000"))
        .run(app)
        .await
}

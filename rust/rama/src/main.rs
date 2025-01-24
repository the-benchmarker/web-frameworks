use rama::extract::Path;
use rama::routing::{get, post};
use rama::Router;

#[tokio::main]
async fn main() {
    let router: Router = Router::new()
        .route("/", get(|| async {}))
        .route("/user", post(|| async {}))
        .route("/user/{id}", get(|Path(id): Path<String>| async move { id }));

    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();
    rama::serve(listener, router.into_make_service())
        .await
        .unwrap();
}

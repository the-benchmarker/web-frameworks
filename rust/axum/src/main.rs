use axum::extract::Path;
use axum::routing::{get, post};
use axum::Router;

#[tokio::main]
async fn main() {
    let router: Router = Router::new();

    let router: Router = router.route("/", get(|| async {}));

    let router: Router = router.route(
        "/user/:id",
        get(|Path(id): Path<u8>| async move { id.to_string() }),
    );

    let router: Router = router.route("/user", post(|| async {}));

    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();
    axum::serve(listener, router.into_make_service())
        .await
        .unwrap();
}

use axum::extract::Path;
use axum::routing::{get, post};
use axum::{Router, Server};

#[tokio::main]
async fn main() {
    let router: Router = Router::new();

    let router: Router = router.route("/", get(|| async {}));

    let router: Router = router.route(
        "/user/:id",
        get(|id: Path<u8>| async move { id.to_string() }),
    );

    let router: Router = router.route("/user", post(|| async {}));

    Server::bind(&"0.0.0.0:3000".parse().unwrap())
        .serve(router.into_make_service())
        .await
        .unwrap();
}

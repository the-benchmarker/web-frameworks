use axum::{extract::Path, routing, Router};

#[tokio::main]
async fn main() {
    let router = Router::new()
        .route("/", routing::get(|| async {}))
        .route(
            "/user/:id",
            routing::get(|id: Path<u64>| async move { id.to_string() }),
        )
        .route("/user", routing::post(|| async {}));

    axum::Server::bind(&"0.0.0.0:3000".parse().unwrap())
        .serve(router.into_make_service())
        .await
        .unwrap();
}

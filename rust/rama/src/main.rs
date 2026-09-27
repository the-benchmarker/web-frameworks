use rama::{
    Layer,
    http::{
        StatusCode,
        layer::error_handling::ErrorHandlerLayer,
        server::HttpServer,
        service::web::{Router, extract::Path},
    },
    layer::ArcLayer,
    net::address::SocketAddress,
    rt::Executor,
};
use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct GetUserParams {
    id: Box<str>,
}

#[tokio::main]
async fn main() {
    HttpServer::auto(Executor::default())
        .listen(
            SocketAddress::default_ipv4(3000),
            (ArcLayer::new(), ErrorHandlerLayer::new()).into_layer(
                Router::new()
                    .with_get("/", StatusCode::OK)
                    .with_post("/user", StatusCode::OK)
                    .with_get(
                        "/user/{id}",
                        async |Path(GetUserParams { id }): Path<GetUserParams>| id,
                    ),
            ),
        )
        .await
        .unwrap();
}

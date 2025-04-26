use rama::{
    http::{
        StatusCode,
        server::HttpServer,
        service::web::{Router, extract::Path},
    },
    net::address::SocketAddress,
    rt::Executor,
};
use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct GetUserParams {
    id: String,
}

#[tokio::main]
async fn main() {
    HttpServer::auto(Executor::default())
        .listen(
            SocketAddress::default_ipv4(3000),
            Router::new()
                .get("/", StatusCode::OK)
                .post("/user", StatusCode::OK)
                .get(
                    "/user/{id}",
                    async |Path(GetUserParams { id }): Path<GetUserParams>| id,
                ),
        )
        .await
        .unwrap();
}

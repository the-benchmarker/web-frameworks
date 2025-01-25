use rama::extract::Path;
use rama::{
    http::{
        matcher::HttpMatcher,
        server::HttpServer,
        service::web::{extract::Path, match_service},
        StatusCode,
    },
    rt::Executor,
};
use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct GetUserParams {
    #[expect(unused)]
    id: String,
}

#[tokio::main]
async fn main() {
    HttpServer::auto(Executor::default())
        .listen(
            "0.0.0.0:3000",
            match_service!{
                HttpMatcher::get("/") => StatusCode::OK,
                HttpMatcher::post("/user") => StatusCode::OK,
                HttpMatcher::get("/user/:id") => |Path(_): Path<GetUserParams>| async move { StatusCode::OK },
                _ => StatusCode::NOT_FOUND,
            }
        )
        .await
        .unwrap();
}

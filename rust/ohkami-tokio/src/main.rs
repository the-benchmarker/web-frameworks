use ohkami::prelude::*;

#[tokio::main]
async fn main() {
    Ohkami::new((
        "/".GET(async || { Response::OK() }),
        "/user".POST(async || { Response::OK() }),
        "/user/:id".GET(async |Path(id): Path<String>| { id }),
    ))
    .howl("0.0.0.0:3000")
    .await
}

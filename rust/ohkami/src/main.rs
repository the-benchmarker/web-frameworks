use ohkami::prelude::*;

#[tokio::main]
async fn main() {
    Ohkami::new((
        "/"
            .GET(|| async {Response::OK()}),
        "/user/:id"
            .GET(|id: String| async {id}),
        "/user"
            .POST(|| async {Response::OK()})
    )).howl("0.0.0.0:3000").await
}

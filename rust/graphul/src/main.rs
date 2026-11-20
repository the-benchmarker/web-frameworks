use graphul::{extract::Path, http::Methods, Graphul};

#[tokio::main]
async fn main() {
    let mut app = Graphul::new();

    app.get("/", || async {});
    app.get("/user/:id", |id: Path<u64>| async move { id.to_string() });
    app.post("/user", || async {});

    app.run("0.0.0.0:3000").await;
}

use tako::extractors::path::Path;
use tako::router::Router;
use tako::{serve_per_thread, PerThreadConfig};

fn main() -> std::io::Result<()> {
    let mut router = Router::new();
    router.get("/", || async {});
    router.post("/user", || async {});
    router.get("/user/{id}", |Path(id): Path<String>| async move { id });

    serve_per_thread("0.0.0.0:3000", router, PerThreadConfig::default())
}

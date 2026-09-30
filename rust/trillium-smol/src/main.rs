use trillium::{Conn, Status};
use trillium_router::{Router, RouterConnExt};
use trillium_smol::config;

fn main() {
    config().with_host("0.0.0.0").with_port(3000).run(
        Router::new()
            .get("/", Status::Ok)
            .get("/user/:id", |conn: Conn| async move {
                let id = conn.param("id").unwrap_or_default().to_string();
                conn.ok(id)
            })
            .post("/user", Status::Ok),
    );
}

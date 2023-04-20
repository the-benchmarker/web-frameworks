#![deny(warnings)]

use std::net::SocketAddr;
use viz::{Error, Request, RequestExt, Result, Router, Server, ServiceMaker};

async fn index(_: Request) -> Result<()> {
    Ok(())
}

async fn show_user(req: Request) -> Result<String> {
    let id = req.param("id")?;
    Ok(id)
}

async fn create_user(_: Request) -> Result<()> {
    Ok(())
}

#[tokio::main]
async fn main() -> Result<()> {
    let app = Router::new()
        .get("/", index)
        .post("/user", create_user)
        .get("/user/:id", show_user);

    Server::bind(&SocketAddr::from(([0, 0, 0, 0], 3000)))
        .serve(ServiceMaker::from(app))
        .await
        .map_err(Error::normal)
}

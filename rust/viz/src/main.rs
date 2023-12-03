#![deny(warnings)]

use std::{net::SocketAddr, sync::Arc};
use tokio::net::TcpListener;
use viz::{serve, Request, RequestExt, Result, Router, Tree};

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
    let addr = SocketAddr::from(([0, 0, 0, 0], 3000));
    let listener = TcpListener::bind(addr).await?;

    let app = Router::new()
        .get("/", index)
        .post("/user", create_user)
        .get("/user/:id", show_user);

    let tree = Arc::new(Tree::from(app));

    loop {
        let (stream, addr) = listener.accept().await?;
        let tree = tree.clone();
        tokio::task::spawn(serve(stream, tree, Some(addr)));
    }
}

#![deny(warnings)]

use std::net::SocketAddr;
use viz::{
    get, post, types::Params, Error, Request, RequestExt, Result, Router, Server, ServiceMaker,
};

async fn index(_: Request) -> Result<()> {
    Ok(())
}

async fn show_user(mut req: Request) -> Result<String> {
    let Params(id) = req.extract::<Params<String>>().await?;
    Ok(id)
}

async fn create_user(_: Request) -> Result<()> {
    Ok(())
}

#[tokio::main]
async fn main() -> Result<()> {
    let app = Router::new()
        .route("/", get(index))
        .route("/user", post(create_user))
        .route("/user/:id", get(show_user));

    Server::bind(&SocketAddr::from(([0, 0, 0, 0], 3000)))
        .serve(ServiceMaker::from(app))
        .await
        .map_err(Error::normal)
}

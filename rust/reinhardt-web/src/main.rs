use std::net::SocketAddr;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let addr: SocketAddr = "0.0.0.0:3000".parse()?;
    reinhardt::server::serve(addr, server::routes()).await
}

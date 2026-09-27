use std::net::SocketAddr;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let address: SocketAddr = "0.0.0.0:3000".parse()?;
    reinhardt::server::serve(address, server::routes()).await
}

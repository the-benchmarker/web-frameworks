mod app;
mod bootstrap;
mod routes;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Instantiate application through bootstrap file
    bootstrap::create_app().await
}

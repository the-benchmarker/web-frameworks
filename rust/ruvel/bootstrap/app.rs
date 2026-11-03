use std::env;

use crate::routes::web;
use ruvel::foundation::Application;

pub async fn create_app() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Initialize Application and register providers (Like bootstrap/app.php)
    let app = Application::configure("")
        // .with_provider(app::providers::RouteServiceProvider)
        .with_routing(web::routes)
        .create();

    // 2. Start server
    app.listen(
        env::var("HOST").unwrap_or_else(|_| "0.0.0.0".to_string()),
        env::var("PORT")
            .unwrap_or_else(|_| "3000".to_string())
            .parse::<u16>()
            .unwrap(),
    )
    .await
}

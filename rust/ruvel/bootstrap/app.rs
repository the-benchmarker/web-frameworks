use crate::app;
use ruvel::foundation::Application;

pub async fn create_app() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Initialize Application and register providers (Like bootstrap/app.php)
    let app = Application::new()
        .register_provider(app::providers::RouteServiceProvider)
        .await;

    // 2. Start server
    app.listen(3000).await
}

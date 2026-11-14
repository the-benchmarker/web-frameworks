use crate::routes::web;
use ruvel::foundation::Application;

pub async fn create_app() -> Application {
    // 1. Initialize Application and register providers (Like bootstrap/app.php)
    Application::configure("")
        // .with_provider(app::providers::RouteServiceProvider)
        .with_routing(|routing| {
            routing.web(web::routes);
        })
        .create()
}

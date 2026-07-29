//! URL configuration for benchmark app (RESTful)

use reinhardt::ServerRouter;

use super::views;

pub fn server_url_patterns() -> ServerRouter {
    ServerRouter::new()
        .endpoint(views::index)
        .endpoint(views::user_detail)
        .endpoint(views::user_create)
}

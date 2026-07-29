use reinhardt::ServerRouter;

use super::views;

pub fn url_patterns() -> ServerRouter {
    ServerRouter::new()
        .endpoint(views::index)
        .endpoint(views::user_detail)
        .endpoint(views::user_create)
}

use reinhardt::routes;
use reinhardt::urls::prelude::UnifiedRouter;

#[routes]
pub fn routes() -> UnifiedRouter {
    UnifiedRouter::new().mount("/", crate::apps::benchmark::urls::url_patterns())
}

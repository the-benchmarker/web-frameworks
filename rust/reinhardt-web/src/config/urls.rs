//! URL configuration for server project (RESTful)
//!
//! The `routes` function defines all URL patterns for this project.

use reinhardt::routes;
use reinhardt::urls::prelude::UnifiedRouter;

#[routes]
pub fn routes() -> UnifiedRouter {
    UnifiedRouter::new().mount("/", crate::apps::benchmark::urls::server_url_patterns())
}

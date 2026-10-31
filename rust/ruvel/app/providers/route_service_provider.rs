use crate::routes;
use async_trait::async_trait;
use ruvel::{container::Container, routing::Router, support::ServiceProvider};

pub struct RouteServiceProvider;

#[async_trait]
impl ServiceProvider for RouteServiceProvider {
    async fn register(&self, _container: &mut Container) {
        // Register route-specific configurations if needed
    }

    async fn boot(&self, _container: &Container, router: &mut Router) {
        // Map the web and API routes onto the global Router
        routes::web::routes(router);
    }
}

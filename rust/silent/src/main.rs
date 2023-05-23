use std::sync::Arc;
use async_trait::async_trait;
use silent::prelude::*;

struct Index;

#[async_trait]
impl Handler for Index {
    async fn call(&self, _req: Request) -> Result<Response> {
        Ok(Response::empty())
    }
}

fn main() {
    let mut user_route = Route::new("user").append(
        Route::new("<id>").get(
            |req| async move {
                req.get_path_params::<String>("id")
            }
        ),
    );
    user_route.get_handler_mut().insert(
        Method::POST,
        Arc::new(Index),
    );
    let mut route = Route::new("")
        .append(
            user_route
        );
    route.get_handler_mut().insert(
        Method::GET,
        Arc::new(Index),
    );
    Server::new().bind("0.0.0.0:3000".parse().unwrap()).bind_route(route).run();
}

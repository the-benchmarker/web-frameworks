use ruvel::http::{Controller, Request, Responsable};

#[derive(Default)]
pub struct WelcomeController;

impl Controller for WelcomeController {
    async fn index(&self, _request: Request) -> impl Responsable {
        "Welcome to ruvel"
    }
}

impl WelcomeController {
    pub async fn welcome(&self, _request: Request) -> &'static str {
        "Welcome to Ruvel Again"
    }
}

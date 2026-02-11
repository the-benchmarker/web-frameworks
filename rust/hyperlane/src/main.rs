use hyperlane::*;

pub(crate) async fn init_server_config() -> ServerConfig {
    let server_config: ServerConfig = ServerConfig::new().await;
    server_config.port(3000).await;
    server_config.disable_nodelay().await;
    server_config
}

pub(crate) async fn init_request_config() -> RequestConfig {
    RequestConfig::low_security().await
}

#[derive(Clone, Copy)]
struct RequestMiddleware;

impl ServerHook for RequestMiddleware {
    async fn new(_: &Context) -> Self {
        Self
    }

    async fn handle(self, ctx: &Context) {
        ctx.set_response_version(HttpVersion::Http1_1)
            .await
            .set_response_header(CONNECTION, KEEP_ALIVE)
            .await
            .set_response_header(CONTENT_TYPE, TEXT_PLAIN)
            .await;
    }
}

#[derive(Clone, Copy)]
struct ResponseMiddleware;

impl ServerHook for ResponseMiddleware {
    async fn new(_: &Context) -> Self {
        Self
    }

    async fn handle(self, ctx: &Context) {
        ctx.send().await;
    }
}

#[derive(Clone, Copy)]
struct Index;

impl ServerHook for Index {
    async fn new(_: &Context) -> Self {
        Self
    }

    async fn handle(self, ctx: &Context) {
        if ctx.get_request_method().await.is_get() {
            ctx.set_response_status_code(200).await;
        } else {
            ctx.set_response_status_code(404).await;
        }
        ctx.send().await;
    }
}

#[derive(Clone, Copy)]
struct User;

impl ServerHook for User {
    async fn new(_: &Context) -> Self {
        Self
    }

    async fn handle(self, ctx: &Context) {
        if ctx.get_request_method().await.is_post() {
            ctx.set_response_status_code(200).await;
        } else {
            ctx.set_response_status_code(404).await;
        }
        ctx.send().await;
    }
}

#[derive(Clone, Copy)]
struct UserId;

impl ServerHook for UserId {
    async fn new(_: &Context) -> Self {
        Self
    }

    async fn handle(self, ctx: &Context) {
        if ctx.get_request_method().await.is_get() {
            let id: String = ctx.try_get_route_param("id").await.unwrap_or_default();
            ctx.set_response_status_code(200)
                .await
                .set_response_body(id)
                .await;
        } else {
            ctx.set_response_status_code(404).await;
        }
    }
}

#[tokio::main]
async fn main() {
    let server_config: ServerConfig = init_server_config().await;
    let request_config: RequestConfig = init_request_config().await;

    let server: Server = Server::new().await;
    server.server_config(server_config).await;
    server.request_config(request_config).await;
    server.request_middleware::<RequestMiddleware>().await;
    server.response_middleware::<ResponseMiddleware>().await;
    server.route::<Index>("/").await;
    server.route::<User>("/user").await;
    server.route::<UserId>("/user/{id}").await;

    let server_hook: ServerControlHook = server.run().await.unwrap();
    server_hook.wait().await;
}

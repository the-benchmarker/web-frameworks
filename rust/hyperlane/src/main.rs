use hyperlane::*;

async fn init_server_config() -> ServerConfig {
    let server_config: ServerConfig = ServerConfig::new().await;
    server_config.port(3000).await.disable_nodelay().await;
    server_config
}

async fn init_request_config() -> RequestConfig {
    RequestConfig::low_security().await
}

#[derive(Clone, Copy, Default)]
struct Index;

impl ServerHook for Index {
    async fn new(_: &Context) -> Self {
        Self
    }

    async fn handle(self, ctx: &Context) {
        ctx.set_response_header(CONNECTION, KEEP_ALIVE).await;
        let run = || async {
            if ctx.get_request_method().await.is_get() {
                ctx.set_response_status_code(200).await;
            } else {
                ctx.set_response_status_code(404).await;
            }
            ctx.try_send().await.is_err()
        };
        if run().await {
            ctx.closed().await;
            return;
        }
        while ctx.http_from_stream().await.is_ok() {
            if run().await {
                ctx.closed().await;
                break;
            }
        }
        ctx.closed().await;
    }
}

#[derive(Clone, Copy, Default)]
struct User;

impl ServerHook for User {
    async fn new(_: &Context) -> Self {
        Self
    }

    async fn handle(self, ctx: &Context) {
        ctx.set_response_header(CONNECTION, KEEP_ALIVE).await;
        let run = || async {
            if ctx.get_request_method().await.is_post() {
                ctx.set_response_status_code(200).await;
            } else {
                ctx.set_response_status_code(404).await;
            }
            ctx.try_send().await.is_err()
        };
        if run().await {
            ctx.closed().await;
            return;
        }
        while ctx.http_from_stream().await.is_ok() {
            if run().await {
                ctx.closed().await;
                break;
            }
        }
        ctx.closed().await;
    }
}

#[derive(Clone, Copy, Default)]
struct UserId;

impl ServerHook for UserId {
    async fn new(_: &Context) -> Self {
        Self
    }

    async fn handle(self, ctx: &Context) {
        ctx.set_response_header(CONNECTION, KEEP_ALIVE).await;
        let run = || async {
            if ctx.get_request_method().await.is_get() {
                let id: String = ctx.try_get_route_param("id").await.unwrap_or_default();
                ctx.set_response_status_code(200)
                    .await
                    .set_response_body(id)
                    .await;
            } else {
                ctx.set_response_status_code(404).await;
            };
            ctx.try_send().await.is_err()
        };
        if run().await {
            ctx.closed().await;
            return;
        }
        while ctx.http_from_stream().await.is_ok() {
            if run().await {
                ctx.closed().await;
                break;
            }
        }
        ctx.closed().await;
    }
}

#[tokio::main]
async fn main() {
    Server::new()
        .await
        .server_config(init_server_config().await)
        .await
        .request_config(init_request_config().await)
        .await
        .route::<Index>("/")
        .await
        .route::<User>("/user")
        .await
        .route::<UserId>("/user/{id}")
        .await
        .run()
        .await
        .unwrap()
        .wait()
        .await;
}

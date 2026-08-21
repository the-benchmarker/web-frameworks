use hyperlane::*;

fn init_server_config() -> ServerConfig {
    let mut server_config: ServerConfig = ServerConfig::default();
    server_config.set_address(Server::format_bind_address(DEFAULT_HOST, 3000));
    server_config
}

struct Index;

impl ServerHook for Index {
    async fn new(_: &mut Stream, _: &mut Context) -> Self {
        Self
    }

    async fn handle(self, stream: &mut Stream, ctx: &mut Context) -> Status {
        ctx.get_mut_response().set_status_code(200);
        let data: Vec<u8> = ctx.get_mut_response().build();
        let _ = stream.try_send(data).await;
        Status::Continue
    }
}

struct User;

impl ServerHook for User {
    async fn new(_: &mut Stream, _: &mut Context) -> Self {
        Self
    }

    async fn handle(self, stream: &mut Stream, ctx: &mut Context) -> Status {
        ctx.get_mut_response().set_status_code(200);
        let data: Vec<u8> = ctx.get_mut_response().build();
        let _ = stream.try_send(data).await;
        Status::Continue
    }
}

struct UserId;

impl ServerHook for UserId {
    async fn new(_: &mut Stream, _: &mut Context) -> Self {
        Self
    }

    async fn handle(self, stream: &mut Stream, ctx: &mut Context) -> Status {
        let id: String = ctx.try_get_route_param("id").unwrap_or_default();
        ctx.get_mut_response().set_status_code(200).set_body(id);
        let data: Vec<u8> = ctx.get_mut_response().build();
        let _ = stream.try_send(data).await;
        Status::Continue
    }
}

#[tokio::main]
async fn main() {
    Server::default()
        .server_config(init_server_config())
        .route::<Index>("/")
        .route::<User>("/user")
        .route::<UserId>("/user/{id}")
        .run()
        .await
        .unwrap()
        .wait()
        .await;
}

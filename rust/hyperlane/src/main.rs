use hyperlane::*;

/// Builds the server configuration.
///
/// # Returns
///
/// - `ServerConfig` - The configuration for the benchmark server.
fn init_server_config() -> ServerConfig {
    let mut server_config: ServerConfig = ServerConfig::default();
    server_config.set_address(Server::format_bind_address(DEFAULT_HOST, 3000));
    server_config
}

/// Builds the request configuration.
///
/// # Returns
///
/// - `RequestConfig` - The request parser settings.
fn init_request_config() -> RequestConfig {
    RequestConfig::low_security()
}

#[route("/")]
struct Index;

impl ServerHook for Index {
    /// Creates the route handler.
    ///
    /// # Arguments
    ///
    /// - `&mut Stream` - The current connection.
    /// - `&mut Context` - The current request context.
    ///
    /// # Returns
    ///
    /// - `Self` - The route handler.
    async fn new(_: &mut Stream, _: &mut Context) -> Self {
        Self
    }

    /// Sends the route response.
    ///
    /// # Arguments
    ///
    /// - `Self` - The route handler.
    /// - `&mut Stream` - The current connection.
    /// - `&mut Context` - The current request context.
    ///
    /// # Returns
    ///
    /// - `Status` - Whether the request pipeline continues.
    async fn handle(self, stream: &mut Stream, ctx: &mut Context) -> Status {
        ctx.get_mut_response().set_header(CONNECTION, KEEP_ALIVE);
        if ctx.get_request().get_method().is_get() {
            ctx.get_mut_response().set_status_code(200);
        } else {
            ctx.get_mut_response().set_status_code(404);
        }
        let data: Vec<u8> = ctx.get_mut_response().build();
        if stream.try_send(data).await.is_err() {
            stream.set_closed(true);
            return Status::Reject;
        }
        Status::Continue
    }
}

#[route("/user")]
struct User;

impl ServerHook for User {
    /// Creates the route handler.
    ///
    /// # Arguments
    ///
    /// - `&mut Stream` - The current connection.
    /// - `&mut Context` - The current request context.
    ///
    /// # Returns
    ///
    /// - `Self` - The route handler.
    async fn new(_: &mut Stream, _: &mut Context) -> Self {
        Self
    }

    /// Sends the route response.
    ///
    /// # Arguments
    ///
    /// - `Self` - The route handler.
    /// - `&mut Stream` - The current connection.
    /// - `&mut Context` - The current request context.
    ///
    /// # Returns
    ///
    /// - `Status` - Whether the request pipeline continues.
    async fn handle(self, stream: &mut Stream, ctx: &mut Context) -> Status {
        ctx.get_mut_response().set_header(CONNECTION, KEEP_ALIVE);
        if ctx.get_request().get_method().is_post() {
            ctx.get_mut_response().set_status_code(200);
        } else {
            ctx.get_mut_response().set_status_code(404);
        }
        let data: Vec<u8> = ctx.get_mut_response().build();
        if stream.try_send(data).await.is_err() {
            stream.set_closed(true);
            return Status::Reject;
        }
        Status::Continue
    }
}

#[route("/user/{id}")]
struct UserId;

impl ServerHook for UserId {
    /// Creates the route handler.
    ///
    /// # Arguments
    ///
    /// - `&mut Stream` - The current connection.
    /// - `&mut Context` - The current request context.
    ///
    /// # Returns
    ///
    /// - `Self` - The route handler.
    async fn new(_: &mut Stream, _: &mut Context) -> Self {
        Self
    }

    /// Sends the route response.
    ///
    /// # Arguments
    ///
    /// - `Self` - The route handler.
    /// - `&mut Stream` - The current connection.
    /// - `&mut Context` - The current request context.
    ///
    /// # Returns
    ///
    /// - `Status` - Whether the request pipeline continues.
    async fn handle(self, stream: &mut Stream, ctx: &mut Context) -> Status {
        ctx.get_mut_response().set_header(CONNECTION, KEEP_ALIVE);
        if ctx.get_request().get_method().is_get() {
            let id: &str = ctx.route_params.get("id").map_or("", String::as_str);
            let response: &mut Response = &mut ctx.response;
            response.set_status_code(200);
            let body: &mut ResponseBody = response.get_mut_body();
            body.clear();
            body.extend_from_slice(id.as_bytes());
        } else {
            ctx.get_mut_response().set_status_code(404);
        }
        let data: Vec<u8> = ctx.get_mut_response().build();
        if stream.try_send(data).await.is_err() {
            stream.set_closed(true);
            return Status::Reject;
        }
        Status::Continue
    }
}

/// Starts the benchmark server.
#[hyperlane(server: Server)]
#[tokio::main]
async fn main() {
    server
        .server_config(init_server_config())
        .request_config(init_request_config())
        .run()
        .await
        .unwrap()
        .wait()
        .await;
}

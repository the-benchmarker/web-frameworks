use hyperlane::*;

/// Builds the server configuration.
///
/// `TCP_NODELAY` is enabled: the responses are small and are written one
/// after the other, so the Nagle algorithm would delay the second write for
/// no gain.
///
/// # Returns
///
/// - `ServerConfig` - The configuration of the server.
fn init_server_config() -> ServerConfig {
    let mut server_config: ServerConfig = ServerConfig::default();
    server_config.set_address(Server::format_bind_address(DEFAULT_HOST, 3000));
    server_config.set_nodelay(Some(true));
    server_config
}

/// Builds the request configuration.
///
/// # Returns
///
/// - `RequestConfig` - The configuration of the request parser.
fn init_request_config() -> RequestConfig {
    RequestConfig::low_security()
}

/// The route that answers the index path.
#[route("/")]
struct Index;

/// Implements [`ServerHook`] for [`Index`].
impl ServerHook for Index {
    /// Builds the route.
    ///
    /// # Arguments
    ///
    /// - `&mut Stream` - The stream the request arrived on.
    /// - `&mut Context` - The context of the request.
    ///
    /// # Returns
    ///
    /// - `Self` - The route.
    async fn new(_: &mut Stream, _: &mut Context) -> Self {
        Self
    }

    /// Answers the request.
    ///
    /// The response is only filled here; the bytes leave the process in the
    /// response middleware, so every route pays for one send and not one
    /// send per handler.
    ///
    /// # Arguments
    ///
    /// - `Self` - The route.
    /// - `&mut Stream` - The stream the request arrived on.
    /// - `&mut Context` - The context of the request.
    ///
    /// # Returns
    ///
    /// - `Status` - Whether the pipeline continues.
    async fn handle(self, _: &mut Stream, ctx: &mut Context) -> Status {
        if ctx.get_request().get_method().is_get() {
            ctx.get_mut_response().set_status_code(200);
        } else {
            ctx.get_mut_response().set_status_code(404);
        }
        Status::Continue
    }
}

/// The route that answers the user path.
#[route("/user")]
struct User;

/// Implements [`ServerHook`] for [`User`].
impl ServerHook for User {
    /// Builds the route.
    ///
    /// # Arguments
    ///
    /// - `&mut Stream` - The stream the request arrived on.
    /// - `&mut Context` - The context of the request.
    ///
    /// # Returns
    ///
    /// - `Self` - The route.
    async fn new(_: &mut Stream, _: &mut Context) -> Self {
        Self
    }

    /// Answers the request.
    ///
    /// # Arguments
    ///
    /// - `Self` - The route.
    /// - `&mut Stream` - The stream the request arrived on.
    /// - `&mut Context` - The context of the request.
    ///
    /// # Returns
    ///
    /// - `Status` - Whether the pipeline continues.
    async fn handle(self, _: &mut Stream, ctx: &mut Context) -> Status {
        if ctx.get_request().get_method().is_post() {
            ctx.get_mut_response().set_status_code(200);
        } else {
            ctx.get_mut_response().set_status_code(404);
        }
        Status::Continue
    }
}

/// The route that answers the user path with a parameter.
#[route("/user/{id}")]
struct UserId;

/// Implements [`ServerHook`] for [`UserId`].
impl ServerHook for UserId {
    /// Builds the route.
    ///
    /// # Arguments
    ///
    /// - `&mut Stream` - The stream the request arrived on.
    /// - `&mut Context` - The context of the request.
    ///
    /// # Returns
    ///
    /// - `Self` - The route.
    async fn new(_: &mut Stream, _: &mut Context) -> Self {
        Self
    }

    /// Answers the request.
    ///
    /// # Arguments
    ///
    /// - `Self` - The route.
    /// - `&mut Stream` - The stream the request arrived on.
    /// - `&mut Context` - The context of the request.
    ///
    /// # Returns
    ///
    /// - `Status` - Whether the pipeline continues.
    async fn handle(self, _: &mut Stream, ctx: &mut Context) -> Status {
        if ctx.get_request().get_method().is_get() {
            let id: String = ctx.get_route_param("id");
            ctx.get_mut_response().set_status_code(200).set_body(id);
        } else {
            ctx.get_mut_response().set_status_code(404);
        }
        Status::Continue
    }
}

/// The response middleware that writes the response onto the stream.
///
/// Every route fills the response and leaves the write to this middleware, so
/// the response is built and sent exactly once per request.
#[response_middleware]
struct Send;

/// Implements [`ServerHook`] for [`Send`].
impl ServerHook for Send {
    /// Builds the middleware.
    ///
    /// # Arguments
    ///
    /// - `&mut Stream` - The stream the request arrived on.
    /// - `&mut Context` - The context of the request.
    ///
    /// # Returns
    ///
    /// - `Self` - The middleware.
    async fn new(_: &mut Stream, _: &mut Context) -> Self {
        Self
    }

    /// Writes the response onto the stream.
    ///
    /// # Arguments
    ///
    /// - `Self` - The middleware.
    /// - `&mut Stream` - The stream the request arrived on.
    /// - `&mut Context` - The context of the request.
    ///
    /// # Returns
    ///
    /// - `Status` - Whether the pipeline continues.
    async fn handle(self, stream: &mut Stream, ctx: &mut Context) -> Status {
        let data: Vec<u8> = ctx.get_mut_response().build();
        if stream.try_send(data).await.is_err() {
            stream.set_closed(true);
            return Status::Reject;
        }
        Status::Continue
    }
}

/// Starts the server and serves until it is shut down.
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

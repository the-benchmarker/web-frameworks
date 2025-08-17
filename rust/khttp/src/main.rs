use khttp::{Headers, Method::*, Server};
use std::sync::LazyLock;

static BASE_HEADERS: LazyLock<Headers<'static>> = LazyLock::new(|| {
    let mut headers = Headers::new();
    headers.add(Headers::CONTENT_TYPE, b"text/plain");
    headers
});

fn main() {
    let mut app = Server::builder("0.0.0.0:3000").unwrap();

    // routes
    app.route(Get, "/", |_, res| res.ok0(&BASE_HEADERS));
    app.route(Post, "/user", |_, res| res.ok0(&BASE_HEADERS));
    app.route(Get, "/user/:id", |ctx, res| {
        let id = ctx.params.get("id").unwrap();
        res.ok(&BASE_HEADERS, id.as_bytes())
    });

    // serve
    app.build().serve_epoll().unwrap();
}

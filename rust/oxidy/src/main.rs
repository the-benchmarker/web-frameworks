use oxidy::{route, Context, Returns, Server};

async fn index(mut c: Context) -> Returns {
    c.response.body = String::new();
    (c, None)
}

async fn user(mut c: Context) -> Returns {
    c.response.body = c.request.param("id").await;
    (c, None)
}

async fn user_post(mut c: Context) -> Returns {
    c.response.body = String::new();
    (c, None)
}

#[tokio::main]
async fn main() {
    let mut app = Server::new();
    app.add(route!("get /", index));
    app.add(route!("get /user/:id", user));
    app.add(route!("post /user", user_post));
    app.run("0.0.0.0:3000").await;
}
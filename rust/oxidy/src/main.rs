use oxidy::server::Server;
use oxidy::structs::Context;

fn index(ctx: &mut Context) -> () {
    ctx.response.body = "".to_string();
}

fn user(ctx: &mut Context) -> () {
    if ctx.request.header.get("method").unwrap().to_string() == "GET" {
        ctx.response.body = ctx.request.params.get("id").unwrap().to_string();
    } else {
        ctx.response.body = "".to_string();
    }
}

fn main() {
    let mut app = Server::new();
    app.get("/", index);
    app.get("/user/:id", user);
    app.post("/user", user);
    app.listen("0.0.0.0:3000");
}

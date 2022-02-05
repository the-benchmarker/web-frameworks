use oxidy::server::Server;
use oxidy::structs::Context;

fn index(ctx: &mut Context) -> () {
    ctx.response.body = "".to_string();
}

fn user(ctx: &mut Context) -> () {
    ctx.response.body = ctx.request.params.get("id").unwrap().to_string();
}

fn user_post(ctx: &mut Context) -> () {
    ctx.response.body = "".to_string();
}

fn main() {
    let mut app = Server::new();
    app.get("/", index);
    app.get("/user/:id", user);
    app.post("/user", user_post);
    app.listen("0.0.0.0:3000");
}

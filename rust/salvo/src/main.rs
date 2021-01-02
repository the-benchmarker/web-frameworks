use salvo::prelude::*;
use salvo::http::StatusCode;

#[fn_handler]
async fn index(_conf: Arc<ServerConfig>, _req: &mut Request, _depot: &mut Depot, res: &mut Response) {
    res.set_status_code(StatusCode::OK);
}
#[fn_handler]
async fn get_user(req: &mut Request, _depot: &mut Depot, res: &mut Response) {
    res.render_plain_text(req.params().get("id").map(|s|&**s).unwrap_or_default());
}
#[fn_handler]
async fn create_user(_req: &mut Request, _depot: &mut Depot, res: &mut Response) {
    res.set_status_code(StatusCode::OK);
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let mut router = Router::new("/");
    router.get(index);
    router.scope("user/<id>").get(get_user);
    router.scope("user>").post(create_user);
    let server = Server::with_addr(router, "0.0.0.0:3000");
    server.serve().await?;
    Ok(())
}
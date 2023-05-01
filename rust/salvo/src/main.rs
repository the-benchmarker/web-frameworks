use salvo::prelude::*;
use salvo::routing::*;

#[handler]
fn index(res: &mut Response) {
    res.set_status_code(StatusCode::OK);
}
#[handler]
fn get_user(req: &mut Request, res: &mut Response) {
    res.render(req.params_mut().remove("id").unwrap());
}
#[tokio::main]
async fn main() {
    let router = Router::new().get(index).push(
        Router::with_path("user")
            .post(index)
            .push(Router::with_path("<id>").filter(get()).handle(get_user)),
    );
    let acceptor = TcpListener::new("0.0.0.0:3000").bind().await;
    let mut server = Server::new(acceptor);
    let http1 = server.http1_mut();
    http1.pipeline_flush(true);
    server.serve(router).await
}

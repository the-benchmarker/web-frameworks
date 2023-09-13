use salvo::prelude::*;
use salvo::routing::*;

#[handler]
fn index(res: &mut Response) {
    res.status_code(StatusCode::OK);
}
#[handler]
fn get_user(req: &mut Request, res: &mut Response) {
    res.render(req.params().get("id").unwrap());
}
#[tokio::main]
async fn main() {
    let router = Router::new().get(index).push(
        Router::with_path("user")
            .post(index)
            .push(Router::with_path("<id>").filter(get()).goal(get_user)),
    );
    let acceptor = TcpListener::new("0.0.0.0:3000").bind().await;
    Server::new(acceptor).serve(router).await
//     let mut server = Server::new(acceptor);
//     let http1 = server.http1_mut();
//     http1.pipeline_flush(true);
//     server.serve(router).await
}

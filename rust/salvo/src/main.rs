use salvo::prelude::*;

#[fn_handler]
fn index(res: &mut Response) {
    res.set_status_code(StatusCode::OK);
}
#[fn_handler]
fn get_user(req: &mut Request, res: &mut Response) {
    res.render(req.params().get("id").unwrap());
}
#[tokio::main]
async fn main() {
    let router = Router::new().get(index).push(
        Router::with_path("user")
            .post(index)
            .push(Router::with_path("<id>").get(get_user)),
    );
    let service = Service::new(router);
    salvo::hyper::Server::builder(TcpListener::bind("0.0.0.0:3000"))
        .http1_only(true)
        .http1_pipeline_flush(true)
        .serve(service)
        .await.unwrap();
}

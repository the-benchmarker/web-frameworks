use salvo::prelude::*;

#[fn_handler]
async fn index(res: &mut Response) {
    res.set_status_code(StatusCode::OK);
}
#[fn_handler]
async fn get_user(req: &mut Request, res: &mut Response) {
    res.render(req.params().get("id").unwrap());
}

fn main() {
    let router = Router::new().get(index).push(
        Router::new()
            .path("user")
            .post(index)
            .push(Router::new().path("<id>").get(get_user)),
    );
    salvo::run(async {
        Server::new(TcpListener::bind(([0, 0, 0, 0], 3000)))
            .serve(router)
            .await;
    });
}

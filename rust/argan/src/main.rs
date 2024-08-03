use argan::{handler::HandlerSetter, http::Method, request::RequestHead, Resource, Server};
use hyper_util::{rt::TokioExecutor, server::conn::auto::Builder};

async fn user_id(request_head: RequestHead) -> String {
    request_head.path_params_as::<String>().unwrap()
}

#[tokio::main]
async fn main() {
    let mut root = Resource::new("/");
    root.set_handler_for(Method::GET.to(|| async {}));

    let user = root.subresource_mut("/user");
    user.set_handler_for(Method::POST.to(|| async {}));

    user.subresource_mut("/{id}")
        .set_handler_for(Method::GET.to(user_id));

    let arc_service = root.into_arc_service();
    let connection_builder = Builder::new(TokioExecutor::new());

    Server::new(connection_builder)
        .serve(arc_service, "0.0.0.0:3000")
        .await
        .unwrap();
}

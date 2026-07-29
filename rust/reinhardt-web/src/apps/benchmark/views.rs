use reinhardt::http::ViewResult;
use reinhardt::{Path, Response, get, post};

#[get("/", name = "index")]
pub async fn index() -> ViewResult<Response> {
    Ok(Response::ok())
}

#[get("/user/{id}", name = "user-detail")]
pub async fn user_detail(Path(id): Path<String>) -> ViewResult<Response> {
    Ok(Response::ok().with_body(id))
}

#[post("/user", name = "user-create")]
pub async fn user_create() -> ViewResult<Response> {
    Ok(Response::ok())
}

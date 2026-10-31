use ruvel::{
    http::{HttpStatus, Response},
    routing::Router,
};

pub fn routes(router: &mut Router) {
    router.get("/", |_req| async move { () });
    router.get("/user/{id}", |req| async move {
        Response::new(HttpStatus::Ok).json(format!("{}", req.param("id").unwrap_or_default()))
    });
    router.post("/user", |_req| async move { Response::ok().json("") });
}

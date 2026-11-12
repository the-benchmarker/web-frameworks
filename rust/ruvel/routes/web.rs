use ruvel::{
    http::{Response, Status},
    routing::Router,
};

pub fn routes(router: &mut Router) {
    router.get("/", |_req| async move { () });
    router.get("/user/{id}", |req| async move {
        Response::new(Status::Ok).json(req.param("id").unwrap_or_default().to_string())
    });
    router.post("/user", |_req| async move { Response::ok().json("") });
}

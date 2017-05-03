extern crate futures;
extern crate hyper;

use futures::future::FutureResult;

use hyper::{Get, Post, StatusCode};
use hyper::server::{Http, Service, Request, Response};

#[derive(Clone, Copy)]
struct Echo;

impl Service for Echo {
    type Request = Request;
    type Response = Response;
    type Error = hyper::Error;
    type Future = FutureResult<Response, hyper::Error>;

    fn call(&self, req: Request) -> Self::Future {
        futures::future::ok(match (req.method(), req.path()) {
            // GET '/' return status code 200 with empty body
            (&Get, "/")  => {
                Response::new()
            },
            // GET '/user/:id' return status code 200 with the id
            (&Get, path)  => {
                Response::new()
                    .with_body(path[5..].to_string())
            },
            // POST '/user' return status code 200 with empty body
            (&Post, "/user") => {
                Response::new()
            },
            _ => {
                Response::new()
                    .with_status(StatusCode::NotFound)
            }
        })
    }
}


fn main() {
    let addr = "127.0.0.1:1337".parse().unwrap();

    let server = Http::new().bind(&addr, || Ok(Echo)).unwrap();
    println!("Listening on http://{} with 1 thread.", server.local_addr().unwrap());
    server.run().unwrap();
}

use std::io;

use may_minihttp::{HttpService, HttpServiceFactory, Request, Response};

// Routes
/*
GET /
GET /user/:id
POST /user
*/

#[derive(Clone)]
struct WebFramework;

impl HttpService for WebFramework {
    fn call(&mut self, req: Request, rsp: &mut Response) -> io::Result<()> {
        let method = req.method();
        // println!("method: {:?}", method);

        let path = req.path();
        // println!("path: {:?}", path);

        match (method, path) {
            ("GET", "/") => {
                rsp.header("Content-Type: text/plain");
            }
            (method_, path_) if path_.starts_with("/user") => {
                if method_ == "GET" {
                    let id = path_.split("/").last().unwrap();
                    rsp.header("Content-Type: text/plain");
                    rsp.body_mut().extend_from_slice(id.as_bytes());
                } else if method_ == "POST" {
                    rsp.status_code(200, "OK");
                }
            }
            ("POST", "/user") => {
                rsp.header("Content-Type: text/plain");
            }
            _ => {
                rsp.status_code(404, "Not Found");
            }
        }

        Ok(())
    }
}

struct HttpServer {}

impl HttpServiceFactory for HttpServer {
    type Service = WebFramework;

    fn new_service(&self, _: usize) -> Self::Service {
        WebFramework {}
    }
}

fn main() {
    let http_server = HttpServer {};
    let server = http_server.start("0.0.0.0:3000").unwrap();
    server.join().unwrap();
}

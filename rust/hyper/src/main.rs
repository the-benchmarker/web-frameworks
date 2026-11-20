use http_body_util::{combinators::BoxBody, BodyExt, Empty, Full};
use hyper::{body::Bytes, http::Method, server, service::service_fn, Request, Response};
use hyper_util::rt::TokioIo;
use std::net::SocketAddr;
use tokio::net::TcpListener;

static PATH_PREFIX: &'static str = "/user/";

#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    let addr: SocketAddr = "0.0.0.0:3000".parse().unwrap();
    let listener = TcpListener::bind(addr).await?;

    loop {
        let (stream, _) = listener.accept().await?;
        let io = TokioIo::new(stream);
        tokio::task::spawn(async move {
            let _ = server::conn::http1::Builder::new()
                .pipeline_flush(true)
                .serve_connection(io, service_fn(handle))
                .await;
        });
    }
}

async fn handle(
    req: Request<hyper::body::Incoming>,
) -> Result<Response<BoxBody<hyper::body::Bytes, hyper::Error>>, hyper::Error> {
    match (req.method(), req.uri().path()) {
        (&Method::GET, "/") | (&Method::POST, "/user") => return Ok(Response::new(empty())),
        (&Method::GET, x) => {
            if let Some((_, x)) = x.split_once(PATH_PREFIX) {
                return Ok(Response::new(full(x.to_owned())));
            }
        }
        _ => {}
    }
    return Ok(Response::builder().status(404).body(empty()).unwrap());
}

#[inline]
fn full<T: Into<Bytes>>(chunk: T) -> BoxBody<Bytes, hyper::Error> {
    Full::new(chunk.into())
        .map_err(|never| match never {})
        .boxed()
}

#[inline]
fn empty() -> BoxBody<Bytes, hyper::Error> {
    Empty::<Bytes>::new()
        .map_err(|never| match never {})
        .boxed()
}

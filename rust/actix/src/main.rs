extern crate actix;
extern crate actix_web;

use actix_web::*;

fn main() {
    let sys = actix::System::new("test");

    HttpServer::new(
        || Application::default()
            .resource("/",
                      |r| r.f(|_| httpcodes::HTTPOk))
            .resource("/user",
                      |r| r.f(|_| httpcodes::HTTPOk))
            .resource("/user/{id}",
                      |r| r.f(|req| {
                          let id = Binary::from_slice(req.match_info()["id"].as_ref());
                          httpcodes::HTTPOk.with_body(id)})))
        .bind("0.0.0.0:3000").unwrap()
        .start();

    println!("Started http server: 0.0.0.0:3000");
    let _ = sys.run();
}

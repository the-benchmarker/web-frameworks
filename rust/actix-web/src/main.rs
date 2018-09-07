extern crate actix;
extern crate actix_web;

use actix_web::*;

fn main() {
    let sys = actix::System::new("test");

    server::new(|| {
        App::new()
            .resource("/", |r| r.f(|r| HttpResponse::build_from(r)))
            .resource("/user", |r| r.f(|r| HttpResponse::build_from(r)))
            .resource("/user/{id}", |r| {
                r.f(|req| {
                    let id = Binary::from_slice(req.match_info()["id"].as_ref());
                    HttpResponse::build_from(req).body(id)
                })
            })
    }).bind("0.0.0.0:3000")
        .unwrap()
        .start();

    println!("Started http server: 0.0.0.0:3000");
    let _ = sys.run();
}

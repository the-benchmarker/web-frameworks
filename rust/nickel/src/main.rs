#![warn(rust_2018_idioms)]

// nickel macros are declarative macros that use private macros
// from the nickel namespace without specifying a path, therefore
// assuming that we import private stuff from nickel
use nickel::*;

fn main() {
    let mut server = Nickel::new();
    server.get("/", middleware!(""));
    server.get(
        "/user/:id",
        middleware! {|request|
                    request.param("id").unwrap()
        },
    );
    server.post("/user", middleware!(""));
    server.listen("0.0.0.0:3000").unwrap();
}

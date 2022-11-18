use tide::{Request, Result};

async fn user(req: Request<()>) -> Result<String> {
    let id = req.param("id").unwrap_or("");
    Ok(id.to_string())
}

#[async_std::main]
async fn main() {
    let mut app = tide::new();

    app.at("/").get(|_| async { Ok("") });
    app.at("/user/:id").get(user);
    app.at("/user").post(|_| async { Ok("") });

    app.listen("0.0.0.0:3000").await.unwrap();
}
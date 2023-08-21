use summer_boot::{Request, Result};

#[summer_boot::auto_scan()]
#[summer_boot::main]
async fn main() {
    summer_boot::run();
}

#[summer_boot::get("/")]
pub async fn index(_req: Request<()>) -> Result {
    Ok("".into())
}

#[summer_boot::get("/user/:id")]
pub async fn get_user(req: Request<()>) -> Result {
    let id = req.param("id").unwrap_or("");
    Ok(id.to_string().into())
}

#[summer_boot::post("/user")]
pub async fn post_user(_req: Request<()>) -> Result {
    Ok("".into())
}

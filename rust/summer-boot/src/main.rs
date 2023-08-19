use summer_boot::{Request, Result};

#[summer_boot::auto_scan()]
#[summer_boot::main]
async fn main() {
    summer_boot::run();
}

#[summer_boot::get("/hello")]
pub async fn hello(mut _req: Request<()>) -> Result {
    Ok(format!("Hello, Summer Boot").into())
}

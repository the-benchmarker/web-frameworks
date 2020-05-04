use warp::Filter;

#[tokio::main]
async fn main() {
    let user_handler = warp::path!("user" / u32).map(|id| format!("{}", id));

    let user_register_handler = warp::post().and(warp::path!("user")).map(|| "");

    let index_handler = warp::path!("user").map(|| "");

    let routes = warp::get().and(index_handler.or(user_register_handler).or(user_handler));

    warp::serve(routes).run(([127, 0, 0, 1], 3000)).await;
}

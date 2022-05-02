
use warp::Filter;

#[tokio::main]
async fn main() {
    let index = warp::path::end().map(|| "");
    let user = warp::path!("user" / u32).map(|id: u32| id.to_string());
    let user_post = warp::path!("user").map(|| "");

    let get_routes = warp::get().and(index.or(user));
    let post_routes = warp::post().and(index.or(user_post));

    let routes = get_routes.or(post_routes);

    warp::serve(routes).run(([0, 0, 0, 0], 3000)).await;
}

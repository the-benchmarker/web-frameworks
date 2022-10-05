use warp::Filter;

#[tokio::main]
async fn main() {
    let index = warp::path::end().and(warp::get()).map(|| "");

    let user = warp::path!("user" / u32).and(warp::get()).map(|id: u32| id.to_string());

    let user_post = warp::path("user").and(warp::post()).map(|| "");

    let routes = index.or(user).or(user_post);

    warp::serve(routes)
        .unstable_pipeline()
        .run(([0, 0, 0, 0], 3000))
        .await;
}

use ohkami::prelude::*;

fn benchmark_ohkami() -> Ohkami {
    Ohkami::new((
        "/"
            .GET(|| async {Response::OK()}),
        "/user"
            .POST(|| async {Response::OK()}),
        "/user/:id"
            .GET(|id: String| async {id}),
    ))
}

fn main() {
    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .event_interval(1)
        .build().unwrap()
        .block_on(benchmark_ohkami().howl("0.0.0.0:3000"))
}

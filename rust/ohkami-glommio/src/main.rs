use ohkami::prelude::*;
use ohkami::util::num_cpus;
use glommio::{LocalExecutorPoolBuilder, PoolPlacement, CpuSet};

fn main() {
    LocalExecutorPoolBuilder::new(PoolPlacement::MaxSpread(num_cpus::get(), CpuSet::online().ok()))
        .on_all_shards(|| {
            Ohkami::new((
                "/".GET(async || { Response::OK() }),
                "/user".POST(async || { Response::OK() }),
                "/user/:id".GET(async |Path(id): Path<String>| { id }),
            ))
            .howl("0.0.0.0:3000")
        })
        .unwrap()
        .join_all();
}

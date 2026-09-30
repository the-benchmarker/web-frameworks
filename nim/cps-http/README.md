# CPS HTTP benchmark adapter

This adapter benchmarks the full CPS HTTP/1.1 parser and router with the
repository's allocation-light accept API. It implements the three routes
required by `web-frameworks`, compiles with ARC, LTO, and native CPU tuning,
and runs one shard-local reactor thread per visible CPU. Each thread owns its
router, event loop, listener, and connections; `SO_REUSEPORT` distributes
accepts without a shared scheduler on the HTTP data path.

Generate the manifests and run the official benchmark on a native Linux host:

```sh
CONCURRENCIES=64,256,512 \
ROUTES=GET:/,GET:/user/0,POST:/user \
bundle exec rake config
./run.sh nim/cps-http
```

The official provider addresses the server through its Docker bridge IP. That
topology is not reachable from the macOS host under Docker Desktop, so macOS
container-client runs are diagnostic only and must not be submitted as
official results. The official route contract can still be run there with
`make -f nim/cps-http/.Makefile test` when the server is reachable.

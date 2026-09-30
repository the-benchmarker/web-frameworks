# Contributing

Contributions to framework implementations, correctness checks, benchmark
methodology, and documentation are welcome.

## Benchmark API

Every implementation listens on port `3000`. The shared contract currently
checks three legacy routes. Java/Spring is benchmarked on the six-route REST
workload below. The route and request fixture definitions live together in
[`.tasks/config.rake`](.tasks/config.rake). Both warmup and collection run
**every** route assigned to an implementation, with a separate result file
per method and URL. New frameworks should implement the full workload before
being compared on it.

| Method | URL used by the benchmark | Response | Feature measured |
| --- | --- | --- | --- |
| `GET` | `/health` | `{"status":"ok"}` | Static route and small JSON response |
| `GET` | `/user/42` | Plain text `42` | Numeric path parameter parsing |
| `POST` | `/upload` | `{"filename":"test.bin","size":4096}` | Multipart parsing and file access |
| `POST` | `/deserialization` | Empty body | JSON deserialization only |
| `GET` | `/serialization` | JSON array of 100 objects with `id` and `name` | JSON serialization only |
| `POST` | `/compute` | `{"subtotalCents":28266,"discountCents":1270,"taxCents":3905,"totalCents":30901}` | JSON deserialization and computation |

All successful requests return HTTP `200`. `/upload` receives a multipart
field named `file` containing the repository's 4,096-byte [`test.bin`](test.bin).
`/deserialization` receives
[the fixed JSON object](.tasks/fixtures/deserialization.json), parses it, and
returns no response body. `/compute` receives
[the fixed 16-item order](.tasks/fixtures/compute.json).
Its result has `subtotalCents`, `discountCents`, `taxCents`, and `totalCents`;
each line computes discount and then tax using integer division by 10,000.
`/serialization` serializes the same 100-object array on each request.
The workload uses no database or remote service.

Spring also serves `GET /`, `GET /user/0`, and `POST /user` for the shared
legacy contract; those routes are not part of its measured workload.

### Spring deployment

The Spring implementation uses Spring Boot 4.1.1 on Java 25 LTS and validates
numeric user IDs, compute inputs, and upload filenames. JSON bodies for
`/deserialization` and `/compute` are limited to 64 KiB, including requests
sent with chunked transfer encoding;
multipart requests are limited to 64 KiB. Invalid input receives an HTTP error
with Spring's Problem Details response. The generated container runs as UID
10001, and the server allows up to 20 seconds for graceful shutdown.

The benchmark requests are unauthenticated. Deploy the benchmark service on a
private network; a public deployment needs TLS, access control, and rate limits
at its ingress. These controls are outside the fixed endpoint workload.

Keep responses and request fixtures deterministic. Parse values through the
framework's ordinary HTTP and JSON facilities, then perform the specified
work. Avoid caching the completed response or replacing a parse with a fixed
answer: that changes the feature being measured. Return an error for malformed
JSON and invalid compute inputs.

## Adding a framework

Add the implementation under `<language>/<framework>/` and its `config.yaml`.
The root, language, and framework configs are merged in that order. Define
`bootstrap` as a YAML list of shell commands. Dockerfile generation applies
root commands, language commands, selected engine commands, framework commands,
then inline framework engine commands. Their order and repetitions are kept.

Define `environment` as YAML key/value pairs when needed:

```yaml
environment:
  NODE_ENV: production
  NEXT_TELEMETRY_DISABLED: 1
```

Later config layers override earlier values. Each resulting pair becomes an
`ENV KEY=value` instruction in the Dockerfile. The root config adds
`COPY test.bin /test.bin` immediately after every `FROM` in generated
Dockerfiles, before framework files. `bundle exec rake config` copies the
fixture into each Docker build context.

Add a target for the implementation in `Makefile` and `neph.yaml`, and its
repository metadata in `benchmarker.cr`. Use the shared route spec to check
the legacy contract. When the full workload is implemented, add the framework
to the route selection in `.tasks/config.rake` and verify every route over HTTP.

## Running locally

Install Docker, Ruby and Bundler, `jq`, and
[`zrk`](https://github.com/zoxy-io/zrk) 2.4 or newer. Generate the manifests:

```bash
bundle install
bundle exec rake config
```

Then use the generated `.Makefile` for one framework. For example:

```bash
make -f java/spring/.Makefile build
make -f java/spring/.Makefile test
make -f java/spring/.Makefile warmup
mkdir -p java/spring/.results/{64,256,512}
make -f java/spring/.Makefile collect
make -f java/spring/.Makefile unbuild
```

`CONCURRENCIES`, `DURATION`, `THREADS`, `SERVER_CPUS`, `LOAD_CPUS`, and
`LATENCY_RATE` control the run. Routes are fixed in `.tasks/config.rake` so a
benchmark run does not silently omit an endpoint. Compare measurements made
with the same benchmark revision, hardware, runtime variant, and workload.

# HTTP contracts

These Cucumber features test running implementations against [SCENARIO.md](../SCENARIO.md).

```sh
LANGUAGE=ruby FRAMEWORK=rails BENCHMARK_URL=http://127.0.0.1:3000 bundle exec ruby .spec/run.rb
```

Without `BENCHMARK_URL`, the runner reads the container address from
`<language>/<framework>/ip-<ENGINE>.txt` and uses port 3000.
The framework's top-level `specs` config selects v1 by default. Rails with
`specs: v2` runs both directories; other implementations run their configured
version. `ROUTES` does not narrow correctness checks.

Use `--tags @v1` or `--tags @v2` to inspect one version for Rails, and
`--dry-run` to validate features without sending requests. For example:

```sh
LANGUAGE=ruby FRAMEWORK=rails bundle exec ruby .spec/run.rb --tags @v2 --dry-run
bundle exec rspec spec
```

The v1 and v2 dynamic-route contracts differ: v1 requires a plain-text ID
(including `/user/0`), whereas v2 requires an eight-digit ID and a JSON object.
Running both suites reports compatibility failures if the server only exposes
v2 behavior. The suites keep these expectations independent.

JSON and upload scenarios cover both Content-Length and chunked bodies.
V2 checks exact response fields, media types, status codes, changing payloads,
load sizes, and absence of response compression. Logging, use of framework APIs,
and temporary-file cleanup also require implementation review; HTTP responses
alone cannot prove those runtime requirements.

Repository unit tests for command generation and HTTP transport live in `spec/`.

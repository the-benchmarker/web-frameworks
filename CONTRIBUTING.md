# Contributing

Contributions to framework implementations, correctness checks, benchmark
methodology, and documentation are welcome.

## Benchmark API

Every implementation listens on port `3000`. The v1 contract checks
`GET /`, `GET /user/0`, and `POST /user`. The six-route stateless v2 workload
and exact responses are defined in [SCENARIO.md](SCENARIO.md). Set a top-level
`specs: v2` in the framework's `config.yaml` to enable v2; omission or
`specs: v1` selects v1.
Rails selects v2 workloads and is checked against both contract suites. Every
other framework explicitly selects v1 and runs only the v1 suite.
Run `bundle exec rake config` to generate commands.
The generated test target runs `bundle exec ruby .spec/run.rb`; it reads the
framework config and runs the selected Gherkin features with Cucumber.
Repository unit tests live in `spec/` and run with `bundle exec rspec spec`. v2-only tests,
workloads, and fixture mounts are skipped unless `specs: v2` is present.
Route definitions live in [`.tasks/workloads.rb`](.tasks/workloads.rb), and
request fixtures live at the repository root.
Warmup and collection run every selected route separately. Compare frameworks
on the same scenario version and keep their results separate.

### Rails reference

Rails implements v2 through API controllers, callbacks, and small response
services. It parses the supplied JSON and multipart bodies, hashes uploaded
tempfiles in chunks, and cleans up temporary uploads after each request. The
Rails application does not configure a 64 KiB request-body limit. See
[SCENARIO.md](SCENARIO.md) for validation and load payloads.

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
work. Load scripts vary the POST bodies at a fixed size while specs retain the
shared fixtures. Avoid caching completed responses or replacing parsing with a
fixed answer. Return errors for malformed input as defined by the selected contract.

## Adding a framework

Add the implementation under `<language>/<framework>/` and its `config.yaml`.
The root, language, and framework configs are merged in that order. Define
`bootstrap` as a YAML list of shell commands. Dockerfile generation applies
root commands, language commands, selected engine commands, framework commands,
then inline framework engine commands. Their order and repetitions are kept.

Renovate synchronizes language metadata after updating Dockerfiles and checks
that build and runtime pins agree before running. In the Renovate container,
run `ruby .github/scripts/sync-language-versions.rb --check` to verify the pins,
or omit `--check` to synchronize metadata.

Set `reviewers` as a top-level list of GitHub usernames in
`<language>/config.yaml` for language defaults or
`<language>/<framework>/config.yaml` for framework-specific reviewers:

```yaml
reviewers:
  - your-github-username
```

Framework reviewers override language reviewers. Renovate uses `@waghanza`
when neither config defines reviewers. An empty list disables reviewer requests
for that language or framework. Reviewer requests also apply to auto-merge PRs.
The reviewer workflow runs only when an original PR targeting `develop` is opened.
Release PRs and `master`-to-`develop` sync PRs do not trigger reviewer requests.

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
repository metadata in `benchmarker.cr`. New implementations default to v1. When
migrating to v2, set `specs: v2`, and verify every route
over HTTP against the framework's Docker container.

## Running in Docker

Install Docker, Ruby and Bundler, `jq`, and
[`zrk`](https://github.com/zoxy-io/zrk) 3.0.0 or newer for Lua support. Generate the manifests:

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
`LATENCY_RATE` control the run. The selected spec suite validates the full
contract even when `ROUTES` limits the measured workloads. Compare measurements made
with the same benchmark revision, hardware, runtime variant, and workload.

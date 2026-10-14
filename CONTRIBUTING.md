## How to contribute ?

Contributions of any kind a :heart: accepted

- Adding new frameworks
- Fix some frameworks
- Update dependencies
- Discuss best practices

## Running the benchmarks

Only needed to run benchmarks locally (`./run.sh`) — CI builds each framework
and checks its routes, but never benchmarks, so none of this is required to
open a pull request.

- **Docker**, to build and run each framework
- **Ruby** + `bundle install`, for the `rake` tasks that generate the
  per-framework `.Dockerfile`/`.Makefile`
- **[zrk](https://github.com/zoxy-io/zrk) >= 2.4.0**, the load generator the
  `collect` targets shell out to. `brew install zoxy-io/tap/zrk`, or grab a
  static binary from the [releases](https://github.com/zoxy-io/zrk/releases).
  It must be on `PATH`. The harness runs it in `--closed` mode (added in
  2.2.0) and is validated on 2.4.x; `run.sh` enforces the minimum.

## Adding a framework

Pin image and toolchain versions directly in the language Dockerfiles. Store
language and runtime `version` values in the language `config.yaml` as quoted
`X.Y` strings, for example `version: '1.20'`. Image patch versions and distribution
suffixes are omitted from this metadata. Each framework inherits the language's
shared runtime version. Keep language standards and runtime names in separate
`standard` and `runtime` fields.

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

Define `bootstrap` as a YAML list of shell commands. Dockerfile generation
concatenates commands from the root config, the language config (for example,
`javascript/config.yaml`), then the framework config (for example,
`javascript/nextjs/config.yaml`). Within each file, commands run in this order:
top-level bootstrap, language bootstrap, selected language engine bootstrap,
framework bootstrap, then inline framework engine bootstrap. Command order and
repeated commands are preserved, so language dependency installation runs before
framework build commands.

Define `environment` as YAML key/value pairs:

```yaml
environment:
  NODE_ENV: production
  NEXT_TELEMETRY_DISABLED: 1
```

Environment mappings merge from the root config, then the language config, then
the framework config. Within each file, the order is top-level environment,
language environment, selected language engine environment, framework environment,
then inline framework engine environment. Later values override duplicate keys;
variables for other engines are excluded. Each resulting pair renders as one
`ENV KEY=value` instruction in the Dockerfile.

- All frameworks **SHOULD** follow this rules :

| HTTP   | Route       | Status code | Response body         |
| ------ | ----------- | ----------- | --------------------- |
| `GET`  | `/`         | `200`       | **Empty**             |
| `GET`  | `/user/:id` | `200`       | **id** given as param |
| `POST` | `/user`     | `200`       | **Empty**             |

- All framework **SHOULD** contain a `Dockerfile`

- All framework **SHOULD** be referenced in :
  - `Makefile`, a target group for the language, and a target for the framework
  - `neph.yaml`, a target group for the language, and a target for the framework
  - `benchmarker.cr`, a hash for language containing all frameworks an
    repository information

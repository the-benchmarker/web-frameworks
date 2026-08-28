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
- **[zrk](https://github.com/zoxy-io/zrk) >= 2.2.0**, the load generator the
  `collect` targets shell out to. `brew install zoxy-io/tap/zrk`, or grab a
  static binary from the [releases](https://github.com/zoxy-io/zrk/releases).
  It must be on `PATH`. 2.2.0 is the minimum because the harness runs it in
  `--closed` mode, which older versions do not have.

## Adding a framework

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

<div align="center">

# 🚀 Web Frameworks Benchmark

**A community-maintained, reproducible comparison of backend web frameworks**

[Explore the results](https://web-frameworks-benchmark.netlify.app/) · [Add a framework](CONTRIBUTING.md) · [Report a problem](https://github.com/the-benchmarker/web-frameworks/issues) · [Join the discussion](https://github.com/the-benchmarker/web-frameworks/discussions)

[![CI](https://github.com/the-benchmarker/web-frameworks/actions/workflows/ci.yml/badge.svg)](https://github.com/the-benchmarker/web-frameworks/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![GitHub stars](https://img.shields.io/github/stars/the-benchmarker/web-frameworks?style=flat)](https://github.com/the-benchmarker/web-frameworks/stargazers)
[![Contributors](https://img.shields.io/github/contributors/the-benchmarker/web-frameworks)](https://github.com/the-benchmarker/web-frameworks/graphs/contributors)

</div>

From established full-stack platforms to small HTTP routers, this repository puts hundreds of implementations behind the same tiny API and runs them in isolated containers. Today, it makes a focused part of web-stack performance easier to measure, reproduce, and discuss. The longer-term goal is a comprehensive, evidence-based guide that helps businesses choose a backend framework—not merely a leaderboard that declares one universal winner.

> [!IMPORTANT]
> These are minimal HTTP throughput and latency tests, not a substitute for profiling a production application. Framework features, maintainability, ecosystem, security, database access, and your real workload all matter.

## 🧪 Benchmark contract

Every implementation is expected to listen on port `3000` and provide the same responses:

| Method | Route | Expected status | Expected body |
|:--|:--|:--:|:--|
| `GET` | `/` | `2xx` | Empty |
| `GET` | `/user/:id` | `2xx` | The `id` path parameter |
| `POST` | `/user` | `2xx` | Empty |

Before an implementation is benchmarked, the shared [RSpec contract](.spec/route_spec.rb) verifies these routes. The default generated benchmark runs `GET /` for 15 seconds, disables keep-alive, applies latency correction, and records an `oha` JSON report. Concurrency and routes are configurable.

Collected fields include requests per second, total data received, run duration, and the p50, p75, p90, and p99 latency percentiles. Benchmark configuration is generated from three YAML layers:

```text
config.yaml                  # provider and global settings
<language>/config.yaml       # language image, runtime, and engine settings
<language>/<framework>/config.yaml
                             # framework version, files, command, and overrides
```

## 📊 Results

The latest published results are available on the [Web Frameworks Benchmark dashboard](https://web-frameworks-benchmark.vercel.app). Each result should be read in the context of its framework version, runtime or server variant, concurrency, benchmark revision, and hardware.

For a fair interpretation:

- compare runs produced by the same benchmark revision and machine;
- check the selected runtime/server variant, not only the framework name;
- treat small differences as noise until repeated;
- do not extrapolate this minimal API test to database-heavy or full application workloads.

## 🗺️ Roadmap

The project is evolving from a focused HTTP performance benchmark into a broader decision-support guide for organizations choosing a backend framework. The intended presentation is inspired by the clarity of quadrant-based market analysis, but the evaluation model will be designed specifically for backend frameworks and backed by transparent, reproducible evidence. This project is independent and is not affiliated with or endorsed by Gartner.

+ [ ] ⚡ Use a modern **load generator**, please follow discussion in https://github.com/the-benchmarker/web-frameworks/discussions/8088
+ [ ] 🔧 Optimize implementations, please follow discussion in https://github.com/the-benchmarker/web-frameworks/discussions/8093
+ [ ] 🏗️ Build a real infrastructure using an public cloud
+ [ ] ⚖️  Check HTTP compliance for each framework here
+ [ ] 🧭 Build a framework decision model
+ [ ] 💼 Publish a practical business guide

## 🏃 Run an implementation locally

### 📋 Requirements

- Git
- Docker
- Ruby and Bundler (CI currently uses Ruby 4)
- [`zrk`](https://github.com/zoxy-io/zrk) on `PATH`
- `jq`

Install the Ruby dependencies and generate the Dockerfiles and Makefiles:

```bash
git clone https://github.com/the-benchmarker/web-frameworks.git
cd web-frameworks
bundle install
bundle exec rake config
```

Then choose an implementation—for example, `javascript/fastify`—and use its generated Makefile:

```bash
make -f javascript/fastify/.Makefile build
make -f javascript/fastify/.Makefile test
make -f javascript/fastify/.Makefile warmup
mkdir -p javascript/fastify/.results/10
make -f javascript/fastify/.Makefile collect
make -f javascript/fastify/.Makefile unbuild
```

Results are written beneath `<language>/<framework>/.results/`. Generated targets also include `clean` for removing the framework container.

### 🎛️ Customize a run

The generator accepts comma-separated concurrency levels and routes. Set them while generating the Makefiles:

```bash
CONCURRENCIES=64,256,512 \
ROUTES='GET:/,GET:/user/42,POST:/user' \
bundle exec rake config
```

Then run the same `collect` target. Route entries use the `METHOD:/path` form.

The load generator runs closed-loop over keep-alive connections: every connection sends its next request as soon as the previous response arrives, so the reported rate is the framework's sustained throughput for the whole run. Three more variables control where that work runs:

- `THREADS` sets how many load threads zrk uses. It defaults to the cores in `LOAD_CPUS`, or every host core when unset. zrk's own default of 2 threads is far too few to saturate a fast server.
- `SERVER_CPUS` and `LOAD_CPUS` take cpuset specs such as `0-3` and `4-15`. They pin the framework container (`--cpuset-cpus`) and the load generator (`taskset`) to disjoint cores so neither can steal cycles from the other.
- `LATENCY_RATE` adds a second pass per route at a fixed request rate. The closed-loop run reports throughput and latency at saturation; this pass reports latency at a defined load, with coordinated-omission correction. `LATENCY_RATE=50%` uses half of each framework's own closed-loop rate, so every framework is measured inside what it can sustain; `LATENCY_RATE=20000` uses the same absolute rate for all. The results land in `<route>_latency.json` and are imported as `latency_at_rate_*` metrics, separate from the headline rate. It doubles the collect time and is off by default.

Every `collect` run also writes `.results/<concurrency>/saturation.json`, the share of its allotted CPU the server actually used. Once results are exported to `data.json`, `bundle exec rake db:check_saturation` lists the runs where the server stayed below 50%, which means something else, usually the load generator, was the bottleneck and the number does not describe the framework.
Create a matching `.results/<concurrency>` directory for every configured concurrency level before collecting results; the batch runner does this automatically for its predefined levels.

> [!CAUTION]
> A full benchmark consumes substantial CPU, memory, time, network bandwidth, and container storage. Start with one implementation and keep the load generator separate from services you care about.

## 🤝 Add or update a framework

Contributions are welcome: new frameworks and variants, dependency updates, correctness fixes, documentation, and benchmarking improvements all help.

A typical implementation lives in `<language>/<framework>/` and contains:

- a `config.yaml` describing its version, website, files, startup command, and engine variants;
- the smallest application that fulfills the shared route contract;
- dependency manifests or lockfiles required for a reproducible container build.

To validate a contribution:

```bash
bundle exec rake config
make -f <language>/<framework>/.Makefile build
make -f <language>/<framework>/.Makefile test
make -f <language>/<framework>/.Makefile unbuild
```

Read [CONTRIBUTING.md](CONTRIBUTING.md) before opening a pull request, and follow the [Code of Conduct](CODE_OF_CONDUCT.md). If you maintain the framework you are adding, mention that in the PR—it helps reviewers evaluate idiomatic configuration and tuning.

## 🗂️ Repository map

| Path | Purpose |
|:--|:--|
| `<language>/<framework>/` | Framework implementation and configuration |
| `.spec/` | Shared HTTP correctness contract |
| `.tasks/` | Configuration, CI, collection, cloud, and database tasks |
| `.github/workflows/ci.yml` | Changed-implementation CI matrix |
| `config.yaml` | Global providers and execution templates |
| `data.json` / `data.min.json` | Published benchmark dataset |
| `run.sh` | Batch runner for all, one language, or one framework |

## 💖 Sponsors

Sponsors help cover benchmark infrastructure, repeated test runs, result hosting, and the maintenance involved in keeping hundreds of implementations current.

![Neon](./.assets/neon.svg)

To sponsor hardware, cloud credits, hosting, or ongoing development, [start a sponsorship discussion](https://github.com/the-benchmarker/web-frameworks/discussions/new?category=general). Sponsor names and links can be added here with the sponsor's approval; support never affects benchmark rankings or inclusion decisions.

## 💬 Community and support

- Use [GitHub Discussions](https://github.com/the-benchmarker/web-frameworks/discussions) for methodology, ideas, and general questions.
- Use [GitHub Issues](https://github.com/the-benchmarker/web-frameworks/issues) for reproducible bugs and framework failures.
- Use pull requests for implementation and documentation improvements.

Please include the framework path, runtime variant, host architecture, Docker version, commands used, and relevant logs when reporting a benchmark problem.

## 📄 License

This project is available under the [MIT License](LICENSE).

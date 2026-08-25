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

The latest published results are available on the [Web Frameworks Benchmark dashboard](https://web-frameworks-benchmark.netlify.app/). Each result should be read in the context of its framework version, runtime or server variant, concurrency, benchmark revision, and hardware.

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
- [`oha`](https://github.com/hatoo/oha) on `PATH` or at `~/.cargo/bin/oha`
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


<svg width="1327" height="512" viewBox="0 0 1327 512" fill="none" xmlns="http://www.w3.org/2000/svg">
<rect width="1326.81" height="511.365" rx="4" fill="#161616"/>
<path d="M413.226 96.0894V415.365L289.837 308.315V415.365H97V96L413.226 96.0894ZM135.763 376.602H251.074V223.257L374.465 332.424V134.841L135.763 134.772V376.602Z" fill="#34D59A"/>
<path d="M1229.81 352.35L1107.04 243.699V350.22H1068.43V159.015L1191.19 267.666V161.146H1229.81V352.35Z" fill="white"/>
<path d="M945.475 352.35C891.949 352.35 848.808 309.209 848.808 255.683C848.808 202.156 891.949 159.015 945.475 159.015C999.002 159.015 1042.14 202.156 1042.14 255.683C1042.14 309.209 999.002 352.35 945.475 352.35ZM945.475 315.068C978.496 315.068 1003.26 288.438 1003.26 255.683C1003.26 222.927 978.496 196.297 945.475 196.297C912.454 196.297 887.688 222.927 887.688 255.683C887.688 288.438 912.454 315.068 945.475 315.068Z" fill="white"/>
<path d="M710.358 350.22V161.146H825.933V199.227H748.972V236.509H809.955V273.791H748.972V312.138H825.933V350.22H710.358Z" fill="white"/>
<path d="M667.85 352.35L545.085 243.699V350.22H506.471V159.015L629.236 267.666V161.146H667.85V352.35Z" fill="white"/>
</svg>


To sponsor hardware, cloud credits, hosting, or ongoing development, [start a sponsorship discussion](https://github.com/the-benchmarker/web-frameworks/discussions/new?category=general). Sponsor names and links can be added here with the sponsor's approval; support never affects benchmark rankings or inclusion decisions.

## 💬 Community and support

- Use [GitHub Discussions](https://github.com/the-benchmarker/web-frameworks/discussions) for methodology, ideas, and general questions.
- Use [GitHub Issues](https://github.com/the-benchmarker/web-frameworks/issues) for reproducible bugs and framework failures.
- Use pull requests for implementation and documentation improvements.

Please include the framework path, runtime variant, host architecture, Docker version, commands used, and relevant logs when reporting a benchmark problem.

## 📄 License

This project is available under the [MIT License](LICENSE).

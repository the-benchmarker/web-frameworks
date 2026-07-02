# Web Frameworks Benchmark

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Code of Conduct](https://img.shields.io/badge/Code%20of%20Conduct-Contributor%20Covenant-blue)](CODE_OF_CONDUCT.md)
[![Contributing](https://img.shields.io/badge/Contributions-Welcome-brightgreen)](CONTRIBUTING.md)

**Web Frameworks Benchmark** is a comprehensive performance comparison of web frameworks across multiple programming languages. This project tests and benchmarks HTTP request handling, routing, and response performance for 100+ frameworks.

> **Note**: This repository contains the benchmark implementations. The results and interactive comparisons are available at [https://web-frameworks-benchmark.netlify.app](https://web-frameworks-benchmark.netlify.app) or [the-benchmarker.github.io](https://the-benchmarker.github.io)

---

## Table of Contents

1. [Supported Languages & Frameworks](#supported-languages--frameworks)
2. [How It Works](#how-it-works)
3. [Installation](#installation)
4. [Running Benchmarks](#running-benchmarks)
5. [Project Structure](#project-structure)
6. [Contributing](#contributing)
7. [Benchmark Methodology](#benchmark-methodology)
8. [Internal Architecture](#internal-architecture)
9. [Viewing Results](#viewing-results)
10. [License](#license)

---

## Supported Languages & Frameworks

Currently benchmarking **40+ languages** with **100+ frameworks**:

| Language | Frameworks | Language | Frameworks |
|----------|------------|----------|------------|
| **Go** | aero, air, apirouter, atreugo, aurora, beego, breeze, bunrouter, chi, clevergo, echo, fasthttp, fiber, gin, gofiber, gorilla, h3, hertz, httprouter, iris, lapix, macaron, Martini, negroni, pure, revel, rocket, router, sard, scale, server, standard, tango, tiger, tollbooth, traffic, uadmin, vintage, wUnderTheC | **JavaScript/Node.js** | express, fastify, foxify, hapi, http, koa, nestjs, restana, restify, route, serverless, tseds, typestack |
| **Python** | aiohttp, blacksheep, bottle, cherrypy, django, falcon, fastapi, flask, hug, quart, pyramid, sanic, starlette, tornado, uvicorn, vibora | **Java** | act, avaje, blink, joby, jodd, light, nio, play, prime, quarkus, raw, spring, spark, undertow, vertx |
| **Ruby** | agnostic, camp, cubit, CuttingRoom, em, grape, hanami, nyinyi, puma, rack, rails, roda, cinta, sinatra, syro | **PHP** | aura, cousin, fatfree, fuel, laravel, lemming, lumen, native, nikic, phalcon, phpx, pp, psx, react, slim, symfony, yaf, zend |
| **Rust** | actix, axum, conduit, gouge, hyper, nickel, rouille, rocket, salvo, tide, warp | **C#/.NET** | aspnet, aspnetmvc, carter, dotnet, dotnetcore, Nancy, raw |
| **Crystal** | amber, athena, lucky, prism | **Elixir** | bandit, phoenix, plug, raxx |
| **Clojure** | compojure, pedestal, ring | **Scala** | akka, http4s, play, scalatra |
| **Kotlin** | http4k, kTor, spring, vertx | **Swift** | kitura, vapor |
| **Dart** | shelf, start | **Julia** | genie, http |
| **C** | civetweb, libsoup | **C++** | cppcms, crow, drogon, evhtp, pistache, qdj, restbed, treefrog |
| **Objective-C** | perfect | **Zig** | bun |
| **Nim** | jester, single | **OCaml** | dream, httpaf, opium |
| **Haskell** | scotty, servant, spock, wai | **Erlang** | cowboy |
| **Lua** | lapis, openresty | **Perl** | dancer, mojolicious |
| **D** | hunt, vibe | **F#** | giraffe, saturn, suave |

*...and more! See the [language directories](#project-structure).*

---

## How It Works

This project benchmarks web frameworks by testing them with a standardized set of HTTP endpoints. Each framework implementation must:

1. **Handle specific routes** with defined responses
2. **Run in a Docker container** for isolation and consistency
3. **Be tested** with the same methodology across all frameworks

### Test Endpoints

Each framework must implement these endpoints:

| Method | Route | Status Code | Response Body |
|--------|-------|-------------|---------------|
| `GET` | `/` | `200` | Empty body |
| `GET` | `/user/:id` | `200` | The `id` parameter value |
| `POST` | `/user` | `200` | Empty body |

This ensures a fair comparison of routing, parameter handling, and basic request/response performance.

---

## Installation

### Prerequisites

To run benchmarks locally, you need:

- **Docker** (for containerized testing) - [Install Docker](https://docs.docker.com/get-docker/)
- **Git** (for version control)
- **Make** (for build automation)
- **Python 3** (for data processing scripts)
- **Ruby** (for some build scripts - optional)
- **wrk** or **bombardier** (for load testing - optional)

### Clone the Repository

```bash
git clone https://github.com/the-benchmarker/web-frameworks.git
cd web-frameworks
```

### Install Python Dependencies

```bash
pip install pyyaml requests  # Required for data processing
```

---

## Running Benchmarks

### Quick Start

List all available frameworks and run benchmarks:

```bash
# List all available frameworks
find . -mindepth 3 -type f -name config.yaml | grep -v excluded > ~/list.txt

# Run benchmarks for all frameworks (this will take hours!)
./run.sh
```

### Benchmark Specific Frameworks

```bash
# Run benchmarks for a specific language
./run.sh go

# Run benchmarks for a specific framework
./run.sh go/gin
```

### Manual Testing

Each framework has its own build and test process:

```bash
# Navigate to a framework directory
cd go/gin

# Build the Docker image
make build

# Start the container
make start

# Run functional tests
make test

# Warm up the server
make warmup

# Collect benchmark data
make collect

# Stop and clean up
make unbuild
```

### Available Make Commands

Each framework directory typically has a `.Makefile` with these targets:

| Command | Description |
|---------|-------------|
| `build` | Build the Docker image |
| `start` | Start the container |
| `stop` | Stop the container |
| `test` | Run functional tests |
| `warmup` | Warm up the server (60 seconds) |
| `collect` | Collect benchmark data using load testing |
| `unbuild` | Remove the container and image |
| `memory-idle` | Test memory usage at idle |

---

## Project Structure

```
web-frameworks/
├── README.md                    # Project documentation (this file)
├── CONTRIBUTING.md              # Contribution guidelines
├── CODE_OF_CONDUCT.md           # Code of conduct
├── LICENSE                      # MIT License
├── Makefile                     # Global make commands
├── config.yaml                  # Global configuration
├── data.json                    # Full benchmark results
├── data.min.json                # Minified benchmark results
├── run.sh                       # Main benchmark runner script
├── changelog.py                 # Change log generator
├── sqash.py                     # Data processing script
├── pipeline.lua                 # CI/CD pipeline configuration
├── .github/
│   └── workflows/
│       └── ci.yaml              # GitHub Actions CI workflow
├── .tasks/                      # Task configurations
├── bench/                       # Benchmarking tools and scripts
├── src/                         # Source code for benchmark tools
│
├── LANGUAGE/
│   └── FRAMEWORK/
│       ├── Dockerfile           # Container configuration
│       ├── config.yaml          # Framework-specific metadata
│       ├── main.go (or equiv)   # Framework implementation
│       ├── go.mod (etc.)        # Language-specific files
│       ├── .Makefile            # Framework build/test commands
│       └── .results/            # Benchmark results (auto-generated)
│
└── ... (40+ language directories: go, python, ruby, php, java, javascript, rust, etc.)
```

### Framework Configuration

Each framework has a `config.yaml` file with metadata:

```yaml
framework:
  website: https://github.com/owner/repo
  version: 1.9.0
  engines:
    - gnet
    - fasthttp
  docker_image: golang:1.21  # Optional: custom base image
  port: 3000                # Optional: port number
```

---

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for full guidelines.

### Quick Contribution Guide

1. **Fork** the repository on GitHub
2. **Create a feature branch** (`git checkout -b feature/add-new-framework`)
3. **Make your changes**
4. **Test** your changes (`./run.sh LANGUAGE/FRAMEWORK`)
5. **Commit** your changes with descriptive messages
6. **Push** to the branch (`git push origin feature/add-new-framework`)
7. **Open a Pull Request**

### Adding a New Framework

To add a new framework:

1. **Create the directory structure**:
   ```bash
   mkdir -p LANGUAGE/FRAMEWORK
   cd LANGUAGE/FRAMEWORK
   ```

2. **Add required files**:
   - `Dockerfile` - Container configuration
   - `config.yaml` - Framework metadata (website, version)
   - `main.go` (or equivalent in your language) - Framework implementation
   - `.Makefile` - Build and test commands

3. **Implement the required endpoints** (example for Go with Gin):
   ```go
   package main
   
   import (
       "github.com/gin-gonic/gin"
   )
   
   func main() {
       r := gin.Default()
       
       // Required: GET / returns 200 with empty body
       r.GET("/", func(c *gin.Context) {
           c.Status(200)
       })
       
       // Required: GET /user/:id returns 200 with id as body
       r.GET("/user/:id", func(c *gin.Context) {
           c.String(200, c.Param("id"))
       })
       
       // Required: POST /user returns 200 with empty body
       r.POST("/user", func(c *gin.Context) {
           c.Status(200)
       })
       
       r.Run(":3000")
   }
   ```

4. **Create a Dockerfile**:
   ```dockerfile
   FROM golang:1.21-alpine
   
   WORKDIR /app
   
   RUN go mod init github.com/the-benchmarker/web-frameworks/go/gin
   COPY go.mod go.sum ./
   RUN go mod download
   
   COPY . .
   RUN go build -o app .
   
   EXPOSE 3000
   
   CMD ["./app"]
   ```

5. **Create a .Makefile**:
   ```makefile
   build:
   	docker build -t the-benchmarker/go-gin .
   
   start:
   	docker run -d -p 3000:3000 --name go-gin the-benchmarker/go-gin
   
   stop:
   	docker stop go-gin || true
   
   test:
   	@echo "Testing GET /"
   	@curl -s -o /dev/null -w "%{http_code}" http://localhost:3000/ | grep -q "200"
   	@echo "Testing GET /user/123"
   	@curl -s http://localhost:3000/user/123 | grep -q "123"
   	@echo "Testing POST /user"
   	@curl -s -o /dev/null -w "%{http_code}" -X POST http://localhost:3000/user | grep -q "200"
   
   collect:
   	# Use wrk or other tool to benchmark
   	@echo "Collecting benchmark data..."
   
   unbuild:
   	docker stop go-gin || true
   	docker rm go-gin || true
   	docker rmi the-benchmarker/go-gin || true
   
   warmup:
   	@echo "Warming up for 60 seconds..."
   	@sleep 60
   ```

6. **Test your framework**:
   ```bash
   cd LANGUAGE/FRAMEWORK
   make build
   make start
   make test
   make stop
   make unbuild
   ```

7. **Update global Makefile** (optional but recommended):
   Add targets for your framework in the root `Makefile`

8. **Submit a PR** with your changes

### Framework Requirements

- [x] Must follow the [endpoint specification](#test-endpoints)
- [x] Must have a `Dockerfile`
- [x] Must have a `config.yaml` with website and version
- [x] Must have a `.Makefile` with required targets
- [x] Should be referenced in root `Makefile`

### Code of Conduct

Please follow our [Code of Conduct](CODE_OF_CONDUCT.md) to help maintain a welcoming and inclusive community.

---

## Benchmark Methodology

### Testing Process

The benchmark process follows these steps for each framework:

1. **Build**: Docker image is built for the framework
2. **Start**: Container is started and server begins listening
3. **Warmup**: Server is warmed up with initial requests (60 seconds)
4. **Test**: Functional tests verify endpoints work correctly
5. **Collect**: Benchmark data is collected using load testing tools
6. **Cleanup**: Container is stopped and removed

### Metrics Collected

| Metric | Description |
|--------|-------------|
| **Requests per second (RPS)** | Throughput - how many requests can be handled per second |
| **Average Latency** | Mean response time in milliseconds |
| **p50 Latency** | Median response time |
| **p95 Latency** | 95th percentile - 95% of requests complete within this time |
| **p99 Latency** | 99th percentile - 99% of requests complete within this time |
| **Memory Usage (RSS)** | Resident Set Size - physical memory used |
| **Peak Memory** | Maximum memory used during testing |
| **CPU Usage** | CPU percentage during load |
| **Startup Time** | Time from start to first successful response |

### Load Testing Tools

- **[wrk](https://github.com/wg/wrk)** - Modern HTTP benchmarking tool (primary)
- **[bombardier](https://github.com/codesenven/bombardier)** - Fast cross-platform HTTP benchmarking
- Custom Python scripts for data aggregation and analysis

### Test Configuration

| Parameter | Value |
|-----------|-------|
| Concurrency levels | 64, 256, 512 connections |
| Test duration | 30-60 seconds per concurrency level |
| Warmup period | 60 seconds before testing |
| Requests | GET /, GET /user/:id, POST /user |
| Multiple runs | 3-5 runs for statistical significance |

### Test Environment

- **Hardware**: Standardized across all tests
- **Network**: Local Docker network (no external network latency)
- **OS**: Linux (Ubuntu latest)
- **Docker**: Latest stable version

---

## Internal Architecture

### Data Flow

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│  Framework Code  │────▶│   Dockerfile    │────▶│    .Makefile    │
│  (main.go, etc.) │     │   (Container)   │     │  (build, test)  │
└─────────────────┘     └─────────────────┘     └────────┬────────┘
                                                      │
                                                      ▼
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│   run.sh        │◀────│   make test     │     │   Benchmark    │
│  (Orchestrator)  │     │   (Validation)   │     │   Tools        │
└─────────────────┘     └─────────────────┘     │  (wrk, etc.)    │
                                                      └────────┬────────┘
                                                               │
                                                               ▼
                                                        ┌─────────────────┐
                                                        │   data.json     │
                                                        │  (Results)      │
                                                        └─────────────────┘
```

### Key Components

1. **run.sh** - Main orchestrator script
   - Discovers all frameworks via `config.yaml` files
   - Runs build, test, warmup, and collect for each framework
   - Manages errors and retries failed tests
   - Aggregates results into `data.json`

2. **Pipeline** - CI/CD workflow (GitHub Actions)
   - Runs benchmarks on pull requests
   - Validates new framework additions
   - Updates results data automatically
   - Deploys to the benchmark website

3. **Data Processing** - Python scripts
   - `changelog.py` - Generates change logs
   - `sqash.py` - Processes and minifies data
   - Custom scripts for visualization generation

4. **Website** - Result display
   - Interactive charts and graphs
   - Framework comparisons
   - Historical trends
   - Detailed per-framework metrics

### Result Storage Format

**data.json** contains full benchmark results:

```json
{
  "frameworks": {
    "go-gin": {
      "name": "Gin",
      "language": "Go",
      "language_link": "https://golang.org",
      "framework_link": "https://github.com/gin-gonic/gin",
      "version": "1.9.0",
      "maturity": "Production",
      "last_update": "2026-07-01",
      "stars": 65000,
      "results": {
        "64": {
          "rps": 1234567,
          "mean": 85.2,
          "std": 12.3,
          "min": 50.0,
          "max": 200.0,
          "p50": 80.0,
          "p90": 120.0,
          "p99": 180.0,
          "memory": 45.6,
          "cpu": 85.2
        },
        "256": { ... },
        "512": { ... }
      }
    }
  },
  "metadata": {
    "generated_at": "2026-07-01T12:00:00Z",
    "total_frameworks": 120,
    "total_languages": 42
  }
}
```

### Directory Structure Explanation

- **Language directories** (`go/`, `python/`, etc.) - Group frameworks by language
- **Framework directories** (`go/gin/`, `python/flask/`, etc.) - Individual framework implementations
- **Dockerfile** - Defines the container environment for each framework
- **config.yaml** - Contains framework metadata and configuration
- **.Makefile** - Defines build, test, and benchmark commands specific to each framework
- **.results/** - Directory for storing raw benchmark results

---

## Viewing Results

### Online Dashboards

- **Main Website**: [https://web-frameworks-benchmark.netlify.app](https://web-frameworks-benchmark.netlify.app)
- **Alternative Dashboard**: [https://the-benchmarker.github.io](https://the-benchmarker.github.io)

Features:
- Sort and filter frameworks by language, RPS, latency, etc.
- Compare frameworks side-by-side
- View historical performance trends
- Detailed per-framework metrics
- Export data as JSON/CSV

### Local Viewing

To view and analyze results locally:

```bash
# Query top 10 frameworks by RPS at 64 connections
python3 << 'EOF'
import json

with open('data.json') as f:
    data = json.load(f)

frameworks = []
for name, fw in data['frameworks'].items():
    if '64' in fw['results']:
        rps = fw['results']['64'].get('rps', 0)
        frameworks.append((name, rps, fw['language']))

frameworks.sort(key=lambda x: x[1], reverse=True)

print("Top 10 Frameworks by RPS (64 connections):")
print("-" * 60)
for name, rps, lang in frameworks[:10]:
    print(f"{lang:15} {name:30} {rps:>10,} req/s")
EOF
```

### Generate Summary Report

```bash
python3 << 'EOF'
import json
from collections import defaultdict

with open('data.json') as f:
    data = json.load(f)

# Group by language
languages = defaultdict(list)
for name, fw in data['frameworks'].items():
    languages[fw['language']].append((name, fw))

print("Framework Count by Language:")
print("-" * 40)
for lang in sorted(languages.keys()):
    count = len(languages[lang])
    print(f"{lang:20} {count:3} frameworks")

print(f"\nTotal: {len(data['frameworks'])} frameworks")
EOF
```

---

## Troubleshooting

### Common Issues

**Docker build fails**:
- Check the Dockerfile syntax
- Ensure all required files are included
- Verify base image exists and is accessible

**Tests fail**:
- Verify endpoints are implemented correctly
- Check that the server is listening on the correct port
- Ensure the Docker container is running

**Benchmark collection fails**:
- Install required benchmarking tools (wrk, bombardier)
- Check that the server can handle the load
- Increase warmup time if needed

### Debugging

```bash
# View logs for a running container
cd LANGUAGE/FRAMEWORK
docker logs <container_name>

# Run tests manually
curl -v http://localhost:3000/
curl -v http://localhost:3000/user/123

# Test with wrk manually
wrk -t12 -c400 -d30s http://localhost:3000/
```

---

## License

This project is licensed under the **MIT License** - see the [LICENSE](LICENSE) file for details.

You are free to:
- Use, copy, modify, merge, publish, distribute
- Sublicense and/or sell copies
- Use for commercial purposes

Under the following conditions:
- Include the copyright notice and license in all copies
- The software is provided "AS IS" without warranty

---

## Support & Community

- **Issues**: [GitHub Issues](https://github.com/the-benchmarker/web-frameworks/issues)
- **Discussions**: [GitHub Discussions](https://github.com/the-benchmarker/web-frameworks/discussions)
- **Email**: the-benchmarker@googlegroups.com
- **Website**: [https://the-benchmarker.github.io](https://the-benchmarker.github.io)

---

## Acknowledgments

### Contributors

Thank you to all contributors who have helped make this project possible! See the full list on [GitHub Contributors](https://github.com/the-benchmarker/web-frameworks/graphs/contributors).

### Framework Authors

Special thanks to all framework maintainers who create and maintain these amazing tools. Without your work, this comparison would not be possible.

### Inspiration

This project is inspired by:
- [TechEmpower Web Framework Benchmarks](https://www.techempower.com/benchmarks/) - The original web framework benchmark
- [Go Web Framework Benchmark](https://github.com/smallnest/go-web-framework-benchmark)

---

*Last updated: July 1, 2026*

*Maintained with care by the Web Frameworks Benchmark Team*

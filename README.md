- `ruby`, all tools are made in `ruby`

```sh
sudo dnf install ruby
```

- `wrk`, results are collected using `wrk`

```sh
cd `mktemp -d` && git clone https://github.com/wg/wrk -b 4.2.0 . && make && sudo mv wrk /usr/bin/
```

- `postgresql`, results are stored in `postgresql`

```sh
sudo dnf install postgresql{,-server}
```

- `docker`, each implementation is implemented in an isolated **container**
- `jq`, processing `docker` metadata
- `docker-machine` if you are on `macos`

You can start and configure a postgres from docker via

```sh
docker run  --name postgres -v /var/run/postgresql:/var/run/postgresql -e POSTGRES_PASSWORD=postgres -e POSTGRES_HOST_AUTH_METHOD=trust -d -p 5432:5432 postgres
dropdb -U postgres benchmark
createdb -U postgres benchmark
psql -U postgres -d benchmark < dump.sql
```

## Usage

- Setup

```
bundle install
bundle exec rake config
```

- Build

:warning: On `macos`, you need to use `docker-machine` to allow `docker` usage
for each framework :warning:

```
docker-machine rm default --force
docker-machine create default
eval $(docker-machine env default)
```

```
export FRAMEWORK=php/lumen; make -f $FRAMEWORK/.Makefile build
```

- Run

```
export FRAMEWORK=php/lumen; make -f $FRAMEWORK/.Makefile collect
```

:warning: You need to be on the project main directory :warning:

## CI/CD Matrix System

This repository uses a comprehensive nested CI matrix system that tests frameworks across three levels:
1. **Languages** (Python, JavaScript, Java, Go, Ruby, PHP, etc.)
2. **Frameworks** (Django, Express, Spring Boot, Gin, etc.)
3. **Runtime Versions** (Python 3.9-3.13, Node 18-22, Java 11-21, etc.)

For detailed information:
- 📖 [Complete Matrix System Documentation](.github/MATRIX_SYSTEM.md)
- 🚀 [Quick Reference Guide](.github/MATRIX_QUICKREF.md)
- 🔄 [Migration Guide](.github/MIGRATION_GUIDE.md)

Key features:
- **Selective Testing**: PRs test only default runtime versions by default
- **Smart Caching**: Language-specific dependency caching
- **Flexible Configuration**: Easy to add new languages, frameworks, and runtime versions
- **Optimization**: Manages resources with parallel job limits and exclusions

## Contributing a new benchmark

Please see [CONTRIBUTING.md](CONTRIBUTING.md).

## Results

Please take a look at https://web-frameworks-benchmark.vercel.app/result

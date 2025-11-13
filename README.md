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

## Contributing a new benchmark

Please see [CONTRIBUTING.md](CONTRIBUTING.md).

## Results

Please take a look at https://web-frameworks-benchmark.netlify.app/result

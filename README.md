# Web framework benchmarks

## Required tools

- `ruby`, all tools are made in `ruby`

    ```sh
    sudo dnf install ruby
    ```

- `wrk` -  results are collected using `wrk`

    ```sh
    cd `mktemp -d` && git clone https://github.com/wg/wrk -b 4.2.0 . && make && sudo mv wrk /usr/bin/
    ```

- `postgresql` - results are stored in `postgresql`

    ```sh
    sudo dnf install postgresql{,-server}
    ```

    Or you can start and configure a Postgres from docker via

    ```sh
    docker run -d -p 5432:5432 \
        --name postgres \
        -v /var/run/postgresql:/var/run/postgresql \
        -e POSTGRES_HOST_AUTH_METHOD=trust \
        -e POSTGRES_USER=postgres \
        -e POSTGRES_DB=benchmark \
        postgres:18-trixie
    docker exec -i postgres psql -U postgres -d benchmark < dump.sql
    ```

- `docker` - each implementation is implemented in an isolated **container**
- `jq` - processing `docker` metadata
- `docker-machine` - required if you are on `macOS`

## Usage

### Setup

```sh
bundle install
bundle exec rake config
```

### Build

:warning: On `macOS`, you need to use `docker-machine` to allow `docker` usage
for each framework :warning:

```sh
docker-machine rm default --force
docker-machine create default
eval $(docker-machine env default)
```

```sh
export FRAMEWORK=php/lumen; make -f $FRAMEWORK/.Makefile build
```

### Run

```sh
export FRAMEWORK=php/lumen; make -f $FRAMEWORK/.Makefile collect
```

:warning: You need to be on the project main directory :warning:

## Contributing a new benchmark

Please see [CONTRIBUTING.md](CONTRIBUTING.md).

## Results

Please take a look at <https://web-frameworks-benchmark.vercel.app/result>

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
export FRAMEWORK=php/lumen
cd ${FRAMEWORK}
make -f .Makefile build
```

- Run

```
make -f ${FRAMEWORK}/.Makefile collect
```

:warning: You need to be on the project main directory :warning:

## Results

Please take a look at https://web-frameworks-benchmark.netlify.app/result

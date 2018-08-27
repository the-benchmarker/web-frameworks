# Which is the fastest?

[![Build Status](https://travis-ci.com/tbrand/which_is_the_fastest.svg?branch=master)](https://travis-ci.com/tbrand/which_is_the_fastest)
[![Join the chat at https://gitter.im/which_is_the_fastest/Lobby](https://badges.gitter.im/which_is_the_fastest/Lobby.svg)](https://gitter.im/which_is_the_fastest/Lobby)

This project aims to be a load benchmarking suite, no more, no less

> Measuring response times (routing times) for each framework (middleware).


<div align="center">
  :warning::warning::warning::warning::warning::warning::warning::warning:
</div>

<div align="center">Results are not <b>production-ready</b> <i>yet</i></div>

<div align="center">
  :warning::warning::warning::warning::warning::warning::warning::warning:
</div>

### Additional purposes :

+ Helping decide beetween languages, depending on use case
+ Learning languages, best practices, devops culture ...
+ Having fun :heart:

## Requirements

+ [Crystal](https://crystal-lang.org) as `built-in` tools are made in this language
+ [Docker](https://www.docker.com) as **frameworks** are `isolated` into _containers_

:warning: `docker` is used for **development** purpose, `production` results will be computed on [DigitalOcean](https://www.digitalocean.com) :warning:

## Usage

+ Install all dependencies

~~~sh
shards install
~~~

+ Build internal tools

~~~sh
shards build
~~~

+ Build containers

> job is either a language (example : crystal) or a framework (example : router.cr)

~~~sh
bin/neph [job]
~~~

+ Start the benchmark ....

> tools is a list of language / framework to challenge (example : ruby kemal amber go python)

~~~sh
bin/benchmarker [tools]
~~~

## Results

<!-- Result from here -->
Last update: 2018-08-27
```
OS: Linux (version: 4.17.11-200.fc28.x86_64, arch: x86_64)
CPU Cores: 4
```

### Latency


#### Ranking (top 5)


+ vibora (python)


+ polka (node)


+ rayo (node)


+ roda (ruby)


+ fastify (node)


+ rack-routing (ruby)


#### Full table

| Language (Runtime)        | Framework (Middleware)    |         Average |  50% percentile |  90% percentile |  99% percentile | 99.9% percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| python                    | vibora                    | 0.21 ms | 0.08 ms | 0.29 ms | 3.17 ms | 9.55 ms | 598.00 | 
| node                      | polka                     | 0.46 ms | 0.13 ms | 0.59 ms | 7.38 ms | 23.18 ms | 1479.00 | 
| node                      | rayo                      | 0.52 ms | 0.14 ms | 0.50 ms | 10.36 ms | 36.59 ms | 2079.33 | 
| ruby                      | roda                      | 0.80 ms | 0.15 ms | 2.69 ms | 7.19 ms | 16.35 ms | 1592.67 | 
| node                      | fastify                   | 0.58 ms | 0.17 ms | 0.74 ms | 9.53 ms | 30.97 ms | 1907.00 | 
| ruby                      | rack-routing              | 1.03 ms | 0.19 ms | 2.98 ms | 11.23 ms | 20.40 ms | 2151.33 | 
| node                      | koa                       | 0.54 ms | 0.19 ms | 0.55 ms | 9.57 ms | 29.56 ms | 1790.33 | 
| node                      | express                   | 0.52 ms | 0.23 ms | 0.52 ms | 7.47 ms | 30.91 ms | 1717.33 | 
| node                      | restify                   | 0.62 ms | 0.24 ms | 1.21 ms | 7.02 ms | 15.43 ms | 1271.67 | 
| node                      | hapi                      | 0.64 ms | 0.28 ms | 1.30 ms | 7.01 ms | 20.87 ms | 1346.67 | 
| ruby                      | hanami                    | 1.15 ms | 0.32 ms | 3.51 ms | 9.81 ms | 21.34 ms | 2039.67 | 
| ruby                      | flame                     | 1.17 ms | 0.32 ms | 3.47 ms | 9.76 ms | 17.91 ms | 1977.33 | 
| ruby                      | sinatra                   | 1.27 ms | 0.39 ms | 3.54 ms | 11.70 ms | 18.96 ms | 2245.67 | 
| python                    | flask                     | 0.64 ms | 0.57 ms | 0.99 ms | 1.67 ms | 6.84 ms | 434.67 | 
| python                    | sanic                     | 0.74 ms | 0.57 ms | 1.27 ms | 3.12 ms | 8.85 ms | 585.00 | 
| python                    | django                    | 0.87 ms | 0.89 ms | 1.21 ms | 1.90 ms | 5.52 ms | 338.67 | 
| ruby                      | rails                     | 3.90 ms | 1.56 ms | 10.25 ms | 21.15 ms | 36.78 ms | 4630.33 | 
| python                    | tornado                   | 1.61 ms | 1.70 ms | 1.97 ms | 3.37 ms | 7.42 ms | 597.67 | 
| php                       | symfony                   | 55.88 ms | 53.78 ms | 71.28 ms | 84.99 ms | 86.34 ms | 10939.67 | 
| php                       | laravel                   | 97.81 ms | 95.35 ms | 123.80 ms | 139.49 ms | 139.52 ms | 19140.33 | 

### Requests per second


#### Ranking (top 5)


+ vibora (python)


+ polka (node)


+ rayo (node)


+ fastify (node)


+ koa (node)


+ roda (ruby)


#### Full table

| Language (Runtime)        | Framework (Middleware)    |    Requests / s |      Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python                    | vibora                    | 104830.67 | 8.67 MB |
| node                      | polka                     | 59401.00 | 6.49 MB |
| node                      | rayo                      | 58398.00 | 6.38 MB |
| node                      | fastify                   | 50224.00 | 8.61 MB |
| node                      | koa                       | 43627.67 | 6.73 MB |
| ruby                      | roda                      | 41857.67 | 2.91 MB |
| node                      | express                   | 40076.00 | 7.15 MB |
| node                      | hapi                      | 32456.00 | 5.23 MB |
| node                      | restify                   | 31798.67 | 4.07 MB |
| ruby                      | rack-routing              | 31241.00 | 1.32 MB |
| ruby                      | hanami                    | 20532.67 | 11.34 MB |
| ruby                      | flame                     | 18951.33 | 0.80 MB |
| ruby                      | sinatra                   | 17135.33 | 3.25 MB |
| python                    | flask                     | 16079.00 | 2.89 MB |
| python                    | sanic                     | 14312.67 | 1.86 MB |
| python                    | django                    | 11087.67 | 2.35 MB |
| python                    | tornado                   | 6155.33 | 1.18 MB |
| ruby                      | rails                     | 3685.67 | 0.83 MB |
| php                       | symfony                   | 168.67 | 0.04 MB |
| php                       | laravel                   | 92.67 | 0.06 MB |
<!-- Result till here -->

## How to contribute ?

In any way you want ...

+ Request a framework addition
+ Report a bug (on any implementation)
+ Suggest an idea
+ ...

Any kind of idea is :heart:

## Contributors

- [Taichiro Suzuki](https://github.com/tbrand) - Author, maintainer
- [OvermindDL1](https://github.com/OvermindDL1) - Maintainer
- [Marwan Rabbâa](https://github.com/waghanza) - Mainainer

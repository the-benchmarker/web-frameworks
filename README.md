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
Last update: 2018-07-20
```
OS: Linux (version: 4.17.6-200.fc28.x86_64, arch: x86_64)
CPU Cores: 4
```

<details open><summary>Ranked by latency</summary>

| Language (Runtime)        | Framework (Middleware)    |         Average |  50% percentile |  90% percentile |  99% percentile | 99.9% percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| ruby                      | roda                      | 1.59 ms | 0.16 ms | 3.02 ms | 28.33 ms | 101.66 ms | 5198.00 | 
| ruby                      | rack-routing              | 1.84 ms | 1.08 ms | 2.18 ms | 28.07 ms | 130.81 ms | 5425.00 | 
| ruby                      | flame                     | 3.00 ms | 0.93 ms | 8.28 ms | 24.62 ms | 71.68 ms | 5029.00 | 
| ruby                      | hanami                    | 3.14 ms | 0.22 ms | 9.13 ms | 42.37 ms | 100.27 ms | 7922.00 | 
| ruby                      | sinatra                   | 3.61 ms | 0.24 ms | 11.08 ms | 44.20 ms | 103.81 ms | 8487.00 | 
| ruby                      | rails                     | 14.06 ms | 1.05 ms | 52.24 ms | 99.23 ms | 203.17 ms | 23938.00 | 
| node                      | restify                   | 32.49 ms | 19.87 ms | 43.20 ms | 400.58 ms | 1266.70 ms | 73915.00 | 
| node                      | rayo                      | 39.92 ms | 15.70 ms | 38.90 ms | 821.52 ms | 1726.76 ms | 129911.00 | 
| node                      | fastify                   | 31.57 ms | 15.43 ms | 36.02 ms | 549.57 ms | 1609.57 ms | 96331.00 | 
| python                    | flask                     | 51.37 ms | 50.32 ms | 71.78 ms | 89.10 ms | 383.47 ms | 18486.00 | 
| python                    | sanic                     | 63.06 ms | 60.35 ms | 83.43 ms | 130.32 ms | 1005.81 ms | 38402.00 | 
| node                      | koa                       | 78.22 ms | 23.41 ms | 54.07 ms | 1648.81 ms | 2768.98 ms | 264003.00 | 
| node                      | express                   | 63.87 ms | 25.51 ms | 48.81 ms | 1252.40 ms | 2637.24 ms | 206667.00 | 
| node                      | polka                     | 54.91 ms | 17.57 ms | 42.20 ms | 1272.04 ms | 2538.94 ms | 204262.00 | 
| python                    | django                    | 93.40 ms | 75.81 ms | 107.38 ms | 777.18 ms | 1925.41 ms | 129774.00 | 
| php                       | laravel                   | 217.18 ms | 0.26 ms | 628.92 ms | 3182.26 ms | 7039.96 ms | 625624.00 | 
| php                       | symfony                   | 249.75 ms | 0.66 ms | 850.91 ms | 3675.66 ms | 7040.82 ms | 716615.00 | 
| python                    | tornado                   | 369.06 ms | 122.32 ms | 253.90 ms | 6116.78 ms | 7813.33 ms | 1006422.00 | 

</details>


<details><summary>Ranked by requests</summary>

| Language (Runtime)        | Framework (Middleware)    |    Requests / s |
|---------------------------|---------------------------|----------------:|
| node                      | rayo                      | 51459.00 |
| node                      | fastify                   | 52418.00 |
| node                      | polka                     | 48305.00 |
| node                      | restify                   | 42281.00 |
| ruby                      | roda                      | 42352.00 |
| node                      | koa                       | 36132.00 |
| php                       | symfony                   | 35590.00 |
| ruby                      | rack-routing              | 36539.00 |
| node                      | express                   | 34199.00 |
| php                       | laravel                   | 31501.00 |
| ruby                      | flame                     | 21254.00 |
| python                    | flask                     | 19211.00 |
| ruby                      | hanami                    | 20321.00 |
| ruby                      | sinatra                   | 17641.00 |
| python                    | sanic                     | 15855.00 |
| python                    | django                    | 12172.00 |
| python                    | tornado                   | 7133.00 |
| ruby                      | rails                     | 4529.00 |

</details>

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

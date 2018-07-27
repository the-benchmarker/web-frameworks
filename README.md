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
Last update: 2018-07-27
```
OS: Linux (version: 4.16.11-100.fc26.x86_64, arch: x86_64)
CPU Cores: 8
```

<details open><summary>Ranked by latency (ordered by 50th percentile - lowest is better)</summary> 

| Language (Runtime)        | Framework (Middleware)    |         Average |  50% percentile |  90% percentile |  99% percentile | 99.9% percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust                      | nickel                    | 0.10 ms | 0.10 ms | 0.13 ms | 0.29 ms | 6.09 ms | 90.00 | 
| rust                      | rocket                    | 0.14 ms | 0.12 ms | 0.17 ms | 0.94 ms | 10.57 ms | 185.33 | 
| rust                      | iron                      | 0.55 ms | 0.44 ms | 0.97 ms | 2.64 ms | 25.61 ms | 568.67 | 
| ruby                      | roda                      | 2.87 ms | 1.89 ms | 5.96 ms | 18.51 ms | 98.04 ms | 3741.33 | 
| ruby                      | rack-routing              | 3.89 ms | 2.39 ms | 8.31 ms | 28.54 ms | 119.28 ms | 5734.67 | 
| php                       | symfony                   | 165.73 ms | 3.81 ms | 447.44 ms | 2242.69 ms | 6767.43 ms | 501711.00 | 
| ruby                      | flame                     | 5.80 ms | 4.01 ms | 13.22 ms | 25.76 ms | 96.59 ms | 5618.67 | 
| rust                      | actix-web                 | 4.88 ms | 4.28 ms | 9.10 ms | 14.78 ms | 33.15 ms | 3161.00 | 
| ruby                      | hanami                    | 7.04 ms | 4.29 ms | 17.00 ms | 38.64 ms | 108.35 ms | 8134.67 | 
| php                       | laravel                   | 227.99 ms | 4.50 ms | 639.42 ms | 3343.41 ms | 6769.09 ms | 665383.00 | 
| ruby                      | sinatra                   | 6.88 ms | 4.74 ms | 15.83 ms | 31.13 ms | 100.51 ms | 6791.33 | 
| go                        | fasthttprouter            | 5.82 ms | 5.01 ms | 8.67 ms | 16.98 ms | 234.42 ms | 6260.00 | 
| cpp                       | evhtp                     | 6.71 ms | 5.62 ms | 11.61 ms | 20.50 ms | 142.05 ms | 5456.67 | 
| python                    | vibora                    | 6.70 ms | 5.86 ms | 12.63 ms | 21.33 ms | 46.85 ms | 4518.00 | 
| nim                       | jester                    | 7.37 ms | 6.54 ms | 11.69 ms | 20.19 ms | 79.07 ms | 3724.00 | 
| ruby                      | rails                     | 27.98 ms | 7.30 ms | 88.25 ms | 169.48 ms | 340.07 ms | 40301.33 | 
| go                        | echo                      | 9.12 ms | 8.14 ms | 14.01 ms | 27.50 ms | 230.04 ms | 6470.00 | 
| java                      | act                       | 9.58 ms | 8.30 ms | 14.75 ms | 29.99 ms | 195.24 ms | 7261.00 | 
| go                        | iris                      | 9.46 ms | 8.56 ms | 14.37 ms | 26.23 ms | 210.95 ms | 5069.00 | 
| csharp                    | aspnetcore                | 10.35 ms | 9.06 ms | 16.54 ms | 26.00 ms | 170.00 ms | 5633.00 | 
| scala                     | akkahttp                  | 187.12 ms | 9.21 ms | 66.14 ms | 4111.61 ms | 7187.02 ms | 736735.00 | 
| go                        | gorilla-mux               | 11.19 ms | 9.78 ms | 17.28 ms | 32.09 ms | 202.95 ms | 8184.67 | 
| nim                       | mofuw                     | 14.88 ms | 10.22 ms | 25.19 ms | 85.66 ms | 270.44 ms | 15585.00 | 
| node                      | fastify                   | 23.75 ms | 15.59 ms | 30.39 ms | 238.82 ms | 955.25 ms | 50483.33 | 
| node                      | polka                     | 30.56 ms | 16.01 ms | 42.86 ms | 412.55 ms | 1257.64 ms | 75792.00 | 
| swift                     | vapor                     | 46.80 ms | 16.24 ms | 30.45 ms | 1051.57 ms | 2898.98 ms | 187915.33 | 
| swift                     | perfect                   | 16.51 ms | 16.60 ms | 18.57 ms | 21.08 ms | 230.05 ms | 5712.33 | 
| node                      | rayo                      | 26.62 ms | 17.35 ms | 44.07 ms | 187.76 ms | 872.49 ms | 44607.00 | 
| node                      | koa                       | 48.69 ms | 22.64 ms | 50.81 ms | 872.66 ms | 1898.34 ms | 141229.33 | 
| node                      | restify                   | 32.64 ms | 23.53 ms | 49.41 ms | 216.69 ms | 826.53 ms | 44182.67 | 
| elixir                    | plug                      | 27.40 ms | 25.45 ms | 44.55 ms | 128.54 ms | 855.51 ms | 40143.67 | 
| node                      | express                   | 43.40 ms | 25.83 ms | 58.20 ms | 562.65 ms | 1488.38 ms | 93715.67 | 
| elixir                    | phoenix                   | 30.72 ms | 26.56 ms | 44.88 ms | 222.75 ms | 1238.23 ms | 63119.00 | 
| go                        | gin                       | 54.53 ms | 26.80 ms | 153.32 ms | 278.19 ms | 598.91 ms | 65446.33 | 
| swift                     | kitura                    | 29.67 ms | 29.12 ms | 34.86 ms | 43.53 ms | 320.79 ms | 6973.67 | 
| crystal                   | router.cr                 | 30.40 ms | 29.88 ms | 38.13 ms | 45.87 ms | 165.94 ms | 6685.67 | 
| crystal                   | spider-gazelle            | 32.37 ms | 31.23 ms | 41.10 ms | 49.82 ms | 316.90 ms | 9326.00 | 
| node                      | hapi                      | 73.61 ms | 32.68 ms | 65.04 ms | 1161.22 ms | 2113.80 ms | 189905.33 | 
| crystal                   | lucky                     | 35.75 ms | 35.80 ms | 45.09 ms | 54.01 ms | 267.71 ms | 11580.67 | 
| crystal                   | amber                     | 36.84 ms | 37.09 ms | 44.19 ms | 52.31 ms | 81.25 ms | 6548.67 | 
| crystal                   | kemal                     | 40.37 ms | 39.85 ms | 47.08 ms | 54.43 ms | 168.43 ms | 6473.67 | 
| python                    | flask                     | 47.74 ms | 40.88 ms | 75.94 ms | 113.78 ms | 390.30 ms | 21925.67 | 
| python                    | sanic                     | 57.98 ms | 52.69 ms | 101.48 ms | 165.78 ms | 314.62 ms | 33554.33 | 
| python                    | django                    | 77.66 ms | 69.47 ms | 124.84 ms | 158.27 ms | 803.78 ms | 35987.00 | 
| python                    | tornado                   | 138.65 ms | 117.52 ms | 164.78 ms | 872.52 ms | 1859.92 ms | 147676.67 | 

</details>


<details><summary>Ranked by requests (ordered by number or requests per sencond - highest is better)</summary>

| Language (Runtime)        | Framework (Middleware)    |    Requests / s |      Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| rust                      | actix-web                 | 189871.00 | 215.94 MB |
| go                        | fasthttprouter            | 164166.67 | 264.76 MB |
| python                    | vibora                    | 155670.67 | 176.47 MB |
| nim                       | jester                    | 144966.67 | 291.49 MB |
| cpp                       | evhtp                     | 143894.33 | 139.70 MB |
| java                      | act                       | 115294.00 | 197.19 MB |
| go                        | echo                      | 106699.33 | 187.40 MB |
| rust                      | iron                      | 106594.00 | 134.29 MB |
| go                        | iris                      | 102059.67 | 136.75 MB |
| rust                      | rocket                    | 99168.00 | 156.98 MB |
| csharp                    | aspnetcore                | 95442.67 | 155.59 MB |
| go                        | gorilla-mux               | 89525.33 | 119.42 MB |
| rust                      | nickel                    | 87305.33 | 173.50 MB |
| nim                       | mofuw                     | 80276.00 | 140.91 MB |
| scala                     | akkahttp                  | 60510.33 | 130.02 MB |
| swift                     | perfect                   | 59441.33 | 55.91 MB |
| node                      | fastify                   | 56379.00 | 134.12 MB |
| swift                     | vapor                     | 52937.00 | 71.04 MB |
| php                       | symfony                   | 50216.00 | 249.96 MB |
| node                      | polka                     | 49832.33 | 74.70 MB |
| php                       | laravel                   | 48597.00 | 242.13 MB |
| node                      | rayo                      | 45783.00 | 68.63 MB |
| ruby                      | roda                      | 44426.33 | 42.42 MB |
| elixir                    | plug                      | 40222.00 | 86.95 MB |
| elixir                    | phoenix                   | 38897.33 | 84.14 MB |
| node                      | koa                       | 37386.67 | 79.21 MB |
| node                      | restify                   | 35437.33 | 62.22 MB |
| ruby                      | rack-routing              | 33696.33 | 19.45 MB |
| swift                     | kitura                    | 33015.67 | 61.31 MB |
| crystal                   | router.cr                 | 32586.33 | 30.56 MB |
| node                      | express                   | 32214.33 | 78.93 MB |
| crystal                   | spider-gazelle            | 30427.33 | 32.79 MB |
| go                        | gin                       | 29183.67 | 51.09 MB |
| crystal                   | lucky                     | 27802.00 | 34.20 MB |
| crystal                   | amber                     | 27198.33 | 39.41 MB |
| node                      | hapi                      | 26922.00 | 59.23 MB |
| crystal                   | kemal                     | 25061.00 | 40.81 MB |
| ruby                      | flame                     | 21956.33 | 12.67 MB |
| python                    | flask                     | 20832.33 | 51.29 MB |
| ruby                      | sinatra                   | 18482.00 | 48.02 MB |
| ruby                      | hanami                    | 18177.67 | 137.81 MB |
| python                    | sanic                     | 17628.67 | 31.42 MB |
| python                    | django                    | 12730.33 | 36.87 MB |
| python                    | tornado                   | 8083.33 | 21.41 MB |
| ruby                      | rails                     | 4582.33 | 13.96 MB |

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

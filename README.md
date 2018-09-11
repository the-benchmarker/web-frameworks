# Which is the fastest?

[![Build Status](https://travis-ci.com/the-benchmarker/web-frameworks.svg?branch=master)](https://travis-ci.com/the-benchmarker/web-frameworks)
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
Last update: 2018-09-12
```
OS: Linux (version: 4.16.11-100.fc26.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: rocket (rust)


:two: nickel (rust)


:three: iron (rust)


:four: roda (ruby)


:five: rack-routing (ruby)


#### Full table

| Language (Runtime)        | Framework (Middleware)    |         Average |  50% percentile |  90% percentile |  99% percentile | 99.9% percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust                      | rocket                    | 0.13 ms | 0.11 ms | 0.17 ms | 0.60 ms | 8.43 ms | 156.00 | 
| rust                      | nickel                    | 0.11 ms | 0.12 ms | 0.15 ms | 0.19 ms | 5.59 ms | 78.67 | 
| rust                      | iron                      | 0.56 ms | 0.47 ms | 0.92 ms | 2.64 ms | 27.19 ms | 561.00 | 
| ruby                      | roda                      | 2.90 ms | 2.05 ms | 6.06 ms | 15.21 ms | 75.65 ms | 3081.33 | 
| ruby                      | rack-routing              | 3.63 ms | 2.47 ms | 8.11 ms | 18.74 ms | 67.40 ms | 3859.67 | 
| ruby                      | hanami                    | 5.83 ms | 3.39 ms | 14.05 ms | 36.59 ms | 108.67 ms | 7479.67 | 
| ruby                      | sinatra                   | 7.56 ms | 4.30 ms | 18.49 ms | 43.87 ms | 128.27 ms | 9189.00 | 
| ruby                      | flame                     | 6.43 ms | 4.35 ms | 14.81 ms | 30.40 ms | 103.07 ms | 6524.67 | 
| rust                      | actix-web                 | 5.06 ms | 4.39 ms | 9.26 ms | 15.63 ms | 102.85 ms | 3637.00 | 
| php                       | symfony                   | 246.26 ms | 4.61 ms | 675.55 ms | 3779.48 ms | 6656.05 ms | 738533.33 | 
| php                       | laravel                   | 284.88 ms | 4.77 ms | 336.10 ms | 5574.66 ms | 7436.76 ms | 997258.00 | 
| go                        | fasthttprouter            | 5.59 ms | 5.00 ms | 8.51 ms | 15.79 ms | 97.09 ms | 2782.67 | 
| cpp                       | evhtp                     | 7.23 ms | 6.19 ms | 12.96 ms | 22.46 ms | 80.28 ms | 4545.00 | 
| crystal                   | spider-gazelle            | 7.65 ms | 6.53 ms | 13.80 ms | 24.10 ms | 54.98 ms | 4808.67 | 
| python                    | vibora                    | 7.91 ms | 6.73 ms | 15.09 ms | 26.07 ms | 72.76 ms | 5632.67 | 
| ruby                      | rails                     | 29.92 ms | 6.86 ms | 96.53 ms | 184.94 ms | 373.31 ms | 44107.00 | 
| java                      | act                       | 9.48 ms | 8.18 ms | 14.36 ms | 27.74 ms | 278.72 ms | 9850.00 | 
| go                        | gorilla-mux               | 10.59 ms | 9.33 ms | 16.55 ms | 32.39 ms | 207.40 ms | 6519.67 | 
| go                        | echo                      | 11.04 ms | 9.71 ms | 17.09 ms | 32.45 ms | 231.18 ms | 6620.00 | 
| go                        | iris                      | 11.15 ms | 9.76 ms | 17.02 ms | 32.12 ms | 240.37 ms | 8796.33 | 
| scala                     | akkahttp                  | 203.64 ms | 10.15 ms | 31.72 ms | 4692.81 ms | 7932.67 ms | 815146.67 | 
| nim                       | mofuw                     | 32.14 ms | 10.59 ms | 60.58 ms | 369.61 ms | 737.84 ms | 69376.00 | 
| csharp                    | aspnetcore                | 13.15 ms | 11.05 ms | 21.70 ms | 38.19 ms | 381.55 ms | 11433.00 | 
| node                      | rayo                      | 21.41 ms | 13.54 ms | 31.99 ms | 177.80 ms | 869.77 ms | 43144.67 | 
| node                      | fastify                   | 29.21 ms | 15.93 ms | 34.05 ms | 423.82 ms | 1194.79 ms | 74594.00 | 
| swift                     | perfect                   | 18.20 ms | 17.81 ms | 21.83 ms | 27.04 ms | 116.83 ms | 3229.33 | 
| node                      | polka                     | 33.06 ms | 18.57 ms | 47.62 ms | 432.52 ms | 1342.22 ms | 79283.67 | 
| swift                     | vapor                     | 51.34 ms | 18.74 ms | 38.84 ms | 1146.76 ms | 3176.61 ms | 191806.00 | 
| python                    | bottle                    | 22.86 ms | 18.84 ms | 37.52 ms | 80.38 ms | 269.89 ms | 15440.67 | 
| node                      | koa                       | 32.86 ms | 20.42 ms | 43.85 ms | 351.85 ms | 1177.41 ms | 68220.33 | 
| node                      | restify                   | 28.12 ms | 21.50 ms | 43.65 ms | 110.05 ms | 675.58 ms | 30650.67 | 
| python                    | japronto                  | 22.11 ms | 21.62 ms | 23.88 ms | 28.90 ms | 474.03 ms | 9940.33 | 
| scala                     | http4s                    | 36.19 ms | 23.47 ms | 47.25 ms | 356.82 ms | 2154.62 ms | 108500.33 | 
| node                      | express                   | 46.61 ms | 26.97 ms | 59.77 ms | 650.93 ms | 1655.67 ms | 108274.00 | 
| go                        | gin                       | 52.25 ms | 27.60 ms | 148.41 ms | 218.76 ms | 471.25 ms | 58524.33 | 
| crystal                   | router.cr                 | 30.44 ms | 30.39 ms | 39.56 ms | 48.22 ms | 133.96 ms | 7575.67 | 
| crystal                   | raze                      | 31.36 ms | 31.20 ms | 39.88 ms | 49.35 ms | 333.32 ms | 11685.33 | 
| crystal                   | prism                     | 33.50 ms | 31.78 ms | 42.41 ms | 52.59 ms | 265.61 ms | 9857.33 | 
| crystal                   | raze                      | 31.36 ms | 31.20 ms | 39.88 ms | 49.35 ms | 333.32 ms | 11685.33 | 
| crystal                   | kemal                     | 35.24 ms | 32.03 ms | 46.84 ms | 55.92 ms | 126.00 ms | 7853.33 | 
| node                      | hapi                      | 72.10 ms | 37.05 ms | 79.10 ms | 1132.31 ms | 2313.01 ms | 180855.00 | 
| swift                     | kitura                    | 41.51 ms | 39.48 ms | 55.87 ms | 84.80 ms | 333.57 ms | 14253.67 | 
| crystal                   | lucky                     | 39.62 ms | 40.05 ms | 48.46 ms | 55.19 ms | 174.04 ms | 8550.33 | 
| crystal                   | amber                     | 38.81 ms | 40.24 ms | 50.11 ms | 57.06 ms | 127.58 ms | 9032.00 | 
| python                    | aiohttp                   | 48.59 ms | 42.67 ms | 86.06 ms | 144.89 ms | 291.83 ms | 29504.33 | 
| python                    | flask                     | 64.32 ms | 49.22 ms | 116.01 ms | 195.21 ms | 367.72 ms | 37851.00 | 
| python                    | sanic                     | 69.02 ms | 61.77 ms | 123.49 ms | 203.17 ms | 429.70 ms | 41482.00 | 
| python                    | django                    | 91.95 ms | 83.36 ms | 136.10 ms | 210.84 ms | 918.58 ms | 42665.00 | 
| python                    | tornado                   | 152.13 ms | 123.11 ms | 208.29 ms | 1025.72 ms | 2440.67 ms | 175368.67 | 
| python                    | quart                     | 202.98 ms | 175.52 ms | 341.33 ms | 514.32 ms | 696.58 ms | 99105.00 | 

#### Ranking (top 5)


:one: actix-web (rust)


:two: fasthttprouter (go)


:three: vibora (python)


:four: evhtp (cpp)


:five: spider-gazelle (crystal)


#### Full table

| Language (Runtime)        | Framework (Middleware)    |    Requests / s |      Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| rust                      | actix-web                 | 185224.00 | 210.60 MB |
| go                        | fasthttprouter            | 166349.67 | 268.18 MB |
| python                    | vibora                    | 134891.33 | 153.16 MB |
| cpp                       | evhtp                     | 133382.33 | 129.41 MB |
| crystal                   | spider-gazelle            | 130798.33 | 139.86 MB |
| java                      | act                       | 116036.00 | 198.49 MB |
| rust                      | rocket                    | 105185.67 | 163.79 MB |
| rust                      | iron                      | 104854.33 | 131.70 MB |
| go                        | gorilla-mux               | 93923.33 | 125.25 MB |
| go                        | echo                      | 90065.33 | 157.99 MB |
| go                        | iris                      | 89619.00 | 120.20 MB |
| csharp                    | aspnetcore                | 78003.33 | 126.97 MB |
| nim                       | mofuw                     | 73060.33 | 128.33 MB |
| rust                      | nickel                    | 71579.00 | 142.19 MB |
| node                      | rayo                      | 60292.00 | 90.29 MB |
| swift                     | perfect                   | 54398.33 | 51.19 MB |
| node                      | fastify                   | 53210.00 | 131.86 MB |
| php                       | laravel                   | 53095.00 | 264.51 MB |
| php                       | symfony                   | 52647.00 | 262.11 MB |
| scala                     | akkahttp                  | 49990.00 | 107.41 MB |
| python                    | bottle                    | 45304.33 | 111.71 MB |
| python                    | japronto                  | 44861.67 | 53.50 MB |
| node                      | polka                     | 44553.33 | 66.79 MB |
| ruby                      | roda                      | 43400.67 | 41.47 MB |
| swift                     | vapor                     | 42705.67 | 72.94 MB |
| node                      | koa                       | 40604.67 | 86.00 MB |
| node                      | restify                   | 38839.67 | 68.21 MB |
| scala                     | http4s                    | 38424.67 | 67.35 MB |
| ruby                      | rack-routing              | 34877.00 | 20.16 MB |
| crystal                   | router.cr                 | 32653.67 | 30.62 MB |
| node                      | express                   | 31947.33 | 78.26 MB |
| crystal                   | raze                      | 31496.67 | 29.54 MB |
| crystal                   | prism                     | 30176.00 | 33.34 MB |
| crystal                   | raze                      | 31496.67 | 29.54 MB |
| go                        | gin                       | 29199.00 | 51.14 MB |
| crystal                   | kemal                     | 28165.67 | 45.93 MB |
| crystal                   | amber                     | 25716.00 | 37.29 MB |
| crystal                   | lucky                     | 25111.67 | 30.84 MB |
| node                      | hapi                      | 24308.33 | 63.06 MB |
| swift                     | kitura                    | 23816.33 | 44.24 MB |
| ruby                      | hanami                    | 21965.00 | 166.35 MB |
| python                    | aiohttp                   | 21317.33 | 48.30 MB |
| ruby                      | flame                     | 19945.67 | 11.52 MB |
| ruby                      | sinatra                   | 16865.67 | 43.82 MB |
| python                    | flask                     | 16012.00 | 39.39 MB |
| python                    | sanic                     | 14808.00 | 26.34 MB |
| python                    | django                    | 10675.00 | 30.91 MB |
| python                    | tornado                   | 7178.00 | 19.05 MB |
| python                    | quart                     | 4961.00 | 10.21 MB |
| ruby                      | rails                     | 4295.00 | 13.13 MB |
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

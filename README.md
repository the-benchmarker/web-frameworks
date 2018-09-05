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
Last update: 2018-09-05
```
OS: Linux (version: 4.16.11-100.fc26.x86_64, arch: x86_64)
CPU Cores: 8
```

<details open><summary>Ranked by latency (ordered by 50th percentile - lowest is better)</summary> 

| Language (Runtime)        | Framework (Middleware)    |         Average |  50% percentile |  90% percentile |  99% percentile | 99.9% percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust                      | nickel                    | 0.11 ms | 0.10 ms | 0.14 ms | 0.31 ms | 4.65 ms | 88.33 | 
| rust                      | rocket                    | 0.14 ms | 0.12 ms | 0.18 ms | 0.84 ms | 6.76 ms | 166.33 | 
| rust                      | iron                      | 0.55 ms | 0.45 ms | 0.97 ms | 2.63 ms | 27.26 ms | 546.00 | 
| ruby                      | roda                      | 2.85 ms | 2.02 ms | 6.16 ms | 14.05 ms | 52.37 ms | 2866.67 | 
| ruby                      | rack-routing              | 3.71 ms | 2.43 ms | 7.70 ms | 23.51 ms | 104.15 ms | 4875.67 | 
| ruby                      | hanami                    | 6.70 ms | 3.02 ms | 17.06 ms | 54.84 ms | 141.71 ms | 10685.67 | 
| php                       | symfony                   | 171.45 ms | 3.19 ms | 478.82 ms | 2457.75 ms | 6922.00 ms | 521488.00 | 
| php                       | laravel                   | 236.28 ms | 3.95 ms | 536.97 ms | 3821.15 ms | 6062.81 ms | 733340.33 | 
| ruby                      | flame                     | 6.08 ms | 4.17 ms | 13.94 ms | 27.66 ms | 106.33 ms | 6025.00 | 
| rust                      | actix-web                 | 4.75 ms | 4.21 ms | 8.70 ms | 13.72 ms | 89.51 ms | 3232.00 | 
| cpp                       | evhtp                     | 4.95 ms | 4.50 ms | 8.33 ms | 13.40 ms | 100.46 ms | 2816.33 | 
| go                        | fasthttprouter            | 5.63 ms | 5.11 ms | 8.39 ms | 15.22 ms | 48.25 ms | 2524.00 | 
| ruby                      | sinatra                   | 8.18 ms | 5.38 ms | 19.26 ms | 39.09 ms | 122.72 ms | 8492.33 | 
| crystal                   | spider-gazelle            | 6.75 ms | 5.74 ms | 11.81 ms | 19.89 ms | 43.22 ms | 3906.00 | 
| python                    | vibora                    | 6.81 ms | 5.96 ms | 12.52 ms | 19.83 ms | 44.06 ms | 4190.00 | 
| java                      | act                       | 8.28 ms | 7.25 ms | 12.05 ms | 21.25 ms | 219.79 ms | 6692.00 | 
| ruby                      | rails                     | 27.26 ms | 7.34 ms | 85.99 ms | 165.88 ms | 340.88 ms | 39236.67 | 
| csharp                    | aspnetcore                | 10.09 ms | 8.47 ms | 15.49 ms | 24.71 ms | 373.36 ms | 11616.33 | 
| go                        | iris                      | 9.27 ms | 8.48 ms | 13.92 ms | 25.47 ms | 129.27 ms | 4675.00 | 
| scala                     | akkahttp                  | 160.87 ms | 8.87 ms | 47.09 ms | 3699.45 ms | 6884.99 ms | 660280.67 | 
| go                        | echo                      | 10.75 ms | 9.09 ms | 15.62 ms | 31.55 ms | 383.23 ms | 12691.00 | 
| go                        | gorilla-mux               | 10.89 ms | 9.55 ms | 16.82 ms | 32.07 ms | 240.17 ms | 7384.33 | 
| nim                       | mofuw                     | 29.75 ms | 10.25 ms | 65.15 ms | 304.85 ms | 469.47 ms | 61466.67 | 
| python                    | bottle                    | 19.80 ms | 14.44 ms | 37.60 ms | 83.59 ms | 327.51 ms | 16599.33 | 
| swift                     | perfect                   | 15.39 ms | 15.65 ms | 17.34 ms | 19.43 ms | 159.07 ms | 3051.33 | 
| node                      | fastify                   | 24.74 ms | 15.68 ms | 33.23 ms | 245.33 ms | 951.00 ms | 49878.00 | 
| node                      | polka                     | 24.27 ms | 15.99 ms | 39.38 ms | 170.64 ms | 823.84 ms | 39921.00 | 
| node                      | rayo                      | 30.79 ms | 16.36 ms | 41.31 ms | 432.97 ms | 1280.63 ms | 78758.67 | 
| swift                     | vapor                     | 33.99 ms | 17.12 ms | 29.92 ms | 677.92 ms | 1969.29 ms | 121607.67 | 
| python                    | japronto                  | 19.95 ms | 19.44 ms | 21.03 ms | 28.44 ms | 395.61 ms | 7496.00 | 
| node                      | koa                       | 33.26 ms | 20.90 ms | 44.82 ms | 378.76 ms | 1193.97 ms | 69319.33 | 
| node                      | restify                   | 29.97 ms | 22.77 ms | 47.18 ms | 118.87 ms | 754.96 ms | 34341.67 | 
| node                      | express                   | 37.26 ms | 22.85 ms | 52.24 ms | 432.31 ms | 1326.00 ms | 77545.00 | 
| swift                     | kitura                    | 25.78 ms | 26.03 ms | 27.32 ms | 28.67 ms | 307.63 ms | 8321.67 | 
| crystal                   | raze                      | 29.42 ms | 26.89 ms | 37.10 ms | 47.35 ms | 464.02 ms | 15329.00 | 
| crystal                   | router.cr                 | 30.66 ms | 27.54 ms | 41.00 ms | 49.88 ms | 198.30 ms | 8091.00 | 
| go                        | gin                       | 54.37 ms | 27.67 ms | 153.04 ms | 263.50 ms | 488.78 ms | 63431.00 | 
| crystal                   | raze                      | 29.42 ms | 26.89 ms | 37.10 ms | 47.35 ms | 464.02 ms | 15329.00 | 
| node                      | hapi                      | 65.99 ms | 33.10 ms | 69.51 ms | 1095.64 ms | 2125.74 ms | 172292.67 | 
| crystal                   | prism                     | 33.29 ms | 34.99 ms | 41.98 ms | 50.81 ms | 228.00 ms | 7860.00 | 
| crystal                   | lucky                     | 37.66 ms | 39.37 ms | 46.21 ms | 54.59 ms | 261.05 ms | 10153.67 | 
| crystal                   | kemal                     | 40.64 ms | 40.88 ms | 48.89 ms | 56.41 ms | 331.08 ms | 9643.67 | 
| crystal                   | amber                     | 41.04 ms | 41.31 ms | 49.51 ms | 58.34 ms | 571.87 ms | 21251.33 | 
| python                    | flask                     | 49.56 ms | 43.44 ms | 77.74 ms | 112.96 ms | 357.92 ms | 20094.00 | 
| python                    | sanic                     | 52.49 ms | 47.22 ms | 90.51 ms | 141.90 ms | 297.75 ms | 28409.00 | 
| python                    | django                    | 82.09 ms | 72.08 ms | 114.52 ms | 308.86 ms | 1359.63 ms | 67107.00 | 
| python                    | tornado                   | 136.93 ms | 110.79 ms | 141.89 ms | 1061.46 ms | 2040.40 ms | 166686.67 | 

</details>


<details><summary>Ranked by requests (ordered by number or requests per sencond - highest is better)</summary>

| Language (Runtime)        | Framework (Middleware)    |    Requests / s |      Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| rust                      | actix-web                 | 193339.33 | 219.87 MB |
| cpp                       | evhtp                     | 182199.33 | 176.86 MB |
| go                        | fasthttprouter            | 165250.67 | 265.97 MB |
| python                    | vibora                    | 153539.67 | 174.25 MB |
| crystal                   | spider-gazelle            | 143493.67 | 153.49 MB |
| java                      | act                       | 131951.67 | 225.68 MB |
| rust                      | iron                      | 105635.67 | 133.29 MB |
| go                        | iris                      | 104128.33 | 139.60 MB |
| csharp                    | aspnetcore                | 100832.67 | 164.23 MB |
| rust                      | rocket                    | 96634.33 | 153.24 MB |
| go                        | echo                      | 95888.67 | 167.91 MB |
| go                        | gorilla-mux               | 91789.67 | 122.50 MB |
| rust                      | nickel                    | 79838.67 | 158.50 MB |
| nim                       | mofuw                     | 79714.00 | 139.82 MB |
| scala                     | akkahttp                  | 67392.33 | 144.71 MB |
| swift                     | perfect                   | 63560.33 | 59.70 MB |
| python                    | bottle                    | 56647.00 | 139.59 MB |
| node                      | fastify                   | 53906.33 | 128.16 MB |
| php                       | laravel                   | 53322.67 | 265.76 MB |
| php                       | symfony                   | 52204.00 | 259.79 MB |
| swift                     | vapor                     | 51581.33 | 88.21 MB |
| python                    | japronto                  | 49947.33 | 59.52 MB |
| node                      | polka                     | 49691.67 | 74.49 MB |
| node                      | rayo                      | 49620.00 | 74.39 MB |
| ruby                      | roda                      | 44254.67 | 42.23 MB |
| node                      | koa                       | 39983.00 | 84.67 MB |
| swift                     | kitura                    | 38356.67 | 71.15 MB |
| node                      | restify                   | 36869.33 | 64.71 MB |
| node                      | express                   | 36281.67 | 88.90 MB |
| ruby                      | rack-routing              | 35246.33 | 20.36 MB |
| crystal                   | raze                      | 34128.67 | 32.00 MB |
| crystal                   | router.cr                 | 32489.33 | 30.48 MB |
| crystal                   | raze                      | 34128.67 | 32.00 MB |
| crystal                   | prism                     | 30455.67 | 33.80 MB |
| go                        | gin                       | 28661.00 | 50.17 MB |
| node                      | hapi                      | 26873.67 | 58.73 MB |
| crystal                   | lucky                     | 26354.67 | 32.33 MB |
| crystal                   | kemal                     | 24647.67 | 40.14 MB |
| crystal                   | amber                     | 24460.67 | 35.44 MB |
| ruby                      | flame                     | 21059.00 | 12.17 MB |
| python                    | flask                     | 20075.67 | 49.30 MB |
| python                    | sanic                     | 19302.00 | 34.35 MB |
| ruby                      | hanami                    | 19256.33 | 145.64 MB |
| ruby                      | sinatra                   | 15596.67 | 40.55 MB |
| python                    | django                    | 12554.33 | 36.36 MB |
| python                    | tornado                   | 8603.33 | 22.76 MB |
| ruby                      | rails                     | 4705.33 | 14.33 MB |

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

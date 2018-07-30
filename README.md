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
Last update: 2018-07-30
```
OS: Linux (version: 4.16.11-100.fc26.x86_64, arch: x86_64)
CPU Cores: 8
```

<details open><summary>Ranked by latency (ordered by 50th percentile - lowest is better)</summary> 

| Language (Runtime)        | Framework (Middleware)    |         Average |  50% percentile |  90% percentile |  99% percentile | 99.9% percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust                      | nickel                    | 0.23 ms | 0.14 ms | 0.25 ms | 3.69 ms | 13.71 ms | 508.33 | 
| rust                      | rocket                    | 0.30 ms | 0.19 ms | 0.40 ms | 3.86 ms | 16.43 ms | 550.33 | 
| rust                      | iron                      | 1.18 ms | 0.75 ms | 2.67 ms | 6.90 ms | 79.85 ms | 1589.33 | 
| ruby                      | hanami                    | 14.13 ms | 1.20 ms | 38.77 ms | 204.09 ms | 458.25 ms | 39080.00 | 
| ruby                      | roda                      | 9.12 ms | 1.54 ms | 8.18 ms | 177.47 ms | 584.74 ms | 32708.67 | 
| ruby                      | rack-routing              | 11.79 ms | 1.83 ms | 14.48 ms | 213.15 ms | 670.37 ms | 39535.33 | 
| ruby                      | flame                     | 14.39 ms | 2.42 ms | 36.02 ms | 200.44 ms | 490.25 ms | 38247.00 | 
| ruby                      | rails                     | 52.20 ms | 3.47 ms | 186.17 ms | 368.47 ms | 694.34 ms | 87980.33 | 
| ruby                      | sinatra                   | 19.04 ms | 3.97 ms | 47.15 ms | 251.20 ms | 656.83 ms | 49097.67 | 
| rust                      | actix-web                 | 7.33 ms | 6.62 ms | 13.61 ms | 21.24 ms | 102.93 ms | 5269.00 | 
| php                       | laravel                   | 176.35 ms | 7.16 ms | 318.42 ms | 3705.09 ms | 7822.07 ms | 654613.33 | 
| cpp                       | evhtp                     | 9.05 ms | 8.55 ms | 15.10 ms | 23.26 ms | 112.75 ms | 4940.67 | 
| python                    | vibora                    | 9.62 ms | 8.85 ms | 17.98 ms | 28.14 ms | 56.97 ms | 6269.33 | 
| go                        | fasthttprouter            | 10.22 ms | 9.02 ms | 14.61 ms | 36.61 ms | 256.59 ms | 7186.00 | 
| php                       | symfony                   | 204.40 ms | 10.15 ms | 377.69 ms | 4162.31 ms | 7485.31 ms | 716133.00 | 
| nim                       | jester                    | 11.56 ms | 10.87 ms | 19.07 ms | 28.11 ms | 206.80 ms | 7041.67 | 
| java                      | act                       | 18.39 ms | 11.86 ms | 25.07 ms | 211.97 ms | 679.83 ms | 39619.00 | 
| scala                     | akkahttp                  | 197.87 ms | 12.97 ms | 86.67 ms | 4247.98 ms | 7444.69 ms | 748370.00 | 
| nim                       | mofuw                     | 21.85 ms | 16.45 ms | 31.70 ms | 135.00 ms | 424.41 ms | 23226.00 | 
| go                        | iris                      | 23.92 ms | 17.47 ms | 45.15 ms | 130.44 ms | 589.42 ms | 27706.33 | 
| csharp                    | aspnetcore                | 25.56 ms | 20.08 ms | 49.91 ms | 87.13 ms | 440.20 ms | 18768.67 | 
| go                        | gorilla-mux               | 27.21 ms | 20.43 ms | 53.87 ms | 101.36 ms | 426.22 ms | 23417.33 | 
| go                        | echo                      | 28.39 ms | 21.61 ms | 55.63 ms | 122.18 ms | 351.72 ms | 24364.67 | 
| swift                     | vapor                     | 118.73 ms | 29.95 ms | 48.64 ms | 2569.16 ms | 5406.12 ms | 437998.33 | 
| swift                     | perfect                   | 32.36 ms | 31.93 ms | 40.00 ms | 47.54 ms | 196.27 ms | 6209.33 | 
| node                      | koa                       | 70.54 ms | 32.62 ms | 53.96 ms | 1235.71 ms | 2305.86 ms | 198969.67 | 
| node                      | fastify                   | 60.03 ms | 34.36 ms | 55.72 ms | 946.33 ms | 2153.19 ms | 154177.67 | 
| crystal                   | router.cr                 | 46.92 ms | 34.36 ms | 76.24 ms | 277.56 ms | 591.36 ms | 51909.00 | 
| node                      | express                   | 61.10 ms | 37.80 ms | 57.18 ms | 871.15 ms | 1919.85 ms | 139235.67 | 
| node                      | polka                     | 68.61 ms | 38.36 ms | 77.26 ms | 1060.27 ms | 2427.26 ms | 171359.33 | 
| node                      | rayo                      | 78.70 ms | 39.27 ms | 78.04 ms | 1213.48 ms | 2451.29 ms | 200142.67 | 
| node                      | restify                   | 46.82 ms | 40.21 ms | 61.73 ms | 263.72 ms | 929.41 ms | 49241.33 | 
| crystal                   | spider-gazelle            | 50.40 ms | 40.93 ms | 78.39 ms | 279.42 ms | 527.06 ms | 49734.00 | 
| crystal                   | lucky                     | 56.36 ms | 42.28 ms | 96.10 ms | 348.93 ms | 556.32 ms | 58645.33 | 
| crystal                   | amber                     | 52.71 ms | 42.69 ms | 76.43 ms | 263.00 ms | 414.91 ms | 44815.33 | 
| elixir                    | plug                      | 76.36 ms | 49.14 ms | 133.31 ms | 1059.66 ms | 2877.76 ms | 192303.67 | 
| crystal                   | kemal                     | 71.98 ms | 50.53 ms | 144.44 ms | 429.67 ms | 618.00 ms | 80276.33 | 
| elixir                    | phoenix                   | 100.64 ms | 54.02 ms | 145.59 ms | 1807.55 ms | 3871.78 ms | 296756.00 | 
| node                      | hapi                      | 197.58 ms | 69.27 ms | 383.24 ms | 2636.35 ms | 4079.42 ms | 475899.67 | 
| go                        | gin                       | 82.04 ms | 74.07 ms | 174.77 ms | 317.82 ms | 747.21 ms | 72971.00 | 
| swift                     | kitura                    | 77.81 ms | 75.27 ms | 102.34 ms | 139.25 ms | 387.31 ms | 22120.00 | 
| python                    | flask                     | 106.57 ms | 99.04 ms | 160.59 ms | 219.71 ms | 872.23 ms | 50928.33 | 
| python                    | django                    | 161.53 ms | 157.00 ms | 230.55 ms | 318.85 ms | 1053.81 ms | 68219.33 | 
| python                    | sanic                     | 186.73 ms | 175.36 ms | 333.18 ms | 484.95 ms | 789.68 ms | 106543.00 | 
| python                    | tornado                   | 312.92 ms | 227.81 ms | 397.65 ms | 2529.32 ms | 3852.85 ms | 413497.33 | 

</details>


<details><summary>Ranked by requests (ordered by number or requests per sencond - highest is better)</summary>

| Language (Runtime)        | Framework (Middleware)    |    Requests / s |      Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| rust                      | actix-web                 | 132481.33 | 150.46 MB |
| python                    | vibora                    | 112742.33 | 128.03 MB |
| cpp                       | evhtp                     | 107342.33 | 104.19 MB |
| nim                       | jester                    | 99721.33 | 200.35 MB |
| go                        | fasthttprouter            | 97789.00 | 157.38 MB |
| java                      | act                       | 79347.00 | 135.78 MB |
| nim                       | mofuw                     | 55201.67 | 96.93 MB |
| rust                      | iron                      | 53382.67 | 66.18 MB |
| rust                      | rocket                    | 51678.00 | 81.47 MB |
| go                        | iris                      | 47704.67 | 63.72 MB |
| csharp                    | aspnetcore                | 41133.67 | 66.97 MB |
| rust                      | nickel                    | 41048.33 | 81.78 MB |
| go                        | gorilla-mux               | 39244.67 | 52.23 MB |
| go                        | echo                      | 38355.67 | 67.34 MB |
| swift                     | perfect                   | 30668.67 | 28.86 MB |
| swift                     | vapor                     | 29982.67 | 40.14 MB |
| scala                     | akkahttp                  | 29089.67 | 62.47 MB |
| node                      | koa                       | 28409.67 | 60.16 MB |
| node                      | fastify                   | 27931.33 | 64.81 MB |
| crystal                   | router.cr                 | 26482.00 | 24.88 MB |
| node                      | express                   | 25050.00 | 61.21 MB |
| ruby                      | roda                      | 23644.33 | 22.58 MB |
| crystal                   | spider-gazelle            | 23442.33 | 24.97 MB |
| node                      | restify                   | 23066.33 | 40.35 MB |
| elixir                    | plug                      | 22815.00 | 49.28 MB |
| node                      | polka                     | 22627.67 | 33.87 MB |
| node                      | rayo                      | 22270.67 | 33.32 MB |
| php                       | symfony                   | 22259.00 | 110.80 MB |
| crystal                   | amber                     | 21390.00 | 31.06 MB |
| crystal                   | lucky                     | 21263.33 | 26.21 MB |
| php                       | laravel                   | 20744.33 | 103.11 MB |
| elixir                    | phoenix                   | 20744.00 | 44.77 MB |
| crystal                   | kemal                     | 18690.67 | 30.51 MB |
| ruby                      | rack-routing              | 16473.00 | 9.51 MB |
| node                      | hapi                      | 13526.67 | 29.80 MB |
| go                        | gin                       | 13339.00 | 23.39 MB |
| swift                     | kitura                    | 12618.33 | 23.44 MB |
| ruby                      | hanami                    | 9929.33 | 75.18 MB |
| ruby                      | flame                     | 9705.00 | 5.61 MB |
| python                    | flask                     | 9218.67 | 22.71 MB |
| ruby                      | sinatra                   | 7215.33 | 18.75 MB |
| python                    | django                    | 5918.33 | 17.15 MB |
| python                    | sanic                     | 5265.00 | 9.39 MB |
| python                    | tornado                   | 3855.00 | 10.21 MB |
| ruby                      | rails                     | 2440.33 | 7.45 MB |

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

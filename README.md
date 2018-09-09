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
Last update: 2018-09-10
```
OS: Linux (version: 4.16.11-100.fc26.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: rocket (rust)


:three: iron (rust)


:four: roda (ruby)


:five: hanami (ruby)


#### Full table

| Language (Runtime)        | Framework (Middleware)    |         Average |  50% percentile |  90% percentile |  99% percentile | 99.9% percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust                      | nickel                    | 0.11 ms | 0.10 ms | 0.14 ms | 0.55 ms | 8.62 ms | 156.00 | 
| rust                      | rocket                    | 0.16 ms | 0.12 ms | 0.20 ms | 1.15 ms | 12.05 ms | 249.67 | 
| rust                      | iron                      | 0.58 ms | 0.43 ms | 1.05 ms | 3.21 ms | 29.12 ms | 719.67 | 
| ruby                      | roda                      | 5.09 ms | 1.36 ms | 8.04 ms | 82.20 ms | 531.75 ms | 19137.00 | 
| ruby                      | hanami                    | 7.45 ms | 1.63 ms | 20.19 ms | 80.21 ms | 233.70 ms | 15497.33 | 
| ruby                      | rack-routing              | 3.82 ms | 2.51 ms | 8.56 ms | 20.90 ms | 73.28 ms | 4268.00 | 
| ruby                      | flame                     | 6.23 ms | 3.88 ms | 14.74 ms | 33.40 ms | 120.60 ms | 7065.00 | 
| ruby                      | sinatra                   | 8.11 ms | 3.91 ms | 20.32 ms | 58.85 ms | 169.15 ms | 11879.00 | 
| php                       | symfony                   | 180.15 ms | 4.08 ms | 522.60 ms | 2307.75 ms | 6934.51 ms | 524313.00 | 
| php                       | laravel                   | 238.57 ms | 4.13 ms | 647.38 ms | 3732.14 ms | 6998.46 ms | 700717.33 | 
| rust                      | actix-web                 | 5.42 ms | 4.64 ms | 9.99 ms | 16.53 ms | 99.24 ms | 4117.00 | 
| cpp                       | evhtp                     | 7.39 ms | 5.41 ms | 14.69 ms | 30.78 ms | 71.52 ms | 6083.67 | 
| go                        | fasthttprouter            | 6.63 ms | 5.85 ms | 9.88 ms | 19.78 ms | 215.74 ms | 5125.00 | 
| python                    | vibora                    | 7.50 ms | 6.31 ms | 14.19 ms | 24.61 ms | 52.96 ms | 5178.67 | 
| crystal                   | spider-gazelle            | 7.57 ms | 6.53 ms | 13.52 ms | 22.88 ms | 53.12 ms | 4588.67 | 
| ruby                      | rails                     | 28.64 ms | 6.90 ms | 91.84 ms | 177.57 ms | 367.61 ms | 42147.00 | 
| java                      | act                       | 8.22 ms | 7.00 ms | 12.23 ms | 22.78 ms | 269.54 ms | 8895.33 | 
| scala                     | akkahttp                  | 174.16 ms | 8.26 ms | 23.84 ms | 4268.57 ms | 6871.66 ms | 720546.33 | 
| go                        | iris                      | 10.28 ms | 9.15 ms | 15.60 ms | 30.21 ms | 184.10 ms | 5958.67 | 
| go                        | echo                      | 10.85 ms | 9.33 ms | 16.99 ms | 35.86 ms | 292.53 ms | 8896.00 | 
| csharp                    | aspnetcore                | 10.82 ms | 9.33 ms | 17.44 ms | 28.44 ms | 290.02 ms | 8200.33 | 
| go                        | gorilla-mux               | 12.80 ms | 10.27 ms | 19.07 ms | 44.37 ms | 437.53 ms | 18107.00 | 
| nim                       | mofuw                     | 31.54 ms | 10.30 ms | 72.49 ms | 326.94 ms | 599.99 ms | 66531.33 | 
| node                      | rayo                      | 26.00 ms | 15.37 ms | 38.86 ms | 272.33 ms | 1053.18 ms | 55565.67 | 
| python                    | bottle                    | 19.94 ms | 16.06 ms | 33.49 ms | 63.75 ms | 228.10 ms | 12155.33 | 
| node                      | fastify                   | 25.75 ms | 16.86 ms | 36.55 ms | 215.00 ms | 971.53 ms | 48268.33 | 
| swift                     | perfect                   | 17.31 ms | 17.25 ms | 19.98 ms | 23.59 ms | 151.98 ms | 2831.33 | 
| node                      | polka                     | 27.42 ms | 17.45 ms | 43.86 ms | 225.37 ms | 948.69 ms | 49769.67 | 
| swift                     | vapor                     | 42.34 ms | 17.57 ms | 35.19 ms | 786.81 ms | 2343.80 ms | 146539.33 | 
| scala                     | http4s                    | 37.55 ms | 17.98 ms | 36.48 ms | 618.36 ms | 3077.91 ms | 147039.33 | 
| python                    | japronto                  | 20.79 ms | 20.20 ms | 23.46 ms | 28.26 ms | 195.49 ms | 3266.67 | 
| node                      | restify                   | 25.96 ms | 20.65 ms | 39.67 ms | 89.86 ms | 540.09 ms | 23197.33 | 
| node                      | koa                       | 36.15 ms | 22.07 ms | 49.34 ms | 413.13 ms | 1315.58 ms | 77117.00 | 
| node                      | express                   | 36.83 ms | 24.56 ms | 54.29 ms | 348.91 ms | 1231.03 ms | 66280.33 | 
| crystal                   | prism                     | 30.48 ms | 27.47 ms | 42.70 ms | 60.83 ms | 633.30 ms | 17443.33 | 
| swift                     | kitura                    | 29.52 ms | 28.12 ms | 33.04 ms | 47.07 ms | 463.32 ms | 17317.00 | 
| crystal                   | raze                      | 30.64 ms | 28.41 ms | 39.35 ms | 48.15 ms | 318.69 ms | 9975.33 | 
| go                        | gin                       | 54.85 ms | 30.28 ms | 147.88 ms | 311.92 ms | 603.78 ms | 66759.33 | 
| crystal                   | router.cr                 | 32.54 ms | 30.31 ms | 42.98 ms | 50.83 ms | 171.48 ms | 7923.33 | 
| crystal                   | amber                     | 34.96 ms | 31.31 ms | 48.86 ms | 56.72 ms | 213.28 ms | 9427.33 | 
| crystal                   | raze                      | 30.64 ms | 28.41 ms | 39.35 ms | 48.15 ms | 318.69 ms | 9975.33 | 
| crystal                   | kemal                     | 35.17 ms | 32.18 ms | 49.16 ms | 59.15 ms | 270.37 ms | 11060.67 | 
| python                    | aiohttp                   | 37.57 ms | 32.73 ms | 60.43 ms | 94.78 ms | 293.34 ms | 19688.33 | 
| node                      | hapi                      | 84.05 ms | 34.63 ms | 76.67 ms | 1495.66 ms | 2650.17 ms | 239388.00 | 
| crystal                   | lucky                     | 37.71 ms | 37.52 ms | 49.16 ms | 56.91 ms | 262.63 ms | 12234.33 | 
| python                    | flask                     | 79.61 ms | 53.47 ms | 169.91 ms | 353.51 ms | 689.90 ms | 68878.33 | 
| python                    | sanic                     | 59.31 ms | 54.26 ms | 98.37 ms | 152.58 ms | 267.61 ms | 29712.00 | 
| python                    | django                    | 97.13 ms | 68.22 ms | 210.40 ms | 306.29 ms | 579.93 ms | 67328.67 | 
| python                    | tornado                   | 125.82 ms | 119.73 ms | 150.46 ms | 418.15 ms | 1261.74 ms | 73782.33 | 
| python                    | quart                     | 172.03 ms | 152.69 ms | 266.39 ms | 305.37 ms | 580.03 ms | 64368.33 | 

#### Ranking (top 5)


:one: actix-web (rust)


:two: evhtp (cpp)


:three: vibora (python)


:four: fasthttprouter (go)


:five: act (java)


#### Full table

| Language (Runtime)        | Framework (Middleware)    |    Requests / s |      Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| rust                      | actix-web                 | 172797.00 | 196.47 MB |
| cpp                       | evhtp                     | 147463.33 | 142.98 MB |
| python                    | vibora                    | 145193.67 | 164.86 MB |
| go                        | fasthttprouter            | 142237.00 | 229.42 MB |
| java                      | act                       | 134500.00 | 230.05 MB |
| crystal                   | spider-gazelle            | 129651.33 | 137.63 MB |
| rust                      | iron                      | 101278.00 | 127.67 MB |
| go                        | iris                      | 95282.67 | 127.89 MB |
| go                        | echo                      | 92093.00 | 161.60 MB |
| csharp                    | aspnetcore                | 91976.67 | 149.81 MB |
| rust                      | rocket                    | 88245.67 | 138.90 MB |
| go                        | gorilla-mux               | 84127.00 | 112.47 MB |
| nim                       | mofuw                     | 78404.67 | 137.63 MB |
| rust                      | nickel                    | 75140.67 | 149.57 MB |
| scala                     | akkahttp                  | 70969.67 | 152.31 MB |
| swift                     | perfect                   | 56554.67 | 53.20 MB |
| node                      | rayo                      | 51757.33 | 77.60 MB |
| python                    | bottle                    | 51728.33 | 127.53 MB |
| node                      | fastify                   | 50880.67 | 122.13 MB |
| scala                     | http4s                    | 50869.33 | 89.21 MB |
| python                    | japronto                  | 47449.00 | 56.59 MB |
| swift                     | vapor                     | 46658.67 | 79.90 MB |
| node                      | polka                     | 46610.00 | 69.88 MB |
| php                       | symfony                   | 42661.67 | 212.46 MB |
| node                      | restify                   | 40752.33 | 71.53 MB |
| php                       | laravel                   | 39307.33 | 195.93 MB |
| ruby                      | roda                      | 37582.00 | 35.89 MB |
| node                      | koa                       | 37542.33 | 79.49 MB |
| swift                     | kitura                    | 34033.33 | 63.22 MB |
| node                      | express                   | 33734.00 | 82.64 MB |
| ruby                      | rack-routing              | 33204.33 | 19.18 MB |
| crystal                   | prism                     | 32976.67 | 37.08 MB |
| crystal                   | raze                      | 32399.67 | 30.37 MB |
| crystal                   | router.cr                 | 30641.00 | 28.72 MB |
| crystal                   | raze                      | 32399.67 | 30.37 MB |
| crystal                   | amber                     | 28480.33 | 41.25 MB |
| crystal                   | kemal                     | 28089.67 | 45.77 MB |
| go                        | gin                       | 27029.33 | 47.34 MB |
| python                    | aiohttp                   | 26944.67 | 60.96 MB |
| crystal                   | lucky                     | 26444.67 | 32.53 MB |
| node                      | hapi                      | 25538.00 | 66.12 MB |
| ruby                      | flame                     | 20616.33 | 11.90 MB |
| ruby                      | hanami                    | 17567.67 | 132.93 MB |
| python                    | sanic                     | 16981.00 | 30.27 MB |
| ruby                      | sinatra                   | 15835.00 | 41.12 MB |
| python                    | flask                     | 15815.00 | 38.90 MB |
| python                    | django                    | 11378.00 | 32.96 MB |
| python                    | tornado                   | 8007.67 | 21.31 MB |
| python                    | quart                     | 5646.33 | 11.60 MB |
| ruby                      | rails                     | 4480.67 | 13.65 MB |
| rust                      | actix-web                 | 172797.00 | 196.47 MB |
| cpp                       | evhtp                     | 147463.33 | 142.98 MB |
| python                    | vibora                    | 145193.67 | 164.86 MB |
| go                        | fasthttprouter            | 142237.00 | 229.42 MB |
| java                      | act                       | 134500.00 | 230.05 MB |
| crystal                   | spider-gazelle            | 129651.33 | 137.63 MB |
| rust                      | iron                      | 101278.00 | 127.67 MB |
| go                        | iris                      | 95282.67 | 127.89 MB |
| go                        | echo                      | 92093.00 | 161.60 MB |
| csharp                    | aspnetcore                | 91976.67 | 149.81 MB |
| rust                      | rocket                    | 88245.67 | 138.90 MB |
| go                        | gorilla-mux               | 84127.00 | 112.47 MB |
| nim                       | mofuw                     | 78404.67 | 137.63 MB |
| rust                      | nickel                    | 75140.67 | 149.57 MB |
| scala                     | akkahttp                  | 70969.67 | 152.31 MB |
| swift                     | perfect                   | 56554.67 | 53.20 MB |
| node                      | rayo                      | 51757.33 | 77.60 MB |
| python                    | bottle                    | 51728.33 | 127.53 MB |
| node                      | fastify                   | 50880.67 | 122.13 MB |
| scala                     | http4s                    | 50869.33 | 89.21 MB |
| python                    | japronto                  | 47449.00 | 56.59 MB |
| swift                     | vapor                     | 46658.67 | 79.90 MB |
| node                      | polka                     | 46610.00 | 69.88 MB |
| php                       | symfony                   | 42661.67 | 212.46 MB |
| node                      | restify                   | 40752.33 | 71.53 MB |
| php                       | laravel                   | 39307.33 | 195.93 MB |
| ruby                      | roda                      | 37582.00 | 35.89 MB |
| node                      | koa                       | 37542.33 | 79.49 MB |
| swift                     | kitura                    | 34033.33 | 63.22 MB |
| node                      | express                   | 33734.00 | 82.64 MB |
| ruby                      | rack-routing              | 33204.33 | 19.18 MB |
| crystal                   | prism                     | 32976.67 | 37.08 MB |
| crystal                   | raze                      | 32399.67 | 30.37 MB |
| crystal                   | router.cr                 | 30641.00 | 28.72 MB |
| crystal                   | raze                      | 32399.67 | 30.37 MB |
| crystal                   | amber                     | 28480.33 | 41.25 MB |
| crystal                   | kemal                     | 28089.67 | 45.77 MB |
| go                        | gin                       | 27029.33 | 47.34 MB |
| python                    | aiohttp                   | 26944.67 | 60.96 MB |
| crystal                   | lucky                     | 26444.67 | 32.53 MB |
| node                      | hapi                      | 25538.00 | 66.12 MB |
| ruby                      | flame                     | 20616.33 | 11.90 MB |
| ruby                      | hanami                    | 17567.67 | 132.93 MB |
| python                    | sanic                     | 16981.00 | 30.27 MB |
| ruby                      | sinatra                   | 15835.00 | 41.12 MB |
| python                    | flask                     | 15815.00 | 38.90 MB |
| python                    | django                    | 11378.00 | 32.96 MB |
| python                    | tornado                   | 8007.67 | 21.31 MB |
| python                    | quart                     | 5646.33 | 11.60 MB |
| ruby                      | rails                     | 4480.67 | 13.65 MB |
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

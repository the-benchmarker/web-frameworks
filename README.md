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
Last update: 2018-09-18
```
OS: Linux (version: 4.18.7-200.fc28.x86_64, arch: x86_64)
CPU Cores: 4
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: rocket (rust)


:three: rack-routing (ruby)


:four: roda (ruby)


:five: iron (rust)


#### Full table

| Language (Runtime)        | Framework (Middleware)    |         Average |  50% percentile |  90% percentile |  99% percentile | 99.9% percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust                      | nickel                    (0.10) | 0.04 ms | 0.04 ms | 0.05 ms | 0.06 ms | 0.87 ms | 9.00 | 
| rust                      | rocket                    (0.3) | 0.07 ms | 0.06 ms | 0.10 ms | 0.20 ms | 13.67 ms | 181.67 | 
| ruby                      | rack-routing              (0.0) | 2.15 ms | 0.19 ms | 5.92 ms | 29.28 ms | 86.81 ms | 5535.67 | 
| ruby                      | roda                      (3.11) | 1.72 ms | 0.23 ms | 4.33 ms | 24.50 ms | 98.96 ms | 4744.67 | 
| rust                      | iron                      (0.7) | 0.33 ms | 0.29 ms | 0.50 ms | 1.14 ms | 48.42 ms | 526.00 | 
| php                       | laravel                   (5.6) | 230.16 ms | 0.30 ms | 681.40 ms | 3122.52 ms | 7040.43 ms | 647609.33 | 
| ruby                      | sinatra                   (2.0) | 4.11 ms | 0.33 ms | 12.29 ms | 46.60 ms | 101.35 ms | 8995.33 | 
| ruby                      | hanami                    (1.2) | 3.95 ms | 0.38 ms | 10.76 ms | 51.71 ms | 148.38 ms | 9640.00 | 
| ruby                      | flame                     (5.0 (rc)) | 3.48 ms | 0.38 ms | 10.16 ms | 37.98 ms | 97.25 ms | 7382.67 | 
| php                       | symfony                   (4.1) | 196.28 ms | 1.04 ms | 622.10 ms | 2795.60 ms | 7025.27 ms | 568571.33 | 
| ruby                      | rails                     (5.2) | 22.94 ms | 3.32 ms | 77.42 ms | 149.75 ms | 296.48 ms | 35776.67 | 
| rust                      | actix-web                 (0.6) | 5.48 ms | 4.95 ms | 9.09 ms | 16.39 ms | 91.24 ms | 3487.00 | 
| go                        | fasthttprouter            (??) | 5.70 ms | 5.34 ms | 8.36 ms | 14.13 ms | 53.49 ms | 2397.67 | 
| cpp                       | evhtp                     (??) | 6.12 ms | 5.73 ms | 9.53 ms | 14.65 ms | 35.29 ms | 2771.33 | 
| python                    | vibora                    (0.0) | 6.66 ms | 5.90 ms | 12.01 ms | 18.92 ms | 47.35 ms | 4195.00 | 
| java                      | act                       (1.8) | 7.71 ms | 5.95 ms | 12.45 ms | 35.07 ms | 255.49 ms | 9545.00 | 
| crystal                   | spider-gazelle            (1.1) | 18.88 ms | 7.11 ms | 19.72 ms | 329.31 ms | 612.59 ms | 53813.33 | 
| scala                     | akkahttp                  (10.0) | 220.22 ms | 7.91 ms | 120.53 ms | 5140.23 ms | 7931.03 ms | 880850.67 | 
| csharp                    | aspnetcore                (1.1) | 8.67 ms | 8.05 ms | 11.37 ms | 15.41 ms | 231.34 ms | 6914.33 | 
| go                        | iris                      (??) | 16.24 ms | 10.03 ms | 24.61 ms | 184.45 ms | 576.09 ms | 33542.33 | 
| go                        | gin                       (??) | 16.22 ms | 10.52 ms | 26.94 ms | 166.57 ms | 404.03 ms | 28607.67 | 
| go                        | echo                      (??) | 17.34 ms | 10.77 ms | 30.33 ms | 170.31 ms | 412.19 ms | 30693.00 | 
| go                        | gorilla-mux               (??) | 16.78 ms | 11.54 ms | 30.24 ms | 148.89 ms | 394.26 ms | 27407.00 | 
| nim                       | mofuw                     (???) | 31.17 ms | 12.33 ms | 69.25 ms | 288.10 ms | 733.77 ms | 59098.33 | 
| python                    | bottle                    (0.12) | 21.82 ms | 15.31 ms | 38.42 ms | 74.61 ms | 193.69 ms | 14663.00 | 
| swift                     | perfect                   (3.0) | 15.51 ms | 15.94 ms | 16.75 ms | 17.61 ms | 169.82 ms | 2628.00 | 
| scala                     | http4s                    (0.0) | 17.75 ms | 16.11 ms | 29.65 ms | 48.07 ms | 448.40 ms | 13068.67 | 
| node                      | rayo                      (1.2) | 35.05 ms | 17.02 ms | 39.81 ms | 595.95 ms | 1723.76 ms | 105606.33 | 
| node                      | polka                     (0.4) | 37.91 ms | 17.15 ms | 39.57 ms | 696.08 ms | 1827.54 ms | 120055.67 | 
| crystal                   | router.cr                 (0.2) | 19.78 ms | 18.24 ms | 24.96 ms | 27.58 ms | 228.00 ms | 4819.67 | 
| node                      | fastify                   (1.11) | 38.98 ms | 18.48 ms | 40.60 ms | 704.19 ms | 1935.92 ms | 123475.33 | 
| crystal                   | kemal                     (0.24) | 23.28 ms | 21.03 ms | 30.28 ms | 34.27 ms | 115.78 ms | 5633.00 | 
| node                      | koa                       (2.5) | 40.19 ms | 21.13 ms | 42.57 ms | 694.22 ms | 1803.54 ms | 117135.00 | 
| swift                     | vapor                     (3.0) | 122.91 ms | 21.28 ms | 39.80 ms | 2788.04 ms | 5287.70 ms | 489996.00 | 
| swift                     | kitura                    (2.2) | 24.31 ms | 22.80 ms | 28.47 ms | 46.08 ms | 927.02 ms | 23603.33 | 
| crystal                   | lucky                     (0.11) | 24.01 ms | 23.42 ms | 28.50 ms | 30.87 ms | 241.39 ms | 7690.33 | 
| crystal                   | amber                     (0.9) | 24.40 ms | 23.89 ms | 30.92 ms | 34.13 ms | 235.47 ms | 6528.67 | 
| node                      | restify                   (7.2) | 33.12 ms | 23.91 ms | 48.27 ms | 278.98 ms | 1088.28 ms | 57358.33 | 
| node                      | express                   (4.16) | 62.99 ms | 25.66 ms | 55.86 ms | 1206.55 ms | 2374.86 ms | 193023.00 | 
| node                      | hapi                      (17.5) | 107.33 ms | 31.21 ms | 67.73 ms | 2026.92 ms | 3644.56 ms | 342462.00 | 
| python                    | aiohttp                   (3.4) | 36.23 ms | 34.29 ms | 55.94 ms | 66.28 ms | 386.84 ms | 19177.33 | 
| python                    | flask                     (1.0) | 54.65 ms | 44.93 ms | 85.32 ms | 114.42 ms | 782.76 ms | 33016.00 | 
| python                    | sanic                     (0.7) | 78.66 ms | 67.76 ms | 140.21 ms | 201.70 ms | 379.39 ms | 44753.67 | 
| python                    | django                    (2.0) | 89.63 ms | 86.30 ms | 101.87 ms | 270.49 ms | 1278.17 ms | 54782.00 | 
| python                    | tornado                   (5.1) | 218.51 ms | 129.68 ms | 154.06 ms | 3040.97 ms | 6691.16 ms | 499627.00 | 

#### Ranking (top 5)


:one: (actix-web) (rust)


:two: (fasthttprouter) (go)


:three: (vibora) (python)


:four: (evhtp) (cpp)


:five: (act) (java)


#### Full table

| Language (Runtime)        | Framework (Middleware)    |    Requests / s |      Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| rust                      | actix-web                 (0.6) | 166651.00 | 189.41 MB |
| go                        | fasthttprouter            (0.1) | 159052.33 | 256.65 MB |
| python                    | vibora                    (0.0) | 153733.67 | 174.45 MB |
| cpp                       | evhtp                     (??) | 148770.00 | 144.40 MB |
| java                      | act                       (1.8) | 137189.33 | 234.64 MB |
| crystal                   | spider-gazelle            (1.1) | 117384.00 | 126.14 MB |
| csharp                    | aspnetcore                (1.1) | 115517.33 | 188.06 MB |
| rust                      | rocket                    (0.3) | 103259.67 | 162.61 MB |
| rust                      | nickel                    (0.10) | 102395.33 | 203.44 MB |
| go                        | iris                      (10.7) | 94101.00 | 125.57 MB |
| rust                      | iron                      (0.7) | 93131.33 | 117.19 MB |
| go                        | gin                       (1.3) | 90838.00 | 159.35 MB |
| go                        | echo                      (3.3) | 89201.67 | 156.57 MB |
| go                        | gorilla-mux               (1.6) | 84956.00 | 113.36 MB |
| scala                     | akkahttp                  (10.0) | 73205.67 | 156.93 MB |
| nim                       | mofuw                     (???) | 66418.00 | 116.44 MB |
| swift                     | perfect                   (3.0) | 63030.33 | 59.10 MB |
| scala                     | http4s                    (0.0) | 57657.67 | 100.83 MB |
| crystal                   | router.cr                 (0.2) | 50187.00 | 47.11 MB |
| node                      | polka                     (0.4) | 48948.00 | 73.18 MB |
| node                      | rayo                      (1.2) | 48465.00 | 72.46 MB |
| python                    | bottle                    (0.12) | 47830.33 | 117.63 MB |
| node                      | fastify                   (1.11) | 46348.33 | 113.64 MB |
| swift                     | kitura                    (2.2) | 42485.67 | 78.78 MB |
| crystal                   | kemal                     (0.24) | 42343.33 | 69.04 MB |
| crystal                   | lucky                     (0.11) | 41941.67 | 51.65 MB |
| swift                     | vapor                     (3.0) | 41471.33 | 69.12 MB |
| node                      | koa                       (2.5) | 40719.67 | 85.96 MB |
| crystal                   | amber                     (0.9) | 40608.33 | 58.90 MB |
| ruby                      | roda                      (3.11) | 37953.33 | 36.16 MB |
| node                      | restify                   (7.2) | 36449.67 | 63.82 MB |
| php                       | symfony                   (4.1) | 36113.33 | 179.29 MB |
| node                      | express                   (4.16) | 33565.00 | 81.98 MB |
| ruby                      | rack-routing              (0.0) | 29853.00 | 17.21 MB |
| php                       | laravel                   (5.6) | 28294.33 | 140.52 MB |
| python                    | aiohttp                   (3.4) | 27621.33 | 62.46 MB |
| node                      | hapi                      (17.5) | 27578.00 | 71.28 MB |
| ruby                      | flame                     (5.0 (rc)) | 18465.33 | 10.65 MB |
| python                    | flask                     (1.0) | 18430.00 | 45.33 MB |
| ruby                      | hanami                    (1.2) | 16291.67 | 123.19 MB |
| ruby                      | sinatra                   (2.0) | 15573.33 | 40.38 MB |
| python                    | sanic                     (0.7) | 12880.00 | 22.95 MB |
| python                    | django                    (2.0) | 11184.00 | 32.40 MB |
| python                    | tornado                   (5.1) | 7251.00 | 19.40 MB |
| ruby                      | rails                     (5.2) | 2777.00 | 8.64 MB |
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

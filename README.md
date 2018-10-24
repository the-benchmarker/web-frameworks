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
Last update: 2018-10-24
```
OS: Linux (version: 4.18.16-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: rocket (rust)


:three: iron (rust)


:four: laravel (php)


:five: slim (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.29) | [nickel](http://nickel-org.github.io) (0.10) | 0.06 ms | 0.06 ms | 0.08 ms | 0.10 ms | 0.81 ms | 18.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 0.08 ms | 0.08 ms | 0.12 ms | 0.19 ms | 1.99 ms | 35.33 | 
| rust (1.29) | [iron](http://ironframework.io) (0.6) | 0.31 ms | 0.29 ms | 0.50 ms | 0.83 ms | 18.11 ms | 214.00 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 101.76 ms | 0.29 ms | 241.36 ms | 1916.93 ms | 6803.36 ms | 381248.67 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 110.71 ms | 0.54 ms | 164.04 ms | 2920.29 ms | 6843.05 ms | 502144.33 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.55 ms | 0.77 ms | 10.03 ms | 33.46 ms | 115.94 ms | 6759.67 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.2) | 6.50 ms | 0.83 ms | 19.82 ms | 61.04 ms | 179.92 ms | 12536.00 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 2.77 ms | 0.95 ms | 7.52 ms | 23.23 ms | 87.01 ms | 4740.67 | 
| php (7.2) | [symfony](http://symfony.com) (4.1) | 96.02 ms | 1.25 ms | 165.20 ms | 2206.64 ms | 5125.07 ms | 375442.00 | 
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 2.96 ms | 1.94 ms | 6.55 ms | 15.32 ms | 33.63 ms | 3200.00 | 
| rust (1.29) | [actix-web](http://actix.rs) (0.7) | 3.00 ms | 2.10 ms | 6.47 ms | 15.04 ms | 95.15 ms | 3288.00 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 7.09 ms | 2.18 ms | 20.31 ms | 55.54 ms | 163.46 ms | 11614.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.21 ms | 2.48 ms | 5.61 ms | 10.62 ms | 153.00 ms | 3606.00 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.92 ms | 2.51 ms | 16.13 ms | 40.74 ms | 123.17 ms | 8622.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.74 ms | 2.55 ms | 4.86 ms | 6.28 ms | 131.10 ms | 2786.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.55 ms | 2.86 ms | 7.50 ms | 14.45 ms | 34.79 ms | 3159.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.83 ms | 3.26 ms | 6.61 ms | 10.83 ms | 37.80 ms | 2130.67 | 
| crystal (0.26.1) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 3.87 ms | 3.38 ms | 7.22 ms | 13.42 ms | 31.68 ms | 2720.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 4.12 ms | 4.08 ms | 6.87 ms | 8.61 ms | 17.14 ms | 2084.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 4.80 ms | 4.46 ms | 7.05 ms | 13.83 ms | 194.34 ms | 3775.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 5.34 ms | 4.58 ms | 9.03 ms | 18.09 ms | 107.37 ms | 4233.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.41 ms | 4.68 ms | 8.95 ms | 18.32 ms | 128.69 ms | 3823.33 | 
| go (1.11) | [iris](http://iris-go.com) (10.7) | 5.71 ms | 4.79 ms | 9.60 ms | 18.98 ms | 79.80 ms | 3424.67 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 6.07 ms | 4.90 ms | 10.35 ms | 20.84 ms | 159.63 ms | 4712.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 6.34 ms | 5.09 ms | 10.65 ms | 21.42 ms | 232.37 ms | 5819.00 | 
| go (1.11) | [beego](http://beego.me) (1.10) | 6.57 ms | 5.11 ms | 10.66 ms | 22.01 ms | 354.05 ms | 9261.67 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 6.34 ms | 5.11 ms | 10.85 ms | 22.21 ms | 157.73 ms | 4722.00 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 43.93 ms | 5.17 ms | 188.36 ms | 430.80 ms | 743.37 ms | 100443.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 196.58 ms | 6.42 ms | 84.50 ms | 4595.68 ms | 7932.39 ms | 809335.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 8.42 ms | 6.45 ms | 14.90 ms | 28.05 ms | 213.87 ms | 6570.00 | 
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 8.93 ms | 6.53 ms | 16.00 ms | 36.34 ms | 330.56 ms | 11407.67 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 29.92 ms | 7.32 ms | 95.81 ms | 246.95 ms | 618.88 ms | 52823.00 | 
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 10.44 ms | 7.48 ms | 16.80 ms | 44.70 ms | 527.37 ms | 20212.00 | 
| node (10.12) | [fastify](http://fastify.io) (1.12) | 11.45 ms | 8.17 ms | 18.11 ms | 52.69 ms | 456.84 ms | 17743.00 | 
| node (10.12) | [koa](http://koajs.com) (2.5) | 13.60 ms | 9.56 ms | 22.26 ms | 57.37 ms | 576.63 ms | 22768.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 11.46 ms | 10.24 ms | 20.83 ms | 38.70 ms | 207.73 ms | 8080.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 14.09 ms | 10.79 ms | 18.64 ms | 55.05 ms | 765.71 ms | 29862.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 10.84 ms | 10.88 ms | 12.57 ms | 14.50 ms | 24.60 ms | 1507.33 | 
| node (10.12) | [restify](http://restify.com) (7.2) | 18.35 ms | 14.30 ms | 30.48 ms | 60.92 ms | 497.52 ms | 20133.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 17.20 ms | 14.47 ms | 33.70 ms | 51.11 ms | 79.49 ms | 10844.33 | 
| node (10.12) | [express](http://expressjs.com) (4.16) | 20.06 ms | 15.10 ms | 31.45 ms | 101.29 ms | 707.65 ms | 30249.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 20.55 ms | 19.00 ms | 28.88 ms | 43.21 ms | 251.64 ms | 8048.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 24.95 ms | 19.88 ms | 45.34 ms | 70.59 ms | 397.27 ms | 18277.00 | 
| node (10.12) | [hapi](http://hapijs.com) (17.6) | 32.46 ms | 20.56 ms | 41.05 ms | 427.62 ms | 1315.57 ms | 77336.67 | 
| crystal (0.26.1) | [lucky](http://luckyframework.org) (0.11) | 24.70 ms | 21.59 ms | 35.72 ms | 45.09 ms | 535.26 ms | 19622.33 | 
| crystal (0.26.1) | [raze](http://razecr.com) (0.3) | 24.61 ms | 22.10 ms | 33.31 ms | 40.54 ms | 173.58 ms | 8010.00 | 
| crystal (0.26.1) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 24.11 ms | 22.58 ms | 30.93 ms | 41.15 ms | 173.42 ms | 7203.33 | 
| crystal (0.26.1) | [amber](http://amberframework.org) (0.9) | 26.88 ms | 25.47 ms | 38.65 ms | 45.20 ms | 313.88 ms | 10694.33 | 
| crystal (0.26.1) | [orion](http://github.com/obsidian/orion) (1.6) | 31.33 ms | 28.36 ms | 40.32 ms | 46.41 ms | 178.09 ms | 7578.67 | 
| crystal (0.26.1) | [kemal](http://kemalcr.com) (0.24) | 29.65 ms | 30.34 ms | 36.27 ms | 44.38 ms | 309.77 ms | 10983.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 37.95 ms | 30.94 ms | 71.35 ms | 149.90 ms | 491.10 ms | 32609.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 49.64 ms | 39.58 ms | 86.20 ms | 148.81 ms | 737.55 ms | 42963.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 63.06 ms | 56.03 ms | 106.35 ms | 159.54 ms | 263.19 ms | 30853.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 67.98 ms | 66.30 ms | 88.36 ms | 107.65 ms | 463.23 ms | 22202.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (actix-web) (rust)


:three: (evhtp) (cpp)


:four: (vibora) (python)


:five: (fasthttprouter) (go)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 380398.00 | 455.52 MB |
| rust (1.29) | [actix-web](http://actix.rs) (0.7) | 352244.33 | 400.47 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 323527.00 | 313.87 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 306826.67 | 348.29 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 293582.67 | 474.74 MB |
| java (8) | [act](http://actframework.org) (1.8) | 274318.00 | 469.35 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 273678.33 | 550.12 MB |
| crystal (0.26.1) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 255238.33 | 273.12 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 196631.00 | 320.33 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 184683.00 | 247.46 MB |
| rust (1.29) | [iron](http://ironframework.io) (0.6) | 183729.33 | 231.11 MB |
| go (1.11) | [iris](http://iris-go.com) (10.7) | 173210.33 | 231.65 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 164411.67 | 288.39 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 162383.00 | 253.10 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 157149.67 | 275.59 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 156885.33 | 209.08 MB |
| go (1.11) | [beego](http://beego.me) (1.10) | 156605.00 | 210.80 MB |
| rust (1.29) | [nickel](http://nickel-org.github.io) (0.10) | 149126.33 | 296.33 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 140787.33 | 247.23 MB |
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 125839.67 | 188.54 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 121744.67 | 300.10 MB |
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 115958.00 | 173.62 MB |
| node (10.12) | [fastify](http://fastify.io) (1.12) | 109108.33 | 256.47 MB |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 95706.33 | 475.19 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 93790.67 | 164.29 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 92249.67 | 86.67 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 91042.00 | 451.48 MB |
| node (10.12) | [koa](http://koajs.com) (2.5) | 87273.67 | 184.67 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 85440.33 | 183.26 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 80181.00 | 135.14 MB |
| c (99) | [kore](http://kore.io) (3.1) | 74880.67 | 202.93 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 67429.00 | 350.89 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 61021.67 | 138.46 MB |
| node (10.12) | [restify](http://restify.com) (7.2) | 59604.00 | 104.27 MB |
| node (10.12) | [express](http://expressjs.com) (4.16) | 57614.00 | 140.70 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 47700.67 | 88.46 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 46578.33 | 44.37 MB |
| node (10.12) | [hapi](http://hapijs.com) (17.6) | 43806.67 | 113.49 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 41282.00 | 101.61 MB |
| crystal (0.26.1) | [lucky](http://luckyframework.org) (0.11) | 41133.33 | 50.55 MB |
| crystal (0.26.1) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 40937.33 | 38.41 MB |
| crystal (0.26.1) | [raze](http://razecr.com) (0.3) | 40651.33 | 38.10 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 36667.00 | 21.16 MB |
| crystal (0.26.1) | [amber](http://amberframework.org) (0.9) | 36658.67 | 53.15 MB |
| crystal (0.26.1) | [kemal](http://kemalcr.com) (0.24) | 34137.33 | 55.62 MB |
| crystal (0.26.1) | [orion](http://github.com/obsidian/orion) (1.6) | 31451.33 | 51.26 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 28852.00 | 51.41 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 21692.67 | 12.50 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 21451.67 | 62.17 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.2) | 19938.33 | 150.71 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 18054.67 | 46.81 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 16065.67 | 31.96 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 14332.00 | 38.39 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 4279.33 | 13.15 MB |
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

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
Last update: 2018-10-23
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

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 0.13 ms | 0.11 ms | 0.20 ms | 0.28 ms | 8.42 ms | 102.33 | 
| rust (1.29) | [nickel](http://nickel-org.github.io) (0.10) | 0.11 ms | 0.12 ms | 0.15 ms | 0.18 ms | 5.12 ms | 51.67 | 
| rust (1.29) | [iron](http://ironframework.io) (0.6) | 0.50 ms | 0.43 ms | 0.79 ms | 2.36 ms | 24.17 ms | 482.33 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 2.46 ms | 1.86 ms | 4.88 ms | 12.47 ms | 66.46 ms | 2478.00 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.51 ms | 2.43 ms | 7.38 ms | 19.58 ms | 103.40 ms | 4046.33 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 141.48 ms | 3.36 ms | 246.10 ms | 3178.08 ms | 6219.21 ms | 536956.67 | 
| php (7.2) | [symfony](http://symfony.com) (4.1) | 70.95 ms | 3.78 ms | 161.07 ms | 1554.34 ms | 3279.10 ms | 259690.00 | 
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 4.28 ms | 4.00 ms | 8.25 ms | 13.53 ms | 42.48 ms | 2980.00 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.75 ms | 4.04 ms | 12.87 ms | 25.22 ms | 93.50 ms | 5438.33 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.2) | 6.13 ms | 4.13 ms | 14.07 ms | 30.78 ms | 105.02 ms | 6549.33 | 
| rust (1.29) | [actix-web](http://actix.rs) (0.7) | 4.66 ms | 4.25 ms | 8.28 ms | 13.59 ms | 39.95 ms | 2778.00 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 6.83 ms | 4.33 ms | 16.18 ms | 34.81 ms | 101.87 ms | 7460.00 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.89 ms | 4.44 ms | 7.62 ms | 13.69 ms | 46.79 ms | 2368.33 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 84.58 ms | 4.44 ms | 246.82 ms | 1216.50 ms | 3387.91 ms | 250707.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 5.16 ms | 4.69 ms | 8.52 ms | 14.19 ms | 66.01 ms | 2826.33 | 
| crystal (0.26.1) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 6.15 ms | 5.17 ms | 10.93 ms | 18.74 ms | 41.18 ms | 3711.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 6.02 ms | 5.43 ms | 11.21 ms | 18.02 ms | 43.06 ms | 3854.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 6.79 ms | 6.27 ms | 10.19 ms | 15.72 ms | 63.91 ms | 2822.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 34.61 ms | 6.49 ms | 11.30 ms | 1082.93 ms | 3178.95 ms | 179994.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 8.10 ms | 7.25 ms | 12.12 ms | 21.39 ms | 140.25 ms | 4549.67 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 9.07 ms | 7.29 ms | 12.40 ms | 31.23 ms | 385.04 ms | 16060.33 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 25.38 ms | 7.37 ms | 78.40 ms | 159.36 ms | 321.07 ms | 36765.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 217.25 ms | 7.41 ms | 24.05 ms | 5159.20 ms | 7932.66 ms | 876452.67 | 
| go (1.11) | [iris](http://iris-go.com) (10.7) | 8.33 ms | 7.50 ms | 12.82 ms | 24.13 ms | 250.35 ms | 5650.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 8.97 ms | 8.08 ms | 13.89 ms | 26.35 ms | 163.68 ms | 4996.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 9.03 ms | 8.13 ms | 14.06 ms | 26.97 ms | 160.09 ms | 4821.67 | 
| go (1.11) | [beego](http://beego.me) (1.10) | 9.36 ms | 8.36 ms | 14.41 ms | 27.88 ms | 252.88 ms | 6368.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 9.52 ms | 8.57 ms | 14.84 ms | 27.51 ms | 121.13 ms | 4961.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 11.08 ms | 8.87 ms | 15.28 ms | 35.68 ms | 584.56 ms | 20672.33 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 29.80 ms | 9.97 ms | 89.79 ms | 276.05 ms | 418.44 ms | 57990.67 | 
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 16.44 ms | 11.95 ms | 27.14 ms | 69.49 ms | 504.85 ms | 19550.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 16.99 ms | 13.03 ms | 32.13 ms | 57.87 ms | 294.95 ms | 13194.67 | 
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 21.33 ms | 13.09 ms | 31.57 ms | 196.36 ms | 893.83 ms | 45336.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 76.47 ms | 15.83 ms | 33.36 ms | 1804.59 ms | 3160.97 ms | 307207.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 16.07 ms | 16.19 ms | 18.16 ms | 20.22 ms | 231.02 ms | 4238.33 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 23.53 ms | 16.76 ms | 28.05 ms | 232.18 ms | 1215.77 ms | 57279.33 | 
| node (10.12) | [fastify](http://fastify.io) (1.12) | 29.14 ms | 18.10 ms | 42.11 ms | 298.53 ms | 1123.75 ms | 61359.33 | 
| node (10.12) | [restify](http://restify.com) (7.2) | 27.25 ms | 19.67 ms | 41.19 ms | 156.57 ms | 757.24 ms | 36213.00 | 
| node (10.12) | [koa](http://koajs.com) (2.5) | 34.50 ms | 19.81 ms | 45.42 ms | 447.10 ms | 1279.97 ms | 80148.67 | 
| node (10.12) | [express](http://expressjs.com) (4.16) | 38.55 ms | 23.13 ms | 53.94 ms | 431.02 ms | 1293.85 ms | 80033.00 | 
| crystal (0.26.1) | [raze](http://razecr.com) (0.3) | 28.50 ms | 25.11 ms | 39.87 ms | 50.22 ms | 210.90 ms | 9263.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 26.30 ms | 25.55 ms | 27.40 ms | 45.55 ms | 602.03 ms | 22901.67 | 
| crystal (0.26.1) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 29.81 ms | 26.47 ms | 38.02 ms | 48.76 ms | 382.30 ms | 11763.67 | 
| crystal (0.26.1) | [amber](http://amberframework.org) (0.9) | 31.73 ms | 28.36 ms | 45.83 ms | 56.32 ms | 185.01 ms | 8651.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 32.79 ms | 30.11 ms | 49.72 ms | 75.65 ms | 345.01 ms | 14930.00 | 
| node (10.12) | [hapi](http://hapijs.com) (17.6) | 58.14 ms | 30.82 ms | 65.08 ms | 907.43 ms | 1917.34 ms | 145647.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 45.25 ms | 36.49 ms | 83.58 ms | 108.50 ms | 440.35 ms | 26536.33 | 
| crystal (0.26.1) | [lucky](http://luckyframework.org) (0.11) | 37.87 ms | 38.60 ms | 45.01 ms | 56.00 ms | 266.54 ms | 10376.67 | 
| crystal (0.26.1) | [kemal](http://kemalcr.com) (0.24) | 37.93 ms | 38.60 ms | 44.82 ms | 55.17 ms | 126.73 ms | 8029.33 | 
| crystal (0.26.1) | [orion](http://github.com/obsidian/orion) (1.6) | 39.76 ms | 41.56 ms | 50.07 ms | 60.23 ms | 123.99 ms | 9519.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 51.96 ms | 50.30 ms | 77.10 ms | 110.51 ms | 236.49 ms | 19894.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 76.93 ms | 61.87 ms | 132.25 ms | 310.79 ms | 939.88 ms | 63315.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 106.34 ms | 100.58 ms | 186.66 ms | 255.49 ms | 382.10 ms | 52779.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 110.09 ms | 106.11 ms | 146.98 ms | 159.01 ms | 446.27 ms | 26554.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (actix-web) (rust)


:three: (fasthttprouter) (go)


:four: (evhtp) (cpp)


:five: (vibora) (python)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 215922.33 | 258.52 MB |
| rust (1.29) | [actix-web](http://actix.rs) (0.7) | 193861.67 | 220.18 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 185637.00 | 299.09 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 176336.33 | 170.94 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 170506.00 | 193.69 MB |
| crystal (0.26.1) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 157697.00 | 168.99 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 156218.00 | 314.20 MB |
| java (8) | [act](http://actframework.org) (1.8) | 133908.00 | 228.97 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 118131.67 | 157.79 MB |
| go (1.11) | [iris](http://iris-go.com) (10.7) | 115188.67 | 154.19 MB |
| rust (1.29) | [iron](http://ironframework.io) (0.6) | 109405.67 | 137.70 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 107596.00 | 188.78 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 106748.67 | 187.53 MB |
| go (1.11) | [beego](http://beego.me) (1.10) | 103950.67 | 139.81 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 102628.67 | 137.01 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 95925.67 | 156.07 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 94147.00 | 147.05 MB |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 89569.00 | 445.62 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 86360.67 | 430.03 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 81347.00 | 142.80 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 76355.67 | 163.98 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 75683.00 | 376.33 MB |
| rust (1.29) | [nickel](http://nickel-org.github.io) (0.10) | 70721.67 | 140.10 MB |
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 66876.67 | 100.14 MB |
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 62315.00 | 93.42 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 61511.33 | 151.60 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 61400.33 | 57.76 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 56319.00 | 98.77 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 52794.00 | 90.03 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 50884.33 | 48.61 MB |
| c (99) | [kore](http://kore.io) (3.1) | 46544.67 | 126.11 MB |
| node (10.12) | [fastify](http://fastify.io) (1.12) | 46300.00 | 113.87 MB |
| node (10.12) | [restify](http://restify.com) (7.2) | 42038.67 | 73.73 MB |
| node (10.12) | [koa](http://koajs.com) (2.5) | 41911.67 | 88.58 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 38899.33 | 72.16 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 36260.33 | 20.96 MB |
| node (10.12) | [express](http://expressjs.com) (4.16) | 35689.00 | 87.37 MB |
| crystal (0.26.1) | [raze](http://razecr.com) (0.3) | 34851.00 | 32.67 MB |
| crystal (0.26.1) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 33345.67 | 31.26 MB |
| crystal (0.26.1) | [amber](http://amberframework.org) (0.9) | 31110.00 | 45.08 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 30340.00 | 68.71 MB |
| node (10.12) | [hapi](http://hapijs.com) (17.6) | 28606.67 | 74.07 MB |
| crystal (0.26.1) | [kemal](http://kemalcr.com) (0.24) | 26549.00 | 43.22 MB |
| crystal (0.26.1) | [lucky](http://luckyframework.org) (0.11) | 26361.33 | 32.30 MB |
| crystal (0.26.1) | [orion](http://github.com/obsidian/orion) (1.6) | 24947.67 | 40.64 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 22598.67 | 55.72 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 22155.00 | 12.80 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.2) | 20780.00 | 157.53 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 19032.00 | 33.88 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 18669.33 | 48.51 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 13620.67 | 39.52 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 9518.00 | 18.97 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 8863.67 | 23.76 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 5047.67 | 15.39 MB |
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

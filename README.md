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

> jobs are either languages (example : crystal) or frameworks (example : router.cr)

~~~sh
bin/neph [job1] [job2] [job3] ...
~~~

+ Start the benchmark ....

> tools is a list of language / framework to challenge (example : ruby kemal amber go python)

~~~sh
bin/benchmarker [tools]
~~~

## Results

<!-- Result from here -->
Last update: 2018-11-09
```
OS: Linux (version: 4.18.17-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: symfony (php)


:three: iron (rust)


:four: laravel (php)


:five: rack-routing (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 0.07 ms | 0.07 ms | 0.10 ms | 0.13 ms | 4.22 ms | 40.67 | 
| php (7.2) | [symfony](http://symfony.com) (4.1) | 95.59 ms | 0.27 ms | 177.90 ms | 2279.11 ms | 6932.45 ms | 386186.00 | 
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 0.30 ms | 0.29 ms | 0.50 ms | 0.75 ms | 11.46 ms | 182.33 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 137.00 ms | 0.30 ms | 303.45 ms | 2732.34 ms | 6818.24 ms | 496392.00 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.15 ms | 0.72 ms | 12.26 ms | 37.56 ms | 122.07 ms | 7717.00 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 6.90 ms | 0.80 ms | 21.35 ms | 66.81 ms | 204.06 ms | 13731.00 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 3.03 ms | 0.96 ms | 8.33 ms | 25.70 ms | 100.06 ms | 5265.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 176.43 ms | 1.42 ms | 12.34 ms | 4303.89 ms | 6595.01 ms | 762931.33 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 106.56 ms | 1.44 ms | 178.35 ms | 2539.04 ms | 5658.09 ms | 428380.67 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 141.68 ms | 1.51 ms | 223.64 ms | 3310.88 ms | 6782.19 ms | 579753.67 | 
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 3.25 ms | 2.31 ms | 6.82 ms | 15.40 ms | 94.71 ms | 3880.67 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 8.01 ms | 2.36 ms | 23.26 ms | 61.74 ms | 168.20 ms | 12996.67 | 
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 3.00 ms | 2.39 ms | 6.51 ms | 12.21 ms | 27.20 ms | 2831.00 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 6.59 ms | 2.63 ms | 18.25 ms | 45.96 ms | 141.10 ms | 9801.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.80 ms | 3.11 ms | 7.75 ms | 13.88 ms | 46.25 ms | 3119.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.96 ms | 3.28 ms | 6.79 ms | 11.68 ms | 40.39 ms | 2273.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.34 ms | 3.43 ms | 5.41 ms | 7.53 ms | 23.60 ms | 1778.33 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.02 ms | 3.57 ms | 6.24 ms | 12.77 ms | 153.18 ms | 2861.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 4.22 ms | 3.72 ms | 7.89 ms | 14.30 ms | 32.11 ms | 2918.33 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 5.72 ms | 4.03 ms | 10.35 ms | 46.70 ms | 116.85 ms | 8570.33 | 
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 4.64 ms | 4.20 ms | 8.38 ms | 14.99 ms | 220.05 ms | 4691.67 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 6.01 ms | 4.77 ms | 9.02 ms | 26.97 ms | 79.38 ms | 4479.00 | 
| go (1.11) | [iris](http://iris-go.com) (11.0) | 5.98 ms | 5.00 ms | 9.96 ms | 20.03 ms | 85.14 ms | 3567.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 5.17 ms | 5.10 ms | 8.61 ms | 10.90 ms | 33.62 ms | 2640.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 5.90 ms | 5.19 ms | 8.83 ms | 16.03 ms | 278.29 ms | 5365.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.08 ms | 5.27 ms | 10.09 ms | 20.41 ms | 82.39 ms | 4267.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 6.61 ms | 5.31 ms | 11.23 ms | 23.04 ms | 169.79 ms | 5212.67 | 
| go (1.11) | [beego](http://beego.me) (1.10) | 6.66 ms | 5.39 ms | 11.10 ms | 22.41 ms | 176.37 ms | 5472.67 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 6.51 ms | 5.41 ms | 10.67 ms | 21.15 ms | 215.70 ms | 5288.00 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 7.37 ms | 5.79 ms | 11.72 ms | 24.51 ms | 355.00 ms | 9712.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 7.67 ms | 5.91 ms | 12.32 ms | 22.51 ms | 214.82 ms | 6864.00 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 7.74 ms | 6.11 ms | 12.62 ms | 26.15 ms | 285.80 ms | 8577.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 7.66 ms | 6.14 ms | 12.47 ms | 25.70 ms | 302.07 ms | 8077.00 | 
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 8.93 ms | 6.79 ms | 15.24 ms | 36.28 ms | 334.08 ms | 11343.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 193.48 ms | 6.96 ms | 32.11 ms | 4464.80 ms | 7891.95 ms | 791927.00 | 
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 9.85 ms | 7.28 ms | 16.81 ms | 40.55 ms | 434.33 ms | 15506.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 9.50 ms | 8.41 ms | 14.25 ms | 26.96 ms | 354.31 ms | 13084.00 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 36.47 ms | 8.82 ms | 115.26 ms | 306.49 ms | 856.55 ms | 68010.67 | 
| node (10.12) | [fastify](http://fastify.io) (1.13) | 13.67 ms | 10.39 ms | 22.80 ms | 51.63 ms | 432.63 ms | 17570.33 | 
| python (3.7) | [starlette](http://starlette.io) (0.8) | 11.15 ms | 10.68 ms | 18.95 ms | 26.21 ms | 46.35 ms | 5598.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 11.36 ms | 11.47 ms | 13.36 ms | 15.60 ms | 105.16 ms | 2357.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 13.12 ms | 11.60 ms | 24.23 ms | 43.60 ms | 276.13 ms | 11069.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 16.35 ms | 11.91 ms | 21.34 ms | 87.61 ms | 944.06 ms | 38717.00 | 
| node (10.12) | [koa](http://koajs.com) (2.6) | 15.97 ms | 11.95 ms | 25.71 ms | 67.69 ms | 599.40 ms | 23317.33 | 
| node (10.12) | [restify](http://restify.com) (7.2) | 16.28 ms | 13.26 ms | 26.78 ms | 51.09 ms | 328.63 ms | 13434.00 | 
| node (10.12) | [express](http://expressjs.com) (4.16) | 21.64 ms | 15.62 ms | 31.87 ms | 139.98 ms | 831.64 ms | 38868.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 17.24 ms | 15.69 ms | 29.76 ms | 43.87 ms | 135.54 ms | 9621.67 | 
| node (10.12) | [hapi](http://hapijs.com) (17.7) | 30.62 ms | 19.68 ms | 38.00 ms | 348.25 ms | 1149.24 ms | 66689.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 26.14 ms | 21.22 ms | 47.34 ms | 74.05 ms | 287.41 ms | 16216.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 24.73 ms | 21.51 ms | 33.32 ms | 46.88 ms | 372.52 ms | 10735.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 24.59 ms | 21.78 ms | 37.48 ms | 41.79 ms | 112.38 ms | 6961.67 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 23.91 ms | 22.08 ms | 33.79 ms | 48.74 ms | 238.78 ms | 7858.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 26.88 ms | 23.57 ms | 40.20 ms | 46.18 ms | 309.80 ms | 10949.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 25.47 ms | 24.76 ms | 34.02 ms | 37.90 ms | 299.26 ms | 8470.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 26.87 ms | 24.77 ms | 33.93 ms | 44.96 ms | 233.96 ms | 9187.00 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 30.23 ms | 25.79 ms | 41.81 ms | 95.91 ms | 349.71 ms | 17719.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 38.61 ms | 33.13 ms | 67.67 ms | 120.11 ms | 260.09 ms | 23235.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 41.00 ms | 35.98 ms | 66.34 ms | 105.98 ms | 184.70 ms | 20514.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 50.77 ms | 41.12 ms | 88.00 ms | 170.28 ms | 627.35 ms | 39794.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 73.24 ms | 71.76 ms | 90.88 ms | 107.83 ms | 296.38 ms | 15524.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (actix-web) (rust)


:three: (vibora) (python)


:four: (jester) (nim)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 349126.33 | 417.88 MB |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 318456.00 | 362.01 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 277130.67 | 314.51 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 266085.67 | 534.23 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 264002.67 | 256.33 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 234606.33 | 250.46 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 231473.00 | 373.70 MB |
| java (8) | [act](http://actframework.org) (1.8) | 225777.67 | 441.09 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 215766.00 | 124.88 MB |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 205419.67 | 420.37 MB |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 180841.33 | 228.10 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 172702.33 | 303.13 MB |
| go (1.11) | [iris](http://iris-go.com) (11.0) | 164324.33 | 220.44 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 162532.00 | 264.79 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 150896.33 | 264.92 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 150101.00 | 201.33 MB |
| go (1.11) | [beego](http://beego.me) (1.10) | 149768.33 | 201.88 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 136622.33 | 182.66 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 132193.00 | 176.25 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 132125.33 | 231.93 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 129860.00 | 333.53 MB |
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 124821.67 | 248.58 MB |
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 123610.33 | 185.20 MB |
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 115877.67 | 173.65 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 111967.00 | 275.73 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 91708.67 | 455.21 MB |
| python (3.7) | [starlette](http://starlette.io) (0.8) | 89056.33 | 179.20 MB |
| node (10.12) | [fastify](http://fastify.io) (1.13) | 87739.00 | 206.13 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 86060.67 | 80.90 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 80718.67 | 141.18 MB |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 79300.00 | 392.97 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 74773.67 | 388.32 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 73528.00 | 157.71 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 72318.67 | 121.53 MB |
| node (10.12) | [koa](http://koajs.com) (2.6) | 71491.00 | 150.88 MB |
| node (10.12) | [restify](http://restify.com) (7.2) | 64746.67 | 113.40 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 62753.00 | 326.39 MB |
| c (99) | [kore](http://kore.io) (3.1) | 58995.67 | 159.90 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 58688.00 | 133.10 MB |
| node (10.12) | [express](http://expressjs.com) (4.16) | 55687.67 | 136.13 MB |
| node (10.12) | [hapi](http://hapijs.com) (17.7) | 45829.00 | 118.69 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 45381.33 | 70.06 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 42758.33 | 40.71 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 41752.67 | 77.43 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 40406.00 | 37.90 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 40043.67 | 65.24 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 39211.67 | 36.78 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 39077.67 | 96.31 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 37143.33 | 60.57 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 36543.33 | 44.92 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 33857.00 | 61.79 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 31179.00 | 17.97 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 27074.00 | 48.23 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 24655.00 | 49.19 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 20806.67 | 60.31 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 19415.33 | 11.19 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 18800.67 | 142.24 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 16017.67 | 41.52 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 13351.00 | 35.64 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 3550.33 | 10.81 MB |
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

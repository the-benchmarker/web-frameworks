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
Last update: 2019-01-09
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: iron (rust)


:five: symfony (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 0.09 ms | 0.09 ms | 0.14 ms | 0.19 ms | 2.27 ms | 38.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.15) | 4.68 ms | 0.28 ms | 16.48 ms | 34.96 ms | 86.31 ms | 8224.33 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 5.67 ms | 0.30 ms | 20.52 ms | 42.03 ms | 99.82 ms | 10065.33 | 
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 0.53 ms | 0.49 ms | 0.89 ms | 1.40 ms | 66.98 ms | 807.67 | 
| php (7.2) | [symfony](http://symfony.com) (4.2) | 202.56 ms | 0.50 ms | 319.58 ms | 4575.27 ms | 7585.01 ms | 760116.00 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 163.46 ms | 0.51 ms | 372.52 ms | 3301.37 ms | 7258.27 ms | 575070.67 | 
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 211.35 ms | 0.53 ms | 353.64 ms | 4296.29 ms | 7569.77 ms | 760445.67 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 9.52 ms | 0.56 ms | 31.54 ms | 61.78 ms | 202.56 ms | 15170.33 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 8.90 ms | 0.59 ms | 28.23 ms | 55.14 ms | 122.15 ms | 13409.67 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 10.82 ms | 0.78 ms | 32.73 ms | 63.31 ms | 137.01 ms | 15535.00 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 161.10 ms | 1.12 ms | 297.65 ms | 3543.18 ms | 7341.30 ms | 615781.00 | 
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 138.90 ms | 1.54 ms | 353.31 ms | 2639.80 ms | 5842.05 ms | 459731.33 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 199.81 ms | 1.73 ms | 324.76 ms | 4499.33 ms | 6852.67 ms | 739575.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 155.91 ms | 2.29 ms | 6.40 ms | 4612.35 ms | 6596.47 ms | 743147.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 3.33 ms | 2.95 ms | 5.52 ms | 10.63 ms | 94.81 ms | 2493.67 | 
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 3.98 ms | 3.21 ms | 7.57 ms | 16.21 ms | 95.42 ms | 3731.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 4.06 ms | 3.55 ms | 8.00 ms | 16.35 ms | 38.91 ms | 3525.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.98 ms | 3.98 ms | 10.26 ms | 18.76 ms | 53.46 ms | 4125.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.17 ms | 4.51 ms | 6.06 ms | 10.40 ms | 90.49 ms | 2710.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.5) | 7.19 ms | 4.77 ms | 11.99 ms | 69.79 ms | 127.86 ms | 10734.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.12 ms | 4.83 ms | 7.49 ms | 15.91 ms | 176.05 ms | 5858.00 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 5.12 ms | 4.89 ms | 8.47 ms | 16.67 ms | 49.63 ms | 3133.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 5.55 ms | 4.96 ms | 9.96 ms | 18.30 ms | 38.70 ms | 3541.00 | 
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 5.16 ms | 5.06 ms | 7.60 ms | 15.69 ms | 95.17 ms | 2911.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.42 ms | 5.20 ms | 9.05 ms | 15.79 ms | 74.60 ms | 3117.33 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 39.87 ms | 5.42 ms | 131.22 ms | 328.70 ms | 732.10 ms | 71189.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 6.28 ms | 6.18 ms | 10.64 ms | 13.45 ms | 113.57 ms | 3377.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 7.80 ms | 7.07 ms | 10.79 ms | 20.74 ms | 292.49 ms | 7888.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 8.14 ms | 7.56 ms | 12.56 ms | 25.28 ms | 160.04 ms | 5988.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 8.54 ms | 7.60 ms | 13.13 ms | 26.78 ms | 140.62 ms | 4782.67 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 8.91 ms | 8.24 ms | 13.41 ms | 25.88 ms | 115.95 ms | 4481.00 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 8.85 ms | 8.32 ms | 13.05 ms | 30.43 ms | 180.98 ms | 5235.67 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 9.22 ms | 8.41 ms | 14.17 ms | 27.82 ms | 168.94 ms | 5208.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 178.31 ms | 8.60 ms | 88.01 ms | 4153.07 ms | 7072.12 ms | 721937.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 9.66 ms | 8.63 ms | 14.77 ms | 29.43 ms | 298.41 ms | 8391.33 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 9.60 ms | 8.81 ms | 14.84 ms | 29.39 ms | 116.88 ms | 5432.00 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 9.96 ms | 9.03 ms | 15.26 ms | 30.55 ms | 172.74 ms | 5810.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 10.22 ms | 9.16 ms | 15.77 ms | 32.60 ms | 179.17 ms | 6370.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 13.89 ms | 9.60 ms | 30.04 ms | 60.40 ms | 217.61 ms | 12568.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 16.72 ms | 10.77 ms | 35.63 ms | 72.98 ms | 319.52 ms | 15985.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 12.34 ms | 12.31 ms | 14.86 ms | 17.33 ms | 34.35 ms | 2050.67 | 
| go (1.11) | [gf](http://gfer.me) (1.3) | 13.51 ms | 12.71 ms | 19.25 ms | 38.23 ms | 114.18 ms | 6421.00 | 
| node (11.6) | [polka](http://github.com/lukeed/polka) (0.5) | 20.56 ms | 14.02 ms | 30.59 ms | 140.99 ms | 823.99 ms | 39264.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 23.62 ms | 14.50 ms | 25.06 ms | 308.74 ms | 1350.99 ms | 70990.33 | 
| node (11.6) | [restana](http://github.com/jkyberneees/ana) (2.3) | 22.58 ms | 14.89 ms | 32.94 ms | 210.63 ms | 945.23 ms | 47630.33 | 
| node (11.6) | [foxify](http://foxify.js.org) (0.10) | 24.10 ms | 16.61 ms | 33.60 ms | 187.87 ms | 933.99 ms | 46116.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 18.21 ms | 16.72 ms | 32.98 ms | 54.59 ms | 297.03 ms | 13554.67 | 
| node (11.6) | [fastify](http://fastify.io) (1.13) | 27.05 ms | 17.68 ms | 34.38 ms | 277.83 ms | 1094.29 ms | 59062.33 | 
| node (11.6) | [koa](http://koajs.com) (2.6) | 26.07 ms | 17.87 ms | 33.83 ms | 249.49 ms | 1022.70 ms | 53751.67 | 
| node (11.6) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 24.86 ms | 18.41 ms | 35.75 ms | 167.59 ms | 830.64 ms | 39970.33 | 
| node (11.6) | [rayo](http://rayo.js.org) (1.2) | 25.66 ms | 18.70 ms | 37.37 ms | 170.43 ms | 951.58 ms | 46234.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 27.93 ms | 22.76 ms | 51.35 ms | 80.28 ms | 122.14 ms | 16435.67 | 
| node (11.6) | [restify](http://restify.com) (7.2) | 29.21 ms | 23.35 ms | 42.73 ms | 117.79 ms | 781.43 ms | 35580.33 | 
| node (11.6) | [express](http://expressjs.com) (4.16) | 32.66 ms | 24.74 ms | 44.01 ms | 242.73 ms | 961.58 ms | 49972.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 31.43 ms | 27.49 ms | 43.11 ms | 98.53 ms | 854.52 ms | 31662.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 39.75 ms | 31.88 ms | 73.46 ms | 111.39 ms | 387.91 ms | 23424.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 33.13 ms | 32.14 ms | 41.97 ms | 50.59 ms | 118.02 ms | 7895.33 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 34.75 ms | 35.29 ms | 43.23 ms | 51.42 ms | 124.42 ms | 7009.00 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 38.13 ms | 37.59 ms | 47.53 ms | 62.85 ms | 487.17 ms | 19632.33 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 39.42 ms | 37.66 ms | 49.87 ms | 60.41 ms | 279.34 ms | 9340.67 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 41.55 ms | 39.68 ms | 51.54 ms | 61.86 ms | 348.04 ms | 10487.67 | 
| node (11.6) | [hapi](http://hapijs.com) (17.7) | 67.44 ms | 40.28 ms | 69.65 ms | 1005.99 ms | 2191.98 ms | 159091.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 43.30 ms | 41.66 ms | 55.01 ms | 65.35 ms | 325.81 ms | 12181.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 70.72 ms | 50.32 ms | 150.38 ms | 195.14 ms | 490.19 ms | 46099.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 59.21 ms | 52.69 ms | 104.58 ms | 161.87 ms | 309.29 ms | 32232.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 70.66 ms | 62.82 ms | 127.29 ms | 174.29 ms | 245.48 ms | 37514.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 100.71 ms | 100.38 ms | 121.57 ms | 142.31 ms | 494.87 ms | 22374.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (vibora) (python)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 289366.33 | 167.45 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 244751.33 | 293.05 MB |
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 239642.33 | 272.17 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 214065.67 | 243.03 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 212655.67 | 206.34 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 194210.00 | 390.07 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 188291.00 | 303.25 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 184515.00 | 173.53 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 176835.67 | 189.72 MB |
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 176426.67 | 361.27 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.5) | 173425.67 | 100.19 MB |
| java (8) | [act](http://actframework.org) (1.8) | 146586.33 | 286.06 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 123727.00 | 201.68 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 115941.00 | 153.73 MB |
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 115173.67 | 145.44 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 113489.33 | 199.29 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 109656.33 | 147.31 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 106535.67 | 143.27 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 102463.00 | 179.94 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 102438.67 | 179.84 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 100058.33 | 133.92 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 98169.67 | 130.16 MB |
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 94605.00 | 188.41 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 81249.67 | 208.82 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 80282.33 | 75.42 MB |
| go (1.11) | [gf](http://gfer.me) (1.3) | 74531.33 | 113.46 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 68642.00 | 169.23 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 61075.67 | 102.19 MB |
| node (11.6) | [polka](http://github.com/lukeed/polka) (0.5) | 60852.67 | 91.03 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 59547.33 | 127.79 MB |
| node (11.6) | [restana](http://github.com/jkyberneees/ana) (2.3) | 58350.00 | 87.38 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 58325.00 | 102.18 MB |
| node (11.6) | [foxify](http://foxify.js.org) (0.10) | 52596.00 | 110.41 MB |
| node (11.6) | [fastify](http://fastify.io) (1.13) | 51390.33 | 124.21 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 50114.00 | 248.89 MB |
| node (11.6) | [koa](http://koajs.com) (2.6) | 49765.67 | 105.31 MB |
| node (11.6) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 49559.00 | 74.20 MB |
| c (99) | [kore](http://kore.io) (3.1) | 48087.33 | 130.28 MB |
| node (11.6) | [rayo](http://rayo.js.org) (1.2) | 47775.67 | 71.51 MB |
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 45411.67 | 225.45 MB |
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 44914.33 | 223.20 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 44691.00 | 232.19 MB |
| php (7.2) | [symfony](http://symfony.com) (4.2) | 44103.00 | 218.84 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 39698.33 | 63.31 MB |
| node (11.6) | [restify](http://restify.com) (7.2) | 38210.67 | 66.92 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 37248.33 | 194.31 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 37001.67 | 83.91 MB |
| node (11.6) | [express](http://expressjs.com) (4.16) | 35830.00 | 87.54 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 33277.33 | 61.73 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 30198.33 | 28.32 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 28787.33 | 26.98 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.15) | 27263.67 | 25.98 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 26751.33 | 43.58 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 25657.67 | 63.24 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 25139.67 | 30.96 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 23911.33 | 43.65 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 23098.67 | 37.63 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 22494.00 | 12.98 MB |
| node (11.6) | [hapi](http://hapijs.com) (17.7) | 22466.67 | 58.08 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 17275.67 | 30.78 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 15291.33 | 44.40 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 14506.33 | 28.93 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 14363.33 | 8.28 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 13474.00 | 101.95 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 11811.67 | 30.64 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 9684.00 | 28.77 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3200.00 | 9.79 MB |
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

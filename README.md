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
+ [wrk](https://github.com/wg/wrk) as benchmarking tool, `>= 4.1.0`

:information_source: you need `wrk` **stable**

~~~sh
git clone --branch 4.1.0 https://github.com/wg/wrk
~~~

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
Last update: 2019-01-31
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: zend-framework (php)


:five: symfony (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.09 ms | 0.09 ms | 0.13 ms | 0.16 ms | 4.12 ms | 49.00 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 3.68 ms | 0.21 ms | 13.21 ms | 32.10 ms | 82.18 ms | 7146.00 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.85 ms | 0.27 ms | 17.22 ms | 37.00 ms | 92.77 ms | 8674.00 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 104.72 ms | 0.33 ms | 228.38 ms | 2160.86 ms | 6806.60 ms | 420376.33 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 97.50 ms | 0.33 ms | 209.42 ms | 2127.58 ms | 6786.07 ms | 372973.67 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 112.66 ms | 0.33 ms | 225.82 ms | 2295.03 ms | 6732.52 ms | 420673.33 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 149.89 ms | 0.35 ms | 252.99 ms | 3343.85 ms | 6903.94 ms | 570382.67 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.40 ms | 0.40 ms | 0.64 ms | 0.89 ms | 14.87 ms | 231.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 7.52 ms | 0.45 ms | 24.80 ms | 50.93 ms | 119.97 ms | 12083.67 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 8.12 ms | 0.51 ms | 26.43 ms | 52.03 ms | 157.22 ms | 12727.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 9.65 ms | 0.63 ms | 30.17 ms | 60.40 ms | 138.10 ms | 14504.00 | 
| php (7.3) | [laravel](http://laravel.com) (5.7) | 129.13 ms | 0.94 ms | 323.49 ms | 2438.39 ms | 6866.22 ms | 441424.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 99.43 ms | 1.67 ms | 4.58 ms | 3135.76 ms | 6594.22 ms | 554563.00 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 136.93 ms | 1.78 ms | 246.01 ms | 2817.40 ms | 5821.78 ms | 506328.33 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 3.47 ms | 2.46 ms | 7.51 ms | 16.11 ms | 96.55 ms | 3723.33 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 3.17 ms | 2.81 ms | 5.30 ms | 10.14 ms | 72.38 ms | 1975.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.84 ms | 3.06 ms | 7.88 ms | 16.59 ms | 34.79 ms | 3532.33 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.96 ms | 3.48 ms | 6.16 ms | 12.61 ms | 55.40 ms | 2468.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.53 ms | 3.69 ms | 5.48 ms | 7.75 ms | 20.23 ms | 1801.00 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 4.20 ms | 3.95 ms | 7.17 ms | 13.98 ms | 33.02 ms | 2737.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.91 ms | 4.04 ms | 9.60 ms | 16.71 ms | 40.41 ms | 3649.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 4.92 ms | 4.33 ms | 9.17 ms | 16.94 ms | 34.80 ms | 3365.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.78 ms | 4.38 ms | 7.44 ms | 13.45 ms | 49.80 ms | 2675.33 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 4.96 ms | 4.44 ms | 9.39 ms | 20.15 ms | 99.32 ms | 4274.33 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.7) | 6.85 ms | 4.71 ms | 12.31 ms | 53.48 ms | 124.42 ms | 9356.00 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 40.84 ms | 4.79 ms | 134.30 ms | 355.59 ms | 1016.23 ms | 76917.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 5.45 ms | 5.37 ms | 9.18 ms | 11.32 ms | 164.38 ms | 2981.33 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 6.43 ms | 5.38 ms | 10.56 ms | 20.83 ms | 159.66 ms | 4890.00 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 6.83 ms | 5.68 ms | 11.07 ms | 21.67 ms | 109.95 ms | 3905.33 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 7.03 ms | 5.77 ms | 11.59 ms | 22.33 ms | 160.43 ms | 4451.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.59 ms | 5.77 ms | 11.19 ms | 21.26 ms | 74.57 ms | 4365.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 6.56 ms | 5.90 ms | 9.56 ms | 16.63 ms | 159.41 ms | 3871.00 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 7.52 ms | 6.01 ms | 12.41 ms | 25.38 ms | 230.46 ms | 7158.33 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 7.59 ms | 6.15 ms | 12.64 ms | 25.53 ms | 220.25 ms | 5827.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 7.79 ms | 6.42 ms | 12.84 ms | 25.33 ms | 171.28 ms | 5822.33 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 8.10 ms | 6.48 ms | 13.07 ms | 27.39 ms | 237.06 ms | 8759.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 220.64 ms | 7.98 ms | 126.00 ms | 4991.24 ms | 7934.19 ms | 867491.33 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 9.74 ms | 8.84 ms | 17.28 ms | 30.87 ms | 208.15 ms | 6843.00 | 
| go (1.11) | [gf](http://gfer.me) (1.4) | 10.20 ms | 9.37 ms | 14.88 ms | 31.36 ms | 159.87 ms | 5623.00 | 
| node (11.8) | [restana](http://github.com/jkyberneees/ana) (2.7) | 13.62 ms | 9.43 ms | 23.66 ms | 61.15 ms | 483.11 ms | 19880.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 11.47 ms | 10.07 ms | 19.51 ms | 33.73 ms | 221.82 ms | 8415.33 | 
| node (11.8) | [polka](http://github.com/lukeed/polka) (0.5) | 16.61 ms | 11.58 ms | 28.03 ms | 69.56 ms | 648.21 ms | 25945.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 16.54 ms | 11.65 ms | 21.07 ms | 118.78 ms | 981.94 ms | 42118.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 14.52 ms | 12.59 ms | 25.00 ms | 44.34 ms | 657.47 ms | 17864.67 | 
| python (3.7) | [starlette](http://starlette.io) (0.10) | 15.11 ms | 12.75 ms | 26.70 ms | 43.46 ms | 125.49 ms | 8881.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 12.67 ms | 12.77 ms | 14.39 ms | 16.13 ms | 44.12 ms | 1570.33 | 
| node (11.8) | [foxify](http://foxify.js.org) (0.10) | 19.10 ms | 13.16 ms | 29.55 ms | 130.29 ms | 706.95 ms | 31914.00 | 
| node (11.8) | [rayo](http://rayo.js.org) (1.2) | 17.91 ms | 13.28 ms | 29.56 ms | 73.08 ms | 627.90 ms | 25199.33 | 
| node (11.8) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 20.75 ms | 14.13 ms | 31.24 ms | 161.67 ms | 790.48 ms | 38878.33 | 
| node (11.8) | [fastify](http://fastify.io) (1.13) | 22.70 ms | 14.49 ms | 33.03 ms | 195.08 ms | 1010.33 ms | 50002.33 | 
| node (11.6) | [koa](http://koajs.com) (2.6) | 23.69 ms | 16.59 ms | 33.26 ms | 176.19 ms | 989.91 ms | 47587.00 | 
| node (11.8) | [express](http://expressjs.com) (4.16) | 27.91 ms | 18.53 ms | 37.03 ms | 294.81 ms | 1107.41 ms | 60071.67 | 
| node (11.8) | [restify](http://restify.com) (7.6) | 24.53 ms | 20.15 ms | 40.28 ms | 74.29 ms | 518.51 ms | 21151.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 26.30 ms | 20.36 ms | 52.66 ms | 72.90 ms | 106.82 ms | 16309.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 28.65 ms | 22.26 ms | 38.43 ms | 142.81 ms | 1203.43 ms | 51846.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 25.10 ms | 22.90 ms | 34.57 ms | 47.52 ms | 314.56 ms | 8263.00 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 28.78 ms | 25.60 ms | 41.57 ms | 50.79 ms | 171.45 ms | 8241.33 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 30.96 ms | 27.46 ms | 42.93 ms | 58.08 ms | 542.63 ms | 21983.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 29.02 ms | 30.07 ms | 37.99 ms | 46.61 ms | 314.14 ms | 10693.67 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 38.86 ms | 32.30 ms | 45.53 ms | 301.62 ms | 895.49 ms | 56705.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 37.66 ms | 33.13 ms | 54.03 ms | 144.61 ms | 586.12 ms | 26541.00 | 
| node (11.8) | [hapi](http://hapijs.com) (18.0) | 70.26 ms | 36.38 ms | 67.65 ms | 1151.59 ms | 2307.98 ms | 183865.00 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 39.01 ms | 39.61 ms | 47.85 ms | 56.56 ms | 330.07 ms | 12370.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 50.42 ms | 44.54 ms | 85.91 ms | 140.23 ms | 274.30 ms | 27466.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 69.09 ms | 50.43 ms | 124.29 ms | 347.08 ms | 757.33 ms | 64163.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 63.50 ms | 56.84 ms | 108.90 ms | 152.43 ms | 196.94 ms | 31906.67 | 
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.1) | 81.47 ms | 74.26 ms | 144.98 ms | 185.76 ms | 228.22 ms | 40153.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 91.85 ms | 91.49 ms | 115.05 ms | 131.73 ms | 572.10 ms | 24163.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (actix-web) (rust)


:three: (japronto) (python)


:four: (evhtp) (cpp)


:five: (fasthttprouter) (go)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 314264.00 | 181.92 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 296663.00 | 337.34 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 267234.33 | 319.89 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 250585.33 | 243.34 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 235265.00 | 378.63 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 225112.00 | 211.82 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 219075.33 | 440.46 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 211258.00 | 239.89 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 204009.33 | 417.79 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 200534.67 | 214.56 MB |
| java (8) | [act](http://actframework.org) (1.8) | 200184.67 | 390.67 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.7) | 174780.00 | 101.01 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 151124.00 | 190.72 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 149930.67 | 201.03 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 142427.67 | 190.99 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 141991.00 | 231.23 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 139615.33 | 186.90 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 134024.67 | 235.43 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 130160.33 | 228.61 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 127832.33 | 170.81 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 124439.33 | 167.52 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 104096.00 | 267.68 MB |
| go (1.11) | [gf](http://gfer.me) (1.4) | 98078.67 | 148.90 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 88994.33 | 176.41 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 88577.00 | 218.15 MB |
| node (11.8) | [restana](http://github.com/jkyberneees/ana) (2.7) | 85618.33 | 128.16 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 80469.67 | 75.59 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 76702.67 | 134.29 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 72878.33 | 122.49 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 71837.00 | 153.82 MB |
| node (11.8) | [polka](http://github.com/lukeed/polka) (0.5) | 71140.33 | 106.57 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 71089.00 | 352.63 MB |
| python (3.7) | [starlette](http://starlette.io) (0.10) | 67898.67 | 146.13 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 65871.67 | 326.72 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 64573.67 | 320.00 MB |
| node (11.8) | [rayo](http://rayo.js.org) (1.2) | 64199.33 | 95.97 MB |
| node (11.8) | [foxify](http://foxify.js.org) (0.10) | 63508.00 | 133.42 MB |
| node (11.8) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 62295.67 | 93.16 MB |
| node (11.8) | [fastify](http://fastify.io) (1.13) | 60628.00 | 145.82 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 60610.00 | 300.31 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 60304.33 | 313.06 MB |
| c (99) | [kore](http://kore.io) (3.1) | 58146.67 | 157.55 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 53745.67 | 84.78 MB |
| node (11.6) | [koa](http://koajs.com) (2.6) | 52977.33 | 111.92 MB |
| php (7.3) | [laravel](http://laravel.com) (5.7) | 52858.33 | 275.58 MB |
| node (11.8) | [express](http://expressjs.com) (4.16) | 47280.33 | 115.65 MB |
| node (11.8) | [restify](http://restify.com) (7.6) | 42904.67 | 75.08 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 39694.00 | 90.07 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 39527.67 | 37.04 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 39420.67 | 73.09 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 34781.33 | 33.16 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 34662.33 | 32.47 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 34332.67 | 55.92 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 32182.33 | 39.55 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 29324.33 | 53.51 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 27612.67 | 67.95 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 26345.33 | 15.20 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 26248.67 | 42.75 MB |
| node (11.8) | [hapi](http://hapijs.com) (18.0) | 25057.67 | 64.73 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 20327.67 | 36.20 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 17010.33 | 9.81 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 15935.33 | 46.16 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 15898.33 | 31.66 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 15729.67 | 118.89 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 13253.33 | 34.35 MB |
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.1) | 12326.67 | 26.83 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 10621.67 | 31.32 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3124.33 | 9.52 MB |
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

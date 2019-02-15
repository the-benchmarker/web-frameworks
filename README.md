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
Last update: 2019-02-14
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
threads: 9, connections: 1000
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
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.09 ms | 0.09 ms | 0.13 ms | 0.18 ms | 0.82 ms | 34.00 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 4.52 ms | 0.26 ms | 16.11 ms | 34.53 ms | 85.84 ms | 8094.67 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 5.46 ms | 0.29 ms | 19.69 ms | 40.52 ms | 100.57 ms | 9683.00 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.45 ms | 0.42 ms | 0.76 ms | 1.14 ms | 8.84 ms | 260.33 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 214.02 ms | 0.44 ms | 391.91 ms | 4572.92 ms | 7391.84 ms | 760530.67 | 
| php (7.3) | [laravel](http://laravel.com) (5.7) | 134.76 ms | 0.45 ms | 359.48 ms | 2430.30 ms | 6989.86 ms | 451592.00 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 223.03 ms | 0.47 ms | 348.02 ms | 4727.11 ms | 7419.47 ms | 807267.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 8.18 ms | 0.51 ms | 26.25 ms | 52.67 ms | 172.84 ms | 13210.33 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 235.38 ms | 0.51 ms | 407.59 ms | 5176.69 ms | 7673.65 ms | 851826.33 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 8.78 ms | 0.51 ms | 28.93 ms | 56.47 ms | 128.89 ms | 13849.67 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 211.16 ms | 0.51 ms | 367.16 ms | 4966.53 ms | 7600.04 ms | 792336.33 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 182.12 ms | 0.64 ms | 397.15 ms | 3643.94 ms | 7043.48 ms | 636646.67 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 10.63 ms | 0.71 ms | 32.29 ms | 63.95 ms | 241.69 ms | 16749.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 60.05 ms | 1.96 ms | 4.33 ms | 1901.74 ms | 6572.90 ms | 378977.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.39 ms | 2.74 ms | 6.58 ms | 13.73 ms | 30.62 ms | 2952.67 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 3.44 ms | 2.80 ms | 6.37 ms | 12.80 ms | 36.66 ms | 2679.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 3.00 ms | 2.85 ms | 4.76 ms | 8.74 ms | 103.67 ms | 4317.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.56 ms | 3.56 ms | 9.16 ms | 17.34 ms | 39.92 ms | 3697.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.74 ms | 3.88 ms | 5.55 ms | 8.64 ms | 109.36 ms | 3943.67 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 4.30 ms | 4.10 ms | 7.12 ms | 13.97 ms | 30.68 ms | 2663.00 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.75 ms | 4.27 ms | 6.81 ms | 14.78 ms | 206.84 ms | 6627.33 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 4.72 ms | 4.29 ms | 8.50 ms | 15.59 ms | 33.98 ms | 3071.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.89 ms | 4.30 ms | 7.43 ms | 13.73 ms | 121.57 ms | 4984.00 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.7) | 7.15 ms | 4.45 ms | 11.60 ms | 76.11 ms | 120.78 ms | 11860.67 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 38.52 ms | 4.72 ms | 128.66 ms | 326.80 ms | 839.03 ms | 70479.33 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 5.28 ms | 4.92 ms | 8.87 ms | 19.46 ms | 158.01 ms | 4652.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 5.72 ms | 5.66 ms | 9.63 ms | 11.89 ms | 239.83 ms | 3260.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 7.09 ms | 5.98 ms | 9.73 ms | 20.55 ms | 289.10 ms | 9240.67 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 7.52 ms | 6.42 ms | 11.63 ms | 23.72 ms | 211.34 ms | 5002.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 7.12 ms | 6.76 ms | 10.95 ms | 20.13 ms | 198.11 ms | 5008.67 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 7.93 ms | 7.05 ms | 12.04 ms | 23.69 ms | 62.92 ms | 4001.67 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 8.25 ms | 7.28 ms | 12.94 ms | 25.40 ms | 153.91 ms | 4594.67 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 8.62 ms | 7.55 ms | 13.50 ms | 27.54 ms | 218.03 ms | 6018.33 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 8.84 ms | 7.86 ms | 13.71 ms | 28.51 ms | 211.85 ms | 6310.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 9.05 ms | 8.07 ms | 14.14 ms | 29.23 ms | 158.27 ms | 5490.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 9.10 ms | 8.27 ms | 14.15 ms | 27.62 ms | 116.51 ms | 4902.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 196.19 ms | 8.67 ms | 273.83 ms | 3953.96 ms | 6745.39 ms | 695894.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 12.16 ms | 9.65 ms | 22.89 ms | 41.83 ms | 220.63 ms | 9417.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 14.38 ms | 9.78 ms | 30.62 ms | 55.58 ms | 243.87 ms | 14136.33 | 
| node (11.9) | [restana](http://github.com/jkyberneees/ana) (2.8) | 16.95 ms | 11.21 ms | 27.48 ms | 96.89 ms | 705.46 ms | 32182.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 11.52 ms | 11.54 ms | 13.70 ms | 15.94 ms | 28.84 ms | 1832.33 | 
| go (1.11) | [gf](http://gfer.me) (1.4) | 13.38 ms | 12.15 ms | 18.66 ms | 43.45 ms | 374.27 ms | 12233.33 | 
| python (3.7) | [starlette](http://starlette.io) (0.10) | 15.27 ms | 13.01 ms | 26.56 ms | 46.73 ms | 157.73 ms | 10329.33 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 19.38 ms | 13.01 ms | 23.55 ms | 192.21 ms | 1182.86 ms | 53105.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 19.85 ms | 13.86 ms | 29.04 ms | 180.83 ms | 1007.87 ms | 48690.67 | 
| node (11.9) | [polka](http://github.com/lukeed/polka) (0.5) | 20.40 ms | 15.52 ms | 30.75 ms | 82.69 ms | 717.99 ms | 30868.67 | 
| node (11.9) | [foxify](http://foxify.js.org) (0.10) | 24.98 ms | 16.51 ms | 28.54 ms | 292.70 ms | 1042.00 ms | 57797.33 | 
| node (11.9) | [express](http://expressjs.com) (4.16) | 23.82 ms | 17.14 ms | 28.55 ms | 234.79 ms | 1020.09 ms | 47295.00 | 
| node (11.9) | [rayo](http://rayo.js.org) (1.2) | 22.37 ms | 17.38 ms | 33.14 ms | 88.20 ms | 777.49 ms | 33382.00 | 
| node (11.9) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 26.91 ms | 17.95 ms | 36.08 ms | 221.02 ms | 1032.86 ms | 52583.67 | 
| node (11.9) | [fastify](http://fastify.io) (1.14) | 29.04 ms | 18.46 ms | 34.35 ms | 343.25 ms | 1034.29 ms | 63722.33 | 
| node (11.9) | [koa](http://koajs.com) (2.7) | 29.25 ms | 20.18 ms | 37.54 ms | 292.78 ms | 1103.45 ms | 59368.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 25.04 ms | 21.64 ms | 45.72 ms | 63.35 ms | 111.14 ms | 14011.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 32.73 ms | 24.32 ms | 43.05 ms | 207.02 ms | 1500.18 ms | 61311.67 | 
| node (11.9) | [restify](http://restify.com) (7.7) | 32.02 ms | 24.75 ms | 47.71 ms | 168.52 ms | 867.26 ms | 41763.67 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 30.04 ms | 27.29 ms | 40.88 ms | 47.17 ms | 298.18 ms | 10365.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 29.39 ms | 29.02 ms | 37.44 ms | 42.53 ms | 164.69 ms | 7266.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 36.42 ms | 29.52 ms | 63.41 ms | 96.83 ms | 425.49 ms | 21502.00 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 35.99 ms | 33.30 ms | 46.30 ms | 53.45 ms | 407.71 ms | 13015.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 33.22 ms | 34.07 ms | 41.60 ms | 47.48 ms | 188.13 ms | 6979.33 | 
| node (11.9) | [hapi](http://hapijs.com) (18.1) | 69.14 ms | 37.91 ms | 65.56 ms | 1097.59 ms | 2172.69 ms | 171905.00 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 41.19 ms | 40.81 ms | 46.94 ms | 55.80 ms | 222.60 ms | 8234.00 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 46.88 ms | 44.42 ms | 52.59 ms | 177.43 ms | 869.52 ms | 38835.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 57.26 ms | 48.22 ms | 103.49 ms | 192.92 ms | 361.97 ms | 38120.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 62.85 ms | 48.72 ms | 121.12 ms | 188.55 ms | 685.09 ms | 39283.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 63.77 ms | 55.78 ms | 117.24 ms | 156.32 ms | 236.51 ms | 35352.00 | 
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.1) | 87.76 ms | 74.99 ms | 156.21 ms | 195.33 ms | 258.42 ms | 43429.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 88.19 ms | 87.34 ms | 105.28 ms | 143.32 ms | 429.78 ms | 21743.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (evhtp) (cpp)


:five: (vibora) (python)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 335074.00 | 193.64 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 291662.33 | 348.90 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 269260.00 | 306.25 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 244674.33 | 237.25 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 236544.67 | 268.62 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 219034.00 | 206.04 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 217933.67 | 438.03 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 206531.33 | 220.16 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 205569.67 | 331.45 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.7) | 191147.67 | 110.46 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 187411.33 | 383.33 MB |
| java (8) | [act](http://actframework.org) (1.8) | 171726.00 | 335.14 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 140154.33 | 228.42 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 131877.00 | 166.69 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 130059.67 | 174.04 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 123925.00 | 165.66 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 118686.00 | 158.56 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 114929.00 | 201.76 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 111393.00 | 150.06 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 110447.33 | 193.95 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 109046.00 | 145.59 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 90817.67 | 180.16 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 86229.33 | 81.05 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 85684.00 | 220.28 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 79926.00 | 196.89 MB |
| go (1.11) | [gf](http://gfer.me) (1.4) | 77889.67 | 118.47 MB |
| node (11.9) | [restana](http://github.com/jkyberneees/ana) (2.8) | 73895.67 | 110.58 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 69285.67 | 121.37 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 68061.67 | 113.86 MB |
| python (3.7) | [starlette](http://starlette.io) (0.10) | 67997.00 | 146.56 MB |
| node (11.9) | [polka](http://github.com/lukeed/polka) (0.5) | 56459.67 | 84.50 MB |
| c (99) | [kore](http://kore.io) (3.1) | 55306.00 | 149.92 MB |
| node (11.9) | [foxify](http://foxify.js.org) (0.10) | 55045.33 | 115.60 MB |
| node (11.9) | [express](http://expressjs.com) (4.16) | 53427.67 | 130.71 MB |
| node (11.9) | [rayo](http://rayo.js.org) (1.2) | 50532.00 | 75.60 MB |
| node (11.9) | [fastify](http://fastify.io) (1.14) | 49387.67 | 121.88 MB |
| node (11.9) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 48942.33 | 73.13 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 48743.00 | 105.25 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 47324.33 | 234.85 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 46950.00 | 233.07 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 46904.33 | 233.77 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 46652.33 | 231.81 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 44709.33 | 71.27 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 44047.33 | 228.94 MB |
| node (11.9) | [koa](http://koajs.com) (2.7) | 43954.33 | 92.87 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 40910.00 | 92.80 MB |
| php (7.3) | [laravel](http://laravel.com) (5.7) | 38410.67 | 200.92 MB |
| node (11.9) | [restify](http://restify.com) (7.7) | 35593.00 | 62.37 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 35149.00 | 65.15 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 34104.00 | 32.00 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 33005.00 | 53.80 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 30373.00 | 28.46 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 28196.00 | 26.88 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 28096.33 | 69.27 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 27748.00 | 34.04 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 24225.33 | 44.22 MB |
| node (11.9) | [hapi](http://hapijs.com) (18.1) | 24123.33 | 62.30 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 23388.67 | 13.50 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 22226.33 | 36.21 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 18373.00 | 32.72 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 16564.00 | 48.02 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 15838.00 | 31.57 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 15689.00 | 9.05 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 14507.00 | 109.73 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 12165.33 | 31.54 MB |
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.1) | 11382.33 | 24.81 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 11051.33 | 32.62 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3315.00 | 10.15 MB |
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

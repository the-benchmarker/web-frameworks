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
Last update: 2019-02-04
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
threads: 9, connections: 1000
```
Benchmark running ...

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: slim (php)


:five: flame (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.07 ms | 0.07 ms | 0.09 ms | 0.12 ms | 1.77 ms | 23.00 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 2.96 ms | 0.15 ms | 10.82 ms | 27.99 ms | 105.37 ms | 6350.00 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.63 ms | 0.18 ms | 13.76 ms | 32.37 ms | 88.81 ms | 7306.67 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 87.34 ms | 0.28 ms | 164.34 ms | 1882.45 ms | 6739.96 ms | 383637.33 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.44 ms | 0.29 ms | 19.49 ms | 43.10 ms | 102.48 ms | 9928.00 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.30 ms | 0.30 ms | 0.49 ms | 0.79 ms | 11.06 ms | 192.33 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 6.06 ms | 0.32 ms | 21.41 ms | 43.85 ms | 107.01 ms | 10509.00 | 
| php (7.3) | [laravel](http://laravel.com) (5.7) | 138.73 ms | 0.33 ms | 304.85 ms | 2634.44 ms | 6879.47 ms | 480871.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 7.38 ms | 0.42 ms | 24.75 ms | 50.29 ms | 164.64 ms | 12079.67 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 102.30 ms | 1.26 ms | 192.89 ms | 2169.20 ms | 5416.05 ms | 392409.67 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 78.32 ms | 1.33 ms | 184.49 ms | 1453.55 ms | 5645.02 ms | 328443.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 45.11 ms | 1.33 ms | 2.96 ms | 1505.26 ms | 5488.50 ms | 292830.67 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 97.79 ms | 1.50 ms | 189.21 ms | 2083.03 ms | 7100.27 ms | 418646.33 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 29.89 ms | 2.00 ms | 102.15 ms | 283.85 ms | 728.17 ms | 60259.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.85 ms | 2.01 ms | 6.03 ms | 13.62 ms | 36.30 ms | 2863.67 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 2.81 ms | 2.18 ms | 5.79 ms | 13.20 ms | 34.88 ms | 2676.33 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 2.51 ms | 2.27 ms | 4.23 ms | 10.63 ms | 37.09 ms | 2077.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 2.98 ms | 2.30 ms | 5.51 ms | 9.63 ms | 43.97 ms | 2140.33 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 69.65 ms | 2.47 ms | 148.79 ms | 1444.01 ms | 4182.47 ms | 275843.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.41 ms | 2.83 ms | 7.22 ms | 13.16 ms | 28.59 ms | 2929.33 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 3.64 ms | 3.03 ms | 6.94 ms | 12.87 ms | 43.05 ms | 2738.67 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 3.47 ms | 3.10 ms | 6.15 ms | 11.93 ms | 24.55 ms | 2391.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.22 ms | 3.22 ms | 5.36 ms | 8.06 ms | 94.31 ms | 2108.67 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 4.07 ms | 3.23 ms | 7.83 ms | 18.09 ms | 98.57 ms | 3922.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.05 ms | 3.28 ms | 7.01 ms | 13.21 ms | 31.62 ms | 2487.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.7) | 4.85 ms | 3.58 ms | 7.34 ms | 54.25 ms | 116.45 ms | 8675.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 5.18 ms | 4.50 ms | 8.70 ms | 18.25 ms | 156.45 ms | 4630.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 5.13 ms | 4.60 ms | 7.52 ms | 15.36 ms | 218.06 ms | 4671.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 4.65 ms | 4.60 ms | 7.80 ms | 9.42 ms | 20.45 ms | 2367.00 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 5.41 ms | 4.65 ms | 9.14 ms | 18.17 ms | 89.63 ms | 3292.00 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 5.61 ms | 4.69 ms | 9.41 ms | 18.51 ms | 215.20 ms | 5338.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.59 ms | 4.77 ms | 9.18 ms | 19.54 ms | 161.36 ms | 4617.33 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 6.17 ms | 4.99 ms | 10.47 ms | 21.29 ms | 206.91 ms | 4912.67 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 6.19 ms | 5.01 ms | 10.38 ms | 21.69 ms | 262.20 ms | 5603.00 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 6.09 ms | 5.05 ms | 10.26 ms | 19.86 ms | 54.25 ms | 3668.33 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 6.50 ms | 5.29 ms | 10.99 ms | 22.56 ms | 105.45 ms | 4380.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 7.11 ms | 5.66 ms | 12.27 ms | 20.88 ms | 155.03 ms | 4690.00 | 
| node (11.9) | [polka](http://github.com/lukeed/polka) (0.5) | 9.11 ms | 6.77 ms | 16.78 ms | 37.52 ms | 278.93 ms | 9898.33 | 
| node (11.9) | [restana](http://github.com/jkyberneees/ana) (2.7) | 10.12 ms | 7.00 ms | 19.05 ms | 43.85 ms | 406.51 ms | 14828.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 232.21 ms | 7.12 ms | 254.33 ms | 4884.37 ms | 7687.23 ms | 866146.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 8.85 ms | 7.63 ms | 14.84 ms | 26.98 ms | 211.69 ms | 6492.00 | 
| go (1.11) | [gf](http://gfer.me) (1.4) | 8.85 ms | 7.97 ms | 13.14 ms | 29.87 ms | 284.07 ms | 7186.33 | 
| node (11.9) | [foxify](http://foxify.js.org) (0.10) | 11.43 ms | 8.31 ms | 20.41 ms | 44.82 ms | 391.80 ms | 13673.67 | 
| node (11.9) | [rayo](http://rayo.js.org) (1.2) | 11.75 ms | 8.37 ms | 20.80 ms | 46.39 ms | 479.06 ms | 18103.00 | 
| python (3.7) | [starlette](http://starlette.io) (0.10) | 10.50 ms | 8.93 ms | 18.88 ms | 30.03 ms | 84.84 ms | 6296.00 | 
| node (11.9) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 13.58 ms | 9.79 ms | 21.21 ms | 62.71 ms | 635.40 ms | 25372.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 10.31 ms | 10.32 ms | 11.97 ms | 14.01 ms | 104.56 ms | 3042.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 14.12 ms | 10.69 ms | 22.33 ms | 80.69 ms | 724.84 ms | 31285.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 14.21 ms | 10.77 ms | 18.94 ms | 53.47 ms | 910.53 ms | 31405.33 | 
| node (11.9) | [fastify](http://fastify.io) (1.13) | 15.72 ms | 11.15 ms | 24.96 ms | 111.12 ms | 582.58 ms | 26704.67 | 
| node (11.9) | [koa](http://koajs.com) (2.7) | 17.26 ms | 12.16 ms | 26.16 ms | 123.76 ms | 642.64 ms | 30543.00 | 
| node (11.9) | [restify](http://restify.com) (7.6) | 18.09 ms | 14.74 ms | 29.80 ms | 56.61 ms | 445.86 ms | 17707.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 16.38 ms | 15.08 ms | 29.85 ms | 39.46 ms | 63.88 ms | 8566.67 | 
| node (11.9) | [express](http://expressjs.com) (4.16) | 21.34 ms | 15.16 ms | 32.12 ms | 127.82 ms | 826.89 ms | 38746.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 22.58 ms | 18.74 ms | 33.02 ms | 52.61 ms | 767.06 ms | 26561.33 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 23.17 ms | 20.56 ms | 30.96 ms | 36.74 ms | 308.71 ms | 9959.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 26.95 ms | 22.34 ms | 45.21 ms | 70.73 ms | 426.49 ms | 16877.67 | 
| node (11.9) | [hapi](http://hapijs.com) (18.0) | 34.41 ms | 24.47 ms | 44.11 ms | 333.12 ms | 1183.29 ms | 65084.33 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 27.90 ms | 26.28 ms | 36.96 ms | 42.47 ms | 115.25 ms | 6954.33 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 25.56 ms | 26.95 ms | 33.10 ms | 39.31 ms | 257.53 ms | 9369.67 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 27.12 ms | 28.42 ms | 35.25 ms | 42.28 ms | 346.38 ms | 14331.00 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 30.21 ms | 28.77 ms | 40.64 ms | 46.96 ms | 391.06 ms | 13971.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 37.99 ms | 31.39 ms | 69.82 ms | 130.13 ms | 233.39 ms | 25255.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 38.18 ms | 36.71 ms | 46.84 ms | 53.82 ms | 344.42 ms | 13280.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 47.35 ms | 37.27 ms | 89.90 ms | 127.36 ms | 437.74 ms | 26954.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 45.03 ms | 43.08 ms | 71.65 ms | 101.95 ms | 158.98 ms | 21064.00 | 
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.1) | 52.29 ms | 47.01 ms | 89.81 ms | 119.28 ms | 203.93 ms | 27315.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 68.52 ms | 67.28 ms | 85.69 ms | 109.81 ms | 698.28 ms | 22569.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (fasthttprouter) (go)


:five: (vibora) (python)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 397861.00 | 230.23 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 373867.67 | 447.42 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 359837.00 | 408.77 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 313994.33 | 507.19 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 312544.33 | 354.88 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 275958.33 | 259.39 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 275814.33 | 294.40 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 273483.33 | 265.50 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 265211.33 | 532.82 MB |
| java (8) | [act](http://actframework.org) (1.8) | 258750.33 | 504.96 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 258664.00 | 528.96 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.7) | 256198.00 | 148.01 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 190164.67 | 255.08 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 186267.33 | 303.43 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 184817.67 | 233.19 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 181013.33 | 242.10 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 175926.00 | 235.20 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 162066.33 | 215.61 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 161983.67 | 284.22 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 158423.67 | 277.82 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 153738.00 | 206.70 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 139936.00 | 359.45 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 131375.00 | 261.55 MB |
| node (11.9) | [polka](http://github.com/lukeed/polka) (0.5) | 120461.33 | 180.34 MB |
| node (11.9) | [restana](http://github.com/jkyberneees/ana) (2.7) | 116769.00 | 174.92 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 114628.33 | 282.40 MB |
| go (1.11) | [gf](http://gfer.me) (1.4) | 113494.33 | 172.38 MB |
| node (11.9) | [rayo](http://rayo.js.org) (1.2) | 100131.00 | 150.07 MB |
| node (11.9) | [foxify](http://foxify.js.org) (0.10) | 97914.33 | 205.70 MB |
| python (3.7) | [starlette](http://starlette.io) (0.10) | 96710.67 | 208.18 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 96380.67 | 90.52 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 95710.67 | 475.85 MB |
| node (11.9) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 93039.33 | 139.00 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 91490.00 | 160.11 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 86105.67 | 427.37 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 83218.00 | 413.49 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 82868.67 | 177.47 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 81157.67 | 136.54 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 81146.00 | 401.77 MB |
| node (11.9) | [fastify](http://fastify.io) (1.13) | 79290.67 | 193.94 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 75855.33 | 393.85 MB |
| node (11.9) | [koa](http://koajs.com) (2.7) | 70627.67 | 149.36 MB |
| c (99) | [kore](http://kore.io) (3.1) | 66136.67 | 179.29 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 65793.00 | 103.52 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 61695.33 | 139.98 MB |
| php (7.3) | [laravel](http://laravel.com) (5.7) | 61426.00 | 319.77 MB |
| node (11.9) | [restify](http://restify.com) (7.6) | 59467.33 | 104.05 MB |
| node (11.9) | [express](http://expressjs.com) (4.16) | 57639.67 | 140.79 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 45871.67 | 85.06 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 43821.33 | 41.76 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 43393.33 | 40.72 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 39236.67 | 36.76 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 37652.00 | 92.78 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 37415.67 | 60.97 MB |
| node (11.9) | [hapi](http://hapijs.com) (18.0) | 36989.33 | 95.51 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 35853.67 | 44.29 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 35283.33 | 20.35 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 32778.00 | 59.84 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 28060.67 | 49.98 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 26419.67 | 43.04 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 23550.33 | 13.61 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 22252.33 | 44.43 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 21521.33 | 62.45 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 21087.33 | 159.41 MB |
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.1) | 19206.33 | 41.83 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 17314.67 | 44.92 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 14215.00 | 41.94 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 4298.00 | 13.14 MB |
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

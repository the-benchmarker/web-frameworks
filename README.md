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
Last update: 2018-12-17
```
OS: Linux (version: 4.19.8-200.fc28.x86_64, arch: x86_64)
CPU Cores: 4
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: rack-routing (ruby)


:four: flame (ruby)


:five: roda (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 0.04 ms | 0.04 ms | 0.04 ms | 0.09 ms | 6.99 ms | 83.00 | 
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 0.31 ms | 0.25 ms | 0.49 ms | 1.43 ms | 51.80 ms | 605.67 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 2.11 ms | 0.49 ms | 4.69 ms | 31.16 ms | 100.21 ms | 5739.00 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 3.39 ms | 0.63 ms | 9.65 ms | 32.49 ms | 95.25 ms | 6429.33 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.15) | 1.78 ms | 0.63 ms | 3.46 ms | 26.21 ms | 125.62 ms | 5175.00 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 164.56 ms | 0.76 ms | 457.05 ms | 2940.95 ms | 7156.55 ms | 539606.67 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 3.61 ms | 0.86 ms | 9.13 ms | 47.71 ms | 124.26 ms | 8617.33 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 154.17 ms | 1.07 ms | 363.70 ms | 2843.97 ms | 6823.97 ms | 508126.67 | 
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 135.70 ms | 1.11 ms | 352.88 ms | 2470.87 ms | 6486.63 ms | 462180.00 | 
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 137.50 ms | 1.17 ms | 371.26 ms | 2279.50 ms | 6208.37 ms | 435332.33 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 159.92 ms | 1.28 ms | 410.54 ms | 2709.01 ms | 6958.82 ms | 511003.67 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 4.28 ms | 1.29 ms | 10.31 ms | 49.16 ms | 130.54 ms | 8932.33 | 
| php (7.2) | [symfony](http://symfony.com) (4.2) | 207.66 ms | 1.43 ms | 478.15 ms | 4050.57 ms | 7267.76 ms | 705967.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 166.15 ms | 2.70 ms | 92.06 ms | 4221.66 ms | 6595.56 ms | 728209.00 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 17.50 ms | 3.05 ms | 59.96 ms | 105.89 ms | 200.82 ms | 26412.33 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 4.53 ms | 4.46 ms | 6.15 ms | 8.44 ms | 136.24 ms | 2547.33 | 
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 5.38 ms | 4.46 ms | 10.35 ms | 18.98 ms | 56.12 ms | 4103.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 4.99 ms | 4.55 ms | 7.82 ms | 9.28 ms | 1980.96 ms | 25223.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 4.80 ms | 4.57 ms | 8.73 ms | 14.61 ms | 28.08 ms | 3125.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 6.12 ms | 5.43 ms | 9.07 ms | 15.44 ms | 168.55 ms | 6080.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 5.89 ms | 5.52 ms | 9.69 ms | 16.04 ms | 82.56 ms | 3430.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 7.06 ms | 5.77 ms | 11.49 ms | 26.55 ms | 288.41 ms | 8216.33 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 6.81 ms | 6.02 ms | 11.30 ms | 19.03 ms | 40.06 ms | 3587.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 7.03 ms | 6.17 ms | 12.79 ms | 21.03 ms | 51.67 ms | 4618.00 | 
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 7.20 ms | 6.51 ms | 11.60 ms | 18.43 ms | 46.00 ms | 3544.00 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 9.89 ms | 7.16 ms | 16.06 ms | 83.69 ms | 125.61 ms | 12489.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 7.77 ms | 7.24 ms | 11.17 ms | 17.21 ms | 30.81 ms | 2787.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 204.28 ms | 7.37 ms | 20.09 ms | 5160.18 ms | 7938.35 ms | 861024.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 10.49 ms | 8.77 ms | 12.43 ms | 34.50 ms | 643.78 ms | 20822.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 15.40 ms | 9.40 ms | 23.29 ms | 178.01 ms | 524.96 ms | 32621.67 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 15.21 ms | 10.12 ms | 25.26 ms | 145.84 ms | 552.20 ms | 27589.33 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 16.55 ms | 10.17 ms | 24.69 ms | 204.05 ms | 640.11 ms | 36383.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 15.75 ms | 10.71 ms | 25.81 ms | 147.01 ms | 495.57 ms | 26761.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 16.46 ms | 10.73 ms | 27.64 ms | 158.15 ms | 491.35 ms | 28657.33 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 11.84 ms | 11.06 ms | 15.47 ms | 28.31 ms | 192.84 ms | 4439.00 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 15.47 ms | 11.47 ms | 27.26 ms | 127.01 ms | 308.98 ms | 22014.33 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 19.73 ms | 11.71 ms | 31.17 ms | 227.95 ms | 661.62 ms | 40394.00 | 
| go (1.11) | [gf](http://gfer.me) (1.3) | 25.58 ms | 13.72 ms | 42.11 ms | 295.78 ms | 662.49 ms | 53246.67 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 30.78 ms | 15.21 ms | 34.71 ms | 536.61 ms | 1494.17 ms | 92536.67 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 20.99 ms | 15.43 ms | 35.00 ms | 85.64 ms | 368.49 ms | 17260.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 18.86 ms | 15.90 ms | 29.39 ms | 53.98 ms | 903.19 ms | 28451.67 | 
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 30.37 ms | 16.12 ms | 40.35 ms | 475.06 ms | 1394.64 ms | 82792.67 | 
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 33.67 ms | 16.56 ms | 39.78 ms | 547.42 ms | 1709.14 ms | 99508.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 16.43 ms | 16.64 ms | 17.44 ms | 19.12 ms | 181.32 ms | 6642.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 21.70 ms | 17.02 ms | 34.56 ms | 68.08 ms | 235.34 ms | 13652.00 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 18.75 ms | 17.79 ms | 21.03 ms | 29.79 ms | 164.01 ms | 4277.33 | 
| node (11.1) | [fastify](http://fastify.io) (1.13) | 37.24 ms | 17.94 ms | 41.70 ms | 648.32 ms | 1725.23 ms | 111201.67 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 41.56 ms | 18.26 ms | 44.56 ms | 803.98 ms | 1955.67 ms | 132947.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 67.95 ms | 19.09 ms | 47.18 ms | 1575.44 ms | 3613.33 ms | 271713.67 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 21.26 ms | 19.37 ms | 28.88 ms | 29.92 ms | 100.16 ms | 4437.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 20.58 ms | 20.02 ms | 25.85 ms | 27.17 ms | 73.75 ms | 3883.33 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 22.49 ms | 20.73 ms | 30.86 ms | 32.60 ms | 305.71 ms | 6797.67 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 59.96 ms | 22.07 ms | 51.28 ms | 1195.28 ms | 2419.19 ms | 194226.33 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 51.13 ms | 22.60 ms | 47.08 ms | 966.67 ms | 2208.50 ms | 159920.00 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 25.46 ms | 26.74 ms | 31.37 ms | 34.95 ms | 110.64 ms | 5606.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 27.98 ms | 27.72 ms | 33.72 ms | 45.18 ms | 313.19 ms | 10165.33 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 43.29 ms | 28.60 ms | 56.80 ms | 526.36 ms | 1488.78 ms | 91269.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 31.24 ms | 29.36 ms | 39.51 ms | 42.53 ms | 193.55 ms | 6779.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 36.84 ms | 33.86 ms | 53.22 ms | 66.18 ms | 385.56 ms | 17926.33 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 208.07 ms | 42.19 ms | 349.33 ms | 3431.91 ms | 5283.60 ms | 611157.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 57.47 ms | 47.41 ms | 87.91 ms | 133.35 ms | 454.14 ms | 25777.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 75.80 ms | 61.96 ms | 136.40 ms | 220.19 ms | 460.40 ms | 44697.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 91.48 ms | 87.80 ms | 114.99 ms | 263.34 ms | 1068.04 ms | 51513.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 104.17 ms | 95.57 ms | 151.05 ms | 207.05 ms | 526.23 ms | 46128.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 231.38 ms | 135.05 ms | 226.54 ms | 2976.08 ms | 4196.92 ms | 464629.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (evhtp) (cpp)


:four: (actix-web) (rust)


:five: (fasthttprouter) (go)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 210093.00 | 121.55 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 186931.00 | 223.80 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 155794.00 | 151.15 MB |
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 154551.00 | 175.70 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 152720.67 | 246.98 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 148083.00 | 168.20 MB |
| java (8) | [act](http://actframework.org) (1.8) | 144071.67 | 281.00 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 142305.33 | 151.31 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 136704.33 | 274.59 MB |
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 129087.33 | 264.03 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 122409.33 | 70.84 MB |
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 109841.67 | 218.36 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 105128.67 | 171.05 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 99402.00 | 132.83 MB |
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 99190.00 | 125.42 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 94128.33 | 125.90 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 94090.67 | 125.70 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 91055.00 | 159.89 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 90612.33 | 158.91 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 86047.33 | 115.62 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 85683.00 | 114.57 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 85016.00 | 149.09 MB |
| go (1.11) | [gf](http://gfer.me) (1.3) | 75139.00 | 114.29 MB |
| c (99) | [kore](http://kore.io) (3.1) | 69724.67 | 189.25 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 68547.33 | 147.20 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 60883.67 | 57.14 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 58237.33 | 101.90 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 53887.00 | 80.69 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 52791.33 | 49.52 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 52237.33 | 134.06 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 51149.33 | 76.60 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 50864.67 | 252.21 MB |
| php (7.2) | [symfony](http://symfony.com) (4.2) | 49809.67 | 247.31 MB |
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 49726.00 | 246.78 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 48829.00 | 120.12 MB |
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 48709.00 | 241.48 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 48606.00 | 45.58 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 47319.33 | 115.65 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 47212.33 | 98.98 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 46455.00 | 75.74 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 45460.00 | 68.02 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 43968.33 | 54.00 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 42944.67 | 71.70 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 42722.33 | 221.73 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 39057.00 | 71.36 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 38422.67 | 81.16 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 37799.33 | 92.31 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.15) | 37351.67 | 35.62 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 35671.33 | 185.78 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 35340.00 | 65.65 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 33042.00 | 53.90 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 31019.33 | 17.88 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 30229.00 | 52.90 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 27216.00 | 61.56 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 21491.67 | 32.80 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 19955.33 | 51.57 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 18914.00 | 10.90 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 17718.33 | 134.03 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 17302.00 | 42.52 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 14958.33 | 38.82 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 13572.33 | 24.20 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 10630.67 | 30.80 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 9536.33 | 19.01 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 6455.00 | 17.24 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 3652.67 | 11.18 MB |
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

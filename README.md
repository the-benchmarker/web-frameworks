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
Last update: 2019-02-07
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: zend-expressive (php)


:five: flame (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.07 ms | 0.08 ms | 0.11 ms | 0.14 ms | 3.76 ms | 39.67 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 3.46 ms | 0.19 ms | 12.61 ms | 29.66 ms | 79.07 ms | 6681.33 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.30 ms | 0.22 ms | 15.68 ms | 34.65 ms | 90.42 ms | 8002.33 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 115.92 ms | 0.31 ms | 205.81 ms | 2604.30 ms | 6760.52 ms | 456082.33 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.89 ms | 0.32 ms | 20.67 ms | 44.49 ms | 99.66 ms | 10375.33 | 
| php (7.3) | [laravel](http://laravel.com) (5.7) | 116.64 ms | 0.33 ms | 285.38 ms | 2184.78 ms | 6866.20 ms | 415601.00 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 171.48 ms | 0.33 ms | 259.73 ms | 3940.82 ms | 6793.44 ms | 654341.00 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 157.39 ms | 0.34 ms | 260.77 ms | 3694.24 ms | 6792.09 ms | 602520.33 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 6.82 ms | 0.36 ms | 24.03 ms | 49.16 ms | 119.24 ms | 11771.67 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.35 ms | 0.36 ms | 0.58 ms | 0.80 ms | 9.16 ms | 199.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 8.64 ms | 0.53 ms | 27.59 ms | 54.95 ms | 124.32 ms | 13289.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 92.36 ms | 1.49 ms | 17.85 ms | 2665.91 ms | 6571.78 ms | 485475.33 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 123.15 ms | 2.16 ms | 231.00 ms | 2578.64 ms | 5600.74 ms | 447312.33 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 3.21 ms | 2.33 ms | 6.84 ms | 14.86 ms | 36.86 ms | 3082.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.43 ms | 2.36 ms | 7.55 ms | 16.10 ms | 41.62 ms | 3503.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 2.62 ms | 2.37 ms | 4.81 ms | 9.37 ms | 59.60 ms | 1963.67 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 32.03 ms | 2.63 ms | 108.39 ms | 288.27 ms | 757.59 ms | 61639.00 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 100.17 ms | 2.96 ms | 225.39 ms | 2232.20 ms | 4081.24 ms | 364319.00 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.60 ms | 3.02 ms | 5.79 ms | 11.26 ms | 147.49 ms | 2791.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.96 ms | 3.13 ms | 8.26 ms | 15.49 ms | 36.86 ms | 3383.00 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 3.66 ms | 3.35 ms | 6.37 ms | 12.44 ms | 27.43 ms | 2478.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.37 ms | 3.50 ms | 5.40 ms | 7.53 ms | 21.99 ms | 1766.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.25 ms | 3.55 ms | 6.98 ms | 12.29 ms | 48.49 ms | 2444.33 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 4.17 ms | 3.66 ms | 7.82 ms | 14.48 ms | 29.82 ms | 2939.00 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 4.58 ms | 3.90 ms | 8.34 ms | 18.97 ms | 226.95 ms | 6015.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.7) | 6.47 ms | 4.01 ms | 11.65 ms | 71.18 ms | 124.37 ms | 11016.67 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 5.92 ms | 4.95 ms | 9.88 ms | 19.79 ms | 159.69 ms | 4064.33 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 6.02 ms | 5.08 ms | 10.11 ms | 20.12 ms | 136.90 ms | 3905.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 5.15 ms | 5.08 ms | 8.59 ms | 10.91 ms | 30.58 ms | 2639.00 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 10.00 ms | 5.34 ms | 13.17 ms | 123.35 ms | 457.55 ms | 22889.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 6.40 ms | 5.49 ms | 9.12 ms | 16.74 ms | 277.99 ms | 7890.33 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 6.83 ms | 5.53 ms | 11.38 ms | 22.99 ms | 213.96 ms | 5340.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.40 ms | 5.54 ms | 10.53 ms | 22.79 ms | 113.59 ms | 5000.00 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 6.89 ms | 5.60 ms | 11.65 ms | 23.76 ms | 144.87 ms | 4823.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 6.97 ms | 5.70 ms | 11.65 ms | 23.47 ms | 170.26 ms | 5420.33 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 7.13 ms | 5.80 ms | 11.82 ms | 24.36 ms | 170.87 ms | 5792.67 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 8.15 ms | 6.14 ms | 14.23 ms | 25.78 ms | 212.35 ms | 6253.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 147.26 ms | 6.98 ms | 24.22 ms | 3847.82 ms | 6838.63 ms | 656811.00 | 
| node (11.9) | [restana](http://github.com/jkyberneees/ana) (2.8) | 10.39 ms | 7.51 ms | 18.81 ms | 43.95 ms | 408.07 ms | 14421.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 9.76 ms | 7.75 ms | 17.75 ms | 34.90 ms | 202.60 ms | 7957.00 | 
| go (1.11) | [gf](http://gfer.me) (1.4) | 9.56 ms | 8.40 ms | 13.87 ms | 32.29 ms | 314.46 ms | 10167.67 | 
| node (11.9) | [rayo](http://rayo.js.org) (1.2) | 12.64 ms | 9.14 ms | 22.58 ms | 50.04 ms | 476.48 ms | 17179.67 | 
| node (11.9) | [polka](http://github.com/lukeed/polka) (0.5) | 14.02 ms | 9.56 ms | 24.24 ms | 58.83 ms | 612.28 ms | 24183.00 | 
| node (11.9) | [foxify](http://foxify.js.org) (0.10) | 13.35 ms | 9.60 ms | 21.31 ms | 64.82 ms | 538.49 ms | 21856.00 | 
| node (11.9) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 13.82 ms | 9.86 ms | 22.32 ms | 65.25 ms | 589.30 ms | 23514.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 10.76 ms | 10.79 ms | 12.66 ms | 14.81 ms | 132.37 ms | 2548.67 | 
| python (3.7) | [starlette](http://starlette.io) (0.10) | 12.30 ms | 10.86 ms | 21.30 ms | 32.35 ms | 84.44 ms | 6539.33 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 15.99 ms | 11.02 ms | 19.73 ms | 138.75 ms | 945.01 ms | 42248.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 20.05 ms | 11.49 ms | 24.13 ms | 300.63 ms | 1434.25 ms | 73915.67 | 
| node (11.9) | [fastify](http://fastify.io) (1.14) | 17.07 ms | 12.61 ms | 25.77 ms | 64.92 ms | 674.14 ms | 28535.33 | 
| node (11.9) | [koa](http://koajs.com) (2.7) | 20.60 ms | 14.44 ms | 29.69 ms | 141.01 ms | 867.42 ms | 40952.67 | 
| node (11.9) | [express](http://expressjs.com) (4.16) | 21.79 ms | 14.78 ms | 29.68 ms | 195.50 ms | 919.36 ms | 45670.33 | 
| node (11.9) | [restify](http://restify.com) (7.7) | 19.10 ms | 15.73 ms | 32.09 ms | 59.82 ms | 397.77 ms | 15957.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 20.22 ms | 18.72 ms | 36.32 ms | 50.46 ms | 77.22 ms | 11160.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 24.30 ms | 20.65 ms | 36.11 ms | 52.81 ms | 764.53 ms | 22070.00 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 23.63 ms | 21.61 ms | 33.15 ms | 42.13 ms | 304.75 ms | 10020.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 24.64 ms | 22.04 ms | 32.88 ms | 44.91 ms | 335.05 ms | 13603.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 28.03 ms | 23.38 ms | 44.95 ms | 71.34 ms | 546.28 ms | 23167.33 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 25.08 ms | 24.60 ms | 33.98 ms | 39.17 ms | 221.94 ms | 6936.00 | 
| node (11.9) | [hapi](http://hapijs.com) (18.1) | 34.00 ms | 25.15 ms | 42.43 ms | 325.14 ms | 1095.02 ms | 60400.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 40.46 ms | 32.58 ms | 76.10 ms | 128.74 ms | 279.63 ms | 27289.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 33.04 ms | 32.84 ms | 40.57 ms | 46.33 ms | 317.10 ms | 8562.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 35.11 ms | 36.79 ms | 45.94 ms | 53.16 ms | 326.61 ms | 12306.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 34.31 ms | 38.28 ms | 41.65 ms | 47.33 ms | 335.45 ms | 10972.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 52.97 ms | 41.83 ms | 85.28 ms | 268.01 ms | 949.25 ms | 56893.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 50.67 ms | 47.38 ms | 89.84 ms | 130.96 ms | 213.68 ms | 26793.67 | 
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.1) | 69.99 ms | 64.25 ms | 119.07 ms | 165.79 ms | 215.21 ms | 33713.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 71.07 ms | 71.29 ms | 88.08 ms | 104.29 ms | 470.51 ms | 21912.00 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 365180.67 | 211.40 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 318861.00 | 381.77 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 318032.67 | 361.65 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 270426.33 | 307.17 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 261293.33 | 253.44 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 258429.67 | 243.14 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 257758.00 | 415.78 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 245326.00 | 493.07 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 238159.00 | 254.14 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 226020.00 | 462.97 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.7) | 216321.33 | 125.19 MB |
| java (8) | [act](http://actframework.org) (1.8) | 208956.33 | 408.18 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 169699.33 | 214.19 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 162901.00 | 218.04 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 161429.00 | 216.69 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 153450.33 | 250.17 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 150805.00 | 201.96 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 145501.67 | 255.41 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 144334.00 | 253.48 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 141716.33 | 189.02 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 139838.33 | 188.48 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 123247.33 | 316.78 MB |
| node (11.9) | [restana](http://github.com/jkyberneees/ana) (2.8) | 110815.00 | 165.96 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 109611.00 | 217.88 MB |
| go (1.11) | [gf](http://gfer.me) (1.4) | 108212.33 | 164.40 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 106628.67 | 262.69 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 92152.33 | 86.71 MB |
| node (11.9) | [rayo](http://rayo.js.org) (1.2) | 91062.00 | 136.15 MB |
| node (11.9) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 89869.33 | 134.55 MB |
| node (11.9) | [foxify](http://foxify.js.org) (0.10) | 88598.33 | 186.07 MB |
| node (11.9) | [polka](http://github.com/lukeed/polka) (0.5) | 86507.33 | 129.66 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 82746.00 | 411.42 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 82061.67 | 143.45 MB |
| python (3.7) | [starlette](http://starlette.io) (0.10) | 81465.33 | 175.69 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 79759.33 | 396.57 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 78551.00 | 168.28 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 77142.33 | 129.86 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 72455.33 | 359.21 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 70215.67 | 348.54 MB |
| node (11.9) | [fastify](http://fastify.io) (1.14) | 70142.67 | 173.50 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 63786.67 | 330.99 MB |
| c (99) | [kore](http://kore.io) (3.1) | 60937.67 | 165.16 MB |
| node (11.9) | [koa](http://koajs.com) (2.7) | 60650.33 | 128.06 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 59195.00 | 94.09 MB |
| node (11.9) | [express](http://expressjs.com) (4.16) | 58996.00 | 144.10 MB |
| node (11.9) | [restify](http://restify.com) (7.7) | 55209.00 | 96.66 MB |
| php (7.3) | [laravel](http://laravel.com) (5.7) | 52956.33 | 275.86 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 50493.67 | 114.59 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 42248.33 | 78.35 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 42145.00 | 39.51 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 40529.00 | 66.13 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 40005.00 | 37.51 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 37097.67 | 35.33 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 36848.00 | 90.75 MB |
| node (11.9) | [hapi](http://hapijs.com) (18.1) | 36296.33 | 93.78 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 30598.33 | 37.46 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 29881.33 | 17.24 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 28758.33 | 52.49 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 28737.33 | 46.83 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 25968.67 | 46.27 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 21721.00 | 12.54 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 20433.33 | 59.24 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 19958.67 | 39.82 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 18709.33 | 141.43 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 14834.33 | 38.50 MB |
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.1) | 14264.00 | 31.06 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 13696.00 | 40.47 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3998.00 | 12.23 MB |
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

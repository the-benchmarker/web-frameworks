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
Last update: 2019-01-30
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


:five: laravel (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.07 ms | 0.08 ms | 0.11 ms | 0.14 ms | 4.27 ms | 34.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 3.21 ms | 0.17 ms | 11.88 ms | 29.13 ms | 76.85 ms | 6465.33 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.21 ms | 0.21 ms | 15.75 ms | 35.22 ms | 97.29 ms | 8106.33 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.35 ms | 0.32 ms | 0.59 ms | 1.12 ms | 27.47 ms | 309.33 | 
| php (7.3) | [laravel](http://laravel.com) (5.7) | 100.48 ms | 0.32 ms | 260.44 ms | 1803.20 ms | 6821.03 ms | 350727.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 135.03 ms | 0.33 ms | 201.42 ms | 3255.61 ms | 6766.46 ms | 554240.33 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 6.34 ms | 0.34 ms | 21.98 ms | 46.71 ms | 111.15 ms | 10943.67 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 7.43 ms | 0.35 ms | 26.60 ms | 55.92 ms | 159.81 ms | 13258.33 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 8.41 ms | 0.48 ms | 27.60 ms | 55.96 ms | 132.29 ms | 13427.67 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 140.78 ms | 1.54 ms | 253.01 ms | 2949.32 ms | 5611.06 ms | 510342.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 71.38 ms | 1.59 ms | 3.96 ms | 2264.60 ms | 5504.94 ms | 407553.33 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 113.99 ms | 1.59 ms | 226.55 ms | 2541.75 ms | 5771.75 ms | 430571.33 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 3.11 ms | 2.27 ms | 5.64 ms | 16.74 ms | 92.48 ms | 3214.33 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 3.24 ms | 2.27 ms | 6.78 ms | 15.26 ms | 94.01 ms | 3159.33 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 96.60 ms | 2.35 ms | 196.64 ms | 2074.81 ms | 4411.85 ms | 359637.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.43 ms | 2.52 ms | 7.10 ms | 16.08 ms | 35.37 ms | 3358.67 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 97.07 ms | 2.62 ms | 197.02 ms | 2135.00 ms | 4516.99 ms | 361389.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.20 ms | 3.29 ms | 8.73 ms | 16.84 ms | 48.03 ms | 3688.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.85 ms | 3.32 ms | 5.99 ms | 12.36 ms | 107.43 ms | 3063.67 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 35.10 ms | 3.39 ms | 116.95 ms | 315.83 ms | 845.04 ms | 67144.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.40 ms | 3.41 ms | 5.46 ms | 7.90 ms | 96.18 ms | 2183.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.23 ms | 3.50 ms | 7.23 ms | 13.35 ms | 43.47 ms | 2649.33 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 3.87 ms | 3.62 ms | 6.64 ms | 12.42 ms | 26.79 ms | 2516.33 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 4.46 ms | 3.92 ms | 8.41 ms | 15.22 ms | 30.61 ms | 3113.33 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 5.02 ms | 4.44 ms | 9.60 ms | 20.57 ms | 157.00 ms | 4943.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 5.30 ms | 5.24 ms | 8.87 ms | 10.82 ms | 34.91 ms | 2692.67 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 6.30 ms | 5.26 ms | 10.43 ms | 21.14 ms | 212.83 ms | 5134.33 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 6.44 ms | 5.40 ms | 10.59 ms | 21.08 ms | 106.70 ms | 3825.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.6) | 7.14 ms | 5.57 ms | 14.44 ms | 28.07 ms | 68.43 ms | 5930.00 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 7.18 ms | 5.64 ms | 11.45 ms | 24.45 ms | 244.39 ms | 8941.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.48 ms | 5.68 ms | 10.76 ms | 21.60 ms | 145.51 ms | 4657.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 7.29 ms | 5.81 ms | 12.01 ms | 24.59 ms | 287.37 ms | 7057.67 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 8.09 ms | 5.94 ms | 12.24 ms | 27.75 ms | 373.81 ms | 14580.00 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 7.27 ms | 5.95 ms | 12.06 ms | 24.47 ms | 227.02 ms | 5897.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 6.97 ms | 6.10 ms | 9.71 ms | 17.66 ms | 237.18 ms | 7282.67 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 7.46 ms | 6.17 ms | 12.37 ms | 24.55 ms | 91.54 ms | 4397.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 8.33 ms | 6.46 ms | 14.41 ms | 27.80 ms | 213.86 ms | 7160.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 210.30 ms | 7.67 ms | 68.00 ms | 4874.21 ms | 7941.62 ms | 846931.67 | 
| node (11.8) | [restana](http://github.com/jkyberneees/ana) (2.7) | 12.83 ms | 8.60 ms | 24.35 ms | 59.00 ms | 463.81 ms | 18294.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 10.01 ms | 8.61 ms | 18.79 ms | 35.87 ms | 165.45 ms | 8322.67 | 
| go (1.11) | [gf](http://gfer.me) (1.4) | 9.50 ms | 8.61 ms | 14.16 ms | 30.48 ms | 171.06 ms | 6157.00 | 
| node (11.8) | [rayo](http://rayo.js.org) (1.2) | 13.20 ms | 9.64 ms | 23.67 ms | 51.03 ms | 498.92 ms | 17848.67 | 
| node (11.8) | [foxify](http://foxify.js.org) (0.10) | 14.62 ms | 10.63 ms | 25.41 ms | 59.39 ms | 489.75 ms | 19669.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 18.71 ms | 10.92 ms | 18.98 ms | 250.60 ms | 1175.18 ms | 60533.67 | 
| python (3.7) | [starlette](http://starlette.io) (0.10) | 12.88 ms | 11.03 ms | 22.33 ms | 35.31 ms | 73.70 ms | 6751.33 | 
| node (11.8) | [polka](http://github.com/lukeed/polka) (0.5) | 15.30 ms | 11.28 ms | 26.27 ms | 56.83 ms | 569.48 ms | 22338.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 11.31 ms | 11.35 ms | 13.27 ms | 15.35 ms | 94.60 ms | 2345.67 | 
| node (11.6) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 17.36 ms | 11.72 ms | 26.21 ms | 138.25 ms | 735.98 ms | 34521.00 | 
| node (11.8) | [fastify](http://fastify.io) (1.13) | 16.36 ms | 11.89 ms | 27.77 ms | 65.88 ms | 579.38 ms | 23670.33 | 
| node (11.6) | [koa](http://koajs.com) (2.6) | 16.64 ms | 12.29 ms | 28.06 ms | 70.22 ms | 542.21 ms | 22457.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 15.74 ms | 12.67 ms | 25.76 ms | 49.36 ms | 736.40 ms | 29963.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 20.99 ms | 16.97 ms | 41.62 ms | 63.83 ms | 108.79 ms | 13473.67 | 
| node (11.8) | [express](http://expressjs.com) (4.16) | 22.85 ms | 17.30 ms | 35.89 ms | 103.71 ms | 743.03 ms | 33223.33 | 
| node (11.8) | [restify](http://restify.com) (7.6) | 23.76 ms | 18.91 ms | 39.32 ms | 102.21 ms | 585.06 ms | 26871.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 28.75 ms | 19.58 ms | 59.34 ms | 88.15 ms | 282.91 ms | 20890.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 24.68 ms | 21.49 ms | 37.60 ms | 54.28 ms | 400.84 ms | 16149.00 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 25.80 ms | 22.56 ms | 35.08 ms | 44.04 ms | 381.99 ms | 11831.67 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 27.38 ms | 23.97 ms | 35.36 ms | 47.60 ms | 392.45 ms | 14152.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 27.77 ms | 24.55 ms | 41.45 ms | 55.26 ms | 307.35 ms | 13313.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 25.67 ms | 25.43 ms | 34.24 ms | 40.40 ms | 251.52 ms | 8667.00 | 
| node (11.8) | [hapi](http://hapijs.com) (18.0) | 34.38 ms | 26.10 ms | 49.05 ms | 232.27 ms | 1034.14 ms | 52026.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 33.72 ms | 28.85 ms | 44.95 ms | 74.62 ms | 199.68 ms | 13280.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 42.24 ms | 32.94 ms | 86.03 ms | 150.69 ms | 340.00 ms | 31926.67 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 34.72 ms | 35.19 ms | 42.92 ms | 50.53 ms | 469.44 ms | 11961.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 47.66 ms | 40.00 ms | 90.04 ms | 132.10 ms | 205.15 ms | 28540.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 48.60 ms | 40.06 ms | 81.34 ms | 124.57 ms | 502.95 ms | 25326.00 | 
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.1) | 71.19 ms | 67.45 ms | 113.53 ms | 148.09 ms | 228.05 ms | 29437.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 72.86 ms | 71.21 ms | 92.93 ms | 111.04 ms | 464.84 ms | 19588.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (actix-web) (rust)


:three: (japronto) (python)


:four: (vibora) (python)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 343609.00 | 198.66 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 309752.00 | 352.11 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 306314.33 | 366.43 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 259961.33 | 295.26 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 257803.00 | 250.25 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 250682.33 | 503.50 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 242299.33 | 390.31 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 242036.67 | 227.75 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 222361.00 | 237.12 MB |
| java (8) | [act](http://actframework.org) (1.8) | 206468.33 | 403.08 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 204323.00 | 417.80 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 159117.67 | 199.69 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 154543.00 | 207.14 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.6) | 151682.00 | 87.71 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 151501.00 | 202.15 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 141503.00 | 189.00 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 139422.67 | 227.23 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 137040.00 | 240.61 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 135462.33 | 237.78 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 133510.00 | 178.66 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 133239.00 | 179.62 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 121821.33 | 312.50 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 112579.00 | 222.65 MB |
| go (1.11) | [gf](http://gfer.me) (1.4) | 104818.33 | 159.24 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 104587.67 | 257.15 MB |
| node (11.8) | [restana](http://github.com/jkyberneees/ana) (2.7) | 92213.00 | 138.27 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 87374.67 | 82.05 MB |
| node (11.8) | [rayo](http://rayo.js.org) (1.2) | 85849.67 | 128.63 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 83019.00 | 412.64 MB |
| python (3.7) | [starlette](http://starlette.io) (0.10) | 78619.00 | 169.30 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 78364.00 | 388.97 MB |
| node (11.8) | [foxify](http://foxify.js.org) (0.10) | 78308.67 | 164.65 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 77822.67 | 130.88 MB |
| node (11.6) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 76397.67 | 114.16 MB |
| node (11.8) | [polka](http://github.com/lukeed/polka) (0.5) | 75686.33 | 113.27 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 75240.00 | 131.71 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 74007.33 | 384.89 MB |
| node (11.8) | [fastify](http://fastify.io) (1.13) | 73746.00 | 176.38 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 72159.00 | 357.48 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 71277.67 | 354.01 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 70841.00 | 152.11 MB |
| node (11.6) | [koa](http://koajs.com) (2.6) | 69112.00 | 146.21 MB |
| c (99) | [kore](http://kore.io) (3.1) | 55893.67 | 151.53 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 54440.00 | 86.51 MB |
| php (7.3) | [laravel](http://laravel.com) (5.7) | 54017.67 | 281.36 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 50802.33 | 115.14 MB |
| node (11.8) | [express](http://expressjs.com) (4.16) | 50189.67 | 122.70 MB |
| node (11.8) | [restify](http://restify.com) (7.6) | 46153.33 | 80.74 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 40030.33 | 38.13 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 40007.00 | 74.19 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 38930.33 | 95.96 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 38718.00 | 36.31 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 38701.33 | 36.28 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 36676.00 | 59.76 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 36002.67 | 44.26 MB |
| node (11.8) | [hapi](http://hapijs.com) (18.0) | 33975.33 | 87.85 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 30474.67 | 17.60 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 29567.00 | 48.19 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 28795.00 | 52.61 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 26212.00 | 46.68 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 21542.67 | 42.94 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 21036.67 | 60.97 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 20191.67 | 11.65 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 17176.00 | 129.94 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 15188.67 | 39.38 MB |
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.1) | 13923.00 | 30.34 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 13464.67 | 39.59 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3658.00 | 11.16 MB |
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

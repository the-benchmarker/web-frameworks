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

+ Helping decide between languages, depending on use case
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
Last update: 2019-05-29
```
OS: Linux (version: 5.0.16-300.fc30.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: iron (rust)


:five: lumen (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.35) | [nickel](http://nickel-org.github.io) (0.11) | 0.07 ms | 0.07 ms | 0.10 ms | 0.13 ms | 2.26 ms | 29.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 3.31 ms | 0.16 ms | 12.91 ms | 32.38 ms | 82.79 ms | 7150.67 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.38 ms | 0.20 ms | 17.48 ms | 39.96 ms | 100.00 ms | 9139.67 | 
| rust (1.35) | [iron](http://ironframework.io) (0.6) | 0.31 ms | 0.29 ms | 0.53 ms | 0.88 ms | 12.25 ms | 208.33 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 115.41 ms | 0.35 ms | 227.92 ms | 2695.25 ms | 6805.43 ms | 460521.67 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 143.70 ms | 0.36 ms | 266.39 ms | 3092.10 ms | 6905.31 ms | 538889.00 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 116.28 ms | 0.36 ms | 312.44 ms | 1849.47 ms | 7096.77 ms | 417505.67 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 157.91 ms | 0.36 ms | 279.88 ms | 3524.76 ms | 6928.27 ms | 590573.67 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 113.49 ms | 0.36 ms | 235.26 ms | 2206.71 ms | 6815.35 ms | 431084.67 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 155.29 ms | 0.37 ms | 256.57 ms | 3373.97 ms | 6909.62 ms | 580389.67 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 7.49 ms | 0.40 ms | 27.53 ms | 55.79 ms | 158.45 ms | 13470.33 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 7.11 ms | 0.40 ms | 24.15 ms | 49.91 ms | 112.77 ms | 11855.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 9.03 ms | 0.53 ms | 29.84 ms | 60.27 ms | 159.86 ms | 14509.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 107.51 ms | 1.51 ms | 58.37 ms | 3171.46 ms | 6594.13 ms | 549552.33 | 
| rust (1.35) | [actix-web](http://actix.rs) (0.7) | 2.96 ms | 2.12 ms | 6.24 ms | 14.62 ms | 70.52 ms | 2954.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.42 ms | 2.16 ms | 7.99 ms | 17.45 ms | 35.12 ms | 3743.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 2.89 ms | 2.16 ms | 5.57 ms | 11.61 ms | 27.70 ms | 2343.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 9.20 ms | 2.74 ms | 6.35 ms | 202.42 ms | 1131.36 ms | 59463.67 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.48 ms | 2.86 ms | 5.69 ms | 11.16 ms | 100.00 ms | 3082.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.99 ms | 2.91 ms | 5.08 ms | 7.76 ms | 21.26 ms | 1730.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.00 ms | 3.21 ms | 8.21 ms | 15.28 ms | 34.86 ms | 3348.00 | 
| rust (1.35) | [gotham](http://gotham.rs) (0.3) | 4.29 ms | 3.38 ms | 8.18 ms | 18.98 ms | 216.98 ms | 5897.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.16 ms | 3.47 ms | 6.86 ms | 12.36 ms | 48.93 ms | 2422.33 | 
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 3.85 ms | 3.68 ms | 6.43 ms | 12.54 ms | 28.25 ms | 2416.67 | 
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 3.92 ms | 3.70 ms | 6.57 ms | 13.07 ms | 28.27 ms | 2496.33 | 
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 4.38 ms | 3.94 ms | 8.02 ms | 14.65 ms | 32.85 ms | 2914.33 | 
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 4.20 ms | 4.10 ms | 6.67 ms | 13.05 ms | 63.23 ms | 2955.00 | 
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 4.19 ms | 4.11 ms | 6.82 ms | 13.31 ms | 29.64 ms | 2492.00 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 6.44 ms | 4.21 ms | 11.41 ms | 68.21 ms | 129.06 ms | 10615.00 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 38.85 ms | 4.24 ms | 128.19 ms | 337.96 ms | 969.32 ms | 72346.00 | 
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 4.81 ms | 4.56 ms | 8.04 ms | 13.49 ms | 33.56 ms | 2564.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 5.89 ms | 4.62 ms | 7.88 ms | 18.16 ms | 420.51 ms | 13615.67 | 
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 6.82 ms | 4.73 ms | 14.65 ms | 32.53 ms | 76.58 ms | 6486.67 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 6.72 ms | 4.77 ms | 14.08 ms | 29.09 ms | 219.49 ms | 6776.33 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 6.54 ms | 4.79 ms | 13.41 ms | 27.39 ms | 138.80 ms | 5746.00 | 
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 6.54 ms | 4.88 ms | 13.28 ms | 28.98 ms | 181.67 ms | 6305.33 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 6.88 ms | 4.88 ms | 13.90 ms | 29.60 ms | 255.64 ms | 7557.33 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 7.36 ms | 4.91 ms | 16.01 ms | 33.64 ms | 206.48 ms | 7336.67 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 7.10 ms | 4.94 ms | 15.07 ms | 30.93 ms | 170.79 ms | 6448.33 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 6.54 ms | 4.95 ms | 12.37 ms | 26.59 ms | 123.74 ms | 5281.67 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 6.75 ms | 5.11 ms | 12.95 ms | 27.34 ms | 112.12 ms | 5381.67 | 
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 7.38 ms | 5.35 ms | 12.43 ms | 24.74 ms | 169.81 ms | 6381.00 | 
| node (12.3) | [0http](http://github.com/jkyberneees/0http) (1.0) | 16.69 ms | 5.49 ms | 15.22 ms | 312.47 ms | 747.95 ms | 54882.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.56 ms | 5.63 ms | 10.59 ms | 20.16 ms | 266.14 ms | 5313.33 | 
| node (12.3) | [restana](http://github.com/jkyberneees/ana) (3.0) | 19.92 ms | 5.89 ms | 15.12 ms | 379.36 ms | 997.41 ms | 70937.00 | 
| go (1.12) | [gf](http://goframe.org) (1.6) | 8.93 ms | 6.17 ms | 18.57 ms | 40.62 ms | 192.13 ms | 8617.33 | 
| node (12.3) | [polka](http://github.com/lukeed/polka) (0.5) | 8.73 ms | 6.58 ms | 13.58 ms | 31.78 ms | 416.24 ms | 15420.00 | 
| node (12.3) | [rayo](http://rayo.js.org) (1.3) | 11.16 ms | 7.38 ms | 15.12 ms | 86.76 ms | 686.77 ms | 30204.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 180.84 ms | 7.45 ms | 49.81 ms | 4009.91 ms | 7523.32 ms | 711098.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 9.91 ms | 7.86 ms | 19.86 ms | 37.93 ms | 213.79 ms | 8534.67 | 
| node (12.3) | [foxify](http://foxify.js.org) (0.10) | 11.64 ms | 8.61 ms | 16.60 ms | 89.63 ms | 443.46 ms | 20456.67 | 
| node (12.3) | [fastify](http://fastify.io) (2.4) | 11.95 ms | 8.71 ms | 16.35 ms | 87.40 ms | 508.24 ms | 22337.67 | 
| node (12.3) | [koa](http://koajs.com) (2.7) | 12.84 ms | 8.81 ms | 16.93 ms | 122.33 ms | 547.53 ms | 26835.67 | 
| node (12.3) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 18.70 ms | 9.01 ms | 22.42 ms | 257.08 ms | 789.50 ms | 47468.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 9.06 ms | 9.13 ms | 11.33 ms | 13.43 ms | 25.74 ms | 1850.00 | 
| python (3.7) | [hug](http://hug.rest) (2.5) | 13.21 ms | 9.77 ms | 26.31 ms | 45.46 ms | 219.47 ms | 10749.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 31.99 ms | 10.32 ms | 22.05 ms | 807.26 ms | 2852.69 ms | 161426.67 | 
| node (12.3) | [restify](http://restify.com) (8.2) | 13.62 ms | 10.48 ms | 19.85 ms | 69.60 ms | 431.29 ms | 18499.00 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 13.54 ms | 10.76 ms | 19.03 ms | 43.10 ms | 766.08 ms | 24965.33 | 
| python (3.7) | [starlette](http://starlette.io) (0.12) | 11.85 ms | 11.02 ms | 19.36 ms | 30.05 ms | 98.01 ms | 6029.67 | 
| node (12.3) | [express](http://expressjs.com) (4.16) | 17.63 ms | 11.55 ms | 21.07 ms | 222.16 ms | 773.64 ms | 38717.33 | 
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 20.78 ms | 17.01 ms | 28.55 ms | 52.21 ms | 1036.60 ms | 33201.67 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.24) | 17.92 ms | 17.38 ms | 28.44 ms | 40.83 ms | 71.36 ms | 8202.00 | 
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 22.62 ms | 17.70 ms | 41.89 ms | 138.17 ms | 419.53 ms | 25795.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 20.50 ms | 19.33 ms | 32.68 ms | 47.26 ms | 82.73 ms | 9430.00 | 
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 36.73 ms | 19.58 ms | 37.37 ms | 659.45 ms | 2048.97 ms | 123630.33 | 
| node (12.3) | [hapi](http://hapijs.com) (18.1) | 29.23 ms | 21.90 ms | 36.59 ms | 246.49 ms | 797.62 ms | 45071.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 33.07 ms | 22.64 ms | 49.95 ms | 233.40 ms | 1123.31 ms | 53636.67 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 28.44 ms | 25.25 ms | 39.95 ms | 64.35 ms | 555.31 ms | 21407.33 | 
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 29.62 ms | 27.03 ms | 39.00 ms | 50.07 ms | 238.18 ms | 8863.67 | 
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 31.64 ms | 31.09 ms | 39.20 ms | 50.33 ms | 388.76 ms | 14347.33 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 31.03 ms | 31.32 ms | 48.57 ms | 69.53 ms | 175.98 ms | 14687.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 42.65 ms | 34.91 ms | 79.84 ms | 150.06 ms | 288.35 ms | 29816.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 49.00 ms | 42.55 ms | 98.46 ms | 146.79 ms | 208.79 ms | 30028.67 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 71.12 ms | 60.63 ms | 122.87 ms | 165.97 ms | 343.67 ms | 35377.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 80.93 ms | 76.30 ms | 100.91 ms | 217.81 ms | 930.59 ms | 45231.33 | 
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 86.17 ms | 78.51 ms | 120.94 ms | 195.51 ms | 690.56 ms | 36446.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 84.15 ms | 82.89 ms | 109.14 ms | 134.76 ms | 716.29 ms | 28810.33 | 
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 100.23 ms | 98.12 ms | 137.27 ms | 179.97 ms | 277.19 ms | 29289.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (actix-web) (rust)


:three: (japronto) (python)


:four: (kore) (c)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 370257.00 | 214.21 MB |
| rust (1.35) | [actix-web](http://actix.rs) (0.7) | 349871.33 | 397.48 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 338855.67 | 405.70 MB |
| c (99) | [kore](http://kore.io) (3.1) | 306706.33 | 797.42 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 298486.00 | 289.65 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 269201.67 | 432.44 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 266235.67 | 302.39 MB |
| rust (1.35) | [gotham](http://gotham.rs) (0.3) | 248885.00 | 509.14 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 248065.33 | 498.09 MB |
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 245361.33 | 230.79 MB |
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 241935.00 | 227.60 MB |
| java (8) | [act](http://actframework.org) (1.8) | 240455.00 | 415.33 MB |
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 226350.67 | 282.86 MB |
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 226055.00 | 369.33 MB |
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 225028.00 | 240.87 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 206398.33 | 119.42 MB |
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 196590.67 | 321.34 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 181836.00 | 296.08 MB |
| rust (1.35) | [iron](http://ironframework.io) (0.6) | 173184.00 | 217.95 MB |
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 166684.33 | 259.90 MB |
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 165565.67 | 220.46 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 163414.67 | 218.57 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 160769.00 | 282.13 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 160043.33 | 211.91 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 157359.67 | 212.02 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 156488.67 | 206.73 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 154332.33 | 270.62 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 152717.00 | 204.20 MB |
| node (12.3) | [0http](http://github.com/jkyberneees/0http) (1.0) | 138945.33 | 208.32 MB |
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 136801.33 | 320.43 MB |
| node (12.3) | [restana](http://github.com/jkyberneees/ana) (3.0) | 133095.00 | 199.54 MB |
| node (12.3) | [polka](http://github.com/lukeed/polka) (0.5) | 129895.33 | 194.65 MB |
| go (1.12) | [gf](http://goframe.org) (1.6) | 125151.67 | 189.11 MB |
| node (12.3) | [rayo](http://rayo.js.org) (1.3) | 116957.67 | 175.25 MB |
| rust (1.35) | [nickel](http://nickel-org.github.io) (0.11) | 114239.00 | 226.59 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 107870.33 | 101.47 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 107654.67 | 265.19 MB |
| node (12.3) | [fastify](http://fastify.io) (2.4) | 106044.67 | 274.38 MB |
| node (12.3) | [foxify](http://foxify.js.org) (0.10) | 101640.33 | 213.72 MB |
| node (12.3) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 100275.67 | 150.32 MB |
| node (12.3) | [koa](http://koajs.com) (2.7) | 97153.67 | 205.79 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 87639.67 | 153.42 MB |
| python (3.7) | [starlette](http://starlette.io) (0.12) | 83918.67 | 180.72 MB |
| node (12.3) | [restify](http://restify.com) (8.2) | 83087.33 | 145.49 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 82088.00 | 176.03 MB |
| python (3.7) | [hug](http://hug.rest) (2.5) | 81120.00 | 200.85 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 80574.33 | 135.29 MB |
| node (12.3) | [express](http://expressjs.com) (4.16) | 76776.33 | 188.13 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 63226.33 | 313.33 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 62511.00 | 310.68 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 61575.00 | 305.46 MB |
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 58662.00 | 76.64 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 58378.00 | 302.82 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 58162.00 | 288.49 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.24) | 55606.33 | 119.96 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 55245.33 | 88.46 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 52327.00 | 97.05 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 49061.33 | 111.26 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 46303.67 | 241.40 MB |
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 44033.33 | 82.99 MB |
| node (12.3) | [hapi](http://hapijs.com) (18.1) | 42498.00 | 109.83 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 38803.67 | 36.98 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 36810.67 | 68.39 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 35450.67 | 87.32 MB |
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 33435.33 | 41.12 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 32422.67 | 62.64 MB |
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 31763.33 | 57.96 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 29281.33 | 16.88 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 24954.33 | 44.46 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 21377.00 | 42.57 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 17995.67 | 10.38 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 17078.67 | 129.06 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 14186.00 | 36.82 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 14115.33 | 30.75 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 12115.00 | 35.14 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 11603.33 | 34.25 MB |
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 11386.00 | 28.04 MB |
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 9911.00 | 25.49 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3295.67 | 10.08 MB |
<!-- Result till here -->

## How to contribute ?

In any way you want ...

+ Request a framework addition
+ Report a bug (on any implementation)
+ Suggest an idea
+ ...

Any kind of idea is :heart:

## Contributors

- [Taichiro Suzuki](https://github.com/tbrand) - Author | Maintainer
- [OvermindDL1](https://github.com/OvermindDL1) - Maintainer
- [Marwan Rabbâa](https://github.com/waghanza) - Maintainer

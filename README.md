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
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: zend-framework (php)


:four: symfony (php)


:five: laravel (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 0.06 ms | 0.07 ms | 0.09 ms | 0.12 ms | 7.37 ms | 57.33 | 
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 0.26 ms | 0.22 ms | 0.42 ms | 0.81 ms | 15.31 ms | 185.67 | 
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 78.91 ms | 0.23 ms | 167.92 ms | 1606.14 ms | 6816.34 ms | 336842.33 | 
| php (7.2) | [symfony](http://symfony.com) (4.2) | 90.93 ms | 0.24 ms | 166.76 ms | 1982.60 ms | 6738.53 ms | 381451.00 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 89.95 ms | 0.24 ms | 223.20 ms | 1704.21 ms | 6808.39 ms | 340795.33 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 84.58 ms | 0.24 ms | 158.12 ms | 1831.34 ms | 6822.59 ms | 383792.33 | 
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 110.66 ms | 0.25 ms | 178.82 ms | 2788.94 ms | 6751.06 ms | 462168.00 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 84.23 ms | 0.26 ms | 167.57 ms | 1959.98 ms | 6731.10 ms | 342600.67 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 5.35 ms | 0.72 ms | 16.09 ms | 50.15 ms | 159.13 ms | 10281.00 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.22 ms | 0.77 ms | 9.16 ms | 28.24 ms | 99.92 ms | 5792.33 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 2.39 ms | 0.87 ms | 6.38 ms | 19.71 ms | 81.15 ms | 4064.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 77.27 ms | 1.15 ms | 2.55 ms | 2719.91 ms | 6592.28 ms | 484961.33 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.36 ms | 1.62 ms | 5.40 ms | 10.71 ms | 26.46 ms | 2332.00 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 2.62 ms | 1.91 ms | 5.26 ms | 8.19 ms | 94.79 ms | 2577.33 | 
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 2.43 ms | 1.93 ms | 4.99 ms | 11.52 ms | 88.97 ms | 2554.33 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 6.25 ms | 2.04 ms | 17.74 ms | 47.15 ms | 140.60 ms | 9939.00 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.04 ms | 2.17 ms | 13.69 ms | 34.19 ms | 104.95 ms | 7282.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 2.15 ms | 2.19 ms | 3.44 ms | 6.56 ms | 30.99 ms | 1381.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.38 ms | 2.39 ms | 4.01 ms | 5.46 ms | 34.30 ms | 1200.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 2.98 ms | 2.40 ms | 6.31 ms | 12.52 ms | 28.01 ms | 2694.67 | 
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 2.75 ms | 2.68 ms | 3.97 ms | 7.06 ms | 142.03 ms | 2036.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 3.30 ms | 2.74 ms | 6.37 ms | 11.86 ms | 25.50 ms | 2469.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.22 ms | 2.89 ms | 5.18 ms | 9.77 ms | 20.91 ms | 1797.00 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 4.23 ms | 3.54 ms | 6.99 ms | 25.90 ms | 120.88 ms | 6459.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 4.03 ms | 4.00 ms | 6.72 ms | 8.04 ms | 32.80 ms | 2042.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 4.24 ms | 4.02 ms | 6.01 ms | 12.34 ms | 209.72 ms | 4103.33 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 4.42 ms | 4.07 ms | 7.63 ms | 14.55 ms | 157.37 ms | 3293.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.01 ms | 4.20 ms | 8.17 ms | 22.70 ms | 79.40 ms | 4309.00 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 4.57 ms | 4.28 ms | 7.79 ms | 15.29 ms | 125.14 ms | 3047.00 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 4.81 ms | 4.36 ms | 8.32 ms | 16.14 ms | 102.19 ms | 3639.33 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 4.96 ms | 4.44 ms | 7.05 ms | 21.50 ms | 46.39 ms | 3480.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 5.28 ms | 4.51 ms | 8.99 ms | 17.70 ms | 216.25 ms | 5386.00 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 5.22 ms | 4.53 ms | 9.08 ms | 17.57 ms | 46.53 ms | 3305.33 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 5.32 ms | 4.58 ms | 9.14 ms | 17.80 ms | 99.96 ms | 3621.67 | 
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 6.26 ms | 4.60 ms | 11.59 ms | 24.95 ms | 200.37 ms | 5876.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 5.56 ms | 4.66 ms | 9.48 ms | 18.61 ms | 214.30 ms | 4808.67 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 6.60 ms | 4.71 ms | 12.98 ms | 25.80 ms | 176.55 ms | 6182.33 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 6.94 ms | 5.00 ms | 12.05 ms | 26.51 ms | 262.65 ms | 7891.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 7.14 ms | 6.05 ms | 11.70 ms | 20.26 ms | 161.07 ms | 4974.33 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 28.16 ms | 6.09 ms | 91.65 ms | 233.78 ms | 694.74 ms | 50502.33 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 9.00 ms | 6.19 ms | 16.20 ms | 38.52 ms | 381.39 ms | 13924.67 | 
| go (1.11) | [gf](http://gfer.me) (1.3) | 6.92 ms | 6.30 ms | 10.02 ms | 20.76 ms | 281.85 ms | 5353.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 200.41 ms | 6.32 ms | 29.55 ms | 4972.94 ms | 7916.98 ms | 840478.67 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 9.67 ms | 7.10 ms | 16.52 ms | 40.08 ms | 370.61 ms | 13212.67 | 
| node (11.1) | [fastify](http://fastify.io) (1.13) | 10.73 ms | 7.86 ms | 18.26 ms | 41.39 ms | 385.15 ms | 14104.00 | 
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 11.42 ms | 9.18 ms | 19.64 ms | 38.26 ms | 294.21 ms | 9686.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 9.57 ms | 9.76 ms | 11.18 ms | 12.92 ms | 128.28 ms | 1846.33 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 13.88 ms | 9.77 ms | 17.84 ms | 100.51 ms | 803.11 ms | 34632.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 38.70 ms | 9.92 ms | 21.54 ms | 1079.92 ms | 3483.92 ms | 199651.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 14.24 ms | 12.72 ms | 27.01 ms | 41.67 ms | 70.72 ms | 9155.67 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 16.45 ms | 13.08 ms | 26.28 ms | 53.80 ms | 536.67 ms | 19375.00 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 16.56 ms | 13.90 ms | 27.98 ms | 47.70 ms | 329.77 ms | 12610.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 20.98 ms | 17.20 ms | 37.68 ms | 57.28 ms | 394.26 ms | 14397.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 20.48 ms | 17.59 ms | 27.81 ms | 75.71 ms | 733.38 ms | 23239.67 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 21.61 ms | 19.32 ms | 33.07 ms | 38.40 ms | 123.41 ms | 6840.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 21.61 ms | 20.93 ms | 27.83 ms | 33.59 ms | 306.52 ms | 9748.00 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 34.04 ms | 21.10 ms | 37.85 ms | 458.51 ms | 1288.79 ms | 81286.67 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 24.72 ms | 21.86 ms | 36.01 ms | 41.51 ms | 319.45 ms | 11457.33 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 23.97 ms | 22.62 ms | 30.79 ms | 84.56 ms | 313.66 ms | 13965.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 30.90 ms | 27.31 ms | 53.48 ms | 95.06 ms | 164.47 ms | 18169.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 28.00 ms | 28.21 ms | 33.57 ms | 36.74 ms | 243.01 ms | 7017.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 35.91 ms | 29.56 ms | 63.28 ms | 87.49 ms | 340.19 ms | 18555.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 30.09 ms | 31.82 ms | 38.75 ms | 42.72 ms | 318.30 ms | 9351.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 46.81 ms | 36.98 ms | 86.28 ms | 158.05 ms | 217.21 ms | 30537.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 58.92 ms | 58.94 ms | 69.84 ms | 85.61 ms | 863.92 ms | 31336.00 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 458980.67 | 265.34 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 455064.33 | 544.83 MB |
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 407398.67 | 463.26 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 367549.33 | 417.27 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 366922.33 | 355.71 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 358001.33 | 576.17 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 326623.67 | 655.81 MB |
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 320396.33 | 654.06 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 303729.67 | 324.61 MB |
| java (8) | [act](http://actframework.org) (1.8) | 302355.00 | 590.25 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 270071.67 | 156.18 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 218503.00 | 355.85 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 218103.33 | 292.19 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 212658.33 | 284.03 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 205782.33 | 361.19 MB |
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 205354.00 | 259.54 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 202198.67 | 270.53 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 190581.67 | 334.61 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 190230.67 | 334.07 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 187261.67 | 252.10 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 178388.33 | 237.99 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 169773.00 | 254.39 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 163711.33 | 420.57 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 155414.00 | 232.80 MB |
| go (1.11) | [gf](http://gfer.me) (1.3) | 144429.67 | 219.24 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 139886.33 | 344.82 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 130804.67 | 196.04 MB |
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 119719.00 | 237.23 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 116860.67 | 247.39 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 111283.67 | 265.13 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 102833.00 | 96.70 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 93308.67 | 199.89 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 92840.67 | 194.68 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 92464.67 | 161.59 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 87249.67 | 146.95 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 84255.67 | 417.41 MB |
| php (7.2) | [symfony](http://symfony.com) (4.2) | 82961.00 | 411.61 MB |
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 82383.67 | 408.69 MB |
| c (99) | [kore](http://kore.io) (3.1) | 78619.67 | 213.08 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 77598.67 | 402.55 MB |
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 77127.67 | 382.15 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 73856.67 | 167.38 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 71202.00 | 108.90 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 66182.33 | 161.44 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 63930.67 | 333.04 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 62964.33 | 110.07 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 54727.67 | 52.18 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 51428.33 | 95.39 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 48580.00 | 119.56 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 46877.33 | 43.92 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 45726.33 | 74.52 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 43268.33 | 40.57 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 43263.00 | 111.71 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 40250.33 | 23.19 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 40218.67 | 73.39 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 35758.67 | 43.82 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 33670.33 | 54.86 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 33647.00 | 59.85 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 27932.67 | 80.92 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 25502.00 | 14.69 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 24217.67 | 182.90 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 23580.67 | 47.00 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 20536.33 | 53.23 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 16894.00 | 45.47 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 4556.00 | 13.94 MB |
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

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
Last update: 2018-11-27
```
OS: Linux (version: 4.19.3-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: symfony (php)


:three: laravel (php)


:four: slim (php)


:five: iron (rust)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 0.07 ms | 0.08 ms | 0.11 ms | 0.14 ms | 5.46 ms | 41.67 | 
| php (7.2) | [symfony](http://symfony.com) (4.1) | 87.46 ms | 0.27 ms | 175.19 ms | 1922.27 ms | 6754.87 ms | 364627.67 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 101.47 ms | 0.28 ms | 254.74 ms | 1844.43 ms | 6836.44 ms | 377352.00 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 80.01 ms | 0.29 ms | 177.38 ms | 1709.15 ms | 6755.46 ms | 326898.33 | 
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 0.35 ms | 0.29 ms | 0.66 ms | 1.36 ms | 31.40 ms | 350.33 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 147.47 ms | 0.29 ms | 310.83 ms | 2807.11 ms | 7015.54 ms | 530982.33 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.53 ms | 0.69 ms | 10.08 ms | 33.19 ms | 115.23 ms | 6755.00 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 6.17 ms | 0.81 ms | 18.85 ms | 58.52 ms | 176.43 ms | 12008.00 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 2.68 ms | 0.91 ms | 7.24 ms | 22.62 ms | 86.25 ms | 4623.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 174.09 ms | 1.22 ms | 186.98 ms | 4117.34 ms | 6594.65 ms | 747417.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 2.11 ms | 1.69 ms | 2.85 ms | 11.05 ms | 143.34 ms | 2583.00 | 
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.48 ms | 1.79 ms | 5.53 ms | 11.01 ms | 27.58 ms | 2374.00 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 2.66 ms | 1.95 ms | 5.28 ms | 8.51 ms | 94.50 ms | 2269.00 | 
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 2.51 ms | 1.99 ms | 5.25 ms | 11.96 ms | 29.42 ms | 2389.00 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 6.96 ms | 2.25 ms | 19.74 ms | 53.14 ms | 160.29 ms | 11158.33 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.53 ms | 2.41 ms | 14.94 ms | 37.27 ms | 112.68 ms | 7956.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.57 ms | 2.63 ms | 4.29 ms | 5.87 ms | 82.44 ms | 1972.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.19 ms | 2.70 ms | 6.63 ms | 12.26 ms | 42.33 ms | 2711.00 | 
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 2.80 ms | 2.73 ms | 3.95 ms | 6.62 ms | 90.44 ms | 1344.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.59 ms | 3.09 ms | 6.16 ms | 10.76 ms | 33.95 ms | 2040.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 3.73 ms | 3.19 ms | 7.13 ms | 13.05 ms | 28.86 ms | 2687.00 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 5.14 ms | 3.67 ms | 8.61 ms | 51.28 ms | 113.28 ms | 8686.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 3.90 ms | 3.87 ms | 6.47 ms | 7.70 ms | 16.64 ms | 1932.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 4.64 ms | 4.26 ms | 7.81 ms | 15.48 ms | 161.23 ms | 4347.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.14 ms | 4.26 ms | 8.16 ms | 26.54 ms | 87.38 ms | 4742.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 4.87 ms | 4.33 ms | 6.71 ms | 15.58 ms | 281.97 ms | 7233.67 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 4.64 ms | 4.34 ms | 7.86 ms | 15.54 ms | 49.45 ms | 2877.67 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 5.12 ms | 4.43 ms | 8.52 ms | 17.34 ms | 329.42 ms | 6667.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 5.27 ms | 4.55 ms | 9.02 ms | 17.87 ms | 159.43 ms | 4267.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 5.38 ms | 4.60 ms | 9.31 ms | 18.05 ms | 49.02 ms | 3371.00 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 5.64 ms | 4.62 ms | 8.26 ms | 27.62 ms | 54.56 ms | 4611.00 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 5.71 ms | 4.68 ms | 9.48 ms | 19.20 ms | 274.12 ms | 7015.00 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 5.66 ms | 4.73 ms | 9.65 ms | 18.92 ms | 52.14 ms | 3523.67 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 6.58 ms | 4.86 ms | 11.71 ms | 20.53 ms | 155.29 ms | 5071.00 | 
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 9.08 ms | 6.28 ms | 17.74 ms | 39.75 ms | 314.66 ms | 10693.33 | 
| node (11.1) | [fastify](http://fastify.io) (1.13) | 8.84 ms | 6.51 ms | 14.08 ms | 32.38 ms | 357.05 ms | 12199.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 200.99 ms | 6.53 ms | 80.43 ms | 4726.41 ms | 7921.41 ms | 821720.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 7.62 ms | 6.63 ms | 11.82 ms | 20.22 ms | 210.24 ms | 5267.67 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 30.30 ms | 6.71 ms | 97.92 ms | 248.23 ms | 665.84 ms | 53566.33 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 9.82 ms | 6.89 ms | 17.75 ms | 41.10 ms | 425.58 ms | 14691.00 | 
| go (1.11) | [gf](http://gfer.me) (1.2) | 7.64 ms | 6.93 ms | 11.35 ms | 24.38 ms | 172.81 ms | 5090.00 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 12.05 ms | 8.29 ms | 21.63 ms | 53.19 ms | 514.91 ms | 19940.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 11.41 ms | 9.62 ms | 20.34 ms | 39.96 ms | 390.90 ms | 14718.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 14.33 ms | 9.89 ms | 18.15 ms | 76.49 ms | 964.86 ms | 39528.33 | 
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 12.74 ms | 10.02 ms | 21.15 ms | 43.01 ms | 409.83 ms | 14396.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 10.02 ms | 10.21 ms | 11.70 ms | 13.36 ms | 164.87 ms | 3020.33 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 13.77 ms | 10.58 ms | 21.54 ms | 53.06 ms | 534.29 ms | 20729.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 15.57 ms | 11.93 ms | 30.42 ms | 43.26 ms | 87.22 ms | 9673.67 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 21.02 ms | 14.05 ms | 30.43 ms | 200.91 ms | 889.10 ms | 44251.67 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 18.45 ms | 14.81 ms | 31.35 ms | 58.64 ms | 446.70 ms | 18070.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 22.77 ms | 17.55 ms | 43.12 ms | 68.17 ms | 368.06 ms | 14672.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 21.45 ms | 18.68 ms | 32.99 ms | 45.73 ms | 196.75 ms | 10440.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 22.05 ms | 19.82 ms | 33.33 ms | 43.11 ms | 309.96 ms | 9386.00 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 23.26 ms | 20.01 ms | 35.59 ms | 48.59 ms | 205.80 ms | 9440.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 23.24 ms | 20.11 ms | 32.21 ms | 41.64 ms | 390.94 ms | 11356.33 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 29.98 ms | 21.97 ms | 39.52 ms | 256.11 ms | 1026.99 ms | 53091.67 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 30.10 ms | 27.53 ms | 38.38 ms | 98.88 ms | 272.10 ms | 15305.00 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 27.63 ms | 28.76 ms | 36.21 ms | 62.41 ms | 257.37 ms | 12169.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 35.58 ms | 29.92 ms | 66.74 ms | 116.26 ms | 207.51 ms | 23875.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 32.13 ms | 30.17 ms | 42.54 ms | 81.64 ms | 315.51 ms | 14590.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 45.37 ms | 37.42 ms | 75.75 ms | 135.56 ms | 181.52 ms | 27038.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 43.41 ms | 37.68 ms | 80.01 ms | 99.76 ms | 507.98 ms | 23602.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 64.03 ms | 62.87 ms | 78.90 ms | 96.93 ms | 679.99 ms | 22666.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (fasthttprouter) (go)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 486909.67 | 281.61 MB |
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 421040.33 | 504.07 MB |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 403668.33 | 458.49 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 352240.67 | 567.39 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 340341.00 | 329.88 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 333514.67 | 378.50 MB |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 308896.00 | 631.17 MB |
| java (8) | [act](http://actframework.org) (1.8) | 298235.33 | 582.24 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 292307.00 | 587.65 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 269550.33 | 288.63 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 245935.67 | 142.16 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 210542.33 | 282.16 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 209942.67 | 281.20 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 205767.33 | 335.24 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 195125.33 | 261.92 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 188646.00 | 331.12 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 187963.33 | 330.06 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 183852.33 | 322.82 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 178530.67 | 241.30 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 175229.00 | 233.63 MB |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 153702.67 | 197.12 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 152739.67 | 392.53 MB |
| go (1.11) | [gf](http://gfer.me) (1.2) | 132524.67 | 201.32 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 130782.00 | 322.28 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 130292.00 | 314.52 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 126709.67 | 189.59 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 120105.33 | 179.92 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 101343.33 | 151.90 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 98889.67 | 173.00 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 98291.67 | 92.36 MB |
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 96111.33 | 191.03 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 91322.00 | 195.93 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 86104.67 | 144.76 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 85311.33 | 178.73 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 82766.67 | 175.09 MB |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 76994.00 | 382.27 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 75265.67 | 373.19 MB |
| c (99) | [kore](http://kore.io) (3.1) | 72098.67 | 195.66 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 68524.00 | 155.44 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 61819.00 | 92.44 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 61446.00 | 150.01 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 58508.33 | 102.41 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 57802.33 | 301.38 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 57470.00 | 300.93 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 48217.67 | 45.91 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 46700.67 | 86.64 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 44927.00 | 42.14 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 44672.67 | 110.11 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 42971.33 | 40.29 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 42719.00 | 69.67 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 40998.67 | 105.78 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 37064.00 | 21.36 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 36555.67 | 44.78 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 33940.33 | 62.00 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 31849.33 | 51.91 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 29845.67 | 53.12 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 23376.33 | 13.46 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 23009.00 | 66.60 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 22817.33 | 45.49 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 21072.00 | 159.18 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 18450.67 | 47.83 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 15324.67 | 41.00 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 4230.67 | 12.94 MB |
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

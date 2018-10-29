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

> job is either a language (example : crystal) or a framework (example : router.cr)

~~~sh
bin/neph [job]
~~~

+ Start the benchmark ....

> tools is a list of language / framework to challenge (example : ruby kemal amber go python)

~~~sh
bin/benchmarker [tools]
~~~

## Results

<!-- Result from here -->
Last update: 2018-10-29
```
OS: Linux (version: 4.18.16-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: laravel (php)


:three: symfony (php)


:four: iron (rust)


:five: rack-routing (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.29) | [nickel](http://nickel-org.github.io) (0.10) | 0.08 ms | 0.08 ms | 0.12 ms | 0.17 ms | 1.27 ms | 30.67 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 154.47 ms | 0.41 ms | 340.40 ms | 3081.56 ms | 6940.28 ms | 550498.00 | 
| php (7.2) | [symfony](http://symfony.com) (4.1) | 193.50 ms | 0.44 ms | 279.90 ms | 4611.19 ms | 7114.34 ms | 746574.67 | 
| rust (1.29) | [iron](http://ironframework.io) (0.6) | 0.55 ms | 0.49 ms | 0.99 ms | 1.57 ms | 26.86 ms | 390.00 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 5.81 ms | 1.02 ms | 17.40 ms | 50.31 ms | 151.88 ms | 10455.33 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 8.69 ms | 1.18 ms | 27.03 ms | 76.24 ms | 198.21 ms | 15892.67 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 4.62 ms | 1.25 ms | 13.33 ms | 37.70 ms | 120.45 ms | 7837.33 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 182.62 ms | 1.94 ms | 311.79 ms | 3947.49 ms | 6696.86 ms | 663185.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 77.90 ms | 2.19 ms | 4.67 ms | 2718.09 ms | 6595.15 ms | 487850.67 | 
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 3.78 ms | 3.02 ms | 7.61 ms | 16.29 ms | 40.19 ms | 3490.67 | 
| rust (1.29) | [actix-web](http://actix.rs) (0.7) | 4.00 ms | 3.43 ms | 7.45 ms | 15.02 ms | 35.31 ms | 3157.00 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 8.50 ms | 3.60 ms | 23.50 ms | 54.91 ms | 143.70 ms | 11933.67 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 11.43 ms | 3.70 ms | 33.14 ms | 81.46 ms | 231.81 ms | 17474.33 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.62 ms | 4.37 ms | 7.11 ms | 14.84 ms | 159.18 ms | 3683.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 5.37 ms | 4.43 ms | 10.91 ms | 19.26 ms | 39.58 ms | 4147.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.13 ms | 4.57 ms | 6.01 ms | 10.39 ms | 25.61 ms | 2065.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.00 ms | 4.73 ms | 8.18 ms | 15.85 ms | 61.16 ms | 3069.67 | 
| crystal (0.26.1) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 5.40 ms | 4.91 ms | 9.46 ms | 16.93 ms | 34.89 ms | 3243.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 7.55 ms | 6.58 ms | 10.48 ms | 20.00 ms | 360.70 ms | 9491.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 29.34 ms | 6.64 ms | 11.93 ms | 944.34 ms | 2218.30 ms | 142054.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 8.02 ms | 7.06 ms | 12.36 ms | 24.73 ms | 108.90 ms | 4593.67 | 
| go (1.11) | [iris](http://iris-go.com) (10.7) | 8.33 ms | 7.14 ms | 12.40 ms | 26.33 ms | 289.64 ms | 8504.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 7.86 ms | 7.45 ms | 12.26 ms | 23.31 ms | 145.38 ms | 4954.33 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 8.74 ms | 7.84 ms | 13.46 ms | 26.75 ms | 190.74 ms | 6024.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 8.84 ms | 7.91 ms | 13.97 ms | 27.62 ms | 168.50 ms | 5165.67 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 29.16 ms | 8.44 ms | 80.98 ms | 309.62 ms | 494.13 ms | 64868.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 9.84 ms | 8.60 ms | 14.84 ms | 31.31 ms | 430.78 ms | 11162.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 239.40 ms | 9.01 ms | 227.69 ms | 5020.73 ms | 7914.84 ms | 893248.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 10.03 ms | 9.13 ms | 15.64 ms | 31.23 ms | 153.37 ms | 5816.33 | 
| go (1.11) | [beego](http://beego.me) (1.10) | 10.45 ms | 9.48 ms | 15.94 ms | 32.94 ms | 166.07 ms | 5863.00 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 39.04 ms | 10.19 ms | 123.79 ms | 311.16 ms | 790.38 ms | 66985.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 14.23 ms | 11.44 ms | 26.86 ms | 50.14 ms | 224.01 ms | 10442.33 | 
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 18.31 ms | 11.64 ms | 24.15 ms | 179.92 ms | 890.26 ms | 43890.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 12.25 ms | 12.23 ms | 14.77 ms | 17.42 ms | 175.05 ms | 3765.67 | 
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 17.49 ms | 12.34 ms | 24.81 ms | 113.01 ms | 744.40 ms | 33153.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 18.29 ms | 13.51 ms | 24.09 ms | 138.58 ms | 942.18 ms | 40344.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 17.33 ms | 15.61 ms | 31.41 ms | 52.09 ms | 504.99 ms | 15309.00 | 
| node (10.12) | [fastify](http://fastify.io) (1.12) | 23.85 ms | 18.01 ms | 32.38 ms | 163.37 ms | 826.24 ms | 38215.67 | 
| node (10.12) | [restify](http://restify.com) (7.2) | 22.53 ms | 18.71 ms | 32.17 ms | 74.06 ms | 514.98 ms | 20335.00 | 
| node (10.12) | [koa](http://koajs.com) (2.6) | 25.08 ms | 19.46 ms | 33.83 ms | 154.86 ms | 821.57 ms | 37617.33 | 
| node (10.12) | [foxify](http://foxify.js.org) (0.10) | 32.86 ms | 20.16 ms | 37.54 ms | 458.33 ms | 1313.27 ms | 80794.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 25.87 ms | 23.89 ms | 46.31 ms | 65.70 ms | 102.41 ms | 14562.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 27.34 ms | 25.62 ms | 38.21 ms | 54.14 ms | 184.13 ms | 8302.00 | 
| node (10.12) | [express](http://expressjs.com) (4.16) | 42.18 ms | 26.16 ms | 50.41 ms | 543.24 ms | 1412.26 ms | 93065.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 40.98 ms | 27.25 ms | 93.57 ms | 127.50 ms | 272.26 ms | 29515.67 | 
| crystal (0.26.1) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 31.27 ms | 29.89 ms | 39.35 ms | 67.69 ms | 254.96 ms | 10379.67 | 
| node (10.12) | [hapi](http://hapijs.com) (17.6) | 58.78 ms | 32.04 ms | 58.29 ms | 851.98 ms | 1795.18 ms | 140549.00 | 
| crystal (0.26.1) | [raze](http://razecr.com) (0.3) | 33.05 ms | 32.22 ms | 41.50 ms | 57.07 ms | 499.34 ms | 18858.67 | 
| crystal (0.26.1) | [lucky](http://luckyframework.org) (0.11) | 38.15 ms | 37.85 ms | 45.57 ms | 53.31 ms | 251.93 ms | 8476.00 | 
| crystal (0.26.1) | [amber](http://amberframework.org) (0.9) | 37.95 ms | 38.26 ms | 45.90 ms | 56.02 ms | 279.23 ms | 11221.33 | 
| crystal (0.26.1) | [kemal](http://kemalcr.com) (0.24) | 40.34 ms | 40.92 ms | 49.54 ms | 58.54 ms | 246.57 ms | 8386.33 | 
| crystal (0.26.1) | [orion](http://github.com/obsidian/orion) (1.6) | 46.03 ms | 43.93 ms | 57.21 ms | 68.36 ms | 210.70 ms | 8639.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 60.46 ms | 50.96 ms | 113.06 ms | 194.87 ms | 368.24 ms | 40356.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 72.02 ms | 57.10 ms | 137.49 ms | 174.32 ms | 481.94 ms | 37870.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 102.69 ms | 91.88 ms | 178.71 ms | 287.66 ms | 429.93 ms | 56612.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 95.01 ms | 95.81 ms | 117.77 ms | 142.75 ms | 419.06 ms | 23405.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (actix-web) (rust)


:three: (jester) (nim)


:four: (evhtp) (cpp)


:five: (fasthttprouter) (go)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 272059.67 | 325.42 MB |
| rust (1.29) | [actix-web](http://actix.rs) (0.7) | 233489.67 | 265.45 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 218529.33 | 439.28 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 217825.67 | 211.23 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 202917.00 | 326.34 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 200048.00 | 227.22 MB |
| crystal (0.26.1) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 180105.33 | 193.73 MB |
| java (8) | [act](http://actframework.org) (1.8) | 154662.67 | 264.32 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 129610.67 | 211.13 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 122461.33 | 162.56 MB |
| go (1.11) | [iris](http://iris-go.com) (10.7) | 119458.67 | 160.19 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 113520.33 | 151.67 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 112468.00 | 197.55 MB |
| rust (1.29) | [iron](http://ironframework.io) (0.6) | 107840.00 | 135.86 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 104091.00 | 139.12 MB |
| rust (1.29) | [nickel](http://nickel-org.github.io) (0.10) | 104031.00 | 206.77 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 103363.33 | 181.30 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 99040.00 | 173.73 MB |
| go (1.11) | [beego](http://beego.me) (1.10) | 96039.00 | 130.40 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 80457.00 | 75.67 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 73949.00 | 182.20 MB |
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 73802.00 | 110.56 MB |
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 70051.00 | 104.97 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 63830.67 | 106.98 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 60848.67 | 106.42 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 60407.00 | 129.50 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 56817.00 | 281.62 MB |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 53895.67 | 267.64 MB |
| node (10.12) | [fastify](http://fastify.io) (1.12) | 51082.67 | 123.05 MB |
| node (10.12) | [restify](http://restify.com) (7.2) | 47653.33 | 83.48 MB |
| node (10.12) | [koa](http://koajs.com) (2.6) | 45717.67 | 96.63 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 45418.00 | 236.56 MB |
| c (99) | [kore](http://kore.io) (3.1) | 45245.33 | 122.63 MB |
| node (10.12) | [foxify](http://foxify.js.org) (0.10) | 44218.33 | 92.72 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 42964.67 | 67.79 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 39416.67 | 89.37 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 36511.33 | 67.73 MB |
| node (10.12) | [express](http://expressjs.com) (4.16) | 33644.67 | 82.16 MB |
| crystal (0.26.1) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 32191.67 | 30.18 MB |
| crystal (0.26.1) | [raze](http://razecr.com) (0.3) | 31010.33 | 29.06 MB |
| node (10.12) | [hapi](http://hapijs.com) (17.6) | 28367.00 | 73.38 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 27719.00 | 26.41 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 26551.00 | 65.46 MB |
| crystal (0.26.1) | [amber](http://amberframework.org) (0.9) | 26233.33 | 38.03 MB |
| crystal (0.26.1) | [lucky](http://luckyframework.org) (0.11) | 25965.00 | 31.79 MB |
| crystal (0.26.1) | [kemal](http://kemalcr.com) (0.24) | 24596.67 | 40.07 MB |
| crystal (0.26.1) | [orion](http://github.com/obsidian/orion) (1.6) | 22175.00 | 36.12 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 22119.33 | 12.76 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 17341.67 | 30.91 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 15045.00 | 8.68 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 14783.00 | 111.85 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 14396.33 | 41.78 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 11157.67 | 28.98 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 10241.00 | 27.29 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 9930.67 | 19.77 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 3290.67 | 10.04 MB |
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

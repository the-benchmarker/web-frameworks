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
Last update: 2018-10-26
```
OS: Linux (version: 4.18.16-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: symfony (php)


:three: slim (php)


:four: iron (rust)


:five: laravel (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.29) | [nickel](http://nickel-org.github.io) (0.10) | 0.09 ms | 0.09 ms | 0.12 ms | 0.18 ms | 1.37 ms | 31.67 | 
| php (7.2) | [symfony](http://symfony.com) (4.1) | 133.18 ms | 0.37 ms | 270.02 ms | 2864.47 ms | 6818.00 ms | 490485.33 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 165.37 ms | 0.38 ms | 289.14 ms | 3627.93 ms | 6810.54 ms | 607277.33 | 
| rust (1.29) | [iron](http://ironframework.io) (0.6) | 0.46 ms | 0.47 ms | 0.74 ms | 1.12 ms | 14.62 ms | 260.67 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 113.69 ms | 0.64 ms | 297.67 ms | 1957.78 ms | 6886.33 ms | 405203.67 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 5.32 ms | 0.79 ms | 16.06 ms | 48.55 ms | 143.03 ms | 10023.67 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 8.14 ms | 0.94 ms | 25.54 ms | 75.62 ms | 210.01 ms | 15653.00 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 3.79 ms | 1.19 ms | 10.73 ms | 30.15 ms | 98.53 ms | 6284.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 124.35 ms | 1.72 ms | 4.31 ms | 3832.89 ms | 6595.63 ms | 635557.00 | 
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 3.07 ms | 2.12 ms | 6.48 ms | 14.87 ms | 35.99 ms | 3118.00 | 
| rust (1.29) | [actix-web](http://actix.rs) (0.7) | 2.94 ms | 2.35 ms | 5.72 ms | 12.33 ms | 63.66 ms | 2476.00 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 9.23 ms | 2.82 ms | 26.79 ms | 68.48 ms | 195.62 ms | 14539.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.74 ms | 3.01 ms | 7.83 ms | 14.00 ms | 36.11 ms | 3118.67 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 7.60 ms | 3.23 ms | 20.92 ms | 49.72 ms | 138.34 ms | 10712.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.33 ms | 3.41 ms | 5.43 ms | 8.51 ms | 33.97 ms | 1852.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.21 ms | 3.53 ms | 6.86 ms | 12.21 ms | 48.57 ms | 2312.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.99 ms | 3.56 ms | 6.27 ms | 12.24 ms | 143.99 ms | 3120.67 | 
| crystal (0.26.1) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 4.62 ms | 4.16 ms | 8.32 ms | 15.53 ms | 33.96 ms | 3021.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 5.43 ms | 5.39 ms | 9.23 ms | 10.59 ms | 134.78 ms | 2933.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 6.72 ms | 5.50 ms | 10.25 ms | 21.18 ms | 174.00 ms | 7228.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 6.63 ms | 5.58 ms | 9.46 ms | 18.58 ms | 364.15 ms | 9901.33 | 
| go (1.11) | [iris](http://iris-go.com) (10.7) | 7.08 ms | 6.05 ms | 10.97 ms | 22.45 ms | 112.79 ms | 4249.67 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 7.53 ms | 6.19 ms | 11.78 ms | 25.23 ms | 186.17 ms | 7768.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.85 ms | 6.24 ms | 11.16 ms | 21.16 ms | 145.26 ms | 4652.33 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 7.76 ms | 6.67 ms | 12.30 ms | 25.54 ms | 114.38 ms | 4962.00 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 28.38 ms | 6.89 ms | 89.73 ms | 304.62 ms | 442.83 ms | 64901.33 | 
| go (1.11) | [beego](http://beego.me) (1.10) | 8.12 ms | 7.13 ms | 12.74 ms | 26.25 ms | 113.66 ms | 4736.00 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 8.61 ms | 7.39 ms | 13.43 ms | 28.93 ms | 190.08 ms | 8002.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 222.08 ms | 8.10 ms | 89.77 ms | 4975.94 ms | 7935.33 ms | 873589.67 | 
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 12.09 ms | 8.99 ms | 17.18 ms | 46.01 ms | 530.95 ms | 20992.00 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 35.00 ms | 9.14 ms | 111.20 ms | 273.76 ms | 756.98 ms | 59600.00 | 
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 12.16 ms | 9.32 ms | 17.87 ms | 46.45 ms | 456.09 ms | 17469.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 13.14 ms | 9.38 ms | 27.69 ms | 57.12 ms | 216.53 ms | 11810.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 10.77 ms | 10.86 ms | 12.82 ms | 14.98 ms | 90.07 ms | 2576.33 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 14.71 ms | 11.40 ms | 20.26 ms | 53.11 ms | 730.72 ms | 26829.00 | 
| node (10.12) | [foxify](http://foxify.js.org) (0.10) | 16.54 ms | 12.30 ms | 24.09 ms | 80.58 ms | 644.02 ms | 26759.67 | 
| node (10.12) | [fastify](http://fastify.io) (1.12) | 17.72 ms | 12.84 ms | 24.93 ms | 96.11 ms | 726.49 ms | 31837.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 19.02 ms | 14.15 ms | 28.10 ms | 157.91 ms | 1256.42 ms | 46151.67 | 
| node (10.12) | [koa](http://koajs.com) (2.6) | 23.35 ms | 16.08 ms | 30.31 ms | 215.23 ms | 931.67 ms | 47339.67 | 
| node (10.12) | [restify](http://restify.com) (7.2) | 23.07 ms | 19.30 ms | 36.45 ms | 68.79 ms | 507.28 ms | 19685.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 23.34 ms | 19.45 ms | 42.46 ms | 64.97 ms | 123.01 ms | 13267.00 | 
| node (10.12) | [express](http://expressjs.com) (4.16) | 30.08 ms | 19.74 ms | 36.83 ms | 344.75 ms | 1114.50 ms | 64990.67 | 
| crystal (0.26.1) | [raze](http://razecr.com) (0.3) | 27.59 ms | 25.20 ms | 38.13 ms | 42.31 ms | 185.63 ms | 6102.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 27.50 ms | 25.85 ms | 40.61 ms | 53.73 ms | 266.36 ms | 10781.00 | 
| crystal (0.26.1) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 27.49 ms | 26.44 ms | 34.18 ms | 39.02 ms | 240.33 ms | 6520.00 | 
| node (10.12) | [hapi](http://hapijs.com) (17.6) | 41.82 ms | 27.12 ms | 49.30 ms | 521.06 ms | 1484.57 ms | 90328.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 34.69 ms | 28.88 ms | 62.02 ms | 97.71 ms | 335.81 ms | 18577.00 | 
| crystal (0.26.1) | [lucky](http://luckyframework.org) (0.11) | 34.73 ms | 31.61 ms | 44.71 ms | 53.08 ms | 540.06 ms | 18316.33 | 
| crystal (0.26.1) | [amber](http://amberframework.org) (0.9) | 35.97 ms | 34.25 ms | 43.96 ms | 52.43 ms | 245.48 ms | 7993.67 | 
| crystal (0.26.1) | [kemal](http://kemalcr.com) (0.24) | 35.26 ms | 34.39 ms | 42.29 ms | 46.96 ms | 241.62 ms | 6828.00 | 
| crystal (0.26.1) | [orion](http://github.com/obsidian/orion) (1.6) | 38.26 ms | 35.12 ms | 47.60 ms | 70.38 ms | 258.46 ms | 11463.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 50.87 ms | 41.93 ms | 95.97 ms | 165.51 ms | 297.42 ms | 35084.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 53.37 ms | 49.63 ms | 69.99 ms | 115.90 ms | 398.68 ms | 18979.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 81.22 ms | 80.44 ms | 95.71 ms | 124.95 ms | 717.46 ms | 28460.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 94.12 ms | 83.06 ms | 174.96 ms | 288.68 ms | 502.90 ms | 61005.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (actix-web) (rust)


:three: (vibora) (python)


:four: (evhtp) (cpp)


:five: (jester) (nim)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 342595.67 | 410.13 MB |
| rust (1.29) | [actix-web](http://actix.rs) (0.7) | 321776.67 | 365.63 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 284101.33 | 322.63 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 265023.00 | 257.34 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 248850.00 | 499.75 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 233922.33 | 378.68 MB |
| crystal (0.26.1) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 213132.33 | 227.79 MB |
| java (8) | [act](http://actframework.org) (1.8) | 190067.33 | 325.13 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 150959.00 | 200.68 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 149575.33 | 243.56 MB |
| go (1.11) | [iris](http://iris-go.com) (10.7) | 138922.00 | 185.28 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 135515.00 | 237.87 MB |
| rust (1.29) | [iron](http://ironframework.io) (0.6) | 130476.00 | 164.66 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 128285.00 | 225.26 MB |
| go (1.11) | [beego](http://beego.me) (1.10) | 122918.33 | 165.71 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 122151.33 | 214.21 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 117567.33 | 156.74 MB |
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 94684.33 | 141.96 MB |
| rust (1.29) | [nickel](http://nickel-org.github.io) (0.10) | 92988.33 | 184.38 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 92422.67 | 86.90 MB |
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 90524.00 | 135.66 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 87642.00 | 215.95 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 74210.00 | 124.09 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 72646.67 | 155.95 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 71679.33 | 125.50 MB |
| node (10.12) | [foxify](http://foxify.js.org) (0.10) | 71533.33 | 150.14 MB |
| node (10.12) | [fastify](http://fastify.io) (1.12) | 70307.67 | 166.86 MB |
| c (99) | [kore](http://kore.io) (3.1) | 61993.33 | 168.01 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 59262.33 | 293.55 MB |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 58696.00 | 291.19 MB |
| node (10.12) | [koa](http://koajs.com) (2.6) | 54413.67 | 115.00 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 52876.67 | 84.20 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 50269.00 | 262.15 MB |
| node (10.12) | [restify](http://restify.com) (7.2) | 45745.67 | 80.13 MB |
| node (10.12) | [express](http://expressjs.com) (4.16) | 44894.00 | 109.71 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 44137.33 | 100.11 MB |
| crystal (0.26.1) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 35929.00 | 33.70 MB |
| crystal (0.26.1) | [raze](http://razecr.com) (0.3) | 35909.33 | 33.66 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 35876.67 | 66.54 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 33793.67 | 32.20 MB |
| node (10.12) | [hapi](http://hapijs.com) (17.6) | 33311.00 | 86.08 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 29569.00 | 72.69 MB |
| crystal (0.26.1) | [lucky](http://luckyframework.org) (0.11) | 28875.67 | 35.44 MB |
| crystal (0.26.1) | [kemal](http://kemalcr.com) (0.24) | 28272.33 | 46.04 MB |
| crystal (0.26.1) | [amber](http://amberframework.org) (0.9) | 27327.33 | 39.61 MB |
| crystal (0.26.1) | [orion](http://github.com/obsidian/orion) (1.6) | 25850.00 | 42.12 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 24283.00 | 14.00 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 21170.00 | 37.70 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 18224.67 | 52.90 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 16893.00 | 9.74 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 15850.33 | 119.83 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 13926.33 | 36.11 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 12084.33 | 32.35 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 11022.67 | 21.96 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 3676.33 | 11.17 MB |
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

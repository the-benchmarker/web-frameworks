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
Last update: 2018-11-26
```
OS: Linux (version: 4.19.2-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: symfony (php)


:four: slim (php)


:five: laravel (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 0.06 ms | 0.07 ms | 0.08 ms | 0.11 ms | 3.39 ms | 28.67 | 
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 0.25 ms | 0.21 ms | 0.41 ms | 0.74 ms | 8.88 ms | 173.00 | 
| php (7.2) | [symfony](http://symfony.com) (4.1) | 81.40 ms | 0.24 ms | 164.89 ms | 1808.32 ms | 6717.36 ms | 335983.00 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 106.04 ms | 0.25 ms | 170.78 ms | 2472.49 ms | 6732.36 ms | 446802.33 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 87.78 ms | 0.25 ms | 232.15 ms | 1510.83 ms | 6800.55 ms | 324444.67 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 116.02 ms | 0.26 ms | 194.32 ms | 2730.14 ms | 6767.56 ms | 470569.67 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.13 ms | 0.73 ms | 8.79 ms | 28.52 ms | 110.74 ms | 5826.00 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 5.56 ms | 0.79 ms | 16.70 ms | 52.84 ms | 153.18 ms | 10776.33 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 2.54 ms | 0.85 ms | 6.98 ms | 20.86 ms | 79.27 ms | 4312.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 93.51 ms | 1.20 ms | 20.64 ms | 2899.75 ms | 6567.02 ms | 508109.33 | 
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.44 ms | 1.69 ms | 5.56 ms | 11.95 ms | 27.52 ms | 2526.00 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 2.70 ms | 1.97 ms | 5.41 ms | 8.48 ms | 36.40 ms | 1963.00 | 
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 2.45 ms | 1.97 ms | 4.95 ms | 10.73 ms | 89.43 ms | 2774.67 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 6.36 ms | 2.25 ms | 17.89 ms | 46.44 ms | 140.82 ms | 9841.67 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.21 ms | 2.34 ms | 14.03 ms | 34.50 ms | 102.91 ms | 7372.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.44 ms | 2.39 ms | 4.31 ms | 5.71 ms | 18.26 ms | 1280.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.04 ms | 2.51 ms | 6.35 ms | 12.02 ms | 27.51 ms | 2625.33 | 
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 2.84 ms | 2.80 ms | 3.91 ms | 6.73 ms | 18.61 ms | 1187.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 3.49 ms | 2.90 ms | 6.70 ms | 12.68 ms | 27.43 ms | 2610.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.29 ms | 2.93 ms | 5.45 ms | 10.25 ms | 35.41 ms | 1969.00 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 4.68 ms | 3.66 ms | 7.95 ms | 37.20 ms | 112.92 ms | 7578.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 3.96 ms | 3.78 ms | 6.36 ms | 7.52 ms | 1154.43 ms | 12114.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 4.40 ms | 4.18 ms | 6.26 ms | 12.46 ms | 153.23 ms | 4031.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.00 ms | 4.21 ms | 7.96 ms | 23.99 ms | 131.52 ms | 4834.33 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 4.70 ms | 4.28 ms | 7.77 ms | 15.53 ms | 295.55 ms | 6218.00 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 4.85 ms | 4.42 ms | 8.24 ms | 16.40 ms | 127.78 ms | 3200.67 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 5.02 ms | 4.48 ms | 8.71 ms | 16.96 ms | 124.36 ms | 3255.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 5.41 ms | 4.58 ms | 9.38 ms | 18.60 ms | 152.08 ms | 4240.67 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 5.47 ms | 4.58 ms | 7.93 ms | 26.31 ms | 59.62 ms | 4387.00 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 5.86 ms | 4.59 ms | 9.59 ms | 21.52 ms | 351.45 ms | 9553.00 | 
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 6.02 ms | 4.60 ms | 10.18 ms | 22.63 ms | 206.29 ms | 6484.00 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 5.41 ms | 4.62 ms | 9.30 ms | 18.28 ms | 49.02 ms | 3386.67 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 6.62 ms | 4.71 ms | 13.42 ms | 25.74 ms | 209.52 ms | 6115.67 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 5.76 ms | 4.72 ms | 9.70 ms | 19.57 ms | 226.20 ms | 5767.00 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 6.58 ms | 4.78 ms | 10.79 ms | 26.07 ms | 255.50 ms | 8322.67 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 7.14 ms | 5.13 ms | 12.79 ms | 28.69 ms | 249.60 ms | 7692.67 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 26.56 ms | 5.61 ms | 86.02 ms | 222.58 ms | 572.12 ms | 47741.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 7.72 ms | 5.73 ms | 15.15 ms | 26.96 ms | 161.84 ms | 6660.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 225.56 ms | 6.60 ms | 192.84 ms | 5168.41 ms | 7924.13 ms | 889765.67 | 
| go (1.11) | [gf](http://gfer.me) (1.2) | 7.80 ms | 7.07 ms | 11.68 ms | 24.67 ms | 128.29 ms | 4866.67 | 
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 10.35 ms | 7.32 ms | 17.90 ms | 42.24 ms | 422.84 ms | 15075.67 | 
| node (11.1) | [fastify](http://fastify.io) (1.13) | 11.46 ms | 8.06 ms | 18.97 ms | 47.08 ms | 459.08 ms | 18269.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 11.27 ms | 9.72 ms | 20.41 ms | 38.89 ms | 498.56 ms | 12910.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 12.84 ms | 9.91 ms | 17.85 ms | 47.65 ms | 628.98 ms | 22789.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 10.05 ms | 10.20 ms | 11.68 ms | 13.55 ms | 65.29 ms | 1595.00 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 13.98 ms | 10.39 ms | 22.35 ms | 51.52 ms | 568.73 ms | 22022.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 14.44 ms | 12.87 ms | 25.92 ms | 38.82 ms | 71.21 ms | 8311.67 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 16.91 ms | 13.15 ms | 27.20 ms | 55.73 ms | 599.42 ms | 23185.33 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 18.39 ms | 15.22 ms | 31.96 ms | 55.77 ms | 367.02 ms | 14336.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 22.80 ms | 16.49 ms | 47.48 ms | 69.41 ms | 226.16 ms | 15262.67 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 21.30 ms | 18.10 ms | 32.57 ms | 47.52 ms | 474.70 ms | 14975.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 20.89 ms | 18.70 ms | 29.95 ms | 40.31 ms | 247.39 ms | 9127.00 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 30.91 ms | 23.49 ms | 42.12 ms | 220.19 ms | 933.24 ms | 48055.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 24.62 ms | 26.42 ms | 31.34 ms | 40.20 ms | 187.59 ms | 7019.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 31.82 ms | 26.87 ms | 55.93 ms | 101.08 ms | 213.15 ms | 19438.67 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 29.82 ms | 27.17 ms | 37.31 ms | 61.44 ms | 328.86 ms | 13964.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 26.15 ms | 28.13 ms | 32.49 ms | 40.39 ms | 166.81 ms | 7726.33 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 29.82 ms | 29.43 ms | 35.09 ms | 39.95 ms | 178.70 ms | 5511.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 37.68 ms | 30.35 ms | 69.90 ms | 96.75 ms | 348.33 ms | 21506.00 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 31.79 ms | 32.96 ms | 40.48 ms | 65.80 ms | 477.84 ms | 21009.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 44.09 ms | 37.13 ms | 72.34 ms | 128.74 ms | 167.42 ms | 22734.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 58.93 ms | 59.78 ms | 73.24 ms | 87.57 ms | 445.32 ms | 16665.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (actix-web) (rust)


:three: (evhtp) (cpp)


:four: (vibora) (python)


:five: (fasthttprouter) (go)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 449278.67 | 537.70 MB |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 399234.67 | 453.13 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 358433.33 | 347.49 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 347753.33 | 394.70 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 347315.33 | 560.50 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 315999.33 | 635.20 MB |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 306078.00 | 625.28 MB |
| java (8) | [act](http://actframework.org) (1.8) | 297602.67 | 580.94 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 286866.00 | 306.72 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 255493.67 | 147.88 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 213501.67 | 347.46 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 211355.33 | 283.10 MB |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 209273.67 | 264.51 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 202966.67 | 270.71 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 193574.33 | 258.61 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 192444.00 | 337.81 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 183757.00 | 247.90 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 183294.33 | 321.27 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 180036.67 | 315.49 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 174881.00 | 262.06 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 174426.67 | 232.00 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 165406.00 | 247.74 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 161125.00 | 414.43 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 151579.33 | 227.08 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 135925.67 | 334.85 MB |
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 134153.00 | 268.24 MB |
| go (1.11) | [gf](http://gfer.me) (1.2) | 130365.67 | 197.90 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 111783.00 | 235.05 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 108890.00 | 257.30 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 97619.33 | 91.75 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 95485.00 | 167.06 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 92568.67 | 198.69 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 85269.33 | 143.49 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 83653.00 | 414.30 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 82803.00 | 174.96 MB |
| c (99) | [kore](http://kore.io) (3.1) | 80777.00 | 219.02 MB |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 80365.67 | 398.39 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 76956.67 | 399.30 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 71533.33 | 162.22 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 65071.33 | 158.81 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 62047.67 | 322.76 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 56902.00 | 99.47 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 50912.33 | 48.51 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 49729.33 | 72.18 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 47609.33 | 88.30 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 47329.67 | 44.39 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 46593.33 | 114.85 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 41386.00 | 23.85 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 40995.67 | 38.47 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 38866.00 | 63.31 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 38502.33 | 99.35 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 33830.67 | 41.44 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 33687.67 | 61.49 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 32869.33 | 58.61 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 32694.67 | 53.32 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 27065.33 | 78.58 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 24625.00 | 14.23 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 23342.33 | 176.41 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 23217.33 | 46.25 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 20141.00 | 52.17 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 16423.33 | 43.99 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 4832.33 | 14.78 MB |
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

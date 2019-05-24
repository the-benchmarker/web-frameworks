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
Last update: 2019-05-24
```
OS: Linux (version: 5.0.16-300.fc30.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: zend-framework (php)


:five: flame (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 0.07 ms | 0.08 ms | 0.10 ms | 0.14 ms | 6.31 ms | 52.00 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 2.84 ms | 0.15 ms | 10.42 ms | 26.85 ms | 77.78 ms | 5882.33 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.85 ms | 0.19 ms | 14.58 ms | 33.22 ms | 90.49 ms | 7597.00 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 138.03 ms | 0.32 ms | 263.38 ms | 2972.16 ms | 6811.80 ms | 518013.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.70 ms | 0.32 ms | 19.83 ms | 42.57 ms | 103.58 ms | 9941.67 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 138.28 ms | 0.33 ms | 236.95 ms | 3047.74 ms | 6793.47 ms | 526287.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 133.65 ms | 0.33 ms | 202.14 ms | 3200.09 ms | 6821.25 ms | 538270.67 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 98.36 ms | 0.33 ms | 276.54 ms | 1650.28 ms | 6855.41 ms | 340242.00 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 140.93 ms | 0.33 ms | 247.86 ms | 3213.97 ms | 6772.29 ms | 538942.33 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 149.03 ms | 0.35 ms | 218.51 ms | 3738.47 ms | 6797.05 ms | 602710.00 | 
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 0.35 ms | 0.35 ms | 0.58 ms | 0.91 ms | 37.28 ms | 308.33 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 6.91 ms | 0.35 ms | 24.13 ms | 50.48 ms | 148.31 ms | 12001.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 7.74 ms | 0.43 ms | 25.84 ms | 53.76 ms | 130.45 ms | 12758.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 62.18 ms | 1.49 ms | 3.25 ms | 2099.01 ms | 4949.51 ms | 375642.33 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 2.96 ms | 1.58 ms | 7.05 ms | 15.91 ms | 47.35 ms | 3459.33 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.56 ms | 1.90 ms | 8.73 ms | 19.68 ms | 38.14 ms | 4226.00 | 
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 2.91 ms | 2.31 ms | 5.90 ms | 13.05 ms | 32.14 ms | 2639.00 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 31.88 ms | 2.39 ms | 107.78 ms | 289.25 ms | 834.39 ms | 61826.67 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.25 ms | 2.59 ms | 5.61 ms | 10.66 ms | 49.38 ms | 2254.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 6.47 ms | 2.68 ms | 6.97 ms | 95.20 ms | 978.36 ms | 36551.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.74 ms | 3.04 ms | 8.03 ms | 14.88 ms | 36.95 ms | 3291.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.07 ms | 3.05 ms | 5.20 ms | 7.60 ms | 68.22 ms | 1793.33 | 
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 4.20 ms | 3.38 ms | 7.93 ms | 18.45 ms | 206.80 ms | 4506.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.14 ms | 3.40 ms | 7.18 ms | 13.11 ms | 37.07 ms | 2515.00 | 
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 3.78 ms | 3.56 ms | 6.33 ms | 12.44 ms | 30.02 ms | 2401.33 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 6.20 ms | 3.77 ms | 9.52 ms | 75.58 ms | 117.64 ms | 11716.00 | 
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 3.91 ms | 3.81 ms | 6.37 ms | 12.30 ms | 27.60 ms | 2344.33 | 
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 4.38 ms | 3.90 ms | 7.98 ms | 14.65 ms | 29.26 ms | 2881.33 | 
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 4.03 ms | 3.96 ms | 6.46 ms | 12.35 ms | 29.78 ms | 2333.00 | 
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 4.05 ms | 3.99 ms | 6.53 ms | 12.39 ms | 30.82 ms | 2343.33 | 
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 4.81 ms | 4.59 ms | 7.70 ms | 12.94 ms | 36.37 ms | 2438.33 | 
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 6.53 ms | 4.61 ms | 14.32 ms | 30.45 ms | 156.57 ms | 6418.67 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 6.35 ms | 4.65 ms | 13.42 ms | 27.02 ms | 164.31 ms | 6042.67 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 6.50 ms | 4.65 ms | 13.84 ms | 28.24 ms | 163.88 ms | 6562.67 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 6.83 ms | 4.69 ms | 14.43 ms | 29.92 ms | 233.55 ms | 8599.00 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 6.21 ms | 4.73 ms | 11.95 ms | 25.93 ms | 164.02 ms | 5443.33 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 7.01 ms | 4.74 ms | 15.60 ms | 31.63 ms | 162.23 ms | 6711.67 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 6.54 ms | 4.75 ms | 13.67 ms | 28.10 ms | 104.73 ms | 5682.33 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 6.28 ms | 4.78 ms | 12.26 ms | 26.19 ms | 129.82 ms | 5157.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 5.98 ms | 4.87 ms | 8.26 ms | 16.37 ms | 401.37 ms | 11546.33 | 
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 6.60 ms | 4.87 ms | 10.87 ms | 20.34 ms | 159.29 ms | 6024.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.89 ms | 5.05 ms | 9.71 ms | 19.62 ms | 171.14 ms | 4689.00 | 
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 6.67 ms | 5.10 ms | 12.92 ms | 28.75 ms | 94.27 ms | 6094.33 | 
| node (12.3) | [restana](http://github.com/jkyberneees/ana) (3.0) | 15.18 ms | 5.42 ms | 13.74 ms | 277.93 ms | 758.12 ms | 53335.67 | 
| go (1.12) | [gf](http://goframe.org) (1.6) | 8.47 ms | 5.67 ms | 18.38 ms | 38.54 ms | 155.53 ms | 7988.33 | 
| node (12.3) | [polka](http://github.com/lukeed/polka) (0.5) | 17.69 ms | 5.76 ms | 13.78 ms | 322.26 ms | 648.82 ms | 55672.33 | 
| node (12.3) | [rayo](http://rayo.js.org) (1.3) | 13.44 ms | 5.87 ms | 13.35 ms | 237.85 ms | 680.13 ms | 43597.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 175.22 ms | 6.61 ms | 30.81 ms | 4254.99 ms | 7077.58 ms | 728567.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 8.65 ms | 7.37 ms | 14.86 ms | 24.86 ms | 214.14 ms | 6468.33 | 
| node (12.3) | [foxify](http://foxify.js.org) (0.10) | 11.12 ms | 7.53 ms | 14.32 ms | 107.09 ms | 563.85 ms | 28335.00 | 
| node (12.3) | [fastify](http://fastify.io) (2.3) | 10.60 ms | 7.84 ms | 14.89 ms | 68.98 ms | 469.38 ms | 19369.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 8.11 ms | 8.29 ms | 10.43 ms | 12.33 ms | 63.05 ms | 1901.67 | 
| node (12.3) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 18.05 ms | 8.45 ms | 20.53 ms | 235.05 ms | 688.30 ms | 47142.33 | 
| node (12.3) | [koa](http://koajs.com) (2.7) | 13.14 ms | 9.00 ms | 17.20 ms | 110.31 ms | 691.04 ms | 33674.00 | 
| python (3.7) | [hug](http://hug.rest) (2.5) | 11.70 ms | 9.56 ms | 20.51 ms | 35.46 ms | 217.35 ms | 7672.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 11.56 ms | 9.81 ms | 20.57 ms | 42.56 ms | 495.31 ms | 12147.00 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 17.59 ms | 10.17 ms | 19.48 ms | 242.86 ms | 1310.84 ms | 59712.33 | 
| node (12.3) | [restify](http://restify.com) (8.2) | 13.66 ms | 10.45 ms | 19.55 ms | 67.79 ms | 450.81 ms | 20819.67 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 11.74 ms | 10.52 ms | 20.13 ms | 30.37 ms | 75.11 ms | 6183.00 | 
| node (12.3) | [express](http://expressjs.com) (4.16) | 16.10 ms | 12.32 ms | 22.47 ms | 99.68 ms | 579.10 ms | 25828.00 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.22) | 17.53 ms | 14.90 ms | 30.54 ms | 45.45 ms | 97.81 ms | 9216.00 | 
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 19.30 ms | 16.76 ms | 28.03 ms | 42.48 ms | 538.46 ms | 19827.67 | 
| node (12.3) | [hapi](http://hapijs.com) (18.1) | 28.70 ms | 18.13 ms | 33.06 ms | 337.24 ms | 1088.47 ms | 62697.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 18.84 ms | 18.37 ms | 32.23 ms | 45.68 ms | 81.79 ms | 9485.33 | 
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 26.36 ms | 19.37 ms | 31.62 ms | 232.18 ms | 1333.57 ms | 61336.67 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 24.35 ms | 19.55 ms | 43.38 ms | 64.94 ms | 233.51 ms | 13249.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 25.97 ms | 21.44 ms | 45.18 ms | 73.09 ms | 312.18 ms | 14993.33 | 
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 33.47 ms | 22.32 ms | 73.76 ms | 178.24 ms | 537.65 ms | 37283.00 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 29.45 ms | 24.71 ms | 54.41 ms | 78.42 ms | 144.21 ms | 16927.67 | 
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 30.59 ms | 28.64 ms | 38.85 ms | 54.82 ms | 323.96 ms | 8794.67 | 
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 31.51 ms | 30.59 ms | 39.31 ms | 47.52 ms | 315.38 ms | 10345.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 41.24 ms | 34.97 ms | 76.57 ms | 136.75 ms | 321.46 ms | 27394.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 47.94 ms | 42.93 ms | 88.76 ms | 127.78 ms | 168.89 ms | 25160.33 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 58.90 ms | 54.54 ms | 108.04 ms | 140.84 ms | 230.46 ms | 34528.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 75.09 ms | 66.10 ms | 114.66 ms | 167.39 ms | 680.26 ms | 36828.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 72.97 ms | 73.58 ms | 88.64 ms | 105.31 ms | 689.63 ms | 22884.67 | 
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 116.82 ms | 114.05 ms | 159.46 ms | 210.29 ms | 348.87 ms | 34196.67 | 
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 177.36 ms | 170.01 ms | 233.85 ms | 561.06 ms | 1871.40 ms | 102619.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (kore) (c)


:five: (fasthttprouter) (go)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 398435.00 | 230.57 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 357006.00 | 427.22 MB |
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 337655.33 | 383.67 MB |
| c (99) | [kore](http://kore.io) (3.1) | 303272.33 | 787.52 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 288543.33 | 467.18 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 287424.00 | 326.30 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 285399.33 | 276.81 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 257199.33 | 516.57 MB |
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 250375.33 | 234.94 MB |
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 246242.00 | 503.91 MB |
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 240809.33 | 226.14 MB |
| java (8) | [act](http://actframework.org) (1.8) | 239535.33 | 413.20 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 235596.00 | 136.36 MB |
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 233576.67 | 290.25 MB |
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 232010.00 | 378.63 MB |
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 224890.00 | 240.31 MB |
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 195822.67 | 319.70 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 173027.67 | 281.94 MB |
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 171123.00 | 227.59 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 170546.00 | 227.72 MB |
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 168892.00 | 213.10 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 168094.67 | 294.80 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 168080.00 | 222.37 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 167741.33 | 222.08 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 167429.67 | 224.92 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 166290.00 | 290.99 MB |
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 163854.00 | 255.59 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 161849.00 | 215.97 MB |
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 149940.33 | 351.50 MB |
| node (12.3) | [restana](http://github.com/jkyberneees/ana) (3.0) | 145104.00 | 217.11 MB |
| node (12.3) | [polka](http://github.com/lukeed/polka) (0.5) | 139026.67 | 207.97 MB |
| node (12.3) | [rayo](http://rayo.js.org) (1.3) | 137171.00 | 205.52 MB |
| go (1.12) | [gf](http://goframe.org) (1.6) | 134484.33 | 204.54 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 119716.00 | 112.42 MB |
| node (12.3) | [fastify](http://fastify.io) (2.3) | 118543.67 | 304.82 MB |
| node (12.3) | [foxify](http://foxify.js.org) (0.10) | 117238.33 | 246.23 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 116962.67 | 288.08 MB |
| node (12.3) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 108722.33 | 162.75 MB |
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 108444.00 | 215.30 MB |
| node (12.3) | [koa](http://koajs.com) (2.7) | 101209.33 | 213.64 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 98813.33 | 172.99 MB |
| python (3.7) | [hug](http://hug.rest) (2.5) | 87248.00 | 216.31 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 85298.00 | 183.76 MB |
| node (12.3) | [restify](http://restify.com) (8.2) | 84013.00 | 147.11 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 83714.67 | 139.96 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 79628.00 | 170.70 MB |
| node (12.3) | [express](http://expressjs.com) (4.16) | 73163.33 | 178.65 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 66058.67 | 327.33 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 64255.00 | 318.60 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 61691.00 | 306.01 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 61285.33 | 303.51 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 61100.33 | 316.90 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 59800.00 | 94.72 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.22) | 58099.00 | 125.33 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 53500.00 | 121.22 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 53105.00 | 98.41 MB |
| node (12.3) | [hapi](http://hapijs.com) (18.1) | 49638.33 | 129.04 MB |
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 49400.33 | 62.66 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 49079.33 | 256.11 MB |
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 46646.00 | 87.86 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 45331.33 | 43.16 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 42568.00 | 79.21 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 38906.67 | 95.93 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 34865.67 | 67.38 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 33367.67 | 19.25 MB |
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 32568.33 | 40.14 MB |
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 31441.33 | 57.38 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 25692.00 | 45.74 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 22395.33 | 12.93 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 21178.67 | 42.18 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 18654.00 | 140.99 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 17015.00 | 37.03 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 16500.33 | 42.84 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 13446.33 | 39.03 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 13395.33 | 39.48 MB |
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 8495.33 | 21.88 MB |
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 5482.33 | 13.47 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 4023.67 | 12.31 MB |
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

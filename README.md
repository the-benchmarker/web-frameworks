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
Last update: 2018-11-05
```
OS: Linux (version: 4.18.16-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: symfony (php)


:three: laravel (php)


:four: iron (rust)


:five: rack-routing (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 0.09 ms | 0.09 ms | 0.12 ms | 0.19 ms | 11.59 ms | 91.33 | 
| php (7.2) | [symfony](http://symfony.com) (4.1) | 161.88 ms | 0.43 ms | 291.50 ms | 3710.90 ms | 6863.78 ms | 605838.67 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 159.81 ms | 0.43 ms | 343.00 ms | 3275.83 ms | 7070.83 ms | 568928.33 | 
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 0.49 ms | 0.48 ms | 0.85 ms | 1.27 ms | 14.20 ms | 299.33 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 5.40 ms | 0.97 ms | 16.13 ms | 47.11 ms | 141.49 ms | 9743.00 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 8.58 ms | 1.25 ms | 26.40 ms | 73.39 ms | 203.58 ms | 15399.00 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 4.59 ms | 1.41 ms | 13.06 ms | 36.11 ms | 112.28 ms | 7558.33 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 200.34 ms | 1.91 ms | 334.65 ms | 4553.23 ms | 7963.29 ms | 769625.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 55.43 ms | 2.15 ms | 4.51 ms | 1905.44 ms | 4670.48 ms | 351257.33 | 
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 3.53 ms | 2.58 ms | 7.50 ms | 15.88 ms | 34.87 ms | 3415.00 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 9.89 ms | 3.36 ms | 28.45 ms | 68.78 ms | 167.26 ms | 14815.67 | 
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 4.19 ms | 3.55 ms | 8.19 ms | 17.21 ms | 38.40 ms | 3526.33 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 8.25 ms | 3.56 ms | 22.76 ms | 52.78 ms | 137.76 ms | 11482.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.63 ms | 3.63 ms | 9.46 ms | 18.17 ms | 41.59 ms | 3857.00 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 153.59 ms | 4.49 ms | 288.77 ms | 3355.07 ms | 5804.27 ms | 558220.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.21 ms | 4.70 ms | 5.82 ms | 9.95 ms | 38.78 ms | 1938.00 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.02 ms | 4.84 ms | 7.55 ms | 16.48 ms | 205.73 ms | 3592.67 | 
| crystal (0.26.1) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 5.73 ms | 5.13 ms | 10.11 ms | 18.65 ms | 39.09 ms | 3523.33 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 7.55 ms | 5.34 ms | 13.95 ms | 60.12 ms | 129.44 ms | 10102.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.67 ms | 5.44 ms | 9.17 ms | 15.75 ms | 72.91 ms | 3111.33 | 
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 6.51 ms | 5.47 ms | 11.95 ms | 20.60 ms | 196.57 ms | 4898.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 54.23 ms | 5.50 ms | 10.61 ms | 2206.61 ms | 3675.82 ms | 333851.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 9.23 ms | 7.65 ms | 13.31 ms | 37.21 ms | 383.11 ms | 15434.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 8.36 ms | 7.89 ms | 11.34 ms | 22.32 ms | 220.54 ms | 5727.67 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 8.97 ms | 8.10 ms | 13.66 ms | 26.93 ms | 175.21 ms | 5630.00 | 
| go (1.11) | [iris](http://iris-go.com) (11.0) | 9.20 ms | 8.52 ms | 13.82 ms | 27.40 ms | 167.30 ms | 5395.67 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 27.75 ms | 8.73 ms | 72.26 ms | 288.39 ms | 525.16 ms | 60356.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 9.91 ms | 8.99 ms | 15.30 ms | 30.11 ms | 245.37 ms | 7095.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 213.18 ms | 9.19 ms | 177.60 ms | 4475.36 ms | 7890.33 ms | 808230.00 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 10.62 ms | 9.30 ms | 16.30 ms | 34.41 ms | 359.78 ms | 11207.67 | 
| go (1.11) | [beego](http://beego.me) (1.10) | 10.35 ms | 9.36 ms | 15.89 ms | 31.55 ms | 179.38 ms | 5923.00 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 10.41 ms | 9.38 ms | 16.14 ms | 32.47 ms | 133.25 ms | 6090.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 13.80 ms | 9.45 ms | 28.60 ms | 54.87 ms | 303.33 ms | 13398.67 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 36.39 ms | 9.58 ms | 115.63 ms | 283.56 ms | 815.87 ms | 62108.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 11.30 ms | 9.84 ms | 17.54 ms | 35.82 ms | 251.43 ms | 8223.33 | 
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 15.47 ms | 10.87 ms | 21.59 ms | 75.72 ms | 708.78 ms | 30449.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 14.26 ms | 10.90 ms | 26.74 ms | 50.29 ms | 217.87 ms | 10143.00 | 
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 16.36 ms | 11.49 ms | 22.89 ms | 96.26 ms | 735.27 ms | 32525.00 | 
| python (3.7) | [starlette](http://starlette.io) (0.6) | 15.05 ms | 13.18 ms | 26.09 ms | 41.03 ms | 76.02 ms | 8232.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 14.35 ms | 14.32 ms | 17.13 ms | 21.31 ms | 155.95 ms | 3133.33 | 
| node (10.12) | [fastify](http://fastify.io) (1.13) | 22.51 ms | 15.10 ms | 28.73 ms | 224.87 ms | 874.95 ms | 45838.67 | 
| node (10.12) | [foxify](http://foxify.js.org) (0.10) | 25.50 ms | 16.30 ms | 33.07 ms | 279.90 ms | 1120.05 ms | 58783.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 22.51 ms | 16.37 ms | 28.09 ms | 183.67 ms | 1223.74 ms | 53625.67 | 
| node (10.12) | [restify](http://restify.com) (7.2) | 23.57 ms | 19.31 ms | 36.46 ms | 71.53 ms | 549.62 ms | 21790.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 23.76 ms | 20.40 ms | 39.90 ms | 73.17 ms | 763.02 ms | 33751.33 | 
| node (10.12) | [koa](http://koajs.com) (2.6) | 29.68 ms | 20.71 ms | 39.50 ms | 271.87 ms | 1165.84 ms | 59872.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 25.50 ms | 21.29 ms | 45.17 ms | 66.02 ms | 130.72 ms | 14279.00 | 
| node (10.12) | [express](http://expressjs.com) (4.16) | 35.70 ms | 23.05 ms | 42.21 ms | 455.10 ms | 1405.95 ms | 82731.00 | 
| crystal (0.27.0) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 30.47 ms | 29.48 ms | 38.78 ms | 49.21 ms | 328.58 ms | 9428.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 31.61 ms | 29.73 ms | 44.23 ms | 60.30 ms | 192.27 ms | 8874.33 | 
| crystal (0.26.1) | [kemal](http://kemalcr.com) (0.24) | 33.56 ms | 31.41 ms | 43.75 ms | 52.93 ms | 185.59 ms | 7954.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 40.02 ms | 32.07 ms | 72.32 ms | 114.33 ms | 321.38 ms | 22573.67 | 
| crystal (0.26.1) | [raze](http://razecr.com) (0.3) | 33.29 ms | 33.73 ms | 41.35 ms | 49.90 ms | 182.81 ms | 7249.33 | 
| node (10.12) | [hapi](http://hapijs.com) (17.6) | 60.81 ms | 33.86 ms | 59.21 ms | 935.11 ms | 1992.33 ms | 151052.00 | 
| crystal (0.26.1) | [amber](http://amberframework.org) (0.9) | 37.84 ms | 35.64 ms | 47.99 ms | 81.77 ms | 502.90 ms | 23610.67 | 
| crystal (0.26.1) | [lucky](http://luckyframework.org) (0.11) | 40.00 ms | 39.70 ms | 47.98 ms | 59.15 ms | 406.59 ms | 13029.00 | 
| crystal (0.26.1) | [orion](http://github.com/obsidian/orion) (1.6) | 43.11 ms | 43.36 ms | 52.71 ms | 61.33 ms | 197.44 ms | 9310.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 58.25 ms | 49.81 ms | 102.98 ms | 174.30 ms | 352.60 ms | 35039.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 65.70 ms | 59.13 ms | 88.92 ms | 177.10 ms | 894.61 ms | 41965.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 66.15 ms | 65.48 ms | 120.72 ms | 160.72 ms | 227.71 ms | 38454.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 95.42 ms | 94.46 ms | 128.38 ms | 157.32 ms | 483.16 ms | 27215.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (vibora) (python)


:three: (actix-web) (rust)


:four: (evhtp) (cpp)


:five: (jester) (nim)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 295225.33 | 353.34 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 234033.33 | 265.83 MB |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 231411.00 | 263.19 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 211035.33 | 204.83 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 190705.33 | 383.21 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 186218.67 | 299.54 MB |
| crystal (0.26.1) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 170051.00 | 181.49 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 160551.33 | 92.76 MB |
| java (8) | [act](http://actframework.org) (1.8) | 149102.33 | 254.83 MB |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 146102.00 | 297.74 MB |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 121471.67 | 154.13 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 113606.33 | 185.09 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 110279.33 | 146.69 MB |
| go (1.11) | [iris](http://iris-go.com) (11.0) | 106522.67 | 142.40 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 100238.33 | 175.98 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 98308.33 | 172.62 MB |
| go (1.11) | [beego](http://beego.me) (1.10) | 96217.33 | 129.34 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 95304.33 | 167.34 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 95067.33 | 127.04 MB |
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 94686.67 | 188.45 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 88261.33 | 117.92 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 82759.67 | 212.81 MB |
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 78392.33 | 117.48 MB |
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 74296.00 | 111.34 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 73691.00 | 181.52 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 73576.00 | 69.15 MB |
| python (3.7) | [starlette](http://starlette.io) (0.6) | 67202.33 | 135.26 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 65619.67 | 326.47 MB |
| node (10.12) | [fastify](http://fastify.io) (1.13) | 61292.33 | 144.83 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 54394.33 | 91.15 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 53857.00 | 115.59 MB |
| node (10.12) | [foxify](http://foxify.js.org) (0.10) | 53012.67 | 111.16 MB |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 52099.67 | 258.71 MB |
| c (99) | [kore](http://kore.io) (3.1) | 49331.00 | 133.68 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 48330.67 | 84.57 MB |
| node (10.12) | [restify](http://restify.com) (7.2) | 45029.67 | 78.86 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 43665.67 | 227.40 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 42809.33 | 67.97 MB |
| node (10.12) | [koa](http://koajs.com) (2.6) | 42501.00 | 89.77 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 41753.00 | 217.09 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 40539.67 | 91.92 MB |
| node (10.12) | [express](http://expressjs.com) (4.16) | 38682.67 | 94.54 MB |
| crystal (0.27.0) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 32710.00 | 30.68 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 31763.00 | 58.96 MB |
| crystal (0.26.1) | [raze](http://razecr.com) (0.3) | 29983.33 | 28.10 MB |
| crystal (0.26.1) | [kemal](http://kemalcr.com) (0.24) | 29431.67 | 47.96 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 28053.33 | 26.74 MB |
| node (10.12) | [hapi](http://hapijs.com) (17.6) | 27485.00 | 71.04 MB |
| crystal (0.26.1) | [amber](http://amberframework.org) (0.9) | 26414.33 | 38.27 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 25422.00 | 62.67 MB |
| crystal (0.26.1) | [lucky](http://luckyframework.org) (0.11) | 24991.67 | 30.61 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 23854.00 | 13.76 MB |
| crystal (0.26.1) | [orion](http://github.com/obsidian/orion) (1.6) | 23617.00 | 38.50 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 17706.67 | 31.55 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 15498.67 | 8.93 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 15303.67 | 44.42 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 15210.67 | 30.33 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 14949.33 | 113.05 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 12912.00 | 33.49 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 10269.00 | 27.60 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 3522.33 | 10.76 MB |
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

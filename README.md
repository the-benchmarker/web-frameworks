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
Last update: 2018-11-02
```
OS: Linux (version: 4.18.16-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: laravel (php)


:four: slim (php)


:five: rack-routing (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 0.09 ms | 0.08 ms | 0.13 ms | 0.22 ms | 2.26 ms | 42.00 | 
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 0.50 ms | 0.45 ms | 0.86 ms | 1.69 ms | 40.21 ms | 467.67 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 162.30 ms | 0.52 ms | 362.17 ms | 3288.54 ms | 7223.03 ms | 566341.33 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 230.25 ms | 0.55 ms | 323.00 ms | 5111.23 ms | 7956.16 ms | 862136.33 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 6.20 ms | 1.06 ms | 18.66 ms | 54.25 ms | 165.19 ms | 11251.33 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 5.08 ms | 1.41 ms | 14.50 ms | 41.74 ms | 143.67 ms | 8674.00 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 9.73 ms | 1.58 ms | 29.79 ms | 81.00 ms | 237.74 ms | 17088.67 | 
| php (7.2) | [symfony](http://symfony.com) (4.1) | 170.87 ms | 1.86 ms | 335.32 ms | 3457.90 ms | 5953.30 ms | 589890.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 120.51 ms | 2.05 ms | 77.16 ms | 3555.06 ms | 6594.58 ms | 594567.33 | 
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 3.92 ms | 3.28 ms | 7.35 ms | 15.47 ms | 55.57 ms | 3254.33 | 
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 4.11 ms | 3.41 ms | 8.41 ms | 17.41 ms | 66.94 ms | 3899.33 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 11.24 ms | 3.42 ms | 32.86 ms | 81.21 ms | 218.46 ms | 17420.67 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 9.27 ms | 3.93 ms | 25.59 ms | 60.70 ms | 163.84 ms | 13078.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 5.43 ms | 4.33 ms | 11.09 ms | 21.35 ms | 77.28 ms | 4647.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.75 ms | 4.97 ms | 6.34 ms | 15.42 ms | 75.23 ms | 2961.33 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.29 ms | 5.00 ms | 8.05 ms | 18.31 ms | 83.86 ms | 3189.33 | 
| crystal (0.26.1) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 6.38 ms | 5.45 ms | 11.06 ms | 21.32 ms | 54.35 ms | 4008.67 | 
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 6.69 ms | 5.51 ms | 12.00 ms | 21.30 ms | 234.01 ms | 5717.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 6.13 ms | 5.77 ms | 9.55 ms | 18.51 ms | 92.69 ms | 3631.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 10.66 ms | 6.40 ms | 11.48 ms | 172.29 ms | 2475.36 ms | 55508.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 8.63 ms | 7.87 ms | 13.70 ms | 27.15 ms | 182.77 ms | 5842.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 8.62 ms | 8.01 ms | 11.68 ms | 23.77 ms | 360.42 ms | 8394.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 9.11 ms | 8.24 ms | 13.69 ms | 27.44 ms | 290.73 ms | 6770.33 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 9.71 ms | 8.91 ms | 14.77 ms | 29.18 ms | 236.11 ms | 6615.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 10.12 ms | 9.07 ms | 15.62 ms | 32.50 ms | 198.47 ms | 7328.00 | 
| go (1.11) | [beego](http://beego.me) (1.10) | 10.42 ms | 9.31 ms | 16.15 ms | 32.77 ms | 130.23 ms | 6273.00 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 29.59 ms | 9.51 ms | 71.74 ms | 309.34 ms | 566.61 ms | 63671.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 10.85 ms | 9.52 ms | 16.88 ms | 36.44 ms | 208.47 ms | 8652.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 10.77 ms | 9.57 ms | 16.79 ms | 34.01 ms | 187.57 ms | 6760.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 221.74 ms | 10.03 ms | 140.70 ms | 4563.70 ms | 7160.98 ms | 811975.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 13.37 ms | 10.89 ms | 23.13 ms | 40.80 ms | 285.96 ms | 9752.00 | 
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 16.46 ms | 11.24 ms | 21.00 ms | 150.04 ms | 779.88 ms | 37561.00 | 
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 16.54 ms | 11.66 ms | 20.77 ms | 149.62 ms | 721.50 ms | 35707.33 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 44.18 ms | 11.67 ms | 138.64 ms | 343.87 ms | 879.36 ms | 74765.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 16.40 ms | 12.47 ms | 33.00 ms | 60.72 ms | 222.06 ms | 12662.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 13.62 ms | 13.71 ms | 16.08 ms | 18.52 ms | 128.03 ms | 2206.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 24.29 ms | 15.49 ms | 27.38 ms | 313.20 ms | 1456.54 ms | 68342.33 | 
| python (3.7) | [starlette](http://starlette.io) (0.6) | 17.48 ms | 16.40 ms | 27.88 ms | 44.62 ms | 102.90 ms | 8519.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 19.32 ms | 17.87 ms | 34.79 ms | 56.61 ms | 292.28 ms | 12871.33 | 
| node (10.12) | [fastify](http://fastify.io) (1.13) | 28.00 ms | 18.71 ms | 34.77 ms | 283.26 ms | 975.53 ms | 55106.00 | 
| node (10.12) | [foxify](http://foxify.js.org) (0.10) | 30.46 ms | 20.13 ms | 41.13 ms | 314.28 ms | 1135.94 ms | 62825.33 | 
| node (10.12) | [koa](http://koajs.com) (2.6) | 26.09 ms | 20.44 ms | 37.95 ms | 133.54 ms | 795.89 ms | 35888.67 | 
| node (10.12) | [restify](http://restify.com) (7.2) | 24.59 ms | 20.52 ms | 37.81 ms | 70.60 ms | 549.46 ms | 22243.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 37.33 ms | 26.87 ms | 75.68 ms | 103.87 ms | 268.44 ms | 23086.00 | 
| node (10.12) | [express](http://expressjs.com) (4.16) | 45.33 ms | 27.39 ms | 54.90 ms | 619.56 ms | 1545.77 ms | 101811.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 31.01 ms | 28.61 ms | 45.07 ms | 61.26 ms | 247.58 ms | 10737.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 32.72 ms | 28.83 ms | 56.61 ms | 87.78 ms | 172.96 ms | 17920.67 | 
| node (10.12) | [hapi](http://hapijs.com) (17.6) | 55.42 ms | 30.85 ms | 55.26 ms | 816.26 ms | 1755.92 ms | 133489.00 | 
| crystal (0.26.1) | [raze](http://razecr.com) (0.3) | 34.26 ms | 32.16 ms | 43.76 ms | 51.00 ms | 322.04 ms | 10857.33 | 
| crystal (0.26.1) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 35.78 ms | 35.05 ms | 43.86 ms | 51.39 ms | 183.87 ms | 6580.67 | 
| crystal (0.26.1) | [amber](http://amberframework.org) (0.9) | 39.22 ms | 37.84 ms | 51.55 ms | 59.82 ms | 410.22 ms | 14807.67 | 
| crystal (0.26.1) | [orion](http://github.com/obsidian/orion) (1.6) | 42.75 ms | 39.98 ms | 58.71 ms | 65.98 ms | 318.98 ms | 12624.67 | 
| crystal (0.26.1) | [kemal](http://kemalcr.com) (0.24) | 44.98 ms | 44.45 ms | 56.29 ms | 78.40 ms | 423.15 ms | 15968.67 | 
| crystal (0.26.1) | [lucky](http://luckyframework.org) (0.11) | 44.94 ms | 45.32 ms | 53.08 ms | 60.21 ms | 126.85 ms | 7429.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 54.44 ms | 48.57 ms | 94.13 ms | 163.70 ms | 293.74 ms | 32098.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 81.29 ms | 60.16 ms | 159.64 ms | 243.80 ms | 548.31 ms | 51238.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 69.85 ms | 68.11 ms | 105.80 ms | 144.00 ms | 207.50 ms | 27632.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 107.09 ms | 98.46 ms | 152.00 ms | 186.55 ms | 832.35 ms | 43355.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (actix-web) (rust)


:three: (vibora) (python)


:four: (evhtp) (cpp)


:five: (fasthttprouter) (go)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 249493.67 | 298.60 MB |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 240406.67 | 273.18 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 202570.33 | 230.05 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 194931.33 | 189.20 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 177148.67 | 285.99 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 174636.67 | 351.09 MB |
| crystal (0.26.1) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 154170.67 | 165.80 MB |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 146066.00 | 298.51 MB |
| java (8) | [act](http://actframework.org) (1.8) | 138729.33 | 237.34 MB |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 113595.67 | 144.50 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 112103.33 | 182.56 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 108115.00 | 144.91 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 101495.33 | 135.96 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 99586.00 | 174.88 MB |
| go (1.11) | [beego](http://beego.me) (1.10) | 96170.00 | 129.16 MB |
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 96109.00 | 190.61 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 92858.33 | 163.11 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 92682.00 | 123.58 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 86949.67 | 152.63 MB |
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 78054.67 | 117.01 MB |
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 76019.67 | 113.92 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 75983.00 | 195.40 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 71987.67 | 67.65 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 66110.00 | 162.96 MB |
| python (3.7) | [starlette](http://starlette.io) (0.6) | 57252.33 | 115.25 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 56276.33 | 94.56 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 55660.33 | 97.46 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 55588.33 | 119.12 MB |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 52262.00 | 259.56 MB |
| node (10.12) | [fastify](http://fastify.io) (1.13) | 50922.67 | 119.11 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 48886.33 | 242.30 MB |
| c (99) | [kore](http://kore.io) (3.1) | 47622.33 | 129.10 MB |
| node (10.12) | [koa](http://koajs.com) (2.6) | 43173.67 | 91.13 MB |
| node (10.12) | [restify](http://restify.com) (7.2) | 42876.00 | 75.13 MB |
| node (10.12) | [foxify](http://foxify.js.org) (0.10) | 42733.33 | 89.74 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 38971.33 | 203.05 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 34677.00 | 57.09 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 32283.67 | 59.87 MB |
| node (10.12) | [express](http://expressjs.com) (4.16) | 31754.67 | 77.61 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 31191.00 | 70.71 MB |
| node (10.12) | [hapi](http://hapijs.com) (17.6) | 29536.67 | 76.26 MB |
| crystal (0.26.1) | [raze](http://razecr.com) (0.3) | 28893.67 | 27.09 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 28108.67 | 69.29 MB |
| crystal (0.26.1) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 27642.00 | 25.93 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 25210.00 | 24.04 MB |
| crystal (0.26.1) | [amber](http://amberframework.org) (0.9) | 25184.00 | 36.53 MB |
| crystal (0.26.1) | [orion](http://github.com/obsidian/orion) (1.6) | 22944.00 | 37.39 MB |
| crystal (0.26.1) | [lucky](http://luckyframework.org) (0.11) | 22190.67 | 27.26 MB |
| crystal (0.26.1) | [kemal](http://kemalcr.com) (0.24) | 22025.67 | 35.93 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 20806.00 | 12.01 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 18873.00 | 33.64 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 14232.00 | 28.37 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 13864.33 | 7.99 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 13225.00 | 100.01 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 13146.33 | 38.17 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 11376.00 | 29.51 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 9142.33 | 24.53 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 2904.67 | 8.85 MB |
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

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
Last update: 2018-12-05
```
OS: Linux (version: 4.19.5-200.fc28.x86_64, arch: x86_64)
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
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 0.07 ms | 0.07 ms | 0.10 ms | 0.14 ms | 0.76 ms | 28.33 | 
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 0.25 ms | 0.23 ms | 0.43 ms | 0.77 ms | 9.49 ms | 173.00 | 
| php (7.2) | [symfony](http://symfony.com) (4.2) | 110.94 ms | 0.32 ms | 214.49 ms | 2454.82 ms | 6868.23 ms | 460513.33 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 92.30 ms | 0.33 ms | 197.42 ms | 1882.98 ms | 6778.32 ms | 371787.67 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 106.72 ms | 0.33 ms | 268.58 ms | 1873.96 ms | 6879.80 ms | 398892.67 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 180.24 ms | 0.35 ms | 331.04 ms | 3805.70 ms | 6990.58 ms | 663517.33 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.86 ms | 0.68 ms | 11.09 ms | 37.26 ms | 128.85 ms | 7523.00 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 7.16 ms | 0.83 ms | 22.32 ms | 68.55 ms | 203.76 ms | 14148.33 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 3.14 ms | 0.91 ms | 8.80 ms | 26.61 ms | 97.39 ms | 5476.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 141.91 ms | 1.27 ms | 74.50 ms | 3710.42 ms | 6593.05 ms | 662952.33 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 2.39 ms | 1.95 ms | 4.11 ms | 10.33 ms | 87.88 ms | 2096.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.98 ms | 2.12 ms | 6.28 ms | 14.24 ms | 32.73 ms | 2998.33 | 
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 2.78 ms | 2.15 ms | 5.72 ms | 12.68 ms | 92.31 ms | 2836.33 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 8.09 ms | 2.27 ms | 23.71 ms | 63.03 ms | 192.29 ms | 13299.00 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 6.16 ms | 2.46 ms | 17.03 ms | 43.10 ms | 124.18 ms | 9170.33 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.51 ms | 2.93 ms | 5.72 ms | 11.09 ms | 149.93 ms | 2960.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.69 ms | 2.99 ms | 7.61 ms | 14.12 ms | 35.56 ms | 3102.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.99 ms | 3.01 ms | 4.99 ms | 7.20 ms | 97.69 ms | 2534.00 | 
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 3.15 ms | 3.07 ms | 4.46 ms | 7.75 ms | 31.57 ms | 1415.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.98 ms | 3.31 ms | 6.86 ms | 12.13 ms | 46.86 ms | 2351.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 3.95 ms | 3.42 ms | 7.51 ms | 14.13 ms | 29.77 ms | 2875.67 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 5.66 ms | 4.14 ms | 9.92 ms | 38.95 ms | 117.63 ms | 7824.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 4.78 ms | 4.73 ms | 7.99 ms | 9.67 ms | 18.94 ms | 2412.67 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 6.12 ms | 4.79 ms | 9.18 ms | 28.36 ms | 77.88 ms | 4772.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 5.45 ms | 4.82 ms | 8.05 ms | 15.14 ms | 346.26 ms | 6488.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.88 ms | 4.87 ms | 9.79 ms | 24.37 ms | 90.99 ms | 5123.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 5.96 ms | 4.94 ms | 9.91 ms | 19.99 ms | 169.80 ms | 5104.67 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 5.92 ms | 4.96 ms | 9.88 ms | 19.29 ms | 98.99 ms | 3596.67 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 6.48 ms | 5.21 ms | 10.87 ms | 22.06 ms | 216.10 ms | 5459.33 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 6.44 ms | 5.24 ms | 10.97 ms | 22.07 ms | 160.63 ms | 4415.33 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 6.43 ms | 5.29 ms | 10.69 ms | 20.87 ms | 217.54 ms | 5348.67 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 8.07 ms | 5.30 ms | 15.60 ms | 28.34 ms | 150.41 ms | 6079.67 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 6.78 ms | 5.50 ms | 11.37 ms | 22.52 ms | 120.30 ms | 5103.67 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 7.36 ms | 5.92 ms | 12.07 ms | 25.00 ms | 286.11 ms | 7475.33 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 8.41 ms | 6.24 ms | 13.85 ms | 34.70 ms | 340.59 ms | 11827.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 152.77 ms | 6.40 ms | 329.69 ms | 3389.03 ms | 6850.45 ms | 564002.00 | 
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 9.69 ms | 6.92 ms | 17.25 ms | 44.48 ms | 406.10 ms | 14960.00 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 12.00 ms | 8.11 ms | 21.00 ms | 55.27 ms | 516.70 ms | 20365.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 8.72 ms | 8.11 ms | 14.08 ms | 23.75 ms | 272.17 ms | 7077.67 | 
| go (1.11) | [gf](http://gfer.me) (1.2) | 9.30 ms | 8.45 ms | 13.62 ms | 30.63 ms | 175.66 ms | 6265.00 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 37.46 ms | 8.79 ms | 120.42 ms | 301.03 ms | 742.92 ms | 65318.00 | 
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 14.09 ms | 10.20 ms | 23.87 ms | 54.76 ms | 497.19 ms | 19438.00 | 
| node (11.1) | [fastify](http://fastify.io) (1.13) | 14.25 ms | 10.55 ms | 25.52 ms | 54.02 ms | 439.65 ms | 16320.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 12.42 ms | 10.68 ms | 22.26 ms | 42.11 ms | 502.83 ms | 14786.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 15.47 ms | 10.94 ms | 19.49 ms | 80.40 ms | 989.72 ms | 39823.00 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 15.51 ms | 11.90 ms | 25.24 ms | 55.97 ms | 527.23 ms | 20356.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 11.93 ms | 12.06 ms | 13.48 ms | 15.14 ms | 137.93 ms | 2089.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 18.95 ms | 14.54 ms | 39.26 ms | 61.87 ms | 121.04 ms | 13789.00 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 19.02 ms | 15.54 ms | 32.52 ms | 58.64 ms | 401.40 ms | 15282.67 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 20.82 ms | 15.61 ms | 32.70 ms | 90.56 ms | 709.34 ms | 30486.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 23.77 ms | 21.16 ms | 36.10 ms | 50.88 ms | 186.33 ms | 9106.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 27.23 ms | 21.79 ms | 44.06 ms | 88.67 ms | 546.23 ms | 25572.33 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 24.42 ms | 23.94 ms | 32.85 ms | 43.21 ms | 240.14 ms | 8632.00 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 27.40 ms | 24.04 ms | 41.32 ms | 63.72 ms | 313.00 ms | 12726.00 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 26.50 ms | 26.50 ms | 35.19 ms | 44.03 ms | 372.70 ms | 11312.67 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 30.04 ms | 26.78 ms | 41.72 ms | 70.51 ms | 304.95 ms | 12730.67 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 44.45 ms | 26.94 ms | 49.59 ms | 575.65 ms | 1551.56 ms | 102689.67 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 28.45 ms | 30.18 ms | 37.32 ms | 47.68 ms | 191.83 ms | 8351.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 40.55 ms | 33.38 ms | 74.39 ms | 141.21 ms | 333.95 ms | 29325.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 38.58 ms | 36.69 ms | 45.32 ms | 73.99 ms | 256.05 ms | 11017.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 49.08 ms | 39.20 ms | 90.29 ms | 121.54 ms | 367.73 ms | 25802.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 54.05 ms | 47.99 ms | 95.63 ms | 153.58 ms | 272.11 ms | 30731.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 72.03 ms | 72.05 ms | 89.55 ms | 117.30 ms | 709.46 ms | 24675.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (evhtp) (cpp)


:five: (vibora) (python)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 419043.67 | 242.50 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 355352.00 | 425.29 MB |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 354860.00 | 403.28 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 294413.67 | 285.72 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 286029.00 | 324.85 MB |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 274638.00 | 562.11 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 264620.67 | 426.05 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 264603.33 | 531.68 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 252212.33 | 270.23 MB |
| java (8) | [act](http://actframework.org) (1.8) | 252157.33 | 492.46 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 207593.67 | 119.97 MB |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 191667.33 | 242.48 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 177291.00 | 288.80 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 171516.33 | 301.05 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 167169.67 | 224.16 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 165718.67 | 221.64 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 154502.33 | 208.03 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 153996.00 | 270.29 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 152209.67 | 204.23 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 145841.33 | 255.88 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 136384.67 | 182.02 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 132188.67 | 198.10 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 128865.67 | 331.28 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 121390.00 | 181.77 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 115218.33 | 283.76 MB |
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 110535.67 | 219.52 MB |
| go (1.11) | [gf](http://gfer.me) (1.2) | 109538.67 | 166.07 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 101268.67 | 151.62 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 90057.00 | 157.59 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 84769.67 | 79.51 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 81326.67 | 195.47 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 81110.67 | 170.36 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 78788.33 | 132.52 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 71803.00 | 151.55 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 66727.67 | 330.48 MB |
| c (99) | [kore](http://kore.io) (3.1) | 66557.67 | 180.43 MB |
| php (7.2) | [symfony](http://symfony.com) (4.2) | 66178.67 | 328.05 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 59582.33 | 135.17 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 58681.33 | 304.66 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 56955.33 | 92.74 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 55524.67 | 97.10 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 55255.67 | 134.84 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 51647.00 | 268.97 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 50454.00 | 110.13 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 41931.67 | 77.78 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 41150.00 | 39.19 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 40799.33 | 38.26 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 38321.33 | 94.43 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 37816.00 | 35.44 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 36349.33 | 44.71 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 35203.67 | 57.38 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 33717.33 | 19.44 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 33513.67 | 61.16 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 33403.33 | 86.39 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 26801.33 | 43.65 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 26324.33 | 46.87 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 21332.33 | 61.84 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 20850.67 | 12.02 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 19110.67 | 38.06 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 18152.00 | 137.17 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 15852.33 | 41.11 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 13572.33 | 36.24 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 3429.67 | 10.46 MB |
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

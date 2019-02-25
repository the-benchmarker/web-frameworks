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
Last update: 2019-02-25
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: zend-expressive (php)


:five: laravel (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.06 ms | 0.06 ms | 0.07 ms | 0.10 ms | 0.92 ms | 18.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 3.12 ms | 0.17 ms | 11.50 ms | 27.52 ms | 70.95 ms | 6163.67 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.73 ms | 0.18 ms | 14.23 ms | 32.80 ms | 83.82 ms | 7457.33 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 105.44 ms | 0.28 ms | 183.86 ms | 2427.54 ms | 6751.01 ms | 425481.33 | 
| php (7.3) | [laravel](http://laravel.com) (5.7) | 75.89 ms | 0.28 ms | 227.24 ms | 1078.96 ms | 5722.28 ms | 254630.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.73 ms | 0.30 ms | 20.27 ms | 43.80 ms | 99.52 ms | 10179.33 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 145.16 ms | 0.30 ms | 223.65 ms | 3407.51 ms | 6772.94 ms | 565051.33 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 6.26 ms | 0.32 ms | 23.03 ms | 47.88 ms | 122.24 ms | 11430.67 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.33 ms | 0.34 ms | 0.52 ms | 0.76 ms | 14.04 ms | 200.33 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 7.30 ms | 0.43 ms | 24.36 ms | 48.89 ms | 109.86 ms | 11801.00 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 92.65 ms | 1.09 ms | 218.65 ms | 1806.95 ms | 5752.56 ms | 351361.00 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 79.42 ms | 1.25 ms | 170.84 ms | 1734.01 ms | 5398.10 ms | 307019.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 63.95 ms | 1.31 ms | 150.79 ms | 1305.45 ms | 5687.08 ms | 264045.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 121.79 ms | 1.38 ms | 24.57 ms | 2884.13 ms | 6578.53 ms | 551734.33 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.97 ms | 2.09 ms | 6.37 ms | 14.47 ms | 30.96 ms | 3027.00 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 2.85 ms | 2.13 ms | 5.87 ms | 13.59 ms | 33.66 ms | 2715.67 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 29.90 ms | 2.18 ms | 101.69 ms | 272.42 ms | 730.09 ms | 58513.33 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 2.82 ms | 2.24 ms | 5.13 ms | 10.09 ms | 66.56 ms | 2210.00 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.22 ms | 2.54 ms | 5.61 ms | 10.06 ms | 100.69 ms | 2453.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.60 ms | 2.91 ms | 7.51 ms | 14.52 ms | 30.80 ms | 3128.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.92 ms | 2.94 ms | 5.04 ms | 6.86 ms | 17.97 ms | 1606.67 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 3.47 ms | 3.08 ms | 6.20 ms | 12.08 ms | 27.82 ms | 2429.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.81 ms | 3.21 ms | 6.67 ms | 11.46 ms | 38.66 ms | 2247.67 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 4.11 ms | 3.26 ms | 7.71 ms | 18.09 ms | 100.69 ms | 4536.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 3.84 ms | 3.28 ms | 7.30 ms | 13.31 ms | 29.17 ms | 2749.00 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 5.53 ms | 3.52 ms | 8.37 ms | 72.90 ms | 131.44 ms | 10775.00 | 
| node (11.10) | [restana](http://github.com/jkyberneees/ana) (2.9) | 4.49 ms | 4.42 ms | 6.77 ms | 13.81 ms | 156.75 ms | 3803.33 | 
| node (11.10) | [polka](http://github.com/lukeed/polka) (0.5) | 5.11 ms | 4.54 ms | 8.73 ms | 16.04 ms | 216.01 ms | 5779.67 | 
| node (11.10) | [rayo](http://rayo.js.org) (1.2) | 5.36 ms | 4.58 ms | 8.70 ms | 15.92 ms | 252.34 ms | 7511.67 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 5.34 ms | 4.60 ms | 9.12 ms | 18.28 ms | 151.95 ms | 3676.00 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 5.42 ms | 4.63 ms | 9.20 ms | 18.46 ms | 154.97 ms | 3983.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 5.17 ms | 4.67 ms | 7.67 ms | 14.32 ms | 209.00 ms | 3710.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 4.73 ms | 4.70 ms | 7.95 ms | 9.61 ms | 28.52 ms | 2438.33 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 5.69 ms | 4.75 ms | 9.73 ms | 18.56 ms | 102.78 ms | 3662.67 | 
| node (11.10) | [foxify](http://foxify.js.org) (0.10) | 6.20 ms | 4.89 ms | 9.65 ms | 17.27 ms | 197.42 ms | 5295.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 6.30 ms | 5.01 ms | 10.55 ms | 21.97 ms | 230.68 ms | 6265.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (4.0) | 6.23 ms | 5.02 ms | 10.56 ms | 21.51 ms | 164.71 ms | 4944.00 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 6.28 ms | 5.07 ms | 10.57 ms | 21.58 ms | 266.13 ms | 5410.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.04 ms | 5.13 ms | 9.87 ms | 21.30 ms | 88.50 ms | 4565.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 7.04 ms | 5.18 ms | 11.57 ms | 20.50 ms | 211.43 ms | 5657.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 6.38 ms | 5.26 ms | 10.76 ms | 20.74 ms | 106.91 ms | 3941.33 | 
| node (11.10) | [koa](http://koajs.com) (2.7) | 7.10 ms | 5.39 ms | 9.89 ms | 19.46 ms | 293.68 ms | 8967.00 | 
| node (11.10) | [fastify](http://fastify.io) (1.14) | 6.57 ms | 5.87 ms | 10.27 ms | 19.97 ms | 225.26 ms | 6388.00 | 
| node (11.10) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 8.46 ms | 6.04 ms | 13.88 ms | 33.12 ms | 401.84 ms | 14802.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 8.59 ms | 6.60 ms | 15.76 ms | 28.55 ms | 213.68 ms | 7949.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 187.93 ms | 6.83 ms | 29.66 ms | 4557.48 ms | 7920.58 ms | 782770.00 | 
| node (11.10) | [express](http://expressjs.com) (4.16) | 8.57 ms | 7.61 ms | 12.69 ms | 24.00 ms | 359.02 ms | 12507.00 | 
| node (11.10) | [restify](http://restify.com) (8.0) | 7.89 ms | 7.64 ms | 11.03 ms | 20.50 ms | 216.13 ms | 5858.00 | 
| go (1.11) | [gf](http://goframe.org) (1.5) | 8.84 ms | 8.01 ms | 13.30 ms | 28.82 ms | 117.54 ms | 5550.67 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 12.58 ms | 8.83 ms | 25.78 ms | 44.51 ms | 219.96 ms | 10647.00 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 10.45 ms | 9.39 ms | 17.19 ms | 27.40 ms | 63.89 ms | 5234.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 11.40 ms | 9.95 ms | 20.89 ms | 40.17 ms | 283.02 ms | 9147.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 10.35 ms | 10.47 ms | 12.15 ms | 13.96 ms | 72.72 ms | 1608.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 17.04 ms | 10.68 ms | 19.73 ms | 179.89 ms | 1266.41 ms | 55686.67 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 14.14 ms | 12.97 ms | 24.14 ms | 33.37 ms | 59.84 ms | 7240.00 | 
| node (11.10) | [hapi](http://hapijs.com) (18.1) | 18.43 ms | 14.50 ms | 25.84 ms | 87.01 ms | 658.55 ms | 27463.00 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.5) | 16.02 ms | 14.75 ms | 26.70 ms | 43.95 ms | 115.14 ms | 8591.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 18.38 ms | 15.41 ms | 37.93 ms | 54.38 ms | 93.46 ms | 12253.67 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 25.41 ms | 19.23 ms | 30.44 ms | 195.32 ms | 933.89 ms | 45945.33 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 21.23 ms | 19.47 ms | 30.54 ms | 36.93 ms | 163.59 ms | 5529.00 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 22.86 ms | 21.11 ms | 33.87 ms | 39.78 ms | 311.09 ms | 8398.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 25.78 ms | 21.24 ms | 46.99 ms | 75.00 ms | 307.70 ms | 15921.00 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 26.11 ms | 22.05 ms | 44.97 ms | 73.87 ms | 393.10 ms | 15356.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 31.14 ms | 26.47 ms | 44.17 ms | 50.67 ms | 474.07 ms | 17353.00 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 29.93 ms | 28.81 ms | 39.06 ms | 43.44 ms | 307.60 ms | 10985.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 27.46 ms | 29.23 ms | 35.67 ms | 41.58 ms | 547.33 ms | 13607.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 37.24 ms | 29.84 ms | 69.66 ms | 136.30 ms | 215.32 ms | 26390.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 30.75 ms | 30.84 ms | 36.82 ms | 42.07 ms | 193.29 ms | 6907.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 46.70 ms | 36.88 ms | 82.32 ms | 147.50 ms | 784.87 ms | 40494.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 40.99 ms | 38.99 ms | 65.11 ms | 94.00 ms | 179.78 ms | 16575.33 | 
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.3) | 55.82 ms | 55.33 ms | 91.69 ms | 124.35 ms | 209.27 ms | 27922.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 66.36 ms | 63.27 ms | 90.69 ms | 104.49 ms | 355.84 ms | 19741.67 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 375954.00 | 217.47 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 364843.67 | 436.85 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 355712.67 | 404.19 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 300827.33 | 291.94 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 297714.67 | 338.05 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 288411.00 | 464.93 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 276033.00 | 259.51 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 269516.67 | 541.53 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 260342.67 | 277.03 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 256763.33 | 525.29 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 255425.67 | 147.77 MB |
| java (8) | [act](http://actframework.org) (1.8) | 243191.33 | 474.65 MB |
| node (11.10) | [restana](http://github.com/jkyberneees/ana) (2.9) | 210814.33 | 316.06 MB |
| node (11.10) | [polka](http://github.com/lukeed/polka) (0.5) | 194899.67 | 292.19 MB |
| node (11.10) | [rayo](http://rayo.js.org) (1.2) | 190811.67 | 286.13 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 184515.67 | 247.25 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 181472.00 | 295.71 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 181451.00 | 228.45 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 179847.67 | 240.63 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 171764.67 | 230.16 MB |
| node (11.10) | [fastify](http://fastify.io) (1.14) | 163773.00 | 421.96 MB |
| go (1.11) | [echo](http://echo.labstack.com) (4.0) | 160354.67 | 281.57 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 159726.67 | 215.06 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 159233.33 | 279.34 MB |
| node (11.10) | [foxify](http://foxify.js.org) (0.10) | 156285.33 | 328.33 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 154201.67 | 205.68 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 153943.67 | 305.77 MB |
| node (11.10) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 144334.67 | 216.40 MB |
| node (11.10) | [koa](http://koajs.com) (2.7) | 144293.67 | 304.82 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 140504.67 | 361.13 MB |
| node (11.10) | [restify](http://restify.com) (8.0) | 126257.00 | 221.73 MB |
| node (11.10) | [express](http://expressjs.com) (4.16) | 124357.00 | 304.66 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 120868.67 | 297.45 MB |
| go (1.11) | [gf](http://goframe.org) (1.5) | 114616.00 | 173.03 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 97091.00 | 169.91 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 96317.67 | 90.43 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 95932.00 | 206.60 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 87749.00 | 217.59 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 86282.00 | 428.50 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 84391.33 | 419.52 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 83504.33 | 178.77 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 81285.67 | 136.51 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 78196.67 | 387.95 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 77311.33 | 383.10 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 72745.00 | 377.19 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 70576.67 | 136.18 MB |
| c (99) | [kore](http://kore.io) (3.1) | 64863.67 | 175.89 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.5) | 63716.00 | 137.51 MB |
| node (11.10) | [hapi](http://hapijs.com) (18.1) | 62768.67 | 161.76 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 58619.33 | 132.77 MB |
| php (7.3) | [laravel](http://laravel.com) (5.7) | 58247.67 | 303.55 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 57859.00 | 87.35 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 46314.67 | 43.44 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 46077.67 | 85.46 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 43364.00 | 40.65 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 41058.00 | 39.11 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 40947.33 | 76.11 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 39614.67 | 97.55 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 36896.00 | 60.13 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 34370.67 | 19.85 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 33996.33 | 62.08 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 32757.00 | 40.17 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 31923.00 | 52.02 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 28949.33 | 51.58 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 24278.67 | 48.39 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 22683.67 | 65.85 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 22322.00 | 12.86 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 20382.00 | 154.06 MB |
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.3) | 17811.67 | 38.80 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 17532.00 | 45.50 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 14571.67 | 43.05 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 4292.00 | 13.14 MB |
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

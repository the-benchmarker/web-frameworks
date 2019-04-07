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
Last update: 2019-04-07
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: symfony (php)


:five: slim (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.10 ms | 0.09 ms | 0.14 ms | 0.20 ms | 4.63 ms | 49.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.18) | 5.17 ms | 0.31 ms | 18.26 ms | 39.64 ms | 106.07 ms | 9240.33 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 6.53 ms | 0.40 ms | 22.37 ms | 46.14 ms | 112.81 ms | 11022.33 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 217.14 ms | 0.52 ms | 371.46 ms | 4920.73 ms | 7591.73 ms | 798698.67 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 200.16 ms | 0.53 ms | 339.66 ms | 4378.53 ms | 7113.12 ms | 727190.67 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 196.37 ms | 0.54 ms | 354.44 ms | 4567.85 ms | 7405.19 ms | 736402.33 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.57 ms | 0.55 ms | 0.95 ms | 1.48 ms | 13.85 ms | 340.00 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 181.39 ms | 0.55 ms | 405.59 ms | 3751.56 ms | 7250.74 ms | 638590.33 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 9.47 ms | 0.64 ms | 30.18 ms | 60.53 ms | 163.31 ms | 14527.33 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 10.31 ms | 0.68 ms | 33.00 ms | 63.57 ms | 211.06 ms | 15725.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 11.85 ms | 0.89 ms | 35.46 ms | 68.93 ms | 164.95 ms | 16894.33 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 195.19 ms | 1.83 ms | 355.27 ms | 4258.60 ms | 7325.69 ms | 721695.67 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 194.88 ms | 1.95 ms | 376.55 ms | 4141.19 ms | 7208.37 ms | 692883.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 69.65 ms | 2.57 ms | 41.35 ms | 2089.81 ms | 4672.18 ms | 369873.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 3.88 ms | 2.91 ms | 7.88 ms | 15.51 ms | 62.92 ms | 3774.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 4.04 ms | 3.67 ms | 7.49 ms | 16.11 ms | 37.91 ms | 3326.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 7.72 ms | 4.28 ms | 11.09 ms | 47.20 ms | 760.25 ms | 29072.00 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 4.70 ms | 4.46 ms | 8.40 ms | 16.86 ms | 151.04 ms | 4256.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 5.63 ms | 4.77 ms | 11.62 ms | 19.60 ms | 68.23 ms | 4569.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.57 ms | 4.94 ms | 6.24 ms | 11.10 ms | 75.41 ms | 2162.67 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.87 ms | 5.34 ms | 8.65 ms | 18.36 ms | 229.19 ms | 6157.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 6.22 ms | 5.42 ms | 11.02 ms | 20.32 ms | 39.33 ms | 3870.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 8.15 ms | 5.43 ms | 14.65 ms | 69.95 ms | 123.33 ms | 10968.33 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 6.42 ms | 5.45 ms | 10.83 ms | 22.03 ms | 245.54 ms | 7054.67 | 
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 6.14 ms | 5.56 ms | 10.00 ms | 17.81 ms | 45.49 ms | 3166.67 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 42.90 ms | 6.44 ms | 139.35 ms | 349.82 ms | 913.75 ms | 75745.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 7.48 ms | 6.65 ms | 11.43 ms | 23.33 ms | 100.50 ms | 4866.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 8.75 ms | 8.09 ms | 13.73 ms | 25.32 ms | 158.60 ms | 5988.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 197.79 ms | 8.47 ms | 90.30 ms | 4658.58 ms | 7891.80 ms | 806951.00 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 11.34 ms | 8.57 ms | 23.02 ms | 48.59 ms | 163.07 ms | 9577.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 9.29 ms | 8.60 ms | 12.29 ms | 23.32 ms | 386.43 ms | 10364.33 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 12.23 ms | 8.92 ms | 22.39 ms | 76.21 ms | 369.63 ms | 15316.67 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 11.93 ms | 8.96 ms | 23.71 ms | 51.85 ms | 197.05 ms | 10596.00 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 12.98 ms | 9.07 ms | 26.37 ms | 61.48 ms | 477.03 ms | 16787.33 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 11.29 ms | 9.18 ms | 19.09 ms | 43.30 ms | 410.25 ms | 11793.33 | 
| node (11.13) | [restana](http://github.com/jkyberneees/ana) (2.12) | 11.68 ms | 9.26 ms | 15.46 ms | 47.30 ms | 566.02 ms | 23539.67 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 10.82 ms | 9.30 ms | 18.32 ms | 40.51 ms | 188.45 ms | 7759.00 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 12.84 ms | 9.84 ms | 24.95 ms | 54.15 ms | 117.63 ms | 10258.67 | 
| go (1.12) | [gf](http://goframe.org) (1.5) | 14.10 ms | 10.29 ms | 27.92 ms | 61.82 ms | 272.77 ms | 13119.00 | 
| node (11.13) | [polka](http://github.com/lukeed/polka) (0.5) | 14.27 ms | 10.33 ms | 19.39 ms | 67.45 ms | 596.10 ms | 26166.00 | 
| node (11.13) | [rayo](http://rayo.js.org) (1.2) | 14.78 ms | 10.37 ms | 19.39 ms | 106.13 ms | 650.34 ms | 29948.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 14.27 ms | 11.05 ms | 29.19 ms | 52.91 ms | 216.76 ms | 11046.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 16.06 ms | 12.55 ms | 30.59 ms | 55.97 ms | 226.49 ms | 11113.00 | 
| node (11.13) | [foxify](http://foxify.js.org) (0.10) | 18.23 ms | 13.59 ms | 24.42 ms | 114.63 ms | 716.44 ms | 32543.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 13.91 ms | 13.97 ms | 16.50 ms | 19.28 ms | 73.94 ms | 2229.67 | 
| node (11.13) | [koa](http://koajs.com) (2.7) | 21.55 ms | 14.71 ms | 26.29 ms | 220.67 ms | 945.70 ms | 48581.33 | 
| node (11.13) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 23.36 ms | 15.12 ms | 28.43 ms | 279.35 ms | 994.24 ms | 54931.00 | 
| node (11.13) | [fastify](http://fastify.io) (2.1) | 24.69 ms | 15.99 ms | 25.34 ms | 336.58 ms | 1125.81 ms | 64219.67 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 18.70 ms | 16.35 ms | 31.78 ms | 51.11 ms | 106.05 ms | 9669.00 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 21.82 ms | 16.52 ms | 42.89 ms | 73.54 ms | 254.40 ms | 15339.00 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 22.16 ms | 16.83 ms | 28.38 ms | 127.97 ms | 969.59 ms | 41275.33 | 
| node (11.13) | [express](http://expressjs.com) (4.16) | 25.78 ms | 17.06 ms | 29.18 ms | 322.01 ms | 1108.62 ms | 62035.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 28.34 ms | 18.45 ms | 37.23 ms | 371.81 ms | 1600.07 ms | 81110.33 | 
| node (11.13) | [restify](http://restify.com) (8.2) | 22.40 ms | 18.85 ms | 27.73 ms | 89.35 ms | 614.93 ms | 25479.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 31.41 ms | 25.01 ms | 57.98 ms | 79.71 ms | 116.29 ms | 17738.33 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.12) | 26.80 ms | 25.26 ms | 42.49 ms | 60.62 ms | 141.28 ms | 11725.33 | 
| swift (5.0) | [kitura](http://kitura.io) (2.6) | 28.08 ms | 26.20 ms | 38.75 ms | 56.50 ms | 542.29 ms | 19133.00 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 39.97 ms | 33.76 ms | 71.05 ms | 108.91 ms | 188.27 ms | 22569.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 34.96 ms | 35.04 ms | 44.65 ms | 53.28 ms | 207.41 ms | 7893.00 | 
| node (11.13) | [hapi](http://hapijs.com) (18.1) | 61.93 ms | 38.11 ms | 57.61 ms | 847.54 ms | 1869.30 ms | 137966.67 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 44.86 ms | 38.27 ms | 72.46 ms | 113.28 ms | 423.29 ms | 24284.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 48.93 ms | 38.87 ms | 94.62 ms | 126.25 ms | 376.88 ms | 28651.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 38.68 ms | 39.40 ms | 48.63 ms | 57.38 ms | 334.83 ms | 11771.00 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 45.77 ms | 40.97 ms | 53.67 ms | 253.54 ms | 825.98 ms | 47849.33 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 56.72 ms | 41.41 ms | 56.25 ms | 576.15 ms | 1539.44 ms | 101843.00 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 44.36 ms | 42.34 ms | 56.74 ms | 67.96 ms | 531.52 ms | 19875.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 46.63 ms | 43.32 ms | 58.68 ms | 72.62 ms | 580.53 ms | 17801.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 74.46 ms | 65.67 ms | 136.25 ms | 221.30 ms | 438.04 ms | 46932.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 79.14 ms | 72.73 ms | 132.96 ms | 185.12 ms | 400.19 ms | 36640.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 117.11 ms | 92.02 ms | 242.99 ms | 304.86 ms | 625.17 ms | 66945.67 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 104.64 ms | 98.11 ms | 172.21 ms | 214.56 ms | 294.01 ms | 43133.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 109.07 ms | 107.48 ms | 152.27 ms | 177.28 ms | 424.02 ms | 29613.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (evhtp) (cpp)


:five: (kore) (c)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 256574.67 | 148.42 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 239637.67 | 286.85 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 198607.67 | 225.74 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 194360.33 | 188.75 MB |
| c (99) | [kore](http://kore.io) (3.1) | 189364.33 | 491.86 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 188421.00 | 213.94 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 162835.67 | 261.16 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 156623.00 | 166.88 MB |
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 153548.67 | 144.17 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 152822.67 | 313.40 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 149536.33 | 86.47 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 145818.00 | 293.14 MB |
| java (8) | [act](http://actframework.org) (1.8) | 136328.67 | 265.99 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 105333.67 | 171.75 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 104511.33 | 131.77 MB |
| node (11.13) | [restana](http://github.com/jkyberneees/ana) (2.12) | 99540.67 | 149.04 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 95427.33 | 167.60 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 95092.67 | 126.88 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 94085.67 | 126.39 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 93603.67 | 124.48 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 91924.33 | 161.28 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 88531.67 | 175.84 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 88524.67 | 118.81 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 84959.00 | 114.31 MB |
| node (11.13) | [rayo](http://rayo.js.org) (1.2) | 80599.00 | 120.70 MB |
| node (11.13) | [polka](http://github.com/lukeed/polka) (0.5) | 80588.67 | 120.71 MB |
| go (1.12) | [gf](http://goframe.org) (1.5) | 79244.33 | 119.79 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 76132.67 | 195.66 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 70998.67 | 66.79 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 65513.67 | 161.46 MB |
| node (11.13) | [foxify](http://foxify.js.org) (0.10) | 64309.67 | 135.17 MB |
| node (11.13) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 63192.67 | 94.66 MB |
| node (11.13) | [fastify](http://fastify.io) (2.1) | 62736.00 | 156.79 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 59676.33 | 128.23 MB |
| node (11.13) | [koa](http://koajs.com) (2.7) | 58848.33 | 124.46 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 54273.67 | 117.00 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 53681.00 | 93.96 MB |
| node (11.13) | [express](http://expressjs.com) (4.16) | 52606.00 | 128.76 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 52396.33 | 87.19 MB |
| node (11.13) | [restify](http://restify.com) (8.2) | 48780.33 | 85.64 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 48460.67 | 120.12 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 47806.33 | 237.81 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 47075.00 | 233.33 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 46138.67 | 228.84 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 44947.33 | 222.76 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 40873.67 | 212.08 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.12) | 37156.33 | 80.12 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 36426.00 | 189.78 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.6) | 35935.67 | 66.66 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 32896.67 | 74.62 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 30496.67 | 45.92 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 28465.33 | 26.69 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 25770.67 | 49.81 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 25675.00 | 41.85 MB |
| node (11.13) | [hapi](http://hapijs.com) (18.1) | 24800.33 | 64.13 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.18) | 24771.33 | 23.63 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 23905.00 | 22.45 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 22858.00 | 42.47 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 22621.67 | 27.87 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 22460.33 | 40.98 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 20974.00 | 51.73 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 20759.67 | 33.83 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 19576.33 | 11.30 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 13942.00 | 24.87 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 13451.33 | 7.77 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 12583.00 | 25.09 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 12375.33 | 93.65 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 10757.67 | 27.95 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 9481.33 | 20.65 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 8937.00 | 26.38 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 8820.67 | 25.57 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 2979.00 | 9.10 MB |
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

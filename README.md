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
Last update: 2018-12-28
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: laravel (php)


:five: symfony (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 0.08 ms | 0.08 ms | 0.11 ms | 0.15 ms | 1.04 ms | 27.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.15) | 3.96 ms | 0.23 ms | 14.06 ms | 31.76 ms | 83.33 ms | 7267.67 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 5.01 ms | 0.29 ms | 17.74 ms | 37.30 ms | 92.93 ms | 8794.00 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 143.48 ms | 0.40 ms | 308.55 ms | 3099.05 ms | 6900.03 ms | 521811.33 | 
| php (7.2) | [symfony](http://symfony.com) (4.2) | 186.19 ms | 0.40 ms | 312.35 ms | 4185.46 ms | 7004.42 ms | 688374.33 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 218.10 ms | 0.42 ms | 290.21 ms | 5109.64 ms | 7429.52 ms | 837757.00 | 
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 0.43 ms | 0.45 ms | 0.68 ms | 0.99 ms | 10.37 ms | 233.33 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 7.55 ms | 0.45 ms | 25.41 ms | 49.75 ms | 141.15 ms | 12205.00 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 7.35 ms | 0.46 ms | 24.29 ms | 49.50 ms | 109.40 ms | 11776.33 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 8.79 ms | 0.61 ms | 26.94 ms | 51.65 ms | 116.17 ms | 12726.67 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 163.15 ms | 1.71 ms | 289.52 ms | 3454.13 ms | 6920.46 ms | 588212.00 | 
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 188.96 ms | 1.72 ms | 358.20 ms | 4062.96 ms | 7344.75 ms | 692230.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 120.52 ms | 1.73 ms | 24.02 ms | 3075.89 ms | 6593.23 ms | 588307.67 | 
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 166.98 ms | 1.78 ms | 309.97 ms | 3629.15 ms | 6304.02 ms | 599672.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 2.80 ms | 2.25 ms | 5.44 ms | 8.16 ms | 23.48 ms | 1928.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.33 ms | 2.40 ms | 6.88 ms | 15.56 ms | 37.83 ms | 3262.00 | 
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 3.12 ms | 2.53 ms | 5.87 ms | 12.22 ms | 148.10 ms | 3525.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.30 ms | 3.39 ms | 8.79 ms | 16.94 ms | 40.77 ms | 3642.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.22 ms | 3.42 ms | 5.11 ms | 7.33 ms | 20.68 ms | 1593.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.90 ms | 3.47 ms | 6.00 ms | 11.93 ms | 53.99 ms | 2381.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.22 ms | 3.58 ms | 7.02 ms | 12.63 ms | 48.85 ms | 2455.00 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 34.97 ms | 3.58 ms | 117.10 ms | 305.07 ms | 770.32 ms | 65438.33 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 4.01 ms | 3.69 ms | 6.77 ms | 13.33 ms | 31.46 ms | 2622.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 4.40 ms | 3.84 ms | 8.12 ms | 15.33 ms | 33.07 ms | 3052.00 | 
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 3.93 ms | 3.94 ms | 5.25 ms | 9.58 ms | 104.91 ms | 2625.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.5) | 8.12 ms | 4.00 ms | 13.59 ms | 89.75 ms | 125.03 ms | 15272.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 6.10 ms | 5.25 ms | 8.78 ms | 16.82 ms | 287.55 ms | 7774.67 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 6.43 ms | 5.38 ms | 10.09 ms | 20.48 ms | 174.22 ms | 4938.33 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 6.58 ms | 5.49 ms | 10.27 ms | 21.23 ms | 162.30 ms | 4699.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.37 ms | 5.57 ms | 10.65 ms | 20.92 ms | 110.23 ms | 4422.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 5.67 ms | 5.64 ms | 9.65 ms | 11.00 ms | 179.77 ms | 3120.33 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 6.82 ms | 5.72 ms | 9.64 ms | 25.49 ms | 76.73 ms | 4209.67 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 7.06 ms | 5.91 ms | 11.04 ms | 22.26 ms | 217.98 ms | 5506.00 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 7.42 ms | 6.32 ms | 11.71 ms | 24.19 ms | 118.06 ms | 5033.33 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 7.63 ms | 6.41 ms | 12.08 ms | 25.68 ms | 220.56 ms | 6288.00 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 8.01 ms | 6.62 ms | 12.28 ms | 25.86 ms | 304.75 ms | 9847.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 177.77 ms | 6.93 ms | 88.36 ms | 4065.15 ms | 7689.09 ms | 706820.00 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 8.15 ms | 7.07 ms | 12.62 ms | 26.71 ms | 180.17 ms | 6271.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 12.08 ms | 9.07 ms | 26.74 ms | 48.76 ms | 216.07 ms | 10221.33 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 12.07 ms | 9.24 ms | 19.31 ms | 44.59 ms | 419.09 ms | 13678.67 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 11.79 ms | 9.31 ms | 17.75 ms | 42.15 ms | 409.75 ms | 15198.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 13.03 ms | 9.40 ms | 27.68 ms | 50.20 ms | 353.85 ms | 11587.33 | 
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 13.10 ms | 9.55 ms | 23.52 ms | 52.44 ms | 390.34 ms | 14231.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 9.81 ms | 9.96 ms | 11.66 ms | 13.69 ms | 78.21 ms | 1737.33 | 
| go (1.11) | [gf](http://gfer.me) (1.3) | 11.06 ms | 10.30 ms | 15.34 ms | 33.19 ms | 255.46 ms | 7718.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 15.40 ms | 11.88 ms | 21.51 ms | 66.53 ms | 804.98 ms | 29654.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 14.57 ms | 12.17 ms | 24.99 ms | 47.09 ms | 623.04 ms | 20402.00 | 
| node (11.1) | [fastify](http://fastify.io) (1.13) | 17.34 ms | 13.17 ms | 24.57 ms | 75.56 ms | 648.85 ms | 26707.00 | 
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 17.34 ms | 13.39 ms | 24.97 ms | 68.43 ms | 681.06 ms | 26467.00 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 20.34 ms | 15.44 ms | 29.27 ms | 107.75 ms | 721.75 ms | 30462.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 23.08 ms | 19.79 ms | 45.16 ms | 63.49 ms | 118.55 ms | 14485.33 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 28.73 ms | 20.53 ms | 37.77 ms | 247.25 ms | 1047.80 ms | 53843.33 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 26.18 ms | 22.28 ms | 40.79 ms | 71.25 ms | 496.72 ms | 20221.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 25.89 ms | 23.68 ms | 38.90 ms | 53.65 ms | 127.00 ms | 9337.00 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 26.88 ms | 25.05 ms | 35.40 ms | 43.35 ms | 260.36 ms | 8947.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 32.93 ms | 25.59 ms | 63.31 ms | 91.85 ms | 415.23 ms | 21256.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 28.38 ms | 27.83 ms | 35.97 ms | 41.22 ms | 179.64 ms | 6752.33 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 58.34 ms | 32.22 ms | 53.82 ms | 922.76 ms | 1994.84 ms | 149956.67 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 32.93 ms | 32.52 ms | 40.84 ms | 46.04 ms | 322.84 ms | 10175.00 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 38.28 ms | 36.19 ms | 48.17 ms | 53.78 ms | 221.21 ms | 8772.33 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 39.19 ms | 38.47 ms | 45.74 ms | 52.20 ms | 134.09 ms | 6438.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 40.66 ms | 40.81 ms | 46.89 ms | 116.40 ms | 329.35 ms | 16857.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 51.57 ms | 42.81 ms | 98.36 ms | 173.71 ms | 323.68 ms | 36807.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 57.41 ms | 48.44 ms | 94.74 ms | 154.54 ms | 846.96 ms | 39421.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 55.54 ms | 58.42 ms | 90.44 ms | 118.16 ms | 167.15 ms | 27495.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 84.83 ms | 84.36 ms | 104.02 ms | 126.24 ms | 295.06 ms | 18336.67 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 351438.33 | 203.10 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 313251.33 | 375.12 MB |
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 301335.33 | 342.48 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 270172.00 | 262.23 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 249878.00 | 283.58 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 247169.33 | 496.73 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 239525.00 | 384.13 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 235329.67 | 221.45 MB |
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 226588.67 | 463.87 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 224587.67 | 239.93 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.5) | 211145.00 | 122.18 MB |
| java (8) | [act](http://actframework.org) (1.8) | 198733.67 | 388.07 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 161142.00 | 262.58 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 153639.33 | 205.52 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 149573.00 | 200.27 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 147379.67 | 258.58 MB |
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 140423.00 | 177.18 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 140003.00 | 187.37 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 133387.00 | 179.43 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 131120.67 | 230.09 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 127021.00 | 169.38 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 121165.67 | 212.65 MB |
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 107283.33 | 213.39 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 100025.67 | 94.05 MB |
| go (1.11) | [gf](http://gfer.me) (1.3) | 91760.67 | 139.11 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 91441.33 | 235.04 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 91327.67 | 136.81 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 88823.33 | 132.89 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 84798.33 | 182.02 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 84491.00 | 208.22 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 84180.00 | 126.08 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 79054.67 | 138.37 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 72311.67 | 121.10 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 70332.67 | 166.55 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 65545.67 | 137.59 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 62019.00 | 307.64 MB |
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 59300.00 | 294.61 MB |
| c (99) | [kore](http://kore.io) (3.1) | 57635.33 | 156.14 MB |
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 56795.00 | 281.95 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 55741.00 | 117.74 MB |
| php (7.2) | [symfony](http://symfony.com) (4.2) | 55423.33 | 274.76 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 52788.00 | 273.79 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 46558.00 | 105.58 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 45151.33 | 235.05 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 43246.00 | 65.25 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 43145.33 | 105.40 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 39652.33 | 69.39 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 38777.67 | 71.94 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 36955.33 | 34.63 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 35170.33 | 32.97 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.15) | 32244.00 | 30.73 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 30945.00 | 76.24 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 29794.00 | 48.55 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 28585.00 | 73.96 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 25653.33 | 41.79 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 25649.67 | 31.47 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 25492.00 | 14.72 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 25256.33 | 46.09 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 20683.33 | 36.85 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 17978.00 | 35.90 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 17885.67 | 51.86 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 17441.67 | 10.06 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 16902.00 | 127.72 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 14582.33 | 37.78 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 11591.00 | 30.94 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3650.33 | 11.20 MB |
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

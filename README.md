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
Last update: 2018-12-14
```
OS: Linux (version: 4.19.8-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: zend-framework (php)


:four: lumen (php)


:five: laravel (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 0.07 ms | 0.07 ms | 0.11 ms | 0.15 ms | 0.90 ms | 26.67 | 
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 0.43 ms | 0.41 ms | 0.72 ms | 1.08 ms | 16.48 ms | 277.00 | 
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 166.66 ms | 0.44 ms | 328.01 ms | 3612.73 ms | 6921.50 ms | 601945.33 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 172.86 ms | 0.45 ms | 294.67 ms | 3970.59 ms | 6928.52 ms | 652630.33 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 166.10 ms | 0.45 ms | 308.18 ms | 3434.26 ms | 7597.66 ms | 621966.67 | 
| php (7.2) | [symfony](http://symfony.com) (4.2) | 193.62 ms | 0.45 ms | 341.63 ms | 4280.06 ms | 7370.03 ms | 708268.33 | 
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 212.19 ms | 0.46 ms | 317.93 ms | 4862.99 ms | 7299.12 ms | 797628.00 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 171.54 ms | 0.46 ms | 297.44 ms | 3906.31 ms | 7206.64 ms | 644621.33 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 5.13 ms | 0.87 ms | 15.37 ms | 45.41 ms | 137.94 ms | 9419.67 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 3.93 ms | 1.11 ms | 11.14 ms | 32.67 ms | 112.29 ms | 6748.67 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 8.65 ms | 1.18 ms | 26.57 ms | 74.39 ms | 205.86 ms | 15573.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 183.04 ms | 1.80 ms | 119.12 ms | 3985.22 ms | 6593.76 ms | 741659.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.44 ms | 2.31 ms | 7.45 ms | 17.63 ms | 36.64 ms | 3639.67 | 
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 3.20 ms | 2.41 ms | 6.20 ms | 13.08 ms | 91.46 ms | 3359.33 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 3.10 ms | 2.60 ms | 5.20 ms | 9.42 ms | 28.11 ms | 1854.00 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 9.87 ms | 3.19 ms | 28.54 ms | 70.29 ms | 186.52 ms | 15094.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.26 ms | 3.28 ms | 8.81 ms | 17.12 ms | 44.81 ms | 3677.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.36 ms | 3.47 ms | 5.41 ms | 8.14 ms | 20.75 ms | 1804.33 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 8.10 ms | 3.50 ms | 22.31 ms | 51.99 ms | 136.88 ms | 11264.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.12 ms | 3.72 ms | 6.41 ms | 13.01 ms | 100.83 ms | 2840.33 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 4.31 ms | 3.84 ms | 7.91 ms | 15.41 ms | 32.67 ms | 3053.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.55 ms | 3.96 ms | 7.17 ms | 13.21 ms | 60.55 ms | 2650.33 | 
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 4.03 ms | 4.00 ms | 5.50 ms | 10.35 ms | 132.41 ms | 2198.00 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 7.18 ms | 4.74 ms | 13.41 ms | 56.00 ms | 118.52 ms | 9739.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 5.37 ms | 5.32 ms | 9.02 ms | 11.15 ms | 165.13 ms | 2928.67 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 7.17 ms | 6.09 ms | 11.21 ms | 22.58 ms | 154.36 ms | 4409.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 6.97 ms | 6.16 ms | 9.89 ms | 18.56 ms | 287.42 ms | 6420.33 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 7.33 ms | 6.19 ms | 11.42 ms | 23.62 ms | 114.02 ms | 4819.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.91 ms | 6.24 ms | 11.04 ms | 22.94 ms | 124.65 ms | 4813.67 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 7.50 ms | 6.52 ms | 10.77 ms | 26.61 ms | 98.92 ms | 4347.33 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 8.13 ms | 6.77 ms | 12.87 ms | 27.25 ms | 290.56 ms | 8162.00 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 8.46 ms | 6.96 ms | 12.57 ms | 27.13 ms | 332.74 ms | 11407.67 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 8.33 ms | 7.21 ms | 13.26 ms | 26.84 ms | 217.90 ms | 5863.00 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 9.44 ms | 7.56 ms | 13.67 ms | 32.54 ms | 568.48 ms | 17016.00 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 9.22 ms | 8.04 ms | 14.08 ms | 33.14 ms | 268.66 ms | 8448.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 11.64 ms | 8.91 ms | 23.13 ms | 39.97 ms | 204.05 ms | 9183.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 211.32 ms | 9.09 ms | 79.72 ms | 4507.24 ms | 7896.44 ms | 806498.00 | 
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 13.68 ms | 9.39 ms | 22.56 ms | 61.52 ms | 576.68 ms | 23598.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 12.80 ms | 9.41 ms | 26.29 ms | 46.40 ms | 222.05 ms | 10383.67 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 40.54 ms | 9.73 ms | 129.35 ms | 319.68 ms | 798.23 ms | 69791.00 | 
| go (1.11) | [gf](http://gfer.me) (1.3) | 12.50 ms | 11.06 ms | 17.44 ms | 43.18 ms | 445.36 ms | 14700.67 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 15.29 ms | 11.39 ms | 23.01 ms | 67.63 ms | 608.87 ms | 24688.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 11.66 ms | 11.87 ms | 13.74 ms | 15.74 ms | 28.85 ms | 1900.00 | 
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 17.28 ms | 12.54 ms | 24.79 ms | 132.46 ms | 576.49 ms | 27564.00 | 
| node (11.1) | [fastify](http://fastify.io) (1.13) | 17.97 ms | 12.82 ms | 26.37 ms | 108.23 ms | 602.81 ms | 28396.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 18.79 ms | 13.15 ms | 23.84 ms | 145.07 ms | 1091.87 ms | 48125.00 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 17.60 ms | 13.17 ms | 28.44 ms | 66.90 ms | 636.45 ms | 24764.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 15.23 ms | 13.17 ms | 26.79 ms | 48.20 ms | 666.61 ms | 17430.33 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 24.14 ms | 16.86 ms | 31.50 ms | 220.92 ms | 990.01 ms | 48173.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 23.53 ms | 19.34 ms | 44.59 ms | 67.82 ms | 189.23 ms | 14408.33 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 24.21 ms | 22.13 ms | 33.14 ms | 41.85 ms | 177.47 ms | 5934.00 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 34.70 ms | 23.15 ms | 44.11 ms | 396.98 ms | 1331.85 ms | 75488.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 27.00 ms | 24.97 ms | 40.09 ms | 54.90 ms | 233.46 ms | 9604.33 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 31.57 ms | 25.99 ms | 50.31 ms | 121.29 ms | 662.51 ms | 30642.00 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 29.99 ms | 26.49 ms | 39.12 ms | 46.43 ms | 259.56 ms | 11379.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 35.25 ms | 27.91 ms | 63.62 ms | 94.64 ms | 265.26 ms | 18636.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 29.47 ms | 30.68 ms | 37.27 ms | 45.82 ms | 249.20 ms | 8644.33 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 33.86 ms | 31.33 ms | 41.68 ms | 85.60 ms | 408.65 ms | 14780.00 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 43.11 ms | 32.60 ms | 44.76 ms | 404.98 ms | 1085.59 ms | 71860.67 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 59.08 ms | 34.25 ms | 56.78 ms | 914.09 ms | 1952.78 ms | 144673.00 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 41.71 ms | 35.94 ms | 49.10 ms | 182.44 ms | 493.43 ms | 26277.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 54.28 ms | 47.57 ms | 99.20 ms | 176.88 ms | 297.79 ms | 34721.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 61.31 ms | 51.39 ms | 112.25 ms | 151.31 ms | 333.40 ms | 29964.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 68.33 ms | 60.28 ms | 114.97 ms | 171.16 ms | 256.37 ms | 32697.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 91.85 ms | 89.96 ms | 118.29 ms | 146.23 ms | 420.65 ms | 24307.33 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 331280.67 | 191.75 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 322923.33 | 386.10 MB |
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 300141.33 | 341.08 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 264038.33 | 256.41 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 257728.67 | 292.74 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 233873.33 | 470.00 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 229052.67 | 245.14 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 226576.00 | 365.13 MB |
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 219786.67 | 451.25 MB |
| java (8) | [act](http://actframework.org) (1.8) | 188394.33 | 367.68 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 172216.67 | 99.62 MB |
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 141552.33 | 179.19 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 137606.33 | 224.21 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 135166.00 | 180.53 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 134892.33 | 180.55 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 134076.00 | 235.08 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 123486.33 | 216.81 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 122540.00 | 163.89 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 119024.33 | 209.02 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 114786.67 | 152.87 MB |
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 112924.33 | 224.20 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 110967.33 | 149.38 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 93471.67 | 240.19 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 87561.67 | 131.04 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 86627.67 | 81.49 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 85162.67 | 209.88 MB |
| go (1.11) | [gf](http://gfer.me) (1.3) | 84524.00 | 128.16 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 76292.67 | 114.35 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 75038.67 | 131.55 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 68190.67 | 163.82 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 67683.33 | 142.20 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 66653.67 | 111.36 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 64986.00 | 97.15 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 61767.33 | 132.45 MB |
| c (99) | [kore](http://kore.io) (3.1) | 60413.00 | 163.73 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 52208.00 | 110.28 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 48740.00 | 241.73 MB |
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 48199.67 | 239.15 MB |
| php (7.2) | [symfony](http://symfony.com) (4.2) | 47199.00 | 233.94 MB |
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 46877.33 | 232.28 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 45824.67 | 237.59 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 44562.33 | 101.09 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 42797.33 | 222.65 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 40804.67 | 38.26 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 37779.67 | 92.27 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 37183.67 | 68.94 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 34689.00 | 53.38 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 34255.33 | 32.10 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 33827.33 | 59.18 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 33224.00 | 54.13 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 32686.67 | 31.14 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 29636.00 | 36.49 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 28914.67 | 52.78 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 28448.33 | 70.15 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 27140.00 | 70.17 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 25271.00 | 41.20 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 25199.00 | 14.54 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 19313.00 | 34.42 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 16172.33 | 46.89 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 15909.67 | 9.17 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 14845.00 | 112.27 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 14735.00 | 29.40 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 12949.00 | 33.58 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 10623.67 | 28.45 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 3169.33 | 9.65 MB |
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

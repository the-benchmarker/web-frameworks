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
Last update: 2018-12-18
```
OS: Linux (version: 4.19.8-200.fc28.x86_64, arch: x86_64)
CPU Cores: 4
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: roda (ruby)


:four: laravel (php)


:five: hanami (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 0.04 ms | 0.04 ms | 0.05 ms | 0.05 ms | 0.76 ms | 6.67 | 
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 0.28 ms | 0.23 ms | 0.46 ms | 1.02 ms | 34.55 ms | 381.33 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.15) | 1.71 ms | 0.25 ms | 4.40 ms | 23.96 ms | 81.08 ms | 4603.67 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 128.65 ms | 0.28 ms | 369.26 ms | 2242.59 ms | 7187.60 ms | 445761.00 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 3.47 ms | 0.28 ms | 9.90 ms | 44.31 ms | 107.18 ms | 8225.67 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 136.86 ms | 0.39 ms | 296.10 ms | 2541.61 ms | 5802.82 ms | 475722.00 | 
| php (7.2) | [symfony](http://symfony.com) (4.2) | 139.06 ms | 0.82 ms | 330.62 ms | 2552.97 ms | 5933.11 ms | 470761.67 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 2.09 ms | 0.92 ms | 3.59 ms | 30.68 ms | 122.99 ms | 5695.67 | 
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 137.69 ms | 1.05 ms | 354.60 ms | 2709.60 ms | 7183.19 ms | 492703.67 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 148.83 ms | 1.22 ms | 359.16 ms | 2774.91 ms | 7057.27 ms | 520149.00 | 
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 138.80 ms | 1.23 ms | 370.02 ms | 2409.50 ms | 5964.10 ms | 446033.33 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 4.00 ms | 1.29 ms | 9.61 ms | 46.02 ms | 111.45 ms | 8386.67 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 3.14 ms | 1.65 ms | 6.11 ms | 33.08 ms | 96.69 ms | 6153.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 164.60 ms | 2.55 ms | 5.84 ms | 4729.78 ms | 6591.31 ms | 763052.67 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 17.10 ms | 2.89 ms | 59.12 ms | 105.73 ms | 199.87 ms | 26111.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 4.49 ms | 4.17 ms | 5.92 ms | 7.97 ms | 145.88 ms | 3212.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 4.33 ms | 4.32 ms | 7.41 ms | 8.40 ms | 46.48 ms | 2309.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 4.60 ms | 4.57 ms | 8.29 ms | 14.11 ms | 34.12 ms | 2984.67 | 
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 5.11 ms | 4.76 ms | 9.14 ms | 14.21 ms | 31.91 ms | 2846.00 | 
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 6.46 ms | 5.16 ms | 9.70 ms | 15.70 ms | 228.60 ms | 7955.00 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.57 ms | 5.22 ms | 8.54 ms | 14.27 ms | 148.62 ms | 2930.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 5.78 ms | 5.29 ms | 9.61 ms | 16.36 ms | 151.71 ms | 3940.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.34 ms | 5.47 ms | 10.60 ms | 22.36 ms | 110.22 ms | 4841.33 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 6.76 ms | 6.01 ms | 11.18 ms | 18.94 ms | 36.51 ms | 3548.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 6.83 ms | 6.32 ms | 12.04 ms | 19.02 ms | 41.74 ms | 4256.67 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 9.44 ms | 6.50 ms | 15.33 ms | 85.62 ms | 160.89 ms | 13194.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 7.56 ms | 7.11 ms | 10.95 ms | 15.35 ms | 46.98 ms | 2584.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 220.71 ms | 8.09 ms | 124.88 ms | 4971.43 ms | 7943.20 ms | 871463.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 9.59 ms | 8.67 ms | 11.94 ms | 22.66 ms | 489.40 ms | 12427.67 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 17.34 ms | 9.84 ms | 26.75 ms | 194.47 ms | 527.04 ms | 35230.67 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 15.64 ms | 10.05 ms | 24.44 ms | 156.83 ms | 516.50 ms | 28897.33 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 17.72 ms | 10.11 ms | 28.58 ms | 185.18 ms | 577.18 ms | 34524.00 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 11.26 ms | 10.41 ms | 14.22 ms | 29.05 ms | 164.85 ms | 4346.67 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 17.16 ms | 10.70 ms | 28.98 ms | 171.09 ms | 419.82 ms | 30845.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 17.48 ms | 10.77 ms | 29.60 ms | 176.76 ms | 569.65 ms | 32789.00 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 15.60 ms | 11.33 ms | 28.93 ms | 126.74 ms | 385.57 ms | 23558.33 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 19.55 ms | 11.54 ms | 40.49 ms | 184.96 ms | 514.05 ms | 34895.33 | 
| go (1.11) | [gf](http://gfer.me) (1.3) | 24.81 ms | 12.84 ms | 47.21 ms | 259.25 ms | 600.79 ms | 49098.00 | 
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 24.90 ms | 13.48 ms | 33.07 ms | 356.68 ms | 1286.20 ms | 70090.33 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 24.89 ms | 13.55 ms | 31.00 ms | 344.15 ms | 1302.40 ms | 69531.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 19.85 ms | 14.82 ms | 33.56 ms | 67.20 ms | 318.62 ms | 14859.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 63.71 ms | 15.31 ms | 29.96 ms | 1792.55 ms | 3681.08 ms | 301319.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 17.72 ms | 16.82 ms | 21.11 ms | 27.93 ms | 167.53 ms | 5812.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 16.47 ms | 16.85 ms | 17.40 ms | 18.16 ms | 196.68 ms | 2739.67 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 31.89 ms | 17.13 ms | 39.17 ms | 498.81 ms | 1580.94 ms | 90976.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 18.90 ms | 17.98 ms | 21.24 ms | 29.27 ms | 170.40 ms | 6102.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 22.19 ms | 18.32 ms | 33.32 ms | 55.87 ms | 484.13 ms | 17458.67 | 
| node (11.1) | [fastify](http://fastify.io) (1.13) | 39.89 ms | 18.36 ms | 38.65 ms | 771.90 ms | 1802.87 ms | 125975.33 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 89.15 ms | 19.82 ms | 46.51 ms | 1831.67 ms | 3849.27 ms | 331471.00 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 22.14 ms | 20.15 ms | 29.59 ms | 30.45 ms | 238.88 ms | 8916.00 | 
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 39.65 ms | 20.50 ms | 43.32 ms | 672.35 ms | 1661.67 ms | 111057.67 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 55.15 ms | 21.29 ms | 44.57 ms | 1137.35 ms | 2294.90 ms | 183151.00 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 22.62 ms | 23.39 ms | 28.22 ms | 28.86 ms | 96.50 ms | 4380.67 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 55.21 ms | 24.94 ms | 49.08 ms | 1049.85 ms | 2252.22 ms | 170569.00 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 34.62 ms | 24.95 ms | 48.27 ms | 308.30 ms | 1174.93 ms | 62439.67 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 27.08 ms | 27.37 ms | 29.85 ms | 34.99 ms | 247.12 ms | 7204.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 28.56 ms | 27.85 ms | 31.69 ms | 32.34 ms | 104.59 ms | 4019.00 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 27.71 ms | 29.39 ms | 30.40 ms | 31.05 ms | 37.27 ms | 3352.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 35.72 ms | 29.97 ms | 54.97 ms | 62.15 ms | 392.81 ms | 19582.00 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 165.85 ms | 42.28 ms | 85.31 ms | 2977.76 ms | 4616.66 ms | 510004.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 54.68 ms | 48.40 ms | 78.15 ms | 133.17 ms | 704.98 ms | 34494.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 66.63 ms | 61.13 ms | 114.74 ms | 160.04 ms | 332.94 ms | 35257.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 85.64 ms | 72.52 ms | 120.49 ms | 139.90 ms | 365.59 ms | 29012.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 90.75 ms | 85.69 ms | 109.80 ms | 240.96 ms | 1190.31 ms | 54781.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 253.20 ms | 125.66 ms | 164.15 ms | 3632.49 ms | 6704.22 ms | 639009.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (fasthttprouter) (go)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 218867.33 | 126.43 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 198335.33 | 237.32 MB |
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 176268.00 | 200.25 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 162650.67 | 261.96 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 161289.00 | 156.37 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 154972.33 | 175.77 MB |
| java (8) | [act](http://actframework.org) (1.8) | 153107.00 | 298.61 MB |
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 151129.33 | 309.31 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 144443.00 | 153.81 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 140619.00 | 282.53 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 132921.33 | 76.90 MB |
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 109734.67 | 138.05 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 107492.67 | 174.75 MB |
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 107001.33 | 212.68 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 98136.33 | 131.38 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 95584.33 | 127.50 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 93996.00 | 125.65 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 91985.33 | 161.51 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 90230.67 | 158.40 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 88902.33 | 156.06 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 87042.67 | 116.82 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 86966.67 | 116.24 MB |
| go (1.11) | [gf](http://gfer.me) (1.3) | 77099.00 | 117.05 MB |
| c (99) | [kore](http://kore.io) (3.1) | 75598.33 | 205.02 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 74823.33 | 160.26 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 60616.00 | 90.66 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 59420.00 | 55.73 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 59407.67 | 103.92 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 58884.33 | 88.05 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 56192.67 | 52.70 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 53955.00 | 138.45 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 52523.00 | 49.26 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 51905.00 | 257.43 MB |
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 50912.67 | 252.76 MB |
| php (7.2) | [symfony](http://symfony.com) (4.2) | 50736.00 | 251.59 MB |
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 48595.33 | 240.83 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 48328.33 | 72.18 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 47319.00 | 115.72 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 46782.67 | 115.00 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 45143.00 | 55.48 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 44516.00 | 231.08 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 44322.67 | 72.24 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 43604.33 | 72.73 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 41363.00 | 86.70 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 40572.67 | 85.67 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.15) | 37867.33 | 36.08 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 37123.33 | 68.90 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 36099.33 | 65.95 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 35119.00 | 85.78 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 35116.33 | 61.45 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 34904.67 | 182.14 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 34734.00 | 56.65 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 32025.67 | 18.47 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 28006.67 | 63.37 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 24596.33 | 40.87 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 20855.33 | 53.85 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 20624.33 | 11.89 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 18437.00 | 139.36 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 18318.33 | 44.99 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 15974.67 | 41.42 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 15009.67 | 26.73 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 11490.00 | 22.91 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 11107.33 | 32.13 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 7204.33 | 19.24 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 3740.67 | 11.44 MB |
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

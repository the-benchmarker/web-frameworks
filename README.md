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
Last update: 2019-06-04
```
OS: Linux (version: 5.0.16-300.fc30.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: iron (rust)


:five: zend-framework (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.35) | [nickel](http://nickel-org.github.io) (0.11) | 0.08 ms | 0.08 ms | 0.12 ms | 0.16 ms | 6.66 ms | 86.00 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 3.06 ms | 0.16 ms | 11.71 ms | 30.02 ms | 81.73 ms | 6604.67 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.18 ms | 0.20 ms | 16.40 ms | 37.27 ms | 97.04 ms | 8537.67 | 
| rust (1.35) | [iron](http://ironframework.io) (0.6) | 0.35 ms | 0.31 ms | 0.58 ms | 0.98 ms | 13.95 ms | 224.67 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 107.75 ms | 0.33 ms | 238.38 ms | 2238.66 ms | 5755.87 ms | 423891.00 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 109.19 ms | 0.34 ms | 223.28 ms | 2399.65 ms | 6808.61 ms | 425020.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 6.31 ms | 0.34 ms | 22.23 ms | 47.49 ms | 108.52 ms | 11106.67 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 102.00 ms | 0.35 ms | 282.33 ms | 1749.84 ms | 6840.11 ms | 347784.67 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 118.92 ms | 0.35 ms | 227.31 ms | 2700.40 ms | 6810.92 ms | 465484.00 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 7.70 ms | 0.38 ms | 27.39 ms | 56.86 ms | 135.52 ms | 13553.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 8.55 ms | 0.48 ms | 28.35 ms | 58.17 ms | 126.59 ms | 13880.67 | 
| php (7.3) | [symfony](http://symfony.com) (4.3) | 142.15 ms | 0.50 ms | 253.07 ms | 3156.48 ms | 6820.91 ms | 543172.33 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 132.24 ms | 1.26 ms | 248.25 ms | 2882.27 ms | 5764.03 ms | 487794.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 67.83 ms | 1.56 ms | 3.63 ms | 1990.45 ms | 6587.22 ms | 400871.33 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 3.28 ms | 1.72 ms | 7.87 ms | 16.90 ms | 130.52 ms | 3791.67 | 
| rust (1.35) | [actix-web](http://actix.rs) (0.7) | 3.35 ms | 2.50 ms | 7.09 ms | 15.09 ms | 33.64 ms | 3157.33 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 33.52 ms | 2.74 ms | 113.02 ms | 300.86 ms | 769.12 ms | 64127.00 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.46 ms | 2.81 ms | 5.71 ms | 11.41 ms | 51.51 ms | 2350.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.60 ms | 2.90 ms | 7.07 ms | 15.51 ms | 32.86 ms | 3237.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 7.33 ms | 2.94 ms | 8.74 ms | 120.02 ms | 815.58 ms | 35350.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.76 ms | 3.68 ms | 10.29 ms | 17.68 ms | 47.80 ms | 3990.67 | 
| rust (1.35) | [gotham](http://gotham.rs) (0.3) | 4.56 ms | 3.71 ms | 8.76 ms | 19.54 ms | 212.85 ms | 5752.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.56 ms | 3.80 ms | 5.43 ms | 8.48 ms | 46.13 ms | 1906.67 | 
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 4.16 ms | 4.03 ms | 6.82 ms | 13.90 ms | 28.84 ms | 2571.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.79 ms | 4.11 ms | 8.24 ms | 15.15 ms | 56.79 ms | 2966.67 | 
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 4.63 ms | 4.16 ms | 8.34 ms | 15.08 ms | 31.61 ms | 2946.67 | 
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 4.43 ms | 4.33 ms | 7.07 ms | 13.16 ms | 30.86 ms | 2441.33 | 
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 4.63 ms | 4.49 ms | 7.58 ms | 14.45 ms | 30.67 ms | 2699.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 7.01 ms | 4.50 ms | 12.46 ms | 68.13 ms | 141.43 ms | 10754.00 | 
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 4.79 ms | 4.60 ms | 7.60 ms | 14.63 ms | 30.86 ms | 2620.67 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 6.63 ms | 4.73 ms | 13.53 ms | 28.29 ms | 286.69 ms | 8021.33 | 
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 6.97 ms | 4.78 ms | 14.51 ms | 31.43 ms | 255.73 ms | 8898.33 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 6.87 ms | 4.82 ms | 14.53 ms | 29.54 ms | 168.10 ms | 6806.00 | 
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 5.33 ms | 4.83 ms | 8.84 ms | 14.25 ms | 36.70 ms | 2671.67 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 7.01 ms | 4.86 ms | 15.09 ms | 29.93 ms | 172.91 ms | 6524.33 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 6.52 ms | 4.88 ms | 12.44 ms | 26.88 ms | 118.74 ms | 5328.33 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 6.55 ms | 4.92 ms | 12.56 ms | 26.95 ms | 160.72 ms | 5663.00 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 7.40 ms | 4.92 ms | 16.26 ms | 33.15 ms | 165.12 ms | 7256.00 | 
| node (12.3) | [0http](http://github.com/jkyberneees/0http) (1.0) | 6.59 ms | 4.96 ms | 9.85 ms | 21.74 ms | 305.09 ms | 9759.00 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 7.15 ms | 5.06 ms | 14.65 ms | 30.96 ms | 193.54 ms | 6632.00 | 
| node (12.3) | [restana](http://github.com/jkyberneees/ana) (3.1) | 6.87 ms | 5.08 ms | 10.09 ms | 20.76 ms | 350.32 ms | 11347.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.07 ms | 5.13 ms | 10.25 ms | 20.42 ms | 89.34 ms | 4547.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 6.06 ms | 5.29 ms | 8.73 ms | 15.70 ms | 283.16 ms | 6773.33 | 
| node (12.3) | [polka](http://github.com/lukeed/polka) (0.5) | 7.28 ms | 5.33 ms | 10.81 ms | 22.69 ms | 322.08 ms | 11317.00 | 
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 7.57 ms | 5.54 ms | 14.57 ms | 32.60 ms | 216.74 ms | 9917.33 | 
| node (12.3) | [rayo](http://rayo.js.org) (1.3) | 7.14 ms | 5.71 ms | 10.96 ms | 20.96 ms | 237.64 ms | 7181.00 | 
| go (1.12) | [gf](http://goframe.org) (1.6) | 8.97 ms | 6.11 ms | 18.75 ms | 41.54 ms | 187.39 ms | 9013.00 | 
| node (12.3) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 10.94 ms | 7.37 ms | 15.23 ms | 79.84 ms | 574.96 ms | 26058.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 176.21 ms | 7.64 ms | 32.60 ms | 4235.65 ms | 7760.13 ms | 734980.67 | 
| node (12.3) | [fastify](http://fastify.io) (2.4) | 11.76 ms | 8.12 ms | 15.16 ms | 98.67 ms | 574.05 ms | 26302.00 | 
| node (12.3) | [foxify](http://foxify.js.org) (0.10) | 8.94 ms | 8.16 ms | 13.73 ms | 25.74 ms | 315.89 ms | 10770.67 | 
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 9.28 ms | 8.41 ms | 17.20 ms | 31.87 ms | 211.77 ms | 7085.33 | 
| node (12.3) | [koa](http://koajs.com) (2.7) | 10.14 ms | 8.58 ms | 14.56 ms | 35.55 ms | 465.85 ms | 17048.33 | 
| node (12.3) | [restify](http://restify.com) (8.2) | 10.18 ms | 8.90 ms | 13.51 ms | 25.34 ms | 278.96 ms | 7946.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 13.69 ms | 9.71 ms | 29.10 ms | 59.23 ms | 220.55 ms | 12477.33 | 
| node (12.3) | [express](http://expressjs.com) (4.16) | 13.96 ms | 10.58 ms | 18.61 ms | 81.17 ms | 573.40 ms | 25251.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 10.60 ms | 10.70 ms | 12.91 ms | 15.22 ms | 133.63 ms | 2536.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 12.88 ms | 11.13 ms | 23.38 ms | 45.06 ms | 402.75 ms | 12988.00 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 19.90 ms | 14.66 ms | 21.75 ms | 164.92 ms | 1266.78 ms | 52059.67 | 
| python (3.7) | [hug](http://hug.rest) (2.5) | 16.95 ms | 14.88 ms | 29.41 ms | 49.27 ms | 292.13 ms | 11808.00 | 
| python (3.7) | [starlette](http://starlette.io) (0.12) | 17.73 ms | 15.48 ms | 31.41 ms | 50.23 ms | 138.29 ms | 10378.33 | 
| node (12.3) | [hapi](http://hapijs.com) (18.1) | 22.99 ms | 17.56 ms | 30.08 ms | 141.50 ms | 819.63 ms | 39113.67 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.27) | 22.44 ms | 19.81 ms | 39.58 ms | 66.37 ms | 144.63 ms | 13140.67 | 
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 23.57 ms | 21.40 ms | 29.90 ms | 47.27 ms | 897.28 ms | 33473.33 | 
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 65.84 ms | 23.17 ms | 49.18 ms | 1485.77 ms | 3560.69 ms | 255557.67 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 32.67 ms | 24.66 ms | 62.35 ms | 108.23 ms | 291.33 ms | 20962.67 | 
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 38.88 ms | 27.10 ms | 95.75 ms | 199.25 ms | 540.79 ms | 46128.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 27.36 ms | 27.38 ms | 46.15 ms | 62.03 ms | 92.14 ms | 14146.00 | 
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 30.97 ms | 29.29 ms | 41.85 ms | 50.39 ms | 232.83 ms | 7698.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 35.34 ms | 30.88 ms | 58.35 ms | 96.51 ms | 558.89 ms | 25350.00 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.16) | 39.82 ms | 33.86 ms | 76.84 ms | 117.33 ms | 228.19 ms | 24792.33 | 
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 35.72 ms | 35.79 ms | 42.64 ms | 48.11 ms | 187.33 ms | 6803.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 52.10 ms | 41.64 ms | 101.97 ms | 190.02 ms | 384.06 ms | 39179.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 66.89 ms | 59.22 ms | 120.22 ms | 189.38 ms | 250.47 ms | 39583.33 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 73.97 ms | 71.02 ms | 125.69 ms | 184.51 ms | 250.74 ms | 38721.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 104.63 ms | 92.01 ms | 149.84 ms | 424.76 ms | 1326.91 ms | 84513.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 102.54 ms | 95.62 ms | 153.08 ms | 245.52 ms | 1221.51 ms | 58986.67 | 
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 114.69 ms | 96.31 ms | 178.96 ms | 385.81 ms | 1453.14 ms | 83363.00 | 
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 115.41 ms | 112.99 ms | 159.16 ms | 207.16 ms | 310.52 ms | 34071.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (actix-web) (rust)


:three: (japronto) (python)


:four: (fasthttprouter) (go)


:five: (kore) (c)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 349796.00 | 202.34 MB |
| rust (1.35) | [actix-web](http://actix.rs) (0.7) | 301655.33 | 343.10 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 282586.33 | 338.35 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 271003.67 | 438.39 MB |
| c (99) | [kore](http://kore.io) (3.1) | 257123.00 | 668.02 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 249314.33 | 241.91 MB |
| java (8) | [act](http://actframework.org) (1.8) | 236792.67 | 408.95 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 235712.00 | 267.63 MB |
| rust (1.35) | [gotham](http://gotham.rs) (0.3) | 228824.67 | 467.85 MB |
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 228009.33 | 214.14 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 224864.33 | 451.79 MB |
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 212280.67 | 346.88 MB |
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 212095.00 | 227.02 MB |
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 202832.33 | 190.80 MB |
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 196640.33 | 245.43 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 184662.00 | 106.89 MB |
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 177750.33 | 290.57 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 165631.00 | 221.15 MB |
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 162514.00 | 215.82 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 161213.33 | 214.38 MB |
| node (12.3) | [0http](http://github.com/jkyberneees/0http) (1.0) | 160220.00 | 240.11 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 160108.00 | 260.49 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 158837.33 | 210.31 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 157888.67 | 276.73 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 156016.67 | 273.42 MB |
| node (12.3) | [restana](http://github.com/jkyberneees/ana) (3.1) | 153739.67 | 230.36 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 153171.67 | 205.40 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 151471.67 | 202.34 MB |
| rust (1.35) | [iron](http://ironframework.io) (0.6) | 151417.00 | 190.96 MB |
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 148846.67 | 232.14 MB |
| node (12.3) | [polka](http://github.com/lukeed/polka) (0.5) | 147839.67 | 221.51 MB |
| node (12.3) | [rayo](http://rayo.js.org) (1.3) | 139952.00 | 209.73 MB |
| go (1.12) | [gf](http://goframe.org) (1.6) | 125056.33 | 189.31 MB |
| node (12.3) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 118724.67 | 177.82 MB |
| node (12.3) | [foxify](http://foxify.js.org) (0.10) | 116227.67 | 244.42 MB |
| node (12.3) | [fastify](http://fastify.io) (2.4) | 115721.00 | 294.95 MB |
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 109457.67 | 256.75 MB |
| node (12.3) | [koa](http://koajs.com) (2.7) | 108792.67 | 230.38 MB |
| node (12.3) | [restify](http://restify.com) (8.2) | 98766.67 | 173.29 MB |
| rust (1.35) | [nickel](http://nickel-org.github.io) (0.11) | 95192.33 | 189.39 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 91851.33 | 86.31 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 86746.00 | 151.76 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 85128.00 | 209.71 MB |
| node (12.3) | [express](http://expressjs.com) (4.16) | 83440.33 | 204.41 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 79030.67 | 169.60 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 64061.33 | 107.34 MB |
| php (7.3) | [symfony](http://symfony.com) (4.3) | 63027.33 | 312.50 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 61953.33 | 321.73 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 61661.00 | 305.48 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 61628.00 | 305.70 MB |
| python (3.7) | [hug](http://hug.rest) (2.5) | 60070.67 | 148.78 MB |
| python (3.7) | [starlette](http://starlette.io) (0.12) | 57851.33 | 124.71 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 57460.67 | 285.33 MB |
| node (12.3) | [hapi](http://hapijs.com) (18.1) | 53199.33 | 138.15 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 48999.00 | 74.60 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 47590.00 | 247.77 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.27) | 46695.67 | 101.07 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 45345.00 | 84.09 MB |
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 45328.67 | 56.81 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 42129.33 | 40.18 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 37020.00 | 83.94 MB |
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 36420.00 | 68.60 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 32836.33 | 61.03 MB |
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 31668.33 | 38.87 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 30824.33 | 17.76 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 29012.33 | 71.44 MB |
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 27667.00 | 50.53 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.16) | 26278.33 | 50.76 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 20859.67 | 37.14 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 20259.67 | 11.68 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 16713.67 | 126.37 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 15423.33 | 30.74 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 14968.33 | 38.81 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 13666.33 | 29.78 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 9764.00 | 28.27 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 9647.00 | 28.47 MB |
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 8787.67 | 20.03 MB |
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 8608.67 | 22.17 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3817.00 | 11.68 MB |
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

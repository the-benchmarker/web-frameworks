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
Last update: 2019-05-17
```
OS: Linux (version: 5.0.9-301.fc30.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: iron (rust)


:five: flame (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 0.19 ms | 0.17 ms | 0.25 ms | 0.49 ms | 18.43 ms | 232.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 8.65 ms | 0.54 ms | 30.17 ms | 65.75 ms | 188.29 ms | 15303.33 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 10.78 ms | 0.67 ms | 36.24 ms | 73.51 ms | 187.00 ms | 17711.67 | 
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 1.04 ms | 0.93 ms | 1.64 ms | 3.72 ms | 104.99 ms | 1246.00 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 14.78 ms | 1.18 ms | 44.72 ms | 88.07 ms | 202.42 ms | 21245.33 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 16.21 ms | 1.19 ms | 49.71 ms | 93.38 ms | 278.27 ms | 23509.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 19.26 ms | 1.70 ms | 56.78 ms | 108.45 ms | 239.78 ms | 26726.67 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 94.27 ms | 1.71 ms | 207.29 ms | 1251.44 ms | 7566.56 ms | 337802.67 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 199.28 ms | 1.88 ms | 496.63 ms | 3648.72 ms | 7790.36 ms | 670435.67 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 161.90 ms | 1.90 ms | 369.94 ms | 2339.82 ms | 6399.51 ms | 475460.00 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 184.43 ms | 2.16 ms | 468.71 ms | 2924.29 ms | 7792.04 ms | 623985.00 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 169.38 ms | 2.65 ms | 438.52 ms | 2803.23 ms | 7622.33 ms | 534732.67 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 177.99 ms | 3.16 ms | 391.71 ms | 3079.45 ms | 7826.52 ms | 599838.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 144.67 ms | 4.36 ms | 14.14 ms | 3945.17 ms | 6594.19 ms | 683340.33 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 5.99 ms | 5.10 ms | 12.68 ms | 24.39 ms | 108.86 ms | 5497.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 6.82 ms | 6.03 ms | 13.09 ms | 25.27 ms | 74.89 ms | 5276.67 | 
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 7.15 ms | 6.31 ms | 12.65 ms | 23.85 ms | 68.56 ms | 4582.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 11.63 ms | 7.45 ms | 18.40 ms | 76.25 ms | 892.26 ms | 32829.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 8.67 ms | 7.59 ms | 13.05 ms | 23.36 ms | 111.77 ms | 4349.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 9.73 ms | 7.71 ms | 18.26 ms | 32.59 ms | 77.58 ms | 6708.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 9.31 ms | 8.64 ms | 14.29 ms | 24.63 ms | 116.70 ms | 4668.33 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 10.45 ms | 9.57 ms | 14.95 ms | 29.45 ms | 170.00 ms | 5597.00 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 12.72 ms | 9.62 ms | 23.73 ms | 76.58 ms | 145.59 ms | 12801.67 | 
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 11.37 ms | 9.81 ms | 20.08 ms | 38.51 ms | 211.40 ms | 9479.00 | 
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 12.29 ms | 10.61 ms | 21.14 ms | 39.56 ms | 96.41 ms | 7373.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 13.44 ms | 11.24 ms | 20.82 ms | 44.60 ms | 228.60 ms | 9412.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 178.17 ms | 11.27 ms | 36.22 ms | 4041.82 ms | 7191.39 ms | 708035.00 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 79.45 ms | 12.66 ms | 252.60 ms | 633.58 ms | 1537.61 ms | 136681.00 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 17.92 ms | 14.09 ms | 32.58 ms | 80.21 ms | 253.07 ms | 14980.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 17.43 ms | 14.18 ms | 23.37 ms | 77.40 ms | 758.17 ms | 29015.00 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 20.09 ms | 15.02 ms | 39.87 ms | 88.54 ms | 237.99 ms | 17171.33 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 20.25 ms | 15.32 ms | 39.15 ms | 88.76 ms | 254.29 ms | 17219.00 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 21.48 ms | 15.36 ms | 44.70 ms | 98.79 ms | 320.04 ms | 19535.00 | 
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 21.86 ms | 15.65 ms | 43.23 ms | 108.49 ms | 284.86 ms | 20649.00 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 21.17 ms | 15.81 ms | 40.99 ms | 95.00 ms | 237.53 ms | 17900.33 | 
| node (12.2) | [restana](http://github.com/jkyberneees/ana) (2.13) | 23.29 ms | 15.90 ms | 30.75 ms | 228.31 ms | 919.93 ms | 48766.00 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 19.76 ms | 16.00 ms | 34.49 ms | 80.14 ms | 254.63 ms | 14611.00 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 21.03 ms | 16.79 ms | 37.42 ms | 86.66 ms | 216.41 ms | 15815.00 | 
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 23.22 ms | 19.50 ms | 42.83 ms | 107.77 ms | 437.67 ms | 22353.33 | 
| go (1.12) | [gf](http://goframe.org) (1.6) | 25.98 ms | 19.59 ms | 50.58 ms | 113.40 ms | 267.04 ms | 21885.67 | 
| node (12.2) | [polka](http://github.com/lukeed/polka) (0.5) | 33.26 ms | 20.44 ms | 37.13 ms | 480.74 ms | 1315.84 ms | 81336.00 | 
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 26.62 ms | 20.85 ms | 49.15 ms | 88.15 ms | 260.18 ms | 17559.67 | 
| node (12.2) | [rayo](http://rayo.js.org) (1.2) | 37.87 ms | 21.09 ms | 37.80 ms | 652.08 ms | 1527.63 ms | 106297.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 22.11 ms | 21.81 ms | 26.35 ms | 33.46 ms | 56.41 ms | 3607.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 33.41 ms | 23.02 ms | 68.83 ms | 118.33 ms | 341.74 ms | 24945.67 | 
| node (12.2) | [foxify](http://foxify.js.org) (0.10) | 42.82 ms | 25.46 ms | 43.62 ms | 638.20 ms | 1536.17 ms | 103943.67 | 
| node (12.2) | [fastify](http://fastify.io) (2.3) | 53.18 ms | 26.88 ms | 52.78 ms | 858.97 ms | 1748.57 ms | 138187.33 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 65.25 ms | 27.59 ms | 49.31 ms | 1354.51 ms | 3496.16 ms | 228498.67 | 
| node (12.2) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 62.18 ms | 27.77 ms | 60.27 ms | 1062.57 ms | 2057.76 ms | 171380.00 | 
| node (12.2) | [koa](http://koajs.com) (2.7) | 54.86 ms | 27.87 ms | 48.67 ms | 929.77 ms | 1922.34 ms | 150012.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 33.64 ms | 30.01 ms | 60.36 ms | 105.10 ms | 557.71 ms | 25079.67 | 
| python (3.7) | [hug](http://hug.rest) (2.5) | 46.47 ms | 32.77 ms | 98.88 ms | 154.57 ms | 377.71 ms | 33433.00 | 
| node (12.2) | [express](http://expressjs.com) (4.16) | 68.00 ms | 34.00 ms | 58.58 ms | 1166.65 ms | 2311.37 ms | 185479.67 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 45.02 ms | 38.56 ms | 80.38 ms | 139.18 ms | 275.64 ms | 27528.33 | 
| node (12.2) | [restify](http://restify.com) (8.2) | 47.70 ms | 39.23 ms | 68.98 ms | 220.73 ms | 934.52 ms | 47214.00 | 
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 52.46 ms | 51.12 ms | 68.96 ms | 107.21 ms | 273.16 ms | 16607.00 | 
| node (12.2) | [hapi](http://hapijs.com) (18.1) | 170.72 ms | 55.17 ms | 234.71 ms | 2503.52 ms | 3833.95 ms | 437190.00 | 
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 61.92 ms | 57.97 ms | 83.99 ms | 181.51 ms | 356.80 ms | 26664.67 | 
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 61.03 ms | 58.30 ms | 81.67 ms | 113.38 ms | 305.51 ms | 16641.67 | 
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 60.08 ms | 58.93 ms | 77.50 ms | 99.46 ms | 503.64 ms | 18951.00 | 
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 61.79 ms | 60.05 ms | 80.16 ms | 123.32 ms | 450.17 ms | 20773.00 | 
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 62.88 ms | 61.14 ms | 83.65 ms | 115.44 ms | 533.98 ms | 21219.00 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.2) | 70.30 ms | 61.31 ms | 122.95 ms | 184.92 ms | 337.34 ms | 37377.00 | 
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 65.91 ms | 63.87 ms | 86.25 ms | 106.78 ms | 312.61 ms | 15750.33 | 
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 177.09 ms | 65.25 ms | 115.28 ms | 3343.02 ms | 6831.05 ms | 555543.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 68.16 ms | 66.78 ms | 107.51 ms | 143.28 ms | 237.82 ms | 28950.67 | 
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 70.22 ms | 68.09 ms | 89.42 ms | 153.40 ms | 322.04 ms | 22727.00 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 105.98 ms | 68.41 ms | 226.54 ms | 384.57 ms | 959.32 ms | 86805.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 85.07 ms | 77.84 ms | 129.23 ms | 187.76 ms | 614.66 ms | 35043.67 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 86.51 ms | 79.46 ms | 134.61 ms | 204.70 ms | 357.63 ms | 35879.00 | 
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 98.17 ms | 89.15 ms | 230.68 ms | 419.12 ms | 975.18 ms | 101837.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 125.22 ms | 113.99 ms | 220.49 ms | 356.10 ms | 625.16 ms | 72067.00 | 
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 132.13 ms | 128.54 ms | 190.24 ms | 253.80 ms | 358.75 ms | 44540.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 157.44 ms | 145.48 ms | 261.89 ms | 365.50 ms | 503.23 ms | 72731.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 182.41 ms | 166.65 ms | 293.40 ms | 391.39 ms | 646.88 ms | 71488.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 186.61 ms | 179.65 ms | 259.99 ms | 358.22 ms | 896.94 ms | 60032.67 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 223.27 ms | 208.08 ms | 360.13 ms | 506.16 ms | 664.31 ms | 96534.00 | 
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 576.97 ms | 488.37 ms | 652.17 ms | 4257.29 ms | 6302.28 ms | 634776.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (jester) (nim)


:five: (kore) (c)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 166593.33 | 96.31 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 142544.67 | 170.50 MB |
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 131025.67 | 148.95 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 117012.33 | 235.19 MB |
| c (99) | [kore](http://kore.io) (3.1) | 116160.00 | 301.70 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 108233.67 | 105.11 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 107316.33 | 121.76 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 92008.33 | 148.43 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 89456.67 | 51.72 MB |
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 88325.00 | 181.18 MB |
| java (8) | [act](http://actframework.org) (1.8) | 84222.67 | 145.37 MB |
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 81329.67 | 87.13 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 61240.67 | 82.12 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 59399.00 | 96.70 MB |
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 57578.67 | 72.52 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 55097.00 | 96.76 MB |
| node (12.2) | [restana](http://github.com/jkyberneees/ana) (2.13) | 54178.67 | 81.23 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 53946.33 | 94.68 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 53854.33 | 71.95 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 52624.00 | 70.24 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 52402.00 | 70.42 MB |
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 52281.33 | 70.24 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 50083.67 | 66.57 MB |
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 47037.33 | 93.62 MB |
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 46954.33 | 73.20 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 44599.00 | 41.93 MB |
| node (12.2) | [rayo](http://rayo.js.org) (1.2) | 43036.33 | 64.51 MB |
| go (1.12) | [gf](http://goframe.org) (1.6) | 42897.33 | 65.30 MB |
| node (12.2) | [polka](http://github.com/lukeed/polka) (0.5) | 42860.00 | 64.22 MB |
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 39472.67 | 92.47 MB |
| node (12.2) | [foxify](http://foxify.js.org) (0.10) | 35253.67 | 74.10 MB |
| node (12.2) | [fastify](http://fastify.io) (2.3) | 33923.00 | 90.92 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 33778.33 | 72.50 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 33094.33 | 81.49 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 32438.67 | 54.37 MB |
| node (12.2) | [koa](http://koajs.com) (2.7) | 32205.00 | 68.18 MB |
| node (12.2) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 31797.33 | 47.66 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 30801.00 | 54.02 MB |
| node (12.2) | [express](http://expressjs.com) (4.16) | 26628.00 | 65.26 MB |
| python (3.7) | [hug](http://hug.rest) (2.5) | 23749.67 | 58.90 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 23155.00 | 50.04 MB |
| node (12.2) | [restify](http://restify.com) (8.2) | 22617.00 | 39.69 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 19082.33 | 94.99 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 18958.67 | 94.32 MB |
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 18760.00 | 17.62 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 18319.33 | 29.42 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 17982.67 | 93.56 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 16690.00 | 83.12 MB |
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 16241.00 | 19.96 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 16208.00 | 80.69 MB |
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 16139.00 | 26.37 MB |
| node (12.2) | [hapi](http://hapijs.com) (18.1) | 15982.00 | 41.45 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 15941.00 | 83.05 MB |
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 15827.33 | 14.87 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 15597.00 | 29.04 MB |
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 15373.00 | 25.08 MB |
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 14960.00 | 18.49 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 14763.67 | 14.11 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 14651.67 | 33.24 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.2) | 14372.67 | 31.08 MB |
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 14069.33 | 25.74 MB |
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 13151.33 | 17.37 MB |
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 13049.00 | 24.67 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 11838.67 | 6.85 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 11555.67 | 28.49 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 11397.67 | 21.97 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 11295.67 | 21.00 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 8659.00 | 5.01 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 8113.00 | 14.48 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 7901.67 | 59.89 MB |
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 7481.67 | 19.31 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 6605.33 | 17.17 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 6342.33 | 12.67 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 5302.33 | 15.58 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 5143.00 | 14.93 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 4429.00 | 9.67 MB |
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 1857.00 | 4.57 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 1593.00 | 4.92 MB |
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

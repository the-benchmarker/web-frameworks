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
Last update: 2019-05-15
```
OS: Linux (version: 5.0.9-301.fc30.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: laravel (php)


:five: zend-expressive (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 0.08 ms | 0.08 ms | 0.11 ms | 0.14 ms | 6.29 ms | 47.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.19) | 3.46 ms | 0.20 ms | 12.37 ms | 30.77 ms | 87.26 ms | 6816.00 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.71 ms | 0.26 ms | 16.68 ms | 36.47 ms | 89.81 ms | 8452.33 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 127.29 ms | 0.39 ms | 303.96 ms | 2553.87 ms | 6894.87 ms | 445861.67 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 142.51 ms | 0.40 ms | 242.15 ms | 3521.08 ms | 6891.03 ms | 564461.33 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 193.16 ms | 0.40 ms | 346.08 ms | 4186.24 ms | 6977.34 ms | 700829.33 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 175.59 ms | 0.40 ms | 307.45 ms | 3835.22 ms | 6930.58 ms | 645386.00 | 
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 0.40 ms | 0.40 ms | 0.65 ms | 0.92 ms | 79.40 ms | 504.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 157.42 ms | 0.41 ms | 263.17 ms | 3606.00 ms | 7157.75 ms | 602130.33 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 151.97 ms | 0.43 ms | 295.20 ms | 3228.25 ms | 6900.63 ms | 549289.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 7.26 ms | 0.44 ms | 24.09 ms | 49.54 ms | 115.48 ms | 11747.33 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 8.26 ms | 0.46 ms | 27.93 ms | 56.75 ms | 144.29 ms | 13678.67 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 9.63 ms | 0.66 ms | 29.86 ms | 58.81 ms | 171.26 ms | 14254.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 139.12 ms | 1.74 ms | 73.83 ms | 4032.70 ms | 6594.74 ms | 660337.33 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 3.01 ms | 1.79 ms | 6.91 ms | 14.07 ms | 37.56 ms | 3124.33 | 
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 3.59 ms | 2.74 ms | 7.48 ms | 15.70 ms | 33.59 ms | 3254.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.59 ms | 2.81 ms | 7.18 ms | 15.79 ms | 35.19 ms | 3300.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 10.20 ms | 3.06 ms | 7.47 ms | 243.81 ms | 991.59 ms | 53598.67 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 41.58 ms | 3.52 ms | 139.20 ms | 373.48 ms | 1098.78 ms | 79800.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.70 ms | 3.70 ms | 9.38 ms | 17.04 ms | 41.35 ms | 3711.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.58 ms | 3.83 ms | 5.50 ms | 8.20 ms | 21.52 ms | 1832.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.65 ms | 3.97 ms | 7.59 ms | 14.15 ms | 39.04 ms | 2752.00 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.33 ms | 3.97 ms | 6.64 ms | 14.19 ms | 51.25 ms | 2649.00 | 
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 4.84 ms | 4.25 ms | 8.95 ms | 19.87 ms | 223.79 ms | 5438.33 | 
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 4.97 ms | 4.46 ms | 9.06 ms | 16.86 ms | 37.67 ms | 3282.00 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 7.54 ms | 4.55 ms | 13.10 ms | 77.21 ms | 126.11 ms | 12248.00 | 
| node (12.2) | [restana](http://github.com/jkyberneees/ana) (2.13) | 6.76 ms | 5.26 ms | 10.01 ms | 20.31 ms | 250.87 ms | 7357.67 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 8.18 ms | 5.69 ms | 16.96 ms | 34.76 ms | 167.17 ms | 7385.33 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 8.27 ms | 5.74 ms | 17.24 ms | 35.57 ms | 128.03 ms | 7581.33 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 8.19 ms | 5.81 ms | 16.51 ms | 34.91 ms | 172.95 ms | 7443.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 6.54 ms | 5.83 ms | 9.62 ms | 17.55 ms | 217.28 ms | 4953.33 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 8.50 ms | 5.89 ms | 16.97 ms | 37.62 ms | 352.76 ms | 9861.00 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 9.02 ms | 5.90 ms | 19.16 ms | 40.13 ms | 213.23 ms | 10408.00 | 
| node (12.2) | [polka](http://github.com/lukeed/polka) (0.5) | 8.20 ms | 5.96 ms | 11.44 ms | 25.38 ms | 412.02 ms | 15664.33 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 7.92 ms | 6.10 ms | 14.51 ms | 30.90 ms | 187.19 ms | 6535.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.87 ms | 6.19 ms | 11.16 ms | 21.26 ms | 81.96 ms | 4578.67 | 
| node (12.2) | [rayo](http://rayo.js.org) (1.2) | 8.13 ms | 6.57 ms | 12.35 ms | 24.51 ms | 309.43 ms | 10469.00 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 8.55 ms | 6.63 ms | 15.78 ms | 33.55 ms | 210.71 ms | 6687.00 | 
| go (1.12) | [gf](http://goframe.org) (1.6) | 10.20 ms | 7.26 ms | 20.66 ms | 44.95 ms | 212.86 ms | 10592.00 | 
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 8.54 ms | 7.33 ms | 15.16 ms | 27.85 ms | 167.03 ms | 6254.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 148.88 ms | 7.60 ms | 26.54 ms | 3573.10 ms | 6955.01 ms | 624767.67 | 
| node (12.2) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 11.04 ms | 8.06 ms | 16.66 ms | 44.42 ms | 520.19 ms | 20541.00 | 
| node (12.2) | [fastify](http://fastify.io) (2.3) | 12.05 ms | 8.62 ms | 15.82 ms | 54.85 ms | 630.87 ms | 27401.67 | 
| node (12.2) | [foxify](http://foxify.js.org) (0.10) | 10.04 ms | 8.86 ms | 14.53 ms | 30.83 ms | 416.97 ms | 14451.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 11.80 ms | 9.27 ms | 23.47 ms | 45.81 ms | 228.26 ms | 9506.33 | 
| node (12.2) | [koa](http://koajs.com) (2.7) | 12.33 ms | 9.28 ms | 15.60 ms | 66.67 ms | 622.50 ms | 28050.67 | 
| node (12.2) | [restify](http://restify.com) (8.2) | 12.55 ms | 10.97 ms | 16.40 ms | 42.38 ms | 391.71 ms | 13827.33 | 
| python (3.7) | [hug](http://hug.rest) (2.5) | 15.22 ms | 11.42 ms | 28.87 ms | 51.99 ms | 296.53 ms | 13919.67 | 
| kotlin (1.3) | [ktor](http://ktor.io) (1.1) | 14.27 ms | 11.56 ms | 24.88 ms | 41.63 ms | 162.04 ms | 7786.33 | 
| node (12.2) | [express](http://expressjs.com) (4.16) | 15.61 ms | 11.86 ms | 20.52 ms | 92.51 ms | 706.71 ms | 31264.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 12.02 ms | 12.04 ms | 14.30 ms | 17.69 ms | 40.70 ms | 2154.67 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 13.57 ms | 12.24 ms | 24.23 ms | 37.32 ms | 93.99 ms | 7860.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 13.95 ms | 12.48 ms | 25.76 ms | 46.23 ms | 210.20 ms | 9722.00 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 18.06 ms | 13.34 ms | 23.11 ms | 131.46 ms | 951.96 ms | 42417.33 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.2) | 19.34 ms | 18.59 ms | 30.85 ms | 41.69 ms | 78.40 ms | 8760.33 | 
| node (12.2) | [hapi](http://hapijs.com) (18.1) | 28.66 ms | 19.84 ms | 32.23 ms | 327.01 ms | 1042.43 ms | 60099.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 22.53 ms | 20.93 ms | 34.55 ms | 50.75 ms | 94.14 ms | 9383.33 | 
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 27.73 ms | 25.48 ms | 36.71 ms | 55.86 ms | 395.85 ms | 13290.00 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 32.00 ms | 25.55 ms | 61.58 ms | 87.32 ms | 314.55 ms | 18500.67 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 28.85 ms | 26.39 ms | 50.86 ms | 77.46 ms | 146.46 ms | 16314.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 32.85 ms | 28.44 ms | 52.34 ms | 79.46 ms | 561.77 ms | 21759.67 | 
| swift (5.0) | [kitura](http://kitura.io) (2.6) | 32.86 ms | 28.51 ms | 45.22 ms | 118.88 ms | 863.96 ms | 35741.67 | 
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 30.03 ms | 29.43 ms | 38.88 ms | 73.39 ms | 318.80 ms | 12469.33 | 
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 30.73 ms | 29.58 ms | 39.52 ms | 48.83 ms | 258.18 ms | 10341.00 | 
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 32.25 ms | 30.24 ms | 41.50 ms | 76.73 ms | 321.91 ms | 14618.67 | 
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 33.92 ms | 32.78 ms | 44.55 ms | 53.75 ms | 240.22 ms | 10099.33 | 
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 37.15 ms | 36.40 ms | 44.15 ms | 50.37 ms | 201.91 ms | 5925.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 50.17 ms | 40.48 ms | 97.42 ms | 170.70 ms | 296.69 ms | 35005.00 | 
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 45.94 ms | 44.13 ms | 57.71 ms | 67.25 ms | 284.56 ms | 13064.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 57.48 ms | 55.40 ms | 97.77 ms | 134.92 ms | 190.34 ms | 29141.00 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 76.17 ms | 68.72 ms | 118.74 ms | 156.94 ms | 250.43 ms | 30712.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 88.96 ms | 78.96 ms | 149.00 ms | 213.73 ms | 936.48 ms | 48549.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 88.61 ms | 86.10 ms | 105.79 ms | 228.93 ms | 1053.19 ms | 46254.33 | 
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 122.43 ms | 121.04 ms | 163.94 ms | 205.72 ms | 304.26 ms | 32816.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (kore) (c)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 353301.00 | 204.32 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 284079.33 | 339.88 MB |
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 278184.67 | 316.27 MB |
| c (99) | [kore](http://kore.io) (3.1) | 257495.33 | 668.35 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 247692.67 | 240.22 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 227076.00 | 257.87 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 225279.33 | 452.46 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 217237.00 | 349.90 MB |
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 210293.00 | 430.60 MB |
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 198164.33 | 211.80 MB |
| java (8) | [act](http://actframework.org) (1.8) | 190322.67 | 328.68 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 183515.67 | 106.23 MB |
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 147982.67 | 187.42 MB |
| node (12.2) | [restana](http://github.com/jkyberneees/ana) (2.13) | 147739.67 | 221.43 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 142715.33 | 232.26 MB |
| node (12.2) | [polka](http://github.com/lukeed/polka) (0.5) | 134877.00 | 202.26 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 133668.33 | 234.30 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 132559.00 | 232.34 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 131842.67 | 175.63 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 130302.67 | 173.23 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 128204.67 | 172.09 MB |
| node (12.2) | [rayo](http://rayo.js.org) (1.2) | 127665.33 | 191.33 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 127146.67 | 169.96 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 121743.67 | 162.33 MB |
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 118020.67 | 276.76 MB |
| go (1.12) | [gf](http://goframe.org) (1.6) | 110558.00 | 167.27 MB |
| node (12.2) | [fastify](http://fastify.io) (2.3) | 108643.67 | 278.66 MB |
| node (12.2) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 107613.67 | 161.34 MB |
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 106417.67 | 211.65 MB |
| node (12.2) | [foxify](http://foxify.js.org) (0.10) | 106403.33 | 223.77 MB |
| node (12.2) | [koa](http://koajs.com) (2.7) | 98562.33 | 208.81 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 91340.67 | 224.92 MB |
| node (12.2) | [restify](http://restify.com) (8.2) | 84465.33 | 148.21 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 81734.67 | 76.84 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 79460.67 | 139.06 MB |
| node (12.2) | [express](http://expressjs.com) (4.16) | 76632.00 | 187.65 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 74253.67 | 159.95 MB |
| kotlin (1.3) | [ktor](http://ktor.io) (1.1) | 71678.67 | 111.58 MB |
| python (3.7) | [hug](http://hug.rest) (2.5) | 70716.33 | 175.26 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 68923.33 | 147.82 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 67519.67 | 113.33 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 54823.33 | 272.05 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 54691.00 | 271.07 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 54384.33 | 269.93 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 51968.67 | 258.04 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.2) | 51509.33 | 111.24 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 48573.33 | 252.02 MB |
| node (12.2) | [hapi](http://hapijs.com) (18.1) | 48272.33 | 124.75 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 46215.67 | 71.62 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 44459.33 | 231.40 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 44419.33 | 100.77 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.19) | 37075.33 | 35.35 MB |
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 35946.67 | 33.71 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 35532.00 | 68.59 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 33185.67 | 61.75 MB |
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 32792.00 | 30.73 MB |
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 32299.00 | 40.26 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.6) | 31853.00 | 59.03 MB |
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 31039.00 | 50.66 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 30624.00 | 75.34 MB |
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 29094.33 | 53.13 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 27184.33 | 15.68 MB |
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 27075.33 | 33.16 MB |
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 21779.67 | 35.48 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 21389.67 | 38.07 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 17556.33 | 10.13 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 17518.00 | 34.92 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 15479.00 | 117.15 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 13289.67 | 34.45 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 13079.33 | 28.48 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 11219.33 | 33.24 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 11133.33 | 32.32 MB |
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 8109.67 | 20.87 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3079.67 | 9.44 MB |
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

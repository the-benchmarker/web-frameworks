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
Last update: 2019-02-20
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: zend-framework (php)


:five: iron (rust)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.09 ms | 0.08 ms | 0.13 ms | 0.17 ms | 1.83 ms | 33.00 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 5.16 ms | 0.31 ms | 17.95 ms | 37.49 ms | 88.61 ms | 8865.00 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 6.03 ms | 0.33 ms | 21.34 ms | 43.18 ms | 119.29 ms | 10418.33 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 158.48 ms | 0.46 ms | 350.63 ms | 3271.14 ms | 6889.51 ms | 557358.67 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.51 ms | 0.49 ms | 0.86 ms | 1.24 ms | 23.03 ms | 327.33 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 206.35 ms | 0.50 ms | 367.31 ms | 4385.68 ms | 7757.01 ms | 741677.00 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 187.18 ms | 0.51 ms | 328.96 ms | 4530.86 ms | 7611.22 ms | 722744.00 | 
| php (7.3) | [laravel](http://laravel.com) (5.7) | 161.34 ms | 0.51 ms | 389.06 ms | 2903.77 ms | 7346.45 ms | 541994.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 8.59 ms | 0.54 ms | 27.84 ms | 55.01 ms | 157.28 ms | 13326.67 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 9.47 ms | 0.56 ms | 31.17 ms | 60.81 ms | 175.60 ms | 14976.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 181.75 ms | 0.56 ms | 357.54 ms | 3825.51 ms | 7252.95 ms | 652374.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 11.04 ms | 0.72 ms | 34.41 ms | 66.79 ms | 171.07 ms | 16383.00 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 167.70 ms | 1.08 ms | 294.12 ms | 3710.09 ms | 7341.26 ms | 634533.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 55.50 ms | 2.29 ms | 5.06 ms | 1694.12 ms | 5506.34 ms | 350692.33 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 4.07 ms | 3.50 ms | 8.06 ms | 17.20 ms | 41.80 ms | 3612.33 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 4.05 ms | 3.78 ms | 5.99 ms | 10.00 ms | 86.94 ms | 1971.00 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 4.45 ms | 3.81 ms | 7.95 ms | 16.95 ms | 134.64 ms | 5699.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.91 ms | 3.83 ms | 10.06 ms | 19.28 ms | 41.70 ms | 4081.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.28 ms | 4.58 ms | 6.19 ms | 10.79 ms | 100.08 ms | 2268.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.7) | 8.57 ms | 4.81 ms | 15.39 ms | 85.11 ms | 196.20 ms | 14331.00 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 5.10 ms | 4.83 ms | 8.63 ms | 16.59 ms | 35.20 ms | 3114.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 5.75 ms | 4.89 ms | 9.77 ms | 20.39 ms | 121.98 ms | 6895.00 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.59 ms | 5.11 ms | 8.01 ms | 17.46 ms | 183.74 ms | 6672.00 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 5.88 ms | 5.19 ms | 10.16 ms | 20.91 ms | 232.06 ms | 5936.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.61 ms | 5.39 ms | 9.30 ms | 15.58 ms | 79.63 ms | 3177.67 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 41.18 ms | 5.73 ms | 135.27 ms | 344.00 ms | 833.20 ms | 74147.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 50.03 ms | 6.26 ms | 10.76 ms | 1843.72 ms | 2791.32 ms | 275283.33 | 
| node (11.9) | [restana](http://github.com/jkyberneees/ana) (2.8) | 9.43 ms | 7.32 ms | 13.39 ms | 43.27 ms | 478.85 ms | 18437.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 8.28 ms | 7.72 ms | 12.93 ms | 24.27 ms | 99.04 ms | 5155.33 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 9.04 ms | 8.05 ms | 13.40 ms | 27.08 ms | 291.39 ms | 7798.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 8.72 ms | 8.29 ms | 11.63 ms | 21.12 ms | 288.85 ms | 6733.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 169.19 ms | 8.38 ms | 247.68 ms | 3703.31 ms | 7750.11 ms | 635781.67 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 9.37 ms | 8.74 ms | 14.04 ms | 27.24 ms | 120.20 ms | 4877.00 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 9.61 ms | 8.75 ms | 14.63 ms | 30.42 ms | 175.62 ms | 6111.00 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 10.87 ms | 9.06 ms | 15.80 ms | 65.83 ms | 234.09 ms | 11354.33 | 
| node (11.9) | [polka](http://github.com/lukeed/polka) (0.5) | 11.09 ms | 9.21 ms | 15.10 ms | 45.35 ms | 500.79 ms | 19426.33 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 10.42 ms | 9.29 ms | 15.49 ms | 32.20 ms | 249.02 ms | 9167.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (4.0) | 10.44 ms | 9.34 ms | 16.05 ms | 34.46 ms | 186.72 ms | 7292.33 | 
| node (11.9) | [rayo](http://rayo.js.org) (1.2) | 12.49 ms | 9.52 ms | 16.45 ms | 89.02 ms | 554.98 ms | 24603.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 11.00 ms | 9.84 ms | 16.73 ms | 33.23 ms | 234.55 ms | 6845.33 | 
| node (11.9) | [foxify](http://foxify.js.org) (0.10) | 15.61 ms | 10.91 ms | 19.50 ms | 124.59 ms | 718.56 ms | 33998.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 15.27 ms | 11.82 ms | 31.26 ms | 55.13 ms | 215.61 ms | 11661.33 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 13.81 ms | 11.84 ms | 22.58 ms | 41.70 ms | 293.79 ms | 10989.33 | 
| node (11.9) | [fastify](http://fastify.io) (1.14) | 16.99 ms | 11.96 ms | 20.07 ms | 162.73 ms | 810.11 ms | 40780.67 | 
| node (11.9) | [koa](http://koajs.com) (2.7) | 15.86 ms | 12.03 ms | 20.90 ms | 86.72 ms | 681.16 ms | 30155.00 | 
| go (1.11) | [gf](http://goframe.org) (1.5) | 13.44 ms | 12.88 ms | 18.07 ms | 37.50 ms | 237.40 ms | 7299.67 | 
| node (11.9) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 24.06 ms | 13.67 ms | 25.48 ms | 360.25 ms | 1122.72 ms | 67608.33 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 19.64 ms | 13.70 ms | 23.90 ms | 159.85 ms | 1202.12 ms | 48642.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 13.50 ms | 13.71 ms | 16.41 ms | 18.70 ms | 91.05 ms | 2573.00 | 
| node (11.9) | [restify](http://restify.com) (7.7) | 16.41 ms | 14.15 ms | 22.07 ms | 47.02 ms | 413.32 ms | 14777.67 | 
| python (3.7) | [starlette](http://starlette.io) (0.10) | 17.58 ms | 14.81 ms | 30.42 ms | 51.95 ms | 103.23 ms | 9931.00 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 20.31 ms | 15.67 ms | 36.35 ms | 69.98 ms | 314.44 ms | 17852.00 | 
| node (11.9) | [express](http://expressjs.com) (4.16) | 21.21 ms | 16.11 ms | 26.96 ms | 132.91 ms | 812.60 ms | 36670.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 29.52 ms | 17.14 ms | 34.21 ms | 467.16 ms | 1780.82 ms | 103376.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 29.32 ms | 23.00 ms | 59.19 ms | 94.04 ms | 201.59 ms | 21420.67 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.11) | 28.09 ms | 25.20 ms | 48.54 ms | 74.75 ms | 123.83 ms | 14863.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 35.59 ms | 27.01 ms | 42.33 ms | 256.18 ms | 1247.90 ms | 69098.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 79.94 ms | 28.65 ms | 42.23 ms | 1347.89 ms | 2709.94 ms | 246598.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 32.55 ms | 29.98 ms | 42.39 ms | 49.10 ms | 329.46 ms | 9942.33 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 32.13 ms | 30.98 ms | 40.95 ms | 48.84 ms | 324.79 ms | 8525.67 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 45.49 ms | 32.64 ms | 97.21 ms | 147.57 ms | 586.85 ms | 34284.67 | 
| node (11.9) | [hapi](http://hapijs.com) (18.1) | 47.07 ms | 33.66 ms | 55.09 ms | 515.13 ms | 1398.19 ms | 86075.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 39.42 ms | 34.25 ms | 54.25 ms | 105.57 ms | 735.34 ms | 30209.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 39.04 ms | 36.93 ms | 49.10 ms | 58.39 ms | 333.81 ms | 11799.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 44.77 ms | 44.21 ms | 53.67 ms | 64.82 ms | 269.53 ms | 9671.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 50.12 ms | 47.63 ms | 63.68 ms | 73.40 ms | 421.60 ms | 17770.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 67.65 ms | 50.84 ms | 130.39 ms | 188.69 ms | 475.54 ms | 38840.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 60.63 ms | 52.32 ms | 116.95 ms | 188.24 ms | 358.96 ms | 41308.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 70.68 ms | 60.10 ms | 112.46 ms | 157.25 ms | 268.60 ms | 29799.00 | 
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.1) | 96.96 ms | 89.70 ms | 165.66 ms | 217.75 ms | 288.89 ms | 47603.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 105.54 ms | 99.60 ms | 142.28 ms | 432.49 ms | 1001.17 ms | 75364.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (vibora) (python)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 247088.67 | 142.94 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 245669.33 | 293.80 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 220647.67 | 250.86 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 218702.00 | 248.41 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 207172.67 | 201.05 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 187505.00 | 376.97 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 186384.33 | 175.29 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 180254.67 | 193.04 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 174223.67 | 281.53 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.7) | 168977.33 | 97.68 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 167010.33 | 341.40 MB |
| java (8) | [act](http://actframework.org) (1.8) | 150549.33 | 293.69 MB |
| node (11.9) | [restana](http://github.com/jkyberneees/ana) (2.8) | 120584.33 | 180.65 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 117397.33 | 148.31 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 110399.00 | 147.19 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 109075.67 | 177.53 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 104320.67 | 139.11 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 103298.33 | 138.30 MB |
| node (11.9) | [polka](http://github.com/lukeed/polka) (0.5) | 99616.33 | 149.10 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 98892.00 | 173.48 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 97565.00 | 193.41 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 96575.67 | 130.21 MB |
| go (1.11) | [echo](http://echo.labstack.com) (4.0) | 95465.33 | 167.59 MB |
| node (11.9) | [rayo](http://rayo.js.org) (1.2) | 94338.33 | 141.24 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 89897.00 | 119.83 MB |
| node (11.9) | [fastify](http://fastify.io) (1.14) | 79470.67 | 190.71 MB |
| node (11.9) | [foxify](http://foxify.js.org) (0.10) | 79251.33 | 166.49 MB |
| go (1.11) | [gf](http://goframe.org) (1.5) | 74277.67 | 112.80 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 73925.00 | 69.56 MB |
| node (11.9) | [koa](http://koajs.com) (2.7) | 73867.33 | 156.30 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 73248.33 | 188.30 MB |
| node (11.9) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 72759.33 | 109.01 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 70006.33 | 172.51 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 63455.00 | 106.35 MB |
| node (11.9) | [restify](http://restify.com) (7.7) | 63319.67 | 111.11 MB |
| python (3.7) | [starlette](http://starlette.io) (0.10) | 57992.67 | 125.17 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 56921.33 | 99.68 MB |
| node (11.9) | [express](http://expressjs.com) (4.16) | 55992.33 | 137.09 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 52359.33 | 129.81 MB |
| c (99) | [kore](http://kore.io) (3.1) | 51560.00 | 139.83 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 42827.67 | 212.86 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 42614.67 | 211.74 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 42589.67 | 212.26 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 42375.67 | 210.75 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 40241.67 | 209.01 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 39384.00 | 84.41 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 38583.00 | 61.18 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 36965.67 | 83.87 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.11) | 36170.33 | 69.83 MB |
| php (7.3) | [laravel](http://laravel.com) (5.7) | 36168.00 | 188.32 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 33513.67 | 62.23 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 31974.67 | 52.08 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 31130.33 | 29.18 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 30493.00 | 28.62 MB |
| node (11.9) | [hapi](http://hapijs.com) (18.1) | 27649.00 | 71.47 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 25720.67 | 63.42 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 25260.33 | 31.14 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 24751.67 | 23.59 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 24453.33 | 45.40 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 22132.67 | 40.41 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 21182.00 | 12.22 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 20401.00 | 33.22 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 17401.67 | 31.02 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 15421.33 | 44.66 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 14877.33 | 8.58 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 14213.67 | 28.33 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 13487.67 | 102.14 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 11558.67 | 29.98 MB |
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.1) | 10333.00 | 22.54 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 9788.33 | 28.96 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3105.00 | 9.49 MB |
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

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
Last update: 2019-01-30
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: iron (rust)


:five: symfony (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.11) | 0.08 ms | 0.08 ms | 0.12 ms | 0.17 ms | 7.10 ms | 57.00 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 4.70 ms | 0.28 ms | 16.63 ms | 35.55 ms | 85.81 ms | 8317.67 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 5.62 ms | 0.31 ms | 19.89 ms | 40.55 ms | 113.01 ms | 9740.33 | 
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 0.49 ms | 0.46 ms | 0.81 ms | 1.15 ms | 126.99 ms | 1238.67 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 204.98 ms | 0.47 ms | 359.30 ms | 4420.72 ms | 7216.93 ms | 741539.00 | 
| php (7.3) | [laravel](http://laravel.com) (5.7) | 160.49 ms | 0.47 ms | 419.23 ms | 2877.87 ms | 7036.85 ms | 522429.33 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 205.14 ms | 0.49 ms | 312.42 ms | 5100.06 ms | 7905.05 ms | 808018.00 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 236.25 ms | 0.53 ms | 395.08 ms | 4973.28 ms | 7656.90 ms | 840931.33 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 8.51 ms | 0.54 ms | 27.41 ms | 54.51 ms | 127.91 ms | 13140.67 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 9.60 ms | 0.57 ms | 31.05 ms | 60.98 ms | 141.23 ms | 14922.33 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 132.01 ms | 0.57 ms | 311.63 ms | 2773.06 ms | 7303.27 ms | 494885.67 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 208.46 ms | 0.73 ms | 397.58 ms | 4303.24 ms | 7590.66 ms | 748224.67 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 10.89 ms | 0.76 ms | 32.78 ms | 64.92 ms | 242.38 ms | 16612.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 145.32 ms | 2.47 ms | 22.37 ms | 3728.85 ms | 6599.60 ms | 664362.33 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.94 ms | 2.75 ms | 8.73 ms | 19.10 ms | 48.37 ms | 4079.67 | 
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 3.62 ms | 2.89 ms | 6.89 ms | 14.21 ms | 178.08 ms | 3318.33 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 3.62 ms | 3.07 ms | 5.86 ms | 11.10 ms | 82.67 ms | 2472.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 5.09 ms | 3.88 ms | 9.94 ms | 18.87 ms | 122.44 ms | 6616.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.50 ms | 4.39 ms | 5.72 ms | 10.62 ms | 159.15 ms | 7786.00 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 4.67 ms | 4.57 ms | 7.63 ms | 14.06 ms | 31.00 ms | 2701.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.97 ms | 4.74 ms | 7.69 ms | 13.72 ms | 36.50 ms | 2666.67 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 38.81 ms | 4.78 ms | 127.73 ms | 334.56 ms | 913.55 ms | 71497.67 | 
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 4.84 ms | 4.79 ms | 6.62 ms | 13.80 ms | 138.11 ms | 3349.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 5.46 ms | 4.90 ms | 9.91 ms | 18.49 ms | 38.56 ms | 3571.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.28 ms | 5.01 ms | 7.78 ms | 16.22 ms | 144.19 ms | 4418.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 6.20 ms | 6.12 ms | 10.44 ms | 12.92 ms | 155.92 ms | 3367.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 7.86 ms | 6.91 ms | 10.63 ms | 21.85 ms | 291.73 ms | 8566.00 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.6) | 8.14 ms | 7.13 ms | 15.90 ms | 28.21 ms | 65.16 ms | 6133.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 8.11 ms | 7.48 ms | 12.65 ms | 24.04 ms | 173.57 ms | 5777.67 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 8.47 ms | 7.52 ms | 12.96 ms | 25.94 ms | 117.62 ms | 4763.33 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 8.52 ms | 7.67 ms | 12.95 ms | 25.89 ms | 67.91 ms | 4385.00 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 9.43 ms | 8.13 ms | 14.14 ms | 31.09 ms | 268.85 ms | 9936.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 134.51 ms | 8.29 ms | 224.53 ms | 3269.13 ms | 7311.44 ms | 551438.00 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 9.31 ms | 8.47 ms | 14.45 ms | 29.44 ms | 79.80 ms | 5115.33 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 9.95 ms | 8.58 ms | 14.80 ms | 41.83 ms | 301.63 ms | 10019.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 10.31 ms | 8.81 ms | 16.15 ms | 37.04 ms | 300.37 ms | 9806.33 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 10.09 ms | 8.99 ms | 15.92 ms | 33.93 ms | 137.74 ms | 6024.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 11.74 ms | 9.67 ms | 20.42 ms | 38.24 ms | 259.86 ms | 9561.00 | 
| node (11.6) | [restana](http://github.com/jkyberneees/ana) (2.7) | 14.71 ms | 10.30 ms | 23.78 ms | 70.40 ms | 585.14 ms | 24209.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 13.57 ms | 11.19 ms | 23.68 ms | 41.70 ms | 225.38 ms | 9386.00 | 
| go (1.11) | [gf](http://gfer.me) (1.4) | 13.92 ms | 13.14 ms | 19.77 ms | 40.07 ms | 147.31 ms | 7248.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 13.16 ms | 13.16 ms | 15.67 ms | 18.88 ms | 133.64 ms | 2355.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 19.00 ms | 13.47 ms | 24.17 ms | 161.42 ms | 1130.81 ms | 46669.67 | 
| python (3.7) | [starlette](http://starlette.io) (0.10) | 15.69 ms | 14.05 ms | 25.39 ms | 38.19 ms | 80.20 ms | 7193.67 | 
| node (11.6) | [foxify](http://foxify.js.org) (0.10) | 22.44 ms | 15.39 ms | 28.03 ms | 208.62 ms | 897.89 ms | 44479.00 | 
| node (11.6) | [polka](http://github.com/lukeed/polka) (0.5) | 22.34 ms | 15.61 ms | 33.67 ms | 142.77 ms | 818.76 ms | 38957.67 | 
| node (11.6) | [rayo](http://rayo.js.org) (1.2) | 24.89 ms | 15.89 ms | 33.00 ms | 280.25 ms | 1001.70 ms | 55584.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 18.28 ms | 16.78 ms | 32.95 ms | 54.69 ms | 358.34 ms | 13046.67 | 
| node (11.6) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 26.76 ms | 17.00 ms | 31.81 ms | 337.28 ms | 1138.73 ms | 65043.67 | 
| node (11.6) | [koa](http://koajs.com) (2.6) | 29.74 ms | 21.20 ms | 39.78 ms | 256.07 ms | 1110.90 ms | 56751.67 | 
| node (11.6) | [fastify](http://fastify.io) (1.13) | 28.06 ms | 21.29 ms | 40.06 ms | 186.91 ms | 843.73 ms | 41126.33 | 
| node (11.6) | [express](http://expressjs.com) (4.16) | 34.91 ms | 22.92 ms | 42.17 ms | 388.10 ms | 1191.25 ms | 71250.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 28.38 ms | 24.74 ms | 53.50 ms | 74.66 ms | 114.23 ms | 16716.33 | 
| node (11.6) | [restify](http://restify.com) (7.6) | 30.51 ms | 25.01 ms | 47.08 ms | 116.67 ms | 619.75 ms | 28980.00 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 29.63 ms | 27.13 ms | 40.08 ms | 47.38 ms | 276.26 ms | 8431.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 30.80 ms | 27.20 ms | 45.30 ms | 63.09 ms | 772.84 ms | 25504.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 39.03 ms | 28.88 ms | 81.29 ms | 114.33 ms | 413.66 ms | 25026.33 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 30.76 ms | 29.87 ms | 39.22 ms | 45.24 ms | 176.76 ms | 6769.67 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 32.88 ms | 32.28 ms | 43.55 ms | 53.06 ms | 237.17 ms | 8294.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 36.57 ms | 33.52 ms | 48.87 ms | 57.12 ms | 411.13 ms | 13921.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 40.74 ms | 36.94 ms | 52.71 ms | 60.73 ms | 490.47 ms | 19416.00 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 40.34 ms | 40.14 ms | 47.24 ms | 60.90 ms | 253.02 ms | 8430.33 | 
| node (11.6) | [hapi](http://hapijs.com) (18.0) | 71.59 ms | 40.20 ms | 72.13 ms | 1089.18 ms | 2173.35 ms | 170976.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 58.48 ms | 50.34 ms | 110.81 ms | 196.11 ms | 329.19 ms | 39424.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 72.30 ms | 56.36 ms | 129.57 ms | 250.43 ms | 1012.89 ms | 56505.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 67.58 ms | 59.17 ms | 115.05 ms | 167.56 ms | 289.15 ms | 32096.00 | 
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.1) | 94.48 ms | 81.72 ms | 160.62 ms | 199.44 ms | 250.91 ms | 41723.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 97.04 ms | 95.86 ms | 117.77 ms | 144.12 ms | 479.25 ms | 21204.67 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 278424.33 | 161.04 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 278002.00 | 332.74 MB |
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 261547.33 | 297.39 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 221527.67 | 215.10 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 219685.00 | 249.51 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 206922.00 | 415.89 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 200560.67 | 188.69 MB |
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 185973.67 | 380.23 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 179279.67 | 191.43 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 179045.67 | 288.37 MB |
| java (8) | [act](http://actframework.org) (1.8) | 151565.33 | 295.83 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.6) | 127369.00 | 73.67 MB |
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 124719.00 | 157.21 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 124493.67 | 203.04 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 116313.00 | 156.28 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 115242.00 | 154.28 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 108787.00 | 146.24 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 106772.33 | 187.48 MB |
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.11) | 105579.33 | 210.27 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 104916.67 | 141.39 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 100367.67 | 133.09 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 99791.67 | 175.25 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 86722.67 | 222.87 MB |
| node (11.6) | [restana](http://github.com/jkyberneees/ana) (2.7) | 80839.33 | 121.20 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 75513.00 | 71.03 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 75238.00 | 185.32 MB |
| go (1.11) | [gf](http://gfer.me) (1.4) | 71811.00 | 108.96 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 64615.67 | 108.16 MB |
| python (3.7) | [starlette](http://starlette.io) (0.10) | 63317.00 | 136.36 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 60296.33 | 105.51 MB |
| node (11.6) | [foxify](http://foxify.js.org) (0.10) | 56832.33 | 119.51 MB |
| node (11.6) | [polka](http://github.com/lukeed/polka) (0.5) | 54902.67 | 82.10 MB |
| node (11.6) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 54673.00 | 81.79 MB |
| node (11.6) | [rayo](http://rayo.js.org) (1.2) | 54349.33 | 81.41 MB |
| c (99) | [kore](http://kore.io) (3.1) | 51855.33 | 140.61 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 45970.67 | 228.43 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 44677.00 | 96.70 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 44297.67 | 220.29 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 44073.67 | 219.17 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 43309.00 | 215.31 MB |
| node (11.6) | [fastify](http://fastify.io) (1.13) | 42612.67 | 103.45 MB |
| node (11.6) | [koa](http://koajs.com) (2.6) | 41889.33 | 88.46 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 41314.67 | 214.68 MB |
| node (11.6) | [express](http://expressjs.com) (4.16) | 38417.00 | 93.99 MB |
| php (7.3) | [laravel](http://laravel.com) (5.7) | 37495.00 | 195.84 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 37323.00 | 84.58 MB |
| node (11.6) | [restify](http://restify.com) (7.6) | 34961.67 | 61.24 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 33382.33 | 31.30 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 33082.00 | 61.35 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 32525.00 | 30.49 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 31676.33 | 51.12 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 30125.33 | 49.09 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 27223.67 | 25.95 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 27095.67 | 49.48 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 26446.33 | 65.19 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 24727.67 | 30.33 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 24480.00 | 39.89 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 22772.67 | 13.13 MB |
| node (11.6) | [hapi](http://hapijs.com) (18.0) | 22467.33 | 58.07 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 18091.67 | 32.28 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 15070.33 | 8.69 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 14955.00 | 29.83 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 14484.33 | 42.00 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 13329.33 | 100.81 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 11839.00 | 30.73 MB |
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.1) | 10552.33 | 23.01 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 10055.00 | 29.66 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3293.00 | 10.10 MB |
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

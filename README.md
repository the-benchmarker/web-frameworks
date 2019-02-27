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
Last update: 2019-02-27
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: flame (ruby)


:five: hanami (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.09 ms | 0.09 ms | 0.13 ms | 0.16 ms | 6.80 ms | 50.00 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 3.29 ms | 0.18 ms | 11.88 ms | 28.80 ms | 76.28 ms | 6420.67 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.90 ms | 0.21 ms | 14.19 ms | 31.97 ms | 80.42 ms | 7326.33 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.77 ms | 0.32 ms | 19.91 ms | 42.84 ms | 105.73 ms | 10019.67 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 6.87 ms | 0.36 ms | 23.74 ms | 48.93 ms | 118.92 ms | 11751.33 | 
| php (7.3) | [laravel](http://laravel.com) (5.7) | 128.04 ms | 0.38 ms | 329.27 ms | 2473.38 ms | 6862.49 ms | 434616.33 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 197.60 ms | 0.39 ms | 318.98 ms | 4193.26 ms | 7187.31 ms | 711536.00 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.41 ms | 0.41 ms | 0.65 ms | 0.93 ms | 56.70 ms | 417.67 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 8.45 ms | 0.52 ms | 27.06 ms | 54.66 ms | 127.99 ms | 13109.67 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 175.34 ms | 0.75 ms | 347.38 ms | 3718.09 ms | 6761.43 ms | 640861.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 145.82 ms | 1.51 ms | 262.34 ms | 3210.07 ms | 5923.88 ms | 537397.67 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 237.14 ms | 1.58 ms | 523.52 ms | 4197.30 ms | 7172.53 ms | 759387.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 62.59 ms | 1.71 ms | 3.63 ms | 2179.61 ms | 4951.15 ms | 391643.67 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 119.14 ms | 1.74 ms | 252.35 ms | 2581.11 ms | 7077.76 ms | 452571.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 3.26 ms | 3.03 ms | 5.62 ms | 10.10 ms | 88.40 ms | 2256.00 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 3.91 ms | 3.10 ms | 7.95 ms | 17.23 ms | 109.78 ms | 4102.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 4.05 ms | 3.31 ms | 8.48 ms | 16.61 ms | 49.67 ms | 3693.67 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 40.08 ms | 3.34 ms | 122.33 ms | 420.59 ms | 1354.01 ms | 95250.33 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 5.54 ms | 3.78 ms | 8.99 ms | 58.83 ms | 126.93 ms | 9640.33 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.27 ms | 3.84 ms | 6.48 ms | 13.49 ms | 159.32 ms | 3670.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.71 ms | 3.87 ms | 9.45 ms | 17.04 ms | 43.70 ms | 3721.67 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.6) | 4.56 ms | 4.37 ms | 7.67 ms | 14.87 ms | 35.20 ms | 2845.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.91 ms | 4.39 ms | 5.59 ms | 8.31 ms | 103.03 ms | 1982.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 5.05 ms | 4.46 ms | 9.36 ms | 16.92 ms | 40.78 ms | 3377.00 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 5.18 ms | 4.62 ms | 9.68 ms | 20.15 ms | 193.66 ms | 5038.33 | 
| node (11.10) | [restana](http://github.com/jkyberneees/ana) (2.9) | 5.64 ms | 4.74 ms | 9.19 ms | 17.12 ms | 211.74 ms | 5976.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.36 ms | 5.30 ms | 8.21 ms | 15.27 ms | 42.75 ms | 2851.33 | 
| node (11.10) | [polka](http://github.com/lukeed/polka) (0.5) | 6.99 ms | 5.53 ms | 10.60 ms | 19.70 ms | 236.80 ms | 7253.00 | 
| node (11.10) | [rayo](http://rayo.js.org) (1.2) | 7.92 ms | 5.78 ms | 11.21 ms | 26.80 ms | 392.72 ms | 13815.00 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 7.06 ms | 5.80 ms | 11.60 ms | 22.82 ms | 51.58 ms | 3991.33 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 7.21 ms | 5.86 ms | 12.04 ms | 24.01 ms | 143.90 ms | 4553.67 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 7.78 ms | 5.93 ms | 12.16 ms | 25.91 ms | 325.10 ms | 11380.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 6.34 ms | 6.05 ms | 10.42 ms | 13.29 ms | 1323.33 ms | 14250.00 | 
| go (1.11) | [kami](http://github.com/guregu/kami) (2.2) | 7.65 ms | 6.18 ms | 12.02 ms | 24.09 ms | 309.14 ms | 8599.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (4.0) | 7.71 ms | 6.29 ms | 12.92 ms | 25.65 ms | 166.25 ms | 5307.67 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 7.71 ms | 6.38 ms | 12.69 ms | 24.49 ms | 163.84 ms | 5318.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 7.68 ms | 6.41 ms | 10.52 ms | 23.38 ms | 423.87 ms | 11046.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 7.76 ms | 6.71 ms | 13.18 ms | 29.51 ms | 151.25 ms | 6041.00 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 8.59 ms | 6.89 ms | 14.74 ms | 30.75 ms | 189.22 ms | 5934.67 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 9.32 ms | 7.41 ms | 15.72 ms | 32.20 ms | 259.26 ms | 9744.33 | 
| node (11.10) | [fastify](http://fastify.io) (2.0) | 10.04 ms | 7.55 ms | 13.71 ms | 47.60 ms | 466.87 ms | 18252.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 156.00 ms | 7.85 ms | 26.86 ms | 4068.61 ms | 6902.77 ms | 681423.67 | 
| node (11.10) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 11.27 ms | 7.88 ms | 16.72 ms | 86.26 ms | 502.18 ms | 22437.00 | 
| node (11.10) | [foxify](http://foxify.js.org) (0.10) | 8.98 ms | 8.04 ms | 13.60 ms | 25.61 ms | 340.96 ms | 11450.33 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 10.35 ms | 8.34 ms | 19.32 ms | 35.54 ms | 215.91 ms | 8384.00 | 
| node (11.10) | [express](http://expressjs.com) (4.16) | 10.18 ms | 8.75 ms | 14.96 ms | 29.45 ms | 428.88 ms | 13171.67 | 
| node (11.10) | [koa](http://koajs.com) (2.7) | 10.90 ms | 9.04 ms | 15.01 ms | 41.07 ms | 502.51 ms | 19357.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 11.89 ms | 9.25 ms | 23.25 ms | 47.15 ms | 219.00 ms | 10216.00 | 
| go (1.11) | [gf](http://goframe.org) (1.5) | 10.59 ms | 9.60 ms | 15.58 ms | 31.73 ms | 238.08 ms | 6590.67 | 
| node (11.10) | [restify](http://restify.com) (8.0) | 11.20 ms | 9.63 ms | 15.28 ms | 34.49 ms | 280.02 ms | 9280.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 12.19 ms | 12.21 ms | 14.68 ms | 19.06 ms | 71.72 ms | 2418.33 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 14.53 ms | 12.61 ms | 24.98 ms | 36.37 ms | 70.31 ms | 7376.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 18.51 ms | 13.32 ms | 23.79 ms | 144.35 ms | 1080.15 ms | 47467.67 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 18.02 ms | 13.98 ms | 35.59 ms | 63.33 ms | 275.14 ms | 12729.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 16.21 ms | 14.81 ms | 29.45 ms | 51.13 ms | 192.84 ms | 11213.67 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 19.86 ms | 18.34 ms | 32.23 ms | 47.14 ms | 85.26 ms | 9257.00 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.5) | 22.45 ms | 19.65 ms | 38.75 ms | 57.18 ms | 105.55 ms | 11879.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 23.70 ms | 21.72 ms | 37.75 ms | 55.01 ms | 89.43 ms | 9888.00 | 
| node (11.10) | [hapi](http://hapijs.com) (18.1) | 37.45 ms | 22.10 ms | 39.20 ms | 543.85 ms | 1409.01 ms | 92490.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 27.40 ms | 22.84 ms | 34.32 ms | 121.77 ms | 1056.58 ms | 41247.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 27.11 ms | 24.73 ms | 38.76 ms | 46.89 ms | 123.85 ms | 7252.33 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 31.48 ms | 27.81 ms | 43.87 ms | 114.78 ms | 325.49 ms | 17923.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 32.22 ms | 29.28 ms | 42.97 ms | 54.85 ms | 262.63 ms | 11874.00 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 36.36 ms | 29.53 ms | 67.85 ms | 108.10 ms | 331.49 ms | 20761.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 32.92 ms | 30.09 ms | 49.82 ms | 72.67 ms | 322.68 ms | 14821.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 36.69 ms | 32.48 ms | 51.19 ms | 69.54 ms | 707.35 ms | 26679.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 32.39 ms | 32.91 ms | 42.73 ms | 50.95 ms | 179.16 ms | 8358.67 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 36.62 ms | 36.58 ms | 47.20 ms | 54.92 ms | 242.45 ms | 9089.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 45.77 ms | 39.12 ms | 79.52 ms | 131.00 ms | 210.31 ms | 25399.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 61.26 ms | 47.75 ms | 119.65 ms | 180.86 ms | 399.58 ms | 36766.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 64.31 ms | 57.95 ms | 104.58 ms | 144.60 ms | 204.43 ms | 30446.00 | 
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.3) | 79.00 ms | 70.16 ms | 137.74 ms | 176.76 ms | 296.20 ms | 39801.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 90.30 ms | 87.85 ms | 120.76 ms | 149.11 ms | 573.94 ms | 24812.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (agoo) (ruby)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 293363.00 | 169.64 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 254171.33 | 304.12 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 249474.33 | 283.70 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 233725.33 | 135.18 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 223440.00 | 216.64 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 221186.33 | 251.28 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 218982.67 | 352.41 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.6) | 206274.00 | 193.99 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 196048.67 | 393.81 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 194882.33 | 208.06 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 193953.00 | 396.67 MB |
| node (11.10) | [restana](http://github.com/jkyberneees/ana) (2.9) | 174640.33 | 261.84 MB |
| java (8) | [act](http://actframework.org) (1.8) | 166702.33 | 325.74 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 146930.67 | 184.93 MB |
| node (11.10) | [polka](http://github.com/lukeed/polka) (0.5) | 141610.33 | 211.96 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 138650.33 | 185.74 MB |
| node (11.10) | [rayo](http://rayo.js.org) (1.2) | 136187.67 | 203.92 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 135772.67 | 180.95 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 134444.67 | 180.35 MB |
| go (1.11) | [kami](http://github.com/guregu/kami) (2.2) | 130610.33 | 173.51 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 129602.00 | 211.07 MB |
| go (1.11) | [echo](http://echo.labstack.com) (4.0) | 128019.33 | 224.85 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 127435.33 | 171.96 MB |
| node (11.10) | [fastify](http://fastify.io) (2.0) | 121120.00 | 311.50 MB |
| node (11.10) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 120945.33 | 181.07 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 116748.33 | 204.93 MB |
| node (11.10) | [foxify](http://foxify.js.org) (0.10) | 116381.67 | 244.49 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 109420.00 | 146.37 MB |
| node (11.10) | [koa](http://koajs.com) (2.7) | 103239.00 | 218.31 MB |
| node (11.10) | [express](http://expressjs.com) (4.16) | 102625.00 | 251.21 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 100991.33 | 259.59 MB |
| go (1.11) | [gf](http://goframe.org) (1.5) | 95433.67 | 145.15 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 91424.67 | 181.38 MB |
| node (11.10) | [restify](http://restify.com) (8.0) | 91046.67 | 159.91 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 89635.33 | 220.78 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 80475.67 | 75.66 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 68818.67 | 148.15 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 68294.00 | 146.77 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 68173.33 | 114.02 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 67338.33 | 334.50 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 65910.00 | 115.42 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 63954.67 | 317.84 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 60695.00 | 301.27 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 60202.33 | 149.09 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 59355.33 | 308.10 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 55797.67 | 277.40 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 51544.67 | 81.82 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 50273.67 | 97.04 MB |
| php (7.3) | [laravel](http://laravel.com) (5.7) | 50208.00 | 261.57 MB |
| c (99) | [kore](http://kore.io) (3.1) | 49829.33 | 135.23 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.5) | 44981.67 | 97.08 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 42208.67 | 95.74 MB |
| node (11.10) | [hapi](http://hapijs.com) (18.1) | 41304.00 | 106.78 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 39604.67 | 73.43 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 38902.33 | 37.10 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 36598.67 | 34.36 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 32851.67 | 18.93 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 32377.00 | 39.74 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 31317.00 | 29.35 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 31119.67 | 50.70 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 30142.33 | 74.21 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 29128.00 | 54.14 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 27567.00 | 44.91 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 26965.67 | 49.23 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 22366.00 | 39.86 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 22184.67 | 12.81 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 18576.33 | 140.35 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 17027.00 | 49.32 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 15538.67 | 30.99 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 15133.00 | 39.23 MB |
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.3) | 12628.33 | 27.51 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 10817.33 | 32.01 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3462.00 | 10.62 MB |
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

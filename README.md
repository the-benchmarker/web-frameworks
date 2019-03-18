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
Last update: 2019-03-18
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


:five: iron (rust)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.06 ms | 0.07 ms | 0.08 ms | 0.11 ms | 3.21 ms | 37.67 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.17) | 2.87 ms | 0.15 ms | 10.72 ms | 27.45 ms | 69.56 ms | 6007.33 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.78 ms | 0.19 ms | 13.96 ms | 31.55 ms | 77.79 ms | 7223.33 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 86.14 ms | 0.25 ms | 222.65 ms | 1399.47 ms | 6794.49 ms | 330017.33 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.31 ms | 0.31 ms | 0.51 ms | 0.84 ms | 26.84 ms | 248.33 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.66 ms | 0.31 ms | 19.84 ms | 42.94 ms | 102.24 ms | 9984.00 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 6.92 ms | 0.33 ms | 24.71 ms | 51.16 ms | 127.07 ms | 12204.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 7.21 ms | 0.41 ms | 24.30 ms | 49.91 ms | 120.37 ms | 11923.67 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 164.94 ms | 0.44 ms | 285.21 ms | 3502.45 ms | 7183.47 ms | 605939.00 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 75.56 ms | 1.08 ms | 170.04 ms | 1349.10 ms | 5172.04 ms | 315841.00 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 98.69 ms | 1.13 ms | 184.90 ms | 2183.75 ms | 5466.65 ms | 382944.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 97.67 ms | 1.30 ms | 3.27 ms | 3002.61 ms | 5481.23 ms | 494273.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 72.25 ms | 1.37 ms | 135.11 ms | 1657.78 ms | 5107.58 ms | 311506.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 2.40 ms | 1.44 ms | 5.63 ms | 11.83 ms | 114.52 ms | 2583.33 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 68.08 ms | 1.70 ms | 153.02 ms | 1362.99 ms | 4012.85 ms | 277154.00 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 29.74 ms | 1.73 ms | 102.17 ms | 288.06 ms | 892.82 ms | 61020.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.66 ms | 1.82 ms | 6.04 ms | 12.07 ms | 31.57 ms | 2649.67 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 2.75 ms | 2.19 ms | 5.55 ms | 11.93 ms | 31.22 ms | 2402.33 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.10 ms | 2.28 ms | 5.49 ms | 9.58 ms | 167.78 ms | 4483.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.57 ms | 2.56 ms | 4.49 ms | 5.78 ms | 45.89 ms | 1381.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 7.46 ms | 2.58 ms | 5.35 ms | 149.06 ms | 1148.24 ms | 50132.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.28 ms | 2.65 ms | 7.00 ms | 13.63 ms | 34.03 ms | 2993.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.29 ms | 2.93 ms | 5.41 ms | 9.39 ms | 33.68 ms | 1844.33 | 
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 3.35 ms | 2.99 ms | 5.83 ms | 11.51 ms | 26.16 ms | 2293.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 3.61 ms | 3.01 ms | 6.93 ms | 13.18 ms | 29.42 ms | 2693.00 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 3.98 ms | 3.36 ms | 7.25 ms | 17.30 ms | 101.82 ms | 3970.33 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 5.77 ms | 3.47 ms | 9.87 ms | 68.92 ms | 116.26 ms | 10827.67 | 
| go (1.12) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 5.33 ms | 4.28 ms | 10.32 ms | 21.90 ms | 269.03 ms | 6456.67 | 
| go (1.12) | [iris](http://iris-go.com) (11.1) | 5.39 ms | 4.38 ms | 10.46 ms | 22.30 ms | 114.13 ms | 4796.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 5.01 ms | 4.44 ms | 6.98 ms | 14.96 ms | 222.69 ms | 6751.33 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 6.27 ms | 4.53 ms | 13.14 ms | 27.04 ms | 278.35 ms | 7674.33 | 
| go (1.11) | [beego](http://beego.me) (1.12) | 6.24 ms | 4.60 ms | 12.98 ms | 26.79 ms | 120.81 ms | 5883.67 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 6.36 ms | 4.62 ms | 13.62 ms | 27.05 ms | 159.26 ms | 5777.00 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 6.30 ms | 4.63 ms | 11.55 ms | 25.66 ms | 302.54 ms | 9705.67 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 6.77 ms | 4.63 ms | 15.08 ms | 31.55 ms | 157.78 ms | 6856.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 6.43 ms | 4.70 ms | 11.89 ms | 23.63 ms | 209.25 ms | 6100.33 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 6.40 ms | 4.72 ms | 13.21 ms | 26.80 ms | 170.30 ms | 6080.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.57 ms | 4.74 ms | 9.08 ms | 19.00 ms | 207.48 ms | 5246.00 | 
| node (11.11) | [restana](http://github.com/jkyberneees/ana) (2.10) | 6.60 ms | 5.14 ms | 9.59 ms | 19.62 ms | 240.52 ms | 7284.67 | 
| node (11.11) | [polka](http://github.com/lukeed/polka) (0.5) | 6.65 ms | 5.21 ms | 9.71 ms | 17.24 ms | 222.11 ms | 6569.33 | 
| go (1.12) | [gf](http://goframe.org) (1.5) | 8.02 ms | 5.53 ms | 16.80 ms | 36.04 ms | 116.36 ms | 7271.00 | 
| node (11.11) | [rayo](http://rayo.js.org) (1.2) | 7.24 ms | 5.64 ms | 10.14 ms | 19.66 ms | 329.44 ms | 10713.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 7.39 ms | 5.89 ms | 12.35 ms | 22.63 ms | 210.54 ms | 6916.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 205.17 ms | 7.05 ms | 36.77 ms | 4743.80 ms | 7929.99 ms | 826607.67 | 
| node (11.11) | [fastify](http://fastify.io) (2.1) | 8.87 ms | 7.22 ms | 12.43 ms | 22.95 ms | 359.49 ms | 12120.67 | 
| node (11.11) | [foxify](http://foxify.js.org) (0.10) | 8.94 ms | 7.54 ms | 12.24 ms | 44.91 ms | 420.18 ms | 16016.33 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 10.22 ms | 8.03 ms | 18.44 ms | 35.62 ms | 216.92 ms | 8102.67 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 9.23 ms | 8.49 ms | 14.87 ms | 21.15 ms | 52.41 ms | 4373.67 | 
| node (11.11) | [express](http://expressjs.com) (4.16) | 11.09 ms | 8.77 ms | 15.78 ms | 41.12 ms | 413.77 ms | 14836.67 | 
| node (11.11) | [restify](http://restify.com) (8.1) | 10.61 ms | 9.48 ms | 13.75 ms | 29.37 ms | 241.59 ms | 7446.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 11.09 ms | 9.69 ms | 20.38 ms | 39.92 ms | 217.41 ms | 8444.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.3) | 12.31 ms | 9.76 ms | 17.31 ms | 39.51 ms | 606.12 ms | 21206.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 9.84 ms | 9.93 ms | 11.37 ms | 12.95 ms | 20.66 ms | 1345.33 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 12.73 ms | 11.74 ms | 23.54 ms | 35.75 ms | 101.11 ms | 7746.00 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.8) | 13.43 ms | 12.20 ms | 21.47 ms | 30.41 ms | 67.89 ms | 6017.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 17.19 ms | 14.42 ms | 33.60 ms | 47.65 ms | 82.52 ms | 10003.67 | 
| node (11.11) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 19.79 ms | 14.56 ms | 30.27 ms | 96.79 ms | 697.62 ms | 28312.33 | 
| node (11.11) | [koa](http://koajs.com) (2.7) | 19.03 ms | 14.98 ms | 27.58 ms | 81.69 ms | 667.18 ms | 27475.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.6) | 22.92 ms | 17.74 ms | 30.15 ms | 116.09 ms | 1042.01 ms | 40441.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 22.98 ms | 18.68 ms | 40.86 ms | 64.60 ms | 374.51 ms | 14362.00 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 21.29 ms | 19.25 ms | 30.20 ms | 53.06 ms | 394.33 ms | 14585.33 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 22.61 ms | 20.62 ms | 30.25 ms | 37.31 ms | 109.58 ms | 6614.00 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 23.24 ms | 20.96 ms | 31.12 ms | 39.36 ms | 166.91 ms | 7033.33 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 23.15 ms | 22.88 ms | 28.20 ms | 35.34 ms | 254.71 ms | 8419.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 28.82 ms | 27.21 ms | 37.28 ms | 40.50 ms | 233.59 ms | 7558.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 34.62 ms | 27.38 ms | 63.70 ms | 118.94 ms | 238.15 ms | 23043.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 30.45 ms | 29.73 ms | 35.34 ms | 50.70 ms | 189.47 ms | 8152.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 40.63 ms | 33.20 ms | 65.26 ms | 137.67 ms | 598.17 ms | 33521.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 32.82 ms | 34.28 ms | 41.02 ms | 45.05 ms | 183.54 ms | 7697.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 40.57 ms | 38.67 ms | 60.78 ms | 90.46 ms | 126.23 ms | 16259.67 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 53.04 ms | 47.09 ms | 90.75 ms | 122.93 ms | 164.63 ms | 25468.67 | 
| node (11.11) | [hapi](http://hapijs.com) (18.1) | 138.06 ms | 61.48 ms | 138.72 ms | 1973.69 ms | 3545.36 ms | 331007.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 63.02 ms | 62.93 ms | 76.80 ms | 95.10 ms | 528.53 ms | 19754.33 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 452820.00 | 261.90 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 407525.33 | 487.79 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 353931.33 | 401.81 MB |
| c (99) | [kore](http://kore.io) (3.1) | 348925.33 | 907.01 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 335752.67 | 325.68 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 333142.67 | 378.20 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 313131.67 | 504.65 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 310926.33 | 624.79 MB |
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 284511.33 | 267.49 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 278411.33 | 296.59 MB |
| java (8) | [act](http://actframework.org) (1.8) | 260921.00 | 508.89 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 258514.67 | 529.34 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 252871.33 | 146.29 MB |
| go (1.12) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 201094.00 | 268.68 MB |
| go (1.12) | [iris](http://iris-go.com) (11.1) | 196954.00 | 263.18 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 196177.33 | 319.62 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 178879.00 | 225.99 MB |
| go (1.11) | [beego](http://beego.me) (1.12) | 175793.33 | 236.40 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 175195.67 | 307.31 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 173814.67 | 231.12 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 172159.00 | 301.96 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 169066.00 | 226.12 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 165465.33 | 220.85 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 161445.67 | 414.91 MB |
| node (11.11) | [restana](http://github.com/jkyberneees/ana) (2.10) | 154340.67 | 231.40 MB |
| node (11.11) | [polka](http://github.com/lukeed/polka) (0.5) | 149900.33 | 224.71 MB |
| node (11.11) | [rayo](http://rayo.js.org) (1.2) | 145516.33 | 217.78 MB |
| go (1.12) | [gf](http://goframe.org) (1.5) | 139795.00 | 211.30 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 135044.33 | 332.50 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 131505.67 | 262.05 MB |
| node (11.11) | [foxify](http://foxify.js.org) (0.10) | 125998.67 | 265.02 MB |
| node (11.11) | [fastify](http://fastify.io) (2.1) | 124641.00 | 330.73 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 106279.33 | 229.21 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 102770.00 | 96.55 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 100479.33 | 249.25 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 100017.00 | 496.93 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 99363.33 | 173.97 MB |
| node (11.11) | [express](http://expressjs.com) (4.16) | 96334.00 | 236.02 MB |
| node (11.11) | [restify](http://restify.com) (8.1) | 94955.33 | 166.64 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 94778.00 | 470.23 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 88621.00 | 190.15 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.3) | 88568.33 | 148.96 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 88555.00 | 439.47 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 83644.33 | 434.80 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 81090.33 | 156.40 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 77437.33 | 384.36 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.8) | 73839.33 | 159.45 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 69624.33 | 109.85 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 62352.33 | 324.66 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 61720.67 | 140.01 MB |
| node (11.11) | [koa](http://koajs.com) (2.7) | 59579.00 | 125.97 MB |
| node (11.11) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 58772.00 | 87.95 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.6) | 49056.67 | 90.95 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 48026.33 | 89.18 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.17) | 44817.00 | 42.69 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 44291.33 | 109.00 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 44202.67 | 41.43 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 43436.00 | 40.76 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 42737.67 | 69.67 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 34341.33 | 62.66 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 33967.00 | 19.59 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 33618.33 | 41.17 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 31014.67 | 50.52 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 30753.00 | 54.79 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 25668.67 | 74.50 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 24629.67 | 49.12 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 22560.33 | 13.02 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 18902.00 | 41.17 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 18597.33 | 140.48 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 17738.33 | 46.06 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 15473.33 | 45.80 MB |
| node (11.11) | [hapi](http://hapijs.com) (18.1) | 14038.00 | 36.37 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 4323.00 | 13.25 MB |
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

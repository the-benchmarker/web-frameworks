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
Last update: 2019-02-11
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
threads: 9, connections: 1000
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: iron (rust)


:five: zend-expressive (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.07 ms | 0.07 ms | 0.11 ms | 0.14 ms | 0.82 ms | 29.00 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 3.22 ms | 0.17 ms | 11.84 ms | 28.45 ms | 72.91 ms | 6360.67 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.02 ms | 0.20 ms | 15.15 ms | 34.13 ms | 86.72 ms | 7827.33 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.30 ms | 0.27 ms | 0.50 ms | 0.87 ms | 13.52 ms | 203.67 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 93.44 ms | 0.30 ms | 192.66 ms | 2035.74 ms | 6929.45 ms | 381552.67 | 
| php (7.3) | [laravel](http://laravel.com) (5.7) | 96.92 ms | 0.32 ms | 260.45 ms | 1672.97 ms | 6786.30 ms | 329915.33 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 6.13 ms | 0.34 ms | 21.11 ms | 44.81 ms | 102.32 ms | 10517.67 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 7.03 ms | 0.38 ms | 23.93 ms | 48.94 ms | 122.54 ms | 11745.67 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 146.51 ms | 0.38 ms | 270.78 ms | 3061.66 ms | 6802.14 ms | 535504.67 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 8.14 ms | 0.47 ms | 26.78 ms | 54.72 ms | 128.39 ms | 13074.00 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 76.52 ms | 1.21 ms | 170.12 ms | 1675.31 ms | 5420.99 ms | 302667.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 40.80 ms | 1.40 ms | 2.91 ms | 1406.68 ms | 4951.82 ms | 296701.00 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 116.58 ms | 1.47 ms | 206.59 ms | 2652.49 ms | 5827.41 ms | 470608.67 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 3.13 ms | 2.19 ms | 6.63 ms | 15.16 ms | 92.72 ms | 4104.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.15 ms | 2.20 ms | 6.54 ms | 15.53 ms | 35.68 ms | 3204.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 3.01 ms | 2.29 ms | 5.33 ms | 8.46 ms | 73.07 ms | 1911.67 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 31.03 ms | 2.49 ms | 103.97 ms | 279.94 ms | 751.51 ms | 60140.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.48 ms | 2.87 ms | 5.73 ms | 11.22 ms | 144.45 ms | 2590.67 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 92.39 ms | 2.96 ms | 222.08 ms | 1898.28 ms | 3738.67 ms | 321714.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.02 ms | 2.98 ms | 5.16 ms | 7.37 ms | 97.64 ms | 2470.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.82 ms | 3.07 ms | 7.89 ms | 14.59 ms | 31.03 ms | 3199.67 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 3.63 ms | 3.27 ms | 6.43 ms | 12.89 ms | 29.89 ms | 2553.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.21 ms | 3.51 ms | 7.01 ms | 12.55 ms | 53.98 ms | 2456.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 4.28 ms | 3.69 ms | 8.15 ms | 15.00 ms | 50.00 ms | 3203.00 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 4.46 ms | 3.75 ms | 8.56 ms | 18.81 ms | 149.72 ms | 4309.33 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.7) | 6.57 ms | 3.95 ms | 10.77 ms | 77.80 ms | 122.91 ms | 11964.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 4.98 ms | 4.93 ms | 8.32 ms | 10.18 ms | 30.86 ms | 2538.67 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 6.09 ms | 5.11 ms | 10.07 ms | 20.01 ms | 112.42 ms | 4441.00 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 6.26 ms | 5.14 ms | 10.47 ms | 21.04 ms | 208.98 ms | 4851.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.06 ms | 5.15 ms | 10.05 ms | 19.90 ms | 130.15 ms | 4782.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 5.93 ms | 5.19 ms | 8.75 ms | 15.99 ms | 287.07 ms | 6853.33 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 6.67 ms | 5.25 ms | 10.61 ms | 25.56 ms | 220.03 ms | 8103.67 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 6.88 ms | 5.57 ms | 11.60 ms | 23.29 ms | 165.59 ms | 4953.67 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 7.59 ms | 5.66 ms | 11.82 ms | 26.11 ms | 431.58 ms | 13950.67 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 7.01 ms | 5.72 ms | 11.76 ms | 23.49 ms | 164.97 ms | 5414.67 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 7.11 ms | 5.83 ms | 11.83 ms | 23.98 ms | 164.07 ms | 5024.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 201.68 ms | 6.53 ms | 27.22 ms | 5087.99 ms | 7938.07 ms | 849803.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 7.98 ms | 6.56 ms | 13.02 ms | 22.07 ms | 214.50 ms | 6502.00 | 
| node (11.9) | [polka](http://github.com/lukeed/polka) (0.5) | 12.03 ms | 7.97 ms | 20.40 ms | 55.21 ms | 582.80 ms | 23735.33 | 
| node (11.9) | [foxify](http://foxify.js.org) (0.10) | 9.81 ms | 8.38 ms | 14.45 ms | 33.77 ms | 383.13 ms | 13085.67 | 
| go (1.11) | [gf](http://gfer.me) (1.4) | 9.38 ms | 8.53 ms | 14.04 ms | 30.32 ms | 128.87 ms | 5278.00 | 
| node (11.9) | [restana](http://github.com/jkyberneees/ana) (2.8) | 12.15 ms | 8.57 ms | 22.79 ms | 50.61 ms | 437.43 ms | 16026.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 9.48 ms | 8.70 ms | 15.43 ms | 27.78 ms | 168.39 ms | 6652.00 | 
| node (11.9) | [rayo](http://rayo.js.org) (1.2) | 12.99 ms | 9.55 ms | 22.79 ms | 49.57 ms | 450.75 ms | 16986.33 | 
| node (11.9) | [koa](http://koajs.com) (2.7) | 13.37 ms | 9.87 ms | 22.39 ms | 52.31 ms | 438.62 ms | 17077.33 | 
| node (11.9) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 14.65 ms | 10.59 ms | 23.27 ms | 73.24 ms | 597.54 ms | 24610.33 | 
| python (3.7) | [starlette](http://starlette.io) (0.10) | 11.88 ms | 10.64 ms | 20.68 ms | 32.23 ms | 65.33 ms | 6510.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 15.81 ms | 11.03 ms | 19.98 ms | 128.05 ms | 914.63 ms | 39919.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 13.10 ms | 11.54 ms | 23.60 ms | 42.67 ms | 364.34 ms | 11823.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 11.57 ms | 11.82 ms | 13.33 ms | 15.02 ms | 132.36 ms | 1901.00 | 
| node (11.9) | [fastify](http://fastify.io) (1.14) | 17.31 ms | 13.19 ms | 28.23 ms | 61.39 ms | 605.40 ms | 23190.67 | 
| node (11.9) | [express](http://expressjs.com) (4.16) | 21.04 ms | 16.78 ms | 33.75 ms | 71.62 ms | 616.86 ms | 23960.00 | 
| node (11.9) | [restify](http://restify.com) (7.7) | 20.39 ms | 16.91 ms | 34.74 ms | 63.08 ms | 403.83 ms | 15782.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 20.26 ms | 16.93 ms | 38.16 ms | 56.88 ms | 106.79 ms | 12474.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 24.79 ms | 20.55 ms | 37.42 ms | 54.36 ms | 612.90 ms | 22401.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 25.16 ms | 20.91 ms | 30.94 ms | 94.74 ms | 838.65 ms | 27991.67 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 24.12 ms | 21.92 ms | 35.67 ms | 43.58 ms | 240.89 ms | 8014.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 27.98 ms | 25.17 ms | 40.20 ms | 62.32 ms | 329.27 ms | 13659.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 29.67 ms | 25.52 ms | 44.82 ms | 71.19 ms | 777.21 ms | 28617.33 | 
| node (11.9) | [hapi](http://hapijs.com) (18.1) | 37.12 ms | 26.06 ms | 46.80 ms | 376.69 ms | 1191.69 ms | 69785.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 26.79 ms | 27.63 ms | 35.38 ms | 42.18 ms | 114.63 ms | 6772.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 32.88 ms | 32.70 ms | 39.51 ms | 45.13 ms | 251.69 ms | 7921.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 42.62 ms | 33.21 ms | 82.08 ms | 152.86 ms | 237.78 ms | 30020.67 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 34.18 ms | 33.68 ms | 40.86 ms | 68.97 ms | 564.84 ms | 22571.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 48.04 ms | 40.98 ms | 77.75 ms | 114.48 ms | 497.90 ms | 22912.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 48.75 ms | 45.14 ms | 82.44 ms | 119.81 ms | 171.01 ms | 23630.67 | 
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.1) | 64.16 ms | 60.72 ms | 109.21 ms | 148.13 ms | 230.76 ms | 31055.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 72.69 ms | 72.91 ms | 88.89 ms | 113.26 ms | 637.15 ms | 24501.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (actix-web) (rust)


:three: (japronto) (python)


:four: (evhtp) (cpp)


:five: (vibora) (python)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 344878.00 | 199.48 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 338213.33 | 384.58 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 337721.00 | 404.34 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 290568.67 | 282.10 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 277714.67 | 315.40 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 267298.67 | 430.28 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 264055.00 | 248.28 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 246706.33 | 495.67 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 234071.33 | 250.24 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 232370.33 | 475.82 MB |
| java (8) | [act](http://actframework.org) (1.8) | 231689.67 | 452.45 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.7) | 221164.67 | 127.87 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 172112.00 | 217.00 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 161427.33 | 263.30 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 161055.33 | 215.87 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 156107.67 | 208.58 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 155973.33 | 209.11 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 144478.00 | 253.64 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 141264.67 | 188.44 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 140129.33 | 246.00 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 138169.67 | 186.14 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 124311.33 | 319.47 MB |
| node (11.9) | [foxify](http://foxify.js.org) (0.10) | 109359.00 | 230.03 MB |
| go (1.11) | [gf](http://gfer.me) (1.4) | 107796.67 | 164.14 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 105811.33 | 260.66 MB |
| node (11.9) | [polka](http://github.com/lukeed/polka) (0.5) | 104391.67 | 156.48 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 104361.33 | 207.64 MB |
| node (11.9) | [restana](http://github.com/jkyberneees/ana) (2.8) | 94701.33 | 141.96 MB |
| node (11.9) | [rayo](http://rayo.js.org) (1.2) | 87939.00 | 131.67 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 87359.00 | 434.37 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 85480.67 | 80.32 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 85333.00 | 149.42 MB |
| node (11.9) | [koa](http://koajs.com) (2.7) | 84686.33 | 179.14 MB |
| python (3.7) | [starlette](http://starlette.io) (0.10) | 84193.33 | 181.20 MB |
| node (11.9) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 83976.00 | 125.65 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 82667.67 | 177.00 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 80238.00 | 398.10 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 78398.00 | 131.92 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 75831.33 | 376.34 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 72264.67 | 358.64 MB |
| node (11.9) | [fastify](http://fastify.io) (1.14) | 67457.00 | 163.02 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 66586.00 | 345.50 MB |
| c (99) | [kore](http://kore.io) (3.1) | 63078.00 | 171.02 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 63035.33 | 100.32 MB |
| php (7.3) | [laravel](http://laravel.com) (5.7) | 52900.33 | 275.68 MB |
| node (11.9) | [express](http://expressjs.com) (4.16) | 51997.00 | 126.98 MB |
| node (11.9) | [restify](http://restify.com) (7.7) | 51146.67 | 89.47 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 51097.67 | 115.91 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 42088.67 | 39.50 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 40809.00 | 75.78 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 40756.33 | 66.39 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 39805.67 | 37.93 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 37592.67 | 35.24 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 35653.67 | 87.80 MB |
| node (11.9) | [hapi](http://hapijs.com) (18.1) | 34806.33 | 89.97 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 34363.00 | 56.09 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 31872.67 | 18.37 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 30748.00 | 37.64 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 30161.00 | 55.04 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 25299.00 | 45.08 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 20885.67 | 12.04 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 20672.67 | 60.00 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 20663.33 | 41.21 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 18145.33 | 137.22 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 15698.00 | 40.70 MB |
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.1) | 15639.00 | 34.06 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 13434.00 | 39.51 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 4132.33 | 12.65 MB |
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

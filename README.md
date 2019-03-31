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
Last update: 2019-03-31
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


:five: zend-expressive (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.11 ms | 0.10 ms | 0.16 ms | 0.22 ms | 81.94 ms | 536.67 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.18) | 6.39 ms | 0.37 ms | 22.21 ms | 44.03 ms | 104.17 ms | 10737.00 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 8.15 ms | 0.44 ms | 28.67 ms | 56.39 ms | 151.54 ms | 13793.00 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 201.40 ms | 0.63 ms | 373.11 ms | 4363.21 ms | 7870.66 ms | 752003.00 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 218.92 ms | 0.64 ms | 362.43 ms | 4910.83 ms | 7051.47 ms | 804224.67 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 189.31 ms | 0.65 ms | 343.12 ms | 4375.66 ms | 7786.18 ms | 727983.00 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 187.79 ms | 0.66 ms | 349.67 ms | 3743.20 ms | 7078.78 ms | 655417.00 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 191.99 ms | 0.67 ms | 428.39 ms | 3860.85 ms | 7664.05 ms | 678451.33 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 11.00 ms | 0.76 ms | 34.44 ms | 66.34 ms | 171.00 ms | 16212.00 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 13.26 ms | 0.76 ms | 40.34 ms | 84.63 ms | 509.11 ms | 25925.67 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.87 ms | 0.82 ms | 1.35 ms | 2.16 ms | 68.09 ms | 715.33 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 14.36 ms | 1.20 ms | 41.55 ms | 78.07 ms | 175.77 ms | 19430.67 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 228.14 ms | 2.39 ms | 408.82 ms | 4558.98 ms | 7322.14 ms | 785417.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 46.97 ms | 2.44 ms | 4.94 ms | 1597.03 ms | 4467.19 ms | 302972.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 3.76 ms | 2.97 ms | 7.37 ms | 14.62 ms | 38.57 ms | 3259.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 4.31 ms | 3.64 ms | 8.80 ms | 18.40 ms | 39.62 ms | 3856.67 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 4.37 ms | 4.04 ms | 7.84 ms | 15.57 ms | 66.47 ms | 3210.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 11.83 ms | 4.37 ms | 10.77 ms | 236.73 ms | 1439.07 ms | 63175.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.96 ms | 5.12 ms | 6.69 ms | 12.03 ms | 107.59 ms | 2570.33 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.54 ms | 5.19 ms | 8.38 ms | 17.45 ms | 84.09 ms | 2974.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 6.62 ms | 5.20 ms | 12.27 ms | 22.36 ms | 192.16 ms | 10020.33 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 8.43 ms | 5.23 ms | 14.07 ms | 80.39 ms | 131.44 ms | 12507.33 | 
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 6.65 ms | 5.34 ms | 9.95 ms | 38.40 ms | 187.99 ms | 10941.00 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 6.33 ms | 5.50 ms | 11.01 ms | 21.48 ms | 210.95 ms | 4494.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 6.51 ms | 5.58 ms | 11.48 ms | 21.92 ms | 59.38 ms | 4183.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 6.28 ms | 6.11 ms | 10.16 ms | 17.56 ms | 102.02 ms | 3537.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 8.99 ms | 8.25 ms | 12.16 ms | 23.54 ms | 425.77 ms | 9826.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 9.31 ms | 8.38 ms | 14.01 ms | 27.87 ms | 188.27 ms | 6805.67 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 51.18 ms | 9.16 ms | 162.13 ms | 405.03 ms | 1038.57 ms | 87709.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 192.36 ms | 9.16 ms | 204.61 ms | 4444.23 ms | 7905.31 ms | 769095.00 | 
| go (1.12) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 18.70 ms | 9.62 ms | 38.58 ms | 174.18 ms | 385.63 ms | 31684.33 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 12.26 ms | 9.65 ms | 23.06 ms | 52.83 ms | 163.46 ms | 10094.33 | 
| node (11.12) | [restana](http://github.com/jkyberneees/ana) (2.10) | 13.97 ms | 9.76 ms | 17.99 ms | 112.26 ms | 624.99 ms | 29743.67 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 12.79 ms | 9.84 ms | 24.92 ms | 54.75 ms | 158.39 ms | 10426.00 | 
| go (1.12) | [iris](http://iris-go.com) (11.1) | 11.94 ms | 9.86 ms | 19.90 ms | 47.99 ms | 177.55 ms | 9105.67 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 13.37 ms | 10.02 ms | 26.31 ms | 61.28 ms | 213.60 ms | 11850.00 | 
| go (1.12) | [violetear](http://violetear.org) (5.0) | 12.55 ms | 10.04 ms | 21.64 ms | 51.65 ms | 225.05 ms | 11012.67 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 14.56 ms | 10.58 ms | 29.45 ms | 65.98 ms | 198.09 ms | 12851.67 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 14.05 ms | 10.62 ms | 26.48 ms | 62.17 ms | 313.59 ms | 13551.00 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 12.79 ms | 10.70 ms | 21.16 ms | 48.60 ms | 123.97 ms | 8643.67 | 
| go (1.12) | [gf](http://goframe.org) (1.5) | 16.47 ms | 12.09 ms | 32.81 ms | 76.48 ms | 267.76 ms | 15443.00 | 
| node (11.12) | [polka](http://github.com/lukeed/polka) (0.5) | 19.94 ms | 13.18 ms | 23.28 ms | 220.01 ms | 858.79 ms | 47269.67 | 
| node (11.12) | [rayo](http://rayo.js.org) (1.2) | 17.41 ms | 13.32 ms | 23.21 ms | 95.09 ms | 753.12 ms | 32282.67 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 16.29 ms | 13.65 ms | 27.73 ms | 48.19 ms | 236.40 ms | 9674.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 20.04 ms | 15.28 ms | 34.62 ms | 76.15 ms | 329.12 ms | 16999.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 16.37 ms | 16.09 ms | 20.36 ms | 23.84 ms | 90.63 ms | 3105.67 | 
| node (11.12) | [foxify](http://foxify.js.org) (0.10) | 26.00 ms | 17.20 ms | 30.12 ms | 313.51 ms | 1084.57 ms | 60137.00 | 
| node (11.12) | [fastify](http://fastify.io) (2.1) | 26.20 ms | 17.29 ms | 31.02 ms | 296.77 ms | 978.29 ms | 55479.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.3) | 40.87 ms | 18.32 ms | 33.13 ms | 761.37 ms | 2224.30 ms | 140103.00 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 24.91 ms | 19.02 ms | 45.05 ms | 74.92 ms | 378.33 ms | 16733.00 | 
| node (11.12) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 45.36 ms | 19.95 ms | 39.09 ms | 831.07 ms | 1779.95 ms | 136967.00 | 
| node (11.12) | [koa](http://koajs.com) (2.7) | 35.30 ms | 19.98 ms | 34.75 ms | 558.04 ms | 1467.23 ms | 95929.00 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 21.80 ms | 20.50 ms | 35.17 ms | 50.98 ms | 92.22 ms | 10050.33 | 
| node (11.12) | [restify](http://restify.com) (8.2) | 25.30 ms | 21.10 ms | 32.27 ms | 114.73 ms | 678.34 ms | 30146.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 33.49 ms | 22.41 ms | 62.11 ms | 245.09 ms | 585.42 ms | 45739.33 | 
| node (11.12) | [express](http://expressjs.com) (4.16) | 35.60 ms | 25.76 ms | 38.84 ms | 386.84 ms | 1210.61 ms | 69737.33 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.9) | 33.37 ms | 28.89 ms | 54.85 ms | 105.83 ms | 272.16 ms | 20766.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.6) | 38.41 ms | 33.23 ms | 52.30 ms | 134.40 ms | 1044.12 ms | 39984.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 35.45 ms | 34.61 ms | 57.40 ms | 83.45 ms | 130.81 ms | 17184.33 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 38.03 ms | 37.16 ms | 49.03 ms | 58.82 ms | 333.77 ms | 11248.67 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 47.26 ms | 38.75 ms | 85.53 ms | 134.74 ms | 276.87 ms | 27993.33 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 43.62 ms | 38.93 ms | 57.44 ms | 74.72 ms | 736.07 ms | 28251.33 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 45.44 ms | 40.47 ms | 51.62 ms | 269.67 ms | 759.47 ms | 48071.00 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 53.00 ms | 41.17 ms | 97.20 ms | 160.89 ms | 670.07 ms | 40202.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 57.92 ms | 42.24 ms | 111.61 ms | 201.74 ms | 550.24 ms | 41455.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 43.28 ms | 43.89 ms | 54.04 ms | 81.35 ms | 410.45 ms | 14613.00 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 48.17 ms | 44.74 ms | 59.56 ms | 109.09 ms | 583.67 ms | 26595.33 | 
| node (11.12) | [hapi](http://hapijs.com) (18.1) | 99.06 ms | 46.47 ms | 72.28 ms | 1628.97 ms | 2866.81 ms | 262080.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 50.67 ms | 46.58 ms | 65.35 ms | 79.69 ms | 292.91 ms | 16162.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 90.20 ms | 69.79 ms | 178.64 ms | 240.26 ms | 584.49 ms | 51276.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 85.55 ms | 75.86 ms | 157.93 ms | 260.13 ms | 448.49 ms | 52129.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 91.69 ms | 88.15 ms | 145.37 ms | 192.45 ms | 277.17 ms | 39586.33 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 119.69 ms | 112.69 ms | 201.59 ms | 285.35 ms | 434.24 ms | 56854.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 130.61 ms | 122.49 ms | 180.09 ms | 267.87 ms | 550.05 ms | 43865.33 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 257524.33 | 148.92 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 234821.33 | 281.15 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 209848.00 | 238.59 MB |
| c (99) | [kore](http://kore.io) (3.1) | 184215.00 | 478.85 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 179425.67 | 174.12 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 170668.33 | 193.86 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 168792.00 | 271.34 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 163328.33 | 328.00 MB |
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 163206.00 | 153.46 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 154997.33 | 89.61 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 153079.00 | 160.94 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 150487.33 | 307.37 MB |
| java (8) | [act](http://actframework.org) (1.8) | 128066.67 | 249.97 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 109048.67 | 177.69 MB |
| go (1.12) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 89034.33 | 118.55 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 88829.67 | 119.31 MB |
| go (1.12) | [iris](http://iris-go.com) (11.1) | 88207.67 | 118.53 MB |
| node (11.12) | [restana](http://github.com/jkyberneees/ana) (2.10) | 87741.67 | 131.46 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 85033.00 | 149.38 MB |
| go (1.12) | [violetear](http://violetear.org) (5.0) | 84905.00 | 113.54 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 82128.33 | 144.22 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 81682.67 | 107.92 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 80464.67 | 159.76 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 78737.67 | 106.04 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 76860.67 | 102.63 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 71875.67 | 89.32 MB |
| go (1.12) | [gf](http://goframe.org) (1.5) | 69538.33 | 105.63 MB |
| node (11.12) | [polka](http://github.com/lukeed/polka) (0.5) | 67251.00 | 100.80 MB |
| node (11.12) | [rayo](http://rayo.js.org) (1.2) | 67118.33 | 100.60 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 62038.00 | 159.40 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 59966.00 | 56.43 MB |
| node (11.12) | [fastify](http://fastify.io) (2.1) | 53593.67 | 139.36 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 52500.67 | 129.40 MB |
| node (11.12) | [foxify](http://foxify.js.org) (0.10) | 50962.33 | 107.12 MB |
| node (11.12) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 47758.33 | 71.55 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.3) | 47265.33 | 78.87 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 45939.33 | 98.89 MB |
| node (11.12) | [koa](http://koajs.com) (2.7) | 44182.33 | 93.51 MB |
| node (11.12) | [restify](http://restify.com) (8.2) | 43980.00 | 77.17 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 43438.33 | 76.12 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 41254.67 | 102.29 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 40474.00 | 86.67 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 37055.67 | 192.42 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 36633.00 | 181.66 MB |
| node (11.12) | [express](http://expressjs.com) (4.16) | 36378.33 | 89.01 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 36274.67 | 179.92 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 36179.67 | 179.44 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 35742.00 | 177.42 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.9) | 31684.67 | 68.67 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 30604.33 | 159.18 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 28916.67 | 65.54 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.6) | 27620.00 | 51.24 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 26325.00 | 24.70 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 25373.67 | 41.68 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 24404.67 | 22.87 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 23287.67 | 37.94 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 23000.33 | 28.20 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 21867.67 | 42.20 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 20739.00 | 37.87 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 20468.33 | 38.06 MB |
| node (11.12) | [hapi](http://hapijs.com) (18.1) | 20187.33 | 52.28 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.18) | 19942.67 | 19.02 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 19275.00 | 31.41 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 18552.00 | 45.71 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 15728.00 | 9.09 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 12096.00 | 21.56 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 11595.00 | 6.69 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 11545.67 | 33.50 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 10846.33 | 21.62 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 10089.67 | 76.38 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 8891.67 | 23.06 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 8375.00 | 18.29 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 7466.33 | 21.97 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 2489.67 | 7.62 MB |
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

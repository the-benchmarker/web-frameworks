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
Last update: 2019-03-20
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


:five: symfony (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.08 ms | 0.08 ms | 0.12 ms | 0.15 ms | 14.34 ms | 105.67 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.18) | 3.21 ms | 0.18 ms | 11.59 ms | 28.08 ms | 72.92 ms | 6270.00 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.09 ms | 0.20 ms | 15.35 ms | 34.42 ms | 87.94 ms | 7930.33 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 88.79 ms | 0.28 ms | 238.72 ms | 1386.02 ms | 6985.38 ms | 334600.67 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 109.63 ms | 0.29 ms | 213.21 ms | 2331.71 ms | 6755.01 ms | 431056.00 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.31 ms | 0.29 ms | 0.52 ms | 0.82 ms | 12.08 ms | 192.00 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.94 ms | 0.30 ms | 21.03 ms | 45.27 ms | 102.84 ms | 10579.33 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 151.59 ms | 0.32 ms | 304.31 ms | 3074.59 ms | 6962.66 ms | 560455.67 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 7.49 ms | 0.37 ms | 26.13 ms | 54.56 ms | 133.80 ms | 12967.00 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 131.75 ms | 0.51 ms | 323.16 ms | 2586.48 ms | 7414.49 ms | 478276.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 8.46 ms | 0.52 ms | 27.19 ms | 54.74 ms | 131.06 ms | 13169.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 66.43 ms | 1.55 ms | 3.71 ms | 1830.39 ms | 4944.40 ms | 352195.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 2.74 ms | 1.65 ms | 6.28 ms | 13.76 ms | 38.07 ms | 2932.00 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 104.71 ms | 1.66 ms | 232.39 ms | 2187.69 ms | 5695.94 ms | 389557.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.14 ms | 2.10 ms | 6.73 ms | 16.53 ms | 36.09 ms | 3407.33 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 3.02 ms | 2.21 ms | 6.24 ms | 14.74 ms | 35.56 ms | 2929.67 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.17 ms | 2.49 ms | 5.59 ms | 10.30 ms | 90.01 ms | 2288.00 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 32.27 ms | 2.64 ms | 108.53 ms | 293.61 ms | 769.09 ms | 62508.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 5.65 ms | 2.70 ms | 5.71 ms | 72.77 ms | 681.46 ms | 31077.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.59 ms | 2.87 ms | 7.70 ms | 15.22 ms | 35.70 ms | 3301.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.14 ms | 3.00 ms | 5.33 ms | 7.91 ms | 85.13 ms | 2770.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.12 ms | 3.35 ms | 7.17 ms | 13.85 ms | 55.21 ms | 2654.33 | 
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 3.85 ms | 3.47 ms | 6.90 ms | 13.69 ms | 31.55 ms | 2724.00 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 61.85 ms | 3.59 ms | 145.58 ms | 1297.03 ms | 3259.10 ms | 229819.67 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 4.25 ms | 3.70 ms | 8.10 ms | 18.47 ms | 137.24 ms | 3698.33 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 4.35 ms | 3.72 ms | 8.42 ms | 15.58 ms | 43.39 ms | 3249.00 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 5.74 ms | 3.84 ms | 9.37 ms | 67.79 ms | 118.81 ms | 10179.00 | 
| go (1.12) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 5.56 ms | 4.52 ms | 10.74 ms | 22.79 ms | 60.28 ms | 4387.67 | 
| node (11.11) | [restana](http://github.com/jkyberneees/ana) (2.10) | 5.31 ms | 4.54 ms | 8.95 ms | 16.64 ms | 236.12 ms | 6962.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 5.38 ms | 4.62 ms | 7.66 ms | 14.97 ms | 285.89 ms | 8518.67 | 
| node (11.11) | [polka](http://github.com/lukeed/polka) (0.5) | 5.74 ms | 4.65 ms | 9.25 ms | 17.56 ms | 280.07 ms | 8975.33 | 
| node (11.11) | [rayo](http://rayo.js.org) (1.2) | 5.53 ms | 4.65 ms | 9.23 ms | 16.40 ms | 252.79 ms | 6407.67 | 
| go (1.12) | [iris](http://iris-go.com) (11.1) | 6.18 ms | 4.76 ms | 11.67 ms | 25.61 ms | 177.85 ms | 6434.33 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 6.31 ms | 4.79 ms | 11.65 ms | 26.00 ms | 292.11 ms | 7272.00 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 6.79 ms | 4.82 ms | 14.37 ms | 29.85 ms | 120.77 ms | 6447.00 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 6.84 ms | 4.83 ms | 14.25 ms | 30.05 ms | 273.50 ms | 6909.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.84 ms | 4.84 ms | 9.95 ms | 20.29 ms | 164.71 ms | 5151.00 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 6.73 ms | 4.97 ms | 13.69 ms | 27.95 ms | 59.96 ms | 5551.33 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 7.19 ms | 4.97 ms | 15.35 ms | 32.27 ms | 162.96 ms | 6769.00 | 
| go (1.11) | [beego](http://beego.me) (1.12) | 7.18 ms | 5.10 ms | 14.63 ms | 31.20 ms | 168.98 ms | 6798.67 | 
| node (11.11) | [foxify](http://foxify.js.org) (0.10) | 6.95 ms | 5.23 ms | 10.43 ms | 20.39 ms | 231.11 ms | 7648.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 7.43 ms | 5.34 ms | 12.90 ms | 24.38 ms | 211.90 ms | 6497.67 | 
| node (11.11) | [koa](http://koajs.com) (2.7) | 7.78 ms | 5.68 ms | 10.93 ms | 23.86 ms | 366.80 ms | 12955.33 | 
| go (1.12) | [gf](http://goframe.org) (1.5) | 8.95 ms | 6.11 ms | 18.71 ms | 41.45 ms | 218.27 ms | 8908.00 | 
| node (11.11) | [fastify](http://fastify.io) (2.1) | 8.34 ms | 6.18 ms | 12.06 ms | 32.96 ms | 416.30 ms | 14016.33 | 
| node (11.11) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 9.36 ms | 6.86 ms | 14.78 ms | 34.83 ms | 455.65 ms | 16411.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 236.31 ms | 6.94 ms | 207.42 ms | 5467.07 ms | 7923.22 ms | 923538.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 8.66 ms | 7.76 ms | 13.86 ms | 24.42 ms | 282.05 ms | 8175.00 | 
| node (11.11) | [restify](http://restify.com) (8.1) | 8.29 ms | 7.82 ms | 11.88 ms | 21.96 ms | 204.21 ms | 5805.67 | 
| node (11.11) | [express](http://expressjs.com) (4.16) | 9.69 ms | 8.51 ms | 14.25 ms | 28.84 ms | 349.82 ms | 11781.00 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 12.81 ms | 8.83 ms | 27.03 ms | 49.14 ms | 231.55 ms | 11007.33 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 10.72 ms | 9.13 ms | 18.13 ms | 28.06 ms | 83.75 ms | 5722.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.3) | 12.69 ms | 10.30 ms | 18.44 ms | 34.59 ms | 632.66 ms | 21030.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 10.22 ms | 10.45 ms | 12.23 ms | 14.03 ms | 113.82 ms | 1872.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 15.01 ms | 10.57 ms | 21.80 ms | 90.29 ms | 1329.07 ms | 43406.00 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.7) | 15.61 ms | 14.28 ms | 26.41 ms | 40.91 ms | 74.66 ms | 8178.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 18.18 ms | 16.08 ms | 28.96 ms | 44.19 ms | 65.78 ms | 8553.67 | 
| node (11.11) | [hapi](http://hapijs.com) (18.1) | 23.56 ms | 17.41 ms | 29.45 ms | 198.99 ms | 761.72 ms | 38847.67 | 
| swift (4.2) | [kitura](http://kitura.io) (2.6) | 21.97 ms | 18.47 ms | 28.95 ms | 65.30 ms | 894.12 ms | 28222.33 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 25.22 ms | 18.78 ms | 47.74 ms | 86.16 ms | 304.76 ms | 17126.67 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 22.41 ms | 19.97 ms | 40.40 ms | 63.53 ms | 116.61 ms | 13183.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 27.12 ms | 20.38 ms | 53.38 ms | 99.81 ms | 310.17 ms | 19690.33 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 22.71 ms | 20.40 ms | 30.99 ms | 41.56 ms | 308.92 ms | 8774.33 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 23.32 ms | 21.52 ms | 31.48 ms | 37.88 ms | 241.70 ms | 7401.00 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 27.83 ms | 24.11 ms | 38.41 ms | 88.55 ms | 388.66 ms | 17331.00 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 27.76 ms | 29.01 ms | 35.40 ms | 41.61 ms | 240.21 ms | 7439.00 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 37.63 ms | 31.47 ms | 43.69 ms | 202.61 ms | 992.94 ms | 47868.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 38.88 ms | 32.98 ms | 72.73 ms | 135.61 ms | 273.50 ms | 27650.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 44.04 ms | 35.20 ms | 76.42 ms | 115.54 ms | 168.73 ms | 22375.67 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 32.88 ms | 35.85 ms | 40.58 ms | 52.03 ms | 239.79 ms | 9073.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 43.38 ms | 37.27 ms | 71.89 ms | 111.38 ms | 409.21 ms | 20744.00 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 67.72 ms | 58.26 ms | 129.72 ms | 177.91 ms | 238.20 ms | 41372.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 69.97 ms | 68.17 ms | 93.79 ms | 124.31 ms | 384.40 ms | 21447.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (kore) (c)


:five: (vibora) (python)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 402465.33 | 232.83 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 356754.33 | 427.23 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 339292.00 | 385.38 MB |
| c (99) | [kore](http://kore.io) (3.1) | 322343.67 | 836.46 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 304497.33 | 345.79 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 295363.00 | 475.04 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 284018.33 | 275.69 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 256154.00 | 515.16 MB |
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 251493.33 | 236.33 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 242775.33 | 495.65 MB |
| java (8) | [act](http://actframework.org) (1.8) | 239875.67 | 468.47 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 232274.00 | 247.49 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 231874.33 | 134.02 MB |
| node (11.11) | [restana](http://github.com/jkyberneees/ana) (2.10) | 189657.33 | 284.45 MB |
| go (1.12) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 187914.67 | 251.49 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 184131.67 | 299.87 MB |
| node (11.11) | [polka](http://github.com/lukeed/polka) (0.5) | 181326.33 | 271.88 MB |
| node (11.11) | [rayo](http://rayo.js.org) (1.2) | 179420.33 | 268.94 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 172462.67 | 217.89 MB |
| go (1.12) | [iris](http://iris-go.com) (11.1) | 171994.67 | 229.07 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 167254.67 | 222.49 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 161604.33 | 283.45 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 160400.00 | 281.23 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 159293.00 | 213.28 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 152592.33 | 203.12 MB |
| go (1.11) | [beego](http://beego.me) (1.12) | 152588.67 | 205.63 MB |
| node (11.11) | [foxify](http://foxify.js.org) (0.10) | 145371.00 | 305.80 MB |
| node (11.11) | [fastify](http://fastify.io) (2.1) | 142913.67 | 368.54 MB |
| node (11.11) | [koa](http://koajs.com) (2.7) | 139624.33 | 295.76 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 135440.67 | 348.16 MB |
| node (11.11) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 132913.33 | 199.24 MB |
| go (1.12) | [gf](http://goframe.org) (1.5) | 125929.33 | 191.10 MB |
| node (11.11) | [restify](http://restify.com) (8.1) | 120026.33 | 210.73 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 116168.33 | 286.46 MB |
| node (11.11) | [express](http://expressjs.com) (4.16) | 108108.67 | 264.97 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 97701.67 | 485.96 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 97331.00 | 91.56 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 95491.33 | 189.44 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 92849.67 | 200.02 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 90291.33 | 158.05 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 85688.33 | 212.56 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.3) | 83085.00 | 139.38 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 82013.67 | 175.87 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 74602.00 | 369.80 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 68815.67 | 357.51 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.7) | 64371.67 | 138.83 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 61391.67 | 305.26 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 57304.33 | 90.91 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 57094.67 | 285.37 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 56622.00 | 294.66 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 54966.67 | 124.66 MB |
| node (11.11) | [hapi](http://hapijs.com) (18.1) | 54534.33 | 140.79 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.6) | 48719.00 | 90.35 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 45952.33 | 88.78 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 43682.00 | 40.99 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 42774.67 | 40.09 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 42421.67 | 78.85 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.18) | 39997.67 | 38.11 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 39383.67 | 96.89 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 36576.67 | 44.94 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 36065.67 | 58.78 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 31432.33 | 18.14 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 29998.33 | 54.74 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 28440.33 | 46.37 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 27418.67 | 48.82 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 23053.33 | 45.95 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 22929.67 | 66.54 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 21509.00 | 12.42 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 17125.00 | 129.51 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 15306.67 | 33.36 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 15182.00 | 39.34 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 13954.33 | 41.38 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3962.67 | 12.17 MB |
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

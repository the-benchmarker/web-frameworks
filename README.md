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
Last update: 2019-03-26
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: symfony (php)


:five: slim (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.06 ms | 0.06 ms | 0.09 ms | 0.12 ms | 3.00 ms | 41.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.18) | 2.75 ms | 0.15 ms | 9.96 ms | 25.75 ms | 75.87 ms | 5644.00 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.51 ms | 0.18 ms | 13.36 ms | 31.01 ms | 76.37 ms | 7022.33 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 90.58 ms | 0.24 ms | 169.38 ms | 2007.05 ms | 6720.69 ms | 377427.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 87.08 ms | 0.25 ms | 147.41 ms | 2073.78 ms | 6777.59 ms | 403966.67 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 98.89 ms | 0.25 ms | 225.14 ms | 1955.48 ms | 6447.81 ms | 375272.67 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.30 ms | 0.27 ms | 0.49 ms | 0.87 ms | 16.38 ms | 211.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.42 ms | 0.30 ms | 19.03 ms | 41.30 ms | 97.30 ms | 9577.33 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 6.32 ms | 0.33 ms | 22.49 ms | 47.06 ms | 120.64 ms | 11205.33 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 7.23 ms | 0.42 ms | 23.92 ms | 48.78 ms | 111.99 ms | 11685.00 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 92.81 ms | 1.22 ms | 166.13 ms | 2149.24 ms | 5728.52 ms | 368277.00 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 92.05 ms | 1.26 ms | 171.13 ms | 2030.80 ms | 5266.59 ms | 354774.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 63.09 ms | 1.49 ms | 3.25 ms | 2074.56 ms | 5504.92 ms | 385175.33 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 28.28 ms | 1.69 ms | 97.56 ms | 270.96 ms | 777.91 ms | 57531.67 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 65.18 ms | 1.85 ms | 160.37 ms | 1251.03 ms | 3966.31 ms | 249210.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 2.82 ms | 1.94 ms | 6.19 ms | 12.30 ms | 50.07 ms | 2850.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.71 ms | 1.97 ms | 5.92 ms | 12.21 ms | 40.06 ms | 2652.00 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 2.85 ms | 2.15 ms | 5.88 ms | 13.46 ms | 124.22 ms | 3363.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 6.13 ms | 2.76 ms | 7.18 ms | 63.38 ms | 736.12 ms | 32236.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.47 ms | 2.85 ms | 7.35 ms | 13.54 ms | 32.55 ms | 2986.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.36 ms | 2.94 ms | 5.61 ms | 10.98 ms | 26.50 ms | 2064.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 3.56 ms | 3.02 ms | 6.72 ms | 12.55 ms | 27.84 ms | 2575.67 | 
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 3.34 ms | 3.02 ms | 5.78 ms | 11.24 ms | 23.51 ms | 2241.33 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 4.05 ms | 3.05 ms | 7.58 ms | 18.22 ms | 230.50 ms | 6052.00 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.67 ms | 3.08 ms | 5.78 ms | 11.57 ms | 107.75 ms | 3414.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.13 ms | 3.14 ms | 5.25 ms | 7.30 ms | 96.47 ms | 2115.33 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 5.60 ms | 3.19 ms | 7.89 ms | 77.81 ms | 126.47 ms | 11619.67 | 
| go (1.12) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 5.21 ms | 4.28 ms | 10.19 ms | 21.88 ms | 106.07 ms | 4411.67 | 
| go (1.12) | [iris](http://iris-go.com) (11.1) | 5.17 ms | 4.31 ms | 9.85 ms | 21.23 ms | 158.07 ms | 4775.00 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 5.83 ms | 4.40 ms | 11.48 ms | 24.55 ms | 284.45 ms | 7871.33 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 5.85 ms | 4.44 ms | 12.14 ms | 25.02 ms | 165.19 ms | 5945.00 | 
| node (11.12) | [restana](http://github.com/jkyberneees/ana) (2.10) | 4.78 ms | 4.48 ms | 7.48 ms | 14.53 ms | 206.79 ms | 5083.33 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 5.88 ms | 4.61 ms | 11.06 ms | 23.72 ms | 227.03 ms | 5545.00 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 6.18 ms | 4.64 ms | 12.81 ms | 25.80 ms | 111.08 ms | 5605.33 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 6.68 ms | 4.73 ms | 14.20 ms | 28.94 ms | 208.72 ms | 6497.00 | 
| node (11.12) | [polka](http://github.com/lukeed/polka) (0.5) | 5.82 ms | 4.74 ms | 9.43 ms | 17.78 ms | 214.97 ms | 5906.33 | 
| node (11.12) | [rayo](http://rayo.js.org) (1.2) | 6.37 ms | 4.89 ms | 9.75 ms | 19.27 ms | 236.38 ms | 7070.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 5.58 ms | 5.02 ms | 8.40 ms | 14.71 ms | 158.36 ms | 3449.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.23 ms | 5.30 ms | 10.05 ms | 22.10 ms | 233.07 ms | 5833.67 | 
| go (1.12) | [gf](http://goframe.org) (1.5) | 7.88 ms | 5.38 ms | 16.20 ms | 35.75 ms | 237.11 ms | 8668.33 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 8.07 ms | 5.46 ms | 17.52 ms | 36.05 ms | 125.02 ms | 7503.33 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 7.19 ms | 5.69 ms | 13.64 ms | 24.92 ms | 137.97 ms | 5237.67 | 
| node (11.12) | [koa](http://koajs.com) (2.7) | 7.86 ms | 6.18 ms | 11.12 ms | 23.10 ms | 321.41 ms | 11430.67 | 
| node (11.12) | [fastify](http://fastify.io) (2.1) | 7.76 ms | 6.29 ms | 11.73 ms | 22.49 ms | 319.62 ms | 9764.67 | 
| node (11.12) | [foxify](http://foxify.js.org) (0.10) | 7.83 ms | 6.29 ms | 11.86 ms | 22.86 ms | 283.26 ms | 9029.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 8.39 ms | 6.32 ms | 16.84 ms | 30.16 ms | 113.89 ms | 6679.67 | 
| node (11.12) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 10.38 ms | 6.76 ms | 14.30 ms | 95.51 ms | 524.96 ms | 25663.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 179.18 ms | 6.93 ms | 200.17 ms | 3815.92 ms | 6860.44 ms | 682326.33 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 10.39 ms | 8.12 ms | 17.82 ms | 32.67 ms | 282.71 ms | 8797.67 | 
| node (11.12) | [express](http://expressjs.com) (4.16) | 10.05 ms | 8.24 ms | 13.85 ms | 49.89 ms | 443.95 ms | 17621.67 | 
| node (11.12) | [restify](http://restify.com) (8.2) | 9.36 ms | 8.46 ms | 12.77 ms | 26.50 ms | 250.22 ms | 7535.00 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 9.80 ms | 8.56 ms | 15.85 ms | 23.35 ms | 73.25 ms | 4685.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 10.25 ms | 10.36 ms | 11.82 ms | 13.43 ms | 168.62 ms | 2931.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.3) | 14.34 ms | 10.43 ms | 19.54 ms | 69.76 ms | 845.78 ms | 33597.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 12.80 ms | 10.99 ms | 23.33 ms | 42.43 ms | 385.59 ms | 12506.33 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.9) | 13.38 ms | 12.19 ms | 22.29 ms | 33.70 ms | 56.95 ms | 6770.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 17.12 ms | 13.90 ms | 34.71 ms | 49.32 ms | 81.44 ms | 11281.67 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 22.24 ms | 14.63 ms | 51.06 ms | 70.01 ms | 230.92 ms | 16966.00 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 17.76 ms | 16.89 ms | 30.52 ms | 42.12 ms | 63.44 ms | 9111.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.6) | 23.49 ms | 17.58 ms | 31.97 ms | 118.97 ms | 1061.15 ms | 42578.00 | 
| node (11.12) | [hapi](http://hapijs.com) (18.1) | 23.69 ms | 18.69 ms | 31.38 ms | 113.72 ms | 775.93 ms | 34998.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 27.32 ms | 22.26 ms | 44.35 ms | 81.71 ms | 539.87 ms | 21841.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 23.77 ms | 22.72 ms | 30.91 ms | 67.94 ms | 140.84 ms | 10601.33 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 25.15 ms | 24.87 ms | 32.58 ms | 37.38 ms | 313.69 ms | 10569.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 26.71 ms | 28.25 ms | 34.36 ms | 40.14 ms | 175.86 ms | 7322.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 35.68 ms | 29.26 ms | 40.70 ms | 217.88 ms | 701.82 ms | 41686.33 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 29.18 ms | 29.50 ms | 35.18 ms | 39.41 ms | 317.53 ms | 9274.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 31.32 ms | 30.60 ms | 35.99 ms | 39.20 ms | 219.27 ms | 5896.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 39.04 ms | 33.39 ms | 70.08 ms | 129.93 ms | 255.47 ms | 25489.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 39.40 ms | 36.76 ms | 57.68 ms | 81.56 ms | 121.03 ms | 13272.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 44.88 ms | 40.25 ms | 64.90 ms | 100.64 ms | 441.96 ms | 19895.00 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 50.84 ms | 45.93 ms | 88.07 ms | 116.39 ms | 159.51 ms | 26588.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 68.36 ms | 69.99 ms | 81.76 ms | 101.96 ms | 408.50 ms | 17560.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (agoo-c) (c)


:three: (actix-web) (rust)


:four: (jester) (nim)


:five: (vibora) (python)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 388815.33 | 465.47 MB |
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 369366.33 | 213.72 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 355885.33 | 404.70 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 312800.00 | 628.82 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 311038.00 | 353.16 MB |
| c (99) | [kore](http://kore.io) (3.1) | 288263.67 | 748.89 MB |
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 283548.00 | 266.80 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 280011.00 | 271.86 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 278838.33 | 298.04 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 267422.33 | 547.78 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 267009.00 | 154.51 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 257099.00 | 415.97 MB |
| java (8) | [act](http://actframework.org) (1.8) | 229495.67 | 448.36 MB |
| node (11.12) | [restana](http://github.com/jkyberneees/ana) (2.10) | 203376.00 | 304.23 MB |
| go (1.12) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 202382.67 | 270.12 MB |
| go (1.12) | [iris](http://iris-go.com) (11.1) | 201471.67 | 269.53 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 188828.00 | 254.39 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 184766.33 | 323.99 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 177940.00 | 236.03 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 176228.67 | 222.59 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 173883.67 | 232.86 MB |
| node (11.12) | [polka](http://github.com/lukeed/polka) (0.5) | 169292.67 | 253.73 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 168890.67 | 275.26 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 164515.67 | 288.69 MB |
| node (11.12) | [rayo](http://rayo.js.org) (1.2) | 157706.33 | 235.78 MB |
| node (11.12) | [fastify](http://fastify.io) (2.1) | 147003.00 | 369.59 MB |
| go (1.12) | [gf](http://goframe.org) (1.5) | 142696.33 | 215.96 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 142624.00 | 366.63 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 138746.67 | 184.89 MB |
| node (11.12) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 135613.33 | 203.27 MB |
| node (11.12) | [koa](http://koajs.com) (2.7) | 134013.67 | 283.94 MB |
| node (11.12) | [foxify](http://foxify.js.org) (0.10) | 131133.00 | 275.73 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 128534.33 | 255.79 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 126513.00 | 311.74 MB |
| node (11.12) | [express](http://expressjs.com) (4.16) | 111932.67 | 274.11 MB |
| node (11.12) | [restify](http://restify.com) (8.2) | 108143.33 | 189.79 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 100660.67 | 216.87 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 98662.00 | 244.31 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 98113.33 | 92.21 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 97372.00 | 483.56 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 96192.00 | 477.50 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 88586.00 | 438.77 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 87520.33 | 454.59 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 87333.67 | 432.79 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 84967.00 | 148.62 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.3) | 82060.67 | 137.48 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 80108.33 | 171.70 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.9) | 75454.67 | 162.97 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 66397.33 | 345.58 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 62329.00 | 141.20 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 60124.00 | 95.51 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 56680.33 | 109.52 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 51047.33 | 94.87 MB |
| node (11.12) | [hapi](http://hapijs.com) (18.1) | 48786.33 | 126.14 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.6) | 47205.00 | 87.50 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.18) | 46905.67 | 44.72 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 43045.33 | 40.35 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 40099.00 | 37.56 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 37569.00 | 61.20 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 37265.00 | 91.71 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 36575.67 | 21.10 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 34335.67 | 42.03 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 32299.00 | 52.63 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 31960.67 | 58.37 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 27054.67 | 48.19 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 25215.00 | 50.32 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 23558.67 | 13.58 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 22152.67 | 64.25 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 20353.67 | 153.98 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 19741.67 | 43.01 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 17689.67 | 45.85 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 14116.33 | 41.51 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 4547.33 | 13.92 MB |
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

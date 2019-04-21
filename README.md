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
Last update: 2019-04-21
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


:five: zend-expressive (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 0.13 ms | 0.10 ms | 0.14 ms | 0.39 ms | 31.76 ms | 571.67 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.19) | 5.66 ms | 0.23 ms | 23.84 ms | 62.40 ms | 162.55 ms | 13656.33 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 7.88 ms | 0.32 ms | 33.30 ms | 76.68 ms | 200.16 ms | 17439.67 | 
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 0.46 ms | 0.34 ms | 0.66 ms | 2.74 ms | 141.75 ms | 1335.00 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 189.96 ms | 0.55 ms | 356.90 ms | 3929.11 ms | 7104.83 ms | 666201.33 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 12.53 ms | 0.58 ms | 47.37 ms | 103.74 ms | 262.82 ms | 24005.33 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 217.12 ms | 0.67 ms | 393.47 ms | 4357.93 ms | 7366.98 ms | 753762.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 13.21 ms | 0.69 ms | 47.90 ms | 100.12 ms | 240.63 ms | 23671.67 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 15.26 ms | 0.75 ms | 56.86 ms | 117.13 ms | 261.35 ms | 27791.00 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 188.99 ms | 0.85 ms | 427.43 ms | 3488.23 ms | 7100.68 ms | 621556.67 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 223.16 ms | 1.01 ms | 561.52 ms | 3919.21 ms | 7334.02 ms | 703624.67 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 139.07 ms | 1.57 ms | 291.38 ms | 2760.51 ms | 6484.22 ms | 500208.33 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 219.62 ms | 1.78 ms | 377.36 ms | 4670.28 ms | 7514.72 ms | 788722.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 110.80 ms | 1.95 ms | 5.82 ms | 3781.93 ms | 6597.33 ms | 616746.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 3.92 ms | 2.83 ms | 8.11 ms | 19.20 ms | 68.62 ms | 4008.67 | 
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 4.35 ms | 3.33 ms | 8.89 ms | 20.37 ms | 181.04 ms | 4632.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 4.22 ms | 3.62 ms | 8.27 ms | 18.27 ms | 71.09 ms | 3857.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 6.66 ms | 3.78 ms | 8.62 ms | 51.61 ms | 594.99 ms | 25186.00 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 58.56 ms | 4.59 ms | 196.84 ms | 535.82 ms | 1194.89 ms | 113936.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 6.18 ms | 4.66 ms | 12.66 ms | 27.72 ms | 93.42 ms | 5759.33 | 
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 5.18 ms | 4.87 ms | 8.68 ms | 17.70 ms | 61.04 ms | 3298.33 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.38 ms | 4.94 ms | 8.58 ms | 18.37 ms | 109.72 ms | 3893.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 6.27 ms | 4.96 ms | 12.97 ms | 29.88 ms | 122.13 ms | 6214.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 6.20 ms | 5.02 ms | 12.01 ms | 25.07 ms | 80.46 ms | 5055.33 | 
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 7.16 ms | 5.33 ms | 14.68 ms | 32.84 ms | 183.39 ms | 7185.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.51 ms | 5.52 ms | 8.25 ms | 14.44 ms | 37.56 ms | 2734.00 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 9.53 ms | 6.61 ms | 19.16 ms | 40.81 ms | 254.83 ms | 10205.67 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 9.88 ms | 6.71 ms | 20.83 ms | 44.48 ms | 211.71 ms | 9123.00 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 9.98 ms | 7.17 ms | 20.27 ms | 42.32 ms | 151.53 ms | 8569.00 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 9.41 ms | 7.18 ms | 17.71 ms | 37.69 ms | 167.61 ms | 7409.00 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 10.60 ms | 7.29 ms | 22.57 ms | 47.16 ms | 169.82 ms | 9554.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 8.56 ms | 7.33 ms | 14.30 ms | 32.74 ms | 210.92 ms | 8114.67 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 10.12 ms | 7.59 ms | 19.62 ms | 41.69 ms | 186.62 ms | 8719.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 8.71 ms | 7.62 ms | 13.59 ms | 29.25 ms | 232.55 ms | 6293.67 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 11.03 ms | 7.64 ms | 23.25 ms | 48.28 ms | 166.28 ms | 9965.33 | 
| node (11.14) | [restana](http://github.com/jkyberneees/ana) (2.13) | 13.04 ms | 7.91 ms | 24.97 ms | 72.93 ms | 515.41 ms | 23443.67 | 
| node (11.14) | [polka](http://github.com/lukeed/polka) (0.5) | 11.26 ms | 8.20 ms | 20.95 ms | 49.25 ms | 412.74 ms | 15557.00 | 
| node (11.14) | [rayo](http://rayo.js.org) (1.2) | 13.70 ms | 8.60 ms | 24.46 ms | 94.30 ms | 591.33 ms | 27069.67 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 12.20 ms | 8.86 ms | 27.60 ms | 53.00 ms | 168.12 ms | 11020.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 170.38 ms | 9.43 ms | 33.62 ms | 3894.97 ms | 7110.42 ms | 694935.33 | 
| go (1.12) | [gf](http://goframe.org) (1.6) | 14.17 ms | 9.82 ms | 29.85 ms | 65.57 ms | 202.20 ms | 13268.00 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 15.70 ms | 9.90 ms | 34.16 ms | 103.09 ms | 169.46 ms | 19550.00 | 
| node (11.14) | [foxify](http://foxify.js.org) (0.10) | 16.40 ms | 10.40 ms | 28.09 ms | 106.22 ms | 658.72 ms | 29551.33 | 
| node (11.14) | [koa](http://koajs.com) (2.7) | 17.16 ms | 11.23 ms | 28.97 ms | 97.19 ms | 695.78 ms | 30816.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 14.16 ms | 11.47 ms | 25.44 ms | 49.01 ms | 211.35 ms | 9920.00 | 
| node (11.14) | [fastify](http://fastify.io) (2.3) | 18.96 ms | 11.76 ms | 30.94 ms | 159.79 ms | 785.24 ms | 38215.33 | 
| node (11.14) | [restify](http://restify.com) (8.2) | 17.53 ms | 13.40 ms | 30.68 ms | 66.79 ms | 367.04 ms | 14117.67 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 19.80 ms | 14.15 ms | 26.53 ms | 123.89 ms | 1086.77 ms | 45121.33 | 
| node (11.14) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 24.23 ms | 14.21 ms | 40.53 ms | 255.65 ms | 925.01 ms | 51096.33 | 
| node (11.14) | [express](http://expressjs.com) (4.16) | 21.82 ms | 14.50 ms | 36.01 ms | 139.71 ms | 819.62 ms | 37035.00 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 17.80 ms | 15.20 ms | 30.52 ms | 50.91 ms | 234.68 ms | 10696.33 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 17.49 ms | 15.76 ms | 30.85 ms | 46.82 ms | 124.23 ms | 9713.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 29.54 ms | 15.89 ms | 34.40 ms | 519.81 ms | 1578.48 ms | 99717.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 16.82 ms | 16.00 ms | 22.95 ms | 35.24 ms | 96.77 ms | 5316.67 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.16) | 25.46 ms | 23.36 ms | 40.45 ms | 64.05 ms | 138.83 ms | 11705.67 | 
| swift (5.0) | [kitura](http://kitura.io) (2.6) | 31.33 ms | 25.57 ms | 44.44 ms | 96.90 ms | 1205.45 ms | 42172.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 30.41 ms | 29.07 ms | 52.24 ms | 76.70 ms | 178.13 ms | 16674.00 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 32.04 ms | 29.99 ms | 44.00 ms | 54.10 ms | 270.03 ms | 12000.33 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 36.34 ms | 32.69 ms | 59.29 ms | 82.03 ms | 144.01 ms | 16541.67 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 41.09 ms | 33.37 ms | 78.73 ms | 110.94 ms | 516.09 ms | 31322.33 | 
| node (11.14) | [hapi](http://hapijs.com) (18.1) | 70.23 ms | 33.76 ms | 70.66 ms | 1128.92 ms | 2315.73 ms | 184203.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 44.40 ms | 35.79 ms | 58.68 ms | 206.29 ms | 441.16 ms | 32163.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 37.78 ms | 37.14 ms | 52.98 ms | 86.80 ms | 246.50 ms | 14358.00 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 47.54 ms | 39.99 ms | 67.89 ms | 200.94 ms | 346.72 ms | 30300.00 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 39.40 ms | 40.52 ms | 52.31 ms | 71.12 ms | 317.45 ms | 13664.67 | 
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 47.07 ms | 41.53 ms | 54.70 ms | 241.48 ms | 708.52 ms | 47527.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 48.72 ms | 44.84 ms | 73.80 ms | 125.40 ms | 443.97 ms | 24343.00 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 47.46 ms | 46.76 ms | 60.56 ms | 77.07 ms | 248.12 ms | 11627.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 71.64 ms | 70.07 ms | 109.37 ms | 154.59 ms | 252.57 ms | 31794.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 89.81 ms | 80.19 ms | 161.34 ms | 257.31 ms | 397.99 ms | 52451.67 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 104.18 ms | 100.09 ms | 160.75 ms | 215.69 ms | 286.80 ms | 38610.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 104.47 ms | 101.37 ms | 131.54 ms | 199.40 ms | 643.49 ms | 33797.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 115.70 ms | 108.47 ms | 150.90 ms | 304.92 ms | 1236.43 ms | 61089.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (kore) (c)


:five: (jester) (nim)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 268047.00 | 154.86 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 237708.00 | 284.44 MB |
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 230007.67 | 261.54 MB |
| c (99) | [kore](http://kore.io) (3.1) | 207923.33 | 540.37 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 185497.00 | 372.82 MB |
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 183234.67 | 172.38 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 182458.33 | 207.16 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 174926.33 | 282.63 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 167543.00 | 162.59 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 167038.33 | 179.39 MB |
| java (8) | [act](http://actframework.org) (1.8) | 159651.00 | 311.75 MB |
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 149184.67 | 305.91 MB |
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 121509.00 | 153.36 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 113647.67 | 153.07 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 111208.33 | 195.27 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 110409.33 | 179.82 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 109332.67 | 146.11 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 107942.67 | 146.05 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 105094.33 | 139.91 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 103812.67 | 140.18 MB |
| node (11.14) | [polka](http://github.com/lukeed/polka) (0.5) | 99519.67 | 149.16 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 99132.00 | 174.08 MB |
| node (11.14) | [restana](http://github.com/jkyberneees/ana) (2.13) | 94890.67 | 142.25 MB |
| node (11.14) | [rayo](http://rayo.js.org) (1.2) | 93104.67 | 139.57 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 92468.33 | 237.75 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 87869.00 | 50.77 MB |
| go (1.12) | [gf](http://goframe.org) (1.6) | 80090.33 | 121.00 MB |
| node (11.14) | [foxify](http://foxify.js.org) (0.10) | 74585.00 | 156.89 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 72995.33 | 179.88 MB |
| node (11.14) | [fastify](http://fastify.io) (2.3) | 71628.00 | 188.34 MB |
| node (11.14) | [koa](http://koajs.com) (2.7) | 69466.33 | 147.14 MB |
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 63343.00 | 125.85 MB |
| node (11.14) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 61957.67 | 92.85 MB |
| node (11.14) | [restify](http://restify.com) (8.2) | 60443.67 | 106.18 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 59724.33 | 100.30 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 59269.33 | 103.97 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 58308.67 | 54.86 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 57501.67 | 124.02 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 56928.33 | 141.25 MB |
| node (11.14) | [express](http://expressjs.com) (4.16) | 55483.67 | 135.99 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 54639.33 | 271.17 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 51049.00 | 254.05 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 50041.33 | 107.48 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 49164.33 | 244.22 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 47898.33 | 238.24 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 43079.67 | 67.83 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.16) | 39585.67 | 85.56 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 37498.67 | 195.21 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.6) | 33924.33 | 63.15 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 33770.00 | 176.12 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 33604.00 | 76.19 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 31125.33 | 29.21 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 27767.00 | 53.61 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 27081.33 | 25.45 MB |
| node (11.14) | [hapi](http://hapijs.com) (18.1) | 26336.67 | 68.27 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 25907.00 | 48.14 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 25597.33 | 41.77 MB |
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 23734.67 | 29.69 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 23670.33 | 38.59 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.19) | 23128.33 | 22.11 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 22010.67 | 27.22 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 21035.67 | 38.52 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 20115.33 | 49.56 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 16481.00 | 9.52 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 13888.00 | 27.74 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 11358.33 | 20.30 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 10242.00 | 5.92 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 9679.00 | 25.16 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 9542.33 | 20.88 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 9265.00 | 27.48 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 8591.33 | 24.93 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 8320.00 | 63.11 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 2172.00 | 6.70 MB |
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

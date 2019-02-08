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
Last update: 2019-02-08
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


:four: zend-expressive (php)


:five: iron (rust)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.06 ms | 0.07 ms | 0.09 ms | 0.11 ms | 0.73 ms | 23.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 2.44 ms | 0.13 ms | 8.88 ms | 24.81 ms | 68.02 ms | 5318.00 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.01 ms | 0.16 ms | 11.04 ms | 27.18 ms | 68.67 ms | 6026.00 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 101.40 ms | 0.23 ms | 163.88 ms | 2400.83 ms | 6796.14 ms | 429411.00 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.25 ms | 0.23 ms | 0.42 ms | 0.70 ms | 8.01 ms | 156.33 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 4.61 ms | 0.26 ms | 16.54 ms | 37.54 ms | 90.64 ms | 8557.00 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 5.34 ms | 0.29 ms | 18.51 ms | 39.66 ms | 98.69 ms | 9295.00 | 
| php (7.3) | [laravel](http://laravel.com) (5.7) | 98.13 ms | 0.29 ms | 222.25 ms | 1929.24 ms | 6774.11 ms | 379416.67 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 5.99 ms | 0.36 ms | 19.76 ms | 41.25 ms | 100.56 ms | 9763.33 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 79.55 ms | 0.37 ms | 174.93 ms | 1552.80 ms | 6749.71 ms | 337700.33 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 23.84 ms | 1.01 ms | 83.66 ms | 248.29 ms | 675.05 ms | 52174.00 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 66.02 ms | 1.12 ms | 145.25 ms | 1211.59 ms | 5070.45 ms | 275501.67 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 92.99 ms | 1.16 ms | 154.02 ms | 2150.95 ms | 5237.29 ms | 381502.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 97.94 ms | 1.22 ms | 3.07 ms | 2408.76 ms | 5505.38 ms | 471048.00 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 76.67 ms | 1.25 ms | 151.10 ms | 1695.39 ms | 5420.28 ms | 317723.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.85 ms | 1.67 ms | 6.54 ms | 15.78 ms | 35.24 ms | 3295.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 2.09 ms | 1.73 ms | 3.50 ms | 8.64 ms | 32.08 ms | 1622.33 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 2.69 ms | 2.07 ms | 5.58 ms | 12.34 ms | 33.68 ms | 2491.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 2.84 ms | 2.11 ms | 5.42 ms | 8.82 ms | 100.28 ms | 2846.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.03 ms | 2.37 ms | 6.42 ms | 13.20 ms | 34.34 ms | 2848.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.50 ms | 2.48 ms | 3.98 ms | 6.02 ms | 51.29 ms | 1517.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.7) | 5.12 ms | 2.77 ms | 7.70 ms | 61.03 ms | 85.51 ms | 9878.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.33 ms | 2.86 ms | 5.74 ms | 11.37 ms | 27.17 ms | 2196.67 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 3.37 ms | 2.90 ms | 6.19 ms | 12.01 ms | 27.87 ms | 2464.00 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 3.76 ms | 3.04 ms | 7.11 ms | 16.87 ms | 69.06 ms | 3298.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 3.62 ms | 3.05 ms | 6.92 ms | 13.04 ms | 28.80 ms | 2677.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 3.75 ms | 3.67 ms | 6.28 ms | 8.66 ms | 17.51 ms | 1945.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.04 ms | 4.22 ms | 8.35 ms | 20.39 ms | 95.35 ms | 4146.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 4.85 ms | 4.38 ms | 8.10 ms | 16.12 ms | 198.57 ms | 4927.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 4.71 ms | 4.44 ms | 6.86 ms | 13.07 ms | 111.43 ms | 2857.00 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 5.28 ms | 4.45 ms | 8.43 ms | 18.30 ms | 234.20 ms | 8214.67 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 5.54 ms | 4.57 ms | 9.25 ms | 19.35 ms | 281.06 ms | 7032.67 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 5.41 ms | 4.59 ms | 9.09 ms | 18.43 ms | 174.04 ms | 5400.67 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 5.54 ms | 4.66 ms | 9.39 ms | 19.51 ms | 276.06 ms | 5712.00 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 5.62 ms | 4.70 ms | 9.73 ms | 19.33 ms | 53.86 ms | 3591.67 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 6.30 ms | 4.74 ms | 11.55 ms | 21.23 ms | 205.92 ms | 5275.67 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 5.89 ms | 4.77 ms | 9.77 ms | 19.45 ms | 230.14 ms | 6855.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 7.46 ms | 4.87 ms | 14.82 ms | 26.94 ms | 221.69 ms | 6382.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 195.92 ms | 6.14 ms | 27.15 ms | 4919.85 ms | 7903.22 ms | 829583.67 | 
| node (11.9) | [polka](http://github.com/lukeed/polka) (0.5) | 8.62 ms | 6.19 ms | 16.61 ms | 36.21 ms | 294.52 ms | 9610.00 | 
| go (1.11) | [gf](http://gfer.me) (1.4) | 7.92 ms | 6.87 ms | 11.69 ms | 27.09 ms | 260.86 ms | 8612.33 | 
| node (11.9) | [rayo](http://rayo.js.org) (1.2) | 10.61 ms | 7.88 ms | 19.81 ms | 40.84 ms | 330.41 ms | 11200.67 | 
| python (3.7) | [starlette](http://starlette.io) (0.10) | 9.30 ms | 8.06 ms | 16.82 ms | 29.11 ms | 69.12 ms | 5662.67 | 
| node (11.9) | [restana](http://github.com/jkyberneees/ana) (2.8) | 10.88 ms | 8.42 ms | 20.21 ms | 39.40 ms | 306.49 ms | 10253.00 | 
| node (11.9) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 11.48 ms | 8.53 ms | 19.60 ms | 42.82 ms | 430.38 ms | 14961.33 | 
| node (11.9) | [foxify](http://foxify.js.org) (0.10) | 12.47 ms | 9.30 ms | 22.37 ms | 46.41 ms | 436.86 ms | 14803.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 11.28 ms | 9.60 ms | 20.03 ms | 42.20 ms | 403.32 ms | 12171.67 | 
| node (11.9) | [fastify](http://fastify.io) (1.14) | 12.89 ms | 9.63 ms | 22.48 ms | 48.77 ms | 467.56 ms | 16642.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 14.58 ms | 9.76 ms | 17.64 ms | 94.09 ms | 998.07 ms | 41092.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 9.81 ms | 9.98 ms | 11.34 ms | 12.96 ms | 86.84 ms | 2088.67 | 
| node (11.9) | [koa](http://koajs.com) (2.7) | 14.14 ms | 10.68 ms | 22.94 ms | 51.56 ms | 555.83 ms | 20485.00 | 
| node (11.9) | [express](http://expressjs.com) (4.16) | 18.19 ms | 12.76 ms | 26.81 ms | 111.32 ms | 786.35 ms | 35254.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 14.85 ms | 13.09 ms | 28.99 ms | 43.77 ms | 68.61 ms | 9054.00 | 
| node (11.9) | [restify](http://restify.com) (7.7) | 17.31 ms | 14.25 ms | 29.63 ms | 51.38 ms | 375.24 ms | 14168.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 24.09 ms | 16.80 ms | 47.40 ms | 93.66 ms | 309.50 ms | 19661.67 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 19.76 ms | 16.98 ms | 26.73 ms | 45.53 ms | 604.57 ms | 24086.33 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 23.91 ms | 21.23 ms | 35.60 ms | 47.90 ms | 320.96 ms | 9492.00 | 
| node (11.9) | [hapi](http://hapijs.com) (18.1) | 29.64 ms | 22.13 ms | 39.99 ms | 234.52 ms | 979.81 ms | 49854.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 22.31 ms | 23.34 ms | 29.12 ms | 37.34 ms | 43.55 ms | 5817.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 23.49 ms | 24.86 ms | 30.35 ms | 38.28 ms | 257.84 ms | 7548.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 27.11 ms | 25.08 ms | 36.14 ms | 47.03 ms | 528.18 ms | 17906.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 30.68 ms | 26.00 ms | 55.55 ms | 104.52 ms | 321.38 ms | 21552.67 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 25.56 ms | 26.35 ms | 32.50 ms | 45.92 ms | 321.42 ms | 11044.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 36.80 ms | 29.07 ms | 60.50 ms | 104.28 ms | 430.04 ms | 21553.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 40.07 ms | 30.95 ms | 81.09 ms | 125.23 ms | 207.38 ms | 26490.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 31.06 ms | 31.23 ms | 39.04 ms | 84.88 ms | 553.87 ms | 23521.33 | 
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.1) | 48.25 ms | 44.50 ms | 86.09 ms | 122.51 ms | 160.66 ms | 27116.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 58.85 ms | 59.07 ms | 71.97 ms | 120.28 ms | 610.44 ms | 24188.67 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 469437.33 | 271.72 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 424491.67 | 507.65 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 373238.67 | 423.99 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 362282.67 | 411.52 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 348401.33 | 337.91 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 330113.33 | 533.03 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 322223.00 | 647.65 MB |
| java (8) | [act](http://actframework.org) (1.8) | 290986.00 | 567.94 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 287337.00 | 270.25 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 276681.33 | 296.95 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 273817.00 | 560.73 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 205060.33 | 275.03 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 199926.33 | 325.75 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.7) | 198240.33 | 115.12 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 197210.00 | 263.22 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 188006.67 | 235.89 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 184732.00 | 247.59 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 183417.67 | 321.86 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 179874.33 | 243.20 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 175429.67 | 307.98 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 172093.00 | 229.95 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 163650.67 | 420.85 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 141588.33 | 349.01 MB |
| go (1.11) | [gf](http://gfer.me) (1.4) | 131533.33 | 199.47 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 131027.67 | 262.01 MB |
| node (11.9) | [polka](http://github.com/lukeed/polka) (0.5) | 130365.00 | 195.24 MB |
| python (3.7) | [starlette](http://starlette.io) (0.10) | 109857.67 | 236.57 MB |
| node (11.9) | [rayo](http://rayo.js.org) (1.2) | 105084.00 | 157.18 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 102224.67 | 96.06 MB |
| node (11.9) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 102184.67 | 152.91 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 100299.00 | 497.93 MB |
| node (11.9) | [restana](http://github.com/jkyberneees/ana) (2.8) | 100001.00 | 149.46 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 99107.67 | 173.44 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 98614.67 | 489.43 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 91207.00 | 195.35 MB |
| node (11.9) | [fastify](http://fastify.io) (1.14) | 90767.33 | 221.78 MB |
| node (11.9) | [foxify](http://foxify.js.org) (0.10) | 90577.33 | 189.90 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 90464.33 | 469.91 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 90341.00 | 447.78 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 90143.00 | 446.72 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 86208.00 | 144.85 MB |
| node (11.9) | [koa](http://koajs.com) (2.7) | 81185.00 | 171.33 MB |
| c (99) | [kore](http://kore.io) (3.1) | 78892.33 | 214.02 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 71025.33 | 161.04 MB |
| php (7.3) | [laravel](http://laravel.com) (5.7) | 70964.67 | 369.51 MB |
| node (11.9) | [express](http://expressjs.com) (4.16) | 67932.33 | 165.80 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 61540.67 | 92.99 MB |
| node (11.9) | [restify](http://restify.com) (7.7) | 59796.00 | 104.58 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 53078.00 | 50.58 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 51967.67 | 96.40 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 47468.33 | 116.87 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 45253.33 | 42.42 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 42861.67 | 24.71 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 42699.00 | 40.02 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 41070.67 | 50.46 MB |
| node (11.9) | [hapi](http://hapijs.com) (18.1) | 40851.67 | 105.50 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 39182.33 | 63.88 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 36894.67 | 67.42 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 34494.33 | 61.35 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 32959.67 | 53.73 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 27723.00 | 15.98 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 27535.00 | 79.92 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 26952.33 | 53.77 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 23892.33 | 180.51 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 21348.33 | 55.34 MB |
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.1) | 20960.33 | 45.61 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 16707.00 | 49.14 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 5424.33 | 16.66 MB |
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

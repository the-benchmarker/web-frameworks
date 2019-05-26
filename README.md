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
Last update: 2019-05-26
```
OS: Linux (version: 5.0.16-300.fc30.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: flame (ruby)


:five: slim (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.35) | [nickel](http://nickel-org.github.io) (0.11) | 0.09 ms | 0.10 ms | 0.13 ms | 0.18 ms | 5.15 ms | 45.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 3.04 ms | 0.16 ms | 11.21 ms | 27.99 ms | 74.84 ms | 6184.67 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.96 ms | 0.20 ms | 14.75 ms | 33.19 ms | 80.97 ms | 7615.33 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.98 ms | 0.33 ms | 20.79 ms | 44.23 ms | 104.76 ms | 10369.67 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 115.42 ms | 0.34 ms | 217.88 ms | 2556.86 ms | 6800.61 ms | 455305.33 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 90.46 ms | 0.34 ms | 277.67 ms | 1306.09 ms | 7062.07 ms | 301041.67 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 137.08 ms | 0.34 ms | 241.31 ms | 3044.81 ms | 6939.14 ms | 524795.00 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 174.87 ms | 0.35 ms | 275.58 ms | 3897.37 ms | 6898.22 ms | 647599.00 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 134.31 ms | 0.36 ms | 260.98 ms | 2984.44 ms | 6930.88 ms | 508801.00 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 163.23 ms | 0.36 ms | 265.43 ms | 3720.63 ms | 7096.96 ms | 620420.67 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 6.97 ms | 0.36 ms | 24.13 ms | 49.85 ms | 124.92 ms | 11931.67 | 
| rust (1.35) | [iron](http://ironframework.io) (0.6) | 0.36 ms | 0.37 ms | 0.57 ms | 0.89 ms | 11.24 ms | 209.33 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 7.95 ms | 0.48 ms | 25.60 ms | 52.41 ms | 136.14 ms | 12529.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 30.30 ms | 1.47 ms | 3.03 ms | 1015.14 ms | 3814.02 ms | 194974.33 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 2.86 ms | 1.74 ms | 6.50 ms | 13.93 ms | 33.38 ms | 3029.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.24 ms | 2.23 ms | 6.78 ms | 16.09 ms | 35.57 ms | 3333.67 | 
| rust (1.35) | [actix-web](http://actix.rs) (0.7) | 3.08 ms | 2.33 ms | 6.36 ms | 14.01 ms | 36.28 ms | 2884.00 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 32.32 ms | 2.42 ms | 109.59 ms | 293.96 ms | 780.49 ms | 62852.67 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.48 ms | 2.87 ms | 5.73 ms | 11.10 ms | 86.40 ms | 2354.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 9.90 ms | 2.90 ms | 7.06 ms | 240.24 ms | 921.83 ms | 53917.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.10 ms | 3.25 ms | 8.76 ms | 16.48 ms | 34.78 ms | 3594.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.34 ms | 3.41 ms | 5.36 ms | 8.07 ms | 23.89 ms | 1792.67 | 
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 3.94 ms | 3.73 ms | 6.59 ms | 12.75 ms | 30.36 ms | 2456.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.49 ms | 3.79 ms | 7.21 ms | 14.35 ms | 48.38 ms | 2708.33 | 
| rust (1.35) | [gotham](http://gotham.rs) (0.3) | 4.40 ms | 3.81 ms | 8.33 ms | 18.20 ms | 101.08 ms | 4151.33 | 
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 4.28 ms | 3.82 ms | 7.70 ms | 14.20 ms | 30.81 ms | 2792.33 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 5.91 ms | 3.88 ms | 9.39 ms | 67.62 ms | 121.53 ms | 10581.33 | 
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 4.18 ms | 4.11 ms | 6.76 ms | 13.12 ms | 29.01 ms | 2465.33 | 
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 4.19 ms | 4.11 ms | 6.79 ms | 13.15 ms | 28.56 ms | 2456.67 | 
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 4.36 ms | 4.27 ms | 6.95 ms | 12.91 ms | 29.38 ms | 2408.33 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 6.37 ms | 4.64 ms | 13.17 ms | 27.51 ms | 214.28 ms | 6695.33 | 
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 6.69 ms | 4.67 ms | 14.47 ms | 31.55 ms | 123.96 ms | 6623.00 | 
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 5.07 ms | 4.68 ms | 8.36 ms | 14.21 ms | 36.38 ms | 2647.00 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 7.29 ms | 4.83 ms | 15.91 ms | 33.15 ms | 234.54 ms | 8689.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 5.42 ms | 4.87 ms | 8.14 ms | 14.44 ms | 216.16 ms | 4794.67 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 7.00 ms | 4.87 ms | 15.10 ms | 30.61 ms | 208.35 ms | 6569.67 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 6.38 ms | 4.88 ms | 12.10 ms | 25.52 ms | 107.37 ms | 5051.00 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 7.01 ms | 4.90 ms | 14.93 ms | 30.38 ms | 123.59 ms | 6465.00 | 
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 7.79 ms | 4.92 ms | 16.50 ms | 32.29 ms | 207.84 ms | 7244.00 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 7.20 ms | 4.95 ms | 14.67 ms | 31.47 ms | 339.71 ms | 9465.00 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 6.63 ms | 4.97 ms | 12.73 ms | 27.40 ms | 180.63 ms | 6175.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.90 ms | 5.13 ms | 9.68 ms | 18.51 ms | 210.00 ms | 4624.00 | 
| node (12.3) | [restana](http://github.com/jkyberneees/ana) (3.0) | 13.13 ms | 5.26 ms | 13.32 ms | 230.33 ms | 616.07 ms | 41723.33 | 
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 7.08 ms | 5.33 ms | 13.99 ms | 30.95 ms | 94.85 ms | 6297.00 | 
| node (12.3) | [polka](http://github.com/lukeed/polka) (0.5) | 8.86 ms | 5.71 ms | 12.77 ms | 54.28 ms | 540.90 ms | 21371.00 | 
| go (1.12) | [gf](http://goframe.org) (1.6) | 8.61 ms | 5.99 ms | 18.18 ms | 38.12 ms | 94.60 ms | 7687.67 | 
| node (12.3) | [rayo](http://rayo.js.org) (1.3) | 10.43 ms | 6.49 ms | 13.84 ms | 116.02 ms | 553.71 ms | 27424.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 192.12 ms | 7.56 ms | 82.29 ms | 4645.86 ms | 7937.79 ms | 803805.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 9.88 ms | 7.76 ms | 18.43 ms | 34.76 ms | 211.97 ms | 7572.67 | 
| node (12.3) | [foxify](http://foxify.js.org) (0.10) | 11.60 ms | 7.84 ms | 15.19 ms | 106.95 ms | 551.58 ms | 28099.00 | 
| node (12.3) | [koa](http://koajs.com) (2.7) | 13.01 ms | 8.43 ms | 15.84 ms | 167.16 ms | 574.39 ms | 32620.00 | 
| node (12.3) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 16.74 ms | 8.63 ms | 20.11 ms | 215.76 ms | 673.47 ms | 41852.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 8.66 ms | 8.82 ms | 10.85 ms | 12.84 ms | 82.47 ms | 1964.00 | 
| node (12.3) | [fastify](http://fastify.io) (2.3) | 11.98 ms | 9.29 ms | 17.64 ms | 42.10 ms | 519.20 ms | 18840.33 | 
| node (12.3) | [restify](http://restify.com) (8.2) | 14.60 ms | 10.30 ms | 18.87 ms | 95.27 ms | 595.32 ms | 30248.00 | 
| node (12.3) | [express](http://expressjs.com) (4.16) | 15.41 ms | 10.42 ms | 20.20 ms | 150.74 ms | 622.72 ms | 32032.33 | 
| python (3.7) | [hug](http://hug.rest) (2.5) | 13.01 ms | 10.55 ms | 24.14 ms | 42.10 ms | 219.10 ms | 9444.33 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 14.76 ms | 10.77 ms | 20.10 ms | 61.14 ms | 941.22 ms | 34408.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 13.07 ms | 10.80 ms | 22.24 ms | 42.06 ms | 784.59 ms | 22216.00 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 12.99 ms | 11.61 ms | 21.78 ms | 34.41 ms | 61.30 ms | 6739.67 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.23) | 18.99 ms | 16.20 ms | 32.92 ms | 54.43 ms | 135.76 ms | 10563.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 21.39 ms | 17.05 ms | 41.76 ms | 68.78 ms | 109.39 ms | 14415.67 | 
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 20.48 ms | 17.05 ms | 27.82 ms | 45.07 ms | 748.30 ms | 26937.33 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 27.83 ms | 18.88 ms | 60.49 ms | 90.95 ms | 323.30 ms | 19689.00 | 
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 29.36 ms | 19.97 ms | 65.86 ms | 160.04 ms | 470.02 ms | 33919.33 | 
| node (12.3) | [hapi](http://hapijs.com) (18.1) | 30.52 ms | 20.44 ms | 34.35 ms | 344.34 ms | 1146.09 ms | 65119.00 | 
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 31.59 ms | 20.59 ms | 39.84 ms | 364.38 ms | 1830.75 ms | 83585.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 31.26 ms | 21.54 ms | 64.53 ms | 100.56 ms | 334.25 ms | 22674.67 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 30.94 ms | 24.89 ms | 52.61 ms | 77.94 ms | 118.30 ms | 16149.67 | 
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 30.93 ms | 28.46 ms | 39.21 ms | 54.12 ms | 163.36 ms | 8612.33 | 
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 30.24 ms | 28.56 ms | 37.90 ms | 44.42 ms | 317.93 ms | 9879.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 72.73 ms | 36.81 ms | 103.06 ms | 659.29 ms | 1005.11 ms | 124828.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 53.29 ms | 42.50 ms | 108.43 ms | 160.05 ms | 234.07 ms | 35568.00 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 63.85 ms | 62.94 ms | 93.06 ms | 125.70 ms | 165.70 ms | 22256.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 82.66 ms | 77.15 ms | 123.88 ms | 172.09 ms | 469.10 ms | 32090.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 85.05 ms | 83.06 ms | 110.64 ms | 200.39 ms | 1353.84 ms | 52625.33 | 
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 101.02 ms | 85.01 ms | 155.10 ms | 256.76 ms | 900.15 ms | 50298.67 | 
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 115.74 ms | 111.90 ms | 161.11 ms | 216.64 ms | 417.07 ms | 36224.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (kore) (c)


:five: (fasthttprouter) (go)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 380005.00 | 219.88 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 332315.67 | 397.90 MB |
| rust (1.35) | [actix-web](http://actix.rs) (0.7) | 323555.33 | 367.57 MB |
| c (99) | [kore](http://kore.io) (3.1) | 277425.67 | 720.54 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 268709.33 | 429.41 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 264787.00 | 300.59 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 264129.33 | 256.12 MB |
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 239594.33 | 225.04 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 233069.33 | 468.24 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 232454.67 | 134.54 MB |
| rust (1.35) | [gotham](http://gotham.rs) (0.3) | 231839.00 | 473.53 MB |
| java (8) | [act](http://actframework.org) (1.8) | 230783.00 | 398.31 MB |
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 229130.33 | 244.60 MB |
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 225261.33 | 211.75 MB |
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 224624.00 | 367.28 MB |
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 215135.00 | 268.25 MB |
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 186378.67 | 304.63 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 174944.67 | 284.73 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 171859.67 | 230.06 MB |
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 167924.67 | 223.22 MB |
| rust (1.35) | [iron](http://ironframework.io) (0.6) | 164682.67 | 208.08 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 163642.33 | 217.77 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 161556.67 | 213.28 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 157791.67 | 276.74 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 157346.67 | 211.59 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 157046.67 | 275.24 MB |
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 154976.33 | 241.51 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 153871.00 | 205.99 MB |
| node (12.3) | [restana](http://github.com/jkyberneees/ana) (3.0) | 143795.67 | 215.30 MB |
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 139223.67 | 326.44 MB |
| node (12.3) | [polka](http://github.com/lukeed/polka) (0.5) | 136513.67 | 204.42 MB |
| node (12.3) | [rayo](http://rayo.js.org) (1.3) | 130712.00 | 195.49 MB |
| go (1.12) | [gf](http://goframe.org) (1.6) | 130297.67 | 197.66 MB |
| node (12.3) | [foxify](http://foxify.js.org) (0.10) | 112062.00 | 235.38 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 112028.33 | 105.19 MB |
| node (12.3) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 108574.67 | 162.66 MB |
| node (12.3) | [fastify](http://fastify.io) (2.3) | 108421.00 | 263.82 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 107037.33 | 263.73 MB |
| node (12.3) | [koa](http://koajs.com) (2.7) | 104817.67 | 221.68 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 91436.67 | 160.12 MB |
| node (12.3) | [restify](http://restify.com) (8.2) | 85818.33 | 150.31 MB |
| node (12.3) | [express](http://expressjs.com) (4.16) | 82305.00 | 201.37 MB |
| rust (1.35) | [nickel](http://nickel-org.github.io) (0.11) | 80764.33 | 160.30 MB |
| python (3.7) | [hug](http://hug.rest) (2.5) | 79467.67 | 197.10 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 78914.00 | 132.06 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 77187.33 | 166.47 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 76840.67 | 164.69 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 63544.67 | 315.31 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 63234.00 | 313.20 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 62386.00 | 309.12 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 61803.67 | 98.15 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 58724.33 | 291.02 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 58493.67 | 303.81 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.23) | 53890.67 | 116.53 MB |
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 53480.67 | 68.95 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 51888.00 | 96.23 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 49585.67 | 112.29 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 46658.67 | 243.04 MB |
| node (12.3) | [hapi](http://hapijs.com) (18.1) | 45855.33 | 118.68 MB |
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 42416.00 | 79.92 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 42233.33 | 40.22 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 39069.00 | 72.66 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 34596.00 | 85.10 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 32838.33 | 63.34 MB |
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 32573.67 | 40.13 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 32470.33 | 18.74 MB |
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 32010.00 | 58.42 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 23072.00 | 41.09 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 21369.67 | 12.32 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 20042.67 | 39.97 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 18448.00 | 139.49 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 16144.67 | 41.85 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 15575.67 | 33.91 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 11711.00 | 34.51 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 11688.33 | 33.86 MB |
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 9762.33 | 24.05 MB |
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 8588.67 | 22.11 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3968.00 | 12.14 MB |
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

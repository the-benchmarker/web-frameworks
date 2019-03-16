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
Last update: 2019-03-16
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
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.08 ms | 0.07 ms | 0.12 ms | 0.15 ms | 5.58 ms | 68.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.17) | 2.92 ms | 0.16 ms | 10.81 ms | 27.91 ms | 73.17 ms | 6105.67 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.81 ms | 0.19 ms | 14.19 ms | 32.32 ms | 81.95 ms | 7375.33 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.30 ms | 0.28 ms | 0.52 ms | 0.82 ms | 43.31 ms | 319.67 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 115.83 ms | 0.29 ms | 188.47 ms | 2692.79 ms | 6818.62 ms | 458379.00 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 6.19 ms | 0.30 ms | 22.68 ms | 48.33 ms | 122.05 ms | 11355.00 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.53 ms | 0.31 ms | 19.27 ms | 41.75 ms | 90.59 ms | 9680.67 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 178.87 ms | 0.38 ms | 309.50 ms | 3754.49 ms | 6821.95 ms | 643341.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 7.59 ms | 0.44 ms | 24.96 ms | 50.89 ms | 116.06 ms | 12176.67 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 93.58 ms | 0.57 ms | 274.32 ms | 1317.20 ms | 6846.65 ms | 327795.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 46.55 ms | 1.48 ms | 3.14 ms | 1382.32 ms | 6581.81 ms | 335696.67 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 137.92 ms | 1.80 ms | 230.20 ms | 3283.13 ms | 6128.38 ms | 532360.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 2.74 ms | 1.99 ms | 5.76 ms | 13.04 ms | 33.08 ms | 2637.00 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 3.02 ms | 2.01 ms | 6.46 ms | 16.40 ms | 180.72 ms | 3913.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.93 ms | 2.06 ms | 6.09 ms | 14.54 ms | 33.54 ms | 2961.67 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 29.79 ms | 2.06 ms | 101.58 ms | 275.99 ms | 707.87 ms | 58539.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 8.05 ms | 2.74 ms | 7.55 ms | 128.83 ms | 1142.15 ms | 47159.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.40 ms | 2.81 ms | 7.11 ms | 14.23 ms | 33.53 ms | 3038.67 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 100.34 ms | 2.86 ms | 214.04 ms | 2193.48 ms | 4610.02 ms | 364812.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.04 ms | 2.92 ms | 5.28 ms | 7.68 ms | 60.89 ms | 1829.33 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 6.19 ms | 3.18 ms | 8.48 ms | 84.72 ms | 119.85 ms | 13391.00 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 4.07 ms | 3.39 ms | 7.61 ms | 17.81 ms | 149.13 ms | 4058.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 62.27 ms | 3.61 ms | 142.48 ms | 1287.86 ms | 3007.93 ms | 228053.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.38 ms | 3.65 ms | 7.25 ms | 13.19 ms | 40.62 ms | 2597.67 | 
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 4.11 ms | 3.83 ms | 7.20 ms | 14.40 ms | 31.65 ms | 2824.00 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.44 ms | 3.90 ms | 6.76 ms | 14.96 ms | 172.84 ms | 4650.33 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 4.53 ms | 4.00 ms | 8.62 ms | 16.38 ms | 34.71 ms | 3296.67 | 
| go (1.12) | [iris](http://iris-go.com) (11.1) | 6.17 ms | 4.67 ms | 11.29 ms | 25.65 ms | 302.40 ms | 9437.33 | 
| go (1.11) | [beego](http://beego.me) (1.12) | 7.01 ms | 5.12 ms | 14.07 ms | 30.00 ms | 136.42 ms | 6050.33 | 
| node (11.11) | [restana](http://github.com/jkyberneees/ana) (2.10) | 6.55 ms | 5.14 ms | 9.59 ms | 18.96 ms | 264.08 ms | 6886.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.16 ms | 5.16 ms | 10.33 ms | 20.94 ms | 188.81 ms | 4998.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 7.14 ms | 5.19 ms | 11.44 ms | 21.36 ms | 213.55 ms | 6968.33 | 
| go (1.12) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 6.75 ms | 5.24 ms | 12.74 ms | 27.03 ms | 74.90 ms | 5123.67 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 7.56 ms | 5.27 ms | 15.34 ms | 33.18 ms | 274.22 ms | 8694.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 5.96 ms | 5.30 ms | 8.79 ms | 16.03 ms | 159.09 ms | 4911.00 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 7.98 ms | 5.56 ms | 13.46 ms | 31.97 ms | 331.74 ms | 14308.33 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 8.21 ms | 5.73 ms | 16.12 ms | 35.40 ms | 194.81 ms | 9411.67 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 8.98 ms | 6.02 ms | 18.97 ms | 40.66 ms | 234.78 ms | 8903.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 8.42 ms | 6.08 ms | 16.75 ms | 33.68 ms | 181.55 ms | 7120.00 | 
| node (11.11) | [polka](http://github.com/lukeed/polka) (0.5) | 7.63 ms | 6.29 ms | 11.21 ms | 21.88 ms | 310.15 ms | 10218.67 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 8.99 ms | 6.37 ms | 18.23 ms | 37.69 ms | 150.04 ms | 7713.67 | 
| node (11.11) | [rayo](http://rayo.js.org) (1.2) | 7.80 ms | 6.86 ms | 11.75 ms | 21.28 ms | 298.01 ms | 9185.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 190.59 ms | 7.24 ms | 79.14 ms | 4482.12 ms | 7912.03 ms | 776466.00 | 
| go (1.12) | [gf](http://goframe.org) (1.5) | 10.49 ms | 7.81 ms | 20.49 ms | 45.91 ms | 289.85 ms | 9832.67 | 
| node (11.11) | [foxify](http://foxify.js.org) (0.10) | 8.51 ms | 7.84 ms | 12.71 ms | 22.14 ms | 320.96 ms | 9472.00 | 
| node (11.11) | [fastify](http://fastify.io) (2.1) | 11.37 ms | 8.57 ms | 15.32 ms | 63.22 ms | 474.15 ms | 19152.00 | 
| node (11.11) | [express](http://expressjs.com) (4.16) | 10.84 ms | 8.70 ms | 15.75 ms | 31.36 ms | 395.68 ms | 13328.33 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 10.42 ms | 8.86 ms | 18.21 ms | 28.59 ms | 61.82 ms | 5943.67 | 
| node (11.11) | [restify](http://restify.com) (8.1) | 10.87 ms | 9.39 ms | 14.79 ms | 27.88 ms | 319.22 ms | 10007.00 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 11.75 ms | 9.83 ms | 18.94 ms | 35.58 ms | 239.27 ms | 8597.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 10.22 ms | 10.28 ms | 12.06 ms | 13.99 ms | 90.34 ms | 2330.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.3) | 16.60 ms | 11.05 ms | 19.96 ms | 122.23 ms | 1054.95 ms | 46257.67 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 13.22 ms | 12.16 ms | 19.86 ms | 29.72 ms | 63.75 ms | 5332.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 16.23 ms | 12.66 ms | 31.17 ms | 42.57 ms | 89.64 ms | 9515.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 14.47 ms | 13.29 ms | 26.02 ms | 44.07 ms | 256.02 ms | 10793.33 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.7) | 14.90 ms | 13.45 ms | 25.75 ms | 40.10 ms | 118.66 ms | 8665.00 | 
| node (11.11) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 24.12 ms | 14.81 ms | 31.54 ms | 293.58 ms | 956.11 ms | 54724.67 | 
| node (11.11) | [koa](http://koajs.com) (2.7) | 21.43 ms | 15.90 ms | 30.44 ms | 125.51 ms | 806.26 ms | 36741.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 23.87 ms | 19.75 ms | 41.53 ms | 72.98 ms | 266.26 ms | 13884.33 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 22.59 ms | 20.38 ms | 31.24 ms | 39.93 ms | 303.74 ms | 7505.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.6) | 26.69 ms | 20.70 ms | 33.22 ms | 145.58 ms | 1389.32 ms | 52816.67 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 24.74 ms | 21.63 ms | 38.75 ms | 64.23 ms | 391.17 ms | 15139.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 26.45 ms | 23.06 ms | 34.92 ms | 76.09 ms | 391.74 ms | 13925.00 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 27.77 ms | 25.08 ms | 39.17 ms | 50.43 ms | 253.89 ms | 8755.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 36.57 ms | 26.87 ms | 77.75 ms | 147.32 ms | 261.83 ms | 30351.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 32.00 ms | 29.03 ms | 40.15 ms | 73.19 ms | 627.50 ms | 27316.67 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 29.71 ms | 29.92 ms | 39.29 ms | 50.50 ms | 319.33 ms | 10625.00 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 33.46 ms | 33.63 ms | 41.00 ms | 48.77 ms | 398.16 ms | 10586.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 43.23 ms | 34.37 ms | 82.19 ms | 114.15 ms | 443.84 ms | 25848.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 40.52 ms | 34.46 ms | 75.50 ms | 113.28 ms | 185.57 ms | 23606.00 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 57.97 ms | 55.70 ms | 94.14 ms | 125.34 ms | 173.47 ms | 26219.00 | 
| node (11.11) | [hapi](http://hapijs.com) (18.1) | 119.05 ms | 60.01 ms | 131.20 ms | 1643.14 ms | 3103.92 ms | 267509.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 64.38 ms | 64.28 ms | 78.28 ms | 98.92 ms | 434.23 ms | 17474.00 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 374966.67 | 216.64 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 364839.33 | 436.72 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 355189.33 | 403.97 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 319299.00 | 362.76 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 291501.33 | 282.99 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 278677.33 | 161.23 MB |
| c (99) | [kore](http://kore.io) (3.1) | 278077.33 | 722.77 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 255198.67 | 522.19 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 239268.00 | 481.08 MB |
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 232734.00 | 218.88 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 220043.67 | 234.72 MB |
| java (8) | [act](http://actframework.org) (1.8) | 219824.33 | 429.19 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 214390.67 | 344.84 MB |
| go (1.12) | [iris](http://iris-go.com) (11.1) | 178367.33 | 241.10 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 173425.67 | 218.77 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 159856.67 | 260.46 MB |
| go (1.12) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 154158.33 | 206.76 MB |
| node (11.11) | [restana](http://github.com/jkyberneees/ana) (2.10) | 153914.00 | 230.70 MB |
| go (1.11) | [beego](http://beego.me) (1.12) | 152695.67 | 205.02 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 146073.67 | 256.24 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 142896.67 | 190.35 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 139616.67 | 359.03 MB |
| node (11.11) | [polka](http://github.com/lukeed/polka) (0.5) | 137540.00 | 206.22 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 134088.67 | 179.09 MB |
| node (11.11) | [rayo](http://rayo.js.org) (1.2) | 130753.00 | 196.02 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 127924.67 | 315.31 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 124576.00 | 166.87 MB |
| node (11.11) | [foxify](http://foxify.js.org) (0.10) | 120232.00 | 252.76 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 119684.00 | 210.23 MB |
| node (11.11) | [fastify](http://fastify.io) (2.1) | 114312.67 | 283.81 MB |
| go (1.12) | [gf](http://goframe.org) (1.5) | 103947.33 | 157.61 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 98427.67 | 195.48 MB |
| node (11.11) | [express](http://expressjs.com) (4.16) | 97808.67 | 239.39 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 97565.33 | 485.31 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 97439.33 | 91.70 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 96384.67 | 207.86 MB |
| node (11.11) | [restify](http://restify.com) (8.1) | 95094.00 | 166.78 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 86638.00 | 214.73 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 81455.00 | 174.91 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.3) | 77731.33 | 130.06 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 75921.00 | 376.34 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 74849.33 | 371.98 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 74765.00 | 144.27 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 72583.33 | 126.94 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 70939.67 | 352.54 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.7) | 69004.00 | 149.15 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 65282.33 | 148.01 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 60031.00 | 95.26 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 59582.67 | 308.91 MB |
| node (11.11) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 58660.00 | 87.67 MB |
| node (11.11) | [koa](http://koajs.com) (2.7) | 55245.33 | 116.61 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 54451.67 | 284.27 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.17) | 43934.33 | 41.90 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 43614.67 | 40.89 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.6) | 43269.33 | 80.38 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 42400.00 | 104.24 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 41725.67 | 77.57 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 38274.67 | 35.87 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 35702.00 | 43.78 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 34051.00 | 55.47 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 33760.67 | 19.45 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 32007.67 | 58.43 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 31161.00 | 55.45 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 30326.33 | 49.44 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 25440.33 | 50.74 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 23640.00 | 68.63 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 23170.33 | 13.37 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 20682.67 | 156.43 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 17284.00 | 37.67 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 16828.67 | 43.63 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 15049.67 | 44.36 MB |
| node (11.11) | [hapi](http://hapijs.com) (18.1) | 14166.00 | 36.67 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 4299.33 | 13.17 MB |
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

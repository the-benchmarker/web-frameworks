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
Last update: 2019-05-06
```
OS: Linux (version: 5.0.9-301.fc30.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: iron (rust)


:five: slim (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 0.08 ms | 0.08 ms | 0.11 ms | 0.15 ms | 3.05 ms | 34.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.19) | 4.53 ms | 0.27 ms | 15.98 ms | 34.91 ms | 84.51 ms | 8102.67 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 5.71 ms | 0.32 ms | 20.01 ms | 40.68 ms | 93.35 ms | 9761.67 | 
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 0.45 ms | 0.43 ms | 0.75 ms | 1.13 ms | 26.19 ms | 296.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 188.32 ms | 0.47 ms | 336.69 ms | 3988.38 ms | 7170.17 ms | 672775.33 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 180.05 ms | 0.47 ms | 302.30 ms | 4391.34 ms | 7322.28 ms | 701366.00 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 129.81 ms | 0.48 ms | 311.69 ms | 2238.50 ms | 7247.95 ms | 453926.33 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 129.99 ms | 0.48 ms | 370.64 ms | 2218.98 ms | 6994.56 ms | 424654.67 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 215.36 ms | 0.50 ms | 335.00 ms | 4799.74 ms | 7723.60 ms | 800909.00 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 198.25 ms | 0.50 ms | 326.45 ms | 4670.98 ms | 7275.18 ms | 753336.00 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 8.08 ms | 0.50 ms | 26.55 ms | 52.82 ms | 121.18 ms | 12722.33 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 8.70 ms | 0.53 ms | 28.38 ms | 53.85 ms | 123.38 ms | 13447.67 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 10.84 ms | 0.72 ms | 33.77 ms | 65.82 ms | 146.88 ms | 16079.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 184.59 ms | 1.86 ms | 185.65 ms | 4665.67 ms | 6595.06 ms | 791177.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 3.12 ms | 2.00 ms | 6.83 ms | 14.45 ms | 33.33 ms | 3086.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.41 ms | 2.39 ms | 7.25 ms | 15.73 ms | 33.37 ms | 3354.67 | 
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 3.37 ms | 2.69 ms | 6.38 ms | 13.27 ms | 109.77 ms | 2786.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 6.32 ms | 3.06 ms | 7.62 ms | 46.26 ms | 850.41 ms | 30803.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.29 ms | 3.35 ms | 8.70 ms | 16.93 ms | 40.32 ms | 3600.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.54 ms | 3.80 ms | 5.46 ms | 8.36 ms | 22.16 ms | 1803.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.51 ms | 3.91 ms | 7.18 ms | 13.39 ms | 44.87 ms | 2558.33 | 
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 4.25 ms | 4.10 ms | 6.90 ms | 13.42 ms | 31.39 ms | 2563.33 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 4.67 ms | 4.26 ms | 8.36 ms | 15.68 ms | 31.92 ms | 3056.00 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.53 ms | 4.36 ms | 6.80 ms | 14.02 ms | 53.39 ms | 2553.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 7.40 ms | 4.40 ms | 12.21 ms | 78.90 ms | 119.75 ms | 12475.67 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 38.72 ms | 4.62 ms | 126.40 ms | 320.34 ms | 815.83 ms | 69769.33 | 
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 4.97 ms | 4.63 ms | 9.00 ms | 19.00 ms | 98.55 ms | 3839.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 7.53 ms | 5.94 ms | 9.78 ms | 24.83 ms | 423.23 ms | 14312.00 | 
| node (11.14) | [restana](http://github.com/jkyberneees/ana) (2.13) | 8.22 ms | 6.07 ms | 10.64 ms | 27.09 ms | 402.97 ms | 15477.33 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 9.33 ms | 6.54 ms | 19.03 ms | 41.83 ms | 274.25 ms | 10169.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 7.37 ms | 6.88 ms | 11.68 ms | 21.65 ms | 213.99 ms | 5346.67 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 9.54 ms | 6.97 ms | 19.47 ms | 41.52 ms | 139.04 ms | 8119.67 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 10.02 ms | 6.97 ms | 21.30 ms | 46.20 ms | 182.32 ms | 9484.00 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 9.52 ms | 7.09 ms | 18.86 ms | 40.89 ms | 140.03 ms | 8206.67 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 9.65 ms | 7.15 ms | 19.05 ms | 41.90 ms | 183.15 ms | 8752.33 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 9.13 ms | 7.24 ms | 15.98 ms | 36.48 ms | 204.80 ms | 8629.33 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 9.28 ms | 7.54 ms | 16.08 ms | 37.02 ms | 230.65 ms | 7716.67 | 
| node (11.14) | [polka](http://github.com/lukeed/polka) (0.5) | 9.33 ms | 8.46 ms | 13.84 ms | 27.05 ms | 378.54 ms | 12701.00 | 
| node (11.14) | [rayo](http://rayo.js.org) (1.2) | 9.27 ms | 8.52 ms | 13.79 ms | 24.67 ms | 387.47 ms | 12571.67 | 
| go (1.12) | [gf](http://goframe.org) (1.6) | 12.03 ms | 8.91 ms | 24.48 ms | 53.62 ms | 244.29 ms | 10930.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 224.36 ms | 9.09 ms | 329.00 ms | 4668.15 ms | 7740.10 ms | 801964.33 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 10.67 ms | 9.39 ms | 16.97 ms | 31.21 ms | 287.00 ms | 8614.67 | 
| node (11.14) | [foxify](http://foxify.js.org) (0.10) | 12.78 ms | 9.80 ms | 18.08 ms | 45.95 ms | 489.16 ms | 19753.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 12.30 ms | 9.90 ms | 20.50 ms | 48.09 ms | 224.97 ms | 10279.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 10.59 ms | 10.64 ms | 12.85 ms | 15.31 ms | 79.40 ms | 1964.00 | 
| node (11.14) | [koa](http://koajs.com) (2.7) | 14.88 ms | 10.78 ms | 19.34 ms | 101.09 ms | 646.93 ms | 29636.33 | 
| node (11.14) | [fastify](http://fastify.io) (2.3) | 12.31 ms | 10.80 ms | 17.77 ms | 33.66 ms | 436.71 ms | 15764.33 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 13.81 ms | 12.18 ms | 24.69 ms | 36.87 ms | 123.44 ms | 7823.00 | 
| kotlin (1.3) | [ktor](http://ktor.io) (1.1) | 15.35 ms | 12.20 ms | 27.22 ms | 48.67 ms | 276.95 ms | 10321.33 | 
| node (11.14) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 16.83 ms | 12.49 ms | 21.86 ms | 125.49 ms | 730.32 ms | 35085.67 | 
| node (11.14) | [restify](http://restify.com) (8.2) | 15.19 ms | 12.86 ms | 18.63 ms | 49.44 ms | 434.00 ms | 16868.67 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 26.46 ms | 13.45 ms | 24.57 ms | 496.96 ms | 1867.40 ms | 100576.67 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 18.42 ms | 14.27 ms | 30.01 ms | 53.62 ms | 590.66 ms | 18357.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 32.66 ms | 15.97 ms | 34.74 ms | 438.31 ms | 778.42 ms | 77660.00 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.16) | 20.82 ms | 18.97 ms | 34.28 ms | 55.42 ms | 117.42 ms | 10675.67 | 
| node (11.14) | [express](http://expressjs.com) (4.16) | 43.08 ms | 21.24 ms | 42.92 ms | 594.53 ms | 1233.14 ms | 109840.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 25.40 ms | 22.40 ms | 44.73 ms | 64.16 ms | 141.85 ms | 13429.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 26.39 ms | 24.34 ms | 37.03 ms | 43.67 ms | 320.03 ms | 9027.67 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 34.84 ms | 25.21 ms | 71.56 ms | 95.28 ms | 238.67 ms | 21264.00 | 
| node (11.14) | [hapi](http://hapijs.com) (18.1) | 49.81 ms | 28.47 ms | 44.31 ms | 740.68 ms | 1741.54 ms | 123886.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 35.05 ms | 29.54 ms | 59.80 ms | 89.05 ms | 491.00 ms | 20157.00 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 32.72 ms | 31.59 ms | 54.01 ms | 73.56 ms | 119.61 ms | 15235.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 32.56 ms | 32.96 ms | 39.70 ms | 74.57 ms | 203.41 ms | 11312.00 | 
| swift (5.0) | [kitura](http://kitura.io) (2.6) | 35.61 ms | 33.10 ms | 48.56 ms | 74.07 ms | 647.15 ms | 26433.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 38.65 ms | 34.46 ms | 51.59 ms | 84.98 ms | 417.41 ms | 19639.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 36.95 ms | 35.33 ms | 45.12 ms | 56.83 ms | 286.71 ms | 10255.67 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 35.36 ms | 35.93 ms | 44.07 ms | 56.94 ms | 204.25 ms | 9440.67 | 
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 37.40 ms | 38.08 ms | 43.24 ms | 55.04 ms | 405.24 ms | 12451.67 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 41.05 ms | 41.09 ms | 48.13 ms | 58.27 ms | 508.70 ms | 15169.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 60.67 ms | 47.30 ms | 113.23 ms | 152.09 ms | 226.68 ms | 33505.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 54.86 ms | 50.08 ms | 90.85 ms | 150.08 ms | 395.21 ms | 28879.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 87.77 ms | 81.21 ms | 121.34 ms | 144.09 ms | 393.39 ms | 25339.67 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 86.39 ms | 81.29 ms | 149.78 ms | 205.41 ms | 285.93 ms | 44571.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 88.01 ms | 85.44 ms | 103.33 ms | 209.84 ms | 1102.05 ms | 47378.33 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 336470.67 | 194.53 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 312047.33 | 373.42 MB |
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 279196.00 | 317.44 MB |
| c (99) | [kore](http://kore.io) (3.1) | 252814.67 | 656.93 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 252442.00 | 286.69 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 250170.00 | 242.85 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 232345.00 | 466.82 MB |
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 222914.00 | 209.55 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 209688.67 | 223.94 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 208137.00 | 335.75 MB |
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 199924.33 | 408.40 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 192214.00 | 111.04 MB |
| java (8) | [act](http://actframework.org) (1.8) | 169555.33 | 292.80 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 140008.00 | 228.12 MB |
| node (11.14) | [restana](http://github.com/jkyberneees/ana) (2.13) | 136014.33 | 203.96 MB |
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 135179.67 | 169.95 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 118971.67 | 208.78 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 116710.67 | 154.58 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 114998.33 | 154.86 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 114915.33 | 201.60 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 114065.33 | 153.07 MB |
| node (11.14) | [rayo](http://rayo.js.org) (1.2) | 113529.00 | 170.26 MB |
| node (11.14) | [polka](http://github.com/lukeed/polka) (0.5) | 113049.00 | 169.51 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 112601.00 | 149.30 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 111252.33 | 148.23 MB |
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 110349.33 | 219.28 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 93602.33 | 240.70 MB |
| go (1.12) | [gf](http://goframe.org) (1.6) | 92867.00 | 140.76 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 92676.67 | 87.00 MB |
| node (11.14) | [fastify](http://fastify.io) (2.3) | 91710.67 | 239.13 MB |
| node (11.14) | [foxify](http://foxify.js.org) (0.10) | 86557.00 | 182.04 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 84869.33 | 209.22 MB |
| node (11.14) | [koa](http://koajs.com) (2.7) | 79965.33 | 169.43 MB |
| node (11.14) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 79812.33 | 119.59 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 73108.33 | 157.80 MB |
| node (11.14) | [restify](http://restify.com) (8.2) | 70262.00 | 123.38 MB |
| kotlin (1.3) | [ktor](http://ktor.io) (1.1) | 66880.00 | 104.13 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 64256.33 | 107.49 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 60881.00 | 106.71 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 56870.67 | 140.94 MB |
| node (11.14) | [express](http://expressjs.com) (4.16) | 52189.33 | 127.73 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 50971.33 | 109.56 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.16) | 48675.67 | 105.22 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 44916.67 | 222.61 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 44201.33 | 219.11 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 43702.33 | 216.61 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 42276.00 | 209.66 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 41141.67 | 213.35 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 41060.67 | 66.87 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 40855.33 | 92.61 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 37382.67 | 35.08 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 34846.33 | 181.62 MB |
| node (11.14) | [hapi](http://hapijs.com) (18.1) | 33382.00 | 86.17 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 31389.00 | 29.41 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 31317.67 | 58.27 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 30814.00 | 59.57 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.6) | 28626.67 | 53.09 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 28496.67 | 70.17 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 28341.00 | 46.19 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.19) | 28153.00 | 26.83 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 26751.00 | 32.98 MB |
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 26724.00 | 33.04 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 26226.00 | 42.75 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 24266.67 | 44.30 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 22418.67 | 12.92 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 18477.67 | 32.95 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 16618.67 | 33.16 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 15770.33 | 9.10 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 14683.00 | 110.96 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 11778.00 | 30.55 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 11580.00 | 25.24 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 11243.67 | 32.60 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 11154.67 | 32.89 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3296.67 | 10.14 MB |
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

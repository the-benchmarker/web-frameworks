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
Last update: 2019-01-23
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


:five: flame (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.11) | 0.09 ms | 0.08 ms | 0.13 ms | 0.18 ms | 1.50 ms | 33.00 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 4.85 ms | 0.29 ms | 16.97 ms | 35.68 ms | 83.64 ms | 8415.33 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 5.80 ms | 0.34 ms | 20.15 ms | 41.06 ms | 95.53 ms | 9853.67 | 
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 0.52 ms | 0.50 ms | 0.88 ms | 1.26 ms | 11.57 ms | 299.33 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 8.75 ms | 0.58 ms | 27.79 ms | 54.61 ms | 140.69 ms | 13316.67 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 9.52 ms | 0.59 ms | 30.68 ms | 59.30 ms | 142.64 ms | 14660.67 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 10.64 ms | 0.76 ms | 32.34 ms | 62.60 ms | 138.80 ms | 15357.33 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 206.11 ms | 1.37 ms | 383.58 ms | 4366.69 ms | 7379.03 ms | 729822.33 | 
| php (7.3) | [laravel](http://laravel.com) (5.7) | 161.56 ms | 1.66 ms | 440.03 ms | 2801.98 ms | 6205.98 ms | 506512.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 170.58 ms | 1.86 ms | 324.30 ms | 3597.16 ms | 7220.21 ms | 611968.33 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 225.89 ms | 2.09 ms | 390.83 ms | 5053.24 ms | 7810.16 ms | 826765.00 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 225.09 ms | 2.15 ms | 354.43 ms | 4833.82 ms | 7939.80 ms | 845094.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 139.49 ms | 2.42 ms | 106.12 ms | 3235.87 ms | 6585.38 ms | 602306.33 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 3.21 ms | 2.98 ms | 5.71 ms | 9.99 ms | 29.75 ms | 2125.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 4.08 ms | 3.23 ms | 8.59 ms | 17.95 ms | 38.93 ms | 3840.00 | 
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 4.22 ms | 3.38 ms | 8.17 ms | 17.38 ms | 113.61 ms | 3946.67 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 191.67 ms | 3.41 ms | 296.62 ms | 4663.52 ms | 7285.92 ms | 745179.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 5.18 ms | 4.24 ms | 10.46 ms | 18.86 ms | 43.45 ms | 4074.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.42 ms | 4.74 ms | 6.42 ms | 11.30 ms | 98.95 ms | 2472.00 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 5.12 ms | 4.90 ms | 8.61 ms | 15.86 ms | 34.01 ms | 3002.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 5.68 ms | 5.05 ms | 10.35 ms | 18.49 ms | 38.19 ms | 3644.00 | 
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 5.29 ms | 5.12 ms | 8.11 ms | 16.02 ms | 94.75 ms | 2895.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.38 ms | 5.19 ms | 8.75 ms | 15.55 ms | 45.00 ms | 3003.00 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.60 ms | 5.29 ms | 8.20 ms | 16.69 ms | 161.50 ms | 3962.67 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 40.05 ms | 5.30 ms | 131.75 ms | 332.20 ms | 774.51 ms | 72025.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 6.11 ms | 6.01 ms | 10.34 ms | 12.94 ms | 235.15 ms | 3499.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 7.96 ms | 7.27 ms | 12.68 ms | 24.54 ms | 176.11 ms | 5451.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 7.99 ms | 7.31 ms | 10.93 ms | 21.15 ms | 347.89 ms | 6741.00 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.6) | 8.36 ms | 7.71 ms | 15.69 ms | 27.90 ms | 59.81 ms | 5857.00 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 8.98 ms | 7.80 ms | 13.32 ms | 29.47 ms | 239.67 ms | 8666.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 180.36 ms | 8.17 ms | 25.14 ms | 4764.11 ms | 7928.19 ms | 799682.67 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 9.04 ms | 8.24 ms | 13.66 ms | 27.49 ms | 117.30 ms | 5123.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 9.79 ms | 8.75 ms | 15.23 ms | 32.04 ms | 226.06 ms | 6759.00 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 9.80 ms | 8.93 ms | 14.83 ms | 29.38 ms | 189.71 ms | 6228.33 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 10.54 ms | 9.38 ms | 15.77 ms | 33.75 ms | 191.00 ms | 8117.67 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 10.59 ms | 9.43 ms | 16.19 ms | 36.08 ms | 194.40 ms | 7957.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 10.79 ms | 9.54 ms | 16.45 ms | 35.05 ms | 244.12 ms | 8653.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 13.00 ms | 10.85 ms | 23.10 ms | 40.93 ms | 243.88 ms | 11070.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 14.40 ms | 11.63 ms | 25.98 ms | 46.40 ms | 260.02 ms | 11113.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 13.18 ms | 13.10 ms | 15.51 ms | 18.13 ms | 231.93 ms | 5996.00 | 
| go (1.11) | [gf](http://gfer.me) (1.4) | 15.18 ms | 13.51 ms | 19.89 ms | 51.06 ms | 513.68 ms | 18183.00 | 
| node (11.6) | [restana](http://github.com/jkyberneees/ana) (2.7) | 19.00 ms | 14.00 ms | 33.74 ms | 78.08 ms | 528.57 ms | 21327.33 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 19.96 ms | 14.86 ms | 25.58 ms | 141.21 ms | 1011.46 ms | 39818.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 17.91 ms | 15.21 ms | 31.39 ms | 59.01 ms | 727.99 ms | 23585.33 | 
| node (11.6) | [rayo](http://rayo.js.org) (1.2) | 25.07 ms | 16.86 ms | 35.49 ms | 222.29 ms | 1021.70 ms | 51891.00 | 
| node (11.6) | [polka](http://github.com/lukeed/polka) (0.5) | 23.69 ms | 17.02 ms | 35.55 ms | 143.34 ms | 838.87 ms | 39197.33 | 
| node (11.6) | [foxify](http://foxify.js.org) (0.10) | 24.66 ms | 18.13 ms | 35.14 ms | 135.75 ms | 912.14 ms | 41097.33 | 
| node (11.6) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 30.10 ms | 18.42 ms | 35.90 ms | 394.62 ms | 1231.59 ms | 73647.67 | 
| node (11.6) | [fastify](http://fastify.io) (1.13) | 33.74 ms | 20.55 ms | 41.06 ms | 429.86 ms | 1311.83 ms | 79706.33 | 
| node (11.6) | [restify](http://restify.com) (7.6) | 25.55 ms | 20.92 ms | 39.61 ms | 82.65 ms | 559.10 ms | 22951.67 | 
| node (11.6) | [koa](http://koajs.com) (2.6) | 31.19 ms | 23.63 ms | 44.16 ms | 216.01 ms | 948.38 ms | 46720.33 | 
| node (11.6) | [express](http://expressjs.com) (4.16) | 31.92 ms | 24.67 ms | 44.71 ms | 195.74 ms | 963.31 ms | 45566.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 29.37 ms | 25.44 ms | 51.22 ms | 72.24 ms | 114.46 ms | 14951.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 39.42 ms | 28.88 ms | 47.83 ms | 367.34 ms | 1407.03 ms | 80361.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 31.16 ms | 30.93 ms | 38.87 ms | 47.09 ms | 243.11 ms | 6942.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 43.06 ms | 32.04 ms | 83.39 ms | 119.55 ms | 458.44 ms | 28692.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 34.41 ms | 35.17 ms | 43.53 ms | 49.81 ms | 247.61 ms | 7497.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 38.37 ms | 35.95 ms | 48.58 ms | 55.63 ms | 354.69 ms | 12134.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 36.55 ms | 37.56 ms | 45.28 ms | 57.06 ms | 400.58 ms | 13222.67 | 
| node (11.6) | [hapi](http://hapijs.com) (18.0) | 65.08 ms | 40.14 ms | 69.61 ms | 888.60 ms | 1973.97 ms | 142568.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 41.80 ms | 40.21 ms | 52.14 ms | 57.90 ms | 258.62 ms | 9380.00 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 42.11 ms | 42.50 ms | 49.53 ms | 57.53 ms | 196.09 ms | 7583.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 60.48 ms | 52.89 ms | 109.56 ms | 180.44 ms | 300.92 ms | 38049.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 64.87 ms | 56.91 ms | 96.30 ms | 143.57 ms | 537.13 ms | 26447.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 71.29 ms | 64.75 ms | 119.37 ms | 163.52 ms | 360.74 ms | 34141.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 110.77 ms | 110.84 ms | 137.25 ms | 173.94 ms | 522.57 ms | 26396.67 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 297749.33 | 172.18 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 255261.67 | 305.50 MB |
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 228419.67 | 259.39 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 202829.67 | 230.11 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 201120.00 | 195.01 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 191676.33 | 385.01 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 183947.33 | 173.07 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 172459.67 | 183.48 MB |
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 172162.00 | 351.48 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 166281.67 | 267.99 MB |
| java (8) | [act](http://actframework.org) (1.8) | 151303.00 | 295.38 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.6) | 121401.33 | 70.28 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 120068.00 | 195.62 MB |
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 115788.00 | 146.16 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 111058.67 | 148.94 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 107011.33 | 142.99 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 100917.67 | 135.00 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 100541.33 | 176.56 MB |
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.11) | 98631.67 | 196.11 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 95925.33 | 129.00 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 95622.67 | 167.94 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 92855.00 | 124.06 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 79078.33 | 203.29 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 75839.00 | 71.25 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 71599.67 | 176.41 MB |
| go (1.11) | [gf](http://gfer.me) (1.4) | 69484.67 | 105.46 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 64307.00 | 112.65 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 59835.00 | 128.54 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 58819.33 | 98.49 MB |
| node (11.6) | [restana](http://github.com/jkyberneees/ana) (2.7) | 58523.33 | 87.67 MB |
| node (11.6) | [rayo](http://rayo.js.org) (1.2) | 51774.00 | 77.54 MB |
| node (11.6) | [polka](http://github.com/lukeed/polka) (0.5) | 50956.67 | 76.29 MB |
| c (99) | [kore](http://kore.io) (3.1) | 50714.33 | 137.43 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 49973.67 | 248.56 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 49946.67 | 248.18 MB |
| node (11.6) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 49684.33 | 74.26 MB |
| node (11.6) | [foxify](http://foxify.js.org) (0.10) | 47685.00 | 100.08 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 45121.33 | 224.10 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 45004.33 | 223.40 MB |
| node (11.6) | [fastify](http://fastify.io) (1.13) | 43792.67 | 107.43 MB |
| node (11.6) | [restify](http://restify.com) (7.6) | 41714.33 | 73.13 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 41215.67 | 213.92 MB |
| node (11.6) | [koa](http://koajs.com) (2.6) | 37210.33 | 78.69 MB |
| node (11.6) | [express](http://expressjs.com) (4.16) | 35626.67 | 87.09 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 35131.00 | 79.64 MB |
| php (7.3) | [laravel](http://laravel.com) (5.7) | 35102.67 | 183.04 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 32047.67 | 30.05 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 31516.33 | 47.25 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 30503.00 | 56.60 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 29120.00 | 27.29 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 27493.67 | 44.81 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 26319.00 | 25.08 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 25680.00 | 46.91 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 24591.67 | 60.60 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 23755.33 | 29.11 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 23486.33 | 38.28 MB |
| node (11.6) | [hapi](http://hapijs.com) (18.0) | 22981.67 | 59.23 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 22002.00 | 12.69 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 17117.67 | 30.53 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 15221.67 | 44.16 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 14625.33 | 8.44 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 14165.00 | 28.23 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 13402.67 | 101.35 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 12007.33 | 31.14 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 8791.33 | 25.85 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3190.00 | 9.75 MB |
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

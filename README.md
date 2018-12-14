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
Last update: 2018-12-16
```
OS: Linux (version: 4.19.8-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: zend-framework (php)


:three: symfony (php)


:four: iron (rust)


:five: slim (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 0.06 ms | 0.06 ms | 0.08 ms | 0.10 ms | 0.40 ms | 20.00 | 
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 93.60 ms | 0.22 ms | 177.96 ms | 2025.59 ms | 6765.35 ms | 407565.33 | 
| php (7.2) | [symfony](http://symfony.com) (4.2) | 71.42 ms | 0.23 ms | 153.89 ms | 1516.94 ms | 6726.20 ms | 319254.00 | 
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 0.25 ms | 0.23 ms | 0.42 ms | 0.75 ms | 10.19 ms | 162.67 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 69.23 ms | 0.24 ms | 144.76 ms | 1605.84 ms | 5852.37 ms | 293201.67 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 96.57 ms | 0.24 ms | 237.06 ms | 1786.02 ms | 7045.02 ms | 362913.00 | 
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 94.53 ms | 0.24 ms | 166.76 ms | 2048.76 ms | 6755.78 ms | 413153.67 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 100.11 ms | 0.24 ms | 175.65 ms | 2306.31 ms | 6801.81 ms | 429632.33 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.18 ms | 0.76 ms | 8.99 ms | 28.28 ms | 104.21 ms | 5787.67 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 2.53 ms | 0.86 ms | 6.92 ms | 20.87 ms | 82.69 ms | 4315.00 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 5.44 ms | 0.87 ms | 16.17 ms | 49.43 ms | 157.97 ms | 10164.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 66.29 ms | 1.11 ms | 2.34 ms | 2244.06 ms | 6580.23 ms | 433784.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.52 ms | 1.42 ms | 5.87 ms | 14.15 ms | 34.24 ms | 2935.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 1.75 ms | 1.48 ms | 2.86 ms | 5.59 ms | 68.02 ms | 1238.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 2.44 ms | 1.78 ms | 4.85 ms | 7.38 ms | 93.97 ms | 2275.00 | 
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 2.38 ms | 1.91 ms | 4.91 ms | 10.87 ms | 92.40 ms | 2785.67 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 6.46 ms | 2.23 ms | 18.27 ms | 47.04 ms | 137.90 ms | 9990.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.23 ms | 2.28 ms | 3.03 ms | 5.11 ms | 79.35 ms | 1852.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 2.84 ms | 2.29 ms | 5.99 ms | 11.94 ms | 51.16 ms | 2677.00 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.12 ms | 2.40 ms | 13.61 ms | 33.60 ms | 111.58 ms | 7163.33 | 
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 2.62 ms | 2.57 ms | 3.54 ms | 6.08 ms | 144.29 ms | 1913.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 3.19 ms | 2.63 ms | 6.21 ms | 11.54 ms | 32.84 ms | 2420.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.01 ms | 2.77 ms | 4.76 ms | 9.01 ms | 25.14 ms | 1630.00 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 4.11 ms | 3.37 ms | 7.38 ms | 18.43 ms | 107.81 ms | 5696.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 3.58 ms | 3.55 ms | 5.99 ms | 6.95 ms | 15.27 ms | 1794.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 4.10 ms | 3.81 ms | 5.56 ms | 10.99 ms | 253.38 ms | 5499.33 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 4.38 ms | 3.88 ms | 7.30 ms | 14.63 ms | 223.23 ms | 5332.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 4.61 ms | 3.90 ms | 7.41 ms | 20.82 ms | 183.21 ms | 4439.67 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 4.35 ms | 4.06 ms | 7.31 ms | 14.05 ms | 103.48 ms | 3288.33 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 4.59 ms | 4.16 ms | 7.85 ms | 15.22 ms | 156.40 ms | 4115.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 4.86 ms | 4.33 ms | 8.45 ms | 16.44 ms | 209.91 ms | 4181.67 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 4.86 ms | 4.34 ms | 6.82 ms | 21.61 ms | 55.88 ms | 3532.67 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 4.92 ms | 4.40 ms | 8.43 ms | 16.64 ms | 101.30 ms | 3682.33 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 4.86 ms | 4.40 ms | 8.39 ms | 16.09 ms | 92.29 ms | 3110.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 5.71 ms | 4.50 ms | 8.91 ms | 19.34 ms | 419.12 ms | 12087.00 | 
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 6.18 ms | 4.62 ms | 10.79 ms | 25.77 ms | 214.05 ms | 6191.33 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 5.80 ms | 4.64 ms | 10.24 ms | 19.64 ms | 163.65 ms | 5516.67 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 7.60 ms | 5.38 ms | 13.83 ms | 31.35 ms | 257.84 ms | 8749.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 6.55 ms | 5.65 ms | 11.54 ms | 19.49 ms | 203.78 ms | 4885.67 | 
| go (1.11) | [gf](http://gfer.me) (1.3) | 6.77 ms | 5.92 ms | 9.91 ms | 23.34 ms | 354.14 ms | 6976.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 178.59 ms | 5.96 ms | 26.32 ms | 4301.45 ms | 6849.78 ms | 732725.67 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 28.70 ms | 6.17 ms | 92.99 ms | 238.02 ms | 612.71 ms | 51172.67 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 8.78 ms | 6.35 ms | 16.28 ms | 34.50 ms | 264.18 ms | 8915.67 | 
| node (11.1) | [fastify](http://fastify.io) (1.13) | 10.74 ms | 7.44 ms | 16.82 ms | 66.51 ms | 455.79 ms | 19101.33 | 
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 11.35 ms | 8.33 ms | 19.21 ms | 41.41 ms | 416.43 ms | 14688.67 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 11.43 ms | 8.45 ms | 19.92 ms | 43.85 ms | 388.21 ms | 13909.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 13.70 ms | 9.57 ms | 17.48 ms | 72.47 ms | 886.95 ms | 36288.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 9.59 ms | 9.84 ms | 11.09 ms | 12.67 ms | 162.34 ms | 2698.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 11.25 ms | 9.92 ms | 20.70 ms | 38.99 ms | 248.32 ms | 9271.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 13.90 ms | 11.14 ms | 28.85 ms | 41.85 ms | 64.57 ms | 9728.67 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 16.49 ms | 13.23 ms | 28.21 ms | 50.75 ms | 406.30 ms | 15223.00 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 18.23 ms | 13.55 ms | 27.85 ms | 73.52 ms | 737.04 ms | 30666.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 19.56 ms | 15.16 ms | 35.88 ms | 57.20 ms | 309.39 ms | 14496.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 18.79 ms | 16.09 ms | 27.12 ms | 58.95 ms | 179.51 ms | 10050.00 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 19.97 ms | 16.79 ms | 27.48 ms | 84.47 ms | 196.71 ms | 12891.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 19.08 ms | 17.23 ms | 26.35 ms | 40.58 ms | 509.15 ms | 14346.67 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 21.24 ms | 17.36 ms | 29.03 ms | 128.58 ms | 419.84 ms | 20302.00 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 22.11 ms | 19.66 ms | 33.03 ms | 45.71 ms | 387.93 ms | 12219.67 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 30.04 ms | 22.72 ms | 39.80 ms | 226.31 ms | 1062.28 ms | 51822.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 32.40 ms | 26.54 ms | 63.16 ms | 114.98 ms | 200.77 ms | 23562.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 27.43 ms | 27.60 ms | 33.08 ms | 38.45 ms | 382.96 ms | 8719.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 35.13 ms | 27.86 ms | 66.56 ms | 91.92 ms | 410.52 ms | 22279.00 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 32.50 ms | 30.42 ms | 38.12 ms | 64.30 ms | 122.03 ms | 8495.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 43.71 ms | 35.80 ms | 85.87 ms | 140.74 ms | 202.46 ms | 29253.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 59.59 ms | 58.85 ms | 69.94 ms | 90.02 ms | 691.41 ms | 28982.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (evhtp) (cpp)


:five: (vibora) (python)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 538760.67 | 311.61 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 479412.67 | 574.05 MB |
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 416372.67 | 472.86 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 392345.67 | 381.04 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 390028.33 | 442.66 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 385675.00 | 620.98 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 342186.67 | 688.19 MB |
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 332099.33 | 678.63 MB |
| java (8) | [act](http://actframework.org) (1.8) | 317159.00 | 618.72 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 316029.67 | 337.46 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 275102.33 | 159.22 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 237832.00 | 386.97 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 227745.67 | 305.15 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 225672.33 | 301.57 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 214699.00 | 286.66 MB |
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 212167.67 | 267.67 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 210912.33 | 370.47 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 203296.67 | 356.35 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 200751.00 | 351.61 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 199854.00 | 269.71 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 189253.33 | 252.09 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 176348.67 | 453.23 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 172374.00 | 258.17 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 153206.00 | 377.64 MB |
| go (1.11) | [gf](http://gfer.me) (1.3) | 152126.67 | 231.24 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 145860.00 | 218.36 MB |
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 139695.33 | 277.09 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 127952.67 | 191.50 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 121682.00 | 284.36 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 103646.00 | 97.36 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 98953.67 | 209.16 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 98917.00 | 207.31 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 97982.33 | 210.01 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 95365.67 | 166.82 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 89428.00 | 150.52 MB |
| c (99) | [kore](http://kore.io) (3.1) | 87522.00 | 237.20 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 87046.67 | 431.08 MB |
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 84042.00 | 416.46 MB |
| php (7.2) | [symfony](http://symfony.com) (4.2) | 83754.33 | 415.63 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 81737.33 | 129.05 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 79761.67 | 413.74 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 78056.33 | 177.06 MB |
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 75381.67 | 373.98 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 65194.33 | 113.99 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 64616.33 | 157.67 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 63784.33 | 332.10 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 54596.00 | 51.18 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 53098.00 | 130.75 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 52925.33 | 98.10 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 52506.67 | 49.15 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 50822.67 | 48.41 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 50612.00 | 82.45 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 45124.67 | 55.41 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 40852.33 | 23.54 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 40240.00 | 103.81 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 36857.67 | 67.24 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 33390.67 | 59.42 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 32389.67 | 52.70 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 29612.33 | 85.91 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 25112.00 | 14.47 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 24594.67 | 49.00 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 23761.67 | 179.51 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 19846.00 | 51.44 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 16626.33 | 44.38 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 4463.00 | 13.67 MB |
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

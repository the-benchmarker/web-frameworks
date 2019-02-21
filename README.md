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
Last update: 2019-02-20
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


:five: lumen (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.07 ms | 0.07 ms | 0.11 ms | 0.14 ms | 5.87 ms | 44.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 3.94 ms | 0.22 ms | 14.01 ms | 31.95 ms | 85.57 ms | 7296.67 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.92 ms | 0.26 ms | 17.37 ms | 37.99 ms | 129.49 ms | 8998.00 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 165.99 ms | 0.36 ms | 292.19 ms | 3672.57 ms | 6800.08 ms | 615738.00 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 119.42 ms | 0.38 ms | 251.03 ms | 2632.19 ms | 6822.88 ms | 453199.00 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.41 ms | 0.39 ms | 0.65 ms | 1.09 ms | 28.71 ms | 332.67 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 169.36 ms | 0.43 ms | 305.62 ms | 3866.51 ms | 6809.30 ms | 628890.33 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 7.51 ms | 0.44 ms | 25.14 ms | 51.83 ms | 114.50 ms | 12291.67 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 9.05 ms | 0.48 ms | 31.09 ms | 65.89 ms | 155.29 ms | 15501.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 9.51 ms | 0.64 ms | 29.71 ms | 58.20 ms | 148.95 ms | 14215.33 | 
| php (7.3) | [laravel](http://laravel.com) (5.7) | 130.86 ms | 0.81 ms | 339.68 ms | 2374.58 ms | 6907.64 ms | 440325.67 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 141.26 ms | 1.49 ms | 268.27 ms | 3066.25 ms | 6176.55 ms | 519317.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 152.64 ms | 1.56 ms | 262.96 ms | 3273.43 ms | 6813.59 ms | 563548.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 100.65 ms | 1.70 ms | 3.85 ms | 2941.63 ms | 6593.25 ms | 536513.33 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.47 ms | 2.76 ms | 6.71 ms | 15.24 ms | 40.28 ms | 3166.67 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 3.73 ms | 3.03 ms | 7.55 ms | 15.29 ms | 82.15 ms | 3273.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 3.55 ms | 3.49 ms | 6.01 ms | 10.70 ms | 29.41 ms | 2271.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.66 ms | 3.76 ms | 9.42 ms | 17.17 ms | 38.63 ms | 3723.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 6.91 ms | 4.39 ms | 12.26 ms | 66.95 ms | 116.41 ms | 10956.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.94 ms | 4.45 ms | 5.64 ms | 8.69 ms | 83.00 ms | 2044.33 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.74 ms | 4.59 ms | 7.14 ms | 14.28 ms | 101.85 ms | 3097.33 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 39.36 ms | 4.66 ms | 130.70 ms | 346.97 ms | 914.85 ms | 74093.67 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 4.87 ms | 4.67 ms | 8.13 ms | 15.65 ms | 49.86 ms | 2980.00 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 5.21 ms | 4.70 ms | 9.81 ms | 20.38 ms | 215.10 ms | 4778.00 | 
| node (11.9) | [restana](http://github.com/jkyberneees/ana) (2.8) | 5.71 ms | 4.80 ms | 9.38 ms | 16.79 ms | 219.68 ms | 5200.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 5.42 ms | 4.82 ms | 9.90 ms | 17.80 ms | 36.07 ms | 3506.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.26 ms | 5.22 ms | 8.10 ms | 13.94 ms | 38.96 ms | 2741.67 | 
| node (11.9) | [rayo](http://rayo.js.org) (1.2) | 6.82 ms | 5.29 ms | 10.31 ms | 20.47 ms | 272.81 ms | 7910.00 | 
| node (11.9) | [polka](http://github.com/lukeed/polka) (0.5) | 7.26 ms | 5.37 ms | 10.68 ms | 23.51 ms | 339.65 ms | 11516.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 63.36 ms | 5.97 ms | 11.11 ms | 1350.17 ms | 1700.74 ms | 238515.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 7.47 ms | 6.20 ms | 12.03 ms | 23.94 ms | 170.56 ms | 4946.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.93 ms | 6.22 ms | 11.37 ms | 22.01 ms | 160.85 ms | 5026.67 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 7.58 ms | 6.31 ms | 12.26 ms | 24.27 ms | 78.88 ms | 4274.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 7.03 ms | 6.34 ms | 10.03 ms | 18.27 ms | 212.79 ms | 4139.00 | 
| node (11.9) | [fastify](http://fastify.io) (1.14) | 9.19 ms | 6.60 ms | 12.58 ms | 34.86 ms | 478.94 ms | 18734.00 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 8.45 ms | 7.08 ms | 13.95 ms | 28.03 ms | 138.66 ms | 5328.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (4.0) | 8.62 ms | 7.10 ms | 13.92 ms | 28.07 ms | 196.65 ms | 7846.00 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 8.63 ms | 7.16 ms | 14.10 ms | 27.78 ms | 194.43 ms | 7195.33 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 8.78 ms | 7.23 ms | 14.33 ms | 29.95 ms | 225.13 ms | 6581.67 | 
| node (11.9) | [foxify](http://foxify.js.org) (0.10) | 8.66 ms | 7.31 ms | 13.03 ms | 26.15 ms | 370.35 ms | 12979.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 9.03 ms | 7.65 ms | 14.63 ms | 28.91 ms | 195.22 ms | 7129.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 9.59 ms | 7.79 ms | 16.66 ms | 37.66 ms | 233.76 ms | 9540.00 | 
| node (11.9) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 12.71 ms | 8.62 ms | 17.23 ms | 118.93 ms | 621.91 ms | 29093.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 170.15 ms | 8.65 ms | 65.51 ms | 3754.35 ms | 7073.41 ms | 664834.33 | 
| node (11.9) | [koa](http://koajs.com) (2.7) | 9.79 ms | 8.84 ms | 14.52 ms | 28.98 ms | 347.46 ms | 11781.67 | 
| node (11.9) | [express](http://expressjs.com) (4.16) | 10.60 ms | 8.86 ms | 15.51 ms | 32.37 ms | 385.19 ms | 13875.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 11.21 ms | 9.49 ms | 18.41 ms | 33.17 ms | 317.00 ms | 10800.00 | 
| node (11.9) | [restify](http://restify.com) (7.7) | 11.14 ms | 9.91 ms | 14.78 ms | 27.93 ms | 295.19 ms | 8935.00 | 
| go (1.11) | [gf](http://goframe.org) (1.5) | 11.68 ms | 10.52 ms | 17.45 ms | 38.25 ms | 154.36 ms | 7024.00 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 20.36 ms | 11.83 ms | 31.08 ms | 225.52 ms | 419.86 ms | 35799.33 | 
| python (3.7) | [starlette](http://starlette.io) (0.10) | 14.19 ms | 13.13 ms | 23.23 ms | 33.88 ms | 71.24 ms | 6633.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 14.75 ms | 13.25 ms | 26.51 ms | 46.82 ms | 336.51 ms | 11663.33 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 25.43 ms | 13.40 ms | 24.22 ms | 472.15 ms | 1888.51 ms | 96179.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 13.43 ms | 13.50 ms | 15.56 ms | 19.80 ms | 41.98 ms | 2197.00 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.5) | 21.36 ms | 18.15 ms | 36.30 ms | 57.82 ms | 111.18 ms | 10997.67 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.11) | 20.79 ms | 18.29 ms | 35.97 ms | 57.37 ms | 90.80 ms | 11465.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 25.04 ms | 20.05 ms | 47.98 ms | 64.51 ms | 103.71 ms | 14003.33 | 
| node (11.9) | [hapi](http://hapijs.com) (18.1) | 29.30 ms | 21.40 ms | 37.05 ms | 261.83 ms | 984.37 ms | 51651.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 27.45 ms | 24.99 ms | 38.91 ms | 55.96 ms | 476.07 ms | 16203.67 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 27.94 ms | 25.01 ms | 35.55 ms | 53.85 ms | 625.31 ms | 19903.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 27.78 ms | 26.80 ms | 37.72 ms | 45.54 ms | 185.54 ms | 7277.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 32.48 ms | 27.14 ms | 57.10 ms | 88.66 ms | 406.99 ms | 18911.67 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 31.18 ms | 27.88 ms | 50.49 ms | 72.55 ms | 259.30 ms | 14434.00 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 33.88 ms | 33.67 ms | 46.66 ms | 64.20 ms | 408.21 ms | 16489.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 35.91 ms | 36.40 ms | 45.50 ms | 54.74 ms | 401.40 ms | 14964.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 47.41 ms | 38.48 ms | 92.85 ms | 171.16 ms | 306.49 ms | 35850.00 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 39.34 ms | 38.57 ms | 47.74 ms | 57.72 ms | 275.68 ms | 9772.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 46.63 ms | 43.76 ms | 62.49 ms | 76.01 ms | 327.77 ms | 13595.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 59.87 ms | 55.78 ms | 83.80 ms | 135.23 ms | 555.46 ms | 25171.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 62.19 ms | 57.90 ms | 111.47 ms | 157.85 ms | 200.07 ms | 35885.00 | 
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.1) | 77.72 ms | 74.63 ms | 116.98 ms | 148.28 ms | 193.80 ms | 28670.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 83.00 ms | 83.53 ms | 103.14 ms | 124.07 ms | 473.08 ms | 22560.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (agoo-c) (c)


:three: (actix-web) (rust)


:four: (vibora) (python)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 286357.00 | 342.83 MB |
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 279247.67 | 161.48 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 255581.33 | 290.59 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 227916.00 | 258.76 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 221951.67 | 215.56 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 195657.00 | 393.27 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 194106.67 | 182.58 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 192968.00 | 310.95 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 192387.00 | 111.16 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 190117.67 | 389.10 MB |
| java (8) | [act](http://actframework.org) (1.8) | 185027.67 | 361.38 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 180888.33 | 192.92 MB |
| node (11.9) | [restana](http://github.com/jkyberneees/ana) (2.8) | 167525.33 | 250.88 MB |
| node (11.9) | [rayo](http://rayo.js.org) (1.2) | 147598.33 | 220.94 MB |
| node (11.9) | [polka](http://github.com/lukeed/polka) (0.5) | 147455.00 | 220.65 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 143930.33 | 181.26 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 134531.67 | 219.16 MB |
| node (11.9) | [fastify](http://fastify.io) (1.14) | 131600.67 | 314.62 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 129743.33 | 173.00 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 129083.67 | 173.23 MB |
| node (11.9) | [foxify](http://foxify.js.org) (0.10) | 122765.00 | 257.91 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 116625.67 | 157.01 MB |
| go (1.11) | [echo](http://echo.labstack.com) (4.0) | 116361.33 | 204.34 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 115734.00 | 203.24 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 114730.00 | 228.29 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 114209.33 | 152.77 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 111005.33 | 148.15 MB |
| node (11.9) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 110774.67 | 165.82 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 109349.00 | 281.18 MB |
| node (11.9) | [koa](http://koajs.com) (2.7) | 105618.67 | 223.53 MB |
| node (11.9) | [express](http://expressjs.com) (4.16) | 99863.33 | 244.71 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 92145.00 | 226.96 MB |
| node (11.9) | [restify](http://restify.com) (7.7) | 90181.00 | 158.15 MB |
| go (1.11) | [gf](http://goframe.org) (1.5) | 87114.67 | 132.19 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 74985.67 | 131.37 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 73985.00 | 69.54 MB |
| python (3.7) | [starlette](http://starlette.io) (0.10) | 70390.67 | 151.43 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 68284.00 | 338.85 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 66919.33 | 165.72 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 66326.67 | 111.30 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 62031.33 | 307.41 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 61724.67 | 132.50 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 59853.33 | 297.31 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 58545.67 | 290.06 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 56457.33 | 293.16 MB |
| c (99) | [kore](http://kore.io) (3.1) | 50088.33 | 135.75 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.11) | 49322.00 | 95.16 MB |
| php (7.3) | [laravel](http://laravel.com) (5.7) | 47926.00 | 249.97 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.5) | 47596.67 | 102.92 MB |
| node (11.9) | [hapi](http://hapijs.com) (18.1) | 42002.33 | 108.39 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 41643.00 | 94.48 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 40201.00 | 61.78 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 36447.33 | 67.60 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 36096.67 | 33.84 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 36009.67 | 58.70 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 32509.33 | 30.97 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 32506.33 | 60.42 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 31047.00 | 76.52 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 29633.33 | 27.78 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 28208.33 | 34.52 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 26094.33 | 15.05 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 25505.33 | 46.57 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 23033.67 | 41.04 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 21999.33 | 35.85 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 17148.67 | 9.89 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 16670.67 | 48.32 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 16273.67 | 32.43 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 14107.67 | 106.85 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 13439.67 | 34.86 MB |
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.1) | 12757.67 | 27.79 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 11668.33 | 34.28 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3247.00 | 9.95 MB |
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

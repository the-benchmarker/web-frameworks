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
Last update: 2019-01-14
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


:four: iron (rust)


:five: zend-framework (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 0.06 ms | 0.06 ms | 0.07 ms | 0.10 ms | 0.57 ms | 17.67 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.15) | 3.43 ms | 0.19 ms | 12.44 ms | 29.61 ms | 75.44 ms | 6632.67 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.35 ms | 0.21 ms | 16.59 ms | 36.47 ms | 89.44 ms | 8439.67 | 
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 0.32 ms | 0.32 ms | 0.50 ms | 0.78 ms | 20.03 ms | 206.67 | 
| php (7.2) | [zend-framework](http://framework.zend.com) (3.1) | 133.72 ms | 0.32 ms | 245.64 ms | 2852.86 ms | 6800.84 ms | 517900.67 | 
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 127.95 ms | 0.32 ms | 201.81 ms | 3128.51 ms | 6773.19 ms | 515674.33 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 135.50 ms | 0.33 ms | 219.23 ms | 3243.12 ms | 6876.62 ms | 546107.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.95 ms | 0.33 ms | 20.72 ms | 44.16 ms | 100.08 ms | 10301.00 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 6.83 ms | 0.38 ms | 23.32 ms | 45.98 ms | 113.97 ms | 11278.00 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 138.01 ms | 0.49 ms | 322.49 ms | 2776.73 ms | 7002.12 ms | 492580.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 8.16 ms | 0.51 ms | 26.25 ms | 51.88 ms | 120.26 ms | 12597.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 105.59 ms | 1.36 ms | 34.13 ms | 3080.62 ms | 5506.58 ms | 528802.67 | 
| php (7.2) | [symfony](http://symfony.com) (4.2) | 96.85 ms | 1.41 ms | 186.11 ms | 2239.59 ms | 5672.16 ms | 375672.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.99 ms | 2.02 ms | 6.59 ms | 14.56 ms | 33.06 ms | 3076.67 | 
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 3.03 ms | 2.12 ms | 6.53 ms | 15.44 ms | 34.77 ms | 3113.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 2.35 ms | 2.15 ms | 3.63 ms | 8.53 ms | 33.26 ms | 1576.67 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 69.47 ms | 2.29 ms | 154.69 ms | 1470.18 ms | 3912.14 ms | 269236.33 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 35.94 ms | 2.61 ms | 119.73 ms | 341.54 ms | 980.24 ms | 73198.33 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.50 ms | 2.92 ms | 5.71 ms | 10.92 ms | 102.31 ms | 2894.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.01 ms | 3.02 ms | 5.16 ms | 7.34 ms | 31.60 ms | 1679.67 | 
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 3.21 ms | 3.02 ms | 4.86 ms | 8.93 ms | 125.73 ms | 2624.33 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 3.54 ms | 3.10 ms | 6.40 ms | 12.85 ms | 30.81 ms | 2577.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.98 ms | 3.12 ms | 8.27 ms | 15.95 ms | 35.93 ms | 3434.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 3.92 ms | 3.33 ms | 7.54 ms | 13.72 ms | 28.95 ms | 2836.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.02 ms | 3.33 ms | 6.74 ms | 11.89 ms | 94.09 ms | 2584.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.5) | 5.89 ms | 3.87 ms | 9.07 ms | 70.49 ms | 122.23 ms | 10714.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 4.53 ms | 4.34 ms | 7.33 ms | 8.94 ms | 1167.06 ms | 12514.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 5.19 ms | 4.65 ms | 7.66 ms | 14.77 ms | 212.07 ms | 4585.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 5.53 ms | 4.68 ms | 9.21 ms | 18.41 ms | 223.98 ms | 5026.67 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 6.18 ms | 4.81 ms | 9.25 ms | 28.76 ms | 85.74 ms | 4991.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.86 ms | 4.86 ms | 9.38 ms | 22.06 ms | 234.87 ms | 7050.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 6.44 ms | 5.22 ms | 10.87 ms | 23.15 ms | 204.59 ms | 5051.33 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 6.42 ms | 5.25 ms | 10.63 ms | 21.38 ms | 212.62 ms | 5412.33 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 6.34 ms | 5.31 ms | 10.40 ms | 21.09 ms | 158.62 ms | 4526.67 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 7.58 ms | 5.39 ms | 13.92 ms | 25.27 ms | 210.76 ms | 6284.33 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 6.61 ms | 5.45 ms | 11.20 ms | 22.36 ms | 62.99 ms | 4084.33 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 7.02 ms | 5.56 ms | 11.40 ms | 23.81 ms | 230.86 ms | 7491.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 7.20 ms | 5.84 ms | 11.91 ms | 24.42 ms | 277.59 ms | 6964.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 179.33 ms | 7.29 ms | 28.52 ms | 4303.15 ms | 7159.54 ms | 739553.33 | 
| go (1.11) | [gf](http://gfer.me) (1.3) | 8.50 ms | 7.73 ms | 12.61 ms | 26.91 ms | 174.94 ms | 5290.00 | 
| node (11.6) | [restana](http://github.com/jkyberneees/ana) (2.7) | 11.29 ms | 7.85 ms | 21.58 ms | 49.20 ms | 370.18 ms | 14703.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 9.67 ms | 8.67 ms | 16.25 ms | 29.69 ms | 214.78 ms | 7393.67 | 
| node (11.6) | [polka](http://github.com/lukeed/polka) (0.5) | 12.50 ms | 8.76 ms | 23.16 ms | 53.14 ms | 453.13 ms | 16997.33 | 
| node (11.6) | [rayo](http://rayo.js.org) (1.2) | 13.62 ms | 9.63 ms | 24.38 ms | 55.99 ms | 522.56 ms | 20210.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 10.66 ms | 10.75 ms | 12.56 ms | 14.38 ms | 92.45 ms | 1967.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 11.96 ms | 10.78 ms | 21.78 ms | 38.82 ms | 211.58 ms | 8711.00 | 
| node (11.6) | [foxify](http://foxify.js.org) (0.10) | 14.83 ms | 10.92 ms | 24.97 ms | 61.30 ms | 517.45 ms | 20163.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 17.35 ms | 11.20 ms | 19.40 ms | 175.83 ms | 1113.45 ms | 48564.00 | 
| node (11.6) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 17.18 ms | 11.35 ms | 26.56 ms | 147.30 ms | 680.73 ms | 34003.00 | 
| node (11.6) | [fastify](http://fastify.io) (1.13) | 20.21 ms | 12.68 ms | 29.27 ms | 207.80 ms | 851.83 ms | 44059.67 | 
| node (11.6) | [koa](http://koajs.com) (2.6) | 16.99 ms | 12.75 ms | 27.43 ms | 72.23 ms | 631.42 ms | 25807.00 | 
| node (11.6) | [express](http://expressjs.com) (4.16) | 23.03 ms | 17.30 ms | 36.28 ms | 114.39 ms | 756.48 ms | 33983.33 | 
| node (11.6) | [restify](http://restify.com) (7.44) | 21.07 ms | 17.31 ms | 36.10 ms | 65.21 ms | 421.01 ms | 18182.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 19.11 ms | 18.77 ms | 29.35 ms | 40.11 ms | 107.10 ms | 8435.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 29.99 ms | 21.41 ms | 34.40 ms | 302.16 ms | 1625.91 ms | 68182.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 26.62 ms | 21.87 ms | 46.47 ms | 72.26 ms | 404.84 ms | 16621.00 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 25.64 ms | 24.95 ms | 34.06 ms | 40.04 ms | 316.23 ms | 11164.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 25.19 ms | 25.23 ms | 33.31 ms | 39.41 ms | 232.33 ms | 6928.67 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 29.44 ms | 26.02 ms | 41.90 ms | 50.77 ms | 309.59 ms | 10155.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 29.85 ms | 26.31 ms | 43.64 ms | 48.62 ms | 322.87 ms | 10865.67 | 
| node (11.6) | [hapi](http://hapijs.com) (17.8) | 43.78 ms | 27.13 ms | 51.15 ms | 590.25 ms | 1611.15 ms | 101834.00 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 27.34 ms | 29.28 ms | 36.56 ms | 42.45 ms | 247.52 ms | 7836.00 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 33.29 ms | 32.74 ms | 41.30 ms | 49.04 ms | 231.57 ms | 6813.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 41.59 ms | 32.75 ms | 80.19 ms | 140.95 ms | 281.37 ms | 29434.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 43.46 ms | 37.41 ms | 84.77 ms | 123.76 ms | 173.76 ms | 27783.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 61.04 ms | 39.65 ms | 80.10 ms | 461.10 ms | 957.70 ms | 83655.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 77.57 ms | 79.75 ms | 98.00 ms | 118.06 ms | 305.57 ms | 19080.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (evhtp) (cpp)


:five: (gotham) (rust)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 408934.33 | 236.46 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 360449.33 | 431.40 MB |
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 346558.33 | 393.81 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 289148.67 | 280.72 MB |
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 278710.67 | 569.67 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 274255.33 | 258.04 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 273015.33 | 310.06 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 266362.67 | 426.58 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 262998.00 | 527.77 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 255827.00 | 274.04 MB |
| java (8) | [act](http://actframework.org) (1.8) | 254829.00 | 497.37 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.5) | 232039.33 | 134.22 MB |
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 185541.00 | 234.01 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 182799.00 | 297.70 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 178365.67 | 240.23 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 170075.00 | 298.74 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 154656.00 | 271.35 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 154384.00 | 206.59 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 153941.33 | 206.02 MB |
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 152005.67 | 302.30 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 148122.33 | 259.87 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 145110.67 | 194.66 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 137472.67 | 183.34 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 133532.33 | 343.35 MB |
| go (1.11) | [gf](http://gfer.me) (1.3) | 117995.67 | 179.70 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 104432.67 | 257.45 MB |
| node (11.6) | [restana](http://github.com/jkyberneees/ana) (2.7) | 102547.00 | 153.57 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 92943.67 | 87.38 MB |
| node (11.6) | [polka](http://github.com/lukeed/polka) (0.5) | 92771.33 | 139.10 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 89688.67 | 156.91 MB |
| node (11.6) | [rayo](http://rayo.js.org) (1.2) | 87042.00 | 130.40 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 86275.67 | 428.65 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 82873.00 | 177.32 MB |
| php (7.2) | [symfony](http://symfony.com) (4.2) | 79719.67 | 395.84 MB |
| node (11.6) | [foxify](http://foxify.js.org) (0.10) | 78866.00 | 165.66 MB |
| node (11.6) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 77875.00 | 116.57 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 75797.67 | 127.52 MB |
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 74215.00 | 367.74 MB |
| node (11.6) | [fastify](http://fastify.io) (1.13) | 71086.33 | 167.93 MB |
| c (99) | [kore](http://kore.io) (3.1) | 68765.67 | 186.47 MB |
| node (11.6) | [koa](http://koajs.com) (2.6) | 68366.33 | 144.46 MB |
| php (7.2) | [zend-framework](http://framework.zend.com) (3.1) | 67607.33 | 335.01 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 67182.00 | 348.52 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 64505.00 | 102.57 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 58116.33 | 302.30 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 52149.67 | 118.31 MB |
| node (11.6) | [restify](http://restify.com) (7.44) | 50217.00 | 87.94 MB |
| node (11.6) | [express](http://expressjs.com) (4.16) | 50194.33 | 122.52 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 41876.00 | 77.64 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 39650.00 | 37.16 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 39165.33 | 36.71 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 37886.67 | 93.35 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.15) | 37408.33 | 35.64 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 36467.00 | 59.43 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 33554.00 | 61.24 MB |
| node (11.6) | [hapi](http://hapijs.com) (17.8) | 33333.33 | 85.99 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 33157.67 | 54.04 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 30631.33 | 37.39 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 29494.00 | 17.00 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 25678.00 | 45.71 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 23930.33 | 47.73 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 21513.00 | 12.41 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 20960.33 | 60.80 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 18727.33 | 141.65 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 15694.33 | 40.67 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 12578.67 | 37.06 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3608.00 | 11.03 MB |
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

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
Last update: 2018-12-04
```
OS: Linux (version: 4.19.5-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: laravel (php)


:four: symfony (php)


:five: slim (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 0.07 ms | 0.07 ms | 0.11 ms | 0.15 ms | 3.10 ms | 36.67 | 
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 0.30 ms | 0.26 ms | 0.49 ms | 0.92 ms | 27.78 ms | 270.00 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 112.84 ms | 0.37 ms | 288.91 ms | 2061.44 ms | 6890.71 ms | 397383.67 | 
| php (7.2) | [symfony](http://symfony.com) (4.2) | 115.87 ms | 0.39 ms | 254.56 ms | 2374.33 ms | 6905.41 ms | 438644.67 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 137.84 ms | 0.39 ms | 240.54 ms | 3339.23 ms | 6785.50 ms | 536961.00 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 173.03 ms | 0.42 ms | 298.51 ms | 3686.71 ms | 7245.69 ms | 640908.67 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.48 ms | 0.93 ms | 13.05 ms | 39.90 ms | 124.82 ms | 8180.67 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 3.40 ms | 1.13 ms | 9.37 ms | 28.04 ms | 100.10 ms | 5750.67 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 8.21 ms | 1.18 ms | 25.28 ms | 73.20 ms | 210.80 ms | 15213.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 168.00 ms | 1.52 ms | 110.82 ms | 4230.49 ms | 6593.10 ms | 736671.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 2.79 ms | 2.20 ms | 5.19 ms | 11.71 ms | 88.51 ms | 2461.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.40 ms | 2.37 ms | 7.32 ms | 16.76 ms | 36.90 ms | 3500.33 | 
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 3.36 ms | 2.46 ms | 6.97 ms | 15.25 ms | 45.24 ms | 3171.00 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 9.21 ms | 3.02 ms | 26.56 ms | 66.96 ms | 180.15 ms | 14275.67 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 7.19 ms | 3.27 ms | 19.52 ms | 46.31 ms | 128.26 ms | 9966.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.14 ms | 3.28 ms | 8.46 ms | 16.00 ms | 36.77 ms | 3468.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.86 ms | 3.39 ms | 6.04 ms | 11.88 ms | 53.97 ms | 2384.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.26 ms | 3.52 ms | 7.06 ms | 12.42 ms | 41.33 ms | 2388.67 | 
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 3.72 ms | 3.63 ms | 5.20 ms | 8.94 ms | 38.70 ms | 1610.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.55 ms | 3.65 ms | 5.55 ms | 9.55 ms | 36.17 ms | 2041.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 4.68 ms | 4.13 ms | 8.77 ms | 15.59 ms | 32.76 ms | 3161.67 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 6.27 ms | 4.50 ms | 10.66 ms | 48.62 ms | 129.30 ms | 8791.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 5.28 ms | 5.09 ms | 8.61 ms | 10.20 ms | 1176.60 ms | 12954.33 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 6.67 ms | 5.09 ms | 9.81 ms | 30.03 ms | 97.37 ms | 5056.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 6.46 ms | 5.30 ms | 10.45 ms | 21.77 ms | 224.88 ms | 6230.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 6.06 ms | 5.43 ms | 9.17 ms | 16.70 ms | 207.82 ms | 3955.67 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 6.59 ms | 5.46 ms | 10.70 ms | 21.88 ms | 170.50 ms | 4820.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.44 ms | 5.53 ms | 10.40 ms | 22.02 ms | 142.72 ms | 5456.67 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 6.85 ms | 5.63 ms | 11.23 ms | 22.59 ms | 165.75 ms | 5115.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 7.25 ms | 5.73 ms | 11.80 ms | 24.63 ms | 289.27 ms | 7934.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 7.16 ms | 5.89 ms | 12.05 ms | 23.80 ms | 109.37 ms | 4446.67 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 7.35 ms | 5.97 ms | 11.81 ms | 23.94 ms | 282.83 ms | 7182.67 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 7.50 ms | 6.00 ms | 12.22 ms | 25.61 ms | 299.01 ms | 8372.33 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 8.20 ms | 6.44 ms | 14.31 ms | 26.20 ms | 215.73 ms | 7035.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 198.76 ms | 7.38 ms | 34.07 ms | 4662.32 ms | 7918.52 ms | 820743.00 | 
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 10.40 ms | 7.49 ms | 19.37 ms | 45.52 ms | 383.46 ms | 13379.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 10.02 ms | 8.05 ms | 19.05 ms | 37.28 ms | 171.67 ms | 7764.33 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 11.92 ms | 8.19 ms | 19.54 ms | 63.68 ms | 533.47 ms | 21928.67 | 
| go (1.11) | [gf](http://gfer.me) (1.2) | 9.98 ms | 8.97 ms | 14.76 ms | 32.34 ms | 212.93 ms | 7700.00 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 13.23 ms | 9.02 ms | 22.53 ms | 60.06 ms | 579.16 ms | 23260.33 | 
| node (11.1) | [fastify](http://fastify.io) (1.13) | 16.70 ms | 10.29 ms | 23.33 ms | 178.86 ms | 751.00 ms | 39250.33 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 43.07 ms | 10.84 ms | 135.70 ms | 314.93 ms | 785.64 ms | 69469.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 16.55 ms | 11.21 ms | 20.49 ms | 96.89 ms | 1022.26 ms | 43339.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 14.01 ms | 11.67 ms | 24.50 ms | 49.47 ms | 470.94 ms | 18324.00 | 
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 16.76 ms | 11.91 ms | 27.72 ms | 69.81 ms | 643.87 ms | 26873.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 11.84 ms | 12.09 ms | 13.78 ms | 15.55 ms | 130.58 ms | 2036.00 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 20.20 ms | 14.40 ms | 30.70 ms | 117.74 ms | 799.95 ms | 36468.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 19.05 ms | 15.49 ms | 36.64 ms | 61.31 ms | 131.01 ms | 12709.00 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 19.48 ms | 16.00 ms | 33.08 ms | 62.51 ms | 355.36 ms | 13661.33 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 23.31 ms | 16.82 ms | 35.17 ms | 147.61 ms | 828.74 ms | 39659.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 25.15 ms | 22.16 ms | 37.60 ms | 54.96 ms | 256.41 ms | 10804.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 25.98 ms | 22.60 ms | 34.97 ms | 48.65 ms | 306.90 ms | 10357.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 25.67 ms | 22.86 ms | 34.21 ms | 53.57 ms | 314.04 ms | 11335.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 27.15 ms | 24.57 ms | 40.18 ms | 67.84 ms | 333.17 ms | 13231.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 28.81 ms | 27.29 ms | 40.52 ms | 58.58 ms | 445.95 ms | 15981.00 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 34.98 ms | 29.03 ms | 53.08 ms | 148.71 ms | 776.87 ms | 28399.67 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 48.22 ms | 30.87 ms | 56.57 ms | 645.82 ms | 1687.73 ms | 107286.00 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 33.09 ms | 32.28 ms | 45.76 ms | 64.28 ms | 332.08 ms | 13870.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 35.54 ms | 35.63 ms | 43.05 ms | 49.94 ms | 395.06 ms | 14495.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 44.06 ms | 37.13 ms | 81.64 ms | 135.15 ms | 241.66 ms | 27856.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 51.24 ms | 44.42 ms | 76.24 ms | 135.80 ms | 687.54 ms | 31404.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 58.35 ms | 54.35 ms | 100.77 ms | 163.73 ms | 241.40 ms | 33113.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 79.06 ms | 76.60 ms | 99.29 ms | 144.79 ms | 470.02 ms | 20481.00 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 358019.00 | 207.05 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 319729.33 | 382.55 MB |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 296089.00 | 336.76 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 257154.33 | 292.06 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 252473.00 | 245.06 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 240661.00 | 388.90 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 240331.33 | 483.14 MB |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 232907.33 | 476.70 MB |
| java (8) | [act](http://actframework.org) (1.8) | 217442.33 | 424.69 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 212219.00 | 226.53 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 189096.67 | 109.42 MB |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 175621.33 | 221.48 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 156528.33 | 274.47 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 154782.00 | 205.71 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 154014.33 | 250.99 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 148703.67 | 198.89 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 144036.33 | 192.40 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 140452.00 | 246.67 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 137371.00 | 241.16 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 137115.00 | 185.54 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 136181.33 | 181.60 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 122867.67 | 315.41 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 109562.33 | 163.93 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 105205.33 | 259.23 MB |
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 103029.00 | 204.47 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 102633.33 | 153.86 MB |
| go (1.11) | [gf](http://gfer.me) (1.2) | 102504.00 | 155.58 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 91658.00 | 137.29 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 86290.67 | 204.74 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 83818.33 | 146.82 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 83771.33 | 78.76 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 75117.00 | 126.25 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 70962.33 | 149.02 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 70727.00 | 151.69 MB |
| c (99) | [kore](http://kore.io) (3.1) | 60992.67 | 165.33 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 59815.33 | 126.34 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 59656.00 | 295.61 MB |
| php (7.2) | [symfony](http://symfony.com) (4.2) | 57098.00 | 283.21 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 55400.33 | 125.44 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 53714.67 | 278.74 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 52958.00 | 92.63 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 51366.00 | 125.37 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 48414.00 | 78.90 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 48128.67 | 250.72 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 39986.33 | 74.12 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 38756.67 | 36.36 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 38285.33 | 35.91 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 37919.33 | 36.11 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 36519.33 | 90.02 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 34875.67 | 56.88 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 30456.33 | 55.70 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 29838.33 | 48.62 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 29154.33 | 75.38 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 28857.67 | 16.64 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 28620.00 | 35.03 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 23837.67 | 42.46 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 19381.00 | 56.19 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 17821.00 | 10.28 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 17760.00 | 35.38 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 15673.67 | 118.45 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 13891.33 | 36.02 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 12382.67 | 33.33 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 2964.67 | 9.07 MB |
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

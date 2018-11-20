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
Last update: 2018-11-20
```
OS: Linux (version: 4.18.18-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: laravel (php)


:three: lumen (php)


:four: iron (rust)


:five: rack-routing (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 0.11 ms | 0.11 ms | 0.14 ms | 0.18 ms | 10.52 ms | 98.00 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 197.04 ms | 0.52 ms | 411.16 ms | 3939.61 ms | 7296.14 ms | 686186.67 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 228.37 ms | 0.54 ms | 354.47 ms | 5077.90 ms | 7900.90 ms | 844923.67 | 
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 0.59 ms | 0.58 ms | 0.89 ms | 1.24 ms | 89.65 ms | 725.67 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 7.10 ms | 1.19 ms | 21.35 ms | 63.21 ms | 203.93 ms | 13113.67 | 
| php (7.2) | [symfony](http://symfony.com) (4.1) | 224.97 ms | 1.42 ms | 383.56 ms | 4689.61 ms | 7193.36 ms | 793223.67 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 5.38 ms | 1.63 ms | 15.18 ms | 43.89 ms | 137.99 ms | 9078.33 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 11.95 ms | 1.80 ms | 37.15 ms | 104.06 ms | 314.82 ms | 21858.67 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 209.49 ms | 1.92 ms | 319.01 ms | 4670.72 ms | 7784.06 ms | 784669.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 198.73 ms | 2.29 ms | 159.72 ms | 4769.14 ms | 6594.17 ms | 818620.33 | 
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 4.21 ms | 4.13 ms | 7.62 ms | 14.75 ms | 51.05 ms | 3320.33 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 13.26 ms | 4.15 ms | 38.73 ms | 95.19 ms | 239.46 ms | 20436.67 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 10.90 ms | 4.52 ms | 30.25 ms | 72.24 ms | 204.35 ms | 15588.67 | 
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 4.66 ms | 4.70 ms | 7.93 ms | 16.01 ms | 45.55 ms | 3217.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 5.30 ms | 5.21 ms | 7.40 ms | 16.70 ms | 133.69 ms | 3179.33 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.71 ms | 5.31 ms | 8.63 ms | 17.62 ms | 107.15 ms | 3218.33 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 6.18 ms | 5.39 ms | 10.75 ms | 19.92 ms | 41.71 ms | 3692.33 | 
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 7.26 ms | 5.67 ms | 12.67 ms | 21.56 ms | 308.07 ms | 7639.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 6.27 ms | 5.70 ms | 12.41 ms | 19.79 ms | 49.14 ms | 4299.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 49.64 ms | 6.17 ms | 11.36 ms | 1964.26 ms | 3616.21 ms | 314332.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 6.82 ms | 6.30 ms | 10.06 ms | 16.71 ms | 107.81 ms | 3223.33 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 10.82 ms | 7.81 ms | 21.16 ms | 68.49 ms | 139.47 ms | 12069.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 9.04 ms | 8.33 ms | 14.24 ms | 26.97 ms | 182.51 ms | 5607.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 10.38 ms | 8.91 ms | 12.94 ms | 32.67 ms | 644.31 ms | 19037.33 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 10.19 ms | 9.27 ms | 15.47 ms | 31.32 ms | 237.16 ms | 7057.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.1) | 9.69 ms | 9.30 ms | 12.61 ms | 15.51 ms | 216.67 ms | 3646.33 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 10.89 ms | 9.74 ms | 16.62 ms | 33.62 ms | 232.50 ms | 7479.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 183.14 ms | 9.84 ms | 73.77 ms | 4329.50 ms | 7837.80 ms | 751551.33 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 11.07 ms | 9.90 ms | 14.52 ms | 36.18 ms | 158.96 ms | 5641.00 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 11.62 ms | 10.08 ms | 18.14 ms | 37.89 ms | 139.36 ms | 6777.67 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 12.15 ms | 10.09 ms | 18.03 ms | 38.63 ms | 369.20 ms | 15012.33 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 12.25 ms | 10.48 ms | 19.67 ms | 40.20 ms | 174.00 ms | 7136.67 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 11.95 ms | 10.50 ms | 18.60 ms | 35.35 ms | 102.27 ms | 6105.67 | 
| go (1.11) | [beego](http://beego.me) (1.10) | 12.69 ms | 10.59 ms | 19.77 ms | 43.39 ms | 327.59 ms | 11510.67 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 15.20 ms | 11.62 ms | 30.06 ms | 57.31 ms | 185.84 ms | 11361.00 | 
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 18.18 ms | 12.69 ms | 26.90 ms | 113.36 ms | 691.80 ms | 31499.67 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 19.23 ms | 13.51 ms | 26.89 ms | 120.25 ms | 771.66 ms | 35148.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 17.46 ms | 14.09 ms | 31.79 ms | 53.93 ms | 186.66 ms | 10389.33 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 20.65 ms | 14.40 ms | 28.70 ms | 135.01 ms | 795.10 ms | 37363.67 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 57.68 ms | 15.60 ms | 180.35 ms | 461.36 ms | 1203.17 ms | 99274.33 | 
| go (1.11) | [gf](http://gfer.me) (1.2) | 17.06 ms | 15.83 ms | 25.43 ms | 54.22 ms | 197.64 ms | 9451.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 15.79 ms | 16.05 ms | 18.11 ms | 20.86 ms | 109.75 ms | 2418.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 26.07 ms | 16.53 ms | 28.69 ms | 320.57 ms | 1316.83 ms | 70406.67 | 
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 24.88 ms | 16.88 ms | 31.64 ms | 232.99 ms | 937.13 ms | 48706.00 | 
| node (11.1) | [fastify](http://fastify.io) (1.13) | 31.57 ms | 18.96 ms | 32.68 ms | 450.31 ms | 1286.88 ms | 81324.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 55.69 ms | 24.69 ms | 146.57 ms | 299.02 ms | 1338.72 ms | 85555.00 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 39.18 ms | 25.88 ms | 50.10 ms | 431.57 ms | 1265.17 ms | 76948.67 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 47.85 ms | 29.15 ms | 61.06 ms | 581.30 ms | 1520.81 ms | 99392.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 32.30 ms | 30.28 ms | 45.57 ms | 61.30 ms | 318.92 ms | 10761.00 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 41.19 ms | 30.99 ms | 59.28 ms | 275.59 ms | 1072.96 ms | 57716.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 36.79 ms | 33.34 ms | 62.86 ms | 92.86 ms | 161.27 ms | 19030.33 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 37.62 ms | 36.18 ms | 47.41 ms | 115.84 ms | 387.59 ms | 21945.00 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 39.68 ms | 36.39 ms | 56.23 ms | 69.96 ms | 277.99 ms | 11328.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 38.41 ms | 38.85 ms | 49.82 ms | 58.39 ms | 272.38 ms | 11124.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 45.51 ms | 40.63 ms | 63.44 ms | 88.98 ms | 574.97 ms | 25275.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 40.90 ms | 41.56 ms | 52.61 ms | 66.08 ms | 491.24 ms | 18924.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 51.29 ms | 42.55 ms | 83.51 ms | 126.22 ms | 486.06 ms | 28350.00 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 48.58 ms | 45.92 ms | 59.34 ms | 89.77 ms | 570.08 ms | 22990.33 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 108.57 ms | 48.88 ms | 92.28 ms | 1635.92 ms | 2838.37 ms | 268429.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 80.87 ms | 69.13 ms | 144.02 ms | 241.51 ms | 495.22 ms | 47741.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 84.14 ms | 79.09 ms | 124.21 ms | 161.10 ms | 709.95 ms | 35188.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 101.42 ms | 98.40 ms | 162.52 ms | 231.94 ms | 321.53 ms | 46799.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 126.03 ms | 122.26 ms | 172.16 ms | 221.91 ms | 440.41 ms | 36116.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (actix-web) (rust)


:three: (evhtp) (cpp)


:four: (vibora) (python)


:five: (fasthttprouter) (go)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 227152.00 | 271.73 MB |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 196433.33 | 223.20 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 173095.00 | 167.98 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 167793.67 | 190.35 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 163826.67 | 264.96 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 158114.00 | 169.20 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 156772.00 | 315.19 MB |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 133531.33 | 273.24 MB |
| java (8) | [act](http://actframework.org) (1.8) | 132438.67 | 258.56 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 110029.33 | 63.63 MB |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 102893.00 | 129.85 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 101720.33 | 165.54 MB |
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.1) | 101023.67 | 58.39 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 96218.67 | 129.04 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 90970.00 | 159.71 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 89494.67 | 119.16 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 86465.00 | 115.84 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 86212.00 | 151.34 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 82956.67 | 110.81 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 82026.67 | 143.96 MB |
| go (1.11) | [beego](http://beego.me) (1.10) | 80345.67 | 108.45 MB |
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 79164.67 | 157.77 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 69430.33 | 178.35 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 66217.00 | 99.20 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 63940.00 | 60.11 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 62697.00 | 93.84 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 59235.00 | 88.78 MB |
| go (1.11) | [gf](http://gfer.me) (1.2) | 59179.00 | 89.81 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 59144.00 | 145.76 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 53814.00 | 90.11 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 51152.33 | 120.05 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 51115.67 | 107.46 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 50480.00 | 250.67 MB |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 48795.67 | 242.35 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 45445.00 | 97.62 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 43066.33 | 223.39 MB |
| c (99) | [kore](http://kore.io) (3.1) | 41278.33 | 111.91 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 38649.00 | 201.53 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 33524.67 | 70.92 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 32546.67 | 57.08 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 30916.33 | 57.40 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 29207.00 | 71.53 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 28257.67 | 49.56 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 27787.33 | 26.07 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 27734.67 | 46.57 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 27611.33 | 62.66 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 25955.67 | 24.32 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 24798.33 | 30.51 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 24622.33 | 40.12 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 23937.33 | 22.82 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 21925.00 | 35.73 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 20839.00 | 38.05 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 19951.67 | 49.16 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 18373.67 | 47.55 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 18228.67 | 10.53 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 12738.33 | 22.75 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 11762.67 | 6.80 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 11596.67 | 33.63 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 10739.00 | 81.41 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 9873.33 | 19.68 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 9649.00 | 25.08 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 7710.33 | 20.74 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 2215.67 | 6.79 MB |
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

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

> job is either a language (example : crystal) or a framework (example : router.cr)

~~~sh
bin/neph [job]
~~~

+ Start the benchmark ....

> tools is a list of language / framework to challenge (example : ruby kemal amber go python)

~~~sh
bin/benchmarker [tools]
~~~

## Results

<!-- Result from here -->
Last update: 2018-10-25
```
OS: Linux (version: 4.18.16-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: laravel (php)


:three: iron (rust)


:four: rack-routing (ruby)


:five: roda (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.29) | [nickel](http://nickel-org.github.io) (0.10) | 0.11 ms | 0.11 ms | 0.15 ms | 0.23 ms | 10.29 ms | 79.33 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 174.35 ms | 0.66 ms | 463.37 ms | 3104.72 ms | 7043.72 ms | 558050.67 | 
| rust (1.29) | [iron](http://ironframework.io) (0.6) | 0.76 ms | 0.70 ms | 1.16 ms | 2.29 ms | 42.95 ms | 602.33 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 6.82 ms | 1.68 ms | 20.03 ms | 55.02 ms | 175.85 ms | 11558.33 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 5.66 ms | 1.87 ms | 16.08 ms | 43.47 ms | 135.89 ms | 9141.00 | 
| php (7.2) | [symfony](http://symfony.com) (4.1) | 147.82 ms | 1.97 ms | 277.26 ms | 3343.34 ms | 7037.91 ms | 569654.67 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 187.65 ms | 2.07 ms | 339.52 ms | 3710.37 ms | 6721.33 ms | 654951.00 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.2) | 11.13 ms | 2.19 ms | 33.74 ms | 88.33 ms | 251.79 ms | 18781.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 84.29 ms | 2.83 ms | 6.13 ms | 2713.97 ms | 6594.83 ms | 488253.00 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 13.24 ms | 4.67 ms | 37.98 ms | 90.24 ms | 228.09 ms | 19606.00 | 
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 4.81 ms | 4.83 ms | 8.88 ms | 17.28 ms | 37.58 ms | 3678.33 | 
| rust (1.29) | [actix-web](http://actix.rs) (0.7) | 5.17 ms | 5.03 ms | 9.38 ms | 18.51 ms | 94.95 ms | 3827.33 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 10.77 ms | 5.06 ms | 29.05 ms | 66.15 ms | 178.51 ms | 14416.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 6.05 ms | 5.66 ms | 9.55 ms | 13.65 ms | 54.31 ms | 2616.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 6.46 ms | 5.90 ms | 9.59 ms | 19.14 ms | 180.24 ms | 4656.00 | 
| crystal (0.26.1) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 7.00 ms | 5.98 ms | 11.60 ms | 21.85 ms | 47.79 ms | 3940.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 6.54 ms | 6.28 ms | 10.62 ms | 17.78 ms | 62.72 ms | 3376.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 7.02 ms | 6.44 ms | 14.08 ms | 21.76 ms | 55.01 ms | 4824.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 8.17 ms | 8.00 ms | 13.65 ms | 18.81 ms | 255.58 ms | 4523.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 10.73 ms | 9.25 ms | 16.95 ms | 33.23 ms | 177.80 ms | 6639.67 | 
| go (1.11) | [iris](http://iris-go.com) (10.7) | 10.49 ms | 9.69 ms | 15.46 ms | 30.76 ms | 138.66 ms | 5743.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 10.35 ms | 9.70 ms | 14.33 ms | 29.42 ms | 224.06 ms | 6330.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 159.51 ms | 9.74 ms | 29.42 ms | 3859.66 ms | 7167.14 ms | 675280.67 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 10.97 ms | 9.91 ms | 16.03 ms | 32.69 ms | 246.62 ms | 7762.67 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 11.70 ms | 10.35 ms | 17.66 ms | 36.40 ms | 213.96 ms | 7194.33 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 29.57 ms | 10.65 ms | 64.97 ms | 300.04 ms | 651.63 ms | 61576.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 12.24 ms | 10.66 ms | 18.88 ms | 37.93 ms | 87.57 ms | 6464.67 | 
| go (1.11) | [beego](http://beego.me) (1.10) | 12.30 ms | 10.93 ms | 18.60 ms | 34.90 ms | 125.61 ms | 6071.00 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 12.76 ms | 10.97 ms | 19.97 ms | 41.87 ms | 186.84 ms | 7887.00 | 
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 20.25 ms | 13.52 ms | 23.61 ms | 240.82 ms | 897.51 ms | 49168.33 | 
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 21.23 ms | 14.26 ms | 26.22 ms | 212.31 ms | 965.24 ms | 49014.00 | 
| node (10.12) | [foxify](http://foxify.js.org) (0.10) | 22.48 ms | 14.89 ms | 27.06 ms | 241.96 ms | 965.98 ms | 52056.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 17.50 ms | 14.90 ms | 28.49 ms | 52.13 ms | 305.93 ms | 11879.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 15.40 ms | 15.38 ms | 18.07 ms | 21.84 ms | 51.14 ms | 2357.67 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 55.77 ms | 15.94 ms | 173.40 ms | 409.02 ms | 955.04 ms | 89533.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 26.18 ms | 17.39 ms | 29.93 ms | 302.38 ms | 1411.23 ms | 68377.67 | 
| node (10.12) | [fastify](http://fastify.io) (1.12) | 30.45 ms | 18.87 ms | 32.54 ms | 423.99 ms | 1201.85 ms | 75045.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 40.62 ms | 21.77 ms | 43.31 ms | 720.92 ms | 2626.02 ms | 144415.33 | 
| node (10.12) | [koa](http://koajs.com) (2.5) | 32.57 ms | 22.48 ms | 39.97 ms | 341.12 ms | 1211.43 ms | 66390.00 | 
| node (10.12) | [restify](http://restify.com) (7.2) | 30.35 ms | 24.44 ms | 41.00 ms | 161.17 ms | 754.10 ms | 36219.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 36.87 ms | 31.10 ms | 70.38 ms | 103.96 ms | 170.84 ms | 23439.33 | 
| node (10.12) | [express](http://expressjs.com) (4.16) | 47.00 ms | 31.59 ms | 59.39 ms | 552.21 ms | 1494.33 ms | 93040.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 36.04 ms | 32.85 ms | 52.66 ms | 68.05 ms | 241.29 ms | 11631.00 | 
| crystal (0.26.1) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 39.18 ms | 37.70 ms | 49.24 ms | 59.04 ms | 140.55 ms | 7759.33 | 
| crystal (0.26.1) | [raze](http://razecr.com) (0.3) | 39.06 ms | 38.77 ms | 49.23 ms | 58.49 ms | 323.52 ms | 11183.67 | 
| crystal (0.26.1) | [kemal](http://kemalcr.com) (0.24) | 43.82 ms | 40.64 ms | 57.48 ms | 67.72 ms | 179.87 ms | 8975.33 | 
| node (10.12) | [hapi](http://hapijs.com) (17.6) | 85.34 ms | 41.20 ms | 77.49 ms | 1296.78 ms | 2454.62 ms | 215414.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 54.56 ms | 41.57 ms | 103.80 ms | 143.15 ms | 397.63 ms | 33218.67 | 
| crystal (0.26.1) | [amber](http://amberframework.org) (0.9) | 44.16 ms | 42.48 ms | 56.80 ms | 67.17 ms | 211.15 ms | 10100.67 | 
| crystal (0.26.1) | [orion](http://github.com/obsidian/orion) (1.6) | 47.40 ms | 43.76 ms | 60.47 ms | 71.18 ms | 499.19 ms | 19271.67 | 
| crystal (0.26.1) | [lucky](http://luckyframework.org) (0.11) | 48.53 ms | 47.26 ms | 56.42 ms | 133.16 ms | 447.47 ms | 22874.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 79.54 ms | 66.58 ms | 151.26 ms | 235.41 ms | 460.28 ms | 50620.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 82.48 ms | 72.02 ms | 142.52 ms | 200.54 ms | 553.97 ms | 36730.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 131.38 ms | 110.96 ms | 240.86 ms | 388.02 ms | 549.47 ms | 78047.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 128.43 ms | 116.02 ms | 192.41 ms | 232.19 ms | 602.30 ms | 47933.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (actix-web) (rust)


:three: (jester) (nim)


:four: (evhtp) (cpp)


:five: (vibora) (python)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 198290.00 | 237.14 MB |
| rust (1.29) | [actix-web](http://actix.rs) (0.7) | 179502.67 | 203.89 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 157783.00 | 316.95 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 149670.67 | 145.01 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 148902.00 | 169.05 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 144039.00 | 232.62 MB |
| crystal (0.26.1) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 138049.33 | 147.19 MB |
| java (8) | [act](http://actframework.org) (1.8) | 107081.33 | 183.06 MB |
| go (1.11) | [iris](http://iris-go.com) (10.7) | 92910.33 | 124.40 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 90928.00 | 147.99 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 90689.67 | 121.38 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 83946.67 | 147.44 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 81462.67 | 143.05 MB |
| rust (1.29) | [iron](http://ironframework.io) (0.6) | 80459.33 | 100.98 MB |
| go (1.11) | [beego](http://beego.me) (1.10) | 79999.00 | 107.65 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 79978.33 | 140.30 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 78699.00 | 105.12 MB |
| rust (1.29) | [nickel](http://nickel-org.github.io) (0.10) | 77896.00 | 154.89 MB |
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 65898.33 | 98.57 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 64065.67 | 60.26 MB |
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 61334.00 | 91.89 MB |
| node (10.12) | [foxify](http://foxify.js.org) (0.10) | 58719.33 | 123.51 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 58009.67 | 142.91 MB |
| node (10.12) | [fastify](http://fastify.io) (1.12) | 51395.67 | 120.61 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 50230.67 | 84.00 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 46991.33 | 100.86 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 44044.67 | 77.14 MB |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 43523.67 | 216.22 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 43270.67 | 214.58 MB |
| node (10.12) | [koa](http://koajs.com) (2.5) | 39828.00 | 84.33 MB |
| node (10.12) | [restify](http://restify.com) (7.2) | 36741.00 | 64.42 MB |
| c (99) | [kore](http://kore.io) (3.1) | 35333.67 | 95.79 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 32308.33 | 168.48 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 31684.33 | 50.08 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 28446.67 | 64.48 MB |
| node (10.12) | [express](http://expressjs.com) (4.16) | 28125.67 | 68.70 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 27595.33 | 51.25 MB |
| crystal (0.26.1) | [raze](http://razecr.com) (0.3) | 25685.67 | 24.07 MB |
| crystal (0.26.1) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 25356.67 | 23.80 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 22601.33 | 21.58 MB |
| crystal (0.26.1) | [kemal](http://kemalcr.com) (0.24) | 22584.33 | 36.82 MB |
| crystal (0.26.1) | [amber](http://amberframework.org) (0.9) | 22217.00 | 32.22 MB |
| node (10.12) | [hapi](http://hapijs.com) (17.6) | 21990.00 | 56.93 MB |
| crystal (0.26.1) | [lucky](http://luckyframework.org) (0.11) | 21152.00 | 25.94 MB |
| crystal (0.26.1) | [orion](http://github.com/obsidian/orion) (1.6) | 20871.00 | 34.02 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 19236.33 | 47.33 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 18936.00 | 10.93 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 13117.67 | 23.40 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 11898.33 | 6.86 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 11893.33 | 34.53 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.2) | 11472.33 | 86.86 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 9670.33 | 25.14 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 7805.00 | 15.55 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 7509.67 | 20.10 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 2284.67 | 7.00 MB |
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

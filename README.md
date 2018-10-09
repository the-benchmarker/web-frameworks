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
Last update: 2018-10-09
```
OS: Linux (version: 4.16.11-100.fc26.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: rocket (rust)


:three: iron (rust)


:four: roda (ruby)


:five: rack-routing (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust | [nickel](http://nickel-org.github.io) (0.10) | 0.11 ms | 0.09 ms | 0.13 ms | 0.68 ms | 6.35 ms | 155.67 | 
| rust | [rocket](http://rocket.rs) (0.3) | 0.15 ms | 0.12 ms | 0.20 ms | 1.05 ms | 7.36 ms | 205.00 | 
| rust | [iron](http://ironframework.io) (0.6) | 0.59 ms | 0.40 ms | 1.17 ms | 3.68 ms | 27.20 ms | 722.33 | 
| ruby | [roda](http://roda.jeremyevans.net) (3.12) | 2.67 ms | 1.99 ms | 5.35 ms | 13.32 ms | 78.67 ms | 2711.33 | 
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.70 ms | 2.61 ms | 7.56 ms | 20.29 ms | 135.04 ms | 4199.67 | 
| ruby | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.72 ms | 4.16 ms | 12.56 ms | 25.96 ms | 111.62 ms | 5547.67 | 
| ruby | [hanami](http://hanamirb.org) (1.2) | 6.68 ms | 4.66 ms | 14.94 ms | 32.81 ms | 121.18 ms | 6901.33 | 
| go | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.61 ms | 4.86 ms | 9.42 ms | 16.74 ms | 46.46 ms | 3093.33 | 
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 5.48 ms | 4.95 ms | 9.10 ms | 14.47 ms | 54.70 ms | 2851.33 | 
| rust | [actix-web](http://actix.rs) (0.7) | 5.72 ms | 4.99 ms | 10.76 ms | 19.55 ms | 106.89 ms | 4418.00 | 
| php | [symfony](http://symfony.com) (4.1) | 159.56 ms | 5.15 ms | 444.49 ms | 2219.47 ms | 6205.30 ms | 474888.67 | 
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 7.21 ms | 5.34 ms | 15.85 ms | 31.31 ms | 81.83 ms | 6706.33 | 
| php | [laravel](http://laravel.com) (5.7) | 260.68 ms | 5.64 ms | 344.57 ms | 4689.98 ms | 6493.86 ms | 871259.67 | 
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 6.81 ms | 6.01 ms | 11.96 ms | 19.55 ms | 45.63 ms | 3987.67 | 
| python | [vibora](http://vibora.io) (0.0) | 6.67 ms | 6.08 ms | 12.45 ms | 19.71 ms | 45.15 ms | 4350.67 | 
| ruby | [rails](http://rubyonrails.org) (5.2) | 26.66 ms | 7.87 ms | 82.36 ms | 166.24 ms | 326.34 ms | 38449.67 | 
| java | [act](http://actframework.org) (1.8) | 9.02 ms | 8.01 ms | 13.65 ms | 26.74 ms | 197.09 ms | 7141.33 | 
| go | [iris](http://iris-go.com) (10.7) | 9.21 ms | 8.36 ms | 14.18 ms | 24.90 ms | 113.52 ms | 4538.00 | 
| scala | [akkahttp](http://akka.io) (10.1) | 176.06 ms | 8.61 ms | 74.82 ms | 3985.24 ms | 6975.82 ms | 693415.67 | 
| go | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 10.39 ms | 9.20 ms | 16.45 ms | 31.26 ms | 166.13 ms | 5714.33 | 
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 10.68 ms | 9.35 ms | 16.73 ms | 30.95 ms | 175.00 ms | 6230.00 | 
| go | [echo](http://echo.labstack.com) (3.3) | 10.87 ms | 9.47 ms | 17.15 ms | 31.59 ms | 232.00 ms | 6830.33 | 
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 12.98 ms | 9.87 ms | 19.12 ms | 69.06 ms | 582.98 ms | 21754.67 | 
| go | [beego](http://beego.me) (1.10) | 11.37 ms | 9.89 ms | 18.13 ms | 33.52 ms | 141.96 ms | 6317.67 | 
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 30.83 ms | 10.73 ms | 92.56 ms | 277.50 ms | 418.09 ms | 58164.67 | 
| node | [polka](http://github.com/lukeed/polka) (0.5) | 19.87 ms | 12.93 ms | 27.67 ms | 187.84 ms | 796.95 ms | 41261.00 | 
| node | [rayo](http://rayo.js.org) (1.2) | 21.76 ms | 14.06 ms | 35.73 ms | 136.45 ms | 797.76 ms | 37824.00 | 
| python | [bottle](http://bottlepy.org) (0.12) | 19.79 ms | 15.40 ms | 37.12 ms | 66.32 ms | 243.49 ms | 13111.00 | 
| node | [fastify](http://fastify.io) (1.12) | 24.10 ms | 15.58 ms | 33.20 ms | 221.74 ms | 973.44 ms | 49881.33 | 
| swift | [perfect](http://perfect.org) (3.0) | 18.04 ms | 18.03 ms | 20.46 ms | 23.89 ms | 114.04 ms | 4103.67 | 
| swift | [vapor](http://vapor.codes) (3.0) | 32.50 ms | 18.14 ms | 31.70 ms | 542.04 ms | 1726.30 ms | 102837.00 | 
| scala | [http4s](http://http4s.org) (0.18) | 23.72 ms | 19.42 ms | 36.02 ms | 72.29 ms | 1353.12 ms | 47043.67 | 
| node | [koa](http://koajs.com) (2.5) | 34.35 ms | 20.41 ms | 45.35 ms | 461.60 ms | 1325.29 ms | 81203.00 | 
| node | [restify](http://restify.com) (7.2) | 27.96 ms | 21.62 ms | 38.81 ms | 139.52 ms | 772.63 ms | 36439.67 | 
| node | [express](http://expressjs.com) (4.16) | 42.24 ms | 25.37 ms | 56.12 ms | 541.70 ms | 1481.42 ms | 94196.67 | 
| crystal | [kemal](http://kemalcr.com) (0.24) | 35.51 ms | 32.21 ms | 48.34 ms | 60.11 ms | 499.12 ms | 18239.33 | 
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 33.89 ms | 32.44 ms | 44.49 ms | 54.44 ms | 251.67 ms | 9159.67 | 
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 38.10 ms | 33.39 ms | 65.64 ms | 98.13 ms | 141.23 ms | 19864.67 | 
| swift | [kitura](http://kitura.io) (2.2) | 35.08 ms | 33.41 ms | 44.42 ms | 62.88 ms | 214.79 ms | 8489.33 | 
| crystal | [amber](http://amberframework.org) (0.9) | 43.88 ms | 36.40 ms | 52.05 ms | 333.04 ms | 910.76 ms | 58906.00 | 
| node | [hapi](http://hapijs.com) (17.6) | 73.18 ms | 37.81 ms | 78.94 ms | 1144.16 ms | 2144.19 ms | 180239.33 | 
| crystal | [lucky](http://luckyframework.org) (0.11) | 39.76 ms | 39.35 ms | 47.09 ms | 55.83 ms | 128.74 ms | 7124.67 | 
| python | [flask](http://flask.pocoo.org) (1.0) | 57.80 ms | 39.37 ms | 122.45 ms | 163.79 ms | 374.18 ms | 37245.00 | 
| python | [sanic](http://github.com/huge-success/sanic) (0.8) | 62.51 ms | 56.48 ms | 101.50 ms | 163.36 ms | 251.32 ms | 29383.00 | 
| python | [django](http://djangoproject.com) (2.1) | 74.90 ms | 69.01 ms | 104.33 ms | 160.46 ms | 544.19 ms | 26682.00 | 
| python | [tornado](http://tornadoweb.org) (5.1) | 108.64 ms | 107.27 ms | 136.98 ms | 189.96 ms | 645.75 ms | 37902.00 | 
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 124.87 ms | 114.60 ms | 206.55 ms | 285.49 ms | 372.25 ms | 57824.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (evhtp) (cpp)


:two: (fasthttprouter) (go)


:three: (actix-web) (rust)


:four: (vibora) (python)


:five: (spider-gazelle) (crystal)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 164413.33 | 159.67 MB |
| go | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 161730.33 | 260.17 MB |
| rust | [actix-web](http://actix.rs) (0.7) | 160016.00 | 181.96 MB |
| python | [vibora](http://vibora.io) (0.0) | 151188.33 | 171.53 MB |
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 139499.00 | 149.40 MB |
| java | [act](http://actframework.org) (1.8) | 120500.00 | 206.19 MB |
| go | [iris](http://iris-go.com) (10.7) | 104314.00 | 139.37 MB |
| rust | [iron](http://ironframework.io) (0.6) | 98968.00 | 124.82 MB |
| go | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 94483.33 | 126.29 MB |
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 91961.33 | 161.40 MB |
| go | [echo](http://echo.labstack.com) (3.3) | 90194.33 | 158.31 MB |
| rust | [rocket](http://rocket.rs) (0.3) | 87814.00 | 139.28 MB |
| go | [beego](http://beego.me) (1.10) | 87009.67 | 117.31 MB |
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 83404.67 | 135.81 MB |
| rust | [nickel](http://nickel-org.github.io) (0.10) | 80589.00 | 160.36 MB |
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 74356.67 | 130.60 MB |
| scala | [akkahttp](http://akka.io) (10.1) | 69082.33 | 148.11 MB |
| php | [laravel](http://laravel.com) (5.7) | 68007.67 | 338.57 MB |
| node | [polka](http://github.com/lukeed/polka) (0.5) | 64612.33 | 96.84 MB |
| node | [rayo](http://rayo.js.org) (1.2) | 56794.00 | 85.00 MB |
| swift | [perfect](http://perfect.org) (3.0) | 54737.67 | 51.42 MB |
| python | [bottle](http://bottlepy.org) (0.12) | 54455.67 | 134.23 MB |
| node | [fastify](http://fastify.io) (1.12) | 54268.67 | 133.38 MB |
| php | [symfony](http://symfony.com) (4.1) | 51347.33 | 255.56 MB |
| scala | [http4s](http://http4s.org) (0.18) | 48237.33 | 84.59 MB |
| swift | [vapor](http://vapor.codes) (3.0) | 47973.00 | 81.69 MB |
| ruby | [roda](http://roda.jeremyevans.net) (3.12) | 46919.67 | 44.83 MB |
| node | [koa](http://koajs.com) (2.5) | 41469.00 | 87.77 MB |
| node | [restify](http://restify.com) (7.2) | 40341.00 | 70.78 MB |
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 34275.67 | 19.81 MB |
| node | [express](http://expressjs.com) (4.16) | 33507.67 | 82.02 MB |
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 29380.67 | 27.56 MB |
| swift | [kitura](http://kitura.io) (2.2) | 28164.00 | 52.45 MB |
| crystal | [kemal](http://kemalcr.com) (0.24) | 28062.00 | 45.72 MB |
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 26611.00 | 60.38 MB |
| crystal | [amber](http://amberframework.org) (0.9) | 26213.67 | 37.98 MB |
| crystal | [lucky](http://luckyframework.org) (0.11) | 25004.00 | 30.68 MB |
| node | [hapi](http://hapijs.com) (17.6) | 23602.33 | 61.23 MB |
| ruby | [flame](http://github.com/AlexWayfer/flame) (4.18) | 22210.33 | 12.84 MB |
| python | [flask](http://flask.pocoo.org) (1.0) | 19055.67 | 47.00 MB |
| ruby | [hanami](http://hanamirb.org) (1.2) | 19049.33 | 144.44 MB |
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 17645.00 | 45.89 MB |
| python | [sanic](http://github.com/huge-success/sanic) (0.8) | 16132.67 | 28.81 MB |
| python | [django](http://djangoproject.com) (2.1) | 13044.67 | 37.87 MB |
| python | [tornado](http://tornadoweb.org) (5.1) | 8955.67 | 24.05 MB |
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 7942.67 | 15.87 MB |
| ruby | [rails](http://rubyonrails.org) (5.2) | 4813.33 | 14.65 MB |
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

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
Last update: 2018-09-24
```
OS: Linux (version: 4.16.11-100.fc26.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: rocket (rust)


:two: nickel (rust)


:three: iron (rust)


:four: roda (ruby)


:five: rack-routing (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust | [rocket](http://nickel-org.github.io) (0.3) | 0.13 ms | 0.11 ms | 0.21 ms | 0.31 ms | 9.09 ms | 103.00 | 
| rust | [nickel](http://nickel.rs) (0.10) | 0.10 ms | 0.11 ms | 0.14 ms | 0.17 ms | 5.86 ms | 52.33 | 
| rust | [iron](http://ironframework.io) (0.7) | 0.56 ms | 0.46 ms | 0.91 ms | 2.69 ms | 45.78 ms | 646.33 | 
| ruby | [roda](http://roda.jeremyevans.net) (3.12) | 2.82 ms | 1.79 ms | 6.06 ms | 18.85 ms | 83.82 ms | 3793.67 | 
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.76 ms | 2.23 ms | 8.42 ms | 27.44 ms | 97.03 ms | 5387.67 | 
| php | [symfony](http://symfony.com) (4.1) | 165.66 ms | 3.39 ms | 426.83 ms | 2582.55 ms | 6933.87 ms | 552950.00 | 
| ruby | [flame](http://github.com/AlexWayfer/flame) (5.0 (rc)) | 5.99 ms | 3.75 ms | 14.06 ms | 35.05 ms | 118.21 ms | 7174.00 | 
| ruby | [hanami](http://hanamirb.org) (1.2) | 6.52 ms | 3.79 ms | 15.71 ms | 41.55 ms | 112.04 ms | 8386.33 | 
| go | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.96 ms | 4.39 ms | 7.70 ms | 14.70 ms | 217.41 ms | 4445.67 | 
| rust | [actix-web](http://actix.rs) (0.7) | 5.22 ms | 4.47 ms | 9.44 ms | 15.60 ms | 36.63 ms | 3213.00 | 
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 5.57 ms | 4.88 ms | 9.37 ms | 15.93 ms | 101.43 ms | 3113.00 | 
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 7.91 ms | 4.89 ms | 18.95 ms | 44.39 ms | 130.71 ms | 9344.33 | 
| php | [laravel](http://laravel.com) (5.7) | 275.45 ms | 4.90 ms | 525.80 ms | 4926.64 ms | 7239.44 ms | 905625.33 | 
| python | [vibora](http://vibora.io) (0.0) | 6.64 ms | 5.87 ms | 12.33 ms | 20.49 ms | 48.65 ms | 4366.67 | 
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 6.89 ms | 5.88 ms | 12.16 ms | 20.44 ms | 47.98 ms | 4071.67 | 
| ruby | [rails](http://rubyonrails.org) (5.2) | 28.77 ms | 6.89 ms | 93.04 ms | 179.03 ms | 323.52 ms | 42497.33 | 
| java | [act](http://actframework.org) (1.8) | 8.36 ms | 7.25 ms | 12.27 ms | 22.16 ms | 222.81 ms | 7025.33 | 
| go | [echo](http://echo.labstack.com) (3.3) | 9.28 ms | 8.17 ms | 14.10 ms | 27.49 ms | 245.44 ms | 8225.67 | 
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 9.36 ms | 8.27 ms | 14.47 ms | 27.98 ms | 235.41 ms | 7385.33 | 
| go | [iris](http://iris-go.com) (10.7) | 10.44 ms | 8.49 ms | 14.54 ms | 35.27 ms | 503.93 ms | 17610.00 | 
| scala | [akkahttp](http://akka.io) (10.0) | 228.32 ms | 8.55 ms | 127.82 ms | 5115.25 ms | 7935.92 ms | 890462.00 | 
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (1.1) | 9.75 ms | 8.56 ms | 15.67 ms | 24.55 ms | 226.25 ms | 5893.33 | 
| go | [beego](http://beego.me) (1.10) | 10.84 ms | 9.52 ms | 16.69 ms | 32.83 ms | 154.65 ms | 6535.00 | 
| go | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 11.03 ms | 9.58 ms | 17.20 ms | 33.85 ms | 307.09 ms | 8144.00 | 
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 31.92 ms | 10.29 ms | 74.46 ms | 328.06 ms | 568.31 ms | 67312.00 | 
| node | [rayo](http://rayo.js.org) (1.2) | 23.26 ms | 14.43 ms | 35.57 ms | 189.07 ms | 930.71 ms | 46418.00 | 
| python | [bottle](http://bottlepy.org) (0.12) | 18.29 ms | 14.78 ms | 30.97 ms | 59.45 ms | 276.03 ms | 12883.67 | 
| node | [fastify](http://fastify.io) (1.11) | 25.97 ms | 15.95 ms | 33.76 ms | 290.54 ms | 1075.37 ms | 58899.00 | 
| node | [polka](http://github.com/lukeed/polka) (0.5) | 27.48 ms | 16.17 ms | 41.28 ms | 306.43 ms | 1091.66 ms | 60628.33 | 
| swift | [perfect](http://perfect.org) (3.0) | 16.98 ms | 17.17 ms | 19.16 ms | 21.84 ms | 165.03 ms | 4552.33 | 
| swift | [vapor](http://vapor.codes) (3.0) | 32.78 ms | 17.55 ms | 30.69 ms | 580.20 ms | 1741.95 ms | 106799.33 | 
| scala | [http4s](http://http4s.org) (0.0) | 21.41 ms | 19.00 ms | 36.19 ms | 62.58 ms | 879.06 ms | 23465.67 | 
| node | [koa](http://koajs.com) (2.5) | 30.12 ms | 19.15 ms | 42.27 ms | 301.53 ms | 1086.03 ms | 60773.00 | 
| node | [restify](http://restify.com) (7.2) | 28.84 ms | 21.41 ms | 43.32 ms | 137.17 ms | 780.16 ms | 37242.33 | 
| node | [express](http://expressjs.com) (4.16) | 37.91 ms | 21.69 ms | 49.31 ms | 500.06 ms | 1415.80 ms | 87638.67 | 
| swift | [kitura](http://kitura.io) (2.2) | 28.91 ms | 28.91 ms | 32.51 ms | 37.15 ms | 314.49 ms | 7454.00 | 
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 31.35 ms | 30.21 ms | 40.19 ms | 47.92 ms | 119.93 ms | 7025.67 | 
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 36.87 ms | 32.29 ms | 62.41 ms | 90.86 ms | 254.18 ms | 18939.33 | 
| crystal | [prism](http://github.com/vladfaust/prism) (0.4 (beta 3)) | 33.71 ms | 33.92 ms | 44.02 ms | 51.67 ms | 183.50 ms | 8777.67 | 
| node | [hapi](http://hapijs.com) (17.5) | 68.11 ms | 34.66 ms | 71.50 ms | 1049.48 ms | 2121.69 ms | 169943.33 | 
| crystal | [amber](http://amberframework.org) (0.9) | 38.68 ms | 36.57 ms | 49.37 ms | 56.28 ms | 224.87 ms | 8687.67 | 
| python | [flask](http://flask.pocoo.org) (1.0) | 48.47 ms | 37.81 ms | 96.26 ms | 133.80 ms | 530.37 ms | 35738.00 | 
| crystal | [lucky](http://luckyframework.org) (0.11) | 37.94 ms | 38.27 ms | 45.72 ms | 52.73 ms | 325.24 ms | 11603.00 | 
| crystal | [kemal](http://kemalcr.com) (0.24) | 42.43 ms | 41.18 ms | 49.23 ms | 138.49 ms | 829.64 ms | 32629.67 | 
| python | [sanic](http://github.com/huge-success/sanic) (0.8) | 60.64 ms | 52.89 ms | 104.60 ms | 165.52 ms | 405.89 ms | 33565.00 | 
| python | [django](http://djangoproject.com) (2.1) | 74.74 ms | 67.69 ms | 118.97 ms | 177.81 ms | 743.56 ms | 36216.67 | 
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 118.83 ms | 107.11 ms | 207.74 ms | 304.32 ms | 458.05 ms | 63983.33 | 
| python | [tornado](http://tornadoweb.org) (5.1) | 111.53 ms | 108.33 ms | 147.73 ms | 191.46 ms | 748.46 ms | 37248.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (fasthttprouter) (go)


:two: (actix-web) (rust)


:three: (evhtp) (cpp)


:four: (vibora) (python)


:five: (spider-gazelle) (crystal)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| go | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 182700.33 | 294.64 MB |
| rust | [actix-web](http://actix.rs) (0.7) | 178377.67 | 202.88 MB |
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 166544.00 | 161.75 MB |
| python | [vibora](http://vibora.io) (0.0) | 157024.67 | 178.28 MB |
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 141333.33 | 151.26 MB |
| java | [act](http://actframework.org) (1.8) | 130977.00 | 224.04 MB |
| go | [echo](http://echo.labstack.com) (3.3) | 106283.67 | 186.35 MB |
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 105253.33 | 184.88 MB |
| rust | [iron](http://ironframework.io) (0.7) | 105094.33 | 132.57 MB |
| go | [iris](http://iris-go.com) (10.7) | 102518.67 | 137.33 MB |
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (1.1) | 99630.33 | 162.24 MB |
| go | [beego](http://beego.me) (1.10) | 92208.00 | 124.15 MB |
| rust | [rocket](http://nickel-org.github.io) (0.3) | 91594.33 | 143.69 MB |
| go | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 90625.00 | 120.97 MB |
| rust | [nickel](http://nickel.rs) (0.10) | 79057.67 | 157.18 MB |
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 77599.33 | 136.12 MB |
| scala | [akkahttp](http://akka.io) (10.0) | 67736.67 | 145.51 MB |
| swift | [perfect](http://perfect.org) (3.0) | 57474.67 | 54.08 MB |
| python | [bottle](http://bottlepy.org) (0.12) | 56446.00 | 139.01 MB |
| php | [laravel](http://laravel.com) (5.7) | 56232.67 | 280.23 MB |
| node | [rayo](http://rayo.js.org) (1.2) | 55196.33 | 82.70 MB |
| node | [fastify](http://fastify.io) (1.11) | 53754.00 | 130.56 MB |
| node | [polka](http://github.com/lukeed/polka) (0.5) | 49905.67 | 74.82 MB |
| swift | [vapor](http://vapor.codes) (3.0) | 49534.67 | 84.15 MB |
| scala | [http4s](http://http4s.org) (0.0) | 49285.67 | 86.33 MB |
| php | [symfony](http://symfony.com) (4.1) | 47494.33 | 236.22 MB |
| ruby | [roda](http://roda.jeremyevans.net) (3.12) | 45237.67 | 43.20 MB |
| node | [koa](http://koajs.com) (2.5) | 42669.33 | 90.33 MB |
| node | [restify](http://restify.com) (7.2) | 39158.67 | 68.66 MB |
| node | [express](http://expressjs.com) (4.16) | 37878.67 | 92.79 MB |
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 34330.00 | 19.84 MB |
| swift | [kitura](http://kitura.io) (2.2) | 34110.00 | 63.34 MB |
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 32015.67 | 30.02 MB |
| crystal | [prism](http://github.com/vladfaust/prism) (0.4 (beta 3)) | 29889.67 | 33.22 MB |
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 27377.33 | 61.98 MB |
| crystal | [lucky](http://luckyframework.org) (0.11) | 26347.00 | 32.60 MB |
| node | [hapi](http://hapijs.com) (17.5) | 25661.00 | 66.47 MB |
| crystal | [amber](http://amberframework.org) (0.9) | 25523.00 | 37.00 MB |
| crystal | [kemal](http://kemalcr.com) (0.24) | 24729.33 | 40.27 MB |
| python | [flask](http://flask.pocoo.org) (1.0) | 21648.67 | 53.31 MB |
| ruby | [flame](http://github.com/AlexWayfer/flame) (5.0 (rc)) | 21367.00 | 12.35 MB |
| ruby | [hanami](http://hanamirb.org) (1.2) | 19677.00 | 149.11 MB |
| python | [sanic](http://github.com/huge-success/sanic) (0.8) | 16776.00 | 29.85 MB |
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 16128.33 | 41.93 MB |
| python | [django](http://djangoproject.com) (2.1) | 13206.67 | 38.22 MB |
| python | [tornado](http://tornadoweb.org) (5.1) | 8667.00 | 23.23 MB |
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 8448.33 | 16.82 MB |
| ruby | [rails](http://rubyonrails.org) (5.2) | 4453.00 | 13.58 MB |
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

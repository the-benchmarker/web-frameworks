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
Last update: 2018-10-10
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
| rust | [nickel](http://nickel-org.github.io) (0.10) | 0.10 ms | 0.09 ms | 0.12 ms | 0.37 ms | 5.37 ms | 111.00 | 
| rust | [rocket](http://rocket.rs) (0.3) | 0.16 ms | 0.12 ms | 0.21 ms | 1.08 ms | 14.89 ms | 258.33 | 
| rust | [iron](http://ironframework.io) (0.6) | 0.60 ms | 0.46 ms | 1.07 ms | 3.42 ms | 25.11 ms | 661.33 | 
| ruby | [roda](http://roda.jeremyevans.net) (3.12) | 2.49 ms | 1.85 ms | 4.74 ms | 13.69 ms | 96.57 ms | 2945.33 | 
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.51 ms | 2.46 ms | 7.44 ms | 18.63 ms | 112.41 ms | 3910.67 | 
| ruby | [hanami](http://hanamirb.org) (1.2) | 6.08 ms | 4.17 ms | 13.94 ms | 28.63 ms | 90.26 ms | 6124.67 | 
| ruby | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.83 ms | 4.20 ms | 12.91 ms | 25.07 ms | 73.11 ms | 5397.67 | 
| rust | [actix-web](http://actix.rs) (0.7) | 5.08 ms | 4.45 ms | 9.36 ms | 15.77 ms | 61.08 ms | 3420.67 | 
| php | [slim](http://slimframework.com) (3.11) | 150.76 ms | 4.63 ms | 388.11 ms | 2657.33 ms | 6884.17 ms | 511973.67 | 
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 5.86 ms | 4.96 ms | 9.71 ms | 21.25 ms | 77.56 ms | 4367.00 | 
| go | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.91 ms | 5.03 ms | 10.10 ms | 18.78 ms | 50.47 ms | 3568.33 | 
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 7.19 ms | 5.06 ms | 16.25 ms | 31.77 ms | 98.07 ms | 6891.67 | 
| php | [laravel](http://laravel.com) (5.7) | 263.16 ms | 5.75 ms | 354.07 ms | 4757.24 ms | 6845.82 ms | 884354.00 | 
| python | [vibora](http://vibora.io) (0.0) | 6.43 ms | 5.77 ms | 12.03 ms | 19.75 ms | 50.52 ms | 4282.67 | 
| php | [symfony](http://symfony.com) (4.1) | 69.26 ms | 5.88 ms | 63.51 ms | 1797.86 ms | 6925.21 ms | 423098.00 | 
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 7.03 ms | 6.16 ms | 12.47 ms | 21.12 ms | 55.40 ms | 4322.67 | 
| java | [act](http://actframework.org) (1.8) | 8.10 ms | 7.04 ms | 12.52 ms | 25.99 ms | 162.41 ms | 5598.67 | 
| ruby | [rails](http://rubyonrails.org) (5.2) | 27.85 ms | 7.31 ms | 88.26 ms | 171.97 ms | 346.83 ms | 40438.33 | 
| scala | [akkahttp](http://akka.io) (10.1) | 190.28 ms | 8.29 ms | 25.36 ms | 4867.20 ms | 7918.70 ms | 816651.67 | 
| go | [iris](http://iris-go.com) (10.7) | 9.96 ms | 8.80 ms | 15.83 ms | 30.31 ms | 162.11 ms | 5436.33 | 
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 10.90 ms | 9.02 ms | 16.78 ms | 36.85 ms | 368.18 ms | 12176.67 | 
| go | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 10.70 ms | 9.27 ms | 17.02 ms | 33.91 ms | 249.38 ms | 8138.67 | 
| go | [beego](http://beego.me) (1.10) | 11.33 ms | 9.73 ms | 17.86 ms | 34.05 ms | 242.30 ms | 8090.33 | 
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 11.49 ms | 9.87 ms | 18.45 ms | 34.38 ms | 240.69 ms | 7781.00 | 
| go | [echo](http://echo.labstack.com) (3.3) | 12.47 ms | 10.64 ms | 20.32 ms | 39.14 ms | 144.05 ms | 7356.33 | 
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 32.45 ms | 11.07 ms | 96.86 ms | 307.52 ms | 509.79 ms | 62835.67 | 
| node | [rayo](http://rayo.js.org) (1.2) | 16.89 ms | 11.74 ms | 23.72 ms | 125.33 ms | 636.97 ms | 31015.33 | 
| node | [polka](http://github.com/lukeed/polka) (0.5) | 18.79 ms | 12.08 ms | 26.36 ms | 173.17 ms | 768.63 ms | 39216.33 | 
| node | [fastify](http://fastify.io) (1.12) | 28.24 ms | 16.11 ms | 36.85 ms | 349.16 ms | 1089.70 ms | 65135.33 | 
| node | [koa](http://koajs.com) (2.5) | 26.16 ms | 16.24 ms | 37.35 ms | 234.26 ms | 1011.58 ms | 52857.33 | 
| python | [bottle](http://bottlepy.org) (0.12) | 20.77 ms | 16.91 ms | 36.54 ms | 65.48 ms | 303.88 ms | 14669.33 | 
| swift | [perfect](http://perfect.org) (3.0) | 16.81 ms | 16.94 ms | 19.22 ms | 22.23 ms | 55.57 ms | 2371.00 | 
| swift | [vapor](http://vapor.codes) (3.0) | 34.47 ms | 17.31 ms | 31.02 ms | 645.52 ms | 2230.82 ms | 118088.33 | 
| scala | [http4s](http://http4s.org) (0.18) | 26.73 ms | 18.45 ms | 35.51 ms | 273.24 ms | 1786.84 ms | 75468.00 | 
| node | [restify](http://restify.com) (7.2) | 25.11 ms | 19.26 ms | 38.51 ms | 93.21 ms | 614.40 ms | 26547.33 | 
| node | [express](http://expressjs.com) (4.16) | 39.78 ms | 22.52 ms | 51.42 ms | 565.43 ms | 1488.64 ms | 95150.00 | 
| swift | [kitura](http://kitura.io) (2.2) | 28.24 ms | 28.27 ms | 32.73 ms | 38.21 ms | 309.73 ms | 7645.67 | 
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 32.07 ms | 31.79 ms | 42.01 ms | 50.82 ms | 287.55 ms | 8945.67 | 
| node | [hapi](http://hapijs.com) (17.6) | 65.18 ms | 32.83 ms | 68.59 ms | 1031.34 ms | 2081.26 ms | 166595.33 | 
| crystal | [kemal](http://kemalcr.com) (0.24) | 36.22 ms | 32.96 ms | 50.69 ms | 60.18 ms | 127.34 ms | 8919.33 | 
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 44.29 ms | 38.90 ms | 74.13 ms | 125.16 ms | 211.10 ms | 23336.00 | 
| crystal | [lucky](http://luckyframework.org) (0.11) | 39.22 ms | 39.76 ms | 44.25 ms | 52.59 ms | 364.13 ms | 10314.33 | 
| python | [flask](http://flask.pocoo.org) (1.0) | 48.41 ms | 40.93 ms | 75.52 ms | 126.31 ms | 526.16 ms | 25075.00 | 
| crystal | [amber](http://amberframework.org) (0.9) | 38.76 ms | 43.54 ms | 48.14 ms | 57.25 ms | 216.86 ms | 10681.00 | 
| python | [sanic](http://github.com/huge-success/sanic) (0.8) | 67.46 ms | 61.92 ms | 108.42 ms | 176.50 ms | 311.02 ms | 32539.00 | 
| python | [django](http://djangoproject.com) (2.1) | 86.02 ms | 63.40 ms | 181.16 ms | 242.73 ms | 587.75 ms | 53233.00 | 
| python | [tornado](http://tornadoweb.org) (5.1) | 108.29 ms | 103.71 ms | 134.35 ms | 227.44 ms | 1096.39 ms | 53344.67 | 
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 121.29 ms | 107.96 ms | 202.43 ms | 279.61 ms | 409.56 ms | 57111.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (actix-web) (rust)


:two: (evhtp) (cpp)


:three: (vibora) (python)


:four: (fasthttprouter) (go)


:five: (spider-gazelle) (crystal)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| rust | [actix-web](http://actix.rs) (0.7) | 180423.33 | 205.19 MB |
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 162571.33 | 157.89 MB |
| python | [vibora](http://vibora.io) (0.0) | 160044.00 | 181.74 MB |
| go | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 155972.00 | 250.14 MB |
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 136271.67 | 145.58 MB |
| java | [act](http://actframework.org) (1.8) | 134279.67 | 229.72 MB |
| rust | [iron](http://ironframework.io) (0.6) | 98004.67 | 123.32 MB |
| go | [iris](http://iris-go.com) (10.7) | 97472.33 | 132.68 MB |
| go | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 93179.67 | 125.11 MB |
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 91244.00 | 148.64 MB |
| rust | [nickel](http://nickel-org.github.io) (0.10) | 88162.00 | 175.31 MB |
| go | [beego](http://beego.me) (1.10) | 87972.00 | 118.79 MB |
| rust | [rocket](http://rocket.rs) (0.3) | 86448.67 | 135.40 MB |
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 86040.67 | 151.12 MB |
| go | [echo](http://echo.labstack.com) (3.3) | 79883.67 | 140.22 MB |
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 72842.33 | 127.92 MB |
| node | [rayo](http://rayo.js.org) (1.2) | 71632.00 | 107.39 MB |
| node | [polka](http://github.com/lukeed/polka) (0.5) | 68407.33 | 102.51 MB |
| php | [laravel](http://laravel.com) (5.7) | 67039.67 | 333.95 MB |
| scala | [akkahttp](http://akka.io) (10.1) | 65707.00 | 141.09 MB |
| swift | [perfect](http://perfect.org) (3.0) | 58486.00 | 55.03 MB |
| php | [symfony](http://symfony.com) (4.1) | 57951.00 | 288.72 MB |
| php | [slim](http://slimframework.com) (3.11) | 54502.00 | 271.21 MB |
| node | [fastify](http://fastify.io) (1.12) | 51879.33 | 126.74 MB |
| python | [bottle](http://bottlepy.org) (0.12) | 50270.33 | 123.88 MB |
| scala | [http4s](http://http4s.org) (0.18) | 50181.33 | 88.02 MB |
| ruby | [roda](http://roda.jeremyevans.net) (3.12) | 49845.33 | 47.63 MB |
| swift | [vapor](http://vapor.codes) (3.0) | 49759.67 | 85.03 MB |
| node | [koa](http://koajs.com) (2.5) | 48976.33 | 103.74 MB |
| node | [restify](http://restify.com) (7.2) | 43485.00 | 76.14 MB |
| node | [express](http://expressjs.com) (4.16) | 36915.33 | 90.46 MB |
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 36097.00 | 20.87 MB |
| swift | [kitura](http://kitura.io) (2.2) | 34713.00 | 64.52 MB |
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 30672.33 | 28.76 MB |
| crystal | [kemal](http://kemalcr.com) (0.24) | 27245.00 | 44.40 MB |
| node | [hapi](http://hapijs.com) (17.6) | 27132.67 | 70.37 MB |
| crystal | [lucky](http://luckyframework.org) (0.11) | 25417.00 | 31.29 MB |
| crystal | [amber](http://amberframework.org) (0.9) | 25358.33 | 36.74 MB |
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 23185.00 | 52.60 MB |
| ruby | [flame](http://github.com/AlexWayfer/flame) (4.18) | 21873.00 | 12.64 MB |
| ruby | [hanami](http://hanamirb.org) (1.2) | 20962.00 | 158.84 MB |
| python | [flask](http://flask.pocoo.org) (1.0) | 20607.67 | 50.83 MB |
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 17703.67 | 46.04 MB |
| python | [sanic](http://github.com/huge-success/sanic) (0.8) | 14977.33 | 26.75 MB |
| python | [django](http://djangoproject.com) (2.1) | 12269.33 | 35.61 MB |
| python | [tornado](http://tornadoweb.org) (5.1) | 9053.33 | 24.26 MB |
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 8214.67 | 16.39 MB |
| ruby | [rails](http://rubyonrails.org) (5.2) | 4595.67 | 14.03 MB |
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

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
Last update: 2018-09-19
```
OS: Linux (version: 4.18.7-200.fc28.x86_64, arch: x86_64)
CPU Cores: 4
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: rocket (rust)


:three: laravel (php)


:four: hanami (ruby)


:five: iron (rust)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50% percentile | 90% percentile | 99% percentile | 99.9% percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust | [nickel](http://nickel.rs) (0.10) | 0.04 ms | 0.04 ms | 0.05 ms | 0.05 ms | 1.02 ms | 10.33 | 
| rust | [rocket](http://rocket.rs) (0.3) | 0.07 ms | 0.05 ms | 0.09 ms | 0.20 ms | 18.75 ms | 301.33 | 
| php | [laravel](http://laravel.com) (5.7) | 254.78 ms | 0.22 ms | 854.02 ms | 3254.27 ms | 7088.94 ms | 700420.67 | 
| ruby | [hanami](http://hanamirb.org) (1.2) | 3.44 ms | 0.28 ms | 9.80 ms | 43.84 ms | 108.53 ms | 8221.67 | 
| rust | [iron](http://ironframework.io) (0.7) | 0.33 ms | 0.29 ms | 0.46 ms | 1.00 ms | 49.36 ms | 496.67 | 
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 4.07 ms | 0.43 ms | 11.60 ms | 44.89 ms | 102.76 ms | 8523.00 | 
| php | [symfony](http://symfony.com) (4.1) | 167.04 ms | 0.46 ms | 511.40 ms | 2425.83 ms | 7022.24 ms | 502642.67 | 
| ruby | [roda](http://roda.jeremyevans.net) (3.11) | 1.68 ms | 0.46 ms | 3.33 ms | 25.72 ms | 108.63 ms | 4919.00 | 
| ruby | [flame](http://github.com/AlexWayfer/flame) (5.0 (rc)) | 3.30 ms | 0.56 ms | 9.26 ms | 37.02 ms | 90.66 ms | 7036.33 | 
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 2.09 ms | 0.73 ms | 3.59 ms | 36.62 ms | 113.78 ms | 6307.33 | 
| ruby | [rails](http://rubyonrails.org) (5.2) | 16.17 ms | 2.01 ms | 58.01 ms | 103.38 ms | 200.39 ms | 25634.67 | 
| rust | [actix-web](http://actix.rs) (0.7) | 5.47 ms | 4.97 ms | 9.32 ms | 14.58 ms | 152.10 ms | 5009.33 | 
| go | [fasthttprouter](http://github.com/godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.57 ms | 5.17 ms | 8.58 ms | 14.90 ms | 50.63 ms | 2632.33 | 
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 5.98 ms | 5.50 ms | 9.74 ms | 16.57 ms | 129.11 ms | 3944.33 | 
| java | [act](http://actframework.org) (1.8) | 7.13 ms | 5.75 ms | 11.63 ms | 26.06 ms | 208.20 ms | 7074.67 | 
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 7.00 ms | 6.16 ms | 11.48 ms | 19.59 ms | 38.00 ms | 3573.00 | 
| python | [vibora](http://vibora.io) (0.0) | 6.87 ms | 6.28 ms | 12.25 ms | 18.78 ms | 39.28 ms | 4260.00 | 
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (1.1) | 8.55 ms | 7.89 ms | 11.30 ms | 16.36 ms | 286.45 ms | 6764.33 | 
| scala | [akkahttp](http://akka.io) (10.0) | 209.14 ms | 8.01 ms | 51.71 ms | 4874.23 ms | 7923.02 ms | 845993.00 | 
| go | [iris](http://iris-go.com) (10.7) | 17.41 ms | 10.06 ms | 26.81 ms | 192.86 ms | 579.23 ms | 36953.00 | 
| go | [echo](http://echo.labstack.com) (3.3) | 17.19 ms | 10.53 ms | 28.02 ms | 180.25 ms | 456.01 ms | 32134.00 | 
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 19.15 ms | 10.81 ms | 34.53 ms | 189.43 ms | 582.79 ms | 36453.67 | 
| go | [gorilla-mux](http://gorillatoolkit.org/pkg/mux) (1.6) | 16.28 ms | 11.34 ms | 29.85 ms | 134.17 ms | 509.07 ms | 28332.00 | 
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 29.51 ms | 12.05 ms | 65.22 ms | 273.88 ms | 455.06 ms | 55193.00 | 
| scala | [http4s](http://http4s.org) (0.0) | 32.41 ms | 15.79 ms | 29.32 ms | 636.87 ms | 2486.66 ms | 130169.33 | 
| swift | [perfect](http://perfect.org) (3.0) | 15.78 ms | 16.11 ms | 16.72 ms | 17.71 ms | 163.47 ms | 3869.00 | 
| node | [polka](http://github.com/lukeed/polka) (0.4) | 39.70 ms | 17.35 ms | 39.35 ms | 757.20 ms | 1766.61 ms | 125214.67 | 
| node | [rayo](http://rayo.js.org) (1.2) | 47.79 ms | 17.44 ms | 41.58 ms | 1007.59 ms | 2025.80 ms | 159976.33 | 
| python | [bottle](http://bottlepy.org) (0.12) | 23.72 ms | 17.84 ms | 31.89 ms | 96.56 ms | 696.14 ms | 33155.00 | 
| node | [fastify](http://fastify.io) (1.11) | 42.41 ms | 18.86 ms | 39.29 ms | 823.07 ms | 1994.06 ms | 137416.67 | 
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 20.20 ms | 20.16 ms | 24.27 ms | 24.81 ms | 27.97 ms | 2980.33 | 
| crystal | [amber](http://amberframework.org) (0.9) | 22.66 ms | 20.34 ms | 30.23 ms | 31.01 ms | 103.36 ms | 5048.33 | 
| crystal | [kemal](http://kemalcr.com) (0.24) | 23.54 ms | 21.16 ms | 29.53 ms | 30.40 ms | 49.91 ms | 4131.00 | 
| node | [koa](http://koajs.com) (2.5) | 52.57 ms | 21.87 ms | 42.02 ms | 1033.60 ms | 2115.16 ms | 166767.67 | 
| swift | [kitura](http://kitura.io) (2.2) | 23.13 ms | 22.32 ms | 28.87 ms | 41.65 ms | 602.40 ms | 16730.67 | 
| crystal | [prism](http://github.com/vladfaust/prism) (0.4 (beta 3)) | 21.14 ms | 22.69 ms | 25.69 ms | 26.24 ms | 307.19 ms | 6589.67 | 
| swift | [vapor](http://vapor.codes) (3.0) | 125.77 ms | 23.03 ms | 117.88 ms | 2602.53 ms | 4088.48 ms | 442244.67 | 
| node | [restify](http://restify.com) (7.2) | 31.98 ms | 23.50 ms | 45.98 ms | 256.54 ms | 1069.46 ms | 54232.33 | 
| node | [express](http://expressjs.com) (4.16) | 54.52 ms | 24.97 ms | 46.95 ms | 1025.79 ms | 2173.49 ms | 164880.67 | 
| crystal | [lucky](http://luckyframework.org) (0.11) | 24.73 ms | 25.33 ms | 28.29 ms | 29.46 ms | 99.63 ms | 4019.00 | 
| node | [hapi](http://hapijs.com) (17.5) | 95.19 ms | 31.29 ms | 64.36 ms | 1799.81 ms | 3347.29 ms | 299918.67 | 
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 36.42 ms | 33.59 ms | 51.47 ms | 59.35 ms | 369.85 ms | 16412.33 | 
| python | [flask](http://flask.pocoo.org) (1.0) | 55.13 ms | 50.12 ms | 73.58 ms | 100.40 ms | 926.48 ms | 35897.67 | 
| python | [sanic](http://github.com/channelcat/sanic) (0.7) | 73.46 ms | 70.22 ms | 102.26 ms | 132.48 ms | 323.95 ms | 22737.67 | 
| python | [django](http://djangoproject.com) (2.0) | 91.11 ms | 85.32 ms | 106.65 ms | 273.70 ms | 1263.38 ms | 59886.00 | 
| python | [tornado](http://tornadoweb.org) (5.1) | 219.22 ms | 131.05 ms | 155.50 ms | 3025.25 ms | 5522.88 ms | 515396.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (actix-web) (rust)


:two: (fasthttprouter) (go)


:three: (evhtp) (cpp)


:four: (vibora) (python)


:five: (act) (java)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| rust | [actix-web](http://actix.rs) (0.7) | 167456.33 | 190.34 MB |
| go | [fasthttprouter](http://github.com/godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 163812.00 | 264.07 MB |
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 156528.33 | 151.85 MB |
| python | [vibora](http://vibora.io) (0.0) | 152735.00 | 173.33 MB |
| java | [act](http://actframework.org) (1.8) | 143542.67 | 245.13 MB |
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 139355.00 | 148.66 MB |
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (1.1) | 115616.00 | 188.34 MB |
| rust | [nickel](http://nickel.rs) (0.10) | 106479.67 | 211.49 MB |
| rust | [rocket](http://rocket.rs) (0.3) | 103703.67 | 163.65 MB |
| rust | [iron](http://ironframework.io) (0.7) | 95716.00 | 120.77 MB |
| go | [iris](http://iris-go.com) (10.7) | 94416.33 | 126.29 MB |
| go | [echo](http://echo.labstack.com) (3.3) | 91055.33 | 159.34 MB |
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 89927.67 | 157.69 MB |
| go | [gorilla-mux](http://gorillatoolkit.org/pkg/mux) (1.6) | 87375.00 | 116.63 MB |
| scala | [akkahttp](http://akka.io) (10.0) | 71847.33 | 154.06 MB |
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 70828.67 | 124.06 MB |
| swift | [perfect](http://perfect.org) (3.0) | 62600.00 | 58.72 MB |
| scala | [http4s](http://http4s.org) (0.0) | 58303.33 | 101.90 MB |
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 49139.00 | 46.11 MB |
| node | [polka](http://github.com/lukeed/polka) (0.4) | 48535.00 | 72.55 MB |
| node | [rayo](http://rayo.js.org) (1.2) | 48077.00 | 71.90 MB |
| crystal | [prism](http://github.com/vladfaust/prism) (0.4 (beta 3)) | 47668.33 | 52.91 MB |
| python | [bottle](http://bottlepy.org) (0.12) | 47643.33 | 117.15 MB |
| node | [fastify](http://fastify.io) (1.11) | 45910.67 | 112.09 MB |
| crystal | [amber](http://amberframework.org) (0.9) | 43956.67 | 63.74 MB |
| swift | [kitura](http://kitura.io) (2.2) | 43087.67 | 79.92 MB |
| crystal | [kemal](http://kemalcr.com) (0.24) | 42340.00 | 69.01 MB |
| swift | [vapor](http://vapor.codes) (3.0) | 41997.33 | 70.06 MB |
| crystal | [lucky](http://luckyframework.org) (0.11) | 40666.67 | 49.87 MB |
| node | [koa](http://koajs.com) (2.5) | 40346.33 | 85.19 MB |
| ruby | [roda](http://roda.jeremyevans.net) (3.11) | 39789.33 | 37.90 MB |
| node | [restify](http://restify.com) (7.2) | 36979.67 | 64.71 MB |
| node | [express](http://expressjs.com) (4.16) | 34919.67 | 85.29 MB |
| php | [symfony](http://symfony.com) (4.1) | 34618.67 | 171.70 MB |
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 31272.00 | 18.04 MB |
| php | [laravel](http://laravel.com) (5.7) | 29509.00 | 146.58 MB |
| node | [hapi](http://hapijs.com) (17.5) | 27952.00 | 72.19 MB |
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 27428.33 | 62.03 MB |
| ruby | [flame](http://github.com/AlexWayfer/flame) (5.0 (rc)) | 19499.00 | 11.25 MB |
| ruby | [hanami](http://hanamirb.org) (1.2) | 18557.67 | 140.29 MB |
| python | [flask](http://flask.pocoo.org) (1.0) | 18254.00 | 44.85 MB |
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 15715.33 | 40.77 MB |
| python | [sanic](http://github.com/channelcat/sanic) (0.7) | 13417.33 | 23.91 MB |
| python | [django](http://djangoproject.com) (2.0) | 11111.67 | 32.19 MB |
| python | [tornado](http://tornadoweb.org) (5.1) | 7309.00 | 19.55 MB |
| ruby | [rails](http://rubyonrails.org) (5.2) | 3952.33 | 12.10 MB |
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

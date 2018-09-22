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
Last update: 2018-09-22
```
OS: Linux (version: 4.18.8-200.fc28.x86_64, arch: x86_64)
CPU Cores: 4
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: rocket (rust)


:three: laravel (php)


:four: symfony (php)


:five: rack-routing (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust | [nickel](http://nickel.rs) (0.10) | 0.04 ms | 0.04 ms | 0.04 ms | 0.05 ms | 3.57 ms | 22.33 | 
| rust | [rocket](http://rocket.rs) (0.3) | 0.07 ms | 0.06 ms | 0.10 ms | 0.18 ms | 8.53 ms | 124.67 | 
| php | [laravel](http://laravel.com) (5.7) | 240.97 ms | 0.23 ms | 776.80 ms | 3210.26 ms | 7045.40 ms | 664731.00 | 
| php | [symfony](http://symfony.com) (4.1) | 153.51 ms | 0.25 ms | 470.28 ms | 2072.65 ms | 7028.63 ms | 475190.00 | 
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 2.13 ms | 0.26 ms | 5.66 ms | 29.53 ms | 89.33 ms | 5576.33 | 
| rust | [iron](http://ironframework.io) (0.7) | 0.33 ms | 0.27 ms | 0.51 ms | 1.12 ms | 54.27 ms | 641.00 | 
| ruby | [roda](http://roda.jeremyevans.net) (3.12) | 1.68 ms | 0.38 ms | 3.53 ms | 25.20 ms | 95.94 ms | 4822.00 | 
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 4.07 ms | 0.43 ms | 11.70 ms | 45.77 ms | 102.85 ms | 8709.67 | 
| ruby | [hanami](http://hanamirb.org) (1.2) | 3.40 ms | 0.50 ms | 8.88 ms | 45.08 ms | 115.52 ms | 8155.33 | 
| ruby | [flame](http://github.com/AlexWayfer/flame) (5.0 (rc)) | 3.25 ms | 0.98 ms | 8.13 ms | 37.32 ms | 91.97 ms | 6814.00 | 
| ruby | [rails](http://rubyonrails.org) (5.2) | 16.24 ms | 1.97 ms | 58.58 ms | 103.65 ms | 200.98 ms | 25748.00 | 
| rust | [actix-web](http://actix.rs) (0.7) | 5.15 ms | 4.72 ms | 9.28 ms | 14.49 ms | 29.93 ms | 2915.00 | 
| go | [fasthttprouter](http://github.com/godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.50 ms | 5.12 ms | 8.44 ms | 14.91 ms | 147.97 ms | 3577.67 | 
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 5.95 ms | 5.44 ms | 9.56 ms | 15.67 ms | 166.46 ms | 4255.67 | 
| java | [act](http://actframework.org) (1.8) | 7.22 ms | 5.72 ms | 11.64 ms | 29.01 ms | 197.47 ms | 7858.67 | 
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 7.09 ms | 6.22 ms | 11.61 ms | 20.22 ms | 68.93 ms | 4021.67 | 
| python | [vibora](http://vibora.io) (0.0) | 6.98 ms | 6.54 ms | 12.15 ms | 18.68 ms | 52.58 ms | 4252.67 | 
| scala | [akkahttp](http://akka.io) (10.0) | 203.59 ms | 7.37 ms | 20.58 ms | 5161.22 ms | 7934.56 ms | 858812.33 | 
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (1.1) | 9.45 ms | 7.73 ms | 11.20 ms | 31.34 ms | 453.68 ms | 18952.33 | 
| go | [iris](http://iris-go.com) (10.7) | 15.32 ms | 9.66 ms | 24.45 ms | 157.13 ms | 685.15 ms | 31389.33 | 
| go | [echo](http://echo.labstack.com) (3.3) | 15.75 ms | 10.62 ms | 28.04 ms | 145.44 ms | 375.07 ms | 25925.00 | 
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 17.49 ms | 10.64 ms | 29.53 ms | 172.66 ms | 577.19 ms | 33189.67 | 
| go | [gorilla-mux](http://gorillatoolkit.org/pkg/mux) (1.6) | 16.79 ms | 11.40 ms | 31.75 ms | 136.38 ms | 420.71 ms | 25698.67 | 
| go | [beego](http://beego.me) (1.10) | 17.91 ms | 11.47 ms | 30.43 ms | 173.08 ms | 474.71 ms | 31536.00 | 
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 29.45 ms | 12.02 ms | 68.55 ms | 270.64 ms | 460.40 ms | 54646.33 | 
| scala | [http4s](http://http4s.org) (0.0) | 54.41 ms | 15.09 ms | 28.64 ms | 1252.70 ms | 2602.58 ms | 221348.33 | 
| node | [rayo](http://rayo.js.org) (1.2) | 28.76 ms | 15.60 ms | 35.32 ms | 434.28 ms | 1494.86 ms | 82173.33 | 
| python | [bottle](http://bottlepy.org) (0.12) | 22.82 ms | 16.20 ms | 45.14 ms | 73.91 ms | 459.61 ms | 19743.33 | 
| node | [polka](http://github.com/lukeed/polka) (0.5) | 32.48 ms | 17.19 ms | 38.92 ms | 525.03 ms | 1572.03 ms | 93424.00 | 
| crystal | [prism](http://github.com/vladfaust/prism) (0.4 (beta 3)) | 18.72 ms | 17.43 ms | 25.59 ms | 26.34 ms | 28.25 ms | 3284.67 | 
| swift | [perfect](http://perfect.org) (3.0) | 17.56 ms | 17.63 ms | 19.62 ms | 24.20 ms | 159.16 ms | 3654.00 | 
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 19.14 ms | 17.95 ms | 23.36 ms | 23.93 ms | 92.97 ms | 3395.00 | 
| node | [restify](http://restify.com) (7.2) | 25.08 ms | 19.99 ms | 31.97 ms | 149.00 ms | 807.32 ms | 37485.00 | 
| crystal | [kemal](http://kemalcr.com) (0.24) | 21.45 ms | 20.01 ms | 27.12 ms | 32.31 ms | 507.46 ms | 12725.33 | 
| node | [fastify](http://fastify.io) (1.11) | 50.41 ms | 20.64 ms | 43.40 ms | 969.04 ms | 2091.74 ms | 159834.33 | 
| swift | [vapor](http://vapor.codes) (3.0) | 114.47 ms | 21.03 ms | 50.12 ms | 2605.17 ms | 4874.72 ms | 439198.00 | 
| node | [koa](http://koajs.com) (2.5) | 55.80 ms | 21.69 ms | 43.62 ms | 1123.34 ms | 2269.57 ms | 181888.00 | 
| crystal | [lucky](http://luckyframework.org) (0.11) | 22.44 ms | 22.20 ms | 26.93 ms | 27.64 ms | 96.95 ms | 3871.00 | 
| crystal | [amber](http://amberframework.org) (0.9) | 23.30 ms | 22.43 ms | 29.45 ms | 30.11 ms | 181.47 ms | 4819.00 | 
| node | [express](http://expressjs.com) (4.16) | 69.44 ms | 25.15 ms | 49.51 ms | 1365.68 ms | 2518.06 ms | 221555.33 | 
| swift | [kitura](http://kitura.io) (2.2) | 27.34 ms | 27.48 ms | 30.73 ms | 37.78 ms | 236.54 ms | 5694.00 | 
| node | [hapi](http://hapijs.com) (17.5) | 89.76 ms | 29.17 ms | 58.19 ms | 1747.56 ms | 3220.65 ms | 290123.33 | 
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 36.75 ms | 36.87 ms | 43.68 ms | 53.80 ms | 515.75 ms | 17737.67 | 
| python | [flask](http://flask.pocoo.org) (1.0) | 54.67 ms | 47.71 ms | 77.82 ms | 149.15 ms | 683.62 ms | 36342.00 | 
| python | [sanic](http://github.com/channelcat/sanic) (0.8) | 75.91 ms | 67.88 ms | 119.00 ms | 154.08 ms | 357.53 ms | 31782.33 | 
| python | [django](http://djangoproject.com) (2.1) | 87.76 ms | 84.88 ms | 107.91 ms | 192.27 ms | 997.00 ms | 44345.33 | 
| python | [tornado](http://tornadoweb.org) (5.1) | 201.81 ms | 129.40 ms | 160.32 ms | 2817.13 ms | 4339.23 ms | 426745.33 | 

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
| rust | [actix-web](http://actix.rs) (0.7) | 173129.67 | 196.67 MB |
| go | [fasthttprouter](http://github.com/godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 164520.33 | 265.04 MB |
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 157672.67 | 153.04 MB |
| python | [vibora](http://vibora.io) (0.0) | 151328.00 | 171.77 MB |
| java | [act](http://actframework.org) (1.8) | 144782.00 | 247.41 MB |
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 138114.33 | 147.63 MB |
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (1.1) | 117882.67 | 192.11 MB |
| rust | [nickel](http://nickel.rs) (0.10) | 116725.00 | 231.47 MB |
| rust | [rocket](http://rocket.rs) (0.3) | 107713.67 | 169.29 MB |
| rust | [iron](http://ironframework.io) (0.7) | 96094.67 | 121.15 MB |
| go | [iris](http://iris-go.com) (10.7) | 96062.00 | 128.16 MB |
| go | [echo](http://echo.labstack.com) (3.3) | 90728.00 | 159.04 MB |
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 90442.00 | 158.55 MB |
| go | [beego](http://beego.me) (1.10) | 88055.67 | 118.42 MB |
| go | [gorilla-mux](http://gorillatoolkit.org/pkg/mux) (1.6) | 85391.00 | 114.08 MB |
| scala | [akkahttp](http://akka.io) (10.0) | 75342.33 | 161.37 MB |
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 71712.33 | 125.79 MB |
| scala | [http4s](http://http4s.org) (0.0) | 60403.33 | 105.67 MB |
| swift | [perfect](http://perfect.org) (3.0) | 56230.33 | 52.78 MB |
| crystal | [prism](http://github.com/vladfaust/prism) (0.4 (beta 3)) | 53143.00 | 59.98 MB |
| node | [rayo](http://rayo.js.org) (1.2) | 52862.33 | 79.04 MB |
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 51818.67 | 48.63 MB |
| node | [polka](http://github.com/lukeed/polka) (0.5) | 48693.67 | 72.74 MB |
| python | [bottle](http://bottlepy.org) (0.12) | 48067.67 | 118.14 MB |
| crystal | [kemal](http://kemalcr.com) (0.24) | 46851.67 | 76.34 MB |
| crystal | [lucky](http://luckyframework.org) (0.11) | 44612.67 | 54.92 MB |
| node | [restify](http://restify.com) (7.2) | 44533.33 | 77.93 MB |
| node | [fastify](http://fastify.io) (1.11) | 42770.00 | 104.61 MB |
| crystal | [amber](http://amberframework.org) (0.9) | 42456.67 | 61.57 MB |
| node | [koa](http://koajs.com) (2.5) | 40398.33 | 85.32 MB |
| ruby | [roda](http://roda.jeremyevans.net) (3.12) | 39703.67 | 37.82 MB |
| swift | [vapor](http://vapor.codes) (3.0) | 39638.67 | 66.16 MB |
| swift | [kitura](http://kitura.io) (2.2) | 36505.33 | 67.72 MB |
| node | [express](http://expressjs.com) (4.16) | 34878.67 | 85.14 MB |
| php | [symfony](http://symfony.com) (4.1) | 33078.00 | 164.02 MB |
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 30304.00 | 17.47 MB |
| node | [hapi](http://hapijs.com) (17.5) | 30280.67 | 78.25 MB |
| php | [laravel](http://laravel.com) (5.7) | 30145.00 | 149.84 MB |
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 27198.33 | 61.52 MB |
| ruby | [flame](http://github.com/AlexWayfer/flame) (5.0 (rc)) | 19770.67 | 11.39 MB |
| ruby | [hanami](http://hanamirb.org) (1.2) | 18833.67 | 142.41 MB |
| python | [flask](http://flask.pocoo.org) (1.0) | 18438.33 | 45.35 MB |
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 15698.00 | 40.73 MB |
| python | [sanic](http://github.com/channelcat/sanic) (0.8) | 13035.67 | 23.22 MB |
| python | [django](http://djangoproject.com) (2.1) | 11207.67 | 32.46 MB |
| python | [tornado](http://tornadoweb.org) (5.1) | 7251.00 | 19.42 MB |
| ruby | [rails](http://rubyonrails.org) (5.2) | 3936.00 | 12.04 MB |
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

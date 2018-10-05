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
Last update: 2018-09-26
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
| rust | [nickel](http://nickel.rs) (0.10) | 0.11 ms | 0.11 ms | 0.14 ms | 0.31 ms | 6.11 ms | 107.33 | 
| rust | [rocket](http://nickel-org.github.io) (0.3) | 0.15 ms | 0.12 ms | 0.21 ms | 0.77 ms | 9.61 ms | 172.33 | 
| rust | [iron](http://ironframework.io) (0.7) | 0.61 ms | 0.44 ms | 1.14 ms | 3.61 ms | 33.47 ms | 779.33 | 
| ruby | [roda](http://roda.jeremyevans.net) (3.12) | 3.13 ms | 1.97 ms | 6.67 ms | 20.57 ms | 101.83 ms | 4197.67 | 
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.25 ms | 2.35 ms | 9.98 ms | 30.24 ms | 118.29 ms | 6137.67 | 
| ruby | [flame](http://github.com/AlexWayfer/flame) (5.0 (rc)) | 6.61 ms | 3.38 ms | 16.39 ms | 45.91 ms | 142.71 ms | 9371.00 | 
| php | [symfony](http://symfony.com) (4.1) | 193.15 ms | 3.66 ms | 529.87 ms | 3505.07 ms | 5903.08 ms | 614762.67 | 
| ruby | [hanami](http://hanamirb.org) (1.2) | 7.06 ms | 4.18 ms | 17.10 ms | 40.80 ms | 133.92 ms | 8531.33 | 
| php | [laravel](http://laravel.com) (5.7) | 243.99 ms | 4.42 ms | 587.04 ms | 3831.52 ms | 6494.07 ms | 735153.33 | 
| rust | [actix-web](http://actix.rs) (0.7) | 5.49 ms | 4.63 ms | 9.95 ms | 17.41 ms | 72.62 ms | 3552.67 | 
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 5.75 ms | 4.98 ms | 9.77 ms | 16.79 ms | 50.77 ms | 3241.67 | 
| go | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.82 ms | 5.12 ms | 9.05 ms | 17.38 ms | 59.67 ms | 3057.00 | 
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 8.00 ms | 5.35 ms | 18.65 ms | 38.34 ms | 101.76 ms | 8268.67 | 
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 6.75 ms | 5.76 ms | 11.86 ms | 20.46 ms | 49.83 ms | 4053.00 | 
| python | [vibora](http://vibora.io) (0.0) | 7.07 ms | 6.09 ms | 13.40 ms | 22.99 ms | 56.87 ms | 4878.33 | 
| ruby | [rails](http://rubyonrails.org) (5.2) | 32.30 ms | 6.81 ms | 106.49 ms | 200.71 ms | 403.26 ms | 48330.67 | 
| java | [act](http://actframework.org) (1.8) | 8.46 ms | 7.47 ms | 12.24 ms | 21.84 ms | 221.57 ms | 6457.33 | 
| scala | [akkahttp](http://akka.io) (10.0) | 200.01 ms | 8.55 ms | 56.12 ms | 4629.18 ms | 7916.37 ms | 811798.67 | 
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (1.1) | 10.21 ms | 8.56 ms | 15.53 ms | 24.99 ms | 443.51 ms | 12445.67 | 
| go | [iris](http://iris-go.com) (10.7) | 9.93 ms | 8.83 ms | 15.28 ms | 29.35 ms | 126.24 ms | 5407.00 | 
| go | [echo](http://echo.labstack.com) (3.3) | 10.74 ms | 9.33 ms | 16.63 ms | 32.51 ms | 302.63 ms | 8021.67 | 
| go | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 12.25 ms | 9.89 ms | 18.18 ms | 42.86 ms | 503.78 ms | 17208.67 | 
| go | [beego](http://beego.me) (1.10) | 11.38 ms | 9.90 ms | 17.58 ms | 35.35 ms | 247.57 ms | 7367.33 | 
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 12.25 ms | 10.42 ms | 19.55 ms | 40.49 ms | 200.37 ms | 7905.67 | 
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 30.86 ms | 10.59 ms | 63.18 ms | 321.40 ms | 533.90 ms | 63827.33 | 
| node | [rayo](http://rayo.js.org) (1.2) | 20.34 ms | 13.12 ms | 26.52 ms | 204.43 ms | 895.01 ms | 46446.33 | 
| python | [bottle](http://bottlepy.org) (0.12) | 19.87 ms | 15.22 ms | 37.02 ms | 69.42 ms | 234.92 ms | 13711.00 | 
| swift | [perfect](http://perfect.org) (3.0) | 15.98 ms | 16.26 ms | 18.37 ms | 20.93 ms | 172.87 ms | 2840.67 | 
| node | [polka](http://github.com/lukeed/polka) (0.5) | 26.63 ms | 16.45 ms | 40.65 ms | 243.42 ms | 1039.90 ms | 53737.00 | 
| swift | [vapor](http://vapor.codes) (3.0) | 49.25 ms | 17.01 ms | 33.04 ms | 1072.45 ms | 2731.84 ms | 195691.33 | 
| node | [fastify](http://fastify.io) (1.12) | 30.08 ms | 17.54 ms | 39.31 ms | 383.16 ms | 1221.57 ms | 70694.00 | 
| scala | [http4s](http://http4s.org) (0.0) | 20.94 ms | 17.60 ms | 33.51 ms | 63.95 ms | 1029.10 ms | 33535.00 | 
| node | [koa](http://koajs.com) (2.5) | 30.37 ms | 18.78 ms | 41.02 ms | 342.33 ms | 1163.02 ms | 65382.00 | 
| node | [restify](http://restify.com) (7.2) | 29.44 ms | 20.75 ms | 41.88 ms | 206.14 ms | 904.40 ms | 46636.00 | 
| node | [express](http://expressjs.com) (4.16) | 40.30 ms | 23.63 ms | 52.76 ms | 519.06 ms | 1453.65 ms | 89511.33 | 
| swift | [kitura](http://kitura.io) (2.2) | 27.91 ms | 26.30 ms | 31.60 ms | 60.81 ms | 606.36 ms | 23865.00 | 
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 30.23 ms | 29.43 ms | 38.92 ms | 47.51 ms | 172.49 ms | 7173.00 | 
| node | [hapi](http://hapijs.com) (17.6) | 52.08 ms | 31.16 ms | 63.42 ms | 684.80 ms | 1642.94 ms | 112586.33 | 
| crystal | [prism](http://github.com/vladfaust/prism) (0.4 (beta 3)) | 33.90 ms | 34.41 ms | 44.38 ms | 52.81 ms | 200.39 ms | 10100.33 | 
| crystal | [amber](http://amberframework.org) (0.9) | 37.66 ms | 35.73 ms | 49.36 ms | 57.42 ms | 428.87 ms | 13444.67 | 
| python | [flask](http://flask.pocoo.org) (1.0) | 49.75 ms | 38.26 ms | 92.93 ms | 153.46 ms | 378.50 ms | 29313.67 | 
| crystal | [lucky](http://luckyframework.org) (0.11) | 37.14 ms | 39.02 ms | 46.47 ms | 54.67 ms | 314.89 ms | 11330.00 | 
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 44.27 ms | 39.84 ms | 74.84 ms | 131.18 ms | 243.94 ms | 25171.67 | 
| crystal | [kemal](http://kemalcr.com) (0.24) | 40.64 ms | 40.57 ms | 48.61 ms | 55.71 ms | 246.18 ms | 7641.33 | 
| python | [sanic](http://github.com/huge-success/sanic) (0.8) | 57.43 ms | 52.83 ms | 96.86 ms | 153.26 ms | 357.19 ms | 30868.00 | 
| python | [django](http://djangoproject.com) (2.1) | 89.48 ms | 77.49 ms | 149.53 ms | 218.44 ms | 579.93 ms | 39797.67 | 
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 124.86 ms | 113.69 ms | 212.34 ms | 305.45 ms | 458.51 ms | 62528.67 | 
| python | [tornado](http://tornadoweb.org) (5.1) | 138.32 ms | 127.17 ms | 204.02 ms | 364.26 ms | 1543.67 ms | 73404.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (actix-web) (rust)


:two: (evhtp) (cpp)


:three: (fasthttprouter) (go)


:four: (vibora) (python)


:five: (spider-gazelle) (crystal)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| rust | [actix-web](http://actix.rs) (0.7) | 171486.67 | 194.97 MB |
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 162704.33 | 157.98 MB |
| go | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 160546.00 | 258.91 MB |
| python | [vibora](http://vibora.io) (0.0) | 149919.33 | 170.18 MB |
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 144040.00 | 154.28 MB |
| java | [act](http://actframework.org) (1.8) | 130173.33 | 222.51 MB |
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (1.1) | 100602.33 | 163.99 MB |
| go | [iris](http://iris-go.com) (10.7) | 98915.33 | 132.66 MB |
| rust | [iron](http://ironframework.io) (0.7) | 96775.67 | 121.28 MB |
| go | [echo](http://echo.labstack.com) (3.3) | 92747.33 | 162.76 MB |
| go | [beego](http://beego.me) (1.10) | 88631.00 | 119.64 MB |
| go | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 87529.00 | 116.90 MB |
| rust | [rocket](http://nickel-org.github.io) (0.3) | 85524.00 | 132.78 MB |
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 82760.67 | 145.18 MB |
| rust | [nickel](http://nickel.rs) (0.10) | 76027.00 | 150.82 MB |
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 75525.00 | 132.54 MB |
| scala | [akkahttp](http://akka.io) (10.0) | 67689.00 | 145.44 MB |
| node | [rayo](http://rayo.js.org) (1.2) | 64413.33 | 96.58 MB |
| swift | [perfect](http://perfect.org) (3.0) | 60628.67 | 57.05 MB |
| python | [bottle](http://bottlepy.org) (0.12) | 52951.00 | 130.52 MB |
| scala | [http4s](http://http4s.org) (0.0) | 52873.67 | 92.61 MB |
| swift | [vapor](http://vapor.codes) (3.0) | 50517.00 | 86.07 MB |
| node | [polka](http://github.com/lukeed/polka) (0.5) | 49454.33 | 74.09 MB |
| node | [fastify](http://fastify.io) (1.12) | 48239.33 | 117.69 MB |
| php | [symfony](http://symfony.com) (4.1) | 46942.00 | 233.67 MB |
| node | [koa](http://koajs.com) (2.5) | 43786.33 | 92.66 MB |
| ruby | [roda](http://roda.jeremyevans.net) (3.12) | 40776.00 | 38.95 MB |
| node | [restify](http://restify.com) (7.2) | 40653.67 | 71.39 MB |
| php | [laravel](http://laravel.com) (5.7) | 39830.67 | 198.41 MB |
| swift | [kitura](http://kitura.io) (2.2) | 36869.33 | 68.42 MB |
| node | [express](http://expressjs.com) (4.16) | 35080.33 | 85.90 MB |
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 33040.67 | 30.97 MB |
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 30292.67 | 17.51 MB |
| crystal | [prism](http://github.com/vladfaust/prism) (0.4 (beta 3)) | 29712.33 | 33.01 MB |
| node | [hapi](http://hapijs.com) (17.6) | 28313.33 | 73.53 MB |
| crystal | [lucky](http://luckyframework.org) (0.11) | 26824.00 | 32.87 MB |
| crystal | [amber](http://amberframework.org) (0.9) | 26487.00 | 38.37 MB |
| crystal | [kemal](http://kemalcr.com) (0.24) | 24735.33 | 40.28 MB |
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 23306.00 | 52.82 MB |
| python | [flask](http://flask.pocoo.org) (1.0) | 20520.67 | 50.59 MB |
| ruby | [flame](http://github.com/AlexWayfer/flame) (5.0 (rc)) | 19625.67 | 11.32 MB |
| ruby | [hanami](http://hanamirb.org) (1.2) | 18047.33 | 136.80 MB |
| python | [sanic](http://github.com/huge-success/sanic) (0.8) | 17514.33 | 31.17 MB |
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 15898.00 | 41.33 MB |
| python | [django](http://djangoproject.com) (2.1) | 11039.67 | 31.98 MB |
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 7988.67 | 15.93 MB |
| python | [tornado](http://tornadoweb.org) (5.1) | 7098.67 | 18.95 MB |
| ruby | [rails](http://rubyonrails.org) (5.2) | 3962.33 | 12.16 MB |
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

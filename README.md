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


:one: nickel (rust)


:two: rocket (rust)


:three: iron (rust)


:four: roda (ruby)


:five: rack-routing (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust | [nickel](http://nickel.rs) (0.10) | 0.11 ms | 0.10 ms | 0.13 ms | 0.42 ms | 7.99 ms | 121.67 | 
| rust | [rocket](http://rocket.rs) (0.3) | 0.16 ms | 0.12 ms | 0.21 ms | 1.09 ms | 10.73 ms | 231.67 | 
| rust | [iron](http://ironframework.io) (0.7) | 0.59 ms | 0.47 ms | 0.99 ms | 3.07 ms | 28.63 ms | 664.67 | 
| ruby | [roda](http://roda.jeremyevans.net) (3.12) | 2.66 ms | 1.91 ms | 5.27 ms | 14.90 ms | 95.42 ms | 3078.67 | 
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.69 ms | 2.50 ms | 8.14 ms | 19.61 ms | 96.18 ms | 3996.00 | 
| ruby | [hanami](http://hanamirb.org) (1.2) | 6.50 ms | 3.62 ms | 16.03 ms | 39.12 ms | 107.80 ms | 8149.00 | 
| ruby | [flame](http://github.com/AlexWayfer/flame) (5.0 (rc)) | 5.81 ms | 4.14 ms | 12.15 ms | 31.56 ms | 139.49 ms | 6538.00 | 
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 7.80 ms | 4.56 ms | 18.99 ms | 43.45 ms | 111.87 ms | 9169.67 | 
| rust | [actix-web](http://actix.rs) (0.7) | 5.47 ms | 4.66 ms | 9.84 ms | 16.27 ms | 71.86 ms | 3378.67 | 
| php | [symfony](http://symfony.com) (4.1) | 186.46 ms | 4.89 ms | 488.31 ms | 3027.86 ms | 6731.12 ms | 579195.00 | 
| php | [laravel](http://laravel.com) (5.7) | 289.80 ms | 5.12 ms | 536.73 ms | 4998.33 ms | 6514.35 ms | 939221.67 | 
| go | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.95 ms | 5.17 ms | 9.10 ms | 18.06 ms | 216.26 ms | 5263.00 | 
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 6.15 ms | 5.28 ms | 10.63 ms | 18.16 ms | 66.59 ms | 3590.00 | 
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 6.70 ms | 5.69 ms | 11.83 ms | 20.05 ms | 43.92 ms | 3978.67 | 
| python | [vibora](http://vibora.io) (0.0) | 6.51 ms | 5.79 ms | 12.17 ms | 19.73 ms | 38.93 ms | 4229.00 | 
| ruby | [rails](http://rubyonrails.org) (5.2) | 27.72 ms | 6.52 ms | 90.27 ms | 173.35 ms | 338.50 ms | 41258.67 | 
| java | [act](http://actframework.org) (1.8) | 8.92 ms | 7.69 ms | 13.42 ms | 24.99 ms | 284.65 ms | 8211.00 | 
| scala | [akkahttp](http://akka.io) (10.0) | 198.28 ms | 8.47 ms | 26.09 ms | 4665.98 ms | 7929.28 ms | 818263.00 | 
| go | [iris](http://iris-go.com) (10.7) | 9.94 ms | 8.79 ms | 15.09 ms | 29.32 ms | 224.07 ms | 6983.67 | 
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (1.1) | 10.82 ms | 9.11 ms | 17.22 ms | 28.77 ms | 377.37 ms | 10867.33 | 
| go | [beego](http://beego.me) (1.10) | 10.55 ms | 9.35 ms | 16.27 ms | 30.06 ms | 219.73 ms | 5871.67 | 
| go | [gorilla-mux](http://gorillatoolkit.org/pkg/mux) (1.6) | 10.92 ms | 9.59 ms | 16.96 ms | 32.83 ms | 227.14 ms | 6955.67 | 
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 11.65 ms | 10.02 ms | 18.26 ms | 35.51 ms | 199.99 ms | 8128.33 | 
| go | [echo](http://echo.labstack.com) (3.3) | 11.90 ms | 10.38 ms | 18.82 ms | 36.14 ms | 100.76 ms | 6350.00 | 
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 31.17 ms | 10.55 ms | 65.44 ms | 331.51 ms | 557.75 ms | 65110.00 | 
| node | [rayo](http://rayo.js.org) (1.2) | 22.21 ms | 14.29 ms | 33.14 ms | 174.62 ms | 874.94 ms | 43559.67 | 
| python | [bottle](http://bottlepy.org) (0.12) | 19.90 ms | 15.35 ms | 35.74 ms | 76.19 ms | 513.71 ms | 19261.67 | 
| node | [polka](http://github.com/lukeed/polka) (0.5) | 23.77 ms | 15.68 ms | 38.44 ms | 148.28 ms | 850.12 ms | 40476.67 | 
| node | [fastify](http://fastify.io) (1.11) | 25.42 ms | 15.75 ms | 32.32 ms | 270.54 ms | 1021.85 ms | 56256.00 | 
| swift | [vapor](http://vapor.codes) (3.0) | 38.98 ms | 17.28 ms | 34.48 ms | 799.79 ms | 2263.98 ms | 143395.67 | 
| swift | [perfect](http://perfect.org) (3.0) | 17.40 ms | 17.41 ms | 19.84 ms | 22.85 ms | 112.82 ms | 2479.67 | 
| node | [koa](http://koajs.com) (2.5) | 32.39 ms | 18.36 ms | 40.91 ms | 425.86 ms | 1270.23 ms | 76741.00 | 
| scala | [http4s](http://http4s.org) (0.0) | 22.63 ms | 19.81 ms | 39.29 ms | 70.33 ms | 598.66 ms | 21371.33 | 
| node | [restify](http://restify.com) (7.2) | 27.29 ms | 21.19 ms | 43.21 ms | 97.46 ms | 629.61 ms | 26415.67 | 
| node | [express](http://expressjs.com) (4.16) | 38.36 ms | 22.92 ms | 51.24 ms | 464.09 ms | 1411.38 ms | 84826.00 | 
| swift | [kitura](http://kitura.io) (2.2) | 25.24 ms | 25.43 ms | 27.93 ms | 31.28 ms | 457.13 ms | 10895.67 | 
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 31.11 ms | 27.76 ms | 41.04 ms | 51.70 ms | 414.63 ms | 16671.67 | 
| crystal | [prism](http://github.com/vladfaust/prism) (0.4 (beta 3)) | 33.43 ms | 30.40 ms | 41.84 ms | 52.39 ms | 445.70 ms | 16767.00 | 
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 36.09 ms | 32.27 ms | 56.60 ms | 85.25 ms | 304.24 ms | 17077.00 | 
| crystal | [amber](http://amberframework.org) (0.9) | 37.35 ms | 33.45 ms | 51.96 ms | 61.39 ms | 486.85 ms | 19922.00 | 
| node | [hapi](http://hapijs.com) (17.5) | 73.20 ms | 33.78 ms | 71.03 ms | 1179.60 ms | 2307.41 ms | 193492.00 | 
| python | [flask](http://flask.pocoo.org) (1.0) | 47.60 ms | 35.39 ms | 104.74 ms | 129.44 ms | 574.37 ms | 33024.33 | 
| crystal | [lucky](http://luckyframework.org) (0.11) | 36.46 ms | 36.21 ms | 46.98 ms | 55.26 ms | 243.13 ms | 9176.00 | 
| crystal | [kemal](http://kemalcr.com) (0.24) | 44.67 ms | 41.36 ms | 49.47 ms | 213.11 ms | 779.72 ms | 42889.67 | 
| python | [sanic](http://github.com/channelcat/sanic) (0.8) | 53.69 ms | 48.82 ms | 84.71 ms | 131.85 ms | 398.12 ms | 25132.00 | 
| python | [django](http://djangoproject.com) (2.1) | 81.90 ms | 65.04 ms | 166.10 ms | 201.43 ms | 483.58 ms | 44104.33 | 
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 113.09 ms | 101.81 ms | 182.01 ms | 285.10 ms | 695.87 ms | 53916.00 | 
| python | [tornado](http://tornadoweb.org) (5.1) | 112.65 ms | 110.29 ms | 133.62 ms | 229.30 ms | 1070.84 ms | 45193.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (actix-web) (rust)


:two: (fasthttprouter) (go)


:three: (vibora) (python)


:four: (evhtp) (cpp)


:five: (spider-gazelle) (crystal)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| rust | [actix-web](http://actix.rs) (0.7) | 171628.67 | 195.18 MB |
| go | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 160512.33 | 261.32 MB |
| python | [vibora](http://vibora.io) (0.0) | 159620.33 | 181.25 MB |
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 155354.00 | 150.82 MB |
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 145037.33 | 155.39 MB |
| java | [act](http://actframework.org) (1.8) | 123011.67 | 210.43 MB |
| rust | [iron](http://ironframework.io) (0.7) | 100344.33 | 126.33 MB |
| go | [iris](http://iris-go.com) (10.7) | 99748.67 | 133.73 MB |
| go | [beego](http://beego.me) (1.10) | 93389.67 | 125.70 MB |
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (1.1) | 92719.67 | 151.09 MB |
| go | [gorilla-mux](http://gorillatoolkit.org/pkg/mux) (1.6) | 91320.00 | 122.19 MB |
| rust | [rocket](http://rocket.rs) (0.3) | 86359.00 | 135.86 MB |
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 86029.00 | 150.99 MB |
| go | [echo](http://echo.labstack.com) (3.3) | 84317.67 | 147.88 MB |
| rust | [nickel](http://nickel.rs) (0.10) | 80443.67 | 160.31 MB |
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 76102.67 | 133.62 MB |
| php | [symfony](http://symfony.com) (4.1) | 70562.00 | 351.23 MB |
| scala | [akkahttp](http://akka.io) (10.0) | 63528.67 | 136.45 MB |
| php | [laravel](http://laravel.com) (5.7) | 63035.33 | 313.98 MB |
| node | [rayo](http://rayo.js.org) (1.2) | 56896.33 | 85.14 MB |
| swift | [perfect](http://perfect.org) (3.0) | 56619.67 | 53.18 MB |
| python | [bottle](http://bottlepy.org) (0.12) | 54803.67 | 134.87 MB |
| node | [fastify](http://fastify.io) (1.11) | 54675.00 | 132.68 MB |
| node | [polka](http://github.com/lukeed/polka) (0.5) | 51235.33 | 76.54 MB |
| swift | [vapor](http://vapor.codes) (3.0) | 48444.67 | 82.06 MB |
| ruby | [roda](http://roda.jeremyevans.net) (3.12) | 47365.00 | 45.22 MB |
| scala | [http4s](http://http4s.org) (0.0) | 46434.67 | 81.38 MB |
| node | [koa](http://koajs.com) (2.5) | 44708.00 | 94.59 MB |
| node | [restify](http://restify.com) (7.2) | 39195.33 | 68.82 MB |
| swift | [kitura](http://kitura.io) (2.2) | 39027.67 | 72.42 MB |
| node | [express](http://expressjs.com) (4.16) | 35966.33 | 88.13 MB |
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 34410.33 | 19.89 MB |
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 32430.67 | 30.41 MB |
| crystal | [prism](http://github.com/vladfaust/prism) (0.4 (beta 3)) | 30709.33 | 34.01 MB |
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 27885.00 | 63.15 MB |
| crystal | [lucky](http://luckyframework.org) (0.11) | 27194.67 | 33.46 MB |
| crystal | [amber](http://amberframework.org) (0.9) | 26615.67 | 38.57 MB |
| node | [hapi](http://hapijs.com) (17.5) | 26287.33 | 68.12 MB |
| crystal | [kemal](http://kemalcr.com) (0.24) | 24433.00 | 39.77 MB |
| ruby | [flame](http://github.com/AlexWayfer/flame) (5.0 (rc)) | 22215.67 | 12.84 MB |
| python | [flask](http://flask.pocoo.org) (1.0) | 22024.67 | 54.29 MB |
| ruby | [hanami](http://hanamirb.org) (1.2) | 19677.00 | 149.02 MB |
| python | [sanic](http://github.com/channelcat/sanic) (0.8) | 18570.33 | 33.04 MB |
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 16375.67 | 42.54 MB |
| python | [django](http://djangoproject.com) (2.1) | 12420.67 | 35.94 MB |
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 8732.00 | 17.39 MB |
| python | [tornado](http://tornadoweb.org) (5.1) | 8720.33 | 23.35 MB |
| ruby | [rails](http://rubyonrails.org) (5.2) | 4624.33 | 14.09 MB |
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

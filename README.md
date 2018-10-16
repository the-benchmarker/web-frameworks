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
Last update: 2018-10-16
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
| rust | [nickel](http://nickel-org.github.io) (0.10) | 0.09 ms | 0.09 ms | 0.12 ms | 0.18 ms | 5.41 ms | 64.00 | 
| rust | [rocket](http://rocket.rs) (0.3) | 0.13 ms | 0.12 ms | 0.18 ms | 0.26 ms | 7.54 ms | 109.00 | 
| rust | [iron](http://ironframework.io) (0.6) | 0.54 ms | 0.44 ms | 0.89 ms | 3.19 ms | 28.94 ms | 612.67 | 
| ruby | [roda](http://roda.jeremyevans.net) (3.13) | 2.52 ms | 1.86 ms | 5.02 ms | 13.45 ms | 81.80 ms | 2786.67 | 
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.42 ms | 2.39 ms | 7.46 ms | 16.73 ms | 83.19 ms | 3449.00 | 
| ruby | [hanami](http://hanamirb.org) (1.2) | 6.10 ms | 4.18 ms | 13.96 ms | 29.60 ms | 89.29 ms | 6295.67 | 
| ruby | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.84 ms | 4.24 ms | 12.61 ms | 25.86 ms | 128.38 ms | 5540.67 | 
| rust | [actix-web](http://actix.rs) (0.7) | 5.00 ms | 4.44 ms | 8.88 ms | 14.61 ms | 49.79 ms | 3073.00 | 
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 7.16 ms | 4.47 ms | 17.10 ms | 37.51 ms | 119.51 ms | 8016.00 | 
| go | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.26 ms | 4.66 ms | 8.43 ms | 15.12 ms | 45.01 ms | 2682.00 | 
| php | [slim](http://slimframework.com) (3.11) | 142.76 ms | 4.77 ms | 399.37 ms | 2128.91 ms | 5784.17 ms | 463478.00 | 
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 5.67 ms | 5.00 ms | 9.45 ms | 15.92 ms | 50.18 ms | 3126.33 | 
| php | [laravel](http://laravel.com) (5.7) | 248.83 ms | 5.30 ms | 321.13 ms | 4564.89 ms | 6634.85 ms | 840777.67 | 
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 6.44 ms | 5.43 ms | 11.45 ms | 19.49 ms | 50.06 ms | 3902.33 | 
| python | [vibora](http://vibora.io) (0.0) | 6.21 ms | 5.58 ms | 11.54 ms | 18.46 ms | 43.34 ms | 4006.67 | 
| php | [symfony](http://symfony.com) (4.1) | 65.53 ms | 5.68 ms | 87.29 ms | 1388.27 ms | 5164.82 ms | 271366.00 | 
| nim | [jester](http://github.com/dom96/jester) (0.4) | 7.60 ms | 6.72 ms | 12.30 ms | 20.44 ms | 51.91 ms | 3835.33 | 
| ruby | [rails](http://rubyonrails.org) (5.2) | 26.25 ms | 7.39 ms | 81.74 ms | 165.87 ms | 358.84 ms | 38302.67 | 
| go | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 8.53 ms | 7.79 ms | 13.05 ms | 23.75 ms | 116.92 ms | 4195.67 | 
| java | [act](http://actframework.org) (1.8) | 8.95 ms | 7.99 ms | 13.48 ms | 23.04 ms | 291.62 ms | 8445.67 | 
| go | [iris](http://iris-go.com) (10.7) | 9.55 ms | 8.27 ms | 14.37 ms | 28.02 ms | 331.15 ms | 9782.33 | 
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 9.78 ms | 8.65 ms | 15.40 ms | 29.86 ms | 115.94 ms | 5582.33 | 
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 10.91 ms | 8.79 ms | 14.66 ms | 32.55 ms | 578.74 ms | 21083.67 | 
| go | [beego](http://beego.me) (1.10) | 10.34 ms | 8.93 ms | 15.81 ms | 31.36 ms | 254.10 ms | 9471.00 | 
| go | [echo](http://echo.labstack.com) (3.3) | 10.29 ms | 9.01 ms | 16.49 ms | 31.65 ms | 148.12 ms | 5655.67 | 
| scala | [akkahttp](http://akka.io) (10.1) | 211.25 ms | 9.11 ms | 217.49 ms | 4242.17 ms | 7067.23 ms | 760227.00 | 
| go | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 10.47 ms | 9.18 ms | 16.66 ms | 32.97 ms | 185.10 ms | 6631.67 | 
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 29.70 ms | 10.34 ms | 80.37 ms | 283.50 ms | 505.67 ms | 57720.00 | 
| node | [rayo](http://rayo.js.org) (1.2) | 19.99 ms | 12.69 ms | 31.40 ms | 154.76 ms | 749.63 ms | 36213.00 | 
| node | [polka](http://github.com/lukeed/polka) (0.5) | 19.40 ms | 13.00 ms | 30.50 ms | 136.50 ms | 665.20 ms | 32043.33 | 
| python | [bottle](http://bottlepy.org) (0.12) | 18.51 ms | 14.61 ms | 33.26 ms | 65.07 ms | 220.91 ms | 12884.33 | 
| node | [fastify](http://fastify.io) (1.12) | 24.84 ms | 15.64 ms | 36.26 ms | 224.60 ms | 978.93 ms | 49809.00 | 
| node | [koa](http://koajs.com) (2.5) | 30.10 ms | 15.91 ms | 35.92 ms | 456.57 ms | 1304.65 ms | 80387.67 | 
| swift | [vapor](http://vapor.codes) (3.0) | 27.23 ms | 16.88 ms | 29.53 ms | 383.93 ms | 1396.68 ms | 77265.67 | 
| swift | [perfect](http://perfect.org) (3.0) | 17.44 ms | 17.48 ms | 20.05 ms | 23.62 ms | 145.93 ms | 2585.00 | 
| scala | [http4s](http://http4s.org) (0.18) | 20.75 ms | 18.14 ms | 34.45 ms | 59.81 ms | 825.00 ms | 26602.00 | 
| node | [express](http://expressjs.com) (4.16) | 33.45 ms | 21.22 ms | 49.14 ms | 300.54 ms | 1163.51 ms | 63065.00 | 
| node | [restify](http://restify.com) (7.2) | 29.26 ms | 21.36 ms | 46.62 ms | 134.73 ms | 700.15 ms | 34150.67 | 
| swift | [kitura](http://kitura.io) (2.2) | 26.32 ms | 25.98 ms | 30.44 ms | 37.69 ms | 528.62 ms | 11838.33 | 
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 31.02 ms | 27.14 ms | 39.39 ms | 54.52 ms | 767.68 ms | 28095.00 | 
| node | [hapi](http://hapijs.com) (17.6) | 50.55 ms | 28.62 ms | 60.72 ms | 702.17 ms | 1652.77 ms | 116182.33 | 
| crystal | [amber](http://amberframework.org) (0.9) | 33.87 ms | 30.20 ms | 46.80 ms | 57.21 ms | 213.93 ms | 10738.00 | 
| crystal | [lucky](http://luckyframework.org) (0.11) | 33.46 ms | 30.88 ms | 42.33 ms | 52.07 ms | 247.12 ms | 7909.67 | 
| crystal | [kemal](http://kemalcr.com) (0.24) | 34.55 ms | 31.61 ms | 44.10 ms | 55.39 ms | 84.35 ms | 7304.33 | 
| crystal | [orion](http://github.com/obsidian/orion) (1.6) | 36.50 ms | 32.15 ms | 55.70 ms | 65.95 ms | 142.57 ms | 10811.33 | 
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 35.81 ms | 32.85 ms | 56.53 ms | 93.12 ms | 181.77 ms | 16765.00 | 
| python | [flask](http://flask.pocoo.org) (1.0) | 45.26 ms | 37.58 ms | 82.28 ms | 107.54 ms | 444.61 ms | 23797.00 | 
| python | [sanic](http://github.com/huge-success/sanic) (0.8) | 56.19 ms | 50.80 ms | 92.25 ms | 136.87 ms | 322.95 ms | 25669.67 | 
| python | [django](http://djangoproject.com) (2.1) | 74.56 ms | 63.15 ms | 115.78 ms | 179.98 ms | 711.53 ms | 36772.00 | 
| python | [tornado](http://tornadoweb.org) (5.1) | 106.59 ms | 105.71 ms | 133.68 ms | 186.47 ms | 759.54 ms | 37620.67 | 
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 133.71 ms | 134.39 ms | 218.66 ms | 307.38 ms | 445.06 ms | 66713.00 | 

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
| rust | [actix-web](http://actix.rs) (0.7) | 182498.67 | 207.52 MB |
| go | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 173192.00 | 279.82 MB |
| python | [vibora](http://vibora.io) (0.0) | 164350.33 | 186.63 MB |
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 164322.00 | 159.40 MB |
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 150232.67 | 161.01 MB |
| nim | [jester](http://github.com/dom96/jester) (0.4) | 143248.33 | 287.82 MB |
| java | [act](http://actframework.org) (1.8) | 121155.67 | 207.28 MB |
| go | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 112173.67 | 150.31 MB |
| rust | [iron](http://ironframework.io) (0.6) | 106439.00 | 134.14 MB |
| go | [iris](http://iris-go.com) (10.7) | 104858.00 | 139.51 MB |
| rust | [rocket](http://rocket.rs) (0.3) | 103781.67 | 164.11 MB |
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 99954.00 | 175.51 MB |
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 98228.67 | 159.81 MB |
| go | [beego](http://beego.me) (1.10) | 97318.67 | 130.85 MB |
| go | [echo](http://echo.labstack.com) (3.3) | 95263.00 | 167.23 MB |
| rust | [nickel](http://nickel-org.github.io) (0.10) | 95048.33 | 188.87 MB |
| go | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 94338.00 | 125.72 MB |
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 75624.67 | 132.67 MB |
| scala | [akkahttp](http://akka.io) (10.1) | 71848.00 | 154.38 MB |
| php | [laravel](http://laravel.com) (5.7) | 69112.33 | 344.38 MB |
| php | [slim](http://slimframework.com) (3.11) | 65935.33 | 327.48 MB |
| node | [rayo](http://rayo.js.org) (1.2) | 62164.33 | 93.13 MB |
| node | [polka](http://github.com/lukeed/polka) (0.5) | 62038.00 | 92.88 MB |
| php | [symfony](http://symfony.com) (4.1) | 60505.33 | 301.39 MB |
| python | [bottle](http://bottlepy.org) (0.12) | 57142.67 | 140.79 MB |
| swift | [perfect](http://perfect.org) (3.0) | 56432.67 | 53.04 MB |
| node | [fastify](http://fastify.io) (1.12) | 52739.33 | 128.76 MB |
| scala | [http4s](http://http4s.org) (0.18) | 51632.33 | 90.63 MB |
| swift | [vapor](http://vapor.codes) (3.0) | 51328.67 | 87.62 MB |
| node | [koa](http://koajs.com) (2.5) | 50736.33 | 107.42 MB |
| ruby | [roda](http://roda.jeremyevans.net) (3.13) | 49750.33 | 47.54 MB |
| node | [restify](http://restify.com) (7.2) | 38158.33 | 66.92 MB |
| node | [express](http://expressjs.com) (4.16) | 37996.00 | 93.00 MB |
| swift | [kitura](http://kitura.io) (2.2) | 37302.33 | 69.35 MB |
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 36943.00 | 21.35 MB |
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 33365.67 | 31.29 MB |
| node | [hapi](http://hapijs.com) (17.6) | 30228.33 | 78.28 MB |
| crystal | [lucky](http://luckyframework.org) (0.11) | 29596.67 | 36.32 MB |
| crystal | [amber](http://amberframework.org) (0.9) | 29457.67 | 42.72 MB |
| crystal | [kemal](http://kemalcr.com) (0.24) | 29041.00 | 47.27 MB |
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 28190.00 | 63.96 MB |
| crystal | [orion](http://github.com/obsidian/orion) (1.6) | 27241.00 | 44.78 MB |
| python | [flask](http://flask.pocoo.org) (1.0) | 22114.33 | 54.55 MB |
| ruby | [flame](http://github.com/AlexWayfer/flame) (4.18) | 21762.33 | 12.58 MB |
| ruby | [hanami](http://hanamirb.org) (1.2) | 20841.67 | 158.02 MB |
| python | [sanic](http://github.com/huge-success/sanic) (0.8) | 18039.00 | 32.11 MB |
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 17810.33 | 46.25 MB |
| python | [django](http://djangoproject.com) (2.1) | 13234.00 | 38.43 MB |
| python | [tornado](http://tornadoweb.org) (5.1) | 9203.00 | 24.72 MB |
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 7452.33 | 14.89 MB |
| ruby | [rails](http://rubyonrails.org) (5.2) | 4884.33 | 14.88 MB |
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

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
Last update: 2018-09-23
```
OS: Linux (version: 4.18.8-200.fc28.x86_64, arch: x86_64)
CPU Cores: 4
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: rocket (rust)


:three: rack-routing (ruby)


:four: iron (rust)


:five: symfony (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust | [nickel](http://nickel.rs) (0.10) | 0.05 ms | 0.05 ms | 0.05 ms | 0.05 ms | 0.99 ms | 15.33 | 
| rust | [rocket](http://rocket.rs) (0.3) | 0.07 ms | 0.06 ms | 0.09 ms | 0.20 ms | 16.46 ms | 181.00 | 
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 2.16 ms | 0.25 ms | 5.67 ms | 31.50 ms | 95.46 ms | 5846.67 | 
| rust | [iron](http://ironframework.io) (0.7) | 0.33 ms | 0.28 ms | 0.51 ms | 1.16 ms | 70.05 ms | 643.00 | 
| php | [symfony](http://symfony.com) (4.1) | 171.53 ms | 0.43 ms | 551.59 ms | 2301.30 ms | 7048.75 ms | 512899.00 | 
| ruby | [roda](http://roda.jeremyevans.net) (3.12) | 1.70 ms | 0.44 ms | 3.40 ms | 27.84 ms | 101.77 ms | 5148.33 | 
| ruby | [flame](http://github.com/AlexWayfer/flame) (5.0 (rc)) | 3.36 ms | 0.56 ms | 9.39 ms | 35.87 ms | 88.24 ms | 6838.67 | 
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 4.12 ms | 0.66 ms | 11.21 ms | 47.71 ms | 102.36 ms | 8857.67 | 
| ruby | [hanami](http://hanamirb.org) (1.2) | 3.50 ms | 0.86 ms | 9.00 ms | 44.17 ms | 117.76 ms | 8090.00 | 
| php | [laravel](http://laravel.com) (5.7) | 340.26 ms | 1.41 ms | 1189.81 ms | 4461.40 ms | 7194.96 ms | 905849.33 | 
| ruby | [rails](http://rubyonrails.org) (5.2) | 16.76 ms | 2.50 ms | 59.30 ms | 104.84 ms | 204.00 ms | 26084.67 | 
| rust | [actix-web](http://actix.rs) (0.7) | 5.26 ms | 4.81 ms | 9.27 ms | 14.69 ms | 31.34 ms | 2872.00 | 
| go | [fasthttprouter](http://github.com/godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.66 ms | 5.26 ms | 8.53 ms | 14.54 ms | 50.88 ms | 2548.00 | 
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 6.07 ms | 5.61 ms | 9.66 ms | 16.00 ms | 140.46 ms | 3866.00 | 
| java | [act](http://actframework.org) (1.8) | 7.95 ms | 5.77 ms | 11.56 ms | 37.58 ms | 451.59 ms | 16248.33 | 
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 7.28 ms | 6.40 ms | 12.24 ms | 20.08 ms | 39.64 ms | 3776.33 | 
| python | [vibora](http://vibora.io) (0.0) | 7.26 ms | 6.54 ms | 13.14 ms | 19.92 ms | 47.03 ms | 4672.67 | 
| scala | [akkahttp](http://akka.io) (10.0) | 207.01 ms | 7.42 ms | 22.66 ms | 4994.13 ms | 7931.62 ms | 853557.67 | 
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (1.1) | 9.24 ms | 7.88 ms | 11.30 ms | 16.67 ms | 514.13 ms | 16589.33 | 
| go | [iris](http://iris-go.com) (10.7) | 15.73 ms | 9.97 ms | 25.24 ms | 167.00 ms | 557.97 ms | 30715.67 | 
| go | [echo](http://echo.labstack.com) (3.3) | 15.11 ms | 10.52 ms | 25.87 ms | 129.95 ms | 422.18 ms | 24199.33 | 
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 15.30 ms | 10.56 ms | 26.30 ms | 137.24 ms | 445.86 ms | 25538.00 | 
| go | [beego](http://beego.me) (1.10) | 17.62 ms | 11.33 ms | 28.69 ms | 172.97 ms | 496.37 ms | 31694.00 | 
| go | [gorilla-mux](http://gorillatoolkit.org/pkg/mux) (1.6) | 16.66 ms | 11.54 ms | 29.78 ms | 147.32 ms | 374.49 ms | 26108.00 | 
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 30.39 ms | 12.03 ms | 70.44 ms | 281.52 ms | 550.56 ms | 57286.33 | 
| scala | [http4s](http://http4s.org) (0.0) | 62.02 ms | 15.55 ms | 29.92 ms | 1718.21 ms | 3719.26 ms | 296367.33 | 
| swift | [perfect](http://perfect.org) (3.0) | 15.63 ms | 16.06 ms | 16.97 ms | 18.34 ms | 109.35 ms | 3266.33 | 
| node | [polka](http://github.com/lukeed/polka) (0.5) | 36.80 ms | 16.86 ms | 41.43 ms | 665.95 ms | 1759.90 ms | 115078.33 | 
| node | [rayo](http://rayo.js.org) (1.2) | 39.98 ms | 17.45 ms | 42.11 ms | 749.29 ms | 1900.47 ms | 127354.00 | 
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 18.64 ms | 17.54 ms | 23.76 ms | 24.75 ms | 225.87 ms | 4556.67 | 
| python | [bottle](http://bottlepy.org) (0.12) | 22.09 ms | 17.63 ms | 32.73 ms | 82.61 ms | 502.62 ms | 20904.67 | 
| crystal | [prism](http://github.com/vladfaust/prism) (0.4 (beta 3)) | 20.93 ms | 18.53 ms | 25.81 ms | 26.42 ms | 236.98 ms | 6120.33 | 
| swift | [vapor](http://vapor.codes) (3.0) | 114.59 ms | 19.36 ms | 86.08 ms | 2304.00 ms | 3885.39 ms | 399123.67 | 
| node | [fastify](http://fastify.io) (1.11) | 49.61 ms | 20.40 ms | 45.34 ms | 959.24 ms | 2069.51 ms | 157123.67 | 
| crystal | [kemal](http://kemalcr.com) (0.24) | 22.44 ms | 20.54 ms | 30.11 ms | 31.16 ms | 170.62 ms | 6175.33 | 
| node | [restify](http://restify.com) (7.2) | 29.71 ms | 20.84 ms | 43.81 ms | 227.59 ms | 993.34 ms | 49599.00 | 
| node | [koa](http://koajs.com) (2.5) | 47.28 ms | 21.11 ms | 46.36 ms | 874.69 ms | 2179.32 ms | 148497.33 | 
| crystal | [lucky](http://luckyframework.org) (0.11) | 23.28 ms | 22.08 ms | 27.74 ms | 28.46 ms | 242.69 ms | 8001.33 | 
| crystal | [amber](http://amberframework.org) (0.9) | 23.65 ms | 22.87 ms | 29.23 ms | 29.80 ms | 164.90 ms | 4909.00 | 
| node | [express](http://expressjs.com) (4.16) | 61.49 ms | 25.31 ms | 52.31 ms | 1178.45 ms | 2491.40 ms | 193364.00 | 
| swift | [kitura](http://kitura.io) (2.2) | 28.23 ms | 27.61 ms | 30.44 ms | 38.62 ms | 477.51 ms | 19971.33 | 
| node | [hapi](http://hapijs.com) (17.5) | 122.83 ms | 29.78 ms | 74.48 ms | 2418.11 ms | 4019.99 ms | 403083.00 | 
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 36.64 ms | 34.45 ms | 50.67 ms | 63.43 ms | 226.71 ms | 11194.67 | 
| python | [flask](http://flask.pocoo.org) (1.0) | 55.12 ms | 48.01 ms | 79.81 ms | 104.48 ms | 634.86 ms | 28864.33 | 
| python | [sanic](http://github.com/channelcat/sanic) (0.8) | 73.43 ms | 66.80 ms | 119.40 ms | 185.35 ms | 550.44 ms | 38948.33 | 
| python | [django](http://djangoproject.com) (2.1) | 94.03 ms | 82.38 ms | 119.81 ms | 402.38 ms | 1281.35 ms | 72547.00 | 
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 127.82 ms | 122.07 ms | 199.40 ms | 270.86 ms | 570.14 ms | 54319.67 | 
| python | [tornado](http://tornadoweb.org) (5.1) | 212.07 ms | 128.91 ms | 165.60 ms | 2614.31 ms | 3678.64 ms | 417546.33 | 

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
| rust | [actix-web](http://actix.rs) (0.7) | 169928.33 | 193.04 MB |
| go | [fasthttprouter](http://github.com/godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 159938.67 | 257.95 MB |
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 152686.67 | 148.16 MB |
| python | [vibora](http://vibora.io) (0.0) | 146492.00 | 166.28 MB |
| java | [act](http://actframework.org) (1.8) | 141747.33 | 242.42 MB |
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 134402.33 | 143.91 MB |
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (1.1) | 115704.67 | 188.50 MB |
| rust | [rocket](http://rocket.rs) (0.3) | 103137.00 | 163.81 MB |
| rust | [nickel](http://nickel.rs) (0.10) | 98143.00 | 195.09 MB |
| rust | [iron](http://ironframework.io) (0.7) | 93827.33 | 118.06 MB |
| go | [iris](http://iris-go.com) (10.7) | 93424.33 | 124.60 MB |
| go | [echo](http://echo.labstack.com) (3.3) | 90739.67 | 159.09 MB |
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 90443.67 | 158.60 MB |
| go | [beego](http://beego.me) (1.10) | 87160.00 | 117.05 MB |
| go | [gorilla-mux](http://gorillatoolkit.org/pkg/mux) (1.6) | 85657.00 | 114.43 MB |
| scala | [akkahttp](http://akka.io) (10.0) | 73408.67 | 157.42 MB |
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 69886.67 | 122.43 MB |
| swift | [perfect](http://perfect.org) (3.0) | 62862.67 | 58.97 MB |
| scala | [http4s](http://http4s.org) (0.0) | 57928.67 | 101.36 MB |
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 52787.00 | 49.53 MB |
| python | [bottle](http://bottlepy.org) (0.12) | 49825.67 | 122.51 MB |
| node | [polka](http://github.com/lukeed/polka) (0.5) | 48388.67 | 72.32 MB |
| crystal | [prism](http://github.com/vladfaust/prism) (0.4 (beta 3)) | 47885.33 | 53.45 MB |
| node | [rayo](http://rayo.js.org) (1.2) | 47538.67 | 71.06 MB |
| crystal | [kemal](http://kemalcr.com) (0.24) | 44329.67 | 72.24 MB |
| crystal | [lucky](http://luckyframework.org) (0.11) | 42869.67 | 52.87 MB |
| swift | [vapor](http://vapor.codes) (3.0) | 42803.67 | 71.39 MB |
| crystal | [amber](http://amberframework.org) (0.9) | 42718.33 | 61.96 MB |
| node | [fastify](http://fastify.io) (1.11) | 42633.33 | 104.14 MB |
| node | [koa](http://koajs.com) (2.5) | 40130.33 | 84.71 MB |
| node | [restify](http://restify.com) (7.2) | 40024.00 | 70.03 MB |
| ruby | [roda](http://roda.jeremyevans.net) (3.12) | 39283.33 | 37.43 MB |
| swift | [kitura](http://kitura.io) (2.2) | 36608.67 | 67.88 MB |
| node | [express](http://expressjs.com) (4.16) | 34332.67 | 83.89 MB |
| php | [symfony](http://symfony.com) (4.1) | 33541.67 | 166.50 MB |
| php | [laravel](http://laravel.com) (5.7) | 32260.33 | 160.44 MB |
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 29857.33 | 17.21 MB |
| node | [hapi](http://hapijs.com) (17.5) | 27521.33 | 71.13 MB |
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 27185.33 | 61.47 MB |
| ruby | [flame](http://github.com/AlexWayfer/flame) (5.0 (rc)) | 18998.33 | 10.96 MB |
| ruby | [hanami](http://hanamirb.org) (1.2) | 18401.67 | 139.21 MB |
| python | [flask](http://flask.pocoo.org) (1.0) | 18175.67 | 44.66 MB |
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 15532.33 | 40.28 MB |
| python | [sanic](http://github.com/channelcat/sanic) (0.8) | 13559.00 | 24.15 MB |
| python | [django](http://djangoproject.com) (2.1) | 11062.33 | 32.01 MB |
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 7606.00 | 15.15 MB |
| python | [tornado](http://tornadoweb.org) (5.1) | 7133.00 | 19.11 MB |
| ruby | [rails](http://rubyonrails.org) (5.2) | 3817.00 | 11.67 MB |
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

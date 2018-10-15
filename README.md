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
Last update: 2018-10-15
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
| rust | [nickel](http://nickel-org.github.io) (0.10) | 0.11 ms | 0.11 ms | 0.14 ms | 0.26 ms | 6.80 ms | 110.33 | 
| rust | [rocket](http://rocket.rs) (0.3) | 0.14 ms | 0.12 ms | 0.20 ms | 0.33 ms | 9.38 ms | 146.33 | 
| rust | [iron](http://ironframework.io) (0.6) | 0.60 ms | 0.41 ms | 1.14 ms | 4.00 ms | 35.53 ms | 807.00 | 
| ruby | [roda](http://roda.jeremyevans.net) (3.13) | 2.59 ms | 1.92 ms | 5.08 ms | 13.54 ms | 82.07 ms | 2704.67 | 
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.45 ms | 2.46 ms | 7.35 ms | 17.54 ms | 70.10 ms | 3532.33 | 
| ruby | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.79 ms | 4.19 ms | 12.78 ms | 24.96 ms | 79.30 ms | 5360.33 | 
| php | [slim](http://slimframework.com) (3.11) | 151.91 ms | 4.28 ms | 383.21 ms | 3049.77 ms | 6117.47 ms | 534246.00 | 
| ruby | [hanami](http://hanamirb.org) (1.2) | 6.19 ms | 4.38 ms | 13.99 ms | 29.72 ms | 123.58 ms | 6305.00 | 
| go | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.84 ms | 4.40 ms | 7.54 ms | 13.76 ms | 74.50 ms | 2388.33 | 
| php | [symfony](http://symfony.com) (4.1) | 115.86 ms | 4.57 ms | 115.24 ms | 3211.73 ms | 7908.00 ms | 561198.67 | 
| rust | [actix-web](http://actix.rs) (0.7) | 5.35 ms | 4.58 ms | 10.05 ms | 18.71 ms | 111.39 ms | 4332.00 | 
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 5.61 ms | 4.93 ms | 9.38 ms | 15.88 ms | 54.23 ms | 3159.33 | 
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 7.04 ms | 5.20 ms | 15.43 ms | 31.04 ms | 119.31 ms | 6592.67 | 
| php | [laravel](http://laravel.com) (5.7) | 274.93 ms | 5.43 ms | 462.57 ms | 4798.96 ms | 7079.46 ms | 906304.33 | 
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 6.43 ms | 5.52 ms | 11.23 ms | 18.62 ms | 43.02 ms | 3690.33 | 
| python | [vibora](http://vibora.io) (0.0) | 6.77 ms | 6.09 ms | 12.80 ms | 20.68 ms | 49.48 ms | 4544.67 | 
| ruby | [rails](http://rubyonrails.org) (5.2) | 27.59 ms | 6.95 ms | 87.79 ms | 171.15 ms | 329.44 ms | 40325.00 | 
| java | [act](http://actframework.org) (1.8) | 8.44 ms | 7.58 ms | 12.70 ms | 22.16 ms | 121.93 ms | 4872.00 | 
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 9.16 ms | 8.02 ms | 13.99 ms | 28.37 ms | 365.85 ms | 8726.00 | 
| scala | [akkahttp](http://akka.io) (10.1) | 189.77 ms | 8.46 ms | 26.42 ms | 4572.28 ms | 7919.30 ms | 800352.33 | 
| go | [echo](http://echo.labstack.com) (3.3) | 9.50 ms | 8.49 ms | 14.72 ms | 27.47 ms | 162.62 ms | 5016.67 | 
| go | [iris](http://iris-go.com) (10.7) | 10.52 ms | 9.15 ms | 16.85 ms | 32.46 ms | 176.16 ms | 6450.33 | 
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 15.32 ms | 9.23 ms | 18.80 ms | 160.68 ms | 1430.68 ms | 52429.00 | 
| go | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 11.78 ms | 10.12 ms | 18.93 ms | 36.50 ms | 197.46 ms | 7596.67 | 
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 30.81 ms | 10.17 ms | 97.37 ms | 276.37 ms | 447.59 ms | 59156.33 | 
| go | [beego](http://beego.me) (1.10) | 12.55 ms | 10.68 ms | 20.39 ms | 39.82 ms | 211.17 ms | 8226.00 | 
| node | [rayo](http://rayo.js.org) (1.2) | 19.02 ms | 12.07 ms | 29.72 ms | 135.76 ms | 758.68 ms | 35664.00 | 
| node | [polka](http://github.com/lukeed/polka) (0.5) | 26.07 ms | 14.36 ms | 36.63 ms | 324.01 ms | 993.03 ms | 59770.00 | 
| python | [bottle](http://bottlepy.org) (0.12) | 17.98 ms | 14.41 ms | 32.55 ms | 56.13 ms | 202.19 ms | 11225.67 | 
| node | [fastify](http://fastify.io) (1.12) | 22.41 ms | 14.86 ms | 30.70 ms | 198.55 ms | 824.56 ms | 41830.67 | 
| swift | [perfect](http://perfect.org) (3.0) | 16.36 ms | 16.58 ms | 18.68 ms | 20.69 ms | 109.61 ms | 2368.00 | 
| swift | [vapor](http://vapor.codes) (3.0) | 27.70 ms | 17.18 ms | 30.26 ms | 401.43 ms | 1736.49 ms | 86735.00 | 
| scala | [http4s](http://http4s.org) (0.18) | 44.66 ms | 17.26 ms | 33.98 ms | 1025.77 ms | 3859.33 ms | 212925.33 | 
| node | [koa](http://koajs.com) (2.5) | 29.15 ms | 18.72 ms | 40.62 ms | 310.12 ms | 1073.86 ms | 59131.00 | 
| node | [restify](http://restify.com) (7.2) | 28.82 ms | 22.50 ms | 44.42 ms | 99.15 ms | 674.32 ms | 30345.00 | 
| node | [express](http://expressjs.com) (4.16) | 37.30 ms | 22.66 ms | 50.19 ms | 458.50 ms | 1341.80 ms | 81179.67 | 
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 29.08 ms | 26.21 ms | 40.34 ms | 49.94 ms | 178.40 ms | 7172.67 | 
| swift | [kitura](http://kitura.io) (2.2) | 28.52 ms | 28.47 ms | 32.10 ms | 38.57 ms | 459.33 ms | 11318.67 | 
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 36.07 ms | 32.39 ms | 60.21 ms | 81.84 ms | 128.39 ms | 16557.33 | 
| node | [hapi](http://hapijs.com) (17.6) | 68.72 ms | 32.54 ms | 68.96 ms | 1139.65 ms | 2194.69 ms | 183125.67 | 
| crystal | [kemal](http://kemalcr.com) (0.24) | 37.31 ms | 33.82 ms | 48.55 ms | 58.85 ms | 69.55 ms | 8186.33 | 
| crystal | [amber](http://amberframework.org) (0.9) | 38.23 ms | 36.23 ms | 48.77 ms | 59.53 ms | 192.49 ms | 8463.33 | 
| python | [flask](http://flask.pocoo.org) (1.0) | 46.01 ms | 38.42 ms | 83.58 ms | 121.41 ms | 674.55 ms | 32594.33 | 
| crystal | [lucky](http://luckyframework.org) (0.11) | 39.86 ms | 39.18 ms | 47.32 ms | 55.42 ms | 259.53 ms | 8756.33 | 
| python | [sanic](http://github.com/huge-success/sanic) (0.8) | 56.29 ms | 53.35 ms | 85.08 ms | 132.63 ms | 239.48 ms | 23363.00 | 
| python | [django](http://djangoproject.com) (2.1) | 73.34 ms | 68.93 ms | 110.51 ms | 165.99 ms | 913.37 ms | 41859.00 | 
| python | [tornado](http://tornadoweb.org) (5.1) | 105.01 ms | 102.71 ms | 128.32 ms | 168.03 ms | 926.31 ms | 39317.00 | 
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 115.37 ms | 108.90 ms | 171.67 ms | 235.07 ms | 351.36 ms | 40951.33 | 

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
| go | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 187077.33 | 301.52 MB |
| rust | [actix-web](http://actix.rs) (0.7) | 172139.33 | 195.72 MB |
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 165156.00 | 160.16 MB |
| python | [vibora](http://vibora.io) (0.0) | 151890.67 | 172.38 MB |
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 149909.00 | 160.36 MB |
| java | [act](http://actframework.org) (1.8) | 130011.67 | 222.53 MB |
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 107414.33 | 188.63 MB |
| go | [echo](http://echo.labstack.com) (3.3) | 102833.00 | 180.55 MB |
| rust | [iron](http://ironframework.io) (0.6) | 97341.67 | 122.25 MB |
| rust | [rocket](http://rocket.rs) (0.3) | 93625.00 | 146.90 MB |
| go | [iris](http://iris-go.com) (10.7) | 93179.67 | 124.03 MB |
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 88359.67 | 143.80 MB |
| go | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 84558.33 | 112.74 MB |
| go | [beego](http://beego.me) (1.10) | 80295.67 | 108.75 MB |
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 79133.33 | 138.98 MB |
| rust | [nickel](http://nickel-org.github.io) (0.10) | 78128.00 | 155.41 MB |
| php | [slim](http://slimframework.com) (3.11) | 74861.33 | 372.32 MB |
| php | [laravel](http://laravel.com) (5.7) | 69204.67 | 344.47 MB |
| php | [symfony](http://symfony.com) (4.1) | 68513.33 | 341.35 MB |
| node | [rayo](http://rayo.js.org) (1.2) | 65960.00 | 98.82 MB |
| scala | [akkahttp](http://akka.io) (10.1) | 64172.33 | 137.82 MB |
| swift | [perfect](http://perfect.org) (3.0) | 59732.33 | 56.12 MB |
| python | [bottle](http://bottlepy.org) (0.12) | 58276.33 | 143.64 MB |
| node | [fastify](http://fastify.io) (1.12) | 56987.67 | 139.18 MB |
| node | [polka](http://github.com/lukeed/polka) (0.5) | 55900.00 | 83.56 MB |
| scala | [http4s](http://http4s.org) (0.18) | 52709.00 | 92.49 MB |
| swift | [vapor](http://vapor.codes) (3.0) | 51170.33 | 87.12 MB |
| ruby | [roda](http://roda.jeremyevans.net) (3.13) | 48270.33 | 46.14 MB |
| node | [koa](http://koajs.com) (2.5) | 44158.00 | 93.49 MB |
| node | [restify](http://restify.com) (7.2) | 37554.67 | 65.77 MB |
| node | [express](http://expressjs.com) (4.16) | 36810.67 | 89.91 MB |
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 36574.33 | 21.15 MB |
| swift | [kitura](http://kitura.io) (2.2) | 34533.67 | 64.17 MB |
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 33903.67 | 31.79 MB |
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 27796.00 | 63.02 MB |
| node | [hapi](http://hapijs.com) (17.6) | 27242.67 | 70.48 MB |
| crystal | [amber](http://amberframework.org) (0.9) | 26557.00 | 38.49 MB |
| crystal | [kemal](http://kemalcr.com) (0.24) | 26509.33 | 43.19 MB |
| crystal | [lucky](http://luckyframework.org) (0.11) | 24853.33 | 30.68 MB |
| ruby | [flame](http://github.com/AlexWayfer/flame) (4.18) | 22060.67 | 12.75 MB |
| python | [flask](http://flask.pocoo.org) (1.0) | 21992.67 | 54.06 MB |
| ruby | [hanami](http://hanamirb.org) (1.2) | 20497.33 | 155.35 MB |
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 18093.67 | 47.06 MB |
| python | [sanic](http://github.com/huge-success/sanic) (0.8) | 17743.00 | 31.63 MB |
| python | [django](http://djangoproject.com) (2.1) | 13513.67 | 39.09 MB |
| python | [tornado](http://tornadoweb.org) (5.1) | 9317.33 | 25.09 MB |
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 8545.00 | 17.02 MB |
| ruby | [rails](http://rubyonrails.org) (5.2) | 4639.67 | 14.15 MB |
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

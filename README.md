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
Last update: 2018-10-05
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
| rust | [nickel](http://nickel.rs) (0.10) | 0.10 ms | 0.09 ms | 0.13 ms | 0.33 ms | 4.80 ms | 94.00 | 
| rust | [rocket](http://nickel-org.github.io) (0.3) | 0.13 ms | 0.11 ms | 0.17 ms | 0.80 ms | 7.97 ms | 169.33 | 
| rust | [iron](http://ironframework.io) (0.7) | 0.53 ms | 0.45 ms | 0.83 ms | 2.50 ms | 22.72 ms | 495.00 | 
| ruby | [roda](http://roda.jeremyevans.net) (3.12) | 2.67 ms | 1.93 ms | 5.52 ms | 13.82 ms | 76.64 ms | 2817.00 | 
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.66 ms | 2.00 ms | 8.31 ms | 30.34 ms | 114.58 ms | 6045.33 | 
| ruby | [hanami](http://hanamirb.org) (1.2) | 6.40 ms | 2.58 ms | 17.16 ms | 48.78 ms | 142.86 ms | 9981.67 | 
| php | [symfony](http://symfony.com) (4.1) | 164.82 ms | 3.41 ms | 508.35 ms | 2499.58 ms | 6893.46 ms | 498460.67 | 
| ruby | [flame](http://github.com/AlexWayfer/flame) (5.0 (rc)) | 6.15 ms | 3.75 ms | 14.80 ms | 33.50 ms | 114.45 ms | 7082.33 | 
| php | [laravel](http://laravel.com) (5.7) | 242.58 ms | 3.97 ms | 628.01 ms | 3772.35 ms | 6978.88 ms | 727498.33 | 
| rust | [actix-web](http://actix.rs) (0.7) | 4.80 ms | 4.13 ms | 8.87 ms | 14.40 ms | 45.21 ms | 2995.00 | 
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 7.39 ms | 4.37 ms | 17.92 ms | 41.16 ms | 112.48 ms | 8687.67 | 
| go | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.17 ms | 4.45 ms | 7.82 ms | 15.58 ms | 231.76 ms | 5780.67 | 
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 5.06 ms | 4.46 ms | 8.70 ms | 14.45 ms | 94.84 ms | 2925.67 | 
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 6.34 ms | 5.39 ms | 11.10 ms | 18.90 ms | 43.86 ms | 3706.67 | 
| python | [vibora](http://vibora.io) (0.0) | 6.26 ms | 5.57 ms | 11.74 ms | 19.20 ms | 46.19 ms | 4120.67 | 
| java | [act](http://actframework.org) (1.8) | 8.15 ms | 6.90 ms | 11.54 ms | 22.10 ms | 274.82 ms | 8751.33 | 
| ruby | [rails](http://rubyonrails.org) (5.2) | 28.33 ms | 6.96 ms | 91.44 ms | 175.50 ms | 368.97 ms | 41741.33 | 
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (1.1) | 8.96 ms | 7.81 ms | 14.56 ms | 22.69 ms | 228.33 ms | 6427.00 | 
| go | [iris](http://iris-go.com) (10.7) | 8.76 ms | 7.88 ms | 13.28 ms | 24.64 ms | 217.66 ms | 5780.67 | 
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 9.34 ms | 8.32 ms | 14.55 ms | 27.45 ms | 124.02 ms | 5625.67 | 
| go | [echo](http://echo.labstack.com) (3.3) | 9.92 ms | 8.55 ms | 15.10 ms | 29.74 ms | 250.54 ms | 9324.67 | 
| go | [beego](http://beego.me) (1.10) | 10.07 ms | 8.78 ms | 15.65 ms | 30.83 ms | 244.60 ms | 7486.00 | 
| scala | [akkahttp](http://akka.io) (10.0) | 234.01 ms | 8.88 ms | 194.28 ms | 4948.01 ms | 7927.61 ms | 878577.67 | 
| go | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 10.44 ms | 9.08 ms | 16.28 ms | 31.62 ms | 281.05 ms | 7932.33 | 
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 31.16 ms | 10.02 ms | 76.92 ms | 320.78 ms | 553.97 ms | 66176.00 | 
| node | [rayo](http://rayo.js.org) (1.2) | 20.73 ms | 12.71 ms | 30.14 ms | 194.26 ms | 839.54 ms | 43644.00 | 
| node | [polka](http://github.com/lukeed/polka) (0.5) | 21.50 ms | 13.49 ms | 32.87 ms | 182.60 ms | 836.78 ms | 42317.00 | 
| python | [bottle](http://bottlepy.org) (0.12) | 17.89 ms | 13.59 ms | 33.31 ms | 66.21 ms | 316.50 ms | 15533.67 | 
| swift | [perfect](http://perfect.org) (3.0) | 15.94 ms | 16.11 ms | 18.15 ms | 20.11 ms | 254.53 ms | 5145.33 | 
| scala | [http4s](http://http4s.org) (0.0) | 23.75 ms | 16.43 ms | 31.82 ms | 175.44 ms | 1335.39 ms | 68015.67 | 
| node | [fastify](http://fastify.io) (1.12) | 25.39 ms | 16.55 ms | 37.44 ms | 207.08 ms | 931.57 ms | 46775.33 | 
| swift | [vapor](http://vapor.codes) (3.0) | 24.27 ms | 16.74 ms | 29.12 ms | 254.07 ms | 1323.38 ms | 60308.00 | 
| node | [koa](http://koajs.com) (2.5) | 28.44 ms | 17.63 ms | 38.22 ms | 322.65 ms | 1112.40 ms | 62174.67 | 
| node | [restify](http://restify.com) (7.2) | 24.76 ms | 19.07 ms | 33.52 ms | 118.47 ms | 732.79 ms | 33468.33 | 
| node | [express](http://expressjs.com) (4.16) | 33.43 ms | 22.01 ms | 49.55 ms | 307.69 ms | 1079.11 ms | 59420.67 | 
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 26.69 ms | 23.33 ms | 37.40 ms | 45.55 ms | 239.60 ms | 9059.67 | 
| crystal | [prism](http://github.com/vladfaust/prism) (0.4 (beta 3)) | 27.09 ms | 23.98 ms | 39.24 ms | 47.56 ms | 406.88 ms | 12031.00 | 
| swift | [kitura](http://kitura.io) (2.2) | 27.01 ms | 27.00 ms | 29.70 ms | 32.90 ms | 405.86 ms | 9145.67 | 
| node | [hapi](http://hapijs.com) (17.6) | 49.43 ms | 27.89 ms | 57.64 ms | 650.86 ms | 1457.55 ms | 108373.67 | 
| crystal | [lucky](http://luckyframework.org) (0.11) | 32.07 ms | 28.62 ms | 41.38 ms | 49.24 ms | 176.59 ms | 7482.00 | 
| crystal | [kemal](http://kemalcr.com) (0.24) | 31.77 ms | 28.69 ms | 42.92 ms | 53.67 ms | 341.87 ms | 10511.33 | 
| crystal | [amber](http://amberframework.org) (0.9) | 34.41 ms | 33.14 ms | 47.10 ms | 54.68 ms | 272.34 ms | 11707.67 | 
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 36.29 ms | 35.41 ms | 51.45 ms | 71.14 ms | 273.76 ms | 14154.00 | 
| python | [flask](http://flask.pocoo.org) (1.0) | 44.47 ms | 37.02 ms | 77.74 ms | 101.53 ms | 527.33 ms | 25048.33 | 
| python | [sanic](http://github.com/huge-success/sanic) (0.8) | 56.31 ms | 50.72 ms | 91.00 ms | 145.15 ms | 299.80 ms | 26701.67 | 
| python | [django](http://djangoproject.com) (2.1) | 85.61 ms | 79.11 ms | 129.52 ms | 160.30 ms | 676.24 ms | 30652.33 | 
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 107.78 ms | 97.08 ms | 171.69 ms | 274.91 ms | 630.02 ms | 51132.67 | 
| python | [tornado](http://tornadoweb.org) (5.1) | 110.27 ms | 107.59 ms | 141.96 ms | 243.70 ms | 1218.14 ms | 49780.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (actix-web) (rust)


:two: (fasthttprouter) (go)


:three: (evhtp) (cpp)


:four: (vibora) (python)


:five: (spider-gazelle) (crystal)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| rust | [actix-web](http://actix.rs) (0.7) | 194141.67 | 220.68 MB |
| go | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 184275.33 | 297.05 MB |
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 183494.67 | 178.16 MB |
| python | [vibora](http://vibora.io) (0.0) | 168331.67 | 190.98 MB |
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 153952.67 | 164.63 MB |
| java | [act](http://actframework.org) (1.8) | 142418.67 | 243.53 MB |
| rust | [iron](http://ironframework.io) (0.7) | 110890.33 | 139.68 MB |
| go | [iris](http://iris-go.com) (10.7) | 110554.67 | 147.68 MB |
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (1.1) | 109571.67 | 178.60 MB |
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 104864.67 | 184.03 MB |
| rust | [rocket](http://nickel-org.github.io) (0.3) | 102391.00 | 160.21 MB |
| go | [echo](http://echo.labstack.com) (3.3) | 101482.00 | 177.89 MB |
| go | [beego](http://beego.me) (1.10) | 98793.00 | 132.92 MB |
| go | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 96085.67 | 128.10 MB |
| rust | [nickel](http://nickel.rs) (0.10) | 86959.67 | 172.80 MB |
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 82879.33 | 145.35 MB |
| scala | [akkahttp](http://akka.io) (10.0) | 70295.33 | 151.04 MB |
| node | [rayo](http://rayo.js.org) (1.2) | 63947.67 | 95.65 MB |
| swift | [perfect](http://perfect.org) (3.0) | 61644.33 | 57.89 MB |
| python | [bottle](http://bottlepy.org) (0.12) | 59952.67 | 147.82 MB |
| node | [polka](http://github.com/lukeed/polka) (0.5) | 59751.67 | 89.41 MB |
| scala | [http4s](http://http4s.org) (0.0) | 56067.33 | 98.22 MB |
| php | [symfony](http://symfony.com) (4.1) | 55533.00 | 276.45 MB |
| swift | [vapor](http://vapor.codes) (3.0) | 52511.33 | 89.43 MB |
| node | [fastify](http://fastify.io) (1.12) | 50044.00 | 122.38 MB |
| ruby | [roda](http://roda.jeremyevans.net) (3.12) | 47234.00 | 45.10 MB |
| node | [koa](http://koajs.com) (2.5) | 46790.33 | 98.98 MB |
| node | [restify](http://restify.com) (7.2) | 45754.00 | 80.21 MB |
| php | [laravel](http://laravel.com) (5.7) | 44012.67 | 219.25 MB |
| node | [express](http://expressjs.com) (4.16) | 37198.67 | 91.07 MB |
| crystal | [prism](http://github.com/vladfaust/prism) (0.4 (beta 3)) | 36955.67 | 41.69 MB |
| swift | [kitura](http://kitura.io) (2.2) | 36669.33 | 68.03 MB |
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 36536.00 | 34.23 MB |
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 35830.33 | 20.69 MB |
| crystal | [kemal](http://kemalcr.com) (0.24) | 31315.00 | 50.96 MB |
| crystal | [lucky](http://luckyframework.org) (0.11) | 31017.00 | 38.14 MB |
| node | [hapi](http://hapijs.com) (17.6) | 30976.67 | 80.40 MB |
| crystal | [amber](http://amberframework.org) (0.9) | 28499.00 | 41.30 MB |
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 27180.00 | 61.47 MB |
| python | [flask](http://flask.pocoo.org) (1.0) | 22385.00 | 55.06 MB |
| ruby | [flame](http://github.com/AlexWayfer/flame) (5.0 (rc)) | 20939.33 | 12.10 MB |
| ruby | [hanami](http://hanamirb.org) (1.2) | 20069.67 | 151.98 MB |
| python | [sanic](http://github.com/huge-success/sanic) (0.8) | 17566.67 | 31.27 MB |
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 17261.67 | 44.85 MB |
| python | [django](http://djangoproject.com) (2.1) | 11420.67 | 33.07 MB |
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 9150.33 | 18.21 MB |
| python | [tornado](http://tornadoweb.org) (5.1) | 8823.67 | 23.66 MB |
| ruby | [rails](http://rubyonrails.org) (5.2) | 4522.33 | 13.79 MB |
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

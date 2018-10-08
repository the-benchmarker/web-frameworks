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
Last update: 2018-10-09
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
| rust | [nickel](http://nickel-org.github.io) (0.10) | 0.10 ms | 0.10 ms | 0.13 ms | 0.27 ms | 4.63 ms | 88.33 | 
| rust | [rocket](http://rocket.rs) (0.3) | 0.14 ms | 0.12 ms | 0.19 ms | 0.82 ms | 5.80 ms | 167.00 | 
| rust | [iron](http://ironframework.io) (0.6) | 0.57 ms | 0.45 ms | 1.02 ms | 3.19 ms | 24.84 ms | 622.00 | 
| ruby | [roda](http://roda.jeremyevans.net) (3.12) | 2.56 ms | 1.90 ms | 4.96 ms | 13.50 ms | 91.71 ms | 2894.33 | 
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.35 ms | 2.36 ms | 7.36 ms | 16.77 ms | 76.06 ms | 3470.33 | 
| ruby | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.63 ms | 4.02 ms | 12.50 ms | 24.25 ms | 140.77 ms | 5284.67 | 
| ruby | [hanami](http://hanamirb.org) (1.2) | 6.12 ms | 4.28 ms | 13.84 ms | 29.17 ms | 108.27 ms | 6181.33 | 
| php | [symfony](http://symfony.com) (4.1) | 143.95 ms | 4.51 ms | 380.94 ms | 1816.13 ms | 6861.45 ms | 455333.67 | 
| rust | [actix-web](http://actix.rs) (0.7) | 5.15 ms | 4.58 ms | 9.13 ms | 15.07 ms | 58.33 ms | 3162.00 | 
| go | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.47 ms | 4.81 ms | 8.85 ms | 15.69 ms | 47.93 ms | 2829.00 | 
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 5.96 ms | 5.31 ms | 10.11 ms | 16.71 ms | 42.78 ms | 3384.00 | 
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 7.75 ms | 5.65 ms | 16.90 ms | 34.57 ms | 110.77 ms | 7303.00 | 
| python | [vibora](http://vibora.io) (0.0) | 6.48 ms | 5.90 ms | 12.10 ms | 19.05 ms | 43.17 ms | 4199.33 | 
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 6.98 ms | 6.11 ms | 12.30 ms | 20.34 ms | 53.39 ms | 4137.67 | 
| php | [laravel](http://laravel.com) (5.7) | 285.78 ms | 6.19 ms | 509.48 ms | 5198.28 ms | 7388.04 ms | 945005.33 | 
| java | [act](http://actframework.org) (1.8) | 7.81 ms | 6.97 ms | 11.67 ms | 18.60 ms | 195.23 ms | 5697.33 | 
| scala | [akkahttp](http://akka.io) (10.0) | 208.27 ms | 7.78 ms | 22.62 ms | 5067.82 ms | 7934.97 ms | 862008.67 | 
| go | [iris](http://iris-go.com) (10.7) | 9.50 ms | 8.54 ms | 14.61 ms | 26.98 ms | 134.17 ms | 5005.33 | 
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 10.27 ms | 8.59 ms | 14.09 ms | 26.85 ms | 505.75 ms | 15568.00 | 
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 10.51 ms | 9.20 ms | 16.51 ms | 31.30 ms | 125.63 ms | 5492.33 | 
| go | [echo](http://echo.labstack.com) (3.3) | 11.19 ms | 9.66 ms | 17.73 ms | 34.28 ms | 223.36 ms | 7121.33 | 
| go | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 11.36 ms | 9.91 ms | 18.11 ms | 33.98 ms | 186.01 ms | 6601.67 | 
| go | [beego](http://beego.me) (1.10) | 11.96 ms | 10.27 ms | 19.14 ms | 37.36 ms | 151.90 ms | 7237.67 | 
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 33.28 ms | 10.79 ms | 103.62 ms | 309.17 ms | 570.97 ms | 64181.00 | 
| node | [rayo](http://rayo.js.org) (1.2) | 16.88 ms | 11.80 ms | 27.21 ms | 76.00 ms | 648.63 ms | 25364.67 | 
| node | [polka](http://github.com/lukeed/polka) (0.5) | 17.79 ms | 12.30 ms | 25.89 ms | 121.33 ms | 675.20 ms | 31818.33 | 
| python | [bottle](http://bottlepy.org) (0.12) | 20.34 ms | 15.47 ms | 37.81 ms | 66.37 ms | 449.67 ms | 17991.00 | 
| node | [fastify](http://fastify.io) (1.12) | 23.93 ms | 16.00 ms | 34.67 ms | 182.69 ms | 889.76 ms | 43538.00 | 
| scala | [http4s](http://http4s.org) (0.0) | 35.10 ms | 16.34 ms | 32.64 ms | 596.99 ms | 2977.12 ms | 132333.33 | 
| swift | [vapor](http://vapor.codes) (3.0) | 26.86 ms | 17.15 ms | 28.57 ms | 376.00 ms | 1521.57 ms | 77901.67 | 
| swift | [perfect](http://perfect.org) (3.0) | 17.50 ms | 17.49 ms | 19.97 ms | 23.25 ms | 103.99 ms | 2592.67 | 
| node | [koa](http://koajs.com) (2.5) | 32.15 ms | 18.63 ms | 42.37 ms | 425.78 ms | 1273.19 ms | 76658.67 | 
| ruby | [rails](http://rubyonrails.org) (5.2) | 32.94 ms | 23.19 ms | 77.39 ms | 143.70 ms | 323.34 ms | 32605.67 | 
| node | [express](http://expressjs.com) (4.16) | 42.21 ms | 24.41 ms | 53.02 ms | 592.66 ms | 1538.84 ms | 100561.00 | 
| node | [restify](http://restify.com) (7.2) | 31.88 ms | 24.86 ms | 51.96 ms | 113.18 ms | 685.71 ms | 32082.33 | 
| swift | [kitura](http://kitura.io) (2.2) | 26.37 ms | 26.05 ms | 29.96 ms | 38.43 ms | 310.96 ms | 5780.00 | 
| crystal | [prism](http://github.com/vladfaust/prism) (0.4 (beta 3)) | 29.71 ms | 27.25 ms | 42.12 ms | 52.07 ms | 316.64 ms | 8314.33 | 
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 37.36 ms | 31.06 ms | 68.33 ms | 110.25 ms | 214.62 ms | 22122.67 | 
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 33.38 ms | 31.86 ms | 44.16 ms | 58.90 ms | 265.82 ms | 11072.67 | 
| node | [hapi](http://hapijs.com) (17.6) | 56.85 ms | 32.16 ms | 66.47 ms | 819.63 ms | 1801.95 ms | 132274.00 | 
| crystal | [kemal](http://kemalcr.com) (0.24) | 38.18 ms | 35.07 ms | 51.45 ms | 65.69 ms | 190.64 ms | 9868.33 | 
| crystal | [lucky](http://luckyframework.org) (0.11) | 42.16 ms | 35.41 ms | 50.78 ms | 217.94 ms | 850.24 ms | 46836.67 | 
| python | [flask](http://flask.pocoo.org) (1.0) | 53.26 ms | 41.81 ms | 99.62 ms | 144.69 ms | 672.44 ms | 33244.33 | 
| crystal | [amber](http://amberframework.org) (0.9) | 43.23 ms | 44.49 ms | 54.97 ms | 66.19 ms | 252.03 ms | 10784.00 | 
| python | [sanic](http://github.com/huge-success/sanic) (0.8) | 60.76 ms | 53.53 ms | 101.68 ms | 164.51 ms | 257.88 ms | 31296.00 | 
| python | [django](http://djangoproject.com) (2.1) | 81.35 ms | 65.86 ms | 148.31 ms | 224.51 ms | 537.98 ms | 42186.00 | 
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 116.21 ms | 105.71 ms | 196.49 ms | 285.73 ms | 421.79 ms | 55721.67 | 
| python | [tornado](http://tornadoweb.org) (5.1) | 115.26 ms | 112.39 ms | 139.92 ms | 219.46 ms | 1084.87 ms | 50097.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (actix-web) (rust)


:two: (fasthttprouter) (go)


:three: (vibora) (python)


:four: (evhtp) (cpp)


:five: (act) (java)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| rust | [actix-web](http://actix.rs) (0.7) | 178076.00 | 202.41 MB |
| go | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 167485.33 | 268.46 MB |
| python | [vibora](http://vibora.io) (0.0) | 158304.00 | 179.71 MB |
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 155024.33 | 150.43 MB |
| java | [act](http://actframework.org) (1.8) | 139255.33 | 238.03 MB |
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 138762.00 | 147.18 MB |
| go | [iris](http://iris-go.com) (10.7) | 102426.33 | 137.10 MB |
| rust | [iron](http://ironframework.io) (0.6) | 102078.00 | 128.33 MB |
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 99669.67 | 162.18 MB |
| rust | [rocket](http://rocket.rs) (0.3) | 98986.67 | 154.09 MB |
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 93945.33 | 164.98 MB |
| go | [echo](http://echo.labstack.com) (3.3) | 88304.67 | 155.12 MB |
| go | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 87356.67 | 117.57 MB |
| go | [beego](http://beego.me) (1.10) | 83764.67 | 111.96 MB |
| rust | [nickel](http://nickel-org.github.io) (0.10) | 83313.67 | 165.76 MB |
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 72845.67 | 127.90 MB |
| node | [rayo](http://rayo.js.org) (1.2) | 68204.33 | 102.11 MB |
| node | [polka](http://github.com/lukeed/polka) (0.5) | 67710.33 | 101.42 MB |
| scala | [akkahttp](http://akka.io) (10.0) | 66253.67 | 142.25 MB |
| php | [laravel](http://laravel.com) (5.7) | 60008.33 | 298.36 MB |
| swift | [perfect](http://perfect.org) (3.0) | 55956.00 | 52.64 MB |
| scala | [http4s](http://http4s.org) (0.0) | 55820.67 | 97.91 MB |
| python | [bottle](http://bottlepy.org) (0.12) | 53450.67 | 131.70 MB |
| php | [symfony](http://symfony.com) (4.1) | 52559.33 | 261.70 MB |
| node | [fastify](http://fastify.io) (1.12) | 52251.33 | 127.02 MB |
| swift | [vapor](http://vapor.codes) (3.0) | 52066.00 | 88.70 MB |
| ruby | [roda](http://roda.jeremyevans.net) (3.12) | 49152.00 | 46.95 MB |
| node | [koa](http://koajs.com) (2.5) | 44730.00 | 94.54 MB |
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 37773.00 | 21.83 MB |
| swift | [kitura](http://kitura.io) (2.2) | 37432.33 | 69.54 MB |
| node | [express](http://expressjs.com) (4.16) | 35188.00 | 86.06 MB |
| node | [restify](http://restify.com) (7.2) | 33728.00 | 59.16 MB |
| crystal | [prism](http://github.com/vladfaust/prism) (0.4 (beta 3)) | 32894.67 | 37.09 MB |
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 29998.33 | 28.12 MB |
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 27811.00 | 63.07 MB |
| node | [hapi](http://hapijs.com) (17.6) | 27774.00 | 71.98 MB |
| crystal | [kemal](http://kemalcr.com) (0.24) | 25860.33 | 42.13 MB |
| crystal | [lucky](http://luckyframework.org) (0.11) | 25782.33 | 31.71 MB |
| crystal | [amber](http://amberframework.org) (0.9) | 22834.00 | 33.11 MB |
| ruby | [flame](http://github.com/AlexWayfer/flame) (4.18) | 22771.33 | 13.16 MB |
| ruby | [hanami](http://hanamirb.org) (1.2) | 20835.00 | 157.94 MB |
| python | [flask](http://flask.pocoo.org) (1.0) | 19180.33 | 47.30 MB |
| python | [sanic](http://github.com/huge-success/sanic) (0.8) | 16776.67 | 29.93 MB |
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 16403.33 | 42.67 MB |
| python | [django](http://djangoproject.com) (2.1) | 12278.33 | 35.65 MB |
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 8615.33 | 17.16 MB |
| python | [tornado](http://tornadoweb.org) (5.1) | 8493.00 | 22.74 MB |
| ruby | [rails](http://rubyonrails.org) (5.2) | 3901.67 | 11.87 MB |
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

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
Last update: 2018-10-18
```
OS: Linux (version: 3.10.0-862.2.3.el7.x86_64, arch: x86_64)
CPU Cores: 6
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
| rust | [nickel](http://nickel-org.github.io) (0.10) | 1.30 ms | 0.06 ms | 0.08 ms | 0.13 ms | 4590.76 ms | 60971.00 | 
| rust | [rocket](http://rocket.rs) (0.3) | 1.08 ms | 0.09 ms | 0.15 ms | 0.25 ms | 3064.24 ms | 44731.00 | 
| rust | [iron](http://ironframework.io) (0.6) | 1.35 ms | 0.27 ms | 0.62 ms | 1.69 ms | 3075.63 ms | 45065.00 | 
| ruby | [roda](http://roda.jeremyevans.net) (3.13) | 2.60 ms | 1.62 ms | 5.05 ms | 17.22 ms | 355.95 ms | 5394.67 | 
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.03 ms | 1.99 ms | 5.93 ms | 18.10 ms | 350.85 ms | 5536.00 | 
| ruby | [flame](http://github.com/AlexWayfer/flame) (4.18) | 4.87 ms | 3.63 ms | 9.88 ms | 20.87 ms | 319.13 ms | 5171.67 | 
| rust | [actix-web](http://actix.rs) (0.7) | 5.16 ms | 3.82 ms | 11.03 ms | 24.15 ms | 57.33 ms | 4867.00 | 
| ruby | [hanami](http://hanamirb.org) (1.2) | 5.12 ms | 3.86 ms | 10.03 ms | 23.87 ms | 377.99 ms | 5672.33 | 
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 6.56 ms | 4.95 ms | 12.74 ms | 32.31 ms | 340.32 ms | 7375.67 | 
| python | [vibora](http://vibora.io) (0.0) | 7.09 ms | 5.03 ms | 16.50 ms | 30.90 ms | 76.81 ms | 7052.00 | 
| go | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 6.25 ms | 5.37 ms | 10.90 ms | 18.92 ms | 58.55 ms | 3811.33 | 
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 5.97 ms | 5.40 ms | 11.22 ms | 13.07 ms | 35.90 ms | 3676.67 | 
| php | [slim](http://slimframework.com) (3.11) | 131.51 ms | 5.46 ms | 365.36 ms | 1864.91 ms | 5745.55 ms | 405821.33 | 
| c | [kore](http://kore.io) (3.1) | 5.85 ms | 5.60 ms | 9.41 ms | 11.56 ms | 1356.87 ms | 15499.33 | 
| php | [symfony](http://symfony.com) (4.1) | 141.91 ms | 6.43 ms | 355.75 ms | 2008.18 ms | 3689.41 ms | 402117.00 | 
| java | [act](http://actframework.org) (1.8) | 8.36 ms | 6.69 ms | 14.40 ms | 30.80 ms | 213.06 ms | 7880.67 | 
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 7.71 ms | 7.20 ms | 11.77 ms | 19.05 ms | 191.77 ms | 4049.00 | 
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 8.88 ms | 7.92 ms | 15.19 ms | 25.85 ms | 70.30 ms | 5031.00 | 
| nim | [jester](http://github.com/dom96/jester) (0.4) | 8.48 ms | 7.96 ms | 13.61 ms | 23.19 ms | 89.33 ms | 4756.00 | 
| go | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 8.89 ms | 7.98 ms | 14.11 ms | 25.18 ms | 251.50 ms | 7879.00 | 
| scala | [akkahttp](http://akka.io) (10.1) | 80.12 ms | 8.14 ms | 21.03 ms | 1941.63 ms | 4085.41 ms | 339480.67 | 
| go | [iris](http://iris-go.com) (10.7) | 9.08 ms | 8.42 ms | 14.64 ms | 25.33 ms | 169.89 ms | 4932.33 | 
| php | [laravel](http://laravel.com) (5.7) | 236.69 ms | 8.84 ms | 489.35 ms | 3593.35 ms | 6877.30 ms | 715269.67 | 
| go | [echo](http://echo.labstack.com) (3.3) | 9.78 ms | 8.85 ms | 16.05 ms | 26.91 ms | 191.92 ms | 5897.67 | 
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 9.93 ms | 9.00 ms | 16.26 ms | 27.24 ms | 121.45 ms | 5819.33 | 
| go | [beego](http://beego.me) (1.10) | 10.29 ms | 9.41 ms | 16.82 ms | 27.89 ms | 117.26 ms | 5317.33 | 
| go | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 11.01 ms | 9.89 ms | 17.72 ms | 29.19 ms | 213.44 ms | 6040.33 | 
| python | [bottle](http://bottlepy.org) (0.12) | 14.12 ms | 11.25 ms | 22.49 ms | 38.19 ms | 345.06 ms | 10084.00 | 
| node | [polka](http://github.com/lukeed/polka) (0.5) | 16.85 ms | 11.62 ms | 24.18 ms | 138.48 ms | 775.48 ms | 35657.00 | 
| node | [rayo](http://rayo.js.org) (1.2) | 23.15 ms | 12.18 ms | 28.47 ms | 367.04 ms | 1115.61 ms | 66489.00 | 
| scala | [http4s](http://http4s.org) (0.18) | 14.78 ms | 13.06 ms | 25.98 ms | 44.85 ms | 223.09 ms | 9475.33 | 
| swift | [vapor](http://vapor.codes) (3.0) | 16.37 ms | 13.10 ms | 25.17 ms | 47.99 ms | 494.37 ms | 15245.33 | 
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 31.56 ms | 13.22 ms | 84.35 ms | 263.24 ms | 382.94 ms | 54302.67 | 
| swift | [perfect](http://perfect.org) (3.0) | 13.61 ms | 13.69 ms | 18.05 ms | 22.21 ms | 147.17 ms | 3808.33 | 
| ruby | [rails](http://rubyonrails.org) (5.2) | 24.99 ms | 16.10 ms | 59.99 ms | 122.11 ms | 284.65 ms | 26498.33 | 
| node | [fastify](http://fastify.io) (1.12) | 47.68 ms | 16.37 ms | 42.69 ms | 988.35 ms | 2070.70 ms | 162646.00 | 
| node | [koa](http://koajs.com) (2.5) | 46.09 ms | 18.54 ms | 47.45 ms | 909.76 ms | 1965.95 ms | 148151.67 | 
| node | [express](http://expressjs.com) (4.16) | 51.96 ms | 22.54 ms | 54.20 ms | 977.73 ms | 2034.67 ms | 155155.00 | 
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 24.02 ms | 23.04 ms | 34.35 ms | 48.36 ms | 155.00 ms | 8418.33 | 
| node | [restify](http://restify.com) (7.2) | 36.81 ms | 24.25 ms | 55.00 ms | 375.13 ms | 1193.91 ms | 69918.67 | 
| swift | [kitura](http://kitura.io) (2.5) | 25.14 ms | 24.44 ms | 32.65 ms | 44.43 ms | 155.32 ms | 7004.33 | 
| node | [hapi](http://hapijs.com) (17.6) | 85.68 ms | 26.41 ms | 67.75 ms | 1660.04 ms | 2825.34 ms | 271782.33 | 
| python | [flask](http://flask.pocoo.org) (1.0) | 36.89 ms | 31.95 ms | 55.94 ms | 85.25 ms | 608.28 ms | 22728.67 | 
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 40.31 ms | 38.73 ms | 52.12 ms | 64.54 ms | 331.68 ms | 11580.00 | 
| crystal | [lucky](http://luckyframework.org) (0.11) | 46.02 ms | 42.91 ms | 60.97 ms | 72.01 ms | 225.87 ms | 10235.33 | 
| crystal | [kemal](http://kemalcr.com) (0.24) | 44.97 ms | 44.08 ms | 57.91 ms | 75.89 ms | 316.12 ms | 14903.00 | 
| crystal | [orion](http://github.com/obsidian/orion) (1.6) | 50.36 ms | 46.13 ms | 69.72 ms | 83.37 ms | 329.21 ms | 15188.00 | 
| python | [sanic](http://github.com/huge-success/sanic) (0.8) | 58.81 ms | 50.11 ms | 107.59 ms | 169.64 ms | 281.28 ms | 35438.33 | 
| crystal | [amber](http://amberframework.org) (0.9) | 53.46 ms | 51.49 ms | 67.40 ms | 76.26 ms | 190.99 ms | 10445.33 | 
| python | [django](http://djangoproject.com) (2.1) | 64.46 ms | 59.09 ms | 85.10 ms | 149.34 ms | 817.94 ms | 32621.00 | 
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 88.24 ms | 74.49 ms | 161.75 ms | 219.00 ms | 293.24 ms | 52379.67 | 
| python | [tornado](http://tornadoweb.org) (5.1) | 100.07 ms | 101.04 ms | 122.86 ms | 148.60 ms | 484.23 ms | 22982.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (actix-web) (rust)


:two: (vibora) (python)


:three: (evhtp) (cpp)


:four: (act) (java)


:five: (fasthttprouter) (go)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| rust | [actix-web](http://actix.rs) (0.7) | 197910.67 | 225.01 MB |
| python | [vibora](http://vibora.io) (0.0) | 164846.00 | 187.18 MB |
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 151523.00 | 147.15 MB |
| java | [act](http://actframework.org) (1.8) | 150106.33 | 256.90 MB |
| go | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 146133.33 | 235.03 MB |
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 120873.67 | 196.81 MB |
| nim | [jester](http://github.com/dom96/jester) (0.4) | 115441.33 | 232.03 MB |
| rust | [iron](http://ironframework.io) (0.6) | 108982.33 | 137.55 MB |
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 108385.33 | 116.09 MB |
| go | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 106872.00 | 143.34 MB |
| go | [iris](http://iris-go.com) (10.7) | 102012.67 | 136.48 MB |
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 95876.00 | 168.23 MB |
| go | [echo](http://echo.labstack.com) (3.3) | 95570.33 | 167.70 MB |
| go | [beego](http://beego.me) (1.10) | 91726.67 | 123.34 MB |
| go | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 85920.00 | 114.74 MB |
| rust | [rocket](http://rocket.rs) (0.3) | 82145.33 | 131.51 MB |
| scala | [akkahttp](http://akka.io) (10.1) | 76769.67 | 164.80 MB |
| node | [polka](http://github.com/lukeed/polka) (0.5) | 73421.33 | 110.03 MB |
| swift | [perfect](http://perfect.org) (3.0) | 69924.67 | 65.79 MB |
| python | [bottle](http://bottlepy.org) (0.12) | 68868.33 | 169.62 MB |
| node | [rayo](http://rayo.js.org) (1.2) | 68686.33 | 102.95 MB |
| scala | [http4s](http://http4s.org) (0.18) | 67236.67 | 117.94 MB |
| rust | [nickel](http://nickel-org.github.io) (0.10) | 66501.00 | 132.40 MB |
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 60742.00 | 106.61 MB |
| swift | [vapor](http://vapor.codes) (3.0) | 59751.67 | 100.47 MB |
| node | [fastify](http://fastify.io) (1.12) | 51839.00 | 127.39 MB |
| c | [kore](http://kore.io) (3.1) | 48936.33 | 132.77 MB |
| php | [slim](http://slimframework.com) (3.11) | 46956.33 | 233.62 MB |
| node | [koa](http://koajs.com) (2.5) | 44486.67 | 94.19 MB |
| php | [symfony](http://symfony.com) (4.1) | 43057.00 | 214.18 MB |
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 40345.67 | 91.56 MB |
| swift | [kitura](http://kitura.io) (2.5) | 39179.00 | 72.96 MB |
| ruby | [roda](http://roda.jeremyevans.net) (3.13) | 37473.33 | 35.79 MB |
| php | [laravel](http://laravel.com) (5.7) | 37070.33 | 184.75 MB |
| node | [express](http://expressjs.com) (4.16) | 36898.00 | 90.36 MB |
| node | [restify](http://restify.com) (7.2) | 35095.00 | 61.56 MB |
| node | [hapi](http://hapijs.com) (17.6) | 32225.00 | 83.67 MB |
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 31646.67 | 18.28 MB |
| python | [flask](http://flask.pocoo.org) (1.0) | 26443.00 | 65.19 MB |
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 23767.00 | 22.29 MB |
| crystal | [lucky](http://luckyframework.org) (0.11) | 20970.33 | 25.78 MB |
| crystal | [kemal](http://kemalcr.com) (0.24) | 20486.67 | 33.38 MB |
| ruby | [flame](http://github.com/AlexWayfer/flame) (4.18) | 19401.67 | 11.20 MB |
| crystal | [orion](http://github.com/obsidian/orion) (1.6) | 19363.67 | 31.58 MB |
| crystal | [amber](http://amberframework.org) (0.9) | 18410.00 | 26.70 MB |
| ruby | [hanami](http://hanamirb.org) (1.2) | 18396.33 | 139.49 MB |
| python | [sanic](http://github.com/huge-success/sanic) (0.8) | 17256.33 | 30.76 MB |
| python | [django](http://djangoproject.com) (2.1) | 15233.33 | 44.24 MB |
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 14524.33 | 37.77 MB |
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 11516.33 | 23.01 MB |
| python | [tornado](http://tornadoweb.org) (5.1) | 9283.67 | 24.89 MB |
| ruby | [rails](http://rubyonrails.org) (5.2) | 3833.00 | 11.77 MB |
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

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
| rust | [nickel](http://nickel-org.github.io) (0.10) | 0.11 ms | 0.10 ms | 0.14 ms | 0.51 ms | 9.21 ms | 155.33 | 
| rust | [rocket](http://rocket.rs) (0.3) | 0.15 ms | 0.12 ms | 0.20 ms | 1.14 ms | 11.12 ms | 246.33 | 
| rust | [iron](http://ironframework.io) (0.6) | 0.58 ms | 0.43 ms | 1.01 ms | 3.56 ms | 27.82 ms | 741.33 | 
| ruby | [roda](http://roda.jeremyevans.net) (3.12) | 3.35 ms | 1.53 ms | 6.58 ms | 37.35 ms | 221.18 ms | 7890.67 | 
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.04 ms | 1.99 ms | 9.26 ms | 35.23 ms | 147.06 ms | 7101.00 | 
| ruby | [flame](http://github.com/AlexWayfer/flame) (5.0 (rc)) | 6.82 ms | 3.01 ms | 17.67 ms | 49.08 ms | 164.88 ms | 10165.00 | 
| ruby | [hanami](http://hanamirb.org) (1.2) | 6.79 ms | 3.23 ms | 16.77 ms | 55.76 ms | 174.39 ms | 10988.00 | 
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 8.65 ms | 3.40 ms | 22.52 ms | 68.51 ms | 176.60 ms | 13606.33 | 
| rust | [actix-web](http://actix.rs) (0.7) | 5.31 ms | 4.45 ms | 9.86 ms | 16.80 ms | 36.73 ms | 3473.67 | 
| php | [symfony](http://symfony.com) (4.1) | 58.32 ms | 4.47 ms | 106.24 ms | 1138.49 ms | 5821.27 ms | 228887.00 | 
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 5.91 ms | 4.99 ms | 10.43 ms | 18.27 ms | 100.05 ms | 3676.00 | 
| go | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.91 ms | 5.12 ms | 9.11 ms | 18.30 ms | 188.84 ms | 4501.00 | 
| php | [laravel](http://laravel.com) (5.7) | 269.35 ms | 5.50 ms | 384.28 ms | 5104.73 ms | 6781.61 ms | 926589.00 | 
| python | [vibora](http://vibora.io) (0.0) | 7.45 ms | 6.14 ms | 14.47 ms | 26.48 ms | 58.96 ms | 5557.67 | 
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 7.36 ms | 6.16 ms | 13.42 ms | 24.82 ms | 66.52 ms | 4923.67 | 
| ruby | [rails](http://rubyonrails.org) (5.2) | 32.30 ms | 6.74 ms | 106.30 ms | 205.32 ms | 403.85 ms | 48916.67 | 
| java | [act](http://actframework.org) (1.8) | 8.82 ms | 7.39 ms | 12.91 ms | 32.76 ms | 220.16 ms | 8672.33 | 
| go | [iris](http://iris-go.com) (10.7) | 9.29 ms | 8.29 ms | 14.46 ms | 27.11 ms | 213.00 ms | 5959.00 | 
| scala | [akkahttp](http://akka.io) (10.0) | 208.01 ms | 8.33 ms | 70.48 ms | 4738.05 ms | 7925.77 ms | 836959.33 | 
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (1.1) | 11.40 ms | 8.83 ms | 17.45 ms | 32.89 ms | 583.26 ms | 19348.00 | 
| go | [beego](http://beego.me) (1.10) | 11.84 ms | 8.85 ms | 16.73 ms | 44.96 ms | 718.40 ms | 26442.33 | 
| go | [echo](http://echo.labstack.com) (3.3) | 11.37 ms | 9.50 ms | 17.60 ms | 36.31 ms | 367.77 ms | 11805.33 | 
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 11.60 ms | 9.93 ms | 18.44 ms | 36.67 ms | 248.51 ms | 9018.00 | 
| go | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 12.32 ms | 10.45 ms | 19.91 ms | 40.05 ms | 300.52 ms | 9308.67 | 
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 32.84 ms | 10.88 ms | 69.68 ms | 352.95 ms | 632.39 ms | 69326.67 | 
| node | [rayo](http://rayo.js.org) (1.2) | 19.40 ms | 12.71 ms | 28.75 ms | 140.13 ms | 760.28 ms | 36888.33 | 
| node | [polka](http://github.com/lukeed/polka) (0.5) | 20.01 ms | 13.28 ms | 30.65 ms | 143.57 ms | 733.21 ms | 35674.33 | 
| python | [bottle](http://bottlepy.org) (0.12) | 22.31 ms | 15.14 ms | 43.28 ms | 89.10 ms | 239.36 ms | 17698.67 | 
| node | [fastify](http://fastify.io) (1.12) | 30.34 ms | 16.54 ms | 37.92 ms | 429.14 ms | 1250.23 ms | 77148.67 | 
| swift | [perfect](http://perfect.org) (3.0) | 16.96 ms | 17.11 ms | 19.27 ms | 22.10 ms | 97.94 ms | 2308.33 | 
| swift | [vapor](http://vapor.codes) (3.0) | 37.47 ms | 17.30 ms | 32.23 ms | 698.73 ms | 2093.43 ms | 130147.67 | 
| scala | [http4s](http://http4s.org) (0.0) | 37.47 ms | 17.67 ms | 34.77 ms | 702.51 ms | 3226.05 ms | 145115.00 | 
| node | [koa](http://koajs.com) (2.5) | 36.30 ms | 20.21 ms | 46.10 ms | 522.80 ms | 1460.32 ms | 90442.00 | 
| node | [restify](http://restify.com) (7.2) | 28.94 ms | 20.80 ms | 41.16 ms | 198.47 ms | 879.44 ms | 44194.33 | 
| node | [express](http://expressjs.com) (4.16) | 48.26 ms | 25.69 ms | 59.08 ms | 728.64 ms | 1702.62 ms | 119482.33 | 
| swift | [kitura](http://kitura.io) (2.2) | 26.70 ms | 26.55 ms | 30.88 ms | 39.32 ms | 385.12 ms | 10603.67 | 
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 28.63 ms | 26.73 ms | 37.28 ms | 45.49 ms | 380.17 ms | 10259.00 | 
| crystal | [prism](http://github.com/vladfaust/prism) (0.4 (beta 3)) | 31.57 ms | 28.98 ms | 43.08 ms | 49.87 ms | 324.95 ms | 11906.67 | 
| crystal | [kemal](http://kemalcr.com) (0.24) | 33.35 ms | 30.40 ms | 47.28 ms | 55.46 ms | 193.88 ms | 9682.00 | 
| crystal | [lucky](http://luckyframework.org) (0.11) | 34.21 ms | 32.98 ms | 44.04 ms | 51.75 ms | 124.00 ms | 7994.00 | 
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 39.57 ms | 36.87 ms | 60.44 ms | 91.87 ms | 255.69 ms | 16731.67 | 
| node | [hapi](http://hapijs.com) (17.6) | 98.08 ms | 38.62 ms | 87.16 ms | 1675.51 ms | 2895.24 ms | 274205.00 | 
| crystal | [amber](http://amberframework.org) (0.9) | 38.11 ms | 40.91 ms | 45.31 ms | 53.32 ms | 191.14 ms | 8220.33 | 
| python | [flask](http://flask.pocoo.org) (1.0) | 63.26 ms | 49.12 ms | 113.48 ms | 206.14 ms | 380.36 ms | 37770.67 | 
| python | [sanic](http://github.com/huge-success/sanic) (0.8) | 66.81 ms | 60.70 ms | 110.73 ms | 177.93 ms | 477.65 ms | 34797.00 | 
| python | [django](http://djangoproject.com) (2.1) | 85.81 ms | 72.67 ms | 142.00 ms | 218.17 ms | 541.19 ms | 40488.67 | 
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 129.19 ms | 113.67 ms | 224.94 ms | 340.82 ms | 548.96 ms | 68509.67 | 
| python | [tornado](http://tornadoweb.org) (5.1) | 120.84 ms | 113.98 ms | 172.60 ms | 252.49 ms | 615.83 ms | 42908.67 | 

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
| rust | [actix-web](http://actix.rs) (0.7) | 181366.33 | 206.18 MB |
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 160448.00 | 155.76 MB |
| go | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 160172.00 | 259.69 MB |
| python | [vibora](http://vibora.io) (0.0) | 150942.00 | 171.33 MB |
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 137288.00 | 147.33 MB |
| java | [act](http://actframework.org) (1.8) | 133305.00 | 227.78 MB |
| go | [iris](http://iris-go.com) (10.7) | 104413.00 | 138.65 MB |
| rust | [iron](http://ironframework.io) (0.6) | 100946.67 | 127.66 MB |
| go | [beego](http://beego.me) (1.10) | 96670.00 | 131.34 MB |
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (1.1) | 94758.67 | 154.42 MB |
| go | [echo](http://echo.labstack.com) (3.3) | 90121.33 | 158.03 MB |
| rust | [rocket](http://rocket.rs) (0.3) | 87427.00 | 138.92 MB |
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 86781.00 | 152.16 MB |
| go | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 82263.67 | 110.48 MB |
| rust | [nickel](http://nickel-org.github.io) (0.10) | 78781.33 | 156.65 MB |
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 72921.67 | 127.89 MB |
| scala | [akkahttp](http://akka.io) (10.0) | 68185.00 | 146.50 MB |
| node | [rayo](http://rayo.js.org) (1.2) | 64374.33 | 96.28 MB |
| php | [laravel](http://laravel.com) (5.7) | 63083.67 | 314.25 MB |
| node | [polka](http://github.com/lukeed/polka) (0.5) | 61540.00 | 92.17 MB |
| swift | [perfect](http://perfect.org) (3.0) | 57757.33 | 54.36 MB |
| php | [symfony](http://symfony.com) (4.1) | 53250.00 | 265.23 MB |
| node | [fastify](http://fastify.io) (1.12) | 51092.00 | 124.73 MB |
| scala | [http4s](http://http4s.org) (0.0) | 50670.67 | 88.75 MB |
| python | [bottle](http://bottlepy.org) (0.12) | 50097.00 | 123.47 MB |
| swift | [vapor](http://vapor.codes) (3.0) | 49659.67 | 84.73 MB |
| ruby | [roda](http://roda.jeremyevans.net) (3.12) | 41641.00 | 39.73 MB |
| node | [koa](http://koajs.com) (2.5) | 40951.67 | 86.75 MB |
| node | [restify](http://restify.com) (7.2) | 40445.33 | 71.02 MB |
| swift | [kitura](http://kitura.io) (2.2) | 36707.67 | 68.08 MB |
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 34534.33 | 32.37 MB |
| node | [express](http://expressjs.com) (4.16) | 32964.33 | 80.74 MB |
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 32587.33 | 18.82 MB |
| crystal | [prism](http://github.com/vladfaust/prism) (0.4 (beta 3)) | 31799.33 | 35.65 MB |
| crystal | [lucky](http://luckyframework.org) (0.11) | 29610.33 | 36.46 MB |
| crystal | [kemal](http://kemalcr.com) (0.24) | 29508.33 | 48.04 MB |
| crystal | [amber](http://amberframework.org) (0.9) | 26017.33 | 37.70 MB |
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 25314.00 | 57.27 MB |
| node | [hapi](http://hapijs.com) (17.6) | 23474.67 | 60.64 MB |
| ruby | [hanami](http://hanamirb.org) (1.2) | 19078.67 | 144.49 MB |
| ruby | [flame](http://github.com/AlexWayfer/flame) (5.0 (rc)) | 19054.33 | 11.00 MB |
| python | [flask](http://flask.pocoo.org) (1.0) | 16355.00 | 40.26 MB |
| python | [sanic](http://github.com/huge-success/sanic) (0.8) | 15011.33 | 26.73 MB |
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 14834.00 | 38.54 MB |
| python | [django](http://djangoproject.com) (2.1) | 11616.67 | 33.63 MB |
| python | [tornado](http://tornadoweb.org) (5.1) | 8040.00 | 21.48 MB |
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 7583.67 | 15.10 MB |
| ruby | [rails](http://rubyonrails.org) (5.2) | 3982.33 | 12.10 MB |
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

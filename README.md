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
Last update: 2018-11-09
```
OS: Linux (version: 4.18.17-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: symfony (php)


:three: iron (rust)


:four: laravel (php)


:five: rack-routing (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 0.07 ms | 0.07 ms | 0.09 ms | 0.12 ms | 4.12 ms | 36.67 | 
| php (7.2) | [symfony](http://symfony.com) (4.1) | 123.24 ms | 0.27 ms | 219.56 ms | 2692.19 ms | 6742.76 ms | 471942.33 | 
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 0.30 ms | 0.28 ms | 0.51 ms | 0.83 ms | 12.04 ms | 199.33 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 98.40 ms | 0.34 ms | 263.48 ms | 1672.36 ms | 6829.23 ms | 353368.67 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.81 ms | 0.70 ms | 10.94 ms | 36.78 ms | 121.70 ms | 7427.67 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 6.81 ms | 0.72 ms | 21.24 ms | 65.95 ms | 213.37 ms | 13589.67 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 82.05 ms | 0.73 ms | 186.56 ms | 1690.65 ms | 6689.72 ms | 323241.00 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 3.13 ms | 1.00 ms | 8.67 ms | 26.22 ms | 95.26 ms | 5383.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 37.85 ms | 1.34 ms | 2.93 ms | 1248.54 ms | 3307.89 ms | 233356.67 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 7.94 ms | 2.02 ms | 23.45 ms | 65.57 ms | 190.85 ms | 13646.67 | 
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 3.07 ms | 2.12 ms | 6.53 ms | 15.62 ms | 37.41 ms | 3238.67 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 84.92 ms | 2.25 ms | 175.45 ms | 1872.18 ms | 4219.94 ms | 319070.67 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 6.30 ms | 2.34 ms | 17.62 ms | 46.06 ms | 136.59 ms | 9696.67 | 
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 2.97 ms | 2.34 ms | 5.94 ms | 12.51 ms | 89.39 ms | 2886.00 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.46 ms | 2.85 ms | 5.72 ms | 10.95 ms | 48.29 ms | 2339.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.55 ms | 2.93 ms | 7.41 ms | 14.33 ms | 46.54 ms | 3135.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.99 ms | 3.32 ms | 6.85 ms | 11.84 ms | 27.16 ms | 2282.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.28 ms | 3.42 ms | 5.34 ms | 7.57 ms | 77.87 ms | 1874.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 4.29 ms | 3.83 ms | 7.93 ms | 14.42 ms | 29.62 ms | 2927.00 | 
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 4.66 ms | 3.90 ms | 7.65 ms | 14.33 ms | 366.27 ms | 9803.33 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 5.78 ms | 4.11 ms | 9.78 ms | 54.80 ms | 116.62 ms | 9108.67 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 6.11 ms | 4.84 ms | 9.09 ms | 27.68 ms | 82.37 ms | 4601.67 | 
| go (1.11) | [iris](http://iris-go.com) (11.0) | 5.88 ms | 4.87 ms | 9.74 ms | 20.05 ms | 216.32 ms | 5062.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 5.96 ms | 4.94 ms | 9.74 ms | 19.88 ms | 277.19 ms | 6367.33 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 6.55 ms | 5.27 ms | 11.06 ms | 22.68 ms | 225.97 ms | 5448.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 5.51 ms | 5.30 ms | 9.04 ms | 11.43 ms | 1161.88 ms | 12858.67 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 6.51 ms | 5.32 ms | 10.67 ms | 21.14 ms | 228.74 ms | 5659.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 6.04 ms | 5.32 ms | 8.95 ms | 16.54 ms | 236.53 ms | 5983.00 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 6.61 ms | 5.38 ms | 11.29 ms | 22.26 ms | 53.23 ms | 4056.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.43 ms | 5.46 ms | 10.76 ms | 21.93 ms | 107.67 ms | 4984.00 | 
| go (1.11) | [beego](http://beego.me) (1.10) | 6.75 ms | 5.52 ms | 11.37 ms | 22.54 ms | 59.35 ms | 4089.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 6.91 ms | 5.64 ms | 11.65 ms | 22.82 ms | 111.21 ms | 4311.67 | 
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 7.92 ms | 5.88 ms | 12.63 ms | 29.96 ms | 315.65 ms | 10823.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 7.48 ms | 5.95 ms | 11.78 ms | 19.60 ms | 203.75 ms | 5202.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 8.90 ms | 7.41 ms | 15.42 ms | 28.22 ms | 229.04 ms | 6794.33 | 
| node (10.12) | [fastify](http://fastify.io) (1.13) | 9.62 ms | 7.46 ms | 14.99 ms | 36.85 ms | 377.86 ms | 12432.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 213.16 ms | 7.52 ms | 118.87 ms | 4821.30 ms | 7940.19 ms | 849963.67 | 
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 10.79 ms | 8.00 ms | 18.92 ms | 42.20 ms | 422.10 ms | 14611.00 | 
| python (3.7) | [starlette](http://starlette.io) (0.6) | 10.90 ms | 9.33 ms | 18.74 ms | 28.57 ms | 96.40 ms | 5884.00 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 37.47 ms | 9.41 ms | 120.58 ms | 312.07 ms | 836.56 ms | 66771.67 | 
| node (10.12) | [koa](http://koajs.com) (2.6) | 14.44 ms | 10.31 ms | 23.13 ms | 55.95 ms | 617.10 ms | 23818.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 13.68 ms | 10.90 ms | 19.18 ms | 40.64 ms | 666.34 ms | 24330.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 13.99 ms | 11.66 ms | 24.11 ms | 47.31 ms | 424.64 ms | 18749.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 11.66 ms | 11.80 ms | 13.25 ms | 14.96 ms | 26.68 ms | 1525.67 | 
| node (10.12) | [express](http://expressjs.com) (4.16) | 18.97 ms | 14.52 ms | 29.92 ms | 70.24 ms | 642.55 ms | 27058.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 17.48 ms | 15.21 ms | 30.95 ms | 47.33 ms | 86.44 ms | 9945.33 | 
| node (10.12) | [restify](http://restify.com) (7.2) | 19.05 ms | 15.43 ms | 32.37 ms | 59.38 ms | 433.25 ms | 16818.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 27.76 ms | 18.92 ms | 55.23 ms | 82.80 ms | 309.18 ms | 19399.67 | 
| node (10.12) | [hapi](http://hapijs.com) (17.7) | 26.36 ms | 19.02 ms | 37.19 ms | 202.67 ms | 968.70 ms | 47305.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 22.32 ms | 19.84 ms | 34.47 ms | 50.28 ms | 173.98 ms | 8783.33 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 25.43 ms | 21.83 ms | 34.99 ms | 40.83 ms | 414.30 ms | 11342.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 26.87 ms | 24.32 ms | 40.99 ms | 48.92 ms | 399.88 ms | 11873.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 29.50 ms | 28.20 ms | 40.54 ms | 47.91 ms | 465.89 ms | 17744.33 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 27.03 ms | 28.70 ms | 35.52 ms | 73.90 ms | 312.26 ms | 13275.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 31.78 ms | 29.79 ms | 44.91 ms | 53.51 ms | 539.16 ms | 15170.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 37.85 ms | 31.63 ms | 68.17 ms | 132.52 ms | 240.54 ms | 26267.00 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 30.28 ms | 31.89 ms | 39.46 ms | 46.80 ms | 179.19 ms | 8532.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 40.29 ms | 35.76 ms | 66.23 ms | 111.73 ms | 155.87 ms | 20321.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 48.73 ms | 39.06 ms | 92.38 ms | 126.85 ms | 396.03 ms | 29225.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 74.69 ms | 76.35 ms | 92.69 ms | 118.93 ms | 448.67 ms | 21443.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (actix-web) (rust)


:three: (vibora) (python)


:four: (fasthttprouter) (go)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 356867.33 | 427.12 MB |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 324181.00 | 368.32 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 305431.00 | 346.69 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 269424.00 | 435.09 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 268418.00 | 260.52 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 263020.33 | 528.78 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 229824.67 | 245.63 MB |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 217297.67 | 445.41 MB |
| java (8) | [act](http://actframework.org) (1.8) | 216388.67 | 422.73 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 214844.00 | 124.29 MB |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 191086.67 | 241.31 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 170531.33 | 299.27 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 166402.67 | 223.32 MB |
| go (1.11) | [iris](http://iris-go.com) (11.0) | 164158.67 | 219.87 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 158738.33 | 258.80 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 152634.67 | 204.32 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 151919.67 | 266.61 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 149641.33 | 262.66 MB |
| go (1.11) | [beego](http://beego.me) (1.10) | 147635.33 | 198.35 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 143493.33 | 192.23 MB |
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 136530.00 | 204.58 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 131294.00 | 337.14 MB |
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 127667.33 | 253.65 MB |
| node (10.12) | [fastify](http://fastify.io) (1.13) | 117047.00 | 282.24 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 114572.33 | 282.51 MB |
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 104980.00 | 157.11 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 94500.00 | 469.42 MB |
| python (3.7) | [starlette](http://starlette.io) (0.6) | 91928.33 | 184.86 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 86867.00 | 81.63 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 83394.00 | 146.02 MB |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 82554.33 | 409.10 MB |
| node (10.12) | [koa](http://koajs.com) (2.6) | 81298.33 | 171.82 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 78334.67 | 131.70 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 76306.00 | 395.81 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 73074.67 | 156.56 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 65237.33 | 102.28 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 61801.33 | 322.09 MB |
| node (10.12) | [express](http://expressjs.com) (4.16) | 59716.67 | 145.85 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 58382.00 | 132.49 MB |
| c (99) | [kore](http://kore.io) (3.1) | 55936.67 | 151.59 MB |
| node (10.12) | [restify](http://restify.com) (7.2) | 55512.00 | 97.13 MB |
| node (10.12) | [hapi](http://hapijs.com) (17.7) | 47181.33 | 121.90 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 44922.33 | 83.31 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 41228.00 | 39.28 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 39515.67 | 97.35 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 39338.33 | 36.86 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 38087.67 | 35.70 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 36433.67 | 44.76 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 34491.67 | 19.87 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 33803.00 | 61.69 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 33145.67 | 53.99 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 31092.67 | 50.66 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 28364.33 | 50.51 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 25229.67 | 50.35 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 21190.00 | 61.46 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 20491.33 | 11.81 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 19051.00 | 144.03 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 16179.33 | 41.96 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 12923.33 | 34.56 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 3432.00 | 10.47 MB |
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

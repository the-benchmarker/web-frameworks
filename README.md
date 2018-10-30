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
Last update: 2018-10-30
```
OS: Linux (version: 4.18.16-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
threads: 9, connections: 1000
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: laravel (php)


:three: iron (rust)


:four: rack-routing (ruby)


:five: roda (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 0.09 ms | 0.08 ms | 0.13 ms | 0.17 ms | 1.25 ms | 32.00 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 126.64 ms | 0.44 ms | 336.76 ms | 2211.19 ms | 6959.76 ms | 427179.67 | 
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 0.50 ms | 0.49 ms | 0.84 ms | 1.17 ms | 37.11 ms | 362.00 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 5.61 ms | 1.19 ms | 16.49 ms | 47.61 ms | 159.68 ms | 9924.33 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 4.54 ms | 1.48 ms | 12.77 ms | 35.75 ms | 114.91 ms | 7436.00 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 9.31 ms | 1.71 ms | 28.22 ms | 75.36 ms | 195.18 ms | 15930.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 70.72 ms | 1.96 ms | 4.91 ms | 2247.67 ms | 4951.82 ms | 383861.00 | 
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 3.73 ms | 2.47 ms | 8.53 ms | 17.66 ms | 39.13 ms | 3880.67 | 
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 3.54 ms | 2.88 ms | 6.49 ms | 12.91 ms | 91.07 ms | 3002.33 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 150.43 ms | 3.34 ms | 258.65 ms | 3240.83 ms | 6394.02 ms | 561535.00 | 
| php (7.2) | [symfony](http://symfony.com) (4.1) | 162.43 ms | 3.49 ms | 310.11 ms | 3411.09 ms | 6183.14 ms | 586094.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.76 ms | 3.62 ms | 9.71 ms | 18.26 ms | 37.03 ms | 3908.67 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 10.63 ms | 3.72 ms | 30.39 ms | 74.09 ms | 191.04 ms | 15880.33 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 8.89 ms | 4.15 ms | 24.10 ms | 55.12 ms | 147.67 ms | 11969.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.85 ms | 4.19 ms | 5.74 ms | 9.92 ms | 62.19 ms | 2073.00 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.67 ms | 4.44 ms | 7.20 ms | 15.05 ms | 101.53 ms | 2869.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.92 ms | 4.64 ms | 7.80 ms | 14.07 ms | 52.04 ms | 2776.67 | 
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 5.61 ms | 5.10 ms | 10.65 ms | 18.02 ms | 158.62 ms | 4161.00 | 
| crystal (0.26.1) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 5.71 ms | 5.14 ms | 9.97 ms | 18.01 ms | 38.01 ms | 3400.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 25.52 ms | 5.49 ms | 9.61 ms | 853.58 ms | 1606.15 ms | 126354.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 7.57 ms | 6.49 ms | 10.33 ms | 20.37 ms | 447.70 ms | 9898.33 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 8.25 ms | 7.27 ms | 12.65 ms | 25.67 ms | 115.30 ms | 4907.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 8.13 ms | 7.49 ms | 13.03 ms | 24.36 ms | 268.55 ms | 7870.67 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 8.61 ms | 7.54 ms | 13.33 ms | 28.13 ms | 174.45 ms | 6358.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 223.18 ms | 7.89 ms | 104.44 ms | 5221.79 ms | 7937.38 ms | 889400.33 | 
| go (1.11) | [iris](http://iris-go.com) (11.0) | 9.12 ms | 8.40 ms | 13.65 ms | 27.77 ms | 180.07 ms | 5703.67 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 9.62 ms | 8.46 ms | 14.70 ms | 31.18 ms | 248.64 ms | 8931.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 9.56 ms | 8.52 ms | 14.57 ms | 30.31 ms | 253.51 ms | 8092.67 | 
| go (1.11) | [beego](http://beego.me) (1.10) | 9.80 ms | 8.57 ms | 14.76 ms | 32.46 ms | 359.40 ms | 9584.00 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 9.58 ms | 8.74 ms | 14.90 ms | 30.00 ms | 120.74 ms | 5670.67 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 29.77 ms | 8.90 ms | 86.77 ms | 306.05 ms | 468.82 ms | 63476.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 12.73 ms | 10.12 ms | 22.92 ms | 43.34 ms | 291.11 ms | 9467.33 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 40.16 ms | 10.90 ms | 126.81 ms | 307.83 ms | 763.71 ms | 67253.33 | 
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 14.99 ms | 11.07 ms | 22.82 ms | 70.64 ms | 537.32 ms | 22326.00 | 
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 19.34 ms | 11.48 ms | 23.63 ms | 249.78 ms | 931.39 ms | 51950.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 12.10 ms | 12.09 ms | 14.60 ms | 17.09 ms | 95.27 ms | 2529.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 25.79 ms | 13.19 ms | 23.93 ms | 475.43 ms | 1935.65 ms | 98299.33 | 
| node (10.12) | [fastify](http://fastify.io) (1.12) | 27.50 ms | 16.67 ms | 34.27 ms | 354.71 ms | 1142.36 ms | 65568.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 18.23 ms | 16.75 ms | 33.07 ms | 54.89 ms | 227.80 ms | 11995.00 | 
| node (10.12) | [koa](http://koajs.com) (2.6) | 23.43 ms | 17.52 ms | 34.19 ms | 126.85 ms | 795.77 ms | 35722.67 | 
| node (10.12) | [foxify](http://foxify.js.org) (0.10) | 25.34 ms | 17.84 ms | 37.24 ms | 172.73 ms | 916.28 ms | 43980.67 | 
| node (10.12) | [restify](http://restify.com) (7.2) | 22.64 ms | 18.66 ms | 34.98 ms | 69.67 ms | 465.42 ms | 18420.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 25.87 ms | 24.75 ms | 42.23 ms | 63.95 ms | 143.77 ms | 12590.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 27.86 ms | 25.98 ms | 40.55 ms | 55.23 ms | 199.85 ms | 9151.00 | 
| node (10.12) | [express](http://expressjs.com) (4.16) | 39.97 ms | 27.30 ms | 50.19 ms | 456.88 ms | 1316.83 ms | 79434.00 | 
| crystal (0.26.1) | [raze](http://razecr.com) (0.3) | 29.86 ms | 27.43 ms | 38.11 ms | 47.78 ms | 172.79 ms | 6542.67 | 
| crystal (0.26.1) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 29.47 ms | 28.58 ms | 37.06 ms | 43.22 ms | 241.80 ms | 6698.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 36.92 ms | 29.87 ms | 65.91 ms | 95.28 ms | 320.13 ms | 19101.00 | 
| crystal (0.26.1) | [lucky](http://luckyframework.org) (0.11) | 33.84 ms | 31.41 ms | 46.12 ms | 54.84 ms | 258.97 ms | 10354.00 | 
| crystal (0.26.1) | [kemal](http://kemalcr.com) (0.24) | 35.50 ms | 33.30 ms | 46.28 ms | 58.62 ms | 192.76 ms | 8276.33 | 
| node (10.12) | [hapi](http://hapijs.com) (17.6) | 73.62 ms | 34.28 ms | 65.39 ms | 1255.26 ms | 2396.78 ms | 201888.33 | 
| crystal (0.26.1) | [amber](http://amberframework.org) (0.9) | 38.25 ms | 38.47 ms | 46.35 ms | 89.21 ms | 266.29 ms | 12933.33 | 
| crystal (0.26.1) | [orion](http://github.com/obsidian/orion) (1.6) | 42.16 ms | 39.78 ms | 54.01 ms | 63.58 ms | 235.60 ms | 8701.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 62.48 ms | 47.97 ms | 124.61 ms | 163.79 ms | 494.06 ms | 35608.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 60.67 ms | 49.19 ms | 119.43 ms | 222.73 ms | 429.56 ms | 45586.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 94.91 ms | 90.36 ms | 128.73 ms | 163.13 ms | 625.22 ms | 36766.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 105.14 ms | 97.73 ms | 196.24 ms | 299.63 ms | 468.95 ms | 66268.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (actix-web) (rust)


:three: (vibora) (python)


:four: (evhtp) (cpp)


:five: (jester) (nim)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 299285.00 | 358.36 MB |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 261557.00 | 297.27 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 230848.67 | 262.11 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 229509.00 | 222.56 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 213867.33 | 430.09 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 201064.67 | 323.36 MB |
| crystal (0.26.1) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 170595.00 | 182.33 MB |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 169342.33 | 347.24 MB |
| java (8) | [act](http://actframework.org) (1.8) | 148957.33 | 254.63 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 130776.67 | 213.12 MB |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 119720.67 | 151.14 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 119573.67 | 160.55 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 116012.67 | 155.32 MB |
| go (1.11) | [iris](http://iris-go.com) (11.0) | 108704.67 | 145.56 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 105331.00 | 184.85 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 104924.00 | 184.23 MB |
| go (1.11) | [beego](http://beego.me) (1.10) | 104702.67 | 140.65 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 103582.00 | 138.43 MB |
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 95483.33 | 189.19 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 94089.67 | 165.21 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 81387.00 | 200.64 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 81197.00 | 76.29 MB |
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 76242.67 | 114.14 MB |
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 74473.33 | 111.55 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 66883.67 | 143.51 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 66420.33 | 110.88 MB |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 63266.00 | 314.55 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 61407.67 | 305.13 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 59833.00 | 104.81 MB |
| c (99) | [kore](http://kore.io) (3.1) | 57530.33 | 155.94 MB |
| node (10.12) | [fastify](http://fastify.io) (1.12) | 54307.00 | 129.79 MB |
| node (10.12) | [koa](http://koajs.com) (2.6) | 49256.67 | 104.26 MB |
| node (10.12) | [foxify](http://foxify.js.org) (0.10) | 47822.00 | 100.33 MB |
| node (10.12) | [restify](http://restify.com) (7.2) | 46208.33 | 80.87 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 41383.33 | 215.65 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 40708.67 | 64.69 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 39394.33 | 89.35 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 36255.33 | 67.28 MB |
| crystal (0.26.1) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 33634.33 | 31.54 MB |
| crystal (0.26.1) | [raze](http://razecr.com) (0.3) | 33294.00 | 31.20 MB |
| node (10.12) | [express](http://expressjs.com) (4.16) | 32692.33 | 79.83 MB |
| crystal (0.26.1) | [lucky](http://luckyframework.org) (0.11) | 29134.00 | 35.78 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 28137.67 | 26.83 MB |
| crystal (0.26.1) | [kemal](http://kemalcr.com) (0.24) | 27810.33 | 45.32 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 27547.33 | 67.84 MB |
| node (10.12) | [hapi](http://hapijs.com) (17.6) | 26420.67 | 68.38 MB |
| crystal (0.26.1) | [amber](http://amberframework.org) (0.9) | 26408.33 | 38.28 MB |
| crystal (0.26.1) | [orion](http://github.com/obsidian/orion) (1.6) | 23329.33 | 38.01 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 22904.67 | 13.21 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 17927.33 | 31.96 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 16340.33 | 47.45 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 14389.00 | 8.30 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 13768.67 | 104.14 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 12049.67 | 31.24 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 10334.67 | 27.72 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 9859.67 | 19.63 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 3191.33 | 9.74 MB |
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

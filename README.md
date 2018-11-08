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
Last update: 2018-11-08
```
OS: Linux (version: 4.18.16-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: laravel (php)


:four: rack-routing (ruby)


:five: roda (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 0.09 ms | 0.09 ms | 0.13 ms | 0.18 ms | 4.34 ms | 50.00 | 
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 0.53 ms | 0.50 ms | 0.88 ms | 1.31 ms | 17.81 ms | 322.33 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 130.05 ms | 0.51 ms | 370.37 ms | 2123.09 ms | 6978.66 ms | 427095.33 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 6.35 ms | 1.16 ms | 19.06 ms | 53.65 ms | 164.20 ms | 11250.67 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 5.06 ms | 1.46 ms | 14.48 ms | 40.64 ms | 136.99 ms | 8518.67 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 10.33 ms | 1.55 ms | 31.89 ms | 86.15 ms | 234.99 ms | 18220.67 | 
| php (7.2) | [symfony](http://symfony.com) (4.1) | 215.90 ms | 1.96 ms | 368.41 ms | 4347.69 ms | 7470.96 ms | 763632.67 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 195.62 ms | 1.98 ms | 312.08 ms | 4510.21 ms | 7478.50 ms | 748977.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 123.16 ms | 2.35 ms | 132.47 ms | 2627.90 ms | 4952.77 ms | 502361.67 | 
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 4.00 ms | 3.37 ms | 8.01 ms | 17.20 ms | 47.61 ms | 3651.00 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 156.02 ms | 3.43 ms | 289.88 ms | 3475.60 ms | 5975.63 ms | 573816.67 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 11.69 ms | 3.83 ms | 33.81 ms | 82.58 ms | 205.44 ms | 17716.00 | 
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 4.17 ms | 3.87 ms | 7.45 ms | 15.13 ms | 90.68 ms | 3200.33 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 9.70 ms | 4.23 ms | 26.68 ms | 61.36 ms | 172.24 ms | 13395.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 5.25 ms | 4.44 ms | 10.33 ms | 17.81 ms | 45.58 ms | 3839.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.21 ms | 5.04 ms | 7.76 ms | 15.91 ms | 79.32 ms | 2789.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.89 ms | 5.13 ms | 6.55 ms | 11.23 ms | 36.95 ms | 2074.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 5.81 ms | 5.22 ms | 10.10 ms | 18.93 ms | 39.05 ms | 3519.67 | 
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 6.74 ms | 5.47 ms | 11.68 ms | 20.60 ms | 365.59 ms | 9639.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.68 ms | 5.56 ms | 8.97 ms | 14.69 ms | 102.20 ms | 3053.33 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 8.73 ms | 6.64 ms | 15.55 ms | 53.77 ms | 135.35 ms | 9739.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 7.22 ms | 7.03 ms | 12.06 ms | 16.79 ms | 258.75 ms | 4215.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 8.34 ms | 7.89 ms | 13.04 ms | 23.54 ms | 134.24 ms | 4644.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 8.74 ms | 8.03 ms | 11.35 ms | 24.27 ms | 247.65 ms | 8917.33 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 9.22 ms | 8.58 ms | 13.94 ms | 26.72 ms | 68.57 ms | 4494.00 | 
| go (1.11) | [iris](http://iris-go.com) (11.0) | 10.10 ms | 8.70 ms | 14.03 ms | 30.90 ms | 394.76 ms | 14820.00 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 9.13 ms | 8.72 ms | 12.73 ms | 30.44 ms | 98.65 ms | 4687.33 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 10.13 ms | 9.10 ms | 15.03 ms | 31.42 ms | 230.78 ms | 8627.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 10.01 ms | 9.22 ms | 15.39 ms | 30.00 ms | 164.73 ms | 5740.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 193.86 ms | 9.25 ms | 31.56 ms | 4455.91 ms | 7914.74 ms | 787655.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 10.70 ms | 9.39 ms | 16.19 ms | 34.63 ms | 315.67 ms | 10085.33 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 10.44 ms | 9.45 ms | 16.15 ms | 32.69 ms | 169.74 ms | 6204.00 | 
| go (1.11) | [beego](http://beego.me) (1.10) | 11.25 ms | 9.84 ms | 17.29 ms | 36.59 ms | 230.72 ms | 8067.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 13.76 ms | 10.85 ms | 25.62 ms | 43.88 ms | 217.30 ms | 9588.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 14.95 ms | 12.22 ms | 24.59 ms | 46.55 ms | 508.68 ms | 15435.00 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 46.34 ms | 12.29 ms | 146.63 ms | 374.66 ms | 979.65 ms | 80241.33 | 
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 17.47 ms | 12.82 ms | 23.08 ms | 121.66 ms | 754.51 ms | 34534.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 14.17 ms | 14.23 ms | 16.43 ms | 18.82 ms | 33.31 ms | 1961.00 | 
| python (3.7) | [starlette](http://starlette.io) (0.6) | 17.04 ms | 15.03 ms | 29.76 ms | 44.20 ms | 77.52 ms | 9197.67 | 
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 23.19 ms | 15.26 ms | 33.05 ms | 199.93 ms | 937.00 ms | 46448.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 24.52 ms | 15.86 ms | 27.87 ms | 304.63 ms | 1431.11 ms | 72532.00 | 
| node (10.12) | [fastify](http://fastify.io) (1.13) | 24.18 ms | 17.65 ms | 32.75 ms | 189.82 ms | 806.72 ms | 40345.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 21.15 ms | 19.50 ms | 38.25 ms | 63.94 ms | 305.41 ms | 14570.67 | 
| node (10.12) | [restify](http://restify.com) (7.2) | 25.77 ms | 20.18 ms | 35.96 ms | 121.27 ms | 742.95 ms | 34052.67 | 
| node (10.12) | [koa](http://koajs.com) (2.6) | 38.39 ms | 24.69 ms | 45.00 ms | 499.64 ms | 1397.17 ms | 85886.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 30.18 ms | 26.28 ms | 50.76 ms | 76.29 ms | 134.85 ms | 14399.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 29.81 ms | 27.68 ms | 41.56 ms | 49.04 ms | 248.67 ms | 7994.33 | 
| node (10.12) | [express](http://expressjs.com) (4.16) | 42.12 ms | 28.36 ms | 54.87 ms | 458.17 ms | 1267.81 ms | 78829.67 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 30.87 ms | 28.59 ms | 44.90 ms | 62.54 ms | 173.58 ms | 9934.33 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 38.31 ms | 34.74 ms | 51.89 ms | 70.08 ms | 417.73 ms | 19556.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 47.24 ms | 35.89 ms | 92.98 ms | 136.74 ms | 611.49 ms | 35404.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 36.11 ms | 37.05 ms | 45.86 ms | 52.69 ms | 183.09 ms | 8200.00 | 
| node (10.12) | [hapi](http://hapijs.com) (17.7) | 67.53 ms | 38.31 ms | 69.65 ms | 1009.29 ms | 2100.01 ms | 160934.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 42.34 ms | 39.28 ms | 55.09 ms | 62.01 ms | 334.05 ms | 12019.00 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 44.91 ms | 40.67 ms | 57.64 ms | 66.80 ms | 428.44 ms | 18158.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 40.72 ms | 42.25 ms | 50.41 ms | 59.95 ms | 410.40 ms | 14588.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 70.32 ms | 55.36 ms | 142.54 ms | 262.17 ms | 483.02 ms | 55808.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 67.94 ms | 58.74 ms | 119.05 ms | 165.10 ms | 299.62 ms | 31607.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 70.64 ms | 61.93 ms | 111.96 ms | 170.27 ms | 689.06 ms | 35781.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 106.51 ms | 99.01 ms | 149.01 ms | 198.23 ms | 526.74 ms | 35854.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (actix-web) (rust)


:three: (vibora) (python)


:four: (jester) (nim)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 254467.00 | 304.49 MB |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 217096.33 | 246.79 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 198927.33 | 225.79 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 186507.33 | 374.99 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 183520.00 | 178.11 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 178522.67 | 287.32 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 167705.67 | 178.84 MB |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 146529.67 | 299.78 MB |
| java (8) | [act](http://actframework.org) (1.8) | 137600.33 | 235.37 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 130242.33 | 75.30 MB |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 113319.33 | 142.75 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 113029.00 | 184.30 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 109500.00 | 192.21 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 106317.67 | 142.68 MB |
| go (1.11) | [iris](http://iris-go.com) (11.0) | 103943.33 | 139.08 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 99369.33 | 132.65 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 98424.00 | 172.72 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 95426.33 | 127.18 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 94142.67 | 165.25 MB |
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 90717.00 | 180.22 MB |
| go (1.11) | [beego](http://beego.me) (1.10) | 88393.00 | 118.66 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 76081.00 | 195.59 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 71845.33 | 67.57 MB |
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 69861.67 | 104.67 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 69645.00 | 171.66 MB |
| python (3.7) | [starlette](http://starlette.io) (0.6) | 59290.67 | 119.29 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 57277.00 | 95.86 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 56466.33 | 280.30 MB |
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 55021.00 | 82.43 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 53071.00 | 113.80 MB |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 52423.00 | 260.27 MB |
| node (10.12) | [fastify](http://fastify.io) (1.13) | 52202.67 | 123.91 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 51469.67 | 90.11 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 46747.33 | 242.75 MB |
| node (10.12) | [restify](http://restify.com) (7.2) | 44122.00 | 77.28 MB |
| c (99) | [kore](http://kore.io) (3.1) | 42694.33 | 115.78 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 37493.00 | 195.37 MB |
| node (10.12) | [koa](http://koajs.com) (2.6) | 35987.67 | 76.02 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 34892.67 | 79.10 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 33010.00 | 30.96 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 32465.33 | 60.28 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 32396.33 | 49.14 MB |
| node (10.12) | [express](http://expressjs.com) (4.16) | 30700.67 | 74.99 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 27676.67 | 25.93 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 26407.00 | 32.40 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 25458.33 | 24.29 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 24706.67 | 40.26 MB |
| node (10.12) | [hapi](http://hapijs.com) (17.7) | 23675.67 | 61.25 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 23383.33 | 42.69 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 22263.67 | 54.87 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 22194.67 | 36.16 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 20320.33 | 11.72 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 15759.00 | 28.08 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 14740.67 | 29.37 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 14081.00 | 40.87 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 13235.33 | 7.63 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 12443.00 | 94.18 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 10964.67 | 28.50 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 8987.33 | 24.03 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 2768.00 | 8.43 MB |
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

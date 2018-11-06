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
Last update: 2018-11-06
```
OS: Linux (version: 4.18.16-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: laravel (php)


:three: lumen (php)


:four: iron (rust)


:five: rack-routing (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 0.09 ms | 0.09 ms | 0.14 ms | 0.19 ms | 4.44 ms | 45.00 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 142.95 ms | 0.47 ms | 366.97 ms | 2561.62 ms | 6970.99 ms | 482472.33 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 216.92 ms | 0.50 ms | 346.98 ms | 4745.55 ms | 7567.31 ms | 798172.00 | 
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 0.60 ms | 0.57 ms | 0.98 ms | 1.65 ms | 48.86 ms | 523.67 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 5.84 ms | 1.06 ms | 17.49 ms | 49.31 ms | 140.78 ms | 10331.67 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 4.83 ms | 1.36 ms | 13.86 ms | 39.33 ms | 134.10 ms | 8176.00 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 9.51 ms | 1.46 ms | 29.35 ms | 79.33 ms | 220.37 ms | 16797.00 | 
| php (7.2) | [symfony](http://symfony.com) (4.1) | 227.47 ms | 1.88 ms | 342.86 ms | 4920.44 ms | 7369.11 ms | 829761.67 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 186.02 ms | 1.92 ms | 339.50 ms | 4100.42 ms | 7222.70 ms | 678204.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 134.41 ms | 2.70 ms | 8.43 ms | 3864.42 ms | 6595.64 ms | 652134.00 | 
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 3.92 ms | 3.08 ms | 8.27 ms | 17.38 ms | 39.23 ms | 3708.67 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 11.04 ms | 3.42 ms | 32.32 ms | 79.52 ms | 221.18 ms | 17064.00 | 
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 4.36 ms | 3.85 ms | 7.89 ms | 16.18 ms | 98.38 ms | 4363.67 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 8.71 ms | 3.87 ms | 23.89 ms | 54.90 ms | 142.56 ms | 11954.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 5.16 ms | 4.09 ms | 10.65 ms | 19.69 ms | 43.62 ms | 4180.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.35 ms | 4.82 ms | 6.08 ms | 10.87 ms | 61.58 ms | 2122.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.26 ms | 5.01 ms | 7.82 ms | 16.45 ms | 158.81 ms | 4408.33 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 5.68 ms | 5.14 ms | 9.92 ms | 17.78 ms | 36.98 ms | 3375.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.51 ms | 5.40 ms | 8.90 ms | 14.36 ms | 65.64 ms | 2853.00 | 
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 6.31 ms | 5.48 ms | 11.50 ms | 19.07 ms | 217.25 ms | 4958.00 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 8.01 ms | 5.98 ms | 14.04 ms | 53.98 ms | 121.74 ms | 9571.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 6.40 ms | 6.32 ms | 10.76 ms | 13.37 ms | 128.13 ms | 3419.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 7.64 ms | 7.16 ms | 10.54 ms | 18.21 ms | 165.62 ms | 4403.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 7.50 ms | 7.26 ms | 11.64 ms | 20.80 ms | 175.61 ms | 4856.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 9.17 ms | 8.47 ms | 13.93 ms | 26.55 ms | 149.06 ms | 4528.00 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 9.63 ms | 8.84 ms | 14.93 ms | 28.85 ms | 120.83 ms | 5076.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 192.08 ms | 9.09 ms | 124.13 ms | 4254.15 ms | 6973.27 ms | 744568.00 | 
| go (1.11) | [beego](http://beego.me) (1.10) | 10.01 ms | 9.11 ms | 15.27 ms | 30.56 ms | 124.91 ms | 5691.00 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 10.08 ms | 9.19 ms | 15.03 ms | 30.85 ms | 217.27 ms | 6429.67 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 10.52 ms | 9.46 ms | 16.00 ms | 32.86 ms | 243.48 ms | 7745.00 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 10.57 ms | 9.52 ms | 16.18 ms | 32.64 ms | 230.49 ms | 7334.33 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 30.00 ms | 9.80 ms | 74.23 ms | 313.42 ms | 445.78 ms | 63968.67 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 13.26 ms | 9.90 ms | 25.97 ms | 47.82 ms | 221.48 ms | 9955.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 14.59 ms | 11.22 ms | 28.36 ms | 49.39 ms | 223.12 ms | 10720.33 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 43.87 ms | 11.58 ms | 138.25 ms | 352.53 ms | 896.87 ms | 75554.33 | 
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 16.92 ms | 12.60 ms | 24.18 ms | 113.78 ms | 572.21 ms | 26560.00 | 
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 19.63 ms | 12.62 ms | 24.38 ms | 220.47 ms | 969.35 ms | 48961.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 13.02 ms | 13.17 ms | 15.49 ms | 17.89 ms | 153.30 ms | 2766.00 | 
| python (3.7) | [starlette](http://starlette.io) (0.6) | 16.48 ms | 13.91 ms | 29.75 ms | 47.74 ms | 135.11 ms | 9700.00 | 
| node (10.12) | [fastify](http://fastify.io) (1.13) | 22.26 ms | 14.52 ms | 27.18 ms | 245.78 ms | 941.04 ms | 50597.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 25.91 ms | 16.33 ms | 28.01 ms | 354.20 ms | 1444.09 ms | 77261.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 45.92 ms | 16.67 ms | 33.89 ms | 1107.28 ms | 2541.70 ms | 195179.67 | 
| node (10.12) | [foxify](http://foxify.js.org) (0.10) | 28.08 ms | 19.39 ms | 38.99 ms | 246.35 ms | 991.04 ms | 52231.67 | 
| node (10.12) | [restify](http://restify.com) (7.2) | 28.09 ms | 23.26 ms | 42.46 ms | 88.56 ms | 539.21 ms | 23578.33 | 
| node (10.12) | [koa](http://koajs.com) (2.6) | 40.56 ms | 24.42 ms | 45.39 ms | 560.78 ms | 1409.48 ms | 94840.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 27.61 ms | 24.47 ms | 45.45 ms | 66.80 ms | 130.25 ms | 12679.33 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 29.23 ms | 27.01 ms | 40.70 ms | 47.62 ms | 180.78 ms | 7124.00 | 
| node (10.12) | [express](http://expressjs.com) (4.16) | 42.04 ms | 27.55 ms | 52.93 ms | 491.19 ms | 1402.69 ms | 85269.67 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 30.36 ms | 28.87 ms | 41.92 ms | 59.07 ms | 236.02 ms | 8941.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 42.51 ms | 34.55 ms | 77.92 ms | 131.31 ms | 373.15 ms | 25564.67 | 
| node (10.12) | [hapi](http://hapijs.com) (17.6) | 67.60 ms | 35.38 ms | 66.34 ms | 1022.11 ms | 2129.20 ms | 167701.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 34.97 ms | 35.56 ms | 44.77 ms | 52.32 ms | 191.99 ms | 8765.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 39.57 ms | 37.55 ms | 50.98 ms | 58.02 ms | 255.37 ms | 8420.67 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 40.61 ms | 39.64 ms | 52.46 ms | 61.12 ms | 412.92 ms | 13562.00 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 37.82 ms | 39.65 ms | 47.85 ms | 56.15 ms | 250.45 ms | 8905.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 45.27 ms | 41.60 ms | 56.55 ms | 65.54 ms | 423.90 ms | 12632.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 61.30 ms | 50.28 ms | 117.77 ms | 185.08 ms | 378.61 ms | 39172.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 76.54 ms | 63.47 ms | 137.25 ms | 191.21 ms | 696.53 ms | 41027.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 67.52 ms | 64.09 ms | 114.78 ms | 166.75 ms | 241.60 ms | 32348.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 101.56 ms | 100.05 ms | 123.27 ms | 174.91 ms | 895.15 ms | 32234.67 | 

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
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 262875.67 | 314.81 MB |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 217941.00 | 247.75 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 210417.67 | 238.84 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 205796.33 | 199.37 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 189904.00 | 381.85 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 177770.33 | 287.39 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 170824.67 | 182.46 MB |
| java (8) | [act](http://actframework.org) (1.8) | 155993.33 | 266.93 MB |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 149674.33 | 306.44 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 144350.67 | 83.44 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 123728.33 | 201.51 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 105749.00 | 141.70 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 102853.00 | 180.67 MB |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 100878.67 | 127.50 MB |
| go (1.11) | [beego](http://beego.me) (1.10) | 99329.00 | 134.51 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 98498.67 | 131.75 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 94589.67 | 166.09 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 92677.67 | 123.70 MB |
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 91492.67 | 181.44 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 87243.33 | 153.18 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 78747.67 | 202.43 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 74946.33 | 70.43 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 71928.00 | 177.08 MB |
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 69267.00 | 103.83 MB |
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 67596.33 | 101.36 MB |
| node (10.12) | [fastify](http://fastify.io) (1.13) | 61696.00 | 151.07 MB |
| python (3.7) | [starlette](http://starlette.io) (0.6) | 61473.67 | 123.60 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 59945.33 | 105.09 MB |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 56360.33 | 279.90 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 56245.00 | 120.79 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 55175.33 | 92.37 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 52664.33 | 261.49 MB |
| c (99) | [kore](http://kore.io) (3.1) | 49227.67 | 133.42 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 45935.67 | 238.31 MB |
| node (10.12) | [foxify](http://foxify.js.org) (0.10) | 44147.67 | 92.66 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 39776.67 | 207.26 MB |
| node (10.12) | [restify](http://restify.com) (7.2) | 37483.67 | 65.57 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 36978.00 | 83.83 MB |
| node (10.12) | [koa](http://koajs.com) (2.6) | 36209.00 | 76.57 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 33842.00 | 31.73 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 32924.67 | 61.08 MB |
| node (10.12) | [express](http://expressjs.com) (4.16) | 31929.67 | 77.95 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 28896.67 | 27.08 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 28604.33 | 42.14 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 26692.67 | 25.43 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 26354.67 | 42.94 MB |
| node (10.12) | [hapi](http://hapijs.com) (17.6) | 25795.00 | 66.72 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 24868.67 | 30.64 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 24683.33 | 60.85 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 24133.67 | 44.03 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 22011.00 | 12.69 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 21678.33 | 35.33 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 16969.67 | 30.24 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 14858.33 | 29.60 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 14721.00 | 8.48 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 13522.67 | 102.20 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 13218.00 | 38.37 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 11607.00 | 30.11 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 9641.67 | 25.71 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 2922.00 | 8.92 MB |
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

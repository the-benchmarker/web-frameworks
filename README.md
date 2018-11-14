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

> jobs are either languages (example : crystal) or frameworks (example : router.cr)

~~~sh
bin/neph [job1] [job2] [job3] ...
~~~

+ Start the benchmark ....

> tools is a list of language / framework to challenge (example : ruby kemal amber go python)

~~~sh
bin/benchmarker [tools]
~~~

## Results

<!-- Result from here -->
Last update: 2018-11-14
```
OS: Linux (version: 4.18.17-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: symfony (php)


:four: laravel (php)


:five: rack-routing (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 0.08 ms | 0.08 ms | 0.12 ms | 0.16 ms | 1.23 ms | 30.33 | 
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 0.45 ms | 0.41 ms | 0.79 ms | 1.27 ms | 14.46 ms | 303.33 | 
| php (7.2) | [symfony](http://symfony.com) (4.1) | 196.83 ms | 0.45 ms | 325.60 ms | 4522.74 ms | 7062.33 ms | 737202.33 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 161.94 ms | 0.47 ms | 392.13 ms | 3021.92 ms | 7115.55 ms | 550733.33 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 5.80 ms | 0.97 ms | 17.49 ms | 49.87 ms | 142.02 ms | 10393.67 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 4.30 ms | 1.22 ms | 12.35 ms | 35.06 ms | 120.69 ms | 7286.33 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 9.37 ms | 1.34 ms | 29.05 ms | 80.07 ms | 221.08 ms | 16860.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 149.39 ms | 2.00 ms | 72.61 ms | 3801.92 ms | 6594.60 ms | 674951.33 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 201.76 ms | 2.14 ms | 324.39 ms | 4602.74 ms | 7362.99 ms | 753440.00 | 
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 3.52 ms | 2.63 ms | 7.00 ms | 15.35 ms | 89.29 ms | 3265.33 | 
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 3.50 ms | 2.70 ms | 7.16 ms | 14.54 ms | 33.54 ms | 3170.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.49 ms | 3.40 ms | 9.38 ms | 17.96 ms | 41.65 ms | 3900.33 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 163.67 ms | 3.43 ms | 242.01 ms | 3842.85 ms | 7120.36 ms | 642571.33 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 10.74 ms | 3.45 ms | 31.17 ms | 76.23 ms | 210.62 ms | 16378.00 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 8.86 ms | 3.70 ms | 24.52 ms | 57.84 ms | 166.53 ms | 12545.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 5.02 ms | 4.52 ms | 9.16 ms | 17.37 ms | 36.35 ms | 3380.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.35 ms | 4.78 ms | 6.10 ms | 10.77 ms | 148.31 ms | 3345.33 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.27 ms | 4.98 ms | 7.89 ms | 16.31 ms | 158.66 ms | 4184.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.27 ms | 5.20 ms | 8.57 ms | 14.47 ms | 38.13 ms | 2768.00 | 
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 6.02 ms | 5.24 ms | 11.04 ms | 18.70 ms | 266.41 ms | 6942.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 6.81 ms | 5.68 ms | 9.58 ms | 18.60 ms | 361.29 ms | 10124.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 6.26 ms | 6.12 ms | 10.61 ms | 13.82 ms | 142.35 ms | 3457.67 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 8.19 ms | 6.28 ms | 14.73 ms | 46.30 ms | 124.86 ms | 8903.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 7.21 ms | 6.65 ms | 11.37 ms | 22.20 ms | 215.69 ms | 5708.33 | 
| go (1.11) | [iris](http://iris-go.com) (11.0) | 8.76 ms | 7.62 ms | 12.79 ms | 28.05 ms | 295.65 ms | 9420.33 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 8.56 ms | 8.08 ms | 12.55 ms | 29.83 ms | 132.51 ms | 4963.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 9.08 ms | 8.16 ms | 14.05 ms | 29.87 ms | 173.83 ms | 5940.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 9.36 ms | 8.54 ms | 13.82 ms | 27.64 ms | 234.97 ms | 7371.00 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 9.55 ms | 8.55 ms | 14.51 ms | 30.97 ms | 245.98 ms | 8163.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 184.11 ms | 8.63 ms | 27.01 ms | 4546.28 ms | 7928.73 ms | 787823.33 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 9.57 ms | 8.76 ms | 14.77 ms | 29.85 ms | 134.57 ms | 5792.67 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 9.55 ms | 8.86 ms | 14.54 ms | 28.46 ms | 173.20 ms | 5609.67 | 
| go (1.11) | [beego](http://beego.me) (1.10) | 9.81 ms | 9.03 ms | 14.98 ms | 30.65 ms | 118.46 ms | 5493.67 | 
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 11.90 ms | 9.26 ms | 17.41 ms | 46.04 ms | 470.50 ms | 18085.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 12.82 ms | 10.37 ms | 21.19 ms | 41.26 ms | 473.60 ms | 12571.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 15.14 ms | 11.34 ms | 27.46 ms | 56.05 ms | 371.56 ms | 15536.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 11.58 ms | 11.57 ms | 13.76 ms | 16.10 ms | 93.24 ms | 2557.33 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 44.12 ms | 12.14 ms | 138.00 ms | 348.34 ms | 791.86 ms | 74904.33 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 19.82 ms | 13.16 ms | 29.59 ms | 154.39 ms | 787.99 ms | 38927.67 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 19.20 ms | 14.00 ms | 28.64 ms | 111.83 ms | 703.70 ms | 31257.67 | 
| node (11.1) | [fastify](http://fastify.io) (1.13) | 20.42 ms | 14.19 ms | 28.13 ms | 170.51 ms | 800.38 ms | 39095.00 | 
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 20.39 ms | 15.12 ms | 27.87 ms | 108.98 ms | 803.22 ms | 34881.33 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 26.20 ms | 16.11 ms | 27.51 ms | 388.31 ms | 1492.36 ms | 81853.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 18.49 ms | 16.80 ms | 33.21 ms | 54.87 ms | 515.46 ms | 15316.67 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 28.23 ms | 19.40 ms | 36.58 ms | 275.18 ms | 1013.46 ms | 54840.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 24.34 ms | 22.42 ms | 39.40 ms | 55.44 ms | 97.42 ms | 11106.00 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 29.13 ms | 24.12 ms | 43.45 ms | 110.93 ms | 626.72 ms | 27674.33 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 41.10 ms | 27.22 ms | 50.86 ms | 488.20 ms | 1450.58 ms | 85732.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 31.45 ms | 28.90 ms | 45.47 ms | 62.44 ms | 215.76 ms | 10070.00 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 35.79 ms | 32.10 ms | 47.59 ms | 107.30 ms | 563.23 ms | 21524.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 32.78 ms | 32.50 ms | 42.21 ms | 51.11 ms | 191.23 ms | 8235.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 40.75 ms | 33.39 ms | 72.12 ms | 97.43 ms | 276.74 ms | 20743.33 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 32.90 ms | 33.49 ms | 41.15 ms | 49.24 ms | 321.69 ms | 10680.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 34.06 ms | 33.53 ms | 44.42 ms | 53.24 ms | 326.03 ms | 10972.67 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 42.78 ms | 36.26 ms | 47.33 ms | 304.07 ms | 755.97 ms | 53003.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 42.42 ms | 38.89 ms | 54.07 ms | 64.23 ms | 565.12 ms | 19577.67 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 73.64 ms | 39.28 ms | 68.51 ms | 1146.25 ms | 2246.25 ms | 182180.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 63.18 ms | 54.35 ms | 110.95 ms | 183.36 ms | 353.62 ms | 35388.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 73.85 ms | 58.22 ms | 139.88 ms | 215.46 ms | 530.61 ms | 43170.67 | 
| go (1.11) | [gf](http://gfer.me) (1.1) | 61.05 ms | 66.33 ms | 113.89 ms | 182.17 ms | 452.81 ms | 46523.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 85.52 ms | 78.85 ms | 150.19 ms | 213.33 ms | 307.75 ms | 46599.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 98.52 ms | 100.40 ms | 119.45 ms | 138.71 ms | 494.37 ms | 24053.00 | 

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
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 287162.00 | 343.70 MB |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 278093.67 | 316.03 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 245549.67 | 278.82 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 204962.67 | 198.99 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 202254.00 | 406.50 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 196910.67 | 210.56 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 177273.67 | 284.90 MB |
| java (8) | [act](http://actframework.org) (1.8) | 172877.33 | 337.62 MB |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 163181.33 | 333.31 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 146559.00 | 238.88 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 137290.00 | 79.34 MB |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 133489.33 | 169.19 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 117366.67 | 205.83 MB |
| go (1.11) | [iris](http://iris-go.com) (11.0) | 116427.33 | 154.72 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 109115.00 | 191.53 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 106064.33 | 142.46 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 105823.67 | 141.22 MB |
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 104989.33 | 208.32 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 104002.33 | 182.67 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 102585.00 | 137.50 MB |
| go (1.11) | [beego](http://beego.me) (1.10) | 102238.00 | 138.01 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 93146.33 | 176.21 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 84614.00 | 79.53 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 81205.67 | 208.66 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 71354.33 | 175.76 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 63854.33 | 153.66 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 63712.33 | 315.80 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 63665.67 | 95.44 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 61420.67 | 91.92 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 58572.67 | 125.65 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 57973.00 | 101.54 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 57551.67 | 120.92 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 56386.00 | 94.07 MB |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 51641.33 | 256.10 MB |
| c (99) | [kore](http://kore.io) (3.1) | 50019.33 | 135.58 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 48382.33 | 251.14 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 45112.67 | 95.38 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 42335.67 | 220.40 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 41332.00 | 93.73 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 38171.33 | 61.67 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 36736.33 | 64.41 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 32342.67 | 79.00 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 31874.67 | 59.11 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 30679.00 | 28.74 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 30506.67 | 28.60 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 29817.33 | 28.45 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 29332.33 | 47.79 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 28170.00 | 34.60 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 26356.00 | 48.10 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 25516.00 | 62.82 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 23448.67 | 38.19 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 23119.00 | 59.70 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 22267.33 | 12.85 MB |
| go (1.11) | [gf](http://gfer.me) (1.1) | 16921.00 | 53.39 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 16264.00 | 29.00 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 14513.33 | 8.37 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 14080.33 | 40.82 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 13686.33 | 103.49 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 11891.00 | 30.85 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 11747.67 | 23.39 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 9927.67 | 26.54 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 2915.33 | 8.86 MB |
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

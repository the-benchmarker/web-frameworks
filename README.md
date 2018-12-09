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
Last update: 2018-12-09
```
OS: Linux (version: 4.19.6-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: laravel (php)


:four: slim (php)


:five: symfony (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 0.09 ms | 0.09 ms | 0.14 ms | 0.19 ms | 4.20 ms | 65.33 | 
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 0.51 ms | 0.48 ms | 0.86 ms | 1.28 ms | 33.77 ms | 361.33 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 189.27 ms | 0.57 ms | 452.94 ms | 3604.44 ms | 7287.10 ms | 629335.67 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 198.75 ms | 0.58 ms | 338.33 ms | 4547.58 ms | 7601.16 ms | 752286.67 | 
| php (7.2) | [symfony](http://symfony.com) (4.2) | 163.75 ms | 0.58 ms | 330.80 ms | 3380.75 ms | 7232.52 ms | 591448.00 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 134.90 ms | 0.58 ms | 298.91 ms | 2630.29 ms | 6390.85 ms | 482351.67 | 
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 180.06 ms | 0.58 ms | 318.11 ms | 4500.34 ms | 7676.50 ms | 712511.00 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 6.16 ms | 1.33 ms | 18.25 ms | 50.70 ms | 152.89 ms | 10616.67 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 4.75 ms | 1.55 ms | 13.40 ms | 37.55 ms | 126.94 ms | 7821.33 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 9.98 ms | 1.86 ms | 30.25 ms | 80.02 ms | 215.01 ms | 16927.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 173.99 ms | 2.36 ms | 132.33 ms | 4345.99 ms | 6594.85 ms | 761578.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 3.55 ms | 2.85 ms | 6.58 ms | 11.16 ms | 28.62 ms | 2421.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.96 ms | 3.01 ms | 8.37 ms | 17.58 ms | 39.53 ms | 3755.00 | 
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 3.90 ms | 3.01 ms | 7.52 ms | 16.21 ms | 133.00 ms | 4047.00 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 11.35 ms | 4.06 ms | 32.49 ms | 76.93 ms | 186.53 ms | 16671.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 5.26 ms | 4.26 ms | 10.75 ms | 19.27 ms | 43.48 ms | 4156.33 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 9.38 ms | 4.45 ms | 25.34 ms | 56.84 ms | 149.97 ms | 12452.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.17 ms | 4.61 ms | 5.95 ms | 10.68 ms | 38.83 ms | 2102.33 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.01 ms | 4.91 ms | 7.52 ms | 15.52 ms | 54.23 ms | 2719.33 | 
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 5.05 ms | 5.01 ms | 6.74 ms | 14.80 ms | 154.95 ms | 3739.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 5.73 ms | 5.12 ms | 10.31 ms | 18.87 ms | 37.70 ms | 3628.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.35 ms | 5.20 ms | 8.83 ms | 15.65 ms | 68.54 ms | 3068.33 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 7.75 ms | 5.41 ms | 14.52 ms | 51.91 ms | 127.30 ms | 9575.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 5.99 ms | 5.88 ms | 10.07 ms | 13.31 ms | 240.14 ms | 3466.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 7.91 ms | 7.27 ms | 10.97 ms | 21.15 ms | 289.83 ms | 6494.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 8.56 ms | 7.56 ms | 12.90 ms | 26.44 ms | 259.33 ms | 6521.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 8.20 ms | 7.64 ms | 13.27 ms | 24.37 ms | 170.01 ms | 5217.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 189.46 ms | 7.96 ms | 439.14 ms | 3682.80 ms | 7454.99 ms | 697823.00 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 8.74 ms | 8.17 ms | 12.87 ms | 29.84 ms | 145.29 ms | 5097.67 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 8.99 ms | 8.21 ms | 13.63 ms | 27.91 ms | 173.97 ms | 5157.67 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 9.45 ms | 8.60 ms | 14.45 ms | 29.23 ms | 222.86 ms | 6112.67 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 9.71 ms | 8.80 ms | 15.07 ms | 30.60 ms | 230.83 ms | 6275.67 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 10.00 ms | 9.14 ms | 15.35 ms | 31.05 ms | 181.90 ms | 5978.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 10.21 ms | 9.32 ms | 15.85 ms | 31.58 ms | 151.63 ms | 6054.67 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 10.25 ms | 9.35 ms | 15.60 ms | 33.10 ms | 222.68 ms | 6496.00 | 
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 14.43 ms | 10.23 ms | 23.86 ms | 68.64 ms | 549.35 ms | 21640.67 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 13.04 ms | 10.85 ms | 19.96 ms | 38.00 ms | 367.11 ms | 12592.33 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 16.79 ms | 11.67 ms | 21.50 ms | 151.10 ms | 770.41 ms | 38300.33 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 46.71 ms | 12.01 ms | 146.47 ms | 348.59 ms | 946.81 ms | 76643.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 12.45 ms | 12.13 ms | 15.14 ms | 23.13 ms | 89.10 ms | 3162.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 14.52 ms | 12.20 ms | 23.94 ms | 43.11 ms | 293.88 ms | 9862.00 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 17.26 ms | 12.63 ms | 26.26 ms | 83.99 ms | 642.66 ms | 27051.00 | 
| go (1.11) | [gf](http://gfer.me) (1.3) | 13.63 ms | 12.82 ms | 19.46 ms | 39.55 ms | 192.75 ms | 7668.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 26.96 ms | 14.19 ms | 25.21 ms | 453.81 ms | 1703.66 ms | 94068.67 | 
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 20.03 ms | 14.59 ms | 27.33 ms | 130.92 ms | 738.23 ms | 34460.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 19.23 ms | 14.93 ms | 30.56 ms | 107.91 ms | 924.41 ms | 37414.33 | 
| node (11.1) | [fastify](http://fastify.io) (1.13) | 27.34 ms | 19.95 ms | 38.34 ms | 204.94 ms | 919.10 ms | 45838.33 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 33.97 ms | 21.21 ms | 40.72 ms | 448.05 ms | 1275.49 ms | 78580.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 29.13 ms | 22.57 ms | 58.87 ms | 97.67 ms | 163.96 ms | 20612.00 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 42.96 ms | 25.52 ms | 48.22 ms | 609.15 ms | 1556.03 ms | 103366.00 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 33.90 ms | 27.36 ms | 52.61 ms | 139.22 ms | 801.13 ms | 36004.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 31.74 ms | 28.06 ms | 40.77 ms | 111.76 ms | 325.00 ms | 16886.67 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 30.54 ms | 28.18 ms | 44.76 ms | 61.32 ms | 235.33 ms | 10399.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 31.85 ms | 29.88 ms | 40.89 ms | 82.72 ms | 192.81 ms | 11600.00 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 33.34 ms | 30.21 ms | 44.01 ms | 99.86 ms | 314.77 ms | 17278.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 42.52 ms | 33.12 ms | 84.47 ms | 122.29 ms | 378.02 ms | 26360.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 37.25 ms | 34.32 ms | 49.64 ms | 88.72 ms | 405.35 ms | 18321.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 45.77 ms | 37.30 ms | 56.11 ms | 300.98 ms | 740.04 ms | 52291.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 40.38 ms | 40.83 ms | 50.69 ms | 60.37 ms | 259.01 ms | 9789.67 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 97.50 ms | 44.33 ms | 82.61 ms | 1585.88 ms | 2741.38 ms | 256517.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 80.02 ms | 49.26 ms | 183.40 ms | 259.19 ms | 572.12 ms | 61643.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 68.48 ms | 58.32 ms | 128.29 ms | 219.51 ms | 433.11 ms | 45551.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 80.84 ms | 81.33 ms | 128.70 ms | 187.91 ms | 258.40 ms | 38524.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 102.65 ms | 99.33 ms | 141.20 ms | 179.66 ms | 743.06 ms | 33380.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (evhtp) (cpp)


:five: (vibora) (python)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 280167.00 | 162.14 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 261340.67 | 312.86 MB |
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 248592.33 | 282.60 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 212763.00 | 206.40 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 204009.33 | 231.75 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 196338.67 | 394.57 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 185825.67 | 298.90 MB |
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 180033.67 | 368.33 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 170569.00 | 182.33 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 151980.33 | 87.69 MB |
| java (8) | [act](http://actframework.org) (1.8) | 151829.33 | 296.36 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 121413.33 | 197.91 MB |
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 116640.67 | 146.70 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 115262.33 | 153.95 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 114778.00 | 201.37 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 109295.67 | 146.71 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 104245.33 | 139.78 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 100929.67 | 177.25 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 99245.33 | 174.29 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 97033.67 | 129.77 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 96385.67 | 129.43 MB |
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 86042.67 | 171.37 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 80122.00 | 75.34 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 79651.67 | 119.40 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 78369.00 | 201.37 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 75914.00 | 113.82 MB |
| go (1.11) | [gf](http://gfer.me) (1.3) | 72770.00 | 110.54 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 70228.00 | 172.83 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 67384.67 | 100.97 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 62617.00 | 109.69 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 61022.33 | 101.97 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 58868.67 | 123.80 MB |
| c (99) | [kore](http://kore.io) (3.1) | 53827.00 | 145.91 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 46357.67 | 110.29 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 41346.67 | 87.34 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 39736.67 | 197.03 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 39468.67 | 85.12 MB |
| php (7.2) | [symfony](http://symfony.com) (4.2) | 38082.33 | 188.78 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 37626.00 | 85.28 MB |
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 36264.67 | 180.09 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 35005.67 | 56.38 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 34670.00 | 84.78 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 34417.00 | 178.47 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 33734.00 | 175.74 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 32596.00 | 60.51 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 32274.00 | 30.28 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 32180.67 | 56.39 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 31846.00 | 29.87 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 30427.33 | 49.60 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 27059.00 | 25.82 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 26256.33 | 32.25 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 24633.67 | 60.72 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 24459.67 | 44.65 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 24392.00 | 39.73 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 20957.67 | 12.09 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 20603.67 | 53.31 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 15322.67 | 27.32 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 14508.33 | 42.09 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 13703.67 | 7.91 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 12833.00 | 97.01 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 12387.33 | 24.69 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 11240.33 | 29.16 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 9373.67 | 25.04 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 2739.33 | 8.36 MB |
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

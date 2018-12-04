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
Last update: 2018-12-05
```
OS: Linux (version: 4.19.5-300.fc29.x86_64, arch: x86_64)
CPU Cores: 4
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: rack-routing (ruby)


:three: iron (rust)


:four: flame (ruby)


:five: roda (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 0.06 ms | 0.05 ms | 0.06 ms | 0.15 ms | 9.09 ms | 119.67 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 2.61 ms | 0.33 ms | 7.05 ms | 35.52 ms | 103.74 ms | 6634.67 | 
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 0.52 ms | 0.35 ms | 0.90 ms | 3.86 ms | 55.52 ms | 859.00 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 4.23 ms | 0.50 ms | 12.66 ms | 42.84 ms | 93.34 ms | 8401.33 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 1.99 ms | 0.81 ms | 3.42 ms | 31.38 ms | 128.35 ms | 5794.67 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 184.98 ms | 1.36 ms | 529.17 ms | 2986.20 ms | 6908.12 ms | 556446.67 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 5.21 ms | 1.59 ms | 13.12 ms | 53.41 ms | 124.64 ms | 9954.67 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 180.69 ms | 1.69 ms | 472.71 ms | 2997.33 ms | 7080.23 ms | 560577.00 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 4.34 ms | 1.96 ms | 8.97 ms | 54.28 ms | 122.64 ms | 9455.33 | 
| php (7.2) | [symfony](http://symfony.com) (4.2) | 118.04 ms | 2.23 ms | 357.33 ms | 1852.52 ms | 6000.27 ms | 366482.00 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 187.89 ms | 2.78 ms | 471.52 ms | 3592.13 ms | 6998.62 ms | 615300.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 130.07 ms | 3.46 ms | 9.96 ms | 3681.01 ms | 6596.52 ms | 629716.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 4.87 ms | 4.22 ms | 7.23 ms | 8.62 ms | 2484.97 ms | 34351.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 6.39 ms | 5.64 ms | 11.38 ms | 18.21 ms | 91.62 ms | 4016.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 5.94 ms | 5.79 ms | 7.13 ms | 10.16 ms | 213.39 ms | 3602.67 | 
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 7.40 ms | 6.28 ms | 13.16 ms | 21.70 ms | 101.96 ms | 4744.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 7.32 ms | 6.80 ms | 10.70 ms | 18.26 ms | 102.57 ms | 3800.33 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 11.63 ms | 7.75 ms | 17.09 ms | 119.48 ms | 373.77 ms | 20779.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 9.69 ms | 7.75 ms | 17.04 ms | 36.75 ms | 196.80 ms | 8233.67 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 24.71 ms | 8.18 ms | 75.44 ms | 129.84 ms | 223.61 ms | 32547.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 8.57 ms | 8.22 ms | 13.99 ms | 21.93 ms | 51.33 ms | 4387.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 183.57 ms | 9.23 ms | 28.31 ms | 4439.21 ms | 7936.27 ms | 771470.67 | 
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 18.95 ms | 9.23 ms | 25.01 ms | 255.30 ms | 450.47 ms | 42613.00 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 12.75 ms | 9.32 ms | 21.87 ms | 90.99 ms | 134.00 ms | 14188.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 10.18 ms | 9.48 ms | 18.09 ms | 28.84 ms | 104.67 ms | 6338.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 11.61 ms | 10.29 ms | 16.08 ms | 31.94 ms | 196.34 ms | 6304.67 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 19.07 ms | 12.46 ms | 31.17 ms | 185.99 ms | 504.50 ms | 33689.67 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 17.07 ms | 12.79 ms | 29.80 ms | 136.17 ms | 392.74 ms | 24431.33 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 18.93 ms | 13.51 ms | 32.68 ms | 173.40 ms | 494.67 ms | 30189.00 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 18.02 ms | 13.51 ms | 30.46 ms | 144.73 ms | 526.84 ms | 27029.67 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 17.65 ms | 13.57 ms | 31.44 ms | 127.50 ms | 347.74 ms | 22914.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 15.55 ms | 13.68 ms | 20.76 ms | 57.81 ms | 281.95 ms | 11375.67 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 17.36 ms | 13.81 ms | 31.09 ms | 119.68 ms | 377.32 ms | 21943.67 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 20.28 ms | 14.43 ms | 32.28 ms | 175.23 ms | 564.83 ms | 29818.00 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 16.79 ms | 15.37 ms | 22.19 ms | 44.99 ms | 360.44 ms | 8204.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 17.95 ms | 16.43 ms | 21.23 ms | 25.12 ms | 159.72 ms | 4578.33 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 25.24 ms | 17.29 ms | 48.41 ms | 89.64 ms | 261.30 ms | 18750.33 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 19.93 ms | 19.05 ms | 25.24 ms | 26.48 ms | 149.60 ms | 5266.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 20.90 ms | 19.07 ms | 27.68 ms | 29.67 ms | 99.88 ms | 4568.00 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 55.88 ms | 19.18 ms | 52.50 ms | 1080.02 ms | 2255.69 ms | 179457.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 20.73 ms | 19.96 ms | 25.46 ms | 33.72 ms | 61.98 ms | 4083.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 21.01 ms | 20.43 ms | 26.79 ms | 28.41 ms | 173.05 ms | 5859.67 | 
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 43.85 ms | 20.86 ms | 53.05 ms | 764.09 ms | 2086.11 ms | 133107.33 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 75.48 ms | 22.48 ms | 60.77 ms | 1527.62 ms | 2853.70 ms | 251751.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 24.44 ms | 22.80 ms | 31.18 ms | 34.80 ms | 122.83 ms | 5119.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 199.70 ms | 23.39 ms | 123.68 ms | 4327.59 ms | 6857.08 ms | 737930.33 | 
| go (1.11) | [gf](http://gfer.me) (1.2) | 29.99 ms | 24.19 ms | 57.27 ms | 177.01 ms | 479.91 ms | 33250.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 28.30 ms | 24.19 ms | 41.26 ms | 104.96 ms | 591.82 ms | 27544.00 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 24.25 ms | 24.41 ms | 28.33 ms | 29.86 ms | 169.44 ms | 4480.33 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 63.82 ms | 24.75 ms | 58.71 ms | 1263.08 ms | 2680.31 ms | 205668.67 | 
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 90.04 ms | 26.03 ms | 70.56 ms | 1799.39 ms | 3298.96 ms | 297251.67 | 
| node (11.1) | [fastify](http://fastify.io) (1.13) | 100.25 ms | 26.40 ms | 74.21 ms | 1853.57 ms | 3257.16 ms | 316518.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 37.89 ms | 27.08 ms | 53.35 ms | 281.45 ms | 2403.95 ms | 89018.33 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 137.26 ms | 32.53 ms | 91.32 ms | 2550.37 ms | 3933.05 ms | 433503.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 37.18 ms | 36.12 ms | 46.72 ms | 63.06 ms | 320.37 ms | 10271.67 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 73.36 ms | 39.64 ms | 82.37 ms | 1071.06 ms | 1984.68 ms | 168436.67 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 295.30 ms | 54.32 ms | 683.45 ms | 4353.37 ms | 6517.60 ms | 815096.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 51.92 ms | 54.99 ms | 66.16 ms | 83.58 ms | 722.69 ms | 24721.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 71.08 ms | 64.32 ms | 103.59 ms | 132.76 ms | 761.74 ms | 31753.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 88.56 ms | 72.18 ms | 156.88 ms | 234.92 ms | 463.13 ms | 49069.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 122.43 ms | 114.99 ms | 158.82 ms | 261.07 ms | 1220.82 ms | 57598.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 143.72 ms | 131.43 ms | 220.79 ms | 293.27 ms | 526.63 ms | 62077.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 243.37 ms | 172.84 ms | 230.73 ms | 2589.16 ms | 4911.24 ms | 424506.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (fasthttprouter) (go)


:four: (actix-web) (rust)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 165301.67 | 95.58 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 142404.33 | 170.33 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 124725.33 | 200.52 MB |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 124099.67 | 140.88 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 108102.67 | 104.95 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 107864.33 | 115.35 MB |
| java (8) | [act](http://actframework.org) (1.8) | 107783.33 | 210.13 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 107629.33 | 122.17 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 99731.00 | 200.43 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 92413.67 | 53.45 MB |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 88927.33 | 181.02 MB |
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 81402.33 | 161.89 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 76957.67 | 102.73 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 74951.67 | 131.56 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 72946.33 | 97.41 MB |
| c (99) | [kore](http://kore.io) (3.1) | 72771.00 | 197.50 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 70079.33 | 123.01 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 69970.67 | 93.33 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 69600.33 | 93.16 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 66319.33 | 107.97 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 63544.33 | 83.73 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 60880.67 | 106.67 MB |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 60661.67 | 77.17 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 55517.67 | 52.08 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 49955.33 | 46.83 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 49111.00 | 46.16 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 47611.33 | 58.55 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 47520.00 | 77.44 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 46454.67 | 230.92 MB |
| php (7.2) | [symfony](http://symfony.com) (4.2) | 45258.33 | 224.80 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 44935.33 | 96.46 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 42813.00 | 109.83 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 41202.33 | 75.28 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 41194.67 | 61.61 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 40615.33 | 66.23 MB |
| go (1.11) | [gf](http://gfer.me) (1.2) | 40528.67 | 60.50 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 39299.00 | 204.11 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 38876.33 | 58.18 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 37985.33 | 93.36 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 36935.33 | 55.25 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 33831.67 | 59.23 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 33421.00 | 31.85 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 32968.67 | 69.64 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 32529.00 | 78.92 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 31625.33 | 164.69 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 31519.33 | 66.09 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 31280.33 | 52.35 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 27177.67 | 50.47 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 25926.00 | 63.29 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 24761.33 | 14.28 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 22018.00 | 38.55 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 21558.00 | 35.33 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 19282.33 | 43.63 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 15441.67 | 39.87 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 15270.33 | 8.80 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 14827.33 | 112.28 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 13849.67 | 34.04 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 12419.33 | 32.21 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 11547.33 | 20.58 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 8162.67 | 23.64 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 6861.33 | 13.68 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 5315.00 | 14.26 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 2582.67 | 7.92 MB |
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

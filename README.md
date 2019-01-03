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
Last update: 2019-01-03
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: rack-routing (ruby)


:three: roda (ruby)


:four: slim (php)


:five: zend-expressive (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 0.08 ms | 0.08 ms | 0.13 ms | 0.16 ms | 1.48 ms | 35.33 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.00 ms | 0.19 ms | 15.20 ms | 34.66 ms | 86.25 ms | 7898.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.15) | 4.12 ms | 0.23 ms | 14.47 ms | 32.82 ms | 106.13 ms | 7595.33 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 170.25 ms | 0.37 ms | 318.48 ms | 3484.76 ms | 7127.26 ms | 614682.33 | 
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 136.28 ms | 0.39 ms | 272.77 ms | 3014.60 ms | 6828.98 ms | 517820.67 | 
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 0.41 ms | 0.41 ms | 0.66 ms | 0.93 ms | 10.74 ms | 238.00 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 170.87 ms | 0.42 ms | 381.75 ms | 3451.78 ms | 6935.58 ms | 594161.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 7.37 ms | 0.44 ms | 24.59 ms | 50.58 ms | 137.93 ms | 12014.67 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 8.75 ms | 0.46 ms | 29.64 ms | 61.88 ms | 154.04 ms | 14743.00 | 
| php (7.2) | [symfony](http://symfony.com) (4.2) | 158.53 ms | 0.49 ms | 321.24 ms | 3230.50 ms | 6808.79 ms | 566562.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 8.89 ms | 0.57 ms | 28.26 ms | 55.91 ms | 148.70 ms | 13572.00 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 162.80 ms | 0.59 ms | 295.20 ms | 3559.41 ms | 7272.86 ms | 614162.33 | 
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 109.78 ms | 0.78 ms | 278.23 ms | 2049.49 ms | 6745.83 ms | 383953.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 127.31 ms | 1.77 ms | 14.63 ms | 3656.03 ms | 6593.67 ms | 623002.33 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.36 ms | 2.83 ms | 6.64 ms | 12.66 ms | 31.21 ms | 2843.33 | 
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 3.78 ms | 2.93 ms | 7.74 ms | 16.10 ms | 151.55 ms | 4400.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 3.86 ms | 3.69 ms | 6.95 ms | 10.26 ms | 75.96 ms | 3319.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.85 ms | 3.92 ms | 9.88 ms | 17.57 ms | 41.27 ms | 3838.00 | 
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 4.58 ms | 4.23 ms | 6.14 ms | 11.76 ms | 217.32 ms | 5986.33 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.61 ms | 4.41 ms | 7.03 ms | 14.57 ms | 54.34 ms | 2654.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.17 ms | 4.63 ms | 5.86 ms | 9.86 ms | 140.85 ms | 2967.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 5.22 ms | 4.70 ms | 9.56 ms | 17.30 ms | 42.70 ms | 3419.00 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.5) | 7.26 ms | 4.79 ms | 12.49 ms | 68.16 ms | 119.19 ms | 10904.00 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 5.03 ms | 4.82 ms | 8.34 ms | 15.88 ms | 34.34 ms | 2922.00 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 41.47 ms | 4.88 ms | 137.86 ms | 358.69 ms | 921.80 ms | 77064.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.43 ms | 5.43 ms | 8.57 ms | 14.99 ms | 48.80 ms | 2944.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 7.06 ms | 5.64 ms | 9.93 ms | 13.62 ms | 493.18 ms | 20158.67 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 7.03 ms | 5.82 ms | 11.44 ms | 22.82 ms | 115.31 ms | 4381.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.88 ms | 6.21 ms | 11.38 ms | 20.97 ms | 104.64 ms | 4428.00 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 7.48 ms | 6.23 ms | 11.96 ms | 23.85 ms | 161.12 ms | 4753.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 7.24 ms | 6.27 ms | 10.09 ms | 19.88 ms | 262.77 ms | 7906.67 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 8.24 ms | 6.47 ms | 12.86 ms | 27.91 ms | 367.99 ms | 11412.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 8.38 ms | 6.93 ms | 13.73 ms | 27.13 ms | 192.05 ms | 6743.67 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 8.48 ms | 7.19 ms | 13.90 ms | 27.22 ms | 151.77 ms | 5274.33 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 8.71 ms | 7.27 ms | 13.89 ms | 28.16 ms | 188.59 ms | 7687.00 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 8.26 ms | 7.30 ms | 11.51 ms | 33.82 ms | 155.03 ms | 6460.00 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 8.69 ms | 7.50 ms | 14.13 ms | 27.14 ms | 118.16 ms | 5190.67 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 10.95 ms | 8.10 ms | 23.14 ms | 45.11 ms | 123.75 ms | 9170.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 123.23 ms | 8.47 ms | 37.81 ms | 3042.99 ms | 6004.27 ms | 532590.67 | 
| go (1.11) | [gf](http://gfer.me) (1.3) | 10.21 ms | 9.41 ms | 14.53 ms | 29.16 ms | 239.52 ms | 6724.33 | 
| node (11.2) | [restana](http://github.com/jkyberneees/ana) (2.3) | 13.81 ms | 9.96 ms | 25.51 ms | 56.64 ms | 445.45 ms | 15471.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 11.51 ms | 10.31 ms | 18.05 ms | 31.22 ms | 359.37 ms | 10458.67 | 
| node (11.2) | [rayo](http://rayo.js.org) (1.2) | 16.91 ms | 11.46 ms | 28.70 ms | 89.68 ms | 695.86 ms | 29450.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 11.89 ms | 12.02 ms | 13.96 ms | 16.21 ms | 91.52 ms | 2852.00 | 
| node (11.2) | [polka](http://github.com/lukeed/polka) (0.5) | 18.43 ms | 12.48 ms | 29.78 ms | 111.98 ms | 743.15 ms | 33516.33 | 
| node (11.2) | [foxify](http://foxify.js.org) (0.10) | 17.33 ms | 12.68 ms | 28.75 ms | 67.42 ms | 625.63 ms | 24066.00 | 
| node (11.2) | [fastify](http://fastify.io) (1.13) | 21.59 ms | 13.21 ms | 28.12 ms | 254.47 ms | 1016.33 ms | 54975.33 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 18.94 ms | 13.38 ms | 22.96 ms | 180.83 ms | 1070.98 ms | 50738.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 49.66 ms | 15.76 ms | 33.03 ms | 1184.63 ms | 3235.38 ms | 222343.67 | 
| node (11.2) | [express](http://expressjs.com) (4.16) | 19.96 ms | 15.84 ms | 31.35 ms | 67.99 ms | 646.66 ms | 24415.67 | 
| node (11.2) | [koa](http://koajs.com) (2.6) | 25.25 ms | 16.13 ms | 33.66 ms | 278.76 ms | 1049.77 ms | 58103.33 | 
| node (11.2) | [restify](http://restify.com) (7.2) | 22.11 ms | 18.25 ms | 36.18 ms | 69.38 ms | 446.08 ms | 18116.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 24.70 ms | 22.87 ms | 40.07 ms | 53.49 ms | 84.51 ms | 10686.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 26.55 ms | 23.89 ms | 37.44 ms | 53.12 ms | 238.87 ms | 9235.67 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 25.99 ms | 24.21 ms | 36.05 ms | 52.28 ms | 232.16 ms | 8599.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 30.28 ms | 28.58 ms | 42.45 ms | 51.32 ms | 326.08 ms | 12655.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 37.35 ms | 30.13 ms | 70.56 ms | 119.73 ms | 334.31 ms | 23798.00 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 34.57 ms | 30.91 ms | 50.99 ms | 62.84 ms | 445.90 ms | 14121.33 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 32.42 ms | 32.15 ms | 44.86 ms | 52.78 ms | 189.01 ms | 9570.33 | 
| node (11.2) | [hapi](http://hapijs.com) (17.7) | 55.58 ms | 33.05 ms | 57.95 ms | 776.92 ms | 1760.65 ms | 127879.33 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 39.52 ms | 39.78 ms | 50.94 ms | 57.95 ms | 481.87 ms | 16290.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 51.92 ms | 45.40 ms | 91.48 ms | 155.84 ms | 306.95 ms | 31215.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 50.46 ms | 47.08 ms | 67.29 ms | 82.98 ms | 522.78 ms | 23500.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 60.28 ms | 53.88 ms | 86.48 ms | 138.32 ms | 517.62 ms | 24416.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 62.72 ms | 55.28 ms | 103.87 ms | 147.07 ms | 210.39 ms | 28918.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 92.79 ms | 91.89 ms | 119.41 ms | 138.67 ms | 443.53 ms | 22691.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (actix-web) (rust)


:three: (agoo-c) (c)


:four: (vibora) (python)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 292092.67 | 349.53 MB |
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 263478.00 | 299.43 MB |
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 259155.67 | 149.96 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 219783.33 | 249.68 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 213893.33 | 207.68 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 201892.00 | 325.68 MB |
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 197518.00 | 404.36 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 190083.33 | 381.90 MB |
| java (8) | [act](http://actframework.org) (1.8) | 190021.33 | 370.77 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 186378.00 | 199.00 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 186021.00 | 174.95 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.5) | 176883.67 | 102.33 MB |
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 146282.00 | 184.92 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 138634.00 | 186.41 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 134955.67 | 220.02 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 130439.33 | 174.18 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 125628.33 | 220.55 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 124979.00 | 166.76 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 119149.33 | 209.27 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 116193.67 | 204.01 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 114939.67 | 155.06 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 112989.67 | 150.67 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 100243.33 | 257.53 MB |
| go (1.11) | [gf](http://gfer.me) (1.3) | 98513.00 | 149.85 MB |
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 92218.67 | 183.32 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 87595.67 | 215.79 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 82114.33 | 77.21 MB |
| node (11.2) | [restana](http://github.com/jkyberneees/ana) (2.3) | 80529.33 | 120.52 MB |
| node (11.2) | [rayo](http://rayo.js.org) (1.2) | 72025.33 | 107.94 MB |
| node (11.2) | [fastify](http://fastify.io) (1.13) | 68984.67 | 164.70 MB |
| node (11.2) | [polka](http://github.com/lukeed/polka) (0.5) | 67785.00 | 101.48 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 66961.00 | 112.31 MB |
| node (11.2) | [foxify](http://foxify.js.org) (0.10) | 66642.33 | 140.04 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 60415.33 | 105.61 MB |
| php (7.2) | [symfony](http://symfony.com) (4.2) | 58888.33 | 292.72 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 58850.33 | 126.15 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 57415.33 | 285.24 MB |
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 56226.33 | 280.32 MB |
| node (11.2) | [express](http://expressjs.com) (4.16) | 54510.33 | 133.14 MB |
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 54403.33 | 270.24 MB |
| node (11.2) | [koa](http://koajs.com) (2.6) | 54282.00 | 114.53 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 52607.00 | 273.46 MB |
| c (99) | [kore](http://kore.io) (3.1) | 51552.33 | 139.69 MB |
| node (11.2) | [restify](http://restify.com) (7.2) | 47466.67 | 83.03 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 43307.33 | 225.75 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 42797.33 | 68.61 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 40540.00 | 91.97 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 38560.00 | 71.51 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 37280.33 | 60.76 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 33608.00 | 31.50 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 32033.33 | 18.48 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 31229.00 | 29.27 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.15) | 31109.33 | 29.67 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 28221.33 | 51.51 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 27921.00 | 68.71 MB |
| node (11.2) | [hapi](http://hapijs.com) (17.7) | 27861.00 | 72.10 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 25194.00 | 30.87 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 21081.33 | 34.33 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 19990.00 | 35.63 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 17565.67 | 10.13 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 16404.33 | 47.60 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 15979.33 | 31.84 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 14619.67 | 110.60 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 14382.33 | 37.29 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 10375.33 | 30.72 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3073.00 | 9.45 MB |
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

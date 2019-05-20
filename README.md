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

+ Helping decide between languages, depending on use case
+ Learning languages, best practices, devops culture ...
+ Having fun :heart:

## Requirements

+ [Crystal](https://crystal-lang.org) as `built-in` tools are made in this language
+ [Docker](https://www.docker.com) as **frameworks** are `isolated` into _containers_
+ [wrk](https://github.com/wg/wrk) as benchmarking tool, `>= 4.1.0`

:information_source: you need `wrk` **stable**

~~~sh
git clone --branch 4.1.0 https://github.com/wg/wrk
~~~

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
Last update: 2019-05-20
```
OS: Linux (version: 5.0.9-301.fc30.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: iron (rust)


:five: lumen (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 0.19 ms | 0.13 ms | 0.25 ms | 1.21 ms | 24.88 ms | 513.67 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 8.81 ms | 0.45 ms | 33.35 ms | 74.17 ms | 202.02 ms | 17020.67 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 10.98 ms | 0.56 ms | 41.87 ms | 88.23 ms | 230.13 ms | 20814.67 | 
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 0.85 ms | 0.59 ms | 1.28 ms | 6.24 ms | 194.47 ms | 2247.00 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 116.50 ms | 1.44 ms | 241.07 ms | 1993.87 ms | 6731.46 ms | 418200.33 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 19.41 ms | 1.45 ms | 64.89 ms | 125.19 ms | 291.25 ms | 30862.00 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 18.44 ms | 1.50 ms | 60.38 ms | 122.40 ms | 347.13 ms | 29356.00 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 118.39 ms | 1.54 ms | 288.22 ms | 2254.77 ms | 6271.14 ms | 417808.00 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 118.99 ms | 1.75 ms | 289.29 ms | 1935.07 ms | 7446.27 ms | 397721.67 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 147.80 ms | 1.90 ms | 353.68 ms | 2355.27 ms | 7459.65 ms | 510645.33 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 23.78 ms | 1.99 ms | 76.98 ms | 149.83 ms | 327.07 ms | 36537.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 76.93 ms | 3.78 ms | 12.31 ms | 2459.49 ms | 4682.76 ms | 400522.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 6.99 ms | 4.87 ms | 16.13 ms | 39.44 ms | 151.88 ms | 8527.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 7.01 ms | 5.35 ms | 15.40 ms | 34.68 ms | 129.13 ms | 7602.67 | 
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 7.69 ms | 5.69 ms | 16.24 ms | 38.94 ms | 169.89 ms | 8343.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 7.86 ms | 5.97 ms | 14.46 ms | 31.11 ms | 148.57 ms | 6082.33 | 
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 8.40 ms | 6.93 ms | 14.34 ms | 27.61 ms | 82.60 ms | 5152.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 10.65 ms | 7.12 ms | 21.91 ms | 58.14 ms | 409.82 ms | 16209.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 10.37 ms | 7.43 ms | 21.44 ms | 46.45 ms | 156.60 ms | 9690.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 9.05 ms | 7.70 ms | 15.16 ms | 31.34 ms | 129.95 ms | 5911.67 | 
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 10.84 ms | 8.06 ms | 21.07 ms | 44.42 ms | 197.75 ms | 9593.33 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 10.27 ms | 8.49 ms | 17.41 ms | 36.13 ms | 112.77 ms | 6579.00 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 216.64 ms | 8.62 ms | 570.04 ms | 3741.36 ms | 7717.22 ms | 699643.33 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 12.83 ms | 8.99 ms | 25.80 ms | 84.57 ms | 141.64 ms | 14727.67 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 71.63 ms | 9.19 ms | 232.03 ms | 593.84 ms | 1663.41 ms | 128284.33 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 15.97 ms | 10.98 ms | 33.69 ms | 75.03 ms | 217.84 ms | 14903.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 13.16 ms | 11.37 ms | 20.29 ms | 44.38 ms | 292.93 ms | 9513.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 15.87 ms | 11.58 ms | 26.79 ms | 105.74 ms | 341.96 ms | 18573.33 | 
| node (12.2) | [restana](http://github.com/jkyberneees/ana) (3.0) | 19.00 ms | 12.08 ms | 24.72 ms | 210.55 ms | 788.32 ms | 43771.00 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 18.02 ms | 12.12 ms | 37.64 ms | 89.73 ms | 230.36 ms | 17539.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 214.22 ms | 12.32 ms | 193.10 ms | 4421.88 ms | 7871.78 ms | 790825.67 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 17.55 ms | 12.53 ms | 33.25 ms | 90.45 ms | 252.75 ms | 16840.00 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 18.41 ms | 12.79 ms | 37.74 ms | 90.93 ms | 236.06 ms | 17332.00 | 
| node (12.2) | [polka](http://github.com/lukeed/polka) (0.5) | 18.38 ms | 13.76 ms | 26.64 ms | 95.84 ms | 664.03 ms | 29339.00 | 
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 17.39 ms | 14.06 ms | 28.60 ms | 59.23 ms | 305.52 ms | 13547.67 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 19.21 ms | 14.13 ms | 36.72 ms | 91.27 ms | 263.27 ms | 17437.67 | 
| node (12.2) | [rayo](http://rayo.js.org) (1.3) | 21.34 ms | 14.31 ms | 29.64 ms | 192.88 ms | 879.25 ms | 44271.33 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 151.92 ms | 14.97 ms | 347.43 ms | 2261.86 ms | 6820.07 ms | 455182.33 | 
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 28.44 ms | 16.24 ms | 49.63 ms | 277.12 ms | 513.96 ms | 48758.67 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 24.70 ms | 16.30 ms | 54.54 ms | 121.98 ms | 281.03 ms | 24574.00 | 
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 27.01 ms | 17.74 ms | 59.68 ms | 139.78 ms | 336.07 ms | 27827.67 | 
| node (12.2) | [foxify](http://foxify.js.org) (0.10) | 29.54 ms | 18.05 ms | 37.03 ms | 380.21 ms | 1151.68 ms | 68847.67 | 
| go (1.12) | [gf](http://goframe.org) (1.6) | 32.74 ms | 18.42 ms | 77.11 ms | 217.86 ms | 516.62 ms | 42566.33 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 29.45 ms | 19.53 ms | 63.81 ms | 155.30 ms | 353.64 ms | 30292.33 | 
| node (12.2) | [fastify](http://fastify.io) (2.3) | 40.63 ms | 20.81 ms | 40.55 ms | 626.41 ms | 1466.31 ms | 107122.00 | 
| node (12.2) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 45.24 ms | 21.97 ms | 52.12 ms | 779.59 ms | 1675.51 ms | 125062.33 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 36.23 ms | 22.05 ms | 45.25 ms | 435.01 ms | 1727.81 ms | 92102.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 24.49 ms | 23.20 ms | 33.94 ms | 51.63 ms | 101.78 ms | 7940.33 | 
| node (12.2) | [koa](http://koajs.com) (2.7) | 42.46 ms | 23.36 ms | 54.88 ms | 590.75 ms | 1505.39 ms | 99529.00 | 
| node (12.2) | [express](http://expressjs.com) (4.16) | 33.74 ms | 23.38 ms | 45.55 ms | 309.22 ms | 1082.85 ms | 59066.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 30.25 ms | 24.02 ms | 57.64 ms | 112.25 ms | 260.37 ms | 22483.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 29.69 ms | 25.48 ms | 50.30 ms | 91.71 ms | 924.99 ms | 38003.00 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 35.32 ms | 30.24 ms | 59.99 ms | 112.95 ms | 313.51 ms | 20704.67 | 
| node (12.2) | [restify](http://restify.com) (8.2) | 39.92 ms | 31.19 ms | 61.62 ms | 192.69 ms | 927.61 ms | 43203.00 | 
| python (3.7) | [hug](http://hug.rest) (2.5) | 43.54 ms | 37.09 ms | 76.50 ms | 147.59 ms | 495.95 ms | 29217.67 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.22) | 49.53 ms | 43.42 ms | 91.32 ms | 147.84 ms | 305.49 ms | 31638.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 52.36 ms | 45.06 ms | 92.33 ms | 142.85 ms | 256.89 ms | 29880.33 | 
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 48.25 ms | 45.57 ms | 63.53 ms | 96.38 ms | 412.58 ms | 14637.00 | 
| node (12.2) | [hapi](http://hapijs.com) (18.1) | 106.02 ms | 48.23 ms | 101.00 ms | 1637.48 ms | 2892.41 ms | 263596.00 | 
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 51.07 ms | 49.02 ms | 64.54 ms | 90.70 ms | 500.19 ms | 22494.33 | 
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 55.91 ms | 50.36 ms | 72.05 ms | 173.35 ms | 1302.49 ms | 53644.33 | 
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 125.51 ms | 50.80 ms | 85.34 ms | 2312.77 ms | 4794.90 ms | 389717.67 | 
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 60.35 ms | 54.12 ms | 83.72 ms | 141.47 ms | 586.07 ms | 28867.00 | 
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 60.43 ms | 56.04 ms | 81.31 ms | 154.74 ms | 260.13 ms | 23040.33 | 
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 59.17 ms | 57.14 ms | 75.94 ms | 101.31 ms | 296.77 ms | 17114.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 69.63 ms | 57.93 ms | 115.00 ms | 221.33 ms | 687.68 ms | 39901.00 | 
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 65.85 ms | 59.59 ms | 97.61 ms | 165.14 ms | 277.20 ms | 26661.00 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 81.60 ms | 65.39 ms | 149.65 ms | 255.22 ms | 596.38 ms | 50368.33 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 75.97 ms | 68.93 ms | 125.78 ms | 203.09 ms | 409.80 ms | 39216.67 | 
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 89.25 ms | 69.01 ms | 224.21 ms | 421.09 ms | 994.93 ms | 102272.67 | 
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 84.00 ms | 77.23 ms | 135.66 ms | 199.21 ms | 378.36 ms | 39792.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 108.33 ms | 93.18 ms | 189.94 ms | 333.52 ms | 556.53 ms | 64524.00 | 
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 108.25 ms | 104.61 ms | 159.07 ms | 220.65 ms | 378.84 ms | 39980.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 130.00 ms | 123.89 ms | 201.41 ms | 308.37 ms | 470.33 ms | 55950.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 157.13 ms | 142.41 ms | 239.10 ms | 364.46 ms | 882.51 ms | 62098.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 154.03 ms | 145.70 ms | 229.12 ms | 303.01 ms | 504.54 ms | 51801.67 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 198.44 ms | 191.43 ms | 340.00 ms | 457.06 ms | 619.91 ms | 102198.00 | 
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 472.46 ms | 453.44 ms | 630.09 ms | 1548.41 ms | 3131.95 ms | 248286.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (evhtp) (cpp)


:five: (jester) (nim)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 166459.67 | 96.42 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 159241.33 | 190.62 MB |
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 138384.33 | 157.33 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 126352.00 | 122.64 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 121601.33 | 244.56 MB |
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 117969.67 | 125.96 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 113976.00 | 129.41 MB |
| c (99) | [kore](http://kore.io) (3.1) | 113129.33 | 294.06 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 96462.00 | 55.78 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 95040.00 | 153.47 MB |
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 94751.33 | 194.22 MB |
| java (8) | [act](http://actframework.org) (1.8) | 92272.00 | 159.43 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 73035.00 | 118.98 MB |
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 72916.33 | 91.42 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 71280.33 | 125.20 MB |
| node (12.2) | [restana](http://github.com/jkyberneees/ana) (3.0) | 70355.33 | 105.46 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 63861.00 | 112.18 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 63519.67 | 84.38 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 63237.00 | 86.49 MB |
| node (12.2) | [polka](http://github.com/lukeed/polka) (0.5) | 61211.67 | 91.74 MB |
| node (12.2) | [rayo](http://rayo.js.org) (1.3) | 59416.00 | 89.04 MB |
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 58715.00 | 137.60 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 57515.00 | 77.00 MB |
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 52858.67 | 82.39 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 49637.00 | 67.54 MB |
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 47524.33 | 94.76 MB |
| node (12.2) | [foxify](http://foxify.js.org) (0.10) | 46740.33 | 98.29 MB |
| node (12.2) | [fastify](http://fastify.io) (2.3) | 45384.67 | 115.51 MB |
| go (1.12) | [gf](http://goframe.org) (1.6) | 44932.67 | 67.26 MB |
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 44682.33 | 58.63 MB |
| node (12.2) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 42328.67 | 63.44 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 39929.00 | 53.27 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 39808.00 | 69.83 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 39635.00 | 37.30 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 37998.00 | 63.24 MB |
| node (12.2) | [express](http://expressjs.com) (4.16) | 36535.33 | 89.41 MB |
| node (12.2) | [koa](http://koajs.com) (2.7) | 35405.33 | 75.01 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 34943.33 | 86.15 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 34385.00 | 73.82 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 29395.00 | 63.51 MB |
| node (12.2) | [restify](http://restify.com) (8.2) | 28658.33 | 50.30 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 23319.33 | 115.99 MB |
| python (3.7) | [hug](http://hug.rest) (2.5) | 23216.00 | 57.59 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 22433.67 | 116.66 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 22338.33 | 111.19 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 21846.33 | 34.48 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 21386.00 | 106.43 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.22) | 21086.33 | 45.56 MB |
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 20055.00 | 18.86 MB |
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 19776.33 | 32.27 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 19548.67 | 44.34 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 19125.67 | 35.60 MB |
| node (12.2) | [hapi](http://hapijs.com) (18.1) | 18389.33 | 47.84 MB |
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 17847.67 | 33.76 MB |
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 17172.67 | 22.10 MB |
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 16696.67 | 27.26 MB |
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 16670.00 | 30.52 MB |
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 16459.67 | 20.50 MB |
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 14937.67 | 18.35 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 14736.00 | 36.32 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 14647.33 | 13.99 MB |
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 14160.00 | 13.31 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 13104.67 | 25.32 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 12947.33 | 24.11 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 12512.33 | 65.08 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 11624.67 | 6.73 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 9893.67 | 49.28 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 9518.00 | 17.00 MB |
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 9200.00 | 23.71 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 7674.33 | 15.32 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 7389.67 | 4.27 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 6603.00 | 50.05 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 6270.33 | 18.51 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 6223.00 | 18.06 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 5537.33 | 14.41 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 5005.00 | 10.94 MB |
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 2027.67 | 5.00 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 1778.33 | 5.50 MB |
<!-- Result till here -->

## How to contribute ?

In any way you want ...

+ Request a framework addition
+ Report a bug (on any implementation)
+ Suggest an idea
+ ...

Any kind of idea is :heart:

## Contributors

- [Taichiro Suzuki](https://github.com/tbrand) - Author | Maintainer
- [OvermindDL1](https://github.com/OvermindDL1) - Maintainer
- [Marwan Rabbâa](https://github.com/waghanza) - Maintainer

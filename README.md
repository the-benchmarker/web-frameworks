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
Last update: 2019-01-04
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: symfony (php)


:five: laravel (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 0.07 ms | 0.07 ms | 0.11 ms | 0.14 ms | 0.83 ms | 29.00 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.15) | 3.27 ms | 0.18 ms | 11.98 ms | 28.98 ms | 79.02 ms | 6473.00 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.99 ms | 0.21 ms | 14.89 ms | 33.25 ms | 84.37 ms | 7642.67 | 
| php (7.2) | [symfony](http://symfony.com) (4.2) | 103.76 ms | 0.29 ms | 205.05 ms | 2273.27 ms | 6895.82 ms | 417091.33 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 120.23 ms | 0.30 ms | 290.45 ms | 2184.75 ms | 6978.73 ms | 417344.00 | 
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 0.33 ms | 0.31 ms | 0.54 ms | 0.85 ms | 24.07 ms | 252.67 | 
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 91.24 ms | 0.32 ms | 184.00 ms | 2027.41 ms | 6809.36 ms | 383445.00 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 6.18 ms | 0.33 ms | 22.13 ms | 44.29 ms | 126.36 ms | 10754.33 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 6.16 ms | 0.34 ms | 21.15 ms | 44.43 ms | 117.12 ms | 10474.33 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 8.35 ms | 0.51 ms | 26.70 ms | 53.58 ms | 119.90 ms | 12905.00 | 
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 95.36 ms | 1.16 ms | 212.74 ms | 1994.06 ms | 5382.12 ms | 352236.67 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 138.54 ms | 1.31 ms | 257.04 ms | 3072.50 ms | 6958.59 ms | 525705.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 82.18 ms | 1.47 ms | 4.88 ms | 2513.66 ms | 6586.92 ms | 448618.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.11 ms | 2.10 ms | 6.62 ms | 15.33 ms | 35.62 ms | 3202.00 | 
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 3.04 ms | 2.27 ms | 6.34 ms | 13.77 ms | 87.81 ms | 2994.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 2.59 ms | 2.41 ms | 4.25 ms | 10.57 ms | 28.81 ms | 1979.00 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 31.74 ms | 2.63 ms | 106.72 ms | 286.70 ms | 763.43 ms | 61262.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.05 ms | 2.94 ms | 5.24 ms | 7.39 ms | 59.29 ms | 1807.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.67 ms | 3.11 ms | 5.86 ms | 11.79 ms | 54.22 ms | 2386.33 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 3.52 ms | 3.18 ms | 6.20 ms | 12.05 ms | 27.15 ms | 2421.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.96 ms | 3.18 ms | 8.28 ms | 15.32 ms | 38.48 ms | 3367.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.04 ms | 3.28 ms | 7.17 ms | 13.52 ms | 36.49 ms | 2592.33 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 3.91 ms | 3.33 ms | 7.44 ms | 14.28 ms | 30.58 ms | 2896.33 | 
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 3.45 ms | 3.35 ms | 4.93 ms | 8.54 ms | 83.70 ms | 1865.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.5) | 5.54 ms | 3.76 ms | 9.13 ms | 65.02 ms | 120.66 ms | 9958.00 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 98.44 ms | 3.92 ms | 179.71 ms | 2101.43 ms | 4490.52 ms | 371334.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 4.85 ms | 4.78 ms | 8.17 ms | 10.12 ms | 37.79 ms | 2516.00 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 6.15 ms | 4.82 ms | 9.30 ms | 27.90 ms | 67.80 ms | 4654.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 5.61 ms | 4.90 ms | 8.42 ms | 16.34 ms | 265.82 ms | 6025.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 5.95 ms | 4.98 ms | 9.91 ms | 20.02 ms | 219.81 ms | 4760.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.93 ms | 5.13 ms | 9.77 ms | 19.04 ms | 81.54 ms | 4053.33 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 6.29 ms | 5.14 ms | 10.15 ms | 20.90 ms | 172.97 ms | 6289.67 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 6.41 ms | 5.31 ms | 10.74 ms | 21.66 ms | 86.54 ms | 3929.33 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 6.90 ms | 5.50 ms | 11.51 ms | 23.57 ms | 287.06 ms | 6671.00 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 6.98 ms | 5.62 ms | 11.74 ms | 23.82 ms | 203.34 ms | 5597.33 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 7.05 ms | 5.78 ms | 11.68 ms | 23.54 ms | 112.93 ms | 5029.00 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 7.20 ms | 5.92 ms | 12.02 ms | 23.93 ms | 163.45 ms | 5345.67 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 8.25 ms | 6.32 ms | 13.56 ms | 27.55 ms | 212.10 ms | 7741.00 | 
| node (11.2) | [polka](http://github.com/lukeed/polka) (0.5) | 9.95 ms | 7.37 ms | 17.87 ms | 40.32 ms | 338.21 ms | 12702.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 217.65 ms | 7.40 ms | 105.88 ms | 4770.69 ms | 7900.43 ms | 849935.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 10.09 ms | 8.08 ms | 19.02 ms | 38.24 ms | 281.73 ms | 9811.00 | 
| go (1.11) | [gf](http://gfer.me) (1.3) | 9.16 ms | 8.51 ms | 13.21 ms | 26.48 ms | 161.80 ms | 4883.67 | 
| node (11.2) | [restana](http://github.com/jkyberneees/ana) (2.3) | 11.74 ms | 8.69 ms | 21.97 ms | 45.24 ms | 363.25 ms | 12628.67 | 
| node (11.2) | [foxify](http://foxify.js.org) (0.10) | 12.13 ms | 9.05 ms | 20.51 ms | 47.09 ms | 403.16 ms | 14338.00 | 
| node (11.2) | [fastify](http://fastify.io) (1.13) | 12.81 ms | 9.35 ms | 20.71 ms | 61.70 ms | 483.15 ms | 19493.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 11.63 ms | 10.17 ms | 21.18 ms | 40.82 ms | 179.49 ms | 8921.00 | 
| node (11.2) | [rayo](http://rayo.js.org) (1.2) | 14.09 ms | 10.39 ms | 24.43 ms | 52.85 ms | 517.87 ms | 19825.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 15.43 ms | 10.94 ms | 19.54 ms | 91.19 ms | 931.66 ms | 38639.33 | 
| node (11.2) | [koa](http://koajs.com) (2.6) | 14.93 ms | 11.42 ms | 24.75 ms | 53.16 ms | 486.15 ms | 18278.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 11.58 ms | 11.66 ms | 13.23 ms | 14.98 ms | 42.77 ms | 1431.33 | 
| node (11.2) | [restify](http://restify.com) (7.2) | 16.91 ms | 13.70 ms | 27.45 ms | 52.12 ms | 480.24 ms | 16439.67 | 
| node (11.2) | [express](http://expressjs.com) (4.16) | 18.25 ms | 14.08 ms | 27.97 ms | 71.16 ms | 626.21 ms | 25032.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 19.51 ms | 18.37 ms | 31.12 ms | 45.94 ms | 85.10 ms | 8790.33 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 22.55 ms | 20.83 ms | 31.18 ms | 39.77 ms | 310.53 ms | 9005.00 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 23.56 ms | 21.53 ms | 32.33 ms | 41.71 ms | 250.83 ms | 9652.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 24.70 ms | 22.30 ms | 37.52 ms | 44.17 ms | 307.40 ms | 8691.67 | 
| node (11.2) | [hapi](http://hapijs.com) (17.7) | 36.14 ms | 22.77 ms | 41.37 ms | 453.27 ms | 1310.13 ms | 81534.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 25.04 ms | 22.90 ms | 37.27 ms | 52.36 ms | 318.52 ms | 10168.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 26.77 ms | 23.13 ms | 41.90 ms | 66.63 ms | 326.15 ms | 14872.00 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 27.48 ms | 24.83 ms | 40.81 ms | 57.20 ms | 540.82 ms | 16353.00 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 30.18 ms | 26.75 ms | 46.59 ms | 54.33 ms | 248.49 ms | 11955.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 39.84 ms | 31.76 ms | 73.66 ms | 144.84 ms | 323.91 ms | 28853.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 35.19 ms | 34.62 ms | 41.51 ms | 50.86 ms | 384.51 ms | 14784.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 46.61 ms | 40.54 ms | 72.84 ms | 107.76 ms | 585.39 ms | 27620.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 49.95 ms | 43.73 ms | 87.41 ms | 123.17 ms | 233.18 ms | 25703.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 72.86 ms | 73.06 ms | 91.29 ms | 112.35 ms | 436.88 ms | 19847.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (evhtp) (cpp)


:five: (onyx) (crystal)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 381209.67 | 220.51 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 346689.67 | 414.99 MB |
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 328339.00 | 373.09 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 290899.00 | 282.46 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 269634.67 | 253.55 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 269232.33 | 305.75 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 264782.67 | 531.73 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 254795.33 | 410.60 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 254637.33 | 271.22 MB |
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 254299.00 | 519.99 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.5) | 240291.33 | 138.89 MB |
| java (8) | [act](http://actframework.org) (1.8) | 234384.00 | 457.66 MB |
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 174461.00 | 220.63 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 170573.67 | 277.90 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 169421.00 | 297.31 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 164802.33 | 220.58 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 159571.33 | 213.84 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 154183.67 | 206.87 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 144875.33 | 254.18 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 142249.33 | 249.50 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 141631.33 | 190.77 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 136847.67 | 183.27 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 123941.33 | 318.02 MB |
| node (11.2) | [polka](http://github.com/lukeed/polka) (0.5) | 113181.67 | 169.64 MB |
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 108907.00 | 216.80 MB |
| go (1.11) | [gf](http://gfer.me) (1.3) | 108475.33 | 165.04 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 105637.33 | 260.13 MB |
| node (11.2) | [fastify](http://fastify.io) (1.13) | 97220.33 | 228.62 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 95305.00 | 166.99 MB |
| node (11.2) | [restana](http://github.com/jkyberneees/ana) (2.3) | 95087.33 | 142.43 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 92854.67 | 461.93 MB |
| node (11.2) | [foxify](http://foxify.js.org) (0.10) | 92176.00 | 193.35 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 86974.67 | 81.69 MB |
| node (11.2) | [rayo](http://rayo.js.org) (1.2) | 81979.00 | 122.55 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 78352.33 | 131.76 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 77517.67 | 166.07 MB |
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 76678.33 | 380.49 MB |
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 74045.67 | 367.64 MB |
| node (11.2) | [koa](http://koajs.com) (2.6) | 73943.33 | 156.19 MB |
| php (7.2) | [symfony](http://symfony.com) (4.2) | 73522.00 | 364.48 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 69808.00 | 362.98 MB |
| c (99) | [kore](http://kore.io) (3.1) | 65740.33 | 178.39 MB |
| node (11.2) | [restify](http://restify.com) (7.2) | 62630.00 | 109.54 MB |
| node (11.2) | [express](http://expressjs.com) (4.16) | 61580.67 | 150.34 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 57491.00 | 299.33 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 53420.33 | 84.92 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 51476.00 | 116.81 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 43381.33 | 40.69 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 42102.33 | 39.48 MB |
| node (11.2) | [hapi](http://hapijs.com) (17.7) | 40060.67 | 103.70 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 39636.00 | 64.60 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 39337.00 | 72.94 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.15) | 39171.00 | 37.32 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 37558.00 | 92.46 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 36269.33 | 44.57 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 32290.67 | 52.61 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 32151.67 | 18.53 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 28680.00 | 52.35 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 27242.67 | 48.53 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 21588.00 | 62.66 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 20810.00 | 12.00 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 20668.00 | 156.20 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 20090.67 | 40.09 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 15315.00 | 39.71 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 13384.00 | 39.46 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 4035.00 | 12.36 MB |
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

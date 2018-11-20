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
Last update: 2018-11-23
```
OS: Linux (version: 4.19.2-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: symfony (php)


:four: laravel (php)


:five: lumen (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 0.06 ms | 0.06 ms | 0.08 ms | 0.11 ms | 2.64 ms | 27.33 | 
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 0.26 ms | 0.24 ms | 0.45 ms | 0.73 ms | 8.65 ms | 165.00 | 
| php (7.2) | [symfony](http://symfony.com) (4.1) | 124.87 ms | 0.27 ms | 205.04 ms | 2771.38 ms | 6804.53 ms | 499842.00 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 114.71 ms | 0.27 ms | 259.32 ms | 2192.08 ms | 6827.16 ms | 425697.00 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 103.84 ms | 0.28 ms | 191.03 ms | 2385.04 ms | 6895.05 ms | 418888.33 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 124.47 ms | 0.28 ms | 202.81 ms | 2951.37 ms | 6756.42 ms | 495683.33 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.45 ms | 0.69 ms | 9.87 ms | 31.84 ms | 108.99 ms | 6470.33 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 5.78 ms | 0.76 ms | 17.44 ms | 55.92 ms | 169.18 ms | 11411.33 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 2.77 ms | 0.90 ms | 7.73 ms | 22.23 ms | 85.20 ms | 4647.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 86.39 ms | 1.22 ms | 45.86 ms | 2378.27 ms | 4952.11 ms | 422058.33 | 
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.60 ms | 1.65 ms | 6.11 ms | 12.74 ms | 33.07 ms | 2750.33 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 2.65 ms | 1.91 ms | 5.34 ms | 8.90 ms | 147.37 ms | 2857.00 | 
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 2.45 ms | 1.98 ms | 5.00 ms | 11.49 ms | 33.91 ms | 2282.00 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 6.53 ms | 2.17 ms | 18.60 ms | 48.62 ms | 141.50 ms | 10263.67 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.29 ms | 2.40 ms | 14.18 ms | 35.11 ms | 108.00 ms | 7476.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.53 ms | 2.42 ms | 4.54 ms | 6.01 ms | 19.50 ms | 1415.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.19 ms | 2.72 ms | 6.59 ms | 12.70 ms | 31.73 ms | 2741.00 | 
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 2.90 ms | 2.83 ms | 4.13 ms | 7.12 ms | 19.87 ms | 1271.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.38 ms | 2.96 ms | 5.72 ms | 9.20 ms | 36.29 ms | 1811.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 3.57 ms | 3.02 ms | 6.79 ms | 12.54 ms | 31.63 ms | 2589.00 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 4.88 ms | 3.63 ms | 8.38 ms | 44.24 ms | 122.55 ms | 8124.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 4.29 ms | 4.08 ms | 6.11 ms | 12.63 ms | 190.75 ms | 2844.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.12 ms | 4.21 ms | 8.36 ms | 23.27 ms | 176.98 ms | 5385.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.1) | 4.51 ms | 4.27 ms | 5.11 ms | 8.66 ms | 91.65 ms | 2430.33 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 4.61 ms | 4.29 ms | 7.70 ms | 15.17 ms | 146.47 ms | 3828.67 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 4.71 ms | 4.35 ms | 7.83 ms | 15.49 ms | 155.13 ms | 4226.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 4.45 ms | 4.41 ms | 7.44 ms | 8.94 ms | 18.15 ms | 2256.33 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 5.27 ms | 4.53 ms | 8.65 ms | 17.26 ms | 284.36 ms | 6476.00 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 5.35 ms | 4.54 ms | 7.58 ms | 26.50 ms | 66.40 ms | 4454.33 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 5.28 ms | 4.56 ms | 9.11 ms | 17.76 ms | 156.84 ms | 3961.67 | 
| go (1.11) | [beego](http://beego.me) (1.10) | 5.37 ms | 4.58 ms | 9.17 ms | 18.34 ms | 154.76 ms | 4110.67 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 5.41 ms | 4.62 ms | 9.22 ms | 18.18 ms | 107.44 ms | 3985.67 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 5.61 ms | 4.71 ms | 9.48 ms | 17.81 ms | 141.57 ms | 4042.67 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 6.53 ms | 4.91 ms | 10.42 ms | 18.53 ms | 208.96 ms | 4786.00 | 
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 7.48 ms | 5.03 ms | 14.37 ms | 32.18 ms | 275.37 ms | 8986.00 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 7.51 ms | 5.34 ms | 13.43 ms | 30.12 ms | 248.68 ms | 8037.33 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 8.04 ms | 5.74 ms | 15.02 ms | 33.69 ms | 267.95 ms | 8845.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 8.22 ms | 6.12 ms | 16.12 ms | 30.81 ms | 158.76 ms | 6722.67 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 31.31 ms | 6.75 ms | 96.53 ms | 273.50 ms | 860.12 ms | 59121.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 174.25 ms | 6.77 ms | 31.08 ms | 4421.23 ms | 7918.22 ms | 759275.00 | 
| go (1.11) | [gf](http://gfer.me) (1.2) | 7.93 ms | 7.00 ms | 11.52 ms | 26.30 ms | 239.44 ms | 8164.00 | 
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 11.81 ms | 8.60 ms | 19.16 ms | 52.52 ms | 476.45 ms | 18563.67 | 
| node (11.1) | [fastify](http://fastify.io) (1.13) | 11.83 ms | 8.60 ms | 18.98 ms | 60.05 ms | 443.74 ms | 18136.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 11.46 ms | 9.90 ms | 20.59 ms | 41.13 ms | 488.84 ms | 11892.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 14.72 ms | 10.67 ms | 18.59 ms | 72.38 ms | 920.57 ms | 38218.00 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 14.55 ms | 10.80 ms | 23.01 ms | 67.32 ms | 546.92 ms | 22124.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 14.59 ms | 11.83 ms | 27.78 ms | 40.55 ms | 94.36 ms | 9153.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 11.51 ms | 11.87 ms | 13.01 ms | 14.17 ms | 60.56 ms | 1605.00 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 17.14 ms | 13.29 ms | 28.25 ms | 58.87 ms | 574.33 ms | 22286.00 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 18.00 ms | 14.70 ms | 30.51 ms | 55.50 ms | 402.22 ms | 16288.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 20.08 ms | 17.89 ms | 30.12 ms | 41.92 ms | 181.57 ms | 6928.67 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 22.45 ms | 19.51 ms | 33.02 ms | 69.49 ms | 408.76 ms | 16030.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 22.58 ms | 19.64 ms | 33.16 ms | 55.47 ms | 388.84 ms | 15207.33 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 22.26 ms | 20.98 ms | 30.17 ms | 40.55 ms | 303.12 ms | 10823.33 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 29.14 ms | 22.83 ms | 38.74 ms | 182.69 ms | 887.47 ms | 43786.67 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 27.52 ms | 23.43 ms | 38.98 ms | 89.02 ms | 311.67 ms | 14950.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 26.64 ms | 24.73 ms | 35.62 ms | 72.25 ms | 435.47 ms | 15745.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 24.06 ms | 24.96 ms | 32.24 ms | 39.23 ms | 160.78 ms | 6694.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 35.54 ms | 28.10 ms | 67.56 ms | 129.53 ms | 248.25 ms | 26618.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 31.03 ms | 28.32 ms | 41.23 ms | 49.53 ms | 325.21 ms | 10270.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 42.03 ms | 33.71 ms | 77.22 ms | 113.22 ms | 350.34 ms | 23073.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 45.07 ms | 34.90 ms | 81.99 ms | 139.57 ms | 314.27 ms | 28520.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 63.64 ms | 62.73 ms | 81.55 ms | 98.42 ms | 454.41 ms | 19586.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (actix-web) (rust)


:three: (fasthttprouter) (go)


:four: (evhtp) (cpp)


:five: (vibora) (python)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 432741.67 | 518.02 MB |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 409523.00 | 465.46 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 355472.33 | 573.37 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 348807.00 | 338.25 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 340684.33 | 386.49 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 307169.33 | 617.25 MB |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 301872.33 | 616.95 MB |
| java (8) | [act](http://actframework.org) (1.8) | 289182.33 | 564.23 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 279470.00 | 299.03 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 254380.00 | 147.18 MB |
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.1) | 221998.67 | 128.23 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 221629.33 | 361.09 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 214063.00 | 286.37 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 209343.33 | 280.58 MB |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 200652.67 | 253.19 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 198963.67 | 349.48 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 190833.67 | 255.47 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 186216.33 | 326.70 MB |
| go (1.11) | [beego](http://beego.me) (1.10) | 185585.33 | 249.70 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 184201.00 | 323.20 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 175541.33 | 235.07 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 150698.33 | 225.66 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 150626.67 | 387.01 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 144704.00 | 216.96 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 137933.67 | 206.55 MB |
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 135466.67 | 267.82 MB |
| go (1.11) | [gf](http://gfer.me) (1.2) | 131504.33 | 200.02 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 129527.33 | 319.24 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 106678.33 | 249.46 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 100790.67 | 211.36 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 97141.33 | 169.85 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 92601.67 | 198.51 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 85685.33 | 80.56 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 82951.00 | 139.44 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 79288.67 | 392.89 MB |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 79127.33 | 392.12 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 78583.00 | 165.86 MB |
| c (99) | [kore](http://kore.io) (3.1) | 73093.33 | 198.04 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 72756.00 | 165.06 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 72449.67 | 115.58 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 72277.00 | 374.70 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 64544.00 | 157.62 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 61540.67 | 320.33 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 58517.67 | 102.52 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 49713.00 | 92.18 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 46449.33 | 44.24 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 45676.33 | 74.49 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 45255.33 | 42.49 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 44581.33 | 109.81 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 41675.33 | 39.09 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 39584.67 | 102.19 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 38319.00 | 47.32 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 37590.33 | 21.66 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 37277.33 | 68.10 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 31937.67 | 52.06 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 30741.33 | 54.77 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 24490.67 | 70.99 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 24232.00 | 13.98 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 22851.33 | 45.59 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 22468.33 | 169.76 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 19589.67 | 50.80 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 15220.00 | 40.78 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 4137.67 | 12.60 MB |
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

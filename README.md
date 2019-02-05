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
Last update: 2019-02-05
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: laravel (php)


:five: lumen (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.08 ms | 0.08 ms | 0.11 ms | 0.14 ms | 16.44 ms | 159.00 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 2.79 ms | 0.14 ms | 10.49 ms | 27.42 ms | 67.75 ms | 5978.33 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.56 ms | 0.18 ms | 13.38 ms | 31.73 ms | 80.85 ms | 7143.67 | 
| php (7.3) | [laravel](http://laravel.com) (5.7) | 135.28 ms | 0.27 ms | 370.91 ms | 2380.86 ms | 7236.15 ms | 468120.33 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 111.41 ms | 0.28 ms | 292.63 ms | 1950.27 ms | 6747.48 ms | 384746.33 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.35 ms | 0.29 ms | 19.09 ms | 42.52 ms | 98.62 ms | 9748.33 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.30 ms | 0.29 ms | 0.47 ms | 0.78 ms | 38.89 ms | 279.33 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 6.27 ms | 0.31 ms | 22.88 ms | 48.78 ms | 128.25 ms | 11462.67 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 136.99 ms | 0.31 ms | 346.96 ms | 2490.76 ms | 7047.03 ms | 471768.00 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 144.47 ms | 0.32 ms | 351.14 ms | 2597.08 ms | 7309.63 ms | 502485.33 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 135.91 ms | 0.34 ms | 359.90 ms | 2382.63 ms | 7634.62 ms | 473221.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 194.75 ms | 0.35 ms | 491.06 ms | 3474.02 ms | 7443.18 ms | 647550.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 6.77 ms | 0.39 ms | 22.88 ms | 47.16 ms | 110.95 ms | 11232.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 91.45 ms | 1.31 ms | 3.02 ms | 3174.55 ms | 6593.36 ms | 537923.33 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 32.23 ms | 1.90 ms | 105.86 ms | 329.60 ms | 1098.46 ms | 72791.33 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.87 ms | 1.98 ms | 6.35 ms | 14.21 ms | 32.22 ms | 2986.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 2.27 ms | 2.10 ms | 3.99 ms | 7.90 ms | 27.69 ms | 1673.00 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 2.71 ms | 2.12 ms | 5.52 ms | 12.18 ms | 42.64 ms | 2522.00 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 2.96 ms | 2.30 ms | 5.46 ms | 8.90 ms | 138.00 ms | 2258.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.34 ms | 2.79 ms | 6.99 ms | 13.09 ms | 30.44 ms | 2900.00 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 3.29 ms | 2.87 ms | 5.87 ms | 11.90 ms | 26.51 ms | 2352.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.81 ms | 2.91 ms | 4.65 ms | 6.22 ms | 18.39 ms | 1395.67 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 3.79 ms | 3.06 ms | 7.09 ms | 16.59 ms | 39.80 ms | 3232.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 7.85 ms | 3.07 ms | 7.16 ms | 173.86 ms | 258.54 ms | 26627.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.53 ms | 3.09 ms | 5.96 ms | 10.68 ms | 25.67 ms | 2024.33 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.7) | 6.45 ms | 3.20 ms | 9.96 ms | 81.01 ms | 129.43 ms | 13413.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 4.34 ms | 4.30 ms | 7.23 ms | 8.59 ms | 18.61 ms | 2163.33 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 4.91 ms | 4.42 ms | 8.16 ms | 16.27 ms | 159.98 ms | 4206.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 4.76 ms | 4.45 ms | 6.90 ms | 13.29 ms | 148.74 ms | 2773.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 5.12 ms | 4.50 ms | 8.75 ms | 17.50 ms | 112.64 ms | 3599.00 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 5.42 ms | 4.57 ms | 9.04 ms | 19.96 ms | 142.12 ms | 4451.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 5.51 ms | 4.62 ms | 9.38 ms | 18.90 ms | 160.39 ms | 4740.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.32 ms | 4.63 ms | 8.58 ms | 18.55 ms | 77.54 ms | 3796.00 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 5.55 ms | 4.63 ms | 9.30 ms | 18.83 ms | 221.70 ms | 5336.00 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 5.80 ms | 4.79 ms | 9.90 ms | 19.68 ms | 159.43 ms | 4104.00 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 5.79 ms | 4.82 ms | 9.77 ms | 19.01 ms | 162.60 ms | 4290.67 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 7.31 ms | 4.85 ms | 14.57 ms | 27.24 ms | 211.34 ms | 6799.33 | 
| node (11.9) | [restana](http://github.com/jkyberneees/ana) (2.7) | 8.18 ms | 5.67 ms | 16.13 ms | 35.33 ms | 229.27 ms | 8544.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 185.06 ms | 6.02 ms | 472.81 ms | 3737.58 ms | 7750.24 ms | 657876.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 8.07 ms | 6.17 ms | 14.86 ms | 28.13 ms | 209.52 ms | 6473.67 | 
| go (1.11) | [gf](http://gfer.me) (1.4) | 8.81 ms | 7.86 ms | 13.10 ms | 28.71 ms | 237.09 ms | 7409.00 | 
| node (11.9) | [polka](http://github.com/lukeed/polka) (0.5) | 12.18 ms | 8.19 ms | 21.44 ms | 52.77 ms | 555.94 ms | 21834.67 | 
| node (11.9) | [koa](http://koajs.com) (2.7) | 12.14 ms | 8.33 ms | 20.49 ms | 62.29 ms | 477.27 ms | 19153.33 | 
| python (3.7) | [starlette](http://starlette.io) (0.10) | 9.74 ms | 8.41 ms | 16.34 ms | 25.20 ms | 105.20 ms | 5214.00 | 
| node (11.9) | [rayo](http://rayo.js.org) (1.2) | 12.49 ms | 9.21 ms | 21.87 ms | 46.16 ms | 462.92 ms | 17444.33 | 
| node (11.9) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 12.53 ms | 9.35 ms | 20.65 ms | 45.55 ms | 436.96 ms | 17359.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 11.25 ms | 9.79 ms | 20.60 ms | 41.20 ms | 194.81 ms | 8667.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 14.10 ms | 10.18 ms | 18.40 ms | 74.83 ms | 873.13 ms | 34374.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 10.44 ms | 10.55 ms | 12.22 ms | 13.94 ms | 75.60 ms | 1609.00 | 
| node (11.9) | [express](http://expressjs.com) (4.16) | 16.32 ms | 10.98 ms | 23.01 ms | 151.69 ms | 756.59 ms | 32066.67 | 
| node (11.9) | [foxify](http://foxify.js.org) (0.10) | 14.07 ms | 11.09 ms | 23.43 ms | 47.60 ms | 447.76 ms | 16508.67 | 
| node (11.9) | [fastify](http://fastify.io) (1.13) | 20.37 ms | 11.42 ms | 26.24 ms | 244.96 ms | 717.85 ms | 48767.33 | 
| node (11.9) | [restify](http://restify.com) (7.6) | 17.11 ms | 14.12 ms | 29.56 ms | 52.90 ms | 349.82 ms | 13354.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 17.26 ms | 14.44 ms | 32.26 ms | 43.59 ms | 65.99 ms | 10142.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 24.87 ms | 16.95 ms | 54.93 ms | 80.38 ms | 232.96 ms | 17752.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 20.66 ms | 18.16 ms | 30.44 ms | 45.24 ms | 594.65 ms | 15970.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 21.59 ms | 20.20 ms | 28.22 ms | 38.69 ms | 235.16 ms | 6358.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 23.26 ms | 23.49 ms | 28.61 ms | 36.28 ms | 244.48 ms | 8188.00 | 
| node (11.9) | [hapi](http://hapijs.com) (18.0) | 37.57 ms | 24.99 ms | 45.71 ms | 472.41 ms | 1279.93 ms | 80752.67 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 29.26 ms | 28.07 ms | 39.64 ms | 44.94 ms | 308.80 ms | 11040.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 33.46 ms | 28.70 ms | 58.21 ms | 103.06 ms | 225.97 ms | 20259.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 27.27 ms | 28.96 ms | 35.85 ms | 40.75 ms | 284.49 ms | 9310.33 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 32.19 ms | 31.65 ms | 38.51 ms | 42.15 ms | 53.01 ms | 5233.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 42.36 ms | 32.92 ms | 84.33 ms | 106.62 ms | 377.36 ms | 25046.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 41.23 ms | 33.70 ms | 77.51 ms | 108.12 ms | 141.04 ms | 26411.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 36.29 ms | 35.15 ms | 42.56 ms | 50.51 ms | 245.54 ms | 10342.00 | 
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.1) | 50.85 ms | 46.57 ms | 83.11 ms | 113.24 ms | 150.73 ms | 21632.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 68.08 ms | 66.89 ms | 86.62 ms | 119.96 ms | 442.56 ms | 20594.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (vibora) (python)


:five: (fasthttprouter) (go)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 419944.67 | 242.90 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 382948.33 | 458.57 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 364969.67 | 414.42 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 320103.00 | 363.32 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 312592.67 | 505.05 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 311981.67 | 302.63 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 293349.00 | 589.45 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 292887.67 | 275.59 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 273372.00 | 290.40 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.7) | 273080.67 | 157.99 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 272185.00 | 556.85 MB |
| java (8) | [act](http://actframework.org) (1.8) | 270279.67 | 527.37 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 200147.67 | 267.55 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 197531.00 | 321.76 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 190346.67 | 254.07 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 185700.00 | 234.95 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 184551.00 | 246.99 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 180276.33 | 242.78 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 180149.67 | 316.16 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 170426.00 | 299.02 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 167926.67 | 223.77 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 143431.33 | 368.37 MB |
| node (11.9) | [restana](http://github.com/jkyberneees/ana) (2.7) | 134484.33 | 201.64 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 127271.67 | 313.11 MB |
| go (1.11) | [gf](http://gfer.me) (1.4) | 116038.33 | 176.31 MB |
| python (3.7) | [starlette](http://starlette.io) (0.10) | 102214.00 | 220.34 MB |
| node (11.9) | [polka](http://github.com/lukeed/polka) (0.5) | 101311.67 | 151.84 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 99467.33 | 174.12 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 99028.33 | 197.68 MB |
| node (11.9) | [koa](http://koajs.com) (2.7) | 97272.00 | 205.97 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 95278.33 | 89.53 MB |
| node (11.9) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 94919.67 | 141.86 MB |
| node (11.9) | [rayo](http://rayo.js.org) (1.2) | 92417.33 | 138.28 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 83534.67 | 140.69 MB |
| node (11.9) | [foxify](http://foxify.js.org) (0.10) | 77585.67 | 162.65 MB |
| node (11.9) | [express](http://expressjs.com) (4.16) | 76376.00 | 191.28 MB |
| node (11.9) | [fastify](http://fastify.io) (1.13) | 76277.00 | 187.09 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 68271.67 | 108.36 MB |
| c (99) | [kore](http://kore.io) (3.1) | 67386.67 | 182.75 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 63260.33 | 313.95 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 62843.67 | 312.40 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 62578.00 | 310.75 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 61843.33 | 310.04 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 61179.00 | 138.82 MB |
| node (11.9) | [restify](http://restify.com) (7.6) | 60783.00 | 106.60 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 57834.33 | 302.53 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 55188.00 | 118.23 MB |
| php (7.3) | [laravel](http://laravel.com) (5.7) | 51518.00 | 271.57 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 47658.67 | 88.50 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 46177.00 | 43.99 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 45746.00 | 42.89 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 43653.67 | 107.54 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 43251.33 | 40.56 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 36611.00 | 59.69 MB |
| node (11.9) | [hapi](http://hapijs.com) (18.0) | 36124.00 | 93.29 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 36098.33 | 20.81 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 33441.00 | 61.09 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 31554.33 | 38.64 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 31053.00 | 55.28 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 28380.00 | 46.27 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 25031.00 | 49.88 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 24213.33 | 70.30 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 23888.00 | 13.77 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 20365.67 | 153.89 MB |
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.1) | 19728.00 | 42.99 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 18925.33 | 49.04 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 14256.67 | 41.99 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 4138.67 | 12.75 MB |
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

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
Last update: 2019-04-12
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: iron (rust)


:five: laravel (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.08 ms | 0.08 ms | 0.12 ms | 0.16 ms | 1.06 ms | 31.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.19) | 3.20 ms | 0.18 ms | 11.52 ms | 28.10 ms | 72.71 ms | 6249.67 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.44 ms | 0.24 ms | 16.24 ms | 34.83 ms | 84.14 ms | 8161.33 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.32 ms | 0.30 ms | 0.52 ms | 0.86 ms | 9.57 ms | 194.33 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 134.86 ms | 0.36 ms | 315.07 ms | 2568.90 ms | 6790.92 ms | 460143.00 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 158.85 ms | 0.36 ms | 253.20 ms | 3611.33 ms | 6877.12 ms | 604439.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 6.57 ms | 0.39 ms | 22.10 ms | 45.79 ms | 105.79 ms | 10834.33 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 7.75 ms | 0.42 ms | 25.96 ms | 52.90 ms | 134.89 ms | 12755.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 8.80 ms | 0.59 ms | 27.33 ms | 53.55 ms | 139.04 ms | 13064.67 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 118.19 ms | 1.24 ms | 221.70 ms | 2729.45 ms | 5690.58 ms | 452902.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 88.78 ms | 1.50 ms | 3.24 ms | 2973.48 ms | 6592.68 ms | 516827.00 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 121.41 ms | 1.50 ms | 239.10 ms | 2696.86 ms | 5815.27 ms | 451707.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 2.96 ms | 1.86 ms | 6.75 ms | 14.56 ms | 39.32 ms | 3183.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.04 ms | 2.26 ms | 6.32 ms | 13.00 ms | 31.55 ms | 2834.67 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 3.01 ms | 2.31 ms | 6.03 ms | 12.85 ms | 93.22 ms | 3374.33 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 78.59 ms | 2.45 ms | 175.25 ms | 1671.98 ms | 4469.23 ms | 287012.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 5.99 ms | 2.87 ms | 6.94 ms | 43.19 ms | 849.00 ms | 32396.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 108.26 ms | 2.95 ms | 205.19 ms | 2487.27 ms | 5373.29 ms | 417792.67 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.70 ms | 3.19 ms | 5.86 ms | 11.83 ms | 45.76 ms | 2381.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.09 ms | 3.26 ms | 8.43 ms | 16.19 ms | 37.95 ms | 3499.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.17 ms | 3.26 ms | 5.25 ms | 7.49 ms | 43.65 ms | 1780.00 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 34.22 ms | 3.30 ms | 114.73 ms | 297.14 ms | 903.08 ms | 63905.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.08 ms | 3.38 ms | 6.94 ms | 12.06 ms | 29.04 ms | 2319.33 | 
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 3.93 ms | 3.57 ms | 6.84 ms | 13.73 ms | 47.97 ms | 2851.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 4.28 ms | 3.71 ms | 8.10 ms | 14.94 ms | 30.56 ms | 3039.00 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 4.37 ms | 3.77 ms | 8.47 ms | 18.56 ms | 99.90 ms | 3963.33 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 5.78 ms | 3.86 ms | 8.88 ms | 68.37 ms | 115.32 ms | 10220.00 | 
| node (11.13) | [restana](http://github.com/jkyberneees/ana) (2.13) | 5.73 ms | 4.70 ms | 9.23 ms | 18.07 ms | 225.53 ms | 6386.67 | 
| node (11.13) | [polka](http://github.com/lukeed/polka) (0.5) | 6.05 ms | 4.87 ms | 9.59 ms | 17.18 ms | 212.08 ms | 5684.67 | 
| node (11.13) | [rayo](http://rayo.js.org) (1.2) | 6.56 ms | 4.93 ms | 9.70 ms | 19.01 ms | 296.01 ms | 9288.00 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 6.92 ms | 5.03 ms | 14.05 ms | 29.37 ms | 171.72 ms | 6582.33 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 6.95 ms | 5.06 ms | 12.57 ms | 28.69 ms | 257.26 ms | 8932.33 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 7.61 ms | 5.26 ms | 15.96 ms | 32.79 ms | 240.07 ms | 7952.67 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 7.84 ms | 5.27 ms | 15.79 ms | 33.46 ms | 359.70 ms | 11224.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 5.99 ms | 5.32 ms | 8.88 ms | 16.48 ms | 212.80 ms | 4591.67 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 7.05 ms | 5.38 ms | 13.37 ms | 28.69 ms | 68.76 ms | 5406.00 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 8.08 ms | 5.48 ms | 17.46 ms | 36.00 ms | 154.29 ms | 7372.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.39 ms | 5.51 ms | 10.55 ms | 21.48 ms | 77.48 ms | 4556.00 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 7.75 ms | 5.53 ms | 15.66 ms | 33.32 ms | 190.49 ms | 7399.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 7.80 ms | 6.24 ms | 12.99 ms | 21.08 ms | 210.26 ms | 5638.67 | 
| go (1.12) | [gf](http://goframe.org) (1.6) | 9.26 ms | 6.50 ms | 19.16 ms | 41.54 ms | 140.41 ms | 8560.67 | 
| node (11.13) | [foxify](http://foxify.js.org) (0.10) | 8.08 ms | 6.75 ms | 12.25 ms | 22.81 ms | 330.40 ms | 10455.67 | 
| node (11.13) | [fastify](http://fastify.io) (2.2) | 10.38 ms | 7.26 ms | 13.37 ms | 74.85 ms | 572.34 ms | 24245.33 | 
| node (11.13) | [koa](http://koajs.com) (2.7) | 8.72 ms | 7.55 ms | 13.11 ms | 27.73 ms | 351.99 ms | 12332.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 199.24 ms | 7.62 ms | 89.76 ms | 4330.30 ms | 7132.26 ms | 765386.67 | 
| node (11.13) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 12.49 ms | 7.82 ms | 16.38 ms | 141.02 ms | 678.76 ms | 33602.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 9.87 ms | 8.48 ms | 17.77 ms | 33.18 ms | 214.18 ms | 7690.00 | 
| node (11.13) | [restify](http://restify.com) (8.2) | 9.73 ms | 8.56 ms | 12.97 ms | 32.39 ms | 280.37 ms | 8634.00 | 
| node (11.13) | [express](http://expressjs.com) (4.16) | 10.64 ms | 8.78 ms | 15.06 ms | 30.20 ms | 441.39 ms | 15757.67 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 13.15 ms | 11.01 ms | 23.80 ms | 42.81 ms | 175.49 ms | 8699.33 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 14.57 ms | 11.05 ms | 20.10 ms | 47.91 ms | 833.55 ms | 30047.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 11.33 ms | 11.57 ms | 13.43 ms | 15.29 ms | 203.21 ms | 3355.67 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 11.83 ms | 11.72 ms | 20.04 ms | 28.36 ms | 55.28 ms | 6072.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 16.69 ms | 12.00 ms | 24.35 ms | 153.77 ms | 1200.79 ms | 41091.67 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.13) | 18.03 ms | 16.47 ms | 31.41 ms | 46.74 ms | 88.13 ms | 9841.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 20.00 ms | 18.69 ms | 32.18 ms | 44.16 ms | 65.13 ms | 9212.67 | 
| swift (5.0) | [kitura](http://kitura.io) (2.6) | 22.56 ms | 18.70 ms | 31.54 ms | 82.70 ms | 911.98 ms | 32651.00 | 
| node (11.13) | [hapi](http://hapijs.com) (18.1) | 30.93 ms | 19.82 ms | 32.59 ms | 383.97 ms | 1131.16 ms | 70012.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 23.92 ms | 21.39 ms | 33.61 ms | 38.72 ms | 235.51 ms | 7273.00 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 28.47 ms | 21.67 ms | 48.50 ms | 103.62 ms | 345.39 ms | 20102.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 24.43 ms | 22.43 ms | 35.58 ms | 41.36 ms | 239.01 ms | 7337.00 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 24.41 ms | 23.60 ms | 41.19 ms | 60.57 ms | 98.30 ms | 12386.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 30.74 ms | 26.86 ms | 48.31 ms | 88.29 ms | 396.74 ms | 16344.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 28.52 ms | 30.11 ms | 37.08 ms | 42.99 ms | 182.83 ms | 8170.00 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 33.76 ms | 31.57 ms | 45.20 ms | 69.18 ms | 324.10 ms | 12736.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 33.38 ms | 33.71 ms | 40.88 ms | 45.97 ms | 313.07 ms | 8404.00 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 32.79 ms | 34.49 ms | 41.73 ms | 45.74 ms | 187.23 ms | 8438.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 41.14 ms | 35.28 ms | 72.98 ms | 134.67 ms | 250.01 ms | 25420.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 50.14 ms | 45.51 ms | 83.22 ms | 118.31 ms | 183.81 ms | 23668.33 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 66.39 ms | 63.36 ms | 99.39 ms | 129.70 ms | 196.77 ms | 24098.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 79.78 ms | 66.69 ms | 140.26 ms | 208.26 ms | 525.51 ms | 41264.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 78.26 ms | 77.33 ms | 95.89 ms | 122.51 ms | 704.65 ms | 27669.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (kore) (c)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 377580.00 | 218.33 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 332901.00 | 398.45 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 327356.33 | 371.88 MB |
| c (99) | [kore](http://kore.io) (3.1) | 288506.33 | 749.03 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 277775.00 | 269.76 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 263140.33 | 298.68 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 253091.33 | 408.00 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 250818.67 | 504.30 MB |
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 246875.33 | 232.19 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 234690.00 | 480.04 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 231893.33 | 248.33 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 228231.67 | 132.04 MB |
| java (8) | [act](http://actframework.org) (1.8) | 218142.33 | 426.00 MB |
| node (11.13) | [restana](http://github.com/jkyberneees/ana) (2.13) | 175517.67 | 262.95 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 173857.33 | 218.83 MB |
| node (11.13) | [polka](http://github.com/lukeed/polka) (0.5) | 159885.67 | 239.74 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 158830.33 | 258.83 MB |
| node (11.13) | [rayo](http://rayo.js.org) (1.2) | 156886.00 | 235.26 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 156143.00 | 208.77 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 155645.67 | 208.31 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 150856.67 | 200.57 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 144436.33 | 253.10 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 143814.00 | 252.29 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 141253.33 | 190.51 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 137496.67 | 183.58 MB |
| node (11.13) | [fastify](http://fastify.io) (2.2) | 130127.33 | 329.13 MB |
| node (11.13) | [foxify](http://foxify.js.org) (0.10) | 127462.33 | 268.05 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 125818.00 | 323.22 MB |
| node (11.13) | [koa](http://koajs.com) (2.7) | 121644.33 | 257.68 MB |
| go (1.12) | [gf](http://goframe.org) (1.6) | 121170.33 | 183.73 MB |
| node (11.13) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 115397.00 | 173.04 MB |
| node (11.13) | [restify](http://restify.com) (8.2) | 106486.67 | 186.95 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 105106.67 | 258.90 MB |
| node (11.13) | [express](http://expressjs.com) (4.16) | 101483.33 | 248.71 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 98118.67 | 195.16 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 88229.00 | 83.01 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 84927.33 | 421.85 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 83407.67 | 179.77 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 81332.33 | 404.12 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 77558.00 | 135.73 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 77538.33 | 130.40 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 77171.33 | 383.00 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 77034.00 | 191.07 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 76874.33 | 165.01 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 74162.00 | 368.16 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 64571.00 | 334.94 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 59276.00 | 94.15 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.13) | 56286.33 | 121.59 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 54269.00 | 282.45 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 50142.33 | 113.74 MB |
| node (11.13) | [hapi](http://hapijs.com) (18.1) | 48020.00 | 123.67 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.6) | 47266.67 | 87.60 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 41579.67 | 38.99 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 41395.67 | 79.86 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 40433.33 | 37.88 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.19) | 40268.00 | 38.36 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 37952.00 | 70.61 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 35171.67 | 57.29 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 32906.67 | 80.95 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 30435.67 | 37.33 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 30307.33 | 55.32 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 29297.33 | 47.76 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 28767.00 | 16.60 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 25388.33 | 45.22 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 19948.33 | 39.81 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 19477.00 | 11.24 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 16449.67 | 124.36 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 14960.00 | 32.54 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 14504.67 | 37.59 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 12509.00 | 36.82 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 12499.67 | 36.26 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3735.00 | 11.46 MB |
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

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
Last update: 2019-04-11
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
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.06 ms | 0.07 ms | 0.08 ms | 0.10 ms | 0.44 ms | 18.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.18) | 2.69 ms | 0.14 ms | 9.78 ms | 25.62 ms | 70.80 ms | 5571.67 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.52 ms | 0.18 ms | 13.19 ms | 30.45 ms | 79.28 ms | 6920.00 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 92.42 ms | 0.24 ms | 165.82 ms | 2168.59 ms | 6724.12 ms | 382725.33 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 105.47 ms | 0.26 ms | 321.61 ms | 1557.13 ms | 7071.00 ms | 349600.00 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 110.96 ms | 0.27 ms | 262.10 ms | 2196.29 ms | 6851.38 ms | 418350.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.41 ms | 0.29 ms | 19.05 ms | 41.55 ms | 93.52 ms | 9616.00 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.30 ms | 0.30 ms | 0.48 ms | 0.79 ms | 7.49 ms | 174.33 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 34.58 ms | 0.32 ms | 22.01 ms | 1091.90 ms | 6680.86 ms | 283574.33 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 7.19 ms | 0.41 ms | 23.89 ms | 49.31 ms | 121.71 ms | 11694.33 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 83.02 ms | 0.93 ms | 160.06 ms | 1830.82 ms | 5414.65 ms | 348955.67 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 82.86 ms | 1.16 ms | 155.83 ms | 1756.76 ms | 5293.48 ms | 339080.67 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 88.67 ms | 1.20 ms | 162.64 ms | 1918.08 ms | 5580.49 ms | 358659.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 2.78 ms | 1.21 ms | 7.04 ms | 15.74 ms | 38.58 ms | 3430.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 78.42 ms | 1.28 ms | 2.96 ms | 2491.09 ms | 6588.93 ms | 474168.33 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 29.43 ms | 1.67 ms | 101.41 ms | 285.83 ms | 785.66 ms | 60512.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.85 ms | 1.67 ms | 6.52 ms | 16.05 ms | 34.53 ms | 3338.67 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 2.80 ms | 2.04 ms | 5.40 ms | 8.95 ms | 128.47 ms | 2711.00 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 2.69 ms | 2.16 ms | 5.50 ms | 12.73 ms | 33.41 ms | 2535.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 12.39 ms | 2.56 ms | 5.22 ms | 355.28 ms | 1426.39 ms | 80995.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.33 ms | 2.56 ms | 6.69 ms | 14.45 ms | 103.08 ms | 5046.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.93 ms | 2.63 ms | 3.17 ms | 13.91 ms | 148.43 ms | 5876.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.14 ms | 2.86 ms | 4.98 ms | 9.32 ms | 35.05 ms | 1721.67 | 
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 3.57 ms | 2.96 ms | 5.91 ms | 12.53 ms | 119.66 ms | 5346.33 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 3.61 ms | 2.98 ms | 6.95 ms | 13.44 ms | 30.57 ms | 2751.67 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 3.78 ms | 3.05 ms | 6.53 ms | 16.13 ms | 277.99 ms | 5705.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 4.72 ms | 3.25 ms | 7.26 ms | 52.24 ms | 112.62 ms | 8916.00 | 
| node (11.13) | [restana](http://github.com/jkyberneees/ana) (2.13) | 4.39 ms | 4.20 ms | 6.28 ms | 14.79 ms | 208.34 ms | 5817.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 4.73 ms | 4.24 ms | 6.45 ms | 14.71 ms | 279.83 ms | 6895.33 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 5.51 ms | 4.30 ms | 11.33 ms | 23.09 ms | 209.87 ms | 5350.33 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 5.64 ms | 4.42 ms | 10.27 ms | 22.84 ms | 242.78 ms | 8084.67 | 
| node (11.13) | [polka](http://github.com/lukeed/polka) (0.5) | 4.57 ms | 4.45 ms | 6.51 ms | 13.93 ms | 212.31 ms | 5249.00 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 5.94 ms | 4.46 ms | 12.13 ms | 24.87 ms | 234.80 ms | 6870.00 | 
| node (11.13) | [rayo](http://rayo.js.org) (1.2) | 4.70 ms | 4.46 ms | 6.97 ms | 14.65 ms | 202.56 ms | 5667.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.34 ms | 4.49 ms | 8.52 ms | 23.04 ms | 112.46 ms | 4908.67 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 5.84 ms | 4.49 ms | 11.98 ms | 24.77 ms | 104.83 ms | 5104.33 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 6.47 ms | 4.52 ms | 13.56 ms | 28.76 ms | 295.88 ms | 8976.00 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 7.30 ms | 4.55 ms | 13.93 ms | 60.27 ms | 198.43 ms | 11332.67 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 6.03 ms | 4.57 ms | 11.15 ms | 24.85 ms | 181.22 ms | 7882.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 5.96 ms | 4.69 ms | 10.61 ms | 18.90 ms | 155.82 ms | 5145.00 | 
| node (11.13) | [foxify](http://foxify.js.org) (0.10) | 5.80 ms | 4.72 ms | 9.30 ms | 14.99 ms | 218.44 ms | 6366.33 | 
| node (11.13) | [koa](http://koajs.com) (2.7) | 6.45 ms | 4.85 ms | 9.47 ms | 17.56 ms | 290.64 ms | 9278.67 | 
| go (1.12) | [gf](http://goframe.org) (1.6) | 7.75 ms | 5.19 ms | 16.37 ms | 35.52 ms | 181.65 ms | 8116.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 6.71 ms | 5.71 ms | 11.33 ms | 18.72 ms | 212.00 ms | 5399.00 | 
| node (11.13) | [fastify](http://fastify.io) (2.2) | 7.03 ms | 5.74 ms | 11.04 ms | 22.28 ms | 326.21 ms | 10400.00 | 
| node (11.13) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 7.77 ms | 5.79 ms | 12.90 ms | 30.84 ms | 367.95 ms | 11629.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 200.24 ms | 6.50 ms | 65.46 ms | 4947.78 ms | 7912.49 ms | 834646.67 | 
| node (11.13) | [express](http://expressjs.com) (4.16) | 7.88 ms | 7.15 ms | 11.88 ms | 21.19 ms | 312.85 ms | 9654.33 | 
| node (11.13) | [restify](http://restify.com) (8.2) | 8.05 ms | 7.67 ms | 10.73 ms | 21.13 ms | 254.24 ms | 7649.00 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 9.26 ms | 7.68 ms | 18.26 ms | 28.14 ms | 85.24 ms | 6286.33 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 9.58 ms | 7.89 ms | 15.70 ms | 27.89 ms | 217.28 ms | 7013.67 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 11.48 ms | 8.44 ms | 15.85 ms | 58.41 ms | 591.11 ms | 22153.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 8.90 ms | 9.06 ms | 10.60 ms | 12.53 ms | 82.43 ms | 1706.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 41.32 ms | 9.49 ms | 20.80 ms | 1143.28 ms | 2566.72 ms | 200900.67 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.13) | 13.87 ms | 12.05 ms | 25.25 ms | 41.22 ms | 206.44 ms | 10619.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 14.32 ms | 12.68 ms | 24.06 ms | 30.87 ms | 48.59 ms | 6465.33 | 
| swift (5.0) | [kitura](http://kitura.io) (2.6) | 20.65 ms | 14.32 ms | 24.73 ms | 207.45 ms | 1013.92 ms | 48674.00 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 21.68 ms | 14.86 ms | 50.51 ms | 73.24 ms | 225.62 ms | 16178.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 24.18 ms | 17.33 ms | 50.66 ms | 74.97 ms | 259.48 ms | 18072.00 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 18.67 ms | 17.39 ms | 32.16 ms | 48.96 ms | 90.65 ms | 10357.33 | 
| node (11.13) | [hapi](http://hapijs.com) (18.1) | 23.60 ms | 17.47 ms | 27.57 ms | 204.44 ms | 895.94 ms | 45641.33 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 21.58 ms | 19.82 ms | 27.51 ms | 40.04 ms | 54.24 ms | 5295.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 22.25 ms | 20.20 ms | 31.42 ms | 41.28 ms | 212.86 ms | 9342.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 22.31 ms | 21.62 ms | 30.04 ms | 34.64 ms | 233.00 ms | 7014.00 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 27.58 ms | 25.88 ms | 36.72 ms | 41.30 ms | 65.47 ms | 5570.00 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 28.72 ms | 27.08 ms | 36.82 ms | 41.65 ms | 253.01 ms | 10770.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 35.24 ms | 28.38 ms | 66.18 ms | 135.39 ms | 234.77 ms | 25312.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 39.37 ms | 28.82 ms | 75.03 ms | 109.14 ms | 161.66 ms | 22581.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 30.71 ms | 32.54 ms | 40.41 ms | 45.27 ms | 318.60 ms | 9862.33 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 52.05 ms | 42.05 ms | 98.93 ms | 139.82 ms | 227.98 ms | 32358.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 62.44 ms | 53.18 ms | 102.85 ms | 157.73 ms | 631.13 ms | 35623.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 62.26 ms | 60.74 ms | 73.43 ms | 90.80 ms | 448.28 ms | 16806.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (kore) (c)


:five: (vibora) (python)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 467383.67 | 270.37 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 428200.33 | 512.71 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 369217.67 | 419.52 MB |
| c (99) | [kore](http://kore.io) (3.1) | 361407.67 | 939.50 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 352222.67 | 399.37 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 349357.67 | 338.12 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 333690.33 | 536.80 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 321388.00 | 646.27 MB |
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 287327.33 | 269.97 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 279242.67 | 297.58 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 279234.67 | 161.48 MB |
| java (8) | [act](http://actframework.org) (1.8) | 279159.00 | 545.09 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 274002.00 | 560.65 MB |
| node (11.13) | [restana](http://github.com/jkyberneees/ana) (2.13) | 229799.67 | 344.63 MB |
| node (11.13) | [polka](http://github.com/lukeed/polka) (0.5) | 214839.33 | 322.01 MB |
| node (11.13) | [rayo](http://rayo.js.org) (1.2) | 212855.00 | 319.18 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 209531.33 | 341.10 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 192791.67 | 257.46 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 191875.67 | 254.43 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 188876.33 | 238.33 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 185137.33 | 324.63 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 183835.00 | 247.78 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 181784.00 | 240.70 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 180602.33 | 316.64 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 176816.67 | 236.11 MB |
| node (11.13) | [fastify](http://fastify.io) (2.2) | 170709.33 | 424.71 MB |
| node (11.13) | [foxify](http://foxify.js.org) (0.10) | 168804.00 | 355.10 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 167829.33 | 431.15 MB |
| node (11.13) | [koa](http://koajs.com) (2.7) | 161103.00 | 341.35 MB |
| node (11.13) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 155253.00 | 232.49 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 148399.33 | 365.79 MB |
| go (1.12) | [gf](http://goframe.org) (1.6) | 147536.67 | 222.90 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 135276.67 | 269.29 MB |
| node (11.13) | [express](http://expressjs.com) (4.16) | 131537.00 | 322.32 MB |
| node (11.13) | [restify](http://restify.com) (8.2) | 126169.33 | 221.52 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 112873.33 | 243.23 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 110858.67 | 104.10 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 105437.00 | 261.51 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 97407.00 | 170.50 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 97175.67 | 163.29 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 96326.00 | 477.77 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 93621.33 | 464.58 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 90199.67 | 193.37 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 88368.00 | 438.92 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 85925.33 | 425.72 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.13) | 75486.00 | 163.21 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 69615.33 | 157.80 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 69092.67 | 109.30 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 64185.33 | 333.88 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.6) | 61643.67 | 114.29 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 54518.00 | 105.43 MB |
| node (11.13) | [hapi](http://hapijs.com) (18.1) | 54167.00 | 139.95 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 53190.00 | 277.71 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 51943.33 | 96.67 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.18) | 47951.33 | 45.67 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 45843.67 | 42.99 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 45251.67 | 111.19 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 45025.00 | 73.35 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 44736.33 | 41.93 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 36392.33 | 21.00 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 36085.67 | 44.49 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 34460.33 | 62.90 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 32763.33 | 53.36 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 30695.67 | 54.64 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 26007.67 | 51.90 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 23623.67 | 13.62 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 19722.67 | 42.94 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 18892.67 | 143.07 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 17813.33 | 46.19 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 16317.33 | 47.33 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 15563.00 | 46.00 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 4369.67 | 13.40 MB |
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

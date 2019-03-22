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
Last update: 2019-03-22
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: zend-framework (php)


:five: iron (rust)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.07 ms | 0.08 ms | 0.10 ms | 0.12 ms | 4.27 ms | 46.00 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.18) | 2.51 ms | 0.14 ms | 8.95 ms | 25.27 ms | 70.14 ms | 5385.00 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.25 ms | 0.17 ms | 11.93 ms | 28.52 ms | 70.19 ms | 6387.00 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 113.52 ms | 0.26 ms | 185.18 ms | 2768.48 ms | 6837.24 ms | 468776.67 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.28 ms | 0.27 ms | 0.45 ms | 0.69 ms | 9.95 ms | 166.33 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 158.81 ms | 0.28 ms | 229.50 ms | 3653.50 ms | 6757.65 ms | 611580.33 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 5.72 ms | 0.29 ms | 20.64 ms | 44.11 ms | 109.71 ms | 10401.33 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.17 ms | 0.30 ms | 17.89 ms | 39.22 ms | 97.02 ms | 9039.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 6.87 ms | 0.39 ms | 23.25 ms | 48.19 ms | 107.46 ms | 11436.33 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 112.29 ms | 0.55 ms | 287.91 ms | 1978.81 ms | 6923.54 ms | 390794.00 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 83.50 ms | 1.18 ms | 161.12 ms | 1811.05 ms | 5394.38 ms | 335853.33 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 80.90 ms | 1.25 ms | 151.90 ms | 1933.57 ms | 5493.23 ms | 336361.33 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 26.37 ms | 1.32 ms | 91.47 ms | 261.19 ms | 708.89 ms | 55269.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 130.89 ms | 1.33 ms | 37.36 ms | 3319.31 ms | 5504.34 ms | 565927.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 2.68 ms | 1.47 ms | 6.16 ms | 13.48 ms | 43.87 ms | 2949.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.75 ms | 1.73 ms | 6.07 ms | 14.61 ms | 31.43 ms | 3009.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 111.10 ms | 1.85 ms | 176.97 ms | 2499.66 ms | 5872.65 ms | 461977.00 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 2.76 ms | 2.00 ms | 5.83 ms | 13.87 ms | 179.87 ms | 3765.33 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.16 ms | 2.49 ms | 5.60 ms | 10.30 ms | 108.93 ms | 2301.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.27 ms | 2.66 ms | 6.85 ms | 14.03 ms | 33.80 ms | 3007.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 7.36 ms | 2.70 ms | 6.78 ms | 126.13 ms | 998.77 ms | 45336.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.90 ms | 2.87 ms | 4.84 ms | 6.75 ms | 95.95 ms | 2986.00 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 5.35 ms | 2.97 ms | 7.42 ms | 76.13 ms | 123.45 ms | 11568.33 | 
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 3.48 ms | 3.13 ms | 6.10 ms | 11.98 ms | 29.00 ms | 2400.67 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 3.83 ms | 3.21 ms | 6.82 ms | 16.41 ms | 156.18 ms | 4143.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.97 ms | 3.25 ms | 6.98 ms | 12.78 ms | 35.83 ms | 2456.33 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 3.97 ms | 3.42 ms | 7.56 ms | 13.93 ms | 30.60 ms | 2869.00 | 
| node (11.12) | [restana](http://github.com/jkyberneees/ana) (2.10) | 4.01 ms | 4.06 ms | 6.01 ms | 13.46 ms | 147.62 ms | 3118.00 | 
| go (1.12) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 5.29 ms | 4.31 ms | 10.36 ms | 22.18 ms | 166.61 ms | 5048.00 | 
| go (1.12) | [iris](http://iris-go.com) (11.1) | 5.46 ms | 4.41 ms | 10.59 ms | 22.81 ms | 155.23 ms | 5097.00 | 
| node (11.12) | [polka](http://github.com/lukeed/polka) (0.5) | 4.59 ms | 4.44 ms | 7.33 ms | 14.26 ms | 163.96 ms | 4422.33 | 
| node (11.12) | [rayo](http://rayo.js.org) (1.2) | 4.72 ms | 4.48 ms | 7.33 ms | 14.54 ms | 183.10 ms | 4834.33 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 6.02 ms | 4.49 ms | 12.00 ms | 25.15 ms | 230.98 ms | 8046.67 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 6.21 ms | 4.53 ms | 13.05 ms | 27.29 ms | 170.48 ms | 6678.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 5.04 ms | 4.55 ms | 7.25 ms | 13.94 ms | 276.77 ms | 5913.67 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 6.43 ms | 4.56 ms | 12.88 ms | 27.97 ms | 288.13 ms | 9366.00 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 6.34 ms | 4.58 ms | 12.91 ms | 27.03 ms | 204.12 ms | 8329.33 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 6.57 ms | 4.65 ms | 14.24 ms | 29.32 ms | 140.78 ms | 6228.67 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 6.03 ms | 4.69 ms | 11.30 ms | 24.29 ms | 215.44 ms | 5345.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.52 ms | 4.69 ms | 8.91 ms | 22.05 ms | 86.20 ms | 4566.00 | 
| node (11.12) | [foxify](http://foxify.js.org) (0.10) | 5.89 ms | 4.74 ms | 9.32 ms | 15.00 ms | 263.07 ms | 6641.00 | 
| node (11.12) | [koa](http://koajs.com) (2.7) | 6.29 ms | 4.87 ms | 9.54 ms | 16.87 ms | 248.31 ms | 7084.67 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 7.04 ms | 4.96 ms | 12.78 ms | 22.58 ms | 202.14 ms | 5284.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 7.79 ms | 5.17 ms | 15.99 ms | 31.50 ms | 209.28 ms | 7282.67 | 
| go (1.12) | [gf](http://goframe.org) (1.5) | 8.08 ms | 5.51 ms | 16.93 ms | 36.80 ms | 216.98 ms | 8096.67 | 
| node (11.12) | [fastify](http://fastify.io) (2.1) | 6.48 ms | 5.61 ms | 9.02 ms | 19.97 ms | 300.45 ms | 10261.67 | 
| node (11.12) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 8.13 ms | 5.95 ms | 12.45 ms | 30.53 ms | 452.00 ms | 16669.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 199.56 ms | 6.67 ms | 28.46 ms | 5081.76 ms | 7922.58 ms | 844654.33 | 
| node (11.12) | [express](http://expressjs.com) (4.16) | 7.79 ms | 6.76 ms | 11.50 ms | 21.09 ms | 333.84 ms | 11001.00 | 
| node (11.12) | [restify](http://restify.com) (8.2) | 7.37 ms | 7.34 ms | 9.72 ms | 17.39 ms | 192.59 ms | 4327.00 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 9.62 ms | 8.20 ms | 16.99 ms | 25.95 ms | 65.71 ms | 5415.00 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 11.64 ms | 9.68 ms | 19.73 ms | 34.05 ms | 284.67 ms | 9683.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 9.56 ms | 9.71 ms | 11.41 ms | 13.35 ms | 146.24 ms | 3220.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 13.46 ms | 9.97 ms | 20.99 ms | 67.29 ms | 937.21 ms | 33060.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.3) | 13.39 ms | 10.23 ms | 18.50 ms | 37.72 ms | 780.86 ms | 28821.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 15.23 ms | 13.00 ms | 27.95 ms | 40.06 ms | 72.64 ms | 8183.00 | 
| node (11.12) | [hapi](http://hapijs.com) (18.1) | 20.30 ms | 14.75 ms | 27.11 ms | 126.55 ms | 750.54 ms | 34852.67 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.8) | 16.23 ms | 15.16 ms | 25.26 ms | 36.61 ms | 113.84 ms | 7359.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 22.49 ms | 17.81 ms | 39.35 ms | 74.37 ms | 386.40 ms | 17186.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.6) | 21.08 ms | 17.86 ms | 30.75 ms | 46.54 ms | 530.54 ms | 19709.67 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 22.27 ms | 20.41 ms | 29.04 ms | 40.99 ms | 255.02 ms | 9968.33 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 25.86 ms | 21.30 ms | 42.17 ms | 66.55 ms | 463.35 ms | 17957.00 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 23.31 ms | 21.61 ms | 39.93 ms | 59.83 ms | 139.53 ms | 12099.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 22.52 ms | 22.14 ms | 29.44 ms | 32.94 ms | 121.64 ms | 5589.33 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 24.84 ms | 25.64 ms | 32.61 ms | 38.12 ms | 233.62 ms | 7100.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 55.36 ms | 27.70 ms | 72.06 ms | 559.49 ms | 845.95 ms | 103890.33 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 31.71 ms | 30.77 ms | 38.64 ms | 47.10 ms | 316.05 ms | 10438.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 31.28 ms | 31.10 ms | 37.39 ms | 43.13 ms | 175.30 ms | 6385.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 40.26 ms | 31.28 ms | 69.67 ms | 126.05 ms | 629.21 ms | 28903.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 35.54 ms | 33.02 ms | 46.59 ms | 52.15 ms | 327.44 ms | 9905.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 37.26 ms | 33.03 ms | 67.49 ms | 94.56 ms | 136.50 ms | 20446.00 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 52.33 ms | 47.57 ms | 94.16 ms | 126.39 ms | 190.15 ms | 27268.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 61.77 ms | 62.20 ms | 75.88 ms | 91.65 ms | 604.89 ms | 22248.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (vibora) (python)


:five: (kore) (c)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 418380.00 | 242.04 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 416397.67 | 498.34 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 376056.67 | 427.33 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 334421.33 | 379.76 MB |
| c (99) | [kore](http://kore.io) (3.1) | 318151.67 | 826.87 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 309054.67 | 299.79 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 294149.33 | 170.22 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 290857.33 | 468.42 MB |
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 273404.33 | 257.22 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 267545.33 | 548.02 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 265092.00 | 532.44 MB |
| java (8) | [act](http://actframework.org) (1.8) | 264880.00 | 517.43 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 250661.00 | 267.35 MB |
| node (11.12) | [restana](http://github.com/jkyberneees/ana) (2.10) | 235957.67 | 353.78 MB |
| node (11.12) | [polka](http://github.com/lukeed/polka) (0.5) | 211637.33 | 317.34 MB |
| node (11.12) | [rayo](http://rayo.js.org) (1.2) | 209020.67 | 313.02 MB |
| go (1.12) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 200646.00 | 268.73 MB |
| go (1.12) | [iris](http://iris-go.com) (11.1) | 193409.67 | 258.06 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 190861.67 | 310.87 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 185749.00 | 234.76 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 184005.00 | 246.38 MB |
| node (11.12) | [fastify](http://fastify.io) (2.1) | 180216.67 | 460.54 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 178439.67 | 312.80 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 177379.67 | 311.19 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 176384.33 | 237.25 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 173209.00 | 230.35 MB |
| node (11.12) | [foxify](http://foxify.js.org) (0.10) | 170091.00 | 357.18 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 169400.33 | 225.00 MB |
| node (11.12) | [koa](http://koajs.com) (2.7) | 160723.67 | 340.17 MB |
| node (11.12) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 156630.33 | 234.85 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 143873.00 | 369.53 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 139217.00 | 343.00 MB |
| go (1.12) | [gf](http://goframe.org) (1.5) | 139082.67 | 210.31 MB |
| node (11.12) | [express](http://expressjs.com) (4.16) | 135770.33 | 332.20 MB |
| node (11.12) | [restify](http://restify.com) (8.2) | 132505.67 | 232.69 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 110623.33 | 219.45 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 103567.00 | 222.68 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 102761.00 | 96.56 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 99235.67 | 492.83 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 95480.00 | 167.14 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 91092.33 | 451.93 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 89767.67 | 192.21 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 88348.00 | 219.09 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 85934.33 | 426.67 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.3) | 84959.67 | 142.77 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 80319.00 | 398.23 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 76802.33 | 398.12 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 67018.67 | 151.92 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 63733.00 | 331.58 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.8) | 61288.33 | 132.50 MB |
| node (11.12) | [hapi](http://hapijs.com) (18.1) | 60590.00 | 157.63 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 55485.33 | 89.54 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.18) | 51512.67 | 49.06 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.6) | 48994.67 | 90.83 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 46034.00 | 113.41 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 44733.67 | 72.87 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 44593.67 | 41.80 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 43393.00 | 83.64 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 40676.00 | 75.67 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 40098.33 | 37.55 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 39494.33 | 22.76 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 32174.00 | 39.37 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 31977.67 | 58.38 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 30680.00 | 54.63 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 29237.67 | 47.63 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 27328.00 | 54.52 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 26047.33 | 75.62 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 24714.67 | 14.25 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 22459.67 | 169.90 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 19158.33 | 41.71 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 18590.33 | 48.19 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 15682.67 | 46.32 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 4882.33 | 14.98 MB |
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

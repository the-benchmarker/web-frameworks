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
Last update: 2019-03-14
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


:five: lumen (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.08 ms | 0.09 ms | 0.12 ms | 0.15 ms | 7.19 ms | 72.00 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.17) | 2.78 ms | 0.15 ms | 10.17 ms | 26.73 ms | 74.05 ms | 5800.67 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.54 ms | 0.18 ms | 13.34 ms | 30.23 ms | 84.21 ms | 6932.00 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 115.48 ms | 0.27 ms | 206.30 ms | 2711.81 ms | 6859.80 ms | 465544.33 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 118.30 ms | 0.29 ms | 200.84 ms | 2643.82 ms | 6756.03 ms | 484191.67 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 105.80 ms | 0.29 ms | 184.81 ms | 2507.68 ms | 6787.08 ms | 442830.67 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.31 ms | 0.29 ms | 0.51 ms | 0.85 ms | 17.71 ms | 210.33 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 101.78 ms | 0.30 ms | 260.77 ms | 1913.55 ms | 6783.43 ms | 360300.00 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.67 ms | 0.32 ms | 19.65 ms | 42.21 ms | 96.42 ms | 9817.33 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 6.41 ms | 0.32 ms | 22.78 ms | 48.04 ms | 138.55 ms | 11405.67 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 7.24 ms | 0.44 ms | 23.74 ms | 47.87 ms | 104.15 ms | 11489.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 61.95 ms | 1.30 ms | 3.06 ms | 2088.77 ms | 5502.18 ms | 381478.67 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 82.36 ms | 1.36 ms | 173.52 ms | 1791.21 ms | 5544.46 ms | 317741.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 2.59 ms | 1.56 ms | 5.94 ms | 12.33 ms | 90.08 ms | 2954.00 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 33.45 ms | 2.02 ms | 102.57 ms | 391.22 ms | 1204.92 ms | 85066.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.86 ms | 2.06 ms | 5.86 ms | 13.94 ms | 32.03 ms | 2870.00 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 95.82 ms | 2.57 ms | 172.12 ms | 2220.07 ms | 6706.52 ms | 425453.33 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 3.56 ms | 2.58 ms | 7.58 ms | 16.45 ms | 38.07 ms | 3402.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 11.23 ms | 2.76 ms | 6.64 ms | 315.14 ms | 1280.85 ms | 71465.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.47 ms | 2.84 ms | 7.24 ms | 13.73 ms | 33.17 ms | 3016.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 3.64 ms | 2.99 ms | 7.01 ms | 13.57 ms | 27.49 ms | 2774.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.95 ms | 3.02 ms | 4.83 ms | 6.70 ms | 91.08 ms | 2341.33 | 
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 3.47 ms | 3.08 ms | 6.14 ms | 12.27 ms | 26.19 ms | 2435.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.87 ms | 3.19 ms | 6.87 ms | 12.10 ms | 27.86 ms | 2358.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 5.45 ms | 3.21 ms | 8.66 ms | 68.85 ms | 134.82 ms | 10830.33 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.86 ms | 3.39 ms | 6.12 ms | 12.32 ms | 102.90 ms | 2578.00 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 4.16 ms | 3.40 ms | 7.66 ms | 18.30 ms | 212.65 ms | 4833.67 | 
| go (1.12) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 5.53 ms | 4.46 ms | 10.70 ms | 22.96 ms | 107.83 ms | 4988.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 5.09 ms | 4.51 ms | 7.32 ms | 14.49 ms | 325.97 ms | 6857.67 | 
| go (1.12) | [gin](http://gin-gonic.github.io/gin) (1.3) | 6.26 ms | 4.62 ms | 13.12 ms | 26.54 ms | 159.81 ms | 5637.67 | 
| go (1.12) | [iris](http://iris-go.com) (11.1) | 5.93 ms | 4.65 ms | 11.54 ms | 24.68 ms | 107.14 ms | 4849.67 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 6.93 ms | 4.68 ms | 13.49 ms | 29.06 ms | 371.62 ms | 13066.33 | 
| go (1.11) | [beego](http://beego.me) (1.12) | 6.37 ms | 4.69 ms | 13.05 ms | 27.54 ms | 116.83 ms | 5864.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.49 ms | 4.69 ms | 8.99 ms | 19.50 ms | 155.36 ms | 4639.00 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 6.12 ms | 4.75 ms | 11.48 ms | 24.48 ms | 156.63 ms | 4949.33 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 6.53 ms | 4.80 ms | 13.44 ms | 27.47 ms | 161.96 ms | 5924.33 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 7.05 ms | 5.06 ms | 12.17 ms | 22.57 ms | 170.29 ms | 5587.33 | 
| node (11.11) | [restana](http://github.com/jkyberneees/ana) (2.9) | 6.84 ms | 5.46 ms | 9.62 ms | 18.62 ms | 255.54 ms | 7919.33 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 8.30 ms | 5.53 ms | 17.68 ms | 36.65 ms | 294.04 ms | 9151.33 | 
| go (1.12) | [gf](http://goframe.org) (1.5) | 21.39 ms | 5.74 ms | 46.69 ms | 290.07 ms | 782.25 ms | 52793.67 | 
| node (11.11) | [polka](http://github.com/lukeed/polka) (0.5) | 7.63 ms | 6.47 ms | 11.00 ms | 19.98 ms | 308.81 ms | 9422.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 7.97 ms | 6.57 ms | 12.92 ms | 22.89 ms | 213.71 ms | 6324.67 | 
| node (11.11) | [rayo](http://rayo.js.org) (1.2) | 7.69 ms | 6.73 ms | 11.36 ms | 20.78 ms | 277.15 ms | 8449.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 194.77 ms | 6.87 ms | 27.70 ms | 4817.10 ms | 7936.77 ms | 822603.33 | 
| node (11.11) | [foxify](http://foxify.js.org) (0.10) | 8.61 ms | 8.07 ms | 12.91 ms | 20.64 ms | 285.88 ms | 8004.00 | 
| node (11.11) | [fastify](http://fastify.io) (2.0) | 9.51 ms | 8.27 ms | 13.66 ms | 23.26 ms | 355.18 ms | 11797.00 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 10.21 ms | 8.61 ms | 18.74 ms | 27.61 ms | 51.24 ms | 5934.00 | 
| node (11.11) | [restify](http://restify.com) (8.1) | 11.22 ms | 9.64 ms | 14.58 ms | 35.77 ms | 336.61 ms | 11312.67 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 22.03 ms | 9.65 ms | 19.01 ms | 359.02 ms | 634.07 ms | 58675.33 | 
| node (11.11) | [express](http://expressjs.com) (4.16) | 12.13 ms | 9.96 ms | 17.17 ms | 37.18 ms | 415.79 ms | 15122.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 12.37 ms | 10.88 ms | 22.44 ms | 40.62 ms | 383.79 ms | 12155.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.3) | 16.91 ms | 11.44 ms | 20.58 ms | 109.06 ms | 1044.13 ms | 45477.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 12.96 ms | 13.15 ms | 15.11 ms | 17.27 ms | 107.20 ms | 2466.67 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 14.83 ms | 13.22 ms | 26.01 ms | 41.06 ms | 75.88 ms | 8411.67 | 
| node (11.11) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 19.12 ms | 13.46 ms | 27.30 ms | 142.95 ms | 734.79 ms | 33268.67 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.7) | 15.80 ms | 13.86 ms | 25.83 ms | 38.32 ms | 118.24 ms | 7528.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 17.54 ms | 14.59 ms | 34.31 ms | 46.86 ms | 75.75 ms | 10189.33 | 
| node (11.11) | [koa](http://koajs.com) (2.7) | 20.24 ms | 14.77 ms | 28.06 ms | 147.19 ms | 784.47 ms | 36692.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 24.82 ms | 19.91 ms | 45.87 ms | 66.56 ms | 390.80 ms | 17101.33 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 21.98 ms | 20.66 ms | 30.23 ms | 37.17 ms | 245.48 ms | 6908.67 | 
| swift (4.2) | [kitura](http://kitura.io) (2.6) | 23.91 ms | 20.83 ms | 36.46 ms | 52.36 ms | 603.90 ms | 17532.00 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 26.83 ms | 21.48 ms | 49.95 ms | 68.11 ms | 368.89 ms | 15564.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 22.83 ms | 22.19 ms | 29.86 ms | 46.10 ms | 230.68 ms | 8104.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 26.10 ms | 23.40 ms | 37.93 ms | 42.28 ms | 308.18 ms | 8623.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 37.42 ms | 29.48 ms | 75.25 ms | 136.11 ms | 263.68 ms | 28859.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 28.07 ms | 29.90 ms | 33.38 ms | 41.00 ms | 389.08 ms | 9977.67 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 29.57 ms | 31.34 ms | 37.19 ms | 40.21 ms | 312.74 ms | 9728.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 33.07 ms | 33.19 ms | 41.74 ms | 144.75 ms | 257.74 ms | 21365.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 43.34 ms | 35.30 ms | 77.62 ms | 106.69 ms | 369.78 ms | 23854.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 40.77 ms | 37.11 ms | 66.87 ms | 96.73 ms | 130.05 ms | 20391.00 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 53.67 ms | 50.80 ms | 87.69 ms | 119.16 ms | 180.62 ms | 24268.33 | 
| node (11.11) | [hapi](http://hapijs.com) (18.1) | 136.34 ms | 56.71 ms | 127.85 ms | 2040.05 ms | 3548.82 ms | 341731.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 68.61 ms | 68.37 ms | 86.35 ms | 101.45 ms | 691.29 ms | 27764.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (vibora) (python)


:four: (kore) (c)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 420217.67 | 243.09 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 368319.00 | 440.88 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 308371.00 | 350.13 MB |
| c (99) | [kore](http://kore.io) (3.1) | 305265.33 | 791.73 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 299227.33 | 290.26 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 289423.33 | 328.92 MB |
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 276827.33 | 260.24 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 275655.67 | 294.29 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 274838.33 | 552.53 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 271415.67 | 156.98 MB |
| java (8) | [act](http://actframework.org) (1.8) | 257153.67 | 502.34 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 247001.67 | 506.01 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 243907.00 | 388.60 MB |
| go (1.12) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 190878.00 | 255.13 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 189498.33 | 308.48 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 178806.67 | 225.39 MB |
| go (1.12) | [iris](http://iris-go.com) (11.1) | 177345.33 | 237.02 MB |
| go (1.12) | [gin](http://gin-gonic.github.io/gin) (1.3) | 173748.33 | 304.65 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 171654.67 | 227.98 MB |
| go (1.11) | [beego](http://beego.me) (1.12) | 171130.67 | 230.15 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 167361.67 | 293.47 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 165865.67 | 222.24 MB |
| node (11.11) | [restana](http://github.com/jkyberneees/ana) (2.9) | 149074.67 | 223.48 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 141987.00 | 365.01 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 135743.00 | 180.79 MB |
| node (11.11) | [polka](http://github.com/lukeed/polka) (0.5) | 135304.33 | 202.76 MB |
| go (1.12) | [gf](http://goframe.org) (1.5) | 132636.67 | 201.09 MB |
| node (11.11) | [rayo](http://rayo.js.org) (1.2) | 132541.00 | 198.72 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 125631.67 | 309.61 MB |
| node (11.11) | [fastify](http://fastify.io) (2.0) | 116648.00 | 305.66 MB |
| node (11.11) | [foxify](http://foxify.js.org) (0.10) | 116081.00 | 244.12 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 98411.33 | 212.06 MB |
| node (11.11) | [restify](http://restify.com) (8.1) | 93752.33 | 164.62 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 91649.00 | 455.16 MB |
| node (11.11) | [express](http://expressjs.com) (4.16) | 89259.67 | 218.51 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 88348.00 | 154.62 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 88143.67 | 174.94 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 86670.33 | 430.08 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 85872.33 | 212.98 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 80237.67 | 172.42 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 76700.67 | 380.14 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 76054.00 | 377.31 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 75884.00 | 71.36 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.3) | 74490.00 | 124.90 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 72548.33 | 376.99 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 68982.00 | 133.31 MB |
| node (11.11) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 65886.67 | 98.71 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 65180.67 | 103.40 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.7) | 63560.00 | 137.37 MB |
| node (11.11) | [koa](http://koajs.com) (2.7) | 59654.67 | 125.90 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 59215.00 | 134.23 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 57693.00 | 300.69 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.17) | 46346.00 | 44.19 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 45089.67 | 42.26 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 44244.67 | 41.48 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.6) | 41725.67 | 77.34 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 40905.67 | 100.81 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 38806.00 | 72.11 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 37813.33 | 46.48 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 36413.00 | 59.31 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 36213.67 | 20.85 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 34129.33 | 62.30 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 31992.67 | 52.11 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 29448.33 | 52.43 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 24735.67 | 49.32 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 23531.00 | 68.31 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 22515.33 | 12.99 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 19933.33 | 150.62 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 18573.33 | 40.45 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 17679.33 | 45.83 MB |
| node (11.11) | [hapi](http://hapijs.com) (18.1) | 15137.00 | 39.16 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 14328.67 | 42.38 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 4251.67 | 13.06 MB |
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

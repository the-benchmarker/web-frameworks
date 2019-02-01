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
Last update: 2019-02-01
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
threads: 9, connections: 1000
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: lumen (php)


:five: slim (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.08 ms | 0.08 ms | 0.12 ms | 0.14 ms | 0.96 ms | 29.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 3.61 ms | 0.20 ms | 13.13 ms | 30.61 ms | 80.04 ms | 6917.67 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.48 ms | 0.24 ms | 16.35 ms | 35.74 ms | 84.55 ms | 8292.00 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 175.15 ms | 0.38 ms | 297.06 ms | 3775.50 ms | 6830.36 ms | 641016.00 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 167.65 ms | 0.39 ms | 312.37 ms | 3625.32 ms | 6804.47 ms | 604921.33 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.40 ms | 0.40 ms | 0.63 ms | 0.95 ms | 12.30 ms | 229.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 7.33 ms | 0.43 ms | 24.34 ms | 49.99 ms | 114.13 ms | 11866.00 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 8.70 ms | 0.48 ms | 29.06 ms | 59.85 ms | 169.42 ms | 14325.00 | 
| php (7.3) | [laravel](http://laravel.com) (5.7) | 116.61 ms | 0.56 ms | 334.11 ms | 1820.46 ms | 6913.31 ms | 389023.67 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 9.58 ms | 0.64 ms | 29.88 ms | 59.41 ms | 129.35 ms | 14351.67 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 127.92 ms | 1.24 ms | 242.85 ms | 2843.26 ms | 5682.33 ms | 476098.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 61.81 ms | 1.76 ms | 4.27 ms | 2076.10 ms | 4952.05 ms | 370742.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 3.22 ms | 2.68 ms | 5.78 ms | 14.50 ms | 94.22 ms | 3008.00 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 117.54 ms | 2.89 ms | 226.44 ms | 2619.20 ms | 4965.09 ms | 435078.33 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 3.75 ms | 3.01 ms | 7.47 ms | 15.01 ms | 39.41 ms | 3171.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.80 ms | 3.37 ms | 7.31 ms | 14.82 ms | 35.32 ms | 3207.67 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 113.43 ms | 3.94 ms | 218.02 ms | 2545.29 ms | 4646.91 ms | 424107.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.95 ms | 4.14 ms | 9.75 ms | 17.10 ms | 40.49 ms | 3720.00 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 4.71 ms | 4.58 ms | 7.68 ms | 14.74 ms | 35.44 ms | 2770.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.77 ms | 4.62 ms | 7.23 ms | 14.08 ms | 106.94 ms | 2733.00 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.7) | 6.74 ms | 4.63 ms | 11.73 ms | 57.88 ms | 125.75 ms | 9699.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.17 ms | 4.72 ms | 5.80 ms | 9.44 ms | 59.26 ms | 1925.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 5.40 ms | 4.78 ms | 9.89 ms | 18.02 ms | 58.14 ms | 3632.33 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 5.46 ms | 4.88 ms | 10.34 ms | 21.05 ms | 165.77 ms | 4987.67 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 43.54 ms | 5.20 ms | 144.10 ms | 381.39 ms | 1008.50 ms | 81152.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.30 ms | 5.30 ms | 8.25 ms | 13.85 ms | 63.87 ms | 2796.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 6.02 ms | 5.91 ms | 10.03 ms | 12.87 ms | 78.16 ms | 3107.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.87 ms | 6.28 ms | 11.45 ms | 21.42 ms | 112.13 ms | 4683.67 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 7.67 ms | 6.33 ms | 12.34 ms | 24.58 ms | 207.82 ms | 5170.00 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 8.01 ms | 6.60 ms | 13.02 ms | 25.83 ms | 191.74 ms | 5540.00 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 7.90 ms | 6.61 ms | 12.68 ms | 24.57 ms | 172.04 ms | 5168.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 7.67 ms | 6.88 ms | 10.38 ms | 19.03 ms | 288.55 ms | 7829.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 8.61 ms | 7.15 ms | 14.19 ms | 28.90 ms | 222.21 ms | 6464.67 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 8.82 ms | 7.42 ms | 14.60 ms | 28.34 ms | 146.24 ms | 5631.00 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 8.91 ms | 7.53 ms | 14.49 ms | 28.61 ms | 218.00 ms | 6487.00 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 9.11 ms | 7.54 ms | 14.59 ms | 29.33 ms | 249.65 ms | 8976.33 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 10.04 ms | 8.13 ms | 17.49 ms | 36.92 ms | 285.11 ms | 10657.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 186.96 ms | 8.20 ms | 90.63 ms | 4125.01 ms | 7585.10 ms | 732464.00 | 
| node (11.9) | [restana](http://github.com/jkyberneees/ana) (2.7) | 13.85 ms | 9.30 ms | 24.20 ms | 65.17 ms | 539.43 ms | 21613.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 12.63 ms | 9.39 ms | 24.36 ms | 48.98 ms | 274.43 ms | 11334.33 | 
| node (11.9) | [rayo](http://rayo.js.org) (1.2) | 14.41 ms | 9.82 ms | 24.42 ms | 65.69 ms | 573.73 ms | 23086.33 | 
| node (11.9) | [polka](http://github.com/lukeed/polka) (0.5) | 15.47 ms | 10.37 ms | 26.80 ms | 68.41 ms | 637.81 ms | 25313.33 | 
| go (1.11) | [gf](http://gfer.me) (1.4) | 11.63 ms | 10.42 ms | 17.43 ms | 38.30 ms | 176.00 ms | 6900.67 | 
| node (11.9) | [foxify](http://foxify.js.org) (0.10) | 16.48 ms | 11.84 ms | 28.09 ms | 65.17 ms | 567.68 ms | 21975.33 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 15.30 ms | 12.24 ms | 21.88 ms | 46.73 ms | 689.64 ms | 25557.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 12.11 ms | 12.28 ms | 14.26 ms | 16.44 ms | 84.27 ms | 1972.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 14.19 ms | 12.66 ms | 25.21 ms | 44.48 ms | 292.74 ms | 11429.67 | 
| python (3.7) | [starlette](http://starlette.io) (0.10) | 14.65 ms | 13.18 ms | 22.72 ms | 35.05 ms | 152.83 ms | 6353.33 | 
| node (11.9) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 20.23 ms | 13.97 ms | 30.14 ms | 141.54 ms | 851.12 ms | 38812.33 | 
| node (11.9) | [koa](http://koajs.com) (2.7) | 21.53 ms | 14.36 ms | 31.39 ms | 193.66 ms | 854.99 ms | 44111.33 | 
| node (11.9) | [fastify](http://fastify.io) (1.13) | 21.95 ms | 14.78 ms | 31.68 ms | 169.40 ms | 902.63 ms | 44449.33 | 
| node (11.9) | [express](http://expressjs.com) (4.16) | 25.91 ms | 18.48 ms | 36.97 ms | 188.65 ms | 971.29 ms | 47137.67 | 
| node (11.9) | [restify](http://restify.com) (7.6) | 25.52 ms | 20.47 ms | 41.72 ms | 82.80 ms | 622.46 ms | 25825.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 27.01 ms | 21.32 ms | 52.85 ms | 79.28 ms | 126.69 ms | 15919.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 32.67 ms | 23.28 ms | 34.97 ms | 309.97 ms | 1506.18 ms | 82514.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 28.70 ms | 26.13 ms | 40.37 ms | 57.40 ms | 321.24 ms | 10585.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 35.37 ms | 26.67 ms | 64.99 ms | 100.60 ms | 330.71 ms | 20554.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 29.77 ms | 26.95 ms | 40.97 ms | 73.36 ms | 475.44 ms | 14217.00 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 31.41 ms | 32.31 ms | 40.11 ms | 54.60 ms | 317.90 ms | 12968.00 | 
| node (11.9) | [hapi](http://hapijs.com) (18.0) | 60.17 ms | 34.63 ms | 63.69 ms | 871.68 ms | 1948.36 ms | 141870.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 41.08 ms | 35.83 ms | 55.96 ms | 207.72 ms | 990.06 ms | 45437.00 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 40.51 ms | 37.15 ms | 53.00 ms | 103.51 ms | 271.43 ms | 15743.33 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 39.64 ms | 40.28 ms | 49.06 ms | 56.69 ms | 255.69 ms | 9268.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 48.80 ms | 41.84 ms | 89.85 ms | 160.52 ms | 380.99 ms | 31768.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 60.28 ms | 53.17 ms | 95.31 ms | 160.81 ms | 746.65 ms | 38276.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 64.09 ms | 56.73 ms | 113.13 ms | 157.55 ms | 235.83 ms | 33307.00 | 
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.1) | 85.53 ms | 74.95 ms | 143.13 ms | 186.37 ms | 343.54 ms | 39546.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 92.61 ms | 93.17 ms | 118.33 ms | 146.02 ms | 457.20 ms | 25963.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (evhtp) (cpp)


:five: (vibora) (python)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 315410.67 | 182.51 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 257291.33 | 307.97 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 256688.00 | 291.76 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 213043.67 | 206.86 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 208799.33 | 237.17 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 197536.67 | 185.90 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 192921.67 | 387.63 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 192293.33 | 309.57 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 183251.00 | 375.41 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 181376.00 | 194.00 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.7) | 180867.00 | 104.47 MB |
| java (8) | [act](http://actframework.org) (1.8) | 179041.67 | 349.58 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 149298.67 | 188.54 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 126563.33 | 169.29 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 126025.00 | 205.29 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 123410.67 | 165.15 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 122973.67 | 165.11 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 114253.33 | 200.55 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 111953.67 | 196.59 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 111249.33 | 149.74 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 111006.00 | 148.29 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 105268.67 | 270.68 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 101958.67 | 202.67 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 87363.33 | 215.36 MB |
| go (1.11) | [gf](http://gfer.me) (1.4) | 87170.67 | 132.34 MB |
| node (11.9) | [restana](http://github.com/jkyberneees/ana) (2.7) | 85920.33 | 128.44 MB |
| node (11.9) | [rayo](http://rayo.js.org) (1.2) | 82037.67 | 123.00 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 80771.33 | 76.02 MB |
| node (11.9) | [polka](http://github.com/lukeed/polka) (0.5) | 77106.67 | 115.58 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 75841.00 | 132.83 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 74652.67 | 371.50 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 72046.33 | 358.07 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 69894.67 | 117.53 MB |
| node (11.9) | [foxify](http://foxify.js.org) (0.10) | 69187.67 | 145.36 MB |
| python (3.7) | [starlette](http://starlette.io) (0.10) | 67210.00 | 144.89 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 65033.00 | 139.27 MB |
| node (11.9) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 63489.67 | 94.98 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 62913.67 | 312.23 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 62016.00 | 307.32 MB |
| node (11.9) | [koa](http://koajs.com) (2.7) | 59934.67 | 126.68 MB |
| node (11.9) | [fastify](http://fastify.io) (1.13) | 58899.33 | 145.05 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 57976.67 | 300.81 MB |
| c (99) | [kore](http://kore.io) (3.1) | 50346.33 | 136.53 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 49378.00 | 78.74 MB |
| php (7.3) | [laravel](http://laravel.com) (5.7) | 48600.00 | 253.17 MB |
| node (11.9) | [express](http://expressjs.com) (4.16) | 47129.00 | 115.11 MB |
| node (11.9) | [restify](http://restify.com) (7.6) | 42673.67 | 74.68 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 39869.67 | 90.40 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 39473.67 | 73.19 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 35492.00 | 33.81 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 34518.67 | 56.25 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 33692.00 | 31.59 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 32198.33 | 30.18 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 28990.00 | 71.36 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 28619.67 | 16.49 MB |
| node (11.9) | [hapi](http://hapijs.com) (18.0) | 26108.00 | 67.43 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 25984.00 | 42.34 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 25115.00 | 30.74 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 24812.33 | 45.29 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 21474.33 | 38.28 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 17511.67 | 10.10 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 16684.00 | 48.39 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 15686.67 | 31.26 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 14668.67 | 110.95 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 13308.00 | 34.49 MB |
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.1) | 11612.67 | 25.31 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 10554.00 | 31.16 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 2932.67 | 9.01 MB |
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

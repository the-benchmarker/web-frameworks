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
Last update: 2019-05-06
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


:five: zend-framework (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 0.07 ms | 0.08 ms | 0.10 ms | 0.14 ms | 4.43 ms | 36.00 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.19) | 2.60 ms | 0.13 ms | 9.84 ms | 26.43 ms | 96.15 ms | 5834.33 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.53 ms | 0.17 ms | 13.75 ms | 31.87 ms | 80.03 ms | 7244.00 | 
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 0.27 ms | 0.25 ms | 0.46 ms | 0.75 ms | 13.66 ms | 188.33 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 120.29 ms | 0.28 ms | 239.04 ms | 2529.79 ms | 6885.60 ms | 470797.33 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 112.45 ms | 0.29 ms | 216.36 ms | 2280.66 ms | 6882.03 ms | 449282.33 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 88.25 ms | 0.29 ms | 241.28 ms | 1363.03 ms | 6831.60 ms | 334744.33 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 125.15 ms | 0.29 ms | 214.14 ms | 2833.77 ms | 6757.24 ms | 485338.00 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.51 ms | 0.29 ms | 19.42 ms | 41.77 ms | 93.00 ms | 9739.67 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 116.70 ms | 0.30 ms | 220.26 ms | 2593.88 ms | 6779.95 ms | 467788.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 111.93 ms | 0.30 ms | 195.25 ms | 2617.39 ms | 6871.26 ms | 464452.67 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 6.33 ms | 0.31 ms | 23.27 ms | 49.06 ms | 131.13 ms | 11648.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 7.52 ms | 0.43 ms | 25.04 ms | 51.50 ms | 123.48 ms | 12306.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 61.94 ms | 1.28 ms | 2.93 ms | 1928.01 ms | 6592.67 ms | 417763.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 2.61 ms | 1.38 ms | 6.15 ms | 13.38 ms | 41.34 ms | 2943.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.60 ms | 1.76 ms | 5.72 ms | 12.83 ms | 41.80 ms | 2726.67 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 31.16 ms | 2.00 ms | 107.37 ms | 297.92 ms | 829.73 ms | 63261.00 | 
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 2.78 ms | 2.12 ms | 5.76 ms | 12.81 ms | 33.54 ms | 2602.33 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.17 ms | 2.45 ms | 5.56 ms | 10.15 ms | 153.09 ms | 3388.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 4.76 ms | 2.50 ms | 6.01 ms | 34.41 ms | 704.90 ms | 24688.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.74 ms | 2.58 ms | 4.93 ms | 7.17 ms | 35.77 ms | 1704.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.24 ms | 2.76 ms | 6.83 ms | 12.40 ms | 27.72 ms | 2736.33 | 
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 3.35 ms | 2.97 ms | 6.01 ms | 11.72 ms | 25.87 ms | 2373.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.62 ms | 3.06 ms | 6.43 ms | 11.07 ms | 28.68 ms | 2150.33 | 
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 4.09 ms | 3.20 ms | 7.85 ms | 18.86 ms | 162.52 ms | 4257.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 3.79 ms | 3.29 ms | 7.22 ms | 13.18 ms | 28.00 ms | 2725.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 5.00 ms | 3.38 ms | 7.63 ms | 62.62 ms | 116.89 ms | 9498.00 | 
| node (11.14) | [restana](http://github.com/jkyberneees/ana) (2.13) | 4.53 ms | 4.40 ms | 7.50 ms | 14.54 ms | 125.04 ms | 3381.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 4.92 ms | 4.52 ms | 7.17 ms | 13.51 ms | 213.77 ms | 4398.67 | 
| node (11.14) | [rayo](http://rayo.js.org) (1.2) | 5.21 ms | 4.53 ms | 8.32 ms | 16.02 ms | 282.60 ms | 8297.00 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 6.13 ms | 4.54 ms | 12.56 ms | 25.46 ms | 181.48 ms | 7020.00 | 
| node (11.14) | [polka](http://github.com/lukeed/polka) (0.5) | 5.11 ms | 4.55 ms | 8.29 ms | 14.79 ms | 237.50 ms | 6819.00 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 6.62 ms | 4.67 ms | 13.83 ms | 28.77 ms | 285.92 ms | 7925.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.45 ms | 4.68 ms | 9.11 ms | 21.05 ms | 156.10 ms | 4417.33 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 6.33 ms | 4.73 ms | 11.86 ms | 25.83 ms | 240.32 ms | 7782.67 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 6.72 ms | 4.74 ms | 13.91 ms | 28.73 ms | 239.87 ms | 8418.33 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 6.55 ms | 4.74 ms | 12.57 ms | 22.99 ms | 209.72 ms | 6211.67 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 7.17 ms | 4.75 ms | 15.23 ms | 32.49 ms | 302.62 ms | 9945.00 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 6.90 ms | 4.80 ms | 13.90 ms | 29.88 ms | 310.16 ms | 9580.67 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 6.40 ms | 4.87 ms | 12.37 ms | 26.56 ms | 68.53 ms | 5128.67 | 
| node (11.14) | [foxify](http://foxify.js.org) (0.10) | 6.52 ms | 4.93 ms | 9.72 ms | 19.05 ms | 277.19 ms | 8031.33 | 
| node (11.14) | [koa](http://koajs.com) (2.7) | 7.27 ms | 5.17 ms | 9.90 ms | 21.96 ms | 348.57 ms | 12319.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 8.76 ms | 5.82 ms | 18.42 ms | 33.29 ms | 171.21 ms | 8125.33 | 
| go (1.12) | [gf](http://goframe.org) (1.6) | 8.73 ms | 5.85 ms | 18.11 ms | 40.71 ms | 293.77 ms | 10278.67 | 
| node (11.14) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 8.09 ms | 6.07 ms | 13.47 ms | 28.96 ms | 354.41 ms | 11692.00 | 
| node (11.14) | [fastify](http://fastify.io) (2.3) | 8.92 ms | 6.36 ms | 12.58 ms | 37.44 ms | 484.72 ms | 21559.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 191.13 ms | 6.50 ms | 26.80 ms | 4941.51 ms | 7929.14 ms | 826427.00 | 
| node (11.14) | [express](http://expressjs.com) (4.16) | 8.81 ms | 7.78 ms | 12.83 ms | 25.69 ms | 363.73 ms | 12980.00 | 
| node (11.14) | [restify](http://restify.com) (8.2) | 8.29 ms | 7.79 ms | 11.42 ms | 22.96 ms | 198.44 ms | 5847.33 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 10.55 ms | 8.00 ms | 17.13 ms | 32.30 ms | 492.20 ms | 14489.33 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 10.12 ms | 8.28 ms | 19.62 ms | 30.20 ms | 79.65 ms | 6612.33 | 
| kotlin (1.3.31) | [ktor](http://ktor.io) (1.1.5) | 14.41 ms | 9.80 ms | 22.48 ms | 125.42 ms | 300.48 ms | 20205.67 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 14.36 ms | 9.97 ms | 17.53 ms | 81.45 ms | 996.81 ms | 40019.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 10.00 ms | 10.16 ms | 11.63 ms | 13.27 ms | 93.78 ms | 1653.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 13.04 ms | 10.48 ms | 21.89 ms | 43.34 ms | 791.12 ms | 25792.00 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.16) | 14.92 ms | 13.55 ms | 25.03 ms | 37.12 ms | 123.93 ms | 7543.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 16.63 ms | 14.32 ms | 29.79 ms | 41.80 ms | 69.55 ms | 9082.33 | 
| node (11.14) | [hapi](http://hapijs.com) (18.1) | 28.71 ms | 17.05 ms | 28.91 ms | 440.39 ms | 1242.96 ms | 77863.00 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 20.60 ms | 18.34 ms | 36.42 ms | 56.36 ms | 103.85 ms | 11745.00 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 21.08 ms | 18.51 ms | 32.56 ms | 55.21 ms | 382.04 ms | 12714.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 22.40 ms | 19.19 ms | 34.58 ms | 59.44 ms | 318.81 ms | 13422.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 22.40 ms | 19.50 ms | 32.48 ms | 41.58 ms | 245.81 ms | 9486.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 23.17 ms | 20.49 ms | 35.66 ms | 48.83 ms | 317.58 ms | 11347.33 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 22.07 ms | 20.56 ms | 27.51 ms | 42.53 ms | 299.48 ms | 7982.33 | 
| swift (5.0) | [kitura](http://kitura.io) (2.6) | 27.75 ms | 22.51 ms | 37.28 ms | 160.71 ms | 918.86 ms | 41383.00 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 27.68 ms | 26.31 ms | 38.98 ms | 47.46 ms | 396.31 ms | 11067.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 30.05 ms | 27.37 ms | 38.61 ms | 73.59 ms | 389.33 ms | 15872.00 | 
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 26.80 ms | 28.05 ms | 32.25 ms | 39.14 ms | 309.75 ms | 7814.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 32.32 ms | 29.53 ms | 42.64 ms | 61.33 ms | 320.65 ms | 12438.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 36.70 ms | 30.16 ms | 65.99 ms | 149.43 ms | 263.44 ms | 26691.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 43.25 ms | 41.45 ms | 75.75 ms | 110.67 ms | 157.99 ms | 22895.33 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 54.84 ms | 52.65 ms | 90.66 ms | 123.85 ms | 316.30 ms | 27782.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 65.34 ms | 60.00 ms | 94.60 ms | 136.97 ms | 578.90 ms | 27147.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 64.00 ms | 62.05 ms | 85.15 ms | 109.08 ms | 369.57 ms | 20739.67 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 449749.00 | 260.16 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 420606.33 | 503.58 MB |
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 360130.33 | 408.76 MB |
| c (99) | [kore](http://kore.io) (3.1) | 347578.00 | 903.77 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 328967.33 | 373.08 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 321499.67 | 311.71 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 299188.00 | 480.96 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 291493.67 | 585.56 MB |
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 287537.67 | 270.35 MB |
| java (8) | [act](http://actframework.org) (1.8) | 272489.00 | 470.53 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 265236.00 | 153.41 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 262447.33 | 279.63 MB |
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 257890.67 | 527.14 MB |
| node (11.14) | [restana](http://github.com/jkyberneees/ana) (2.13) | 212105.67 | 317.94 MB |
| node (11.14) | [rayo](http://rayo.js.org) (1.2) | 197071.33 | 295.45 MB |
| node (11.14) | [polka](http://github.com/lukeed/polka) (0.5) | 195730.33 | 293.37 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 192538.00 | 313.62 MB |
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 182571.00 | 230.47 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 178690.00 | 238.85 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 171305.33 | 227.00 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 166368.67 | 220.75 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 165382.00 | 289.82 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 164098.67 | 220.50 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 163919.33 | 287.24 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 161711.33 | 215.87 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 158269.00 | 406.67 MB |
| node (11.14) | [foxify](http://foxify.js.org) (0.10) | 157202.00 | 330.09 MB |
| node (11.14) | [fastify](http://fastify.io) (2.3) | 154215.33 | 379.17 MB |
| node (11.14) | [koa](http://koajs.com) (2.7) | 149504.00 | 316.55 MB |
| node (11.14) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 144773.67 | 216.91 MB |
| go (1.12) | [gf](http://goframe.org) (1.6) | 129716.67 | 196.98 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 129428.33 | 318.85 MB |
| node (11.14) | [express](http://expressjs.com) (4.16) | 121442.67 | 297.54 MB |
| node (11.14) | [restify](http://restify.com) (8.2) | 120909.33 | 212.31 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 103894.33 | 223.78 MB |
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 103337.67 | 205.25 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 100946.67 | 250.47 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 100118.00 | 93.98 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 88683.00 | 155.23 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 86394.00 | 144.92 MB |
| kotlin (1.3.31) | [ktor](http://ktor.io) (1.1.5) | 82396.00 | 128.25 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 80211.67 | 172.11 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 75164.67 | 372.32 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 74933.67 | 371.16 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 73153.67 | 362.48 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 70157.00 | 348.06 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 69709.33 | 361.64 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 68788.00 | 109.44 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.16) | 67813.00 | 146.66 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 60920.67 | 138.18 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 55040.00 | 286.71 MB |
| node (11.14) | [hapi](http://hapijs.com) (18.1) | 54521.00 | 140.88 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.19) | 50114.00 | 47.87 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 49972.00 | 96.38 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 48406.00 | 89.92 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 44911.00 | 110.68 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 44860.00 | 42.05 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 44375.00 | 41.62 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 42991.33 | 70.14 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.6) | 40205.67 | 74.54 MB |
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 37116.00 | 46.01 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 36408.33 | 21.01 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 35822.33 | 44.20 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 33646.33 | 61.44 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 30662.33 | 49.99 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 29815.67 | 53.09 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 23293.67 | 46.49 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 23162.33 | 13.36 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 20247.00 | 153.08 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 18132.33 | 39.47 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 17008.33 | 44.08 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 15161.67 | 44.79 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 15040.00 | 43.60 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 4124.33 | 12.62 MB |
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

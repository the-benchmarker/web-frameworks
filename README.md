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
Last update: 2019-05-09
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


:five: symfony (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 0.06 ms | 0.06 ms | 0.08 ms | 0.10 ms | 2.42 ms | 27.00 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.19) | 2.74 ms | 0.14 ms | 10.08 ms | 26.78 ms | 69.36 ms | 5817.33 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.71 ms | 0.18 ms | 14.12 ms | 32.93 ms | 84.04 ms | 7447.00 | 
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 0.30 ms | 0.30 ms | 0.47 ms | 0.78 ms | 14.23 ms | 196.33 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 108.14 ms | 0.30 ms | 201.96 ms | 2444.42 ms | 6775.92 ms | 431195.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 110.96 ms | 0.30 ms | 188.23 ms | 2676.12 ms | 6767.74 ms | 459327.67 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 103.80 ms | 0.30 ms | 267.58 ms | 1939.72 ms | 6854.20 ms | 363089.33 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 133.90 ms | 0.31 ms | 223.89 ms | 3112.74 ms | 6859.50 ms | 527994.00 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.81 ms | 0.32 ms | 20.32 ms | 43.95 ms | 108.82 ms | 10227.67 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 171.91 ms | 0.33 ms | 380.25 ms | 3401.77 ms | 7262.82 ms | 594607.67 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 6.31 ms | 0.33 ms | 22.97 ms | 46.42 ms | 112.66 ms | 11197.00 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 184.83 ms | 0.33 ms | 361.48 ms | 3789.40 ms | 7327.31 ms | 655699.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 7.83 ms | 0.43 ms | 26.49 ms | 54.46 ms | 128.17 ms | 12964.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 117.59 ms | 1.33 ms | 140.62 ms | 2491.17 ms | 6564.06 ms | 495385.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 2.48 ms | 1.48 ms | 5.65 ms | 12.22 ms | 137.80 ms | 2914.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.86 ms | 1.86 ms | 6.42 ms | 15.13 ms | 32.92 ms | 3142.00 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 2.79 ms | 2.07 ms | 5.37 ms | 8.88 ms | 69.20 ms | 1990.00 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 31.80 ms | 2.09 ms | 108.52 ms | 297.48 ms | 796.15 ms | 63352.00 | 
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 2.90 ms | 2.18 ms | 6.03 ms | 14.15 ms | 32.92 ms | 2837.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 7.57 ms | 2.60 ms | 6.20 ms | 132.06 ms | 1138.50 ms | 50950.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.60 ms | 2.72 ms | 4.01 ms | 5.97 ms | 25.61 ms | 1267.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.38 ms | 2.82 ms | 7.16 ms | 13.27 ms | 33.34 ms | 2933.00 | 
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 3.40 ms | 3.01 ms | 5.98 ms | 11.99 ms | 27.19 ms | 2373.33 | 
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 3.85 ms | 3.02 ms | 7.10 ms | 17.19 ms | 100.70 ms | 3940.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.41 ms | 3.02 ms | 5.46 ms | 10.49 ms | 33.70 ms | 1925.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 3.67 ms | 3.11 ms | 6.97 ms | 12.97 ms | 28.92 ms | 2653.00 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 5.71 ms | 3.23 ms | 8.32 ms | 78.15 ms | 130.09 ms | 11901.33 | 
| node (12.2) | [restana](http://github.com/jkyberneees/ana) (2.13) | 4.71 ms | 4.42 ms | 7.36 ms | 14.85 ms | 177.37 ms | 4781.33 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 5.66 ms | 4.43 ms | 11.37 ms | 23.11 ms | 176.94 ms | 5978.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 6.13 ms | 4.45 ms | 7.10 ms | 43.35 ms | 426.00 ms | 18176.67 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 6.22 ms | 4.51 ms | 12.59 ms | 26.18 ms | 227.66 ms | 8480.00 | 
| node (12.2) | [polka](http://github.com/lukeed/polka) (0.5) | 4.89 ms | 4.54 ms | 8.04 ms | 14.10 ms | 184.07 ms | 4214.33 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 5.71 ms | 4.57 ms | 10.73 ms | 22.76 ms | 107.65 ms | 4685.00 | 
| node (12.2) | [rayo](http://rayo.js.org) (1.2) | 5.21 ms | 4.59 ms | 8.49 ms | 15.11 ms | 221.98 ms | 5826.67 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 6.22 ms | 4.59 ms | 12.84 ms | 26.56 ms | 149.48 ms | 5910.00 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 6.24 ms | 4.61 ms | 12.92 ms | 26.08 ms | 167.76 ms | 6210.67 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 6.45 ms | 4.62 ms | 13.99 ms | 28.38 ms | 108.14 ms | 5845.33 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 6.05 ms | 4.64 ms | 11.48 ms | 25.23 ms | 229.25 ms | 6474.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 6.36 ms | 4.78 ms | 11.68 ms | 18.20 ms | 152.50 ms | 4629.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.64 ms | 4.89 ms | 9.13 ms | 19.45 ms | 159.08 ms | 4071.33 | 
| node (12.2) | [foxify](http://foxify.js.org) (0.10) | 6.67 ms | 5.22 ms | 9.77 ms | 17.09 ms | 202.84 ms | 5376.33 | 
| go (1.12) | [gf](http://goframe.org) (1.6) | 8.29 ms | 5.52 ms | 17.28 ms | 39.67 ms | 238.40 ms | 9694.67 | 
| node (12.2) | [koa](http://koajs.com) (2.7) | 7.46 ms | 5.74 ms | 10.51 ms | 21.77 ms | 312.03 ms | 10280.33 | 
| node (12.2) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 8.43 ms | 5.87 ms | 12.49 ms | 31.83 ms | 488.78 ms | 18740.33 | 
| node (12.2) | [fastify](http://fastify.io) (2.3) | 7.38 ms | 5.92 ms | 11.31 ms | 21.90 ms | 349.18 ms | 11757.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 7.71 ms | 6.03 ms | 14.19 ms | 25.21 ms | 212.90 ms | 6564.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 221.73 ms | 7.10 ms | 238.73 ms | 4899.96 ms | 7787.74 ms | 848539.67 | 
| node (12.2) | [restify](http://restify.com) (8.2) | 8.14 ms | 7.79 ms | 10.73 ms | 21.90 ms | 230.35 ms | 5855.00 | 
| kotlin (1.3) | [ktor](http://ktor.io) (1.1) | 10.52 ms | 7.99 ms | 19.26 ms | 34.09 ms | 178.22 ms | 7531.00 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 9.53 ms | 8.12 ms | 16.86 ms | 25.26 ms | 75.89 ms | 5189.67 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 10.46 ms | 8.35 ms | 16.99 ms | 31.63 ms | 217.44 ms | 7396.33 | 
| node (12.2) | [express](http://expressjs.com) (4.16) | 10.30 ms | 8.53 ms | 15.14 ms | 30.31 ms | 417.00 ms | 14066.00 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 13.06 ms | 9.45 ms | 17.20 ms | 67.34 ms | 783.71 ms | 29738.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 9.49 ms | 9.70 ms | 10.95 ms | 12.67 ms | 100.47 ms | 2806.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 11.78 ms | 10.31 ms | 21.82 ms | 40.45 ms | 182.65 ms | 8655.67 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.16) | 13.70 ms | 12.06 ms | 23.68 ms | 32.57 ms | 78.36 ms | 6920.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 15.25 ms | 13.54 ms | 25.39 ms | 34.63 ms | 59.41 ms | 6967.33 | 
| node (12.2) | [hapi](http://hapijs.com) (18.1) | 19.84 ms | 15.52 ms | 24.84 ms | 134.70 ms | 655.02 ms | 29495.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 22.46 ms | 16.59 ms | 43.57 ms | 69.22 ms | 320.18 ms | 16570.67 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 19.36 ms | 17.58 ms | 33.38 ms | 47.76 ms | 119.99 ms | 10020.00 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 24.18 ms | 19.54 ms | 43.65 ms | 64.04 ms | 238.51 ms | 13831.67 | 
| swift (5.0) | [kitura](http://kitura.io) (2.6) | 32.32 ms | 22.89 ms | 36.76 ms | 360.02 ms | 1512.53 ms | 70748.00 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 26.06 ms | 23.59 ms | 35.88 ms | 44.26 ms | 198.57 ms | 8864.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 28.02 ms | 24.29 ms | 39.70 ms | 55.78 ms | 467.07 ms | 20485.33 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 28.63 ms | 24.88 ms | 39.43 ms | 47.33 ms | 233.48 ms | 8630.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 25.22 ms | 25.24 ms | 33.90 ms | 41.53 ms | 234.79 ms | 7302.33 | 
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 27.89 ms | 27.02 ms | 34.71 ms | 43.65 ms | 203.01 ms | 8505.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 35.75 ms | 28.94 ms | 67.45 ms | 118.32 ms | 258.57 ms | 23978.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 29.11 ms | 29.42 ms | 37.42 ms | 77.19 ms | 428.33 ms | 19943.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 32.60 ms | 29.63 ms | 42.93 ms | 49.46 ms | 182.08 ms | 8472.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 40.10 ms | 32.72 ms | 72.47 ms | 103.36 ms | 165.25 ms | 22061.33 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 53.19 ms | 51.60 ms | 88.86 ms | 120.96 ms | 161.76 ms | 26906.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 65.53 ms | 54.30 ms | 120.91 ms | 159.99 ms | 551.22 ms | 35303.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 67.68 ms | 65.91 ms | 83.54 ms | 103.52 ms | 546.76 ms | 18296.33 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 440507.00 | 254.85 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 400929.33 | 479.44 MB |
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 355557.00 | 403.41 MB |
| c (99) | [kore](http://kore.io) (3.1) | 344100.33 | 894.41 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 337149.33 | 327.10 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 335957.33 | 539.95 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 317266.00 | 360.33 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 303665.00 | 610.65 MB |
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 282724.67 | 265.59 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 272767.00 | 157.78 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 272392.67 | 289.93 MB |
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 268632.67 | 549.42 MB |
| java (8) | [act](http://actframework.org) (1.8) | 266729.00 | 459.97 MB |
| node (12.2) | [restana](http://github.com/jkyberneees/ana) (2.13) | 208947.67 | 313.36 MB |
| node (12.2) | [polka](http://github.com/lukeed/polka) (0.5) | 198106.00 | 296.98 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 195232.00 | 317.74 MB |
| node (12.2) | [rayo](http://rayo.js.org) (1.2) | 192327.33 | 288.40 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 191651.33 | 256.62 MB |
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 184101.33 | 232.39 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 182933.00 | 242.44 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 180939.67 | 317.44 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 177229.67 | 233.41 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 174863.67 | 306.36 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 174651.00 | 234.70 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 173706.00 | 232.05 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 158111.33 | 405.90 MB |
| node (12.2) | [fastify](http://fastify.io) (2.3) | 157863.67 | 404.55 MB |
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 150195.33 | 297.85 MB |
| node (12.2) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 147003.00 | 220.29 MB |
| node (12.2) | [foxify](http://foxify.js.org) (0.10) | 146829.00 | 308.73 MB |
| node (12.2) | [koa](http://koajs.com) (2.7) | 140850.00 | 298.29 MB |
| go (1.12) | [gf](http://goframe.org) (1.6) | 140258.00 | 212.16 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 133355.00 | 328.29 MB |
| node (12.2) | [restify](http://restify.com) (8.2) | 122084.67 | 214.40 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 104900.33 | 225.83 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 104627.67 | 98.35 MB |
| node (12.2) | [express](http://expressjs.com) (4.16) | 103476.00 | 253.61 MB |
| kotlin (1.3) | [ktor](http://ktor.io) (1.1) | 100805.67 | 157.17 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 97221.33 | 241.15 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 94615.33 | 165.73 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 90098.33 | 150.47 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 84757.33 | 181.58 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.16) | 73147.33 | 157.93 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 71515.33 | 354.86 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 69553.00 | 344.77 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 65553.00 | 148.59 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 65350.33 | 338.85 MB |
| node (12.2) | [hapi](http://hapijs.com) (18.1) | 60580.00 | 158.15 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 58084.67 | 94.03 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 55374.00 | 275.06 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 53099.33 | 276.87 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 52109.33 | 100.67 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 48396.67 | 242.77 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.19) | 47108.33 | 44.85 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 46326.67 | 114.16 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 44544.00 | 82.75 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.6) | 40578.33 | 75.29 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 39728.67 | 37.24 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 38308.67 | 35.93 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 36184.33 | 66.11 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 35816.33 | 58.44 MB |
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 35533.67 | 44.29 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 34679.00 | 20.01 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 34372.67 | 42.21 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 30318.33 | 49.44 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 29511.33 | 52.55 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 25180.67 | 50.22 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 22010.67 | 12.69 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 20247.00 | 153.08 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 18781.00 | 40.89 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 16334.67 | 42.34 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 15131.33 | 43.90 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 14430.33 | 42.74 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 4040.67 | 12.34 MB |
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

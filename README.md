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
Last update: 2018-12-28
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


:five: laravel (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 0.08 ms | 0.08 ms | 0.12 ms | 0.16 ms | 1.31 ms | 30.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.15) | 4.70 ms | 0.27 ms | 16.78 ms | 35.61 ms | 86.78 ms | 8371.67 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 5.45 ms | 0.28 ms | 19.76 ms | 41.66 ms | 153.24 ms | 10450.33 | 
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 186.99 ms | 0.43 ms | 385.92 ms | 3902.71 ms | 7329.20 ms | 662058.33 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 208.15 ms | 0.44 ms | 451.97 ms | 4132.71 ms | 7380.77 ms | 712588.33 | 
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 0.46 ms | 0.45 ms | 0.75 ms | 1.10 ms | 26.95 ms | 288.67 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 173.35 ms | 0.45 ms | 302.06 ms | 3899.71 ms | 7455.28 ms | 661265.67 | 
| php (7.2) | [symfony](http://symfony.com) (4.2) | 147.08 ms | 0.48 ms | 342.67 ms | 2742.36 ms | 7497.58 ms | 521348.33 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 222.70 ms | 0.49 ms | 373.07 ms | 4822.04 ms | 7643.09 ms | 810709.00 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 8.31 ms | 0.49 ms | 27.85 ms | 54.01 ms | 125.95 ms | 13284.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 8.12 ms | 0.51 ms | 26.49 ms | 52.80 ms | 117.94 ms | 12695.33 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 10.42 ms | 0.77 ms | 31.27 ms | 59.70 ms | 125.22 ms | 14741.67 | 
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 214.99 ms | 0.86 ms | 392.56 ms | 4403.72 ms | 7616.15 ms | 761118.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 74.46 ms | 2.16 ms | 4.94 ms | 2706.89 ms | 6593.95 ms | 486463.33 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.65 ms | 2.50 ms | 8.06 ms | 17.94 ms | 37.28 ms | 3785.33 | 
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 3.59 ms | 2.96 ms | 6.55 ms | 12.64 ms | 91.21 ms | 2959.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.36 ms | 3.49 ms | 8.88 ms | 16.93 ms | 39.52 ms | 3603.33 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.09 ms | 3.62 ms | 6.39 ms | 13.12 ms | 110.76 ms | 2866.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 3.75 ms | 3.69 ms | 5.26 ms | 7.53 ms | 121.26 ms | 1436.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.56 ms | 3.70 ms | 5.52 ms | 8.74 ms | 39.43 ms | 1868.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.80 ms | 4.08 ms | 7.56 ms | 13.96 ms | 128.19 ms | 4659.33 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 4.72 ms | 4.21 ms | 8.64 ms | 15.66 ms | 32.30 ms | 3128.67 | 
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 4.32 ms | 4.31 ms | 5.98 ms | 12.23 ms | 33.09 ms | 2188.00 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.5) | 8.16 ms | 4.47 ms | 13.69 ms | 87.03 ms | 200.90 ms | 15154.67 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 4.75 ms | 4.62 ms | 7.85 ms | 14.66 ms | 33.67 ms | 2806.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 5.72 ms | 5.66 ms | 9.61 ms | 12.06 ms | 193.42 ms | 3146.33 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 44.01 ms | 6.49 ms | 143.38 ms | 363.21 ms | 857.07 ms | 78540.67 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 7.51 ms | 6.59 ms | 10.46 ms | 26.89 ms | 100.99 ms | 4537.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 7.26 ms | 6.60 ms | 10.25 ms | 19.08 ms | 217.56 ms | 5411.67 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 8.00 ms | 6.62 ms | 11.90 ms | 26.14 ms | 305.33 ms | 9298.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 7.67 ms | 6.65 ms | 11.82 ms | 34.30 ms | 168.94 ms | 7904.67 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 8.18 ms | 7.08 ms | 12.48 ms | 26.82 ms | 237.84 ms | 6721.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 8.31 ms | 7.11 ms | 13.21 ms | 27.17 ms | 272.92 ms | 6232.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 139.72 ms | 7.80 ms | 131.86 ms | 3533.81 ms | 7268.39 ms | 619298.33 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 9.12 ms | 7.88 ms | 13.65 ms | 29.13 ms | 309.14 ms | 9882.33 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 9.37 ms | 8.32 ms | 14.27 ms | 30.20 ms | 209.68 ms | 7369.00 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 9.57 ms | 8.68 ms | 14.62 ms | 29.69 ms | 183.10 ms | 6036.67 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 9.82 ms | 8.86 ms | 15.33 ms | 30.57 ms | 126.70 ms | 5525.33 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 11.24 ms | 9.11 ms | 21.35 ms | 39.02 ms | 273.21 ms | 9203.00 | 
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 14.86 ms | 9.67 ms | 23.05 ms | 91.84 ms | 685.00 ms | 31296.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 12.62 ms | 10.26 ms | 21.64 ms | 42.23 ms | 291.58 ms | 11320.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 11.90 ms | 11.47 ms | 14.69 ms | 17.24 ms | 144.26 ms | 6004.67 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 16.46 ms | 11.83 ms | 23.29 ms | 95.93 ms | 715.82 ms | 31621.67 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 15.98 ms | 12.11 ms | 23.92 ms | 70.49 ms | 586.11 ms | 24197.00 | 
| go (1.11) | [gf](http://gfer.me) (1.3) | 27.61 ms | 12.31 ms | 67.53 ms | 262.99 ms | 961.85 ms | 54392.33 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 18.11 ms | 12.89 ms | 23.15 ms | 132.49 ms | 1062.14 ms | 45940.00 | 
| node (11.1) | [fastify](http://fastify.io) (1.13) | 19.34 ms | 13.98 ms | 29.43 ms | 104.77 ms | 665.78 ms | 28494.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 17.77 ms | 15.40 ms | 30.15 ms | 52.58 ms | 944.96 ms | 26122.00 | 
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 24.90 ms | 15.84 ms | 29.00 ms | 327.65 ms | 1175.25 ms | 64105.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 22.67 ms | 21.31 ms | 38.87 ms | 54.61 ms | 84.29 ms | 11435.33 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 32.92 ms | 22.38 ms | 42.20 ms | 341.89 ms | 1278.55 ms | 65291.33 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 34.73 ms | 23.51 ms | 44.26 ms | 394.82 ms | 1269.63 ms | 72206.00 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 31.79 ms | 26.03 ms | 50.62 ms | 124.62 ms | 603.76 ms | 28282.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 29.26 ms | 26.92 ms | 43.29 ms | 59.51 ms | 190.88 ms | 10150.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 28.42 ms | 28.35 ms | 35.63 ms | 44.49 ms | 248.60 ms | 7806.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 37.96 ms | 31.58 ms | 60.16 ms | 105.88 ms | 806.46 ms | 28410.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 35.03 ms | 31.61 ms | 45.33 ms | 53.60 ms | 554.74 ms | 20475.33 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 32.70 ms | 33.44 ms | 40.69 ms | 48.27 ms | 167.98 ms | 6868.67 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 34.59 ms | 33.65 ms | 43.72 ms | 51.04 ms | 266.32 ms | 9644.33 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 39.75 ms | 37.94 ms | 47.11 ms | 63.73 ms | 572.01 ms | 23542.00 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 40.04 ms | 41.93 ms | 50.07 ms | 58.76 ms | 311.42 ms | 11041.67 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 87.30 ms | 45.29 ms | 83.21 ms | 1374.71 ms | 2671.58 ms | 218106.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 59.51 ms | 50.66 ms | 99.95 ms | 142.62 ms | 487.95 ms | 28076.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 64.40 ms | 54.16 ms | 119.80 ms | 162.80 ms | 232.69 ms | 36328.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 61.74 ms | 54.41 ms | 109.37 ms | 184.48 ms | 366.69 ms | 36833.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 88.80 ms | 86.94 ms | 108.68 ms | 131.76 ms | 491.30 ms | 20551.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (agoo-c) (c)


:three: (actix-web) (rust)


:four: (evhtp) (cpp)


:five: (vibora) (python)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 300878.67 | 360.26 MB |
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 262946.67 | 151.99 MB |
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 256192.67 | 291.36 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 248989.33 | 241.78 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 242475.00 | 275.47 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 228452.00 | 367.62 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 218951.00 | 440.32 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 208127.00 | 221.81 MB |
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 207952.00 | 425.50 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 198276.67 | 186.45 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.5) | 187522.33 | 108.41 MB |
| java (8) | [act](http://actframework.org) (1.8) | 172922.00 | 337.84 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 133411.33 | 234.30 MB |
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 130990.00 | 165.63 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 130618.33 | 212.70 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 127785.67 | 170.74 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 121818.33 | 161.44 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 119973.00 | 210.67 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 111104.00 | 148.78 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 107425.33 | 142.88 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 103234.00 | 181.21 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 101730.33 | 136.66 MB |
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 100795.00 | 200.00 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 93290.67 | 239.78 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 85711.33 | 128.33 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 84759.33 | 79.73 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 82055.00 | 202.34 MB |
| go (1.11) | [gf](http://gfer.me) (1.3) | 76272.67 | 116.01 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 73350.67 | 109.93 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 71128.33 | 106.63 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 67604.33 | 113.35 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 64831.67 | 113.40 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 60499.67 | 147.89 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 57681.00 | 121.20 MB |
| c (99) | [kore](http://kore.io) (3.1) | 55650.67 | 150.82 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 50957.67 | 109.16 MB |
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 48172.33 | 239.57 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 46718.00 | 231.96 MB |
| php (7.2) | [symfony](http://symfony.com) (4.2) | 45524.00 | 226.04 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 45372.67 | 102.92 MB |
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 45111.67 | 224.04 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 41715.33 | 217.11 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 40405.33 | 210.63 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 39584.67 | 96.88 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 38728.33 | 59.70 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 37789.00 | 79.86 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 35272.00 | 33.08 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 34245.00 | 63.54 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 33529.33 | 58.71 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 30523.00 | 28.60 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 28774.33 | 46.88 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 28539.00 | 52.08 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.15) | 27123.33 | 25.85 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 26727.33 | 65.84 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 25826.00 | 31.64 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 24878.00 | 40.52 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 23653.00 | 13.65 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 20054.00 | 51.95 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 16684.67 | 29.74 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 16666.67 | 48.39 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 15750.67 | 31.40 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 15724.00 | 9.07 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 15370.00 | 116.20 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 12262.67 | 31.81 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 11007.67 | 29.50 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 2894.00 | 8.89 MB |
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

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
Last update: 2019-03-13
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


:five: symfony (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.07 ms | 0.07 ms | 0.11 ms | 0.14 ms | 0.78 ms | 30.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.17) | 2.99 ms | 0.16 ms | 11.03 ms | 27.03 ms | 70.09 ms | 6016.67 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.86 ms | 0.20 ms | 14.38 ms | 32.42 ms | 82.82 ms | 7441.33 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.31 ms | 0.27 ms | 0.51 ms | 0.96 ms | 21.04 ms | 237.67 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 97.84 ms | 0.28 ms | 184.55 ms | 2114.41 ms | 6749.01 ms | 405952.00 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 89.54 ms | 0.29 ms | 175.11 ms | 1972.62 ms | 6831.01 ms | 376931.00 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 160.97 ms | 0.31 ms | 243.60 ms | 3589.94 ms | 6774.00 ms | 607959.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.72 ms | 0.32 ms | 19.86 ms | 42.32 ms | 93.65 ms | 9881.67 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 96.76 ms | 0.34 ms | 266.53 ms | 1475.49 ms | 6820.67 ms | 355885.67 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 6.65 ms | 0.35 ms | 22.97 ms | 47.34 ms | 115.34 ms | 11319.33 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 7.91 ms | 0.47 ms | 25.80 ms | 51.53 ms | 115.95 ms | 12455.67 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 88.55 ms | 1.24 ms | 159.94 ms | 2070.59 ms | 5473.07 ms | 359341.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 71.87 ms | 1.37 ms | 3.36 ms | 2196.98 ms | 6543.35 ms | 400176.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 2.82 ms | 1.55 ms | 6.58 ms | 14.60 ms | 86.21 ms | 3228.33 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 71.56 ms | 1.95 ms | 193.10 ms | 1278.35 ms | 3590.64 ms | 241117.33 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.04 ms | 2.07 ms | 6.45 ms | 15.16 ms | 33.18 ms | 3156.67 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 2.86 ms | 2.15 ms | 5.79 ms | 12.98 ms | 144.80 ms | 3169.33 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 30.17 ms | 2.22 ms | 102.11 ms | 275.61 ms | 732.18 ms | 58683.00 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.12 ms | 2.42 ms | 5.57 ms | 10.05 ms | 104.77 ms | 2504.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 6.94 ms | 2.69 ms | 6.52 ms | 118.51 ms | 1001.14 ms | 42424.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.61 ms | 2.93 ms | 7.69 ms | 14.61 ms | 35.66 ms | 3191.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.98 ms | 2.97 ms | 5.11 ms | 6.97 ms | 37.45 ms | 1695.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.68 ms | 3.15 ms | 6.45 ms | 10.26 ms | 26.10 ms | 2034.33 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 4.02 ms | 3.19 ms | 7.44 ms | 17.82 ms | 211.54 ms | 4779.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 3.81 ms | 3.25 ms | 7.24 ms | 13.61 ms | 29.46 ms | 2782.33 | 
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 3.59 ms | 3.27 ms | 6.19 ms | 12.31 ms | 28.46 ms | 2429.00 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 5.65 ms | 3.57 ms | 8.55 ms | 67.12 ms | 116.48 ms | 10764.67 | 
| go (1.12) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 5.57 ms | 4.41 ms | 10.64 ms | 22.88 ms | 201.38 ms | 7121.67 | 
| go (1.12) | [iris](http://iris-go.com) (11.1) | 5.45 ms | 4.43 ms | 10.64 ms | 22.49 ms | 134.97 ms | 4443.67 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 6.09 ms | 4.56 ms | 12.51 ms | 25.90 ms | 162.88 ms | 6191.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 5.22 ms | 4.62 ms | 7.56 ms | 14.24 ms | 281.57 ms | 6824.00 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 6.27 ms | 4.64 ms | 13.18 ms | 26.38 ms | 67.04 ms | 5425.33 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 5.98 ms | 4.69 ms | 11.31 ms | 23.72 ms | 104.96 ms | 4620.33 | 
| go (1.11) | [beego](http://beego.me) (1.12) | 6.45 ms | 4.69 ms | 13.30 ms | 28.10 ms | 122.53 ms | 6038.33 | 
| go (1.12) | [gin](http://gin-gonic.github.io/gin) (1.3) | 6.56 ms | 4.73 ms | 14.03 ms | 27.87 ms | 66.99 ms | 5769.00 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 6.96 ms | 4.76 ms | 15.39 ms | 31.34 ms | 79.78 ms | 6443.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.51 ms | 4.87 ms | 8.76 ms | 17.67 ms | 78.39 ms | 3665.67 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 7.49 ms | 5.26 ms | 13.47 ms | 25.78 ms | 161.95 ms | 5391.67 | 
| go (1.12) | [gf](http://goframe.org) (1.5) | 8.33 ms | 5.58 ms | 17.19 ms | 38.06 ms | 244.93 ms | 9979.33 | 
| node (11.11) | [restana](http://github.com/jkyberneees/ana) (2.9) | 7.33 ms | 6.43 ms | 10.34 ms | 19.15 ms | 279.62 ms | 8022.00 | 
| node (11.11) | [polka](http://github.com/lukeed/polka) (0.5) | 7.81 ms | 6.53 ms | 11.60 ms | 21.68 ms | 310.89 ms | 9821.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 8.24 ms | 6.59 ms | 14.31 ms | 26.99 ms | 212.56 ms | 6414.00 | 
| node (11.11) | [rayo](http://rayo.js.org) (1.2) | 7.92 ms | 6.89 ms | 11.34 ms | 20.08 ms | 339.55 ms | 10948.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 173.94 ms | 7.37 ms | 30.92 ms | 4145.30 ms | 7914.37 ms | 727765.33 | 
| node (11.11) | [foxify](http://foxify.js.org) (0.10) | 9.54 ms | 8.53 ms | 13.61 ms | 24.06 ms | 344.49 ms | 11359.33 | 
| node (11.11) | [restify](http://restify.com) (8.1) | 10.22 ms | 9.24 ms | 13.18 ms | 24.77 ms | 220.70 ms | 6143.67 | 
| node (11.11) | [fastify](http://fastify.io) (2.0) | 10.99 ms | 9.27 ms | 15.85 ms | 31.52 ms | 370.19 ms | 12361.00 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 12.33 ms | 9.34 ms | 22.65 ms | 41.19 ms | 290.90 ms | 10609.33 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 10.41 ms | 9.77 ms | 16.97 ms | 23.84 ms | 168.49 ms | 5853.33 | 
| node (11.11) | [express](http://expressjs.com) (4.16) | 12.16 ms | 9.82 ms | 16.59 ms | 34.19 ms | 483.74 ms | 18028.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.3) | 12.67 ms | 10.12 ms | 18.13 ms | 30.78 ms | 599.15 ms | 20556.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 10.16 ms | 10.34 ms | 11.89 ms | 13.59 ms | 86.46 ms | 1660.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 12.17 ms | 10.80 ms | 21.98 ms | 39.44 ms | 293.74 ms | 10179.67 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 14.24 ms | 13.25 ms | 23.86 ms | 33.91 ms | 117.80 ms | 6727.00 | 
| node (11.11) | [koa](http://koajs.com) (2.7) | 18.41 ms | 14.57 ms | 28.46 ms | 63.05 ms | 580.00 ms | 22050.00 | 
| node (11.11) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 22.12 ms | 14.72 ms | 33.07 ms | 178.81 ms | 808.05 ms | 40812.00 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.7) | 15.56 ms | 14.72 ms | 25.57 ms | 36.24 ms | 70.86 ms | 7673.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 16.62 ms | 15.78 ms | 25.53 ms | 33.60 ms | 55.54 ms | 6933.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.6) | 21.60 ms | 17.75 ms | 30.38 ms | 71.84 ms | 1025.71 ms | 31730.00 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 22.94 ms | 19.81 ms | 36.60 ms | 63.18 ms | 303.88 ms | 12515.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 24.46 ms | 21.67 ms | 34.06 ms | 37.88 ms | 309.60 ms | 9154.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 25.97 ms | 22.13 ms | 39.71 ms | 61.75 ms | 311.25 ms | 14100.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 23.55 ms | 23.85 ms | 28.63 ms | 36.72 ms | 303.82 ms | 8763.00 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 27.68 ms | 24.41 ms | 38.40 ms | 44.57 ms | 471.86 ms | 14520.67 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 26.95 ms | 29.05 ms | 35.22 ms | 38.52 ms | 198.16 ms | 7763.00 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 33.51 ms | 29.91 ms | 42.93 ms | 50.42 ms | 556.13 ms | 19225.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 31.02 ms | 31.13 ms | 37.22 ms | 40.98 ms | 245.64 ms | 6692.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 39.10 ms | 33.23 ms | 74.26 ms | 137.95 ms | 320.62 ms | 29028.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 43.96 ms | 36.73 ms | 84.92 ms | 123.91 ms | 168.62 ms | 26112.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 48.91 ms | 42.81 ms | 76.56 ms | 138.41 ms | 686.55 ms | 33057.67 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 56.00 ms | 47.26 ms | 94.31 ms | 127.10 ms | 158.15 ms | 25767.67 | 
| node (11.11) | [hapi](http://hapijs.com) (18.1) | 117.02 ms | 61.05 ms | 129.60 ms | 1620.24 ms | 3116.52 ms | 263789.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 68.95 ms | 67.93 ms | 90.69 ms | 115.25 ms | 542.70 ms | 26887.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (kore) (c)


:five: (fasthttprouter) (go)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 405094.67 | 234.32 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 357355.00 | 427.89 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 343816.00 | 390.54 MB |
| c (99) | [kore](http://kore.io) (3.1) | 313267.67 | 813.83 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 300435.67 | 486.60 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 299575.33 | 340.22 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 296643.00 | 287.83 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 277517.00 | 557.25 MB |
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 265469.33 | 249.71 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 261428.00 | 279.69 MB |
| java (8) | [act](http://actframework.org) (1.8) | 259619.33 | 507.20 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 257797.33 | 527.54 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 252033.33 | 145.85 MB |
| go (1.12) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 193515.00 | 259.06 MB |
| go (1.12) | [iris](http://iris-go.com) (11.1) | 191144.67 | 256.17 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 183521.00 | 299.14 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 178395.33 | 239.75 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 174767.67 | 220.33 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 174261.00 | 305.80 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 174189.00 | 231.75 MB |
| go (1.11) | [beego](http://beego.me) (1.12) | 170282.33 | 228.83 MB |
| go (1.12) | [gin](http://gin-gonic.github.io/gin) (1.3) | 168319.67 | 295.57 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 163250.00 | 217.08 MB |
| node (11.11) | [restana](http://github.com/jkyberneees/ana) (2.9) | 138153.00 | 207.01 MB |
| go (1.12) | [gf](http://goframe.org) (1.5) | 137022.00 | 207.28 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 135198.00 | 347.38 MB |
| node (11.11) | [polka](http://github.com/lukeed/polka) (0.5) | 133049.33 | 199.28 MB |
| node (11.11) | [rayo](http://rayo.js.org) (1.2) | 132177.00 | 197.90 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 123101.33 | 303.43 MB |
| node (11.11) | [foxify](http://foxify.js.org) (0.10) | 109102.00 | 229.43 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 105886.67 | 210.72 MB |
| node (11.11) | [fastify](http://fastify.io) (2.0) | 105149.00 | 264.25 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 97167.67 | 91.17 MB |
| node (11.11) | [restify](http://restify.com) (8.1) | 96601.00 | 169.61 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 94660.33 | 203.77 MB |
| node (11.11) | [express](http://expressjs.com) (4.16) | 89558.00 | 219.36 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 89539.67 | 444.80 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 88169.33 | 154.28 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 87965.33 | 436.34 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 85055.00 | 210.97 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.3) | 83924.67 | 140.70 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 82873.67 | 177.76 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 79662.67 | 394.73 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 79281.67 | 392.76 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 74417.33 | 385.75 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 70074.00 | 135.21 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 65534.67 | 103.61 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.7) | 64325.00 | 139.06 MB |
| node (11.11) | [koa](http://koajs.com) (2.7) | 60122.00 | 127.07 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 59806.00 | 135.59 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 59600.67 | 310.02 MB |
| node (11.11) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 57755.33 | 86.40 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.6) | 48891.67 | 90.76 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 44746.00 | 83.11 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 42800.67 | 40.17 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.17) | 42791.67 | 40.75 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 40664.33 | 38.10 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 38419.67 | 94.62 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 37052.33 | 60.43 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 35969.67 | 65.63 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 33155.67 | 19.14 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 32261.33 | 39.51 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 30029.33 | 48.91 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 27497.33 | 48.97 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 23611.00 | 47.05 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 22361.67 | 12.90 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 20634.33 | 59.72 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 19257.67 | 145.50 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 17911.00 | 38.99 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 16159.33 | 41.94 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 14060.00 | 41.38 MB |
| node (11.11) | [hapi](http://hapijs.com) (18.1) | 14018.00 | 36.22 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 4247.33 | 13.02 MB |
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

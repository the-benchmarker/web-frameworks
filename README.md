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
Last update: 2019-03-15
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: lumen (php)


:five: iron (rust)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.08 ms | 0.08 ms | 0.13 ms | 0.16 ms | 7.50 ms | 73.67 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.17) | 3.42 ms | 0.20 ms | 12.37 ms | 28.97 ms | 72.07 ms | 6551.00 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.15 ms | 0.22 ms | 15.48 ms | 34.44 ms | 88.65 ms | 7927.00 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 124.32 ms | 0.34 ms | 224.68 ms | 2928.05 ms | 6785.25 ms | 487496.33 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.36 ms | 0.37 ms | 0.57 ms | 0.78 ms | 28.93 ms | 266.00 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 7.37 ms | 0.37 ms | 25.53 ms | 52.65 ms | 152.58 ms | 12628.33 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 6.60 ms | 0.40 ms | 22.09 ms | 45.29 ms | 101.27 ms | 10743.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 8.31 ms | 0.50 ms | 27.20 ms | 55.24 ms | 119.01 ms | 13231.33 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 110.22 ms | 0.54 ms | 215.82 ms | 2435.27 ms | 6783.46 ms | 444562.00 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 111.81 ms | 0.61 ms | 298.56 ms | 1932.38 ms | 6851.42 ms | 385227.33 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 101.02 ms | 1.48 ms | 196.15 ms | 2192.12 ms | 5472.22 ms | 389082.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 130.20 ms | 1.51 ms | 4.13 ms | 4104.19 ms | 6593.72 ms | 674826.33 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 105.34 ms | 1.56 ms | 222.52 ms | 2167.38 ms | 5532.88 ms | 383290.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 99.97 ms | 1.59 ms | 199.12 ms | 2317.41 ms | 5543.04 ms | 382030.33 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 2.93 ms | 1.74 ms | 6.56 ms | 13.93 ms | 120.49 ms | 3403.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.00 ms | 2.04 ms | 6.34 ms | 14.23 ms | 31.48 ms | 3006.67 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 3.23 ms | 2.14 ms | 7.10 ms | 16.21 ms | 92.17 ms | 3428.00 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 31.44 ms | 2.49 ms | 106.39 ms | 286.63 ms | 799.92 ms | 60951.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.99 ms | 2.87 ms | 5.17 ms | 6.98 ms | 75.66 ms | 1724.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.77 ms | 2.97 ms | 8.16 ms | 15.94 ms | 37.85 ms | 3443.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 7.78 ms | 3.14 ms | 7.69 ms | 99.25 ms | 868.78 ms | 42156.67 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.77 ms | 3.20 ms | 6.02 ms | 12.47 ms | 110.58 ms | 2964.33 | 
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 3.67 ms | 3.33 ms | 6.45 ms | 12.90 ms | 30.19 ms | 2564.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 4.09 ms | 3.53 ms | 7.81 ms | 14.32 ms | 32.44 ms | 2949.00 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 4.20 ms | 3.69 ms | 7.85 ms | 17.63 ms | 74.03 ms | 3463.33 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 6.26 ms | 3.96 ms | 10.39 ms | 73.38 ms | 135.93 ms | 11123.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.95 ms | 4.87 ms | 7.69 ms | 13.96 ms | 36.80 ms | 2781.33 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 7.28 ms | 4.92 ms | 15.13 ms | 32.53 ms | 284.31 ms | 8564.67 | 
| go (1.12) | [gin](http://gin-gonic.github.io/gin) (1.3) | 7.24 ms | 5.11 ms | 14.88 ms | 31.03 ms | 132.77 ms | 6657.33 | 
| go (1.12) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 6.79 ms | 5.15 ms | 13.00 ms | 28.31 ms | 213.98 ms | 6346.00 | 
| go (1.12) | [iris](http://iris-go.com) (11.1) | 6.92 ms | 5.25 ms | 12.89 ms | 28.56 ms | 185.71 ms | 6649.00 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 7.06 ms | 5.36 ms | 13.09 ms | 28.34 ms | 110.30 ms | 5529.67 | 
| node (11.11) | [restana](http://github.com/jkyberneees/ana) (2.9) | 7.29 ms | 5.50 ms | 9.87 ms | 21.74 ms | 349.15 ms | 11524.67 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 8.02 ms | 5.57 ms | 16.23 ms | 33.93 ms | 190.82 ms | 8301.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.40 ms | 5.58 ms | 10.89 ms | 21.49 ms | 176.33 ms | 5017.33 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 8.23 ms | 5.78 ms | 16.84 ms | 35.03 ms | 172.92 ms | 7170.33 | 
| go (1.11) | [beego](http://beego.me) (1.12) | 8.23 ms | 5.87 ms | 16.38 ms | 34.92 ms | 125.10 ms | 7370.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 6.77 ms | 5.88 ms | 9.74 ms | 19.04 ms | 293.30 ms | 6507.33 | 
| go (1.12) | [gf](http://goframe.org) (1.5) | 8.95 ms | 6.44 ms | 17.58 ms | 38.81 ms | 281.40 ms | 9424.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 164.05 ms | 6.68 ms | 24.73 ms | 4064.17 ms | 6755.61 ms | 684017.67 | 
| node (11.11) | [rayo](http://rayo.js.org) (1.2) | 7.78 ms | 6.75 ms | 11.80 ms | 23.05 ms | 271.04 ms | 8343.67 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 9.61 ms | 6.79 ms | 17.82 ms | 38.35 ms | 236.57 ms | 11636.67 | 
| node (11.11) | [polka](http://github.com/lukeed/polka) (0.5) | 8.38 ms | 7.46 ms | 12.60 ms | 24.72 ms | 301.64 ms | 9890.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 30.61 ms | 7.72 ms | 24.33 ms | 611.97 ms | 794.24 ms | 98357.33 | 
| node (11.11) | [fastify](http://fastify.io) (2.0) | 10.23 ms | 8.52 ms | 14.75 ms | 30.06 ms | 381.70 ms | 12696.67 | 
| node (11.11) | [foxify](http://foxify.js.org) (0.10) | 10.29 ms | 8.57 ms | 14.64 ms | 31.67 ms | 422.65 ms | 14758.67 | 
| node (11.11) | [restify](http://restify.com) (8.1) | 10.50 ms | 9.17 ms | 13.93 ms | 28.81 ms | 260.17 ms | 7558.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.3) | 13.66 ms | 10.79 ms | 18.99 ms | 44.51 ms | 621.28 ms | 21666.00 | 
| node (11.11) | [express](http://expressjs.com) (4.16) | 14.80 ms | 10.94 ms | 19.12 ms | 96.62 ms | 631.77 ms | 27452.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 11.08 ms | 11.24 ms | 12.89 ms | 14.70 ms | 23.25 ms | 1575.67 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 13.15 ms | 12.05 ms | 22.84 ms | 35.36 ms | 94.36 ms | 7139.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 13.76 ms | 12.42 ms | 25.37 ms | 43.66 ms | 180.33 ms | 9425.00 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 14.28 ms | 12.66 ms | 21.97 ms | 39.41 ms | 362.19 ms | 12010.33 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 15.16 ms | 13.38 ms | 26.01 ms | 39.14 ms | 82.53 ms | 7802.33 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.7) | 16.39 ms | 14.99 ms | 26.86 ms | 39.49 ms | 327.63 ms | 12626.33 | 
| node (11.11) | [koa](http://koajs.com) (2.7) | 20.51 ms | 15.36 ms | 31.37 ms | 94.89 ms | 729.36 ms | 31723.33 | 
| node (11.11) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 23.20 ms | 16.31 ms | 34.55 ms | 163.20 ms | 831.60 ms | 39041.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.6) | 21.60 ms | 18.16 ms | 32.07 ms | 52.88 ms | 604.62 ms | 24608.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 42.96 ms | 18.56 ms | 34.33 ms | 522.51 ms | 625.09 ms | 95805.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 25.47 ms | 19.93 ms | 42.46 ms | 74.53 ms | 676.06 ms | 23392.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 23.06 ms | 21.96 ms | 30.79 ms | 35.62 ms | 138.66 ms | 6215.33 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 26.24 ms | 22.18 ms | 42.50 ms | 70.25 ms | 251.58 ms | 14007.33 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 24.90 ms | 23.25 ms | 31.16 ms | 49.51 ms | 313.12 ms | 10844.67 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 26.13 ms | 26.97 ms | 34.18 ms | 39.79 ms | 244.17 ms | 7349.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 31.94 ms | 28.39 ms | 46.28 ms | 58.92 ms | 323.62 ms | 11881.00 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 31.02 ms | 30.50 ms | 37.25 ms | 45.30 ms | 388.26 ms | 10243.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 40.00 ms | 33.19 ms | 75.49 ms | 131.04 ms | 227.62 ms | 27259.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 32.28 ms | 33.21 ms | 39.13 ms | 47.39 ms | 323.44 ms | 9751.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 43.64 ms | 34.37 ms | 88.37 ms | 130.94 ms | 186.84 ms | 28425.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 48.44 ms | 36.84 ms | 93.87 ms | 145.72 ms | 565.30 ms | 35456.00 | 
| node (11.11) | [hapi](http://hapijs.com) (18.1) | 131.23 ms | 59.70 ms | 135.80 ms | 1846.80 ms | 3195.83 ms | 306622.67 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 76.15 ms | 68.27 ms | 134.12 ms | 182.05 ms | 223.29 ms | 38600.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 76.71 ms | 74.74 ms | 99.98 ms | 129.68 ms | 598.26 ms | 25394.33 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 364306.00 | 210.85 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 353658.33 | 423.23 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 329950.67 | 374.96 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 291829.67 | 283.19 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 289667.33 | 328.98 MB |
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 260075.67 | 244.62 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 249517.33 | 403.76 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 244989.00 | 260.67 MB |
| c (99) | [kore](http://kore.io) (3.1) | 244782.33 | 635.68 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 238612.33 | 488.60 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 221339.00 | 128.08 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 213734.33 | 429.57 MB |
| java (8) | [act](http://actframework.org) (1.8) | 200773.67 | 391.64 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 168710.33 | 212.51 MB |
| go (1.12) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 153254.33 | 206.08 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 152182.33 | 203.28 MB |
| go (1.12) | [iris](http://iris-go.com) (11.1) | 151923.33 | 203.58 MB |
| go (1.12) | [gin](http://gin-gonic.github.io/gin) (1.3) | 150334.33 | 263.69 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 148959.00 | 199.28 MB |
| node (11.11) | [restana](http://github.com/jkyberneees/ana) (2.9) | 146721.00 | 219.93 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 140697.33 | 229.24 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 136074.33 | 239.02 MB |
| node (11.11) | [rayo](http://rayo.js.org) (1.2) | 131372.33 | 196.98 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 131157.67 | 173.84 MB |
| go (1.11) | [beego](http://beego.me) (1.12) | 129812.67 | 175.29 MB |
| node (11.11) | [polka](http://github.com/lukeed/polka) (0.5) | 123230.00 | 184.40 MB |
| go (1.12) | [gf](http://goframe.org) (1.5) | 122851.67 | 185.99 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 116134.33 | 298.23 MB |
| node (11.11) | [fastify](http://fastify.io) (2.0) | 110740.33 | 287.80 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 109483.67 | 269.71 MB |
| node (11.11) | [foxify](http://foxify.js.org) (0.10) | 104071.67 | 218.70 MB |
| node (11.11) | [restify](http://restify.com) (8.1) | 96113.00 | 168.76 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 90471.67 | 84.98 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 88756.33 | 175.93 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 85704.67 | 183.43 MB |
| node (11.11) | [express](http://expressjs.com) (4.16) | 81282.33 | 198.74 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.3) | 78871.00 | 132.17 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 78739.00 | 137.86 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 76037.00 | 163.61 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 75997.00 | 377.04 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 73614.33 | 364.88 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 72576.00 | 179.99 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 72238.67 | 358.63 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 70135.00 | 348.55 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 65889.00 | 127.21 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 62387.33 | 323.69 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.7) | 61565.67 | 132.86 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 58022.33 | 92.26 MB |
| node (11.11) | [koa](http://koajs.com) (2.7) | 56617.67 | 119.61 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 54950.00 | 286.19 MB |
| node (11.11) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 53254.00 | 79.57 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 51683.33 | 117.21 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.6) | 47445.67 | 87.94 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 43531.33 | 40.80 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 40693.67 | 100.32 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 39643.67 | 37.19 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 38906.33 | 72.41 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 38040.33 | 61.99 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.17) | 37567.67 | 35.81 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 32829.67 | 40.20 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 31065.33 | 50.62 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 30900.67 | 17.84 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 30632.33 | 55.93 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 26479.67 | 47.13 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 24689.00 | 49.20 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 21981.00 | 63.80 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 19370.00 | 11.17 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 17366.00 | 131.43 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 15432.67 | 40.01 MB |
| node (11.11) | [hapi](http://hapijs.com) (18.1) | 14288.67 | 37.00 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 13284.33 | 28.94 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 12588.00 | 37.14 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 4077.33 | 12.50 MB |
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

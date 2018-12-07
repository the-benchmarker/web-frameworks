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
Last update: 2018-12-07
```
OS: Linux (version: 4.19.6-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: laravel (php)


:four: symfony (php)


:five: slim (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 0.06 ms | 0.06 ms | 0.09 ms | 0.11 ms | 0.69 ms | 22.67 | 
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 0.32 ms | 0.30 ms | 0.51 ms | 0.88 ms | 20.46 ms | 244.33 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 106.56 ms | 0.33 ms | 279.61 ms | 1890.48 ms | 6873.63 ms | 369012.67 | 
| php (7.2) | [symfony](http://symfony.com) (4.2) | 117.35 ms | 0.34 ms | 244.81 ms | 2457.38 ms | 6755.93 ms | 430465.33 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 131.72 ms | 0.35 ms | 239.26 ms | 3003.21 ms | 6790.61 ms | 503971.67 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 192.94 ms | 0.38 ms | 299.28 ms | 4231.76 ms | 7059.24 ms | 711097.33 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.42 ms | 0.69 ms | 12.97 ms | 42.75 ms | 135.25 ms | 8684.00 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 3.29 ms | 0.88 ms | 9.21 ms | 29.12 ms | 100.67 ms | 5925.67 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 7.60 ms | 0.96 ms | 23.58 ms | 70.43 ms | 203.13 ms | 14622.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 52.26 ms | 1.50 ms | 3.04 ms | 1862.92 ms | 5504.58 ms | 364815.33 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 2.77 ms | 1.95 ms | 5.69 ms | 14.29 ms | 76.52 ms | 2782.00 | 
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 3.10 ms | 2.22 ms | 6.42 ms | 13.98 ms | 91.94 ms | 3238.33 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 8.61 ms | 2.43 ms | 25.24 ms | 67.06 ms | 187.96 ms | 14147.33 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.23 ms | 2.56 ms | 6.37 ms | 13.40 ms | 34.59 ms | 2882.33 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 7.05 ms | 2.73 ms | 19.72 ms | 50.19 ms | 136.85 ms | 10617.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.36 ms | 3.36 ms | 5.47 ms | 7.95 ms | 108.35 ms | 2206.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.31 ms | 3.42 ms | 8.78 ms | 16.49 ms | 50.33 ms | 3626.33 | 
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 3.85 ms | 3.45 ms | 5.19 ms | 9.30 ms | 281.50 ms | 7315.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.01 ms | 3.67 ms | 6.17 ms | 12.43 ms | 77.99 ms | 2448.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.53 ms | 3.83 ms | 7.32 ms | 13.73 ms | 46.47 ms | 2723.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 4.70 ms | 4.19 ms | 8.78 ms | 15.98 ms | 33.52 ms | 3220.67 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 6.89 ms | 4.53 ms | 12.94 ms | 55.15 ms | 125.06 ms | 9675.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.19 ms | 5.25 ms | 10.50 ms | 20.90 ms | 132.22 ms | 5097.33 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 6.40 ms | 5.35 ms | 10.57 ms | 21.10 ms | 210.73 ms | 4659.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 6.14 ms | 5.42 ms | 9.15 ms | 11.09 ms | 2554.25 ms | 35716.67 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 6.52 ms | 5.43 ms | 10.81 ms | 21.24 ms | 105.66 ms | 4102.33 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 7.13 ms | 5.49 ms | 10.29 ms | 31.52 ms | 129.80 ms | 5578.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 6.23 ms | 5.59 ms | 9.22 ms | 16.85 ms | 211.89 ms | 3997.00 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 7.03 ms | 5.59 ms | 11.26 ms | 22.99 ms | 288.66 ms | 8242.33 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 7.15 ms | 5.76 ms | 12.01 ms | 24.75 ms | 220.64 ms | 5630.33 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 7.17 ms | 5.79 ms | 11.93 ms | 24.41 ms | 218.39 ms | 5976.67 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 7.26 ms | 5.83 ms | 12.23 ms | 24.96 ms | 167.30 ms | 5706.00 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 7.97 ms | 6.19 ms | 12.57 ms | 26.13 ms | 424.17 ms | 11938.00 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 8.56 ms | 7.03 ms | 13.46 ms | 31.27 ms | 342.16 ms | 11443.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 144.62 ms | 7.45 ms | 119.21 ms | 3115.88 ms | 5932.69 ms | 557079.67 | 
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 11.63 ms | 8.30 ms | 21.29 ms | 51.84 ms | 391.49 ms | 14326.67 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 8.94 ms | 8.38 ms | 14.58 ms | 26.84 ms | 225.55 ms | 8382.67 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 13.04 ms | 8.54 ms | 21.17 ms | 72.25 ms | 601.18 ms | 26336.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 10.35 ms | 8.74 ms | 17.59 ms | 33.45 ms | 285.65 ms | 9332.00 | 
| go (1.11) | [gf](http://gfer.me) (1.3) | 10.11 ms | 9.00 ms | 14.49 ms | 30.30 ms | 427.51 ms | 10624.67 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 43.16 ms | 10.32 ms | 138.74 ms | 347.02 ms | 915.51 ms | 75274.33 | 
| node (11.1) | [fastify](http://fastify.io) (1.13) | 15.78 ms | 10.79 ms | 26.89 ms | 67.34 ms | 613.85 ms | 24355.33 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 13.97 ms | 11.40 ms | 20.09 ms | 32.83 ms | 598.44 ms | 20610.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 11.52 ms | 11.71 ms | 13.32 ms | 15.22 ms | 172.31 ms | 3541.00 | 
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 16.62 ms | 11.89 ms | 26.83 ms | 71.91 ms | 650.14 ms | 25827.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 14.22 ms | 12.27 ms | 25.15 ms | 46.06 ms | 419.27 ms | 16351.33 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 16.31 ms | 13.46 ms | 26.42 ms | 51.94 ms | 333.52 ms | 12550.33 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 19.94 ms | 13.57 ms | 30.37 ms | 129.16 ms | 858.24 ms | 39844.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 21.63 ms | 17.49 ms | 42.35 ms | 70.51 ms | 138.21 ms | 14439.33 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 25.21 ms | 18.96 ms | 40.79 ms | 107.22 ms | 802.32 ms | 34576.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 24.51 ms | 21.57 ms | 36.19 ms | 52.58 ms | 457.49 ms | 16495.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 30.96 ms | 23.90 ms | 59.57 ms | 89.53 ms | 342.44 ms | 19824.00 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 30.37 ms | 24.92 ms | 40.45 ms | 107.83 ms | 508.13 ms | 27280.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 28.86 ms | 25.19 ms | 42.30 ms | 57.81 ms | 462.50 ms | 18200.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 26.86 ms | 25.52 ms | 35.80 ms | 69.86 ms | 318.90 ms | 11931.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 32.14 ms | 27.79 ms | 45.08 ms | 111.57 ms | 399.00 ms | 18326.00 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 28.00 ms | 28.59 ms | 38.38 ms | 46.78 ms | 246.29 ms | 8947.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 35.97 ms | 31.61 ms | 51.52 ms | 109.30 ms | 346.83 ms | 19968.33 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 61.20 ms | 32.41 ms | 61.06 ms | 876.85 ms | 1858.97 ms | 147801.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 49.13 ms | 42.45 ms | 89.66 ms | 166.47 ms | 307.36 ms | 32799.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 56.93 ms | 44.44 ms | 108.68 ms | 185.85 ms | 682.10 ms | 42085.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 62.72 ms | 50.38 ms | 123.06 ms | 199.88 ms | 312.37 ms | 41490.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 77.71 ms | 77.50 ms | 104.03 ms | 138.25 ms | 469.26 ms | 26306.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (actix-web) (rust)


:three: (japronto) (python)


:four: (evhtp) (cpp)


:five: (vibora) (python)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 385952.00 | 223.33 MB |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 322040.00 | 366.18 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 307585.00 | 368.25 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 260480.67 | 252.94 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 249748.00 | 283.69 MB |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 242330.33 | 497.08 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 235167.33 | 472.52 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 230278.33 | 369.58 MB |
| java (8) | [act](http://actframework.org) (1.8) | 214311.33 | 418.68 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 211226.00 | 226.58 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 180724.33 | 104.48 MB |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 170478.67 | 215.11 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 152268.00 | 203.04 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 151850.67 | 247.36 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 148848.00 | 199.20 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 146203.67 | 256.45 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 143932.00 | 192.31 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 138809.33 | 186.80 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 138787.67 | 243.64 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 136320.00 | 239.39 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 131260.67 | 175.43 MB |
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 130876.00 | 259.77 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 124426.33 | 186.33 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 113121.67 | 290.77 MB |
| go (1.11) | [gf](http://gfer.me) (1.3) | 101457.00 | 154.66 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 100127.33 | 246.71 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 97950.67 | 146.73 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 97133.33 | 145.62 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 86016.33 | 80.82 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 79650.33 | 139.34 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 79215.33 | 188.54 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 75953.00 | 127.70 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 72389.33 | 155.15 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 70304.67 | 147.45 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 64405.00 | 112.81 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 63427.67 | 314.10 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 63347.00 | 133.67 MB |
| php (7.2) | [symfony](http://symfony.com) (4.2) | 61709.00 | 305.63 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 60772.00 | 95.32 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 59525.33 | 308.59 MB |
| c (99) | [kore](http://kore.io) (3.1) | 55512.33 | 150.54 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 50449.33 | 262.49 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 49782.33 | 112.87 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 45307.00 | 110.59 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 41361.67 | 76.71 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 39341.67 | 37.46 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 36954.33 | 34.64 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 35547.67 | 33.34 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 35098.33 | 57.18 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 34738.00 | 42.70 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 33478.33 | 82.52 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 31459.00 | 57.43 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 29456.67 | 16.98 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 28126.00 | 45.84 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 27918.00 | 71.99 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 21432.67 | 38.17 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 18644.00 | 54.06 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 18207.33 | 10.49 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 16998.67 | 128.43 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 16922.00 | 33.74 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 14936.33 | 38.72 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 12395.67 | 33.31 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 2974.00 | 9.07 MB |
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

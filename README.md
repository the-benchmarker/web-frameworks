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
Last update: 2019-05-22
```
OS: Linux (version: 5.0.16-300.fc30.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: iron (rust)


:five: slim (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 0.09 ms | 0.08 ms | 0.13 ms | 0.17 ms | 1.74 ms | 32.00 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 4.48 ms | 0.26 ms | 16.16 ms | 34.81 ms | 86.90 ms | 8145.00 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 5.72 ms | 0.31 ms | 20.87 ms | 42.87 ms | 102.02 ms | 10251.00 | 
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 0.48 ms | 0.46 ms | 0.83 ms | 1.25 ms | 13.30 ms | 289.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 193.53 ms | 0.52 ms | 377.64 ms | 4021.94 ms | 7198.76 ms | 678414.33 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 172.10 ms | 0.52 ms | 392.03 ms | 3480.87 ms | 6856.34 ms | 595447.33 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 193.53 ms | 0.53 ms | 328.07 ms | 4589.63 ms | 7354.20 ms | 742247.33 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 149.91 ms | 0.54 ms | 303.85 ms | 3149.22 ms | 6966.88 ms | 560097.00 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 214.56 ms | 0.56 ms | 406.20 ms | 4707.09 ms | 7601.57 ms | 774027.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 8.49 ms | 0.59 ms | 26.64 ms | 52.06 ms | 109.71 ms | 12625.00 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 9.71 ms | 0.60 ms | 31.36 ms | 61.11 ms | 140.84 ms | 14991.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 10.49 ms | 0.73 ms | 32.16 ms | 62.02 ms | 200.77 ms | 15253.67 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 181.55 ms | 2.26 ms | 378.46 ms | 3864.05 ms | 6901.11 ms | 635085.33 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 3.47 ms | 2.31 ms | 7.84 ms | 15.53 ms | 36.77 ms | 3484.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 136.40 ms | 2.33 ms | 6.00 ms | 3702.27 ms | 6594.79 ms | 654645.33 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.72 ms | 2.87 ms | 7.74 ms | 16.06 ms | 36.97 ms | 3445.33 | 
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 3.99 ms | 3.14 ms | 7.76 ms | 16.37 ms | 95.88 ms | 3546.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 12.14 ms | 3.26 ms | 7.95 ms | 292.76 ms | 1414.94 ms | 75439.00 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.36 ms | 3.99 ms | 6.76 ms | 14.01 ms | 100.70 ms | 2749.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 5.08 ms | 4.18 ms | 10.27 ms | 18.26 ms | 37.40 ms | 3955.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.12 ms | 4.44 ms | 5.83 ms | 10.56 ms | 44.44 ms | 2061.00 | 
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 5.45 ms | 4.78 ms | 9.58 ms | 17.45 ms | 34.76 ms | 3300.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.21 ms | 4.84 ms | 8.26 ms | 15.67 ms | 63.85 ms | 2984.00 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 8.47 ms | 5.01 ms | 14.18 ms | 85.47 ms | 131.45 ms | 13368.33 | 
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 5.50 ms | 5.01 ms | 9.74 ms | 20.36 ms | 221.41 ms | 5131.33 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 40.92 ms | 5.58 ms | 132.01 ms | 331.60 ms | 905.27 ms | 71793.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 6.71 ms | 5.96 ms | 9.81 ms | 17.69 ms | 160.18 ms | 4389.00 | 
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 9.00 ms | 6.27 ms | 18.76 ms | 41.09 ms | 128.65 ms | 8096.33 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 9.66 ms | 6.56 ms | 20.64 ms | 44.12 ms | 223.50 ms | 9443.33 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 9.54 ms | 6.84 ms | 19.54 ms | 41.21 ms | 181.32 ms | 8572.67 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 9.42 ms | 6.95 ms | 18.86 ms | 41.04 ms | 97.37 ms | 7943.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 7.79 ms | 7.09 ms | 12.33 ms | 23.47 ms | 160.38 ms | 5421.67 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 9.07 ms | 7.09 ms | 15.97 ms | 38.00 ms | 239.90 ms | 8467.67 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 10.10 ms | 7.46 ms | 20.47 ms | 43.78 ms | 135.33 ms | 8749.00 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 9.79 ms | 7.56 ms | 17.65 ms | 41.87 ms | 251.49 ms | 10206.67 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 10.47 ms | 7.69 ms | 21.42 ms | 47.46 ms | 116.05 ms | 9164.00 | 
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 9.28 ms | 8.11 ms | 18.28 ms | 35.14 ms | 200.30 ms | 8391.00 | 
| node (12.2) | [restana](http://github.com/jkyberneees/ana) (3.0) | 9.69 ms | 8.40 ms | 14.10 ms | 31.43 ms | 414.44 ms | 14026.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 185.17 ms | 8.63 ms | 31.05 ms | 4542.26 ms | 7919.87 ms | 784462.33 | 
| go (1.12) | [gf](http://goframe.org) (1.6) | 12.73 ms | 9.40 ms | 25.72 ms | 57.68 ms | 227.00 ms | 11836.00 | 
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 11.97 ms | 9.49 ms | 22.99 ms | 43.96 ms | 284.01 ms | 10778.00 | 
| node (12.2) | [polka](http://github.com/lukeed/polka) (0.5) | 12.17 ms | 9.59 ms | 16.45 ms | 41.36 ms | 535.32 ms | 22005.33 | 
| node (12.2) | [rayo](http://rayo.js.org) (1.3) | 11.68 ms | 9.63 ms | 16.87 ms | 33.95 ms | 438.80 ms | 15687.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 14.37 ms | 10.65 ms | 28.02 ms | 51.16 ms | 271.87 ms | 12297.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 10.96 ms | 10.87 ms | 13.59 ms | 16.34 ms | 156.43 ms | 4047.33 | 
| node (12.2) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 19.75 ms | 11.32 ms | 21.69 ms | 304.62 ms | 918.61 ms | 55398.33 | 
| node (12.2) | [fastify](http://fastify.io) (2.3) | 15.25 ms | 11.57 ms | 19.64 ms | 104.62 ms | 649.07 ms | 29897.33 | 
| node (12.2) | [koa](http://koajs.com) (2.7) | 14.41 ms | 11.86 ms | 20.40 ms | 42.34 ms | 516.39 ms | 19750.33 | 
| node (12.2) | [foxify](http://foxify.js.org) (0.10) | 17.07 ms | 12.50 ms | 21.32 ms | 130.05 ms | 773.65 ms | 37686.67 | 
| node (12.2) | [restify](http://restify.com) (8.2) | 15.35 ms | 13.10 ms | 19.59 ms | 51.78 ms | 437.86 ms | 15763.33 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 15.59 ms | 13.94 ms | 25.86 ms | 41.82 ms | 156.70 ms | 8507.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 16.02 ms | 14.06 ms | 28.55 ms | 50.99 ms | 372.94 ms | 13923.00 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 21.04 ms | 14.11 ms | 24.64 ms | 192.52 ms | 1181.37 ms | 55321.33 | 
| python (3.7) | [hug](http://hug.rest) (2.5) | 19.73 ms | 15.79 ms | 35.11 ms | 61.14 ms | 304.02 ms | 13109.33 | 
| node (12.2) | [express](http://expressjs.com) (4.16) | 27.30 ms | 16.99 ms | 29.18 ms | 389.93 ms | 1257.31 ms | 71573.00 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.22) | 25.54 ms | 23.03 ms | 41.40 ms | 65.64 ms | 126.29 ms | 11837.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 24.48 ms | 25.02 ms | 40.56 ms | 56.66 ms | 85.47 ms | 12369.33 | 
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 26.93 ms | 25.04 ms | 38.70 ms | 56.70 ms | 607.90 ms | 16498.00 | 
| node (12.2) | [hapi](http://hapijs.com) (18.1) | 45.56 ms | 27.84 ms | 44.64 ms | 652.15 ms | 1589.81 ms | 108182.00 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 38.00 ms | 27.91 ms | 76.14 ms | 109.22 ms | 342.09 ms | 24177.33 | 
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 43.65 ms | 29.31 ms | 54.90 ms | 505.69 ms | 1779.97 ms | 99995.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 38.98 ms | 32.35 ms | 61.08 ms | 104.02 ms | 579.61 ms | 25338.00 | 
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 36.49 ms | 34.17 ms | 44.24 ms | 66.28 ms | 237.99 ms | 11295.33 | 
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 34.23 ms | 34.68 ms | 41.49 ms | 51.71 ms | 182.31 ms | 7624.00 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 43.17 ms | 39.76 ms | 76.00 ms | 97.81 ms | 161.73 ms | 20778.67 | 
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 40.88 ms | 40.01 ms | 47.09 ms | 145.10 ms | 332.18 ms | 20396.67 | 
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 43.64 ms | 40.11 ms | 50.95 ms | 90.43 ms | 489.61 ms | 20295.33 | 
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 49.35 ms | 40.12 ms | 118.25 ms | 266.56 ms | 680.81 ms | 58947.67 | 
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 40.16 ms | 40.56 ms | 46.54 ms | 56.65 ms | 249.22 ms | 8101.00 | 
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 43.99 ms | 43.32 ms | 51.87 ms | 106.80 ms | 404.06 ms | 18399.00 | 
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 53.01 ms | 51.46 ms | 62.05 ms | 75.35 ms | 596.31 ms | 19448.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 67.30 ms | 55.69 ms | 129.40 ms | 216.08 ms | 409.43 ms | 44558.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 68.90 ms | 62.63 ms | 121.38 ms | 170.88 ms | 237.87 ms | 36438.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 102.43 ms | 68.75 ms | 225.77 ms | 283.30 ms | 630.43 ms | 67568.00 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 90.02 ms | 78.11 ms | 172.09 ms | 241.42 ms | 293.23 ms | 49395.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 95.57 ms | 94.63 ms | 119.05 ms | 149.10 ms | 648.31 ms | 26601.00 | 
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 103.81 ms | 101.63 ms | 145.54 ms | 190.06 ms | 256.12 ms | 32222.00 | 
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 251.99 ms | 256.94 ms | 308.98 ms | 496.27 ms | 1350.03 ms | 81060.00 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 311615.00 | 180.34 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 272575.00 | 326.31 MB |
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 239466.33 | 272.17 MB |
| c (99) | [kore](http://kore.io) (3.1) | 234143.00 | 608.73 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 217074.67 | 210.77 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 214584.33 | 345.17 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 204092.67 | 231.79 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 195248.67 | 392.46 MB |
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 179735.67 | 192.51 MB |
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 177707.67 | 363.51 MB |
| java (8) | [act](http://actframework.org) (1.8) | 165784.67 | 286.20 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 161509.33 | 93.40 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 139250.33 | 226.76 MB |
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 124128.00 | 166.16 MB |
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 122293.67 | 154.12 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 116274.33 | 155.71 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 116170.67 | 154.40 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 115949.00 | 154.16 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 115539.67 | 202.58 MB |
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 113927.00 | 177.59 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 111503.67 | 147.13 MB |
| node (12.2) | [restana](http://github.com/jkyberneees/ana) (3.0) | 109670.00 | 164.40 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 108706.00 | 190.77 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 107417.00 | 144.84 MB |
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 98834.67 | 196.29 MB |
| node (12.2) | [polka](http://github.com/lukeed/polka) (0.5) | 92835.33 | 139.19 MB |
| node (12.2) | [rayo](http://rayo.js.org) (1.3) | 90603.33 | 135.85 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 89378.00 | 84.04 MB |
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 89107.00 | 209.01 MB |
| go (1.12) | [gf](http://goframe.org) (1.6) | 87541.00 | 132.42 MB |
| node (12.2) | [fastify](http://fastify.io) (2.3) | 82830.00 | 216.24 MB |
| node (12.2) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 78192.00 | 117.17 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 75545.67 | 186.12 MB |
| node (12.2) | [koa](http://koajs.com) (2.7) | 74822.00 | 158.42 MB |
| node (12.2) | [foxify](http://foxify.js.org) (0.10) | 72771.00 | 152.96 MB |
| node (12.2) | [restify](http://restify.com) (8.2) | 69269.00 | 121.54 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 68088.00 | 119.31 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 64578.00 | 139.15 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 64538.33 | 138.52 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 60489.67 | 101.21 MB |
| node (12.2) | [express](http://expressjs.com) (4.16) | 52929.67 | 129.61 MB |
| python (3.7) | [hug](http://hug.rest) (2.5) | 51894.33 | 128.73 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 45687.33 | 226.89 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 41958.00 | 208.28 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 40996.67 | 93.02 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 40166.67 | 199.10 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.22) | 39647.67 | 85.66 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 38610.00 | 200.25 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 37357.67 | 69.33 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 36842.33 | 182.76 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 35260.33 | 56.78 MB |
| node (12.2) | [hapi](http://hapijs.com) (18.1) | 34868.67 | 89.99 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 34429.00 | 179.26 MB |
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 29898.33 | 56.36 MB |
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 29487.00 | 38.79 MB |
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 28758.33 | 26.97 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 28538.00 | 27.20 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 28249.67 | 52.57 MB |
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 27265.67 | 44.42 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 25849.00 | 63.72 MB |
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 25039.67 | 23.48 MB |
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 24608.33 | 30.39 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 23350.33 | 45.03 MB |
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 23091.33 | 28.44 MB |
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 22637.00 | 41.31 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 22458.33 | 12.95 MB |
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 18924.00 | 30.84 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 15726.00 | 28.03 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 15132.00 | 8.73 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 14706.67 | 29.30 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 13222.00 | 99.98 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 12156.67 | 31.54 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 11445.33 | 24.89 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 10481.00 | 30.41 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 10104.67 | 29.80 MB |
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 9556.67 | 24.60 MB |
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 3666.33 | 9.01 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3110.67 | 9.57 MB |
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

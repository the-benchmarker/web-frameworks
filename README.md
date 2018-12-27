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
Last update: 2018-12-27
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: slim (php)


:four: laravel (php)


:five: symfony (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 0.11 ms | 0.11 ms | 0.16 ms | 0.26 ms | 13.81 ms | 133.33 | 
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 0.61 ms | 0.56 ms | 0.98 ms | 1.63 ms | 37.88 ms | 469.33 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 211.87 ms | 0.57 ms | 313.64 ms | 5016.43 ms | 7915.14 ms | 811223.67 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 155.09 ms | 0.58 ms | 413.26 ms | 2783.50 ms | 6983.64 ms | 496674.33 | 
| php (7.2) | [symfony](http://symfony.com) (4.2) | 146.82 ms | 0.60 ms | 284.76 ms | 3353.42 ms | 7234.52 ms | 572170.67 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 179.52 ms | 0.62 ms | 338.16 ms | 3793.09 ms | 7561.38 ms | 660138.33 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 6.47 ms | 1.39 ms | 19.11 ms | 54.07 ms | 165.95 ms | 11290.33 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.15) | 5.21 ms | 1.73 ms | 14.66 ms | 40.54 ms | 131.23 ms | 8474.33 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 10.27 ms | 1.85 ms | 31.21 ms | 84.67 ms | 222.87 ms | 17811.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 51.98 ms | 2.42 ms | 5.14 ms | 1705.92 ms | 4486.50 ms | 311122.67 | 
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 211.84 ms | 3.05 ms | 410.40 ms | 4595.03 ms | 7582.38 ms | 752354.00 | 
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 227.07 ms | 3.26 ms | 426.95 ms | 4766.73 ms | 7606.26 ms | 798443.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 3.92 ms | 3.74 ms | 6.17 ms | 10.89 ms | 28.54 ms | 2078.33 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 12.31 ms | 4.20 ms | 35.40 ms | 86.10 ms | 249.19 ms | 18538.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 4.71 ms | 4.61 ms | 8.92 ms | 17.74 ms | 45.21 ms | 3732.00 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 10.09 ms | 4.73 ms | 27.22 ms | 63.22 ms | 171.53 ms | 13648.00 | 
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 4.98 ms | 4.75 ms | 9.23 ms | 18.27 ms | 122.55 ms | 3777.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 5.89 ms | 5.04 ms | 11.88 ms | 19.44 ms | 44.29 ms | 4260.00 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 5.67 ms | 5.35 ms | 9.01 ms | 16.40 ms | 66.64 ms | 3237.33 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 6.14 ms | 5.38 ms | 10.89 ms | 19.69 ms | 41.79 ms | 3763.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 5.73 ms | 5.44 ms | 8.42 ms | 14.27 ms | 222.46 ms | 3963.00 | 
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 6.44 ms | 5.66 ms | 10.44 ms | 17.16 ms | 98.80 ms | 3289.00 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 6.96 ms | 6.13 ms | 10.73 ms | 21.70 ms | 62.59 ms | 3682.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 6.65 ms | 6.24 ms | 10.33 ms | 18.82 ms | 73.28 ms | 3457.00 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 9.28 ms | 7.07 ms | 16.78 ms | 48.97 ms | 139.89 ms | 9554.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 29.62 ms | 7.58 ms | 14.03 ms | 809.81 ms | 1706.65 ms | 124617.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 9.18 ms | 8.76 ms | 12.48 ms | 24.45 ms | 214.99 ms | 5722.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 10.94 ms | 9.29 ms | 17.79 ms | 37.25 ms | 182.88 ms | 8535.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 175.59 ms | 9.36 ms | 40.84 ms | 4036.75 ms | 7129.44 ms | 707780.00 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 10.21 ms | 9.48 ms | 14.29 ms | 33.53 ms | 147.54 ms | 5413.67 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 10.83 ms | 9.63 ms | 16.05 ms | 33.22 ms | 326.88 ms | 9608.67 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 10.75 ms | 9.66 ms | 16.00 ms | 34.26 ms | 212.27 ms | 7895.00 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 11.59 ms | 10.12 ms | 17.45 ms | 35.18 ms | 270.28 ms | 9089.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 11.37 ms | 10.15 ms | 17.45 ms | 32.99 ms | 125.55 ms | 6009.33 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 11.68 ms | 10.23 ms | 18.20 ms | 37.55 ms | 156.65 ms | 6788.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 12.06 ms | 10.53 ms | 18.59 ms | 37.27 ms | 237.68 ms | 7452.67 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 12.32 ms | 10.78 ms | 18.56 ms | 38.97 ms | 203.48 ms | 8033.67 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 16.64 ms | 11.29 ms | 37.71 ms | 66.89 ms | 217.61 ms | 14073.67 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 47.44 ms | 13.45 ms | 147.10 ms | 353.59 ms | 883.47 ms | 76916.00 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 19.26 ms | 13.46 ms | 24.82 ms | 151.20 ms | 845.17 ms | 41268.33 | 
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 22.94 ms | 14.20 ms | 35.67 ms | 180.00 ms | 941.49 ms | 46388.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 18.34 ms | 14.64 ms | 32.46 ms | 59.52 ms | 296.17 ms | 12263.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 15.24 ms | 15.20 ms | 18.00 ms | 21.95 ms | 54.75 ms | 2442.00 | 
| go (1.11) | [gf](http://gfer.me) (1.3) | 16.57 ms | 15.36 ms | 24.24 ms | 47.88 ms | 133.28 ms | 8125.33 | 
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 21.19 ms | 16.01 ms | 27.25 ms | 161.62 ms | 821.66 ms | 39132.67 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 23.12 ms | 16.03 ms | 33.48 ms | 163.04 ms | 901.03 ms | 41450.33 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 35.84 ms | 17.03 ms | 30.34 ms | 649.18 ms | 2102.84 ms | 122935.33 | 
| node (11.1) | [fastify](http://fastify.io) (1.13) | 28.28 ms | 17.07 ms | 33.81 ms | 341.72 ms | 1137.29 ms | 66331.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 21.84 ms | 20.26 ms | 39.58 ms | 65.34 ms | 307.46 ms | 15216.00 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 45.28 ms | 25.98 ms | 48.51 ms | 704.53 ms | 1656.61 ms | 114765.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 32.69 ms | 30.25 ms | 38.92 ms | 66.50 ms | 546.58 ms | 22092.33 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 53.92 ms | 30.84 ms | 60.66 ms | 839.66 ms | 1962.24 ms | 135865.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 35.27 ms | 31.44 ms | 68.40 ms | 96.29 ms | 151.77 ms | 22359.67 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 33.55 ms | 32.25 ms | 44.87 ms | 60.74 ms | 255.45 ms | 9536.33 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 45.63 ms | 35.54 ms | 66.63 ms | 300.18 ms | 1137.12 ms | 61279.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 37.96 ms | 36.75 ms | 49.11 ms | 58.36 ms | 331.83 ms | 10165.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 47.33 ms | 40.34 ms | 77.99 ms | 108.99 ms | 299.76 ms | 20702.00 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 42.88 ms | 43.75 ms | 53.65 ms | 64.32 ms | 286.74 ms | 12422.33 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 44.46 ms | 43.88 ms | 52.53 ms | 71.56 ms | 200.35 ms | 8691.00 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 47.18 ms | 47.46 ms | 58.00 ms | 66.56 ms | 242.35 ms | 9721.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 53.49 ms | 50.29 ms | 66.54 ms | 113.60 ms | 592.15 ms | 27716.33 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 102.70 ms | 50.81 ms | 95.21 ms | 1510.63 ms | 2711.00 ms | 245123.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 69.98 ms | 62.69 ms | 120.93 ms | 192.17 ms | 377.41 ms | 39432.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 77.94 ms | 69.72 ms | 113.62 ms | 192.26 ms | 894.89 ms | 42133.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 93.93 ms | 95.42 ms | 143.74 ms | 190.78 ms | 291.23 ms | 39692.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 110.78 ms | 109.95 ms | 139.18 ms | 174.85 ms | 422.09 ms | 28058.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (vibora) (python)


:five: (onyx) (crystal)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 242318.33 | 140.04 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 205357.00 | 245.90 MB |
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 185394.67 | 210.83 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 175456.33 | 199.17 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 165227.00 | 155.35 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 159100.67 | 319.38 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 159012.00 | 154.21 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 158391.00 | 169.05 MB |
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 142271.33 | 291.28 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 135709.33 | 218.42 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 119709.00 | 69.25 MB |
| java (8) | [act](http://actframework.org) (1.8) | 109431.67 | 213.74 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 102315.00 | 166.63 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 98176.00 | 172.44 MB |
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 97009.00 | 122.61 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 92918.67 | 122.93 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 92582.67 | 123.40 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 86318.33 | 151.59 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 86101.67 | 115.77 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 85742.00 | 150.62 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 81887.00 | 109.22 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 81172.33 | 108.46 MB |
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 74573.67 | 148.41 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 67351.33 | 173.15 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 64715.00 | 97.02 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 64650.33 | 60.81 MB |
| go (1.11) | [gf](http://gfer.me) (1.3) | 61092.67 | 92.33 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 56855.67 | 85.04 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 55860.33 | 137.67 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 55783.33 | 117.34 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 52761.67 | 79.01 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 51451.00 | 86.01 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 51396.00 | 124.70 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 50030.33 | 87.62 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 49520.00 | 106.31 MB |
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 45983.33 | 228.79 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 43260.67 | 214.58 MB |
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 41581.67 | 206.88 MB |
| c (99) | [kore](http://kore.io) (3.1) | 40767.67 | 110.50 MB |
| php (7.2) | [symfony](http://symfony.com) (4.2) | 40281.67 | 199.87 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 35781.67 | 186.03 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 34443.33 | 179.50 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 34252.33 | 72.43 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 30997.33 | 29.07 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 29558.00 | 67.03 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 29293.67 | 54.42 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 28537.33 | 69.85 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 26675.67 | 43.75 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 26602.67 | 24.94 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 25151.00 | 44.06 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.15) | 24650.33 | 23.50 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 23045.33 | 37.57 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 22481.33 | 27.59 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 20966.67 | 51.69 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 20847.67 | 38.09 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 19907.67 | 11.49 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 19341.67 | 31.52 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 17474.33 | 45.14 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 14312.33 | 25.83 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 12818.33 | 37.23 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 12677.33 | 7.32 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 12480.33 | 94.52 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 10555.00 | 21.08 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 10358.33 | 26.88 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 8804.33 | 23.54 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 2689.67 | 8.25 MB |
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

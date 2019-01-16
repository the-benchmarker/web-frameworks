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
Last update: 2019-01-16
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


:four: iron (rust)


:five: zend-expressive (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.11) | 0.07 ms | 0.07 ms | 0.10 ms | 0.13 ms | 0.77 ms | 25.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.15) | 2.85 ms | 0.15 ms | 10.77 ms | 28.18 ms | 71.88 ms | 6131.00 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.56 ms | 0.18 ms | 13.76 ms | 32.41 ms | 80.93 ms | 7333.00 | 
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 0.26 ms | 0.24 ms | 0.43 ms | 0.76 ms | 20.18 ms | 209.67 | 
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 121.86 ms | 0.27 ms | 192.19 ms | 2851.30 ms | 6820.95 ms | 489321.33 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 109.86 ms | 0.28 ms | 173.95 ms | 2837.37 ms | 6761.73 ms | 464930.33 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.49 ms | 0.29 ms | 19.67 ms | 43.61 ms | 105.28 ms | 10020.33 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 6.34 ms | 0.31 ms | 23.46 ms | 50.01 ms | 115.92 ms | 11745.00 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 81.09 ms | 0.34 ms | 234.82 ms | 1162.25 ms | 6816.66 ms | 289932.67 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 7.32 ms | 0.42 ms | 24.70 ms | 50.97 ms | 113.33 ms | 12129.00 | 
| php (7.2) | [symfony](http://symfony.com) (4.2) | 109.77 ms | 1.03 ms | 201.01 ms | 2356.53 ms | 6730.48 ms | 453711.00 | 
| php (7.2) | [slim](http://slimframework.com) (3.12) | 77.54 ms | 1.31 ms | 145.95 ms | 1749.26 ms | 5514.28 ms | 321819.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 62.33 ms | 1.37 ms | 3.72 ms | 1999.55 ms | 6588.06 ms | 409109.33 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 28.61 ms | 1.81 ms | 98.50 ms | 268.07 ms | 745.70 ms | 57411.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.84 ms | 1.91 ms | 6.18 ms | 14.63 ms | 36.15 ms | 3042.33 | 
| php (7.2) | [zend-framework](http://framework.zend.com) (3.1) | 72.34 ms | 1.96 ms | 167.93 ms | 1567.50 ms | 4313.62 ms | 267233.33 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 2.37 ms | 2.09 ms | 4.46 ms | 7.75 ms | 22.13 ms | 1737.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 2.82 ms | 2.12 ms | 5.43 ms | 8.81 ms | 92.91 ms | 2178.67 | 
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 2.70 ms | 2.16 ms | 5.59 ms | 12.11 ms | 76.12 ms | 2519.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.60 ms | 2.60 ms | 4.53 ms | 5.91 ms | 34.52 ms | 1448.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.14 ms | 2.65 ms | 6.42 ms | 12.15 ms | 32.76 ms | 2658.33 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 3.35 ms | 2.92 ms | 5.93 ms | 11.86 ms | 26.02 ms | 2368.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 3.57 ms | 2.94 ms | 6.88 ms | 13.01 ms | 30.92 ms | 2681.67 | 
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 3.22 ms | 2.99 ms | 4.81 ms | 8.70 ms | 153.86 ms | 3534.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.43 ms | 3.06 ms | 5.61 ms | 10.19 ms | 29.77 ms | 1931.33 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.5) | 5.05 ms | 3.23 ms | 7.51 ms | 64.39 ms | 114.41 ms | 9806.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 4.21 ms | 4.20 ms | 7.13 ms | 8.30 ms | 18.25 ms | 2166.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 4.78 ms | 4.45 ms | 6.97 ms | 14.00 ms | 182.93 ms | 2652.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 5.85 ms | 4.47 ms | 8.65 ms | 20.63 ms | 307.90 ms | 13522.33 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 5.13 ms | 4.52 ms | 8.67 ms | 17.46 ms | 103.91 ms | 3421.00 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 5.36 ms | 4.58 ms | 9.10 ms | 18.30 ms | 101.09 ms | 3909.33 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 5.49 ms | 4.63 ms | 9.43 ms | 18.28 ms | 104.44 ms | 4335.33 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 5.62 ms | 4.64 ms | 8.00 ms | 26.60 ms | 82.36 ms | 4354.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 6.07 ms | 4.67 ms | 9.67 ms | 21.56 ms | 304.80 ms | 10604.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.51 ms | 4.74 ms | 8.92 ms | 19.02 ms | 132.64 ms | 4628.33 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 5.86 ms | 4.87 ms | 9.89 ms | 20.09 ms | 105.22 ms | 3820.67 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 6.00 ms | 4.94 ms | 10.02 ms | 19.78 ms | 111.44 ms | 4389.67 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 8.91 ms | 4.95 ms | 11.43 ms | 127.27 ms | 400.88 ms | 21952.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 8.65 ms | 5.56 ms | 19.16 ms | 36.15 ms | 206.55 ms | 7953.33 | 
| node (11.6) | [restana](http://github.com/jkyberneees/ana) (2.7) | 8.25 ms | 5.92 ms | 15.94 ms | 33.93 ms | 260.71 ms | 8560.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 215.09 ms | 6.62 ms | 68.41 ms | 4951.10 ms | 7932.38 ms | 860352.33 | 
| go (1.11) | [gf](http://gfer.me) (1.3) | 7.72 ms | 6.99 ms | 11.64 ms | 24.18 ms | 165.39 ms | 4505.33 | 
| node (11.6) | [polka](http://github.com/lukeed/polka) (0.5) | 11.32 ms | 8.23 ms | 20.44 ms | 44.19 ms | 409.29 ms | 15019.67 | 
| node (11.6) | [rayo](http://rayo.js.org) (1.2) | 11.31 ms | 8.37 ms | 20.42 ms | 42.81 ms | 411.10 ms | 13928.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 17.76 ms | 9.16 ms | 20.02 ms | 296.69 ms | 1260.82 ms | 70705.33 | 
| node (11.6) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 13.24 ms | 9.52 ms | 20.69 ms | 60.64 ms | 551.66 ms | 23451.33 | 
| node (11.6) | [foxify](http://foxify.js.org) (0.10) | 13.36 ms | 9.59 ms | 22.04 ms | 52.96 ms | 537.37 ms | 20437.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 10.22 ms | 10.28 ms | 11.96 ms | 13.81 ms | 21.71 ms | 1474.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 14.12 ms | 10.34 ms | 18.62 ms | 67.64 ms | 859.81 ms | 34397.33 | 
| node (11.6) | [fastify](http://fastify.io) (1.13) | 14.00 ms | 10.77 ms | 22.87 ms | 47.63 ms | 483.47 ms | 17119.00 | 
| node (11.6) | [koa](http://koajs.com) (2.6) | 13.89 ms | 10.95 ms | 22.63 ms | 45.91 ms | 463.18 ms | 16241.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 15.95 ms | 12.45 ms | 32.02 ms | 42.42 ms | 67.09 ms | 9304.00 | 
| node (11.6) | [express](http://expressjs.com) (4.16) | 16.21 ms | 12.63 ms | 25.94 ms | 56.63 ms | 533.13 ms | 19793.00 | 
| node (11.6) | [restify](http://restify.com) (7.5) | 17.76 ms | 14.90 ms | 30.35 ms | 53.84 ms | 295.91 ms | 12033.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 24.30 ms | 17.76 ms | 50.89 ms | 63.63 ms | 378.45 ms | 16882.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 22.29 ms | 19.33 ms | 31.51 ms | 48.38 ms | 599.87 ms | 21466.00 | 
| node (11.6) | [hapi](http://hapijs.com) (17.8) | 29.61 ms | 21.93 ms | 39.12 ms | 239.42 ms | 1069.73 ms | 54058.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 23.83 ms | 23.54 ms | 32.39 ms | 36.52 ms | 146.01 ms | 6321.00 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 27.79 ms | 24.51 ms | 39.55 ms | 44.37 ms | 438.57 ms | 14722.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 26.18 ms | 27.48 ms | 34.14 ms | 38.07 ms | 155.25 ms | 6329.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 32.63 ms | 27.81 ms | 56.32 ms | 106.33 ms | 309.53 ms | 20312.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 27.55 ms | 29.76 ms | 36.49 ms | 41.57 ms | 299.07 ms | 9287.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 42.12 ms | 31.15 ms | 79.40 ms | 101.85 ms | 350.06 ms | 24984.00 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 32.04 ms | 32.68 ms | 39.28 ms | 43.86 ms | 254.34 ms | 11068.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 33.87 ms | 35.79 ms | 44.22 ms | 48.55 ms | 251.77 ms | 10987.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 39.24 ms | 38.02 ms | 68.89 ms | 99.97 ms | 134.21 ms | 20850.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 63.74 ms | 63.20 ms | 90.33 ms | 103.06 ms | 276.43 ms | 18757.33 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 416753.33 | 241.06 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 396780.67 | 475.03 MB |
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 366624.67 | 416.64 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 337117.33 | 326.80 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 334763.00 | 380.30 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 328653.00 | 530.06 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 299477.67 | 601.82 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 287258.33 | 270.26 MB |
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 283059.33 | 579.07 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 280568.33 | 299.69 MB |
| java (8) | [act](http://actframework.org) (1.8) | 272840.33 | 531.92 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.5) | 270364.00 | 156.40 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 198756.33 | 323.77 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 194509.67 | 260.81 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 191260.00 | 255.93 MB |
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 187423.33 | 236.54 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 186065.33 | 326.40 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 185023.00 | 247.95 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 176755.67 | 310.30 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 175418.00 | 307.98 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 169678.00 | 228.08 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 165711.00 | 220.88 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 151201.00 | 388.58 MB |
| node (11.6) | [restana](http://github.com/jkyberneees/ana) (2.7) | 132532.33 | 198.48 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 131229.67 | 323.39 MB |
| go (1.11) | [gf](http://gfer.me) (1.3) | 129064.00 | 195.75 MB |
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.11) | 118084.67 | 236.22 MB |
| node (11.6) | [polka](http://github.com/lukeed/polka) (0.5) | 100980.67 | 151.29 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 100884.67 | 176.53 MB |
| node (11.6) | [rayo](http://rayo.js.org) (1.2) | 99883.33 | 149.54 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 97544.00 | 91.65 MB |
| node (11.6) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 94807.33 | 141.78 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 93177.33 | 199.84 MB |
| php (7.2) | [slim](http://slimframework.com) (3.12) | 92623.67 | 459.78 MB |
| php (7.2) | [zend-framework](http://framework.zend.com) (3.1) | 91260.33 | 453.52 MB |
| php (7.2) | [symfony](http://symfony.com) (4.2) | 88408.67 | 438.81 MB |
| node (11.6) | [foxify](http://foxify.js.org) (0.10) | 87704.67 | 184.03 MB |
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 83070.33 | 411.80 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 82973.67 | 139.57 MB |
| node (11.6) | [fastify](http://fastify.io) (1.13) | 80747.00 | 199.50 MB |
| node (11.6) | [koa](http://koajs.com) (2.6) | 79072.00 | 167.03 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 76999.67 | 399.89 MB |
| c (99) | [kore](http://kore.io) (3.1) | 68755.00 | 186.50 MB |
| node (11.6) | [express](http://expressjs.com) (4.16) | 68477.33 | 167.19 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 65336.00 | 148.10 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 63403.00 | 330.21 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 58679.67 | 94.40 MB |
| node (11.6) | [restify](http://restify.com) (7.5) | 58081.67 | 101.58 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 45981.67 | 85.29 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.15) | 45108.00 | 42.96 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 42871.33 | 105.49 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 41790.67 | 39.22 MB |
| node (11.6) | [hapi](http://hapijs.com) (17.8) | 40965.00 | 105.89 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 38247.00 | 35.86 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 36371.00 | 59.27 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 36157.67 | 20.87 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 36069.33 | 44.34 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 31847.67 | 56.71 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 31634.00 | 57.79 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 29480.33 | 48.05 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 25913.67 | 51.76 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 24921.00 | 72.23 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 23339.67 | 13.47 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 20208.00 | 152.70 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 17496.33 | 45.36 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 15332.67 | 45.09 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 4494.67 | 13.74 MB |
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

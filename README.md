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
Last update: 2018-11-30
```
OS: Linux (version: 4.19.4-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: lumen (php)


:four: symfony (php)


:five: laravel (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 0.07 ms | 0.08 ms | 0.10 ms | 0.13 ms | 9.25 ms | 101.33 | 
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 0.28 ms | 0.26 ms | 0.48 ms | 0.78 ms | 19.14 ms | 219.33 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 177.55 ms | 0.35 ms | 298.90 ms | 3878.31 ms | 6843.36 ms | 646582.33 | 
| php (7.2) | [symfony](http://symfony.com) (4.1) | 148.02 ms | 0.36 ms | 268.23 ms | 3270.78 ms | 6915.20 ms | 553011.00 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 154.26 ms | 0.38 ms | 347.20 ms | 3011.53 ms | 6886.52 ms | 539210.67 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.38 ms | 0.72 ms | 12.98 ms | 39.87 ms | 134.21 ms | 8220.00 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 126.68 ms | 0.76 ms | 241.02 ms | 2946.48 ms | 6768.19 ms | 487392.67 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 3.58 ms | 1.01 ms | 10.08 ms | 30.34 ms | 104.34 ms | 6255.33 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 7.73 ms | 1.05 ms | 23.80 ms | 71.35 ms | 215.12 ms | 14740.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 66.90 ms | 1.26 ms | 2.83 ms | 2344.67 ms | 5503.79 ms | 411046.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 2.46 ms | 1.96 ms | 3.89 ms | 11.04 ms | 161.74 ms | 4681.00 | 
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 2.76 ms | 2.04 ms | 5.57 ms | 12.18 ms | 95.41 ms | 3901.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.96 ms | 2.34 ms | 6.03 ms | 11.15 ms | 26.89 ms | 2523.33 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 6.75 ms | 2.52 ms | 18.98 ms | 48.41 ms | 140.81 ms | 10266.33 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 8.90 ms | 2.66 ms | 25.97 ms | 66.64 ms | 183.58 ms | 14190.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.74 ms | 2.69 ms | 4.88 ms | 6.95 ms | 20.55 ms | 1595.67 | 
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 3.10 ms | 3.02 ms | 4.33 ms | 7.42 ms | 79.26 ms | 1386.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.96 ms | 3.20 ms | 8.09 ms | 15.10 ms | 33.84 ms | 3281.00 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.77 ms | 3.29 ms | 5.94 ms | 11.82 ms | 92.04 ms | 2450.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 4.27 ms | 3.74 ms | 8.02 ms | 15.14 ms | 31.05 ms | 3056.67 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 6.01 ms | 4.21 ms | 10.60 ms | 49.65 ms | 122.66 ms | 8761.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 4.34 ms | 4.30 ms | 7.27 ms | 8.91 ms | 17.51 ms | 2188.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.74 ms | 4.34 ms | 7.45 ms | 13.20 ms | 44.39 ms | 2651.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 5.07 ms | 4.62 ms | 7.59 ms | 13.97 ms | 211.22 ms | 3520.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.56 ms | 4.65 ms | 9.33 ms | 21.14 ms | 105.41 ms | 4733.33 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 6.58 ms | 5.43 ms | 10.96 ms | 21.44 ms | 210.06 ms | 4980.00 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 6.85 ms | 5.55 ms | 11.21 ms | 23.38 ms | 219.75 ms | 5664.00 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 6.87 ms | 5.62 ms | 11.62 ms | 22.91 ms | 163.49 ms | 4744.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 6.86 ms | 5.69 ms | 11.29 ms | 22.25 ms | 137.47 ms | 4217.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 7.61 ms | 5.91 ms | 12.30 ms | 26.01 ms | 410.80 ms | 9453.33 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 7.36 ms | 6.07 ms | 12.21 ms | 23.95 ms | 119.03 ms | 4814.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 7.45 ms | 6.22 ms | 12.43 ms | 23.75 ms | 148.32 ms | 4499.67 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 7.57 ms | 6.33 ms | 10.74 ms | 30.92 ms | 104.83 ms | 5190.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 8.64 ms | 6.71 ms | 15.29 ms | 29.04 ms | 228.79 ms | 8250.00 | 
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 9.00 ms | 6.93 ms | 15.80 ms | 37.73 ms | 322.68 ms | 10798.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 158.70 ms | 7.12 ms | 42.85 ms | 3640.31 ms | 6806.19 ms | 643067.33 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 11.89 ms | 8.17 ms | 20.91 ms | 55.76 ms | 502.15 ms | 19807.67 | 
| go (1.11) | [gf](http://gfer.me) (1.2) | 9.26 ms | 8.50 ms | 13.28 ms | 28.25 ms | 274.68 ms | 6465.67 | 
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 11.66 ms | 8.59 ms | 19.68 ms | 49.32 ms | 429.83 ms | 15479.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 9.69 ms | 8.71 ms | 15.95 ms | 28.87 ms | 172.60 ms | 7691.00 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 38.17 ms | 8.89 ms | 123.50 ms | 314.96 ms | 815.06 ms | 67888.00 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 14.77 ms | 10.15 ms | 25.01 ms | 65.27 ms | 608.41 ms | 24663.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 11.50 ms | 10.18 ms | 21.09 ms | 40.12 ms | 220.50 ms | 8755.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 10.86 ms | 11.01 ms | 12.56 ms | 14.24 ms | 89.48 ms | 2692.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 14.46 ms | 11.06 ms | 19.03 ms | 54.77 ms | 849.42 ms | 32257.00 | 
| node (11.1) | [fastify](http://fastify.io) (1.13) | 15.05 ms | 11.27 ms | 23.86 ms | 56.71 ms | 581.21 ms | 22766.00 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 17.91 ms | 13.27 ms | 27.44 ms | 91.29 ms | 674.96 ms | 28710.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 19.29 ms | 14.96 ms | 37.36 ms | 58.71 ms | 108.64 ms | 12478.33 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 22.31 ms | 16.26 ms | 34.12 ms | 129.35 ms | 801.73 ms | 36268.67 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 22.16 ms | 17.55 ms | 36.40 ms | 80.18 ms | 503.43 ms | 23501.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 23.00 ms | 20.02 ms | 31.32 ms | 42.65 ms | 380.01 ms | 11383.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 23.88 ms | 21.18 ms | 34.86 ms | 57.47 ms | 250.83 ms | 10319.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 22.84 ms | 21.21 ms | 31.21 ms | 45.80 ms | 227.12 ms | 6899.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 31.55 ms | 22.91 ms | 63.13 ms | 98.59 ms | 337.66 ms | 23946.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 29.31 ms | 24.94 ms | 42.24 ms | 54.39 ms | 706.41 ms | 24969.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 27.57 ms | 28.18 ms | 35.87 ms | 70.56 ms | 200.05 ms | 11660.33 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 48.50 ms | 31.03 ms | 58.05 ms | 629.52 ms | 1573.19 ms | 104587.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 36.35 ms | 32.05 ms | 47.93 ms | 105.47 ms | 274.95 ms | 16159.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 32.71 ms | 32.43 ms | 39.90 ms | 49.89 ms | 331.97 ms | 11514.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 44.71 ms | 38.59 ms | 82.49 ms | 134.86 ms | 236.07 ms | 27972.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 53.45 ms | 45.76 ms | 86.75 ms | 136.44 ms | 700.78 ms | 30357.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 56.40 ms | 49.74 ms | 97.30 ms | 162.98 ms | 227.63 ms | 30910.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 86.49 ms | 84.65 ms | 109.73 ms | 132.96 ms | 731.28 ms | 30351.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (actix-web) (rust)


:three: (japronto) (python)


:four: (evhtp) (cpp)


:five: (gotham) (rust)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 423100.33 | 244.68 MB |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 364440.00 | 413.61 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 325200.33 | 389.12 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 322592.67 | 312.94 MB |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 275334.00 | 563.61 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 268591.00 | 304.87 MB |
| java (8) | [act](http://actframework.org) (1.8) | 263348.00 | 514.05 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 247009.33 | 399.96 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 232721.00 | 249.38 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 223560.67 | 449.48 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 201698.00 | 116.74 MB |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 196624.00 | 248.21 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 185350.00 | 301.75 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 147973.00 | 198.42 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 144741.00 | 194.69 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 142262.67 | 249.59 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 141692.00 | 189.10 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 135581.33 | 238.17 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 134837.67 | 236.52 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 134311.00 | 180.33 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 131752.00 | 175.14 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 123292.00 | 184.69 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 118545.67 | 304.81 MB |
| go (1.11) | [gf](http://gfer.me) (1.2) | 108755.67 | 165.78 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 104527.00 | 257.57 MB |
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 104102.67 | 206.52 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 101248.33 | 151.45 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 96293.67 | 202.48 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 94716.67 | 166.00 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 91224.33 | 85.68 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 84042.67 | 180.39 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 82284.00 | 123.19 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 81613.00 | 192.82 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 79826.33 | 134.52 MB |
| c (99) | [kore](http://kore.io) (3.1) | 74755.00 | 202.68 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 70953.00 | 112.80 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 66285.67 | 328.66 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 65346.67 | 137.89 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 60099.67 | 311.68 MB |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 59568.00 | 295.22 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 54412.67 | 123.24 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 53130.67 | 129.64 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 50195.33 | 261.52 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 49572.00 | 86.83 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 43597.33 | 80.83 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 43359.00 | 40.64 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 41760.00 | 68.04 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 36648.00 | 34.36 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 36111.00 | 34.41 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 34989.00 | 63.91 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 34213.67 | 84.31 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 30866.00 | 37.79 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 29592.33 | 17.05 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 28962.00 | 74.85 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 27665.67 | 45.10 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 23224.67 | 41.38 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 18998.33 | 10.95 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 18966.00 | 54.96 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 18149.67 | 36.18 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 16709.67 | 126.33 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 14402.67 | 37.34 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 11322.33 | 30.24 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 3366.33 | 10.28 MB |
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

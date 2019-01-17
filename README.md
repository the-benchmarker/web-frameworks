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
Last update: 2019-01-17
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


:five: lumen (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.11) | 0.08 ms | 0.08 ms | 0.12 ms | 0.15 ms | 14.31 ms | 116.00 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.15) | 4.40 ms | 0.22 ms | 17.59 ms | 46.41 ms | 117.97 ms | 10108.00 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 5.93 ms | 0.28 ms | 23.67 ms | 54.86 ms | 150.50 ms | 12514.00 | 
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 0.44 ms | 0.45 ms | 0.69 ms | 1.02 ms | 37.52 ms | 394.33 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 200.04 ms | 0.45 ms | 354.41 ms | 4156.21 ms | 7370.69 ms | 709213.00 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 8.17 ms | 0.45 ms | 28.48 ms | 61.96 ms | 140.62 ms | 14342.67 | 
| php (7.2) | [slim](http://slimframework.com) (3.12) | 198.43 ms | 0.46 ms | 340.11 ms | 4206.13 ms | 7197.67 ms | 711653.33 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 9.84 ms | 0.51 ms | 35.78 ms | 76.80 ms | 186.87 ms | 17954.67 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 128.11 ms | 0.55 ms | 360.62 ms | 2080.31 ms | 6934.55 ms | 406218.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 11.28 ms | 0.63 ms | 40.46 ms | 84.40 ms | 197.47 ms | 19988.33 | 
| php (7.2) | [symfony](http://symfony.com) (4.2) | 181.16 ms | 1.41 ms | 352.23 ms | 3678.24 ms | 5889.54 ms | 626011.67 | 
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 163.36 ms | 1.48 ms | 294.48 ms | 3460.79 ms | 6197.54 ms | 588051.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 100.23 ms | 1.87 ms | 5.15 ms | 2917.64 ms | 4952.47 ms | 498181.33 | 
| php (7.2) | [zend-framework](http://framework.zend.com) (3.1) | 142.59 ms | 2.15 ms | 333.36 ms | 2737.48 ms | 6212.62 ms | 485156.33 | 
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 3.98 ms | 3.20 ms | 8.11 ms | 17.19 ms | 88.39 ms | 3684.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 3.61 ms | 3.32 ms | 6.70 ms | 11.63 ms | 36.60 ms | 2577.00 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 48.10 ms | 3.47 ms | 161.53 ms | 445.41 ms | 1287.00 ms | 94942.33 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.93 ms | 3.58 ms | 7.49 ms | 15.03 ms | 41.55 ms | 3277.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 5.15 ms | 4.27 ms | 10.07 ms | 17.52 ms | 42.33 ms | 3784.33 | 
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 4.63 ms | 4.39 ms | 6.70 ms | 14.21 ms | 99.95 ms | 2928.00 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.69 ms | 4.50 ms | 7.18 ms | 14.92 ms | 58.17 ms | 2716.33 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 4.80 ms | 4.60 ms | 8.04 ms | 15.76 ms | 40.32 ms | 2964.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 5.18 ms | 4.63 ms | 9.51 ms | 18.04 ms | 41.02 ms | 3537.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.44 ms | 4.91 ms | 5.85 ms | 10.11 ms | 42.69 ms | 1944.00 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.5) | 9.27 ms | 5.30 ms | 18.14 ms | 83.03 ms | 144.88 ms | 13550.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.77 ms | 5.67 ms | 8.98 ms | 14.79 ms | 52.81 ms | 2829.67 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 7.74 ms | 6.31 ms | 12.49 ms | 24.52 ms | 234.81 ms | 6554.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 6.63 ms | 6.42 ms | 11.03 ms | 15.84 ms | 192.39 ms | 3729.67 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 7.98 ms | 6.45 ms | 12.68 ms | 26.47 ms | 190.49 ms | 7367.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 7.18 ms | 6.56 ms | 11.85 ms | 22.87 ms | 86.05 ms | 4633.67 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 8.64 ms | 6.74 ms | 13.52 ms | 28.86 ms | 366.09 ms | 11785.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 7.73 ms | 6.97 ms | 10.68 ms | 21.33 ms | 219.83 ms | 5729.33 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 8.71 ms | 7.39 ms | 14.34 ms | 27.74 ms | 91.20 ms | 4981.67 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 9.04 ms | 7.51 ms | 14.49 ms | 29.09 ms | 242.95 ms | 8754.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 9.70 ms | 7.84 ms | 15.81 ms | 33.13 ms | 309.74 ms | 10340.00 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 9.47 ms | 8.00 ms | 15.63 ms | 31.16 ms | 289.10 ms | 7445.00 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 8.94 ms | 8.38 ms | 12.29 ms | 33.98 ms | 123.76 ms | 5570.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 202.38 ms | 9.14 ms | 55.13 ms | 4421.14 ms | 7889.06 ms | 792987.33 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 10.95 ms | 9.15 ms | 20.53 ms | 41.17 ms | 218.08 ms | 9511.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 12.57 ms | 10.39 ms | 22.50 ms | 39.53 ms | 243.96 ms | 10350.67 | 
| go (1.11) | [gf](http://gfer.me) (1.4) | 10.47 ms | 9.42 ms | 15.13 ms | 34.34 ms | 361.14 ms | 8879.33 |
| node (11.6) | [polka](http://github.com/lukeed/polka) (0.5) | 17.40 ms | 11.48 ms | 31.67 ms | 89.67 ms | 613.46 ms | 25871.33 | 
| node (11.6) | [foxify](http://foxify.js.org) (0.10) | 16.51 ms | 11.75 ms | 27.29 ms | 81.33 ms | 569.42 ms | 23759.00 | 
| node (11.6) | [rayo](http://rayo.js.org) (1.2) | 20.51 ms | 13.02 ms | 35.23 ms | 136.68 ms | 853.66 ms | 38808.33 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 19.98 ms | 13.52 ms | 24.46 ms | 187.42 ms | 1270.36 ms | 55815.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 13.78 ms | 13.72 ms | 16.29 ms | 21.89 ms | 156.00 ms | 3220.00 | 
| node (11.6) | [restana](http://github.com/jkyberneees/ana) (2.7) | 19.26 ms | 13.73 ms | 33.69 ms | 83.57 ms | 591.31 ms | 26060.33 | 
| node (11.6) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 22.61 ms | 14.93 ms | 34.49 ms | 186.29 ms | 926.35 ms | 45209.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 16.66 ms | 15.33 ms | 29.99 ms | 49.87 ms | 230.38 ms | 11123.67 | 
| node (11.6) | [koa](http://koajs.com) (2.6) | 25.91 ms | 17.24 ms | 37.49 ms | 232.13 ms | 996.94 ms | 52379.00 | 
| node (11.6) | [fastify](http://fastify.io) (1.13) | 31.27 ms | 18.48 ms | 41.54 ms | 413.92 ms | 1295.92 ms | 76998.33 | 
| node (11.6) | [express](http://expressjs.com) (4.16) | 31.23 ms | 20.47 ms | 42.40 ms | 332.54 ms | 1197.94 ms | 65889.00 | 
| node (11.6) | [restify](http://restify.com) (7.5) | 32.41 ms | 24.33 ms | 53.41 ms | 177.36 ms | 837.20 ms | 42002.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 29.57 ms | 24.35 ms | 35.48 ms | 167.91 ms | 1047.51 ms | 46792.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 26.13 ms | 26.50 ms | 41.87 ms | 51.25 ms | 91.44 ms | 12297.67 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 31.39 ms | 27.50 ms | 43.48 ms | 79.99 ms | 481.99 ms | 23900.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 31.44 ms | 29.08 ms | 42.28 ms | 55.14 ms | 465.46 ms | 19244.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 42.88 ms | 29.95 ms | 90.37 ms | 138.93 ms | 286.80 ms | 29449.00 | 
| node (11.6) | [hapi](http://hapijs.com) (17.8) | 53.09 ms | 33.00 ms | 59.84 ms | 708.51 ms | 1692.25 ms | 115017.33 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 33.93 ms | 34.75 ms | 45.94 ms | 53.48 ms | 205.96 ms | 10817.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 38.36 ms | 35.13 ms | 51.55 ms | 89.12 ms | 491.67 ms | 22074.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 36.77 ms | 35.61 ms | 50.77 ms | 60.48 ms | 712.73 ms | 19475.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 41.56 ms | 37.00 ms | 56.94 ms | 69.38 ms | 263.15 ms | 13419.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 55.44 ms | 49.06 ms | 94.92 ms | 162.90 ms | 309.97 ms | 32109.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 67.51 ms | 56.03 ms | 118.30 ms | 199.83 ms | 568.94 ms | 36504.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 66.38 ms | 60.17 ms | 114.88 ms | 166.45 ms | 227.21 ms | 32526.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 99.56 ms | 100.65 ms | 120.81 ms | 151.42 ms | 743.25 ms | 26776.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (vibora) (python)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 268218.33 | 155.13 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 249385.67 | 298.57 MB |
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 246011.00 | 279.82 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 201103.67 | 228.41 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 199595.33 | 193.77 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 196016.67 | 184.49 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 195964.33 | 316.40 MB |
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 190003.67 | 388.89 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 189256.00 | 202.62 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 180251.00 | 362.42 MB |
| java (8) | [act](http://actframework.org) (1.8) | 179628.33 | 350.81 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.5) | 146900.67 | 84.95 MB |
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 134674.67 | 170.14 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 126392.67 | 169.42 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 124765.67 | 167.24 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 122349.33 | 199.11 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 119121.00 | 159.74 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 114031.33 | 199.86 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 112722.33 | 197.98 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 111452.33 | 150.17 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 105682.33 | 185.59 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 105002.00 | 139.68 MB |
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.11) | 99067.33 | 196.78 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 95556.33 | 245.57 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 82476.67 | 203.31 MB |
| go (1.11) | [gf](http://gfer.me) (1.4) | 96848.00 | 146.89 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 72286.33 | 67.93 MB |
| node (11.6) | [foxify](http://foxify.js.org) (0.10) | 70103.67 | 147.35 MB |
| node (11.6) | [polka](http://github.com/lukeed/polka) (0.5) | 69053.00 | 103.48 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 64764.67 | 108.67 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 64198.67 | 112.33 MB |
| node (11.6) | [rayo](http://rayo.js.org) (1.2) | 62425.33 | 93.56 MB |
| node (11.6) | [restana](http://github.com/jkyberneees/ana) (2.7) | 60353.67 | 90.42 MB |
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 60050.67 | 298.21 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 59787.67 | 128.08 MB |
| node (11.6) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 59149.67 | 88.63 MB |
| php (7.2) | [zend-framework](http://framework.zend.com) (3.1) | 57435.33 | 285.37 MB |
| php (7.2) | [symfony](http://symfony.com) (4.2) | 55752.67 | 277.05 MB |
| php (7.2) | [slim](http://slimframework.com) (3.12) | 55420.67 | 274.84 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 51464.67 | 267.37 MB |
| node (11.6) | [koa](http://koajs.com) (2.6) | 50049.00 | 106.00 MB |
| node (11.6) | [fastify](http://fastify.io) (1.13) | 47048.67 | 116.99 MB |
| c (99) | [kore](http://kore.io) (3.1) | 45337.67 | 122.92 MB |
| node (11.6) | [express](http://expressjs.com) (4.16) | 42188.33 | 103.33 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 41766.00 | 67.79 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 40439.33 | 211.01 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 37978.33 | 86.14 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 37583.33 | 69.74 MB |
| node (11.6) | [restify](http://restify.com) (7.5) | 35520.33 | 62.36 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 32727.67 | 53.34 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 32494.67 | 30.48 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 29712.33 | 27.86 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.15) | 29285.00 | 27.99 MB |
| node (11.6) | [hapi](http://hapijs.com) (17.8) | 27403.00 | 71.00 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 27372.67 | 49.98 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 26760.00 | 33.04 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 24740.00 | 61.01 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 23765.33 | 38.75 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 21671.33 | 12.53 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 18579.33 | 33.14 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 15647.33 | 9.04 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 15243.00 | 30.40 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 14963.00 | 43.45 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 13041.33 | 98.89 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 11296.33 | 29.38 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 9813.33 | 28.99 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 2664.67 | 8.17 MB |
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

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
Last update: 2019-05-21
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


:five: zend-framework (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 0.09 ms | 0.09 ms | 0.14 ms | 0.18 ms | 4.40 ms | 42.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 4.74 ms | 0.28 ms | 16.82 ms | 37.23 ms | 106.42 ms | 8593.00 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 6.26 ms | 0.36 ms | 21.56 ms | 43.73 ms | 119.32 ms | 10542.33 | 
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 0.50 ms | 0.49 ms | 0.84 ms | 1.25 ms | 12.26 ms | 289.67 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 200.98 ms | 0.52 ms | 381.88 ms | 4477.04 ms | 7242.67 ms | 731714.00 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 9.49 ms | 0.54 ms | 31.77 ms | 61.89 ms | 212.09 ms | 15306.33 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 193.14 ms | 0.56 ms | 337.04 ms | 4553.71 ms | 7455.47 ms | 734871.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 205.53 ms | 0.56 ms | 371.89 ms | 4648.80 ms | 7328.09 ms | 755331.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 8.84 ms | 0.58 ms | 28.36 ms | 56.18 ms | 131.16 ms | 13587.00 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 186.40 ms | 0.59 ms | 320.75 ms | 4410.90 ms | 7585.97 ms | 732527.67 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 157.63 ms | 0.59 ms | 440.74 ms | 2752.70 ms | 7066.90 ms | 502001.67 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 208.91 ms | 0.59 ms | 346.31 ms | 5046.22 ms | 7921.20 ms | 809911.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 12.02 ms | 0.90 ms | 36.22 ms | 69.88 ms | 153.30 ms | 17082.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 3.35 ms | 2.08 ms | 7.22 ms | 15.97 ms | 84.35 ms | 3464.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 94.45 ms | 2.10 ms | 4.96 ms | 3289.59 ms | 6594.28 ms | 555466.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 12.53 ms | 3.37 ms | 8.08 ms | 312.11 ms | 1422.29 ms | 74778.33 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 4.11 ms | 3.47 ms | 8.34 ms | 16.61 ms | 38.18 ms | 3591.67 | 
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 4.10 ms | 3.51 ms | 7.73 ms | 15.91 ms | 39.74 ms | 3238.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 5.03 ms | 3.98 ms | 10.20 ms | 18.55 ms | 40.98 ms | 4014.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.10 ms | 4.44 ms | 5.82 ms | 10.69 ms | 98.80 ms | 2398.33 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.72 ms | 4.50 ms | 7.31 ms | 15.37 ms | 148.40 ms | 3003.00 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 9.33 ms | 4.87 ms | 16.50 ms | 91.83 ms | 128.99 ms | 15682.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.23 ms | 4.96 ms | 8.22 ms | 14.78 ms | 42.79 ms | 2867.67 | 
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 5.81 ms | 5.12 ms | 10.51 ms | 21.26 ms | 202.00 ms | 4970.67 | 
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 5.86 ms | 5.16 ms | 10.00 ms | 18.30 ms | 41.84 ms | 3370.00 | 
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 9.59 ms | 6.72 ms | 19.91 ms | 44.10 ms | 186.09 ms | 9229.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 7.88 ms | 6.74 ms | 10.60 ms | 20.24 ms | 430.54 ms | 11023.67 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 47.38 ms | 7.13 ms | 152.63 ms | 376.67 ms | 939.21 ms | 82211.00 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 9.83 ms | 7.18 ms | 19.53 ms | 42.88 ms | 236.16 ms | 9456.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 7.87 ms | 7.33 ms | 12.72 ms | 23.56 ms | 134.70 ms | 5178.67 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 10.29 ms | 7.39 ms | 21.40 ms | 45.22 ms | 184.00 ms | 9363.67 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 10.03 ms | 7.52 ms | 17.69 ms | 42.02 ms | 428.67 ms | 13621.67 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 11.00 ms | 7.54 ms | 23.56 ms | 50.23 ms | 301.13 ms | 11830.67 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 10.32 ms | 7.56 ms | 20.37 ms | 44.46 ms | 202.81 ms | 11034.33 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 10.73 ms | 7.60 ms | 21.87 ms | 47.94 ms | 218.95 ms | 11806.67 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 9.64 ms | 7.84 ms | 17.21 ms | 38.52 ms | 134.93 ms | 7061.33 | 
| node (12.2) | [restana](http://github.com/jkyberneees/ana) (3.0) | 9.24 ms | 7.86 ms | 13.65 ms | 30.76 ms | 416.63 ms | 15228.33 | 
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 9.39 ms | 8.32 ms | 18.57 ms | 35.77 ms | 168.51 ms | 7939.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 220.42 ms | 9.48 ms | 58.91 ms | 4773.67 ms | 7907.95 ms | 850765.67 | 
| go (1.12) | [gf](http://goframe.org) (1.6) | 12.95 ms | 9.53 ms | 26.39 ms | 56.18 ms | 216.28 ms | 11224.67 | 
| node (12.2) | [polka](http://github.com/lukeed/polka) (0.5) | 12.81 ms | 9.54 ms | 16.17 ms | 104.39 ms | 579.68 ms | 27404.00 | 
| node (12.2) | [rayo](http://rayo.js.org) (1.3) | 12.03 ms | 9.60 ms | 17.00 ms | 37.35 ms | 518.28 ms | 19539.33 | 
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 13.13 ms | 9.79 ms | 25.68 ms | 51.08 ms | 181.72 ms | 10139.00 | 
| node (12.2) | [foxify](http://foxify.js.org) (0.10) | 15.72 ms | 11.23 ms | 19.70 ms | 126.42 ms | 717.71 ms | 34036.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 11.62 ms | 11.60 ms | 14.11 ms | 16.49 ms | 67.40 ms | 2084.00 | 
| node (12.2) | [fastify](http://fastify.io) (2.3) | 15.61 ms | 11.65 ms | 20.11 ms | 117.95 ms | 652.73 ms | 30687.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 17.32 ms | 12.01 ms | 36.97 ms | 65.82 ms | 236.19 ms | 14171.67 | 
| node (12.2) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 20.61 ms | 12.62 ms | 23.57 ms | 275.14 ms | 973.86 ms | 55523.67 | 
| node (12.2) | [koa](http://koajs.com) (2.7) | 18.93 ms | 13.03 ms | 22.39 ms | 200.08 ms | 864.61 ms | 45359.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 16.56 ms | 14.04 ms | 28.83 ms | 74.18 ms | 372.33 ms | 16129.67 | 
| node (12.2) | [restify](http://restify.com) (8.2) | 16.86 ms | 14.23 ms | 20.76 ms | 73.61 ms | 493.97 ms | 20097.33 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 28.97 ms | 14.25 ms | 26.09 ms | 489.60 ms | 1990.03 ms | 101937.33 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 16.74 ms | 15.80 ms | 26.68 ms | 41.64 ms | 88.70 ms | 7887.67 | 
| python (3.7) | [hug](http://hug.rest) (2.5) | 20.20 ms | 16.22 ms | 36.99 ms | 65.02 ms | 216.84 ms | 12919.00 | 
| node (12.2) | [express](http://expressjs.com) (4.16) | 24.46 ms | 16.87 ms | 28.93 ms | 262.87 ms | 1028.57 ms | 55575.00 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.22) | 25.75 ms | 22.61 ms | 44.22 ms | 67.67 ms | 160.17 ms | 13468.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 28.33 ms | 26.68 ms | 48.06 ms | 65.95 ms | 94.58 ms | 13863.33 | 
| node (12.2) | [hapi](http://hapijs.com) (18.1) | 42.94 ms | 27.15 ms | 43.14 ms | 614.83 ms | 1588.58 ms | 102647.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 37.15 ms | 32.07 ms | 56.28 ms | 86.53 ms | 284.10 ms | 16200.00 | 
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 42.47 ms | 32.72 ms | 54.34 ms | 332.90 ms | 1642.77 ms | 79530.67 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 37.22 ms | 33.15 ms | 57.56 ms | 92.63 ms | 402.52 ms | 17839.00 | 
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 38.49 ms | 35.70 ms | 53.05 ms | 81.91 ms | 633.84 ms | 27353.00 | 
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 38.22 ms | 38.16 ms | 47.17 ms | 56.06 ms | 275.26 ms | 9271.00 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 44.14 ms | 39.92 ms | 73.01 ms | 100.54 ms | 184.07 ms | 20158.00 | 
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 39.93 ms | 39.93 ms | 48.63 ms | 95.03 ms | 322.51 ms | 14594.33 | 
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 50.65 ms | 43.51 ms | 120.94 ms | 248.18 ms | 626.67 ms | 56370.67 | 
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 43.64 ms | 43.92 ms | 53.70 ms | 76.13 ms | 211.58 ms | 11222.33 | 
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 43.90 ms | 44.25 ms | 52.47 ms | 84.28 ms | 344.62 ms | 16045.33 | 
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 46.37 ms | 45.59 ms | 56.59 ms | 94.72 ms | 532.22 ms | 17718.67 | 
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 46.44 ms | 47.32 ms | 55.31 ms | 68.78 ms | 293.69 ms | 12904.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 61.25 ms | 53.37 ms | 105.22 ms | 189.30 ms | 272.51 ms | 35619.67 | 
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 58.34 ms | 57.19 ms | 69.89 ms | 79.10 ms | 291.19 ms | 13030.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 75.69 ms | 76.76 ms | 114.01 ms | 147.95 ms | 210.14 ms | 30126.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 100.79 ms | 87.78 ms | 176.98 ms | 220.55 ms | 604.74 ms | 43866.00 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 98.59 ms | 88.67 ms | 176.43 ms | 225.89 ms | 282.67 ms | 50874.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 99.47 ms | 96.14 ms | 127.01 ms | 155.43 ms | 416.21 ms | 23514.00 | 
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 102.00 ms | 99.47 ms | 144.80 ms | 192.15 ms | 289.80 ms | 33168.00 | 
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 265.10 ms | 238.67 ms | 325.96 ms | 1151.46 ms | 2399.98 ms | 184822.67 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 305584.00 | 176.79 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 241852.00 | 289.63 MB |
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 230903.00 | 262.49 MB |
| c (99) | [kore](http://kore.io) (3.1) | 225373.00 | 585.54 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 214561.33 | 208.33 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 211938.67 | 240.54 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 196670.33 | 317.24 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 193497.33 | 389.09 MB |
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 170348.67 | 346.64 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 167800.67 | 97.11 MB |
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 166318.00 | 177.21 MB |
| java (8) | [act](http://actframework.org) (1.8) | 156586.67 | 270.33 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 126477.33 | 206.03 MB |
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 119279.67 | 150.24 MB |
| node (12.2) | [restana](http://github.com/jkyberneees/ana) (3.0) | 117849.00 | 176.60 MB |
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 116782.33 | 155.05 MB |
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 111396.33 | 173.46 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 110494.67 | 145.79 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 109309.00 | 145.31 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 109107.00 | 144.87 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 108474.00 | 146.47 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 106859.00 | 187.52 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 105858.67 | 185.58 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 103726.00 | 138.94 MB |
| node (12.2) | [polka](http://github.com/lukeed/polka) (0.5) | 94016.67 | 140.99 MB |
| node (12.2) | [rayo](http://rayo.js.org) (1.3) | 91874.33 | 137.72 MB |
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 91619.67 | 182.38 MB |
| go (1.12) | [gf](http://goframe.org) (1.6) | 85875.67 | 129.93 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 84599.33 | 79.55 MB |
| node (12.2) | [fastify](http://fastify.io) (2.3) | 81475.33 | 211.36 MB |
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 80528.33 | 188.81 MB |
| node (12.2) | [foxify](http://foxify.js.org) (0.10) | 77632.33 | 163.28 MB |
| node (12.2) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 75053.33 | 112.58 MB |
| node (12.2) | [koa](http://koajs.com) (2.7) | 69052.67 | 146.30 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 68618.00 | 120.32 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 64196.00 | 158.24 MB |
| node (12.2) | [restify](http://restify.com) (8.2) | 64127.33 | 112.60 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 60405.33 | 100.93 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 59398.00 | 128.05 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 56157.33 | 120.46 MB |
| node (12.2) | [express](http://expressjs.com) (4.16) | 53807.67 | 131.74 MB |
| python (3.7) | [hug](http://hug.rest) (2.5) | 51055.67 | 126.67 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 41717.33 | 65.60 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.22) | 39686.00 | 85.81 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 39555.67 | 196.21 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 39346.00 | 195.16 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 38917.33 | 192.98 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 38109.33 | 188.91 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 36013.67 | 186.78 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 36009.00 | 81.66 MB |
| node (12.2) | [hapi](http://hapijs.com) (18.1) | 34501.00 | 90.08 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 31621.33 | 164.79 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 27393.00 | 50.93 MB |
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 27264.00 | 35.99 MB |
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 26988.33 | 50.86 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 26938.67 | 25.67 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 26937.33 | 49.94 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 26391.67 | 65.06 MB |
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 25818.33 | 24.21 MB |
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 24957.67 | 23.40 MB |
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 22831.00 | 28.12 MB |
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 22803.33 | 37.19 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 22771.33 | 43.99 MB |
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 21304.33 | 38.91 MB |
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 21258.00 | 26.13 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 20471.33 | 11.80 MB |
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 16996.67 | 27.71 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 16888.00 | 30.12 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 14426.67 | 8.32 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 13480.67 | 101.89 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 13150.33 | 26.17 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 10612.67 | 27.53 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 10193.67 | 22.20 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 9910.00 | 28.77 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 9851.00 | 29.13 MB |
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 9731.67 | 25.04 MB |
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 3864.00 | 9.50 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 2692.00 | 8.25 MB |
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

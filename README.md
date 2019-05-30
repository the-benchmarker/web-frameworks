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
Last update: 2019-05-30
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
| rust (1.35) | [nickel](http://nickel-org.github.io) (0.11) | 0.11 ms | 0.10 ms | 0.16 ms | 0.20 ms | 10.71 ms | 75.67 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 5.65 ms | 0.35 ms | 19.69 ms | 42.27 ms | 130.82 ms | 9911.00 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 7.28 ms | 0.44 ms | 25.02 ms | 50.83 ms | 118.36 ms | 12238.33 | 
| rust (1.35) | [iron](http://ironframework.io) (0.6) | 0.63 ms | 0.61 ms | 1.02 ms | 1.49 ms | 25.31 ms | 381.00 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 169.29 ms | 0.70 ms | 357.42 ms | 3620.72 ms | 7508.85 ms | 623214.33 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 91.70 ms | 0.72 ms | 212.61 ms | 1666.30 ms | 7025.73 ms | 335017.33 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 208.06 ms | 0.72 ms | 373.86 ms | 4915.48 ms | 7547.08 ms | 787271.00 | 
| php (7.3) | [symfony](http://symfony.com) (4.3) | 192.87 ms | 0.73 ms | 346.16 ms | 3944.50 ms | 7253.11 ms | 697681.33 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 196.89 ms | 0.77 ms | 429.61 ms | 4215.80 ms | 7677.95 ms | 709147.33 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 11.76 ms | 0.77 ms | 37.69 ms | 72.77 ms | 191.92 ms | 18012.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 11.01 ms | 0.80 ms | 33.93 ms | 65.36 ms | 164.33 ms | 15939.33 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 13.50 ms | 0.99 ms | 41.02 ms | 79.92 ms | 190.01 ms | 19514.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 71.28 ms | 2.81 ms | 6.03 ms | 2361.00 ms | 6577.58 ms | 435448.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 196.72 ms | 3.10 ms | 407.38 ms | 4374.04 ms | 7412.13 ms | 718551.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 4.50 ms | 3.43 ms | 10.06 ms | 17.95 ms | 70.71 ms | 4162.00 | 
| rust (1.35) | [actix-web](http://actix.rs) (0.7) | 5.07 ms | 4.81 ms | 9.57 ms | 19.20 ms | 152.00 ms | 4144.33 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 5.04 ms | 4.96 ms | 9.64 ms | 18.61 ms | 40.19 ms | 3877.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 7.95 ms | 5.44 ms | 11.10 ms | 57.77 ms | 619.39 ms | 26075.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 5.84 ms | 5.53 ms | 9.76 ms | 12.50 ms | 44.03 ms | 2579.67 | 
| rust (1.35) | [gotham](http://gotham.rs) (0.3) | 7.34 ms | 5.80 ms | 11.61 ms | 23.27 ms | 360.11 ms | 9573.33 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 6.50 ms | 5.94 ms | 9.77 ms | 19.11 ms | 114.81 ms | 3359.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 9.52 ms | 6.00 ms | 17.19 ms | 81.45 ms | 139.62 ms | 12790.67 | 
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 7.07 ms | 6.28 ms | 10.93 ms | 17.70 ms | 39.12 ms | 3083.00 | 
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 7.20 ms | 6.38 ms | 11.06 ms | 18.23 ms | 44.47 ms | 3251.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 7.04 ms | 6.41 ms | 14.18 ms | 22.47 ms | 48.41 ms | 4973.00 | 
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 7.72 ms | 6.54 ms | 12.46 ms | 23.60 ms | 48.34 ms | 4215.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 7.28 ms | 6.76 ms | 11.34 ms | 19.76 ms | 102.04 ms | 3798.67 | 
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 7.61 ms | 6.83 ms | 11.33 ms | 19.04 ms | 43.01 ms | 3340.67 | 
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 7.66 ms | 6.92 ms | 11.35 ms | 18.39 ms | 40.40 ms | 3224.33 | 
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 8.77 ms | 8.23 ms | 13.57 ms | 23.54 ms | 55.67 ms | 4138.67 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 57.48 ms | 8.40 ms | 186.62 ms | 457.70 ms | 1117.05 ms | 100136.33 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 11.69 ms | 9.14 ms | 22.61 ms | 49.34 ms | 197.28 ms | 9643.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 10.28 ms | 9.22 ms | 16.07 ms | 30.76 ms | 217.39 ms | 6370.33 | 
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 12.67 ms | 9.32 ms | 26.25 ms | 56.97 ms | 214.67 ms | 11654.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 10.80 ms | 9.39 ms | 13.46 ms | 36.48 ms | 576.68 ms | 18791.67 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 13.17 ms | 9.71 ms | 26.20 ms | 56.94 ms | 337.60 ms | 13908.00 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 13.79 ms | 9.72 ms | 29.36 ms | 62.26 ms | 299.06 ms | 13241.00 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 12.95 ms | 9.86 ms | 25.11 ms | 55.22 ms | 231.49 ms | 11141.67 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 12.13 ms | 9.86 ms | 21.12 ms | 48.03 ms | 215.92 ms | 9850.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 165.35 ms | 10.00 ms | 29.33 ms | 4157.01 ms | 7917.02 ms | 724786.33 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 12.45 ms | 10.05 ms | 22.40 ms | 48.88 ms | 118.41 ms | 8950.33 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 13.57 ms | 10.06 ms | 27.53 ms | 58.35 ms | 190.76 ms | 11679.00 | 
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 13.02 ms | 10.51 ms | 23.89 ms | 49.96 ms | 272.99 ms | 12407.67 | 
| go (1.12) | [gf](http://goframe.org) (1.6) | 16.65 ms | 11.89 ms | 34.44 ms | 75.88 ms | 238.41 ms | 15288.67 | 
| node (12.3) | [0http](http://github.com/jkyberneees/0http) (1.0) | 21.60 ms | 12.65 ms | 25.29 ms | 305.92 ms | 869.07 ms | 53071.67 | 
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 16.58 ms | 12.67 ms | 32.02 ms | 58.64 ms | 267.77 ms | 14270.33 | 
| node (12.3) | [restana](http://github.com/jkyberneees/ana) (3.0) | 19.44 ms | 13.25 ms | 25.79 ms | 173.94 ms | 733.09 ms | 38081.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 14.10 ms | 14.19 ms | 16.78 ms | 19.81 ms | 161.92 ms | 2906.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 19.82 ms | 14.67 ms | 41.18 ms | 67.62 ms | 230.68 ms | 14250.67 | 
| node (12.3) | [polka](http://github.com/lukeed/polka) (0.5) | 26.14 ms | 15.50 ms | 30.89 ms | 338.81 ms | 1028.33 ms | 60839.67 | 
| node (12.3) | [rayo](http://rayo.js.org) (1.3) | 29.09 ms | 16.01 ms | 31.05 ms | 432.20 ms | 1117.89 ms | 73709.00 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 49.63 ms | 18.38 ms | 33.15 ms | 1023.17 ms | 2250.10 ms | 179580.00 | 
| python (3.7) | [starlette](http://starlette.io) (0.12) | 22.17 ms | 18.42 ms | 40.00 ms | 62.94 ms | 109.33 ms | 12871.33 | 
| python (3.7) | [hug](http://hug.rest) (2.5) | 25.12 ms | 20.18 ms | 44.67 ms | 71.77 ms | 307.25 ms | 14434.67 | 
| node (12.3) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 46.12 ms | 21.86 ms | 45.33 ms | 719.47 ms | 1638.24 ms | 122380.33 | 
| node (12.3) | [fastify](http://fastify.io) (2.4) | 40.40 ms | 22.44 ms | 39.29 ms | 624.28 ms | 1458.19 ms | 103567.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 25.84 ms | 23.10 ms | 45.56 ms | 79.52 ms | 732.28 ms | 22753.00 | 
| node (12.3) | [foxify](http://foxify.js.org) (0.10) | 35.93 ms | 23.39 ms | 44.89 ms | 409.62 ms | 1218.64 ms | 71218.00 | 
| node (12.3) | [koa](http://koajs.com) (2.7) | 37.75 ms | 24.89 ms | 42.62 ms | 467.73 ms | 1149.06 ms | 76608.67 | 
| node (12.3) | [restify](http://restify.com) (8.2) | 31.26 ms | 25.69 ms | 41.54 ms | 202.11 ms | 644.35 ms | 33929.67 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.24) | 34.68 ms | 29.54 ms | 62.50 ms | 104.11 ms | 212.49 ms | 20841.33 | 
| node (12.3) | [express](http://expressjs.com) (4.16) | 50.89 ms | 30.37 ms | 52.81 ms | 681.65 ms | 1601.96 ms | 112823.33 | 
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 34.77 ms | 32.10 ms | 50.98 ms | 67.13 ms | 497.38 ms | 17907.33 | 
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 81.54 ms | 37.79 ms | 70.27 ms | 1443.30 ms | 3028.78 ms | 235364.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 43.05 ms | 40.98 ms | 63.59 ms | 107.66 ms | 235.50 ms | 20609.00 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 54.25 ms | 42.53 ms | 105.53 ms | 158.88 ms | 547.22 ms | 39180.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 52.12 ms | 43.71 ms | 97.46 ms | 137.11 ms | 462.95 ms | 29294.00 | 
| node (12.3) | [hapi](http://hapijs.com) (18.1) | 79.55 ms | 45.03 ms | 70.51 ms | 1172.59 ms | 2294.69 ms | 185714.00 | 
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 50.52 ms | 47.59 ms | 62.12 ms | 85.35 ms | 593.68 ms | 25967.33 | 
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 51.90 ms | 50.91 ms | 64.86 ms | 78.13 ms | 420.83 ms | 14941.00 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 59.04 ms | 57.14 ms | 86.48 ms | 104.54 ms | 239.03 ms | 21068.67 | 
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 63.80 ms | 57.47 ms | 146.95 ms | 313.45 ms | 743.05 ms | 69985.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 81.83 ms | 71.64 ms | 138.20 ms | 226.42 ms | 402.91 ms | 42057.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 95.09 ms | 88.86 ms | 159.69 ms | 212.10 ms | 350.18 ms | 44885.33 | 
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 111.74 ms | 108.75 ms | 157.63 ms | 205.48 ms | 298.48 ms | 34994.67 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 125.50 ms | 117.60 ms | 183.17 ms | 230.50 ms | 339.65 ms | 36842.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 129.04 ms | 118.23 ms | 202.52 ms | 278.95 ms | 816.55 ms | 58607.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 128.56 ms | 126.40 ms | 174.37 ms | 211.91 ms | 676.35 ms | 42070.00 | 
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 150.84 ms | 146.09 ms | 200.09 ms | 298.03 ms | 985.04 ms | 57178.00 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 226734.33 | 131.17 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 190309.00 | 227.73 MB |
| rust (1.35) | [actix-web](http://actix.rs) (0.7) | 187544.33 | 213.12 MB |
| c (99) | [kore](http://kore.io) (3.1) | 166322.33 | 432.25 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 155270.67 | 150.76 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 147516.00 | 167.55 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 141235.00 | 227.52 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 140163.00 | 281.90 MB |
| rust (1.35) | [gotham](http://gotham.rs) (0.3) | 135180.33 | 276.82 MB |
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 133089.67 | 125.23 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 131597.67 | 76.08 MB |
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 131478.33 | 123.67 MB |
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 126322.67 | 135.04 MB |
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 124838.00 | 155.89 MB |
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 123577.00 | 202.09 MB |
| java (8) | [act](http://actframework.org) (1.8) | 117542.00 | 202.96 MB |
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 110296.33 | 180.32 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 96223.67 | 156.81 MB |
| rust (1.35) | [iron](http://ironframework.io) (0.6) | 94429.67 | 119.23 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 91637.00 | 122.38 MB |
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 87227.00 | 116.16 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 86978.67 | 115.88 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 84810.33 | 112.45 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 84292.00 | 147.80 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 82902.67 | 111.35 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 81977.00 | 109.82 MB |
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 81082.00 | 126.43 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 80966.67 | 142.13 MB |
| rust (1.35) | [nickel](http://nickel-org.github.io) (0.11) | 72364.33 | 144.03 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 69121.00 | 65.01 MB |
| node (12.3) | [0http](http://github.com/jkyberneees/0http) (1.0) | 68588.33 | 102.70 MB |
| go (1.12) | [gf](http://goframe.org) (1.6) | 67805.00 | 102.64 MB |
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 65576.33 | 153.77 MB |
| node (12.3) | [restana](http://github.com/jkyberneees/ana) (3.0) | 65458.33 | 98.08 MB |
| node (12.3) | [polka](http://github.com/lukeed/polka) (0.5) | 56430.00 | 84.57 MB |
| node (12.3) | [rayo](http://rayo.js.org) (1.3) | 54376.33 | 81.43 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 53667.33 | 132.27 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 48880.00 | 81.51 MB |
| python (3.7) | [starlette](http://starlette.io) (0.12) | 46102.00 | 99.39 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 45770.00 | 98.08 MB |
| node (12.3) | [fastify](http://fastify.io) (2.4) | 44293.00 | 111.68 MB |
| node (12.3) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 42749.67 | 64.02 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 42491.67 | 74.48 MB |
| python (3.7) | [hug](http://hug.rest) (2.5) | 40074.00 | 99.39 MB |
| node (12.3) | [foxify](http://foxify.js.org) (0.10) | 37898.00 | 79.69 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 37045.33 | 184.01 MB |
| node (12.3) | [koa](http://koajs.com) (2.7) | 36395.00 | 76.92 MB |
| node (12.3) | [restify](http://restify.com) (8.2) | 35143.67 | 61.62 MB |
| php (7.3) | [symfony](http://symfony.com) (4.3) | 33056.33 | 163.99 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 32863.00 | 52.02 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 32242.33 | 160.00 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 31908.33 | 158.36 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 31110.00 | 161.53 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.24) | 29805.00 | 64.31 MB |
| node (12.3) | [express](http://expressjs.com) (4.16) | 29371.00 | 71.76 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 28370.00 | 52.63 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 28028.33 | 146.08 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 23860.33 | 54.11 MB |
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 23084.67 | 43.52 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 22590.33 | 21.55 MB |
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 21390.67 | 27.82 MB |
| node (12.3) | [hapi](http://hapijs.com) (18.1) | 20926.67 | 54.12 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 19905.33 | 36.96 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 19684.67 | 48.48 MB |
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 19472.33 | 23.99 MB |
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 18973.67 | 34.67 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 17526.33 | 10.12 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 16858.67 | 32.54 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 12470.00 | 22.25 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 11591.00 | 6.69 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 10880.67 | 82.36 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 10493.33 | 20.92 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 9453.00 | 24.54 MB |
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 8869.67 | 22.85 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 7892.67 | 17.22 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 7554.67 | 22.40 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 7540.00 | 21.83 MB |
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 6443.00 | 15.88 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 2218.67 | 6.80 MB |
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

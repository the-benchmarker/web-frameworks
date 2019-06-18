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

+ Make framework list

~~~sh
bin/make config
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
Last update: 2019-06-18
```
OS: Linux (version: 5.0.16-300.fc30.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: cuba (ruby)


:four: rack-routing (ruby)


:five: iron (rust)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.35) | [nickel](https://nickel-org.github.io) (0.11) | 0.09 ms | 0.09 ms | 0.13 ms | 0.18 ms | 6.44 ms | 62.00 | 
| ruby (2.6) | [roda](https://roda.jeremyevans.net) (3.21) | 3.06 ms | 0.16 ms | 11.48 ms | 29.34 ms | 78.90 ms | 6448.67 | 
| ruby (2.6) | [cuba](https://cuba.is) (3.9) | 3.40 ms | 0.18 ms | 12.57 ms | 31.15 ms | 83.30 ms | 6887.33 | 
| ruby (2.6) | [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 4.09 ms | 0.20 ms | 15.58 ms | 35.37 ms | 97.91 ms | 8096.00 | 
| rust (1.35) | [iron](https://ironframework.io) (0.6) | 0.31 ms | 0.29 ms | 0.51 ms | 0.88 ms | 9.58 ms | 196.00 | 
| php (7.3) | [zend-framework](https://framework.zend.com) (3.1) | 79.24 ms | 0.33 ms | 211.18 ms | 1369.20 ms | 6768.32 ms | 300031.67 | 
| php (7.3) | [slim](https://slimframework.com) (3.12) | 114.77 ms | 0.35 ms | 208.84 ms | 2721.11 ms | 6757.87 ms | 454779.67 | 
| ruby (2.6) | [flame](https://github.com/AlexWayfer/flame) (4.18) | 6.31 ms | 0.35 ms | 22.05 ms | 46.92 ms | 107.82 ms | 10991.00 | 
| php (7.3) | [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 128.88 ms | 0.35 ms | 216.96 ms | 3143.40 ms | 6796.38 ms | 518954.00 | 
| php (7.3) | [symfony](https://symfony.com) (4.3) | 117.77 ms | 0.35 ms | 222.59 ms | 2647.06 ms | 6813.67 ms | 474093.33 | 
| php (7.3) | [lumen](https://lumen.laravel.com) (5.8) | 141.02 ms | 0.35 ms | 262.72 ms | 3192.29 ms | 6803.45 ms | 527054.33 | 
| php (7.3) | [laravel](https://laravel.com) (5.8) | 152.17 ms | 0.36 ms | 298.35 ms | 3399.09 ms | 7058.00 ms | 558029.33 | 
| ruby (2.6) | [hanami](https://hanamirb.org) (1.3) | 7.47 ms | 0.37 ms | 26.45 ms | 55.05 ms | 138.76 ms | 13129.00 | 
| ruby (2.6) | [sinatra](https://sinatrarb.com) (2.0) | 8.34 ms | 0.47 ms | 27.85 ms | 56.36 ms | 123.75 ms | 13523.33 | 
| rust (nightly) | [rocket](https://rocket.rs) (0.4) | 71.47 ms | 1.46 ms | 2.96 ms | 2627.08 ms | 6593.53 ms | 483460.33 | 
| c (11) | [agoo-c](https://github.com/ohler55/agoo-c) (0.5) | 2.97 ms | 1.81 ms | 6.81 ms | 14.98 ms | 45.61 ms | 3257.33 | 
| python (3.7) | [japronto](https://github.com/squeaky-pl/japronto) (0.1) | 3.02 ms | 1.99 ms | 6.69 ms | 15.24 ms | 36.63 ms | 3210.33 | 
| rust (1.35) | [actix-web](https://actix.rs) (0.7) | 3.17 ms | 2.32 ms | 6.68 ms | 15.32 ms | 36.59 ms | 3107.00 | 
| go (1.12) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.07 ms | 2.36 ms | 5.53 ms | 9.82 ms | 99.37 ms | 2263.33 | 
| ruby (2.6) | [rails](https://rubyonrails.org) (5.2) | 33.06 ms | 2.43 ms | 111.66 ms | 302.38 ms | 905.65 ms | 64637.00 | 
| c (99) | [kore](https://kore.io) (3.1) | 5.11 ms | 2.76 ms | 6.93 ms | 24.37 ms | 702.16 ms | 23137.00 | 
| cpp (11) | [evhtp](https://criticalstack/libevhtp) (1.2) | 2.87 ms | 2.90 ms | 4.71 ms | 6.67 ms | 31.29 ms | 1467.67 | 
| python (3.6) | [vibora](https://vibora.io) (0.0) | 4.10 ms | 3.17 ms | 8.75 ms | 16.43 ms | 48.28 ms | 3662.67 | 
| rust (1.35) | [gotham](https://gotham.rs) (0.3) | 4.10 ms | 3.17 ms | 7.67 ms | 18.37 ms | 277.31 ms | 5512.33 | 
| crystal (0.29) | [router.cr](https://github.com/tbrand/router.cr) (0.2) | 3.70 ms | 3.45 ms | 6.31 ms | 12.56 ms | 27.90 ms | 2409.67 | 
| nim (0.2) | [jester](https://github.com/dom96/jester) (0.4) | 4.04 ms | 3.47 ms | 6.70 ms | 12.50 ms | 41.77 ms | 2351.33 | 
| crystal (0.29) | [raze](https://razecr.com) (0.3) | 3.72 ms | 3.49 ms | 6.21 ms | 12.41 ms | 27.83 ms | 2371.00 | 
| crystal (0.29) | [kemal](https://kemalcr.com) (0.25) | 3.91 ms | 3.66 ms | 6.48 ms | 13.41 ms | 29.76 ms | 2496.33 | 
| crystal (0.29) | [spider-gazelle](https://spider-gazelle.net) (1.4) | 4.18 ms | 3.71 ms | 7.67 ms | 13.72 ms | 28.37 ms | 2729.33 | 
| ruby (2.6) | [agoo](https://github.com/ohler55/agoo) (2.8) | 6.70 ms | 3.74 ms | 10.68 ms | 82.62 ms | 121.30 ms | 12727.00 | 
| crystal (0.29) | [amber](https://amberframework.org) (0.28) | 4.29 ms | 4.30 ms | 6.79 ms | 12.95 ms | 29.33 ms | 2382.33 | 
| crystal (0.29) | [orion](https://github.com/obsidian/orion) (1.7) | 4.74 ms | 4.54 ms | 7.75 ms | 13.20 ms | 35.13 ms | 2482.33 | 
| go (1.12) | [chi](https://github.com/go-chi/chi) (4.0) | 6.07 ms | 4.57 ms | 12.53 ms | 25.53 ms | 120.21 ms | 5359.67 | 
| go (1.12) | [gorouter](https://github.com/vardius/gorouter/wiki) (4.0) | 6.58 ms | 4.58 ms | 14.07 ms | 31.13 ms | 215.07 ms | 7064.67 | 
| node (12.4) | [0http](https://github.com/jkyberneees/0http) (1.0) | 6.04 ms | 4.59 ms | 8.97 ms | 23.43 ms | 320.37 ms | 11981.00 | 
| node (12.4) | [restana](https://github.com/jkyberneees/ana) (3.1) | 5.45 ms | 4.60 ms | 9.03 ms | 16.35 ms | 201.68 ms | 5709.33 | 
| go (1.12) | [echo](https://echo.labstack.com) (4.1) | 6.43 ms | 4.62 ms | 13.49 ms | 27.59 ms | 217.94 ms | 6587.00 | 
| go (1.12) | [kami](https://github.com/guregu/kami) (2.2) | 6.07 ms | 4.69 ms | 11.41 ms | 24.34 ms | 163.95 ms | 5089.00 | 
| go (1.12) | [gin](https://gin-gonic.com) (1.4) | 6.70 ms | 4.72 ms | 14.10 ms | 28.95 ms | 230.24 ms | 6997.67 | 
| go (1.12) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 6.87 ms | 4.72 ms | 14.95 ms | 30.53 ms | 184.49 ms | 6799.33 | 
| go (1.12) | [beego](https://beego.me) (1.12) | 6.62 ms | 4.76 ms | 13.64 ms | 28.50 ms | 211.36 ms | 6458.67 | 
| go (1.12) | [violetear](https://violetear.org) (7.0) | 6.27 ms | 4.77 ms | 12.01 ms | 25.95 ms | 172.14 ms | 5247.00 | 
| java (8) | [act](https://actframework.org) (1.8) | 5.72 ms | 4.83 ms | 9.47 ms | 19.74 ms | 137.52 ms | 4574.33 | 
| csharp (7.3) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (2.2) | 6.58 ms | 4.85 ms | 8.18 ms | 42.50 ms | 423.41 ms | 17335.33 | 
| node (12.4) | [polka](https://github.com/lukeed/polka) (0.5) | 6.61 ms | 4.87 ms | 9.81 ms | 21.51 ms | 284.05 ms | 9112.67 | 
| node (12.4) | [rayo](https://rayo.js.org) (1.3) | 6.32 ms | 4.93 ms | 9.87 ms | 20.46 ms | 276.55 ms | 7553.33 | 
| kotlin (1.3) | [ktor](https://ktor.io) (1.2) | 6.67 ms | 5.07 ms | 13.12 ms | 28.44 ms | 135.26 ms | 5969.33 | 
| python (3.7) | [falcon](https://falconframework.org) (2.0) | 7.93 ms | 5.69 ms | 14.48 ms | 30.84 ms | 250.04 ms | 8536.33 | 
| go (1.12) | [gf](https://goframe.org) (1.6) | 8.62 ms | 5.82 ms | 18.38 ms | 39.90 ms | 173.09 ms | 8293.00 | 
| node (12.4) | [muneem](https://github.com/node-muneem/muneem) (2.4) | 9.57 ms | 6.66 ms | 14.21 ms | 76.72 ms | 473.84 ms | 20939.00 | 
| node (12.4) | [foxify](https://foxify.js.org) (0.1) | 8.69 ms | 7.04 ms | 12.34 ms | 28.18 ms | 398.60 ms | 14338.00 | 
| scala (2.12) | [akkahttp](https://akka.io) (10.1) | 219.04 ms | 7.04 ms | 107.99 ms | 5065.73 ms | 7928.14 ms | 873925.67 | 
| node (12.4) | [fastify](https://fastify.io) (2.4) | 10.85 ms | 7.09 ms | 13.05 ms | 101.14 ms | 641.08 ms | 29913.67 | 
| python (3.7) | [bottle](https://bottlepy.org) (0.12) | 8.82 ms | 7.83 ms | 14.46 ms | 28.78 ms | 212.09 ms | 6955.67 | 
| node (12.4) | [koa](https://koajs.com) (2.7) | 9.42 ms | 8.04 ms | 13.66 ms | 29.16 ms | 440.82 ms | 16095.00 | 
| node (12.4) | [restify](https://restify.com) (8.2) | 10.03 ms | 8.56 ms | 13.42 ms | 29.90 ms | 334.67 ms | 10704.33 | 
| java (8) | [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 10.69 ms | 8.76 ms | 14.10 ms | 43.03 ms | 316.76 ms | 10326.67 | 
| swift (5.0) | [perfect](https://perfect.org) (3.1) | 8.93 ms | 8.99 ms | 11.06 ms | 13.12 ms | 45.69 ms | 1796.00 | 
| node (12.4) | [express](https://expressjs.com) (4.16) | 12.23 ms | 9.14 ms | 16.30 ms | 70.18 ms | 566.99 ms | 23155.33 | 
| python (3.7) | [hug](https://hug.rest) (2.5) | 13.15 ms | 10.05 ms | 20.70 ms | 81.78 ms | 298.85 ms | 15632.00 | 
| python (3.7) | [starlette](https://starlette.io) (0.12) | 11.75 ms | 10.43 ms | 19.10 ms | 27.74 ms | 72.81 ms | 5520.33 | 
| swift (5.0) | [vapor](https://vapor.codes) (3.3) | 15.82 ms | 10.70 ms | 19.85 ms | 90.78 ms | 1004.83 ms | 40253.00 | 
| scala (2.12) | [http4s](https://http4s.org) (0.18) | 138.17 ms | 15.98 ms | 430.49 ms | 869.29 ms | 1669.01 ms | 203312.00 | 
| python (3.7) | [aiohttp](https://aiohttp.readthedocs.io) (3.5) | 20.45 ms | 16.50 ms | 38.12 ms | 53.92 ms | 76.66 ms | 11211.00 | 
| python (3.7) | [fastapi](https://fastapi.tiangolo.com) (0.29) | 17.59 ms | 16.96 ms | 29.63 ms | 45.60 ms | 80.58 ms | 9206.33 | 
| node (12.4) | [hapi](https://hapijs.com) (18.1) | 23.50 ms | 17.48 ms | 29.79 ms | 174.64 ms | 847.90 ms | 40707.33 | 
| swift (5.0) | [kitura-nio](https://kitura.io) (2.7) | 28.36 ms | 20.01 ms | 38.48 ms | 253.91 ms | 1505.48 ms | 61595.67 | 
| swift (5.0) | [kitura](https://kitura.io) (2.7) | 25.19 ms | 20.39 ms | 30.13 ms | 131.37 ms | 1195.70 ms | 53497.67 | 
| python (3.7) | [flask](https://flask.pocoo.org) (1.0) | 28.75 ms | 22.07 ms | 52.25 ms | 87.45 ms | 321.79 ms | 17517.00 | 
| python (3.7) | [molten](https://moltenframework.com) (0.27) | 31.46 ms | 23.17 ms | 55.86 ms | 103.22 ms | 411.15 ms | 23170.33 | 
| python (3.7) | [bocadillo](https://bocadilloproject.github.io) (0.16) | 33.08 ms | 27.45 ms | 57.24 ms | 95.50 ms | 150.55 ms | 18289.67 | 
| crystal (0.28) | [lucky](https://luckyframework.org) (0.14) | 29.94 ms | 27.87 ms | 40.72 ms | 51.44 ms | 196.20 ms | 8111.67 | 
| node (12.4) | [turbo_polka](https://github.com/mafintosh/turbo-http) (2.0) | 33.90 ms | 32.43 ms | 40.61 ms | 48.83 ms | 217.92 ms | 7525.67 | 
| crystal (0.29) | [athena](https://github.com/blacksmoke16/athena) (0.6) | 43.66 ms | 33.80 ms | 113.65 ms | 226.86 ms | 542.11 ms | 53147.67 | 
| python (3.7) | [sanic](https://github.com/huge-success/sanic) (19.3) | 42.57 ms | 35.93 ms | 78.98 ms | 144.49 ms | 316.57 ms | 29418.67 | 
| python (3.7) | [quart](https://pgjones.gitlab.io/quart) (0.9) | 53.66 ms | 50.28 ms | 99.99 ms | 158.79 ms | 213.49 ms | 33567.33 | 
| python (3.7) | [responder](https://python-responder.org) (1.3) | 67.95 ms | 62.65 ms | 117.82 ms | 167.17 ms | 244.33 ms | 37051.67 | 
| python (3.7) | [django](https://djangoproject.com) (2.2) | 85.09 ms | 66.44 ms | 164.28 ms | 263.01 ms | 564.21 ms | 52072.67 | 
| python (3.7) | [tornado](https://tornadoweb.org) (5.1) | 87.09 ms | 79.09 ms | 130.87 ms | 185.99 ms | 808.82 ms | 46936.33 | 
| python (3.7) | [masonite](https://masoniteproject.com) (2.2) | 101.97 ms | 85.17 ms | 161.46 ms | 292.28 ms | 851.06 ms | 56858.33 | 
| crystal (0.29) | [onyx](https://onyxframework.org) (0.5) | 113.14 ms | 105.10 ms | 154.86 ms | 353.98 ms | 480.26 ms | 53440.00 | 
| python (3.7) | [cyclone](https://cyclone.io) (0.0) | 355.61 ms | 268.56 ms | 380.81 ms | 3611.40 ms | 7088.23 ms | 591131.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (evhtp) (cpp)


:five: (fasthttprouter) (go)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](https://github.com/ohler55/agoo-c) (0.5) | 378756.67 | 219.10 MB |
| python (3.7) | [japronto](https://github.com/squeaky-pl/japronto) (0.1) | 368944.33 | 441.69 MB |
| rust (1.35) | [actix-web](https://actix.rs) (0.7) | 327769.67 | 372.28 MB |
| cpp (11) | [evhtp](https://criticalstack/libevhtp) (1.2) | 304343.00 | 295.00 MB |
| go (1.12) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 302726.00 | 488.19 MB |
| c (99) | [kore](https://kore.io) (3.1) | 289643.00 | 752.03 MB |
| python (3.6) | [vibora](https://vibora.io) (0.0) | 274191.33 | 311.10 MB |
| crystal (0.29) | [router.cr](https://github.com/tbrand/router.cr) (0.2) | 256993.00 | 241.52 MB |
| crystal (0.29) | [raze](https://razecr.com) (0.3) | 256450.33 | 240.82 MB |
| nim (0.2) | [jester](https://github.com/dom96/jester) (0.4) | 254162.67 | 510.57 MB |
| rust (1.35) | [gotham](https://gotham.rs) (0.3) | 252883.67 | 517.75 MB |
| java (8) | [act](https://actframework.org) (1.8) | 251596.00 | 433.97 MB |
| crystal (0.29) | [kemal](https://kemalcr.com) (0.25) | 244192.67 | 398.81 MB |
| crystal (0.29) | [spider-gazelle](https://spider-gazelle.net) (1.4) | 235877.33 | 250.96 MB |
| ruby (2.6) | [agoo](https://github.com/ohler55/agoo) (2.8) | 230396.33 | 133.34 MB |
| crystal (0.29) | [amber](https://amberframework.org) (0.28) | 219260.00 | 401.41 MB |
| crystal (0.29) | [orion](https://github.com/obsidian/orion) (1.7) | 199439.33 | 326.06 MB |
| node (12.4) | [0http](https://github.com/jkyberneees/0http) (1.0) | 185117.33 | 277.30 MB |
| node (12.4) | [restana](https://github.com/jkyberneees/ana) (3.1) | 180754.33 | 270.97 MB |
| go (1.12) | [chi](https://github.com/go-chi/chi) (4.0) | 177490.33 | 237.71 MB |
| csharp (7.3) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (2.2) | 173511.67 | 282.28 MB |
| go (1.12) | [kami](https://github.com/guregu/kami) (2.2) | 172814.00 | 228.94 MB |
| go (1.12) | [gorouter](https://github.com/vardius/gorouter/wiki) (4.0) | 171729.67 | 228.07 MB |
| go (1.12) | [echo](https://echo.labstack.com) (4.1) | 169420.33 | 296.67 MB |
| go (1.12) | [violetear](https://violetear.org) (7.0) | 168213.67 | 222.74 MB |
| rust (1.35) | [iron](https://ironframework.io) (0.6) | 166957.67 | 210.47 MB |
| go (1.12) | [gin](https://gin-gonic.com) (1.4) | 165846.33 | 290.62 MB |
| go (1.12) | [beego](https://beego.me) (1.12) | 164186.00 | 220.60 MB |
| go (1.12) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 162918.00 | 217.10 MB |
| kotlin (1.3) | [ktor](https://ktor.io) (1.2) | 161458.67 | 251.54 MB |
| node (12.4) | [rayo](https://rayo.js.org) (1.3) | 161139.33 | 241.35 MB |
| node (12.4) | [polka](https://github.com/lukeed/polka) (0.5) | 157929.00 | 236.77 MB |
| python (3.7) | [falcon](https://falconframework.org) (2.0) | 135964.00 | 318.91 MB |
| node (12.4) | [fastify](https://fastify.io) (2.4) | 135865.67 | 341.29 MB |
| node (12.4) | [muneem](https://github.com/node-muneem/muneem) (2.4) | 133444.67 | 199.98 MB |
| go (1.12) | [gf](https://goframe.org) (1.6) | 131129.33 | 198.23 MB |
| node (12.4) | [foxify](https://foxify.js.org) (0.1) | 126425.00 | 265.84 MB |
| node (12.4) | [koa](https://koajs.com) (2.7) | 117501.67 | 248.76 MB |
| java (8) | [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 116657.00 | 82.47 MB |
| python (3.7) | [bottle](https://bottlepy.org) (0.12) | 115772.67 | 285.32 MB |
| swift (5.0) | [perfect](https://perfect.org) (3.1) | 109663.33 | 103.02 MB |
| node (12.4) | [restify](https://restify.com) (8.2) | 103941.00 | 182.12 MB |
| node (12.4) | [express](https://expressjs.com) (4.16) | 95346.33 | 233.36 MB |
| python (3.7) | [hug](https://hug.rest) (2.5) | 85102.67 | 210.92 MB |
| python (3.7) | [starlette](https://starlette.io) (0.12) | 85049.00 | 182.81 MB |
| scala (2.12) | [akkahttp](https://akka.io) (10.1) | 81657.67 | 175.24 MB |
| rust (1.35) | [nickel](https://nickel-org.github.io) (0.11) | 81421.67 | 161.71 MB |
| swift (5.0) | [vapor](https://vapor.codes) (3.3) | 77800.33 | 129.93 MB |
| php (7.3) | [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 63587.33 | 315.10 MB |
| php (7.3) | [slim](https://slimframework.com) (3.12) | 63510.33 | 314.63 MB |
| rust (nightly) | [rocket](https://rocket.rs) (0.4) | 62745.33 | 99.02 MB |
| php (7.3) | [symfony](https://symfony.com) (4.3) | 61835.33 | 306.42 MB |
| php (7.3) | [zend-framework](https://framework.zend.com) (3.1) | 59460.67 | 294.94 MB |
| php (7.3) | [lumen](https://lumen.laravel.com) (5.8) | 58963.67 | 305.79 MB |
| python (3.7) | [fastapi](https://fastapi.tiangolo.com) (0.29) | 58146.33 | 125.84 MB |
| node (12.4) | [hapi](https://hapijs.com) (18.1) | 54743.33 | 140.70 MB |
| php (7.3) | [laravel](https://laravel.com) (5.8) | 51111.33 | 265.94 MB |
| python (3.7) | [aiohttp](https://aiohttp.readthedocs.io) (3.5) | 49719.67 | 112.71 MB |
| swift (5.0) | [kitura](https://kitura.io) (2.7) | 46533.67 | 86.29 MB |
| scala (2.12) | [http4s](https://http4s.org) (0.18) | 45882.33 | 80.45 MB |
| swift (5.0) | [kitura-nio](https://kitura.io) (2.7) | 42750.67 | 80.56 MB |
| ruby (2.6) | [roda](https://roda.jeremyevans.net) (3.21) | 42028.00 | 40.05 MB |
| ruby (2.6) | [cuba](https://cuba.is) (3.9) | 38209.67 | 45.06 MB |
| python (3.7) | [flask](https://flask.pocoo.org) (1.0) | 36492.67 | 89.80 MB |
| crystal (0.29) | [athena](https://github.com/blacksmoke16/athena) (0.6) | 35462.00 | 44.41 MB |
| python (3.7) | [molten](https://moltenframework.com) (0.27) | 34449.33 | 63.96 MB |
| crystal (0.28) | [lucky](https://luckyframework.org) (0.14) | 33175.33 | 40.77 MB |
| python (3.7) | [bocadillo](https://bocadilloproject.github.io) (0.16) | 31545.67 | 60.99 MB |
| ruby (2.6) | [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 31330.00 | 18.08 MB |
| node (12.4) | [turbo_polka](https://github.com/mafintosh/turbo-http) (2.0) | 29140.00 | 27.37 MB |
| python (3.7) | [sanic](https://github.com/huge-success/sanic) (19.3) | 25092.67 | 44.66 MB |
| ruby (2.6) | [flame](https://github.com/AlexWayfer/flame) (4.18) | 20254.00 | 11.68 MB |
| python (3.7) | [quart](https://pgjones.gitlab.io/quart) (0.9) | 19287.33 | 38.45 MB |
| ruby (2.6) | [hanami](https://hanamirb.org) (1.3) | 17136.67 | 129.65 MB |
| ruby (2.6) | [sinatra](https://sinatrarb.com) (2.0) | 15384.67 | 39.87 MB |
| python (3.7) | [responder](https://python-responder.org) (1.3) | 14836.33 | 32.31 MB |
| python (3.7) | [django](https://djangoproject.com) (2.2) | 12063.00 | 34.96 MB |
| python (3.7) | [tornado](https://tornadoweb.org) (5.1) | 11314.00 | 33.67 MB |
| python (3.7) | [masonite](https://masoniteproject.com) (2.2) | 10010.67 | 22.50 MB |
| crystal (0.29) | [onyx](https://onyxframework.org) (0.5) | 9109.67 | 23.40 MB |
| ruby (2.6) | [rails](https://rubyonrails.org) (5.2) | 3879.33 | 11.89 MB |
| python (3.7) | [cyclone](https://cyclone.io) (0.0) | 2645.33 | 7.23 MB |
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

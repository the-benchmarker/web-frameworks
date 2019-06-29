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
Last update: 2019-06-28
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


:five: laravel (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.35) | [nickel](https://nickel-org.github.io) (0.11) | 0.10 ms | 0.10 ms | 0.14 ms | 0.18 ms | 4.08 ms | 46.67 | 
| ruby (2.6) | [roda](https://roda.jeremyevans.net) (3.21) | 3.59 ms | 0.19 ms | 13.21 ms | 32.95 ms | 82.04 ms | 7280.67 | 
| ruby (2.6) | [cuba](https://cuba.is) (3.9) | 3.93 ms | 0.20 ms | 14.61 ms | 35.01 ms | 88.01 ms | 7805.33 | 
| ruby (2.6) | [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 4.79 ms | 0.24 ms | 17.62 ms | 40.18 ms | 101.39 ms | 9157.67 | 
| php (7.3) | [laravel](https://laravel.com) (5.8) | 105.47 ms | 0.36 ms | 291.45 ms | 1774.41 ms | 6871.27 ms | 369601.00 | 
| php (7.3) | [symfony](https://symfony.com) (4.3) | 128.16 ms | 0.37 ms | 239.96 ms | 2907.38 ms | 6908.92 ms | 503985.67 | 
| ruby (2.6) | [flame](https://github.com/AlexWayfer/flame) (4.18) | 6.91 ms | 0.37 ms | 24.08 ms | 51.93 ms | 121.92 ms | 12080.00 | 
| php (7.3) | [slim](https://slimframework.com) (3.12) | 173.74 ms | 0.38 ms | 305.31 ms | 3725.92 ms | 6835.48 ms | 623015.00 | 
| php (7.3) | [lumen](https://lumen.laravel.com) (5.8) | 172.36 ms | 0.39 ms | 275.18 ms | 3983.66 ms | 6838.47 ms | 658906.67 | 
| ruby (2.6) | [hanami](https://hanamirb.org) (1.3) | 8.00 ms | 0.39 ms | 28.34 ms | 60.90 ms | 167.45 ms | 14285.67 | 
| rust (1.35) | [iron](https://ironframework.io) (0.6) | 0.42 ms | 0.40 ms | 0.65 ms | 1.05 ms | 14.47 ms | 240.33 | 
| ruby (2.6) | [sinatra](https://sinatrarb.com) (2.0) | 9.77 ms | 0.55 ms | 32.07 ms | 66.23 ms | 150.42 ms | 15686.00 | 
| php (7.3) | [zend-framework](https://framework.zend.com) (3.1) | 102.31 ms | 0.66 ms | 274.33 ms | 1710.53 ms | 6824.35 ms | 375656.00 | 
| php (7.3) | [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 121.39 ms | 0.89 ms | 244.83 ms | 2735.86 ms | 5830.37 ms | 457058.00 | 
| rust (nightly) | [rocket](https://rocket.rs) (0.4) | 57.27 ms | 1.59 ms | 3.83 ms | 1957.34 ms | 4939.84 ms | 342107.67 | 
| c (11) | [agoo-c](https://github.com/ohler55/agoo-c) (0.5) | 3.37 ms | 2.04 ms | 7.79 ms | 16.78 ms | 64.50 ms | 3680.67 | 
| rust (1.35) | [actix-web](https://actix.rs) (0.7) | 3.69 ms | 2.78 ms | 7.99 ms | 16.65 ms | 34.99 ms | 3490.00 | 
| python (3.7) | [japronto](https://github.com/squeaky-pl/japronto) (0.1) | 3.53 ms | 3.00 ms | 6.74 ms | 14.59 ms | 32.86 ms | 3060.00 | 
| java (8) | [rapidoid](https://rapidoid.org) (5.5) | 3.77 ms | 3.06 ms | 7.47 ms | 15.78 ms | 143.84 ms | 3665.67 | 
| c (99) | [kore](https://kore.io) (3.1) | 7.58 ms | 3.14 ms | 7.82 ms | 99.39 ms | 1012.85 ms | 40607.33 | 
| go (1.12) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.75 ms | 3.19 ms | 5.92 ms | 11.88 ms | 50.56 ms | 2402.67 | 
| ruby (2.6) | [rails](https://rubyonrails.org) (5.2) | 38.04 ms | 3.66 ms | 126.67 ms | 333.17 ms | 844.90 ms | 71196.67 | 
| cpp (11) | [evhtp](https://criticalstack/libevhtp) (1.2) | 3.75 ms | 4.10 ms | 5.50 ms | 8.11 ms | 19.74 ms | 1808.33 | 
| python (3.6) | [vibora](https://vibora.io) (0.0) | 5.02 ms | 4.11 ms | 10.55 ms | 17.84 ms | 38.80 ms | 3987.67 | 
| crystal (0.29) | [router.cr](https://github.com/tbrand/router.cr) (0.2) | 4.33 ms | 4.19 ms | 7.16 ms | 13.77 ms | 29.54 ms | 2606.00 | 
| rust (1.35) | [gotham](https://gotham.rs) (0.3) | 4.78 ms | 4.28 ms | 9.29 ms | 19.61 ms | 75.86 ms | 3873.33 | 
| crystal (0.29) | [spider-gazelle](https://spider-gazelle.net) (1.4) | 4.79 ms | 4.30 ms | 8.63 ms | 15.44 ms | 30.08 ms | 3029.67 | 
| crystal (0.29) | [raze](https://razecr.com) (0.3) | 4.49 ms | 4.38 ms | 7.22 ms | 13.68 ms | 30.02 ms | 2535.67 | 
| crystal (0.29) | [kemal](https://kemalcr.com) (0.25) | 4.74 ms | 4.56 ms | 7.63 ms | 14.81 ms | 29.16 ms | 2685.67 | 
| ruby (2.6) | [agoo](https://github.com/ohler55/agoo) (2.8) | 7.05 ms | 4.63 ms | 12.43 ms | 66.61 ms | 118.60 ms | 10441.33 | 
| crystal (0.29) | [amber](https://amberframework.org) (0.28) | 5.01 ms | 4.73 ms | 7.87 ms | 14.08 ms | 34.33 ms | 2517.00 | 
| crystal (0.29) | [orion](https://github.com/obsidian/orion) (1.7) | 5.42 ms | 4.91 ms | 8.95 ms | 14.94 ms | 39.72 ms | 2801.00 | 
| nim (0.2) | [jester](https://github.com/dom96/jester) (0.4) | 5.42 ms | 4.95 ms | 9.51 ms | 17.27 ms | 62.89 ms | 3445.67 | 
| go (1.12) | [chi](https://github.com/go-chi/chi) (4.0) | 7.46 ms | 5.01 ms | 14.40 ms | 31.23 ms | 491.93 ms | 14003.67 | 
| go (1.12) | [gorouter](https://github.com/vardius/gorouter/wiki) (4.0) | 7.36 ms | 5.06 ms | 15.95 ms | 33.62 ms | 126.71 ms | 7245.33 | 
| go (1.12) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 8.06 ms | 5.15 ms | 17.58 ms | 36.63 ms | 309.01 ms | 10251.00 | 
| go (1.12) | [echo](https://echo.labstack.com) (4.1) | 7.39 ms | 5.17 ms | 15.78 ms | 31.73 ms | 136.23 ms | 6582.33 | 
| node (12.4) | [0http](https://github.com/jkyberneees/0http) (1.0) | 6.78 ms | 5.19 ms | 9.91 ms | 19.40 ms | 313.01 ms | 9674.67 | 
| go (1.12) | [gin](https://gin-gonic.com) (1.4) | 7.55 ms | 5.23 ms | 16.11 ms | 32.65 ms | 171.09 ms | 7134.67 | 
| go (1.12) | [kami](https://github.com/guregu/kami) (2.2) | 7.03 ms | 5.32 ms | 13.21 ms | 28.51 ms | 157.25 ms | 5689.67 | 
| go (1.12) | [beego](https://beego.me) (1.12) | 7.76 ms | 5.46 ms | 16.24 ms | 33.57 ms | 86.77 ms | 6738.67 | 
| go (1.12) | [violetear](https://violetear.org) (7.0) | 7.29 ms | 5.46 ms | 14.08 ms | 29.75 ms | 122.35 ms | 6504.67 | 
| node (12.4) | [restana](https://github.com/jkyberneees/ana) (3.1) | 7.10 ms | 5.56 ms | 10.41 ms | 21.69 ms | 294.97 ms | 9287.33 | 
| java (8) | [act](https://actframework.org) (1.8) | 6.43 ms | 5.66 ms | 10.44 ms | 20.45 ms | 116.49 ms | 4591.00 | 
| csharp (7.3) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (2.2) | 6.71 ms | 5.87 ms | 9.45 ms | 16.83 ms | 292.63 ms | 8375.67 | 
| node (12.4) | [polka](https://github.com/lukeed/polka) (0.5) | 8.46 ms | 6.05 ms | 11.47 ms | 36.74 ms | 440.78 ms | 17377.00 | 
| node (12.4) | [rayo](https://rayo.js.org) (1.3) | 8.15 ms | 6.31 ms | 11.85 ms | 25.37 ms | 372.99 ms | 13190.00 | 
| go (1.12) | [gf](https://goframe.org) (1.6) | 10.19 ms | 6.50 ms | 20.50 ms | 47.92 ms | 465.77 ms | 16883.00 | 
| kotlin (1.3) | [ktor](https://ktor.io) (1.2) | 7.97 ms | 6.59 ms | 14.83 ms | 29.82 ms | 216.99 ms | 8433.67 | 
| node (12.4) | [muneem](https://github.com/node-muneem/muneem) (2.4) | 11.34 ms | 7.46 ms | 15.37 ms | 101.40 ms | 608.87 ms | 28164.33 | 
| node (12.4) | [fastify](https://fastify.io) (2.4) | 10.98 ms | 8.24 ms | 15.00 ms | 64.92 ms | 477.18 ms | 19270.67 | 
| python (3.7) | [falcon](https://falconframework.org) (2.0) | 9.34 ms | 8.45 ms | 16.67 ms | 30.79 ms | 210.40 ms | 6625.00 | 
| node (12.4) | [foxify](https://foxify.js.org) (0.1) | 10.57 ms | 8.57 ms | 14.30 ms | 56.71 ms | 490.68 ms | 20334.33 | 
| scala (2.12) | [akkahttp](https://akka.io) (10.1) | 202.93 ms | 8.70 ms | 70.24 ms | 4417.78 ms | 7267.96 ms | 777240.33 | 
| node (12.4) | [koa](https://koajs.com) (2.7) | 11.29 ms | 8.89 ms | 15.11 ms | 63.16 ms | 541.04 ms | 23112.67 | 
| node (12.4) | [express](https://expressjs.com) (4.16) | 12.43 ms | 10.00 ms | 17.35 ms | 36.72 ms | 497.24 ms | 19140.00 | 
| node (12.4) | [restify](https://restify.com) (8.2) | 11.89 ms | 10.39 ms | 15.33 ms | 37.28 ms | 399.05 ms | 13836.67 | 
| swift (5.0) | [perfect](https://perfect.org) (3.1) | 11.00 ms | 10.95 ms | 13.43 ms | 15.57 ms | 26.07 ms | 1969.67 | 
| python (3.7) | [bottle](https://bottlepy.org) (0.12) | 14.34 ms | 10.97 ms | 28.27 ms | 49.62 ms | 215.42 ms | 10991.33 | 
| java (8) | [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 14.27 ms | 11.57 ms | 22.29 ms | 58.63 ms | 324.33 ms | 12523.00 | 
| scala (2.12) | [http4s](https://http4s.org) (0.18) | 16.22 ms | 12.41 ms | 25.60 ms | 61.39 ms | 799.52 ms | 35537.00 | 
| python (3.7) | [hug](https://hug.rest) (2.5) | 17.56 ms | 13.06 ms | 36.31 ms | 66.47 ms | 223.64 ms | 14629.33 | 
| swift (5.0) | [vapor](https://vapor.codes) (3.3) | 23.06 ms | 14.08 ms | 21.99 ms | 342.30 ms | 1391.48 ms | 75321.67 | 
| python (3.7) | [starlette](https://starlette.io) (0.12) | 19.33 ms | 14.71 ms | 40.58 ms | 71.54 ms | 152.13 ms | 15305.33 | 
| node (12.4) | [hapi](https://hapijs.com) (18.1) | 30.05 ms | 20.14 ms | 33.60 ms | 330.99 ms | 1085.99 ms | 63980.33 | 
| swift (5.0) | [kitura](https://kitura.io) (2.7) | 24.58 ms | 22.68 ms | 31.71 ms | 48.92 ms | 746.90 ms | 23397.33 | 
| python (3.7) | [fastapi](https://fastapi.tiangolo.com) (0.31) | 29.59 ms | 25.80 ms | 52.54 ms | 91.78 ms | 262.42 ms | 18290.00 | 
| swift (5.0) | [kitura-nio](https://kitura.io) (2.7) | 45.63 ms | 26.34 ms | 50.36 ms | 761.36 ms | 2157.78 ms | 135070.67 | 
| python (3.7) | [aiohttp](https://aiohttp.readthedocs.io) (3.5) | 28.06 ms | 26.55 ms | 49.29 ms | 72.07 ms | 109.95 ms | 15111.67 | 
| python (3.7) | [molten](https://moltenframework.com) (0.27) | 41.10 ms | 32.62 ms | 78.61 ms | 128.67 ms | 420.37 ms | 27800.67 | 
| crystal (0.28) | [lucky](https://luckyframework.org) (0.14) | 34.65 ms | 33.12 ms | 42.47 ms | 48.13 ms | 247.69 ms | 7289.00 | 
| crystal (0.29) | [athena](https://github.com/blacksmoke16/athena) (0.7) | 232.18 ms | 33.61 ms | 867.04 ms | 2242.31 ms | 3475.65 ms | 480969.33 | 
| python (3.7) | [flask](https://flask.pocoo.org) (1.0) | 39.21 ms | 34.33 ms | 68.18 ms | 114.90 ms | 513.69 ms | 25527.67 | 
| node (12.4) | [turbo_polka](https://github.com/mafintosh/turbo-http) (2.0) | 37.70 ms | 36.65 ms | 45.01 ms | 53.86 ms | 440.48 ms | 15106.33 | 
| python (3.7) | [bocadillo](https://bocadilloproject.github.io) (0.17) | 43.18 ms | 40.63 ms | 71.43 ms | 104.50 ms | 233.26 ms | 20097.33 | 
| python (3.7) | [sanic](https://github.com/huge-success/sanic) (19.6) | 55.97 ms | 46.04 ms | 109.75 ms | 193.54 ms | 326.71 ms | 39944.33 | 
| python (3.7) | [quart](https://pgjones.gitlab.io/quart) (0.9) | 67.44 ms | 63.76 ms | 110.50 ms | 151.26 ms | 220.30 ms | 34020.33 | 
| python (3.7) | [responder](https://python-responder.org) (1.3) | 85.36 ms | 80.55 ms | 149.43 ms | 216.09 ms | 292.39 ms | 47740.67 | 
| python (3.7) | [tornado](https://tornadoweb.org) (5.1) | 108.55 ms | 99.83 ms | 163.27 ms | 213.31 ms | 1049.78 ms | 52401.33 | 
| crystal (0.29) | [onyx](https://onyxframework.org) (0.5) | 110.77 ms | 107.13 ms | 156.08 ms | 203.06 ms | 281.82 ms | 34028.00 | 
| python (3.7) | [django](https://djangoproject.com) (2.2) | 114.96 ms | 109.77 ms | 161.79 ms | 443.94 ms | 1213.70 ms | 80364.00 | 
| python (3.7) | [masonite](https://masoniteproject.com) (2.2) | 136.16 ms | 117.54 ms | 193.40 ms | 665.73 ms | 1837.40 ms | 121446.00 | 
| python (3.7) | [cyclone](https://cyclone.io) (0.0) | 377.92 ms | 293.65 ms | 384.91 ms | 4160.46 ms | 7078.62 ms | 658982.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (rapidoid) (java)


:five: (fasthttprouter) (go)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](https://github.com/ohler55/agoo-c) (0.5) | 331197.67 | 191.47 MB |
| python (3.7) | [japronto](https://github.com/squeaky-pl/japronto) (0.1) | 280697.67 | 336.07 MB |
| rust (1.35) | [actix-web](https://actix.rs) (0.7) | 278434.67 | 316.61 MB |
| java (8) | [rapidoid](https://rapidoid.org) (5.5) | 263196.67 | 473.27 MB |
| go (1.12) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 247006.00 | 397.89 MB |
| c (99) | [kore](https://kore.io) (3.1) | 242035.33 | 628.61 MB |
| cpp (11) | [evhtp](https://criticalstack/libevhtp) (1.2) | 236114.33 | 229.23 MB |
| crystal (0.29) | [router.cr](https://github.com/tbrand/router.cr) (0.2) | 217994.33 | 204.97 MB |
| python (3.6) | [vibora](https://vibora.io) (0.0) | 216692.33 | 245.99 MB |
| rust (1.35) | [gotham](https://gotham.rs) (0.3) | 211466.67 | 432.66 MB |
| crystal (0.29) | [raze](https://razecr.com) (0.3) | 209498.00 | 197.05 MB |
| java (8) | [act](https://actframework.org) (1.8) | 207461.67 | 358.11 MB |
| crystal (0.29) | [spider-gazelle](https://spider-gazelle.net) (1.4) | 205223.33 | 217.77 MB |
| crystal (0.29) | [kemal](https://kemalcr.com) (0.25) | 198916.67 | 325.24 MB |
| nim (0.2) | [jester](https://github.com/dom96/jester) (0.4) | 197717.67 | 397.34 MB |
| crystal (0.29) | [amber](https://amberframework.org) (0.28) | 187563.33 | 343.62 MB |
| ruby (2.6) | [agoo](https://github.com/ohler55/agoo) (2.8) | 179513.00 | 103.91 MB |
| crystal (0.29) | [orion](https://github.com/obsidian/orion) (1.7) | 174787.67 | 285.79 MB |
| go (1.12) | [chi](https://github.com/go-chi/chi) (4.0) | 154078.00 | 206.00 MB |
| go (1.12) | [gorouter](https://github.com/vardius/gorouter/wiki) (4.0) | 151288.67 | 201.39 MB |
| node (12.4) | [0http](https://github.com/jkyberneees/0http) (1.0) | 150085.67 | 224.96 MB |
| go (1.12) | [kami](https://github.com/guregu/kami) (2.2) | 148396.67 | 196.29 MB |
| go (1.12) | [echo](https://echo.labstack.com) (4.1) | 147917.00 | 259.38 MB |
| node (12.4) | [restana](https://github.com/jkyberneees/ana) (3.1) | 146727.67 | 219.80 MB |
| go (1.12) | [gin](https://gin-gonic.com) (1.4) | 145708.00 | 255.43 MB |
| go (1.12) | [violetear](https://violetear.org) (7.0) | 145431.67 | 192.80 MB |
| csharp (7.3) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (2.2) | 144175.00 | 234.81 MB |
| go (1.12) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 142804.67 | 190.36 MB |
| go (1.12) | [beego](https://beego.me) (1.12) | 142726.67 | 191.71 MB |
| rust (1.35) | [iron](https://ironframework.io) (0.6) | 140999.33 | 178.18 MB |
| node (12.4) | [polka](https://github.com/lukeed/polka) (0.5) | 136684.33 | 204.83 MB |
| kotlin (1.3) | [ktor](https://ktor.io) (1.2) | 132990.33 | 207.29 MB |
| node (12.4) | [rayo](https://rayo.js.org) (1.3) | 132793.00 | 199.05 MB |
| go (1.12) | [gf](https://goframe.org) (1.6) | 118678.67 | 180.03 MB |
| node (12.4) | [muneem](https://github.com/node-muneem/muneem) (2.4) | 116643.33 | 174.84 MB |
| node (12.4) | [fastify](https://fastify.io) (2.4) | 111481.33 | 285.04 MB |
| node (12.4) | [foxify](https://foxify.js.org) (0.1) | 109565.00 | 230.43 MB |
| python (3.7) | [falcon](https://falconframework.org) (2.0) | 108442.33 | 254.27 MB |
| node (12.4) | [koa](https://koajs.com) (2.7) | 103925.67 | 220.05 MB |
| swift (5.0) | [perfect](https://perfect.org) (3.1) | 90023.67 | 84.65 MB |
| node (12.4) | [restify](https://restify.com) (8.2) | 89298.33 | 156.69 MB |
| node (12.4) | [express](https://expressjs.com) (4.16) | 88357.33 | 216.34 MB |
| java (8) | [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 86699.33 | 63.69 MB |
| rust (1.35) | [nickel](https://nickel-org.github.io) (0.11) | 80940.00 | 160.34 MB |
| python (3.7) | [bottle](https://bottlepy.org) (0.12) | 74916.67 | 184.66 MB |
| scala (2.12) | [http4s](https://http4s.org) (0.18) | 74641.67 | 130.63 MB |
| scala (2.12) | [akkahttp](https://akka.io) (10.1) | 68282.33 | 146.67 MB |
| swift (5.0) | [vapor](https://vapor.codes) (3.3) | 65391.67 | 109.42 MB |
| python (3.7) | [hug](https://hug.rest) (2.5) | 64617.33 | 160.33 MB |
| php (7.3) | [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 62818.00 | 312.07 MB |
| php (7.3) | [slim](https://slimframework.com) (3.12) | 60902.33 | 302.01 MB |
| php (7.3) | [symfony](https://symfony.com) (4.3) | 58619.33 | 290.59 MB |
| python (3.7) | [starlette](https://starlette.io) (0.12) | 58261.00 | 125.57 MB |
| php (7.3) | [zend-framework](https://framework.zend.com) (3.1) | 57941.67 | 287.35 MB |
| rust (nightly) | [rocket](https://rocket.rs) (0.4) | 55797.00 | 88.73 MB |
| php (7.3) | [lumen](https://lumen.laravel.com) (5.8) | 55675.33 | 288.85 MB |
| node (12.4) | [hapi](https://hapijs.com) (18.1) | 46051.67 | 119.09 MB |
| php (7.3) | [laravel](https://laravel.com) (5.8) | 45731.00 | 238.39 MB |
| swift (5.0) | [kitura](https://kitura.io) (2.7) | 41504.33 | 76.99 MB |
| python (3.7) | [aiohttp](https://aiohttp.readthedocs.io) (3.5) | 36182.00 | 81.98 MB |
| ruby (2.6) | [roda](https://roda.jeremyevans.net) (3.21) | 35845.00 | 34.15 MB |
| python (3.7) | [fastapi](https://fastapi.tiangolo.com) (0.31) | 35159.00 | 75.84 MB |
| swift (5.0) | [kitura-nio](https://kitura.io) (2.7) | 33739.67 | 63.54 MB |
| ruby (2.6) | [cuba](https://cuba.is) (3.9) | 32721.67 | 38.54 MB |
| crystal (0.29) | [athena](https://github.com/blacksmoke16/athena) (0.7) | 29118.67 | 36.53 MB |
| crystal (0.28) | [lucky](https://luckyframework.org) (0.14) | 28404.33 | 35.05 MB |
| ruby (2.6) | [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 26749.33 | 15.42 MB |
| node (12.4) | [turbo_polka](https://github.com/mafintosh/turbo-http) (2.0) | 26511.67 | 24.88 MB |
| python (3.7) | [molten](https://moltenframework.com) (0.27) | 26257.00 | 48.86 MB |
| python (3.7) | [flask](https://flask.pocoo.org) (1.0) | 25943.33 | 63.93 MB |
| python (3.7) | [bocadillo](https://bocadilloproject.github.io) (0.17) | 23681.67 | 45.84 MB |
| python (3.7) | [sanic](https://github.com/huge-success/sanic) (19.6) | 19294.33 | 34.40 MB |
| ruby (2.6) | [flame](https://github.com/AlexWayfer/flame) (4.18) | 18535.67 | 10.69 MB |
| ruby (2.6) | [hanami](https://hanamirb.org) (1.3) | 15990.67 | 120.92 MB |
| python (3.7) | [quart](https://pgjones.gitlab.io/quart) (0.9) | 14847.00 | 29.58 MB |
| ruby (2.6) | [sinatra](https://sinatrarb.com) (2.0) | 13102.67 | 33.96 MB |
| python (3.7) | [responder](https://python-responder.org) (1.3) | 11888.33 | 25.92 MB |
| python (3.7) | [tornado](https://tornadoweb.org) (5.1) | 9033.67 | 26.58 MB |
| crystal (0.29) | [onyx](https://onyxframework.org) (0.5) | 8966.67 | 23.04 MB |
| python (3.7) | [django](https://djangoproject.com) (2.2) | 8842.67 | 25.63 MB |
| python (3.7) | [masonite](https://masoniteproject.com) (2.2) | 7621.33 | 18.76 MB |
| ruby (2.6) | [rails](https://rubyonrails.org) (5.2) | 3361.33 | 10.28 MB |
| python (3.7) | [cyclone](https://cyclone.io) (0.0) | 1694.00 | 4.60 MB |
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

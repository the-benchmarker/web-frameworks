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
Last update: 2019-06-13
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
| rust (1.35) | [nickel](https://nickel-org.github.io) (0.11) | 0.07 ms | 0.07 ms | 0.09 ms | 0.12 ms | 1.03 ms | 21.33 | 
| ruby (2.6) | [roda](https://roda.jeremyevans.net) (3.2) | 2.83 ms | 0.15 ms | 10.69 ms | 28.35 ms | 75.81 ms | 6164.33 | 
| ruby (2.6) | [cuba](https://cuba.is) (3.9) | 3.06 ms | 0.16 ms | 11.46 ms | 29.19 ms | 80.28 ms | 6419.33 | 
| ruby (2.6) | [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 3.79 ms | 0.19 ms | 14.22 ms | 32.64 ms | 80.92 ms | 7413.33 | 
| rust (1.35) | [iron](https://ironframework.io) (0.6) | 0.32 ms | 0.31 ms | 0.53 ms | 0.84 ms | 12.25 ms | 192.00 | 
| ruby (2.6) | [flame](https://github.com/AlexWayfer/flame) (4.18) | 5.57 ms | 0.31 ms | 19.68 ms | 42.93 ms | 98.03 ms | 9951.00 | 
| ruby (2.6) | [hanami](https://hanamirb.org) (1.3) | 6.56 ms | 0.33 ms | 23.86 ms | 50.37 ms | 123.70 ms | 11912.00 | 
| php (7.3) | [zend-framework](https://framework.zend.com) (3.1) | 106.92 ms | 0.33 ms | 226.42 ms | 2356.15 ms | 6808.61 ms | 412945.67 | 
| php (7.3) | [symfony](https://symfony.com) (4.3) | 127.89 ms | 0.34 ms | 207.75 ms | 3038.97 ms | 6800.65 ms | 536383.00 | 
| php (7.3) | [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 115.80 ms | 0.34 ms | 221.20 ms | 2664.33 ms | 6791.07 ms | 453680.67 | 
| php (7.3) | [laravel](https://laravel.com) (5.8) | 138.28 ms | 0.34 ms | 292.07 ms | 2847.95 ms | 6856.98 ms | 495701.33 | 
| php (7.3) | [slim](https://slimframework.com) (3.12) | 141.35 ms | 0.35 ms | 236.82 ms | 3218.81 ms | 6778.63 ms | 535888.33 | 
| ruby (2.6) | [sinatra](https://sinatrarb.com) (2.0) | 7.69 ms | 0.42 ms | 26.45 ms | 55.05 ms | 125.86 ms | 13035.67 | 
| php (7.3) | [lumen](https://lumen.laravel.com) (5.8) | 152.17 ms | 0.89 ms | 285.83 ms | 3328.11 ms | 6966.13 ms | 562191.67 | 
| rust (nightly) | [rocket](https://rocket.rs) (0.4) | 80.60 ms | 1.47 ms | 32.43 ms | 1857.94 ms | 4989.82 ms | 387502.00 | 
| c (11) | [agoo-c](https://github.com/ohler55/agoo-c) (0.5) | 2.64 ms | 1.67 ms | 5.85 ms | 12.95 ms | 31.36 ms | 2774.67 | 
| python (3.7) | [japronto](https://github.com/squeaky-pl/japronto) (0.1) | 2.98 ms | 1.86 ms | 6.75 ms | 15.93 ms | 33.77 ms | 3319.67 | 
| ruby (2.6) | [rails](https://rubyonrails.org) (5.2) | 29.80 ms | 2.08 ms | 100.95 ms | 271.84 ms | 707.13 ms | 57812.00 | 
| go (1.12) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 2.96 ms | 2.23 ms | 5.50 ms | 9.58 ms | 48.47 ms | 2105.67 | 
| rust (1.35) | [actix-web](https://actix.rs) (0.7) | 2.81 ms | 2.26 ms | 5.76 ms | 12.64 ms | 32.30 ms | 2577.33 | 
| c (99) | [kore](https://kore.io) (3.1) | 5.30 ms | 2.69 ms | 5.61 ms | 63.67 ms | 961.33 ms | 28866.00 | 
| cpp (11) | [evhtp](https://criticalstack/libevhtp) (1.2) | 2.82 ms | 2.79 ms | 4.79 ms | 6.75 ms | 85.60 ms | 2013.00 | 
| python (3.6) | [vibora](https://vibora.io) (0.0) | 3.35 ms | 2.85 ms | 6.89 ms | 12.68 ms | 44.62 ms | 2833.67 | 
| crystal (0.29) | [router.cr](https://github.com/tbrand/router.cr) (0.2) | 3.54 ms | 3.28 ms | 5.97 ms | 11.93 ms | 24.91 ms | 2301.33 | 
| ruby (2.6) | [agoo](https://github.com/ohler55/agoo) (2.8) | 6.65 ms | 3.31 ms | 9.55 ms | 85.38 ms | 118.84 ms | 13869.33 | 
| crystal (0.29) | [raze](https://razecr.com) (0.3) | 3.62 ms | 3.38 ms | 6.02 ms | 12.30 ms | 27.82 ms | 2321.67 | 
| rust (1.35) | [gotham](https://gotham.rs) (0.3) | 4.18 ms | 3.44 ms | 8.17 ms | 18.00 ms | 54.40 ms | 3581.00 | 
| nim (0.2) | [jester](https://github.com/dom96/jester) (0.4) | 4.15 ms | 3.52 ms | 6.84 ms | 12.54 ms | 30.38 ms | 2408.33 | 
| crystal (0.29) | [spider-gazelle](https://spider-gazelle.net) (1.4) | 4.01 ms | 3.53 ms | 7.29 ms | 13.16 ms | 26.29 ms | 2603.00 | 
| crystal (0.29) | [kemal](https://kemalcr.com) (0.25) | 3.81 ms | 3.64 ms | 6.24 ms | 12.34 ms | 32.13 ms | 2330.67 | 
| crystal (0.29) | [amber](https://amberframework.org) (0.28) | 4.34 ms | 4.37 ms | 6.65 ms | 12.03 ms | 30.64 ms | 2188.67 | 
| csharp (7.3) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (2.2) | 4.84 ms | 4.39 ms | 6.94 ms | 14.20 ms | 278.89 ms | 5955.00 | 
| node (12.4) | [0http](https://github.com/jkyberneees/0http) (1.0) | 5.03 ms | 4.47 ms | 7.56 ms | 15.13 ms | 261.82 ms | 7813.33 | 
| node (12.4) | [restana](https://github.com/jkyberneees/ana) (3.1) | 5.13 ms | 4.49 ms | 7.62 ms | 15.63 ms | 249.13 ms | 7638.00 | 
| crystal (0.29) | [orion](https://github.com/obsidian/orion) (1.7) | 4.62 ms | 4.50 ms | 7.43 ms | 12.53 ms | 30.32 ms | 2364.33 | 
| go (1.12) | [chi](https://github.com/go-chi/chi) (4.0) | 6.07 ms | 4.50 ms | 12.16 ms | 25.29 ms | 274.10 ms | 8099.00 | 
| go (1.12) | [gorouter](https://github.com/vardius/gorouter/wiki) (4.0) | 6.14 ms | 4.51 ms | 13.18 ms | 27.80 ms | 66.40 ms | 5616.33 | 
| go (1.12) | [gin](https://gin-gonic.com) (1.4) | 6.48 ms | 4.63 ms | 13.71 ms | 27.99 ms | 224.87 ms | 6935.67 | 
| go (1.12) | [beego](https://beego.me) (1.12) | 6.42 ms | 4.63 ms | 13.07 ms | 27.81 ms | 293.20 ms | 7630.67 | 
| go (1.12) | [echo](https://echo.labstack.com) (4.1) | 6.38 ms | 4.64 ms | 13.41 ms | 27.24 ms | 110.68 ms | 5959.00 | 
| node (12.4) | [rayo](https://rayo.js.org) (1.3) | 5.52 ms | 4.65 ms | 9.09 ms | 16.51 ms | 250.29 ms | 5673.33 | 
| go (1.12) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 6.85 ms | 4.68 ms | 14.90 ms | 30.75 ms | 186.76 ms | 7449.33 | 
| node (12.4) | [polka](https://github.com/lukeed/polka) (0.5) | 6.07 ms | 4.68 ms | 9.22 ms | 21.08 ms | 266.66 ms | 9812.67 | 
| go (1.12) | [violetear](https://violetear.org) (7.0) | 6.14 ms | 4.70 ms | 11.90 ms | 25.21 ms | 206.63 ms | 5252.33 | 
| go (1.12) | [kami](https://github.com/guregu/kami) (2.2) | 6.30 ms | 4.72 ms | 11.77 ms | 25.47 ms | 231.03 ms | 7173.33 | 
| python (3.7) | [falcon](https://falconframework.org) (2.0) | 6.48 ms | 4.74 ms | 10.59 ms | 24.22 ms | 226.43 ms | 8800.67 | 
| java (8) | [act](https://actframework.org) (1.8) | 5.60 ms | 4.82 ms | 9.06 ms | 20.04 ms | 113.90 ms | 4466.33 | 
| kotlin (1.3) | [ktor](https://ktor.io) (1.2) | 7.60 ms | 5.16 ms | 13.35 ms | 31.92 ms | 435.56 ms | 15945.00 | 
| go (1.12) | [gf](https://goframe.org) (1.6) | 8.30 ms | 5.51 ms | 17.63 ms | 38.76 ms | 186.72 ms | 8718.67 | 
| node (12.4) | [foxify](https://foxify.js.org) (0.1) | 7.18 ms | 5.81 ms | 10.42 ms | 18.73 ms | 261.33 ms | 7363.33 | 
| node (12.4) | [fastify](https://fastify.io) (2.4) | 7.65 ms | 6.13 ms | 11.54 ms | 20.60 ms | 319.01 ms | 9805.00 | 
| python (3.7) | [bottle](https://bottlepy.org) (0.12) | 8.26 ms | 6.13 ms | 15.54 ms | 30.15 ms | 156.50 ms | 5839.00 | 
| node (12.4) | [koa](https://koajs.com) (2.7) | 7.81 ms | 6.16 ms | 10.76 ms | 22.01 ms | 373.49 ms | 12369.33 | 
| node (12.4) | [muneem](https://github.com/node-muneem/muneem) (2.4) | 8.78 ms | 6.26 ms | 12.35 ms | 66.27 ms | 419.19 ms | 17950.67 | 
| scala (2.12) | [akkahttp](https://akka.io) (10.1) | 208.64 ms | 7.27 ms | 30.21 ms | 4962.04 ms | 7917.89 ms | 851751.00 | 
| node (12.4) | [restify](https://restify.com) (8.2) | 8.45 ms | 7.80 ms | 10.91 ms | 25.16 ms | 269.30 ms | 8069.00 | 
| swift (5.0) | [perfect](https://perfect.org) (3.1) | 8.00 ms | 7.99 ms | 9.93 ms | 11.73 ms | 24.97 ms | 1547.33 | 
| python (3.7) | [starlette](https://starlette.io) (0.12) | 10.35 ms | 8.35 ms | 18.99 ms | 30.56 ms | 118.98 ms | 6391.33 | 
| node (12.4) | [express](https://expressjs.com) (4.16) | 9.92 ms | 8.36 ms | 14.10 ms | 27.56 ms | 409.69 ms | 14297.00 | 
| python (3.7) | [hug](https://hug.rest) (2.5) | 26.35 ms | 8.82 ms | 32.63 ms | 351.67 ms | 545.09 ms | 62090.33 | 
| swift (5.0) | [vapor](https://vapor.codes) (3.3) | 12.46 ms | 9.21 ms | 17.15 ms | 42.03 ms | 769.90 ms | 27380.33 | 
| scala (2.12) | [http4s](https://http4s.org) (0.18) | 11.49 ms | 10.00 ms | 20.89 ms | 40.98 ms | 283.89 ms | 9221.33 | 
| python (3.7) | [fastapi](https://fastapi.tiangolo.com) (0.29) | 15.79 ms | 14.37 ms | 26.11 ms | 38.20 ms | 85.87 ms | 7728.00 | 
| node (12.4) | [hapi](https://hapijs.com) (18.1) | 18.97 ms | 15.25 ms | 24.38 ms | 90.80 ms | 706.86 ms | 29994.33 | 
| python (3.7) | [aiohttp](https://aiohttp.readthedocs.io) (3.5) | 20.07 ms | 15.42 ms | 38.08 ms | 70.36 ms | 388.79 ms | 22138.33 | 
| swift (5.0) | [kitura](https://kitura.io) (2.7) | 19.58 ms | 15.64 ms | 26.03 ms | 48.68 ms | 882.60 ms | 36683.33 | 
| swift (5.0) | [kitura-nio](https://kitura.io) (2.7) | 28.72 ms | 17.26 ms | 29.43 ms | 383.71 ms | 1887.85 ms | 90587.00 | 
| java (8) | [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 32.07 ms | 18.21 ms | 80.03 ms | 204.48 ms | 487.18 ms | 43071.33 | 
| python (3.7) | [flask](https://flask.pocoo.org) (1.0) | 25.48 ms | 19.90 ms | 46.56 ms | 85.08 ms | 379.71 ms | 16851.67 | 
| python (3.7) | [molten](https://moltenframework.com) (0.27) | 33.88 ms | 22.94 ms | 38.15 ms | 304.03 ms | 604.31 ms | 50807.33 | 
| python (3.7) | [bocadillo](https://bocadilloproject.github.io) (0.16) | 27.21 ms | 24.55 ms | 49.37 ms | 77.08 ms | 137.37 ms | 15558.00 | 
| crystal (0.28) | [lucky](https://luckyframework.org) (0.14) | 29.46 ms | 27.63 ms | 36.93 ms | 62.00 ms | 458.41 ms | 11799.33 | 
| crystal (0.29) | [athena](https://github.com/blacksmoke16/athena) (0.6) | 36.72 ms | 27.86 ms | 96.28 ms | 196.49 ms | 485.77 ms | 45680.00 | 
| node (12.4) | [turbo_polka](https://github.com/mafintosh/turbo-http) (2.0) | 33.48 ms | 30.95 ms | 39.58 ms | 49.78 ms | 810.91 ms | 25151.33 | 
| python (3.7) | [sanic](https://github.com/huge-success/sanic) (19.3) | 71.53 ms | 32.44 ms | 108.85 ms | 679.26 ms | 925.19 ms | 134378.67 | 
| python (3.7) | [quart](https://pgjones.gitlab.io/quart) (0.9) | 45.47 ms | 37.36 ms | 83.75 ms | 115.98 ms | 172.02 ms | 22944.67 | 
| python (3.7) | [responder](https://python-responder.org) (1.3) | 56.34 ms | 51.04 ms | 97.05 ms | 128.63 ms | 206.44 ms | 27086.33 | 
| python (3.7) | [django](https://djangoproject.com) (2.2) | 71.95 ms | 61.95 ms | 133.21 ms | 154.66 ms | 513.48 ms | 32156.33 | 
| python (3.7) | [masonite](https://masoniteproject.com) (2.1) | 80.59 ms | 71.29 ms | 131.23 ms | 173.71 ms | 893.76 ms | 42314.33 | 
| python (3.7) | [tornado](https://tornadoweb.org) (5.1) | 143.91 ms | 74.43 ms | 416.36 ms | 991.21 ms | 1327.42 ms | 215429.33 | 
| crystal (0.29) | [onyx](https://onyxframework.org) (0.5) | 113.41 ms | 111.39 ms | 154.78 ms | 203.55 ms | 347.35 ms | 33022.33 | 
| python (3.7) | [cyclone](https://cyclone.io) (0.0) | 246.20 ms | 201.89 ms | 251.64 ms | 2525.36 ms | 5150.45 ms | 410322.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (kore) (c)


:five: (fasthttprouter) (go)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](https://github.com/ohler55/agoo-c) (0.5) | 419629.33 | 242.64 MB |
| python (3.7) | [japronto](https://github.com/squeaky-pl/japronto) (0.1) | 388023.00 | 464.39 MB |
| rust (1.35) | [actix-web](https://actix.rs) (0.7) | 346242.00 | 392.85 MB |
| c (99) | [kore](https://kore.io) (3.1) | 323478.67 | 840.94 MB |
| go (1.12) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 315839.33 | 509.00 MB |
| cpp (11) | [evhtp](https://criticalstack/libevhtp) (1.2) | 312287.33 | 302.82 MB |
| python (3.6) | [vibora](https://vibora.io) (0.0) | 310790.33 | 352.34 MB |
| crystal (0.29) | [router.cr](https://github.com/tbrand/router.cr) (0.2) | 268451.00 | 252.15 MB |
| crystal (0.29) | [raze](https://razecr.com) (0.3) | 263282.33 | 247.33 MB |
| ruby (2.6) | [agoo](https://github.com/ohler55/agoo) (2.8) | 261269.33 | 151.18 MB |
| java (8) | [act](https://actframework.org) (1.8) | 259053.33 | 446.93 MB |
| nim (0.2) | [jester](https://github.com/dom96/jester) (0.4) | 254776.67 | 512.29 MB |
| crystal (0.29) | [kemal](https://kemalcr.com) (0.25) | 247584.00 | 404.55 MB |
| rust (1.35) | [gotham](https://gotham.rs) (0.3) | 247234.00 | 505.09 MB |
| crystal (0.29) | [spider-gazelle](https://spider-gazelle.net) (1.4) | 245825.33 | 261.98 MB |
| crystal (0.29) | [amber](https://amberframework.org) (0.28) | 216088.00 | 395.76 MB |
| crystal (0.29) | [orion](https://github.com/obsidian/orion) (1.7) | 204081.33 | 333.67 MB |
| node (12.4) | [0http](https://github.com/jkyberneees/0http) (1.0) | 200512.67 | 300.41 MB |
| node (12.4) | [restana](https://github.com/jkyberneees/ana) (3.1) | 199721.00 | 299.12 MB |
| csharp (7.3) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (2.2) | 196650.33 | 320.06 MB |
| go (1.12) | [chi](https://github.com/go-chi/chi) (4.0) | 183072.33 | 245.52 MB |
| go (1.12) | [gorouter](https://github.com/vardius/gorouter/wiki) (4.0) | 181548.33 | 241.58 MB |
| node (12.4) | [rayo](https://rayo.js.org) (1.3) | 177052.67 | 265.34 MB |
| node (12.4) | [polka](https://github.com/lukeed/polka) (0.5) | 176803.67 | 265.06 MB |
| go (1.12) | [beego](https://beego.me) (1.12) | 171832.67 | 231.05 MB |
| go (1.12) | [violetear](https://violetear.org) (7.0) | 171531.00 | 226.98 MB |
| go (1.12) | [echo](https://echo.labstack.com) (4.1) | 171505.00 | 300.33 MB |
| rust (1.35) | [iron](https://ironframework.io) (0.6) | 171021.33 | 216.07 MB |
| go (1.12) | [kami](https://github.com/guregu/kami) (2.2) | 170894.67 | 226.02 MB |
| go (1.12) | [gin](https://gin-gonic.com) (1.4) | 170276.67 | 298.41 MB |
| go (1.12) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 166076.67 | 221.53 MB |
| python (3.7) | [falcon](https://falconframework.org) (2.0) | 162916.00 | 381.96 MB |
| kotlin (1.3) | [ktor](https://ktor.io) (1.2) | 156229.00 | 243.64 MB |
| node (12.4) | [fastify](https://fastify.io) (2.4) | 149757.33 | 378.47 MB |
| node (12.4) | [muneem](https://github.com/node-muneem/muneem) (2.4) | 145798.67 | 218.53 MB |
| node (12.4) | [foxify](https://foxify.js.org) (0.1) | 140641.33 | 295.76 MB |
| go (1.12) | [gf](https://goframe.org) (1.6) | 138374.67 | 210.06 MB |
| node (12.4) | [koa](https://koajs.com) (2.7) | 136908.00 | 289.91 MB |
| rust (1.35) | [nickel](https://nickel-org.github.io) (0.11) | 127364.33 | 251.91 MB |
| python (3.7) | [bottle](https://bottlepy.org) (0.12) | 126107.67 | 310.83 MB |
| swift (5.0) | [perfect](https://perfect.org) (3.1) | 123577.33 | 116.09 MB |
| node (12.4) | [restify](https://restify.com) (8.2) | 121796.33 | 213.66 MB |
| node (12.4) | [express](https://expressjs.com) (4.16) | 108326.33 | 265.37 MB |
| python (3.7) | [starlette](https://starlette.io) (0.12) | 99569.33 | 214.61 MB |
| scala (2.12) | [http4s](https://http4s.org) (0.18) | 95056.67 | 166.42 MB |
| swift (5.0) | [vapor](https://vapor.codes) (3.3) | 91303.00 | 153.33 MB |
| python (3.7) | [hug](https://hug.rest) (2.5) | 88910.33 | 220.49 MB |
| scala (2.12) | [akkahttp](https://akka.io) (10.1) | 76695.33 | 164.70 MB |
| php (7.3) | [slim](https://slimframework.com) (3.12) | 65692.33 | 325.45 MB |
| node (12.4) | [hapi](https://hapijs.com) (18.1) | 64872.00 | 168.46 MB |
| php (7.3) | [lumen](https://lumen.laravel.com) (5.8) | 64504.00 | 334.58 MB |
| php (7.3) | [symfony](https://symfony.com) (4.3) | 64240.33 | 318.57 MB |
| python (3.7) | [fastapi](https://fastapi.tiangolo.com) (0.29) | 63856.67 | 138.07 MB |
| php (7.3) | [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 63838.67 | 316.36 MB |
| php (7.3) | [zend-framework](https://framework.zend.com) (3.1) | 60292.33 | 298.87 MB |
| rust (nightly) | [rocket](https://rocket.rs) (0.4) | 59221.33 | 92.78 MB |
| python (3.7) | [aiohttp](https://aiohttp.readthedocs.io) (3.5) | 56614.67 | 128.27 MB |
| swift (5.0) | [kitura](https://kitura.io) (2.7) | 56431.00 | 104.75 MB |
| java (8) | [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 56138.67 | 72.20 MB |
| php (7.3) | [laravel](https://laravel.com) (5.8) | 51614.67 | 269.03 MB |
| swift (5.0) | [kitura-nio](https://kitura.io) (2.7) | 50873.00 | 95.85 MB |
| ruby (2.6) | [roda](https://roda.jeremyevans.net) (3.2) | 45726.00 | 43.57 MB |
| crystal (0.29) | [athena](https://github.com/blacksmoke16/athena) (0.6) | 43586.67 | 54.44 MB |
| ruby (2.6) | [cuba](https://cuba.is) (3.9) | 42217.00 | 49.73 MB |
| python (3.7) | [flask](https://flask.pocoo.org) (1.0) | 41232.33 | 101.65 MB |
| python (3.7) | [molten](https://moltenframework.com) (0.27) | 38988.00 | 72.40 MB |
| python (3.7) | [bocadillo](https://bocadilloproject.github.io) (0.16) | 38138.33 | 73.69 MB |
| crystal (0.28) | [lucky](https://luckyframework.org) (0.14) | 33958.00 | 41.87 MB |
| ruby (2.6) | [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 33819.33 | 19.49 MB |
| node (12.4) | [turbo_polka](https://github.com/mafintosh/turbo-http) (2.0) | 30381.00 | 28.49 MB |
| python (3.7) | [sanic](https://github.com/huge-success/sanic) (19.3) | 27151.00 | 48.36 MB |
| ruby (2.6) | [flame](https://github.com/AlexWayfer/flame) (4.18) | 23003.67 | 13.26 MB |
| python (3.7) | [quart](https://pgjones.gitlab.io/quart) (0.9) | 22525.33 | 44.90 MB |
| ruby (2.6) | [hanami](https://hanamirb.org) (1.3) | 19503.33 | 147.41 MB |
| python (3.7) | [responder](https://python-responder.org) (1.3) | 17734.00 | 38.59 MB |
| ruby (2.6) | [sinatra](https://sinatrarb.com) (2.0) | 16643.67 | 43.16 MB |
| python (3.7) | [django](https://djangoproject.com) (2.2) | 13874.00 | 40.21 MB |
| python (3.7) | [tornado](https://tornadoweb.org) (5.1) | 13108.00 | 38.39 MB |
| python (3.7) | [masonite](https://masoniteproject.com) (2.1) | 12266.33 | 30.21 MB |
| crystal (0.29) | [onyx](https://onyxframework.org) (0.5) | 8769.00 | 22.55 MB |
| ruby (2.6) | [rails](https://rubyonrails.org) (5.2) | 4301.67 | 13.20 MB |
| python (3.7) | [cyclone](https://cyclone.io) (0.0) | 2867.67 | 7.79 MB |
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

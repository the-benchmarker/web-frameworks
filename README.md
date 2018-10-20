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

> job is either a language (example : crystal) or a framework (example : router.cr)

~~~sh
bin/neph [job]
~~~

+ Start the benchmark ....

> tools is a list of language / framework to challenge (example : ruby kemal amber go python)

~~~sh
bin/benchmarker [tools]
~~~

## Results

<!-- Result from here -->
```
OS: Linux (version: 4.18.14-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
threads: 9, connections: 1000
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: rocket (rust)


:three: iron (rust)


:four: rack-routing (ruby)


:five: hanami (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.29) | [nickel](http://nickel-org.github.io) (0.10) | 0.09 ms | 0.09 ms | 0.13 ms | 0.15 ms | 5.26 ms | 47.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 0.10 ms | 0.09 ms | 0.15 ms | 0.19 ms | 6.45 ms | 54.67 | 
| rust (1.29) | [iron](http://ironframework.io) (0.6) | 0.42 ms | 0.44 ms | 0.65 ms | 0.89 ms | 14.58 ms | 233.67 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.95 ms | 0.82 ms | 14.59 ms | 45.86 ms | 147.63 ms | 9422.00 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.2) | 8.14 ms | 1.07 ms | 25.25 ms | 75.47 ms | 225.93 ms | 15620.67 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 98.50 ms | 1.10 ms | 237.03 ms | 1885.78 ms | 5188.08 ms | 340266.33 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 3.84 ms | 1.11 ms | 10.75 ms | 33.30 ms | 119.69 ms | 6790.00 | 
| php (7.2) | [symfony](http://symfony.com) (4.1) | 118.09 ms | 1.45 ms | 212.79 ms | 2524.55 ms | 5650.58 ms | 457143.67 | 
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 3.27 ms | 2.56 ms | 6.63 ms | 13.71 ms | 31.84 ms | 2995.33 | 
| rust (1.29) | [actix-web](http://actix.rs) (0.7) | 3.42 ms | 2.58 ms | 7.00 ms | 14.87 ms | 129.09 ms | 4270.67 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 86.83 ms | 2.83 ms | 163.67 ms | 2060.78 ms | 4734.41 ms | 343743.67 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 10.76 ms | 3.09 ms | 31.29 ms | 84.40 ms | 245.50 ms | 17705.00 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 7.92 ms | 3.13 ms | 22.04 ms | 54.97 ms | 166.16 ms | 11723.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.46 ms | 3.47 ms | 5.46 ms | 8.08 ms | 142.64 ms | 2718.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.50 ms | 3.65 ms | 8.88 ms | 16.26 ms | 37.88 ms | 3531.33 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.15 ms | 3.77 ms | 6.41 ms | 12.70 ms | 56.81 ms | 2493.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.83 ms | 4.52 ms | 7.53 ms | 13.44 ms | 36.51 ms | 2670.67 | 
| crystal (0.26.1) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 5.27 ms | 4.74 ms | 9.34 ms | 16.59 ms | 34.00 ms | 3206.00 | 
| c (??) | [kore](http://kore.io) (3.1) | 4.81 ms | 4.75 ms | 7.92 ms | 10.27 ms | 17.71 ms | 2390.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.44 ms | 5.44 ms | 10.80 ms | 21.45 ms | 132.99 ms | 5016.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 6.99 ms | 5.51 ms | 10.93 ms | 23.25 ms | 353.24 ms | 9725.67 | 
| go (1.11) | [iris](http://iris-go.com) (10.7) | 7.08 ms | 5.80 ms | 11.45 ms | 22.85 ms | 155.16 ms | 4396.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 6.71 ms | 5.91 ms | 9.62 ms | 18.81 ms | 270.60 ms | 5627.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 8.05 ms | 6.27 ms | 12.93 ms | 27.18 ms | 421.50 ms | 10821.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 7.82 ms | 6.30 ms | 12.93 ms | 26.62 ms | 233.73 ms | 6586.00 | 
| go (1.11) | [beego](http://beego.me) (1.10) | 7.87 ms | 6.50 ms | 12.96 ms | 26.06 ms | 163.47 ms | 5160.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 7.95 ms | 6.64 ms | 13.20 ms | 24.86 ms | 203.42 ms | 5441.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 184.80 ms | 7.28 ms | 36.85 ms | 4501.70 ms | 7922.96 ms | 771862.67 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 39.27 ms | 7.79 ms | 149.89 ms | 401.41 ms | 617.30 ms | 88556.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 10.22 ms | 9.14 ms | 16.20 ms | 27.80 ms | 222.23 ms | 8087.00 | 
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 13.89 ms | 9.70 ms | 21.83 ms | 70.81 ms | 568.59 ms | 24116.33 | 
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 16.09 ms | 10.00 ms | 26.25 ms | 120.20 ms | 712.84 ms | 31495.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 11.38 ms | 11.33 ms | 13.31 ms | 15.31 ms | 28.92 ms | 1691.33 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 50.01 ms | 12.25 ms | 158.65 ms | 399.32 ms | 997.02 ms | 86060.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 20.32 ms | 12.55 ms | 23.03 ms | 246.48 ms | 1318.74 ms | 63941.33 | 
| node (10.12) | [fastify](http://fastify.io) (1.12) | 17.60 ms | 12.93 ms | 28.60 ms | 72.61 ms | 582.11 ms | 23600.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 14.43 ms | 13.16 ms | 26.01 ms | 43.33 ms | 295.10 ms | 10984.67 | 
| node (10.12) | [restify](http://restify.com) (7.2) | 20.23 ms | 16.17 ms | 31.40 ms | 66.08 ms | 504.14 ms | 20283.00 | 
| node (10.12) | [koa](http://koajs.com) (2.5) | 24.66 ms | 16.90 ms | 35.26 ms | 200.95 ms | 1012.25 ms | 49612.67 | 
| python (3.6) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 23.26 ms | 17.39 ms | 49.20 ms | 78.74 ms | 120.57 ms | 17200.00 | 
| node (10.12) | [express](http://expressjs.com) (4.16) | 26.97 ms | 20.34 ms | 42.02 ms | 132.10 ms | 875.29 ms | 39275.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 25.73 ms | 22.89 ms | 39.67 ms | 52.88 ms | 187.44 ms | 9079.33 | 
| crystal (0.26.1) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 26.87 ms | 24.34 ms | 35.81 ms | 44.21 ms | 614.19 ms | 20135.67 | 
| crystal (0.25.1) | [raze](http://razecr.com) (0.3) | 26.70 ms | 25.18 ms | 34.65 ms | 41.45 ms | 384.98 ms | 10755.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 32.76 ms | 26.77 ms | 59.65 ms | 92.44 ms | 402.41 ms | 21322.00 | 
| crystal (0.25.1) | [lucky](http://luckyframework.org) (0.11) | 29.87 ms | 26.89 ms | 40.20 ms | 47.39 ms | 388.99 ms | 12232.33 | 
| crystal (0.26.1) | [amber](http://amberframework.org) (0.9) | 31.83 ms | 27.55 ms | 45.39 ms | 52.92 ms | 315.31 ms | 12666.33 | 
| node (10.12) | [hapi](http://hapijs.com) (17.6) | 47.16 ms | 28.41 ms | 54.40 ms | 670.56 ms | 1678.84 ms | 112525.00 | 
| crystal (0.26.1) | [orion](http://github.com/obsidian/orion) (1.6) | 35.36 ms | 36.48 ms | 45.45 ms | 52.86 ms | 326.02 ms | 11317.67 | 
| crystal (0.26.1) | [kemal](http://kemalcr.com) (0.24) | 37.50 ms | 37.57 ms | 47.35 ms | 56.04 ms | 324.94 ms | 11819.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 46.68 ms | 40.12 ms | 85.08 ms | 141.28 ms | 289.79 ms | 28892.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 55.18 ms | 48.12 ms | 89.30 ms | 130.87 ms | 490.39 ms | 25587.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 81.05 ms | 73.75 ms | 142.96 ms | 214.79 ms | 317.60 ms | 44878.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 85.28 ms | 82.72 ms | 113.59 ms | 135.73 ms | 481.72 ms | 24638.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (actix-web) (rust)


:three: (evhtp) (cpp)


:four: (vibora) (python)


:five: (fasthttprouter) (go)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 305494.00 | 365.84 MB |
| rust (1.29) | [actix-web](http://actix.rs) (0.7) | 295184.00 | 335.76 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 256479.33 | 249.06 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 236531.33 | 268.50 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 223523.67 | 361.57 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 216214.00 | 434.53 MB |
| java (8) | [act](http://actframework.org) (1.8) | 215706.00 | 368.86 MB |
| crystal (0.26.1) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 185172.33 | 197.85 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 146282.33 | 196.74 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 143172.67 | 233.39 MB |
| rust (1.29) | [iron](http://ironframework.io) (0.6) | 142273.00 | 178.65 MB |
| go (1.11) | [iris](http://iris-go.com) (10.7) | 138448.67 | 185.43 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 131881.33 | 209.04 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 126966.33 | 222.89 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 126263.33 | 221.68 MB |
| go (1.11) | [beego](http://beego.me) (1.10) | 125391.33 | 169.15 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 123919.67 | 165.58 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 108859.00 | 191.14 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 97922.00 | 241.23 MB |
| rust (1.29) | [nickel](http://nickel-org.github.io) (0.10) | 89344.00 | 177.56 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 87807.33 | 82.54 MB |
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 87148.67 | 130.52 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 83216.67 | 417.64 MB |
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 79645.33 | 119.37 MB |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 79351.00 | 393.75 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 73728.00 | 157.80 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 71840.67 | 125.59 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 70808.33 | 118.82 MB |
| node (10.12) | [fastify](http://fastify.io) (1.12) | 67747.67 | 159.46 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 64049.67 | 319.02 MB |
| c (??) | [kore](http://kore.io) (3.1) | 60141.00 | 162.95 MB |
| node (10.12) | [restify](http://restify.com) (7.2) | 53394.00 | 93.43 MB |
| node (10.12) | [koa](http://koajs.com) (2.5) | 51496.00 | 108.70 MB |
| python (3.6) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 47062.33 | 106.69 MB |
| node (10.12) | [express](http://expressjs.com) (4.16) | 42299.33 | 103.43 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 39066.33 | 72.49 MB |
| crystal (0.26.1) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 37666.00 | 35.38 MB |
| crystal (0.25.1) | [raze](http://razecr.com) (0.3) | 37101.00 | 34.78 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 33540.33 | 31.98 MB |
| crystal (0.25.1) | [lucky](http://luckyframework.org) (0.11) | 33050.00 | 40.66 MB |
| node (10.12) | [hapi](http://hapijs.com) (17.6) | 32150.00 | 83.03 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 31716.00 | 78.11 MB |
| crystal (0.26.1) | [amber](http://amberframework.org) (0.9) | 31267.33 | 45.30 MB |
| crystal (0.26.1) | [orion](http://github.com/obsidian/orion) (1.6) | 28418.67 | 46.30 MB |
| crystal (0.26.1) | [kemal](http://kemalcr.com) (0.24) | 26881.33 | 43.78 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 26161.33 | 15.09 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 22128.67 | 39.39 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 17944.67 | 52.10 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 16200.67 | 9.34 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.2) | 15853.33 | 119.82 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 12567.00 | 25.01 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 11912.00 | 30.97 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 11432.00 | 30.58 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 2556.00 | 7.87 MB |
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

> job is either a language (example : crystal) or a framework (example : router.cr)

~~~sh
bin/neph [job]
~~~

+ Start the benchmark ....

> tools is a list of language / framework to challenge (example : ruby kemal amber go python)

~~~sh
bin/benchmarker [tools]
~~~

## Results

<!-- Result from here -->
Last update: 2018-10-22
```
OS: Linux (version: 4.18.14-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: rocket (rust)


:three: iron (rust)


:four: rack-routing (ruby)


:five: hanami (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.29) | [nickel](http://nickel-org.github.io) (0.10) | 0.09 ms | 0.09 ms | 0.13 ms | 0.15 ms | 5.26 ms | 47.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 0.10 ms | 0.09 ms | 0.15 ms | 0.19 ms | 6.45 ms | 54.67 | 
| rust (1.29) | [iron](http://ironframework.io) (0.6) | 0.42 ms | 0.44 ms | 0.65 ms | 0.89 ms | 14.58 ms | 233.67 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.95 ms | 0.82 ms | 14.59 ms | 45.86 ms | 147.63 ms | 9422.00 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.2) | 8.14 ms | 1.07 ms | 25.25 ms | 75.47 ms | 225.93 ms | 15620.67 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 98.50 ms | 1.10 ms | 237.03 ms | 1885.78 ms | 5188.08 ms | 340266.33 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 3.84 ms | 1.11 ms | 10.75 ms | 33.30 ms | 119.69 ms | 6790.00 | 
| php (7.2) | [symfony](http://symfony.com) (4.1) | 118.09 ms | 1.45 ms | 212.79 ms | 2524.55 ms | 5650.58 ms | 457143.67 | 
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 3.27 ms | 2.56 ms | 6.63 ms | 13.71 ms | 31.84 ms | 2995.33 | 
| rust (1.29) | [actix-web](http://actix.rs) (0.7) | 3.42 ms | 2.58 ms | 7.00 ms | 14.87 ms | 129.09 ms | 4270.67 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 86.83 ms | 2.83 ms | 163.67 ms | 2060.78 ms | 4734.41 ms | 343743.67 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 10.76 ms | 3.09 ms | 31.29 ms | 84.40 ms | 245.50 ms | 17705.00 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 7.92 ms | 3.13 ms | 22.04 ms | 54.97 ms | 166.16 ms | 11723.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.46 ms | 3.47 ms | 5.46 ms | 8.08 ms | 142.64 ms | 2718.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.50 ms | 3.65 ms | 8.88 ms | 16.26 ms | 37.88 ms | 3531.33 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.15 ms | 3.77 ms | 6.41 ms | 12.70 ms | 56.81 ms | 2493.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.83 ms | 4.52 ms | 7.53 ms | 13.44 ms | 36.51 ms | 2670.67 | 
| crystal (0.26.1) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 5.27 ms | 4.74 ms | 9.34 ms | 16.59 ms | 34.00 ms | 3206.00 | 
| c (??) | [kore](http://kore.io) (3.1) | 4.81 ms | 4.75 ms | 7.92 ms | 10.27 ms | 17.71 ms | 2390.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.44 ms | 5.44 ms | 10.80 ms | 21.45 ms | 132.99 ms | 5016.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 6.99 ms | 5.51 ms | 10.93 ms | 23.25 ms | 353.24 ms | 9725.67 | 
| go (1.11) | [iris](http://iris-go.com) (10.7) | 7.08 ms | 5.80 ms | 11.45 ms | 22.85 ms | 155.16 ms | 4396.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 6.71 ms | 5.91 ms | 9.62 ms | 18.81 ms | 270.60 ms | 5627.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 8.05 ms | 6.27 ms | 12.93 ms | 27.18 ms | 421.50 ms | 10821.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 7.82 ms | 6.30 ms | 12.93 ms | 26.62 ms | 233.73 ms | 6586.00 | 
| go (1.11) | [beego](http://beego.me) (1.10) | 7.87 ms | 6.50 ms | 12.96 ms | 26.06 ms | 163.47 ms | 5160.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 7.95 ms | 6.64 ms | 13.20 ms | 24.86 ms | 203.42 ms | 5441.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 184.80 ms | 7.28 ms | 36.85 ms | 4501.70 ms | 7922.96 ms | 771862.67 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 39.27 ms | 7.79 ms | 149.89 ms | 401.41 ms | 617.30 ms | 88556.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 10.22 ms | 9.14 ms | 16.20 ms | 27.80 ms | 222.23 ms | 8087.00 | 
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 13.89 ms | 9.70 ms | 21.83 ms | 70.81 ms | 568.59 ms | 24116.33 | 
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 16.09 ms | 10.00 ms | 26.25 ms | 120.20 ms | 712.84 ms | 31495.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 11.38 ms | 11.33 ms | 13.31 ms | 15.31 ms | 28.92 ms | 1691.33 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 50.01 ms | 12.25 ms | 158.65 ms | 399.32 ms | 997.02 ms | 86060.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 20.32 ms | 12.55 ms | 23.03 ms | 246.48 ms | 1318.74 ms | 63941.33 | 
| node (10.12) | [fastify](http://fastify.io) (1.12) | 17.60 ms | 12.93 ms | 28.60 ms | 72.61 ms | 582.11 ms | 23600.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 14.43 ms | 13.16 ms | 26.01 ms | 43.33 ms | 295.10 ms | 10984.67 | 
| node (10.12) | [restify](http://restify.com) (7.2) | 20.23 ms | 16.17 ms | 31.40 ms | 66.08 ms | 504.14 ms | 20283.00 | 
| node (10.12) | [koa](http://koajs.com) (2.5) | 24.66 ms | 16.90 ms | 35.26 ms | 200.95 ms | 1012.25 ms | 49612.67 | 
| python (3.6) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 23.26 ms | 17.39 ms | 49.20 ms | 78.74 ms | 120.57 ms | 17200.00 | 
| node (10.12) | [express](http://expressjs.com) (4.16) | 26.97 ms | 20.34 ms | 42.02 ms | 132.10 ms | 875.29 ms | 39275.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 25.73 ms | 22.89 ms | 39.67 ms | 52.88 ms | 187.44 ms | 9079.33 | 
| crystal (0.26.1) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 26.87 ms | 24.34 ms | 35.81 ms | 44.21 ms | 614.19 ms | 20135.67 | 
| crystal (0.25.1) | [raze](http://razecr.com) (0.3) | 26.70 ms | 25.18 ms | 34.65 ms | 41.45 ms | 384.98 ms | 10755.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 32.76 ms | 26.77 ms | 59.65 ms | 92.44 ms | 402.41 ms | 21322.00 | 
| crystal (0.25.1) | [lucky](http://luckyframework.org) (0.11) | 29.87 ms | 26.89 ms | 40.20 ms | 47.39 ms | 388.99 ms | 12232.33 | 
| crystal (0.26.1) | [amber](http://amberframework.org) (0.9) | 31.83 ms | 27.55 ms | 45.39 ms | 52.92 ms | 315.31 ms | 12666.33 | 
| node (10.12) | [hapi](http://hapijs.com) (17.6) | 47.16 ms | 28.41 ms | 54.40 ms | 670.56 ms | 1678.84 ms | 112525.00 | 
| crystal (0.26.1) | [orion](http://github.com/obsidian/orion) (1.6) | 35.36 ms | 36.48 ms | 45.45 ms | 52.86 ms | 326.02 ms | 11317.67 | 
| crystal (0.26.1) | [kemal](http://kemalcr.com) (0.24) | 37.50 ms | 37.57 ms | 47.35 ms | 56.04 ms | 324.94 ms | 11819.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 46.68 ms | 40.12 ms | 85.08 ms | 141.28 ms | 289.79 ms | 28892.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 55.18 ms | 48.12 ms | 89.30 ms | 130.87 ms | 490.39 ms | 25587.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 81.05 ms | 73.75 ms | 142.96 ms | 214.79 ms | 317.60 ms | 44878.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 85.28 ms | 82.72 ms | 113.59 ms | 135.73 ms | 481.72 ms | 24638.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (actix-web) (rust)


:three: (evhtp) (cpp)


:four: (vibora) (python)


:five: (fasthttprouter) (go)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 305494.00 | 365.84 MB |
| rust (1.29) | [actix-web](http://actix.rs) (0.7) | 295184.00 | 335.76 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 256479.33 | 249.06 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 236531.33 | 268.50 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 223523.67 | 361.57 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 216214.00 | 434.53 MB |
| java (8) | [act](http://actframework.org) (1.8) | 215706.00 | 368.86 MB |
| crystal (0.26.1) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 185172.33 | 197.85 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 146282.33 | 196.74 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 143172.67 | 233.39 MB |
| rust (1.29) | [iron](http://ironframework.io) (0.6) | 142273.00 | 178.65 MB |
| go (1.11) | [iris](http://iris-go.com) (10.7) | 138448.67 | 185.43 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 131881.33 | 209.04 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 126966.33 | 222.89 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 126263.33 | 221.68 MB |
| go (1.11) | [beego](http://beego.me) (1.10) | 125391.33 | 169.15 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 123919.67 | 165.58 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 108859.00 | 191.14 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 97922.00 | 241.23 MB |
| rust (1.29) | [nickel](http://nickel-org.github.io) (0.10) | 89344.00 | 177.56 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 87807.33 | 82.54 MB |
| node (10.12) | [polka](http://github.com/lukeed/polka) (0.5) | 87148.67 | 130.52 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 83216.67 | 417.64 MB |
| node (10.12) | [rayo](http://rayo.js.org) (1.2) | 79645.33 | 119.37 MB |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 79351.00 | 393.75 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 73728.00 | 157.80 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 71840.67 | 125.59 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 70808.33 | 118.82 MB |
| node (10.12) | [fastify](http://fastify.io) (1.12) | 67747.67 | 159.46 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 64049.67 | 319.02 MB |
| c (??) | [kore](http://kore.io) (3.1) | 60141.00 | 162.95 MB |
| node (10.12) | [restify](http://restify.com) (7.2) | 53394.00 | 93.43 MB |
| node (10.12) | [koa](http://koajs.com) (2.5) | 51496.00 | 108.70 MB |
| python (3.6) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 47062.33 | 106.69 MB |
| node (10.12) | [express](http://expressjs.com) (4.16) | 42299.33 | 103.43 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 39066.33 | 72.49 MB |
| crystal (0.26.1) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 37666.00 | 35.38 MB |
| crystal (0.25.1) | [raze](http://razecr.com) (0.3) | 37101.00 | 34.78 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 33540.33 | 31.98 MB |
| crystal (0.25.1) | [lucky](http://luckyframework.org) (0.11) | 33050.00 | 40.66 MB |
| node (10.12) | [hapi](http://hapijs.com) (17.6) | 32150.00 | 83.03 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 31716.00 | 78.11 MB |
| crystal (0.26.1) | [amber](http://amberframework.org) (0.9) | 31267.33 | 45.30 MB |
| crystal (0.26.1) | [orion](http://github.com/obsidian/orion) (1.6) | 28418.67 | 46.30 MB |
| crystal (0.26.1) | [kemal](http://kemalcr.com) (0.24) | 26881.33 | 43.78 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 26161.33 | 15.09 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 22128.67 | 39.39 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 17944.67 | 52.10 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 16200.67 | 9.34 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.2) | 15853.33 | 119.82 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 12567.00 | 25.01 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 11912.00 | 30.97 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 11432.00 | 30.58 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 2556.00 | 7.87 MB |
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

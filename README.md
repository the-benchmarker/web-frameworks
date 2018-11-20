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
Last update: 2018-11-22
```
OS: Linux (version: 4.19.2-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
threads: 9, connections: 1000
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: laravel (php)


:three: symfony (php)


:four: iron (rust)


:five: slim (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 0.07 ms | 0.07 ms | 0.11 ms | 0.13 ms | 0.86 ms | 29.33 |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 110.67 ms | 0.34 ms | 293.83 ms | 1999.73 ms | 6866.80 ms | 379235.33 |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 135.96 ms | 0.35 ms | 242.80 ms | 3186.45 ms | 6870.37 ms | 524940.67 |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 0.37 ms | 0.36 ms | 0.60 ms | 0.91 ms | 35.10 ms | 303.67 |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 141.00 ms | 0.38 ms | 257.61 ms | 3186.47 ms | 6805.18 ms | 544059.00 |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 200.91 ms | 0.41 ms | 336.26 ms | 4135.12 ms | 7231.92 ms | 711746.33 |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.38 ms | 0.71 ms | 12.72 ms | 42.34 ms | 153.93 ms | 8608.33 |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 7.70 ms | 0.97 ms | 23.86 ms | 69.96 ms | 203.74 ms | 14535.00 |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 3.48 ms | 1.01 ms | 9.59 ms | 30.67 ms | 115.54 ms | 6229.67 |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 177.52 ms | 1.54 ms | 160.70 ms | 3771.15 ms | 5481.39 ms | 679427.00 |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 9.23 ms | 2.57 ms | 27.06 ms | 71.75 ms | 200.65 ms | 15099.33 |
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.38 ms | 2.58 ms | 6.98 ms | 14.76 ms | 30.66 ms | 3163.67 |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 3.30 ms | 2.59 ms | 6.65 ms | 13.82 ms | 93.24 ms | 3074.33 |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 7.64 ms | 3.12 ms | 21.22 ms | 51.49 ms | 136.26 ms | 11051.00 |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.96 ms | 3.50 ms | 6.16 ms | 12.35 ms | 153.88 ms | 3201.33 |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.56 ms | 3.61 ms | 9.14 ms | 17.31 ms | 40.13 ms | 3725.00 |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.57 ms | 3.77 ms | 5.49 ms | 7.99 ms | 97.48 ms | 2565.33 |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 3.93 ms | 3.79 ms | 5.37 ms | 9.62 ms | 161.73 ms | 3350.00 |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 4.70 ms | 4.23 ms | 8.63 ms | 15.77 ms | 35.09 ms | 3141.33 |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 6.75 ms | 4.97 ms | 11.48 ms | 45.47 ms | 117.80 ms | 8447.67 |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.09 ms | 5.10 ms | 7.92 ms | 13.47 ms | 31.33 ms | 2616.00 |
| c (99) | [kore](http://kore.io) (3.1) | 5.43 ms | 5.34 ms | 9.01 ms | 11.77 ms | 25.19 ms | 2756.00 |
| java (8) | [act](http://actframework.org) (1.8) | 6.38 ms | 5.50 ms | 10.25 ms | 20.27 ms | 194.38 ms | 7201.67 |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 7.06 ms | 5.74 ms | 11.34 ms | 22.81 ms | 183.27 ms | 5872.33 |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 7.15 ms | 5.79 ms | 11.65 ms | 23.03 ms | 233.17 ms | 5851.67 |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 7.16 ms | 5.84 ms | 11.69 ms | 22.82 ms | 235.75 ms | 6395.33 |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 7.38 ms | 5.89 ms | 12.13 ms | 24.35 ms | 238.20 ms | 7268.67 |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 6.66 ms | 5.91 ms | 9.65 ms | 17.99 ms | 216.80 ms | 5546.67 |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 7.76 ms | 6.32 ms | 13.11 ms | 25.88 ms | 121.05 ms | 5393.33 |
| go (1.11) | [beego](http://beego.me) (1.10) | 7.84 ms | 6.37 ms | 13.00 ms | 26.69 ms | 180.25 ms | 6004.33 |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 7.52 ms | 6.40 ms | 10.80 ms | 30.36 ms | 92.70 ms | 5102.00 |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 8.08 ms | 6.51 ms | 13.24 ms | 26.98 ms | 307.30 ms | 8109.00 |
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.1) | 7.53 ms | 7.25 ms | 9.77 ms | 11.50 ms | 40.67 ms | 1772.00 |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 9.88 ms | 7.39 ms | 19.66 ms | 38.11 ms | 209.86 ms | 8156.00 |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 10.46 ms | 7.70 ms | 18.05 ms | 48.38 ms | 368.32 ms | 14244.67 |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 10.32 ms | 7.98 ms | 16.80 ms | 42.42 ms | 434.83 ms | 15374.67 |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 162.86 ms | 7.99 ms | 35.70 ms | 3909.18 ms | 6965.82 ms | 685898.33 |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 11.50 ms | 8.42 ms | 24.48 ms | 45.83 ms | 207.84 ms | 9887.33 |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 40.56 ms | 9.33 ms | 131.31 ms | 330.21 ms | 987.38 ms | 71823.33 |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 12.62 ms | 9.51 ms | 20.04 ms | 47.59 ms | 542.10 ms | 19806.67 |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 14.47 ms | 9.85 ms | 23.82 ms | 67.72 ms | 601.79 ms | 25015.67 |
| go (1.11) | [gf](http://gfer.me) (1.2) | 11.12 ms | 9.92 ms | 17.10 ms | 37.03 ms | 227.10 ms | 7504.67 |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 18.23 ms | 11.80 ms | 20.56 ms | 182.64 ms | 1106.98 ms | 52970.33 |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 12.02 ms | 12.17 ms | 13.86 ms | 15.72 ms | 94.12 ms | 2632.67 |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 26.85 ms | 13.20 ms | 26.75 ms | 531.67 ms | 2578.32 ms | 109880.00 |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 18.22 ms | 13.69 ms | 29.86 ms | 71.05 ms | 605.41 ms | 25212.67 |
| node (11.1) | [koa](http://koajs.com) (2.6) | 19.54 ms | 14.91 ms | 30.21 ms | 72.15 ms | 724.39 ms | 30051.00 |
| node (11.1) | [express](http://expressjs.com) (4.16) | 24.56 ms | 17.69 ms | 37.46 ms | 140.48 ms | 889.49 ms | 41146.33 |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 21.09 ms | 19.08 ms | 35.75 ms | 55.37 ms | 91.10 ms | 10873.67 |
| node (11.1) | [restify](http://restify.com) (7.2) | 24.62 ms | 19.69 ms | 41.44 ms | 82.87 ms | 506.06 ms | 22803.33 |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 24.78 ms | 21.87 ms | 34.36 ms | 79.56 ms | 272.71 ms | 12676.67 |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 25.07 ms | 22.54 ms | 36.93 ms | 52.05 ms | 322.03 ms | 10314.00 |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 26.75 ms | 23.90 ms | 38.34 ms | 46.27 ms | 247.00 ms | 8922.00 |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 33.13 ms | 24.48 ms | 64.23 ms | 104.49 ms | 417.56 ms | 24134.33 |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 34.96 ms | 25.65 ms | 45.08 ms | 224.23 ms | 1228.53 ms | 61196.00 |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 42.04 ms | 28.02 ms | 50.39 ms | 505.07 ms | 1392.22 ms | 87860.33 |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 33.46 ms | 30.21 ms | 44.24 ms | 95.28 ms | 559.46 ms | 26209.00 |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 29.26 ms | 30.79 ms | 39.28 ms | 49.00 ms | 240.31 ms | 8665.33 |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 37.83 ms | 38.13 ms | 49.95 ms | 60.89 ms | 327.83 ms | 11589.33 |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 49.57 ms | 44.11 ms | 91.63 ms | 151.90 ms | 264.90 ms | 33048.00 |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 58.49 ms | 52.30 ms | 87.32 ms | 138.99 ms | 649.01 ms | 31167.00 |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 68.08 ms | 61.04 ms | 111.32 ms | 176.12 ms | 244.26 ms | 30660.00 |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 84.67 ms | 82.63 ms | 105.86 ms | 132.40 ms | 876.88 ms | 38298.67 |

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
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 301484.67 | 361.02 MB |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 292460.33 | 332.49 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 247767.33 | 240.54 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 235224.67 | 267.10 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 235197.67 | 380.61 MB |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 220804.00 | 452.51 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 210867.00 | 423.89 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 209034.33 | 225.49 MB |
| java (8) | [act](http://actframework.org) (1.8) | 202403.00 | 395.28 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 170590.33 | 98.64 MB |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 158375.00 | 199.85 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 141383.67 | 230.32 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 139944.67 | 187.50 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 139231.00 | 184.66 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 138302.00 | 185.02 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 137859.67 | 242.06 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 135536.33 | 237.97 MB |
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.1) | 130951.33 | 75.59 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 128536.33 | 225.68 MB |
| go (1.11) | [beego](http://beego.me) (1.10) | 127485.00 | 170.88 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 124973.00 | 166.03 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 110342.00 | 165.35 MB |
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 109947.67 | 218.48 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 109114.33 | 163.53 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 107725.67 | 276.95 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 98002.00 | 241.61 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 95286.67 | 228.01 MB |
| go (1.11) | [gf](http://gfer.me) (1.2) | 91602.67 | 139.54 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 85075.67 | 127.37 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 83006.67 | 77.96 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 73388.33 | 122.98 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 69572.00 | 121.83 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 66429.00 | 142.30 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 63000.00 | 132.19 MB |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 60601.33 | 300.52 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 59425.00 | 294.34 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 58762.00 | 123.99 MB |
| c (99) | [kore](http://kore.io) (3.1) | 56052.67 | 151.92 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 55134.00 | 286.19 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 50779.33 | 264.48 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 48609.67 | 110.30 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 48475.33 | 118.39 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 43864.00 | 76.78 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 40924.00 | 38.35 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 40193.67 | 61.30 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 39857.00 | 73.90 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 37509.00 | 35.15 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 37202.00 | 35.46 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 34561.33 | 42.36 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 34006.33 | 55.42 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 33073.33 | 81.40 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 32305.67 | 83.71 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 31025.33 | 56.63 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 29660.33 | 17.10 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 25981.00 | 42.33 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 21294.33 | 37.91 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 17059.33 | 49.49 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 16863.33 | 9.72 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 16788.33 | 126.91 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 14900.33 | 29.68 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 13851.33 | 35.95 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 11726.00 | 31.77 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 3170.67 | 9.66 MB |
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

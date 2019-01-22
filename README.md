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
+ [wrk](https://github.com/wg/wrk) as benchmarking tool.

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
Last update: 2019-01-21
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: flame (ruby)


:five: lumen (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.11) | 0.07 ms | 0.07 ms | 0.10 ms | 0.13 ms | 0.99 ms | 29.33 |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.15) | 2.88 ms | 0.14 ms | 10.98 ms | 28.80 ms | 77.44 ms | 6272.33 |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.64 ms | 0.18 ms | 14.02 ms | 32.98 ms | 84.56 ms | 7451.00 |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.51 ms | 0.28 ms | 19.90 ms | 44.56 ms | 100.59 ms | 10202.00 |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 140.10 ms | 0.29 ms | 319.85 ms | 2800.56 ms | 7320.25 ms | 509821.00 |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 137.92 ms | 0.30 ms | 336.49 ms | 2561.78 ms | 7326.41 ms | 483345.33 |
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 0.31 ms | 0.30 ms | 0.48 ms | 0.75 ms | 11.03 ms | 179.33 |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 6.47 ms | 0.30 ms | 23.99 ms | 51.29 ms | 135.75 ms | 12076.67 |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 131.70 ms | 0.31 ms | 296.96 ms | 2601.27 ms | 7224.48 ms | 477414.33 |
| php (7.3) | [laravel](http://laravel.com) (5.7) | 180.79 ms | 0.31 ms | 460.78 ms | 3550.73 ms | 6947.26 ms | 610178.33 |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 154.17 ms | 0.32 ms | 353.11 ms | 3172.94 ms | 6995.02 ms | 545395.67 |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 183.68 ms | 0.34 ms | 437.44 ms | 3547.21 ms | 6374.69 ms | 612269.00 |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 6.97 ms | 0.40 ms | 23.66 ms | 48.49 ms | 110.28 ms | 11572.00 |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 105.17 ms | 1.37 ms | 4.05 ms | 2927.86 ms | 6592.49 ms | 536920.33 |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 29.10 ms | 1.83 ms | 99.45 ms | 273.16 ms | 722.82 ms | 58285.00 |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.75 ms | 1.88 ms | 5.81 ms | 13.55 ms | 30.24 ms | 2805.33 |
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 2.23 ms | 1.90 ms | 3.79 ms | 9.19 ms | 89.00 ms | 2182.67 |
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 2.65 ms | 2.12 ms | 5.48 ms | 11.65 ms | 95.21 ms | 2574.00 |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 2.88 ms | 2.19 ms | 5.46 ms | 8.89 ms | 48.14 ms | 2008.00 |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.69 ms | 2.62 ms | 4.64 ms | 6.46 ms | 99.44 ms | 2263.00 |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.33 ms | 2.80 ms | 6.94 ms | 13.20 ms | 41.24 ms | 2914.33 |
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 3.05 ms | 2.89 ms | 4.46 ms | 7.58 ms | 141.25 ms | 2392.00 |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 3.32 ms | 2.92 ms | 5.87 ms | 11.67 ms | 27.54 ms | 2323.33 |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 3.57 ms | 2.97 ms | 6.79 ms | 13.00 ms | 29.01 ms | 2654.33 |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.59 ms | 3.14 ms | 5.94 ms | 10.19 ms | 77.06 ms | 2796.67 |
| c (99) | [kore](http://kore.io) (3.1) | 4.40 ms | 4.37 ms | 7.36 ms | 8.75 ms | 27.18 ms | 2232.33 |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 4.77 ms | 4.37 ms | 6.65 ms | 12.70 ms | 275.60 ms | 5521.33 |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 5.08 ms | 4.43 ms | 8.38 ms | 17.02 ms | 231.53 ms | 6201.33 |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 5.14 ms | 4.53 ms | 8.60 ms | 16.98 ms | 103.93 ms | 3711.00 |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 5.44 ms | 4.61 ms | 9.10 ms | 18.03 ms | 171.45 ms | 4974.00 |
| java (8) | [act](http://actframework.org) (1.8) | 5.48 ms | 4.61 ms | 8.99 ms | 20.28 ms | 128.81 ms | 4616.33 |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 5.73 ms | 4.67 ms | 8.30 ms | 27.27 ms | 79.06 ms | 4584.00 |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 6.01 ms | 4.73 ms | 9.99 ms | 21.38 ms | 292.99 ms | 8037.33 |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 5.96 ms | 4.81 ms | 10.17 ms | 20.66 ms | 160.91 ms | 4965.00 |
| go (1.11) | [beego](http://beego.me) (1.11) | 6.04 ms | 4.85 ms | 10.03 ms | 20.34 ms | 227.65 ms | 6783.33 |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.6) | 5.84 ms | 4.90 ms | 11.38 ms | 20.88 ms | 49.87 ms | 4557.67 |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 6.50 ms | 5.03 ms | 9.92 ms | 17.95 ms | 156.21 ms | 5202.67 |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 6.23 ms | 5.07 ms | 10.47 ms | 20.95 ms | 105.63 ms | 4489.33 |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 193.91 ms | 6.21 ms | 477.51 ms | 4095.23 ms | 7061.01 ms | 676845.33 |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 7.56 ms | 6.32 ms | 11.52 ms | 19.09 ms | 214.97 ms | 6328.00 |
| go (1.11) | [gf](http://gfer.me) (1.4) | 8.16 ms | 7.36 ms | 11.91 ms | 26.00 ms | 240.53 ms | 6703.00 |
| node (11.6) | [restana](http://github.com/jkyberneees/ana) (2.7) | 10.05 ms | 7.38 ms | 19.48 ms | 39.89 ms | 283.88 ms | 10326.67 |
| node (11.6) | [polka](http://github.com/lukeed/polka) (0.5) | 10.30 ms | 7.63 ms | 18.19 ms | 39.84 ms | 379.63 ms | 13271.67 |
| node (11.6) | [koa](http://koajs.com) (2.6) | 11.74 ms | 8.38 ms | 20.16 ms | 46.71 ms | 470.84 ms | 17126.33 |
| node (11.6) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 14.17 ms | 8.42 ms | 19.60 ms | 155.33 ms | 731.98 ms | 37389.00 |
| node (11.6) | [rayo](http://rayo.js.org) (1.2) | 11.81 ms | 8.45 ms | 20.27 ms | 47.50 ms | 511.70 ms | 19182.00 |
| node (11.6) | [foxify](http://foxify.js.org) (0.10) | 11.90 ms | 9.00 ms | 20.29 ms | 44.60 ms | 402.56 ms | 14467.00 |
| node (11.6) | [fastify](http://fastify.io) (1.13) | 12.36 ms | 9.12 ms | 20.26 ms | 45.93 ms | 491.79 ms | 18434.00 |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 14.90 ms | 10.52 ms | 18.59 ms | 103.11 ms | 916.95 ms | 39631.67 |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 13.72 ms | 10.59 ms | 21.95 ms | 106.77 ms | 591.26 ms | 23942.00 |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 11.17 ms | 11.24 ms | 12.72 ms | 14.25 ms | 143.52 ms | 2549.67 |
| node (11.6) | [express](http://expressjs.com) (4.16) | 16.17 ms | 12.53 ms | 24.91 ms | 69.33 ms | 577.55 ms | 22523.00 |
| node (11.6) | [restify](http://restify.com) (7.6) | 16.96 ms | 13.88 ms | 28.07 ms | 53.12 ms | 406.06 ms | 15312.00 |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 16.61 ms | 14.28 ms | 33.13 ms | 43.94 ms | 74.11 ms | 10458.67 |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 35.77 ms | 18.59 ms | 33.59 ms | 554.39 ms | 2093.65 ms | 118645.00 |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 25.31 ms | 19.28 ms | 51.41 ms | 74.04 ms | 242.22 ms | 16791.00 |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 22.72 ms | 19.87 ms | 31.91 ms | 36.38 ms | 182.80 ms | 7775.67 |
| node (11.6) | [hapi](http://hapijs.com) (18.0) | 30.14 ms | 22.43 ms | 38.62 ms | 257.83 ms | 1050.75 ms | 54280.33 |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 28.01 ms | 24.72 ms | 40.81 ms | 45.85 ms | 311.46 ms | 11231.00 |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 30.89 ms | 26.25 ms | 45.46 ms | 85.19 ms | 421.67 ms | 21527.33 |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 27.24 ms | 26.26 ms | 37.22 ms | 41.25 ms | 268.42 ms | 7646.00 |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 26.14 ms | 28.20 ms | 31.31 ms | 39.45 ms | 230.52 ms | 6729.00 |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 34.46 ms | 28.60 ms | 63.84 ms | 110.90 ms | 181.43 ms | 22411.00 |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 27.28 ms | 29.27 ms | 35.73 ms | 40.35 ms | 174.28 ms | 7541.67 |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 37.65 ms | 31.32 ms | 72.88 ms | 101.96 ms | 131.04 ms | 22087.33 |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 42.44 ms | 33.27 ms | 78.03 ms | 117.72 ms | 651.01 ms | 30821.33 |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 64.67 ms | 63.70 ms | 82.74 ms | 105.90 ms | 458.15 ms | 20753.33 |

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 458164.00 | 265.09 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 395071.00 | 473.03 MB |
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 369147.00 | 419.15 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 328714.33 | 318.89 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 323722.33 | 367.28 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 320645.67 | 517.53 MB |
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 288553.67 | 589.86 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 288398.67 | 271.28 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 286174.00 | 574.87 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 279937.33 | 298.68 MB |
| java (8) | [act](http://actframework.org) (1.8) | 276930.67 | 540.68 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 201149.00 | 327.50 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 197563.00 | 264.27 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 191295.33 | 255.91 MB |
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 191068.00 | 240.92 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 183950.33 | 323.05 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 183041.33 | 244.55 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.6) | 178271.00 | 103.08 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 169851.67 | 298.03 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 168303.67 | 295.23 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 166827.33 | 224.83 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 160078.00 | 213.41 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 151342.67 | 388.82 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 132004.33 | 324.97 MB |
| go (1.11) | [gf](http://gfer.me) (1.4) | 124970.00 | 189.84 MB |
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.11) | 116127.00 | 231.26 MB |
| node (11.6) | [restana](http://github.com/jkyberneees/ana) (2.7) | 111683.33 | 167.09 MB |
| node (11.6) | [polka](http://github.com/lukeed/polka) (0.5) | 110631.00 | 165.46 MB |
| node (11.6) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 105084.00 | 157.25 MB |
| node (11.6) | [rayo](http://rayo.js.org) (1.2) | 100768.67 | 150.69 MB |
| node (11.6) | [fastify](http://fastify.io) (1.13) | 98038.00 | 234.35 MB |
| node (11.6) | [koa](http://koajs.com) (2.6) | 97722.00 | 206.85 MB |
| node (11.6) | [foxify](http://foxify.js.org) (0.10) | 93419.33 | 196.04 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 90043.00 | 84.63 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 88300.33 | 154.63 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 82677.67 | 139.22 MB |
| c (99) | [kore](http://kore.io) (3.1) | 70149.33 | 190.25 MB |
| node (11.6) | [express](http://expressjs.com) (4.16) | 69961.33 | 170.80 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 67724.67 | 335.74 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 63618.67 | 144.34 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 63244.67 | 314.15 MB |
| node (11.6) | [restify](http://restify.com) (7.6) | 62773.33 | 109.78 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 61933.67 | 307.84 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 60367.67 | 300.19 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 58808.67 | 306.08 MB |
| php (7.3) | [laravel](http://laravel.com) (5.7) | 52784.00 | 278.33 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 52274.67 | 82.90 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 48606.33 | 109.19 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 46383.33 | 86.04 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.15) | 44826.67 | 42.71 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 43213.67 | 40.53 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 41717.33 | 102.71 MB |
| node (11.6) | [hapi](http://hapijs.com) (18.0) | 40482.67 | 104.53 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 38602.33 | 36.18 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 36867.67 | 60.11 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 36301.33 | 44.81 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 35452.33 | 20.45 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 35262.33 | 64.40 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 33148.33 | 54.06 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 30688.00 | 54.66 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 27436.33 | 54.70 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 24646.67 | 71.58 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 23227.67 | 13.40 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 19841.67 | 149.95 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 18343.33 | 47.55 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 15158.00 | 44.73 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 4410.00 | 13.52 MB |
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

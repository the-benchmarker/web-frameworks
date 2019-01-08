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
Last update: 2019-01-08
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


:five: zend-expressive (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 0.10 ms | 0.10 ms | 0.14 ms | 0.24 ms | 15.18 ms | 152.67 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.15) | 5.03 ms | 0.30 ms | 17.43 ms | 36.31 ms | 89.58 ms | 8620.00 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 6.38 ms | 0.37 ms | 21.94 ms | 44.74 ms | 123.14 ms | 10757.33 | 
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 0.51 ms | 0.48 ms | 0.84 ms | 1.23 ms | 19.59 ms | 303.00 | 
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 197.93 ms | 0.52 ms | 355.10 ms | 4227.70 ms | 7579.00 ms | 726271.67 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 197.15 ms | 0.54 ms | 359.51 ms | 4013.20 ms | 7341.33 ms | 691018.00 | 
| php (7.2) | [symfony](http://symfony.com) (4.2) | 167.27 ms | 0.60 ms | 366.70 ms | 3538.14 ms | 6920.78 ms | 587260.00 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 10.12 ms | 0.60 ms | 32.98 ms | 65.08 ms | 199.30 ms | 15935.00 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 9.61 ms | 0.66 ms | 30.60 ms | 60.67 ms | 188.85 ms | 14787.67 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 11.38 ms | 0.84 ms | 34.39 ms | 66.41 ms | 144.05 ms | 16250.67 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 194.61 ms | 1.04 ms | 302.52 ms | 4605.10 ms | 7861.14 ms | 758220.67 | 
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 181.45 ms | 1.15 ms | 442.34 ms | 3358.13 ms | 7492.99 ms | 605345.33 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 206.53 ms | 1.70 ms | 494.48 ms | 3941.58 ms | 7291.11 ms | 683667.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 186.76 ms | 2.44 ms | 162.52 ms | 4524.22 ms | 6597.40 ms | 782171.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 3.43 ms | 2.98 ms | 5.57 ms | 14.03 ms | 80.32 ms | 2495.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 4.00 ms | 3.76 ms | 7.60 ms | 14.19 ms | 36.44 ms | 3169.33 | 
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 4.47 ms | 3.79 ms | 8.51 ms | 18.18 ms | 166.04 ms | 4740.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 5.44 ms | 4.56 ms | 11.20 ms | 19.23 ms | 44.46 ms | 4224.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.63 ms | 4.94 ms | 6.59 ms | 11.45 ms | 94.17 ms | 2569.00 | 
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 5.17 ms | 5.05 ms | 7.76 ms | 15.67 ms | 148.08 ms | 3127.00 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 5.32 ms | 5.06 ms | 8.92 ms | 17.31 ms | 39.27 ms | 3191.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.50 ms | 5.12 ms | 8.04 ms | 17.14 ms | 178.79 ms | 5648.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.5) | 8.53 ms | 5.20 ms | 16.48 ms | 70.09 ms | 121.24 ms | 11505.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 6.19 ms | 5.29 ms | 11.01 ms | 21.41 ms | 114.16 ms | 5219.00 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 45.06 ms | 5.56 ms | 148.46 ms | 381.21 ms | 967.84 ms | 82287.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.93 ms | 5.64 ms | 9.81 ms | 17.05 ms | 42.88 ms | 3301.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 27.63 ms | 6.26 ms | 11.12 ms | 898.68 ms | 2203.17 ms | 136415.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 8.44 ms | 7.48 ms | 11.21 ms | 23.94 ms | 383.62 ms | 10351.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 138.10 ms | 7.59 ms | 113.21 ms | 3535.38 ms | 7288.08 ms | 605445.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 8.68 ms | 7.87 ms | 13.38 ms | 32.40 ms | 192.97 ms | 7523.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 9.07 ms | 7.88 ms | 13.50 ms | 28.15 ms | 358.75 ms | 8995.33 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 9.76 ms | 8.78 ms | 14.34 ms | 30.21 ms | 280.61 ms | 8910.00 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 9.56 ms | 8.98 ms | 14.23 ms | 32.95 ms | 139.87 ms | 5410.67 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 10.85 ms | 9.34 ms | 16.27 ms | 37.23 ms | 243.07 ms | 10447.67 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 10.48 ms | 9.44 ms | 16.23 ms | 32.72 ms | 133.09 ms | 6059.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 11.19 ms | 9.54 ms | 16.49 ms | 36.49 ms | 422.68 ms | 14209.67 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 10.71 ms | 9.54 ms | 16.35 ms | 34.48 ms | 258.93 ms | 6951.33 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 11.04 ms | 9.68 ms | 16.64 ms | 35.34 ms | 316.40 ms | 9585.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 18.37 ms | 12.09 ms | 41.01 ms | 72.86 ms | 201.23 ms | 15545.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 14.73 ms | 12.21 ms | 26.54 ms | 47.26 ms | 223.84 ms | 10424.33 | 
| node (11.2) | [restana](http://github.com/jkyberneees/ana) (2.3) | 20.43 ms | 12.90 ms | 32.67 ms | 174.10 ms | 812.17 ms | 40403.33 | 
| go (1.11) | [gf](http://gfer.me) (1.3) | 17.99 ms | 13.78 ms | 29.62 ms | 107.25 ms | 479.55 ms | 22887.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 14.02 ms | 14.01 ms | 16.74 ms | 19.33 ms | 112.57 ms | 3514.67 | 
| node (11.2) | [polka](http://github.com/lukeed/polka) (0.5) | 22.14 ms | 15.42 ms | 31.91 ms | 163.03 ms | 886.22 ms | 42668.33 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 23.97 ms | 15.89 ms | 27.15 ms | 263.33 ms | 1355.78 ms | 66794.00 | 
| node (11.2) | [foxify](http://foxify.js.org) (0.10) | 24.11 ms | 16.13 ms | 32.05 ms | 231.60 ms | 1004.47 ms | 51401.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 20.35 ms | 18.02 ms | 35.60 ms | 61.92 ms | 735.02 ms | 22925.00 | 
| node (11.2) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 34.71 ms | 19.05 ms | 36.30 ms | 571.15 ms | 1443.23 ms | 96435.33 | 
| node (11.2) | [fastify](http://fastify.io) (1.13) | 31.52 ms | 19.58 ms | 40.33 ms | 386.93 ms | 1206.84 ms | 71035.00 | 
| node (11.2) | [rayo](http://rayo.js.org) (1.2) | 29.62 ms | 19.82 ms | 41.50 ms | 288.03 ms | 1100.74 ms | 59328.33 | 
| node (11.2) | [koa](http://koajs.com) (2.6) | 31.86 ms | 21.41 ms | 41.49 ms | 333.10 ms | 1162.25 ms | 64557.33 | 
| node (11.2) | [express](http://expressjs.com) (4.16) | 34.52 ms | 21.50 ms | 42.42 ms | 436.08 ms | 1287.08 ms | 77649.67 | 
| node (11.2) | [restify](http://restify.com) (7.2) | 32.28 ms | 26.73 ms | 49.56 ms | 103.25 ms | 704.74 ms | 30031.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 31.28 ms | 29.11 ms | 40.69 ms | 54.71 ms | 203.01 ms | 9119.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 32.13 ms | 29.55 ms | 52.68 ms | 76.11 ms | 161.46 ms | 15459.67 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 37.89 ms | 30.52 ms | 48.46 ms | 287.47 ms | 698.37 ms | 51618.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 34.03 ms | 30.98 ms | 40.96 ms | 67.22 ms | 420.73 ms | 18570.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 34.40 ms | 34.33 ms | 44.08 ms | 53.54 ms | 67.21 ms | 7487.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 42.20 ms | 35.48 ms | 74.06 ms | 106.71 ms | 384.48 ms | 22668.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 42.14 ms | 38.72 ms | 52.26 ms | 94.23 ms | 433.50 ms | 21827.67 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 43.96 ms | 41.40 ms | 54.22 ms | 64.32 ms | 508.26 ms | 18593.33 | 
| node (11.2) | [hapi](http://hapijs.com) (17.7) | 81.49 ms | 41.48 ms | 76.16 ms | 1274.13 ms | 2488.84 ms | 205013.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 46.16 ms | 46.90 ms | 56.98 ms | 67.60 ms | 374.44 ms | 14691.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 63.05 ms | 54.88 ms | 113.19 ms | 187.71 ms | 409.02 ms | 38587.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 78.44 ms | 65.47 ms | 136.72 ms | 236.54 ms | 564.43 ms | 42666.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 79.63 ms | 72.20 ms | 139.91 ms | 200.66 ms | 342.43 ms | 41459.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 109.43 ms | 106.03 ms | 147.30 ms | 191.89 ms | 728.91 ms | 35045.00 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 282375.67 | 163.42 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 238365.00 | 285.35 MB |
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 215957.00 | 245.32 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 194343.00 | 188.56 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 193236.67 | 219.34 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 178611.00 | 359.13 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 177732.67 | 167.26 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 172535.00 | 277.73 MB |
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 171870.00 | 352.59 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 161855.67 | 174.03 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.5) | 150130.33 | 86.80 MB |
| java (8) | [act](http://actframework.org) (1.8) | 139315.33 | 272.01 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 118416.33 | 192.93 MB |
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 117533.33 | 148.06 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 110574.33 | 147.73 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 105051.33 | 184.28 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 102659.33 | 137.56 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 95141.00 | 128.19 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 95014.33 | 166.85 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 93251.00 | 125.35 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 91874.00 | 161.38 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 91684.00 | 122.80 MB |
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 82356.67 | 163.75 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 70385.00 | 180.84 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 70354.33 | 66.17 MB |
| go (1.11) | [gf](http://gfer.me) (1.3) | 67365.33 | 102.42 MB |
| node (11.2) | [restana](http://github.com/jkyberneees/ana) (2.3) | 63227.00 | 94.62 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 61690.33 | 152.01 MB |
| node (11.2) | [polka](http://github.com/lukeed/polka) (0.5) | 56678.33 | 84.88 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 56299.67 | 94.02 MB |
| node (11.2) | [foxify](http://foxify.js.org) (0.10) | 54300.67 | 114.17 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 53525.67 | 93.79 MB |
| node (11.2) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 49858.33 | 74.65 MB |
| c (99) | [kore](http://kore.io) (3.1) | 48316.33 | 131.01 MB |
| node (11.2) | [fastify](http://fastify.io) (1.13) | 45596.00 | 109.62 MB |
| node (11.2) | [rayo](http://rayo.js.org) (1.2) | 43631.00 | 65.35 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 42753.00 | 212.40 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 41474.00 | 88.80 MB |
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 41138.67 | 204.34 MB |
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 41075.00 | 204.04 MB |
| node (11.2) | [express](http://expressjs.com) (4.16) | 40668.67 | 99.53 MB |
| node (11.2) | [koa](http://koajs.com) (2.6) | 40489.00 | 85.64 MB |
| php (7.2) | [symfony](http://symfony.com) (4.2) | 40478.33 | 200.98 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 38017.67 | 197.34 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 36829.67 | 192.21 MB |
| node (11.2) | [restify](http://restify.com) (7.2) | 33118.00 | 58.01 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 31660.00 | 29.72 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 31564.00 | 71.53 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 30433.00 | 56.49 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 29553.33 | 48.18 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 29169.33 | 27.35 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 29151.33 | 47.84 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.15) | 25347.00 | 24.16 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 24089.67 | 29.69 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 24087.33 | 59.39 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 22750.00 | 41.53 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 21858.00 | 35.61 MB |
| node (11.2) | [hapi](http://hapijs.com) (17.7) | 21821.67 | 56.39 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 20111.00 | 11.61 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 16246.67 | 28.95 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 13296.00 | 7.67 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 13256.00 | 38.45 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 12826.33 | 25.60 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 12666.00 | 95.89 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 11199.33 | 29.06 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 8909.67 | 26.45 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 2831.00 | 8.68 MB |
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

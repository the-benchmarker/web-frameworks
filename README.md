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
Last update: 2018-12-27
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: slim (php)


:three: laravel (php)


:four: lumen (php)


:five: zend-expressive (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 0.08 ms | 0.08 ms | 0.11 ms | 0.16 ms | 2.02 ms | 31.00 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 213.68 ms | 0.42 ms | 302.79 ms | 4772.90 ms | 7385.90 ms | 797245.67 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 162.83 ms | 0.43 ms | 355.60 ms | 3217.07 ms | 6980.68 ms | 577550.00 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 243.05 ms | 0.44 ms | 348.75 ms | 5325.35 ms | 7715.15 ms | 890964.00 | 
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 207.62 ms | 0.44 ms | 296.62 ms | 4965.37 ms | 7569.91 ms | 800463.67 | 
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 0.48 ms | 0.46 ms | 0.82 ms | 1.21 ms | 14.44 ms | 289.33 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.91 ms | 1.15 ms | 14.22 ms | 42.05 ms | 136.68 ms | 8676.33 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.15) | 3.91 ms | 1.34 ms | 10.81 ms | 30.86 ms | 108.09 ms | 6403.33 | 
| php (7.2) | [symfony](http://symfony.com) (4.2) | 128.10 ms | 1.50 ms | 271.94 ms | 2792.13 ms | 5664.49 ms | 462869.67 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 7.85 ms | 1.52 ms | 23.58 ms | 63.62 ms | 173.15 ms | 13413.00 | 
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 168.61 ms | 1.61 ms | 300.18 ms | 3832.12 ms | 7247.09 ms | 645757.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 93.45 ms | 1.97 ms | 5.29 ms | 2969.44 ms | 6586.17 ms | 511025.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.45 ms | 2.49 ms | 7.39 ms | 15.69 ms | 36.56 ms | 3361.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 3.08 ms | 2.55 ms | 5.19 ms | 12.95 ms | 138.49 ms | 3466.67 | 
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 3.68 ms | 2.73 ms | 6.95 ms | 15.54 ms | 152.82 ms | 4738.67 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 9.63 ms | 3.45 ms | 27.39 ms | 66.90 ms | 180.80 ms | 14338.33 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 7.95 ms | 3.65 ms | 21.59 ms | 50.14 ms | 140.24 ms | 10871.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.79 ms | 3.91 ms | 5.74 ms | 10.47 ms | 48.00 ms | 2264.33 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.38 ms | 4.00 ms | 6.89 ms | 14.64 ms | 58.18 ms | 2761.33 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 4.29 ms | 4.06 ms | 7.28 ms | 13.59 ms | 30.88 ms | 2683.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 4.62 ms | 4.08 ms | 8.46 ms | 16.15 ms | 53.23 ms | 3273.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 5.06 ms | 4.10 ms | 10.26 ms | 18.26 ms | 65.53 ms | 4040.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.68 ms | 4.11 ms | 7.55 ms | 13.61 ms | 31.32 ms | 2674.67 | 
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 4.50 ms | 4.38 ms | 6.52 ms | 12.57 ms | 64.34 ms | 2665.00 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 6.73 ms | 4.81 ms | 11.69 ms | 52.29 ms | 120.17 ms | 9031.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 5.68 ms | 5.60 ms | 9.59 ms | 12.26 ms | 122.78 ms | 3072.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 7.70 ms | 6.53 ms | 12.08 ms | 24.89 ms | 168.05 ms | 5214.33 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 7.73 ms | 6.68 ms | 11.92 ms | 24.74 ms | 165.83 ms | 4807.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 7.46 ms | 6.78 ms | 10.70 ms | 20.13 ms | 226.40 ms | 5505.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 7.62 ms | 6.93 ms | 12.39 ms | 25.04 ms | 158.08 ms | 5759.33 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 8.11 ms | 6.94 ms | 12.92 ms | 26.64 ms | 223.75 ms | 5838.33 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 7.91 ms | 7.10 ms | 11.81 ms | 27.99 ms | 71.33 ms | 4541.00 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 8.29 ms | 7.26 ms | 13.13 ms | 26.42 ms | 164.24 ms | 4932.33 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 8.31 ms | 7.37 ms | 13.03 ms | 25.78 ms | 108.52 ms | 4501.33 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 8.67 ms | 7.61 ms | 13.57 ms | 28.41 ms | 168.18 ms | 5784.00 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 8.83 ms | 7.84 ms | 13.94 ms | 27.81 ms | 179.00 ms | 5822.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 192.68 ms | 7.84 ms | 33.67 ms | 4598.62 ms | 7926.92 ms | 802983.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 11.74 ms | 9.23 ms | 20.94 ms | 42.40 ms | 421.25 ms | 13260.33 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 13.64 ms | 9.81 ms | 18.80 ms | 60.84 ms | 633.27 ms | 26571.00 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 36.65 ms | 9.91 ms | 114.27 ms | 274.07 ms | 792.61 ms | 60106.33 | 
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 16.89 ms | 10.92 ms | 27.17 ms | 135.59 ms | 684.84 ms | 32332.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 11.17 ms | 11.10 ms | 13.64 ms | 16.46 ms | 46.83 ms | 2084.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 14.87 ms | 11.45 ms | 28.23 ms | 53.69 ms | 261.22 ms | 13073.00 | 
| go (1.11) | [gf](http://gfer.me) (1.3) | 12.20 ms | 11.47 ms | 17.47 ms | 35.45 ms | 102.40 ms | 6121.67 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 17.55 ms | 11.55 ms | 26.15 ms | 137.16 ms | 717.79 ms | 34729.00 | 
| node (11.1) | [fastify](http://fastify.io) (1.13) | 17.47 ms | 12.95 ms | 23.64 ms | 91.21 ms | 688.33 ms | 30075.33 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 25.20 ms | 14.86 ms | 24.40 ms | 367.12 ms | 1443.56 ms | 81367.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 16.80 ms | 15.30 ms | 30.58 ms | 51.06 ms | 265.44 ms | 11321.00 | 
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 20.42 ms | 16.14 ms | 30.94 ms | 73.77 ms | 673.25 ms | 26590.00 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 25.11 ms | 16.90 ms | 32.49 ms | 233.79 ms | 1009.09 ms | 52369.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 24.94 ms | 21.51 ms | 44.93 ms | 71.32 ms | 115.03 ms | 14401.67 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 32.04 ms | 21.97 ms | 41.86 ms | 312.86 ms | 1180.11 ms | 64011.67 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 28.43 ms | 22.96 ms | 43.75 ms | 105.58 ms | 689.32 ms | 30179.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 26.44 ms | 24.25 ms | 38.62 ms | 53.99 ms | 183.35 ms | 9451.33 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 27.92 ms | 24.32 ms | 36.90 ms | 70.57 ms | 473.93 ms | 20574.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 30.54 ms | 27.74 ms | 41.20 ms | 50.88 ms | 241.72 ms | 7760.33 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 30.62 ms | 31.97 ms | 38.19 ms | 46.14 ms | 301.05 ms | 8311.33 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 36.54 ms | 33.22 ms | 46.09 ms | 61.40 ms | 434.67 ms | 19345.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 38.99 ms | 33.40 ms | 61.79 ms | 109.14 ms | 609.13 ms | 24161.33 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 56.89 ms | 37.46 ms | 64.75 ms | 707.00 ms | 1602.87 ms | 112929.00 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 40.00 ms | 38.54 ms | 49.55 ms | 60.61 ms | 356.16 ms | 12915.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 41.72 ms | 39.77 ms | 47.37 ms | 81.01 ms | 568.12 ms | 22708.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 54.80 ms | 49.36 ms | 100.62 ms | 179.59 ms | 343.47 ms | 36678.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 58.78 ms | 51.76 ms | 85.90 ms | 155.24 ms | 648.57 ms | 34216.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 59.93 ms | 60.55 ms | 98.35 ms | 141.01 ms | 201.32 ms | 28813.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 88.13 ms | 85.89 ms | 110.23 ms | 134.49 ms | 479.19 ms | 20978.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (evhtp) (cpp)


:five: (jester) (nim)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 326683.33 | 188.94 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 301220.33 | 360.53 MB |
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 267241.00 | 303.88 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 237206.67 | 230.35 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 223980.00 | 450.44 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 219005.67 | 206.09 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 214659.67 | 347.80 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 214139.00 | 229.78 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 211726.00 | 240.34 MB |
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 197914.67 | 403.89 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 174764.33 | 100.99 MB |
| java (8) | [act](http://actframework.org) (1.8) | 161437.67 | 315.15 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 129098.67 | 171.63 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 127374.67 | 207.76 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 126717.00 | 222.27 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 126642.00 | 169.51 MB |
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 125907.00 | 158.90 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 121887.67 | 214.05 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 118671.00 | 208.41 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 118537.00 | 159.34 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 115594.67 | 156.17 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 112844.33 | 148.98 MB |
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 111597.00 | 221.82 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 91479.67 | 234.92 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 88342.00 | 83.06 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 87237.00 | 130.73 MB |
| go (1.11) | [gf](http://gfer.me) (1.3) | 82629.00 | 126.12 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 76313.67 | 114.16 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 73818.67 | 181.73 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 72241.00 | 108.28 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 71938.33 | 169.49 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 66926.67 | 143.73 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 65377.33 | 114.60 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 62392.00 | 105.25 MB |
| php (7.2) | [symfony](http://symfony.com) (4.2) | 56949.33 | 282.67 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 54063.67 | 267.84 MB |
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 53378.67 | 264.79 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 53301.67 | 111.78 MB |
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 53155.67 | 263.79 MB |
| c (99) | [kore](http://kore.io) (3.1) | 52721.67 | 142.91 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 52008.67 | 109.95 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 50630.00 | 262.91 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 44584.67 | 70.59 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 42963.00 | 223.71 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 42069.33 | 95.44 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 40181.33 | 98.22 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 38138.67 | 66.72 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 37939.00 | 70.35 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 36485.67 | 34.22 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.15) | 32725.00 | 31.20 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 32509.00 | 30.47 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 32389.67 | 52.78 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 27653.00 | 34.10 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 26157.67 | 15.10 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 25931.67 | 63.77 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 24606.00 | 40.11 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 24469.67 | 63.31 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 24355.00 | 44.48 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 19194.67 | 34.23 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 16918.67 | 49.13 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 16748.33 | 33.42 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 16290.67 | 123.22 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 16238.33 | 9.36 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 13248.67 | 34.35 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 11053.33 | 29.74 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 3485.00 | 10.75 MB |
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

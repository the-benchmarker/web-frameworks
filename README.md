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
Last update: 2019-03-19
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: zend-framework (php)


:five: laravel (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.07 ms | 0.07 ms | 0.09 ms | 0.13 ms | 0.63 ms | 23.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.18) | 2.59 ms | 0.14 ms | 9.36 ms | 24.83 ms | 65.71 ms | 5385.33 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.34 ms | 0.17 ms | 12.70 ms | 29.66 ms | 73.90 ms | 6711.00 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 107.57 ms | 0.26 ms | 185.85 ms | 2485.69 ms | 6770.86 ms | 456928.00 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 99.45 ms | 0.27 ms | 246.21 ms | 1812.56 ms | 6784.66 ms | 359656.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.13 ms | 0.29 ms | 17.93 ms | 39.08 ms | 92.55 ms | 9032.67 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 5.88 ms | 0.30 ms | 21.19 ms | 44.62 ms | 121.15 ms | 10566.67 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.33 ms | 0.34 ms | 0.52 ms | 0.77 ms | 9.51 ms | 205.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 6.58 ms | 0.38 ms | 22.22 ms | 45.89 ms | 100.75 ms | 10910.67 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 102.12 ms | 1.01 ms | 187.24 ms | 2294.74 ms | 5176.68 ms | 386815.33 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 92.83 ms | 1.37 ms | 185.14 ms | 2048.45 ms | 5776.03 ms | 364237.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 86.57 ms | 1.42 ms | 77.87 ms | 2281.84 ms | 4945.10 ms | 402538.33 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 27.32 ms | 1.44 ms | 94.37 ms | 268.59 ms | 792.83 ms | 56872.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 2.77 ms | 1.53 ms | 6.52 ms | 14.76 ms | 75.06 ms | 3190.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.67 ms | 1.77 ms | 5.93 ms | 13.49 ms | 30.28 ms | 2828.00 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 2.84 ms | 2.10 ms | 5.42 ms | 9.00 ms | 152.68 ms | 3041.33 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 70.27 ms | 2.13 ms | 171.14 ms | 1335.90 ms | 4031.70 ms | 252257.33 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 2.73 ms | 2.17 ms | 5.57 ms | 12.04 ms | 35.50 ms | 2437.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 3.79 ms | 2.54 ms | 5.41 ms | 11.84 ms | 450.48 ms | 15151.00 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 70.98 ms | 2.56 ms | 151.94 ms | 1522.37 ms | 3814.02 ms | 262902.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.08 ms | 2.57 ms | 6.49 ms | 12.28 ms | 27.96 ms | 2715.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.80 ms | 2.71 ms | 4.89 ms | 6.47 ms | 52.27 ms | 1751.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.46 ms | 2.99 ms | 6.08 ms | 10.66 ms | 26.30 ms | 2091.00 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 4.01 ms | 3.06 ms | 7.18 ms | 17.40 ms | 221.90 ms | 6544.33 | 
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 3.47 ms | 3.12 ms | 6.08 ms | 11.90 ms | 28.50 ms | 2378.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 5.60 ms | 3.18 ms | 9.18 ms | 71.58 ms | 118.61 ms | 11255.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 3.94 ms | 3.35 ms | 7.54 ms | 14.08 ms | 29.67 ms | 2874.00 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 5.58 ms | 4.36 ms | 11.49 ms | 23.25 ms | 158.09 ms | 5417.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 4.91 ms | 4.52 ms | 7.24 ms | 13.90 ms | 205.69 ms | 3758.67 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 5.68 ms | 4.53 ms | 10.74 ms | 22.53 ms | 162.02 ms | 5458.67 | 
| go (1.12) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 5.76 ms | 4.55 ms | 11.28 ms | 24.08 ms | 183.99 ms | 5426.00 | 
| go (1.11) | [beego](http://beego.me) (1.12) | 6.28 ms | 4.57 ms | 12.85 ms | 27.18 ms | 187.03 ms | 7368.67 | 
| go (1.12) | [iris](http://iris-go.com) (11.1) | 6.12 ms | 4.59 ms | 11.42 ms | 25.82 ms | 372.38 ms | 8920.00 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 6.43 ms | 4.62 ms | 13.82 ms | 28.07 ms | 161.74 ms | 6181.00 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 6.61 ms | 4.62 ms | 14.50 ms | 29.96 ms | 169.67 ms | 6679.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 6.16 ms | 4.72 ms | 11.31 ms | 20.96 ms | 138.55 ms | 5098.67 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 6.67 ms | 4.78 ms | 14.18 ms | 28.47 ms | 117.61 ms | 6208.33 | 
| node (11.11) | [restana](http://github.com/jkyberneees/ana) (2.10) | 6.30 ms | 4.84 ms | 9.23 ms | 17.24 ms | 280.50 ms | 8597.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.71 ms | 4.91 ms | 9.15 ms | 21.53 ms | 110.06 ms | 4382.33 | 
| node (11.11) | [rayo](http://rayo.js.org) (1.2) | 6.40 ms | 4.96 ms | 9.43 ms | 15.88 ms | 288.18 ms | 7877.67 | 
| node (11.11) | [polka](http://github.com/lukeed/polka) (0.5) | 6.52 ms | 5.05 ms | 9.54 ms | 16.09 ms | 269.84 ms | 7983.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 7.20 ms | 5.68 ms | 13.42 ms | 25.11 ms | 213.36 ms | 6667.00 | 
| go (1.12) | [gf](http://goframe.org) (1.5) | 8.81 ms | 6.10 ms | 18.48 ms | 39.29 ms | 136.39 ms | 8124.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 186.87 ms | 6.72 ms | 29.97 ms | 4554.52 ms | 7702.22 ms | 768676.67 | 
| node (11.11) | [fastify](http://fastify.io) (2.1) | 9.06 ms | 6.81 ms | 11.95 ms | 25.98 ms | 448.90 ms | 16809.67 | 
| node (11.11) | [foxify](http://foxify.js.org) (0.10) | 8.04 ms | 7.03 ms | 11.95 ms | 22.10 ms | 329.96 ms | 10287.00 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 8.84 ms | 8.27 ms | 14.91 ms | 21.37 ms | 51.60 ms | 4540.00 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 10.45 ms | 8.29 ms | 18.64 ms | 33.95 ms | 174.40 ms | 7851.67 | 
| node (11.11) | [express](http://expressjs.com) (4.16) | 10.55 ms | 8.48 ms | 14.48 ms | 30.24 ms | 425.65 ms | 15532.00 | 
| node (11.11) | [restify](http://restify.com) (8.1) | 10.10 ms | 9.07 ms | 13.61 ms | 24.64 ms | 212.52 ms | 6114.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 9.20 ms | 9.34 ms | 10.97 ms | 12.75 ms | 101.05 ms | 1680.33 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.3) | 12.72 ms | 9.61 ms | 17.07 ms | 41.97 ms | 780.66 ms | 27663.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 13.15 ms | 10.64 ms | 22.26 ms | 48.04 ms | 444.03 ms | 20472.33 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.8) | 14.22 ms | 12.04 ms | 25.26 ms | 37.33 ms | 68.46 ms | 7814.00 | 
| node (11.11) | [koa](http://koajs.com) (2.7) | 16.43 ms | 13.09 ms | 24.91 ms | 57.55 ms | 547.00 ms | 20541.33 | 
| node (11.11) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 20.42 ms | 13.87 ms | 28.05 ms | 156.39 ms | 847.47 ms | 39918.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 15.12 ms | 14.24 ms | 23.74 ms | 32.92 ms | 108.75 ms | 8404.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.6) | 19.41 ms | 16.53 ms | 28.32 ms | 45.25 ms | 745.60 ms | 21284.33 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 18.68 ms | 16.60 ms | 31.21 ms | 49.54 ms | 90.15 ms | 9717.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 22.52 ms | 17.27 ms | 43.80 ms | 65.06 ms | 417.09 ms | 15803.33 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 21.94 ms | 19.62 ms | 30.92 ms | 35.07 ms | 233.97 ms | 6961.67 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 22.02 ms | 20.39 ms | 23.39 ms | 42.64 ms | 471.03 ms | 15016.00 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 22.62 ms | 20.95 ms | 33.07 ms | 39.97 ms | 376.87 ms | 8072.67 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 25.22 ms | 21.67 ms | 40.79 ms | 62.23 ms | 316.09 ms | 12842.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 28.27 ms | 23.02 ms | 37.25 ms | 186.80 ms | 839.56 ms | 41852.00 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 27.94 ms | 25.06 ms | 38.64 ms | 43.07 ms | 314.25 ms | 11186.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 32.50 ms | 27.34 ms | 57.94 ms | 104.25 ms | 229.86 ms | 20650.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 41.47 ms | 31.30 ms | 78.76 ms | 116.63 ms | 432.47 ms | 26652.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 40.12 ms | 34.28 ms | 70.77 ms | 105.45 ms | 144.51 ms | 20951.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 35.04 ms | 34.92 ms | 41.27 ms | 44.61 ms | 324.79 ms | 11097.33 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 51.19 ms | 47.30 ms | 79.81 ms | 108.06 ms | 139.32 ms | 20920.67 | 
| node (11.11) | [hapi](http://hapijs.com) (18.1) | 106.93 ms | 55.09 ms | 117.06 ms | 1536.30 ms | 2899.60 ms | 247803.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 59.53 ms | 58.33 ms | 72.93 ms | 85.51 ms | 686.00 ms | 20398.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (vibora) (python)


:five: (kore) (c)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 426535.33 | 246.52 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 418657.33 | 500.88 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 357958.67 | 406.59 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 350770.67 | 397.85 MB |
| c (99) | [kore](http://kore.io) (3.1) | 334588.00 | 869.45 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 331184.67 | 533.66 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 315279.00 | 305.86 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 299730.00 | 602.54 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 277419.67 | 160.51 MB |
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 273597.00 | 257.17 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 265037.00 | 542.21 MB |
| java (8) | [act](http://actframework.org) (1.8) | 260753.67 | 509.11 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 253262.67 | 270.70 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 193973.67 | 260.30 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 190560.33 | 310.27 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 184341.67 | 243.79 MB |
| go (1.12) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 183906.33 | 246.86 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 180238.33 | 227.55 MB |
| go (1.12) | [iris](http://iris-go.com) (11.1) | 179711.67 | 240.28 MB |
| go (1.11) | [beego](http://beego.me) (1.12) | 178164.33 | 239.97 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 172231.67 | 229.47 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 171126.00 | 300.47 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 166074.33 | 426.89 MB |
| node (11.11) | [restana](http://github.com/jkyberneees/ana) (2.10) | 164006.67 | 245.83 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 162911.00 | 285.82 MB |
| node (11.11) | [rayo](http://rayo.js.org) (1.2) | 157763.00 | 236.51 MB |
| node (11.11) | [polka](http://github.com/lukeed/polka) (0.5) | 156385.00 | 234.50 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 145558.33 | 358.50 MB |
| node (11.11) | [fastify](http://fastify.io) (2.1) | 130717.33 | 344.35 MB |
| node (11.11) | [foxify](http://foxify.js.org) (0.10) | 128921.00 | 271.03 MB |
| go (1.12) | [gf](http://goframe.org) (1.5) | 128175.67 | 194.02 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 118509.67 | 235.04 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 112078.33 | 241.51 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 106769.33 | 100.50 MB |
| node (11.11) | [express](http://expressjs.com) (4.16) | 102554.67 | 251.29 MB |
| node (11.11) | [restify](http://restify.com) (8.1) | 100309.00 | 175.98 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 99945.33 | 247.91 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 97594.67 | 485.00 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 91916.33 | 160.81 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.3) | 89836.67 | 150.45 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 89783.67 | 445.19 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 88432.33 | 189.65 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 88368.33 | 439.10 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 81534.33 | 423.44 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 79731.67 | 395.00 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.8) | 71276.00 | 153.98 MB |
| node (11.11) | [koa](http://koajs.com) (2.7) | 67295.33 | 142.23 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 66912.67 | 151.72 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 63221.67 | 329.25 MB |
| node (11.11) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 62802.33 | 94.01 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 58774.33 | 94.80 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 53961.67 | 104.18 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.6) | 52066.67 | 96.56 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.18) | 49872.33 | 47.48 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 46119.33 | 113.47 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 45881.67 | 74.84 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 45211.67 | 42.40 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 43802.33 | 41.07 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 41715.33 | 77.44 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 39611.00 | 48.63 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 38451.00 | 22.15 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 35543.33 | 64.91 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 32333.33 | 57.58 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 29163.33 | 47.52 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 25769.67 | 74.67 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 25245.33 | 50.30 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 24915.00 | 14.35 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 21821.33 | 164.98 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 19473.33 | 42.50 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 19419.00 | 50.43 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 16499.00 | 48.81 MB |
| node (11.11) | [hapi](http://hapijs.com) (18.1) | 15543.33 | 40.21 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 4704.67 | 14.46 MB |
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

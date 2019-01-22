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
+ [wrk](https://github.com/wg/wrk) as benchmarking tool, `>= 4.1.0`

:information_soucre: you need `wrk` **stable**

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
Last update: 2019-01-22
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: symfony (php)


:five: laravel (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.11) | 0.07 ms | 0.07 ms | 0.10 ms | 0.14 ms | 0.96 ms | 24.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 4.02 ms | 0.23 ms | 14.56 ms | 32.56 ms | 82.61 ms | 7493.00 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.91 ms | 0.27 ms | 17.87 ms | 37.58 ms | 90.53 ms | 8870.67 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 183.94 ms | 0.38 ms | 314.96 ms | 3942.00 ms | 6936.71 ms | 663890.00 | 
| php (7.3) | [laravel](http://laravel.com) (5.7) | 129.69 ms | 0.38 ms | 308.70 ms | 2500.22 ms | 6893.70 ms | 467809.67 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 139.65 ms | 0.40 ms | 269.00 ms | 3156.04 ms | 6791.26 ms | 524237.67 | 
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 0.44 ms | 0.42 ms | 0.75 ms | 1.13 ms | 17.58 ms | 289.00 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 8.11 ms | 0.44 ms | 28.03 ms | 56.43 ms | 141.97 ms | 13649.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 7.33 ms | 0.45 ms | 23.88 ms | 48.50 ms | 107.15 ms | 11564.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 9.42 ms | 0.63 ms | 29.04 ms | 56.73 ms | 129.07 ms | 13841.67 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 182.62 ms | 1.56 ms | 315.93 ms | 3895.74 ms | 6076.74 ms | 653374.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 54.02 ms | 1.75 ms | 3.65 ms | 1759.10 ms | 4950.12 ms | 337129.33 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 224.35 ms | 1.84 ms | 334.77 ms | 5081.83 ms | 7363.82 ms | 834502.33 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.17 ms | 2.22 ms | 6.63 ms | 14.87 ms | 35.75 ms | 3152.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 2.89 ms | 2.45 ms | 5.30 ms | 9.12 ms | 25.18 ms | 2008.33 | 
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 3.30 ms | 2.71 ms | 6.01 ms | 11.76 ms | 30.11 ms | 2417.67 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 144.26 ms | 2.97 ms | 297.94 ms | 3143.69 ms | 6204.54 ms | 524069.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.14 ms | 3.20 ms | 8.52 ms | 17.11 ms | 62.27 ms | 3742.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.26 ms | 3.38 ms | 5.27 ms | 8.12 ms | 87.76 ms | 2188.33 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 3.83 ms | 3.53 ms | 6.51 ms | 12.63 ms | 31.15 ms | 2482.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 4.26 ms | 3.71 ms | 7.85 ms | 14.60 ms | 32.74 ms | 2936.67 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 35.92 ms | 3.76 ms | 119.06 ms | 316.18 ms | 835.74 ms | 67376.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.40 ms | 3.77 ms | 7.02 ms | 12.60 ms | 62.32 ms | 2615.33 | 
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 4.06 ms | 4.06 ms | 5.55 ms | 10.56 ms | 192.55 ms | 3042.00 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.38 ms | 4.08 ms | 6.60 ms | 13.95 ms | 54.39 ms | 2569.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 6.19 ms | 5.30 ms | 8.90 ms | 17.79 ms | 290.10 ms | 7813.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 5.71 ms | 5.66 ms | 9.70 ms | 11.41 ms | 159.84 ms | 3129.00 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.6) | 6.98 ms | 5.89 ms | 13.90 ms | 24.69 ms | 89.99 ms | 5438.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 7.18 ms | 6.09 ms | 11.14 ms | 22.97 ms | 115.20 ms | 4891.67 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 7.87 ms | 6.09 ms | 11.21 ms | 25.22 ms | 493.01 ms | 14622.67 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 7.18 ms | 6.11 ms | 10.20 ms | 26.15 ms | 139.50 ms | 4368.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.68 ms | 6.13 ms | 10.62 ms | 20.37 ms | 133.71 ms | 4326.33 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 7.65 ms | 6.56 ms | 11.98 ms | 24.84 ms | 172.08 ms | 5219.67 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 8.03 ms | 6.79 ms | 12.76 ms | 27.24 ms | 223.66 ms | 6031.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 8.11 ms | 6.95 ms | 12.89 ms | 26.79 ms | 198.09 ms | 5706.00 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 8.08 ms | 7.01 ms | 12.75 ms | 26.19 ms | 120.93 ms | 5131.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 8.05 ms | 7.04 ms | 12.87 ms | 25.66 ms | 121.09 ms | 5019.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 220.35 ms | 7.37 ms | 40.87 ms | 5068.55 ms | 7932.53 ms | 876291.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 10.33 ms | 8.74 ms | 18.94 ms | 35.31 ms | 393.48 ms | 11152.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 11.85 ms | 9.40 ms | 21.07 ms | 40.75 ms | 290.49 ms | 10002.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 10.39 ms | 10.39 ms | 12.50 ms | 14.83 ms | 57.27 ms | 1798.33 | 
| go (1.11) | [gf](http://gfer.me) (1.4) | 12.00 ms | 11.05 ms | 17.28 ms | 38.98 ms | 216.30 ms | 7703.33 | 
| node (11.6) | [foxify](http://foxify.js.org) (0.10) | 17.00 ms | 12.38 ms | 25.15 ms | 90.76 ms | 600.84 ms | 25379.33 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 17.51 ms | 12.64 ms | 22.21 ms | 115.44 ms | 1054.67 ms | 43784.67 | 
| node (11.6) | [restana](http://github.com/jkyberneees/ana) (2.7) | 16.76 ms | 12.66 ms | 28.02 ms | 62.60 ms | 568.62 ms | 22120.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 21.86 ms | 13.11 ms | 26.83 ms | 307.57 ms | 1298.44 ms | 76985.67 | 
| node (11.6) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 17.22 ms | 13.18 ms | 25.27 ms | 60.77 ms | 616.93 ms | 24586.67 | 
| node (11.6) | [rayo](http://rayo.js.org) (1.2) | 17.10 ms | 13.38 ms | 26.48 ms | 62.38 ms | 573.57 ms | 22206.00 | 
| node (11.6) | [polka](http://github.com/lukeed/polka) (0.5) | 19.11 ms | 13.97 ms | 28.20 ms | 93.33 ms | 796.45 ms | 34081.67 | 
| node (11.6) | [fastify](http://fastify.io) (1.13) | 21.99 ms | 16.74 ms | 31.78 ms | 108.13 ms | 777.07 ms | 34212.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 21.00 ms | 17.73 ms | 37.40 ms | 50.56 ms | 136.29 ms | 10460.33 | 
| node (11.6) | [koa](http://koajs.com) (2.6) | 25.66 ms | 18.94 ms | 34.87 ms | 183.50 ms | 990.48 ms | 46259.67 | 
| node (11.6) | [express](http://expressjs.com) (4.16) | 28.19 ms | 19.87 ms | 36.68 ms | 252.39 ms | 987.27 ms | 52678.33 | 
| node (11.6) | [restify](http://restify.com) (7.6) | 26.21 ms | 22.26 ms | 41.78 ms | 75.10 ms | 471.89 ms | 20227.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 24.55 ms | 22.73 ms | 33.75 ms | 40.29 ms | 220.44 ms | 6237.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 25.73 ms | 23.84 ms | 35.83 ms | 41.10 ms | 239.87 ms | 7147.67 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 30.97 ms | 24.43 ms | 41.05 ms | 174.42 ms | 1201.07 ms | 53074.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 38.68 ms | 28.56 ms | 74.65 ms | 145.87 ms | 529.89 ms | 27811.33 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 32.99 ms | 31.56 ms | 41.11 ms | 52.12 ms | 548.79 ms | 16272.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 35.58 ms | 31.82 ms | 47.44 ms | 60.93 ms | 632.55 ms | 22522.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 31.09 ms | 32.05 ms | 38.89 ms | 45.32 ms | 119.90 ms | 6952.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 35.95 ms | 33.36 ms | 45.55 ms | 52.19 ms | 264.05 ms | 11294.00 | 
| node (11.6) | [hapi](http://hapijs.com) (18.0) | 55.35 ms | 34.07 ms | 58.54 ms | 801.00 ms | 1884.65 ms | 129667.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 48.14 ms | 42.63 ms | 82.91 ms | 142.35 ms | 313.03 ms | 26978.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 53.04 ms | 44.56 ms | 94.28 ms | 119.67 ms | 506.76 ms | 26043.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 53.91 ms | 48.12 ms | 91.73 ms | 124.48 ms | 188.91 ms | 25213.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 82.78 ms | 84.74 ms | 101.94 ms | 122.47 ms | 439.54 ms | 21877.67 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 342303.67 | 197.66 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 330261.00 | 395.42 MB |
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 281345.67 | 319.76 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 270524.00 | 262.63 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 264722.33 | 300.70 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 247046.67 | 232.37 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 236000.67 | 474.36 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 231125.00 | 246.58 MB |
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 218159.00 | 446.60 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 214037.00 | 345.08 MB |
| java (8) | [act](http://actframework.org) (1.8) | 188523.33 | 368.12 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 158989.33 | 258.84 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.6) | 149183.00 | 86.25 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 139819.67 | 245.42 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 137070.00 | 182.88 MB |
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 136684.67 | 172.81 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 136483.33 | 182.50 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 129622.00 | 173.41 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 124854.00 | 219.24 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 123926.67 | 166.78 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 123814.33 | 217.13 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 123800.67 | 165.45 MB |
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.11) | 119346.67 | 237.16 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 101895.33 | 261.63 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 95062.00 | 89.22 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 87308.00 | 215.17 MB |
| go (1.11) | [gf](http://gfer.me) (1.4) | 84719.00 | 128.63 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 76330.00 | 133.65 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 76148.33 | 163.43 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 70254.67 | 117.92 MB |
| node (11.6) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 68786.33 | 102.86 MB |
| node (11.6) | [foxify](http://foxify.js.org) (0.10) | 67984.00 | 142.86 MB |
| node (11.6) | [restana](http://github.com/jkyberneees/ana) (2.7) | 67723.67 | 101.41 MB |
| node (11.6) | [rayo](http://rayo.js.org) (1.2) | 65607.00 | 98.03 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 64056.67 | 318.36 MB |
| node (11.6) | [polka](http://github.com/lukeed/polka) (0.5) | 63472.00 | 94.97 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 58893.00 | 292.19 MB |
| c (99) | [kore](http://kore.io) (3.1) | 57894.67 | 156.84 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 57446.67 | 285.07 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 54557.67 | 283.38 MB |
| node (11.6) | [fastify](http://fastify.io) (1.13) | 54026.33 | 132.74 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 51845.67 | 257.07 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 50653.33 | 80.68 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 48675.67 | 110.41 MB |
| node (11.6) | [koa](http://koajs.com) (2.6) | 46615.00 | 98.44 MB |
| php (7.3) | [laravel](http://laravel.com) (5.7) | 45590.67 | 237.58 MB |
| node (11.6) | [express](http://expressjs.com) (4.16) | 44270.00 | 108.23 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 40390.67 | 37.87 MB |
| node (11.6) | [restify](http://restify.com) (7.6) | 39488.00 | 69.11 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 38390.00 | 35.98 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 36648.67 | 68.04 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 32268.67 | 52.56 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 31708.33 | 30.23 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 29784.67 | 36.66 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 28214.67 | 45.96 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 27577.33 | 50.34 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 27379.00 | 67.38 MB |
| node (11.6) | [hapi](http://hapijs.com) (18.0) | 26837.00 | 69.44 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 26044.00 | 15.02 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 21307.33 | 37.97 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 18741.67 | 54.42 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 18723.00 | 37.34 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 17409.67 | 10.05 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 15784.33 | 119.30 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 13556.33 | 35.16 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 11828.67 | 34.95 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3557.00 | 10.91 MB |
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

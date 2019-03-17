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
Last update: 2019-03-17
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: laravel (php)


:five: lumen (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.07 ms | 0.07 ms | 0.10 ms | 0.13 ms | 3.86 ms | 54.00 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.17) | 3.37 ms | 0.19 ms | 12.23 ms | 29.28 ms | 79.91 ms | 6558.33 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.33 ms | 0.23 ms | 15.76 ms | 34.50 ms | 87.86 ms | 8015.33 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 100.34 ms | 0.32 ms | 278.17 ms | 1632.83 ms | 5757.64 ms | 336421.33 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 107.95 ms | 0.33 ms | 215.53 ms | 2360.65 ms | 6779.34 ms | 418702.00 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 152.11 ms | 0.34 ms | 270.65 ms | 3303.82 ms | 6810.14 ms | 572442.00 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 6.44 ms | 0.36 ms | 22.03 ms | 45.95 ms | 106.32 ms | 10848.33 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.37 ms | 0.37 ms | 0.61 ms | 0.86 ms | 16.05 ms | 236.00 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 7.57 ms | 0.39 ms | 25.99 ms | 53.65 ms | 137.19 ms | 12831.67 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 8.89 ms | 0.53 ms | 28.94 ms | 58.79 ms | 136.05 ms | 14084.00 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 126.51 ms | 1.39 ms | 253.38 ms | 2668.87 ms | 6903.24 ms | 474475.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 133.07 ms | 1.55 ms | 224.89 ms | 3051.87 ms | 6810.83 ms | 529955.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 162.43 ms | 1.66 ms | 4.67 ms | 4340.12 ms | 6593.26 ms | 742115.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 2.85 ms | 1.71 ms | 6.40 ms | 13.84 ms | 93.38 ms | 3205.33 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 3.19 ms | 2.21 ms | 6.81 ms | 15.06 ms | 35.95 ms | 3123.67 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 89.94 ms | 2.54 ms | 184.03 ms | 1922.57 ms | 5581.79 ms | 350093.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.59 ms | 2.59 ms | 7.76 ms | 17.30 ms | 40.96 ms | 3650.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 5.78 ms | 2.64 ms | 7.17 ms | 49.74 ms | 721.37 ms | 30092.33 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 34.22 ms | 2.90 ms | 116.31 ms | 309.68 ms | 836.59 ms | 66280.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.11 ms | 3.26 ms | 8.65 ms | 16.43 ms | 40.71 ms | 3565.00 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.76 ms | 3.26 ms | 5.90 ms | 11.53 ms | 129.89 ms | 2646.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.12 ms | 3.40 ms | 7.09 ms | 13.03 ms | 48.64 ms | 2606.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.51 ms | 3.67 ms | 5.52 ms | 7.80 ms | 131.04 ms | 2019.00 | 
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 4.24 ms | 4.02 ms | 7.30 ms | 14.04 ms | 30.09 ms | 2752.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 4.78 ms | 4.22 ms | 9.00 ms | 16.43 ms | 31.82 ms | 3310.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 5.81 ms | 4.25 ms | 9.81 ms | 46.48 ms | 115.85 ms | 8712.67 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 4.84 ms | 4.32 ms | 9.30 ms | 19.70 ms | 155.08 ms | 4452.67 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 7.07 ms | 5.07 ms | 14.55 ms | 29.68 ms | 219.94 ms | 7172.33 | 
| go (1.12) | [iris](http://iris-go.com) (11.1) | 6.62 ms | 5.14 ms | 12.65 ms | 27.19 ms | 59.13 ms | 5128.00 | 
| go (1.12) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 6.75 ms | 5.15 ms | 13.05 ms | 28.22 ms | 211.67 ms | 6236.33 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 7.15 ms | 5.24 ms | 14.44 ms | 29.93 ms | 165.85 ms | 6521.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.37 ms | 5.30 ms | 10.54 ms | 21.71 ms | 142.51 ms | 5427.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 6.16 ms | 5.30 ms | 9.01 ms | 17.01 ms | 332.94 ms | 8144.00 | 
| go (1.11) | [beego](http://beego.me) (1.12) | 7.51 ms | 5.40 ms | 15.21 ms | 32.28 ms | 150.83 ms | 6738.00 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 7.18 ms | 5.48 ms | 13.39 ms | 28.41 ms | 141.56 ms | 5521.33 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 8.21 ms | 5.49 ms | 17.62 ms | 37.03 ms | 181.66 ms | 8183.67 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 8.06 ms | 5.50 ms | 16.73 ms | 34.77 ms | 191.38 ms | 8836.33 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 8.56 ms | 6.65 ms | 15.87 ms | 28.25 ms | 232.58 ms | 7603.00 | 
| go (1.12) | [gf](http://goframe.org) (1.5) | 9.67 ms | 6.70 ms | 19.33 ms | 43.72 ms | 286.88 ms | 11209.00 | 
| node (11.11) | [restana](http://github.com/jkyberneees/ana) (2.10) | 8.81 ms | 7.68 ms | 13.03 ms | 30.02 ms | 359.76 ms | 12923.00 | 
| node (11.11) | [polka](http://github.com/lukeed/polka) (0.5) | 8.52 ms | 7.81 ms | 13.07 ms | 24.29 ms | 313.89 ms | 9821.67 | 
| node (11.11) | [rayo](http://rayo.js.org) (1.2) | 8.89 ms | 8.03 ms | 13.33 ms | 25.77 ms | 354.73 ms | 11909.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 201.69 ms | 8.09 ms | 61.40 ms | 4845.72 ms | 7932.26 ms | 832645.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 9.52 ms | 8.52 ms | 15.60 ms | 29.04 ms | 272.70 ms | 7462.67 | 
| node (11.11) | [foxify](http://foxify.js.org) (0.10) | 10.60 ms | 8.83 ms | 15.03 ms | 28.94 ms | 429.98 ms | 14681.00 | 
| node (11.11) | [fastify](http://fastify.io) (2.1) | 14.54 ms | 10.00 ms | 18.74 ms | 160.24 ms | 624.41 ms | 31926.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 10.44 ms | 10.60 ms | 12.30 ms | 14.04 ms | 101.08 ms | 2029.33 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 12.16 ms | 10.60 ms | 21.49 ms | 33.49 ms | 75.60 ms | 6722.33 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.3) | 15.35 ms | 11.05 ms | 19.07 ms | 79.30 ms | 862.19 ms | 35412.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 20.97 ms | 11.71 ms | 24.76 ms | 334.84 ms | 1372.65 ms | 75888.67 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 13.21 ms | 11.74 ms | 22.43 ms | 40.23 ms | 223.06 ms | 9040.33 | 
| node (11.11) | [restify](http://restify.com) (8.1) | 15.12 ms | 12.55 ms | 21.05 ms | 44.77 ms | 530.54 ms | 19946.67 | 
| node (11.11) | [express](http://expressjs.com) (4.16) | 15.89 ms | 12.67 ms | 21.32 ms | 50.93 ms | 657.98 ms | 26758.67 | 
| node (11.11) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 24.02 ms | 16.84 ms | 36.32 ms | 119.58 ms | 896.68 ms | 40263.00 | 
| node (11.11) | [koa](http://koajs.com) (2.7) | 25.85 ms | 16.85 ms | 33.73 ms | 275.29 ms | 1073.35 ms | 58167.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 20.75 ms | 17.57 ms | 40.15 ms | 63.29 ms | 106.35 ms | 13377.67 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.7) | 19.69 ms | 18.41 ms | 30.85 ms | 45.27 ms | 113.64 ms | 8588.67 | 
| swift (4.2) | [kitura](http://kitura.io) (2.6) | 26.06 ms | 19.67 ms | 30.83 ms | 239.44 ms | 910.29 ms | 50853.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 26.90 ms | 22.54 ms | 45.18 ms | 72.37 ms | 251.44 ms | 14742.00 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 26.16 ms | 22.63 ms | 47.70 ms | 76.29 ms | 124.11 ms | 15681.33 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 34.28 ms | 24.73 ms | 73.15 ms | 109.71 ms | 275.81 ms | 24938.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 27.51 ms | 25.68 ms | 36.42 ms | 46.06 ms | 401.64 ms | 16213.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 27.57 ms | 28.18 ms | 35.31 ms | 46.88 ms | 226.21 ms | 9044.00 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 30.55 ms | 31.12 ms | 39.74 ms | 50.18 ms | 317.88 ms | 10313.67 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 34.44 ms | 32.30 ms | 43.49 ms | 58.17 ms | 529.70 ms | 21974.33 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 34.50 ms | 33.25 ms | 40.51 ms | 67.47 ms | 324.22 ms | 12855.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 42.13 ms | 34.94 ms | 76.13 ms | 140.81 ms | 321.05 ms | 27684.00 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 36.34 ms | 37.19 ms | 48.29 ms | 59.29 ms | 280.98 ms | 11133.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 47.30 ms | 41.40 ms | 74.52 ms | 108.66 ms | 496.63 ms | 23303.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 50.01 ms | 46.82 ms | 81.34 ms | 117.75 ms | 167.30 ms | 24183.33 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 74.40 ms | 70.53 ms | 128.13 ms | 171.31 ms | 260.92 ms | 37435.33 | 
| node (11.11) | [hapi](http://hapijs.com) (18.1) | 185.70 ms | 71.06 ms | 177.82 ms | 2687.70 ms | 4293.76 ms | 458835.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 77.28 ms | 75.96 ms | 99.12 ms | 143.86 ms | 782.48 ms | 33837.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (actix-web) (rust)


:three: (japronto) (python)


:four: (kore) (c)


:five: (vibora) (python)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 380110.00 | 220.01 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 316168.33 | 359.53 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 302395.00 | 361.97 MB |
| c (99) | [kore](http://kore.io) (3.1) | 288156.00 | 748.69 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 263403.00 | 299.14 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 253512.00 | 509.47 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 249048.67 | 241.58 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 243714.33 | 393.34 MB |
| java (8) | [act](http://actframework.org) (1.8) | 226710.00 | 442.82 MB |
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 225098.33 | 211.78 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 210025.00 | 121.52 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 207548.33 | 425.43 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 207399.67 | 221.58 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 159304.67 | 200.84 MB |
| go (1.12) | [iris](http://iris-go.com) (11.1) | 158478.67 | 212.50 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 157236.33 | 255.92 MB |
| go (1.12) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 155911.67 | 208.20 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 151144.00 | 265.18 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 150039.33 | 200.24 MB |
| go (1.11) | [beego](http://beego.me) (1.12) | 144772.00 | 194.82 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 144131.00 | 191.96 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 138465.00 | 242.79 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 136273.67 | 182.45 MB |
| node (11.11) | [restana](http://github.com/jkyberneees/ana) (2.10) | 122106.33 | 183.07 MB |
| node (11.11) | [polka](http://github.com/lukeed/polka) (0.5) | 120047.67 | 180.01 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 119687.00 | 307.75 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 118038.00 | 234.93 MB |
| node (11.11) | [rayo](http://rayo.js.org) (1.2) | 117474.33 | 176.08 MB |
| go (1.12) | [gf](http://goframe.org) (1.5) | 115644.67 | 175.45 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 106410.33 | 262.31 MB |
| node (11.11) | [foxify](http://foxify.js.org) (0.10) | 100814.00 | 211.99 MB |
| node (11.11) | [fastify](http://fastify.io) (2.1) | 98678.33 | 249.52 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 95405.00 | 89.63 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 82383.00 | 177.62 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 80684.33 | 141.37 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 77965.67 | 387.39 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.3) | 77560.33 | 130.22 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 76855.00 | 190.57 MB |
| node (11.11) | [express](http://expressjs.com) (4.16) | 72539.33 | 177.43 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 72357.67 | 359.05 MB |
| node (11.11) | [restify](http://restify.com) (8.1) | 71643.33 | 125.48 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 69136.33 | 148.01 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 68996.00 | 342.55 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 66159.00 | 327.84 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 64344.00 | 333.90 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 52322.00 | 272.55 MB |
| node (11.11) | [koa](http://koajs.com) (2.7) | 52185.33 | 110.39 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 51299.00 | 116.39 MB |
| node (11.11) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 51270.00 | 76.74 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.7) | 50737.00 | 109.62 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.6) | 45448.00 | 84.25 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 39594.67 | 76.51 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 39054.00 | 64.72 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 38020.33 | 93.64 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.17) | 37978.67 | 36.21 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 37046.67 | 34.73 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 36630.00 | 34.32 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 33239.00 | 61.68 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 32724.00 | 53.31 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 29614.33 | 17.08 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 29517.33 | 53.91 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 29459.67 | 36.04 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 27349.00 | 44.55 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 24910.00 | 44.37 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 21260.67 | 61.70 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 20041.67 | 39.97 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 19847.33 | 11.46 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 16969.33 | 128.33 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 14405.67 | 37.38 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 13466.33 | 29.32 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 12718.67 | 37.54 MB |
| node (11.11) | [hapi](http://hapijs.com) (18.1) | 12168.33 | 31.46 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3740.67 | 11.47 MB |
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

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
Last update: 2019-02-25
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
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.08 ms | 0.09 ms | 0.13 ms | 0.15 ms | 2.29 ms | 36.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 3.31 ms | 0.19 ms | 12.02 ms | 28.89 ms | 73.35 ms | 6469.00 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.47 ms | 0.24 ms | 16.15 ms | 36.16 ms | 94.40 ms | 8316.00 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 143.01 ms | 0.35 ms | 258.12 ms | 3138.81 ms | 6811.61 ms | 541667.33 | 
| php (7.3) | [laravel](http://laravel.com) (5.7) | 106.97 ms | 0.36 ms | 282.71 ms | 1827.51 ms | 6872.34 ms | 369262.00 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 157.61 ms | 0.36 ms | 273.89 ms | 3498.78 ms | 6798.86 ms | 596843.67 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.40 ms | 0.39 ms | 0.65 ms | 0.93 ms | 8.35 ms | 218.33 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 6.50 ms | 0.39 ms | 21.67 ms | 44.73 ms | 105.60 ms | 10579.00 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 7.10 ms | 0.43 ms | 23.09 ms | 45.68 ms | 113.05 ms | 11184.33 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 8.56 ms | 0.53 ms | 27.63 ms | 55.98 ms | 145.74 ms | 13450.00 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 136.43 ms | 1.45 ms | 229.82 ms | 2982.03 ms | 5539.40 ms | 506701.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 167.06 ms | 1.63 ms | 144.58 ms | 3620.07 ms | 4949.96 ms | 640426.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 130.27 ms | 2.02 ms | 238.68 ms | 2927.22 ms | 6849.15 ms | 509072.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.43 ms | 2.54 ms | 7.13 ms | 15.53 ms | 36.70 ms | 3323.67 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 134.68 ms | 2.56 ms | 272.42 ms | 2721.57 ms | 5872.28 ms | 475638.67 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 3.49 ms | 2.58 ms | 7.49 ms | 16.02 ms | 38.06 ms | 3343.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 3.21 ms | 2.73 ms | 6.61 ms | 9.09 ms | 22.17 ms | 2342.00 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 32.98 ms | 2.85 ms | 110.16 ms | 295.69 ms | 817.85 ms | 63399.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.24 ms | 3.34 ms | 8.70 ms | 16.73 ms | 37.65 ms | 3640.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.08 ms | 3.59 ms | 6.40 ms | 13.15 ms | 52.23 ms | 2531.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.59 ms | 3.83 ms | 5.48 ms | 7.55 ms | 21.74 ms | 1781.67 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 4.26 ms | 4.02 ms | 7.20 ms | 14.32 ms | 32.41 ms | 2746.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 4.75 ms | 4.12 ms | 8.85 ms | 16.81 ms | 82.73 ms | 3694.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 6.13 ms | 4.19 ms | 10.80 ms | 61.58 ms | 117.09 ms | 9740.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.65 ms | 4.24 ms | 7.28 ms | 12.96 ms | 49.07 ms | 2574.33 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 4.84 ms | 4.37 ms | 8.99 ms | 19.41 ms | 160.91 ms | 4310.33 | 
| node (11.10) | [restana](http://github.com/jkyberneees/ana) (2.9) | 5.61 ms | 4.65 ms | 9.02 ms | 18.23 ms | 265.25 ms | 7407.33 | 
| node (11.10) | [rayo](http://rayo.js.org) (1.2) | 6.26 ms | 4.80 ms | 9.63 ms | 19.73 ms | 360.72 ms | 10059.33 | 
| node (11.10) | [polka](http://github.com/lukeed/polka) (0.5) | 6.12 ms | 4.82 ms | 9.47 ms | 18.60 ms | 283.77 ms | 8626.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 5.35 ms | 5.30 ms | 9.08 ms | 11.27 ms | 53.19 ms | 2862.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 6.71 ms | 5.54 ms | 10.94 ms | 22.01 ms | 258.12 ms | 5124.67 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 7.06 ms | 5.61 ms | 11.09 ms | 23.14 ms | 337.96 ms | 8631.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.48 ms | 5.68 ms | 10.57 ms | 20.98 ms | 143.13 ms | 4902.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 6.49 ms | 5.71 ms | 9.51 ms | 17.85 ms | 222.58 ms | 5166.33 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 7.40 ms | 5.78 ms | 11.79 ms | 24.94 ms | 353.80 ms | 9609.33 | 
| node (11.10) | [fastify](http://fastify.io) (1.14) | 8.72 ms | 6.17 ms | 11.81 ms | 65.21 ms | 421.05 ms | 18723.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (4.0) | 7.77 ms | 6.32 ms | 13.06 ms | 26.71 ms | 119.43 ms | 4955.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 7.78 ms | 6.33 ms | 13.08 ms | 26.22 ms | 171.23 ms | 5339.67 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 7.93 ms | 6.49 ms | 13.30 ms | 27.03 ms | 171.63 ms | 5952.67 | 
| node (11.10) | [foxify](http://foxify.js.org) (0.10) | 8.24 ms | 6.58 ms | 12.38 ms | 26.83 ms | 352.64 ms | 12111.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 189.18 ms | 6.89 ms | 27.04 ms | 4946.83 ms | 7933.95 ms | 823464.67 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 8.34 ms | 6.95 ms | 13.90 ms | 27.64 ms | 125.60 ms | 5187.67 | 
| node (11.10) | [koa](http://koajs.com) (2.7) | 8.48 ms | 7.29 ms | 12.90 ms | 23.50 ms | 404.70 ms | 12753.33 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 9.03 ms | 7.76 ms | 16.64 ms | 30.88 ms | 212.91 ms | 6607.67 | 
| node (11.10) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 11.46 ms | 8.58 ms | 17.43 ms | 47.51 ms | 508.53 ms | 20764.00 | 
| node (11.10) | [restify](http://restify.com) (8.0) | 9.81 ms | 8.70 ms | 13.21 ms | 27.89 ms | 288.88 ms | 8920.00 | 
| node (11.10) | [express](http://expressjs.com) (4.16) | 10.91 ms | 8.86 ms | 15.38 ms | 36.88 ms | 467.57 ms | 17331.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 10.46 ms | 9.02 ms | 17.35 ms | 34.14 ms | 217.89 ms | 7700.33 | 
| go (1.11) | [gf](http://goframe.org) (1.5) | 10.76 ms | 9.67 ms | 15.87 ms | 34.04 ms | 212.00 ms | 8148.67 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 12.87 ms | 11.78 ms | 21.97 ms | 36.57 ms | 106.61 ms | 7257.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 12.41 ms | 12.62 ms | 14.40 ms | 16.59 ms | 134.50 ms | 3034.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 16.72 ms | 12.66 ms | 22.06 ms | 84.28 ms | 914.79 ms | 36624.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 34.01 ms | 12.74 ms | 27.85 ms | 738.05 ms | 3462.34 ms | 156339.00 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 15.87 ms | 12.91 ms | 29.36 ms | 48.70 ms | 296.41 ms | 11597.00 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.5) | 19.97 ms | 16.89 ms | 34.83 ms | 49.33 ms | 77.20 ms | 10121.33 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 18.09 ms | 17.35 ms | 28.56 ms | 41.19 ms | 70.06 ms | 7913.67 | 
| node (11.10) | [hapi](http://hapijs.com) (18.1) | 28.66 ms | 20.71 ms | 34.40 ms | 270.67 ms | 1038.23 ms | 53601.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 22.31 ms | 20.85 ms | 36.23 ms | 51.43 ms | 86.17 ms | 10371.67 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 28.69 ms | 23.05 ms | 40.39 ms | 113.58 ms | 1070.67 ms | 42409.33 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 28.04 ms | 25.34 ms | 37.12 ms | 44.33 ms | 469.99 ms | 16855.00 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 34.84 ms | 25.68 ms | 73.71 ms | 110.01 ms | 401.45 ms | 25174.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 28.94 ms | 25.73 ms | 41.07 ms | 54.36 ms | 545.34 ms | 19787.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 31.02 ms | 26.85 ms | 43.55 ms | 78.15 ms | 546.51 ms | 21675.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 27.52 ms | 27.40 ms | 36.87 ms | 43.86 ms | 244.91 ms | 8489.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 37.21 ms | 30.95 ms | 50.88 ms | 164.29 ms | 907.39 ms | 38986.67 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 32.12 ms | 33.05 ms | 42.11 ms | 50.65 ms | 265.19 ms | 11342.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 35.37 ms | 34.71 ms | 46.01 ms | 94.09 ms | 359.07 ms | 17725.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 41.36 ms | 35.59 ms | 72.84 ms | 128.71 ms | 235.40 ms | 25105.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 55.27 ms | 46.23 ms | 101.00 ms | 143.20 ms | 677.28 ms | 33289.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 52.80 ms | 48.60 ms | 89.83 ms | 132.14 ms | 201.64 ms | 27966.00 | 
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.3) | 73.16 ms | 68.24 ms | 124.47 ms | 166.96 ms | 240.67 ms | 35034.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 78.94 ms | 77.29 ms | 100.38 ms | 120.79 ms | 569.32 ms | 22673.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (vibora) (python)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 316820.00 | 183.28 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 304006.67 | 363.85 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 293230.67 | 333.38 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 254048.33 | 288.42 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 246312.67 | 239.17 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 228915.33 | 459.99 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 227747.00 | 367.61 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 222536.00 | 209.36 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 210902.00 | 225.08 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 208351.67 | 426.50 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 207216.67 | 119.86 MB |
| java (8) | [act](http://actframework.org) (1.8) | 198461.00 | 387.46 MB |
| node (11.10) | [restana](http://github.com/jkyberneees/ana) (2.9) | 179852.00 | 269.41 MB |
| node (11.10) | [polka](http://github.com/lukeed/polka) (0.5) | 168039.00 | 252.02 MB |
| node (11.10) | [rayo](http://rayo.js.org) (1.2) | 163519.00 | 245.21 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 149003.33 | 188.19 MB |
| node (11.10) | [fastify](http://fastify.io) (1.14) | 146279.67 | 344.94 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 146203.67 | 238.26 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 145264.33 | 194.58 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 142155.33 | 191.44 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 137386.33 | 184.33 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 128171.67 | 225.06 MB |
| go (1.11) | [echo](http://echo.labstack.com) (4.0) | 128090.67 | 224.77 MB |
| node (11.10) | [foxify](http://foxify.js.org) (0.10) | 128057.67 | 269.33 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 124421.00 | 166.65 MB |
| node (11.10) | [koa](http://koajs.com) (2.7) | 122896.67 | 260.40 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 120577.00 | 162.85 MB |
| node (11.10) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 114538.67 | 171.60 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 113192.33 | 290.63 MB |
| node (11.10) | [restify](http://restify.com) (8.0) | 104250.67 | 183.05 MB |
| node (11.10) | [express](http://expressjs.com) (4.16) | 100628.00 | 246.56 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 97188.33 | 239.52 MB |
| go (1.11) | [gf](http://goframe.org) (1.5) | 94340.67 | 143.33 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 91210.33 | 181.71 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 80571.67 | 75.78 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 78802.67 | 169.87 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 72822.00 | 127.38 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 72136.33 | 358.17 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 71892.00 | 357.20 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 71845.33 | 154.09 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 70276.00 | 117.70 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 69165.67 | 343.25 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 65332.00 | 161.88 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 63620.33 | 315.38 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 58813.33 | 305.02 MB |
| c (99) | [kore](http://kore.io) (3.1) | 56834.00 | 154.10 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 54913.33 | 105.95 MB |
| php (7.3) | [laravel](http://laravel.com) (5.7) | 50853.33 | 264.95 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.5) | 50230.33 | 108.17 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 45228.67 | 102.61 MB |
| node (11.10) | [hapi](http://hapijs.com) (18.1) | 44456.67 | 115.14 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 38645.00 | 36.82 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 38195.67 | 70.78 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 37194.00 | 62.77 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 36515.33 | 34.22 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 36089.33 | 33.81 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 34822.33 | 42.79 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 32488.33 | 79.98 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 31827.67 | 59.18 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 31579.67 | 51.47 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 28736.33 | 52.47 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 28691.00 | 16.54 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 28578.67 | 46.57 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 25152.67 | 44.82 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 19653.67 | 11.34 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 19137.00 | 38.19 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 18266.00 | 52.95 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 17965.67 | 135.83 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 14958.00 | 38.77 MB |
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.3) | 13624.67 | 29.67 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 12369.00 | 36.57 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3885.00 | 11.87 MB |
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

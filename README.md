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
Last update: 2019-02-28
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: slim (php)


:five: laravel (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.08 ms | 0.09 ms | 0.12 ms | 0.16 ms | 5.88 ms | 57.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.17) | 3.63 ms | 0.19 ms | 13.57 ms | 35.98 ms | 95.17 ms | 7781.67 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.56 ms | 0.24 ms | 17.34 ms | 41.14 ms | 101.54 ms | 9268.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 155.57 ms | 0.37 ms | 260.18 ms | 3492.86 ms | 6708.36 ms | 578721.33 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 125.50 ms | 0.37 ms | 297.81 ms | 2405.81 ms | 6901.47 ms | 458184.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 6.96 ms | 0.40 ms | 24.27 ms | 53.37 ms | 133.02 ms | 12338.00 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.40 ms | 0.40 ms | 0.62 ms | 0.96 ms | 12.30 ms | 224.33 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 7.43 ms | 0.42 ms | 26.23 ms | 53.66 ms | 130.79 ms | 12893.33 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 8.87 ms | 0.55 ms | 28.98 ms | 58.68 ms | 132.30 ms | 14053.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 114.21 ms | 1.76 ms | 4.29 ms | 3738.32 ms | 6593.28 ms | 622620.00 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 147.42 ms | 1.93 ms | 286.76 ms | 2935.34 ms | 6375.14 ms | 539996.00 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 146.90 ms | 2.25 ms | 278.24 ms | 3269.41 ms | 6774.79 ms | 533011.67 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 97.29 ms | 2.44 ms | 204.93 ms | 2171.42 ms | 5729.07 ms | 401643.33 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 110.12 ms | 2.46 ms | 229.31 ms | 2463.01 ms | 5013.13 ms | 413493.00 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 3.69 ms | 2.79 ms | 7.99 ms | 16.77 ms | 36.59 ms | 3491.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.61 ms | 2.89 ms | 7.12 ms | 15.43 ms | 35.06 ms | 3235.67 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 39.21 ms | 2.95 ms | 131.87 ms | 368.77 ms | 1075.92 ms | 78130.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 6.47 ms | 3.23 ms | 7.87 ms | 45.88 ms | 722.37 ms | 29991.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.64 ms | 3.67 ms | 9.38 ms | 17.50 ms | 49.15 ms | 3772.33 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 3.84 ms | 3.91 ms | 6.43 ms | 8.57 ms | 33.94 ms | 2043.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.34 ms | 3.98 ms | 6.75 ms | 14.13 ms | 62.49 ms | 2659.00 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.6) | 4.38 ms | 4.16 ms | 7.44 ms | 14.19 ms | 33.37 ms | 2751.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.82 ms | 4.21 ms | 5.54 ms | 8.05 ms | 90.15 ms | 2275.33 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 4.94 ms | 4.41 ms | 9.23 ms | 20.08 ms | 159.88 ms | 4853.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 6.36 ms | 4.49 ms | 10.91 ms | 50.51 ms | 121.06 ms | 8911.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 5.36 ms | 4.50 ms | 10.16 ms | 21.39 ms | 70.27 ms | 4376.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.24 ms | 5.21 ms | 7.96 ms | 14.17 ms | 38.54 ms | 2720.67 | 
| node (11.10) | [restana](http://github.com/jkyberneees/ana) (2.9) | 6.62 ms | 5.22 ms | 9.99 ms | 20.35 ms | 257.44 ms | 7853.67 | 
| node (11.10) | [rayo](http://rayo.js.org) (1.2) | 7.28 ms | 5.53 ms | 10.74 ms | 21.89 ms | 326.25 ms | 9857.67 | 
| node (11.10) | [polka](http://github.com/lukeed/polka) (0.5) | 7.14 ms | 5.54 ms | 10.87 ms | 22.48 ms | 263.29 ms | 7923.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.50 ms | 5.73 ms | 10.68 ms | 21.32 ms | 193.64 ms | 5019.67 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 7.29 ms | 5.73 ms | 11.59 ms | 23.87 ms | 304.14 ms | 8735.67 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 7.16 ms | 5.82 ms | 12.11 ms | 24.04 ms | 108.50 ms | 4489.67 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 7.31 ms | 5.95 ms | 12.24 ms | 24.41 ms | 152.05 ms | 4522.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 7.19 ms | 6.02 ms | 9.78 ms | 19.71 ms | 426.52 ms | 11793.00 | 
| go (1.11) | [kami](http://github.com/guregu/kami) (2.2) | 7.68 ms | 6.40 ms | 12.60 ms | 25.04 ms | 57.70 ms | 4384.00 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 7.94 ms | 6.48 ms | 13.45 ms | 26.16 ms | 141.34 ms | 5134.33 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 8.03 ms | 6.55 ms | 13.37 ms | 27.02 ms | 219.42 ms | 5868.33 | 
| go (1.11) | [echo](http://echo.labstack.com) (4.0) | 8.24 ms | 6.58 ms | 13.80 ms | 28.21 ms | 187.67 ms | 7105.67 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 8.50 ms | 6.91 ms | 14.17 ms | 28.28 ms | 236.03 ms | 7776.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 193.65 ms | 7.70 ms | 62.78 ms | 4676.68 ms | 7923.04 ms | 810306.33 | 
| node (11.10) | [foxify](http://foxify.js.org) (0.10) | 8.87 ms | 8.08 ms | 13.98 ms | 26.16 ms | 313.02 ms | 9906.33 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 10.29 ms | 8.34 ms | 18.85 ms | 38.58 ms | 411.23 ms | 12645.33 | 
| node (11.10) | [fastify](http://fastify.io) (2.0) | 12.41 ms | 8.61 ms | 15.93 ms | 88.41 ms | 604.14 ms | 27218.67 | 
| node (11.10) | [koa](http://koajs.com) (2.7) | 10.04 ms | 8.71 ms | 14.79 ms | 34.04 ms | 422.32 ms | 13842.67 | 
| node (11.10) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 12.90 ms | 8.91 ms | 18.79 ms | 87.45 ms | 640.40 ms | 28081.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 11.61 ms | 9.19 ms | 21.77 ms | 43.71 ms | 287.87 ms | 9921.00 | 
| node (11.10) | [express](http://expressjs.com) (4.16) | 12.67 ms | 9.73 ms | 18.12 ms | 52.89 ms | 541.11 ms | 20771.00 | 
| go (1.11) | [gf](http://goframe.org) (1.5) | 11.06 ms | 9.84 ms | 16.46 ms | 34.38 ms | 314.19 ms | 8879.00 | 
| node (11.10) | [restify](http://restify.com) (8.0) | 11.74 ms | 9.93 ms | 16.15 ms | 38.02 ms | 327.76 ms | 10875.33 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 16.73 ms | 11.82 ms | 36.41 ms | 64.99 ms | 220.72 ms | 13722.00 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 14.46 ms | 12.47 ms | 24.83 ms | 41.30 ms | 107.54 ms | 7782.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 12.74 ms | 12.95 ms | 14.79 ms | 17.19 ms | 44.86 ms | 1990.33 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.2) | 22.21 ms | 13.43 ms | 24.34 ms | 298.12 ms | 1397.30 ms | 70670.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 16.33 ms | 13.72 ms | 27.61 ms | 49.81 ms | 879.32 ms | 26285.00 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 19.03 ms | 17.70 ms | 30.86 ms | 45.51 ms | 98.65 ms | 8889.00 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.6) | 21.80 ms | 18.78 ms | 37.17 ms | 54.81 ms | 104.09 ms | 11018.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 25.50 ms | 21.14 ms | 48.87 ms | 68.11 ms | 97.62 ms | 15221.33 | 
| node (11.10) | [hapi](http://hapijs.com) (18.1) | 33.94 ms | 23.19 ms | 38.71 ms | 368.08 ms | 1256.70 ms | 72155.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 32.27 ms | 24.71 ms | 43.82 ms | 181.84 ms | 1078.48 ms | 51833.33 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 27.83 ms | 24.98 ms | 38.72 ms | 49.04 ms | 240.45 ms | 9221.33 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 35.13 ms | 25.52 ms | 70.65 ms | 99.27 ms | 271.21 ms | 23309.00 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 28.52 ms | 26.01 ms | 37.52 ms | 48.02 ms | 394.14 ms | 12327.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 38.06 ms | 29.86 ms | 52.21 ms | 261.73 ms | 585.71 ms | 40266.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 33.97 ms | 30.76 ms | 49.03 ms | 66.34 ms | 396.86 ms | 15802.00 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 32.24 ms | 32.88 ms | 42.63 ms | 50.33 ms | 210.63 ms | 10312.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 41.11 ms | 34.18 ms | 51.54 ms | 178.62 ms | 575.66 ms | 36108.00 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 35.61 ms | 34.23 ms | 48.10 ms | 58.16 ms | 481.05 ms | 18108.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 50.09 ms | 42.33 ms | 94.77 ms | 174.86 ms | 336.31 ms | 35906.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 57.95 ms | 52.37 ms | 95.58 ms | 134.05 ms | 187.94 ms | 27375.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 59.04 ms | 56.85 ms | 70.96 ms | 126.64 ms | 718.70 ms | 29334.67 | 
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.3) | 77.97 ms | 70.06 ms | 132.80 ms | 174.52 ms | 287.66 ms | 37169.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 88.75 ms | 88.67 ms | 113.16 ms | 134.23 ms | 498.66 ms | 22226.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (actix-web) (rust)


:three: (agoo-c) (c)


:four: (kore) (c)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 278765.00 | 333.82 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 277726.33 | 315.84 MB |
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 256969.00 | 148.14 MB |
| c (99) | [kore](http://kore.io) (3.1) | 238152.33 | 619.01 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 230709.67 | 223.98 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 230566.00 | 261.83 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.6) | 214658.33 | 201.62 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 212598.33 | 341.88 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 206167.33 | 422.02 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 200078.33 | 401.88 MB |
| java (8) | [act](http://actframework.org) (1.8) | 199296.00 | 389.38 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 189826.00 | 202.29 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 185700.00 | 107.29 MB |
| node (11.10) | [restana](http://github.com/jkyberneees/ana) (2.9) | 151984.00 | 227.23 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 148058.00 | 186.65 MB |
| node (11.10) | [rayo](http://rayo.js.org) (1.2) | 140927.67 | 211.03 MB |
| node (11.10) | [polka](http://github.com/lukeed/polka) (0.5) | 140478.00 | 210.35 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 139048.00 | 226.56 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 138972.67 | 186.35 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 135826.33 | 181.93 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 133324.67 | 178.57 MB |
| go (1.11) | [kami](http://github.com/guregu/kami) (2.2) | 128629.00 | 170.63 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 124206.33 | 218.14 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 122935.67 | 165.71 MB |
| go (1.11) | [echo](http://echo.labstack.com) (4.0) | 122391.67 | 215.01 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 117398.67 | 157.07 MB |
| node (11.10) | [foxify](http://foxify.js.org) (0.10) | 114828.33 | 240.98 MB |
| node (11.10) | [fastify](http://fastify.io) (2.0) | 113817.33 | 278.23 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 105946.33 | 272.32 MB |
| node (11.10) | [koa](http://koajs.com) (2.7) | 105725.00 | 223.86 MB |
| node (11.10) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 104843.33 | 157.01 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 93604.00 | 186.24 MB |
| go (1.11) | [gf](http://goframe.org) (1.5) | 91970.67 | 139.41 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 91445.00 | 225.36 MB |
| node (11.10) | [express](http://expressjs.com) (4.16) | 88783.33 | 217.48 MB |
| node (11.10) | [restify](http://restify.com) (8.0) | 88618.00 | 155.58 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 78114.67 | 73.48 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 74838.00 | 372.12 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 69945.33 | 150.82 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 69506.33 | 345.69 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 69211.67 | 148.52 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 69201.00 | 343.70 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 68209.00 | 119.30 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 66776.67 | 165.65 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 65902.67 | 327.06 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.2) | 65864.67 | 110.19 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 63752.67 | 331.39 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 52601.67 | 101.64 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 49569.67 | 258.32 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 49413.33 | 78.45 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.6) | 46468.00 | 100.49 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 41930.00 | 95.15 MB |
| node (11.10) | [hapi](http://hapijs.com) (18.1) | 40613.00 | 105.26 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.17) | 35618.00 | 34.00 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 35377.67 | 33.19 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 35094.00 | 32.89 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 34840.33 | 64.63 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 32160.33 | 59.85 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 30858.33 | 50.26 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 29244.33 | 72.09 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 29041.33 | 35.68 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 28182.33 | 16.27 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 28061.00 | 51.28 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 25648.00 | 41.79 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 21389.00 | 38.12 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 18349.33 | 10.61 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 17298.00 | 34.44 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 17171.33 | 130.10 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 16754.67 | 48.56 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 14379.67 | 37.36 MB |
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.3) | 12817.00 | 27.94 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 10993.33 | 32.48 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3269.67 | 10.06 MB |
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

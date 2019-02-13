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
Last update: 2019-02-12
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
threads: 9, connections: 1000
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: laravel (php)


:five: hanami (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.06 ms | 0.06 ms | 0.09 ms | 0.12 ms | 1.23 ms | 23.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 3.75 ms | 0.22 ms | 13.51 ms | 30.95 ms | 90.31 ms | 7048.33 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.58 ms | 0.24 ms | 16.71 ms | 35.94 ms | 90.75 ms | 8414.67 | 
| php (7.3) | [laravel](http://laravel.com) (5.7) | 94.52 ms | 0.30 ms | 256.22 ms | 1455.09 ms | 6961.95 ms | 346578.00 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 6.82 ms | 0.37 ms | 23.87 ms | 47.88 ms | 133.56 ms | 11650.00 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.38 ms | 0.38 ms | 0.62 ms | 0.89 ms | 24.58 ms | 298.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 6.83 ms | 0.41 ms | 22.60 ms | 46.03 ms | 106.65 ms | 10981.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 10.02 ms | 0.66 ms | 31.30 ms | 62.51 ms | 167.49 ms | 15060.33 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 112.93 ms | 0.96 ms | 241.10 ms | 2281.47 ms | 6814.31 ms | 411092.67 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 150.65 ms | 1.12 ms | 249.83 ms | 3529.85 ms | 6786.93 ms | 591365.00 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 102.68 ms | 1.25 ms | 207.98 ms | 2229.34 ms | 5893.29 ms | 404852.00 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 100.04 ms | 1.48 ms | 197.43 ms | 2164.41 ms | 5658.82 ms | 386666.33 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 111.00 ms | 1.48 ms | 217.87 ms | 2293.43 ms | 5656.04 ms | 418334.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 139.81 ms | 1.56 ms | 42.17 ms | 3900.56 ms | 6594.08 ms | 675378.33 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.00 ms | 2.08 ms | 6.38 ms | 14.08 ms | 44.50 ms | 3042.67 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 3.34 ms | 2.27 ms | 7.20 ms | 16.69 ms | 36.12 ms | 3377.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 2.91 ms | 2.45 ms | 5.55 ms | 11.77 ms | 29.00 ms | 2388.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.86 ms | 3.02 ms | 8.15 ms | 15.76 ms | 37.40 ms | 3415.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.61 ms | 3.04 ms | 5.83 ms | 11.58 ms | 97.86 ms | 2460.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.25 ms | 3.27 ms | 5.36 ms | 7.89 ms | 23.39 ms | 1802.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.19 ms | 3.42 ms | 7.06 ms | 12.86 ms | 32.48 ms | 2517.67 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 3.79 ms | 3.48 ms | 6.64 ms | 12.75 ms | 28.00 ms | 2564.33 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 4.33 ms | 3.77 ms | 8.18 ms | 15.19 ms | 32.44 ms | 3097.33 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 4.68 ms | 4.07 ms | 8.91 ms | 19.79 ms | 191.90 ms | 4348.00 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.7) | 5.92 ms | 4.21 ms | 10.43 ms | 53.74 ms | 118.08 ms | 9112.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 7.43 ms | 4.90 ms | 13.83 ms | 27.02 ms | 207.05 ms | 6515.67 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 6.11 ms | 5.03 ms | 10.03 ms | 20.88 ms | 229.56 ms | 6271.00 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 6.21 ms | 5.22 ms | 10.29 ms | 20.57 ms | 105.78 ms | 3906.33 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 41.15 ms | 5.24 ms | 135.66 ms | 353.63 ms | 962.59 ms | 75963.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 5.36 ms | 5.26 ms | 9.01 ms | 11.50 ms | 43.32 ms | 2791.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 6.76 ms | 5.44 ms | 11.25 ms | 22.99 ms | 233.42 ms | 6380.67 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 6.75 ms | 5.47 ms | 11.24 ms | 22.81 ms | 170.45 ms | 5459.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.50 ms | 5.56 ms | 10.94 ms | 21.39 ms | 123.81 ms | 4543.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 6.29 ms | 5.58 ms | 9.49 ms | 16.72 ms | 212.78 ms | 4190.67 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 7.11 ms | 5.74 ms | 11.81 ms | 24.05 ms | 217.32 ms | 5696.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 7.90 ms | 5.87 ms | 12.05 ms | 27.73 ms | 371.79 ms | 13746.00 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 7.29 ms | 5.91 ms | 11.93 ms | 23.94 ms | 187.56 ms | 6038.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 9.13 ms | 7.33 ms | 16.55 ms | 33.83 ms | 213.06 ms | 7171.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 180.85 ms | 7.81 ms | 176.50 ms | 3964.58 ms | 7865.47 ms | 727906.33 | 
| go (1.11) | [gf](http://gfer.me) (1.4) | 10.84 ms | 8.66 ms | 14.92 ms | 43.12 ms | 496.36 ms | 19246.33 | 
| python (3.7) | [starlette](http://starlette.io) (0.10) | 10.47 ms | 9.46 ms | 16.62 ms | 24.90 ms | 108.36 ms | 5017.67 | 
| node (11.9) | [restana](http://github.com/jkyberneees/ana) (2.8) | 13.93 ms | 10.16 ms | 25.61 ms | 54.16 ms | 434.93 ms | 16150.00 | 
| node (11.9) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 17.20 ms | 10.90 ms | 25.06 ms | 159.07 ms | 804.82 ms | 39900.33 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 14.18 ms | 11.04 ms | 19.50 ms | 39.37 ms | 688.12 ms | 25545.33 | 
| node (11.9) | [polka](http://github.com/lukeed/polka) (0.5) | 15.89 ms | 11.28 ms | 26.99 ms | 67.25 ms | 621.10 ms | 25318.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 11.41 ms | 11.38 ms | 13.48 ms | 15.56 ms | 80.36 ms | 1712.67 | 
| node (11.9) | [rayo](http://rayo.js.org) (1.2) | 15.32 ms | 11.53 ms | 26.14 ms | 55.07 ms | 568.63 ms | 20447.00 | 
| node (11.9) | [foxify](http://foxify.js.org) (0.10) | 16.82 ms | 12.03 ms | 28.12 ms | 73.76 ms | 610.17 ms | 25192.33 | 
| node (11.9) | [fastify](http://fastify.io) (1.14) | 19.41 ms | 12.83 ms | 29.40 ms | 157.42 ms | 831.41 ms | 39774.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 16.14 ms | 14.66 ms | 29.23 ms | 51.35 ms | 383.93 ms | 13620.33 | 
| node (11.9) | [koa](http://koajs.com) (2.7) | 23.22 ms | 14.74 ms | 30.11 ms | 286.41 ms | 1065.56 ms | 57833.67 | 
| node (11.9) | [restify](http://restify.com) (7.7) | 20.44 ms | 16.55 ms | 33.75 ms | 64.16 ms | 457.02 ms | 19091.67 | 
| node (11.9) | [express](http://expressjs.com) (4.16) | 23.94 ms | 17.35 ms | 35.47 ms | 154.63 ms | 880.31 ms | 41552.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 20.09 ms | 18.84 ms | 31.99 ms | 44.93 ms | 71.34 ms | 8550.67 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 28.13 ms | 20.94 ms | 33.69 ms | 203.52 ms | 1370.32 ms | 61809.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 32.17 ms | 21.46 ms | 72.13 ms | 107.04 ms | 245.09 ms | 24925.67 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 24.43 ms | 22.07 ms | 36.57 ms | 43.49 ms | 388.20 ms | 11438.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 27.04 ms | 23.48 ms | 36.60 ms | 45.63 ms | 474.37 ms | 18282.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 25.10 ms | 24.09 ms | 33.39 ms | 39.47 ms | 115.66 ms | 6521.00 | 
| node (11.9) | [hapi](http://hapijs.com) (18.1) | 53.73 ms | 31.01 ms | 57.24 ms | 790.65 ms | 1837.19 ms | 130995.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 33.98 ms | 33.13 ms | 40.63 ms | 52.19 ms | 552.04 ms | 17381.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 32.80 ms | 33.78 ms | 42.60 ms | 51.92 ms | 400.31 ms | 13709.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 44.77 ms | 36.81 ms | 84.30 ms | 148.67 ms | 285.31 ms | 29826.00 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 40.34 ms | 37.22 ms | 46.73 ms | 209.47 ms | 753.98 ms | 43731.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 48.14 ms | 44.96 ms | 82.61 ms | 126.57 ms | 178.13 ms | 25525.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 54.89 ms | 44.98 ms | 95.68 ms | 129.48 ms | 407.77 ms | 28123.67 | 
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.1) | 63.06 ms | 57.65 ms | 102.51 ms | 140.61 ms | 227.96 ms | 27880.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 77.10 ms | 75.94 ms | 95.64 ms | 117.27 ms | 563.05 ms | 22872.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (agoo-c) (c)


:three: (actix-web) (rust)


:four: (vibora) (python)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 352576.33 | 422.11 MB |
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 344531.00 | 199.31 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 318195.67 | 361.78 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 283093.00 | 321.33 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 272741.00 | 264.80 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 256615.33 | 412.83 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 252513.67 | 507.33 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 250599.33 | 235.62 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 229173.33 | 245.24 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 217679.00 | 445.53 MB |
| java (8) | [act](http://actframework.org) (1.8) | 214493.33 | 418.41 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.7) | 209444.33 | 121.05 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 163941.00 | 220.24 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 157678.67 | 210.87 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 157088.33 | 198.41 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 148388.33 | 260.25 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 147713.33 | 240.77 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 146848.33 | 198.09 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 139039.00 | 357.16 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 138587.67 | 243.27 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 137715.67 | 184.62 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 136396.67 | 182.58 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 135648.33 | 269.20 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 114641.33 | 282.43 MB |
| go (1.11) | [gf](http://gfer.me) (1.4) | 104090.00 | 158.38 MB |
| python (3.7) | [starlette](http://starlette.io) (0.10) | 93785.33 | 202.07 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 86581.33 | 81.27 MB |
| node (11.9) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 81745.00 | 122.41 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 81231.67 | 402.93 MB |
| node (11.9) | [restana](http://github.com/jkyberneees/ana) (2.8) | 80867.33 | 121.10 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 80540.67 | 399.99 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 78187.00 | 388.37 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 76486.33 | 128.24 MB |
| node (11.9) | [polka](http://github.com/lukeed/polka) (0.5) | 74442.00 | 111.29 MB |
| node (11.9) | [rayo](http://rayo.js.org) (1.2) | 74127.00 | 110.86 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 71865.33 | 373.25 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 71151.00 | 353.02 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 70108.67 | 150.27 MB |
| node (11.9) | [fastify](http://fastify.io) (1.14) | 69537.00 | 166.73 MB |
| node (11.9) | [foxify](http://foxify.js.org) (0.10) | 69417.33 | 145.91 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 68816.33 | 120.43 MB |
| node (11.9) | [koa](http://koajs.com) (2.7) | 60012.00 | 126.72 MB |
| c (99) | [kore](http://kore.io) (3.1) | 58370.33 | 158.19 MB |
| php (7.3) | [laravel](http://laravel.com) (5.7) | 56195.33 | 292.70 MB |
| node (11.9) | [restify](http://restify.com) (7.7) | 52590.67 | 92.03 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 49988.33 | 113.41 MB |
| node (11.9) | [express](http://expressjs.com) (4.16) | 49936.67 | 121.96 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 48961.67 | 79.04 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 42456.67 | 78.70 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 40494.67 | 65.99 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 39742.33 | 37.24 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 37812.67 | 35.42 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 34655.67 | 85.47 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 34123.33 | 32.50 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 30261.33 | 55.26 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 29961.67 | 36.68 MB |
| node (11.9) | [hapi](http://hapijs.com) (18.1) | 29275.33 | 75.56 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 28077.67 | 16.20 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 27880.67 | 45.42 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 23513.67 | 41.87 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 21011.00 | 41.92 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 18737.00 | 10.80 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 18735.00 | 141.59 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 18513.67 | 53.67 MB |
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.1) | 15810.33 | 34.43 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 12758.67 | 33.10 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 12652.33 | 37.40 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3109.33 | 9.51 MB |
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

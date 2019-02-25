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
Last update: 2019-02-24
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


:five: iron (rust)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.08 ms | 0.08 ms | 0.12 ms | 0.14 ms | 3.55 ms | 37.67 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 3.32 ms | 0.18 ms | 12.12 ms | 29.29 ms | 74.83 ms | 6533.67 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.17 ms | 0.22 ms | 15.30 ms | 34.32 ms | 86.02 ms | 7891.67 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 119.02 ms | 0.32 ms | 238.50 ms | 2597.81 ms | 6900.22 ms | 451095.33 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.36 ms | 0.36 ms | 0.60 ms | 0.86 ms | 34.73 ms | 310.33 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 6.50 ms | 0.38 ms | 21.90 ms | 45.36 ms | 106.18 ms | 10734.33 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 7.52 ms | 0.41 ms | 25.32 ms | 51.43 ms | 129.58 ms | 12419.67 | 
| php (7.3) | [laravel](http://laravel.com) (5.7) | 118.16 ms | 0.54 ms | 308.28 ms | 2022.00 ms | 6845.67 ms | 414052.33 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 8.28 ms | 0.56 ms | 25.82 ms | 50.59 ms | 122.47 ms | 12303.67 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 99.64 ms | 0.67 ms | 225.72 ms | 2124.18 ms | 5761.12 ms | 380619.67 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 177.21 ms | 1.15 ms | 291.39 ms | 4149.80 ms | 6854.59 ms | 672876.33 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 119.01 ms | 1.23 ms | 229.78 ms | 2713.21 ms | 5797.84 ms | 459188.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 43.88 ms | 1.57 ms | 3.42 ms | 1282.25 ms | 5497.86 ms | 276202.00 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 136.01 ms | 1.57 ms | 227.67 ms | 3040.42 ms | 6011.42 ms | 516742.00 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 3.45 ms | 2.46 ms | 6.87 ms | 15.45 ms | 118.03 ms | 4926.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 2.89 ms | 2.64 ms | 4.82 ms | 11.28 ms | 72.28 ms | 2253.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.59 ms | 2.66 ms | 7.58 ms | 16.42 ms | 46.08 ms | 3510.33 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 32.50 ms | 2.82 ms | 109.04 ms | 291.54 ms | 821.68 ms | 62376.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.33 ms | 3.44 ms | 8.97 ms | 16.55 ms | 37.38 ms | 3620.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.96 ms | 3.48 ms | 6.19 ms | 12.84 ms | 56.81 ms | 2537.67 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 3.92 ms | 3.56 ms | 6.93 ms | 13.82 ms | 31.89 ms | 2731.33 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 4.77 ms | 3.90 ms | 8.72 ms | 19.92 ms | 282.85 ms | 7780.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 4.81 ms | 3.97 ms | 8.45 ms | 16.84 ms | 106.29 ms | 6307.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 5.38 ms | 4.11 ms | 9.14 ms | 41.85 ms | 120.33 ms | 8159.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.78 ms | 4.17 ms | 5.52 ms | 7.78 ms | 83.46 ms | 2329.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.79 ms | 4.58 ms | 7.46 ms | 13.20 ms | 63.56 ms | 2674.00 | 
| node (11.10) | [restana](http://github.com/jkyberneees/ana) (2.9) | 5.68 ms | 4.68 ms | 9.22 ms | 17.47 ms | 249.29 ms | 7326.00 | 
| node (11.10) | [polka](http://github.com/lukeed/polka) (0.5) | 6.14 ms | 4.78 ms | 9.62 ms | 19.35 ms | 288.93 ms | 8829.67 | 
| node (11.10) | [rayo](http://rayo.js.org) (1.2) | 5.86 ms | 4.78 ms | 9.49 ms | 17.12 ms | 221.35 ms | 6177.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 21.23 ms | 4.82 ms | 8.32 ms | 752.86 ms | 2353.39 ms | 148779.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 6.46 ms | 5.41 ms | 10.69 ms | 20.95 ms | 161.36 ms | 4354.00 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 7.03 ms | 5.58 ms | 11.37 ms | 23.98 ms | 280.26 ms | 7675.00 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 6.90 ms | 5.59 ms | 11.06 ms | 22.94 ms | 176.06 ms | 6489.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.52 ms | 5.65 ms | 10.93 ms | 22.72 ms | 94.70 ms | 4784.67 | 
| node (11.10) | [foxify](http://foxify.js.org) (0.10) | 7.43 ms | 5.83 ms | 10.77 ms | 20.63 ms | 292.32 ms | 8783.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 6.71 ms | 6.06 ms | 9.63 ms | 17.33 ms | 208.07 ms | 3992.00 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 7.57 ms | 6.12 ms | 12.44 ms | 25.98 ms | 232.91 ms | 6917.00 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 7.68 ms | 6.12 ms | 12.59 ms | 26.06 ms | 202.76 ms | 7529.33 | 
| go (1.11) | [echo](http://echo.labstack.com) (4.0) | 7.63 ms | 6.19 ms | 12.59 ms | 25.39 ms | 229.03 ms | 6139.33 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 7.73 ms | 6.30 ms | 12.74 ms | 25.50 ms | 281.97 ms | 7247.00 | 
| node (11.10) | [fastify](http://fastify.io) (1.14) | 8.76 ms | 6.42 ms | 12.28 ms | 48.71 ms | 404.47 ms | 15888.00 | 
| node (11.10) | [koa](http://koajs.com) (2.7) | 8.29 ms | 7.14 ms | 12.64 ms | 23.98 ms | 323.50 ms | 10422.00 | 
| node (11.10) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 9.56 ms | 7.26 ms | 16.20 ms | 42.78 ms | 351.14 ms | 12315.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 162.22 ms | 7.30 ms | 170.72 ms | 3648.34 ms | 6862.59 ms | 640302.33 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 8.68 ms | 8.26 ms | 14.98 ms | 24.96 ms | 162.50 ms | 5231.00 | 
| node (11.10) | [restify](http://restify.com) (8.0) | 8.98 ms | 8.39 ms | 12.66 ms | 24.00 ms | 221.28 ms | 6403.33 | 
| node (11.10) | [express](http://expressjs.com) (4.16) | 9.62 ms | 8.49 ms | 13.85 ms | 30.66 ms | 322.30 ms | 11540.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 10.21 ms | 8.68 ms | 18.72 ms | 33.85 ms | 215.95 ms | 8144.33 | 
| go (1.11) | [gf](http://goframe.org) (1.5) | 10.36 ms | 9.05 ms | 13.42 ms | 31.09 ms | 447.91 ms | 15206.33 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 13.02 ms | 11.80 ms | 23.48 ms | 38.55 ms | 77.82 ms | 8072.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 11.97 ms | 12.19 ms | 13.78 ms | 15.72 ms | 106.99 ms | 3017.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 17.98 ms | 12.36 ms | 21.59 ms | 153.85 ms | 1090.20 ms | 49979.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 33.06 ms | 12.52 ms | 26.30 ms | 798.69 ms | 2517.76 ms | 151783.67 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 15.08 ms | 13.07 ms | 23.25 ms | 50.82 ms | 295.40 ms | 10493.33 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.5) | 19.70 ms | 16.30 ms | 36.91 ms | 58.78 ms | 104.78 ms | 12526.33 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.11) | 19.59 ms | 16.82 ms | 35.08 ms | 55.47 ms | 171.99 ms | 12985.33 | 
| node (11.10) | [hapi](http://hapijs.com) (18.1) | 27.91 ms | 18.77 ms | 33.33 ms | 288.31 ms | 972.38 ms | 54713.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 22.62 ms | 19.03 ms | 42.37 ms | 60.16 ms | 101.29 ms | 13056.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 23.78 ms | 21.96 ms | 30.28 ms | 43.20 ms | 239.84 ms | 7126.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 24.34 ms | 21.96 ms | 33.15 ms | 51.38 ms | 734.67 ms | 18840.33 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 25.80 ms | 23.44 ms | 34.52 ms | 63.92 ms | 235.42 ms | 10434.67 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 27.49 ms | 24.44 ms | 36.23 ms | 49.55 ms | 135.75 ms | 8008.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 30.27 ms | 24.46 ms | 53.01 ms | 79.68 ms | 316.97 ms | 16438.00 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 27.95 ms | 25.91 ms | 39.20 ms | 61.75 ms | 443.20 ms | 13300.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 33.45 ms | 31.41 ms | 43.01 ms | 50.82 ms | 200.60 ms | 10468.00 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 36.72 ms | 33.84 ms | 47.91 ms | 59.14 ms | 191.10 ms | 8477.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 35.02 ms | 34.18 ms | 41.71 ms | 74.74 ms | 484.55 ms | 15888.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 45.22 ms | 37.40 ms | 86.86 ms | 158.10 ms | 305.49 ms | 31898.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 50.41 ms | 44.94 ms | 86.01 ms | 127.72 ms | 243.90 ms | 25206.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 54.40 ms | 47.48 ms | 88.69 ms | 143.38 ms | 817.48 ms | 32133.33 | 
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.3) | 69.91 ms | 64.54 ms | 119.08 ms | 155.87 ms | 225.74 ms | 33532.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 82.03 ms | 82.71 ms | 108.47 ms | 127.59 ms | 373.61 ms | 24711.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (actix-web) (rust)


:three: (japronto) (python)


:four: (vibora) (python)


:five: (onyx) (crystal)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 336805.33 | 194.78 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 301111.33 | 342.39 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 293380.67 | 351.24 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 247110.00 | 280.68 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 244779.33 | 230.37 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 236870.67 | 381.80 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 230969.00 | 224.28 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 224260.33 | 450.44 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 224203.67 | 459.55 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 220664.33 | 127.50 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 219953.33 | 234.27 MB |
| java (8) | [act](http://actframework.org) (1.8) | 206589.00 | 403.44 MB |
| node (11.10) | [restana](http://github.com/jkyberneees/ana) (2.9) | 178351.67 | 267.39 MB |
| node (11.10) | [rayo](http://rayo.js.org) (1.2) | 170429.33 | 255.48 MB |
| node (11.10) | [polka](http://github.com/lukeed/polka) (0.5) | 169097.67 | 253.52 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 161656.33 | 204.35 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 149702.33 | 200.14 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 144930.67 | 194.11 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 143221.67 | 192.35 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 140771.67 | 229.43 MB |
| node (11.10) | [fastify](http://fastify.io) (1.14) | 138802.67 | 325.94 MB |
| node (11.10) | [foxify](http://foxify.js.org) (0.10) | 137178.67 | 288.12 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 131718.67 | 231.08 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 131272.00 | 176.82 MB |
| go (1.11) | [echo](http://echo.labstack.com) (4.0) | 129983.00 | 228.30 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 129033.33 | 171.82 MB |
| node (11.10) | [koa](http://koajs.com) (2.7) | 125161.00 | 264.91 MB |
| node (11.10) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 123924.00 | 185.80 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 114324.00 | 293.70 MB |
| node (11.10) | [restify](http://restify.com) (8.0) | 109503.33 | 192.29 MB |
| node (11.10) | [express](http://expressjs.com) (4.16) | 108821.00 | 266.35 MB |
| go (1.11) | [gf](http://goframe.org) (1.5) | 102997.33 | 155.85 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 101986.33 | 251.29 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 101720.00 | 201.68 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 82982.67 | 77.99 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 78754.00 | 169.47 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 75659.67 | 132.50 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 72923.00 | 362.13 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 72244.67 | 120.95 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 69280.00 | 343.92 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 68758.00 | 341.42 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 68092.00 | 168.55 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 67942.33 | 336.94 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 67885.33 | 352.52 MB |
| c (99) | [kore](http://kore.io) (3.1) | 58204.33 | 157.78 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 57630.33 | 124.18 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 56340.67 | 89.45 MB |
| php (7.3) | [laravel](http://laravel.com) (5.7) | 55188.00 | 287.49 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.5) | 53705.67 | 115.97 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.11) | 52698.33 | 101.60 MB |
| node (11.10) | [hapi](http://hapijs.com) (18.1) | 48445.00 | 125.47 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 46052.33 | 104.41 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 41585.33 | 77.10 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 41437.67 | 38.89 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 38809.67 | 36.38 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 38694.00 | 36.92 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 36698.00 | 68.21 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 36422.33 | 59.37 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 33214.67 | 81.91 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 30620.33 | 17.67 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 29943.33 | 54.66 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 29219.33 | 35.75 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 26843.33 | 43.75 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 23690.33 | 42.17 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 20069.00 | 39.98 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 19679.33 | 11.34 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 18657.33 | 54.02 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 16970.67 | 128.22 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 15456.00 | 40.07 MB |
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.3) | 14311.67 | 31.14 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 11988.67 | 35.32 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3947.67 | 12.09 MB |
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

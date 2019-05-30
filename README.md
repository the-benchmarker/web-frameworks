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
Last update: 2019-05-30
```
OS: Linux (version: 5.0.16-300.fc30.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: iron (rust)


:five: laravel (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.35) | [nickel](http://nickel-org.github.io) (0.11) | 0.09 ms | 0.09 ms | 0.14 ms | 0.20 ms | 2.08 ms | 41.67 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 3.42 ms | 0.19 ms | 12.26 ms | 28.46 ms | 73.44 ms | 6456.33 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.54 ms | 0.24 ms | 16.51 ms | 35.91 ms | 90.94 ms | 8350.67 | 
| rust (1.35) | [iron](http://ironframework.io) (0.6) | 0.42 ms | 0.40 ms | 0.69 ms | 1.09 ms | 11.74 ms | 253.33 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 119.35 ms | 0.43 ms | 317.30 ms | 2102.84 ms | 7164.51 ms | 409357.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 7.03 ms | 0.43 ms | 23.01 ms | 46.92 ms | 103.76 ms | 11154.00 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 149.17 ms | 0.43 ms | 306.68 ms | 3003.72 ms | 5987.95 ms | 515967.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 162.73 ms | 0.44 ms | 295.69 ms | 3513.52 ms | 7060.23 ms | 599172.00 | 
| php (7.3) | [symfony](http://symfony.com) (4.3) | 200.77 ms | 0.44 ms | 368.92 ms | 4248.63 ms | 7282.72 ms | 713696.33 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 8.16 ms | 0.44 ms | 27.40 ms | 56.01 ms | 163.92 ms | 13473.67 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 177.79 ms | 0.44 ms | 316.16 ms | 3826.57 ms | 6940.61 ms | 650707.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 9.18 ms | 0.60 ms | 28.72 ms | 56.94 ms | 157.85 ms | 13832.00 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 157.80 ms | 0.84 ms | 338.72 ms | 3063.81 ms | 7091.93 ms | 558024.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 101.41 ms | 1.97 ms | 30.03 ms | 3333.35 ms | 6594.45 ms | 564718.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 3.43 ms | 2.28 ms | 7.55 ms | 15.31 ms | 150.19 ms | 4191.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.65 ms | 2.96 ms | 7.31 ms | 15.58 ms | 33.94 ms | 3301.33 | 
| rust (1.35) | [actix-web](http://actix.rs) (0.7) | 3.91 ms | 2.96 ms | 8.38 ms | 17.41 ms | 39.15 ms | 3654.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 11.24 ms | 3.53 ms | 8.27 ms | 261.77 ms | 1180.14 ms | 65732.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.57 ms | 3.65 ms | 9.11 ms | 16.61 ms | 53.95 ms | 3679.67 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 38.31 ms | 3.80 ms | 127.44 ms | 336.88 ms | 1052.72 ms | 71785.33 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.65 ms | 4.41 ms | 7.07 ms | 14.71 ms | 57.20 ms | 2656.00 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 6.95 ms | 4.42 ms | 11.68 ms | 75.15 ms | 127.57 ms | 11370.33 | 
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 4.63 ms | 4.52 ms | 7.59 ms | 14.44 ms | 29.87 ms | 2705.00 | 
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 4.80 ms | 4.66 ms | 7.65 ms | 14.58 ms | 32.91 ms | 2660.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.34 ms | 4.77 ms | 5.98 ms | 10.21 ms | 30.38 ms | 1967.67 | 
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 5.04 ms | 4.83 ms | 8.11 ms | 14.72 ms | 32.10 ms | 2699.33 | 
| rust (1.35) | [gotham](http://gotham.rs) (0.3) | 5.40 ms | 4.85 ms | 10.32 ms | 20.70 ms | 99.44 ms | 4156.33 | 
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 5.13 ms | 4.94 ms | 8.14 ms | 14.28 ms | 38.71 ms | 2612.00 | 
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 5.61 ms | 4.94 ms | 9.96 ms | 17.77 ms | 34.89 ms | 3419.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.35 ms | 5.30 ms | 8.38 ms | 14.97 ms | 40.42 ms | 2881.33 | 
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 6.50 ms | 5.61 ms | 10.40 ms | 16.95 ms | 44.29 ms | 3151.00 | 
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 8.71 ms | 6.02 ms | 18.09 ms | 39.42 ms | 101.57 ms | 7778.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 6.83 ms | 6.02 ms | 9.68 ms | 16.81 ms | 239.82 ms | 6150.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 7.06 ms | 6.08 ms | 11.65 ms | 23.36 ms | 241.74 ms | 6124.00 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 8.85 ms | 6.13 ms | 18.38 ms | 37.94 ms | 224.24 ms | 8560.00 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 8.62 ms | 6.21 ms | 17.21 ms | 35.84 ms | 173.74 ms | 7773.00 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 9.43 ms | 6.25 ms | 20.44 ms | 42.29 ms | 230.33 ms | 9574.67 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 8.84 ms | 6.27 ms | 17.86 ms | 37.75 ms | 176.94 ms | 8389.67 | 
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 8.70 ms | 6.57 ms | 16.00 ms | 29.64 ms | 211.07 ms | 7107.00 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 9.47 ms | 6.61 ms | 19.71 ms | 40.58 ms | 136.82 ms | 8460.67 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 8.78 ms | 6.62 ms | 15.96 ms | 35.12 ms | 316.69 ms | 9936.67 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 8.67 ms | 6.66 ms | 15.98 ms | 33.99 ms | 77.58 ms | 6327.67 | 
| node (12.3) | [restana](http://github.com/jkyberneees/ana) (3.0) | 9.17 ms | 6.95 ms | 14.33 ms | 36.03 ms | 386.98 ms | 15497.00 | 
| node (12.3) | [0http](http://github.com/jkyberneees/0http) (1.0) | 8.82 ms | 6.98 ms | 14.45 ms | 32.57 ms | 372.90 ms | 11616.67 | 
| node (12.3) | [rayo](http://rayo.js.org) (1.3) | 9.94 ms | 7.87 ms | 15.14 ms | 55.64 ms | 354.23 ms | 14376.00 | 
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 9.35 ms | 8.01 ms | 17.39 ms | 36.05 ms | 383.85 ms | 11684.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 207.44 ms | 8.08 ms | 30.53 ms | 4850.62 ms | 7947.60 ms | 848517.00 | 
| node (12.3) | [polka](http://github.com/lukeed/polka) (0.5) | 12.94 ms | 8.09 ms | 16.38 ms | 145.63 ms | 719.23 ms | 37751.33 | 
| go (1.12) | [gf](http://goframe.org) (1.6) | 11.89 ms | 8.52 ms | 24.67 ms | 52.85 ms | 215.84 ms | 10854.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 10.78 ms | 8.74 ms | 19.73 ms | 38.19 ms | 261.17 ms | 9977.33 | 
| node (12.3) | [foxify](http://foxify.js.org) (0.10) | 13.19 ms | 9.36 ms | 18.21 ms | 96.53 ms | 524.05 ms | 25358.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 10.39 ms | 10.45 ms | 12.90 ms | 15.37 ms | 136.16 ms | 3146.00 | 
| node (12.3) | [koa](http://koajs.com) (2.7) | 16.48 ms | 10.90 ms | 20.63 ms | 190.72 ms | 636.91 ms | 35868.67 | 
| node (12.3) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 20.94 ms | 11.24 ms | 24.92 ms | 254.15 ms | 684.01 ms | 47533.00 | 
| node (12.3) | [fastify](http://fastify.io) (2.4) | 16.27 ms | 11.82 ms | 20.55 ms | 152.08 ms | 650.42 ms | 32700.33 | 
| node (12.3) | [restify](http://restify.com) (8.2) | 16.73 ms | 12.61 ms | 21.45 ms | 111.81 ms | 576.63 ms | 28717.33 | 
| python (3.7) | [hug](http://hug.rest) (2.5) | 14.81 ms | 12.66 ms | 23.94 ms | 42.39 ms | 295.77 ms | 10108.00 | 
| python (3.7) | [starlette](http://starlette.io) (0.12) | 15.10 ms | 12.86 ms | 26.26 ms | 43.57 ms | 118.60 ms | 8832.00 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 16.89 ms | 13.10 ms | 22.16 ms | 94.11 ms | 871.67 ms | 35291.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 25.40 ms | 14.25 ms | 28.86 ms | 398.27 ms | 2989.55 ms | 97140.00 | 
| node (12.3) | [express](http://expressjs.com) (4.16) | 21.62 ms | 14.36 ms | 25.71 ms | 257.91 ms | 821.59 ms | 46083.00 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.27) | 22.71 ms | 17.93 ms | 44.58 ms | 72.69 ms | 140.59 ms | 15595.00 | 
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 23.94 ms | 21.08 ms | 32.09 ms | 54.05 ms | 746.75 ms | 28128.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 23.29 ms | 23.13 ms | 36.40 ms | 49.38 ms | 78.15 ms | 10381.00 | 
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 55.84 ms | 24.26 ms | 46.44 ms | 1094.38 ms | 2797.19 ms | 196468.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 31.39 ms | 24.28 ms | 57.91 ms | 87.02 ms | 256.95 ms | 17717.00 | 
| node (12.3) | [hapi](http://hapijs.com) (18.1) | 35.52 ms | 24.35 ms | 40.50 ms | 378.53 ms | 1197.84 ms | 68943.67 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 36.09 ms | 29.65 ms | 61.04 ms | 125.80 ms | 485.24 ms | 26866.33 | 
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 35.01 ms | 32.87 ms | 42.66 ms | 50.30 ms | 259.33 ms | 8751.67 | 
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 44.41 ms | 35.44 ms | 104.54 ms | 231.29 ms | 563.54 ms | 51191.33 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 39.19 ms | 36.08 ms | 65.04 ms | 89.72 ms | 198.42 ms | 18789.00 | 
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 37.03 ms | 37.22 ms | 43.56 ms | 52.04 ms | 287.68 ms | 11261.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 49.28 ms | 37.71 ms | 95.39 ms | 172.35 ms | 341.80 ms | 35085.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 65.04 ms | 56.65 ms | 113.25 ms | 166.31 ms | 280.66 ms | 32254.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 86.58 ms | 75.73 ms | 141.54 ms | 229.91 ms | 923.50 ms | 49746.67 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 83.12 ms | 79.58 ms | 124.54 ms | 167.83 ms | 439.83 ms | 31124.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 91.97 ms | 91.20 ms | 115.42 ms | 141.27 ms | 661.52 ms | 27912.00 | 
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 105.06 ms | 94.51 ms | 163.58 ms | 221.36 ms | 1055.65 ms | 50587.33 | 
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 112.89 ms | 109.59 ms | 158.56 ms | 209.09 ms | 355.30 ms | 35310.33 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 302416.00 | 174.98 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 275640.67 | 329.98 MB |
| rust (1.35) | [actix-web](http://actix.rs) (0.7) | 262267.33 | 298.15 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 232807.67 | 264.10 MB |
| c (99) | [kore](http://kore.io) (3.1) | 220209.00 | 572.28 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 206690.33 | 200.40 MB |
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 201935.00 | 189.82 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 199713.33 | 321.49 MB |
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 195243.00 | 183.27 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 192679.33 | 386.98 MB |
| java (8) | [act](http://actframework.org) (1.8) | 191421.67 | 330.43 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 191132.00 | 110.44 MB |
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 186212.00 | 304.02 MB |
| rust (1.35) | [gotham](http://gotham.rs) (0.3) | 185617.00 | 379.87 MB |
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 182086.00 | 228.12 MB |
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 174098.67 | 186.11 MB |
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 146788.33 | 239.87 MB |
| rust (1.35) | [iron](http://ironframework.io) (0.6) | 140130.00 | 177.09 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 138908.67 | 226.29 MB |
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 126710.67 | 169.05 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 124445.67 | 165.90 MB |
| node (12.3) | [restana](http://github.com/jkyberneees/ana) (3.0) | 123541.33 | 184.76 MB |
| node (12.3) | [0http](http://github.com/jkyberneees/0http) (1.0) | 122403.00 | 183.09 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 121674.67 | 213.47 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 120734.67 | 161.02 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 120497.67 | 162.61 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 120476.67 | 161.13 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 119450.33 | 159.45 MB |
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 117403.67 | 275.21 MB |
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 115764.33 | 179.96 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 115445.00 | 202.56 MB |
| node (12.3) | [rayo](http://rayo.js.org) (1.3) | 111103.67 | 166.37 MB |
| node (12.3) | [polka](http://github.com/lukeed/polka) (0.5) | 109374.33 | 163.91 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 98303.33 | 242.30 MB |
| go (1.12) | [gf](http://goframe.org) (1.6) | 93930.67 | 141.80 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 93755.33 | 88.04 MB |
| node (12.3) | [foxify](http://foxify.js.org) (0.10) | 91391.67 | 191.83 MB |
| node (12.3) | [fastify](http://fastify.io) (2.4) | 88049.67 | 215.59 MB |
| node (12.3) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 84611.33 | 126.74 MB |
| rust (1.35) | [nickel](http://nickel-org.github.io) (0.11) | 82351.67 | 164.31 MB |
| node (12.3) | [koa](http://koajs.com) (2.7) | 81893.33 | 173.08 MB |
| node (12.3) | [restify](http://restify.com) (8.2) | 72879.33 | 127.52 MB |
| python (3.7) | [hug](http://hug.rest) (2.5) | 68224.33 | 169.16 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 68095.00 | 119.21 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 67879.33 | 113.68 MB |
| python (3.7) | [starlette](http://starlette.io) (0.12) | 67420.00 | 145.00 MB |
| node (12.3) | [express](http://expressjs.com) (4.16) | 63525.67 | 155.19 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 60322.00 | 129.10 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 54505.00 | 270.44 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 54291.00 | 281.52 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 52775.00 | 261.65 MB |
| php (7.3) | [symfony](http://symfony.com) (4.3) | 52252.67 | 259.24 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 50933.00 | 252.60 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.27) | 47111.33 | 101.64 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 43792.00 | 81.19 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 43335.67 | 98.21 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 41854.33 | 218.03 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 41548.00 | 63.06 MB |
| node (12.3) | [hapi](http://hapijs.com) (18.1) | 37847.00 | 97.89 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 37377.67 | 35.60 MB |
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 35981.33 | 67.78 MB |
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 33220.33 | 43.02 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 32337.33 | 79.66 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 30202.00 | 56.13 MB |
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 28279.33 | 34.87 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 28254.33 | 16.29 MB |
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 27001.67 | 49.27 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 25605.67 | 49.40 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 21713.67 | 38.65 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 18228.33 | 10.51 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 15663.00 | 118.54 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 15554.67 | 30.97 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 13992.67 | 36.27 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 11781.67 | 25.66 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 11673.33 | 33.87 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 10449.33 | 30.75 MB |
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 9523.67 | 23.45 MB |
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 8794.33 | 22.64 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3336.67 | 10.21 MB |
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

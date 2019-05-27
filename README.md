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
Last update: 2019-05-26
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


:five: hanami (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.35) | [nickel](http://nickel-org.github.io) (0.11) | 0.09 ms | 0.09 ms | 0.14 ms | 0.19 ms | 1.33 ms | 37.67 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 4.59 ms | 0.27 ms | 16.54 ms | 35.90 ms | 88.33 ms | 8336.33 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 6.07 ms | 0.34 ms | 21.41 ms | 43.00 ms | 100.77 ms | 10439.67 | 
| rust (1.35) | [iron](http://ironframework.io) (0.6) | 0.52 ms | 0.51 ms | 0.85 ms | 1.29 ms | 25.30 ms | 312.33 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 9.08 ms | 0.55 ms | 29.42 ms | 57.63 ms | 138.50 ms | 14170.00 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 8.49 ms | 0.56 ms | 27.02 ms | 53.18 ms | 112.90 ms | 12842.67 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 209.42 ms | 0.57 ms | 439.76 ms | 4235.22 ms | 7527.88 ms | 725594.67 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 184.07 ms | 0.57 ms | 351.46 ms | 3973.93 ms | 7501.74 ms | 661235.00 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 179.37 ms | 0.58 ms | 313.66 ms | 4148.21 ms | 6921.68 ms | 676684.00 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 237.99 ms | 0.60 ms | 379.39 ms | 5289.85 ms | 7419.45 ms | 869896.33 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 169.27 ms | 0.64 ms | 306.45 ms | 3526.69 ms | 7266.23 ms | 636102.33 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 11.21 ms | 0.79 ms | 34.11 ms | 66.54 ms | 152.56 ms | 16204.33 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 197.74 ms | 1.27 ms | 385.08 ms | 4104.88 ms | 7499.69 ms | 703381.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 116.45 ms | 2.26 ms | 5.74 ms | 3587.69 ms | 6599.07 ms | 609649.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 3.26 ms | 2.37 ms | 6.54 ms | 13.49 ms | 33.16 ms | 2913.00 | 
| rust (1.35) | [actix-web](http://actix.rs) (0.7) | 3.94 ms | 3.21 ms | 7.55 ms | 15.67 ms | 101.66 ms | 3711.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 6.96 ms | 3.42 ms | 8.90 ms | 76.96 ms | 877.39 ms | 30682.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 4.30 ms | 3.53 ms | 8.94 ms | 18.88 ms | 39.95 ms | 3948.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.86 ms | 3.86 ms | 9.70 ms | 17.84 ms | 41.14 ms | 3830.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.15 ms | 4.49 ms | 5.76 ms | 10.56 ms | 35.27 ms | 1989.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 9.36 ms | 4.56 ms | 17.00 ms | 92.88 ms | 133.84 ms | 16492.00 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.95 ms | 4.79 ms | 7.55 ms | 15.56 ms | 107.90 ms | 2996.00 | 
| rust (1.35) | [gotham](http://gotham.rs) (0.3) | 5.49 ms | 4.92 ms | 9.82 ms | 20.73 ms | 165.98 ms | 5447.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.26 ms | 5.16 ms | 8.09 ms | 14.70 ms | 40.51 ms | 2763.33 | 
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 6.03 ms | 5.29 ms | 10.39 ms | 19.06 ms | 35.77 ms | 3511.00 | 
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 5.68 ms | 5.33 ms | 9.01 ms | 15.53 ms | 37.26 ms | 2772.33 | 
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 5.85 ms | 5.41 ms | 9.35 ms | 15.66 ms | 38.97 ms | 2793.33 | 
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 5.99 ms | 5.44 ms | 9.81 ms | 17.39 ms | 36.99 ms | 3099.33 | 
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 5.95 ms | 5.46 ms | 9.29 ms | 14.78 ms | 42.30 ms | 2620.00 | 
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 6.42 ms | 5.63 ms | 10.01 ms | 15.81 ms | 40.68 ms | 2868.33 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 40.74 ms | 5.70 ms | 131.14 ms | 326.15 ms | 893.42 ms | 71047.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 7.72 ms | 7.01 ms | 10.93 ms | 19.91 ms | 286.59 ms | 6293.33 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 9.81 ms | 7.18 ms | 19.66 ms | 42.92 ms | 180.01 ms | 8814.00 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 10.43 ms | 7.26 ms | 21.45 ms | 47.96 ms | 291.61 ms | 12050.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 7.89 ms | 7.27 ms | 12.86 ms | 23.81 ms | 176.17 ms | 5441.67 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 10.80 ms | 7.48 ms | 23.18 ms | 49.99 ms | 183.12 ms | 10453.00 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 10.34 ms | 7.51 ms | 20.50 ms | 46.15 ms | 248.63 ms | 10684.67 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 10.40 ms | 7.56 ms | 21.53 ms | 45.51 ms | 141.63 ms | 9103.00 | 
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 10.87 ms | 7.73 ms | 22.29 ms | 50.36 ms | 281.56 ms | 11367.67 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 9.80 ms | 7.74 ms | 17.81 ms | 40.64 ms | 232.19 ms | 8471.33 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 10.01 ms | 8.17 ms | 17.76 ms | 40.56 ms | 209.71 ms | 7848.67 | 
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 10.15 ms | 8.89 ms | 19.44 ms | 42.71 ms | 190.92 ms | 9705.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 231.61 ms | 8.92 ms | 159.58 ms | 4972.39 ms | 7873.14 ms | 880766.33 | 
| node (12.3) | [restana](http://github.com/jkyberneees/ana) (3.0) | 13.50 ms | 9.12 ms | 17.99 ms | 120.60 ms | 653.01 ms | 30422.00 | 
| go (1.12) | [gf](http://goframe.org) (1.6) | 12.25 ms | 9.19 ms | 24.72 ms | 53.51 ms | 197.27 ms | 10753.67 | 
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 12.54 ms | 9.76 ms | 23.09 ms | 44.45 ms | 217.81 ms | 9499.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 10.97 ms | 10.75 ms | 13.69 ms | 15.68 ms | 26.27 ms | 1970.00 | 
| node (12.3) | [rayo](http://rayo.js.org) (1.3) | 16.08 ms | 11.25 ms | 21.07 ms | 163.69 ms | 560.53 ms | 30449.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 14.41 ms | 11.44 ms | 26.45 ms | 52.01 ms | 229.79 ms | 10887.33 | 
| node (12.3) | [polka](http://github.com/lukeed/polka) (0.5) | 19.74 ms | 11.49 ms | 22.39 ms | 304.70 ms | 827.36 ms | 51113.00 | 
| node (12.3) | [foxify](http://foxify.js.org) (0.10) | 20.34 ms | 13.29 ms | 24.57 ms | 213.54 ms | 865.11 ms | 44467.00 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 23.43 ms | 14.46 ms | 25.85 ms | 298.47 ms | 1240.36 ms | 66132.33 | 
| python (3.7) | [starlette](http://starlette.io) (0.12) | 17.15 ms | 15.35 ms | 28.15 ms | 44.45 ms | 115.87 ms | 8736.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 21.90 ms | 15.59 ms | 31.31 ms | 192.53 ms | 1340.09 ms | 58745.00 | 
| node (12.3) | [fastify](http://fastify.io) (2.4) | 25.79 ms | 16.02 ms | 28.01 ms | 346.27 ms | 995.86 ms | 60681.67 | 
| node (12.3) | [koa](http://koajs.com) (2.7) | 25.97 ms | 16.08 ms | 29.23 ms | 336.96 ms | 1075.72 ms | 62561.67 | 
| python (3.7) | [hug](http://hug.rest) (2.5) | 20.45 ms | 16.19 ms | 35.55 ms | 61.96 ms | 374.64 ms | 15086.00 | 
| node (12.3) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 29.31 ms | 16.94 ms | 34.00 ms | 405.21 ms | 1075.52 ms | 69119.00 | 
| node (12.3) | [restify](http://restify.com) (8.2) | 22.10 ms | 17.15 ms | 30.00 ms | 123.40 ms | 541.20 ms | 27453.67 | 
| node (12.3) | [express](http://expressjs.com) (4.16) | 27.39 ms | 18.61 ms | 33.80 ms | 265.62 ms | 990.53 ms | 52247.33 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.24) | 27.43 ms | 23.05 ms | 48.29 ms | 83.88 ms | 218.05 ms | 16598.67 | 
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 28.89 ms | 25.55 ms | 40.92 ms | 58.42 ms | 685.12 ms | 26911.33 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 38.72 ms | 27.23 ms | 86.19 ms | 122.32 ms | 352.96 ms | 27907.00 | 
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 51.56 ms | 27.55 ms | 50.18 ms | 897.31 ms | 2732.50 ms | 167161.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 29.77 ms | 27.59 ms | 47.31 ms | 65.84 ms | 93.55 ms | 12442.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 38.42 ms | 30.51 ms | 72.84 ms | 107.59 ms | 579.34 ms | 24241.33 | 
| node (12.3) | [hapi](http://hapijs.com) (18.1) | 55.46 ms | 31.85 ms | 52.30 ms | 786.21 ms | 1668.72 ms | 126850.33 | 
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 45.40 ms | 38.42 ms | 104.34 ms | 236.28 ms | 681.64 ms | 51079.33 | 
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 44.80 ms | 41.04 ms | 53.70 ms | 133.41 ms | 360.80 ms | 21030.00 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 45.85 ms | 41.44 ms | 73.16 ms | 99.12 ms | 166.79 ms | 21206.67 | 
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 46.10 ms | 43.37 ms | 54.25 ms | 91.97 ms | 349.07 ms | 13483.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 63.42 ms | 53.65 ms | 116.95 ms | 197.35 ms | 396.68 ms | 40628.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 83.13 ms | 78.71 ms | 126.05 ms | 169.01 ms | 254.33 ms | 31182.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 95.10 ms | 88.76 ms | 128.54 ms | 209.23 ms | 791.26 ms | 39030.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 95.21 ms | 94.75 ms | 114.48 ms | 139.23 ms | 570.06 ms | 24414.33 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 102.52 ms | 96.34 ms | 171.07 ms | 223.78 ms | 307.50 ms | 47501.33 | 
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 103.47 ms | 100.25 ms | 147.19 ms | 199.34 ms | 298.13 ms | 33842.67 | 
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 116.77 ms | 104.60 ms | 199.44 ms | 246.01 ms | 706.52 ms | 48868.33 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 302155.33 | 174.82 MB |
| rust (1.35) | [actix-web](http://actix.rs) (0.7) | 242437.00 | 275.70 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 237213.67 | 284.02 MB |
| c (99) | [kore](http://kore.io) (3.1) | 220418.33 | 572.82 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 215890.67 | 245.12 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 214046.00 | 207.75 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 194662.67 | 391.02 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 187076.33 | 301.98 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 178651.00 | 103.28 MB |
| rust (1.35) | [gotham](http://gotham.rs) (0.3) | 177977.67 | 364.73 MB |
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 164517.67 | 154.79 MB |
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 161471.00 | 172.41 MB |
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 160375.33 | 262.24 MB |
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 157990.00 | 148.55 MB |
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 157734.67 | 197.23 MB |
| java (8) | [act](http://actframework.org) (1.8) | 156706.33 | 270.49 MB |
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 147731.00 | 241.58 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 122880.00 | 200.36 MB |
| rust (1.35) | [iron](http://ironframework.io) (0.6) | 115547.33 | 145.86 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 111758.00 | 149.23 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 108100.00 | 189.62 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 108090.33 | 143.87 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 107541.67 | 144.98 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 106201.00 | 186.30 MB |
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 106193.33 | 165.58 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 104315.00 | 138.67 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 104099.00 | 139.19 MB |
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 103986.33 | 138.24 MB |
| node (12.3) | [restana](http://github.com/jkyberneees/ana) (3.0) | 94859.67 | 142.08 MB |
| go (1.12) | [gf](http://goframe.org) (1.6) | 90026.33 | 136.47 MB |
| rust (1.35) | [nickel](http://nickel-org.github.io) (0.11) | 89274.33 | 177.31 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 88776.67 | 83.52 MB |
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 83287.33 | 195.34 MB |
| node (12.3) | [rayo](http://rayo.js.org) (1.3) | 77201.33 | 115.62 MB |
| node (12.3) | [polka](http://github.com/lukeed/polka) (0.5) | 75109.00 | 112.42 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 73274.33 | 180.58 MB |
| node (12.3) | [foxify](http://foxify.js.org) (0.10) | 64594.33 | 135.73 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 63537.33 | 111.20 MB |
| node (12.3) | [fastify](http://fastify.io) (2.4) | 62012.67 | 155.19 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 59879.67 | 99.68 MB |
| python (3.7) | [starlette](http://starlette.io) (0.12) | 58745.67 | 126.59 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 58523.67 | 125.69 MB |
| node (12.3) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 55652.00 | 83.32 MB |
| node (12.3) | [koa](http://koajs.com) (2.7) | 55507.67 | 117.38 MB |
| node (12.3) | [restify](http://restify.com) (8.2) | 51535.00 | 90.48 MB |
| python (3.7) | [hug](http://hug.rest) (2.5) | 49416.67 | 122.50 MB |
| node (12.3) | [express](http://expressjs.com) (4.16) | 48001.00 | 117.36 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 39809.33 | 62.85 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 39723.00 | 196.94 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.24) | 38070.00 | 82.24 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 38000.00 | 188.62 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 37411.33 | 185.58 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 36519.33 | 181.28 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 36139.67 | 187.38 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 35516.00 | 65.81 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 34559.33 | 180.13 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 34046.67 | 77.23 MB |
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 32498.00 | 61.25 MB |
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 31288.00 | 40.38 MB |
| node (12.3) | [hapi](http://hapijs.com) (18.1) | 28770.00 | 74.29 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 28422.67 | 52.83 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 27865.00 | 26.55 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 26392.00 | 64.93 MB |
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 22497.00 | 41.06 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 21903.33 | 42.32 MB |
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 21486.67 | 26.44 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 21027.67 | 12.14 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 16543.00 | 29.46 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 15045.33 | 8.67 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 14066.33 | 106.39 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 11965.33 | 23.86 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 11380.00 | 29.52 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 10328.33 | 30.00 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 10176.33 | 30.00 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 9690.00 | 21.13 MB |
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 9611.67 | 24.70 MB |
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 8229.33 | 20.25 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3139.00 | 9.59 MB |
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

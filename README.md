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
Last update: 2019-05-16
```
OS: Linux (version: 5.0.9-301.fc30.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: iron (rust)


:five: symfony (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 0.09 ms | 0.09 ms | 0.14 ms | 0.18 ms | 5.09 ms | 42.67 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.19) | 4.99 ms | 0.30 ms | 17.47 ms | 36.23 ms | 89.96 ms | 8632.00 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 6.30 ms | 0.35 ms | 22.48 ms | 45.22 ms | 114.75 ms | 10932.33 | 
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 0.56 ms | 0.52 ms | 0.94 ms | 1.40 ms | 24.60 ms | 374.67 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 199.82 ms | 0.56 ms | 367.69 ms | 4513.23 ms | 7283.86 ms | 734115.00 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 195.49 ms | 0.57 ms | 372.21 ms | 4248.85 ms | 7014.72 ms | 701563.67 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 128.98 ms | 0.57 ms | 334.81 ms | 2329.17 ms | 7023.30 ms | 442063.00 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 184.19 ms | 0.58 ms | 339.65 ms | 4171.22 ms | 7478.60 ms | 702561.00 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 173.19 ms | 0.59 ms | 315.77 ms | 4076.37 ms | 7628.09 ms | 689160.67 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 9.74 ms | 0.61 ms | 31.27 ms | 59.01 ms | 135.19 ms | 14768.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 9.34 ms | 0.64 ms | 29.34 ms | 56.74 ms | 120.60 ms | 13832.67 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 158.55 ms | 0.65 ms | 313.81 ms | 3017.88 ms | 7558.05 ms | 553439.67 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 12.22 ms | 0.91 ms | 36.44 ms | 70.27 ms | 149.56 ms | 17240.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 86.32 ms | 2.19 ms | 4.99 ms | 2771.30 ms | 6593.96 ms | 514633.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 3.64 ms | 2.41 ms | 7.99 ms | 14.97 ms | 36.83 ms | 3409.33 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.92 ms | 3.44 ms | 7.61 ms | 15.72 ms | 36.83 ms | 3336.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 5.74 ms | 3.50 ms | 8.27 ms | 19.40 ms | 568.88 ms | 18576.67 | 
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 4.03 ms | 3.56 ms | 7.20 ms | 14.91 ms | 35.44 ms | 3006.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 5.12 ms | 4.09 ms | 10.32 ms | 18.39 ms | 41.46 ms | 3969.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 7.25 ms | 4.86 ms | 12.31 ms | 64.43 ms | 121.13 ms | 10317.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.55 ms | 4.91 ms | 6.15 ms | 10.82 ms | 93.48 ms | 2338.67 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.33 ms | 5.01 ms | 7.64 ms | 16.34 ms | 172.66 ms | 5947.00 | 
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 5.85 ms | 5.16 ms | 10.09 ms | 18.56 ms | 37.04 ms | 3429.33 | 
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 5.74 ms | 5.30 ms | 10.00 ms | 20.08 ms | 84.67 ms | 3636.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.49 ms | 5.37 ms | 8.67 ms | 15.79 ms | 39.31 ms | 2927.33 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 46.23 ms | 6.46 ms | 149.25 ms | 376.13 ms | 1026.42 ms | 81838.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 8.03 ms | 6.91 ms | 10.57 ms | 23.17 ms | 429.22 ms | 13200.00 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 10.19 ms | 7.54 ms | 20.19 ms | 44.09 ms | 252.93 ms | 9797.00 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 10.57 ms | 7.66 ms | 21.76 ms | 47.13 ms | 189.91 ms | 9409.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 8.39 ms | 7.67 ms | 13.40 ms | 25.61 ms | 130.34 ms | 5685.33 | 
| node (12.2) | [restana](http://github.com/jkyberneees/ana) (2.13) | 8.73 ms | 7.78 ms | 12.68 ms | 27.82 ms | 341.87 ms | 11703.67 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 10.87 ms | 8.39 ms | 21.31 ms | 46.42 ms | 176.33 ms | 8997.33 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 11.70 ms | 8.44 ms | 24.44 ms | 51.98 ms | 257.78 ms | 11657.33 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 10.42 ms | 8.46 ms | 18.65 ms | 42.72 ms | 166.54 ms | 8176.00 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 10.67 ms | 8.56 ms | 17.53 ms | 41.31 ms | 391.34 ms | 13582.33 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 11.81 ms | 8.62 ms | 23.94 ms | 53.85 ms | 230.89 ms | 12354.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 214.53 ms | 8.80 ms | 126.20 ms | 4830.06 ms | 7920.41 ms | 835859.33 | 
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 10.28 ms | 9.13 ms | 19.70 ms | 40.94 ms | 312.36 ms | 10725.67 | 
| node (12.2) | [polka](http://github.com/lukeed/polka) (0.5) | 11.95 ms | 9.48 ms | 16.37 ms | 45.49 ms | 546.44 ms | 22489.33 | 
| node (12.2) | [rayo](http://rayo.js.org) (1.2) | 12.34 ms | 9.54 ms | 16.67 ms | 57.28 ms | 558.59 ms | 23424.00 | 
| go (1.12) | [gf](http://goframe.org) (1.6) | 13.59 ms | 9.61 ms | 27.35 ms | 62.37 ms | 430.84 ms | 15549.67 | 
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 12.63 ms | 9.67 ms | 25.08 ms | 46.19 ms | 212.25 ms | 9647.00 | 
| node (12.2) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 19.34 ms | 12.06 ms | 24.44 ms | 234.52 ms | 898.72 ms | 47043.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 15.43 ms | 12.27 ms | 27.85 ms | 50.74 ms | 223.88 ms | 10423.67 | 
| node (12.2) | [foxify](http://foxify.js.org) (0.10) | 14.91 ms | 12.44 ms | 21.29 ms | 43.00 ms | 561.97 ms | 21628.00 | 
| node (12.2) | [koa](http://koajs.com) (2.7) | 17.72 ms | 12.68 ms | 22.35 ms | 147.72 ms | 770.51 ms | 37818.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 12.65 ms | 12.70 ms | 15.23 ms | 17.97 ms | 36.06 ms | 2129.67 | 
| node (12.2) | [fastify](http://fastify.io) (2.3) | 18.91 ms | 13.93 ms | 23.59 ms | 172.25 ms | 687.83 ms | 34996.67 | 
| node (12.2) | [restify](http://restify.com) (8.2) | 16.32 ms | 14.53 ms | 20.67 ms | 45.03 ms | 409.97 ms | 13977.00 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 18.51 ms | 15.01 ms | 33.62 ms | 55.35 ms | 121.48 ms | 11057.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 21.48 ms | 15.76 ms | 33.53 ms | 197.84 ms | 1029.06 ms | 39341.33 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 22.27 ms | 15.91 ms | 27.66 ms | 190.92 ms | 1283.53 ms | 55055.67 | 
| node (12.2) | [express](http://expressjs.com) (4.16) | 24.29 ms | 16.50 ms | 28.94 ms | 253.33 ms | 1004.69 ms | 52348.67 | 
| python (3.7) | [hug](http://hug.rest) (2.5) | 22.12 ms | 16.83 ms | 43.64 ms | 69.23 ms | 310.15 ms | 15315.00 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.2) | 27.59 ms | 25.37 ms | 45.39 ms | 62.51 ms | 149.36 ms | 12633.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 30.71 ms | 27.45 ms | 55.15 ms | 78.01 ms | 146.94 ms | 16827.33 | 
| node (12.2) | [hapi](http://hapijs.com) (18.1) | 59.29 ms | 30.68 ms | 51.57 ms | 935.21 ms | 1908.38 ms | 152935.33 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 43.73 ms | 30.82 ms | 91.53 ms | 123.36 ms | 379.01 ms | 28965.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 44.92 ms | 33.49 ms | 95.37 ms | 134.33 ms | 287.54 ms | 28376.00 | 
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 36.78 ms | 33.91 ms | 44.12 ms | 82.91 ms | 246.78 ms | 14032.00 | 
| swift (5.0) | [kitura](http://kitura.io) (2.6) | 48.25 ms | 34.25 ms | 54.64 ms | 498.18 ms | 1863.91 ms | 102906.33 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 41.85 ms | 37.19 ms | 71.20 ms | 93.16 ms | 172.64 ms | 20252.00 | 
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 39.75 ms | 38.25 ms | 46.34 ms | 185.57 ms | 291.21 ms | 24599.00 | 
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 44.31 ms | 39.58 ms | 50.08 ms | 232.09 ms | 475.99 ms | 35588.33 | 
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 43.22 ms | 40.84 ms | 50.50 ms | 132.47 ms | 302.49 ms | 20439.33 | 
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 45.25 ms | 42.36 ms | 53.16 ms | 127.04 ms | 335.97 ms | 16843.00 | 
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 52.19 ms | 45.56 ms | 54.41 ms | 314.92 ms | 535.86 ms | 46487.67 | 
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 53.96 ms | 53.20 ms | 65.48 ms | 74.89 ms | 429.94 ms | 13993.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 69.91 ms | 61.41 ms | 125.97 ms | 199.23 ms | 431.55 ms | 42320.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 83.87 ms | 72.47 ms | 152.19 ms | 206.77 ms | 280.93 ms | 40733.00 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 101.78 ms | 90.01 ms | 180.43 ms | 229.07 ms | 293.21 ms | 52681.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 112.87 ms | 90.13 ms | 231.50 ms | 300.72 ms | 532.10 ms | 62026.00 | 
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 105.55 ms | 102.92 ms | 150.40 ms | 195.37 ms | 262.22 ms | 34058.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 111.82 ms | 109.65 ms | 146.20 ms | 176.95 ms | 317.53 ms | 27797.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (kore) (c)


:five: (vibora) (python)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 286105.00 | 165.56 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 252477.33 | 302.38 MB |
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 234533.00 | 266.67 MB |
| c (99) | [kore](http://kore.io) (3.1) | 216440.33 | 562.09 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 207826.33 | 235.99 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 196530.00 | 190.83 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 192604.67 | 387.09 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 180447.33 | 290.79 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 169856.67 | 98.26 MB |
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 167531.33 | 179.36 MB |
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 166265.67 | 340.66 MB |
| java (8) | [act](http://actframework.org) (1.8) | 151145.33 | 260.91 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 125821.67 | 204.99 MB |
| node (12.2) | [restana](http://github.com/jkyberneees/ana) (2.13) | 119763.33 | 179.62 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 107977.33 | 145.02 MB |
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 107842.00 | 136.01 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 104476.33 | 183.29 MB |
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 103651.33 | 161.51 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 101757.67 | 135.02 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 101130.33 | 134.30 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 99643.00 | 134.33 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 96125.00 | 128.72 MB |
| node (12.2) | [polka](http://github.com/lukeed/polka) (0.5) | 95504.67 | 143.20 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 95501.33 | 167.59 MB |
| node (12.2) | [rayo](http://rayo.js.org) (1.2) | 93295.33 | 139.64 MB |
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 92992.67 | 184.51 MB |
| go (1.12) | [gf](http://goframe.org) (1.6) | 84030.00 | 127.14 MB |
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 82839.00 | 194.04 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 77151.00 | 72.46 MB |
| node (12.2) | [fastify](http://fastify.io) (2.3) | 75071.67 | 183.51 MB |
| node (12.2) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 74143.33 | 111.18 MB |
| node (12.2) | [foxify](http://foxify.js.org) (0.10) | 73150.33 | 153.87 MB |
| node (12.2) | [koa](http://koajs.com) (2.7) | 69562.00 | 147.36 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 66751.00 | 164.34 MB |
| node (12.2) | [restify](http://restify.com) (8.2) | 63391.67 | 111.33 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 61453.67 | 107.69 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 56675.67 | 94.90 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 56460.00 | 121.78 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 54065.67 | 115.92 MB |
| node (12.2) | [express](http://expressjs.com) (4.16) | 53007.00 | 129.89 MB |
| python (3.7) | [hug](http://hug.rest) (2.5) | 47986.33 | 119.06 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 41674.67 | 65.60 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 38831.00 | 192.58 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 37966.67 | 188.26 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 37400.00 | 185.46 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.2) | 36444.67 | 78.56 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 36395.00 | 180.51 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 36047.33 | 186.98 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 33156.67 | 172.62 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 33146.67 | 75.11 MB |
| node (12.2) | [hapi](http://hapijs.com) (18.1) | 31239.67 | 80.74 MB |
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 27516.00 | 25.79 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.6) | 26905.33 | 49.91 MB |
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 26327.33 | 24.70 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.19) | 25627.00 | 24.45 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 25436.00 | 47.34 MB |
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 24756.33 | 40.32 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 24145.33 | 46.69 MB |
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 23878.67 | 29.81 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 23592.00 | 58.12 MB |
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 22141.67 | 27.18 MB |
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 21807.00 | 39.86 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 20389.33 | 11.77 MB |
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 18439.67 | 30.05 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 14680.33 | 26.18 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 13635.67 | 7.87 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 13107.33 | 99.08 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 12185.33 | 24.30 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 10473.67 | 27.15 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 9808.33 | 21.37 MB |
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 9411.00 | 24.20 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 9116.33 | 26.41 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 8744.67 | 25.95 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 2757.67 | 8.46 MB |
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

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
Last update: 2019-05-24
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
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 0.07 ms | 0.08 ms | 0.10 ms | 0.13 ms | 2.42 ms | 28.67 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 2.86 ms | 0.15 ms | 10.70 ms | 27.37 ms | 72.88 ms | 6003.00 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.71 ms | 0.19 ms | 14.12 ms | 32.10 ms | 81.72 ms | 7340.33 | 
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 0.31 ms | 0.30 ms | 0.50 ms | 0.84 ms | 10.43 ms | 188.33 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 108.64 ms | 0.31 ms | 274.60 ms | 1978.90 ms | 6872.38 ms | 391885.33 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 120.97 ms | 0.31 ms | 206.26 ms | 2809.93 ms | 6809.53 ms | 502777.33 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.61 ms | 0.31 ms | 19.54 ms | 42.34 ms | 96.17 ms | 9845.67 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 123.13 ms | 0.32 ms | 226.70 ms | 2780.80 ms | 7039.60 ms | 498549.00 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 144.63 ms | 0.32 ms | 255.76 ms | 2980.61 ms | 6958.00 ms | 553151.67 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 6.60 ms | 0.33 ms | 23.87 ms | 51.13 ms | 128.72 ms | 11997.00 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 135.32 ms | 0.34 ms | 210.89 ms | 3327.74 ms | 6805.01 ms | 551826.67 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 180.68 ms | 0.34 ms | 282.70 ms | 3995.67 ms | 6882.47 ms | 674517.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 7.18 ms | 0.42 ms | 24.09 ms | 49.71 ms | 134.46 ms | 11850.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 45.98 ms | 1.39 ms | 3.07 ms | 1326.00 ms | 5504.50 ms | 312861.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 2.93 ms | 1.75 ms | 6.88 ms | 14.09 ms | 63.43 ms | 3160.00 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 30.68 ms | 2.02 ms | 104.92 ms | 284.41 ms | 760.79 ms | 60784.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.08 ms | 2.17 ms | 6.53 ms | 14.41 ms | 35.15 ms | 3084.33 | 
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 2.85 ms | 2.29 ms | 5.78 ms | 12.58 ms | 68.27 ms | 2594.67 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.16 ms | 2.48 ms | 5.57 ms | 9.97 ms | 46.11 ms | 2139.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 6.80 ms | 2.87 ms | 6.83 ms | 115.28 ms | 786.05 ms | 36524.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.76 ms | 3.08 ms | 7.83 ms | 14.65 ms | 33.76 ms | 3201.00 | 
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 4.13 ms | 3.18 ms | 8.00 ms | 18.23 ms | 184.21 ms | 4716.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.16 ms | 3.26 ms | 5.14 ms | 7.69 ms | 22.27 ms | 1668.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.96 ms | 3.41 ms | 6.61 ms | 11.19 ms | 28.53 ms | 2103.33 | 
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 3.80 ms | 3.56 ms | 6.42 ms | 12.58 ms | 27.73 ms | 2438.33 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 5.73 ms | 3.62 ms | 9.03 ms | 70.62 ms | 123.51 ms | 10691.00 | 
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 4.17 ms | 3.70 ms | 7.63 ms | 14.03 ms | 30.23 ms | 2779.00 | 
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 3.90 ms | 3.72 ms | 6.39 ms | 12.71 ms | 27.49 ms | 2388.67 | 
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 3.97 ms | 3.90 ms | 6.35 ms | 12.20 ms | 28.37 ms | 2274.33 | 
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 4.12 ms | 4.07 ms | 6.62 ms | 12.72 ms | 29.66 ms | 2368.33 | 
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 4.67 ms | 4.53 ms | 7.49 ms | 12.40 ms | 35.91 ms | 2374.67 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 6.19 ms | 4.61 ms | 13.00 ms | 26.20 ms | 165.19 ms | 5821.33 | 
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 6.58 ms | 4.63 ms | 14.40 ms | 30.21 ms | 107.15 ms | 6214.00 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 6.08 ms | 4.67 ms | 11.64 ms | 25.04 ms | 194.13 ms | 5297.67 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 6.77 ms | 4.69 ms | 14.61 ms | 30.03 ms | 230.40 ms | 7518.67 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 6.60 ms | 4.70 ms | 13.57 ms | 28.42 ms | 281.79 ms | 7822.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 5.21 ms | 4.72 ms | 7.90 ms | 14.26 ms | 192.37 ms | 3212.33 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 7.20 ms | 4.74 ms | 15.77 ms | 32.28 ms | 272.48 ms | 9130.67 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 7.18 ms | 4.76 ms | 14.70 ms | 31.64 ms | 320.28 ms | 11722.00 | 
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 7.26 ms | 4.79 ms | 15.24 ms | 29.89 ms | 188.99 ms | 6486.33 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 6.42 ms | 4.83 ms | 12.49 ms | 26.50 ms | 189.95 ms | 5707.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.67 ms | 4.85 ms | 9.35 ms | 19.78 ms | 116.64 ms | 4161.00 | 
| node (12.3) | [restana](http://github.com/jkyberneees/ana) (3.0) | 13.34 ms | 4.90 ms | 12.99 ms | 234.26 ms | 501.20 ms | 39315.67 | 
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 6.72 ms | 5.20 ms | 12.92 ms | 27.99 ms | 188.77 ms | 6065.33 | 
| go (1.12) | [gf](http://goframe.org) (1.6) | 8.47 ms | 5.63 ms | 18.26 ms | 39.12 ms | 187.45 ms | 8360.00 | 
| node (12.3) | [rayo](http://rayo.js.org) (1.3) | 18.53 ms | 5.82 ms | 14.22 ms | 305.40 ms | 864.59 ms | 60879.67 | 
| node (12.3) | [polka](http://github.com/lukeed/polka) (0.5) | 13.60 ms | 5.98 ms | 13.95 ms | 227.23 ms | 619.93 ms | 42404.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 215.93 ms | 7.24 ms | 71.38 ms | 4923.50 ms | 7923.34 ms | 860641.00 | 
| node (12.3) | [foxify](http://foxify.js.org) (0.10) | 12.64 ms | 7.51 ms | 14.52 ms | 157.06 ms | 823.06 ms | 41593.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 9.27 ms | 7.77 ms | 16.48 ms | 34.07 ms | 281.26 ms | 9800.00 | 
| node (12.3) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 19.28 ms | 7.85 ms | 18.84 ms | 294.10 ms | 960.29 ms | 62340.67 | 
| node (12.3) | [koa](http://koajs.com) (2.7) | 11.29 ms | 8.17 ms | 14.93 ms | 111.14 ms | 477.56 ms | 22682.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 8.37 ms | 8.45 ms | 10.54 ms | 12.71 ms | 88.60 ms | 1971.33 | 
| node (12.3) | [fastify](http://fastify.io) (2.3) | 11.81 ms | 8.55 ms | 17.04 ms | 52.92 ms | 554.31 ms | 23117.67 | 
| node (12.3) | [restify](http://restify.com) (8.2) | 11.35 ms | 8.66 ms | 14.97 ms | 59.71 ms | 370.82 ms | 17110.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 27.36 ms | 8.98 ms | 19.83 ms | 670.03 ms | 2469.41 ms | 141152.67 | 
| node (12.3) | [express](http://expressjs.com) (4.16) | 12.69 ms | 9.45 ms | 17.79 ms | 79.35 ms | 442.05 ms | 18781.00 | 
| python (3.7) | [hug](http://hug.rest) (2.5) | 11.89 ms | 10.49 ms | 18.10 ms | 31.11 ms | 289.27 ms | 9485.67 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 14.05 ms | 10.55 ms | 19.37 ms | 66.00 ms | 765.69 ms | 29119.67 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 12.15 ms | 10.83 ms | 21.88 ms | 34.85 ms | 65.02 ms | 6929.67 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.23) | 17.89 ms | 14.93 ms | 32.14 ms | 51.36 ms | 102.21 ms | 10225.00 | 
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 22.35 ms | 16.26 ms | 27.50 ms | 170.20 ms | 1037.72 ms | 50178.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 18.24 ms | 17.94 ms | 26.65 ms | 39.37 ms | 70.91 ms | 7419.67 | 
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 29.94 ms | 18.43 ms | 33.46 ms | 367.51 ms | 1555.17 ms | 84721.67 | 
| node (12.3) | [hapi](http://hapijs.com) (18.1) | 27.57 ms | 19.31 ms | 32.10 ms | 277.42 ms | 1119.72 ms | 57106.00 | 
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 26.91 ms | 20.86 ms | 54.80 ms | 145.86 ms | 447.80 ms | 29048.67 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 30.01 ms | 20.96 ms | 65.76 ms | 106.23 ms | 402.01 ms | 25582.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 25.72 ms | 22.39 ms | 41.20 ms | 66.75 ms | 451.44 ms | 16257.33 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 31.81 ms | 26.98 ms | 57.46 ms | 87.13 ms | 170.75 ms | 18486.00 | 
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 32.83 ms | 32.05 ms | 38.45 ms | 48.43 ms | 557.39 ms | 19342.67 | 
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 32.65 ms | 33.68 ms | 37.91 ms | 43.10 ms | 175.65 ms | 5399.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 40.64 ms | 33.82 ms | 75.02 ms | 139.58 ms | 275.03 ms | 27311.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 48.35 ms | 41.12 ms | 98.30 ms | 140.67 ms | 202.62 ms | 30636.33 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 61.60 ms | 52.94 ms | 106.69 ms | 141.94 ms | 182.70 ms | 31036.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 77.21 ms | 65.41 ms | 122.49 ms | 203.55 ms | 858.73 ms | 51499.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 70.73 ms | 68.74 ms | 97.52 ms | 128.00 ms | 381.28 ms | 23495.00 | 
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 118.39 ms | 115.03 ms | 163.62 ms | 215.16 ms | 363.40 ms | 35477.33 | 
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 183.71 ms | 167.15 ms | 259.33 ms | 611.73 ms | 1716.71 ms | 105809.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (fasthttprouter) (go)


:five: (kore) (c)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 383479.33 | 221.75 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 343610.33 | 411.50 MB |
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 339671.33 | 385.53 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 295974.67 | 475.63 MB |
| c (99) | [kore](http://kore.io) (3.1) | 286503.33 | 743.84 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 281007.00 | 318.88 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 278844.33 | 270.60 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 254799.67 | 512.14 MB |
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 254522.00 | 520.58 MB |
| java (8) | [act](http://actframework.org) (1.8) | 251323.67 | 433.79 MB |
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 249611.00 | 234.53 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 243058.67 | 140.65 MB |
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 242346.00 | 227.67 MB |
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 237007.33 | 387.45 MB |
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 235654.67 | 252.34 MB |
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 228278.00 | 285.77 MB |
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 201093.00 | 328.83 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 180643.33 | 293.90 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 174296.00 | 234.36 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 173554.33 | 229.47 MB |
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 173018.33 | 218.08 MB |
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 172709.67 | 229.96 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 167785.00 | 225.85 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 166928.67 | 292.38 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 164843.33 | 218.26 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 162540.67 | 284.98 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 161127.00 | 214.64 MB |
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 158974.33 | 247.77 MB |
| node (12.3) | [restana](http://github.com/jkyberneees/ana) (3.0) | 152295.67 | 228.34 MB |
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 149024.67 | 349.38 MB |
| node (12.3) | [rayo](http://rayo.js.org) (1.3) | 139895.33 | 209.73 MB |
| node (12.3) | [polka](http://github.com/lukeed/polka) (0.5) | 137688.00 | 206.37 MB |
| go (1.12) | [gf](http://goframe.org) (1.6) | 134828.67 | 203.69 MB |
| node (12.3) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 116538.00 | 174.55 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 116487.67 | 109.62 MB |
| node (12.3) | [foxify](http://foxify.js.org) (0.10) | 116388.67 | 244.79 MB |
| node (12.3) | [fastify](http://fastify.io) (2.3) | 115002.33 | 280.33 MB |
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 114365.00 | 226.07 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 114110.00 | 281.06 MB |
| node (12.3) | [koa](http://koajs.com) (2.7) | 109510.33 | 231.90 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 104464.00 | 183.23 MB |
| node (12.3) | [restify](http://restify.com) (8.2) | 98076.33 | 172.18 MB |
| node (12.3) | [express](http://expressjs.com) (4.16) | 89866.67 | 220.05 MB |
| python (3.7) | [hug](http://hug.rest) (2.5) | 85201.00 | 211.10 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 83922.33 | 180.56 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 81703.67 | 137.30 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 78809.33 | 168.90 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 66343.67 | 328.68 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 63716.33 | 315.83 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 63578.67 | 99.55 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 63217.00 | 313.71 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 62297.67 | 323.13 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 61890.67 | 307.21 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.23) | 57527.67 | 124.11 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 54908.33 | 101.78 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 54671.00 | 124.01 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 51155.67 | 266.31 MB |
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 50511.67 | 66.04 MB |
| node (12.3) | [hapi](http://hapijs.com) (18.1) | 48273.00 | 124.87 MB |
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 47158.00 | 88.87 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 44916.00 | 42.83 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 39701.33 | 97.75 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 38962.00 | 72.43 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 34511.33 | 19.91 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 32507.67 | 62.78 MB |
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 30697.67 | 56.00 MB |
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 30254.00 | 37.28 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 26058.67 | 46.41 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 22853.67 | 13.19 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 22253.00 | 44.34 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 19351.33 | 146.23 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 17867.00 | 46.37 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 16296.00 | 35.48 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 13608.67 | 40.19 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 13334.00 | 38.62 MB |
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 8389.00 | 21.61 MB |
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 5426.33 | 13.33 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 4184.67 | 12.82 MB |
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

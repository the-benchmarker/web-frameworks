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


:four: symfony (php)


:five: slim (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.35) | [nickel](http://nickel-org.github.io) (0.11) | 0.09 ms | 0.09 ms | 0.12 ms | 0.16 ms | 4.97 ms | 51.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 3.36 ms | 0.18 ms | 12.80 ms | 30.74 ms | 78.70 ms | 6874.00 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.62 ms | 0.23 ms | 17.69 ms | 38.52 ms | 90.70 ms | 8964.33 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 125.73 ms | 0.36 ms | 229.83 ms | 2892.20 ms | 6871.74 ms | 497277.00 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 119.31 ms | 0.37 ms | 239.92 ms | 2615.01 ms | 6803.68 ms | 473742.00 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 112.77 ms | 0.37 ms | 312.12 ms | 1840.36 ms | 7144.08 ms | 396931.00 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 187.14 ms | 0.38 ms | 322.64 ms | 4033.34 ms | 7353.74 ms | 681974.33 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 7.13 ms | 0.40 ms | 24.24 ms | 50.12 ms | 111.01 ms | 11887.67 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 8.62 ms | 0.43 ms | 30.09 ms | 61.61 ms | 174.70 ms | 14801.33 | 
| rust (1.35) | [iron](http://ironframework.io) (0.6) | 0.45 ms | 0.45 ms | 0.73 ms | 1.05 ms | 36.69 ms | 375.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 9.69 ms | 0.58 ms | 31.29 ms | 62.52 ms | 136.38 ms | 15088.67 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 148.22 ms | 0.92 ms | 285.07 ms | 3136.53 ms | 6803.72 ms | 540589.67 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 135.05 ms | 1.04 ms | 309.47 ms | 2725.71 ms | 6833.80 ms | 476075.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 90.24 ms | 1.74 ms | 3.87 ms | 3351.36 ms | 6597.47 ms | 550049.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 3.24 ms | 2.61 ms | 6.59 ms | 14.00 ms | 33.30 ms | 2993.67 | 
| rust (1.35) | [actix-web](http://actix.rs) (0.7) | 3.71 ms | 2.85 ms | 7.61 ms | 16.24 ms | 64.52 ms | 3346.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.50 ms | 3.07 ms | 6.40 ms | 13.06 ms | 30.17 ms | 2775.67 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.98 ms | 3.48 ms | 6.12 ms | 12.51 ms | 100.08 ms | 3112.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 12.54 ms | 3.49 ms | 9.27 ms | 305.87 ms | 1325.63 ms | 72800.00 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 39.19 ms | 4.04 ms | 129.09 ms | 344.58 ms | 916.74 ms | 73829.00 | 
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 4.32 ms | 4.19 ms | 7.02 ms | 13.41 ms | 31.35 ms | 2524.00 | 
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 4.74 ms | 4.25 ms | 8.56 ms | 15.43 ms | 31.36 ms | 3010.33 | 
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 4.48 ms | 4.39 ms | 7.16 ms | 13.91 ms | 29.44 ms | 2546.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 5.22 ms | 4.45 ms | 10.64 ms | 17.44 ms | 42.55 ms | 3859.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.02 ms | 4.47 ms | 5.66 ms | 10.21 ms | 25.97 ms | 1975.00 | 
| rust (1.35) | [gotham](http://gotham.rs) (0.3) | 5.10 ms | 4.49 ms | 9.59 ms | 20.09 ms | 173.62 ms | 6192.33 | 
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 4.71 ms | 4.53 ms | 7.55 ms | 14.85 ms | 33.09 ms | 2673.67 | 
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 4.71 ms | 4.54 ms | 7.39 ms | 13.80 ms | 31.10 ms | 2491.67 | 
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 5.66 ms | 5.05 ms | 9.28 ms | 14.96 ms | 40.23 ms | 2739.33 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 7.44 ms | 5.12 ms | 15.80 ms | 32.78 ms | 169.87 ms | 7207.33 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 7.42 ms | 5.18 ms | 13.22 ms | 58.95 ms | 126.40 ms | 9790.33 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 7.79 ms | 5.24 ms | 15.99 ms | 34.72 ms | 283.98 ms | 9385.00 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 7.34 ms | 5.26 ms | 15.01 ms | 31.24 ms | 178.39 ms | 6868.67 | 
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 7.84 ms | 5.30 ms | 15.85 ms | 35.55 ms | 294.63 ms | 10276.33 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 7.95 ms | 5.30 ms | 17.40 ms | 35.28 ms | 165.28 ms | 7386.33 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 7.58 ms | 5.32 ms | 15.42 ms | 32.53 ms | 194.56 ms | 7458.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.74 ms | 5.41 ms | 8.86 ms | 14.88 ms | 92.29 ms | 3146.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.54 ms | 5.73 ms | 11.11 ms | 21.17 ms | 138.69 ms | 4424.67 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 7.65 ms | 5.74 ms | 14.46 ms | 31.66 ms | 142.08 ms | 6491.00 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 7.70 ms | 5.95 ms | 14.05 ms | 30.15 ms | 127.66 ms | 5898.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 7.24 ms | 6.41 ms | 9.79 ms | 18.60 ms | 229.47 ms | 7927.67 | 
| go (1.12) | [gf](http://goframe.org) (1.6) | 9.65 ms | 6.65 ms | 19.80 ms | 43.06 ms | 303.55 ms | 10694.33 | 
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 8.39 ms | 7.06 ms | 15.99 ms | 33.15 ms | 226.00 ms | 7814.67 | 
| node (12.3) | [restana](http://github.com/jkyberneees/ana) (3.0) | 9.61 ms | 7.50 ms | 15.03 ms | 47.74 ms | 383.10 ms | 14932.67 | 
| node (12.3) | [rayo](http://rayo.js.org) (1.3) | 12.02 ms | 7.86 ms | 15.73 ms | 137.57 ms | 603.94 ms | 29734.67 | 
| node (12.3) | [polka](http://github.com/lukeed/polka) (0.5) | 12.87 ms | 8.05 ms | 15.86 ms | 147.18 ms | 672.93 ms | 36709.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 219.04 ms | 8.15 ms | 204.42 ms | 4733.27 ms | 7923.92 ms | 848207.33 | 
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 11.55 ms | 8.94 ms | 23.97 ms | 46.28 ms | 171.28 ms | 9852.00 | 
| node (12.3) | [foxify](http://foxify.js.org) (0.10) | 13.90 ms | 9.36 ms | 17.96 ms | 135.52 ms | 626.02 ms | 30399.33 | 
| node (12.3) | [fastify](http://fastify.io) (2.3) | 15.11 ms | 10.68 ms | 21.45 ms | 100.08 ms | 637.10 ms | 28555.00 | 
| node (12.3) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 20.12 ms | 10.78 ms | 24.17 ms | 278.04 ms | 637.31 ms | 45925.00 | 
| node (12.3) | [koa](http://koajs.com) (2.7) | 16.92 ms | 10.90 ms | 20.34 ms | 199.21 ms | 726.25 ms | 40503.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 14.11 ms | 11.49 ms | 25.96 ms | 45.11 ms | 215.96 ms | 9365.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 12.32 ms | 12.50 ms | 14.54 ms | 16.36 ms | 93.13 ms | 2387.00 | 
| node (12.3) | [express](http://expressjs.com) (4.16) | 18.61 ms | 13.48 ms | 24.84 ms | 156.74 ms | 668.33 ms | 32702.00 | 
| node (12.3) | [restify](http://restify.com) (8.2) | 18.25 ms | 14.00 ms | 24.11 ms | 121.80 ms | 608.91 ms | 29156.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 16.01 ms | 14.73 ms | 29.11 ms | 48.31 ms | 262.03 ms | 11741.67 | 
| python (3.7) | [hug](http://hug.rest) (2.5) | 18.61 ms | 15.71 ms | 35.05 ms | 63.16 ms | 294.39 ms | 13798.33 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 24.50 ms | 16.65 ms | 27.12 ms | 299.09 ms | 1430.94 ms | 67578.33 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 21.65 ms | 19.33 ms | 39.16 ms | 60.74 ms | 132.53 ms | 13116.67 | 
| node (12.3) | [hapi](http://hapijs.com) (18.1) | 35.65 ms | 23.69 ms | 39.52 ms | 420.39 ms | 1257.44 ms | 74443.00 | 
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 26.39 ms | 24.99 ms | 35.74 ms | 52.20 ms | 500.38 ms | 15074.00 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.23) | 28.92 ms | 26.81 ms | 47.18 ms | 66.32 ms | 136.46 ms | 13652.33 | 
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 67.19 ms | 29.62 ms | 56.67 ms | 1291.69 ms | 3264.48 ms | 225100.00 | 
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 39.40 ms | 30.79 ms | 90.10 ms | 229.65 ms | 809.34 ms | 51963.00 | 
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 33.40 ms | 32.83 ms | 42.10 ms | 49.74 ms | 332.65 ms | 12291.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 32.69 ms | 33.82 ms | 52.34 ms | 82.77 ms | 123.39 ms | 17000.67 | 
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 35.57 ms | 34.95 ms | 41.35 ms | 47.05 ms | 322.59 ms | 10219.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 42.75 ms | 38.58 ms | 72.33 ms | 115.72 ms | 429.67 ms | 23679.67 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 43.25 ms | 39.15 ms | 66.34 ms | 114.14 ms | 621.15 ms | 24819.00 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 48.74 ms | 42.11 ms | 90.18 ms | 122.35 ms | 254.10 ms | 26458.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 60.22 ms | 48.99 ms | 118.71 ms | 227.70 ms | 327.43 ms | 45890.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 80.01 ms | 66.15 ms | 148.56 ms | 194.28 ms | 259.09 ms | 44237.00 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 94.64 ms | 90.69 ms | 163.75 ms | 212.58 ms | 265.69 ms | 50548.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 126.55 ms | 95.51 ms | 262.83 ms | 339.73 ms | 817.82 ms | 81417.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 114.00 ms | 106.73 ms | 165.47 ms | 411.30 ms | 1009.68 ms | 76749.67 | 
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 109.62 ms | 106.88 ms | 152.75 ms | 202.10 ms | 297.16 ms | 33100.33 | 
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 343.07 ms | 326.63 ms | 487.61 ms | 1446.03 ms | 3110.29 ms | 251746.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (fasthttprouter) (go)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 310540.33 | 179.66 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 275634.33 | 329.69 MB |
| rust (1.35) | [actix-web](http://actix.rs) (0.7) | 264077.00 | 300.20 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 233797.33 | 375.60 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 222822.33 | 216.14 MB |
| c (99) | [kore](http://kore.io) (3.1) | 219451.00 | 569.53 MB |
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 217245.00 | 204.32 MB |
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 209139.00 | 196.57 MB |
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 206653.00 | 220.63 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 206472.00 | 234.02 MB |
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 200222.33 | 327.07 MB |
| rust (1.35) | [gotham](http://gotham.rs) (0.3) | 199601.00 | 408.36 MB |
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 199251.00 | 248.43 MB |
| java (8) | [act](http://actframework.org) (1.8) | 197456.67 | 340.88 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 193975.33 | 389.21 MB |
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 166854.33 | 272.59 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 162574.33 | 94.07 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 147510.67 | 258.52 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 144842.33 | 193.20 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 144388.33 | 193.83 MB |
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 144290.33 | 192.22 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 143669.67 | 252.02 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 140966.67 | 187.82 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 138181.67 | 183.29 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 134860.33 | 219.47 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 134464.33 | 177.10 MB |
| rust (1.35) | [iron](http://ironframework.io) (0.6) | 131501.00 | 165.78 MB |
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 122524.33 | 190.73 MB |
| go (1.12) | [gf](http://goframe.org) (1.6) | 116966.33 | 177.18 MB |
| node (12.3) | [restana](http://github.com/jkyberneees/ana) (3.0) | 116302.00 | 173.99 MB |
| node (12.3) | [rayo](http://rayo.js.org) (1.3) | 111437.00 | 166.74 MB |
| node (12.3) | [polka](http://github.com/lukeed/polka) (0.5) | 110183.00 | 164.94 MB |
| rust (1.35) | [nickel](http://nickel-org.github.io) (0.11) | 96903.33 | 192.86 MB |
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 95234.33 | 223.08 MB |
| node (12.3) | [foxify](http://foxify.js.org) (0.10) | 91806.00 | 192.77 MB |
| node (12.3) | [fastify](http://fastify.io) (2.3) | 91226.00 | 223.68 MB |
| node (12.3) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 87193.67 | 130.47 MB |
| node (12.3) | [koa](http://koajs.com) (2.7) | 81930.33 | 173.05 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 80367.67 | 75.39 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 73050.67 | 179.85 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 69763.33 | 149.68 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 67535.67 | 118.23 MB |
| node (12.3) | [express](http://expressjs.com) (4.16) | 66505.00 | 162.44 MB |
| node (12.3) | [restify](http://restify.com) (8.2) | 64314.00 | 112.64 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 61121.00 | 303.42 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 57885.33 | 287.05 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 57477.67 | 285.00 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 56528.33 | 280.17 MB |
| python (3.7) | [hug](http://hug.rest) (2.5) | 55914.33 | 138.49 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 55448.00 | 92.38 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 55077.33 | 285.55 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 52518.67 | 83.13 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 47661.00 | 102.46 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 44902.33 | 233.81 MB |
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 39781.33 | 50.95 MB |
| node (12.3) | [hapi](http://hapijs.com) (18.1) | 39297.33 | 101.51 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 38143.00 | 36.37 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 37495.33 | 69.60 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.23) | 35103.33 | 75.86 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 31142.67 | 70.50 MB |
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 29839.67 | 54.44 MB |
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 29078.67 | 54.86 MB |
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 28141.33 | 34.42 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 27700.00 | 15.96 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 24397.00 | 45.29 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 23891.00 | 58.84 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 21142.00 | 40.89 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 18029.00 | 32.10 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 17936.67 | 10.34 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 14859.67 | 112.32 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 13209.67 | 34.23 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 12657.67 | 25.21 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 10536.00 | 22.95 MB |
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 9063.67 | 23.30 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 8703.67 | 25.82 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 8322.67 | 24.14 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3270.33 | 10.02 MB |
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 2917.67 | 7.19 MB |
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

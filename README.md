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

+ Make framework list

~~~sh
bin/make config
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
Last update: 2019-06-20
```
OS: Linux (version: 5.0.16-300.fc30.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: cuba (ruby)


:four: rack-routing (ruby)


:five: iron (rust)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.35) | [nickel](https://nickel-org.github.io) (0.11) | 0.13 ms | 0.12 ms | 0.18 ms | 0.32 ms | 8.79 ms | 130.33 | 
| ruby (2.6) | [roda](https://roda.jeremyevans.net) (3.21) | 6.40 ms | 0.40 ms | 22.32 ms | 45.93 ms | 114.89 ms | 10982.33 | 
| ruby (2.6) | [cuba](https://cuba.is) (3.9) | 7.62 ms | 0.50 ms | 25.49 ms | 50.45 ms | 118.51 ms | 12261.00 | 
| ruby (2.6) | [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 8.43 ms | 0.52 ms | 28.35 ms | 55.38 ms | 133.97 ms | 13552.33 | 
| rust (1.35) | [iron](https://ironframework.io) (0.6) | 0.84 ms | 0.75 ms | 1.42 ms | 2.40 ms | 48.06 ms | 697.67 | 
| php (7.3) | [zend-framework](https://framework.zend.com) (3.1) | 149.59 ms | 0.78 ms | 374.35 ms | 2933.58 ms | 7413.42 ms | 535136.67 | 
| php (7.3) | [symfony](https://symfony.com) (4.3) | 204.22 ms | 0.93 ms | 443.83 ms | 4077.18 ms | 7677.55 ms | 718044.33 | 
| ruby (2.6) | [flame](https://github.com/AlexWayfer/flame) (4.18) | 11.88 ms | 0.94 ms | 35.98 ms | 67.81 ms | 136.98 ms | 16709.67 | 
| ruby (2.6) | [hanami](https://hanamirb.org) (1.3) | 14.02 ms | 1.02 ms | 42.82 ms | 81.14 ms | 207.43 ms | 20296.00 | 
| php (7.3) | [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 236.11 ms | 1.02 ms | 472.94 ms | 4200.85 ms | 7423.97 ms | 784873.00 | 
| ruby (2.6) | [sinatra](https://sinatrarb.com) (2.0) | 14.58 ms | 1.20 ms | 42.51 ms | 80.64 ms | 174.19 ms | 19997.67 | 
| php (7.3) | [lumen](https://lumen.laravel.com) (5.8) | 121.44 ms | 1.78 ms | 307.43 ms | 1754.74 ms | 6474.01 ms | 396474.67 | 
| php (7.3) | [laravel](https://laravel.com) (5.8) | 142.57 ms | 2.43 ms | 363.14 ms | 2231.06 ms | 7518.58 ms | 466230.33 | 
| php (7.3) | [slim](https://slimframework.com) (3.12) | 212.92 ms | 2.60 ms | 349.70 ms | 4078.20 ms | 6117.24 ms | 729155.33 | 
| rust (nightly) | [rocket](https://rocket.rs) (0.4) | 66.18 ms | 3.12 ms | 6.06 ms | 2247.48 ms | 6594.82 ms | 439597.67 | 
| c (11) | [agoo-c](https://github.com/ohler55/agoo-c) (0.5) | 4.46 ms | 3.75 ms | 9.01 ms | 19.10 ms | 55.49 ms | 4097.67 | 
| python (3.7) | [japronto](https://github.com/squeaky-pl/japronto) (0.1) | 4.91 ms | 4.48 ms | 9.94 ms | 20.28 ms | 41.92 ms | 4265.33 | 
| cpp (11) | [evhtp](https://criticalstack/libevhtp) (1.2) | 5.34 ms | 5.24 ms | 8.05 ms | 12.77 ms | 50.07 ms | 2432.00 | 
| rust (1.35) | [gotham](https://gotham.rs) (0.3) | 6.75 ms | 5.50 ms | 11.30 ms | 22.93 ms | 355.20 ms | 8281.00 | 
| c (99) | [kore](https://kore.io) (3.1) | 10.11 ms | 5.68 ms | 11.21 ms | 128.67 ms | 1152.40 ms | 45402.67 | 
| rust (1.35) | [actix-web](https://actix.rs) (0.7) | 6.16 ms | 5.78 ms | 11.12 ms | 20.80 ms | 49.11 ms | 4009.67 | 
| ruby (2.6) | [agoo](https://github.com/ohler55/agoo) (2.8) | 9.65 ms | 6.02 ms | 16.13 ms | 85.82 ms | 138.50 ms | 13448.67 | 
| python (3.6) | [vibora](https://vibora.io) (0.0) | 6.91 ms | 6.10 ms | 13.95 ms | 23.84 ms | 70.35 ms | 5194.00 | 
| crystal (0.29) | [router.cr](https://github.com/tbrand/router.cr) (0.2) | 7.20 ms | 6.24 ms | 11.29 ms | 18.88 ms | 45.60 ms | 3337.00 | 
| crystal (0.29) | [kemal](https://kemalcr.com) (0.25) | 7.58 ms | 6.57 ms | 11.61 ms | 20.64 ms | 49.44 ms | 3609.33 | 
| crystal (0.29) | [spider-gazelle](https://spider-gazelle.net) (1.4) | 8.34 ms | 7.04 ms | 13.82 ms | 25.69 ms | 65.25 ms | 4758.67 | 
| crystal (0.29) | [raze](https://razecr.com) (0.3) | 8.15 ms | 7.23 ms | 12.18 ms | 21.47 ms | 56.26 ms | 3764.00 | 
| go (1.12) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 8.16 ms | 7.25 ms | 12.10 ms | 24.16 ms | 122.83 ms | 4772.33 | 
| crystal (0.29) | [amber](https://amberframework.org) (0.28) | 8.18 ms | 7.46 ms | 11.87 ms | 20.37 ms | 47.56 ms | 3507.00 | 
| nim (0.2) | [jester](https://github.com/dom96/jester) (0.4) | 8.74 ms | 7.80 ms | 14.21 ms | 25.00 ms | 139.83 ms | 5036.33 | 
| crystal (0.29) | [orion](https://github.com/obsidian/orion) (1.7) | 9.52 ms | 9.24 ms | 14.72 ms | 24.64 ms | 63.81 ms | 4314.33 | 
| scala (2.12) | [akkahttp](https://akka.io) (10.1) | 177.23 ms | 9.77 ms | 32.96 ms | 4060.87 ms | 7225.40 ms | 707542.67 | 
| ruby (2.6) | [rails](https://rubyonrails.org) (5.2) | 59.94 ms | 9.78 ms | 190.85 ms | 463.81 ms | 1174.05 ms | 101450.33 | 
| java (8) | [act](https://actframework.org) (1.8) | 12.35 ms | 9.96 ms | 19.78 ms | 45.72 ms | 176.57 ms | 8882.33 | 
| go (1.12) | [gorouter](https://github.com/vardius/gorouter/wiki) (4.0) | 13.50 ms | 10.01 ms | 26.92 ms | 62.20 ms | 281.98 ms | 12563.00 | 
| go (1.12) | [echo](https://echo.labstack.com) (4.1) | 13.44 ms | 10.20 ms | 26.52 ms | 57.29 ms | 214.53 ms | 11412.00 | 
| go (1.12) | [chi](https://github.com/go-chi/chi) (4.0) | 13.38 ms | 10.25 ms | 25.36 ms | 58.61 ms | 288.49 ms | 12467.67 | 
| go (1.12) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 14.55 ms | 10.32 ms | 30.65 ms | 68.46 ms | 226.33 ms | 13563.00 | 
| node (12.4) | [0http](https://github.com/jkyberneees/0http) (1.0) | 13.46 ms | 10.34 ms | 19.32 ms | 47.44 ms | 535.97 ms | 21151.67 | 
| kotlin (1.3) | [ktor](https://ktor.io) (1.2) | 12.99 ms | 10.42 ms | 24.91 ms | 50.93 ms | 250.97 ms | 11580.67 | 
| go (1.12) | [violetear](https://violetear.org) (7.0) | 13.14 ms | 10.75 ms | 22.70 ms | 50.99 ms | 187.97 ms | 9731.33 | 
| go (1.12) | [gin](https://gin-gonic.com) (1.4) | 14.92 ms | 10.97 ms | 29.29 ms | 66.76 ms | 292.01 ms | 13853.67 | 
| go (1.12) | [kami](https://github.com/guregu/kami) (2.2) | 13.32 ms | 10.99 ms | 22.15 ms | 51.95 ms | 124.07 ms | 9141.00 | 
| node (12.4) | [restana](https://github.com/jkyberneees/ana) (3.1) | 20.42 ms | 11.32 ms | 21.55 ms | 334.17 ms | 1026.75 ms | 62433.67 | 
| csharp (7.3) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (2.2) | 13.70 ms | 11.69 ms | 16.97 ms | 50.59 ms | 590.94 ms | 23271.00 | 
| go (1.12) | [gf](https://goframe.org) (1.6) | 18.27 ms | 13.70 ms | 35.66 ms | 81.20 ms | 264.36 ms | 16349.33 | 
| python (3.7) | [falcon](https://falconframework.org) (2.0) | 17.30 ms | 14.27 ms | 29.06 ms | 55.78 ms | 310.84 ms | 13343.67 | 
| go (1.12) | [beego](https://beego.me) (1.12) | 19.54 ms | 14.55 ms | 37.20 ms | 90.85 ms | 291.86 ms | 17363.67 | 
| node (12.4) | [polka](https://github.com/lukeed/polka) (0.5) | 22.80 ms | 14.59 ms | 25.87 ms | 301.39 ms | 1039.76 ms | 58279.67 | 
| node (12.4) | [rayo](https://rayo.js.org) (1.3) | 26.06 ms | 15.59 ms | 27.67 ms | 393.41 ms | 1191.32 ms | 71415.67 | 
| swift (5.0) | [perfect](https://perfect.org) (3.1) | 17.26 ms | 17.00 ms | 20.81 ms | 24.80 ms | 46.46 ms | 2823.67 | 
| python (3.7) | [bottle](https://bottlepy.org) (0.12) | 21.06 ms | 17.66 ms | 36.42 ms | 64.94 ms | 214.92 ms | 12320.00 | 
| node (12.4) | [koa](https://koajs.com) (2.7) | 31.41 ms | 19.86 ms | 34.22 ms | 408.03 ms | 1227.90 ms | 73879.00 | 
| node (12.4) | [foxify](https://foxify.js.org) (0.1) | 34.15 ms | 20.75 ms | 34.55 ms | 520.89 ms | 1358.30 ms | 86689.33 | 
| python (3.7) | [starlette](https://starlette.io) (0.12) | 23.73 ms | 20.94 ms | 38.70 ms | 67.39 ms | 166.15 ms | 12533.33 | 
| node (12.4) | [fastify](https://fastify.io) (2.4) | 35.25 ms | 22.70 ms | 38.12 ms | 476.94 ms | 1394.02 ms | 82185.67 | 
| scala (2.12) | [http4s](https://http4s.org) (0.18) | 29.77 ms | 22.75 ms | 45.24 ms | 192.25 ms | 1791.70 ms | 65328.33 | 
| python (3.7) | [hug](https://hug.rest) (2.5) | 30.74 ms | 22.81 ms | 61.73 ms | 95.89 ms | 237.24 ms | 20048.67 | 
| swift (5.0) | [vapor](https://vapor.codes) (3.3) | 48.00 ms | 24.38 ms | 37.47 ms | 926.20 ms | 2622.15 ms | 160864.33 | 
| java (8) | [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 35.81 ms | 27.73 ms | 43.52 ms | 278.70 ms | 857.43 ms | 51233.00 | 
| node (12.4) | [express](https://expressjs.com) (4.16) | 45.67 ms | 28.38 ms | 45.35 ms | 645.24 ms | 1628.09 ms | 107370.00 | 
| node (12.4) | [restify](https://restify.com) (8.2) | 43.21 ms | 32.23 ms | 57.91 ms | 326.89 ms | 1011.73 ms | 60076.33 | 
| python (3.7) | [fastapi](https://fastapi.tiangolo.com) (0.29) | 39.87 ms | 33.29 ms | 73.58 ms | 105.92 ms | 159.91 ms | 22292.33 | 
| swift (5.0) | [kitura](https://kitura.io) (2.7) | 44.08 ms | 38.81 ms | 58.63 ms | 161.34 ms | 1133.37 ms | 50097.33 | 
| node (12.4) | [muneem](https://github.com/node-muneem/muneem) (2.4) | 114.80 ms | 45.56 ms | 112.64 ms | 1737.15 ms | 3103.27 ms | 295926.33 | 
| swift (5.0) | [kitura-nio](https://kitura.io) (2.7) | 111.70 ms | 48.36 ms | 83.90 ms | 1752.15 ms | 3814.07 ms | 314968.67 | 
| node (12.4) | [hapi](https://hapijs.com) (18.1) | 112.49 ms | 51.21 ms | 84.65 ms | 1651.65 ms | 2859.98 ms | 276118.67 | 
| python (3.7) | [molten](https://moltenframework.com) (0.27) | 76.89 ms | 59.62 ms | 146.31 ms | 214.37 ms | 449.25 ms | 43806.00 | 
| crystal (0.28) | [lucky](https://luckyframework.org) (0.14) | 67.60 ms | 62.76 ms | 92.57 ms | 163.74 ms | 459.41 ms | 26413.67 | 
| python (3.7) | [aiohttp](https://aiohttp.readthedocs.io) (3.5) | 72.94 ms | 66.05 ms | 122.85 ms | 183.13 ms | 305.21 ms | 36008.33 | 
| crystal (0.29) | [athena](https://github.com/blacksmoke16/athena) (0.6) | 86.73 ms | 66.81 ms | 227.26 ms | 453.11 ms | 1045.04 ms | 106631.00 | 
| python (3.7) | [flask](https://flask.pocoo.org) (1.0) | 91.57 ms | 68.38 ms | 179.44 ms | 295.44 ms | 595.47 ms | 57319.67 | 
| node (12.4) | [turbo_polka](https://github.com/mafintosh/turbo-http) (2.0) | 76.39 ms | 70.10 ms | 85.00 ms | 302.99 ms | 1060.46 ms | 57583.00 | 
| python (3.7) | [bocadillo](https://bocadilloproject.github.io) (0.16) | 92.61 ms | 83.11 ms | 164.45 ms | 267.56 ms | 482.78 ms | 56068.67 | 
| python (3.7) | [quart](https://pgjones.gitlab.io/quart) (0.9) | 96.54 ms | 89.09 ms | 170.69 ms | 233.01 ms | 352.42 ms | 50917.33 | 
| python (3.7) | [sanic](https://github.com/huge-success/sanic) (19.3) | 112.23 ms | 100.28 ms | 193.88 ms | 309.09 ms | 542.06 ms | 63223.33 | 
| crystal (0.29) | [onyx](https://onyxframework.org) (0.5) | 125.36 ms | 121.42 ms | 177.14 ms | 234.68 ms | 358.96 ms | 39431.00 | 
| python (3.7) | [responder](https://python-responder.org) (1.3) | 162.35 ms | 149.46 ms | 268.05 ms | 353.35 ms | 448.06 ms | 73186.33 | 
| python (3.7) | [tornado](https://tornadoweb.org) (5.1) | 173.08 ms | 157.08 ms | 215.41 ms | 772.06 ms | 1846.89 ms | 128187.67 | 
| python (3.7) | [masonite](https://masoniteproject.com) (2.2) | 171.77 ms | 169.35 ms | 211.64 ms | 375.01 ms | 1523.67 ms | 73729.67 | 
| python (3.7) | [django](https://djangoproject.com) (2.2) | 204.04 ms | 196.25 ms | 272.74 ms | 414.20 ms | 1295.83 ms | 74798.67 | 
| python (3.7) | [cyclone](https://cyclone.io) (0.0) | 411.88 ms | 366.66 ms | 448.96 ms | 3206.11 ms | 5888.94 ms | 529281.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (evhtp) (cpp)


:four: (kore) (c)


:five: (vibora) (python)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](https://github.com/ohler55/agoo-c) (0.5) | 220395.33 | 127.52 MB |
| python (3.7) | [japronto](https://github.com/squeaky-pl/japronto) (0.1) | 207245.33 | 248.09 MB |
| cpp (11) | [evhtp](https://criticalstack/libevhtp) (1.2) | 168574.67 | 163.69 MB |
| c (99) | [kore](https://kore.io) (3.1) | 163352.67 | 424.44 MB |
| python (3.6) | [vibora](https://vibora.io) (0.0) | 153034.67 | 173.62 MB |
| rust (1.35) | [actix-web](https://actix.rs) (0.7) | 151870.67 | 172.63 MB |
| rust (1.35) | [gotham](https://gotham.rs) (0.3) | 147317.00 | 301.74 MB |
| ruby (2.6) | [agoo](https://github.com/ohler55/agoo) (2.8) | 132435.67 | 76.65 MB |
| crystal (0.29) | [router.cr](https://github.com/tbrand/router.cr) (0.2) | 131112.00 | 123.16 MB |
| crystal (0.29) | [kemal](https://kemalcr.com) (0.25) | 125597.67 | 205.31 MB |
| nim (0.2) | [jester](https://github.com/dom96/jester) (0.4) | 123427.00 | 247.74 MB |
| crystal (0.29) | [spider-gazelle](https://spider-gazelle.net) (1.4) | 117526.00 | 124.99 MB |
| crystal (0.29) | [raze](https://razecr.com) (0.3) | 117174.00 | 110.02 MB |
| crystal (0.29) | [amber](https://amberframework.org) (0.28) | 116928.33 | 213.83 MB |
| go (1.12) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 116691.00 | 189.56 MB |
| crystal (0.29) | [orion](https://github.com/obsidian/orion) (1.7) | 101821.67 | 166.23 MB |
| java (8) | [act](https://actframework.org) (1.8) | 92493.67 | 159.64 MB |
| go (1.12) | [gorouter](https://github.com/vardius/gorouter/wiki) (4.0) | 83138.67 | 110.02 MB |
| node (12.4) | [0http](https://github.com/jkyberneees/0http) (1.0) | 81918.00 | 122.60 MB |
| go (1.12) | [chi](https://github.com/go-chi/chi) (4.0) | 81706.00 | 109.86 MB |
| go (1.12) | [echo](https://echo.labstack.com) (4.1) | 81565.67 | 143.23 MB |
| kotlin (1.3) | [ktor](https://ktor.io) (1.2) | 80662.67 | 125.71 MB |
| go (1.12) | [violetear](https://violetear.org) (7.0) | 79351.67 | 105.80 MB |
| go (1.12) | [kami](https://github.com/guregu/kami) (2.2) | 78404.33 | 104.29 MB |
| node (12.4) | [restana](https://github.com/jkyberneees/ana) (3.1) | 78002.67 | 116.92 MB |
| go (1.12) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 77586.00 | 103.42 MB |
| csharp (7.3) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (2.2) | 74597.67 | 121.52 MB |
| go (1.12) | [gin](https://gin-gonic.com) (1.4) | 73757.33 | 129.49 MB |
| rust (1.35) | [iron](https://ironframework.io) (0.6) | 72543.67 | 92.09 MB |
| rust (1.35) | [nickel](https://nickel-org.github.io) (0.11) | 67502.67 | 134.46 MB |
| go (1.12) | [gf](https://goframe.org) (1.6) | 61239.67 | 92.67 MB |
| node (12.4) | [polka](https://github.com/lukeed/polka) (0.5) | 60535.67 | 90.67 MB |
| go (1.12) | [beego](https://beego.me) (1.12) | 59538.67 | 77.32 MB |
| python (3.7) | [falcon](https://falconframework.org) (2.0) | 59312.00 | 138.90 MB |
| swift (5.0) | [perfect](https://perfect.org) (3.1) | 56973.67 | 53.58 MB |
| node (12.4) | [rayo](https://rayo.js.org) (1.3) | 56754.33 | 84.94 MB |
| scala (2.12) | [akkahttp](https://akka.io) (10.1) | 49569.33 | 106.34 MB |
| python (3.7) | [bottle](https://bottlepy.org) (0.12) | 48394.67 | 119.20 MB |
| node (12.4) | [koa](https://koajs.com) (2.7) | 44346.67 | 93.84 MB |
| node (12.4) | [fastify](https://fastify.io) (2.4) | 43473.33 | 108.59 MB |
| scala (2.12) | [http4s](https://http4s.org) (0.18) | 43244.00 | 75.78 MB |
| node (12.4) | [foxify](https://foxify.js.org) (0.1) | 43091.33 | 90.50 MB |
| python (3.7) | [starlette](https://starlette.io) (0.12) | 42664.00 | 92.04 MB |
| java (8) | [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 37775.00 | 27.57 MB |
| swift (5.0) | [vapor](https://vapor.codes) (3.3) | 37655.67 | 63.04 MB |
| python (3.7) | [hug](https://hug.rest) (2.5) | 34758.33 | 86.15 MB |
| node (12.4) | [express](https://expressjs.com) (4.16) | 32257.33 | 78.96 MB |
| rust (nightly) | [rocket](https://rocket.rs) (0.4) | 29840.33 | 47.21 MB |
| node (12.4) | [restify](https://restify.com) (8.2) | 27832.67 | 48.88 MB |
| php (7.3) | [zend-framework](https://framework.zend.com) (3.1) | 27694.33 | 137.66 MB |
| php (7.3) | [slim](https://slimframework.com) (3.12) | 27224.00 | 135.31 MB |
| python (3.7) | [fastapi](https://fastapi.tiangolo.com) (0.29) | 26350.33 | 56.96 MB |
| php (7.3) | [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 24978.33 | 124.06 MB |
| php (7.3) | [symfony](https://symfony.com) (4.3) | 24611.00 | 122.24 MB |
| swift (5.0) | [kitura](https://kitura.io) (2.7) | 23935.33 | 44.48 MB |
| node (12.4) | [muneem](https://github.com/node-muneem/muneem) (2.4) | 21137.67 | 31.68 MB |
| php (7.3) | [lumen](https://lumen.laravel.com) (5.8) | 20241.33 | 105.21 MB |
| ruby (2.6) | [roda](https://roda.jeremyevans.net) (3.21) | 19934.00 | 19.03 MB |
| swift (5.0) | [kitura-nio](https://kitura.io) (2.7) | 18556.00 | 35.02 MB |
| crystal (0.29) | [athena](https://github.com/blacksmoke16/athena) (0.6) | 18014.00 | 22.64 MB |
| node (12.4) | [hapi](https://hapijs.com) (18.1) | 17757.67 | 46.06 MB |
| php (7.3) | [laravel](https://laravel.com) (5.8) | 17161.67 | 89.31 MB |
| ruby (2.6) | [cuba](https://cuba.is) (3.9) | 16718.67 | 19.71 MB |
| ruby (2.6) | [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 15218.33 | 8.78 MB |
| crystal (0.28) | [lucky](https://luckyframework.org) (0.14) | 14377.67 | 17.65 MB |
| python (3.7) | [molten](https://moltenframework.com) (0.27) | 14232.67 | 26.49 MB |
| python (3.7) | [aiohttp](https://aiohttp.readthedocs.io) (3.5) | 13913.00 | 31.56 MB |
| node (12.4) | [turbo_polka](https://github.com/mafintosh/turbo-http) (2.0) | 13598.33 | 12.76 MB |
| python (3.7) | [bocadillo](https://bocadilloproject.github.io) (0.16) | 11731.33 | 22.72 MB |
| python (3.7) | [flask](https://flask.pocoo.org) (1.0) | 11439.67 | 28.21 MB |
| ruby (2.6) | [flame](https://github.com/AlexWayfer/flame) (4.18) | 10796.67 | 6.23 MB |
| python (3.7) | [quart](https://pgjones.gitlab.io/quart) (0.9) | 10459.00 | 20.85 MB |
| python (3.7) | [sanic](https://github.com/huge-success/sanic) (19.3) | 9176.33 | 16.38 MB |
| ruby (2.6) | [hanami](https://hanamirb.org) (1.3) | 9125.67 | 69.09 MB |
| ruby (2.6) | [sinatra](https://sinatrarb.com) (2.0) | 8753.67 | 22.72 MB |
| crystal (0.29) | [onyx](https://onyxframework.org) (0.5) | 7900.00 | 20.37 MB |
| python (3.7) | [responder](https://python-responder.org) (1.3) | 6113.00 | 13.34 MB |
| python (3.7) | [tornado](https://tornadoweb.org) (5.1) | 5908.67 | 17.38 MB |
| python (3.7) | [masonite](https://masoniteproject.com) (2.2) | 5714.00 | 12.94 MB |
| python (3.7) | [django](https://djangoproject.com) (2.2) | 4691.67 | 13.62 MB |
| ruby (2.6) | [rails](https://rubyonrails.org) (5.2) | 2121.67 | 6.50 MB |
| python (3.7) | [cyclone](https://cyclone.io) (0.0) | 1415.67 | 3.85 MB |
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

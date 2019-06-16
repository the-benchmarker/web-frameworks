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
Last update: 2019-06-16
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


:five: slim (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.35) | [nickel](https://nickel-org.github.io) (0.11) | 0.10 ms | 0.10 ms | 0.16 ms | 0.20 ms | 1.32 ms | 43.67 | 
| ruby (2.6) | [roda](https://roda.jeremyevans.net) (3.2) | 5.02 ms | 0.33 ms | 17.36 ms | 36.13 ms | 86.64 ms | 8552.00 | 
| ruby (2.6) | [cuba](https://cuba.is) (3.9) | 5.85 ms | 0.38 ms | 19.57 ms | 39.95 ms | 117.38 ms | 9588.33 | 
| ruby (2.6) | [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 6.48 ms | 0.41 ms | 21.94 ms | 42.90 ms | 98.21 ms | 10527.67 | 
| php (7.3) | [slim](https://slimframework.com) (3.12) | 174.39 ms | 0.55 ms | 295.49 ms | 3862.40 ms | 7494.57 ms | 673776.33 | 
| php (7.3) | [lumen](https://lumen.laravel.com) (5.8) | 159.20 ms | 0.56 ms | 308.91 ms | 3370.26 ms | 7012.22 ms | 582517.33 | 
| rust (1.35) | [iron](https://ironframework.io) (0.6) | 0.60 ms | 0.57 ms | 1.01 ms | 1.45 ms | 13.92 ms | 329.67 | 
| php (7.3) | [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 205.73 ms | 0.57 ms | 355.87 ms | 4216.69 ms | 6984.45 ms | 723416.67 | 
| php (7.3) | [zend-framework](https://framework.zend.com) (3.1) | 222.07 ms | 0.59 ms | 421.05 ms | 4838.86 ms | 7695.38 ms | 795665.00 | 
| php (7.3) | [laravel](https://laravel.com) (5.8) | 212.95 ms | 0.60 ms | 414.43 ms | 4797.24 ms | 7902.82 ms | 785096.00 | 
| ruby (2.6) | [flame](https://github.com/AlexWayfer/flame) (4.18) | 9.67 ms | 0.72 ms | 29.62 ms | 56.74 ms | 120.64 ms | 13832.33 | 
| ruby (2.6) | [hanami](https://hanamirb.org) (1.3) | 10.70 ms | 0.73 ms | 33.75 ms | 64.09 ms | 150.69 ms | 15976.00 | 
| ruby (2.6) | [sinatra](https://sinatrarb.com) (2.0) | 12.02 ms | 0.95 ms | 35.03 ms | 66.24 ms | 151.33 ms | 16427.33 | 
| php (7.3) | [symfony](https://symfony.com) (4.3) | 182.80 ms | 1.62 ms | 363.30 ms | 3742.14 ms | 7117.29 ms | 635334.67 | 
| rust (nightly) | [rocket](https://rocket.rs) (0.4) | 93.87 ms | 2.41 ms | 5.19 ms | 3600.64 ms | 6593.93 ms | 582387.00 | 
| c (11) | [agoo-c](https://github.com/ohler55/agoo-c) (0.5) | 3.55 ms | 2.70 ms | 7.22 ms | 14.65 ms | 36.12 ms | 3230.00 | 
| c (99) | [kore](https://kore.io) (3.1) | 7.78 ms | 3.58 ms | 9.17 ms | 96.85 ms | 753.86 ms | 35776.00 | 
| python (3.7) | [japronto](https://github.com/squeaky-pl/japronto) (0.1) | 4.54 ms | 4.00 ms | 9.31 ms | 18.32 ms | 37.28 ms | 3918.00 | 
| rust (1.35) | [actix-web](https://actix.rs) (0.7) | 4.47 ms | 4.17 ms | 8.14 ms | 16.32 ms | 96.14 ms | 3615.00 | 
| python (3.6) | [vibora](https://vibora.io) (0.0) | 5.84 ms | 4.90 ms | 11.78 ms | 19.33 ms | 52.30 ms | 4251.33 | 
| nim (0.2) | [jester](https://github.com/dom96/jester) (0.4) | 5.26 ms | 5.16 ms | 7.95 ms | 14.89 ms | 43.10 ms | 2802.67 | 
| ruby (2.6) | [agoo](https://github.com/ohler55/agoo) (2.8) | 8.63 ms | 5.21 ms | 15.18 ms | 80.51 ms | 134.22 ms | 12963.67 | 
| go (1.12) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.66 ms | 5.28 ms | 8.48 ms | 17.45 ms | 59.80 ms | 2984.00 | 
| cpp (11) | [evhtp](https://criticalstack/libevhtp) (1.2) | 5.36 ms | 5.30 ms | 7.30 ms | 11.44 ms | 34.97 ms | 2030.67 | 
| crystal (0.29) | [router.cr](https://github.com/tbrand/router.cr) (0.2) | 5.77 ms | 5.40 ms | 8.98 ms | 15.57 ms | 36.49 ms | 2699.00 | 
| rust (1.35) | [gotham](https://gotham.rs) (0.3) | 6.41 ms | 5.50 ms | 10.89 ms | 21.09 ms | 102.28 ms | 4572.33 | 
| crystal (0.29) | [raze](https://razecr.com) (0.3) | 6.10 ms | 5.55 ms | 9.54 ms | 15.43 ms | 41.83 ms | 2670.00 | 
| crystal (0.29) | [spider-gazelle](https://spider-gazelle.net) (1.4) | 6.79 ms | 5.74 ms | 11.21 ms | 21.22 ms | 43.93 ms | 3806.00 | 
| crystal (0.29) | [kemal](https://kemalcr.com) (0.25) | 6.96 ms | 6.07 ms | 10.74 ms | 17.57 ms | 43.16 ms | 3037.67 | 
| ruby (2.6) | [rails](https://rubyonrails.org) (5.2) | 47.59 ms | 6.09 ms | 154.46 ms | 394.12 ms | 940.06 ms | 84891.67 | 
| crystal (0.29) | [amber](https://amberframework.org) (0.28) | 7.18 ms | 6.39 ms | 10.70 ms | 16.88 ms | 43.73 ms | 2955.67 | 
| java (8) | [act](https://actframework.org) (1.8) | 11.82 ms | 7.95 ms | 14.57 ms | 152.25 ms | 357.24 ms | 23495.33 | 
| node (12.4) | [0http](https://github.com/jkyberneees/0http) (1.0) | 9.87 ms | 7.98 ms | 13.02 ms | 45.61 ms | 512.42 ms | 20280.00 | 
| go (1.12) | [chi](https://github.com/go-chi/chi) (4.0) | 10.48 ms | 8.10 ms | 20.11 ms | 43.76 ms | 221.74 ms | 9139.00 | 
| crystal (0.29) | [orion](https://github.com/obsidian/orion) (1.7) | 8.62 ms | 8.27 ms | 12.70 ms | 21.02 ms | 58.31 ms | 3667.33 | 
| csharp (7.3) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (2.2) | 8.79 ms | 8.32 ms | 11.85 ms | 22.14 ms | 295.85 ms | 7407.00 | 
| go (1.12) | [echo](https://echo.labstack.com) (4.1) | 11.09 ms | 8.33 ms | 22.43 ms | 47.58 ms | 212.70 ms | 9631.00 | 
| go (1.12) | [gorouter](https://github.com/vardius/gorouter/wiki) (4.0) | 11.36 ms | 8.36 ms | 23.03 ms | 51.41 ms | 297.61 ms | 11243.67 | 
| go (1.12) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 11.93 ms | 8.38 ms | 25.58 ms | 54.49 ms | 296.61 ms | 12049.67 | 
| scala (2.12) | [akkahttp](https://akka.io) (10.1) | 196.99 ms | 8.56 ms | 26.61 ms | 4669.39 ms | 7530.18 ms | 797628.67 | 
| node (12.4) | [restana](https://github.com/jkyberneees/ana) (3.1) | 11.34 ms | 8.78 ms | 14.63 ms | 91.35 ms | 544.87 ms | 25247.00 | 
| go (1.12) | [violetear](https://violetear.org) (7.0) | 10.92 ms | 8.92 ms | 18.72 ms | 43.14 ms | 287.77 ms | 10737.00 | 
| go (1.12) | [beego](https://beego.me) (1.12) | 11.97 ms | 9.11 ms | 22.44 ms | 50.63 ms | 351.52 ms | 14044.67 | 
| go (1.12) | [gin](https://gin-gonic.com) (1.4) | 12.95 ms | 9.42 ms | 24.63 ms | 55.45 ms | 447.70 ms | 17696.67 | 
| go (1.12) | [kami](https://github.com/guregu/kami) (2.2) | 11.36 ms | 9.42 ms | 19.22 ms | 43.80 ms | 223.20 ms | 10558.67 | 
| node (12.4) | [polka](https://github.com/lukeed/polka) (0.5) | 12.16 ms | 9.67 ms | 17.06 ms | 40.53 ms | 487.36 ms | 18928.67 | 
| node (12.4) | [rayo](https://rayo.js.org) (1.3) | 15.01 ms | 9.93 ms | 17.68 ms | 166.97 ms | 670.91 ms | 35603.33 | 
| kotlin (1.3) | [ktor](https://ktor.io) (1.2) | 12.51 ms | 10.17 ms | 22.06 ms | 45.48 ms | 272.77 ms | 11633.67 | 
| python (3.7) | [falcon](https://falconframework.org) (2.0) | 13.32 ms | 10.37 ms | 23.42 ms | 43.98 ms | 291.22 ms | 10782.33 | 
| go (1.12) | [gf](https://goframe.org) (1.6) | 15.43 ms | 11.19 ms | 31.20 ms | 68.30 ms | 209.47 ms | 13433.33 | 
| swift (5.0) | [perfect](https://perfect.org) (3.1) | 11.64 ms | 11.58 ms | 14.46 ms | 16.57 ms | 35.78 ms | 2169.33 | 
| node (12.4) | [muneem](https://github.com/node-muneem/muneem) (2.4) | 16.61 ms | 12.04 ms | 22.18 ms | 108.41 ms | 775.15 ms | 34622.00 | 
| node (12.4) | [fastify](https://fastify.io) (2.4) | 17.74 ms | 12.70 ms | 22.18 ms | 133.82 ms | 800.15 ms | 36722.67 | 
| node (12.4) | [koa](https://koajs.com) (2.7) | 19.78 ms | 13.02 ms | 23.64 ms | 226.62 ms | 953.37 ms | 49659.33 | 
| node (12.4) | [foxify](https://foxify.js.org) (0.1) | 19.13 ms | 13.14 ms | 23.02 ms | 196.18 ms | 823.30 ms | 42998.33 | 
| node (12.4) | [restify](https://restify.com) (8.2) | 15.61 ms | 14.02 ms | 18.92 ms | 41.68 ms | 392.42 ms | 12879.67 | 
| swift (5.0) | [vapor](https://vapor.codes) (3.3) | 19.73 ms | 14.28 ms | 26.44 ms | 125.74 ms | 1069.94 ms | 44333.00 | 
| python (3.7) | [bottle](https://bottlepy.org) (0.12) | 19.87 ms | 14.40 ms | 39.55 ms | 77.21 ms | 300.47 ms | 15666.67 | 
| node (12.4) | [express](https://expressjs.com) (4.16) | 20.02 ms | 15.71 ms | 26.81 ms | 113.91 ms | 743.61 ms | 33243.00 | 
| python (3.7) | [starlette](https://starlette.io) (0.12) | 20.98 ms | 18.06 ms | 37.20 ms | 59.27 ms | 150.95 ms | 12263.67 | 
| python (3.7) | [hug](https://hug.rest) (2.5) | 23.32 ms | 19.03 ms | 39.83 ms | 80.78 ms | 267.98 ms | 15127.33 | 
| scala (2.12) | [http4s](https://http4s.org) (0.18) | 23.84 ms | 22.02 ms | 42.83 ms | 69.05 ms | 377.01 ms | 15894.33 | 
| swift (5.0) | [kitura](https://kitura.io) (2.7) | 29.07 ms | 25.72 ms | 40.65 ms | 59.80 ms | 772.65 ms | 26350.67 | 
| python (3.7) | [fastapi](https://fastapi.tiangolo.com) (0.29) | 32.34 ms | 28.61 ms | 56.85 ms | 77.88 ms | 180.84 ms | 17206.67 | 
| swift (5.0) | [kitura-nio](https://kitura.io) (2.7) | 46.05 ms | 29.07 ms | 49.19 ms | 653.49 ms | 1971.69 ms | 120856.33 | 
| node (12.4) | [hapi](https://hapijs.com) (18.1) | 47.57 ms | 29.14 ms | 42.93 ms | 632.26 ms | 1509.38 ms | 107715.00 | 
| python (3.7) | [aiohttp](https://aiohttp.readthedocs.io) (3.5) | 35.45 ms | 31.28 ms | 58.14 ms | 80.96 ms | 112.73 ms | 16365.67 | 
| python (3.7) | [molten](https://moltenframework.com) (0.27) | 48.59 ms | 37.01 ms | 93.07 ms | 165.89 ms | 526.76 ms | 36064.33 | 
| crystal (0.28) | [lucky](https://luckyframework.org) (0.14) | 44.25 ms | 40.67 ms | 55.07 ms | 136.74 ms | 327.80 ms | 20482.00 | 
| python (3.7) | [flask](https://flask.pocoo.org) (1.0) | 53.83 ms | 41.60 ms | 103.09 ms | 160.33 ms | 373.69 ms | 31713.33 | 
| node (12.4) | [turbo_polka](https://github.com/mafintosh/turbo-http) (2.0) | 47.35 ms | 46.02 ms | 53.58 ms | 65.95 ms | 398.52 ms | 9939.67 | 
| java (8) | [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 61.99 ms | 50.64 ms | 149.78 ms | 333.06 ms | 850.28 ms | 73115.67 | 
| python (3.7) | [bocadillo](https://bocadilloproject.github.io) (0.16) | 56.35 ms | 51.15 ms | 101.27 ms | 150.49 ms | 296.79 ms | 30518.33 | 
| crystal (0.29) | [athena](https://github.com/blacksmoke16/athena) (0.6) | 76.73 ms | 59.18 ms | 201.19 ms | 399.65 ms | 946.84 ms | 94173.00 | 
| python (3.7) | [sanic](https://github.com/huge-success/sanic) (19.3) | 78.69 ms | 68.83 ms | 141.96 ms | 220.88 ms | 365.63 ms | 45539.33 | 
| python (3.7) | [quart](https://pgjones.gitlab.io/quart) (0.9) | 85.23 ms | 82.38 ms | 139.00 ms | 182.19 ms | 243.31 ms | 36742.33 | 
| python (3.7) | [django](https://djangoproject.com) (2.2) | 114.78 ms | 102.46 ms | 183.41 ms | 258.62 ms | 805.30 ms | 48962.00 | 
| python (3.7) | [responder](https://python-responder.org) (1.3) | 111.78 ms | 105.88 ms | 178.34 ms | 221.11 ms | 281.35 ms | 46933.00 | 
| crystal (0.29) | [onyx](https://onyxframework.org) (0.5) | 111.02 ms | 107.94 ms | 157.66 ms | 207.15 ms | 288.33 ms | 35553.33 | 
| python (3.7) | [tornado](https://tornadoweb.org) (5.1) | 127.39 ms | 119.99 ms | 147.44 ms | 497.16 ms | 1292.21 ms | 83885.67 | 
| python (3.7) | [masonite](https://masoniteproject.com) (2.1) | 138.98 ms | 132.48 ms | 188.09 ms | 295.77 ms | 1036.72 ms | 54063.67 | 
| python (3.7) | [cyclone](https://cyclone.io) (0.0) | 383.12 ms | 308.64 ms | 380.41 ms | 3927.30 ms | 6706.70 ms | 625732.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (kore) (c)


:four: (actix-web) (rust)


:five: (jester) (nim)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](https://github.com/ohler55/agoo-c) (0.5) | 283419.67 | 164.02 MB |
| python (3.7) | [japronto](https://github.com/squeaky-pl/japronto) (0.1) | 221078.33 | 264.75 MB |
| c (99) | [kore](https://kore.io) (3.1) | 216075.33 | 561.12 MB |
| rust (1.35) | [actix-web](https://actix.rs) (0.7) | 206398.33 | 234.77 MB |
| nim (0.2) | [jester](https://github.com/dom96/jester) (0.4) | 194339.33 | 390.61 MB |
| python (3.6) | [vibora](https://vibora.io) (0.0) | 178065.33 | 202.23 MB |
| cpp (11) | [evhtp](https://criticalstack/libevhtp) (1.2) | 168274.00 | 163.24 MB |
| go (1.12) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 165250.33 | 266.70 MB |
| crystal (0.29) | [router.cr](https://github.com/tbrand/router.cr) (0.2) | 161308.33 | 151.77 MB |
| ruby (2.6) | [agoo](https://github.com/ohler55/agoo) (2.8) | 155897.00 | 90.16 MB |
| crystal (0.29) | [raze](https://razecr.com) (0.3) | 152859.33 | 143.72 MB |
| rust (1.35) | [gotham](https://gotham.rs) (0.3) | 150340.33 | 307.93 MB |
| crystal (0.29) | [spider-gazelle](https://spider-gazelle.net) (1.4) | 143158.00 | 152.71 MB |
| crystal (0.29) | [kemal](https://kemalcr.com) (0.25) | 135417.67 | 221.09 MB |
| java (8) | [act](https://actframework.org) (1.8) | 134636.00 | 232.22 MB |
| crystal (0.29) | [amber](https://amberframework.org) (0.28) | 131836.00 | 241.38 MB |
| node (12.4) | [0http](https://github.com/jkyberneees/0http) (1.0) | 116756.67 | 174.69 MB |
| crystal (0.29) | [orion](https://github.com/obsidian/orion) (1.7) | 111907.67 | 182.61 MB |
| csharp (7.3) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (2.2) | 108257.33 | 176.53 MB |
| node (12.4) | [restana](https://github.com/jkyberneees/ana) (3.1) | 107855.00 | 161.45 MB |
| go (1.12) | [chi](https://github.com/go-chi/chi) (4.0) | 101579.33 | 136.12 MB |
| rust (1.35) | [iron](https://ironframework.io) (0.6) | 100324.67 | 126.47 MB |
| go (1.12) | [echo](https://echo.labstack.com) (4.1) | 98570.00 | 172.99 MB |
| go (1.12) | [gorouter](https://github.com/vardius/gorouter/wiki) (4.0) | 98252.00 | 131.08 MB |
| go (1.12) | [violetear](https://violetear.org) (7.0) | 96263.67 | 127.71 MB |
| go (1.12) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 94674.33 | 126.36 MB |
| go (1.12) | [kami](https://github.com/guregu/kami) (2.2) | 92873.00 | 123.60 MB |
| go (1.12) | [beego](https://beego.me) (1.12) | 92754.00 | 124.68 MB |
| node (12.4) | [polka](https://github.com/lukeed/polka) (0.5) | 89911.33 | 134.61 MB |
| node (12.4) | [rayo](https://rayo.js.org) (1.3) | 87314.00 | 130.67 MB |
| go (1.12) | [gin](https://gin-gonic.com) (1.4) | 87187.00 | 153.12 MB |
| swift (5.0) | [perfect](https://perfect.org) (3.1) | 84139.00 | 79.19 MB |
| kotlin (1.3) | [ktor](https://ktor.io) (1.2) | 82900.00 | 129.28 MB |
| rust (1.35) | [nickel](https://nickel-org.github.io) (0.11) | 79445.67 | 157.83 MB |
| python (3.7) | [falcon](https://falconframework.org) (2.0) | 76968.67 | 180.34 MB |
| node (12.4) | [muneem](https://github.com/node-muneem/muneem) (2.4) | 75895.33 | 113.69 MB |
| node (12.4) | [fastify](https://fastify.io) (2.4) | 74329.00 | 191.96 MB |
| go (1.12) | [gf](https://goframe.org) (1.6) | 72753.33 | 110.47 MB |
| node (12.4) | [foxify](https://foxify.js.org) (0.1) | 68309.67 | 143.53 MB |
| node (12.4) | [koa](https://koajs.com) (2.7) | 67613.33 | 143.19 MB |
| node (12.4) | [restify](https://restify.com) (8.2) | 65978.33 | 115.83 MB |
| swift (5.0) | [vapor](https://vapor.codes) (3.3) | 60768.67 | 101.43 MB |
| node (12.4) | [express](https://expressjs.com) (4.16) | 57471.33 | 140.68 MB |
| python (3.7) | [bottle](https://bottlepy.org) (0.12) | 55802.00 | 137.53 MB |
| scala (2.12) | [akkahttp](https://akka.io) (10.1) | 53506.33 | 114.79 MB |
| python (3.7) | [starlette](https://starlette.io) (0.12) | 48772.33 | 105.12 MB |
| scala (2.12) | [http4s](https://http4s.org) (0.18) | 45215.00 | 79.18 MB |
| python (3.7) | [hug](https://hug.rest) (2.5) | 44375.00 | 109.98 MB |
| php (7.3) | [symfony](https://symfony.com) (4.3) | 42899.67 | 213.04 MB |
| php (7.3) | [slim](https://slimframework.com) (3.12) | 41169.00 | 204.39 MB |
| php (7.3) | [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 38536.00 | 191.18 MB |
| rust (nightly) | [rocket](https://rocket.rs) (0.4) | 38201.33 | 60.41 MB |
| php (7.3) | [lumen](https://lumen.laravel.com) (5.8) | 37857.33 | 196.34 MB |
| swift (5.0) | [kitura](https://kitura.io) (2.7) | 35707.00 | 66.18 MB |
| php (7.3) | [zend-framework](https://framework.zend.com) (3.1) | 34483.67 | 170.86 MB |
| node (12.4) | [hapi](https://hapijs.com) (18.1) | 33434.33 | 86.24 MB |
| php (7.3) | [laravel](https://laravel.com) (5.8) | 32034.67 | 166.65 MB |
| python (3.7) | [fastapi](https://fastapi.tiangolo.com) (0.29) | 31526.33 | 68.18 MB |
| swift (5.0) | [kitura-nio](https://kitura.io) (2.7) | 30788.67 | 58.04 MB |
| python (3.7) | [aiohttp](https://aiohttp.readthedocs.io) (3.5) | 28367.00 | 64.35 MB |
| ruby (2.6) | [roda](https://roda.jeremyevans.net) (3.2) | 25413.67 | 24.24 MB |
| java (8) | [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 23007.67 | 30.25 MB |
| crystal (0.28) | [lucky](https://luckyframework.org) (0.14) | 22772.00 | 27.97 MB |
| python (3.7) | [molten](https://moltenframework.com) (0.27) | 22721.33 | 42.21 MB |
| ruby (2.6) | [cuba](https://cuba.is) (3.9) | 21891.00 | 25.81 MB |
| node (12.4) | [turbo_polka](https://github.com/mafintosh/turbo-http) (2.0) | 20849.33 | 19.56 MB |
| crystal (0.29) | [athena](https://github.com/blacksmoke16/athena) (0.6) | 20461.00 | 25.62 MB |
| ruby (2.6) | [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 19710.00 | 11.37 MB |
| python (3.7) | [flask](https://flask.pocoo.org) (1.0) | 19508.00 | 48.02 MB |
| python (3.7) | [bocadillo](https://bocadilloproject.github.io) (0.16) | 18046.67 | 34.85 MB |
| ruby (2.6) | [flame](https://github.com/AlexWayfer/flame) (4.18) | 13212.33 | 7.62 MB |
| python (3.7) | [sanic](https://github.com/huge-success/sanic) (19.3) | 12954.33 | 23.10 MB |
| ruby (2.6) | [hanami](https://hanamirb.org) (1.3) | 11931.33 | 90.27 MB |
| python (3.7) | [quart](https://pgjones.gitlab.io/quart) (0.9) | 11698.00 | 23.30 MB |
| ruby (2.6) | [sinatra](https://sinatrarb.com) (2.0) | 10610.33 | 27.52 MB |
| crystal (0.29) | [onyx](https://onyxframework.org) (0.5) | 8928.67 | 23.01 MB |
| python (3.7) | [responder](https://python-responder.org) (1.3) | 8866.33 | 19.34 MB |
| python (3.7) | [django](https://djangoproject.com) (2.2) | 8649.00 | 25.11 MB |
| python (3.7) | [tornado](https://tornadoweb.org) (5.1) | 8076.00 | 23.84 MB |
| python (3.7) | [masonite](https://masoniteproject.com) (2.1) | 7042.00 | 17.35 MB |
| ruby (2.6) | [rails](https://rubyonrails.org) (5.2) | 2676.67 | 8.22 MB |
| python (3.7) | [cyclone](https://cyclone.io) (0.0) | 1712.67 | 4.65 MB |
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

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
Last update: 2018-12-10
```
OS: Linux (version: 4.19.6-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: laravel (php)


:four: slim (php)


:five: symfony (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 0.07 ms | 0.07 ms | 0.10 ms | 0.14 ms | 5.59 ms | 63.33 | 
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 0.30 ms | 0.27 ms | 0.53 ms | 0.88 ms | 8.75 ms | 202.00 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 108.82 ms | 0.32 ms | 253.41 ms | 2280.32 ms | 6807.59 ms | 397527.33 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 129.00 ms | 0.32 ms | 232.49 ms | 2912.68 ms | 6797.39 ms | 501410.00 | 
| php (7.2) | [symfony](http://symfony.com) (4.2) | 113.87 ms | 0.34 ms | 236.16 ms | 2507.51 ms | 6813.58 ms | 440733.67 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 224.25 ms | 0.36 ms | 327.88 ms | 4765.18 ms | 7064.56 ms | 810305.33 | 
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 99.34 ms | 0.41 ms | 221.31 ms | 1897.24 ms | 6804.76 ms | 408542.33 | 
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 147.70 ms | 0.53 ms | 269.11 ms | 3069.19 ms | 6899.88 ms | 550120.00 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.79 ms | 1.03 ms | 10.74 ms | 32.65 ms | 114.52 ms | 6687.67 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 6.81 ms | 1.03 ms | 20.69 ms | 61.76 ms | 184.23 ms | 12741.33 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 3.08 ms | 1.10 ms | 8.44 ms | 24.44 ms | 89.47 ms | 5070.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 62.48 ms | 1.35 ms | 2.86 ms | 2041.04 ms | 6593.09 ms | 413572.33 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.88 ms | 2.14 ms | 5.86 ms | 13.38 ms | 32.67 ms | 2783.00 | 
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 2.83 ms | 2.22 ms | 5.76 ms | 12.37 ms | 119.59 ms | 2792.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 2.47 ms | 2.34 ms | 4.49 ms | 6.90 ms | 20.66 ms | 1660.33 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.33 ms | 2.68 ms | 5.65 ms | 10.45 ms | 50.61 ms | 2223.67 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 7.65 ms | 2.78 ms | 21.62 ms | 54.22 ms | 147.68 ms | 11570.00 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.91 ms | 2.83 ms | 15.62 ms | 38.55 ms | 115.11 ms | 8185.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.80 ms | 3.01 ms | 8.10 ms | 15.63 ms | 34.17 ms | 3385.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.97 ms | 3.06 ms | 5.02 ms | 6.90 ms | 34.02 ms | 1619.67 | 
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 3.28 ms | 3.21 ms | 4.68 ms | 8.41 ms | 162.99 ms | 1714.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.88 ms | 3.24 ms | 6.82 ms | 12.33 ms | 53.39 ms | 2421.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 4.06 ms | 3.55 ms | 7.65 ms | 13.96 ms | 28.03 ms | 2853.00 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 4.99 ms | 3.93 ms | 8.60 ms | 31.23 ms | 113.16 ms | 6976.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 4.52 ms | 4.49 ms | 7.55 ms | 9.06 ms | 34.48 ms | 2280.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 5.41 ms | 4.62 ms | 9.22 ms | 18.27 ms | 161.93 ms | 4339.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 5.18 ms | 4.64 ms | 7.61 ms | 14.36 ms | 272.14 ms | 5399.00 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 5.60 ms | 4.70 ms | 8.46 ms | 23.06 ms | 97.15 ms | 3855.33 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 7.65 ms | 4.77 ms | 16.43 ms | 31.59 ms | 211.47 ms | 7176.33 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 5.76 ms | 4.78 ms | 9.57 ms | 19.51 ms | 139.88 ms | 4406.00 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 5.96 ms | 4.81 ms | 9.97 ms | 20.00 ms | 221.73 ms | 5849.00 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 6.43 ms | 5.02 ms | 10.68 ms | 22.55 ms | 232.41 ms | 7896.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.97 ms | 5.06 ms | 9.77 ms | 21.45 ms | 107.74 ms | 4765.67 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 6.45 ms | 5.23 ms | 10.99 ms | 21.99 ms | 162.80 ms | 4817.00 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 6.90 ms | 5.31 ms | 11.05 ms | 23.00 ms | 303.17 ms | 10300.33 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 7.44 ms | 5.84 ms | 11.97 ms | 26.49 ms | 301.07 ms | 9188.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 9.00 ms | 6.35 ms | 17.67 ms | 33.46 ms | 211.05 ms | 7614.33 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 9.43 ms | 6.66 ms | 16.31 ms | 41.17 ms | 422.80 ms | 15335.00 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 10.05 ms | 6.94 ms | 17.96 ms | 46.75 ms | 398.37 ms | 15862.33 | 
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 10.43 ms | 6.97 ms | 20.21 ms | 48.48 ms | 381.02 ms | 14667.67 | 
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 9.60 ms | 7.58 ms | 14.65 ms | 37.06 ms | 448.47 ms | 16202.00 | 
| go (1.11) | [gf](http://gfer.me) (1.3) | 8.72 ms | 7.85 ms | 13.18 ms | 28.78 ms | 165.46 ms | 5431.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 177.09 ms | 8.43 ms | 157.47 ms | 3699.11 ms | 6180.35 ms | 662363.00 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 35.59 ms | 9.00 ms | 110.56 ms | 262.19 ms | 617.35 ms | 57573.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 11.16 ms | 9.54 ms | 20.67 ms | 40.81 ms | 248.39 ms | 9405.00 | 
| node (11.1) | [fastify](http://fastify.io) (1.13) | 13.69 ms | 10.24 ms | 22.96 ms | 53.64 ms | 487.10 ms | 18552.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 12.99 ms | 10.67 ms | 18.63 ms | 31.51 ms | 619.95 ms | 21456.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 10.96 ms | 11.23 ms | 12.81 ms | 14.59 ms | 191.55 ms | 2997.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 20.75 ms | 13.91 ms | 25.67 ms | 246.78 ms | 422.01 ms | 37433.67 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 19.27 ms | 14.08 ms | 28.98 ms | 130.62 ms | 692.31 ms | 32816.00 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 19.02 ms | 14.94 ms | 31.92 ms | 63.71 ms | 606.04 ms | 22258.67 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 18.74 ms | 15.36 ms | 30.98 ms | 56.90 ms | 451.22 ms | 16998.67 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 21.87 ms | 19.83 ms | 32.00 ms | 44.70 ms | 217.70 ms | 7690.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 25.68 ms | 20.80 ms | 41.00 ms | 69.12 ms | 321.65 ms | 15289.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 24.56 ms | 21.83 ms | 34.80 ms | 49.01 ms | 207.29 ms | 8767.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 25.47 ms | 23.33 ms | 33.96 ms | 39.52 ms | 171.57 ms | 7164.00 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 27.75 ms | 24.66 ms | 39.51 ms | 44.94 ms | 253.15 ms | 9884.00 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 39.94 ms | 27.52 ms | 49.08 ms | 456.43 ms | 1421.23 ms | 82583.33 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 26.74 ms | 28.43 ms | 34.98 ms | 40.47 ms | 110.97 ms | 6673.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 30.94 ms | 29.34 ms | 40.94 ms | 49.71 ms | 462.14 ms | 13424.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 87.91 ms | 33.91 ms | 213.77 ms | 827.11 ms | 1018.58 ms | 169612.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 38.22 ms | 36.03 ms | 45.85 ms | 68.46 ms | 187.45 ms | 9643.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 51.85 ms | 37.98 ms | 85.21 ms | 322.52 ms | 745.31 ms | 56688.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 47.45 ms | 40.68 ms | 85.08 ms | 142.22 ms | 200.34 ms | 29029.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 68.44 ms | 67.79 ms | 85.96 ms | 103.41 ms | 358.32 ms | 15119.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (evhtp) (cpp)


:five: (vibora) (python)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 389075.00 | 224.75 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 356597.00 | 426.74 MB |
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 345372.00 | 392.38 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 292565.33 | 284.07 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 288116.33 | 327.02 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 278449.33 | 448.67 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 270702.67 | 544.17 MB |
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 265102.67 | 543.48 MB |
| java (8) | [act](http://actframework.org) (1.8) | 244764.00 | 478.15 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 244412.00 | 260.86 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 229315.00 | 132.58 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 184621.33 | 300.49 MB |
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 182713.33 | 230.96 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 180939.67 | 317.72 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 179770.00 | 240.00 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 171562.00 | 229.64 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 166886.00 | 224.18 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 157697.00 | 276.57 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 154594.00 | 271.21 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 150300.67 | 199.48 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 141629.33 | 364.19 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 139417.67 | 188.57 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 124653.00 | 186.71 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 118670.33 | 177.65 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 118595.67 | 292.35 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 115856.33 | 243.64 MB |
| go (1.11) | [gf](http://gfer.me) (1.3) | 115616.00 | 176.06 MB |
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 115027.33 | 228.80 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 114161.33 | 171.15 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 100490.67 | 175.85 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 90432.00 | 84.90 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 88017.67 | 207.89 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 81856.33 | 137.82 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 80701.33 | 172.92 MB |
| c (99) | [kore](http://kore.io) (3.1) | 67276.00 | 182.37 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 67197.33 | 333.31 MB |
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 65058.00 | 322.24 MB |
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 64369.67 | 319.14 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 63795.67 | 330.77 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 63618.67 | 101.24 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 63535.33 | 144.09 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 62128.67 | 131.08 MB |
| php (7.2) | [symfony](http://symfony.com) (4.2) | 61497.67 | 304.76 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 56959.67 | 139.07 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 56950.67 | 99.78 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 53841.33 | 280.66 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 45700.33 | 84.71 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 42021.33 | 40.08 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 40548.67 | 66.11 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 39561.33 | 97.45 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 39219.00 | 36.75 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 37483.33 | 35.14 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 35481.00 | 43.61 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 33975.33 | 19.59 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 32794.00 | 84.70 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 31832.00 | 58.09 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 27029.00 | 44.03 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 25635.00 | 45.65 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 21847.67 | 63.24 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 21808.00 | 43.55 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 21676.00 | 12.49 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 18995.67 | 143.58 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 16716.33 | 43.33 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 14355.33 | 38.55 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 3602.00 | 10.97 MB |
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

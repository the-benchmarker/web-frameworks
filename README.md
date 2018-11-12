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
Last update: 2018-11-12
```
OS: Linux (version: 4.18.17-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: slim (php)


:three: lumen (php)


:four: iron (rust)


:five: laravel (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 0.08 ms | 0.08 ms | 0.12 ms | 0.16 ms | 1.18 ms | 28.00 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 198.42 ms | 0.42 ms | 313.90 ms | 4370.43 ms | 7094.07 ms | 731292.33 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 191.18 ms | 0.43 ms | 275.28 ms | 4291.83 ms | 7334.76 ms | 726149.67 | 
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 0.46 ms | 0.43 ms | 0.81 ms | 1.32 ms | 22.66 ms | 329.67 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 155.59 ms | 0.44 ms | 360.32 ms | 3114.54 ms | 7158.81 ms | 543799.67 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 5.22 ms | 0.99 ms | 15.42 ms | 45.96 ms | 141.77 ms | 9478.33 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 4.24 ms | 1.26 ms | 12.09 ms | 33.83 ms | 111.49 ms | 7058.67 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 8.68 ms | 1.50 ms | 26.29 ms | 71.32 ms | 198.72 ms | 15080.67 | 
| php (7.2) | [symfony](http://symfony.com) (4.1) | 138.63 ms | 1.66 ms | 270.24 ms | 3028.17 ms | 5818.25 ms | 509688.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 83.66 ms | 1.94 ms | 4.33 ms | 2704.96 ms | 6572.00 ms | 475446.67 | 
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 3.55 ms | 2.66 ms | 7.40 ms | 15.55 ms | 36.62 ms | 3362.67 | 
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 3.68 ms | 2.87 ms | 7.13 ms | 15.24 ms | 84.55 ms | 3183.33 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 9.65 ms | 3.15 ms | 27.88 ms | 69.06 ms | 181.28 ms | 14793.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.59 ms | 3.55 ms | 9.24 ms | 17.43 ms | 39.80 ms | 3747.00 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 8.44 ms | 3.71 ms | 23.15 ms | 53.49 ms | 139.09 ms | 11629.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.63 ms | 3.80 ms | 5.62 ms | 9.02 ms | 21.06 ms | 1932.33 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.45 ms | 4.18 ms | 6.85 ms | 14.21 ms | 56.44 ms | 2705.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 4.92 ms | 4.44 ms | 8.95 ms | 17.09 ms | 37.73 ms | 3332.33 | 
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 5.41 ms | 4.86 ms | 9.94 ms | 17.31 ms | 224.15 ms | 6226.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.20 ms | 5.10 ms | 8.42 ms | 14.82 ms | 49.85 ms | 2902.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 47.65 ms | 5.81 ms | 10.07 ms | 1620.41 ms | 2337.42 ms | 244555.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 7.05 ms | 6.16 ms | 9.98 ms | 18.40 ms | 344.28 ms | 8237.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 7.75 ms | 6.66 ms | 11.94 ms | 24.88 ms | 154.90 ms | 4875.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 7.43 ms | 6.93 ms | 12.14 ms | 23.49 ms | 172.02 ms | 5703.00 | 
| go (1.11) | [iris](http://iris-go.com) (11.0) | 8.03 ms | 7.04 ms | 12.33 ms | 24.78 ms | 163.76 ms | 4751.00 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 8.60 ms | 7.55 ms | 13.14 ms | 26.42 ms | 301.47 ms | 7728.67 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 9.05 ms | 7.94 ms | 14.01 ms | 28.82 ms | 253.81 ms | 7720.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 157.32 ms | 8.22 ms | 31.45 ms | 3942.25 ms | 6830.86 ms | 670979.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 9.28 ms | 8.39 ms | 14.44 ms | 28.56 ms | 178.53 ms | 5679.00 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 8.94 ms | 8.42 ms | 13.15 ms | 30.87 ms | 116.06 ms | 5079.67 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 9.77 ms | 8.70 ms | 15.09 ms | 31.39 ms | 304.10 ms | 8686.00 | 
| go (1.11) | [beego](http://beego.me) (1.10) | 9.80 ms | 8.82 ms | 14.71 ms | 30.61 ms | 257.45 ms | 8772.67 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 36.37 ms | 9.21 ms | 115.74 ms | 290.66 ms | 747.93 ms | 63146.33 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 12.05 ms | 9.87 ms | 20.68 ms | 38.10 ms | 278.31 ms | 9169.00 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 15.19 ms | 10.97 ms | 24.26 ms | 63.49 ms | 576.49 ms | 22631.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 13.21 ms | 11.21 ms | 21.57 ms | 38.97 ms | 282.92 ms | 9247.67 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 15.26 ms | 11.42 ms | 23.95 ms | 60.74 ms | 517.17 ms | 20947.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 11.83 ms | 11.94 ms | 14.18 ms | 16.51 ms | 108.22 ms | 2782.33 | 
| python (3.7) | [starlette](http://starlette.io) (0.8) | 14.09 ms | 12.91 ms | 24.87 ms | 37.92 ms | 116.32 ms | 8032.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 20.64 ms | 13.47 ms | 23.85 ms | 239.86 ms | 1088.44 ms | 56446.33 | 
| node (10.12) | [foxify](http://foxify.js.org) (0.10) | 18.35 ms | 13.64 ms | 26.53 ms | 87.96 ms | 681.89 ms | 28996.33 | 
| node (10.12) | [fastify](http://fastify.io) (1.13) | 20.57 ms | 15.52 ms | 30.38 ms | 99.09 ms | 739.13 ms | 30951.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 18.79 ms | 17.00 ms | 33.28 ms | 56.58 ms | 509.37 ms | 16792.00 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 27.36 ms | 18.90 ms | 35.12 ms | 274.87 ms | 1091.94 ms | 57358.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 24.08 ms | 21.34 ms | 43.69 ms | 69.45 ms | 121.99 ms | 14631.33 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 29.01 ms | 21.84 ms | 42.42 ms | 157.46 ms | 927.04 ms | 42845.33 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 29.13 ms | 22.95 ms | 45.57 ms | 106.36 ms | 763.36 ms | 34416.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 27.40 ms | 24.92 ms | 36.32 ms | 43.52 ms | 316.61 ms | 9389.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 27.57 ms | 25.36 ms | 40.60 ms | 56.50 ms | 278.68 ms | 11467.00 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 32.67 ms | 29.75 ms | 44.03 ms | 66.80 ms | 247.31 ms | 10839.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 42.03 ms | 32.44 ms | 79.28 ms | 126.39 ms | 732.50 ms | 32468.00 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 32.43 ms | 32.74 ms | 41.40 ms | 49.36 ms | 247.32 ms | 8343.67 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 37.49 ms | 33.32 ms | 52.17 ms | 68.11 ms | 554.73 ms | 22647.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 33.43 ms | 34.37 ms | 42.50 ms | 49.55 ms | 392.83 ms | 10840.33 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 60.16 ms | 37.80 ms | 65.39 ms | 797.98 ms | 1707.33 ms | 126732.00 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 37.90 ms | 38.37 ms | 46.35 ms | 52.19 ms | 335.20 ms | 9307.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 57.61 ms | 50.63 ms | 102.26 ms | 167.18 ms | 312.73 ms | 33738.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 68.85 ms | 52.88 ms | 133.45 ms | 184.51 ms | 452.27 ms | 40080.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 77.78 ms | 67.66 ms | 143.06 ms | 212.83 ms | 295.75 ms | 45344.33 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 66.87 ms | 73.50 ms | 101.49 ms | 110.15 ms | 125.73 ms | 35903.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 91.82 ms | 90.77 ms | 112.40 ms | 145.45 ms | 734.65 ms | 29149.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (actix-web) (rust)


:three: (evhtp) (cpp)


:four: (vibora) (python)


:five: (fasthttprouter) (go)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 289860.00 | 346.87 MB |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 262414.67 | 298.41 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 245365.33 | 238.31 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 236252.00 | 268.28 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 212447.67 | 343.14 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 201532.67 | 405.22 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 201117.33 | 215.27 MB |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 179360.33 | 367.37 MB |
| java (8) | [act](http://actframework.org) (1.8) | 165194.33 | 322.72 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 137650.00 | 224.28 MB |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 129100.33 | 162.81 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 127671.67 | 171.01 MB |
| go (1.11) | [iris](http://iris-go.com) (11.0) | 121697.67 | 162.78 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 115625.00 | 154.71 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 112694.67 | 197.89 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 111359.67 | 195.58 MB |
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 108172.00 | 215.15 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 106682.33 | 187.21 MB |
| go (1.11) | [beego](http://beego.me) (1.10) | 102936.67 | 138.78 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 102715.67 | 137.32 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 85118.67 | 79.98 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 84890.00 | 218.28 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 76894.33 | 189.59 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 75443.67 | 112.94 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 73956.00 | 110.77 MB |
| python (3.7) | [starlette](http://starlette.io) (0.8) | 71338.00 | 143.38 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 65138.33 | 108.91 MB |
| node (10.12) | [foxify](http://foxify.js.org) (0.10) | 61324.00 | 128.93 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 60504.33 | 129.75 MB |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 60275.33 | 299.22 MB |
| node (10.12) | [fastify](http://fastify.io) (1.13) | 58654.00 | 139.68 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 58310.00 | 102.01 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 57240.33 | 283.61 MB |
| c (99) | [kore](http://kore.io) (3.1) | 55798.67 | 151.18 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 52216.67 | 270.74 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 47581.00 | 100.55 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 46203.00 | 73.61 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 43221.00 | 98.06 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 42073.00 | 219.11 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 39743.00 | 97.18 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 38196.67 | 66.92 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 36571.00 | 67.89 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 36250.00 | 33.98 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 30836.33 | 28.89 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 30565.00 | 37.60 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 30279.33 | 28.85 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 29847.67 | 48.64 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 27397.33 | 15.76 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 26790.33 | 43.63 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 26129.33 | 47.70 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 25705.33 | 63.21 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 24810.67 | 14.30 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 24109.33 | 62.25 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 17909.00 | 31.91 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 15331.00 | 44.48 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 15210.33 | 8.77 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 14748.00 | 111.54 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 13288.67 | 26.49 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 13247.67 | 34.35 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 10638.00 | 28.47 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 3521.00 | 10.80 MB |
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

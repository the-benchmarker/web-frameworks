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
```
OS: Linux (version: 4.19.5-300.fc29.x86_64, arch: x86_64)
CPU Cores: 4
threads: 5, connections: 1000
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: rack-routing (ruby)


:four: roda (ruby)


:five: sinatra (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 0.05 ms | 0.05 ms | 0.06 ms | 0.12 ms | 8.28 ms | 105.67 |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 0.42 ms | 0.31 ms | 0.69 ms | 2.80 ms | 24.50 ms | 610.67 |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 2.71 ms | 0.56 ms | 6.48 ms | 38.60 ms | 116.50 ms | 7017.33 |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 2.42 ms | 0.79 ms | 4.50 ms | 38.28 ms | 163.39 ms | 7140.33 |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 6.89 ms | 0.83 ms | 20.57 ms | 71.88 ms | 190.89 ms | 14307.33 |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 5.05 ms | 1.81 ms | 11.55 ms | 58.67 ms | 157.56 ms | 10725.00 |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 4.82 ms | 1.93 ms | 11.56 ms | 47.80 ms | 143.77 ms | 8920.67 |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 197.37 ms | 3.12 ms | 592.22 ms | 3123.81 ms | 6580.40 ms | 577958.00 |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 150.91 ms | 3.49 ms | 10.02 ms | 4243.03 ms | 6597.08 ms | 709209.67 |
| php (7.2) | [symfony](http://symfony.com) (4.2) | 139.47 ms | 3.79 ms | 418.54 ms | 2115.62 ms | 6135.77 ms | 427257.00 |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 203.72 ms | 3.99 ms | 535.33 ms | 3365.08 ms | 6309.10 ms | 619148.00 |
| c (99) | [kore](http://kore.io) (3.1) | 4.80 ms | 4.70 ms | 8.16 ms | 10.72 ms | 33.45 ms | 2614.00 |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 132.72 ms | 4.70 ms | 399.03 ms | 2008.61 ms | 5159.56 ms | 396727.00 |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 7.33 ms | 5.66 ms | 14.22 ms | 27.28 ms | 77.35 ms | 5740.00 |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 6.51 ms | 5.97 ms | 11.76 ms | 19.67 ms | 99.81 ms | 4413.00 |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 22.67 ms | 6.47 ms | 70.95 ms | 121.65 ms | 222.05 ms | 30638.00 |
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 6.82 ms | 7.04 ms | 11.81 ms | 20.32 ms | 57.25 ms | 4466.67 |
| java (8) | [act](http://actframework.org) (1.8) | 9.10 ms | 7.55 ms | 14.41 ms | 31.92 ms | 322.82 ms | 10746.00 |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 9.13 ms | 7.94 ms | 16.05 ms | 27.38 ms | 56.53 ms | 5393.00 |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 9.04 ms | 8.22 ms | 14.98 ms | 26.58 ms | 116.47 ms | 5282.67 |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 9.11 ms | 8.39 ms | 15.56 ms | 26.05 ms | 69.29 ms | 5305.67 |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 192.01 ms | 9.06 ms | 25.28 ms | 4496.82 ms | 7906.73 ms | 789577.33 |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 10.44 ms | 9.89 ms | 16.57 ms | 26.66 ms | 68.99 ms | 5110.67 |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 11.03 ms | 10.01 ms | 19.97 ms | 32.90 ms | 74.77 ms | 7065.00 |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 13.45 ms | 10.54 ms | 23.77 ms | 83.10 ms | 132.03 ms | 13099.67 |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 11.91 ms | 10.76 ms | 17.73 ms | 30.69 ms | 175.53 ms | 5921.00 |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 14.48 ms | 11.42 ms | 23.49 ms | 97.04 ms | 447.00 ms | 20475.33 |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 17.83 ms | 12.50 ms | 28.12 ms | 171.80 ms | 528.74 ms | 29130.33 |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 13.84 ms | 12.51 ms | 18.12 ms | 39.38 ms | 513.14 ms | 14849.33 |
| go (1.11) | [beego](http://beego.me) (1.11) | 17.03 ms | 12.81 ms | 28.26 ms | 139.91 ms | 627.08 ms | 29045.00 |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 14.49 ms | 12.96 ms | 22.05 ms | 65.12 ms | 271.90 ms | 12275.33 |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 15.69 ms | 13.35 ms | 25.07 ms | 88.14 ms | 344.32 ms | 16404.00 |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 17.31 ms | 13.57 ms | 28.67 ms | 133.87 ms | 433.18 ms | 24756.00 |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 15.68 ms | 14.61 ms | 24.62 ms | 48.79 ms | 280.55 ms | 11254.33 |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 17.59 ms | 15.58 ms | 24.46 ms | 44.50 ms | 345.36 ms | 8320.33 |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 17.81 ms | 16.83 ms | 21.61 ms | 33.42 ms | 112.16 ms | 5383.67 |
| go (1.11) | [gf](http://gfer.me) (1.2) | 18.27 ms | 17.20 ms | 26.64 ms | 53.91 ms | 304.27 ms | 11888.67 |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 18.43 ms | 18.05 ms | 23.44 ms | 27.06 ms | 97.45 ms | 4123.67 |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 20.67 ms | 19.76 ms | 25.52 ms | 37.85 ms | 304.20 ms | 8645.67 |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 27.92 ms | 20.61 ms | 52.74 ms | 105.82 ms | 272.97 ms | 20683.00 |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 23.60 ms | 20.78 ms | 32.14 ms | 42.41 ms | 109.55 ms | 6536.00 |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 52.12 ms | 21.04 ms | 56.36 ms | 962.22 ms | 2088.50 ms | 158060.00 |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 30.66 ms | 22.36 ms | 56.77 ms | 111.13 ms | 269.28 ms | 21201.33 |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 23.92 ms | 23.20 ms | 30.06 ms | 35.22 ms | 231.96 ms | 6061.67 |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 79.35 ms | 23.59 ms | 63.75 ms | 1578.42 ms | 2835.62 ms | 258544.67 |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 80.14 ms | 23.88 ms | 64.26 ms | 1431.99 ms | 2784.82 ms | 247795.33 |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 34.88 ms | 23.92 ms | 44.45 ms | 351.72 ms | 2104.61 ms | 95388.00 |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 77.69 ms | 24.19 ms | 65.38 ms | 1502.63 ms | 2810.63 ms | 248519.33 |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 25.15 ms | 24.81 ms | 29.91 ms | 37.54 ms | 175.38 ms | 4722.33 |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 23.94 ms | 25.11 ms | 28.08 ms | 30.55 ms | 100.17 ms | 3968.00 |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 143.75 ms | 28.52 ms | 74.25 ms | 2916.99 ms | 5245.06 ms | 506999.33 |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 107.00 ms | 30.75 ms | 82.29 ms | 1976.42 ms | 3560.67 ms | 336343.00 |
| node (11.1) | [koa](http://koajs.com) (2.6) | 123.28 ms | 31.39 ms | 84.91 ms | 2279.65 ms | 3656.33 ms | 387452.33 |
| node (11.1) | [express](http://expressjs.com) (4.16) | 131.35 ms | 32.16 ms | 91.90 ms | 2391.82 ms | 4083.32 ms | 412858.00 |
| node (11.1) | [restify](http://restify.com) (7.2) | 52.19 ms | 35.02 ms | 70.91 ms | 547.37 ms | 1631.61 ms | 96812.00 |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 45.35 ms | 43.10 ms | 61.64 ms | 82.34 ms | 339.15 ms | 15111.67 |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 52.08 ms | 46.51 ms | 74.70 ms | 97.41 ms | 324.49 ms | 20173.67 |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 84.22 ms | 67.14 ms | 145.31 ms | 201.02 ms | 654.57 ms | 46071.67 |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 311.25 ms | 68.37 ms | 394.89 ms | 4710.48 ms | 7204.54 ms | 863718.67 |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 87.78 ms | 86.68 ms | 120.34 ms | 175.75 ms | 409.20 ms | 28646.00 |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 143.46 ms | 123.59 ms | 167.68 ms | 895.00 ms | 2103.84 ms | 147791.67 |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 171.97 ms | 157.87 ms | 283.82 ms | 434.24 ms | 590.56 ms | 86577.00 |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 302.08 ms | 171.86 ms | 294.40 ms | 3560.34 ms | 5615.90 ms | 590937.00 |

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (act) (java)


:five: (spider-gazelle) (crystal)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 135911.67 | 78.62 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 134364.33 | 160.86 MB |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 111041.33 | 126.20 MB |
| java (8) | [act](http://actframework.org) (1.8) | 109503.67 | 213.59 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 105476.33 | 112.64 MB |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 95894.33 | 195.69 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 95348.67 | 108.21 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 95320.00 | 152.80 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 91240.33 | 183.23 MB |
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 84132.67 | 167.19 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 83801.67 | 81.32 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 83490.00 | 48.27 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 82309.00 | 109.86 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 75606.67 | 132.69 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 74701.67 | 100.06 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 72357.67 | 117.67 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 72003.33 | 94.38 MB |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 71904.67 | 91.05 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 71337.00 | 95.47 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 69492.33 | 121.91 MB |
| c (99) | [kore](http://kore.io) (3.1) | 64365.00 | 174.71 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 64142.67 | 86.39 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 58120.00 | 101.93 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 56053.33 | 120.31 MB |
| go (1.11) | [gf](http://gfer.me) (1.2) | 55388.00 | 84.09 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 55347.67 | 51.92 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 54145.00 | 50.79 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 48455.33 | 79.05 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 42075.33 | 68.63 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 41814.00 | 76.37 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 41333.00 | 50.88 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 39281.00 | 100.92 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 39241.00 | 195.04 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 39222.00 | 36.91 MB |
| php (7.2) | [symfony](http://symfony.com) (4.2) | 39142.67 | 194.75 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 38720.00 | 57.95 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 38483.33 | 67.44 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 35374.33 | 87.12 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 35153.67 | 85.00 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 34908.00 | 52.23 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 33743.67 | 50.56 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 31950.00 | 166.32 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 29913.00 | 155.94 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 28511.67 | 47.41 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 27562.00 | 26.28 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 27115.33 | 56.98 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 26929.00 | 56.93 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 25906.00 | 63.30 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 24250.00 | 42.48 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 23892.67 | 13.78 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 22334.33 | 41.55 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 19134.33 | 43.31 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 15864.00 | 24.43 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 13309.00 | 100.76 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 13241.67 | 7.65 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 11982.33 | 29.51 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 11676.67 | 30.19 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 11212.67 | 19.99 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 9567.33 | 24.83 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 7505.00 | 21.74 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 5778.00 | 11.53 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 4897.67 | 13.19 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 2819.33 | 8.63 MB |
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

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
Last update: 2018-11-28
```
OS: Linux (version: 4.19.3-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: laravel (php)


:four: symfony (php)


:five: slim (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 0.07 ms | 0.08 ms | 0.10 ms | 0.14 ms | 5.79 ms | 43.67 | 
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 0.26 ms | 0.23 ms | 0.43 ms | 0.80 ms | 15.50 ms | 201.00 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 82.87 ms | 0.27 ms | 230.45 ms | 1226.61 ms | 6923.15 ms | 305371.00 | 
| php (7.2) | [symfony](http://symfony.com) (4.1) | 114.30 ms | 0.27 ms | 201.23 ms | 2751.20 ms | 6842.38 ms | 465603.33 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 100.37 ms | 0.28 ms | 179.38 ms | 2257.64 ms | 6752.35 ms | 416758.33 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 111.44 ms | 0.28 ms | 201.38 ms | 2320.01 ms | 6745.07 ms | 436263.33 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.33 ms | 0.80 ms | 9.38 ms | 29.61 ms | 104.04 ms | 6044.00 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 5.66 ms | 0.86 ms | 16.95 ms | 52.85 ms | 159.98 ms | 10847.00 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 2.86 ms | 0.91 ms | 8.03 ms | 23.05 ms | 80.41 ms | 4801.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 80.93 ms | 1.28 ms | 38.28 ms | 2215.13 ms | 6592.80 ms | 450607.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.83 ms | 1.80 ms | 6.23 ms | 15.36 ms | 32.16 ms | 3136.00 | 
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 2.75 ms | 2.04 ms | 5.65 ms | 13.22 ms | 93.92 ms | 3526.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 2.35 ms | 2.21 ms | 3.37 ms | 8.26 ms | 28.81 ms | 1472.33 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 2.92 ms | 2.22 ms | 5.49 ms | 9.44 ms | 97.52 ms | 2340.33 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 7.31 ms | 2.39 ms | 20.98 ms | 54.51 ms | 155.86 ms | 11544.33 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.48 ms | 2.50 ms | 14.72 ms | 36.38 ms | 106.34 ms | 7729.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.67 ms | 2.68 ms | 4.54 ms | 6.24 ms | 83.14 ms | 2245.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.23 ms | 2.72 ms | 6.71 ms | 13.27 ms | 33.14 ms | 2861.33 | 
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 3.02 ms | 2.81 ms | 4.12 ms | 7.02 ms | 211.95 ms | 4518.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.59 ms | 3.06 ms | 6.15 ms | 10.37 ms | 58.38 ms | 2079.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 3.77 ms | 3.23 ms | 7.16 ms | 13.26 ms | 27.35 ms | 2708.33 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 4.85 ms | 3.77 ms | 8.22 ms | 36.55 ms | 109.06 ms | 7649.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 4.55 ms | 4.15 ms | 6.29 ms | 14.19 ms | 281.58 ms | 6442.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 4.64 ms | 4.32 ms | 7.37 ms | 8.82 ms | 1180.25 ms | 16815.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.10 ms | 4.35 ms | 8.45 ms | 20.01 ms | 108.14 ms | 4190.67 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 4.80 ms | 4.37 ms | 8.04 ms | 15.66 ms | 212.47 ms | 4422.33 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 4.92 ms | 4.47 ms | 8.28 ms | 16.11 ms | 105.42 ms | 3166.67 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 5.97 ms | 4.52 ms | 8.89 ms | 20.86 ms | 489.93 ms | 14904.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 6.38 ms | 4.60 ms | 9.29 ms | 23.67 ms | 415.20 ms | 15484.33 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 5.58 ms | 4.61 ms | 8.08 ms | 27.27 ms | 59.42 ms | 4551.00 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 5.47 ms | 4.61 ms | 9.33 ms | 18.58 ms | 217.47 ms | 4955.67 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 5.45 ms | 4.66 ms | 9.24 ms | 17.88 ms | 50.97 ms | 3299.67 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 5.83 ms | 4.83 ms | 9.93 ms | 19.65 ms | 105.39 ms | 3723.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 6.68 ms | 4.91 ms | 10.59 ms | 19.71 ms | 213.70 ms | 6292.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 8.37 ms | 5.21 ms | 18.31 ms | 33.57 ms | 118.44 ms | 6855.33 | 
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 7.87 ms | 5.65 ms | 13.94 ms | 32.79 ms | 293.80 ms | 9484.67 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 8.05 ms | 5.89 ms | 14.92 ms | 33.67 ms | 229.85 ms | 7846.00 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 28.21 ms | 6.26 ms | 90.22 ms | 230.95 ms | 564.57 ms | 49658.33 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 9.32 ms | 6.33 ms | 16.03 ms | 40.32 ms | 442.76 ms | 16721.33 | 
| go (1.11) | [gf](http://gfer.me) (1.2) | 7.19 ms | 6.62 ms | 10.40 ms | 20.85 ms | 120.54 ms | 4541.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 135.97 ms | 7.20 ms | 28.52 ms | 3073.13 ms | 4947.72 ms | 529969.00 | 
| node (11.1) | [fastify](http://fastify.io) (1.13) | 13.22 ms | 9.12 ms | 22.22 ms | 78.42 ms | 489.46 ms | 20902.67 | 
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 11.93 ms | 9.29 ms | 20.04 ms | 45.01 ms | 372.70 ms | 13135.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 34.58 ms | 10.12 ms | 22.00 ms | 858.87 ms | 2391.72 ms | 162851.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 15.66 ms | 10.73 ms | 18.83 ms | 110.85 ms | 1022.71 ms | 44646.67 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 14.24 ms | 11.02 ms | 23.44 ms | 49.08 ms | 521.66 ms | 18869.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 11.43 ms | 11.58 ms | 12.77 ms | 14.27 ms | 91.66 ms | 2363.67 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 16.18 ms | 13.00 ms | 26.46 ms | 53.43 ms | 485.97 ms | 17172.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 15.19 ms | 13.70 ms | 25.48 ms | 35.95 ms | 66.94 ms | 7216.00 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 18.95 ms | 15.95 ms | 32.38 ms | 54.77 ms | 381.91 ms | 14617.67 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 20.04 ms | 18.35 ms | 28.35 ms | 42.58 ms | 234.25 ms | 7988.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 22.38 ms | 19.66 ms | 32.14 ms | 44.48 ms | 387.49 ms | 11231.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 25.25 ms | 21.23 ms | 43.43 ms | 66.20 ms | 402.59 ms | 16061.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 23.10 ms | 22.61 ms | 31.56 ms | 41.82 ms | 235.93 ms | 8630.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 26.12 ms | 23.09 ms | 38.32 ms | 48.06 ms | 668.53 ms | 20015.67 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 33.02 ms | 23.14 ms | 41.11 ms | 339.95 ms | 1162.33 ms | 64998.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 25.04 ms | 25.45 ms | 33.28 ms | 41.93 ms | 319.91 ms | 9845.00 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 30.21 ms | 27.88 ms | 40.12 ms | 82.15 ms | 311.45 ms | 13835.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 32.06 ms | 29.49 ms | 42.49 ms | 51.72 ms | 312.38 ms | 11179.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 35.32 ms | 29.90 ms | 62.94 ms | 113.99 ms | 251.30 ms | 22552.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 43.43 ms | 37.23 ms | 65.93 ms | 111.28 ms | 452.88 ms | 23126.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 46.68 ms | 43.66 ms | 87.72 ms | 144.93 ms | 195.97 ms | 29791.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 67.75 ms | 66.85 ms | 91.79 ms | 109.33 ms | 546.42 ms | 23717.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (vibora) (python)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 427818.00 | 247.55 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 406338.00 | 486.35 MB |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 376213.00 | 427.25 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 338520.67 | 383.91 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 331869.33 | 321.78 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 321664.33 | 516.23 MB |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 299011.33 | 611.50 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 295962.67 | 595.00 MB |
| java (8) | [act](http://actframework.org) (1.8) | 285954.67 | 557.76 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 265098.00 | 283.49 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 250688.33 | 144.68 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 217018.33 | 353.37 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 205041.33 | 274.41 MB |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 198294.00 | 250.17 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 197665.67 | 263.95 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 190529.33 | 334.64 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 188994.00 | 252.96 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 181691.67 | 245.04 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 180256.00 | 316.43 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 179217.67 | 314.17 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 170865.00 | 227.55 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 148287.00 | 380.77 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 140868.00 | 210.99 MB |
| go (1.11) | [gf](http://gfer.me) (1.2) | 138307.33 | 209.63 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 134398.33 | 201.22 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 131838.00 | 324.85 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 129961.67 | 194.52 MB |
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 104704.33 | 207.77 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 95669.67 | 228.62 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 93185.33 | 195.55 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 92302.00 | 161.50 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 91502.67 | 196.65 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 88898.67 | 83.52 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 81336.00 | 136.81 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 78363.00 | 388.16 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 78304.67 | 165.25 MB |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 76238.67 | 377.92 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 72302.67 | 375.73 MB |
| c (99) | [kore](http://kore.io) (3.1) | 69980.00 | 189.79 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 66517.67 | 150.91 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 66411.33 | 162.17 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 62288.67 | 94.94 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 58104.67 | 302.49 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 54879.33 | 96.03 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 49350.33 | 91.51 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 45092.33 | 42.95 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 44536.67 | 72.67 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 43175.33 | 40.48 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 40466.67 | 37.93 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 39753.33 | 97.74 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 39010.33 | 22.48 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 38934.67 | 100.46 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 38670.33 | 47.47 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 33247.67 | 60.72 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 30974.67 | 50.48 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 29515.33 | 52.61 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 23439.00 | 13.51 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 23391.00 | 67.89 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 22929.67 | 173.33 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 22253.67 | 44.40 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 17513.67 | 45.44 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 14229.67 | 38.28 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 4549.67 | 13.88 MB |
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

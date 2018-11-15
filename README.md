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
Last update: 2018-11-19
```
OS: Linux (version: 4.18.18-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
threads: 9, connections: 1000
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: laravel (php)


:three: slim (php)


:four: iron (rust)


:five: rack-routing (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 0.08 ms | 0.08 ms | 0.12 ms | 0.16 ms | 2.65 ms | 38.33 |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 103.54 ms | 0.38 ms | 292.06 ms | 1653.71 ms | 7083.38 ms | 355333.33 |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 160.60 ms | 0.39 ms | 276.46 ms | 3527.72 ms | 6805.05 ms | 598685.33 |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 0.45 ms | 0.45 ms | 0.76 ms | 1.06 ms | 9.80 ms | 251.67 |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.93 ms | 0.94 ms | 14.47 ms | 43.61 ms | 134.19 ms | 8999.33 |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 4.14 ms | 1.23 ms | 11.70 ms | 33.87 ms | 114.35 ms | 7004.67 |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 8.15 ms | 1.40 ms | 24.72 ms | 67.61 ms | 191.35 ms | 14219.67 |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 159.70 ms | 1.79 ms | 298.58 ms | 3393.68 ms | 6081.53 ms | 573791.00 |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 170.32 ms | 1.83 ms | 57.71 ms | 4352.07 ms | 6594.38 ms | 756349.67 |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 121.17 ms | 1.86 ms | 239.27 ms | 2754.17 ms | 6221.00 ms | 455372.67 |
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.28 ms | 2.31 ms | 6.80 ms | 15.65 ms | 37.10 ms | 3268.33 |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 3.51 ms | 2.84 ms | 6.60 ms | 13.57 ms | 64.88 ms | 2830.67 |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 9.35 ms | 3.03 ms | 26.87 ms | 68.15 ms | 187.64 ms | 14497.67 |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.45 ms | 3.50 ms | 8.93 ms | 16.99 ms | 40.09 ms | 3659.33 |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 7.91 ms | 3.58 ms | 21.56 ms | 49.80 ms | 130.98 ms | 10805.00 |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.46 ms | 3.69 ms | 5.48 ms | 8.01 ms | 78.27 ms | 1923.33 |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.38 ms | 4.03 ms | 6.71 ms | 13.69 ms | 147.63 ms | 3416.67 |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.75 ms | 4.28 ms | 7.53 ms | 14.03 ms | 33.58 ms | 2756.67 |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 4.86 ms | 4.45 ms | 8.57 ms | 15.98 ms | 32.59 ms | 3066.33 |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 7.04 ms | 4.87 ms | 12.86 ms | 52.76 ms | 119.09 ms | 9542.33 |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 5.82 ms | 5.11 ms | 10.84 ms | 18.72 ms | 285.54 ms | 7387.67 |
| c (99) | [kore](http://kore.io) (3.1) | 18.85 ms | 5.72 ms | 10.12 ms | 642.68 ms | 1177.70 ms | 98252.00 |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 7.13 ms | 6.37 ms | 10.19 ms | 19.61 ms | 213.36 ms | 4827.67 |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 7.57 ms | 6.53 ms | 11.73 ms | 23.22 ms | 97.66 ms | 4007.67 |
| java (8) | [act](http://actframework.org) (1.8) | 7.29 ms | 6.66 ms | 11.77 ms | 24.14 ms | 149.09 ms | 5087.33 |
| go (1.11) | [iris](http://iris-go.com) (11.0) | 7.90 ms | 6.72 ms | 11.99 ms | 25.15 ms | 234.92 ms | 6817.67 |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 8.17 ms | 7.03 ms | 13.20 ms | 26.80 ms | 115.99 ms | 4899.33 |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 7.89 ms | 7.10 ms | 11.11 ms | 28.60 ms | 178.03 ms | 5020.33 |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 8.26 ms | 7.28 ms | 12.85 ms | 25.80 ms | 122.83 ms | 5228.00 |
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.1) | 7.50 ms | 7.45 ms | 9.49 ms | 11.82 ms | 93.04 ms | 2693.00 |
| go (1.11) | [beego](http://beego.me) (1.10) | 8.40 ms | 7.47 ms | 13.14 ms | 26.60 ms | 164.43 ms | 4963.67 |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 8.61 ms | 7.55 ms | 13.55 ms | 28.00 ms | 167.73 ms | 5782.67 |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 9.24 ms | 8.03 ms | 14.31 ms | 29.87 ms | 291.76 ms | 9115.00 |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 12.46 ms | 9.08 ms | 19.66 ms | 57.80 ms | 484.98 ms | 19290.67 |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 169.55 ms | 9.24 ms | 82.75 ms | 3587.87 ms | 7035.15 ms | 645130.00 |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 10.91 ms | 9.28 ms | 18.93 ms | 35.69 ms | 219.57 ms | 8792.67 |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 37.15 ms | 9.71 ms | 118.06 ms | 294.01 ms | 863.32 ms | 63641.33 |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 14.88 ms | 10.53 ms | 23.14 ms | 64.36 ms | 563.16 ms | 23590.33 |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 14.49 ms | 10.70 ms | 23.49 ms | 59.81 ms | 476.33 ms | 18132.67 |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 13.57 ms | 10.72 ms | 24.88 ms | 45.76 ms | 215.43 ms | 9035.00 |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 11.44 ms | 11.49 ms | 13.52 ms | 15.83 ms | 79.01 ms | 1860.33 |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 18.26 ms | 12.53 ms | 24.11 ms | 151.79 ms | 781.51 ms | 37418.00 |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 18.77 ms | 12.98 ms | 23.82 ms | 154.93 ms | 1209.53 ms | 49301.67 |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 20.24 ms | 13.94 ms | 28.07 ms | 155.25 ms | 801.60 ms | 38720.00 |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 17.50 ms | 15.07 ms | 29.90 ms | 53.18 ms | 809.76 ms | 24496.67 |
| node (11.1) | [koa](http://koajs.com) (2.6) | 26.01 ms | 17.99 ms | 34.50 ms | 242.80 ms | 973.94 ms | 51032.33 |
| node (11.1) | [restify](http://restify.com) (7.2) | 26.74 ms | 22.16 ms | 42.92 ms | 77.27 ms | 548.87 ms | 22080.67 |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 25.71 ms | 22.65 ms | 46.53 ms | 69.82 ms | 132.51 ms | 14634.00 |
| node (11.1) | [express](http://expressjs.com) (4.16) | 28.84 ms | 22.75 ms | 42.48 ms | 121.12 ms | 856.08 ms | 37653.00 |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 25.62 ms | 24.37 ms | 34.46 ms | 48.91 ms | 208.57 ms | 7037.33 |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 27.75 ms | 28.04 ms | 33.81 ms | 40.82 ms | 236.34 ms | 6660.67 |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 35.08 ms | 30.48 ms | 57.27 ms | 82.35 ms | 254.85 ms | 15651.33 |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 30.69 ms | 31.89 ms | 38.64 ms | 44.94 ms | 215.00 ms | 6561.67 |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 32.89 ms | 32.09 ms | 41.60 ms | 47.73 ms | 186.79 ms | 7751.67 |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 53.10 ms | 34.89 ms | 59.40 ms | 672.43 ms | 1604.60 ms | 108456.00 |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 39.63 ms | 36.78 ms | 50.16 ms | 57.31 ms | 332.63 ms | 11704.00 |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 38.69 ms | 37.36 ms | 48.88 ms | 55.34 ms | 242.94 ms | 8806.33 |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 39.05 ms | 38.27 ms | 45.43 ms | 51.28 ms | 185.45 ms | 6509.00 |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 54.43 ms | 46.88 ms | 98.66 ms | 185.05 ms | 338.22 ms | 35776.00 |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 62.67 ms | 51.88 ms | 115.91 ms | 159.89 ms | 720.98 ms | 37164.67 |
| go (1.11) | [gf](http://gfer.me) (1.1) | 53.78 ms | 61.18 ms | 101.26 ms | 173.09 ms | 450.65 ms | 42755.33 |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 73.27 ms | 70.33 ms | 121.65 ms | 179.32 ms | 247.82 ms | 37677.33 |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 89.51 ms | 86.12 ms | 112.54 ms | 142.38 ms | 910.03 ms | 37270.67 |

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (actix-web) (rust)


:three: (evhtp) (cpp)


:four: (vibora) (python)


:five: (jester) (nim)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 320012.00 | 382.89 MB |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 267858.33 | 304.63 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 252723.67 | 245.42 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 243021.67 | 275.87 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 224727.33 | 451.60 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 214320.33 | 346.09 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 201209.33 | 215.92 MB |
| java (8) | [act](http://actframework.org) (1.8) | 175198.67 | 342.18 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 173176.67 | 100.18 MB |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 169679.67 | 346.93 MB |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 134331.00 | 169.66 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 132928.00 | 216.72 MB |
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.1) | 131516.67 | 76.00 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 130110.00 | 173.73 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 128434.00 | 225.34 MB |
| go (1.11) | [iris](http://iris-go.com) (11.0) | 125441.00 | 168.20 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 121263.67 | 212.91 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 118953.67 | 159.50 MB |
| go (1.11) | [beego](http://beego.me) (1.10) | 117168.00 | 157.30 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 114883.00 | 201.64 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 109400.67 | 145.74 MB |
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 103177.00 | 204.54 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 93816.67 | 240.87 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 92530.00 | 138.71 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 86070.33 | 80.91 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 77658.67 | 116.39 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 77050.33 | 189.95 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 76745.33 | 114.97 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 68185.67 | 114.01 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 67891.33 | 142.81 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 65616.00 | 140.73 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 64622.00 | 113.11 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 64494.33 | 153.76 MB |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 60495.33 | 300.42 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 59438.67 | 294.42 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 56472.33 | 293.39 MB |
| c (99) | [kore](http://kore.io) (3.1) | 54495.00 | 147.66 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 48884.33 | 103.25 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 45025.67 | 234.66 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 40030.00 | 90.76 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 39154.33 | 72.61 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 39127.00 | 68.44 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 39004.67 | 63.27 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 38761.67 | 94.71 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 36053.33 | 33.83 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 32759.67 | 30.68 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 31100.67 | 29.62 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 30235.67 | 49.26 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 28645.67 | 70.62 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 26128.00 | 15.07 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 26099.33 | 67.46 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 25773.00 | 31.57 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 25407.67 | 46.36 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 24806.67 | 40.41 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 19446.67 | 34.67 MB |
| go (1.11) | [gf](http://gfer.me) (1.1) | 19218.33 | 60.41 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 16185.67 | 9.33 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 16038.00 | 46.55 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 15740.00 | 119.03 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 13816.00 | 27.57 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 13712.00 | 35.56 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 10979.33 | 29.38 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 3444.33 | 10.53 MB |
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

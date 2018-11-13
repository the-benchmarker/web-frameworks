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
Last update: 2018-11-13
```
OS: Linux (version: 4.18.17-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: laravel (php)


:three: lumen (php)


:four: iron (rust)


:five: laravel (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 0.06 ms | 0.07 ms | 0.09 ms | 0.12 ms | 3.61 ms | 35.33 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 80.26 ms | 0.24 ms | 208.54 ms | 1316.31 ms | 6780.66 ms | 296475.33 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 101.57 ms | 0.26 ms | 163.23 ms | 2529.47 ms | 6741.19 ms | 424497.00 | 
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 0.30 ms | 0.31 ms | 0.48 ms | 0.72 ms | 23.34 ms | 230.67 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.51 ms | 0.68 ms | 10.07 ms | 33.05 ms | 110.03 ms | 6690.67 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 6.25 ms | 0.76 ms | 19.13 ms | 60.08 ms | 180.82 ms | 12364.00 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 2.81 ms | 0.97 ms | 7.63 ms | 23.19 ms | 89.59 ms | 4772.33 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 73.87 ms | 1.19 ms | 146.43 ms | 1559.67 ms | 5146.40 ms | 302006.67 | 
| php (7.2) | [symfony](http://symfony.com) (4.1) | 69.31 ms | 1.28 ms | 143.02 ms | 1353.37 ms | 5065.52 ms | 298687.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 159.17 ms | 1.29 ms | 33.49 ms | 3885.09 ms | 6593.05 ms | 699192.33 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 7.24 ms | 2.14 ms | 20.94 ms | 57.01 ms | 171.86 ms | 11916.00 | 
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 2.94 ms | 2.15 ms | 6.17 ms | 15.86 ms | 34.15 ms | 3180.67 | 
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 2.69 ms | 2.18 ms | 5.41 ms | 10.74 ms | 31.45 ms | 2203.33 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.69 ms | 2.43 ms | 15.46 ms | 39.41 ms | 118.80 ms | 8348.67 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.10 ms | 2.43 ms | 5.58 ms | 9.87 ms | 94.33 ms | 2349.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.46 ms | 2.90 ms | 7.21 ms | 13.24 ms | 31.56 ms | 2937.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.86 ms | 2.96 ms | 4.84 ms | 6.56 ms | 57.25 ms | 1569.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.78 ms | 3.19 ms | 6.67 ms | 11.20 ms | 29.49 ms | 2176.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 3.91 ms | 3.45 ms | 7.33 ms | 13.55 ms | 28.63 ms | 2760.67 | 
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 4.13 ms | 3.62 ms | 6.85 ms | 12.08 ms | 280.44 ms | 6836.33 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 5.26 ms | 3.83 ms | 8.60 ms | 45.22 ms | 114.08 ms | 8380.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 5.29 ms | 4.61 ms | 9.02 ms | 17.28 ms | 93.57 ms | 3211.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 5.37 ms | 4.67 ms | 7.78 ms | 15.71 ms | 288.87 ms | 7346.33 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 5.78 ms | 4.68 ms | 8.44 ms | 27.52 ms | 57.23 ms | 4550.00 | 
| go (1.11) | [iris](http://iris-go.com) (11.0) | 5.81 ms | 4.70 ms | 9.35 ms | 19.38 ms | 281.56 ms | 7914.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.60 ms | 4.81 ms | 9.20 ms | 22.08 ms | 75.61 ms | 4350.67 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 5.88 ms | 4.84 ms | 9.93 ms | 19.76 ms | 99.92 ms | 3745.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 5.00 ms | 4.96 ms | 8.37 ms | 9.93 ms | 17.08 ms | 2502.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 7.45 ms | 5.01 ms | 14.17 ms | 27.96 ms | 154.25 ms | 6130.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 6.36 ms | 5.03 ms | 10.69 ms | 22.19 ms | 233.83 ms | 6448.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 6.48 ms | 5.03 ms | 10.60 ms | 22.44 ms | 238.82 ms | 8254.00 | 
| go (1.11) | [beego](http://beego.me) (1.10) | 6.26 ms | 5.13 ms | 10.58 ms | 20.83 ms | 98.83 ms | 3813.00 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 6.77 ms | 5.37 ms | 11.10 ms | 22.30 ms | 233.38 ms | 7720.00 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 8.13 ms | 5.80 ms | 13.78 ms | 33.16 ms | 336.75 ms | 11820.33 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 8.09 ms | 6.18 ms | 13.58 ms | 31.19 ms | 303.16 ms | 9880.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 8.98 ms | 6.36 ms | 19.07 ms | 33.17 ms | 166.17 ms | 7849.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 217.91 ms | 7.27 ms | 150.87 ms | 4956.20 ms | 7914.80 ms | 862702.00 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 32.30 ms | 7.93 ms | 103.16 ms | 275.18 ms | 731.00 ms | 58553.67 | 
| node (10.12) | [fastify](http://fastify.io) (1.13) | 11.73 ms | 8.34 ms | 20.42 ms | 47.98 ms | 444.16 ms | 16237.67 | 
| node (10.12) | [foxify](http://foxify.js.org) (0.10) | 10.56 ms | 8.39 ms | 17.91 ms | 37.42 ms | 286.90 ms | 9291.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 11.75 ms | 10.38 ms | 21.13 ms | 38.57 ms | 362.39 ms | 10971.33 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 13.72 ms | 10.44 ms | 22.60 ms | 49.59 ms | 498.21 ms | 17835.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 13.82 ms | 10.69 ms | 18.56 ms | 42.40 ms | 823.25 ms | 29799.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 11.32 ms | 11.42 ms | 12.96 ms | 14.63 ms | 25.46 ms | 1530.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 16.91 ms | 13.87 ms | 33.38 ms | 51.15 ms | 84.51 ms | 10825.33 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 19.41 ms | 13.93 ms | 28.76 ms | 115.71 ms | 763.88 ms | 35328.67 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 19.57 ms | 16.37 ms | 33.22 ms | 58.53 ms | 420.03 ms | 15496.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 22.39 ms | 19.02 ms | 32.30 ms | 47.84 ms | 408.18 ms | 17945.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 22.57 ms | 19.79 ms | 32.34 ms | 37.88 ms | 252.01 ms | 9770.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 24.70 ms | 20.70 ms | 39.52 ms | 74.72 ms | 261.74 ms | 13927.67 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 31.25 ms | 22.82 ms | 40.70 ms | 284.47 ms | 1111.58 ms | 57696.00 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 27.60 ms | 24.88 ms | 37.85 ms | 42.91 ms | 250.97 ms | 10692.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 30.58 ms | 25.86 ms | 41.92 ms | 47.88 ms | 319.44 ms | 10634.33 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 25.81 ms | 26.96 ms | 34.72 ms | 39.62 ms | 191.45 ms | 7506.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 37.65 ms | 27.75 ms | 74.18 ms | 161.22 ms | 296.69 ms | 31720.67 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 29.58 ms | 28.04 ms | 39.12 ms | 46.06 ms | 550.33 ms | 14094.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 27.33 ms | 29.64 ms | 35.38 ms | 39.44 ms | 240.34 ms | 8445.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 43.83 ms | 36.31 ms | 74.52 ms | 106.99 ms | 508.52 ms | 24992.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 48.54 ms | 39.06 ms | 91.10 ms | 158.99 ms | 203.87 ms | 29654.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 66.59 ms | 63.98 ms | 86.17 ms | 105.40 ms | 708.07 ms | 24705.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (actix-web) (rust)


:three: (evhtp) (cpp)


:four: (evhtp) (cpp)


:five: (fasthttprouter) (go)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (1.1) | 379311.00 | 454.07 MB |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 348244.00 | 395.31 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 309520.67 | 351.50 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 304369.33 | 295.33 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 297938.33 | 481.40 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 277675.67 | 558.24 MB |
| java (8) | [act](http://actframework.org) (1.8) | 263288.67 | 514.08 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 253175.33 | 271.56 MB |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 238223.33 | 487.45 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 232820.67 | 134.59 MB |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 195995.33 | 247.32 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 183838.33 | 299.43 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 182647.00 | 244.56 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 181801.33 | 318.90 MB |
| go (1.11) | [iris](http://iris-go.com) (11.0) | 174906.00 | 234.21 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 167864.00 | 224.51 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 159691.00 | 280.36 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 159362.00 | 279.72 MB |
| go (1.11) | [beego](http://beego.me) (1.10) | 157929.00 | 212.47 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 149373.33 | 199.13 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 139610.67 | 358.92 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 137289.00 | 205.74 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 134755.00 | 202.02 MB |
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 125999.67 | 249.75 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 122854.33 | 302.83 MB |
| node (10.12) | [fastify](http://fastify.io) (1.13) | 102806.33 | 245.31 MB |
| node (10.12) | [foxify](http://foxify.js.org) (0.10) | 100375.67 | 210.77 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 97867.33 | 485.66 MB |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 95753.00 | 475.27 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 92510.33 | 161.76 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 87801.33 | 82.59 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 84560.00 | 181.20 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 83859.00 | 435.03 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 82011.67 | 173.26 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 81989.00 | 138.19 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 66137.33 | 344.24 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 64165.33 | 145.55 MB |
| c (99) | [kore](http://kore.io) (3.1) | 63392.33 | 171.87 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 62341.67 | 152.13 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 53141.67 | 93.00 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 52574.33 | 81.19 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 46045.33 | 43.85 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 45931.33 | 85.20 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 44149.00 | 41.39 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 41170.00 | 101.51 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 39658.00 | 102.43 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 38974.00 | 36.52 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 37003.67 | 21.33 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 36485.33 | 59.44 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 36250.33 | 44.47 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 33356.67 | 60.89 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 32260.00 | 52.64 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 29974.67 | 53.35 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 23176.67 | 67.24 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 22531.00 | 12.99 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 21691.67 | 43.24 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 20772.00 | 156.98 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 17708.33 | 45.91 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 14730.67 | 39.47 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 3991.67 | 12.19 MB |
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

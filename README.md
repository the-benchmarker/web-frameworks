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
Last update: 2018-12-27
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: laravel (php)


:four: lumen (php)


:five: symfony (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 0.08 ms | 0.08 ms | 0.12 ms | 0.17 ms | 3.49 ms | 73.33 | 
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 0.47 ms | 0.44 ms | 0.79 ms | 1.18 ms | 69.55 ms | 531.00 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 201.97 ms | 0.51 ms | 430.20 ms | 4125.63 ms | 7502.85 ms | 699732.67 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 190.92 ms | 0.51 ms | 349.69 ms | 4358.70 ms | 7680.31 ms | 722346.00 | 
| php (7.2) | [symfony](http://symfony.com) (4.2) | 206.75 ms | 0.51 ms | 311.27 ms | 5034.98 ms | 7573.45 ms | 803859.00 | 
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 241.86 ms | 0.64 ms | 427.66 ms | 5081.45 ms | 7772.22 ms | 856080.00 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 205.49 ms | 0.72 ms | 395.89 ms | 4587.27 ms | 7885.28 ms | 762365.00 | 
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 188.40 ms | 1.08 ms | 407.68 ms | 3766.07 ms | 6580.55 ms | 636847.33 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 6.38 ms | 1.27 ms | 18.92 ms | 53.93 ms | 162.46 ms | 11230.00 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 9.48 ms | 1.41 ms | 29.47 ms | 80.76 ms | 220.98 ms | 16959.67 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.15) | 4.82 ms | 1.48 ms | 13.71 ms | 38.47 ms | 122.08 ms | 7993.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 106.53 ms | 2.20 ms | 58.89 ms | 2875.06 ms | 6548.34 ms | 515386.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 3.52 ms | 2.94 ms | 6.22 ms | 11.07 ms | 89.26 ms | 3428.33 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.96 ms | 3.18 ms | 8.20 ms | 16.88 ms | 37.52 ms | 3656.00 | 
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 4.48 ms | 3.43 ms | 7.15 ms | 15.18 ms | 277.60 ms | 9796.00 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 11.05 ms | 3.95 ms | 31.63 ms | 75.88 ms | 190.65 ms | 16370.33 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 9.05 ms | 4.05 ms | 24.55 ms | 57.33 ms | 227.69 ms | 12865.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.52 ms | 4.12 ms | 5.72 ms | 10.78 ms | 187.61 ms | 9950.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 5.10 ms | 4.17 ms | 10.47 ms | 19.04 ms | 43.95 ms | 4132.33 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 5.20 ms | 4.69 ms | 9.54 ms | 17.37 ms | 34.94 ms | 3423.67 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 4.90 ms | 4.78 ms | 8.05 ms | 14.83 ms | 33.48 ms | 2808.00 | 
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 4.93 ms | 4.96 ms | 6.69 ms | 13.22 ms | 32.02 ms | 2272.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.60 ms | 5.13 ms | 8.37 ms | 15.71 ms | 208.70 ms | 7655.00 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.57 ms | 5.20 ms | 8.20 ms | 16.87 ms | 172.27 ms | 4783.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 6.63 ms | 5.74 ms | 9.82 ms | 12.40 ms | 2669.13 ms | 38809.67 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 7.89 ms | 5.76 ms | 13.67 ms | 54.91 ms | 156.61 ms | 9957.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 6.83 ms | 5.99 ms | 9.83 ms | 17.40 ms | 263.08 ms | 6508.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 182.73 ms | 6.96 ms | 266.77 ms | 3829.17 ms | 7314.53 ms | 677370.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 7.69 ms | 7.15 ms | 12.23 ms | 22.50 ms | 129.79 ms | 4957.67 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 8.59 ms | 7.66 ms | 13.24 ms | 26.56 ms | 137.06 ms | 4612.67 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 8.68 ms | 8.13 ms | 12.51 ms | 30.13 ms | 172.96 ms | 5095.33 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 9.07 ms | 8.27 ms | 13.70 ms | 27.96 ms | 138.54 ms | 5413.67 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 9.71 ms | 8.49 ms | 15.01 ms | 32.61 ms | 198.83 ms | 8743.33 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 9.63 ms | 8.78 ms | 14.55 ms | 28.84 ms | 282.63 ms | 6777.33 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 9.91 ms | 8.98 ms | 15.23 ms | 31.45 ms | 231.65 ms | 6460.67 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 9.96 ms | 9.10 ms | 15.01 ms | 30.05 ms | 246.27 ms | 7282.00 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 11.12 ms | 9.73 ms | 16.83 ms | 35.75 ms | 293.47 ms | 9618.33 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 13.44 ms | 10.21 ms | 26.39 ms | 46.79 ms | 187.66 ms | 10196.67 | 
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 15.66 ms | 10.51 ms | 22.49 ms | 132.74 ms | 674.04 ms | 32564.67 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 42.14 ms | 11.74 ms | 131.03 ms | 317.35 ms | 753.22 ms | 69224.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 15.85 ms | 12.35 ms | 29.10 ms | 51.91 ms | 326.78 ms | 13282.00 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 18.64 ms | 12.40 ms | 25.44 ms | 171.17 ms | 818.32 ms | 40578.33 | 
| go (1.11) | [gf](http://gfer.me) (1.3) | 13.93 ms | 12.42 ms | 18.86 ms | 46.61 ms | 395.26 ms | 15003.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 12.65 ms | 12.65 ms | 15.12 ms | 17.46 ms | 33.34 ms | 1974.33 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 19.68 ms | 13.71 ms | 28.77 ms | 125.22 ms | 794.79 ms | 36745.00 | 
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 21.92 ms | 14.58 ms | 26.93 ms | 230.27 ms | 934.44 ms | 48656.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 19.87 ms | 15.09 ms | 25.53 ms | 104.65 ms | 1068.62 ms | 43590.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 38.60 ms | 16.29 ms | 34.45 ms | 820.31 ms | 2739.89 ms | 165896.33 | 
| node (11.1) | [fastify](http://fastify.io) (1.13) | 27.29 ms | 18.42 ms | 35.45 ms | 284.25 ms | 987.97 ms | 54091.00 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 34.41 ms | 21.42 ms | 40.94 ms | 472.34 ms | 1378.75 ms | 83147.33 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 39.27 ms | 23.80 ms | 46.15 ms | 530.97 ms | 1488.02 ms | 92200.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 28.68 ms | 25.82 ms | 46.20 ms | 69.33 ms | 103.82 ms | 12381.00 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 34.85 ms | 27.16 ms | 52.01 ms | 189.42 ms | 798.68 ms | 40279.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 32.39 ms | 29.47 ms | 48.77 ms | 67.18 ms | 542.08 ms | 17039.33 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 31.41 ms | 30.96 ms | 39.98 ms | 48.96 ms | 248.06 ms | 7933.67 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 33.91 ms | 32.03 ms | 44.17 ms | 52.41 ms | 252.62 ms | 7932.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 35.89 ms | 33.25 ms | 50.04 ms | 59.50 ms | 324.32 ms | 10924.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 43.04 ms | 35.97 ms | 71.44 ms | 121.01 ms | 616.83 ms | 27034.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 41.22 ms | 37.65 ms | 53.68 ms | 120.97 ms | 328.42 ms | 17819.67 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 37.67 ms | 38.26 ms | 48.06 ms | 56.01 ms | 270.53 ms | 11932.67 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 74.74 ms | 40.43 ms | 67.73 ms | 1160.32 ms | 2252.46 ms | 183949.00 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 49.28 ms | 50.64 ms | 58.30 ms | 68.84 ms | 498.02 ms | 18216.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 64.26 ms | 55.45 ms | 113.77 ms | 197.77 ms | 306.28 ms | 38808.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 69.99 ms | 60.23 ms | 133.53 ms | 178.11 ms | 251.79 ms | 42988.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 71.58 ms | 65.95 ms | 100.38 ms | 183.69 ms | 908.84 ms | 45741.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 98.09 ms | 95.06 ms | 131.82 ms | 162.89 ms | 379.05 ms | 27169.33 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 278867.33 | 161.33 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 258058.00 | 309.05 MB |
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 235385.33 | 267.54 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 227197.33 | 220.57 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 209306.00 | 237.60 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 198631.67 | 399.30 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.5) | 190650.33 | 179.32 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 188077.33 | 200.59 MB |
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 183313.67 | 375.08 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 169740.33 | 273.56 MB |
| java (8) | [act](http://actframework.org) (1.8) | 159537.33 | 311.02 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 146471.67 | 84.63 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 138290.00 | 225.46 MB |
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 127707.33 | 160.09 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 115816.33 | 203.22 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 114059.33 | 152.69 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 108764.33 | 145.91 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 104407.67 | 183.35 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 103022.67 | 137.60 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 100211.33 | 135.29 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 99146.00 | 174.08 MB |
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 97885.00 | 194.41 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 90882.67 | 121.32 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 81564.00 | 122.28 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 78534.67 | 201.80 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 78033.00 | 73.37 MB |
| go (1.11) | [gf](http://gfer.me) (1.3) | 75086.67 | 113.90 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 69179.67 | 103.51 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 67473.00 | 166.14 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 62370.33 | 93.39 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 59432.67 | 99.51 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 59323.00 | 124.74 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 57570.33 | 100.72 MB |
| c (99) | [kore](http://kore.io) (3.1) | 52747.33 | 143.01 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 50021.33 | 119.76 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 42403.00 | 211.19 MB |
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 41868.67 | 208.11 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 41572.33 | 87.90 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 40914.33 | 87.87 MB |
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 40613.33 | 201.66 MB |
| php (7.2) | [symfony](http://symfony.com) (4.2) | 40005.67 | 199.17 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 39431.67 | 204.91 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 36829.33 | 90.04 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 35420.67 | 80.32 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 35373.00 | 184.49 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 32192.67 | 56.50 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 32026.33 | 52.92 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 31643.67 | 29.68 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 30807.33 | 57.18 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 29260.00 | 27.42 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 27415.67 | 33.69 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.15) | 26623.67 | 25.37 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 26451.67 | 43.11 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 24355.00 | 44.45 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 23773.00 | 58.54 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 22760.33 | 58.90 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 20199.67 | 11.65 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 20083.33 | 32.73 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 16300.67 | 29.05 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.7) | 14667.00 | 29.23 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 14188.00 | 8.19 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 14015.00 | 40.62 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 13514.67 | 102.27 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 11571.00 | 30.01 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 10014.67 | 26.85 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 3032.67 | 9.29 MB |
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

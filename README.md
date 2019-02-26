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
Last update: 2019-02-26
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: iron (rust)


:five: flame (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.08 ms | 0.08 ms | 0.11 ms | 0.14 ms | 13.08 ms | 97.00 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 4.58 ms | 0.22 ms | 18.12 ms | 47.31 ms | 141.90 ms | 10391.00 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 6.16 ms | 0.28 ms | 24.75 ms | 60.72 ms | 154.90 ms | 13578.00 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.49 ms | 0.42 ms | 0.68 ms | 2.50 ms | 106.74 ms | 1141.33 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 8.28 ms | 0.44 ms | 29.58 ms | 66.66 ms | 160.19 ms | 15235.67 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 11.39 ms | 0.54 ms | 42.35 ms | 92.99 ms | 240.00 ms | 21582.67 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 12.71 ms | 0.68 ms | 43.83 ms | 92.92 ms | 236.62 ms | 21787.00 | 
| php (7.3) | [laravel](http://laravel.com) (5.7) | 198.82 ms | 0.89 ms | 563.49 ms | 3252.68 ms | 7064.14 ms | 611021.67 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 187.91 ms | 1.04 ms | 392.47 ms | 3678.56 ms | 7280.51 ms | 647857.67 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 168.69 ms | 1.32 ms | 333.03 ms | 3481.56 ms | 6484.80 ms | 595673.00 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 118.18 ms | 1.70 ms | 294.30 ms | 2238.95 ms | 5957.97 ms | 423248.67 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 194.60 ms | 1.99 ms | 435.88 ms | 3810.56 ms | 7654.98 ms | 678738.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 57.86 ms | 2.04 ms | 6.35 ms | 1579.31 ms | 3583.45 ms | 289141.33 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 4.16 ms | 3.34 ms | 8.44 ms | 18.33 ms | 68.09 ms | 3863.33 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 5.79 ms | 3.39 ms | 13.83 ms | 29.66 ms | 149.85 ms | 6456.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 4.68 ms | 3.70 ms | 9.98 ms | 23.11 ms | 106.06 ms | 5041.33 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 51.50 ms | 4.07 ms | 171.67 ms | 480.13 ms | 1325.62 ms | 101406.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 5.78 ms | 4.42 ms | 11.76 ms | 25.13 ms | 97.15 ms | 5322.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 269.41 ms | 4.87 ms | 505.09 ms | 5160.24 ms | 7517.60 ms | 894521.33 | 
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.6) | 7.45 ms | 4.92 ms | 16.03 ms | 41.47 ms | 205.09 ms | 9768.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 5.73 ms | 5.05 ms | 9.74 ms | 25.80 ms | 115.44 ms | 4807.00 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 6.17 ms | 5.26 ms | 10.48 ms | 23.29 ms | 92.83 ms | 4402.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 7.96 ms | 5.33 ms | 17.27 ms | 38.91 ms | 179.76 ms | 9010.00 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 7.10 ms | 5.39 ms | 13.77 ms | 30.51 ms | 246.64 ms | 8577.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 7.39 ms | 5.61 ms | 15.71 ms | 30.54 ms | 118.93 ms | 6420.00 | 
| node (11.10) | [restana](http://github.com/jkyberneees/ana) (2.9) | 7.74 ms | 5.76 ms | 11.68 ms | 28.56 ms | 317.53 ms | 9760.00 | 
| node (11.10) | [rayo](http://rayo.js.org) (1.2) | 9.44 ms | 6.54 ms | 16.39 ms | 42.87 ms | 386.36 ms | 13646.33 | 
| node (11.10) | [polka](http://github.com/lukeed/polka) (0.5) | 9.86 ms | 6.57 ms | 18.18 ms | 49.04 ms | 383.55 ms | 13841.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 8.70 ms | 6.92 ms | 14.65 ms | 31.14 ms | 228.19 ms | 7282.33 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 12.91 ms | 6.98 ms | 29.44 ms | 95.73 ms | 166.69 ms | 17946.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 8.87 ms | 7.51 ms | 15.13 ms | 35.55 ms | 171.99 ms | 7742.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 8.95 ms | 7.56 ms | 16.11 ms | 37.49 ms | 181.91 ms | 7335.00 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 9.47 ms | 7.73 ms | 16.19 ms | 32.45 ms | 240.12 ms | 7523.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 165.65 ms | 7.80 ms | 25.85 ms | 4596.86 ms | 7919.60 ms | 759082.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 9.95 ms | 8.04 ms | 16.78 ms | 43.23 ms | 227.67 ms | 9087.67 | 
| node (11.10) | [foxify](http://foxify.js.org) (0.10) | 11.57 ms | 9.03 ms | 20.01 ms | 51.48 ms | 424.53 ms | 16267.33 | 
| node (11.10) | [koa](http://koajs.com) (2.7) | 11.89 ms | 9.26 ms | 19.09 ms | 49.15 ms | 470.10 ms | 17483.00 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 11.70 ms | 9.26 ms | 20.09 ms | 44.00 ms | 384.78 ms | 12503.67 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 11.91 ms | 9.48 ms | 21.91 ms | 44.87 ms | 220.90 ms | 9759.33 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 12.34 ms | 9.60 ms | 22.70 ms | 49.08 ms | 175.50 ms | 9414.33 | 
| node (11.10) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 16.65 ms | 9.78 ms | 25.18 ms | 182.12 ms | 773.64 ms | 39996.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (4.0) | 12.32 ms | 9.81 ms | 22.54 ms | 46.04 ms | 219.78 ms | 9144.33 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 12.78 ms | 9.85 ms | 23.96 ms | 50.84 ms | 191.78 ms | 10402.33 | 
| node (11.10) | [fastify](http://fastify.io) (2.0) | 16.30 ms | 10.27 ms | 25.15 ms | 163.96 ms | 667.76 ms | 33686.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 13.86 ms | 10.88 ms | 24.46 ms | 53.40 ms | 381.94 ms | 12990.00 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 14.19 ms | 10.90 ms | 26.48 ms | 56.87 ms | 187.07 ms | 11189.67 | 
| node (11.10) | [express](http://expressjs.com) (4.16) | 15.99 ms | 10.90 ms | 24.21 ms | 93.00 ms | 696.62 ms | 31070.00 | 
| node (11.10) | [restify](http://restify.com) (8.0) | 14.85 ms | 11.62 ms | 21.64 ms | 74.40 ms | 490.86 ms | 20964.67 | 
| go (1.11) | [gf](http://goframe.org) (1.5) | 14.64 ms | 12.63 ms | 22.96 ms | 50.63 ms | 377.50 ms | 11873.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 13.92 ms | 13.62 ms | 16.70 ms | 23.55 ms | 99.99 ms | 2872.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 20.64 ms | 13.70 ms | 26.35 ms | 182.76 ms | 1102.60 ms | 51857.33 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 55.41 ms | 14.68 ms | 32.22 ms | 1367.55 ms | 2526.92 ms | 229914.67 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 18.34 ms | 15.40 ms | 32.32 ms | 57.22 ms | 141.50 ms | 11149.67 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 18.37 ms | 15.74 ms | 30.15 ms | 56.31 ms | 234.15 ms | 11230.00 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 24.24 ms | 21.15 ms | 41.23 ms | 77.56 ms | 154.92 ms | 14325.33 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.5) | 26.98 ms | 22.32 ms | 49.65 ms | 85.72 ms | 190.72 ms | 17290.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 37.50 ms | 24.30 ms | 43.86 ms | 440.81 ms | 1526.75 ms | 92049.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 28.01 ms | 25.93 ms | 46.76 ms | 70.89 ms | 165.96 ms | 14185.00 | 
| node (11.10) | [hapi](http://hapijs.com) (18.1) | 37.48 ms | 27.29 ms | 51.92 ms | 313.51 ms | 1127.61 ms | 60158.00 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 34.54 ms | 29.48 ms | 49.60 ms | 86.75 ms | 371.46 ms | 17697.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 33.60 ms | 29.50 ms | 47.61 ms | 83.07 ms | 550.24 ms | 20941.00 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 36.92 ms | 32.02 ms | 57.31 ms | 102.33 ms | 475.49 ms | 21215.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 39.94 ms | 34.58 ms | 62.58 ms | 106.39 ms | 499.69 ms | 25099.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 46.02 ms | 42.11 ms | 70.30 ms | 120.22 ms | 261.50 ms | 21491.00 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 50.22 ms | 46.17 ms | 69.53 ms | 134.60 ms | 259.76 ms | 20156.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 48.75 ms | 47.19 ms | 66.97 ms | 123.60 ms | 353.53 ms | 20463.00 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 49.57 ms | 48.89 ms | 65.54 ms | 102.62 ms | 301.15 ms | 18107.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 59.42 ms | 52.99 ms | 106.31 ms | 184.12 ms | 343.28 ms | 36989.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 64.10 ms | 58.07 ms | 93.23 ms | 153.32 ms | 460.86 ms | 24801.33 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 71.73 ms | 59.26 ms | 139.26 ms | 221.49 ms | 372.93 ms | 46097.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 103.43 ms | 95.79 ms | 143.95 ms | 231.65 ms | 578.27 ms | 39850.00 | 
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.3) | 111.22 ms | 113.92 ms | 175.41 ms | 232.69 ms | 336.46 ms | 50542.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (actix-web) (rust)


:two: (japronto) (python)


:three: (agoo-c) (c)


:four: (vibora) (python)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 235681.67 | 267.92 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 234877.67 | 281.22 MB |
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 198443.00 | 114.78 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 190256.67 | 216.03 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 172818.00 | 167.75 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 162171.00 | 326.03 MB |
| crystal (0.27) | [onyx](http://github.com/onyxframework/rest) (0.6) | 158848.67 | 149.31 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 156037.00 | 251.94 MB |
| java (8) | [act](http://actframework.org) (1.8) | 155696.00 | 303.85 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 149142.00 | 304.28 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 142889.67 | 152.73 MB |
| node (11.10) | [restana](http://github.com/jkyberneees/ana) (2.9) | 134224.33 | 201.21 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 125677.33 | 158.17 MB |
| node (11.10) | [rayo](http://rayo.js.org) (1.2) | 117494.00 | 176.10 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 116054.67 | 67.17 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 113809.00 | 152.14 MB |
| node (11.10) | [polka](http://github.com/lukeed/polka) (0.5) | 113665.67 | 170.52 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 108288.00 | 215.32 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (4.0) | 106477.33 | 142.22 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 101061.33 | 164.67 MB |
| node (11.10) | [foxify](http://foxify.js.org) (0.10) | 96812.33 | 203.60 MB |
| node (11.10) | [fastify](http://fastify.io) (2.0) | 93808.67 | 227.60 MB |
| node (11.10) | [koa](http://koajs.com) (2.7) | 93444.67 | 197.92 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 90003.67 | 122.08 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 88052.33 | 226.33 MB |
| node (11.10) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 87649.33 | 131.45 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 85827.00 | 150.61 MB |
| go (1.11) | [echo](http://echo.labstack.com) (4.0) | 82217.00 | 144.38 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 80503.33 | 106.67 MB |
| node (11.10) | [restify](http://restify.com) (8.0) | 75733.00 | 132.97 MB |
| node (11.10) | [express](http://expressjs.com) (4.16) | 75703.67 | 185.48 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 75607.00 | 186.40 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 74268.33 | 100.26 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 72588.00 | 68.27 MB |
| go (1.11) | [gf](http://goframe.org) (1.5) | 70148.33 | 106.78 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 64501.00 | 113.06 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 61408.67 | 102.29 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 56708.33 | 122.07 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 55633.33 | 119.46 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 55185.33 | 136.84 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 49126.33 | 244.33 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 42737.00 | 82.62 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 40354.67 | 200.69 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 39639.33 | 62.59 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.5) | 39036.00 | 84.45 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.7) | 37575.67 | 195.65 MB |
| php (7.3) | [laravel](http://laravel.com) (5.7) | 36690.00 | 191.34 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 35977.00 | 81.59 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 35154.33 | 65.19 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 33371.67 | 166.14 MB |
| node (11.10) | [hapi](http://hapijs.com) (18.1) | 32316.00 | 83.88 MB |
| c (99) | [kore](http://kore.io) (3.1) | 30958.33 | 84.19 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 30696.67 | 152.82 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 29696.67 | 48.54 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 29259.33 | 27.50 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.16) | 28106.00 | 26.86 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 27723.67 | 51.59 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 25450.00 | 62.70 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 22162.67 | 20.85 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 21140.00 | 12.22 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 20663.67 | 33.71 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 20381.33 | 24.98 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 20356.33 | 37.28 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 17382.00 | 31.01 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 15402.33 | 8.90 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 15329.67 | 44.46 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 14632.67 | 29.22 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 11205.00 | 84.96 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 10067.00 | 26.19 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 9379.33 | 27.81 MB |
| python (3.7) | [responder](http://github.com/kennethreitz/responder) (1.3) | 8901.00 | 19.43 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 2496.00 | 7.65 MB |
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

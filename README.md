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
Last update: 2019-04-17
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: zend-expressive (php)


:five: laravel (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 0.10 ms | 0.09 ms | 0.14 ms | 0.18 ms | 5.24 ms | 52.00 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.19) | 4.95 ms | 0.31 ms | 17.51 ms | 37.27 ms | 94.57 ms | 8735.67 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 6.31 ms | 0.35 ms | 22.41 ms | 44.90 ms | 117.99 ms | 10865.00 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 243.78 ms | 0.49 ms | 425.10 ms | 5184.80 ms | 7484.05 ms | 857498.33 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 160.74 ms | 0.50 ms | 411.42 ms | 2949.42 ms | 7077.74 ms | 520525.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 179.76 ms | 0.51 ms | 338.40 ms | 3820.08 ms | 7075.16 ms | 638922.33 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 172.40 ms | 0.53 ms | 335.16 ms | 3943.64 ms | 7123.53 ms | 647028.33 | 
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 0.55 ms | 0.53 ms | 0.91 ms | 1.26 ms | 17.97 ms | 316.33 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 195.34 ms | 0.64 ms | 419.47 ms | 3802.52 ms | 7388.26 ms | 672212.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 9.87 ms | 0.71 ms | 30.57 ms | 58.02 ms | 136.69 ms | 14237.67 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 11.06 ms | 0.73 ms | 34.90 ms | 66.92 ms | 177.58 ms | 16574.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 11.59 ms | 0.89 ms | 34.34 ms | 65.52 ms | 170.35 ms | 16182.67 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 218.93 ms | 1.99 ms | 394.33 ms | 4562.22 ms | 7300.79 ms | 772787.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 91.57 ms | 2.41 ms | 5.42 ms | 2963.05 ms | 6594.07 ms | 523652.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 4.42 ms | 2.98 ms | 7.82 ms | 16.65 ms | 288.97 ms | 12298.33 | 
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 4.31 ms | 3.48 ms | 8.50 ms | 18.36 ms | 97.20 ms | 4487.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 4.14 ms | 3.53 ms | 8.31 ms | 17.34 ms | 36.57 ms | 3690.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 7.91 ms | 3.86 ms | 10.37 ms | 71.59 ms | 866.91 ms | 35013.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.40 ms | 4.78 ms | 6.16 ms | 10.80 ms | 37.56 ms | 2091.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 7.22 ms | 5.06 ms | 12.55 ms | 73.15 ms | 335.83 ms | 15175.33 | 
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 6.60 ms | 5.10 ms | 8.93 ms | 75.83 ms | 200.44 ms | 13505.00 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.35 ms | 5.13 ms | 7.92 ms | 15.71 ms | 112.06 ms | 3601.67 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 5.86 ms | 5.22 ms | 10.51 ms | 19.49 ms | 39.06 ms | 3720.33 | 
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 6.87 ms | 5.32 ms | 10.72 ms | 26.60 ms | 373.74 ms | 13297.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 6.94 ms | 5.40 ms | 9.35 ms | 73.34 ms | 321.76 ms | 15536.33 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 8.79 ms | 5.68 ms | 15.63 ms | 82.00 ms | 131.52 ms | 12815.67 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 46.22 ms | 6.48 ms | 151.43 ms | 383.13 ms | 943.45 ms | 83214.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 8.78 ms | 8.13 ms | 11.66 ms | 24.10 ms | 348.00 ms | 9101.33 | 
| java (8) | [act](http://actframework.org) (1.8) | 8.93 ms | 8.20 ms | 13.75 ms | 26.35 ms | 182.45 ms | 6410.00 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 11.02 ms | 8.32 ms | 22.27 ms | 45.92 ms | 144.77 ms | 9093.00 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 10.70 ms | 8.35 ms | 20.09 ms | 44.46 ms | 285.59 ms | 10295.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 192.18 ms | 8.66 ms | 57.63 ms | 4514.73 ms | 7933.77 ms | 789286.33 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 11.87 ms | 8.73 ms | 24.73 ms | 52.57 ms | 149.77 ms | 10449.67 | 
| node (11.14) | [restana](http://github.com/jkyberneees/ana) (2.13) | 11.52 ms | 9.01 ms | 16.01 ms | 81.51 ms | 502.05 ms | 21451.00 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 11.69 ms | 9.06 ms | 23.12 ms | 48.57 ms | 148.15 ms | 9771.33 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 11.71 ms | 9.11 ms | 22.54 ms | 51.34 ms | 200.15 ms | 10301.33 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 10.98 ms | 9.12 ms | 19.47 ms | 43.78 ms | 139.77 ms | 8058.33 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 11.04 ms | 9.42 ms | 18.50 ms | 43.64 ms | 181.34 ms | 8306.33 | 
| node (11.14) | [polka](http://github.com/lukeed/polka) (0.5) | 12.93 ms | 10.03 ms | 18.30 ms | 46.73 ms | 525.32 ms | 20081.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 13.51 ms | 10.19 ms | 24.67 ms | 45.26 ms | 251.59 ms | 11869.00 | 
| go (1.12) | [gf](http://goframe.org) (1.6) | 14.53 ms | 10.62 ms | 28.71 ms | 63.53 ms | 269.35 ms | 14091.00 | 
| node (11.14) | [rayo](http://rayo.js.org) (1.2) | 14.58 ms | 10.81 ms | 18.69 ms | 113.47 ms | 627.22 ms | 29763.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 15.96 ms | 13.10 ms | 29.43 ms | 52.30 ms | 224.47 ms | 11217.00 | 
| node (11.14) | [foxify](http://foxify.js.org) (0.10) | 18.94 ms | 13.64 ms | 23.67 ms | 172.96 ms | 719.01 ms | 37382.00 | 
| node (11.14) | [fastify](http://fastify.io) (2.2) | 24.05 ms | 14.52 ms | 24.83 ms | 364.35 ms | 1057.31 ms | 64926.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 14.87 ms | 14.90 ms | 17.16 ms | 19.42 ms | 80.49 ms | 1939.67 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 23.66 ms | 15.25 ms | 54.44 ms | 92.07 ms | 299.99 ms | 21309.67 | 
| node (11.14) | [koa](http://koajs.com) (2.7) | 21.45 ms | 15.40 ms | 25.92 ms | 193.60 ms | 864.98 ms | 43431.67 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 17.54 ms | 15.95 ms | 29.63 ms | 46.75 ms | 90.63 ms | 9073.67 | 
| node (11.14) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 31.18 ms | 16.83 ms | 31.05 ms | 523.50 ms | 1402.03 ms | 90697.33 | 
| node (11.14) | [express](http://expressjs.com) (4.16) | 27.68 ms | 17.20 ms | 28.48 ms | 412.07 ms | 1170.68 ms | 71855.00 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 30.53 ms | 17.21 ms | 30.30 ms | 487.07 ms | 1524.92 ms | 91069.67 | 
| node (11.14) | [restify](http://restify.com) (8.2) | 21.44 ms | 17.79 ms | 28.55 ms | 69.84 ms | 621.72 ms | 24298.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 22.03 ms | 20.51 ms | 39.77 ms | 62.64 ms | 305.76 ms | 14674.67 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.16) | 28.50 ms | 24.63 ms | 47.90 ms | 78.34 ms | 277.18 ms | 18228.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 31.20 ms | 26.79 ms | 55.03 ms | 81.80 ms | 107.55 ms | 16711.00 | 
| swift (5.0) | [kitura](http://kitura.io) (2.6) | 31.24 ms | 27.22 ms | 43.39 ms | 64.41 ms | 782.36 ms | 32289.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 33.28 ms | 32.08 ms | 41.28 ms | 86.47 ms | 422.13 ms | 15662.00 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 37.87 ms | 32.55 ms | 58.94 ms | 102.32 ms | 244.26 ms | 17343.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 46.13 ms | 33.99 ms | 91.69 ms | 142.14 ms | 487.19 ms | 32718.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 35.52 ms | 34.30 ms | 45.75 ms | 57.78 ms | 267.03 ms | 9437.33 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 41.48 ms | 34.55 ms | 79.04 ms | 122.28 ms | 197.02 ms | 26781.33 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 39.00 ms | 36.73 ms | 48.11 ms | 59.60 ms | 491.61 ms | 13898.33 | 
| node (11.14) | [hapi](http://hapijs.com) (18.1) | 76.92 ms | 36.73 ms | 61.89 ms | 1279.75 ms | 2470.35 ms | 206871.00 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 41.76 ms | 37.63 ms | 64.46 ms | 105.26 ms | 644.52 ms | 26391.00 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 42.37 ms | 39.57 ms | 55.13 ms | 64.54 ms | 411.81 ms | 13221.67 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 44.02 ms | 43.72 ms | 50.73 ms | 60.02 ms | 216.83 ms | 7486.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 70.39 ms | 63.17 ms | 117.99 ms | 195.89 ms | 368.51 ms | 38041.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 84.44 ms | 69.62 ms | 160.41 ms | 240.60 ms | 412.77 ms | 50489.33 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 106.82 ms | 91.29 ms | 195.21 ms | 244.80 ms | 314.11 ms | 56285.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 106.78 ms | 98.98 ms | 157.71 ms | 240.84 ms | 933.08 ms | 48937.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 107.38 ms | 105.61 ms | 143.43 ms | 173.19 ms | 478.12 ms | 28970.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (evhtp) (cpp)


:five: (kore) (c)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 267646.67 | 155.85 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 243746.00 | 291.81 MB |
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 230669.67 | 262.21 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 203594.33 | 197.65 MB |
| c (99) | [kore](http://kore.io) (3.1) | 197355.33 | 512.97 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 186988.33 | 376.02 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 184799.33 | 209.49 MB |
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 176668.67 | 166.08 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 172929.33 | 277.62 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 166572.67 | 177.33 MB |
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 157763.33 | 323.71 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 149270.67 | 86.17 MB |
| java (8) | [act](http://actframework.org) (1.8) | 133076.00 | 259.60 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 110958.67 | 180.84 MB |
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 108310.33 | 136.39 MB |
| node (11.14) | [restana](http://github.com/jkyberneees/ana) (2.13) | 101942.00 | 152.85 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 100056.33 | 134.41 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 99016.33 | 173.84 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 95028.67 | 126.44 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 94115.33 | 126.21 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 93146.00 | 163.58 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 93114.00 | 123.76 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 92387.00 | 124.42 MB |
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 86861.00 | 172.82 MB |
| node (11.14) | [polka](http://github.com/lukeed/polka) (0.5) | 84726.67 | 126.89 MB |
| node (11.14) | [rayo](http://rayo.js.org) (1.2) | 82744.33 | 124.02 MB |
| go (1.12) | [gf](http://goframe.org) (1.6) | 77485.33 | 116.88 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 77376.67 | 198.96 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 69820.00 | 65.59 MB |
| node (11.14) | [foxify](http://foxify.js.org) (0.10) | 65697.33 | 138.17 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 65434.67 | 161.12 MB |
| node (11.14) | [fastify](http://fastify.io) (2.2) | 64941.33 | 174.92 MB |
| node (11.14) | [koa](http://koajs.com) (2.7) | 58835.00 | 124.51 MB |
| node (11.14) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 58531.67 | 87.62 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 57216.00 | 123.20 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 57013.33 | 122.28 MB |
| node (11.14) | [express](http://expressjs.com) (4.16) | 51876.00 | 127.10 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 51056.67 | 85.61 MB |
| node (11.14) | [restify](http://restify.com) (8.2) | 50435.33 | 88.50 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 48896.33 | 121.28 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 48160.33 | 84.28 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 47531.67 | 236.85 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 45256.33 | 224.26 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 44442.00 | 220.31 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 41068.33 | 213.04 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 40227.00 | 201.04 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 36820.00 | 192.05 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.16) | 35901.00 | 77.36 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.6) | 33207.67 | 61.60 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 32595.67 | 73.95 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 31242.00 | 50.80 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 30572.00 | 28.68 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 27813.67 | 45.31 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 27744.33 | 26.06 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.19) | 25872.33 | 24.66 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.13) | 25670.00 | 49.53 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 25482.67 | 31.38 MB |
| node (11.14) | [hapi](http://hapijs.com) (18.1) | 24889.33 | 64.58 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 24036.00 | 44.69 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 23252.00 | 57.32 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 23111.33 | 37.67 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 22721.00 | 41.49 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 20282.00 | 11.70 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 14531.67 | 25.90 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 12925.67 | 7.46 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 12437.33 | 24.78 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 11551.67 | 87.46 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 11016.33 | 28.58 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 9389.00 | 20.46 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 9219.33 | 26.76 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 9134.33 | 26.99 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 2778.67 | 8.45 MB |
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

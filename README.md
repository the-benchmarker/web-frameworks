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
Last update: 2019-03-16
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
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.07 ms | 0.08 ms | 0.10 ms | 0.13 ms | 2.09 ms | 29.00 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.17) | 2.98 ms | 0.15 ms | 10.89 ms | 27.32 ms | 67.97 ms | 6024.00 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.63 ms | 0.18 ms | 13.74 ms | 31.67 ms | 81.90 ms | 7180.00 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.31 ms | 0.29 ms | 0.49 ms | 0.88 ms | 11.78 ms | 196.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 5.63 ms | 0.31 ms | 19.64 ms | 41.90 ms | 93.89 ms | 9790.67 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 6.46 ms | 0.32 ms | 23.06 ms | 48.53 ms | 115.33 ms | 11512.00 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 90.33 ms | 0.42 ms | 236.03 ms | 1641.34 ms | 6745.87 ms | 329082.67 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 105.32 ms | 0.44 ms | 174.73 ms | 2482.44 ms | 6731.97 ms | 434505.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 7.41 ms | 0.44 ms | 24.27 ms | 49.40 ms | 112.62 ms | 11845.67 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 111.71 ms | 1.13 ms | 204.11 ms | 2600.58 ms | 5798.87 ms | 425455.33 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 105.53 ms | 1.14 ms | 187.63 ms | 2411.54 ms | 5435.83 ms | 408663.33 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 97.27 ms | 1.19 ms | 183.95 ms | 2272.06 ms | 5729.99 ms | 393250.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 93.59 ms | 1.42 ms | 3.55 ms | 3019.30 ms | 4671.04 ms | 474824.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 2.65 ms | 1.71 ms | 6.11 ms | 12.04 ms | 89.30 ms | 2954.00 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 29.64 ms | 1.87 ms | 101.12 ms | 279.01 ms | 811.70 ms | 59360.33 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.80 ms | 2.09 ms | 5.84 ms | 11.74 ms | 29.12 ms | 2561.67 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 2.88 ms | 2.19 ms | 6.06 ms | 13.42 ms | 34.87 ms | 2740.33 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 81.20 ms | 2.32 ms | 157.99 ms | 1753.10 ms | 5549.53 ms | 337645.67 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.17 ms | 2.52 ms | 5.59 ms | 10.07 ms | 90.95 ms | 2249.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 8.35 ms | 2.76 ms | 6.89 ms | 181.40 ms | 1012.01 ms | 48577.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 2.92 ms | 2.88 ms | 5.01 ms | 7.17 ms | 77.56 ms | 1803.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.72 ms | 3.01 ms | 7.81 ms | 14.52 ms | 32.94 ms | 3197.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 3.73 ms | 3.19 ms | 6.49 ms | 10.83 ms | 38.81 ms | 2106.00 | 
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 3.58 ms | 3.27 ms | 6.22 ms | 12.18 ms | 28.33 ms | 2418.00 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 5.61 ms | 3.32 ms | 9.04 ms | 70.48 ms | 124.21 ms | 10896.00 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 3.99 ms | 3.42 ms | 7.34 ms | 17.06 ms | 127.00 ms | 3538.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 4.01 ms | 3.51 ms | 7.51 ms | 13.62 ms | 27.70 ms | 2781.00 | 
| go (1.12) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 5.71 ms | 4.51 ms | 11.35 ms | 24.48 ms | 70.45 ms | 4832.67 | 
| go (1.12) | [iris](http://iris-go.com) (11.1) | 5.67 ms | 4.53 ms | 10.84 ms | 23.76 ms | 205.45 ms | 5415.67 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 6.28 ms | 4.57 ms | 13.01 ms | 27.71 ms | 190.56 ms | 7410.00 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 6.10 ms | 4.59 ms | 12.64 ms | 25.59 ms | 121.09 ms | 5809.00 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 6.37 ms | 4.59 ms | 13.55 ms | 28.12 ms | 147.25 ms | 6179.00 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 5.19 ms | 4.65 ms | 7.56 ms | 14.68 ms | 277.11 ms | 5021.33 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 6.02 ms | 4.65 ms | 11.39 ms | 24.72 ms | 165.87 ms | 5578.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 5.46 ms | 4.66 ms | 9.04 ms | 19.41 ms | 160.94 ms | 4457.00 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 6.97 ms | 4.68 ms | 14.80 ms | 31.60 ms | 288.19 ms | 9539.33 | 
| go (1.11) | [beego](http://beego.me) (1.12) | 6.49 ms | 4.75 ms | 13.40 ms | 27.81 ms | 105.17 ms | 5583.67 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 8.30 ms | 5.03 ms | 17.87 ms | 34.19 ms | 211.60 ms | 8002.33 | 
| node (11.11) | [restana](http://github.com/jkyberneees/ana) (2.10) | 6.67 ms | 5.43 ms | 9.67 ms | 18.39 ms | 198.12 ms | 5394.33 | 
| go (1.12) | [gf](http://goframe.org) (1.5) | 8.19 ms | 5.63 ms | 17.12 ms | 36.90 ms | 176.19 ms | 7631.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 164.66 ms | 6.46 ms | 24.99 ms | 4158.43 ms | 6588.67 ms | 693363.67 | 
| node (11.11) | [polka](http://github.com/lukeed/polka) (0.5) | 7.37 ms | 6.53 ms | 11.06 ms | 19.70 ms | 221.86 ms | 6262.67 | 
| node (11.11) | [rayo](http://rayo.js.org) (1.2) | 7.96 ms | 6.68 ms | 11.28 ms | 21.71 ms | 330.80 ms | 11711.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 8.85 ms | 7.28 ms | 15.49 ms | 29.85 ms | 213.27 ms | 7433.33 | 
| node (11.11) | [foxify](http://foxify.js.org) (0.10) | 9.10 ms | 8.23 ms | 13.21 ms | 23.50 ms | 328.60 ms | 10575.67 | 
| node (11.11) | [fastify](http://fastify.io) (2.1) | 10.32 ms | 8.45 ms | 14.14 ms | 54.45 ms | 424.68 ms | 17139.00 | 
| node (11.11) | [express](http://expressjs.com) (4.16) | 11.81 ms | 9.33 ms | 16.77 ms | 35.87 ms | 449.33 ms | 17171.00 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 10.61 ms | 9.35 ms | 17.44 ms | 25.03 ms | 105.99 ms | 5063.33 | 
| node (11.11) | [restify](http://restify.com) (8.1) | 10.61 ms | 9.76 ms | 13.53 ms | 24.75 ms | 254.92 ms | 7056.00 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 9.62 ms | 9.84 ms | 11.48 ms | 13.27 ms | 37.48 ms | 1711.33 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 11.64 ms | 9.85 ms | 19.01 ms | 32.90 ms | 222.60 ms | 8125.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.3) | 14.03 ms | 10.64 ms | 20.00 ms | 56.43 ms | 718.19 ms | 27664.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 12.15 ms | 10.97 ms | 22.25 ms | 38.86 ms | 253.05 ms | 8478.67 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 13.68 ms | 12.45 ms | 23.28 ms | 33.65 ms | 77.81 ms | 6901.33 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.7) | 15.92 ms | 13.20 ms | 29.36 ms | 43.92 ms | 114.78 ms | 9123.33 | 
| node (11.11) | [koa](http://koajs.com) (2.7) | 17.25 ms | 13.80 ms | 26.91 ms | 57.81 ms | 543.87 ms | 20346.00 | 
| node (11.11) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 21.86 ms | 14.52 ms | 30.34 ms | 195.72 ms | 889.58 ms | 44593.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 17.43 ms | 14.71 ms | 30.19 ms | 41.84 ms | 100.50 ms | 9080.67 | 
| swift (4.2) | [kitura](http://kitura.io) (2.6) | 22.56 ms | 18.38 ms | 29.23 ms | 107.35 ms | 882.98 ms | 35703.67 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 25.14 ms | 20.60 ms | 47.74 ms | 64.69 ms | 246.27 ms | 13676.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 22.21 ms | 21.53 ms | 30.55 ms | 33.92 ms | 230.18 ms | 6599.33 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 25.11 ms | 21.90 ms | 37.22 ms | 59.42 ms | 324.01 ms | 13208.33 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 26.12 ms | 26.12 ms | 33.62 ms | 37.66 ms | 199.84 ms | 7129.00 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 30.96 ms | 27.19 ms | 43.66 ms | 48.76 ms | 168.21 ms | 8593.67 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 30.74 ms | 28.05 ms | 38.76 ms | 60.91 ms | 546.09 ms | 22928.00 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 26.27 ms | 28.25 ms | 31.55 ms | 37.68 ms | 248.05 ms | 8943.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 35.96 ms | 29.66 ms | 63.03 ms | 123.47 ms | 208.57 ms | 23401.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 30.84 ms | 30.94 ms | 37.31 ms | 44.42 ms | 322.56 ms | 10900.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 43.94 ms | 34.61 ms | 78.25 ms | 113.87 ms | 294.42 ms | 22773.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 42.55 ms | 40.58 ms | 66.95 ms | 96.57 ms | 140.87 ms | 19815.33 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 57.23 ms | 53.66 ms | 97.32 ms | 132.20 ms | 169.86 ms | 30936.67 | 
| node (11.11) | [hapi](http://hapijs.com) (18.1) | 118.26 ms | 58.50 ms | 126.51 ms | 1591.80 ms | 2877.67 ms | 262851.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 68.10 ms | 66.67 ms | 89.78 ms | 115.29 ms | 768.28 ms | 29159.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (kore) (c)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 400166.33 | 231.48 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 357996.00 | 428.65 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 352564.67 | 400.67 MB |
| c (99) | [kore](http://kore.io) (3.1) | 310622.33 | 806.31 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 301201.00 | 292.38 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 294489.00 | 473.96 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 287283.67 | 326.28 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 274745.67 | 551.76 MB |
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 265234.33 | 249.40 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 262445.67 | 151.91 MB |
| java (8) | [act](http://actframework.org) (1.8) | 256311.00 | 500.37 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 254845.67 | 521.93 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 246881.33 | 262.71 MB |
| go (1.12) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 186649.67 | 250.52 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 185540.33 | 302.02 MB |
| go (1.12) | [iris](http://iris-go.com) (11.1) | 182805.00 | 243.43 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 178325.33 | 239.07 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 177579.67 | 223.92 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 176135.00 | 308.44 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 174296.67 | 231.58 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 173275.00 | 303.46 MB |
| go (1.11) | [beego](http://beego.me) (1.12) | 168435.00 | 226.87 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 166376.67 | 222.41 MB |
| node (11.11) | [restana](http://github.com/jkyberneees/ana) (2.10) | 148625.33 | 222.76 MB |
| go (1.12) | [gf](http://goframe.org) (1.5) | 136619.33 | 206.98 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 136102.33 | 349.72 MB |
| node (11.11) | [polka](http://github.com/lukeed/polka) (0.5) | 134830.33 | 202.11 MB |
| node (11.11) | [rayo](http://rayo.js.org) (1.2) | 133586.00 | 200.30 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 115512.67 | 284.79 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 115213.00 | 227.84 MB |
| node (11.11) | [fastify](http://fastify.io) (2.1) | 113916.33 | 301.45 MB |
| node (11.11) | [foxify](http://foxify.js.org) (0.10) | 113191.00 | 238.07 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 102223.00 | 96.07 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 96382.33 | 479.59 MB |
| node (11.11) | [restify](http://restify.com) (8.1) | 94393.33 | 165.75 MB |
| node (11.11) | [express](http://expressjs.com) (4.16) | 92712.33 | 227.14 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 92602.00 | 199.52 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 88875.00 | 440.95 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 88379.67 | 154.62 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 86001.33 | 213.33 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 85745.33 | 425.33 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 83830.67 | 416.04 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 83404.00 | 178.94 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 80905.67 | 419.99 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.3) | 80623.67 | 134.99 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 73178.33 | 141.35 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 64904.00 | 338.14 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.7) | 64112.00 | 138.61 MB |
| node (11.11) | [koa](http://koajs.com) (2.7) | 62718.67 | 132.74 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 61896.33 | 98.80 MB |
| node (11.11) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 61272.00 | 91.65 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 57544.67 | 130.44 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.6) | 48909.67 | 90.72 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 44443.67 | 41.68 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.17) | 43134.33 | 41.06 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 41055.00 | 76.32 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 39906.33 | 98.24 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 38483.00 | 36.05 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 38271.33 | 62.34 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 35473.00 | 20.44 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 33428.33 | 61.03 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 32722.33 | 40.07 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 31913.67 | 51.99 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 29469.33 | 52.51 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 23576.00 | 47.02 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 23099.33 | 67.05 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 22666.67 | 13.07 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 19859.00 | 150.06 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 17555.67 | 38.26 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 17297.33 | 44.89 MB |
| node (11.11) | [hapi](http://hapijs.com) (18.1) | 14515.00 | 37.53 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 14445.33 | 42.67 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 4322.00 | 13.28 MB |
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

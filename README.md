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
Last update: 2019-03-19
```
OS: Linux (version: 4.16.3-301.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: zend-framework (php)


:five: iron (rust)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 0.08 ms | 0.08 ms | 0.12 ms | 0.16 ms | 4.74 ms | 64.67 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.18) | 2.98 ms | 0.16 ms | 10.81 ms | 26.69 ms | 74.35 ms | 5924.33 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.99 ms | 0.21 ms | 14.54 ms | 32.62 ms | 81.76 ms | 7502.00 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 104.67 ms | 0.29 ms | 219.87 ms | 2124.09 ms | 6846.50 ms | 412017.33 | 
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 0.34 ms | 0.33 ms | 0.57 ms | 0.90 ms | 13.52 ms | 215.67 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 163.60 ms | 0.34 ms | 253.13 ms | 3724.57 ms | 6761.37 ms | 617184.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 6.22 ms | 0.37 ms | 21.22 ms | 44.73 ms | 106.87 ms | 10518.00 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 134.63 ms | 0.39 ms | 303.60 ms | 2458.56 ms | 6880.35 ms | 454445.67 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 7.35 ms | 0.39 ms | 24.93 ms | 51.57 ms | 129.54 ms | 12320.33 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 7.69 ms | 0.44 ms | 25.58 ms | 52.28 ms | 117.56 ms | 12492.33 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 104.95 ms | 1.10 ms | 233.17 ms | 2014.87 ms | 5694.63 ms | 375319.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 94.90 ms | 1.62 ms | 4.80 ms | 2924.63 ms | 6593.00 ms | 531561.00 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 31.03 ms | 2.02 ms | 106.63 ms | 293.69 ms | 907.38 ms | 62441.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 3.11 ms | 2.31 ms | 6.61 ms | 13.94 ms | 44.17 ms | 2993.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.45 ms | 2.41 ms | 7.38 ms | 16.67 ms | 37.03 ms | 3509.33 | 
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 3.39 ms | 2.51 ms | 7.19 ms | 14.81 ms | 77.64 ms | 3202.00 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 109.80 ms | 2.66 ms | 235.68 ms | 2229.75 ms | 4513.63 ms | 381638.67 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 80.97 ms | 2.83 ms | 192.10 ms | 1644.76 ms | 4408.98 ms | 295835.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 5.22 ms | 3.05 ms | 7.46 ms | 15.81 ms | 585.05 ms | 19789.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.11 ms | 3.17 ms | 8.72 ms | 16.97 ms | 38.70 ms | 3661.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.19 ms | 3.44 ms | 7.10 ms | 13.20 ms | 31.00 ms | 2553.00 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.92 ms | 3.51 ms | 6.10 ms | 11.96 ms | 53.91 ms | 2404.33 | 
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 3.94 ms | 3.70 ms | 6.73 ms | 12.97 ms | 28.09 ms | 2564.00 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 4.30 ms | 3.75 ms | 8.09 ms | 15.02 ms | 46.17 ms | 3095.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.54 ms | 3.81 ms | 5.49 ms | 8.02 ms | 65.92 ms | 1890.33 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 5.87 ms | 4.14 ms | 9.83 ms | 58.92 ms | 139.70 ms | 9506.33 | 
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 4.72 ms | 4.22 ms | 8.72 ms | 19.22 ms | 212.10 ms | 4835.00 | 
| go (1.12) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 5.95 ms | 4.64 ms | 11.61 ms | 24.84 ms | 161.36 ms | 5603.00 | 
| go (1.12) | [iris](http://iris-go.com) (11.1) | 6.04 ms | 4.72 ms | 11.49 ms | 24.67 ms | 159.94 ms | 5378.67 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 7.05 ms | 4.88 ms | 14.37 ms | 30.43 ms | 258.50 ms | 9215.33 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 7.33 ms | 4.90 ms | 15.97 ms | 33.30 ms | 222.80 ms | 7854.67 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 7.04 ms | 4.96 ms | 14.83 ms | 30.33 ms | 118.95 ms | 6548.33 | 
| go (1.11) | [beego](http://beego.me) (1.12) | 7.49 ms | 5.23 ms | 14.90 ms | 32.94 ms | 251.00 ms | 9304.00 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 7.08 ms | 5.23 ms | 14.35 ms | 29.21 ms | 77.68 ms | 5832.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 6.47 ms | 5.61 ms | 9.40 ms | 18.18 ms | 284.42 ms | 6823.67 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 7.55 ms | 5.72 ms | 13.95 ms | 29.84 ms | 180.52 ms | 6477.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.67 ms | 5.93 ms | 10.93 ms | 20.79 ms | 121.61 ms | 4456.33 | 
| go (1.12) | [gf](http://goframe.org) (1.5) | 9.08 ms | 6.26 ms | 18.40 ms | 40.29 ms | 208.95 ms | 10500.33 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 21.81 ms | 7.09 ms | 18.74 ms | 425.25 ms | 727.78 ms | 69463.67 | 
| node (11.11) | [restana](http://github.com/jkyberneees/ana) (2.10) | 9.09 ms | 8.25 ms | 13.49 ms | 25.96 ms | 305.07 ms | 10045.67 | 
| node (11.11) | [rayo](http://rayo.js.org) (1.2) | 9.27 ms | 8.26 ms | 13.43 ms | 26.13 ms | 389.00 ms | 13779.33 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 190.43 ms | 8.26 ms | 67.12 ms | 4334.01 ms | 7922.31 ms | 767465.00 | 
| node (11.11) | [polka](http://github.com/lukeed/polka) (0.5) | 9.66 ms | 8.29 ms | 13.64 ms | 28.48 ms | 441.63 ms | 16359.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 9.42 ms | 8.33 ms | 16.35 ms | 28.63 ms | 163.30 ms | 6751.67 | 
| node (11.11) | [foxify](http://foxify.js.org) (0.10) | 11.78 ms | 9.21 ms | 16.10 ms | 35.91 ms | 523.33 ms | 20283.33 | 
| node (11.11) | [fastify](http://fastify.io) (2.1) | 12.79 ms | 11.05 ms | 17.68 ms | 36.86 ms | 490.55 ms | 18255.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 11.19 ms | 11.25 ms | 13.10 ms | 15.20 ms | 89.79 ms | 2505.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 12.99 ms | 11.54 ms | 23.88 ms | 42.89 ms | 218.09 ms | 9565.33 | 
| node (11.11) | [restify](http://restify.com) (8.1) | 12.82 ms | 11.57 ms | 17.14 ms | 36.02 ms | 346.54 ms | 12311.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.3) | 17.72 ms | 11.83 ms | 20.87 ms | 164.13 ms | 1092.29 ms | 49914.33 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 14.95 ms | 11.95 ms | 27.84 ms | 52.10 ms | 129.76 ms | 10477.67 | 
| node (11.11) | [express](http://expressjs.com) (4.16) | 15.85 ms | 12.12 ms | 20.24 ms | 96.37 ms | 682.02 ms | 30211.67 | 
| python (3.7) | [hug](http://hug.rest) (2.4) | 29.90 ms | 12.18 ms | 26.34 ms | 433.97 ms | 664.17 ms | 75812.67 | 
| node (11.11) | [koa](http://koajs.com) (2.7) | 22.32 ms | 16.38 ms | 33.52 ms | 116.02 ms | 796.70 ms | 35949.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 19.50 ms | 17.22 ms | 31.84 ms | 46.81 ms | 81.32 ms | 9078.33 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.8) | 19.36 ms | 17.29 ms | 32.16 ms | 46.08 ms | 82.54 ms | 9357.33 | 
| node (11.11) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 24.27 ms | 17.41 ms | 36.91 ms | 144.17 ms | 841.23 ms | 38454.67 | 
| swift (4.2) | [kitura](http://kitura.io) (2.6) | 25.71 ms | 21.15 ms | 32.22 ms | 117.47 ms | 947.68 ms | 38782.00 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 24.96 ms | 21.65 ms | 44.11 ms | 67.26 ms | 158.68 ms | 14212.00 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 24.17 ms | 21.91 ms | 35.74 ms | 41.04 ms | 300.36 ms | 7118.33 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 25.08 ms | 21.97 ms | 33.66 ms | 57.49 ms | 243.76 ms | 11156.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 30.66 ms | 22.76 ms | 59.62 ms | 87.72 ms | 255.21 ms | 19571.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 24.11 ms | 23.58 ms | 32.04 ms | 37.18 ms | 226.25 ms | 6081.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 26.57 ms | 24.05 ms | 36.01 ms | 46.82 ms | 320.04 ms | 8560.33 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 41.78 ms | 25.00 ms | 63.18 ms | 412.12 ms | 641.19 ms | 62382.67 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 29.81 ms | 27.99 ms | 38.39 ms | 43.32 ms | 318.81 ms | 10400.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 38.20 ms | 35.73 ms | 50.74 ms | 59.84 ms | 466.40 ms | 16192.67 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 43.92 ms | 38.83 ms | 75.88 ms | 128.55 ms | 222.39 ms | 24903.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 50.55 ms | 42.11 ms | 92.20 ms | 129.37 ms | 417.14 ms | 26745.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 51.56 ms | 48.46 ms | 87.63 ms | 130.43 ms | 194.93 ms | 26933.00 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 72.83 ms | 69.63 ms | 125.20 ms | 165.97 ms | 223.52 ms | 37408.67 | 
| node (11.11) | [hapi](http://hapijs.com) (18.1) | 165.02 ms | 70.12 ms | 160.13 ms | 2396.19 ms | 4024.94 ms | 400443.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 81.92 ms | 81.37 ms | 105.00 ms | 138.05 ms | 349.91 ms | 23215.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (vibora) (python)


:five: (jester) (nim)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.4) | 328419.67 | 189.99 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 310940.33 | 372.37 MB |
| rust (1.32) | [actix-web](http://actix.rs) (0.7) | 291141.67 | 331.09 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 266672.67 | 302.90 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 253092.00 | 508.73 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 249621.33 | 242.37 MB |
| c (99) | [kore](http://kore.io) (3.1) | 246320.33 | 640.04 MB |
| crystal (0.27) | [onyx](http://onyxframework.org) (0.3) | 240168.00 | 225.87 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 236561.67 | 379.87 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 230502.33 | 246.88 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 213451.00 | 123.37 MB |
| rust (1.32) | [gotham](http://gotham.rs) (0.3) | 209028.00 | 428.00 MB |
| java (8) | [act](http://actframework.org) (1.8) | 198304.00 | 387.25 MB |
| go (1.12) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 176903.00 | 237.05 MB |
| go (1.12) | [iris](http://iris-go.com) (11.1) | 172612.67 | 231.31 MB |
| rust (1.32) | [iron](http://ironframework.io) (0.6) | 165821.67 | 208.86 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.0) | 159133.00 | 279.26 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 156475.00 | 274.72 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 152416.67 | 203.40 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 151189.67 | 202.53 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 149347.00 | 243.44 MB |
| go (1.11) | [beego](http://beego.me) (1.12) | 146395.67 | 196.09 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 138010.33 | 183.23 MB |
| go (1.12) | [gf](http://goframe.org) (1.5) | 124764.67 | 188.63 MB |
| node (11.11) | [rayo](http://rayo.js.org) (1.2) | 115638.67 | 172.99 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 114209.00 | 293.20 MB |
| node (11.11) | [polka](http://github.com/lukeed/polka) (0.5) | 113961.67 | 170.74 MB |
| node (11.11) | [restana](http://github.com/jkyberneees/ana) (2.10) | 112946.00 | 169.26 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 108138.67 | 266.45 MB |
| rust (1.32) | [nickel](http://nickel-org.github.io) (0.11) | 95208.00 | 189.60 MB |
| node (11.11) | [foxify](http://foxify.js.org) (0.10) | 94753.33 | 199.20 MB |
| node (11.11) | [fastify](http://fastify.io) (2.1) | 93887.33 | 235.56 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 87940.67 | 82.55 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 81855.00 | 143.32 MB |
| node (11.11) | [restify](http://restify.com) (8.1) | 81441.33 | 142.98 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 76485.33 | 380.16 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 74774.33 | 371.48 MB |
| node (11.11) | [express](http://expressjs.com) (4.16) | 74112.33 | 181.57 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.3) | 73581.00 | 123.25 MB |
| python (3.7) | [hug](http://hug.rest) (2.4) | 73233.33 | 181.31 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 72076.00 | 358.50 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 71945.00 | 154.28 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 71001.33 | 152.92 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 67779.33 | 336.34 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 63891.33 | 331.25 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 55995.33 | 88.19 MB |
| node (11.11) | [koa](http://koajs.com) (2.7) | 52670.33 | 111.43 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.8) | 51870.67 | 111.85 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 51426.67 | 116.66 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 51253.67 | 266.66 MB |
| node (11.11) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 49591.00 | 74.21 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.18) | 43072.00 | 41.09 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.6) | 42719.00 | 79.22 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 41238.33 | 38.66 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.12) | 40862.67 | 78.94 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 40462.67 | 65.91 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 40356.33 | 37.82 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 37072.33 | 45.52 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 33939.67 | 83.57 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 33515.33 | 61.18 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 32833.33 | 61.04 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 32183.67 | 18.55 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 26870.67 | 43.78 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (18.12) | 23495.67 | 41.86 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 20596.67 | 11.90 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 20031.67 | 58.13 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.8) | 19735.00 | 39.35 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 17385.33 | 131.44 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 16636.00 | 43.19 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 13760.00 | 29.99 MB |
| node (11.11) | [hapi](http://hapijs.com) (18.1) | 12126.00 | 31.43 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 11902.67 | 35.14 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 4136.00 | 12.67 MB |
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

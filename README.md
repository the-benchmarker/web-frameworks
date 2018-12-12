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
Last update: 2018-12-13
```
OS: Linux (version: 4.19.7-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: lumen (php)


:four: zend-framework (php)


:five: laravel (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 0.11 ms | 0.10 ms | 0.15 ms | 0.22 ms | 4.06 ms | 59.33 | 
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 0.60 ms | 0.57 ms | 1.02 ms | 1.53 ms | 10.72 ms | 339.00 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 209.78 ms | 0.65 ms | 388.92 ms | 4459.16 ms | 7595.18 ms | 744692.33 | 
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 210.84 ms | 0.65 ms | 401.55 ms | 4560.34 ms | 7367.44 ms | 772581.33 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 222.70 ms | 0.68 ms | 457.03 ms | 4686.66 ms | 7944.55 ms | 790804.33 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 208.10 ms | 0.69 ms | 422.63 ms | 4491.53 ms | 7859.53 ms | 761006.67 | 
| php (7.2) | [symfony](http://symfony.com) (4.2) | 172.51 ms | 0.70 ms | 364.53 ms | 3814.68 ms | 7954.49 ms | 662796.00 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 7.27 ms | 1.24 ms | 22.03 ms | 61.40 ms | 174.77 ms | 12871.33 | 
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 151.58 ms | 1.63 ms | 347.34 ms | 3150.95 ms | 7283.22 ms | 547084.67 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 6.14 ms | 1.69 ms | 17.86 ms | 48.79 ms | 155.89 ms | 10258.67 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 12.15 ms | 1.99 ms | 37.36 ms | 97.84 ms | 277.01 ms | 20840.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 102.31 ms | 2.52 ms | 6.62 ms | 3100.22 ms | 6597.31 ms | 539370.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 3.52 ms | 2.81 ms | 6.27 ms | 11.90 ms | 94.64 ms | 2878.00 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.97 ms | 3.18 ms | 8.06 ms | 17.36 ms | 37.55 ms | 3614.67 | 
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 4.08 ms | 3.40 ms | 7.65 ms | 15.58 ms | 79.03 ms | 3215.33 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 13.29 ms | 4.45 ms | 38.73 ms | 91.24 ms | 213.28 ms | 19810.33 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 5.52 ms | 4.52 ms | 11.13 ms | 19.84 ms | 48.47 ms | 4259.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.44 ms | 4.73 ms | 6.01 ms | 11.32 ms | 35.40 ms | 2065.67 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 10.94 ms | 5.05 ms | 29.79 ms | 66.78 ms | 179.77 ms | 14644.33 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.33 ms | 5.11 ms | 8.04 ms | 16.42 ms | 158.47 ms | 3517.33 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 6.15 ms | 5.29 ms | 11.03 ms | 20.59 ms | 44.85 ms | 3954.33 | 
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 5.71 ms | 5.39 ms | 8.35 ms | 15.96 ms | 158.09 ms | 4407.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.69 ms | 5.47 ms | 9.29 ms | 15.99 ms | 51.62 ms | 3048.33 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 7.78 ms | 5.57 ms | 13.96 ms | 46.78 ms | 126.84 ms | 8971.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 66.82 ms | 6.85 ms | 18.28 ms | 1208.21 ms | 1642.39 ms | 225810.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 8.14 ms | 7.55 ms | 11.39 ms | 22.18 ms | 220.99 ms | 5194.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 8.53 ms | 7.89 ms | 13.55 ms | 25.12 ms | 251.82 ms | 6815.67 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 9.06 ms | 8.49 ms | 13.22 ms | 31.60 ms | 133.69 ms | 5207.67 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 10.16 ms | 9.46 ms | 15.18 ms | 29.63 ms | 190.62 ms | 5172.67 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 11.42 ms | 9.63 ms | 16.23 ms | 37.93 ms | 468.63 ms | 16408.67 | 
| go (1.11) | [iris](http://iris-go.com) (11.1) | 10.78 ms | 9.81 ms | 15.91 ms | 32.64 ms | 126.22 ms | 5928.33 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 11.05 ms | 9.85 ms | 16.94 ms | 34.66 ms | 132.44 ms | 6222.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 221.15 ms | 9.93 ms | 126.83 ms | 4517.62 ms | 7867.25 ms | 824293.67 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 11.57 ms | 9.97 ms | 17.43 ms | 39.28 ms | 217.33 ms | 9258.00 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 11.57 ms | 10.17 ms | 17.77 ms | 36.80 ms | 113.82 ms | 6439.33 | 
| go (1.11) | [beego](http://beego.me) (1.11) | 11.57 ms | 10.28 ms | 17.24 ms | 35.37 ms | 173.21 ms | 6411.33 | 
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 17.88 ms | 11.98 ms | 24.96 ms | 154.76 ms | 737.06 ms | 35786.33 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 15.09 ms | 12.74 ms | 25.72 ms | 46.18 ms | 183.47 ms | 9029.33 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 53.43 ms | 13.08 ms | 168.77 ms | 400.60 ms | 929.40 ms | 88351.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 14.12 ms | 14.03 ms | 17.07 ms | 21.26 ms | 72.66 ms | 2493.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 16.63 ms | 14.18 ms | 26.54 ms | 48.02 ms | 207.53 ms | 9307.33 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 20.50 ms | 14.70 ms | 27.60 ms | 131.81 ms | 807.31 ms | 37520.00 | 
| go (1.11) | [gf](http://gfer.me) (1.3) | 16.65 ms | 15.80 ms | 23.86 ms | 52.63 ms | 167.58 ms | 9114.67 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 22.98 ms | 16.66 ms | 28.99 ms | 168.32 ms | 1134.60 ms | 50191.67 | 
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 27.16 ms | 18.28 ms | 32.34 ms | 292.60 ms | 1073.47 ms | 59250.67 | 
| node (11.1) | [fastify](http://fastify.io) (1.13) | 35.67 ms | 19.70 ms | 34.92 ms | 517.23 ms | 1388.54 ms | 91271.00 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 34.13 ms | 19.87 ms | 40.31 ms | 487.33 ms | 1391.84 ms | 85785.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 27.99 ms | 24.12 ms | 48.54 ms | 133.96 ms | 289.45 ms | 24731.33 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 43.07 ms | 27.58 ms | 48.58 ms | 562.62 ms | 1507.61 ms | 95405.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 32.26 ms | 28.63 ms | 56.56 ms | 89.91 ms | 187.54 ms | 17817.67 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 35.58 ms | 29.36 ms | 44.63 ms | 190.60 ms | 752.61 ms | 41079.00 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 53.34 ms | 32.72 ms | 58.68 ms | 725.32 ms | 1645.74 ms | 117709.33 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 42.32 ms | 33.99 ms | 65.68 ms | 172.62 ms | 872.32 ms | 41319.00 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 40.01 ms | 34.28 ms | 50.96 ms | 159.77 ms | 413.72 ms | 25141.67 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 49.28 ms | 38.01 ms | 83.03 ms | 213.51 ms | 441.66 ms | 38719.33 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 40.21 ms | 38.57 ms | 58.34 ms | 79.83 ms | 344.58 ms | 16643.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 48.57 ms | 41.24 ms | 74.86 ms | 121.57 ms | 611.14 ms | 26728.00 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 42.33 ms | 42.04 ms | 52.94 ms | 122.19 ms | 332.40 ms | 19960.33 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 51.75 ms | 43.03 ms | 64.65 ms | 258.79 ms | 439.92 ms | 36507.00 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 49.26 ms | 48.54 ms | 60.29 ms | 68.31 ms | 149.11 ms | 8753.33 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 107.13 ms | 52.36 ms | 88.42 ms | 1642.89 ms | 2724.28 ms | 263390.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 84.17 ms | 60.01 ms | 183.49 ms | 313.00 ms | 537.04 ms | 69461.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 75.17 ms | 65.02 ms | 126.10 ms | 176.41 ms | 539.13 ms | 33560.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 94.72 ms | 97.91 ms | 146.58 ms | 206.86 ms | 312.30 ms | 43858.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 115.33 ms | 112.97 ms | 145.69 ms | 182.50 ms | 677.70 ms | 31076.00 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.3) | 272081.67 | 157.38 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 255059.00 | 305.30 MB |
| rust (1.31) | [actix-web](http://actix.rs) (0.7) | 230522.00 | 261.99 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 200582.33 | 194.53 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 191665.00 | 217.60 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 183563.33 | 368.89 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 175989.67 | 282.74 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.2) | 161738.33 | 173.80 MB |
| rust (1.31) | [gotham](http://gotham.rs) (0.3) | 161583.00 | 330.98 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 145650.33 | 84.20 MB |
| java (8) | [act](http://actframework.org) (1.8) | 142580.00 | 278.16 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 116577.00 | 190.01 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 111262.67 | 195.27 MB |
| rust (1.31) | [iron](http://ironframework.io) (0.6) | 99430.00 | 126.16 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 96197.67 | 127.80 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 92726.33 | 123.38 MB |
| go (1.11) | [iris](http://iris-go.com) (11.1) | 92282.33 | 123.21 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 89790.00 | 157.68 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 88685.00 | 155.66 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 86930.00 | 116.81 MB |
| go (1.11) | [beego](http://beego.me) (1.11) | 86535.67 | 116.52 MB |
| rust (1.31) | [nickel](http://nickel-org.github.io) (0.10) | 79645.67 | 158.65 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 70897.33 | 106.23 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 70260.00 | 66.07 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 66628.67 | 171.31 MB |
| go (1.11) | [gf](http://gfer.me) (1.3) | 60937.00 | 92.34 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 60425.33 | 148.82 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 58544.00 | 87.64 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 54048.33 | 116.08 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 52114.67 | 87.18 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 49186.33 | 114.17 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 48117.67 | 101.11 MB |
| c (99) | [kore](http://kore.io) (3.1) | 47163.00 | 127.93 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 44690.33 | 66.89 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 39669.33 | 69.46 MB |
| php (7.2) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.0) | 34389.00 | 170.82 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 32380.33 | 68.43 MB |
| php (7.2) | [zend-framework](http://framework.zend.com) (3.0) | 31867.00 | 158.10 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 31573.00 | 71.60 MB |
| php (7.2) | [symfony](http://symfony.com) (4.2) | 31460.67 | 156.00 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 31454.67 | 163.17 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 31305.33 | 50.54 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 30750.33 | 152.48 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 30629.67 | 28.73 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 28760.67 | 149.64 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 27230.33 | 66.55 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 25996.33 | 24.38 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 25679.67 | 44.96 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 25089.67 | 46.55 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 24187.33 | 39.45 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 23489.67 | 28.93 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.14) | 20838.00 | 19.88 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 20508.67 | 50.56 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 20477.67 | 33.39 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 20080.33 | 36.68 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 17626.00 | 10.17 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 17470.00 | 45.20 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 13448.33 | 23.98 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 13156.67 | 38.22 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 11711.00 | 6.76 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 10549.33 | 79.80 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 10496.00 | 20.94 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 9591.00 | 24.89 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 8433.67 | 22.58 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 2396.33 | 7.30 MB |
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

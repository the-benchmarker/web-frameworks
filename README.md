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
Last update: 2019-05-26
```
OS: Linux (version: 5.0.16-300.fc30.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: iron (rust)


:five: zend-framework (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.35) | [nickel](http://nickel-org.github.io) (0.11) | 0.10 ms | 0.10 ms | 0.15 ms | 0.20 ms | 2.97 ms | 43.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 4.88 ms | 0.29 ms | 17.13 ms | 36.03 ms | 87.10 ms | 8492.67 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 6.14 ms | 0.34 ms | 22.06 ms | 44.54 ms | 103.62 ms | 10730.33 | 
| rust (1.35) | [iron](http://ironframework.io) (0.6) | 0.50 ms | 0.48 ms | 0.86 ms | 1.27 ms | 13.47 ms | 294.00 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 194.96 ms | 0.56 ms | 366.87 ms | 4277.95 ms | 7546.52 ms | 708537.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 8.69 ms | 0.56 ms | 27.79 ms | 54.74 ms | 116.32 ms | 13256.00 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 9.79 ms | 0.60 ms | 31.17 ms | 60.18 ms | 156.38 ms | 14931.33 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 144.74 ms | 0.62 ms | 312.59 ms | 3055.73 ms | 7588.02 ms | 552523.33 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 179.78 ms | 0.63 ms | 355.96 ms | 3885.20 ms | 7918.89 ms | 690356.00 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 215.58 ms | 0.63 ms | 418.08 ms | 4426.08 ms | 7907.20 ms | 753831.00 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 191.62 ms | 0.65 ms | 430.51 ms | 3683.47 ms | 7561.43 ms | 643542.33 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 159.01 ms | 0.65 ms | 329.84 ms | 3549.41 ms | 7591.59 ms | 594202.00 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 11.20 ms | 0.83 ms | 33.58 ms | 64.63 ms | 163.70 ms | 15906.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 146.49 ms | 2.32 ms | 5.37 ms | 4714.90 ms | 6595.98 ms | 731522.33 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 3.71 ms | 2.67 ms | 7.76 ms | 16.10 ms | 39.09 ms | 3480.00 | 
| rust (1.35) | [actix-web](http://actix.rs) (0.7) | 3.97 ms | 3.18 ms | 7.81 ms | 16.15 ms | 98.12 ms | 3676.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 13.57 ms | 3.62 ms | 8.62 ms | 348.33 ms | 1450.62 ms | 81696.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 4.68 ms | 4.40 ms | 9.37 ms | 18.39 ms | 40.38 ms | 3923.33 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.35 ms | 4.71 ms | 6.03 ms | 11.02 ms | 37.81 ms | 2081.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 8.83 ms | 4.95 ms | 15.39 ms | 87.04 ms | 135.08 ms | 14297.33 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 5.23 ms | 4.99 ms | 7.83 ms | 16.59 ms | 105.45 ms | 2965.00 | 
| rust (1.35) | [gotham](http://gotham.rs) (0.3) | 5.75 ms | 5.11 ms | 10.30 ms | 21.03 ms | 175.11 ms | 6501.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.28 ms | 5.12 ms | 8.15 ms | 15.23 ms | 56.84 ms | 2927.67 | 
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 5.69 ms | 5.32 ms | 9.15 ms | 16.27 ms | 37.59 ms | 2889.00 | 
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 5.74 ms | 5.34 ms | 9.26 ms | 16.66 ms | 38.56 ms | 2956.67 | 
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 6.13 ms | 5.34 ms | 10.40 ms | 19.07 ms | 36.36 ms | 3470.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 6.27 ms | 5.35 ms | 12.80 ms | 21.17 ms | 50.13 ms | 4674.67 | 
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 5.92 ms | 5.44 ms | 9.41 ms | 15.43 ms | 38.89 ms | 2730.67 | 
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 6.25 ms | 5.61 ms | 9.81 ms | 15.42 ms | 47.78 ms | 2713.33 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 42.29 ms | 5.72 ms | 137.13 ms | 344.75 ms | 897.13 ms | 74370.67 | 
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 7.21 ms | 6.36 ms | 10.87 ms | 17.89 ms | 47.47 ms | 3200.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 7.59 ms | 6.70 ms | 10.50 ms | 20.40 ms | 286.40 ms | 8329.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 7.83 ms | 7.35 ms | 12.45 ms | 23.35 ms | 165.27 ms | 5319.67 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 9.83 ms | 7.40 ms | 19.49 ms | 41.73 ms | 186.12 ms | 8241.67 | 
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 10.57 ms | 7.70 ms | 21.69 ms | 47.68 ms | 201.30 ms | 9655.00 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 10.48 ms | 7.75 ms | 21.36 ms | 45.78 ms | 142.05 ms | 8997.00 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 11.26 ms | 7.88 ms | 24.44 ms | 50.75 ms | 146.72 ms | 10278.33 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 11.27 ms | 8.14 ms | 23.14 ms | 50.15 ms | 254.92 ms | 11732.67 | 
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 9.70 ms | 8.31 ms | 18.57 ms | 37.44 ms | 378.58 ms | 11598.00 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 10.14 ms | 8.33 ms | 18.02 ms | 39.86 ms | 177.91 ms | 7516.33 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 10.96 ms | 8.38 ms | 21.92 ms | 47.24 ms | 176.57 ms | 9325.00 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 10.64 ms | 8.82 ms | 18.34 ms | 41.82 ms | 212.15 ms | 8869.00 | 
| node (12.3) | [restana](http://github.com/jkyberneees/ana) (3.0) | 14.51 ms | 9.33 ms | 19.11 ms | 153.15 ms | 659.39 ms | 35140.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 209.97 ms | 9.52 ms | 79.75 ms | 4423.26 ms | 7208.81 ms | 790338.67 | 
| go (1.12) | [gf](http://goframe.org) (1.6) | 14.09 ms | 10.12 ms | 28.10 ms | 64.28 ms | 352.83 ms | 14772.67 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 11.04 ms | 10.95 ms | 13.50 ms | 15.89 ms | 92.88 ms | 3542.67 | 
| node (12.3) | [polka](http://github.com/lukeed/polka) (0.5) | 18.59 ms | 11.15 ms | 21.17 ms | 267.70 ms | 794.57 ms | 47189.67 | 
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 13.70 ms | 11.95 ms | 21.93 ms | 42.70 ms | 296.37 ms | 9776.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 17.57 ms | 13.85 ms | 32.39 ms | 64.04 ms | 234.86 ms | 12781.67 | 
| node (12.3) | [rayo](http://rayo.js.org) (1.3) | 20.75 ms | 14.05 ms | 25.79 ms | 215.54 ms | 804.32 ms | 42310.67 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 19.09 ms | 14.45 ms | 25.26 ms | 79.40 ms | 948.58 ms | 38046.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 16.99 ms | 14.50 ms | 29.63 ms | 53.28 ms | 664.13 ms | 22074.33 | 
| node (12.3) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 23.89 ms | 14.97 ms | 31.13 ms | 284.71 ms | 809.31 ms | 46355.00 | 
| node (12.3) | [foxify](http://foxify.js.org) (0.10) | 23.41 ms | 15.37 ms | 27.85 ms | 282.60 ms | 866.35 ms | 48470.33 | 
| node (12.3) | [fastify](http://fastify.io) (2.3) | 24.51 ms | 17.52 ms | 30.31 ms | 251.06 ms | 822.43 ms | 44205.00 | 
| python (3.7) | [hug](http://hug.rest) (2.5) | 24.74 ms | 18.54 ms | 49.14 ms | 75.46 ms | 236.85 ms | 16937.67 | 
| node (12.3) | [express](http://expressjs.com) (4.16) | 30.43 ms | 18.90 ms | 32.47 ms | 394.51 ms | 1161.59 ms | 68853.67 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 23.19 ms | 20.82 ms | 38.12 ms | 62.03 ms | 163.90 ms | 12159.67 | 
| node (12.3) | [restify](http://restify.com) (8.2) | 26.80 ms | 21.04 ms | 32.73 ms | 210.69 ms | 661.40 ms | 36651.33 | 
| node (12.3) | [koa](http://koajs.com) (2.7) | 30.35 ms | 22.06 ms | 38.73 ms | 265.47 ms | 957.36 ms | 51048.33 | 
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 28.26 ms | 25.60 ms | 40.11 ms | 57.13 ms | 617.22 ms | 19882.33 | 
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 46.08 ms | 28.80 ms | 51.30 ms | 597.90 ms | 2273.44 ms | 117888.67 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.23) | 32.72 ms | 30.21 ms | 52.11 ms | 73.15 ms | 141.11 ms | 14282.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 51.81 ms | 35.20 ms | 115.40 ms | 157.60 ms | 551.95 ms | 40421.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 36.52 ms | 35.22 ms | 60.61 ms | 80.20 ms | 171.00 ms | 17253.00 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 46.40 ms | 39.13 ms | 83.39 ms | 123.84 ms | 393.25 ms | 26937.33 | 
| node (12.3) | [hapi](http://hapijs.com) (18.1) | 76.48 ms | 39.85 ms | 63.44 ms | 1147.36 ms | 2195.49 ms | 186632.00 | 
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 44.15 ms | 40.34 ms | 54.46 ms | 108.26 ms | 284.50 ms | 14205.00 | 
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 51.60 ms | 43.32 ms | 118.23 ms | 280.14 ms | 731.19 ms | 59850.67 | 
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 49.89 ms | 49.79 ms | 57.07 ms | 91.83 ms | 164.17 ms | 11240.00 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 56.05 ms | 50.64 ms | 99.68 ms | 127.76 ms | 220.83 ms | 30558.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 82.31 ms | 66.44 ms | 158.52 ms | 270.38 ms | 447.61 ms | 54938.67 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 86.44 ms | 84.85 ms | 147.22 ms | 201.59 ms | 267.91 ms | 44899.00 | 
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 102.78 ms | 99.67 ms | 146.61 ms | 195.06 ms | 287.10 ms | 33148.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 120.41 ms | 100.69 ms | 225.67 ms | 288.28 ms | 665.94 ms | 59845.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 114.14 ms | 112.43 ms | 147.44 ms | 179.49 ms | 439.77 ms | 28856.00 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 119.14 ms | 112.50 ms | 179.05 ms | 222.18 ms | 334.15 ms | 41470.00 | 
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 133.98 ms | 126.30 ms | 194.34 ms | 272.82 ms | 949.29 ms | 52679.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (actix-web) (rust)


:three: (kore) (c)


:four: (japronto) (python)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 280021.00 | 161.92 MB |
| rust (1.35) | [actix-web](http://actix.rs) (0.7) | 240841.00 | 273.83 MB |
| c (99) | [kore](http://kore.io) (3.1) | 212693.67 | 552.99 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 210806.33 | 252.29 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 204544.00 | 198.67 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 193436.00 | 388.84 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 179335.33 | 288.98 MB |
| rust (1.35) | [gotham](http://gotham.rs) (0.3) | 173942.67 | 356.19 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 166873.00 | 189.46 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 165608.00 | 95.74 MB |
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 165067.00 | 155.32 MB |
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 164380.33 | 154.66 MB |
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 159160.33 | 169.98 MB |
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 157891.33 | 258.14 MB |
| java (8) | [act](http://actframework.org) (1.8) | 156441.33 | 270.04 MB |
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 150352.00 | 187.48 MB |
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 133197.33 | 217.83 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 127004.33 | 206.60 MB |
| rust (1.35) | [iron](http://ironframework.io) (0.6) | 117449.33 | 147.80 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 109165.00 | 146.38 MB |
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 108369.67 | 168.67 MB |
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 105862.00 | 140.14 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 105349.67 | 184.91 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 102951.67 | 136.66 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 101422.33 | 135.21 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 100460.67 | 135.30 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 99798.00 | 174.71 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 99176.67 | 132.50 MB |
| node (12.3) | [restana](http://github.com/jkyberneees/ana) (3.0) | 91142.67 | 136.64 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 88541.67 | 83.30 MB |
| go (1.12) | [gf](http://goframe.org) (1.6) | 80380.33 | 121.57 MB |
| rust (1.35) | [nickel](http://nickel-org.github.io) (0.11) | 79070.67 | 157.33 MB |
| node (12.3) | [polka](http://github.com/lukeed/polka) (0.5) | 77881.33 | 116.69 MB |
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 73649.33 | 172.71 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 67464.00 | 118.23 MB |
| node (12.3) | [rayo](http://rayo.js.org) (1.3) | 62919.33 | 94.31 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 60737.67 | 149.76 MB |
| node (12.3) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 60136.67 | 90.08 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 59518.00 | 99.62 MB |
| node (12.3) | [foxify](http://foxify.js.org) (0.10) | 58727.67 | 123.32 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 58683.00 | 125.84 MB |
| node (12.3) | [fastify](http://fastify.io) (2.3) | 56790.67 | 142.11 MB |
| node (12.3) | [express](http://expressjs.com) (4.16) | 47080.67 | 115.16 MB |
| node (12.3) | [restify](http://restify.com) (8.2) | 43833.33 | 76.88 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 43224.67 | 93.28 MB |
| python (3.7) | [hug](http://hug.rest) (2.5) | 43057.33 | 106.78 MB |
| node (12.3) | [koa](http://koajs.com) (2.7) | 41085.00 | 86.78 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 40560.67 | 64.59 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 37540.67 | 186.19 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 35850.33 | 66.48 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 34880.00 | 172.93 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 34787.00 | 172.74 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 34741.67 | 172.33 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 33667.67 | 174.66 MB |
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 31012.67 | 58.47 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.23) | 30961.67 | 66.98 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 29961.00 | 156.07 MB |
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 27592.67 | 35.82 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 27554.00 | 62.47 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 26206.33 | 24.98 MB |
| node (12.3) | [hapi](http://hapijs.com) (18.1) | 23769.67 | 61.58 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 22977.33 | 42.74 MB |
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 22666.67 | 27.82 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 21472.00 | 52.92 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 20782.33 | 11.99 MB |
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 20044.33 | 36.60 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 18135.67 | 35.00 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 14706.00 | 8.48 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 13068.00 | 98.78 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 12812.00 | 22.86 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 11570.67 | 23.05 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 11388.33 | 29.55 MB |
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 9669.67 | 24.85 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 8463.33 | 25.05 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 8388.67 | 24.32 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 8285.00 | 18.06 MB |
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 7260.00 | 17.88 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3023.00 | 9.25 MB |
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

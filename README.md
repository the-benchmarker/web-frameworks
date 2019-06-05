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

+ Make framework list

~~~sh
bin/make config
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
Last update: 2019-06-06
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


:five: symfony (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.35) | [nickel](http://nickel-org.github.io) (0.11) | 0.07 ms | 0.06 ms | 0.10 ms | 0.12 ms | 0.99 ms | 24.33 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 3.50 ms | 0.17 ms | 13.67 ms | 32.96 ms | 82.73 ms | 7372.33 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.66 ms | 0.22 ms | 18.59 ms | 41.10 ms | 107.99 ms | 9556.00 | 
| rust (1.35) | [iron](http://ironframework.io) (0.6) | 0.36 ms | 0.36 ms | 0.60 ms | 0.87 ms | 11.31 ms | 205.00 | 
| php (7.3) | [symfony](http://symfony.com) (4.3) | 93.23 ms | 0.37 ms | 216.72 ms | 1956.34 ms | 6787.76 ms | 368518.33 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 154.07 ms | 0.39 ms | 330.01 ms | 3101.21 ms | 6937.61 ms | 549852.00 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 127.39 ms | 0.39 ms | 266.58 ms | 2402.54 ms | 6879.72 ms | 458913.00 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 170.55 ms | 0.39 ms | 304.31 ms | 3452.25 ms | 6962.37 ms | 626142.00 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 7.22 ms | 0.39 ms | 25.05 ms | 52.38 ms | 138.96 ms | 12378.00 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 141.59 ms | 0.39 ms | 279.21 ms | 3045.51 ms | 6945.63 ms | 525800.67 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 8.65 ms | 0.44 ms | 31.61 ms | 65.34 ms | 153.17 ms | 15576.33 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 9.74 ms | 0.60 ms | 31.24 ms | 62.36 ms | 155.40 ms | 15113.00 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 138.01 ms | 0.98 ms | 304.25 ms | 2788.32 ms | 5897.79 ms | 484750.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 106.24 ms | 1.58 ms | 71.40 ms | 2257.22 ms | 4945.40 ms | 441283.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 2.96 ms | 1.90 ms | 6.70 ms | 14.38 ms | 105.26 ms | 3145.33 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.25 ms | 2.30 ms | 6.97 ms | 15.11 ms | 33.41 ms | 3223.33 | 
| rust (1.35) | [actix-web](http://actix.rs) (0.7) | 3.25 ms | 2.37 ms | 6.98 ms | 15.22 ms | 32.82 ms | 3150.00 | 
| c (99) | [kore](http://kore.io) (3.1) | 7.38 ms | 2.91 ms | 7.07 ms | 122.88 ms | 1275.35 ms | 42497.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.01 ms | 3.20 ms | 8.08 ms | 15.23 ms | 38.19 ms | 3328.33 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.91 ms | 3.38 ms | 6.15 ms | 12.48 ms | 104.53 ms | 2782.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.49 ms | 3.61 ms | 5.42 ms | 8.19 ms | 22.40 ms | 1811.67 | 
| rust (1.35) | [gotham](http://gotham.rs) (0.3) | 4.53 ms | 3.85 ms | 8.72 ms | 19.31 ms | 113.75 ms | 4455.33 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.64 ms | 4.05 ms | 7.47 ms | 14.14 ms | 47.45 ms | 2763.33 | 
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 4.19 ms | 4.07 ms | 6.88 ms | 13.25 ms | 30.63 ms | 2536.33 | 
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 4.32 ms | 4.19 ms | 7.00 ms | 13.29 ms | 30.81 ms | 2493.67 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 39.97 ms | 4.23 ms | 132.40 ms | 348.79 ms | 928.44 ms | 74600.67 | 
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 4.39 ms | 4.28 ms | 7.07 ms | 13.86 ms | 31.27 ms | 2544.00 | 
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 4.57 ms | 4.39 ms | 7.23 ms | 14.00 ms | 56.36 ms | 2914.33 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 6.33 ms | 4.40 ms | 10.50 ms | 58.95 ms | 122.92 ms | 9687.00 | 
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 5.01 ms | 4.50 ms | 8.98 ms | 16.00 ms | 32.88 ms | 3116.67 | 
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 5.44 ms | 4.88 ms | 9.03 ms | 14.35 ms | 38.35 ms | 2693.33 | 
| node (12.4) | [restana](http://github.com/jkyberneees/ana) (3.1) | 6.89 ms | 5.05 ms | 9.93 ms | 21.83 ms | 374.66 ms | 11585.00 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 7.08 ms | 5.05 ms | 14.18 ms | 29.78 ms | 279.80 ms | 7860.33 | 
| node (12.4) | [0http](http://github.com/jkyberneees/0http) (1.0) | 6.30 ms | 5.09 ms | 9.77 ms | 18.04 ms | 230.08 ms | 5428.00 | 
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 7.64 ms | 5.27 ms | 16.07 ms | 34.98 ms | 137.63 ms | 7193.67 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 8.00 ms | 5.31 ms | 16.56 ms | 38.49 ms | 299.12 ms | 9166.33 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 7.25 ms | 5.41 ms | 13.55 ms | 29.35 ms | 168.69 ms | 6556.33 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 8.36 ms | 5.49 ms | 18.02 ms | 38.07 ms | 238.00 ms | 8881.67 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 8.22 ms | 5.54 ms | 16.92 ms | 35.81 ms | 202.16 ms | 9458.33 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 7.79 ms | 5.57 ms | 15.59 ms | 32.85 ms | 216.84 ms | 7221.00 | 
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 7.53 ms | 5.61 ms | 13.34 ms | 23.08 ms | 102.47 ms | 5246.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 6.71 ms | 5.61 ms | 9.42 ms | 18.89 ms | 358.75 ms | 10077.67 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 7.53 ms | 5.72 ms | 14.23 ms | 30.41 ms | 143.04 ms | 5985.00 | 
| node (12.4) | [rayo](http://rayo.js.org) (1.3) | 7.48 ms | 5.81 ms | 11.02 ms | 22.25 ms | 313.55 ms | 9833.33 | 
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 7.57 ms | 5.88 ms | 14.87 ms | 32.19 ms | 200.52 ms | 7380.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.88 ms | 6.02 ms | 11.11 ms | 20.88 ms | 215.30 ms | 5603.00 | 
| node (12.4) | [polka](http://github.com/lukeed/polka) (0.5) | 8.04 ms | 6.44 ms | 11.74 ms | 24.21 ms | 368.76 ms | 12719.33 | 
| go (1.12) | [gf](http://goframe.org) (1.6) | 9.96 ms | 7.09 ms | 20.43 ms | 43.69 ms | 222.24 ms | 9172.33 | 
| node (12.4) | [fastify](http://fastify.io) (2.4) | 9.58 ms | 7.87 ms | 14.26 ms | 26.04 ms | 313.43 ms | 9309.67 | 
| node (12.4) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 13.46 ms | 8.01 ms | 17.15 ms | 163.45 ms | 789.32 ms | 39689.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 197.34 ms | 8.03 ms | 91.60 ms | 4489.79 ms | 7142.36 ms | 775517.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 10.32 ms | 8.43 ms | 18.60 ms | 35.62 ms | 215.77 ms | 8232.67 | 
| node (12.4) | [foxify](http://foxify.js.org) (0.10) | 9.94 ms | 8.59 ms | 14.29 ms | 32.05 ms | 421.70 ms | 15312.00 | 
| node (12.4) | [koa](http://koajs.com) (2.7) | 10.46 ms | 8.62 ms | 14.22 ms | 49.44 ms | 491.86 ms | 18371.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 9.61 ms | 9.78 ms | 11.69 ms | 13.91 ms | 26.41 ms | 1834.67 | 
| node (12.4) | [restify](http://restify.com) (8.2) | 11.36 ms | 10.02 ms | 15.08 ms | 30.75 ms | 333.64 ms | 10657.33 | 
| node (12.4) | [express](http://expressjs.com) (4.16) | 14.47 ms | 10.58 ms | 18.62 ms | 110.48 ms | 612.02 ms | 27784.00 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 16.82 ms | 11.20 ms | 20.55 ms | 136.30 ms | 1068.18 ms | 48076.33 | 
| python (3.7) | [hug](http://hug.rest) (2.5) | 13.07 ms | 11.77 ms | 20.65 ms | 36.45 ms | 222.22 ms | 8276.67 | 
| python (3.7) | [starlette](http://starlette.io) (0.12) | 12.29 ms | 11.80 ms | 19.76 ms | 29.16 ms | 109.56 ms | 6141.67 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 14.15 ms | 12.59 ms | 25.72 ms | 46.18 ms | 296.52 ms | 11660.33 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.27) | 19.60 ms | 16.80 ms | 34.16 ms | 49.52 ms | 89.97 ms | 10293.00 | 
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 22.20 ms | 19.07 ms | 33.75 ms | 48.64 ms | 547.01 ms | 17006.00 | 
| node (12.4) | [hapi](http://hapijs.com) (18.1) | 28.55 ms | 20.27 ms | 32.51 ms | 313.14 ms | 1089.79 ms | 60343.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 20.93 ms | 21.59 ms | 32.50 ms | 44.77 ms | 71.49 ms | 9195.00 | 
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 31.09 ms | 21.73 ms | 37.11 ms | 305.97 ms | 1440.98 ms | 75018.67 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 30.60 ms | 21.90 ms | 69.19 ms | 99.02 ms | 233.03 ms | 22095.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 31.97 ms | 25.86 ms | 59.89 ms | 86.97 ms | 332.71 ms | 18718.00 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.16) | 33.79 ms | 26.61 ms | 66.21 ms | 108.84 ms | 203.38 ms | 22170.33 | 
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 30.76 ms | 27.49 ms | 41.19 ms | 61.21 ms | 384.51 ms | 14449.67 | 
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 31.23 ms | 27.65 ms | 40.73 ms | 65.90 ms | 319.10 ms | 13052.00 | 
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 41.01 ms | 31.78 ms | 96.53 ms | 217.95 ms | 553.99 ms | 47163.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 46.21 ms | 39.15 ms | 82.78 ms | 149.81 ms | 298.08 ms | 28384.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 54.54 ms | 46.56 ms | 107.77 ms | 155.41 ms | 237.25 ms | 32270.00 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 75.37 ms | 71.14 ms | 125.37 ms | 164.66 ms | 210.72 ms | 34616.00 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 83.38 ms | 71.79 ms | 131.29 ms | 237.72 ms | 1008.85 ms | 49926.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 78.67 ms | 77.26 ms | 103.96 ms | 129.64 ms | 860.25 ms | 27344.00 | 
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 100.33 ms | 89.24 ms | 164.03 ms | 240.07 ms | 736.13 ms | 45648.33 | 
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 102.41 ms | 99.89 ms | 141.31 ms | 185.54 ms | 280.33 ms | 30431.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (kore) (c)


:five: (vibora) (python)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 370570.33 | 214.46 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 323139.33 | 386.82 MB |
| rust (1.35) | [actix-web](http://actix.rs) (0.7) | 315434.67 | 358.33 MB |
| c (99) | [kore](http://kore.io) (3.1) | 274434.33 | 712.27 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 266435.00 | 302.22 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 253744.33 | 246.35 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 233803.67 | 375.92 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 229411.33 | 460.62 MB |
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 224831.33 | 211.27 MB |
| rust (1.35) | [gotham](http://gotham.rs) (0.3) | 223388.67 | 456.99 MB |
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 218337.00 | 205.38 MB |
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 215602.00 | 352.07 MB |
| java (8) | [act](http://actframework.org) (1.8) | 209010.00 | 361.02 MB |
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 207693.67 | 258.89 MB |
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 195298.33 | 208.90 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 195106.67 | 112.89 MB |
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 173986.33 | 284.34 MB |
| rust (1.35) | [iron](http://ironframework.io) (0.6) | 164712.67 | 207.93 MB |
| node (12.4) | [restana](http://github.com/jkyberneees/ana) (3.1) | 154131.33 | 230.96 MB |
| node (12.4) | [0http](http://github.com/jkyberneees/0http) (1.0) | 152759.67 | 228.99 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 151584.67 | 202.82 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 148854.00 | 242.24 MB |
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 145514.33 | 194.05 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 143727.33 | 190.74 MB |
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 141697.33 | 220.94 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 140492.33 | 246.39 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 140035.33 | 185.36 MB |
| node (12.4) | [rayo](http://rayo.js.org) (1.3) | 136768.00 | 204.94 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 136345.00 | 183.17 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 136196.00 | 238.71 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 135440.67 | 181.30 MB |
| node (12.4) | [polka](http://github.com/lukeed/polka) (0.5) | 133184.67 | 199.56 MB |
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 132408.67 | 310.35 MB |
| rust (1.35) | [nickel](http://nickel-org.github.io) (0.11) | 124742.67 | 248.27 MB |
| node (12.4) | [fastify](http://fastify.io) (2.4) | 115053.00 | 291.04 MB |
| go (1.12) | [gf](http://goframe.org) (1.6) | 110617.67 | 167.06 MB |
| node (12.4) | [foxify](http://foxify.js.org) (0.10) | 109600.33 | 230.40 MB |
| node (12.4) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 109117.00 | 163.58 MB |
| node (12.4) | [koa](http://koajs.com) (2.7) | 107313.33 | 227.17 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 102450.67 | 96.32 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 101324.67 | 249.73 MB |
| node (12.4) | [restify](http://restify.com) (8.2) | 90508.33 | 158.74 MB |
| node (12.4) | [express](http://expressjs.com) (4.16) | 82460.00 | 201.65 MB |
| python (3.7) | [starlette](http://starlette.io) (0.12) | 80852.00 | 174.17 MB |
| python (3.7) | [hug](http://hug.rest) (2.5) | 76848.00 | 190.50 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 75890.67 | 127.56 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 75656.00 | 132.51 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 70629.00 | 151.71 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 59171.00 | 293.12 MB |
| php (7.3) | [symfony](http://symfony.com) (4.3) | 57843.33 | 287.07 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 57568.67 | 285.76 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 57179.67 | 283.73 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 55078.00 | 285.72 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.27) | 51792.33 | 111.85 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 49933.00 | 80.63 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 48240.00 | 251.10 MB |
| node (12.4) | [hapi](http://hapijs.com) (18.1) | 47743.67 | 124.38 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 47639.33 | 108.01 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 45341.67 | 84.13 MB |
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 42042.00 | 79.19 MB |
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 38092.00 | 48.89 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 36615.33 | 34.88 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 36204.67 | 67.32 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 32293.00 | 79.43 MB |
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 32208.33 | 58.83 MB |
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 32006.00 | 39.34 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.16) | 31393.00 | 60.62 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 27697.33 | 15.99 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 22442.33 | 39.96 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 19262.67 | 38.45 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 17665.00 | 10.20 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 14872.67 | 112.40 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 13198.67 | 28.75 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 13141.67 | 34.08 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 12176.33 | 35.86 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 12146.00 | 35.21 MB |
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 9915.00 | 22.49 MB |
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 9701.33 | 24.95 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3196.33 | 9.78 MB |
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

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
Last update: 2019-05-23
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
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 0.09 ms | 0.08 ms | 0.12 ms | 0.17 ms | 1.80 ms | 29.67 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 4.36 ms | 0.26 ms | 15.50 ms | 33.96 ms | 89.85 ms | 7866.00 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 5.90 ms | 0.35 ms | 20.25 ms | 41.07 ms | 97.37 ms | 9881.67 | 
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 0.50 ms | 0.49 ms | 0.82 ms | 1.26 ms | 14.37 ms | 284.33 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 134.03 ms | 0.54 ms | 300.27 ms | 2703.74 ms | 7312.49 ms | 483471.67 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 159.46 ms | 0.55 ms | 382.45 ms | 2835.09 ms | 7012.24 ms | 527478.33 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 8.94 ms | 0.55 ms | 29.20 ms | 56.64 ms | 166.81 ms | 14036.00 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 167.43 ms | 0.55 ms | 301.91 ms | 3856.79 ms | 6893.21 ms | 633574.00 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 190.83 ms | 0.56 ms | 332.03 ms | 4457.04 ms | 7835.04 ms | 734374.67 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 156.79 ms | 0.58 ms | 297.56 ms | 3467.03 ms | 6967.20 ms | 583002.67 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 8.86 ms | 0.63 ms | 27.62 ms | 53.62 ms | 104.86 ms | 13020.33 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 11.07 ms | 0.82 ms | 33.19 ms | 63.56 ms | 146.04 ms | 15708.33 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 190.14 ms | 1.34 ms | 368.56 ms | 3768.45 ms | 6798.76 ms | 655582.67 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 82.20 ms | 2.12 ms | 4.62 ms | 2762.09 ms | 6594.14 ms | 509156.00 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 3.26 ms | 2.21 ms | 6.89 ms | 14.69 ms | 35.37 ms | 3161.67 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.89 ms | 2.61 ms | 8.93 ms | 18.85 ms | 37.34 ms | 4053.67 | 
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 4.01 ms | 3.23 ms | 7.76 ms | 16.45 ms | 94.69 ms | 3722.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 5.37 ms | 3.27 ms | 7.77 ms | 25.56 ms | 483.23 ms | 18196.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.93 ms | 3.76 ms | 9.99 ms | 18.59 ms | 45.48 ms | 3976.00 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.98 ms | 4.26 ms | 5.68 ms | 10.33 ms | 37.05 ms | 1975.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 8.08 ms | 4.54 ms | 13.21 ms | 85.87 ms | 123.06 ms | 13856.67 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.73 ms | 4.55 ms | 7.25 ms | 15.01 ms | 56.11 ms | 2768.33 | 
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 4.73 ms | 4.57 ms | 7.53 ms | 14.62 ms | 33.21 ms | 2652.67 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.04 ms | 4.72 ms | 7.73 ms | 14.46 ms | 39.44 ms | 2707.67 | 
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 5.77 ms | 4.93 ms | 9.98 ms | 21.31 ms | 291.61 ms | 9066.33 | 
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 5.31 ms | 5.06 ms | 8.15 ms | 13.27 ms | 34.11 ms | 2393.33 | 
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 5.34 ms | 5.07 ms | 8.58 ms | 15.29 ms | 36.23 ms | 2780.67 | 
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 6.00 ms | 5.21 ms | 10.43 ms | 18.34 ms | 40.06 ms | 3468.67 | 
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 5.72 ms | 5.29 ms | 9.23 ms | 16.21 ms | 36.66 ms | 2871.33 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 40.56 ms | 5.46 ms | 131.39 ms | 326.36 ms | 850.58 ms | 71104.67 | 
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 6.72 ms | 5.81 ms | 10.55 ms | 17.22 ms | 44.64 ms | 3150.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 7.45 ms | 6.63 ms | 10.64 ms | 20.02 ms | 223.80 ms | 6288.67 | 
| node (12.2) | [restana](http://github.com/jkyberneees/ana) (3.0) | 8.08 ms | 6.65 ms | 11.71 ms | 24.44 ms | 338.07 ms | 10771.67 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 9.90 ms | 7.02 ms | 20.50 ms | 44.58 ms | 185.27 ms | 9258.67 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 9.74 ms | 7.10 ms | 19.66 ms | 42.33 ms | 221.98 ms | 8695.00 | 
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 10.26 ms | 7.11 ms | 21.86 ms | 47.82 ms | 179.26 ms | 9599.00 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 10.13 ms | 7.22 ms | 21.14 ms | 45.42 ms | 108.47 ms | 8895.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 8.32 ms | 7.45 ms | 13.31 ms | 26.83 ms | 198.86 ms | 7194.67 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 10.01 ms | 7.52 ms | 19.77 ms | 42.44 ms | 180.36 ms | 8514.33 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 9.69 ms | 7.73 ms | 17.50 ms | 38.55 ms | 184.33 ms | 8048.33 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 11.83 ms | 8.01 ms | 25.70 ms | 56.67 ms | 164.24 ms | 11662.00 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 9.79 ms | 8.10 ms | 17.14 ms | 38.57 ms | 89.72 ms | 6921.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 191.63 ms | 8.58 ms | 33.73 ms | 4374.25 ms | 7925.30 ms | 780317.00 | 
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 9.81 ms | 8.88 ms | 18.89 ms | 35.11 ms | 280.60 ms | 8869.33 | 
| go (1.12) | [gf](http://goframe.org) (1.6) | 12.23 ms | 8.97 ms | 25.07 ms | 55.35 ms | 213.20 ms | 11182.67 | 
| node (12.2) | [polka](http://github.com/lukeed/polka) (0.5) | 11.21 ms | 9.09 ms | 15.26 ms | 38.99 ms | 497.66 ms | 20229.67 | 
| node (12.2) | [rayo](http://rayo.js.org) (1.3) | 12.86 ms | 9.41 ms | 16.75 ms | 69.37 ms | 663.41 ms | 29533.33 | 
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 12.73 ms | 9.43 ms | 26.43 ms | 48.55 ms | 170.71 ms | 10023.33 | 
| node (12.2) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 18.03 ms | 10.73 ms | 21.03 ms | 242.87 ms | 974.85 ms | 52333.00 | 
| node (12.2) | [foxify](http://foxify.js.org) (0.10) | 15.80 ms | 10.97 ms | 19.69 ms | 130.00 ms | 701.71 ms | 34059.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 11.00 ms | 11.09 ms | 13.47 ms | 16.14 ms | 45.89 ms | 2127.67 | 
| node (12.2) | [fastify](http://fastify.io) (2.3) | 14.39 ms | 11.24 ms | 19.07 ms | 77.96 ms | 623.27 ms | 26308.33 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 14.12 ms | 11.96 ms | 23.48 ms | 42.51 ms | 216.93 ms | 8640.00 | 
| node (12.2) | [restify](http://restify.com) (8.2) | 14.98 ms | 13.13 ms | 19.37 ms | 39.03 ms | 363.96 ms | 12663.67 | 
| node (12.2) | [koa](http://koajs.com) (2.7) | 17.33 ms | 13.22 ms | 23.62 ms | 105.06 ms | 651.57 ms | 29547.00 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 20.01 ms | 13.51 ms | 24.29 ms | 168.21 ms | 1113.10 ms | 52222.33 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 16.43 ms | 13.80 ms | 28.06 ms | 47.16 ms | 115.54 ms | 9128.67 | 
| node (12.2) | [express](http://expressjs.com) (4.16) | 19.56 ms | 13.96 ms | 25.09 ms | 144.92 ms | 837.71 ms | 39098.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 17.10 ms | 15.24 ms | 30.71 ms | 53.09 ms | 361.48 ms | 12939.33 | 
| python (3.7) | [hug](http://hug.rest) (2.5) | 19.78 ms | 15.86 ms | 36.30 ms | 60.68 ms | 263.76 ms | 13307.33 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.22) | 24.06 ms | 21.12 ms | 41.19 ms | 67.39 ms | 147.47 ms | 12897.33 | 
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 26.84 ms | 22.97 ms | 34.47 ms | 90.66 ms | 1071.44 ms | 39288.00 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 28.35 ms | 24.34 ms | 53.07 ms | 79.25 ms | 119.75 ms | 17313.00 | 
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 60.53 ms | 27.33 ms | 51.97 ms | 1136.10 ms | 2966.76 ms | 201256.00 | 
| node (12.2) | [hapi](http://hapijs.com) (18.1) | 43.19 ms | 30.27 ms | 46.50 ms | 480.13 ms | 1306.76 ms | 81754.67 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 38.00 ms | 31.44 ms | 65.99 ms | 94.75 ms | 573.79 ms | 23708.67 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 42.22 ms | 33.25 ms | 87.71 ms | 121.25 ms | 267.19 ms | 26749.67 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 42.37 ms | 36.54 ms | 72.36 ms | 98.08 ms | 234.94 ms | 19841.33 | 
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 43.55 ms | 43.02 ms | 48.29 ms | 59.53 ms | 416.82 ms | 12245.33 | 
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 56.87 ms | 45.59 ms | 138.61 ms | 285.88 ms | 663.86 ms | 65869.67 | 
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 46.65 ms | 47.26 ms | 55.17 ms | 64.74 ms | 277.58 ms | 9257.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 63.24 ms | 54.03 ms | 119.10 ms | 201.52 ms | 403.53 ms | 40717.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 71.08 ms | 61.44 ms | 130.19 ms | 172.54 ms | 263.28 ms | 39859.33 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 107.68 ms | 79.61 ms | 215.96 ms | 280.47 ms | 542.40 ms | 64146.67 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 87.34 ms | 81.19 ms | 136.16 ms | 178.85 ms | 235.36 ms | 34673.00 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 103.48 ms | 101.59 ms | 133.14 ms | 179.76 ms | 654.61 ms | 34092.67 | 
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 105.55 ms | 102.89 ms | 149.80 ms | 200.27 ms | 281.71 ms | 34178.00 | 
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 236.54 ms | 234.44 ms | 341.63 ms | 473.43 ms | 1395.67 ms | 93330.33 | 

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
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 307942.33 | 178.02 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 285241.00 | 341.59 MB |
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 239767.67 | 272.55 MB |
| c (99) | [kore](http://kore.io) (3.1) | 223323.00 | 580.22 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 222589.67 | 216.08 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 219702.00 | 249.38 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 201944.00 | 405.91 MB |
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 198872.00 | 187.14 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 197305.33 | 316.14 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 183574.00 | 106.09 MB |
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 179450.33 | 366.96 MB |
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 175526.67 | 165.14 MB |
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 175034.67 | 218.23 MB |
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 164430.00 | 268.90 MB |
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 162629.67 | 173.44 MB |
| java (8) | [act](http://actframework.org) (1.8) | 154170.33 | 266.19 MB |
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 141685.67 | 231.35 MB |
| node (12.2) | [restana](http://github.com/jkyberneees/ana) (3.0) | 128585.33 | 192.80 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 127709.67 | 208.34 MB |
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 119126.33 | 149.84 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.4) | 112451.33 | 197.31 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 112087.33 | 150.49 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 110407.67 | 193.77 MB |
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 109927.67 | 147.01 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 109431.33 | 143.77 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 108211.33 | 145.82 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 107005.67 | 141.93 MB |
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 105437.33 | 164.44 MB |
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 102336.00 | 203.19 MB |
| node (12.2) | [polka](http://github.com/lukeed/polka) (0.5) | 101381.00 | 152.01 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 99087.67 | 133.19 MB |
| node (12.2) | [rayo](http://rayo.js.org) (1.3) | 95778.00 | 143.42 MB |
| go (1.12) | [gf](http://goframe.org) (1.6) | 91062.33 | 137.46 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 89170.67 | 83.85 MB |
| node (12.2) | [fastify](http://fastify.io) (2.3) | 88261.67 | 225.60 MB |
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 85136.33 | 199.40 MB |
| node (12.2) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 84389.33 | 126.47 MB |
| node (12.2) | [foxify](http://foxify.js.org) (0.10) | 78324.33 | 164.56 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 73606.33 | 181.15 MB |
| node (12.2) | [restify](http://restify.com) (8.2) | 68636.00 | 120.51 MB |
| node (12.2) | [koa](http://koajs.com) (2.7) | 67083.00 | 142.11 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 63904.00 | 107.03 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 62506.00 | 109.60 MB |
| node (12.2) | [express](http://expressjs.com) (4.16) | 62150.67 | 152.30 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 61870.33 | 133.31 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 61766.33 | 132.47 MB |
| python (3.7) | [hug](http://hug.rest) (2.5) | 52324.33 | 129.80 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 44676.67 | 222.03 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.22) | 42355.00 | 91.52 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 41211.67 | 65.84 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 40609.00 | 201.63 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 39962.67 | 198.39 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 39688.67 | 73.61 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 39264.67 | 194.87 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 37658.67 | 85.44 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 37080.67 | 192.60 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 34309.33 | 178.63 MB |
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 32022.00 | 60.35 MB |
| node (12.2) | [hapi](http://hapijs.com) (18.1) | 30849.67 | 79.94 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 29228.33 | 27.86 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 26622.67 | 49.49 MB |
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 26028.00 | 33.85 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 25109.67 | 61.85 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 23797.00 | 46.06 MB |
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 22810.33 | 27.97 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 21752.33 | 12.53 MB |
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 21373.00 | 39.00 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 16575.33 | 29.57 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 14635.67 | 29.19 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 14378.33 | 8.30 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 14297.00 | 108.21 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 11532.67 | 29.92 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 11384.00 | 24.83 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 9912.00 | 28.74 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 9467.33 | 27.92 MB |
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 9408.00 | 24.22 MB |
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 3686.00 | 9.09 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3146.00 | 9.63 MB |
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

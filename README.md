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
Last update: 2019-05-20
```
OS: Linux (version: 5.0.9-301.fc30.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: rack-routing (ruby)


:four: iron (rust)


:five: laravel (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 0.08 ms | 0.08 ms | 0.12 ms | 0.15 ms | 3.32 ms | 41.00 | 
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 3.59 ms | 0.20 ms | 12.97 ms | 30.90 ms | 81.60 ms | 6932.00 | 
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.77 ms | 0.25 ms | 17.22 ms | 37.59 ms | 91.10 ms | 8725.33 | 
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 0.37 ms | 0.36 ms | 0.61 ms | 0.90 ms | 10.27 ms | 217.67 | 
| php (7.3) | [laravel](http://laravel.com) (5.8) | 116.60 ms | 0.40 ms | 325.02 ms | 1887.81 ms | 6916.07 ms | 387406.67 | 
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 122.52 ms | 0.40 ms | 269.24 ms | 2595.64 ms | 7111.06 ms | 452616.00 | 
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 6.90 ms | 0.40 ms | 23.15 ms | 47.75 ms | 122.82 ms | 11349.33 | 
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 175.90 ms | 0.42 ms | 313.83 ms | 3663.77 ms | 7095.33 ms | 648022.00 | 
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 142.96 ms | 0.43 ms | 283.34 ms | 3179.84 ms | 6938.57 ms | 533626.33 | 
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 8.27 ms | 0.44 ms | 28.23 ms | 58.78 ms | 161.79 ms | 13963.00 | 
| php (7.3) | [slim](http://slimframework.com) (3.12) | 179.70 ms | 0.44 ms | 279.31 ms | 4358.44 ms | 7181.82 ms | 700076.33 | 
| php (7.3) | [symfony](http://symfony.com) (4.2) | 163.68 ms | 0.58 ms | 316.62 ms | 3400.30 ms | 6854.40 ms | 591900.33 | 
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 10.10 ms | 0.65 ms | 31.88 ms | 64.20 ms | 152.60 ms | 15447.33 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 124.97 ms | 1.72 ms | 57.06 ms | 3205.20 ms | 4671.85 ms | 548362.33 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 3.04 ms | 2.21 ms | 6.47 ms | 12.96 ms | 33.95 ms | 2851.67 | 
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 3.68 ms | 2.67 ms | 7.58 ms | 16.71 ms | 152.22 ms | 4889.33 | 
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 3.57 ms | 2.91 ms | 7.11 ms | 14.82 ms | 29.92 ms | 3143.67 | 
| c (99) | [kore](http://kore.io) (3.1) | 13.67 ms | 3.38 ms | 8.03 ms | 366.49 ms | 1316.82 ms | 78966.00 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 4.73 ms | 3.76 ms | 9.30 ms | 17.13 ms | 33.61 ms | 3657.33 | 
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.48 ms | 4.28 ms | 6.74 ms | 13.49 ms | 98.99 ms | 2662.67 | 
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 6.84 ms | 4.35 ms | 11.05 ms | 76.50 ms | 116.13 ms | 11711.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.04 ms | 4.51 ms | 5.66 ms | 9.62 ms | 41.24 ms | 1968.33 | 
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 5.15 ms | 4.59 ms | 9.92 ms | 20.73 ms | 203.08 ms | 5214.33 | 
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 5.37 ms | 4.77 ms | 9.60 ms | 17.60 ms | 36.70 ms | 3381.33 | 
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 41.79 ms | 5.00 ms | 137.83 ms | 353.32 ms | 940.52 ms | 76240.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 5.09 ms | 5.01 ms | 7.67 ms | 13.45 ms | 76.01 ms | 2717.67 | 
| node (12.2) | [restana](http://github.com/jkyberneees/ana) (2.13) | 6.86 ms | 5.19 ms | 9.88 ms | 20.23 ms | 308.68 ms | 9677.00 | 
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 7.78 ms | 5.49 ms | 15.72 ms | 33.19 ms | 246.58 ms | 8514.33 | 
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 8.45 ms | 5.64 ms | 17.44 ms | 38.46 ms | 209.66 ms | 10312.67 | 
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 8.49 ms | 5.67 ms | 17.36 ms | 36.56 ms | 353.38 ms | 10913.67 | 
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 8.50 ms | 5.78 ms | 17.94 ms | 37.49 ms | 175.44 ms | 8073.00 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.82 ms | 5.86 ms | 11.07 ms | 22.85 ms | 173.76 ms | 6408.33 | 
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 8.94 ms | 5.94 ms | 19.21 ms | 40.21 ms | 189.24 ms | 8807.00 | 
| node (12.2) | [rayo](http://rayo.js.org) (1.3) | 7.44 ms | 5.94 ms | 11.16 ms | 22.29 ms | 240.72 ms | 7197.67 | 
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 7.95 ms | 5.96 ms | 14.61 ms | 31.91 ms | 182.20 ms | 6968.00 | 
| node (12.2) | [polka](http://github.com/lukeed/polka) (0.5) | 8.02 ms | 5.98 ms | 11.19 ms | 23.89 ms | 370.43 ms | 13519.00 | 
| go (1.12) | [beego](http://beego.me) (1.12) | 8.58 ms | 6.08 ms | 17.48 ms | 36.97 ms | 184.31 ms | 7722.33 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 7.12 ms | 6.25 ms | 9.98 ms | 18.57 ms | 287.30 ms | 7374.33 | 
| go (1.12) | [violetear](http://violetear.org) (7.0) | 8.30 ms | 6.34 ms | 15.69 ms | 33.14 ms | 76.21 ms | 6269.33 | 
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 8.56 ms | 7.08 ms | 14.88 ms | 28.01 ms | 215.46 ms | 6175.00 | 
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 8.47 ms | 7.20 ms | 16.48 ms | 34.37 ms | 179.92 ms | 7239.33 | 
| go (1.12) | [gf](http://goframe.org) (1.6) | 10.95 ms | 7.61 ms | 22.47 ms | 49.58 ms | 252.93 ms | 12242.00 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 187.62 ms | 7.69 ms | 27.65 ms | 4808.27 ms | 7926.61 ms | 812970.33 | 
| node (12.2) | [fastify](http://fastify.io) (2.3) | 11.09 ms | 7.89 ms | 14.54 ms | 74.86 ms | 553.05 ms | 23521.33 | 
| node (12.2) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 11.93 ms | 8.21 ms | 17.13 ms | 89.09 ms | 573.25 ms | 25173.33 | 
| node (12.2) | [foxify](http://foxify.js.org) (0.10) | 10.41 ms | 8.83 ms | 14.52 ms | 38.40 ms | 461.68 ms | 17571.00 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 11.81 ms | 9.02 ms | 24.43 ms | 46.28 ms | 215.20 ms | 9949.00 | 
| node (12.2) | [koa](http://koajs.com) (2.7) | 11.48 ms | 9.05 ms | 14.88 ms | 61.32 ms | 541.66 ms | 23663.67 | 
| node (12.2) | [restify](http://restify.com) (8.2) | 11.26 ms | 10.07 ms | 14.85 ms | 26.63 ms | 300.08 ms | 9204.33 | 
| node (12.2) | [express](http://expressjs.com) (4.16) | 14.23 ms | 10.60 ms | 18.93 ms | 81.45 ms | 591.82 ms | 25622.00 | 
| python (3.7) | [hug](http://hug.rest) (2.5) | 14.94 ms | 11.83 ms | 28.11 ms | 50.79 ms | 221.95 ms | 10407.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 11.80 ms | 11.88 ms | 13.79 ms | 15.89 ms | 66.48 ms | 1755.67 | 
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 16.64 ms | 12.52 ms | 21.87 ms | 77.37 ms | 949.16 ms | 36772.00 | 
| python (3.7) | [starlette](http://starlette.io) (0.11) | 14.92 ms | 13.51 ms | 25.00 ms | 39.49 ms | 144.02 ms | 8215.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 15.51 ms | 13.75 ms | 27.62 ms | 48.67 ms | 442.81 ms | 16238.33 | 
| node (12.2) | [hapi](http://hapijs.com) (18.1) | 25.43 ms | 18.79 ms | 30.84 ms | 219.68 ms | 1024.34 ms | 49749.00 | 
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.22) | 22.20 ms | 19.51 ms | 37.26 ms | 59.59 ms | 161.58 ms | 11466.33 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 23.49 ms | 20.51 ms | 41.92 ms | 56.65 ms | 78.52 ms | 12125.00 | 
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 29.50 ms | 26.76 ms | 38.93 ms | 51.93 ms | 395.31 ms | 12825.00 | 
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 32.87 ms | 27.14 ms | 50.09 ms | 124.60 ms | 493.89 ms | 23610.00 | 
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 42.62 ms | 28.89 ms | 47.55 ms | 561.84 ms | 1693.24 ms | 100667.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 36.99 ms | 29.34 ms | 72.09 ms | 103.65 ms | 270.74 ms | 21875.33 | 
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 32.98 ms | 30.46 ms | 46.72 ms | 72.36 ms | 637.84 ms | 22877.33 | 
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 30.35 ms | 30.61 ms | 38.98 ms | 54.40 ms | 275.02 ms | 11446.33 | 
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 33.60 ms | 32.86 ms | 41.97 ms | 51.98 ms | 481.03 ms | 15692.67 | 
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 35.40 ms | 33.13 ms | 45.68 ms | 58.80 ms | 263.32 ms | 9870.33 | 
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 41.85 ms | 34.57 ms | 97.22 ms | 199.30 ms | 488.23 ms | 46169.67 | 
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 39.05 ms | 35.19 ms | 73.60 ms | 113.37 ms | 206.86 ms | 25293.00 | 
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 34.91 ms | 35.26 ms | 43.38 ms | 75.21 ms | 186.82 ms | 11126.00 | 
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 37.74 ms | 37.88 ms | 48.56 ms | 58.48 ms | 240.21 ms | 9565.67 | 
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 42.59 ms | 42.51 ms | 50.52 ms | 68.98 ms | 416.87 ms | 16673.33 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 51.13 ms | 44.49 ms | 92.34 ms | 158.02 ms | 284.58 ms | 31566.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 57.86 ms | 56.52 ms | 101.45 ms | 142.11 ms | 192.31 ms | 32905.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.2) | 92.36 ms | 76.79 ms | 160.45 ms | 201.46 ms | 560.37 ms | 44439.33 | 
| python (3.7) | [responder](http://python-responder.org) (1.3) | 82.74 ms | 77.36 ms | 128.96 ms | 172.92 ms | 217.74 ms | 31977.67 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 85.28 ms | 85.99 ms | 101.40 ms | 118.93 ms | 558.31 ms | 21886.67 | 
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 121.15 ms | 118.85 ms | 166.41 ms | 218.00 ms | 320.01 ms | 35543.33 | 
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 249.13 ms | 208.14 ms | 395.68 ms | 1024.34 ms | 2348.87 ms | 187059.67 | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (actix-web) (rust)


:three: (japronto) (python)


:four: (kore) (c)


:five: (vibora) (python)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.5) | 325363.67 | 188.26 MB |
| rust (1.34) | [actix-web](http://actix.rs) (0.7) | 278850.33 | 316.77 MB |
| python (3.7) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 278179.33 | 333.12 MB |
| c (99) | [kore](http://kore.io) (3.1) | 234751.67 | 609.77 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 224419.33 | 254.72 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 220363.67 | 213.94 MB |
| go (1.12) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 206844.33 | 332.87 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 201933.67 | 405.70 MB |
| ruby (2.6) | [agoo](http://github.com/ohler55/agoo) (2.8) | 200594.33 | 116.07 MB |
| rust (1.34) | [gotham](http://gotham.rs) (0.3) | 196698.33 | 402.72 MB |
| java (8) | [act](http://actframework.org) (1.8) | 191124.33 | 330.03 MB |
| crystal (0.28) | [spider-gazelle](http://spider-gazelle.net) (1.4) | 182791.00 | 195.94 MB |
| rust (1.34) | [iron](http://ironframework.io) (0.6) | 156875.33 | 198.49 MB |
| node (12.2) | [restana](http://github.com/jkyberneees/ana) (2.13) | 150808.67 | 226.06 MB |
| go (1.12) | [chi](http://github.com/go-chi/chi) (4.0) | 138683.67 | 185.82 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.2) | 136310.00 | 221.91 MB |
| node (12.2) | [polka](http://github.com/lukeed/polka) (0.5) | 134782.67 | 201.94 MB |
| go (1.12) | [gorouter](http://github.com/vardius/gorouter/wiki) (4.0) | 134486.00 | 179.69 MB |
| node (12.2) | [rayo](http://rayo.js.org) (1.3) | 133319.00 | 199.81 MB |
| go (1.12) | [kami](http://github.com/guregu/kami) (2.2) | 132280.67 | 175.97 MB |
| go (1.12) | [echo](http://echo.labstack.com) (4.1) | 130947.00 | 229.53 MB |
| go (1.12) | [gin](http://gin-gonic.com) (1.3) | 128051.67 | 224.44 MB |
| go (1.12) | [violetear](http://violetear.org) (7.0) | 127344.67 | 168.65 MB |
| go (1.12) | [beego](http://beego.me) (1.12) | 126789.67 | 170.02 MB |
| kotlin (1.3) | [ktor](http://ktor.io) (1.2) | 126144.00 | 196.22 MB |
| go (1.12) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.7) | 125508.67 | 167.88 MB |
| python (3.7) | [falcon](http://falconframework.org) (2.0) | 117336.67 | 275.20 MB |
| node (12.2) | [fastify](http://fastify.io) (2.3) | 115441.33 | 298.26 MB |
| node (12.2) | [foxify](http://foxify.js.org) (0.10) | 106697.33 | 224.32 MB |
| node (12.2) | [muneem](http://github.com/node-muneem/muneem/) (2.4) | 106614.33 | 159.69 MB |
| go (1.12) | [gf](http://goframe.org) (1.6) | 105065.33 | 159.09 MB |
| node (12.2) | [koa](http://koajs.com) (2.7) | 102831.00 | 217.84 MB |
| rust (1.34) | [nickel](http://nickel-org.github.io) (0.11) | 102015.67 | 203.02 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 91829.00 | 226.34 MB |
| node (12.2) | [restify](http://restify.com) (8.2) | 89102.33 | 156.46 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.1) | 83545.33 | 78.61 MB |
| node (12.2) | [express](http://expressjs.com) (4.16) | 81234.33 | 199.04 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 70447.33 | 123.34 MB |
| swift (5.0) | [vapor](http://vapor.codes) (3.3) | 69869.67 | 117.36 MB |
| python (3.7) | [hug](http://hug.rest) (2.5) | 69431.00 | 172.22 MB |
| python (3.7) | [starlette](http://starlette.io) (0.11) | 67227.67 | 144.90 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 65771.33 | 140.95 MB |
| php (7.3) | [zend-expressive](http://zendframework.github.io/zend-expressive) (3.2) | 53697.00 | 266.06 MB |
| php (7.3) | [symfony](http://symfony.com) (4.2) | 53684.67 | 266.26 MB |
| php (7.3) | [slim](http://slimframework.com) (3.12) | 53434.67 | 264.88 MB |
| node (12.2) | [hapi](http://hapijs.com) (18.1) | 51761.33 | 134.72 MB |
| php (7.3) | [zend-framework](http://framework.zend.com) (3.1) | 50095.67 | 248.13 MB |
| php (7.3) | [lumen](http://lumen.laravel.com) (5.8) | 48996.33 | 254.15 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.4) | 45574.67 | 73.67 MB |
| python (3.7) | [fastapi](http://fastapi.tiangolo.com) (0.22) | 45448.00 | 98.08 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.5) | 43168.00 | 97.96 MB |
| php (7.3) | [laravel](http://laravel.com) (5.8) | 42882.00 | 223.43 MB |
| ruby (2.6) | [roda](http://roda.jeremyevans.net) (3.2) | 35668.33 | 33.98 MB |
| java (8) | [spring-boot](http://spring.io/projects/spring-boot) (2.1) | 35587.33 | 45.64 MB |
| crystal (0.28) | [raze](http://razecr.com) (0.3) | 33628.33 | 31.53 MB |
| crystal (0.28) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 32580.33 | 30.54 MB |
| python (3.7) | [molten](http://moltenframework.com) (0.7) | 32056.00 | 59.55 MB |
| swift (5.0) | [kitura](http://kitura.io) (2.7) | 30673.33 | 56.89 MB |
| swift (5.0) | [kitura-nio](http://kitura.io) (2.7) | 30352.33 | 57.28 MB |
| crystal (0.28) | [athena](http://github.com/blacksmoke16/athena) (0.6) | 29828.67 | 36.96 MB |
| crystal (0.28) | [kemal](http://kemalcr.com) (0.25) | 29126.33 | 47.46 MB |
| crystal (0.28) | [lucky](http://luckyframework.org) (0.14) | 28181.33 | 34.81 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 28176.33 | 69.30 MB |
| python (3.7) | [bocadillo](http://bocadilloproject.github.io) (0.15) | 26886.00 | 51.98 MB |
| ruby (2.6) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 26867.33 | 15.49 MB |
| crystal (0.28) | [amber](http://amberframework.org) (0.28) | 26164.00 | 47.75 MB |
| crystal (0.28) | [orion](http://github.com/obsidian/orion) (1.7) | 23347.00 | 38.04 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (19.3) | 20150.67 | 35.92 MB |
| ruby (2.6) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 18511.00 | 10.69 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.9) | 17386.67 | 34.65 MB |
| ruby (2.6) | [hanami](http://hanamirb.org) (1.3) | 15418.00 | 116.52 MB |
| ruby (2.6) | [sinatra](http://sinatrarb.com) (2.0) | 12721.67 | 32.98 MB |
| python (3.7) | [responder](http://python-responder.org) (1.3) | 12048.67 | 26.25 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 11355.67 | 33.51 MB |
| python (3.7) | [django](http://djangoproject.com) (2.2) | 10932.33 | 31.67 MB |
| crystal (0.28) | [onyx](http://onyxframework.org) (0.4) | 8187.00 | 21.08 MB |
| python (3.7) | [masonite](http://masoniteproject.com) (2.1) | 4200.67 | 10.33 MB |
| ruby (2.6) | [rails](http://rubyonrails.org) (5.2) | 3053.33 | 9.36 MB |
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

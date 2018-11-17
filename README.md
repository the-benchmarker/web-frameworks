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
Last update: 2018-11-16
```
OS: Linux (version: 4.18.18-200.fc28.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: laravel (php)


:three: iron (rust)


:four: rack-routing (ruby)


:five: hanami (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 0.07 ms | 0.07 ms | 0.11 ms | 0.13 ms | 4.79 ms | 40.67 | 
| php (7.2) | [laravel](http://laravel.com) (5.7) | 103.32 ms | 0.27 ms | 266.70 ms | 1847.07 ms | 6812.51 ms | 369261.67 | 
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 0.35 ms | 0.35 ms | 0.56 ms | 0.78 ms | 11.45 ms | 200.67 | 
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 3.78 ms | 0.67 ms | 10.99 ms | 35.77 ms | 118.08 ms | 7272.67 | 
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 6.62 ms | 0.81 ms | 20.41 ms | 62.53 ms | 181.54 ms | 12897.33 | 
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 3.02 ms | 0.93 ms | 8.36 ms | 25.42 ms | 89.48 ms | 5213.67 | 
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 110.03 ms | 1.33 ms | 198.03 ms | 2490.29 ms | 5671.38 ms | 429414.00 | 
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 120.19 ms | 1.34 ms | 55.33 ms | 3348.45 ms | 6593.84 ms | 595328.33 | 
| php (7.2) | [symfony](http://symfony.com) (4.1) | 99.63 ms | 1.39 ms | 181.73 ms | 2266.09 ms | 5766.55 ms | 406597.67 | 
| php (7.2) | [slim](http://slimframework.com) (3.11) | 95.70 ms | 1.48 ms | 168.49 ms | 2338.95 ms | 5253.15 ms | 392970.00 | 
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 2.98 ms | 1.98 ms | 6.54 ms | 15.88 ms | 34.94 ms | 3280.33 | 
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 2.97 ms | 2.09 ms | 6.30 ms | 14.45 ms | 93.01 ms | 3478.00 | 
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 7.80 ms | 2.45 ms | 22.49 ms | 58.05 ms | 164.84 ms | 12307.33 | 
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 6.13 ms | 2.56 ms | 16.85 ms | 41.42 ms | 118.18 ms | 8863.67 | 
| python (3.6) | [vibora](http://vibora.io) (0.0) | 3.54 ms | 2.84 ms | 7.57 ms | 14.53 ms | 35.33 ms | 3168.00 | 
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 3.56 ms | 2.99 ms | 5.77 ms | 11.26 ms | 101.21 ms | 2557.67 | 
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 3.17 ms | 3.16 ms | 5.34 ms | 7.33 ms | 21.63 ms | 1771.33 | 
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 3.90 ms | 3.52 ms | 6.43 ms | 11.51 ms | 284.85 ms | 6062.00 | 
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 4.36 ms | 3.69 ms | 7.06 ms | 12.55 ms | 53.44 ms | 2465.33 | 
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 4.47 ms | 3.90 ms | 8.49 ms | 15.75 ms | 33.17 ms | 3176.00 | 
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 6.53 ms | 4.14 ms | 11.73 ms | 57.51 ms | 127.94 ms | 10532.33 | 
| c (99) | [kore](http://kore.io) (3.1) | 4.79 ms | 4.72 ms | 8.07 ms | 10.18 ms | 35.12 ms | 2506.00 | 
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 5.72 ms | 4.85 ms | 9.63 ms | 19.07 ms | 161.37 ms | 4366.67 | 
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 5.76 ms | 5.19 ms | 8.63 ms | 14.71 ms | 201.56 ms | 3584.00 | 
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 6.79 ms | 5.22 ms | 9.92 ms | 30.23 ms | 76.41 ms | 4977.67 | 
| java (8) | [act](http://actframework.org) (1.8) | 6.00 ms | 5.23 ms | 10.01 ms | 19.81 ms | 140.87 ms | 4071.67 | 
| go (1.11) | [iris](http://iris-go.com) (11.0) | 6.54 ms | 5.26 ms | 10.46 ms | 21.41 ms | 289.60 ms | 7747.67 | 
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 7.23 ms | 5.38 ms | 11.06 ms | 24.43 ms | 417.72 ms | 13132.00 | 
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 7.71 ms | 5.62 ms | 13.57 ms | 30.75 ms | 272.84 ms | 8870.00 | 
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 7.04 ms | 5.67 ms | 11.95 ms | 24.00 ms | 140.92 ms | 4998.00 | 
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 7.47 ms | 5.68 ms | 12.30 ms | 21.62 ms | 216.33 ms | 6762.67 | 
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.1) | 5.89 ms | 5.73 ms | 7.44 ms | 9.14 ms | 113.25 ms | 1809.33 | 
| go (1.11) | [beego](http://beego.me) (1.10) | 7.64 ms | 5.74 ms | 11.83 ms | 25.08 ms | 491.84 ms | 13806.00 | 
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 7.25 ms | 5.75 ms | 11.98 ms | 24.93 ms | 283.36 ms | 6964.00 | 
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 7.54 ms | 6.13 ms | 12.44 ms | 25.55 ms | 288.64 ms | 6879.67 | 
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 9.87 ms | 7.13 ms | 17.62 ms | 42.23 ms | 383.87 ms | 14037.00 | 
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 9.67 ms | 7.27 ms | 17.01 ms | 40.09 ms | 367.59 ms | 12713.67 | 
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 9.96 ms | 7.43 ms | 16.71 ms | 35.88 ms | 420.00 ms | 13980.67 | 
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 230.39 ms | 7.64 ms | 224.93 ms | 4834.04 ms | 7905.07 ms | 870806.00 | 
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 31.69 ms | 8.00 ms | 100.69 ms | 263.52 ms | 706.06 ms | 56078.00 | 
| node (11.1) | [fastify](http://fastify.io) (1.13) | 11.93 ms | 9.22 ms | 20.27 ms | 44.03 ms | 378.49 ms | 13791.00 | 
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 13.76 ms | 10.95 ms | 18.80 ms | 33.90 ms | 728.06 ms | 25121.33 | 
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 10.87 ms | 11.11 ms | 12.81 ms | 14.78 ms | 145.92 ms | 2353.00 | 
| node (11.1) | [koa](http://koajs.com) (2.6) | 16.00 ms | 11.58 ms | 25.53 ms | 96.98 ms | 584.08 ms | 25263.67 | 
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 15.70 ms | 11.68 ms | 25.37 ms | 61.62 ms | 600.98 ms | 23192.00 | 
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 15.70 ms | 12.64 ms | 25.52 ms | 49.29 ms | 684.37 ms | 31370.67 | 
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 16.33 ms | 14.51 ms | 29.47 ms | 43.17 ms | 79.55 ms | 9126.67 | 
| node (11.1) | [express](http://expressjs.com) (4.16) | 21.59 ms | 15.44 ms | 32.49 ms | 130.24 ms | 846.77 ms | 39656.00 | 
| node (11.1) | [restify](http://restify.com) (7.2) | 19.66 ms | 16.04 ms | 33.53 ms | 61.46 ms | 459.28 ms | 17290.00 | 
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 20.98 ms | 19.09 ms | 32.25 ms | 37.61 ms | 231.26 ms | 6565.00 | 
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 21.94 ms | 20.35 ms | 29.87 ms | 44.73 ms | 180.53 ms | 6834.33 | 
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 24.10 ms | 21.46 ms | 36.57 ms | 43.90 ms | 256.51 ms | 9399.00 | 
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 26.04 ms | 23.38 ms | 38.72 ms | 60.38 ms | 471.64 ms | 16228.00 | 
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 29.60 ms | 24.50 ms | 40.71 ms | 133.21 ms | 767.02 ms | 33765.67 | 
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 33.40 ms | 25.30 ms | 45.35 ms | 244.96 ms | 1096.08 ms | 55819.00 | 
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 27.13 ms | 28.46 ms | 35.53 ms | 42.86 ms | 238.55 ms | 8011.00 | 
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 34.94 ms | 30.64 ms | 46.49 ms | 55.66 ms | 424.47 ms | 16429.00 | 
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 38.45 ms | 32.43 ms | 73.51 ms | 132.96 ms | 246.15 ms | 27529.33 | 
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 32.45 ms | 33.67 ms | 41.84 ms | 46.94 ms | 342.95 ms | 12642.67 | 
| python (3.7) | [django](http://djangoproject.com) (2.1) | 48.17 ms | 37.82 ms | 87.83 ms | 136.95 ms | 647.51 ms | 33212.00 | 
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 50.15 ms | 44.74 ms | 92.66 ms | 146.14 ms | 200.92 ms | 33373.67 | 
| go (1.11) | [gf](http://gfer.me) (1.1) | 41.35 ms | 49.12 ms | 77.95 ms | 117.68 ms | 266.89 ms | 31391.33 | 
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 69.46 ms | 69.13 ms | 85.39 ms | 106.22 ms | 540.84 ms | 21414.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (actix-web) (rust)


:three: (vibora) (python)


:four: (evhtp) (cpp)


:five: (fasthttprouter) (go)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| python (3.6) | [japronto](http://github.com/squeaky-pl/japronto) (0.1) | 386501.00 | 462.45 MB |
| rust (1.30) | [actix-web](http://actix.rs) (0.7) | 350368.00 | 398.31 MB |
| python (3.6) | [vibora](http://vibora.io) (0.0) | 310300.33 | 352.42 MB |
| cpp (11.0) | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 279154.67 | 271.05 MB |
| go (1.11) | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 261741.33 | 421.76 MB |
| rust (1.30) | [gotham](http://gotham.rs) (0.3) | 242309.33 | 496.14 MB |
| nim (0.19) | [jester](http://github.com/dom96/jester) (0.4) | 239419.00 | 481.13 MB |
| java (8) | [act](http://actframework.org) (1.8) | 225552.33 | 440.58 MB |
| crystal (0.27) | [spider-gazelle](http://spider-gazelle.net) (1.1) | 223696.67 | 239.94 MB |
| ruby (2.5) | [agoo](http://github.com/ohler55/agoo) (2.5) | 214316.67 | 123.97 MB |
| rust (1.30) | [iron](http://ironframework.io) (0.6) | 172470.00 | 217.50 MB |
| c (11) | [agoo-c](http://github.com/ohler55/agoo-c) (0.1) | 169047.33 | 97.59 MB |
| go (1.11) | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 168061.67 | 226.86 MB |
| csharp (7.3) | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 163539.00 | 266.54 MB |
| go (1.11) | [iris](http://iris-go.com) (11.0) | 153418.67 | 205.56 MB |
| nim (0.19) | [mofuw](http://github.com/2vg/mofuw) (2.0) | 152878.67 | 268.27 MB |
| go (1.11) | [chi](http://github.com/go-chi/chi) (3.3) | 149160.00 | 199.77 MB |
| go (1.11) | [echo](http://echo.labstack.com) (3.3) | 141056.33 | 247.69 MB |
| node (11.1) | [restana](http://github.com/jkyberneees/ana) (2.3) | 140934.67 | 211.34 MB |
| go (1.11) | [beego](http://beego.me) (1.10) | 140350.33 | 189.29 MB |
| go (1.11) | [gin](http://gin-gonic.github.io/gin) (1.3) | 139441.00 | 244.84 MB |
| python (3.7) | [falcon](http://falconframework.org) (1.4) | 133356.67 | 342.26 MB |
| go (1.11) | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 132986.67 | 177.55 MB |
| node (11.1) | [rayo](http://rayo.js.org) (1.2) | 116943.67 | 174.97 MB |
| node (11.1) | [polka](http://github.com/lukeed/polka) (0.5) | 116351.33 | 174.33 MB |
| python (3.7) | [bottle](http://bottlepy.org) (0.12) | 113730.33 | 279.86 MB |
| rust (1.30) | [nickel](http://nickel-org.github.io) (0.10) | 105740.33 | 210.57 MB |
| node (11.1) | [fastify](http://fastify.io) (1.13) | 99419.67 | 232.80 MB |
| swift (4.2) | [perfect](http://perfect.org) (3.0) | 89404.00 | 84.06 MB |
| php (7.2) | [slim](http://slimframework.com) (3.11) | 87370.00 | 433.57 MB |
| php (7.2) | [symfony](http://symfony.com) (4.1) | 85760.67 | 425.75 MB |
| php (7.2) | [lumen](http://lumen.laravel.com) (5.7) | 80027.33 | 415.51 MB |
| swift (4.2) | [vapor](http://vapor.codes) (3.0) | 79214.00 | 133.56 MB |
| scala (2.12) | [akkahttp](http://akka.io) (10.1) | 75756.67 | 162.32 MB |
| scala (2.12) | [http4s](http://http4s.org) (0.18) | 74761.67 | 130.66 MB |
| node (11.1) | [foxify](http://foxify.js.org) (0.10) | 73726.67 | 154.56 MB |
| node (11.1) | [koa](http://koajs.com) (2.6) | 73575.67 | 155.36 MB |
| c (99) | [kore](http://kore.io) (3.1) | 65770.00 | 178.26 MB |
| rust (nightly) | [rocket](http://rocket.rs) (0.3) | 64952.00 | 103.18 MB |
| python (3.7) | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 62493.67 | 141.72 MB |
| php (7.2) | [laravel](http://laravel.com) (5.7) | 61963.33 | 322.60 MB |
| node (11.1) | [express](http://expressjs.com) (4.16) | 56749.00 | 138.51 MB |
| node (11.1) | [restify](http://restify.com) (7.2) | 54065.67 | 94.57 MB |
| crystal (0.27) | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 47090.33 | 44.20 MB |
| swift (4.2) | [kitura](http://kitura.io) (2.5) | 45454.33 | 84.28 MB |
| ruby (2.5) | [roda](http://roda.jeremyevans.net) (3.13) | 42959.00 | 40.90 MB |
| crystal (0.27) | [kemal](http://kemalcr.com) (0.25) | 40923.33 | 66.65 MB |
| python (3.7) | [flask](http://flask.pocoo.org) (1.0) | 38526.67 | 94.89 MB |
| crystal (0.27) | [raze](http://razecr.com) (0.3) | 36957.67 | 34.63 MB |
| node (11.1) | [hapi](http://hapijs.com) (17.7) | 35973.67 | 93.02 MB |
| crystal (0.27) | [lucky](http://luckyframework.org) (0.11) | 35965.33 | 44.11 MB |
| ruby (2.5) | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 34506.67 | 19.90 MB |
| crystal (0.27) | [amber](http://amberframework.org) (0.11) | 30710.33 | 56.05 MB |
| crystal (0.27) | [orion](http://github.com/obsidian/orion) (1.6) | 28498.00 | 46.41 MB |
| python (3.7) | [sanic](http://github.com/huge-success/sanic) (0.8) | 27839.33 | 49.56 MB |
| go (1.11) | [gf](http://gfer.me) (1.1) | 25293.00 | 78.60 MB |
| python (3.7) | [django](http://djangoproject.com) (2.1) | 21368.00 | 61.95 MB |
| ruby (2.5) | [flame](http://github.com/AlexWayfer/flame) (4.18) | 20927.67 | 12.06 MB |
| python (3.7) | [quart](http://pgjones.gitlab.io/quart) (0.6) | 20726.00 | 41.27 MB |
| ruby (2.5) | [hanami](http://hanamirb.org) (1.3) | 19531.00 | 147.68 MB |
| ruby (2.5) | [sinatra](http://sinatrarb.com) (2.0) | 16386.00 | 42.49 MB |
| python (3.7) | [tornado](http://tornadoweb.org) (5.1) | 14070.00 | 37.84 MB |
| ruby (2.5) | [rails](http://rubyonrails.org) (5.2) | 4056.67 | 12.39 MB |
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

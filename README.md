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

> job is either a language (example : crystal) or a framework (example : router.cr)

~~~sh
bin/neph [job]
~~~

+ Start the benchmark ....

> tools is a list of language / framework to challenge (example : ruby kemal amber go python)

~~~sh
bin/benchmarker [tools]
~~~

## Results

<!-- Result from here -->
Last update: 2018-09-25
```
OS: Linux (version: 4.16.11-100.fc26.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: rocket (rust)


:three: iron (rust)


:four: roda (ruby)


:five: rack-routing (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust | [nickel](http://nickel.rs) (0.10) | 0.14 ms | 0.12 ms | 0.17 ms | 0.89 ms | 10.61 ms | 199.00 | 
| rust | [rocket](https://rocket.rs) (0.3) | 0.25 ms | 0.15 ms | 0.36 ms | 2.60 ms | 16.55 ms | 477.67 | 
| rust | [iron](http://ironframework.io) (0.7) | 0.98 ms | 0.67 ms | 2.00 ms | 5.92 ms | 54.06 ms | 1260.00 | 
| ruby | [roda](http://roda.jeremyevans.net) (3.12) | 2.81 ms | 1.98 ms | 5.87 ms | 14.88 ms | 76.26 ms | 3026.00 | 
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 4.23 ms | 2.33 ms | 9.71 ms | 32.39 ms | 123.06 ms | 6387.33 | 
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 8.46 ms | 2.92 ms | 22.70 ms | 73.76 ms | 238.35 ms | 14737.33 | 
| ruby | [flame](http://github.com/AlexWayfer/flame) (5.0 (rc)) | 9.61 ms | 3.31 ms | 23.46 ms | 99.47 ms | 308.50 ms | 19196.00 | 
| ruby | [hanami](http://hanamirb.org) (1.2) | 10.71 ms | 3.55 ms | 23.67 ms | 134.01 ms | 391.18 ms | 24957.00 | 
| php | [symfony](http://symfony.com) (4.1) | 175.51 ms | 3.59 ms | 512.72 ms | 2511.97 ms | 6912.96 ms | 520427.33 | 
| php | [laravel](http://laravel.com) (5.7) | 273.30 ms | 4.19 ms | 712.37 ms | 4314.95 ms | 6587.67 ms | 840773.00 | 
| go | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 6.49 ms | 5.67 ms | 9.89 ms | 19.70 ms | 116.64 ms | 4036.33 | 
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 6.79 ms | 5.84 ms | 11.89 ms | 19.77 ms | 46.97 ms | 3930.67 | 
| rust | [actix-web](http://actix.rs) (0.7) | 7.08 ms | 6.13 ms | 13.15 ms | 21.66 ms | 49.16 ms | 4611.33 | 
| ruby | [rails](http://rubyonrails.org) (5.2) | 31.49 ms | 6.52 ms | 102.96 ms | 200.98 ms | 469.15 ms | 47866.33 | 
| python | [vibora](http://vibora.io) (0.0) | 7.95 ms | 6.74 ms | 15.30 ms | 26.32 ms | 63.48 ms | 5688.00 | 
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 8.47 ms | 7.51 ms | 14.71 ms | 25.15 ms | 190.01 ms | 7443.67 | 
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (1.1) | 11.45 ms | 9.70 ms | 18.64 ms | 31.85 ms | 195.47 ms | 8597.67 | 
| java | [act](http://actframework.org) (1.8) | 13.50 ms | 10.60 ms | 20.59 ms | 78.63 ms | 441.96 ms | 18584.67 | 
| scala | [akkahttp](http://akka.io) (10.0) | 193.88 ms | 10.69 ms | 47.00 ms | 4157.69 ms | 7185.80 ms | 745354.67 | 
| go | [echo](http://echo.labstack.com) (3.3) | 12.84 ms | 10.96 ms | 20.11 ms | 39.42 ms | 308.69 ms | 10197.33 | 
| go | [iris](http://iris-go.com) (10.7) | 12.79 ms | 11.14 ms | 19.69 ms | 39.10 ms | 302.08 ms | 9452.00 | 
| go | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 15.07 ms | 12.45 ms | 24.34 ms | 50.41 ms | 347.78 ms | 14509.67 | 
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 34.87 ms | 13.83 ms | 59.64 ms | 368.18 ms | 827.37 ms | 69533.33 | 
| node | [rayo](http://rayo.js.org) (1.2) | 24.08 ms | 15.00 ms | 36.30 ms | 219.75 ms | 940.40 ms | 48359.00 | 
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 18.80 ms | 15.63 ms | 32.46 ms | 61.51 ms | 299.83 ms | 12350.00 | 
| go | [beego](http://beego.me) (1.10) | 19.18 ms | 15.77 ms | 33.32 ms | 66.23 ms | 294.14 ms | 14009.00 | 
| node | [polka](http://github.com/lukeed/polka) (0.5) | 30.00 ms | 17.16 ms | 43.43 ms | 368.22 ms | 1143.18 ms | 68335.00 | 
| python | [bottle](http://bottlepy.org) (0.12) | 23.05 ms | 18.51 ms | 39.35 ms | 79.05 ms | 446.70 ms | 17905.67 | 
| node | [fastify](http://fastify.io) (1.12) | 29.62 ms | 19.77 ms | 40.07 ms | 297.75 ms | 1077.70 ms | 57318.00 | 
| swift | [vapor](http://vapor.codes) (3.0) | 43.78 ms | 20.08 ms | 41.41 ms | 812.59 ms | 2342.54 ms | 146080.67 | 
| node | [koa](http://koajs.com) (2.5) | 34.09 ms | 20.14 ms | 45.89 ms | 426.20 ms | 1252.10 ms | 76252.67 | 
| swift | [perfect](http://perfect.org) (3.0) | 21.04 ms | 20.52 ms | 25.96 ms | 31.94 ms | 255.20 ms | 7215.67 | 
| scala | [http4s](http://http4s.org) (0.0) | 27.02 ms | 23.27 ms | 45.86 ms | 84.14 ms | 1077.16 ms | 34267.67 | 
| node | [restify](http://restify.com) (7.2) | 35.27 ms | 26.75 ms | 57.45 ms | 183.88 ms | 732.27 ms | 39516.67 | 
| crystal | [prism](http://github.com/vladfaust/prism) (0.4 (beta 3)) | 30.63 ms | 27.73 ms | 42.57 ms | 50.97 ms | 247.29 ms | 8662.67 | 
| node | [express](http://expressjs.com) (4.16) | 58.73 ms | 30.11 ms | 69.16 ms | 918.49 ms | 2003.58 ms | 149929.33 | 
| crystal | [amber](http://amberframework.org) (0.9) | 33.97 ms | 30.43 ms | 48.38 ms | 58.12 ms | 263.15 ms | 12599.33 | 
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 32.29 ms | 30.65 ms | 42.18 ms | 50.38 ms | 246.28 ms | 10859.67 | 
| crystal | [kemal](http://kemalcr.com) (0.24) | 39.15 ms | 32.14 ms | 50.24 ms | 248.92 ms | 801.18 ms | 47121.00 | 
| swift | [kitura](http://kitura.io) (2.2) | 36.04 ms | 35.98 ms | 42.59 ms | 50.55 ms | 245.53 ms | 6711.67 | 
| node | [hapi](http://hapijs.com) (17.5) | 73.36 ms | 36.80 ms | 79.02 ms | 1140.37 ms | 2203.80 ms | 182246.67 | 
| crystal | [lucky](http://luckyframework.org) (0.11) | 41.04 ms | 40.03 ms | 48.87 ms | 56.00 ms | 330.26 ms | 8742.33 | 
| python | [flask](http://flask.pocoo.org) (1.0) | 54.96 ms | 42.74 ms | 94.01 ms | 170.34 ms | 398.06 ms | 31149.33 | 
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 52.79 ms | 44.70 ms | 96.05 ms | 162.45 ms | 331.44 ms | 33819.33 | 
| python | [sanic](http://github.com/huge-success/sanic) (0.8) | 63.08 ms | 57.20 ms | 104.25 ms | 166.22 ms | 336.52 ms | 32062.00 | 
| python | [django](http://djangoproject.com) (2.1) | 95.00 ms | 61.27 ms | 203.79 ms | 329.74 ms | 561.27 ms | 70532.67 | 
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 174.36 ms | 160.14 ms | 290.33 ms | 405.16 ms | 595.48 ms | 83998.67 | 
| python | [tornado](http://tornadoweb.org) (5.1) | 197.70 ms | 176.16 ms | 306.56 ms | 675.14 ms | 1934.33 ms | 130762.33 | 

### Requests per seconds


#### Ranking (top 5)


:one: (fasthttprouter) (go)


:two: (spider-gazelle) (crystal)


:three: (actix-web) (rust)


:four: (vibora) (python)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| go | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 147391.33 | 239.17 MB |
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 142207.67 | 152.05 MB |
| rust | [actix-web](http://actix.rs) (0.7) | 136537.00 | 155.16 MB |
| python | [vibora](http://vibora.io) (0.0) | 133722.00 | 151.89 MB |
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 117067.67 | 113.56 MB |
| java | [act](http://actframework.org) (1.8) | 89431.00 | 152.95 MB |
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (1.1) | 88177.33 | 143.87 MB |
| go | [iris](http://iris-go.com) (10.7) | 78686.33 | 104.78 MB |
| go | [echo](http://echo.labstack.com) (3.3) | 78664.67 | 137.96 MB |
| go | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 70184.33 | 94.72 MB |
| rust | [iron](http://ironframework.io) (0.7) | 62634.33 | 77.97 MB |
| rust | [nickel](http://nickel.rs) (0.10) | 59611.33 | 118.44 MB |
| rust | [rocket](http://nickel-org.github.io) (0.3) | 59181.67 | 91.62 MB |
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 58103.67 | 101.83 MB |
| go | [beego](http://beego.me) (1.10) | 55793.67 | 75.60 MB |
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 55081.67 | 96.65 MB |
| node | [rayo](http://rayo.js.org) (1.2) | 53387.33 | 80.07 MB |
| php | [laravel](http://laravel.com) (5.7) | 50171.00 | 249.84 MB |
| swift | [perfect](http://perfect.org) (3.0) | 48893.00 | 45.98 MB |
| node | [polka](http://github.com/lukeed/polka) (0.5) | 47441.00 | 71.13 MB |
| scala | [akkahttp](http://akka.io) (10.0) | 46935.00 | 100.79 MB |
| python | [bottle](http://bottlepy.org) (0.12) | 46640.00 | 115.00 MB |
| php | [symfony](http://symfony.com) (4.1) | 45047.33 | 224.16 MB |
| ruby | [roda](http://roda.jeremyevans.net) (3.12) | 44617.33 | 42.65 MB |
| node | [fastify](http://fastify.io) (1.12) | 43519.00 | 108.11 MB |
| node | [koa](http://koajs.com) (2.5) | 41206.33 | 87.29 MB |
| swift | [vapor](http://vapor.codes) (3.0) | 40091.00 | 67.62 MB |
| scala | [http4s](http://http4s.org) (0.0) | 39456.33 | 69.06 MB |
| crystal | [prism](http://github.com/vladfaust/prism) (0.4 (beta 3)) | 32513.67 | 36.58 MB |
| node | [restify](http://restify.com) (7.2) | 31266.33 | 54.81 MB |
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 31090.67 | 17.95 MB |
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 30869.00 | 28.93 MB |
| crystal | [amber](http://amberframework.org) (0.9) | 29118.00 | 42.16 MB |
| node | [express](http://expressjs.com) (4.16) | 28723.67 | 70.35 MB |
| crystal | [kemal](http://kemalcr.com) (0.24) | 28112.00 | 45.85 MB |
| swift | [kitura](http://kitura.io) (2.2) | 27601.00 | 51.32 MB |
| crystal | [lucky](http://luckyframework.org) (0.11) | 24463.67 | 29.98 MB |
| node | [hapi](http://hapijs.com) (17.5) | 23553.33 | 61.14 MB |
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 20089.00 | 45.53 MB |
| python | [flask](http://flask.pocoo.org) (1.0) | 18600.67 | 45.67 MB |
| python | [sanic](http://github.com/huge-success/sanic) (0.8) | 15856.00 | 28.24 MB |
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 15235.33 | 39.60 MB |
| ruby | [flame](http://github.com/AlexWayfer/flame) (5.0 (rc)) | 14583.67 | 8.43 MB |
| ruby | [hanami](http://hanamirb.org) (1.2) | 12728.67 | 96.47 MB |
| python | [django](http://djangoproject.com) (2.1) | 11648.33 | 33.74 MB |
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 5729.67 | 11.45 MB |
| python | [tornado](http://tornadoweb.org) (5.1) | 5033.67 | 13.45 MB |
| ruby | [rails](http://rubyonrails.org) (5.2) | 4124.33 | 12.43 MB |
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

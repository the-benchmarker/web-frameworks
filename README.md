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
Last update: 2018-10-19
```
OS: Linux (version: 3.10.0-862.14.4.el7.x86_64, arch: x86_64)
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
| rust | [nickel](http://nickel-org.github.io) (0.10) | 1.06 ms | 0.07 ms | 0.11 ms | 0.15 ms | 3060.91 ms | 44753.00 | 
| rust | [rocket](http://rocket.rs) (0.3) | 1.11 ms | 0.09 ms | 0.14 ms | 0.25 ms | 3078.76 ms | 45140.67 | 
| rust | [iron](http://ironframework.io) (0.6) | 1.38 ms | 0.29 ms | 0.69 ms | 1.83 ms | 3091.52 ms | 45473.00 | 
| ruby | [roda](http://roda.jeremyevans.net) (3.13) | 1.88 ms | 1.17 ms | 4.03 ms | 11.51 ms | 254.48 ms | 3145.67 | 
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 2.88 ms | 1.77 ms | 5.76 ms | 18.81 ms | 362.38 ms | 6051.67 | 
| rust | [actix-web](http://actix.rs) (0.7) | 4.40 ms | 2.83 ms | 10.16 ms | 25.52 ms | 104.43 ms | 5069.00 | 
| go | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 4.12 ms | 2.89 ms | 8.79 ms | 14.76 ms | 172.02 ms | 3818.33 | 
| python | [vibora](http://vibora.io) (0.0) | 5.66 ms | 3.00 ms | 15.22 ms | 29.36 ms | 66.28 ms | 6799.33 | 
| ruby | [flame](http://github.com/AlexWayfer/flame) (4.18) | 4.37 ms | 3.06 ms | 8.69 ms | 22.34 ms | 351.77 ms | 6355.33 | 
| ruby | [hanami](http://hanamirb.org) (1.2) | 4.54 ms | 3.21 ms | 8.88 ms | 25.73 ms | 335.48 ms | 6424.00 | 
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 5.48 ms | 4.13 ms | 11.01 ms | 23.27 ms | 371.13 ms | 5752.67 | 
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 4.99 ms | 4.59 ms | 10.56 ms | 12.70 ms | 104.42 ms | 3540.67 | 
| php | [slim](http://slimframework.com) (3.11) | 110.11 ms | 4.73 ms | 230.54 ms | 1918.81 ms | 3610.33 ms | 354651.33 | 
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 6.11 ms | 4.96 ms | 11.65 ms | 21.61 ms | 52.78 ms | 4513.67 | 
| java | [act](http://actframework.org) (1.8) | 7.41 ms | 5.21 ms | 13.98 ms | 31.21 ms | 187.12 ms | 6719.67 | 
| php | [symfony](http://symfony.com) (4.1) | 94.74 ms | 5.24 ms | 194.82 ms | 1409.90 ms | 2159.08 ms | 274627.67 | 
| c | [kore](http://kore.io) (3.1) | 6.02 ms | 5.30 ms | 9.00 ms | 11.23 ms | 2183.19 ms | 25708.33 | 
| nim | [jester](http://github.com/dom96/jester) (0.4) | 6.14 ms | 5.39 ms | 11.49 ms | 22.21 ms | 68.70 ms | 4628.67 | 
| go | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 6.76 ms | 5.45 ms | 12.09 ms | 21.50 ms | 163.99 ms | 5259.67 | 
| go | [iris](http://iris-go.com) (10.7) | 6.83 ms | 5.48 ms | 12.21 ms | 21.97 ms | 151.39 ms | 4651.00 | 
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 6.06 ms | 5.52 ms | 10.00 ms | 16.68 ms | 205.20 ms | 4472.00 | 
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 7.34 ms | 6.16 ms | 13.30 ms | 22.72 ms | 199.90 ms | 5117.00 | 
| go | [beego](http://beego.me) (1.10) | 7.71 ms | 6.43 ms | 13.69 ms | 24.20 ms | 187.27 ms | 5762.00 | 
| go | [echo](http://echo.labstack.com) (3.3) | 7.64 ms | 6.49 ms | 13.62 ms | 23.46 ms | 109.07 ms | 4861.33 | 
| php | [laravel](http://laravel.com) (5.7) | 156.21 ms | 6.81 ms | 193.75 ms | 2690.73 ms | 3982.31 ms | 511299.00 | 
| go | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 8.04 ms | 6.89 ms | 14.33 ms | 24.45 ms | 131.16 ms | 5355.33 | 
| node | [polka](http://github.com/lukeed/polka) (0.5) | 10.33 ms | 7.54 ms | 17.57 ms | 46.10 ms | 434.40 ms | 17532.00 | 
| node | [rayo](http://rayo.js.org) (1.2) | 14.45 ms | 8.10 ms | 23.95 ms | 135.65 ms | 662.80 ms | 33187.33 | 
| scala | [akkahttp](http://akka.io) (10.1) | 60.89 ms | 8.36 ms | 13.92 ms | 1602.62 ms | 3591.37 ms | 272402.67 | 
| node | [fastify](http://fastify.io) (1.12) | 13.20 ms | 8.94 ms | 19.78 ms | 69.97 ms | 689.72 ms | 29542.00 | 
| scala | [http4s](http://http4s.org) (0.18) | 11.17 ms | 9.42 ms | 20.18 ms | 39.93 ms | 278.27 ms | 9015.67 | 
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 24.79 ms | 10.27 ms | 63.03 ms | 234.79 ms | 413.41 ms | 48541.67 | 
| python | [bottle](http://bottlepy.org) (0.12) | 10.18 ms | 10.94 ms | 12.81 ms | 24.77 ms | 408.79 ms | 12298.33 | 
| node | [koa](http://koajs.com) (2.5) | 21.66 ms | 11.35 ms | 30.84 ms | 304.08 ms | 1112.53 ms | 62908.00 | 
| swift | [vapor](http://vapor.codes) (3.0) | 12.39 ms | 11.35 ms | 16.95 ms | 27.33 ms | 358.66 ms | 10653.00 | 
| swift | [perfect](http://perfect.org) (3.0) | 12.24 ms | 12.17 ms | 16.51 ms | 21.71 ms | 160.94 ms | 3983.67 | 
| node | [express](http://expressjs.com) (4.16) | 27.82 ms | 14.17 ms | 38.42 ms | 435.92 ms | 1290.67 ms | 79292.33 | 
| node | [restify](http://restify.com) (7.2) | 21.75 ms | 14.75 ms | 37.24 ms | 117.59 ms | 720.12 ms | 34527.33 | 
| ruby | [rails](http://rubyonrails.org) (5.2) | 26.00 ms | 15.33 ms | 65.09 ms | 139.31 ms | 345.02 ms | 29926.33 | 
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 18.18 ms | 16.28 ms | 27.48 ms | 39.98 ms | 170.48 ms | 8391.33 | 
| node | [hapi](http://hapijs.com) (17.6) | 43.30 ms | 20.83 ms | 53.51 ms | 698.96 ms | 1637.46 ms | 118821.33 | 
| python | [flask](http://flask.pocoo.org) (1.0) | 25.94 ms | 21.80 ms | 38.78 ms | 64.91 ms | 718.25 ms | 25011.33 | 
| swift | [kitura](http://kitura.io) (2.5) | 22.65 ms | 22.24 ms | 27.86 ms | 35.95 ms | 224.29 ms | 5199.33 | 
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 39.23 ms | 37.50 ms | 52.61 ms | 62.35 ms | 323.03 ms | 12429.00 | 
| crystal | [lucky](http://luckyframework.org) (0.11) | 43.21 ms | 40.72 ms | 56.89 ms | 65.50 ms | 239.79 ms | 9801.33 | 
| crystal | [orion](http://github.com/obsidian/orion) (1.6) | 45.18 ms | 41.08 ms | 65.67 ms | 76.91 ms | 333.59 ms | 13895.67 | 
| python | [django](http://djangoproject.com) (2.1) | 47.44 ms | 43.08 ms | 71.50 ms | 118.27 ms | 636.55 ms | 26420.00 | 
| crystal | [amber](http://amberframework.org) (0.9) | 46.89 ms | 45.95 ms | 63.29 ms | 71.72 ms | 257.51 ms | 12388.67 | 
| crystal | [kemal](http://kemalcr.com) (0.24) | 48.67 ms | 48.47 ms | 60.42 ms | 68.92 ms | 490.71 ms | 15492.67 | 
| python | [sanic](http://github.com/huge-success/sanic) (0.8) | 54.37 ms | 49.55 ms | 92.88 ms | 143.20 ms | 270.63 ms | 28540.67 | 
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 68.32 ms | 63.44 ms | 105.96 ms | 148.96 ms | 286.09 ms | 27049.67 | 
| python | [tornado](http://tornadoweb.org) (5.1) | 68.29 ms | 65.88 ms | 88.57 ms | 106.67 ms | 279.90 ms | 16632.00 | 

### Requests per seconds


#### Ranking (top 5)


:one: (actix-web) (rust)


:two: (vibora) (python)


:three: (fasthttprouter) (go)


:four: (act) (java)


:five: (evhtp) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| rust | [actix-web](http://actix.rs) (0.7) | 260062.00 | 295.63 MB |
| python | [vibora](http://vibora.io) (0.0) | 251448.67 | 285.33 MB |
| go | [fasthttprouter](http://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 244171.00 | 394.08 MB |
| java | [act](http://actframework.org) (1.8) | 198801.00 | 340.41 MB |
| cpp | [evhtp](http://github.com/criticalstack/libevhtp) (1.2) | 186628.67 | 181.19 MB |
| nim | [jester](http://github.com/dom96/jester) (0.4) | 171393.67 | 344.71 MB |
| crystal | [spider-gazelle](http://spider-gazelle.net) (1.1) | 163942.33 | 175.39 MB |
| csharp | [aspnetcore](http://docs.microsoft.com/en-us/aspnet/index) (2.1) | 155957.67 | 254.02 MB |
| go | [muxie](http://godoc.org/github.com/kataras/muxie) (1.0) | 144736.67 | 193.46 MB |
| go | [iris](http://iris-go.com) (10.7) | 144653.67 | 193.04 MB |
| rust | [iron](http://ironframework.io) (0.6) | 139480.67 | 176.15 MB |
| go | [gin](http://gin-gonic.github.io/gin) (1.3) | 132289.67 | 232.29 MB |
| go | [beego](http://beego.me) (1.10) | 128540.33 | 173.21 MB |
| go | [echo](http://echo.labstack.com) (3.3) | 128243.67 | 225.22 MB |
| go | [gorilla-mux](http://www.gorillatoolkit.org/pkg/mux) (1.6) | 122244.00 | 162.59 MB |
| node | [polka](http://github.com/lukeed/polka) (0.5) | 111937.33 | 167.73 MB |
| rust | [rocket](http://rocket.rs) (0.3) | 99878.33 | 158.35 MB |
| node | [fastify](http://fastify.io) (1.12) | 97985.67 | 238.84 MB |
| python | [bottle](http://bottlepy.org) (0.12) | 97906.33 | 241.22 MB |
| node | [rayo](http://rayo.js.org) (1.2) | 94858.33 | 142.15 MB |
| scala | [http4s](http://http4s.org) (0.18) | 91947.00 | 161.35 MB |
| nim | [mofuw](http://github.com/2vg/mofuw) (2.0) | 89898.67 | 157.89 MB |
| scala | [akkahttp](http://akka.io) (10.1) | 84066.00 | 180.66 MB |
| rust | [nickel](http://nickel-org.github.io) (0.10) | 81982.00 | 163.55 MB |
| swift | [perfect](http://perfect.org) (3.0) | 79166.00 | 74.44 MB |
| swift | [vapor](http://vapor.codes) (3.0) | 77450.33 | 130.43 MB |
| node | [koa](http://koajs.com) (2.5) | 72431.67 | 153.36 MB |
| php | [symfony](http://symfony.com) (4.1) | 70260.00 | 349.71 MB |
| php | [slim](http://slimframework.com) (3.11) | 68998.33 | 343.44 MB |
| ruby | [roda](http://roda.jeremyevans.net) (3.13) | 66568.33 | 63.61 MB |
| php | [laravel](http://laravel.com) (5.7) | 60869.00 | 303.30 MB |
| node | [express](http://expressjs.com) (4.16) | 58911.00 | 144.25 MB |
| node | [restify](http://restify.com) (7.2) | 55899.33 | 98.10 MB |
| python | [aiohttp](http://aiohttp.readthedocs.io) (3.4) | 54381.33 | 123.30 MB |
| c | [kore](http://kore.io) (3.1) | 51623.33 | 139.94 MB |
| ruby | [rack-routing](http://github.com/georgeu2000/rack-routing) (0.0) | 45569.67 | 26.35 MB |
| swift | [kitura](http://kitura.io) (2.5) | 43858.67 | 81.63 MB |
| node | [hapi](http://hapijs.com) (17.6) | 41322.00 | 107.21 MB |
| python | [flask](http://flask.pocoo.org) (1.0) | 38908.33 | 95.91 MB |
| ruby | [flame](http://github.com/AlexWayfer/flame) (4.18) | 29364.67 | 16.97 MB |
| ruby | [hanami](http://hanamirb.org) (1.2) | 28460.67 | 215.66 MB |
| crystal | [router.cr](http://github.com/tbrand/router.cr) (0.2) | 24690.67 | 23.13 MB |
| ruby | [sinatra](http://sinatrarb.com) (2.0) | 23024.00 | 59.83 MB |
| crystal | [lucky](http://luckyframework.org) (0.11) | 22502.67 | 27.66 MB |
| crystal | [orion](http://github.com/obsidian/orion) (1.6) | 21524.67 | 35.04 MB |
| python | [django](http://djangoproject.com) (2.1) | 20999.33 | 60.98 MB |
| crystal | [amber](http://amberframework.org) (0.9) | 20638.00 | 29.89 MB |
| crystal | [kemal](http://kemalcr.com) (0.24) | 20472.67 | 33.35 MB |
| python | [sanic](http://github.com/huge-success/sanic) (0.8) | 18464.00 | 32.99 MB |
| python | [quart](http://pgjones.gitlab.io/quart) (0.6) | 14372.67 | 28.65 MB |
| python | [tornado](http://tornadoweb.org) (5.1) | 13993.33 | 37.59 MB |
| ruby | [rails](http://rubyonrails.org) (5.2) | 4912.00 | 15.08 MB |
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

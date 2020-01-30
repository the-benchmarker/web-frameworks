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
+ [postgresql](https://www.postgresql.org) to store data, `>= 10`

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

+ Create and initialize the database

~~~sh
createdb -U postgres benchmark
psql -U postgres -d benchmark < .ci/dump.sql
export DATABASE_URL="postgresql://postgres@localhost/benchmark"
~~~

+ Make configuration

~~~sh
bin/make config
~~~

+ Build containers

> jobs are either languages (example : crystal) or frameworks (example : router.cr)

~~~sh
bin/neph [job1] [job2] [job3] ...
~~~

+ Export all results readme

~~~sh
bin/db to_readme
~~~

## Results

:information_source:  Updated on **2020-01-30** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 128
   + Connections : 128
   + Duration : 10 (seconds)

|    | Language | Framework | Speed (`req/s`) | Horizontal scale (parallelism) | Vertical scale (concurrency) |
|----|----------|-----------|----------------:|-------------|-------------|
| 1 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 161 699 | | |
| 2 | javascript (13.7)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 161 177 | | |
| 3 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 155 895 | | |
| 4 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 144 700 | | |
| 5 | crystal (0.31)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 143 230 | | |
| 6 | crystal (0.31)| [toro](https://github.com/soveran/toro) (0.4) | 142 935 | | |
| 7 | crystal (0.31)| [raze](https://razecr.com) (0.3) | 139 317 | | |
| 8 | crystal (0.31)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 138 666 | | |
| 9 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.8) | 136 406 | | |
| 10 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 132 838 | | |
| 11 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 132 509 | | |
| 12 | crystal (0.31)| [kemal](https://kemalcr.com) (0.26) | 132 410 | | |
| 13 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.2) | 131 176 | | |
| 14 | go (1.13)| [router](https://github.com/fasthttp/router) (0.6) | 130 303 | | |
| 15 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 129 366 | | |
| 16 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 128 389 | | |
| 17 | crystal (0.31)| [grip](https://github.com/grkek/grip) (0.26) | 128 261 | | |
| 18 | go (1.13)| [fiber](https://gofiber.github.io/fiber) (1.0) | 127 871 | | |
| 19 | crystal (0.31)| [amber](https://amberframework.org) (0.3) | 122 390 | | |
| 20 | crystal (0.31)| [orion](https://github.com/obsidian/orion) (2.1) | 119 662 | | |
| 21 | php (7.4)| [workerman](https://github.com/workerman/workerman) (3.5) | 115 103 | | |
| 22 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.12) | 106 063 | | |
| 23 | java (8)| [act](https://actframework.org) (1.8) | 103 324 | | |
| 24 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 95 341 | | |
| 25 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 91 316 | | |
| 26 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 89 638 | | |
| 27 | csharp (7.3)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.0) | 89 013 | | |
| 28 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 88 416 | | |
| 29 | go (1.13)| [violetear](https://violetear.org) (7.0) | 87 747 | | |
| 30 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 86 843 | | |
| 31 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 86 585 | | |
| 32 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 85 939 | | |
| 33 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 84 545 | | |
| 34 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 84 447 | | |
| 35 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 83 877 | | |
| 36 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.0) | 82 793 | | |
| 37 | go (1.13)| [beego](https://beego.me) (1.12) | 82 610 | | |
| 38 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 82 168 | | |
| 39 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 81 856 | | |
| 40 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.0) | 78 669 | | |
| 41 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 76 881 | | |
| 42 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 73 644 | | |
| 43 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 73 120 | | |
| 44 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 72 807 | | |
| 45 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 70 443 | | |
| 46 | go (1.13)| [gf](https://goframe.org) (1.11) | 70 241 | | |
| 47 | java (8)| [javalin](https://javalin.io) (3.5) | 66 327 | | |
| 48 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 66 087 | | |
| 49 | javascript (13.7)| [fastify](https://fastify.io) (2.11) | 61 893 | | |
| 50 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 61 127 | | |
| 51 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 60 627 | | |
| 52 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 60 118 | | |
| 53 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 58 614 | | |
| 54 | c (11)| [kore](https://kore.io) (3.3) | 56 711 | | |
| 55 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 55 898 | | |
| 56 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 55 749 | | |
| 57 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 55 152 | | |
| 58 | clojure (1.10)| [coast](https://coastonclojure.com) (1.0) | 54 053 | | |
| 59 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 52 553 | | |
| 60 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 51 903 | | |
| 61 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 50 595 | | |
| 62 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 50 402 | | |
| 63 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 49 038 | | |
| 64 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 48 824 | | |
| 65 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 47 637 | | |
| 66 | python (3.8)| [starlette](https://starlette.io) (0.13) | 46 921 | | |
| 67 | rust (1.4)| [nickel](https://nickel-org.github.io) (0.11) | 46 898 | | |
| 68 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 46 883 | | |
| 69 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 45 520 | | |
| 70 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 44 727 | | |
| 71 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 44 657 | | |
| 72 | rust (1.4)| [gotham](https://gotham.rs) (0.4) | 44 597 | | |
| 73 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 42 753 | | |
| 74 | php (7.4)| [imi](https://imiphp.com) (1.0) | 42 264 | | |
| 75 | python (3.8)| [hug](https://hug.rest) (2.6) | 41 637 | | |
| 76 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 40 713 | | |
| 77 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.28) | 40 565 | | |
| 78 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 39 034 | | |
| 79 | javascript (13.7)| [hapi](https://hapijs.com) (19.0) | 37 889 | | |
| 80 | java (8)| [micronaut](https://micronaut.io) (1.2) | 37 817 | | |
| 81 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 36 626 | | |
| 82 | javascript (13.7)| [restify](https://restify.com) (8.5) | 36 247 | | |
| 83 | php (7.4)| [swoft](https://swoft.org) (2.0) | 35 766 | | |
| 84 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.47) | 31 333 | | |
| 85 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 30 133 | | |
| 86 | python (3.8)| [responder](https://python-responder.org) (2.0) | 28 514 | | |
| 87 | fsharp (7.3)| [suave](https://suave.io) (2.5) | 28 463 | | |
| 88 | crystal (0.31)| [lucky](https://luckyframework.org) (0.18) | 28 194 | | |
| 89 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 27 317 | | |
| 90 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 27 268 | | |
| 91 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 27 197 | | |
| 92 | javascript (13.7)| [moleculer](https://moleculer.services) (0.13) | 26 457 | | |
| 93 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 26 338 | | |
| 94 | crystal (0.31)| [athena](https://github.com/blacksmoke16/athena) (0.7) | 25 549 | | |
| 95 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 25 495 | | |
| 96 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 24 726 | | |
| 97 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 24 266 | | |
| 98 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 23 177 | | |
| 99 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 23 127 | | |
| 100 | python (3.8)| [bocadillo](https://bocadilloproject.github.io) (0.18) | 22 125 | | |
| 101 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 21 997 | | |
| 102 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 20 597 | | |
| 103 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a1) | 19 633 | | |
| 104 | rust (1.4)| [iron](https://ironframework.io) (0.6) | 18 673 | | |
| 105 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 17 806 | | |
| 106 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 16 369 | | |
| 107 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 15 561 | | |
| 108 | ruby (2.6)| [grape](https://ruby-grape.org) (1.3) | 12 907 | | |
| 109 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.10) | 12 601 | | |
| 110 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 241 | | |
| 111 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 724 | | |
| 112 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 10 623 | | |
| 113 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 10 260 | | |
| 114 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 8 786 | | |
| 115 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 482 | | |
| 116 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 7 330 | | |
| 117 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 098 | | |
| 118 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 6 912 | | |
| 119 | crystal (0.31)| [onyx](https://onyxframework.org) (0.5) | 5 031 | | |
| 120 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 4 520 | | |
| 121 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 334 | | |
| 122 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 4 028 | | |
| 123 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 3 801 | | |
| 124 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 410 | | |
| 125 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 3 305 | | |
| 126 | php (7.4)| [symfony](https://symfony.com) (4.3) | 2 882 | | |
| 127 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 297 | | |
| 128 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 1 817 | | |
| 129 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 598 | | |
| 130 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 562 | | |
| 131 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 1 416 | | |
| 132 | php (7.4)| [laravel](https://laravel.com) (6.12) | 413 | | |

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

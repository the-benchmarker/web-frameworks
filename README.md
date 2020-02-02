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

:information_source:  Updated on **2020-02-02** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 128
   + Connections : 128
   + Duration : 10 (seconds)

|    | Language | Framework | Speed (`req/s`) | Horizontal scale (parallelism) | Vertical scale (concurrency) |
|----|----------|-----------|----------------:|-------------|-------------|
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 159 722 | | |
| 2 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 159 006 | | |
| 3 | javascript (13.7)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 150 597 | | |
| 4 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 140 038 | | |
| 5 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 139 313 | | |
| 6 | crystal (0.31)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 139 026 | | |
| 7 | crystal (0.31)| [toro](https://github.com/soveran/toro) (0.4) | 138 985 | | |
| 8 | crystal (0.31)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 136 858 | | |
| 9 | crystal (0.31)| [raze](https://razecr.com) (0.3) | 136 461 | | |
| 10 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 134 594 | | |
| 11 | crystal (0.31)| [kemal](https://kemalcr.com) (0.26) | 133 705 | | |
| 12 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.8) | 130 947 | | |
| 13 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 127 386 | | |
| 14 | go (1.13)| [router](https://github.com/fasthttp/router) (0.6) | 127 095 | | |
| 15 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.3) | 126 698 | | |
| 16 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 126 007 | | |
| 17 | crystal (0.31)| [grip](https://github.com/grkek/grip) (0.26) | 125 787 | | |
| 18 | go (1.13)| [fiber](https://gofiber.github.io/fiber) (1.2) | 125 148 | | |
| 19 | crystal (0.31)| [amber](https://amberframework.org) (0.3) | 124 742 | | |
| 20 | crystal (0.31)| [orion](https://github.com/obsidian/orion) (2.1) | 120 099 | | |
| 21 | php (7.4)| [workerman](https://github.com/workerman/workerman) (3.5) | 115 754 | | |
| 22 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.12) | 99 724 | | |
| 23 | java (8)| [act](https://actframework.org) (1.8) | 98 094 | | |
| 24 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 92 391 | | |
| 25 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 88 087 | | |
| 26 | csharp (7.3)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.0) | 87 013 | | |
| 27 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 86 411 | | |
| 28 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 86 171 | | |
| 29 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 85 842 | | |
| 30 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 85 407 | | |
| 31 | go (1.13)| [violetear](https://violetear.org) (7.0) | 84 002 | | |
| 32 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 82 748 | | |
| 33 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 82 482 | | |
| 34 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.0) | 82 114 | | |
| 35 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 81 693 | | |
| 36 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 81 131 | | |
| 37 | go (1.13)| [beego](https://beego.me) (1.12) | 79 695 | | |
| 38 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 78 789 | | |
| 39 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 78 185 | | |
| 40 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 75 813 | | |
| 41 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 74 020 | | |
| 42 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 73 750 | | |
| 43 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.0) | 73 011 | | |
| 44 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 70 266 | | |
| 45 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 67 842 | | |
| 46 | go (1.13)| [gf](https://goframe.org) (1.11) | 67 293 | | |
| 47 | c (11)| [kore](https://kore.io) (3.3) | 64 165 | | |
| 48 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 62 215 | | |
| 49 | javascript (13.7)| [fastify](https://fastify.io) (2.11) | 61 273 | | |
| 50 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 60 267 | | |
| 51 | java (8)| [javalin](https://javalin.io) (3.5) | 59 288 | | |
| 52 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.12) | 57 529 | | |
| 53 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 56 341 | | |
| 54 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 56 252 | | |
| 55 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 55 929 | | |
| 56 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 53 929 | | |
| 57 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 53 883 | | |
| 58 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 53 277 | | |
| 59 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 52 555 | | |
| 60 | clojure (1.10)| [coast](https://coastonclojure.com) (1.0) | 52 365 | | |
| 61 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 51 833 | | |
| 62 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 48 872 | | |
| 63 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 48 634 | | |
| 64 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 47 446 | | |
| 65 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 47 405 | | |
| 66 | java (8)| [micronaut](https://micronaut.io) (1.2) | 46 940 | | |
| 67 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 46 842 | | |
| 68 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 46 167 | | |
| 69 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 45 253 | | |
| 70 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 45 068 | | |
| 71 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 45 025 | | |
| 72 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 44 941 | | |
| 73 | python (3.8)| [hug](https://hug.rest) (2.6) | 44 598 | | |
| 74 | python (3.8)| [starlette](https://starlette.io) (0.13) | 42 658 | | |
| 75 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 40 987 | | |
| 76 | php (7.4)| [imi](https://imiphp.com) (1.0) | 40 977 | | |
| 77 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 39 514 | | |
| 78 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 38 908 | | |
| 79 | javascript (13.7)| [hapi](https://hapijs.com) (19.0) | 38 617 | | |
| 80 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.28) | 38 433 | | |
| 81 | javascript (13.7)| [restify](https://restify.com) (8.5) | 37 727 | | |
| 82 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 37 441 | | |
| 83 | php (7.4)| [swoft](https://swoft.org) (2.0) | 35 385 | | |
| 84 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 34 167 | | |
| 85 | javascript (13.7)| [moleculer](https://moleculer.services) (0.13) | 29 859 | | |
| 86 | python (3.8)| [responder](https://python-responder.org) (2.0) | 28 652 | | |
| 87 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.47) | 28 473 | | |
| 88 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 28 433 | | |
| 89 | crystal (0.31)| [lucky](https://luckyframework.org) (0.18) | 28 118 | | |
| 90 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 25 685 | | |
| 91 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 25 430 | | |
| 92 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 24 931 | | |
| 93 | crystal (0.31)| [athena](https://github.com/blacksmoke16/athena) (0.7) | 24 646 | | |
| 94 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 24 569 | | |
| 95 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 22 910 | | |
| 96 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 403 | | |
| 97 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 22 335 | | |
| 98 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 22 039 | | |
| 99 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 21 917 | | |
| 100 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a1) | 20 442 | | |
| 101 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 20 124 | | |
| 102 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 19 229 | | |
| 103 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 19 187 | | |
| 104 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 17 678 | | |
| 105 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 170 | | |
| 106 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 14 775 | | |
| 107 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.10) | 12 770 | | |
| 108 | ruby (2.6)| [grape](https://ruby-grape.org) (1.3) | 12 276 | | |
| 109 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 286 | | |
| 110 | fsharp (7.3)| [suave](https://suave.io) (2.5) | 10 867 | | |
| 111 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 971 | | |
| 112 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 717 | | |
| 113 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 400 | | |
| 114 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 8 394 | | |
| 115 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 308 | | |
| 116 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 396 | | |
| 117 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 7 195 | | |
| 118 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 053 | | |
| 119 | crystal (0.31)| [onyx](https://onyxframework.org) (0.5) | 5 436 | | |
| 120 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 4 539 | | |
| 121 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 526 | | |
| 122 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 4 202 | | |
| 123 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 3 579 | | |
| 124 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 507 | | |
| 125 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 3 506 | | |
| 126 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 281 | | |
| 127 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 336 | | |
| 128 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 1 887 | | |
| 129 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 549 | | |
| 130 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 515 | | |
| 131 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 1 414 | | |
| 132 | php (7.4)| [laravel](https://laravel.com) (6.12) | 418 | | |

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

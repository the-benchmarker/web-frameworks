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

:information_source:  Updated on **2020-01-29** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 128
   + Connections : 128
   + Duration : 10 (seconds)

|    | Language | Framework | Speed (`req/s`) | Horizontal scale (parallelism) | Vertical scale (concurrency) |
|----|----------|-----------|----------------:|-------------|-------------|
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 166 073 | | |
| 2 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 162 050 | | |
| 3 | javascript (13.7)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 159 304 | | |
| 4 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 148 491 | | |
| 5 | crystal (0.31)| [toro](https://github.com/soveran/toro) (0.4) | 147 128 | | |
| 6 | crystal (0.31)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 144 402 | | |
| 7 | crystal (0.31)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 142 791 | | |
| 8 | crystal (0.31)| [raze](https://razecr.com) (0.3) | 138 769 | | |
| 9 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 136 202 | | |
| 10 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 135 184 | | |
| 11 | crystal (0.31)| [kemal](https://kemalcr.com) (0.26) | 134 621 | | |
| 12 | crystal (0.31)| [grip](https://github.com/grkek/grip) (0.26) | 132 160 | | |
| 13 | crystal (0.31)| [amber](https://amberframework.org) (0.3) | 128 491 | | |
| 14 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.2) | 125 624 | | |
| 15 | go (1.13)| [fiber](https://gofiber.github.io/fiber) (1.0) | 125 190 | | |
| 16 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 125 020 | | |
| 17 | crystal (0.31)| [orion](https://github.com/obsidian/orion) (2.1) | 124 795 | | |
| 18 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.1) | 122 910 | | |
| 19 | go (1.13)| [router](https://github.com/fasthttp/router) (0.6) | 122 628 | | |
| 20 | php (7.4)| [workerman](https://github.com/workerman/workerman) (3.5) | 119 600 | | |
| 21 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.8) | 108 603 | | |
| 22 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.12) | 105 469 | | |
| 23 | java (8)| [act](https://actframework.org) (1.8) | 98 085 | | |
| 24 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 91 750 | | |
| 25 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 88 759 | | |
| 26 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 88 067 | | |
| 27 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 86 588 | | |
| 28 | csharp (7.3)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.0) | 86 155 | | |
| 29 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.3) | 86 046 | | |
| 30 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 85 266 | | |
| 31 | go (1.13)| [beego](https://beego.me) (1.12) | 83 288 | | |
| 32 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.0) | 82 851 | | |
| 33 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 81 939 | | |
| 34 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 81 530 | | |
| 35 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 81 499 | | |
| 36 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 80 917 | | |
| 37 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.0) | 77 388 | | |
| 38 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 76 872 | | |
| 39 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 75 436 | | |
| 40 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 73 571 | | |
| 41 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 73 512 | | |
| 42 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 73 309 | | |
| 43 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 69 600 | | |
| 44 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 69 337 | | |
| 45 | c (11)| [kore](https://kore.io) (3.3) | 66 862 | | |
| 46 | go (1.13)| [gf](https://goframe.org) (1.11) | 66 418 | | |
| 47 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 63 592 | | |
| 48 | java (8)| [javalin](https://javalin.io) (3.5) | 61 700 | | |
| 49 | javascript (13.7)| [fastify](https://fastify.io) (2.11) | 61 338 | | |
| 50 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 60 416 | | |
| 51 | go (1.13)| [violetear](https://violetear.org) (7.0) | 59 936 | | |
| 52 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 58 154 | | |
| 53 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 56 501 | | |
| 54 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 55 212 | | |
| 55 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 54 903 | | |
| 56 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 54 451 | | |
| 57 | clojure (1.10)| [coast](https://coastonclojure.com) (1.0) | 53 457 | | |
| 58 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 52 209 | | |
| 59 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 52 006 | | |
| 60 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 48 778 | | |
| 61 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 48 587 | | |
| 62 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 48 224 | | |
| 63 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 47 649 | | |
| 64 | rust (1.4)| [nickel](https://nickel-org.github.io) (0.11) | 47 642 | | |
| 65 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 47 594 | | |
| 66 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 47 493 | | |
| 67 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 47 379 | | |
| 68 | python (3.8)| [hug](https://hug.rest) (2.6) | 46 392 | | |
| 69 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 46 314 | | |
| 70 | python (3.8)| [starlette](https://starlette.io) (0.13) | 44 959 | | |
| 71 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 44 864 | | |
| 72 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 44 287 | | |
| 73 | rust (1.4)| [gotham](https://gotham.rs) (0.4) | 44 169 | | |
| 74 | java (8)| [micronaut](https://micronaut.io) (1.2) | 42 507 | | |
| 75 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 42 481 | | |
| 76 | php (7.4)| [imi](https://imiphp.com) (1.0) | 42 058 | | |
| 77 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 41 494 | | |
| 78 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 41 056 | | |
| 79 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.28) | 40 348 | | |
| 80 | javascript (13.7)| [hapi](https://hapijs.com) (19.0) | 38 257 | | |
| 81 | javascript (13.7)| [restify](https://restify.com) (8.5) | 36 427 | | |
| 82 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 35 708 | | |
| 83 | php (7.4)| [swoft](https://swoft.org) (2.0) | 35 637 | | |
| 84 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.47) | 30 438 | | |
| 85 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 30 308 | | |
| 86 | python (3.8)| [responder](https://python-responder.org) (2.0) | 30 297 | | |
| 87 | javascript (13.7)| [moleculer](https://moleculer.services) (0.13) | 28 783 | | |
| 88 | crystal (0.31)| [lucky](https://luckyframework.org) (0.18) | 28 024 | | |
| 89 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 26 820 | | |
| 90 | fsharp (7.3)| [suave](https://suave.io) (2.5) | 26 419 | | |
| 91 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 26 068 | | |
| 92 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 25 457 | | |
| 93 | crystal (0.31)| [athena](https://github.com/blacksmoke16/athena) (0.7) | 25 390 | | |
| 94 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 23 863 | | |
| 95 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 23 294 | | |
| 96 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 23 090 | | |
| 97 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 22 777 | | |
| 98 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 607 | | |
| 99 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 22 209 | | |
| 100 | python (3.8)| [bocadillo](https://bocadilloproject.github.io) (0.18) | 20 899 | | |
| 101 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 20 655 | | |
| 102 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 19 961 | | |
| 103 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a1) | 19 624 | | |
| 104 | rust (1.4)| [iron](https://ironframework.io) (0.6) | 18 717 | | |
| 105 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 17 223 | | |
| 106 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 184 | | |
| 107 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 16 312 | | |
| 108 | ruby (2.6)| [grape](https://ruby-grape.org) (1.3) | 13 614 | | |
| 109 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.10) | 13 296 | | |
| 110 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 351 | | |
| 111 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 469 | | |
| 112 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 10 214 | | |
| 113 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 839 | | |
| 114 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 9 298 | | |
| 115 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 925 | | |
| 116 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 7 558 | | |
| 117 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 405 | | |
| 118 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 183 | | |
| 119 | crystal (0.31)| [onyx](https://onyxframework.org) (0.5) | 4 834 | | |
| 120 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 4 618 | | |
| 121 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 480 | | |
| 122 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 4 181 | | |
| 123 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 3 830 | | |
| 124 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 542 | | |
| 125 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 345 | | |
| 126 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 3 213 | | |
| 127 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 336 | | |
| 128 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 1 861 | | |
| 129 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 606 | | |
| 130 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 472 | | |
| 131 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 1 414 | | |
| 132 | php (7.4)| [laravel](https://laravel.com) (6.12) | 396 | | |

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

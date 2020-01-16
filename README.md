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

|    | Language | Framework | Speed (`req/s`) | Horizontal scale (parallelism) | Vertical scale (concurrency) |
|----|----------|-----------|----------------:|-------------|-------------|
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 196 919 | | |
| 2 | javascript (12.13)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 193 353 | | |
| 3 | javascript (12.13)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 184 062 | | |
| 4 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 182 586 | | |
| 5 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.8) | 171 769 | | |
| 6 | go (1.13)| [router](https://github.com/fasthttp/router) (1.6) | 167 387 | | |
| 7 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 166 979 | | |
| 8 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.1) | 164 220 | | |
| 9 | go (1.13)| [fiber](https://fenny.github.io/fiber/#/) (0.6) | 162 330 | | |
| 10 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.2) | 161 720 | | |
| 11 | c (11)| [kore](https://kore.io) (3.3) | 159 210 | | |
| 12 | crystal (0.31)| [toro](https://github.com/soveran/toro) (0.4) | 157 145 | | |
| 13 | crystal (0.31)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 156 240 | | |
| 14 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 154 680 | | |
| 15 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 153 757 | | |
| 16 | crystal (0.31)| [raze](https://razecr.com) (0.3) | 151 466 | | |
| 17 | crystal (0.31)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 149 405 | | |
| 18 | crystal (0.31)| [grip](https://github.com/grkek/grip) (0.26) | 147 688 | | |
| 19 | crystal (0.31)| [kemal](https://kemalcr.com) (0.26) | 145 521 | | |
| 20 | crystal (0.31)| [amber](https://amberframework.org) (0.3) | 136 456 | | |
| 21 | crystal (0.31)| [orion](https://github.com/obsidian/orion) (2.1) | 127 023 | | |
| 22 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.11) | 126 928 | | |
| 23 | php (7.4)| [workerman](https://github.com/workerman/workerman) (3.5) | 115 842 | | |
| 24 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 111 531 | | |
| 25 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.2) | 108 425 | | |
| 26 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 104 589 | | |
| 27 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 104 515 | | |
| 28 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 102 894 | | |
| 29 | csharp (7.3)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.0) | 102 224 | | |
| 30 | go (1.13)| [beego](https://beego.me) (1.12) | 101 273 | | |
| 31 | go (1.13)| [violetear](https://violetear.org) (7.0) | 100 277 | | |
| 32 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 99 668 | | |
| 33 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 99 070 | | |
| 34 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 98 787 | | |
| 35 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 97 858 | | |
| 36 | java (8)| [act](https://actframework.org) (1.8) | 93 632 | | |
| 37 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 93 623 | | |
| 38 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.0) | 90 545 | | |
| 39 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 87 642 | | |
| 40 | javascript (12.13)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 86 634 | | |
| 41 | javascript (12.13)| [0http](https://github.com/jkyberneees/0http) (2.2) | 83 377 | | |
| 42 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 83 337 | | |
| 43 | go (1.13)| [gf](https://goframe.org) (1.11) | 80 673 | | |
| 44 | javascript (12.13)| [restana](https://github.com/jkyberneees/ana) (4.0) | 76 261 | | |
| 45 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 76 192 | | |
| 46 | javascript (12.13)| [polka](https://github.com/lukeed/polka) (0.5) | 74 663 | | |
| 47 | javascript (12.13)| [rayo](https://rayo.js.org) (1.3) | 72 431 | | |
| 48 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 66 033 | | |
| 49 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 62 869 | | |
| 50 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 62 243 | | |
| 51 | javascript (12.13)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 61 992 | | |
| 52 | javascript (12.13)| [foxify](https://foxify.js.org) (0.1) | 59 736 | | |
| 53 | javascript (12.13)| [fastify](https://fastify.io) (2.11) | 59 681 | | |
| 54 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 57 123 | | |
| 55 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 52 905 | | |
| 56 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 52 561 | | |
| 57 | javascript (12.13)| [koa](https://koajs.com) (2.11) | 51 994 | | |
| 58 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 51 925 | | |
| 59 | javascript (12.13)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 51 432 | | |
| 60 | rust (1.4)| [gotham](https://gotham.rs) (0.4) | 50 221 | | |
| 61 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 50 173 | | |
| 62 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 49 783 | | |
| 63 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 49 540 | | |
| 64 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 48 857 | | |
| 65 | python (3.8)| [starlette](https://starlette.io) (0.13) | 47 818 | | |
| 66 | python (3.8)| [hug](https://hug.rest) (2.6) | 46 437 | | |
| 67 | javascript (12.13)| [express](https://expressjs.com) (4.17) | 44 864 | | |
| 68 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 44 819 | | |
| 69 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 44 503 | | |
| 70 | java (8)| [javalin](https://javalin.io) (3.5) | 43 790 | | |
| 71 | php (7.4)| [imi](https://imiphp.com) (1.0) | 43 412 | | |
| 72 | clojure (1.10)| [coast](https://coastonclojure.com) (1.0) | 43 042 | | |
| 73 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 42 635 | | |
| 74 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 39 558 | | |
| 75 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.27) | 37 737 | | |
| 76 | javascript (12.13)| [restify](https://restify.com) (8.5) | 35 702 | | |
| 77 | java (8)| [micronaut](https://micronaut.io) (1.2) | 35 239 | | |
| 78 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 34 388 | | |
| 79 | javascript (12.13)| [hapi](https://hapijs.com) (19.0) | 33 829 | | |
| 80 | php (7.4)| [swoft](https://swoft.org) (2.0) | 33 373 | | |
| 81 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 33 089 | | |
| 82 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 32 132 | | |
| 83 | fsharp (7.3)| [suave](https://suave.io) (2.5) | 32 104 | | |
| 84 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.46) | 31 432 | | |
| 85 | python (3.8)| [responder](https://python-responder.org) (2.0) | 31 019 | | |
| 86 | rust (1.4)| [nickel](https://nickel-org.github.io) (0.11) | 30 868 | | |
| 87 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 28 831 | | |
| 88 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 27 514 | | |
| 89 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 27 395 | | |
| 90 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 26 715 | | |
| 91 | javascript (12.13)| [moleculer](https://moleculer.services) (0.13) | 26 345 | | |
| 92 | crystal (0.31)| [lucky](https://luckyframework.org) (0.18) | 25 392 | | |
| 93 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 25 168 | | |
| 94 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 24 884 | | |
| 95 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 24 703 | | |
| 96 | javascript (12.13)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 748 | | |
| 97 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 21 884 | | |
| 98 | python (3.8)| [bocadillo](https://bocadilloproject.github.io) (0.18) | 21 665 | | |
| 99 | crystal (0.31)| [athena](https://github.com/blacksmoke16/athena) (0.7) | 21 320 | | |
| 100 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 21 300 | | |
| 101 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 20 526 | | |
| 102 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 621 | | |
| 103 | rust (1.4)| [iron](https://ironframework.io) (0.6) | 17 530 | | |
| 104 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 17 530 | | |
| 105 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 17 194 | | |
| 106 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 15 293 | | |
| 107 | ruby (2.6)| [grape](https://ruby-grape.org) (1.3) | 12 340 | | |
| 108 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.10) | 12 240 | | |
| 109 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 607 | | |
| 110 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 10 383 | | |
| 111 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 10 003 | | |
| 112 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 242 | | |
| 113 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 9 231 | | |
| 114 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 7 986 | | |
| 115 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 617 | | |
| 116 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 7 472 | | |
| 117 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 301 | | |
| 118 | crystal (0.31)| [onyx](https://onyxframework.org) (0.5) | 4 875 | | |
| 119 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 632 | | |
| 120 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a1) | 4 235 | | |
| 121 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 4 180 | | |
| 122 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 4 122 | | |
| 123 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 643 | | |
| 124 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 3 636 | | |
| 125 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 402 | | |
| 126 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 333 | | |
| 127 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 2 298 | | |
| 128 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 2 063 | | |
| 129 | php (7.4)| [laravel](https://laravel.com) (6.11) | 2 012 | | |
| 130 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 536 | | |
| 131 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 442 | | |
| 132 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 951 | | |

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

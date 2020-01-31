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

:information_source:  Updated on **2020-01-31** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 128
   + Connections : 128
   + Duration : 10 (seconds)

|    | Language | Framework | Speed (`req/s`) | Horizontal scale (parallelism) | Vertical scale (concurrency) |
|----|----------|-----------|----------------:|-------------|-------------|
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 166 221 | | |
| 2 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 163 590 | | |
| 3 | javascript (13.7)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 158 114 | | |
| 4 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 142 882 | | |
| 5 | crystal (0.31)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 142 813 | | |
| 6 | crystal (0.31)| [toro](https://github.com/soveran/toro) (0.4) | 140 181 | | |
| 7 | crystal (0.31)| [raze](https://razecr.com) (0.3) | 138 920 | | |
| 8 | crystal (0.31)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 135 498 | | |
| 9 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 132 496 | | |
| 10 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.8) | 131 711 | | |
| 11 | crystal (0.31)| [kemal](https://kemalcr.com) (0.26) | 131 348 | | |
| 12 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 127 798 | | |
| 13 | go (1.13)| [router](https://github.com/fasthttp/router) (0.6) | 127 760 | | |
| 14 | crystal (0.31)| [grip](https://github.com/grkek/grip) (0.26) | 127 575 | | |
| 15 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.3) | 127 135 | | |
| 16 | go (1.13)| [fiber](https://gofiber.github.io/fiber) (1.0) | 125 622 | | |
| 17 | crystal (0.31)| [amber](https://amberframework.org) (0.3) | 125 602 | | |
| 18 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 123 940 | | |
| 19 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 123 085 | | |
| 20 | crystal (0.31)| [orion](https://github.com/obsidian/orion) (2.1) | 121 881 | | |
| 21 | php (7.4)| [workerman](https://github.com/workerman/workerman) (3.5) | 119 334 | | |
| 22 | java (8)| [act](https://actframework.org) (1.8) | 101 954 | | |
| 23 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.12) | 100 811 | | |
| 24 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 92 434 | | |
| 25 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 87 009 | | |
| 26 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 86 879 | | |
| 27 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 86 410 | | |
| 28 | csharp (7.3)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.0) | 85 906 | | |
| 29 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 85 869 | | |
| 30 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 85 413 | | |
| 31 | go (1.13)| [violetear](https://violetear.org) (7.0) | 84 011 | | |
| 32 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 83 631 | | |
| 33 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 83 197 | | |
| 34 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.0) | 82 977 | | |
| 35 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 82 167 | | |
| 36 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 81 559 | | |
| 37 | go (1.13)| [beego](https://beego.me) (1.12) | 80 798 | | |
| 38 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 79 792 | | |
| 39 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.0) | 78 664 | | |
| 40 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 78 577 | | |
| 41 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 75 697 | | |
| 42 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 74 197 | | |
| 43 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 73 962 | | |
| 44 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 70 929 | | |
| 45 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 69 881 | | |
| 46 | go (1.13)| [gf](https://goframe.org) (1.11) | 67 876 | | |
| 47 | c (11)| [kore](https://kore.io) (3.3) | 65 860 | | |
| 48 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 65 529 | | |
| 49 | javascript (13.7)| [fastify](https://fastify.io) (2.11) | 60 814 | | |
| 50 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 60 562 | | |
| 51 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 58 329 | | |
| 52 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 58 060 | | |
| 53 | java (8)| [javalin](https://javalin.io) (3.5) | 58 015 | | |
| 54 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 57 767 | | |
| 55 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 56 140 | | |
| 56 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 54 454 | | |
| 57 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 54 382 | | |
| 58 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 54 061 | | |
| 59 | clojure (1.10)| [coast](https://coastonclojure.com) (1.0) | 53 775 | | |
| 60 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 53 327 | | |
| 61 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 49 434 | | |
| 62 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 49 367 | | |
| 63 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 49 084 | | |
| 64 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 49 029 | | |
| 65 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 48 556 | | |
| 66 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 47 564 | | |
| 67 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 47 174 | | |
| 68 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 46 588 | | |
| 69 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 46 267 | | |
| 70 | java (8)| [micronaut](https://micronaut.io) (1.2) | 46 249 | | |
| 71 | python (3.8)| [hug](https://hug.rest) (2.6) | 45 945 | | |
| 72 | python (3.8)| [starlette](https://starlette.io) (0.13) | 45 938 | | |
| 73 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 44 185 | | |
| 74 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 44 071 | | |
| 75 | php (7.4)| [imi](https://imiphp.com) (1.0) | 41 351 | | |
| 76 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 40 826 | | |
| 77 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 39 620 | | |
| 78 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.28) | 39 077 | | |
| 79 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 38 705 | | |
| 80 | javascript (13.7)| [hapi](https://hapijs.com) (19.0) | 38 015 | | |
| 81 | javascript (13.7)| [restify](https://restify.com) (8.5) | 36 301 | | |
| 82 | php (7.4)| [swoft](https://swoft.org) (2.0) | 35 322 | | |
| 83 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 34 858 | | |
| 84 | python (3.8)| [responder](https://python-responder.org) (2.0) | 30 378 | | |
| 85 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 29 324 | | |
| 86 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.47) | 29 083 | | |
| 87 | javascript (13.7)| [moleculer](https://moleculer.services) (0.13) | 28 822 | | |
| 88 | crystal (0.31)| [lucky](https://luckyframework.org) (0.18) | 27 195 | | |
| 89 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 26 904 | | |
| 90 | fsharp (7.3)| [suave](https://suave.io) (2.5) | 26 727 | | |
| 91 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 26 426 | | |
| 92 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 26 064 | | |
| 93 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 25 805 | | |
| 94 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 24 187 | | |
| 95 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 23 688 | | |
| 96 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 23 576 | | |
| 97 | crystal (0.31)| [athena](https://github.com/blacksmoke16/athena) (0.7) | 23 160 | | |
| 98 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 711 | | |
| 99 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 22 378 | | |
| 100 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 21 409 | | |
| 101 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 20 553 | | |
| 102 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a1) | 20 536 | | |
| 103 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 18 681 | | |
| 104 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 18 028 | | |
| 105 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 145 | | |
| 106 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 16 341 | | |
| 107 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.10) | 13 328 | | |
| 108 | ruby (2.6)| [grape](https://ruby-grape.org) (1.3) | 13 099 | | |
| 109 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 001 | | |
| 110 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 10 438 | | |
| 111 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 10 051 | | |
| 112 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 9 250 | | |
| 113 | python (3.8)| [django](https://djangoproject.com) (3.0) | 8 989 | | |
| 114 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 866 | | |
| 115 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 7 730 | | |
| 116 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 305 | | |
| 117 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 106 | | |
| 118 | crystal (0.31)| [onyx](https://onyxframework.org) (0.5) | 4 921 | | |
| 119 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 500 | | |
| 120 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 4 439 | | |
| 121 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 4 194 | | |
| 122 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 3 585 | | |
| 123 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 483 | | |
| 124 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 405 | | |
| 125 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 3 303 | | |
| 126 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 394 | | |
| 127 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 1 889 | | |
| 128 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 544 | | |
| 129 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 509 | | |
| 130 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 1 330 | | |
| 131 | php (7.4)| [laravel](https://laravel.com) (6.12) | 408 | | |

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

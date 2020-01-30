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
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 166 928 | | |
| 2 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 166 221 | | |
| 3 | javascript (13.7)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 161 283 | | |
| 4 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 144 906 | | |
| 5 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 143 031 | | |
| 6 | crystal (0.31)| [toro](https://github.com/soveran/toro) (0.4) | 141 488 | | |
| 7 | crystal (0.31)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 140 546 | | |
| 8 | crystal (0.31)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 139 239 | | |
| 9 | crystal (0.31)| [raze](https://razecr.com) (0.3) | 137 692 | | |
| 10 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 134 453 | | |
| 11 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.8) | 133 261 | | |
| 12 | crystal (0.31)| [kemal](https://kemalcr.com) (0.26) | 132 074 | | |
| 13 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 129 657 | | |
| 14 | go (1.13)| [router](https://github.com/fasthttp/router) (0.6) | 129 189 | | |
| 15 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.3) | 128 862 | | |
| 16 | go (1.13)| [fiber](https://gofiber.github.io/fiber) (1.0) | 127 287 | | |
| 17 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 125 648 | | |
| 18 | crystal (0.31)| [grip](https://github.com/grkek/grip) (0.26) | 124 884 | | |
| 19 | php (7.4)| [workerman](https://github.com/workerman/workerman) (3.5) | 124 454 | | |
| 20 | crystal (0.31)| [amber](https://amberframework.org) (0.3) | 124 385 | | |
| 21 | crystal (0.31)| [orion](https://github.com/obsidian/orion) (2.1) | 120 243 | | |
| 22 | java (8)| [act](https://actframework.org) (1.8) | 102 774 | | |
| 23 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.12) | 99 092 | | |
| 24 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 93 095 | | |
| 25 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 88 484 | | |
| 26 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 88 244 | | |
| 27 | csharp (7.3)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.0) | 87 861 | | |
| 28 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 87 244 | | |
| 29 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 87 014 | | |
| 30 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 86 599 | | |
| 31 | go (1.13)| [violetear](https://violetear.org) (7.0) | 84 804 | | |
| 32 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 84 437 | | |
| 33 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 83 887 | | |
| 34 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 83 674 | | |
| 35 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 83 342 | | |
| 36 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.0) | 81 902 | | |
| 37 | go (1.13)| [beego](https://beego.me) (1.12) | 81 848 | | |
| 38 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 80 301 | | |
| 39 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 79 684 | | |
| 40 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.0) | 79 022 | | |
| 41 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 76 839 | | |
| 42 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 74 826 | | |
| 43 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 74 193 | | |
| 44 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 71 866 | | |
| 45 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 70 951 | | |
| 46 | go (1.13)| [gf](https://goframe.org) (1.11) | 68 189 | | |
| 47 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 65 218 | | |
| 48 | javascript (13.7)| [fastify](https://fastify.io) (2.11) | 62 270 | | |
| 49 | java (8)| [javalin](https://javalin.io) (3.5) | 61 461 | | |
| 50 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 61 115 | | |
| 51 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 59 688 | | |
| 52 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 58 087 | | |
| 53 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 58 073 | | |
| 54 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 57 992 | | |
| 55 | c (11)| [kore](https://kore.io) (3.3) | 56 296 | | |
| 56 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 55 653 | | |
| 57 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 54 968 | | |
| 58 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 54 945 | | |
| 59 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 53 949 | | |
| 60 | clojure (1.10)| [coast](https://coastonclojure.com) (1.0) | 51 708 | | |
| 61 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 51 589 | | |
| 62 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 49 421 | | |
| 63 | java (8)| [micronaut](https://micronaut.io) (1.2) | 48 788 | | |
| 64 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 48 071 | | |
| 65 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 47 927 | | |
| 66 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 47 914 | | |
| 67 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 47 850 | | |
| 68 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 47 751 | | |
| 69 | rust (1.4)| [nickel](https://nickel-org.github.io) (0.11) | 47 731 | | |
| 70 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 47 102 | | |
| 71 | python (3.8)| [hug](https://hug.rest) (2.6) | 46 900 | | |
| 72 | python (3.8)| [starlette](https://starlette.io) (0.13) | 46 210 | | |
| 73 | rust (1.4)| [gotham](https://gotham.rs) (0.4) | 44 845 | | |
| 74 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 44 436 | | |
| 75 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 42 987 | | |
| 76 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 41 740 | | |
| 77 | php (7.4)| [imi](https://imiphp.com) (1.0) | 41 609 | | |
| 78 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 40 211 | | |
| 79 | javascript (13.7)| [hapi](https://hapijs.com) (19.0) | 38 711 | | |
| 80 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.28) | 37 511 | | |
| 81 | javascript (13.7)| [restify](https://restify.com) (8.5) | 37 316 | | |
| 82 | php (7.4)| [swoft](https://swoft.org) (2.0) | 35 879 | | |
| 83 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 34 225 | | |
| 84 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.47) | 30 745 | | |
| 85 | python (3.8)| [responder](https://python-responder.org) (2.0) | 30 495 | | |
| 86 | javascript (13.7)| [moleculer](https://moleculer.services) (0.13) | 29 215 | | |
| 87 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 28 528 | | |
| 88 | fsharp (7.3)| [suave](https://suave.io) (2.5) | 28 394 | | |
| 89 | crystal (0.31)| [lucky](https://luckyframework.org) (0.18) | 27 224 | | |
| 90 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 27 164 | | |
| 91 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 26 726 | | |
| 92 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 26 560 | | |
| 93 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 25 819 | | |
| 94 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 24 590 | | |
| 95 | crystal (0.31)| [athena](https://github.com/blacksmoke16/athena) (0.7) | 24 165 | | |
| 96 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 23 761 | | |
| 97 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 23 174 | | |
| 98 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 938 | | |
| 99 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 21 757 | | |
| 100 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 20 572 | | |
| 101 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 20 203 | | |
| 102 | rust (1.4)| [iron](https://ironframework.io) (0.6) | 18 892 | | |
| 103 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a1) | 18 611 | | |
| 104 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 18 330 | | |
| 105 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 162 | | |
| 106 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 15 511 | | |
| 107 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.10) | 13 428 | | |
| 108 | ruby (2.6)| [grape](https://ruby-grape.org) (1.3) | 12 800 | | |
| 109 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 581 | | |
| 110 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 622 | | |
| 111 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 10 405 | | |
| 112 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 10 067 | | |
| 113 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 9 263 | | |
| 114 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 825 | | |
| 115 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 7 749 | | |
| 116 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 426 | | |
| 117 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 286 | | |
| 118 | crystal (0.31)| [onyx](https://onyxframework.org) (0.5) | 5 883 | | |
| 119 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 4 570 | | |
| 120 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 474 | | |
| 121 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 4 169 | | |
| 122 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 3 636 | | |
| 123 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 518 | | |
| 124 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 416 | | |
| 125 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 3 217 | | |
| 126 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 401 | | |
| 127 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 1 874 | | |
| 128 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 583 | | |
| 129 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 550 | | |
| 130 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 1 456 | | |
| 131 | php (7.4)| [laravel](https://laravel.com) (6.12) | 406 | | |

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

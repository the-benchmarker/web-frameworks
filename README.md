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

:information_source: Updated on **2020-01-23 18:06:56 +01:00** :information_source:

|    | Language | Framework | Speed (`req/s`) | Horizontal scale (parallelism) | Vertical scale (concurrency) |
|----|----------|-----------|----------------:|-------------|-------------|
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 194 307 | | |
| 2 | javascript (12.13)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 189 928 | | |
| 3 | javascript (12.13)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 184 732 | | |
| 4 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 181 843 | | |
| 5 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.8) | 170 458 | | |
| 6 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 165 999 | | |
| 7 | go (1.13)| [router](https://github.com/fasthttp/router) (1.6) | 164 561 | | |
| 8 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 163 995 | | |
| 9 | go (1.13)| [fiber](https://gofiber.github.io/fiber) (1.0) | 162 757 | | |
| 10 | crystal (0.31)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 161 198 | | |
| 11 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.1) | 160 859 | | |
| 12 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.2) | 159 726 | | |
| 13 | crystal (0.31)| [toro](https://github.com/soveran/toro) (0.4) | 158 556 | | |
| 14 | crystal (0.31)| [raze](https://razecr.com) (0.3) | 156 002 | | |
| 15 | c (11)| [kore](https://kore.io) (3.3) | 155 531 | | |
| 16 | crystal (0.31)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 154 542 | | |
| 17 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 150 807 | | |
| 18 | crystal (0.31)| [kemal](https://kemalcr.com) (0.26) | 148 420 | | |
| 19 | crystal (0.31)| [grip](https://github.com/grkek/grip) (0.26) | 144 828 | | |
| 20 | crystal (0.31)| [amber](https://amberframework.org) (0.3) | 139 672 | | |
| 21 | crystal (0.31)| [orion](https://github.com/obsidian/orion) (2.1) | 131 955 | | |
| 22 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.12) | 128 913 | | |
| 23 | php (7.4)| [workerman](https://github.com/workerman/workerman) (3.5) | 128 672 | | |
| 24 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 110 126 | | |
| 25 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.3) | 107 062 | | |
| 26 | java (8)| [act](https://actframework.org) (1.8) | 106 691 | | |
| 27 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 103 516 | | |
| 28 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 103 050 | | |
| 29 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 102 502 | | |
| 30 | go (1.13)| [beego](https://beego.me) (1.12) | 99 451 | | |
| 31 | go (1.13)| [violetear](https://violetear.org) (7.0) | 98 885 | | |
| 32 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 98 790 | | |
| 33 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 98 209 | | |
| 34 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 97 695 | | |
| 35 | csharp (7.3)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.0) | 97 581 | | |
| 36 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 95 769 | | |
| 37 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 93 422 | | |
| 38 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.0) | 91 568 | | |
| 39 | javascript (12.13)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 90 949 | | |
| 40 | javascript (12.13)| [0http](https://github.com/jkyberneees/0http) (2.2) | 87 389 | | |
| 41 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 85 343 | | |
| 42 | javascript (12.13)| [restana](https://github.com/jkyberneees/ana) (4.0) | 82 226 | | |
| 43 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 80 329 | | |
| 44 | go (1.13)| [gf](https://goframe.org) (1.11) | 79 901 | | |
| 45 | javascript (12.13)| [polka](https://github.com/lukeed/polka) (0.5) | 78 218 | | |
| 46 | javascript (12.13)| [rayo](https://rayo.js.org) (1.3) | 76 576 | | |
| 47 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 76 432 | | |
| 48 | javascript (12.13)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 65 866 | | |
| 49 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 64 951 | | |
| 50 | javascript (12.13)| [foxify](https://foxify.js.org) (0.1) | 62 511 | | |
| 51 | javascript (12.13)| [fastify](https://fastify.io) (2.11) | 62 291 | | |
| 52 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 61 953 | | |
| 53 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 61 950 | | |
| 54 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 57 596 | | |
| 55 | javascript (12.13)| [koa](https://koajs.com) (2.11) | 54 406 | | |
| 56 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 53 413 | | |
| 57 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 53 090 | | |
| 58 | javascript (12.13)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 52 924 | | |
| 59 | clojure (1.10)| [coast](https://coastonclojure.com) (1.0) | 52 545 | | |
| 60 | java (8)| [javalin](https://javalin.io) (3.5) | 52 214 | | |
| 61 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 50 634 | | |
| 62 | rust (1.4)| [gotham](https://gotham.rs) (0.4) | 50 079 | | |
| 63 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 49 735 | | |
| 64 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 49 594 | | |
| 65 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 49 265 | | |
| 66 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 49 074 | | |
| 67 | javascript (12.13)| [express](https://expressjs.com) (4.17) | 48 400 | | |
| 68 | python (3.8)| [starlette](https://starlette.io) (0.13) | 47 381 | | |
| 69 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 46 696 | | |
| 70 | python (3.8)| [hug](https://hug.rest) (2.6) | 46 424 | | |
| 71 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 44 628 | | |
| 72 | php (7.4)| [imi](https://imiphp.com) (1.0) | 43 021 | | |
| 73 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 42 504 | | |
| 74 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 41 032 | | |
| 75 | fsharp (7.3)| [suave](https://suave.io) (2.5) | 36 576 | | |
| 76 | php (7.4)| [swoft](https://swoft.org) (2.0) | 35 856 | | |
| 77 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 34 464 | | |
| 78 | javascript (12.13)| [restify](https://restify.com) (8.5) | 34 188 | | |
| 79 | java (8)| [micronaut](https://micronaut.io) (1.2) | 33 761 | | |
| 80 | rust (1.4)| [nickel](https://nickel-org.github.io) (0.11) | 33 692 | | |
| 81 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.28) | 32 207 | | |
| 82 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 31 996 | | |
| 83 | javascript (12.13)| [hapi](https://hapijs.com) (19.0) | 31 561 | | |
| 84 | python (3.8)| [responder](https://python-responder.org) (2.0) | 31 122 | | |
| 85 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.47) | 30 765 | | |
| 86 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 28 867 | | |
| 87 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 28 553 | | |
| 88 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 27 091 | | |
| 89 | javascript (12.13)| [moleculer](https://moleculer.services) (0.13) | 27 017 | | |
| 90 | crystal (0.31)| [lucky](https://luckyframework.org) (0.18) | 26 672 | | |
| 91 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 26 598 | | |
| 92 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 25 757 | | |
| 93 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 25 626 | | |
| 94 | crystal (0.31)| [athena](https://github.com/blacksmoke16/athena) (0.7) | 24 582 | | |
| 95 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 24 508 | | |
| 96 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 24 380 | | |
| 97 | javascript (12.13)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 23 334 | | |
| 98 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 22 062 | | |
| 99 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 20 943 | | |
| 100 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 19 866 | | |
| 101 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a1) | 19 659 | | |
| 102 | python (3.8)| [bocadillo](https://bocadilloproject.github.io) (0.18) | 19 547 | | |
| 103 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 18 174 | | |
| 104 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 18 002 | | |
| 105 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 488 | | |
| 106 | rust (1.4)| [iron](https://ironframework.io) (0.6) | 17 357 | | |
| 107 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 14 450 | | |
| 108 | ruby (2.6)| [grape](https://ruby-grape.org) (1.3) | 13 790 | | |
| 109 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.10) | 12 284 | | |
| 110 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 468 | | |
| 111 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 10 277 | | |
| 112 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 822 | | |
| 113 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 9 156 | | |
| 114 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 473 | | |
| 115 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 8 031 | | |
| 116 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 7 746 | | |
| 117 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 484 | | |
| 118 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 355 | | |
| 119 | crystal (0.31)| [onyx](https://onyxframework.org) (0.5) | 4 883 | | |
| 120 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 330 | | |
| 121 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 4 152 | | |
| 122 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 3 766 | | |
| 123 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 515 | | |
| 124 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 339 | | |
| 125 | php (7.4)| [laravel](https://laravel.com) (6.11) | 3 084 | | |
| 126 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 2 908 | | |
| 127 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 2 608 | | |
| 128 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 349 | | |
| 129 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 2 001 | | |
| 130 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 533 | | |
| 131 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 440 | | |
| 132 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 932 | | |

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

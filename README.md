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

+ Initialize `sqlite` database

~~~sh
bin/db init
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
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 168 591 | | |
| 2 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 166 097 | | |
| 3 | javascript (12.13)| [nanoexpress-pro](https://nanoexpress.js.org) (1.8) | 164 794 | | |
| 4 | javascript (12.13)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 157 094 | | |
| 5 | javascript (12.13)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 151 995 | | |
| 6 | c (11)| [kore](https://kore.io) (3.3) | 145 497 | | |
| 7 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.7) | 130 953 | | |
| 8 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 127 549 | | |
| 9 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 127 320 | | |
| 10 | go (1.13)| [router](https://github.com/fasthttp/router) (1.6) | 125 559 | | |
| 11 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (9.0) | 124 626 | | |
| 12 | crystal (0.31)| [toro](https://github.com/soveran/toro) (0.4) | 123 899 | | |
| 13 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.2) | 122 006 | | |
| 14 | crystal (0.31)| [raze](https://razecr.com) (0.3) | 119 794 | | |
| 15 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 116 492 | | |
| 16 | crystal (0.31)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 113 247 | | |
| 17 | crystal (0.31)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 110 202 | | |
| 18 | crystal (0.31)| [kemal](https://kemalcr.com) (0.26) | 106 567 | | |
| 19 | java (8)| [act](https://actframework.org) (1.8) | 101 278 | | |
| 20 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.11) | 100 484 | | |
| 21 | crystal (0.31)| [orion](https://github.com/obsidian/orion) (2.1) | 97 762 | | |
| 22 | crystal (0.31)| [amber](https://amberframework.org) (0.3) | 96 547 | | |
| 23 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 87 281 | | |
| 24 | php (7.4)| [workerman](https://github.com/workerman/workerman) (3.5) | 86 753 | | |
| 25 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.2) | 85 800 | | |
| 26 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 82 266 | | |
| 27 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 80 505 | | |
| 28 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 78 808 | | |
| 29 | go (1.13)| [violetear](https://violetear.org) (7.0) | 78 553 | | |
| 30 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 77 832 | | |
| 31 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 77 793 | | |
| 32 | go (1.13)| [beego](https://beego.me) (1.12) | 76 789 | | |
| 33 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 76 520 | | |
| 34 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 76 225 | | |
| 35 | csharp (7.3)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.0) | 73 940 | | |
| 36 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 73 469 | | |
| 37 | javascript (12.13)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 70 032 | | |
| 38 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.0) | 68 216 | | |
| 39 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 67 530 | | |
| 40 | javascript (12.13)| [0http](https://github.com/jkyberneees/0http) (2.2) | 67 236 | | |
| 41 | javascript (12.13)| [restana](https://github.com/jkyberneees/ana) (4.0) | 65 867 | | |
| 42 | javascript (12.13)| [polka](https://github.com/lukeed/polka) (0.5) | 62 211 | | |
| 43 | go (1.13)| [gf](https://goframe.org) (1.11) | 61 509 | | |
| 44 | javascript (12.13)| [rayo](https://rayo.js.org) (1.3) | 59 751 | | |
| 45 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 56 291 | | |
| 46 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 56 096 | | |
| 47 | javascript (12.13)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 52 541 | | |
| 48 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 50 071 | | |
| 49 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 49 675 | | |
| 50 | clojure (1.10)| [coast](https://coastonclojure.com) (1.0) | 47 100 | | |
| 51 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 47 010 | | |
| 52 | rust (1.39)| [gotham](https://gotham.rs) (0.4) | 46 407 | | |
| 53 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 46 389 | | |
| 54 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 45 735 | | |
| 55 | javascript (12.13)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 42 942 | | |
| 56 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 42 548 | | |
| 57 | javascript (12.13)| [koa](https://koajs.com) (2.11) | 42 185 | | |
| 58 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 41 310 | | |
| 59 | javascript (12.13)| [fastify](https://fastify.io) (2.11) | 41 183 | | |
| 60 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 40 933 | | |
| 61 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 39 576 | | |
| 62 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 38 660 | | |
| 63 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 38 627 | | |
| 64 | javascript (12.13)| [express](https://expressjs.com) (4.17) | 38 594 | | |
| 65 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 38 246 | | |
| 66 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 38 197 | | |
| 67 | python (3.8)| [hug](https://hug.rest) (2.6) | 36 594 | | |
| 68 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 36 496 | | |
| 69 | python (3.8)| [starlette](https://starlette.io) (0.13) | 36 294 | | |
| 70 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 34 391 | | |
| 71 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 34 280 | | |
| 72 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 33 840 | | |
| 73 | php (7.4)| [slim](https://slimframework.com) (4.4) | 33 572 | | |
| 74 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 33 501 | | |
| 75 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 33 201 | | |
| 76 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.27) | 33 158 | | |
| 77 | rust (1.39)| [nickel](https://nickel-org.github.io) (0.11) | 32 744 | | |
| 78 | php (7.4)| [symfony](https://symfony.com) (4.3) | 32 491 | | |
| 79 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 32 008 | | |
| 80 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 31 560 | | |
| 81 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 31 460 | | |
| 82 | php (7.4)| [imi](https://imiphp.com) (1.0) | 31 388 | | |
| 83 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 31 072 | | |
| 84 | java (8)| [javalin](https://javalin.io) (3.5) | 31 037 | | |
| 85 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 30 345 | | |
| 86 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 30 300 | | |
| 87 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 27 806 | | |
| 88 | php (7.4)| [swoft](https://swoft.org) (2.0) | 27 365 | | |
| 89 | php (7.4)| [laravel](https://laravel.com) (6.9) | 27 109 | | |
| 90 | javascript (12.13)| [restify](https://restify.com) (8.5) | 26 626 | | |
| 91 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 25 987 | | |
| 92 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.45) | 23 898 | | |
| 93 | python (3.8)| [responder](https://python-responder.org) (2.0) | 23 572 | | |
| 94 | crystal (0.31)| [lucky](https://luckyframework.org) (0.18) | 22 941 | | |
| 95 | java (8)| [micronaut](https://micronaut.io) (1.2) | 22 556 | | |
| 96 | javascript (12.13)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 353 | | |
| 97 | javascript (12.13)| [hapi](https://hapijs.com) (18.4) | 21 878 | | |
| 98 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 20 986 | | |
| 99 | javascript (12.13)| [moleculer](https://moleculer.services) (0.13) | 20 645 | | |
| 100 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 20 349 | | |
| 101 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 19 966 | | |
| 102 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 18 878 | | |
| 103 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 18 765 | | |
| 104 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 18 354 | | |
| 105 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 18 322 | | |
| 106 | javascript (12.13)| [foxify](https://foxify.js.org) (0.1) | 17 741 | | |
| 107 | rust (1.39)| [iron](https://ironframework.io) (0.6) | 17 386 | | |
| 108 | python (3.8)| [bocadillo](https://bocadilloproject.github.io) (0.18) | 16 977 | | |
| 109 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 16 347 | | |
| 110 | crystal (0.31)| [athena](https://github.com/blacksmoke16/athena) (0.7) | 15 891 | | |
| 111 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 14 682 | | |
| 112 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 14 285 | | |
| 113 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.9) | 12 425 | | |
| 114 | ruby (2.6)| [grape](https://ruby-grape.org) (1.2) | 11 112 | | |
| 115 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.10) | 9 825 | | |
| 116 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 9 595 | | |
| 117 | crystal (0.31)| [onyx](https://onyxframework.org) (0.5) | 9 105 | | |
| 118 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 8 366 | | |
| 119 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 8 043 | | |
| 120 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 7 381 | | |
| 121 | python (3.8)| [django](https://djangoproject.com) (3.0) | 7 298 | | |
| 122 | fsharp (7.3)| [suave](https://suave.io) (2.5) | 6 645 | | |
| 123 | python (3.8)| [masonite](https://masoniteproject.com) (2.2) | 5 681 | | |
| 124 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 800 | | |
| 125 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 3 422 | | |
| 126 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 273 | | |
| 127 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 478 | | |
| 128 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 399 | | |
| 129 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 1 028 | | |
| 130 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 956 | | |

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

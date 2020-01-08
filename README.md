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
| 1 | javascript (12.13)| [nanoexpress-pro](https://nanoexpress.js.org) (1.9) | 198 075 | | |
| 2 | javascript (12.13)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 193 198 | | |
| 3 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 192 842 | | |
| 4 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 188 251 | | |
| 5 | javascript (12.13)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 185 600 | | |
| 6 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.7) | 168 298 | | |
| 7 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 165 073 | | |
| 8 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.2) | 162 897 | | |
| 9 | go (1.13)| [router](https://github.com/fasthttp/router) (1.6) | 160 612 | | |
| 10 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (9.0) | 160 335 | | |
| 11 | c (11)| [kore](https://kore.io) (3.3) | 159 927 | | |
| 12 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 159 723 | | |
| 13 | crystal (0.31)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 152 559 | | |
| 14 | crystal (0.31)| [raze](https://razecr.com) (0.3) | 148 213 | | |
| 15 | crystal (0.31)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 146 478 | | |
| 16 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 145 595 | | |
| 17 | crystal (0.31)| [toro](https://github.com/soveran/toro) (0.4) | 141 832 | | |
| 18 | crystal (0.31)| [kemal](https://kemalcr.com) (0.26) | 140 356 | | |
| 19 | crystal (0.31)| [amber](https://amberframework.org) (0.3) | 130 629 | | |
| 20 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.11) | 118 562 | | |
| 21 | crystal (0.31)| [orion](https://github.com/obsidian/orion) (2.1) | 117 101 | | |
| 22 | java (8)| [act](https://actframework.org) (1.8) | 115 590 | | |
| 23 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.2) | 106 713 | | |
| 24 | php (7.4)| [workerman](https://github.com/workerman/workerman) (3.5) | 105 979 | | |
| 25 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 103 047 | | |
| 26 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 101 481 | | |
| 27 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 101 436 | | |
| 28 | csharp (7.3)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.0) | 101 245 | | |
| 29 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 101 105 | | |
| 30 | go (1.13)| [beego](https://beego.me) (1.12) | 98 046 | | |
| 31 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 97 127 | | |
| 32 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 95 934 | | |
| 33 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 92 364 | | |
| 34 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 92 275 | | |
| 35 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 89 257 | | |
| 36 | javascript (12.13)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 88 408 | | |
| 37 | go (1.13)| [violetear](https://violetear.org) (7.0) | 86 707 | | |
| 38 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 86 480 | | |
| 39 | javascript (12.13)| [0http](https://github.com/jkyberneees/0http) (2.2) | 83 913 | | |
| 40 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.0) | 81 473 | | |
| 41 | javascript (12.13)| [restana](https://github.com/jkyberneees/ana) (4.0) | 79 519 | | |
| 42 | javascript (12.13)| [polka](https://github.com/lukeed/polka) (0.5) | 76 205 | | |
| 43 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 75 281 | | |
| 44 | javascript (12.13)| [rayo](https://rayo.js.org) (1.3) | 74 417 | | |
| 45 | go (1.13)| [gf](https://goframe.org) (1.11) | 74 229 | | |
| 46 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 73 773 | | |
| 47 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 69 795 | | |
| 48 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 63 142 | | |
| 49 | javascript (12.13)| [foxify](https://foxify.js.org) (0.1) | 62 409 | | |
| 50 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 61 049 | | |
| 51 | javascript (12.13)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 61 044 | | |
| 52 | java (8)| [javalin](https://javalin.io) (3.5) | 60 781 | | |
| 53 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 59 927 | | |
| 54 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 58 776 | | |
| 55 | javascript (12.13)| [koa](https://koajs.com) (2.11) | 55 878 | | |
| 56 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 53 437 | | |
| 57 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 52 396 | | |
| 58 | clojure (1.10)| [coast](https://coastonclojure.com) (1.0) | 52 178 | | |
| 59 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 52 034 | | |
| 60 | javascript (12.13)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 51 565 | | |
| 61 | javascript (12.13)| [fastify](https://fastify.io) (2.11) | 51 535 | | |
| 62 | rust (1.4)| [gotham](https://gotham.rs) (0.4) | 51 505 | | |
| 63 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 51 415 | | |
| 64 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 51 189 | | |
| 65 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 51 145 | | |
| 66 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 50 467 | | |
| 67 | javascript (12.13)| [express](https://expressjs.com) (4.17) | 49 373 | | |
| 68 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 48 426 | | |
| 69 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 48 125 | | |
| 70 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 47 639 | | |
| 71 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 47 176 | | |
| 72 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 46 686 | | |
| 73 | python (3.8)| [hug](https://hug.rest) (2.6) | 46 289 | | |
| 74 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 44 832 | | |
| 75 | python (3.8)| [starlette](https://starlette.io) (0.13) | 44 527 | | |
| 76 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 42 800 | | |
| 77 | php (7.4)| [slim](https://slimframework.com) (4.4) | 42 477 | | |
| 78 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 42 188 | | |
| 79 | php (7.4)| [imi](https://imiphp.com) (1.0) | 42 028 | | |
| 80 | javascript (12.13)| [restify](https://restify.com) (8.5) | 41 185 | | |
| 81 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 40 846 | | |
| 82 | php (7.4)| [symfony](https://symfony.com) (4.3) | 39 846 | | |
| 83 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 39 820 | | |
| 84 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 39 235 | | |
| 85 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 38 920 | | |
| 86 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 38 040 | | |
| 87 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.27) | 37 732 | | |
| 88 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 37 156 | | |
| 89 | php (7.4)| [swoft](https://swoft.org) (2.0) | 35 925 | | |
| 90 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 33 513 | | |
| 91 | javascript (12.13)| [hapi](https://hapijs.com) (18.4) | 33 384 | | |
| 92 | php (7.4)| [laravel](https://laravel.com) (6.1) | 33 304 | | |
| 93 | java (8)| [micronaut](https://micronaut.io) (1.2) | 32 126 | | |
| 94 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 30 368 | | |
| 95 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.45) | 30 257 | | |
| 96 | fsharp (7.3)| [suave](https://suave.io) (2.5) | 29 533 | | |
| 97 | python (3.8)| [responder](https://python-responder.org) (2.0) | 28 628 | | |
| 98 | javascript (12.13)| [moleculer](https://moleculer.services) (0.13) | 28 332 | | |
| 99 | rust (1.4)| [nickel](https://nickel-org.github.io) (0.11) | 28 282 | | |
| 100 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 27 992 | | |
| 101 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 26 768 | | |
| 102 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 25 972 | | |
| 103 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 24 252 | | |
| 104 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 22 704 | | |
| 105 | crystal (0.31)| [athena](https://github.com/blacksmoke16/athena) (0.7) | 22 634 | | |
| 106 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 22 402 | | |
| 107 | javascript (12.13)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 399 | | |
| 108 | crystal (0.31)| [lucky](https://luckyframework.org) (0.18) | 22 331 | | |
| 109 | python (3.8)| [bocadillo](https://bocadilloproject.github.io) (0.18) | 21 279 | | |
| 110 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 20 273 | | |
| 111 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 20 215 | | |
| 112 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 17 725 | | |
| 113 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 16 539 | | |
| 114 | rust (1.4)| [iron](https://ironframework.io) (0.6) | 15 941 | | |
| 115 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 14 551 | | |
| 116 | ruby (2.6)| [grape](https://ruby-grape.org) (1.2) | 12 301 | | |
| 117 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 12 144 | | |
| 118 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 508 | | |
| 119 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 10 272 | | |
| 120 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.10) | 10 049 | | |
| 121 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 410 | | |
| 122 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 9 126 | | |
| 123 | python (3.8)| [masonite](https://masoniteproject.com) (2.2) | 7 142 | | |
| 124 | crystal (0.31)| [onyx](https://onyxframework.org) (0.5) | 4 876 | | |
| 125 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 3 833 | | |
| 126 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 2 736 | | |
| 127 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 369 | | |
| 128 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 496 | | |
| 129 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 410 | | |
| 130 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 1 127 | | |

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

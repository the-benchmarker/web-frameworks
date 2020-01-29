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
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 166 414 | | |
| 2 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 161 307 | | |
| 3 | javascript (13.7)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 160 182 | | |
| 4 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 144 539 | | |
| 5 | crystal (0.31)| [toro](https://github.com/soveran/toro) (0.4) | 143 829 | | |
| 6 | crystal (0.31)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 142 257 | | |
| 7 | crystal (0.31)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 139 602 | | |
| 8 | crystal (0.31)| [raze](https://razecr.com) (0.3) | 138 355 | | |
| 9 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 135 433 | | |
| 10 | crystal (0.31)| [kemal](https://kemalcr.com) (0.26) | 133 210 | | |
| 11 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.8) | 131 801 | | |
| 12 | crystal (0.31)| [grip](https://github.com/grkek/grip) (0.26) | 128 849 | | |
| 13 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 128 133 | | |
| 14 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 127 967 | | |
| 15 | go (1.13)| [router](https://github.com/fasthttp/router) (0.6) | 127 914 | | |
| 16 | crystal (0.31)| [amber](https://amberframework.org) (0.3) | 127 097 | | |
| 17 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.1) | 126 388 | | |
| 18 | go (1.13)| [fiber](https://gofiber.github.io/fiber) (1.0) | 126 202 | | |
| 19 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.2) | 124 847 | | |
| 20 | crystal (0.31)| [orion](https://github.com/obsidian/orion) (2.1) | 120 926 | | |
| 21 | php (7.4)| [workerman](https://github.com/workerman/workerman) (3.5) | 120 865 | | |
| 22 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.12) | 101 937 | | |
| 23 | java (8)| [act](https://actframework.org) (1.8) | 101 067 | | |
| 24 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 93 500 | | |
| 25 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 88 010 | | |
| 26 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.3) | 87 953 | | |
| 27 | csharp (7.3)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.0) | 87 756 | | |
| 28 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 86 369 | | |
| 29 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 86 351 | | |
| 30 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 86 098 | | |
| 31 | go (1.13)| [violetear](https://violetear.org) (7.0) | 84 812 | | |
| 32 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 83 972 | | |
| 33 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 83 685 | | |
| 34 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.0) | 82 666 | | |
| 35 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 82 569 | | |
| 36 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 82 413 | | |
| 37 | go (1.13)| [beego](https://beego.me) (1.12) | 80 509 | | |
| 38 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 79 531 | | |
| 39 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 79 007 | | |
| 40 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.0) | 78 999 | | |
| 41 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 75 640 | | |
| 42 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 74 387 | | |
| 43 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 71 909 | | |
| 44 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 71 067 | | |
| 45 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 70 582 | | |
| 46 | go (1.13)| [gf](https://goframe.org) (1.11) | 68 145 | | |
| 47 | c (11)| [kore](https://kore.io) (3.3) | 65 942 | | |
| 48 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 65 454 | | |
| 49 | javascript (13.7)| [fastify](https://fastify.io) (2.11) | 62 153 | | |
| 50 | java (8)| [javalin](https://javalin.io) (3.5) | 61 689 | | |
| 51 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 60 600 | | |
| 52 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 59 097 | | |
| 53 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 58 347 | | |
| 54 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 57 872 | | |
| 55 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 57 345 | | |
| 56 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 55 234 | | |
| 57 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 54 816 | | |
| 58 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 53 862 | | |
| 59 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 53 495 | | |
| 60 | clojure (1.10)| [coast](https://coastonclojure.com) (1.0) | 53 011 | | |
| 61 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 50 877 | | |
| 62 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 50 122 | | |
| 63 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 49 600 | | |
| 64 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 49 358 | | |
| 65 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 48 218 | | |
| 66 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 47 905 | | |
| 67 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 47 405 | | |
| 68 | rust (1.4)| [nickel](https://nickel-org.github.io) (0.11) | 47 359 | | |
| 69 | python (3.8)| [hug](https://hug.rest) (2.6) | 46 350 | | |
| 70 | java (8)| [micronaut](https://micronaut.io) (1.2) | 46 098 | | |
| 71 | python (3.8)| [starlette](https://starlette.io) (0.13) | 45 980 | | |
| 72 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 44 655 | | |
| 73 | rust (1.4)| [gotham](https://gotham.rs) (0.4) | 44 391 | | |
| 74 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 43 994 | | |
| 75 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 41 841 | | |
| 76 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 41 441 | | |
| 77 | php (7.4)| [imi](https://imiphp.com) (1.0) | 41 263 | | |
| 78 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 40 338 | | |
| 79 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.28) | 38 909 | | |
| 80 | javascript (13.7)| [hapi](https://hapijs.com) (19.0) | 38 385 | | |
| 81 | javascript (13.7)| [restify](https://restify.com) (8.5) | 36 675 | | |
| 82 | php (7.4)| [swoft](https://swoft.org) (2.0) | 35 871 | | |
| 83 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 35 241 | | |
| 84 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.47) | 30 575 | | |
| 85 | python (3.8)| [responder](https://python-responder.org) (2.0) | 29 737 | | |
| 86 | javascript (13.7)| [moleculer](https://moleculer.services) (0.13) | 28 936 | | |
| 87 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 28 314 | | |
| 88 | crystal (0.31)| [lucky](https://luckyframework.org) (0.18) | 27 802 | | |
| 89 | fsharp (7.3)| [suave](https://suave.io) (2.5) | 27 154 | | |
| 90 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 26 744 | | |
| 91 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 26 647 | | |
| 92 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 26 192 | | |
| 93 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 25 869 | | |
| 94 | crystal (0.31)| [athena](https://github.com/blacksmoke16/athena) (0.7) | 24 814 | | |
| 95 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 24 257 | | |
| 96 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 23 944 | | |
| 97 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 818 | | |
| 98 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 22 793 | | |
| 99 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 22 311 | | |
| 100 | python (3.8)| [bocadillo](https://bocadilloproject.github.io) (0.18) | 21 737 | | |
| 101 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 21 440 | | |
| 102 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 20 589 | | |
| 103 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a1) | 20 476 | | |
| 104 | rust (1.4)| [iron](https://ironframework.io) (0.6) | 18 799 | | |
| 105 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 17 992 | | |
| 106 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 236 | | |
| 107 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 16 457 | | |
| 108 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.10) | 13 336 | | |
| 109 | ruby (2.6)| [grape](https://ruby-grape.org) (1.3) | 11 769 | | |
| 110 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 275 | | |
| 111 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 504 | | |
| 112 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 10 481 | | |
| 113 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 10 082 | | |
| 114 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 9 263 | | |
| 115 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 875 | | |
| 116 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 7 721 | | |
| 117 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 384 | | |
| 118 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 279 | | |
| 119 | crystal (0.31)| [onyx](https://onyxframework.org) (0.5) | 4 787 | | |
| 120 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 4 595 | | |
| 121 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 465 | | |
| 122 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 4 220 | | |
| 123 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 3 741 | | |
| 124 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 518 | | |
| 125 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 421 | | |
| 126 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 3 268 | | |
| 127 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 361 | | |
| 128 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 1 874 | | |
| 129 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 549 | | |
| 130 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 538 | | |
| 131 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 1 385 | | |
| 132 | php (7.4)| [laravel](https://laravel.com) (6.12) | 390 | | |
| 133 | nim (1.0)| [phoon](https://github.com/ducdetronquito/phoon) (0.1) | 15 | | |

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

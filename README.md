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

:information_source:  Updated on **2020-02-05** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 128
   + Connections : 128
   + Duration : 10 (seconds)

|    | Language | Framework | Speed (`req/s`) | Horizontal scale (parallelism) | Vertical scale (concurrency) |
|----|----------|-----------|----------------:|-------------|-------------|
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 166 300 | | |
| 2 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 163 068 | | |
| 3 | javascript (13.7)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 157 756 | | |
| 4 | crystal (0.31)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 140 889 | | |
| 5 | crystal (0.31)| [toro](https://github.com/soveran/toro) (0.4) | 139 994 | | |
| 6 | go (1.13)| [fiber](https://gofiber.github.io/fiber) (1.3) | 136 740 | | |
| 7 | crystal (0.31)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 136 326 | | |
| 8 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 135 853 | | |
| 9 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 135 045 | | |
| 10 | crystal (0.31)| [kemal](https://kemalcr.com) (0.26) | 132 581 | | |
| 11 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 132 528 | | |
| 12 | crystal (0.31)| [grip](https://github.com/grkek/grip) (0.27) | 131 002 | | |
| 13 | crystal (0.31)| [raze](https://razecr.com) (0.3) | 130 411 | | |
| 14 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.9) | 125 826 | | |
| 15 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 124 132 | | |
| 16 | go (1.13)| [router](https://github.com/fasthttp/router) (0.6) | 123 552 | | |
| 17 | crystal (0.31)| [amber](https://amberframework.org) (0.3) | 121 340 | | |
| 18 | php (7.4)| [workerman](https://github.com/workerman/workerman) (3.5) | 121 119 | | |
| 19 | crystal (0.31)| [orion](https://github.com/obsidian/orion) (2.1) | 120 354 | | |
| 20 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 116 113 | | |
| 21 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.3) | 111 835 | | |
| 22 | java (8)| [act](https://actframework.org) (1.8) | 97 902 | | |
| 23 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.12) | 89 133 | | |
| 24 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 86 977 | | |
| 25 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 85 692 | | |
| 26 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 82 120 | | |
| 27 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 82 082 | | |
| 28 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 81 856 | | |
| 29 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 81 573 | | |
| 30 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.0) | 81 404 | | |
| 31 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 80 968 | | |
| 32 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 80 364 | | |
| 33 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 80 012 | | |
| 34 | go (1.13)| [violetear](https://violetear.org) (7.0) | 78 420 | | |
| 35 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 77 222 | | |
| 36 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.0) | 77 129 | | |
| 37 | go (1.13)| [beego](https://beego.me) (1.12) | 77 106 | | |
| 38 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 75 123 | | |
| 39 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 74 919 | | |
| 40 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 74 863 | | |
| 41 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 73 185 | | |
| 42 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 72 577 | | |
| 43 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 71 348 | | |
| 44 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 69 751 | | |
| 45 | c (11)| [kore](https://kore.io) (3.3) | 69 509 | | |
| 46 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 65 288 | | |
| 47 | go (1.13)| [gf](https://goframe.org) (1.11) | 63 741 | | |
| 48 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 62 916 | | |
| 49 | java (8)| [javalin](https://javalin.io) (3.5) | 62 165 | | |
| 50 | javascript (13.7)| [fastify](https://fastify.io) (2.11) | 61 069 | | |
| 51 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 60 348 | | |
| 52 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.12) | 59 613 | | |
| 53 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 59 555 | | |
| 54 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 58 108 | | |
| 55 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 56 598 | | |
| 56 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 56 212 | | |
| 57 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 54 214 | | |
| 58 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 53 239 | | |
| 59 | java (8)| [micronaut](https://micronaut.io) (1.2) | 52 055 | | |
| 60 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 51 398 | | |
| 61 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 50 446 | | |
| 62 | clojure (1.10)| [coast](https://coastonclojure.com) (1.0) | 50 326 | | |
| 63 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 49 777 | | |
| 64 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 49 613 | | |
| 65 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 49 599 | | |
| 66 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 49 026 | | |
| 67 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 48 634 | | |
| 68 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 47 715 | | |
| 69 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 47 372 | | |
| 70 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 46 945 | | |
| 71 | python (3.8)| [hug](https://hug.rest) (2.6) | 46 841 | | |
| 72 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 46 734 | | |
| 73 | python (3.8)| [starlette](https://starlette.io) (0.13) | 46 452 | | |
| 74 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 44 107 | | |
| 75 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 44 097 | | |
| 76 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 41 172 | | |
| 77 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 40 694 | | |
| 78 | php (7.4)| [imi](https://imiphp.com) (1.0) | 39 565 | | |
| 79 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 39 481 | | |
| 80 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 38 716 | | |
| 81 | javascript (13.7)| [restify](https://restify.com) (8.5) | 37 885 | | |
| 82 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.28) | 37 559 | | |
| 83 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 35 978 | | |
| 84 | php (7.4)| [swoft](https://swoft.org) (2.0) | 32 920 | | |
| 85 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.48) | 31 078 | | |
| 86 | python (3.8)| [responder](https://python-responder.org) (2.0) | 30 488 | | |
| 87 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 30 147 | | |
| 88 | javascript (13.7)| [moleculer](https://moleculer.services) (0.13) | 30 104 | | |
| 89 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 27 477 | | |
| 90 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 27 049 | | |
| 91 | crystal (0.31)| [lucky](https://luckyframework.org) (0.18) | 26 845 | | |
| 92 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 25 864 | | |
| 93 | crystal (0.31)| [athena](https://github.com/blacksmoke16/athena) (0.7) | 24 933 | | |
| 94 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 24 567 | | |
| 95 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 23 938 | | |
| 96 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 936 | | |
| 97 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 23 613 | | |
| 98 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 326 | | |
| 99 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 21 809 | | |
| 100 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 21 651 | | |
| 101 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a1) | 20 910 | | |
| 102 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 19 262 | | |
| 103 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 19 058 | | |
| 104 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 18 673 | | |
| 105 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 424 | | |
| 106 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 17 020 | | |
| 107 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 15 923 | | |
| 108 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.10) | 13 607 | | |
| 109 | ruby (2.6)| [grape](https://ruby-grape.org) (1.3) | 13 269 | | |
| 110 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 486 | | |
| 111 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 762 | | |
| 112 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 10 344 | | |
| 113 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 996 | | |
| 114 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 9 500 | | |
| 115 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 658 | | |
| 116 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 7 892 | | |
| 117 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 455 | | |
| 118 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 171 | | |
| 119 | crystal (0.31)| [onyx](https://onyxframework.org) (0.5) | 6 573 | | |
| 120 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 4 859 | | |
| 121 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 481 | | |
| 122 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 3 967 | | |
| 123 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 3 547 | | |
| 124 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 449 | | |
| 125 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 3 434 | | |
| 126 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 429 | | |
| 127 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 369 | | |
| 128 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 1 881 | | |
| 129 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 619 | | |
| 130 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 543 | | |
| 131 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 1 390 | | |
| 132 | php (7.4)| [laravel](https://laravel.com) (6.14) | 381 | | |

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

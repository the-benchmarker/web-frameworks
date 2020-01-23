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

:information_source: Updated on **2020-01-23 22:27:22 +01:00** :information_source:

|    | Language | Framework | Speed (`req/s`) | Horizontal scale (parallelism) | Vertical scale (concurrency) |
|----|----------|-----------|----------------:|-------------|-------------|
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 187 679 | | |
| 2 | javascript (12.13)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 182 904 | | |
| 3 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 180 192 | | |
| 4 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.8) | 175 139 | | |
| 5 | javascript (12.13)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 174 477 | | |
| 6 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 170 171 | | |
| 7 | go (1.13)| [router](https://github.com/fasthttp/router) (0.6) | 169 794 | | |
| 8 | go (1.13)| [fiber](https://gofiber.github.io/fiber) (1.0) | 167 749 | | |
| 9 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.1) | 167 419 | | |
| 10 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.2) | 164 099 | | |
| 11 | crystal (0.31)| [toro](https://github.com/soveran/toro) (0.4) | 152 413 | | |
| 12 | crystal (0.31)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 151 791 | | |
| 13 | c (11)| [kore](https://kore.io) (3.3) | 151 288 | | |
| 14 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 148 717 | | |
| 15 | crystal (0.31)| [raze](https://razecr.com) (0.3) | 148 333 | | |
| 16 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 147 899 | | |
| 17 | crystal (0.31)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 147 004 | | |
| 18 | crystal (0.31)| [kemal](https://kemalcr.com) (0.26) | 139 778 | | |
| 19 | crystal (0.31)| [grip](https://github.com/grkek/grip) (0.26) | 135 693 | | |
| 20 | crystal (0.31)| [amber](https://amberframework.org) (0.3) | 130 436 | | |
| 21 | php (7.4)| [workerman](https://github.com/workerman/workerman) (3.5) | 127 923 | | |
| 22 | crystal (0.31)| [orion](https://github.com/obsidian/orion) (2.1) | 123 190 | | |
| 23 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.12) | 121 614 | | |
| 24 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 114 186 | | |
| 25 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.3) | 110 915 | | |
| 26 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 107 063 | | |
| 27 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 106 700 | | |
| 28 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 106 544 | | |
| 29 | csharp (7.3)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.0) | 105 489 | | |
| 30 | go (1.13)| [beego](https://beego.me) (1.12) | 104 410 | | |
| 31 | go (1.13)| [violetear](https://violetear.org) (7.0) | 102 747 | | |
| 32 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 102 445 | | |
| 33 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 101 889 | | |
| 34 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 101 109 | | |
| 35 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 99 084 | | |
| 36 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 96 244 | | |
| 37 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 90 070 | | |
| 38 | java (8)| [act](https://actframework.org) (1.8) | 89 923 | | |
| 39 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.0) | 89 119 | | |
| 40 | javascript (12.13)| [0http](https://github.com/jkyberneees/0http) (2.2) | 85 812 | | |
| 41 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 85 336 | | |
| 42 | javascript (12.13)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 83 349 | | |
| 43 | go (1.13)| [gf](https://goframe.org) (1.11) | 83 253 | | |
| 44 | javascript (12.13)| [restana](https://github.com/jkyberneees/ana) (4.0) | 77 379 | | |
| 45 | javascript (12.13)| [polka](https://github.com/lukeed/polka) (0.5) | 75 657 | | |
| 46 | javascript (12.13)| [rayo](https://rayo.js.org) (1.3) | 73 795 | | |
| 47 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 70 813 | | |
| 48 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 67 145 | | |
| 49 | javascript (12.13)| [foxify](https://foxify.js.org) (0.1) | 61 648 | | |
| 50 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 61 193 | | |
| 51 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 60 960 | | |
| 52 | javascript (12.13)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 59 968 | | |
| 53 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 56 884 | | |
| 54 | java (8)| [javalin](https://javalin.io) (3.5) | 55 112 | | |
| 55 | javascript (12.13)| [fastify](https://fastify.io) (2.11) | 54 663 | | |
| 56 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 52 794 | | |
| 57 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 52 389 | | |
| 58 | javascript (12.13)| [koa](https://koajs.com) (2.11) | 51 124 | | |
| 59 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 51 052 | | |
| 60 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 50 981 | | |
| 61 | clojure (1.10)| [coast](https://coastonclojure.com) (1.0) | 50 901 | | |
| 62 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 50 737 | | |
| 63 | rust (1.4)| [gotham](https://gotham.rs) (0.4) | 49 435 | | |
| 64 | javascript (12.13)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 48 700 | | |
| 65 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 46 693 | | |
| 66 | javascript (12.13)| [express](https://expressjs.com) (4.17) | 46 482 | | |
| 67 | python (3.8)| [hug](https://hug.rest) (2.6) | 45 993 | | |
| 68 | python (3.8)| [starlette](https://starlette.io) (0.13) | 45 478 | | |
| 69 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 44 901 | | |
| 70 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 44 757 | | |
| 71 | php (7.4)| [imi](https://imiphp.com) (1.0) | 43 383 | | |
| 72 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 40 607 | | |
| 73 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 39 489 | | |
| 74 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 38 509 | | |
| 75 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 38 190 | | |
| 76 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.28) | 37 381 | | |
| 77 | php (7.4)| [swoft](https://swoft.org) (2.0) | 37 233 | | |
| 78 | fsharp (7.3)| [suave](https://suave.io) (2.5) | 36 958 | | |
| 79 | rust (1.4)| [nickel](https://nickel-org.github.io) (0.11) | 36 740 | | |
| 80 | javascript (12.13)| [restify](https://restify.com) (8.5) | 35 339 | | |
| 81 | javascript (12.13)| [hapi](https://hapijs.com) (19.0) | 34 890 | | |
| 82 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 33 523 | | |
| 83 | python (3.8)| [responder](https://python-responder.org) (2.0) | 30 792 | | |
| 84 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.47) | 29 673 | | |
| 85 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 28 804 | | |
| 86 | java (8)| [micronaut](https://micronaut.io) (1.2) | 28 145 | | |
| 87 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 27 861 | | |
| 88 | javascript (12.13)| [moleculer](https://moleculer.services) (0.13) | 27 175 | | |
| 89 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 26 145 | | |
| 90 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 25 842 | | |
| 91 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 24 798 | | |
| 92 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 24 458 | | |
| 93 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 23 944 | | |
| 94 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 23 848 | | |
| 95 | crystal (0.31)| [lucky](https://luckyframework.org) (0.18) | 23 211 | | |
| 96 | crystal (0.31)| [athena](https://github.com/blacksmoke16/athena) (0.7) | 23 150 | | |
| 97 | javascript (12.13)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 595 | | |
| 98 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 20 982 | | |
| 99 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 20 440 | | |
| 100 | python (3.8)| [bocadillo](https://bocadilloproject.github.io) (0.18) | 20 440 | | |
| 101 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 19 965 | | |
| 102 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a1) | 18 905 | | |
| 103 | rust (1.4)| [iron](https://ironframework.io) (0.6) | 17 864 | | |
| 104 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 558 | | |
| 105 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 17 034 | | |
| 106 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 16 678 | | |
| 107 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 13 464 | | |
| 108 | ruby (2.6)| [grape](https://ruby-grape.org) (1.3) | 12 389 | | |
| 109 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.10) | 11 754 | | |
| 110 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 904 | | |
| 111 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 862 | | |
| 112 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 9 087 | | |
| 113 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 661 | | |
| 114 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 8 245 | | |
| 115 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 7 952 | | |
| 116 | python (3.8)| [django](https://djangoproject.com) (3.0) | 7 861 | | |
| 117 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 493 | | |
| 118 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 175 | | |
| 119 | crystal (0.31)| [onyx](https://onyxframework.org) (0.5) | 5 084 | | |
| 120 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 4 833 | | |
| 121 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 617 | | |
| 122 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 4 148 | | |
| 123 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 432 | | |
| 124 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 391 | | |
| 125 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 3 278 | | |
| 126 | php (7.4)| [laravel](https://laravel.com) (6.12) | 3 070 | | |
| 127 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 2 419 | | |
| 128 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 2 207 | | |
| 129 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 061 | | |
| 130 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 474 | | |
| 131 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 438 | | |
| 132 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 900 | | |

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

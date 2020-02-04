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

:information_source:  Updated on **2020-02-04** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 128
   + Connections : 128
   + Duration : 10 (seconds)

|    | Language | Framework | Speed (`req/s`) | Horizontal scale (parallelism) | Vertical scale (concurrency) |
|----|----------|-----------|----------------:|-------------|-------------|
| 1 | javascript (13.7)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 160 618 | | |
| 2 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 158 698 | | |
| 3 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 143 968 | | |
| 4 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 143 626 | | |
| 5 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 141 186 | | |
| 6 | crystal (0.31)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 139 968 | | |
| 7 | crystal (0.31)| [toro](https://github.com/soveran/toro) (0.4) | 139 383 | | |
| 8 | crystal (0.31)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 138 085 | | |
| 9 | crystal (0.31)| [raze](https://razecr.com) (0.3) | 137 724 | | |
| 10 | go (1.13)| [fiber](https://gofiber.github.io/fiber) (1.3) | 137 719 | | |
| 11 | crystal (0.31)| [kemal](https://kemalcr.com) (0.26) | 132 753 | | |
| 12 | crystal (0.31)| [grip](https://github.com/grkek/grip) (0.27) | 131 390 | | |
| 13 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.8) | 131 030 | | |
| 14 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 130 621 | | |
| 15 | go (1.13)| [router](https://github.com/fasthttp/router) (0.6) | 130 503 | | |
| 16 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 129 517 | | |
| 17 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.3) | 129 422 | | |
| 18 | crystal (0.31)| [amber](https://amberframework.org) (0.3) | 123 578 | | |
| 19 | crystal (0.31)| [orion](https://github.com/obsidian/orion) (2.1) | 120 203 | | |
| 20 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 118 120 | | |
| 21 | php (7.4)| [workerman](https://github.com/workerman/workerman) (3.5) | 117 443 | | |
| 22 | java (8)| [act](https://actframework.org) (1.8) | 98 763 | | |
| 23 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.12) | 97 953 | | |
| 24 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 90 042 | | |
| 25 | csharp (7.3)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.0) | 87 798 | | |
| 26 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 87 119 | | |
| 27 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 86 215 | | |
| 28 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 86 198 | | |
| 29 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 85 860 | | |
| 30 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 85 560 | | |
| 31 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 83 486 | | |
| 32 | go (1.13)| [beego](https://beego.me) (1.12) | 82 779 | | |
| 33 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 82 448 | | |
| 34 | go (1.13)| [violetear](https://violetear.org) (7.0) | 81 574 | | |
| 35 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 81 551 | | |
| 36 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 81 437 | | |
| 37 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.0) | 77 329 | | |
| 38 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 76 281 | | |
| 39 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.0) | 75 876 | | |
| 40 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 74 209 | | |
| 41 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 73 536 | | |
| 42 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 73 278 | | |
| 43 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 69 259 | | |
| 44 | go (1.13)| [gf](https://goframe.org) (1.11) | 67 219 | | |
| 45 | c (11)| [kore](https://kore.io) (3.3) | 65 864 | | |
| 46 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 65 054 | | |
| 47 | java (8)| [javalin](https://javalin.io) (3.5) | 64 047 | | |
| 48 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 62 029 | | |
| 49 | javascript (13.7)| [fastify](https://fastify.io) (2.11) | 61 716 | | |
| 50 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 61 640 | | |
| 51 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 61 250 | | |
| 52 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.12) | 60 431 | | |
| 53 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 60 008 | | |
| 54 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 58 147 | | |
| 55 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 57 665 | | |
| 56 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 56 962 | | |
| 57 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 55 508 | | |
| 58 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 55 221 | | |
| 59 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 55 161 | | |
| 60 | clojure (1.10)| [coast](https://coastonclojure.com) (1.0) | 53 893 | | |
| 61 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 51 588 | | |
| 62 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 49 219 | | |
| 63 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 47 883 | | |
| 64 | python (3.8)| [hug](https://hug.rest) (2.6) | 47 192 | | |
| 65 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 46 952 | | |
| 66 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 46 505 | | |
| 67 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 46 455 | | |
| 68 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 46 234 | | |
| 69 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 45 375 | | |
| 70 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 45 261 | | |
| 71 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 44 978 | | |
| 72 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 44 555 | | |
| 73 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 43 882 | | |
| 74 | python (3.8)| [starlette](https://starlette.io) (0.13) | 43 357 | | |
| 75 | java (8)| [micronaut](https://micronaut.io) (1.2) | 43 232 | | |
| 76 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 42 686 | | |
| 77 | php (7.4)| [imi](https://imiphp.com) (1.0) | 41 112 | | |
| 78 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 40 770 | | |
| 79 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.28) | 38 814 | | |
| 80 | javascript (13.7)| [restify](https://restify.com) (8.5) | 37 389 | | |
| 81 | php (7.4)| [swoft](https://swoft.org) (2.0) | 36 674 | | |
| 82 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 36 452 | | |
| 83 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 35 953 | | |
| 84 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 35 116 | | |
| 85 | python (3.8)| [responder](https://python-responder.org) (2.0) | 30 658 | | |
| 86 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 30 235 | | |
| 87 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.48) | 30 181 | | |
| 88 | javascript (13.7)| [moleculer](https://moleculer.services) (0.13) | 27 569 | | |
| 89 | crystal (0.31)| [lucky](https://luckyframework.org) (0.18) | 27 269 | | |
| 90 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 27 028 | | |
| 91 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 26 847 | | |
| 92 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 26 244 | | |
| 93 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 26 209 | | |
| 94 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 24 720 | | |
| 95 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 24 609 | | |
| 96 | fsharp (7.3)| [suave](https://suave.io) (2.5) | 24 017 | | |
| 97 | crystal (0.31)| [athena](https://github.com/blacksmoke16/athena) (0.7) | 23 453 | | |
| 98 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 891 | | |
| 99 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 22 009 | | |
| 100 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 21 515 | | |
| 101 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a1) | 21 101 | | |
| 102 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 20 712 | | |
| 103 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 19 215 | | |
| 104 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 19 167 | | |
| 105 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 18 136 | | |
| 106 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 16 975 | | |
| 107 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 14 866 | | |
| 108 | ruby (2.6)| [grape](https://ruby-grape.org) (1.3) | 12 671 | | |
| 109 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.10) | 12 014 | | |
| 110 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 10 772 | | |
| 111 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 10 250 | | |
| 112 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 10 236 | | |
| 113 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 107 | | |
| 114 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 9 160 | | |
| 115 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 841 | | |
| 116 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 458 | | |
| 117 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 435 | | |
| 118 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 7 282 | | |
| 119 | crystal (0.31)| [onyx](https://onyxframework.org) (0.5) | 5 259 | | |
| 120 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 4 952 | | |
| 121 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 497 | | |
| 122 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 4 257 | | |
| 123 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 3 487 | | |
| 124 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 425 | | |
| 125 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 3 285 | | |
| 126 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 078 | | |
| 127 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 137 | | |
| 128 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 1 890 | | |
| 129 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 604 | | |
| 130 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 435 | | |
| 131 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 1 389 | | |
| 132 | php (7.4)| [laravel](https://laravel.com) (6.12) | 418 | | |

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

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

:information_source:  Updated on **2020-02-09** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Connections : 64
   + Duration : 5s (seconds)

|    | Language | Framework | Speed (`req/s`) | Horizontal scale (parallelism) | Vertical scale (concurrency) |
|----|----------|-----------|----------------:|-------------|-------------|
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 835 774 | | |
| 2 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 662 703 | | |
| 3 | go (1.13)| [fiber](https://gofiber.github.io/fiber) (1.4) | 595 885 | | |
| 4 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 565 630 | | |
| 5 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 489 521 | | |
| 6 | javascript (13.7)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 472 886 | | |
| 7 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 458 575 | | |
| 8 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 421 790 | | |
| 9 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.9) | 420 498 | | |
| 10 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.3) | 404 720 | | |
| 11 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 402 119 | | |
| 12 | go (1.13)| [router](https://github.com/fasthttp/router) (0.6) | 400 968 | | |
| 13 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 391 965 | | |
| 14 | go (1.13)| [beego](https://beego.me) (1.12) | 359 279 | | |
| 15 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (3.5) | 349 897 | | |
| 16 | crystal (0.32)| [toro](https://github.com/soveran/toro) (0.4) | 335 812 | | |
| 17 | crystal (0.32)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 326 159 | | |
| 18 | crystal (0.32)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 314 973 | | |
| 19 | crystal (0.32)| [raze](https://razecr.com) (0.3) | 312 842 | | |
| 20 | c (11)| [kore](https://kore.io) (3.3) | 303 289 | | |
| 21 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.12) | 297 706 | | |
| 22 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 294 527 | | |
| 23 | crystal (0.32)| [kemal](https://kemalcr.com) (0.26) | 290 214 | | |
| 24 | crystal (0.32)| [grip](https://github.com/grkek/grip) (0.27) | 285 013 | | |
| 25 | java (8)| [act](https://actframework.org) (1.8) | 282 000 | | |
| 26 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 274 507 | | |
| 27 | crystal (0.32)| [amber](https://amberframework.org) (0.3) | 273 108 | | |
| 28 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 269 892 | | |
| 29 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 269 668 | | |
| 30 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 269 492 | | |
| 31 | go (1.13)| [violetear](https://violetear.org) (7.0) | 267 839 | | |
| 32 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 263 725 | | |
| 33 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 263 585 | | |
| 34 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 262 940 | | |
| 35 | crystal (0.32)| [orion](https://github.com/obsidian/orion) (2.1) | 260 004 | | |
| 36 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 252 895 | | |
| 37 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 249 397 | | |
| 38 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 244 917 | | |
| 39 | elixir (1.1)| [cowboy_stream](https://https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 243 455 | | |
| 40 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.0) | 241 443 | | |
| 41 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 233 822 | | |
| 42 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 228 563 | | |
| 43 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.0) | 228 022 | | |
| 44 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 221 291 | | |
| 45 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 218 634 | | |
| 46 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 218 426 | | |
| 47 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 208 805 | | |
| 48 | go (1.13)| [gf](https://goframe.org) (1.11) | 206 225 | | |
| 49 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 189 598 | | |
| 50 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 188 038 | | |
| 51 | javascript (13.7)| [fastify](https://fastify.io) (2.12) | 177 372 | | |
| 52 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.12) | 173 521 | | |
| 53 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 173 088 | | |
| 54 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 172 318 | | |
| 55 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 168 776 | | |
| 56 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 167 430 | | |
| 57 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 166 149 | | |
| 58 | java (8)| [javalin](https://javalin.io) (3.5) | 165 296 | | |
| 59 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 162 980 | | |
| 60 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 162 842 | | |
| 61 | elixir (1.1)| [cowboy](https://https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 160 063 | | |
| 62 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 159 032 | | |
| 63 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 158 623 | | |
| 64 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 156 373 | | |
| 65 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 151 448 | | |
| 66 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 148 228 | | |
| 67 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 147 268 | | |
| 68 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 146 242 | | |
| 69 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 145 804 | | |
| 70 | crystal (0.32)| [athena](https://github.com/athena-framework/athena) (0.8) | 143 779 | | |
| 71 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 142 134 | | |
| 72 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 138 729 | | |
| 73 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 138 075 | | |
| 74 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 137 492 | | |
| 75 | python (3.8)| [hug](https://hug.rest) (2.6) | 133 790 | | |
| 76 | python (3.8)| [starlette](https://starlette.io) (0.13) | 133 342 | | |
| 77 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 127 793 | | |
| 78 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 124 727 | | |
| 79 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 122 609 | | |
| 80 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 120 673 | | |
| 81 | java (8)| [micronaut](https://micronaut.io) (1.2) | 119 892 | | |
| 82 | php (7.4)| [imi](https://imiphp.com) (1.0) | 119 808 | | |
| 83 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 117 498 | | |
| 84 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 115 527 | | |
| 85 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.28) | 114 060 | | |
| 86 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 110 292 | | |
| 87 | javascript (13.7)| [restify](https://restify.com) (8.5) | 107 058 | | |
| 88 | php (7.4)| [swoft](https://swoft.org) (2.0) | 106 268 | | |
| 89 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 102 918 | | |
| 90 | python (3.8)| [responder](https://python-responder.org) (2.0) | 87 972 | | |
| 91 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 87 227 | | |
| 92 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.48) | 87 098 | | |
| 93 | javascript (13.7)| [moleculer](https://moleculer.services) (0.13) | 83 775 | | |
| 94 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 83 321 | | |
| 95 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 78 286 | | |
| 96 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 75 842 | | |
| 97 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 74 756 | | |
| 98 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 71 020 | | |
| 99 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 71 016 | | |
| 100 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 68 640 | | |
| 101 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 68 254 | | |
| 102 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 67 513 | | |
| 103 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 67 210 | | |
| 104 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 65 943 | | |
| 105 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 61 314 | | |
| 106 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 60 399 | | |
| 107 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a1) | 59 638 | | |
| 108 | crystal (0.32)| [lucky](https://luckyframework.org) (0.18) | 56 184 | | |
| 109 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 53 007 | | |
| 110 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 50 172 | | |
| 111 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 49 379 | | |
| 112 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 47 718 | | |
| 113 | ruby (2.6)| [grape](https://ruby-grape.org) (1.3) | 36 520 | | |
| 114 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 30 666 | | |
| 115 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 30 463 | | |
| 116 | python (3.8)| [django](https://djangoproject.com) (3.0) | 30 203 | | |
| 117 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 29 935 | | |
| 118 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 26 449 | | |
| 119 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 25 909 | | |
| 120 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 22 730 | | |
| 121 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 20 808 | | |
| 122 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 20 650 | | |
| 123 | crystal (0.32)| [onyx](https://onyxframework.org) (0.5) | 16 026 | | |
| 124 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 14 484 | | |
| 125 | php (7.4)| [slim](https://slimframework.com) (4.4) | 13 375 | | |
| 126 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 12 933 | | |
| 127 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 12 446 | | |
| 128 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 12 224 | | |
| 129 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 11 154 | | |
| 130 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 10 340 | | |
| 131 | php (7.4)| [symfony](https://symfony.com) (4.3) | 9 760 | | |
| 132 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 7 032 | | |
| 133 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 5 636 | | |
| 134 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 4 613 | | |
| 135 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 4 573 | | |
| 136 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 4 510 | | |
| 137 | php (7.4)| [laravel](https://laravel.com) (6.14) | 1 290 | | |
| 138 | nim (1.0)| [phoon](https://github.com/ducdetronquito/phoon) (0.1) | 15 | | |

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

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

:information_source:  Updated on **2020-02-07** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 128
   + Connections : 128
   + Duration : 10 (seconds)

|    | Language | Framework | Speed (`req/s`) | Horizontal scale (parallelism) | Vertical scale (concurrency) |
|----|----------|-----------|----------------:|-------------|-------------|
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 168 179 | | |
| 2 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 166 015 | | |
| 3 | javascript (13.7)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 158 161 | | |
| 4 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 144 574 | | |
| 5 | go (1.13)| [fiber](https://gofiber.github.io/fiber) (1.3) | 141 865 | | |
| 6 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 138 352 | | |
| 7 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.9) | 134 180 | | |
| 8 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 131 961 | | |
| 9 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 130 063 | | |
| 10 | go (1.13)| [router](https://github.com/fasthttp/router) (0.6) | 129 624 | | |
| 11 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.3) | 129 139 | | |
| 12 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 128 355 | | |
| 13 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (3.5) | 120 162 | | |
| 14 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.12) | 103 641 | | |
| 15 | crystal (0.32)| [toro](https://github.com/soveran/toro) (0.4) | 96 955 | | |
| 16 | java (8)| [act](https://actframework.org) (1.8) | 96 611 | | |
| 17 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 94 436 | | |
| 18 | crystal (0.32)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 93 580 | | |
| 19 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 90 477 | | |
| 20 | crystal (0.32)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 90 459 | | |
| 21 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 89 882 | | |
| 22 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 89 475 | | |
| 23 | crystal (0.32)| [raze](https://razecr.com) (0.3) | 88 532 | | |
| 24 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 87 815 | | |
| 25 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 87 661 | | |
| 26 | go (1.13)| [violetear](https://violetear.org) (7.0) | 86 448 | | |
| 27 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 85 792 | | |
| 28 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 85 026 | | |
| 29 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 83 882 | | |
| 30 | go (1.13)| [beego](https://beego.me) (1.12) | 82 486 | | |
| 31 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.0) | 81 678 | | |
| 32 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 81 513 | | |
| 33 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 81 489 | | |
| 34 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 81 309 | | |
| 35 | crystal (0.32)| [kemal](https://kemalcr.com) (0.26) | 80 507 | | |
| 36 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.0) | 79 876 | | |
| 37 | crystal (0.32)| [grip](https://github.com/grkek/grip) (0.27) | 79 516 | | |
| 38 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 77 190 | | |
| 39 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 76 320 | | |
| 40 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 76 111 | | |
| 41 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 75 396 | | |
| 42 | crystal (0.32)| [amber](https://amberframework.org) (0.3) | 74 368 | | |
| 43 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 72 932 | | |
| 44 | crystal (0.32)| [orion](https://github.com/obsidian/orion) (2.1) | 70 873 | | |
| 45 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 69 637 | | |
| 46 | go (1.13)| [gf](https://goframe.org) (1.11) | 69 273 | | |
| 47 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 64 955 | | |
| 48 | javascript (13.7)| [fastify](https://fastify.io) (2.12) | 60 638 | | |
| 49 | crystal (0.32)| [athena](https://github.com/athena-framework/athena) (0.8) | 60 387 | | |
| 50 | elixir (1.1)| [cowboy_stream](https://https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 60 377 | | |
| 51 | java (8)| [javalin](https://javalin.io) (3.5) | 60 019 | | |
| 52 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 59 608 | | |
| 53 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 58 899 | | |
| 54 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.12) | 58 870 | | |
| 55 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 58 688 | | |
| 56 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 57 883 | | |
| 57 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 57 729 | | |
| 58 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 56 210 | | |
| 59 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 55 215 | | |
| 60 | c (11)| [kore](https://kore.io) (3.3) | 54 333 | | |
| 61 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 54 246 | | |
| 62 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 54 183 | | |
| 63 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 50 169 | | |
| 64 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 50 135 | | |
| 65 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 50 114 | | |
| 66 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 49 647 | | |
| 67 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 48 182 | | |
| 68 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 47 059 | | |
| 69 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 46 771 | | |
| 70 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 46 763 | | |
| 71 | python (3.8)| [hug](https://hug.rest) (2.6) | 45 034 | | |
| 72 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 44 862 | | |
| 73 | java (8)| [micronaut](https://micronaut.io) (1.2) | 44 698 | | |
| 74 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 44 506 | | |
| 75 | python (3.8)| [starlette](https://starlette.io) (0.13) | 44 294 | | |
| 76 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 43 609 | | |
| 77 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 43 522 | | |
| 78 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 41 973 | | |
| 79 | php (7.4)| [imi](https://imiphp.com) (1.0) | 41 928 | | |
| 80 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 40 664 | | |
| 81 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 40 581 | | |
| 82 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 40 041 | | |
| 83 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.28) | 39 826 | | |
| 84 | javascript (13.7)| [restify](https://restify.com) (8.5) | 38 569 | | |
| 85 | elixir (1.1)| [cowboy](https://https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 37 874 | | |
| 86 | php (7.4)| [swoft](https://swoft.org) (2.0) | 36 393 | | |
| 87 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 35 872 | | |
| 88 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 34 129 | | |
| 89 | javascript (13.7)| [moleculer](https://moleculer.services) (0.13) | 30 424 | | |
| 90 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 30 200 | | |
| 91 | python (3.8)| [responder](https://python-responder.org) (2.0) | 29 819 | | |
| 92 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 28 855 | | |
| 93 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 28 454 | | |
| 94 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.48) | 28 347 | | |
| 95 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 27 301 | | |
| 96 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 26 418 | | |
| 97 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 25 590 | | |
| 98 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 25 458 | | |
| 99 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 23 462 | | |
| 100 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 23 112 | | |
| 101 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 22 972 | | |
| 102 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 724 | | |
| 103 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 21 016 | | |
| 104 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 20 631 | | |
| 105 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 20 506 | | |
| 106 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a1) | 19 661 | | |
| 107 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 18 466 | | |
| 108 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 18 161 | | |
| 109 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 480 | | |
| 110 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 16 174 | | |
| 111 | crystal (0.32)| [lucky](https://luckyframework.org) (0.18) | 14 487 | | |
| 112 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.10) | 13 303 | | |
| 113 | ruby (2.6)| [grape](https://ruby-grape.org) (1.3) | 11 993 | | |
| 114 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 563 | | |
| 115 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 10 365 | | |
| 116 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 10 187 | | |
| 117 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 779 | | |
| 118 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 9 436 | | |
| 119 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 085 | | |
| 120 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 7 912 | | |
| 121 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 463 | | |
| 122 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 418 | | |
| 123 | crystal (0.32)| [onyx](https://onyxframework.org) (0.5) | 5 735 | | |
| 124 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 4 849 | | |
| 125 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 608 | | |
| 126 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 395 | | |
| 127 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 4 319 | | |
| 128 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 3 938 | | |
| 129 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 3 867 | | |
| 130 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 603 | | |
| 131 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 519 | | |
| 132 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 379 | | |
| 133 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 1 943 | | |
| 134 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 555 | | |
| 135 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 522 | | |
| 136 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 1 355 | | |
| 137 | php (7.4)| [laravel](https://laravel.com) (6.14) | 408 | | |

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

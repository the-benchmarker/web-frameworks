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

|    | Language | Framework | Speed (`req/s`) | Horizontal scale (parallelism) | Vertical scale (concurrency) |
|----|----------|-----------|----------------:|-------------|-------------|
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 201 744 | | |
| 2 | javascript (12.13)| [nanoexpress-pro](https://nanoexpress.js.org) (1.9) | 199 308 | | |
| 3 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 196 235 | | |
| 4 | javascript (12.13)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 195 420 | | |
| 5 | javascript (12.13)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 185 995 | | |
| 6 | c (11)| [kore](https://kore.io) (3.3) | 168 658 | | |
| 7 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.7) | 166 889 | | |
| 8 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 166 230 | | |
| 9 | go (1.13)| [router](https://github.com/fasthttp/router) (1.6) | 162 194 | | |
| 10 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 161 369 | | |
| 11 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (9.0) | 158 704 | | |
| 12 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 152 895 | | |
| 13 | crystal (0.31)| [toro](https://github.com/soveran/toro) (0.4) | 151 462 | | |
| 14 | crystal (0.31)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 151 268 | | |
| 15 | go (1.13)| [fiber](https://fenny.github.io/fiber/#/) (0.4) | 147 392 | | |
| 16 | crystal (0.31)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 147 279 | | |
| 17 | crystal (0.31)| [raze](https://razecr.com) (0.3) | 146 979 | | |
| 18 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.2) | 143 904 | | |
| 19 | crystal (0.31)| [kemal](https://kemalcr.com) (0.26) | 139 859 | | |
| 20 | crystal (0.31)| [amber](https://amberframework.org) (0.3) | 130 980 | | |
| 21 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.11) | 127 215 | | |
| 22 | crystal (0.31)| [orion](https://github.com/obsidian/orion) (2.1) | 123 591 | | |
| 23 | java (8)| [act](https://actframework.org) (1.8) | 119 401 | | |
| 24 | php (7.4)| [workerman](https://github.com/workerman/workerman) (3.5) | 108 676 | | |
| 25 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 103 881 | | |
| 26 | csharp (7.3)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.0) | 103 411 | | |
| 27 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 102 078 | | |
| 28 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 100 551 | | |
| 29 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.2) | 99 851 | | |
| 30 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 98 136 | | |
| 31 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 98 126 | | |
| 32 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 96 549 | | |
| 33 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 96 192 | | |
| 34 | go (1.13)| [violetear](https://violetear.org) (7.0) | 93 481 | | |
| 35 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 90 899 | | |
| 36 | go (1.13)| [beego](https://beego.me) (1.12) | 90 388 | | |
| 37 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 90 049 | | |
| 38 | javascript (12.13)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 88 415 | | |
| 39 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 85 216 | | |
| 40 | javascript (12.13)| [0http](https://github.com/jkyberneees/0http) (2.2) | 84 770 | | |
| 41 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.0) | 82 084 | | |
| 42 | javascript (12.13)| [restana](https://github.com/jkyberneees/ana) (4.0) | 79 096 | | |
| 43 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 76 381 | | |
| 44 | javascript (12.13)| [polka](https://github.com/lukeed/polka) (0.5) | 76 313 | | |
| 45 | javascript (12.13)| [rayo](https://rayo.js.org) (1.3) | 74 984 | | |
| 46 | go (1.13)| [gf](https://goframe.org) (1.11) | 72 479 | | |
| 47 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 72 084 | | |
| 48 | javascript (12.13)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 67 843 | | |
| 49 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 66 974 | | |
| 50 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 65 950 | | |
| 51 | javascript (12.13)| [foxify](https://foxify.js.org) (0.1) | 62 786 | | |
| 52 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 60 564 | | |
| 53 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 60 415 | | |
| 54 | java (8)| [javalin](https://javalin.io) (3.5) | 59 978 | | |
| 55 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 56 976 | | |
| 56 | javascript (12.13)| [koa](https://koajs.com) (2.11) | 56 425 | | |
| 57 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 54 551 | | |
| 58 | javascript (12.13)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 54 299 | | |
| 59 | javascript (12.13)| [fastify](https://fastify.io) (2.11) | 53 362 | | |
| 60 | clojure (1.10)| [coast](https://coastonclojure.com) (1.0) | 52 751 | | |
| 61 | rust (1.4)| [gotham](https://gotham.rs) (0.4) | 52 157 | | |
| 62 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 51 662 | | |
| 63 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 51 368 | | |
| 64 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 50 856 | | |
| 65 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 50 384 | | |
| 66 | javascript (12.13)| [express](https://expressjs.com) (4.17) | 49 782 | | |
| 67 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 49 177 | | |
| 68 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 48 741 | | |
| 69 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 48 671 | | |
| 70 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 48 607 | | |
| 71 | python (3.8)| [hug](https://hug.rest) (2.6) | 47 799 | | |
| 72 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 47 446 | | |
| 73 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 47 120 | | |
| 74 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 45 402 | | |
| 75 | php (7.4)| [imi](https://imiphp.com) (1.0) | 45 399 | | |
| 76 | python (3.8)| [starlette](https://starlette.io) (0.13) | 45 068 | | |
| 77 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 44 998 | | |
| 78 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 44 341 | | |
| 79 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 44 024 | | |
| 80 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 42 789 | | |
| 81 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 42 229 | | |
| 82 | php (7.4)| [slim](https://slimframework.com) (4.4) | 42 160 | | |
| 83 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 42 089 | | |
| 84 | php (7.4)| [symfony](https://symfony.com) (4.3) | 41 903 | | |
| 85 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 40 496 | | |
| 86 | javascript (12.13)| [restify](https://restify.com) (8.5) | 40 248 | | |
| 87 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 39 497 | | |
| 88 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.27) | 38 231 | | |
| 89 | php (7.4)| [swoft](https://swoft.org) (2.0) | 37 909 | | |
| 90 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 37 760 | | |
| 91 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 34 763 | | |
| 92 | php (7.4)| [laravel](https://laravel.com) (6.1) | 34 308 | | |
| 93 | java (8)| [micronaut](https://micronaut.io) (1.2) | 33 111 | | |
| 94 | javascript (12.13)| [hapi](https://hapijs.com) (18.4) | 32 846 | | |
| 95 | fsharp (7.3)| [suave](https://suave.io) (2.5) | 32 225 | | |
| 96 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.46) | 29 342 | | |
| 97 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 29 037 | | |
| 98 | rust (1.4)| [nickel](https://nickel-org.github.io) (0.11) | 28 856 | | |
| 99 | javascript (12.13)| [moleculer](https://moleculer.services) (0.13) | 28 489 | | |
| 100 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 27 814 | | |
| 101 | python (3.8)| [responder](https://python-responder.org) (2.0) | 27 248 | | |
| 102 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 26 724 | | |
| 103 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 25 835 | | |
| 104 | crystal (0.31)| [athena](https://github.com/blacksmoke16/athena) (0.7) | 24 844 | | |
| 105 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 24 794 | | |
| 106 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 24 188 | | |
| 107 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 23 390 | | |
| 108 | javascript (12.13)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 431 | | |
| 109 | crystal (0.31)| [lucky](https://luckyframework.org) (0.18) | 22 249 | | |
| 110 | python (3.8)| [bocadillo](https://bocadilloproject.github.io) (0.18) | 20 733 | | |
| 111 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 20 569 | | |
| 112 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 18 904 | | |
| 113 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 18 664 | | |
| 114 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 16 746 | | |
| 115 | rust (1.4)| [iron](https://ironframework.io) (0.6) | 15 930 | | |
| 116 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 15 632 | | |
| 117 | ruby (2.6)| [grape](https://ruby-grape.org) (1.2) | 13 128 | | |
| 118 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 874 | | |
| 119 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.10) | 11 460 | | |
| 120 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 228 | | |
| 121 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 10 026 | | |
| 122 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 944 | | |
| 123 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 9 107 | | |
| 124 | python (3.8)| [masonite](https://masoniteproject.com) (2.2) | 6 929 | | |
| 125 | crystal (0.31)| [onyx](https://onyxframework.org) (0.5) | 4 931 | | |
| 126 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 3 735 | | |
| 127 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 2 328 | | |
| 128 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 323 | | |
| 129 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 1 512 | | |
| 130 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 496 | | |
| 131 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 421 | | |

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

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
| 1 | javascript (12.13)| [nanoexpress-pro](https://nanoexpress.js.org) (1.9) | 199 646 | | |
| 2 | javascript (12.13)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 194 823 | | |
| 3 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 189 612 | | |
| 4 | javascript (12.13)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 185 620 | | |
| 5 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 182 848 | | |
| 6 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.7) | 167 176 | | |
| 7 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 166 014 | | |
| 8 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 161 113 | | |
| 9 | go (1.13)| [router](https://github.com/fasthttp/router) (1.6) | 160 953 | | |
| 10 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (9.0) | 159 210 | | |
| 11 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.2) | 156 369 | | |
| 12 | c (11)| [kore](https://kore.io) (3.3) | 153 767 | | |
| 13 | crystal (0.31)| [toro](https://github.com/soveran/toro) (0.4) | 153 208 | | |
| 14 | crystal (0.31)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 150 885 | | |
| 15 | crystal (0.31)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 143 068 | | |
| 16 | crystal (0.31)| [raze](https://razecr.com) (0.3) | 141 910 | | |
| 17 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 140 275 | | |
| 18 | crystal (0.31)| [kemal](https://kemalcr.com) (0.26) | 137 083 | | |
| 19 | crystal (0.31)| [amber](https://amberframework.org) (0.3) | 130 279 | | |
| 20 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.11) | 128 466 | | |
| 21 | crystal (0.31)| [orion](https://github.com/obsidian/orion) (2.1) | 124 554 | | |
| 22 | java (8)| [act](https://actframework.org) (1.8) | 119 498 | | |
| 23 | php (7.4)| [workerman](https://github.com/workerman/workerman) (3.5) | 109 184 | | |
| 24 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 106 911 | | |
| 25 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.2) | 104 969 | | |
| 26 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 101 102 | | |
| 27 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 99 969 | | |
| 28 | csharp (7.3)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.0) | 99 959 | | |
| 29 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 98 141 | | |
| 30 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 97 880 | | |
| 31 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 96 577 | | |
| 32 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 96 411 | | |
| 33 | go (1.13)| [violetear](https://violetear.org) (7.0) | 96 148 | | |
| 34 | go (1.13)| [beego](https://beego.me) (1.12) | 94 319 | | |
| 35 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 94 082 | | |
| 36 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 90 890 | | |
| 37 | javascript (12.13)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 87 744 | | |
| 38 | javascript (12.13)| [0http](https://github.com/jkyberneees/0http) (2.2) | 84 876 | | |
| 39 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 84 790 | | |
| 40 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.0) | 84 095 | | |
| 41 | javascript (12.13)| [restana](https://github.com/jkyberneees/ana) (4.0) | 79 268 | | |
| 42 | go (1.13)| [gf](https://goframe.org) (1.11) | 78 614 | | |
| 43 | javascript (12.13)| [polka](https://github.com/lukeed/polka) (0.5) | 76 499 | | |
| 44 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 75 673 | | |
| 45 | javascript (12.13)| [rayo](https://rayo.js.org) (1.3) | 75 516 | | |
| 46 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 74 405 | | |
| 47 | java (8)| [javalin](https://javalin.io) (3.5) | 68 805 | | |
| 48 | javascript (12.13)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 67 232 | | |
| 49 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 66 058 | | |
| 50 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 63 403 | | |
| 51 | javascript (12.13)| [foxify](https://foxify.js.org) (0.1) | 62 756 | | |
| 52 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 62 686 | | |
| 53 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 60 669 | | |
| 54 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 59 505 | | |
| 55 | javascript (12.13)| [koa](https://koajs.com) (2.11) | 56 108 | | |
| 56 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 55 464 | | |
| 57 | javascript (12.13)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 54 515 | | |
| 58 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 54 306 | | |
| 59 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 52 161 | | |
| 60 | javascript (12.13)| [fastify](https://fastify.io) (2.11) | 52 048 | | |
| 61 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 52 032 | | |
| 62 | rust (1.4)| [gotham](https://gotham.rs) (0.4) | 51 698 | | |
| 63 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 51 623 | | |
| 64 | clojure (1.10)| [coast](https://coastonclojure.com) (1.0) | 51 348 | | |
| 65 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 51 331 | | |
| 66 | javascript (12.13)| [express](https://expressjs.com) (4.17) | 49 609 | | |
| 67 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 49 235 | | |
| 68 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 48 776 | | |
| 69 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 48 301 | | |
| 70 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 48 081 | | |
| 71 | python (3.8)| [hug](https://hug.rest) (2.6) | 48 017 | | |
| 72 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 46 553 | | |
| 73 | python (3.8)| [starlette](https://starlette.io) (0.13) | 45 774 | | |
| 74 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 45 287 | | |
| 75 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 44 544 | | |
| 76 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 43 598 | | |
| 77 | php (7.4)| [slim](https://slimframework.com) (4.4) | 43 043 | | |
| 78 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 42 956 | | |
| 79 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 42 884 | | |
| 80 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 42 341 | | |
| 81 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 42 296 | | |
| 82 | php (7.4)| [imi](https://imiphp.com) (1.0) | 42 289 | | |
| 83 | php (7.4)| [symfony](https://symfony.com) (4.3) | 41 913 | | |
| 84 | javascript (12.13)| [restify](https://restify.com) (8.5) | 40 455 | | |
| 85 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 40 127 | | |
| 86 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 39 211 | | |
| 87 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.27) | 38 874 | | |
| 88 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 37 739 | | |
| 89 | php (7.4)| [swoft](https://swoft.org) (2.0) | 35 696 | | |
| 90 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 34 631 | | |
| 91 | java (8)| [micronaut](https://micronaut.io) (1.2) | 34 075 | | |
| 92 | php (7.4)| [laravel](https://laravel.com) (6.1) | 33 111 | | |
| 93 | javascript (12.13)| [hapi](https://hapijs.com) (18.4) | 32 923 | | |
| 94 | rust (1.4)| [nickel](https://nickel-org.github.io) (0.11) | 31 063 | | |
| 95 | fsharp (7.3)| [suave](https://suave.io) (2.5) | 30 568 | | |
| 96 | python (3.8)| [responder](https://python-responder.org) (2.0) | 29 273 | | |
| 97 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 28 851 | | |
| 98 | javascript (12.13)| [moleculer](https://moleculer.services) (0.13) | 28 660 | | |
| 99 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.45) | 28 361 | | |
| 100 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 27 086 | | |
| 101 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 26 470 | | |
| 102 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 26 381 | | |
| 103 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 25 907 | | |
| 104 | crystal (0.31)| [athena](https://github.com/blacksmoke16/athena) (0.7) | 24 802 | | |
| 105 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 23 652 | | |
| 106 | crystal (0.31)| [lucky](https://luckyframework.org) (0.18) | 22 386 | | |
| 107 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 22 253 | | |
| 108 | javascript (12.13)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 21 859 | | |
| 109 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 21 124 | | |
| 110 | python (3.8)| [bocadillo](https://bocadilloproject.github.io) (0.18) | 20 717 | | |
| 111 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 20 292 | | |
| 112 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 17 391 | | |
| 113 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 16 855 | | |
| 114 | rust (1.4)| [iron](https://ironframework.io) (0.6) | 15 998 | | |
| 115 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 14 535 | | |
| 116 | ruby (2.6)| [grape](https://ruby-grape.org) (1.2) | 12 637 | | |
| 117 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 12 093 | | |
| 118 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.10) | 11 491 | | |
| 119 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 550 | | |
| 120 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 10 073 | | |
| 121 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 9 110 | | |
| 122 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 8 949 | | |
| 123 | python (3.8)| [masonite](https://masoniteproject.com) (2.2) | 6 943 | | |
| 124 | crystal (0.31)| [onyx](https://onyxframework.org) (0.5) | 4 885 | | |
| 125 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 3 824 | | |
| 126 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 328 | | |
| 127 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 2 316 | | |
| 128 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 1 702 | | |
| 129 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 529 | | |
| 130 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 399 | | |

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

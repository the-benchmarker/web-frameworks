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
| 1 | javascript (12.13)| [nanoexpress-pro](https://nanoexpress.js.org) (1.9) | 205 301 | | |
| 2 | javascript (12.13)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 198 565 | | |
| 3 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 195 403 | | |
| 4 | javascript (12.13)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 188 228 | | |
| 5 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 174 177 | | |
| 6 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 167 240 | | |
| 7 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.7) | 166 687 | | |
| 8 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 161 302 | | |
| 9 | go (1.13)| [router](https://github.com/fasthttp/router) (1.6) | 160 802 | | |
| 10 | c (11)| [kore](https://kore.io) (3.3) | 159 812 | | |
| 11 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.0) | 159 208 | | |
| 12 | go (1.13)| [fiber](https://fenny.github.io/fiber/#/) (0.4) | 154 394 | | |
| 13 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.2) | 152 209 | | |
| 14 | crystal (0.31)| [toro](https://github.com/soveran/toro) (0.4) | 148 284 | | |
| 15 | crystal (0.31)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 147 488 | | |
| 16 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 147 280 | | |
| 17 | crystal (0.31)| [raze](https://razecr.com) (0.3) | 142 802 | | |
| 18 | crystal (0.31)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 140 402 | | |
| 19 | crystal (0.31)| [kemal](https://kemalcr.com) (0.26) | 138 038 | | |
| 20 | crystal (0.31)| [amber](https://amberframework.org) (0.3) | 126 059 | | |
| 21 | java (8)| [act](https://actframework.org) (1.8) | 124 102 | | |
| 22 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.11) | 123 363 | | |
| 23 | crystal (0.31)| [orion](https://github.com/obsidian/orion) (2.1) | 122 738 | | |
| 24 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 104 427 | | |
| 25 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.2) | 104 202 | | |
| 26 | php (7.4)| [workerman](https://github.com/workerman/workerman) (3.5) | 103 118 | | |
| 27 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 101 815 | | |
| 28 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 99 841 | | |
| 29 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 98 989 | | |
| 30 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 97 838 | | |
| 31 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 97 385 | | |
| 32 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 96 125 | | |
| 33 | go (1.13)| [beego](https://beego.me) (1.12) | 93 870 | | |
| 34 | go (1.13)| [violetear](https://violetear.org) (7.0) | 93 815 | | |
| 35 | javascript (12.13)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 92 688 | | |
| 36 | csharp (7.3)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.0) | 92 315 | | |
| 37 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 91 514 | | |
| 38 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 87 274 | | |
| 39 | javascript (12.13)| [0http](https://github.com/jkyberneees/0http) (2.2) | 86 533 | | |
| 40 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 85 313 | | |
| 41 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.0) | 82 093 | | |
| 42 | javascript (12.13)| [polka](https://github.com/lukeed/polka) (0.5) | 76 859 | | |
| 43 | go (1.13)| [gf](https://goframe.org) (1.11) | 76 849 | | |
| 44 | javascript (12.13)| [restana](https://github.com/jkyberneees/ana) (4.0) | 76 320 | | |
| 45 | javascript (12.13)| [rayo](https://rayo.js.org) (1.3) | 74 586 | | |
| 46 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 74 421 | | |
| 47 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 74 224 | | |
| 48 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 73 166 | | |
| 49 | java (8)| [javalin](https://javalin.io) (3.5) | 66 392 | | |
| 50 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 64 940 | | |
| 51 | javascript (12.13)| [foxify](https://foxify.js.org) (0.1) | 64 340 | | |
| 52 | javascript (12.13)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 63 817 | | |
| 53 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 62 261 | | |
| 54 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 60 261 | | |
| 55 | javascript (12.13)| [koa](https://koajs.com) (2.11) | 59 338 | | |
| 56 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 57 895 | | |
| 57 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 55 630 | | |
| 58 | javascript (12.13)| [fastify](https://fastify.io) (2.11) | 54 682 | | |
| 59 | javascript (12.13)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 53 206 | | |
| 60 | rust (1.4)| [gotham](https://gotham.rs) (0.4) | 51 342 | | |
| 61 | clojure (1.10)| [coast](https://coastonclojure.com) (1.0) | 51 280 | | |
| 62 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 50 878 | | |
| 63 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 49 191 | | |
| 64 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 48 836 | | |
| 65 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 48 117 | | |
| 66 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 47 651 | | |
| 67 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 47 340 | | |
| 68 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 46 827 | | |
| 69 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 46 322 | | |
| 70 | javascript (12.13)| [express](https://expressjs.com) (4.17) | 46 058 | | |
| 71 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 45 844 | | |
| 72 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 45 651 | | |
| 73 | python (3.8)| [starlette](https://starlette.io) (0.13) | 44 403 | | |
| 74 | python (3.8)| [hug](https://hug.rest) (2.6) | 44 293 | | |
| 75 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 42 647 | | |
| 76 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 42 536 | | |
| 77 | php (7.4)| [imi](https://imiphp.com) (1.0) | 42 136 | | |
| 78 | php (7.4)| [slim](https://slimframework.com) (4.4) | 41 240 | | |
| 79 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 40 976 | | |
| 80 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 40 494 | | |
| 81 | fsharp (7.3)| [suave](https://suave.io) (2.5) | 40 467 | | |
| 82 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 40 340 | | |
| 83 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 40 303 | | |
| 84 | php (7.4)| [symfony](https://symfony.com) (4.3) | 39 912 | | |
| 85 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 39 202 | | |
| 86 | javascript (12.13)| [hapi](https://hapijs.com) (19.0) | 38 753 | | |
| 87 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 38 378 | | |
| 88 | javascript (12.13)| [restify](https://restify.com) (8.5) | 38 256 | | |
| 89 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 37 808 | | |
| 90 | java (8)| [micronaut](https://micronaut.io) (1.2) | 37 101 | | |
| 91 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.27) | 37 063 | | |
| 92 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 33 758 | | |
| 93 | php (7.4)| [swoft](https://swoft.org) (2.0) | 33 622 | | |
| 94 | php (7.4)| [laravel](https://laravel.com) (6.1) | 30 877 | | |
| 95 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.46) | 29 147 | | |
| 96 | rust (1.4)| [nickel](https://nickel-org.github.io) (0.11) | 28 873 | | |
| 97 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 28 481 | | |
| 98 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 28 333 | | |
| 99 | python (3.8)| [responder](https://python-responder.org) (2.0) | 27 311 | | |
| 100 | javascript (12.13)| [moleculer](https://moleculer.services) (0.13) | 27 245 | | |
| 101 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 26 652 | | |
| 102 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 24 975 | | |
| 103 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 24 604 | | |
| 104 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 24 471 | | |
| 105 | crystal (0.31)| [athena](https://github.com/blacksmoke16/athena) (0.7) | 23 575 | | |
| 106 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 428 | | |
| 107 | javascript (12.13)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 226 | | |
| 108 | crystal (0.31)| [lucky](https://luckyframework.org) (0.18) | 21 590 | | |
| 109 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 21 543 | | |
| 110 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 20 059 | | |
| 111 | python (3.8)| [bocadillo](https://bocadilloproject.github.io) (0.18) | 18 304 | | |
| 112 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 17 979 | | |
| 113 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 17 291 | | |
| 114 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 16 138 | | |
| 115 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 16 009 | | |
| 116 | rust (1.4)| [iron](https://ironframework.io) (0.6) | 15 927 | | |
| 117 | ruby (2.6)| [grape](https://ruby-grape.org) (1.3) | 12 624 | | |
| 118 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 738 | | |
| 119 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 411 | | |
| 120 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.10) | 10 228 | | |
| 121 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 10 137 | | |
| 122 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 713 | | |
| 123 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 8 668 | | |
| 124 | python (3.8)| [masonite](https://masoniteproject.com) (2.2) | 6 882 | | |
| 125 | crystal (0.31)| [onyx](https://onyxframework.org) (0.5) | 4 959 | | |
| 126 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 3 732 | | |
| 127 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 075 | | |
| 128 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 257 | | |
| 129 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 496 | | |
| 130 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 460 | | |
| 131 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 1 154 | | |

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

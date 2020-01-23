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
| 1 | javascript (12.13)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 187 659 | | |
| 2 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 182 069 | | |
| 3 | javascript (12.13)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 181 893 | | |
| 4 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.8) | 181 036 | | |
| 5 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 175 644 | | |
| 6 | go (1.13)| [router](https://github.com/fasthttp/router) (1.6) | 175 529 | | |
| 7 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 174 534 | | |
| 8 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.1) | 173 357 | | |
| 9 | go (1.13)| [fiber](https://fenny.github.io/fiber/#/) (0.6) | 172 407 | | |
| 10 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.2) | 170 084 | | |
| 11 | crystal (0.31)| [toro](https://github.com/soveran/toro) (0.4) | 167 805 | | |
| 12 | crystal (0.31)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 167 750 | | |
| 13 | crystal (0.31)| [raze](https://razecr.com) (0.3) | 162 575 | | |
| 14 | crystal (0.31)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 161 420 | | |
| 15 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 156 750 | | |
| 16 | crystal (0.31)| [kemal](https://kemalcr.com) (0.26) | 154 791 | | |
| 17 | crystal (0.31)| [grip](https://github.com/grkek/grip) (0.26) | 150 827 | | |
| 18 | crystal (0.31)| [amber](https://amberframework.org) (0.3) | 145 294 | | |
| 19 | crystal (0.31)| [orion](https://github.com/obsidian/orion) (2.1) | 138 207 | | |
| 20 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 134 170 | | |
| 21 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.12) | 134 060 | | |
| 22 | php (7.4)| [workerman](https://github.com/workerman/workerman) (3.5) | 128 818 | | |
| 23 | c (11)| [kore](https://kore.io) (3.3) | 124 858 | | |
| 24 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 119 207 | | |
| 25 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.2) | 115 878 | | |
| 26 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 111 296 | | |
| 27 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 111 021 | | |
| 28 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 110 209 | | |
| 29 | go (1.13)| [beego](https://beego.me) (1.12) | 108 038 | | |
| 30 | go (1.13)| [violetear](https://violetear.org) (7.0) | 107 291 | | |
| 31 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 106 863 | | |
| 32 | csharp (7.3)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.0) | 106 398 | | |
| 33 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 105 987 | | |
| 34 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 105 767 | | |
| 35 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 103 425 | | |
| 36 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 99 942 | | |
| 37 | java (8)| [act](https://actframework.org) (1.8) | 96 752 | | |
| 38 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.0) | 93 860 | | |
| 39 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 93 214 | | |
| 40 | javascript (12.13)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 86 184 | | |
| 41 | go (1.13)| [gf](https://goframe.org) (1.11) | 85 739 | | |
| 42 | javascript (12.13)| [0http](https://github.com/jkyberneees/0http) (2.2) | 82 629 | | |
| 43 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 79 793 | | |
| 44 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 77 759 | | |
| 45 | javascript (12.13)| [restana](https://github.com/jkyberneees/ana) (4.0) | 76 087 | | |
| 46 | javascript (12.13)| [polka](https://github.com/lukeed/polka) (0.5) | 73 731 | | |
| 47 | javascript (12.13)| [rayo](https://rayo.js.org) (1.3) | 72 570 | | |
| 48 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 70 194 | | |
| 49 | javascript (12.13)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 62 506 | | |
| 50 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 62 243 | | |
| 51 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 61 547 | | |
| 52 | javascript (12.13)| [foxify](https://foxify.js.org) (0.1) | 59 167 | | |
| 53 | javascript (12.13)| [fastify](https://fastify.io) (2.11) | 57 951 | | |
| 54 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 57 386 | | |
| 55 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 53 917 | | |
| 56 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 52 296 | | |
| 57 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 52 225 | | |
| 58 | javascript (12.13)| [koa](https://koajs.com) (2.11) | 51 826 | | |
| 59 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 51 517 | | |
| 60 | rust (1.4)| [gotham](https://gotham.rs) (0.4) | 51 212 | | |
| 61 | clojure (1.10)| [coast](https://coastonclojure.com) (1.0) | 50 918 | | |
| 62 | javascript (12.13)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 50 732 | | |
| 63 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 50 144 | | |
| 64 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 49 538 | | |
| 65 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 49 538 | | |
| 66 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 48 700 | | |
| 67 | python (3.8)| [starlette](https://starlette.io) (0.13) | 48 481 | | |
| 68 | python (3.8)| [hug](https://hug.rest) (2.6) | 48 126 | | |
| 69 | javascript (12.13)| [express](https://expressjs.com) (4.17) | 45 292 | | |
| 70 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 44 349 | | |
| 71 | php (7.4)| [imi](https://imiphp.com) (1.0) | 43 853 | | |
| 72 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 41 724 | | |
| 73 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.28) | 40 836 | | |
| 74 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 40 422 | | |
| 75 | java (8)| [javalin](https://javalin.io) (3.5) | 40 085 | | |
| 76 | php (7.4)| [swoft](https://swoft.org) (2.0) | 36 601 | | |
| 77 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 35 718 | | |
| 78 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 35 516 | | |
| 79 | javascript (12.13)| [restify](https://restify.com) (8.5) | 35 353 | | |
| 80 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 35 031 | | |
| 81 | javascript (12.13)| [hapi](https://hapijs.com) (19.0) | 33 611 | | |
| 82 | rust (1.4)| [nickel](https://nickel-org.github.io) (0.11) | 32 947 | | |
| 83 | fsharp (7.3)| [suave](https://suave.io) (2.5) | 32 617 | | |
| 84 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.47) | 31 214 | | |
| 85 | python (3.8)| [responder](https://python-responder.org) (2.0) | 30 871 | | |
| 86 | java (8)| [micronaut](https://micronaut.io) (1.2) | 30 867 | | |
| 87 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 30 794 | | |
| 88 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 27 504 | | |
| 89 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 27 368 | | |
| 90 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 26 997 | | |
| 91 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 26 567 | | |
| 92 | crystal (0.31)| [lucky](https://luckyframework.org) (0.18) | 26 194 | | |
| 93 | javascript (12.13)| [moleculer](https://moleculer.services) (0.13) | 25 392 | | |
| 94 | crystal (0.31)| [athena](https://github.com/blacksmoke16/athena) (0.7) | 24 887 | | |
| 95 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 24 511 | | |
| 96 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 24 498 | | |
| 97 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 23 079 | | |
| 98 | javascript (12.13)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 291 | | |
| 99 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 21 908 | | |
| 100 | python (3.8)| [bocadillo](https://bocadilloproject.github.io) (0.18) | 21 713 | | |
| 101 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 20 313 | | |
| 102 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a1) | 20 216 | | |
| 103 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 18 463 | | |
| 104 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 702 | | |
| 105 | rust (1.4)| [iron](https://ironframework.io) (0.6) | 17 476 | | |
| 106 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 17 333 | | |
| 107 | ruby (2.6)| [grape](https://ruby-grape.org) (1.3) | 14 751 | | |
| 108 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 13 359 | | |
| 109 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.10) | 12 235 | | |
| 110 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 378 | | |
| 111 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 10 348 | | |
| 112 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 10 098 | | |
| 113 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 9 183 | | |
| 114 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 421 | | |
| 115 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 7 984 | | |
| 116 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 419 | | |
| 117 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 186 | | |
| 118 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 7 164 | | |
| 119 | crystal (0.31)| [onyx](https://onyxframework.org) (0.5) | 4 600 | | |
| 120 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 474 | | |
| 121 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 4 225 | | |
| 122 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 4 029 | | |
| 123 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 3 633 | | |
| 124 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 519 | | |
| 125 | php (7.4)| [laravel](https://laravel.com) (6.12) | 3 295 | | |
| 126 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 078 | | |
| 127 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 2 946 | | |
| 128 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 251 | | |
| 129 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 2 166 | | |
| 130 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 523 | | |
| 131 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 450 | | |
| 132 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 929 | | |

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



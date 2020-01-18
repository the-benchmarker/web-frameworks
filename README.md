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
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 197 779 | | |
| 2 | javascript (12.13)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 192 118 | | |
| 3 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 191 165 | | |
| 4 | javascript (12.13)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 182 357 | | |
| 5 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.8) | 182 029 | | |
| 6 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 176 686 | | |
| 7 | go (1.13)| [router](https://github.com/fasthttp/router) (1.6) | 175 916 | | |
| 8 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.1) | 173 652 | | |
| 9 | go (1.13)| [fiber](https://fenny.github.io/fiber/#/) (0.6) | 173 086 | | |
| 10 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.2) | 170 673 | | |
| 11 | crystal (0.31)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 166 463 | | |
| 12 | crystal (0.31)| [toro](https://github.com/soveran/toro) (0.4) | 165 437 | | |
| 13 | c (11)| [kore](https://kore.io) (3.3) | 163 836 | | |
| 14 | crystal (0.31)| [raze](https://razecr.com) (0.3) | 162 319 | | |
| 15 | crystal (0.31)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 160 622 | | |
| 16 | crystal (0.31)| [grip](https://github.com/grkek/grip) (0.26) | 158 762 | | |
| 17 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 158 121 | | |
| 18 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 157 828 | | |
| 19 | crystal (0.31)| [kemal](https://kemalcr.com) (0.26) | 155 151 | | |
| 20 | crystal (0.31)| [amber](https://amberframework.org) (0.3) | 145 573 | | |
| 21 | crystal (0.31)| [orion](https://github.com/obsidian/orion) (2.1) | 139 872 | | |
| 22 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.11) | 133 952 | | |
| 23 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 118 346 | | |
| 24 | php (7.4)| [workerman](https://github.com/workerman/workerman) (3.5) | 115 736 | | |
| 25 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.2) | 115 478 | | |
| 26 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 111 681 | | |
| 27 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 111 018 | | |
| 28 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 110 138 | | |
| 29 | csharp (7.3)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.0) | 107 787 | | |
| 30 | go (1.13)| [beego](https://beego.me) (1.12) | 107 784 | | |
| 31 | go (1.13)| [violetear](https://violetear.org) (7.0) | 106 996 | | |
| 32 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 106 040 | | |
| 33 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 105 988 | | |
| 34 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 105 665 | | |
| 35 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 103 618 | | |
| 36 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 99 921 | | |
| 37 | java (8)| [act](https://actframework.org) (1.8) | 96 080 | | |
| 38 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.0) | 94 838 | | |
| 39 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 93 032 | | |
| 40 | javascript (12.13)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 86 573 | | |
| 41 | go (1.13)| [gf](https://goframe.org) (1.11) | 86 149 | | |
| 42 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 82 499 | | |
| 43 | javascript (12.13)| [0http](https://github.com/jkyberneees/0http) (2.2) | 82 069 | | |
| 44 | javascript (12.13)| [restana](https://github.com/jkyberneees/ana) (4.0) | 77 019 | | |
| 45 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 76 873 | | |
| 46 | javascript (12.13)| [polka](https://github.com/lukeed/polka) (0.5) | 72 615 | | |
| 47 | javascript (12.13)| [rayo](https://rayo.js.org) (1.3) | 71 972 | | |
| 48 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 70 127 | | |
| 49 | javascript (12.13)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 62 099 | | |
| 50 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 60 504 | | |
| 51 | javascript (12.13)| [foxify](https://foxify.js.org) (0.1) | 58 324 | | |
| 52 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 57 972 | | |
| 53 | javascript (12.13)| [fastify](https://fastify.io) (2.11) | 57 735 | | |
| 54 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 57 331 | | |
| 55 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 52 637 | | |
| 56 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 52 037 | | |
| 57 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 51 257 | | |
| 58 | javascript (12.13)| [koa](https://koajs.com) (2.11) | 50 940 | | |
| 59 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 50 610 | | |
| 60 | javascript (12.13)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 50 532 | | |
| 61 | rust (1.4)| [gotham](https://gotham.rs) (0.4) | 50 057 | | |
| 62 | clojure (1.10)| [coast](https://coastonclojure.com) (1.0) | 49 798 | | |
| 63 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 49 262 | | |
| 64 | python (3.8)| [hug](https://hug.rest) (2.6) | 48 466 | | |
| 65 | python (3.8)| [starlette](https://starlette.io) (0.13) | 47 930 | | |
| 66 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 46 306 | | |
| 67 | php (7.4)| [imi](https://imiphp.com) (1.0) | 45 632 | | |
| 68 | javascript (12.13)| [express](https://expressjs.com) (4.17) | 44 903 | | |
| 69 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 44 523 | | |
| 70 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 44 394 | | |
| 71 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 42 964 | | |
| 72 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 42 251 | | |
| 73 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.28) | 41 299 | | |
| 74 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 41 260 | | |
| 75 | java (8)| [javalin](https://javalin.io) (3.5) | 39 404 | | |
| 76 | php (7.4)| [swoft](https://swoft.org) (2.0) | 38 201 | | |
| 77 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 36 734 | | |
| 78 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 34 844 | | |
| 79 | fsharp (7.3)| [suave](https://suave.io) (2.5) | 34 628 | | |
| 80 | javascript (12.13)| [restify](https://restify.com) (8.5) | 34 353 | | |
| 81 | java (8)| [micronaut](https://micronaut.io) (1.2) | 33 959 | | |
| 82 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 33 941 | | |
| 83 | javascript (12.13)| [hapi](https://hapijs.com) (19.0) | 33 832 | | |
| 84 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.46) | 31 409 | | |
| 85 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 30 408 | | |
| 86 | rust (1.4)| [nickel](https://nickel-org.github.io) (0.11) | 29 638 | | |
| 87 | python (3.8)| [responder](https://python-responder.org) (2.0) | 28 129 | | |
| 88 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 27 469 | | |
| 89 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 26 958 | | |
| 90 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 26 898 | | |
| 91 | crystal (0.31)| [lucky](https://luckyframework.org) (0.18) | 25 321 | | |
| 92 | javascript (12.13)| [moleculer](https://moleculer.services) (0.13) | 25 239 | | |
| 93 | crystal (0.31)| [athena](https://github.com/blacksmoke16/athena) (0.7) | 25 121 | | |
| 94 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 24 710 | | |
| 95 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 24 497 | | |
| 96 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 22 972 | | |
| 97 | javascript (12.13)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 708 | | |
| 98 | python (3.8)| [bocadillo](https://bocadilloproject.github.io) (0.18) | 21 592 | | |
| 99 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 21 220 | | |
| 100 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 20 966 | | |
| 101 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 20 956 | | |
| 102 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a1) | 19 802 | | |
| 103 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 18 618 | | |
| 104 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 537 | | |
| 105 | rust (1.4)| [iron](https://ironframework.io) (0.6) | 17 324 | | |
| 106 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 17 232 | | |
| 107 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 14 096 | | |
| 108 | ruby (2.6)| [grape](https://ruby-grape.org) (1.3) | 14 053 | | |
| 109 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.10) | 12 318 | | |
| 110 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 468 | | |
| 111 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 10 417 | | |
| 112 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 10 183 | | |
| 113 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 127 | | |
| 114 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 9 098 | | |
| 115 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 8 139 | | |
| 116 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 7 729 | | |
| 117 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 660 | | |
| 118 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 291 | | |
| 119 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 754 | | |
| 120 | crystal (0.31)| [onyx](https://onyxframework.org) (0.5) | 4 629 | | |
| 121 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 4 199 | | |
| 122 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 4 034 | | |
| 123 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 3 792 | | |
| 124 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 673 | | |
| 125 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 310 | | |
| 126 | php (7.4)| [laravel](https://laravel.com) (6.11) | 2 644 | | |
| 127 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 2 533 | | |
| 128 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 237 | | |
| 129 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 2 217 | | |
| 130 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 534 | | |
| 131 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 462 | | |
| 132 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 1 029 | | |

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

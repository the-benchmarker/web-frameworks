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
| 1 | javascript (12.13)| [nanoexpress-pro](https://nanoexpress.js.org) (1.9) | 198 860 | | |
| 2 | javascript (12.13)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 195 234 | | |
| 3 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 188 641 | | |
| 4 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 187 080 | | |
| 5 | javascript (12.13)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 176 985 | | |
| 6 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 169 768 | | |
| 7 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.8) | 165 592 | | |
| 8 | go (1.13)| [router](https://github.com/fasthttp/router) (1.6) | 162 162 | | |
| 9 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 160 647 | | |
| 10 | c (11)| [kore](https://kore.io) (3.3) | 160 572 | | |
| 11 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.0) | 159 683 | | |
| 12 | go (1.13)| [fiber](https://fenny.github.io/fiber/#/) (0.6) | 158 993 | | |
| 13 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.2) | 155 206 | | |
| 14 | crystal (0.31)| [toro](https://github.com/soveran/toro) (0.4) | 149 431 | | |
| 15 | crystal (0.31)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 146 298 | | |
| 16 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 144 426 | | |
| 17 | crystal (0.31)| [raze](https://razecr.com) (0.3) | 142 556 | | |
| 18 | crystal (0.31)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 141 653 | | |
| 19 | crystal (0.31)| [kemal](https://kemalcr.com) (0.26) | 136 613 | | |
| 20 | crystal (0.31)| [amber](https://amberframework.org) (0.3) | 124 102 | | |
| 21 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.11) | 123 228 | | |
| 22 | java (8)| [act](https://actframework.org) (1.8) | 118 310 | | |
| 23 | crystal (0.31)| [orion](https://github.com/obsidian/orion) (2.1) | 118 307 | | |
| 24 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 108 547 | | |
| 25 | php (7.4)| [workerman](https://github.com/workerman/workerman) (3.5) | 107 788 | | |
| 26 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.2) | 104 524 | | |
| 27 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 101 032 | | |
| 28 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 99 422 | | |
| 29 | csharp (7.3)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.0) | 98 219 | | |
| 30 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 98 133 | | |
| 31 | go (1.13)| [violetear](https://violetear.org) (7.0) | 97 221 | | |
| 32 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 96 729 | | |
| 33 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 96 376 | | |
| 34 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 96 066 | | |
| 35 | go (1.13)| [beego](https://beego.me) (1.12) | 94 986 | | |
| 36 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 93 852 | | |
| 37 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 90 907 | | |
| 38 | javascript (12.13)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 86 995 | | |
| 39 | javascript (12.13)| [0http](https://github.com/jkyberneees/0http) (2.2) | 83 858 | | |
| 40 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 83 832 | | |
| 41 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.0) | 82 636 | | |
| 42 | javascript (12.13)| [restana](https://github.com/jkyberneees/ana) (4.0) | 78 635 | | |
| 43 | go (1.13)| [gf](https://goframe.org) (1.11) | 78 279 | | |
| 44 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 75 950 | | |
| 45 | javascript (12.13)| [polka](https://github.com/lukeed/polka) (0.5) | 75 937 | | |
| 46 | javascript (12.13)| [rayo](https://rayo.js.org) (1.3) | 74 702 | | |
| 47 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 74 660 | | |
| 48 | javascript (12.13)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 66 824 | | |
| 49 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 66 029 | | |
| 50 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 62 915 | | |
| 51 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 62 843 | | |
| 52 | javascript (12.13)| [foxify](https://foxify.js.org) (0.1) | 62 575 | | |
| 53 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 60 846 | | |
| 54 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 60 744 | | |
| 55 | javascript (12.13)| [koa](https://koajs.com) (2.11) | 55 676 | | |
| 56 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 54 495 | | |
| 57 | javascript (12.13)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 54 058 | | |
| 58 | java (8)| [javalin](https://javalin.io) (3.5) | 53 483 | | |
| 59 | javascript (12.13)| [fastify](https://fastify.io) (2.11) | 52 374 | | |
| 60 | clojure (1.10)| [coast](https://coastonclojure.com) (1.0) | 52 084 | | |
| 61 | rust (1.4)| [gotham](https://gotham.rs) (0.4) | 51 991 | | |
| 62 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 51 959 | | |
| 63 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 51 701 | | |
| 64 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 50 361 | | |
| 65 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 49 495 | | |
| 66 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 49 103 | | |
| 67 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 48 980 | | |
| 68 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 48 889 | | |
| 69 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 48 716 | | |
| 70 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 47 412 | | |
| 71 | javascript (12.13)| [express](https://expressjs.com) (4.17) | 47 220 | | |
| 72 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 46 166 | | |
| 73 | python (3.8)| [starlette](https://starlette.io) (0.13) | 45 902 | | |
| 74 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 44 291 | | |
| 75 | python (3.8)| [hug](https://hug.rest) (2.6) | 44 068 | | |
| 76 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 43 980 | | |
| 77 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 43 333 | | |
| 78 | php (7.4)| [slim](https://slimframework.com) (4.4) | 43 026 | | |
| 79 | php (7.4)| [symfony](https://symfony.com) (4.3) | 42 696 | | |
| 80 | php (7.4)| [imi](https://imiphp.com) (1.0) | 42 679 | | |
| 81 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 42 453 | | |
| 82 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 42 320 | | |
| 83 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 42 173 | | |
| 84 | javascript (12.13)| [restify](https://restify.com) (8.5) | 40 794 | | |
| 85 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 40 143 | | |
| 86 | javascript (12.13)| [hapi](https://hapijs.com) (19.0) | 39 741 | | |
| 87 | java (8)| [micronaut](https://micronaut.io) (1.2) | 38 556 | | |
| 88 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 37 768 | | |
| 89 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.27) | 37 091 | | |
| 90 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 37 062 | | |
| 91 | php (7.4)| [swoft](https://swoft.org) (2.0) | 35 747 | | |
| 92 | fsharp (7.3)| [suave](https://suave.io) (2.5) | 34 867 | | |
| 93 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 33 761 | | |
| 94 | php (7.4)| [laravel](https://laravel.com) (6.1) | 33 006 | | |
| 95 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.46) | 29 679 | | |
| 96 | rust (1.4)| [nickel](https://nickel-org.github.io) (0.11) | 29 604 | | |
| 97 | python (3.8)| [responder](https://python-responder.org) (2.0) | 29 355 | | |
| 98 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 27 914 | | |
| 99 | javascript (12.13)| [moleculer](https://moleculer.services) (0.13) | 27 552 | | |
| 100 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 26 657 | | |
| 101 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 26 065 | | |
| 102 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 25 800 | | |
| 103 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 25 108 | | |
| 104 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 24 614 | | |
| 105 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 24 136 | | |
| 106 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 24 063 | | |
| 107 | crystal (0.31)| [athena](https://github.com/blacksmoke16/athena) (0.7) | 23 930 | | |
| 108 | javascript (12.13)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 21 964 | | |
| 109 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 21 961 | | |
| 110 | crystal (0.31)| [lucky](https://luckyframework.org) (0.18) | 21 880 | | |
| 111 | python (3.8)| [bocadillo](https://bocadilloproject.github.io) (0.18) | 21 442 | | |
| 112 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 21 108 | | |
| 113 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 20 573 | | |
| 114 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 17 979 | | |
| 115 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 16 863 | | |
| 116 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 16 275 | | |
| 117 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 15 879 | | |
| 118 | rust (1.4)| [iron](https://ironframework.io) (0.6) | 15 848 | | |
| 119 | ruby (2.6)| [grape](https://ruby-grape.org) (1.3) | 13 587 | | |
| 120 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 12 136 | | |
| 121 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.10) | 11 347 | | |
| 122 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 380 | | |
| 123 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 10 029 | | |
| 124 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 613 | | |
| 125 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 9 171 | | |
| 126 | crystal (0.31)| [onyx](https://onyxframework.org) (0.5) | 4 889 | | |
| 127 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 3 685 | | |
| 128 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 2 502 | | |
| 129 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 130 | | |
| 130 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 1 869 | | |
| 131 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 1 468 | | |
| 132 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 449 | | |
| 133 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 410 | | |

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

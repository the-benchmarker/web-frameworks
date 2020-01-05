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

+ Initialize `sqlite` database

~~~sh
bin/db init
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
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 195 384 | | |
| 2 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 193 017 | | |
| 3 | javascript (12.13)| [nanoexpress-pro](https://nanoexpress.js.org) (1.8) | 177 957 | | |
| 4 | javascript (12.13)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 165 241 | | |
| 5 | javascript (12.13)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 164 918 | | |
| 6 | c (11)| [kore](https://kore.io) (3.3) | 163 195 | | |
| 7 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.7) | 143 532 | | |
| 8 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 143 459 | | |
| 9 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 143 333 | | |
| 10 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 139 377 | | |
| 11 | go (1.13)| [router](https://github.com/fasthttp/router) (1.6) | 138 758 | | |
| 12 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (9.0) | 135 965 | | |
| 13 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.2) | 135 875 | | |
| 14 | crystal (0.31)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 132 314 | | |
| 15 | crystal (0.31)| [toro](https://github.com/soveran/toro) (0.4) | 131 688 | | |
| 16 | crystal (0.31)| [raze](https://razecr.com) (0.3) | 128 752 | | |
| 17 | crystal (0.31)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 128 282 | | |
| 18 | crystal (0.31)| [kemal](https://kemalcr.com) (0.26) | 121 531 | | |
| 19 | crystal (0.31)| [amber](https://amberframework.org) (0.3) | 111 855 | | |
| 20 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.11) | 110 977 | | |
| 21 | java (8)| [act](https://actframework.org) (1.8) | 107 463 | | |
| 22 | crystal (0.31)| [orion](https://github.com/obsidian/orion) (2.1) | 104 984 | | |
| 23 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 95 550 | | |
| 24 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.2) | 92 430 | | |
| 25 | php (7.4)| [workerman](https://github.com/workerman/workerman) (3.5) | 90 257 | | |
| 26 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 88 198 | | |
| 27 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 85 847 | | |
| 28 | go (1.13)| [violetear](https://violetear.org) (7.0) | 85 752 | | |
| 29 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 85 101 | | |
| 30 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 85 029 | | |
| 31 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 84 632 | | |
| 32 | go (1.13)| [beego](https://beego.me) (1.12) | 84 018 | | |
| 33 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 83 631 | | |
| 34 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 82 935 | | |
| 35 | csharp (7.3)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.0) | 81 896 | | |
| 36 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 75 145 | | |
| 37 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.0) | 74 728 | | |
| 38 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 74 165 | | |
| 39 | javascript (12.13)| [restana](https://github.com/jkyberneees/ana) (4.0) | 72 909 | | |
| 40 | javascript (12.13)| [polka](https://github.com/lukeed/polka) (0.5) | 68 432 | | |
| 41 | go (1.13)| [gf](https://goframe.org) (1.1) | 67 495 | | |
| 42 | javascript (12.13)| [rayo](https://rayo.js.org) (1.3) | 65 647 | | |
| 43 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 62 271 | | |
| 44 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 61 656 | | |
| 45 | javascript (12.13)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 60 625 | | |
| 46 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 57 400 | | |
| 47 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 53 673 | | |
| 48 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 53 594 | | |
| 49 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 53 406 | | |
| 50 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 52 274 | | |
| 51 | javascript (12.13)| [foxify](https://foxify.js.org) (0.1) | 51 144 | | |
| 52 | clojure (1.10)| [coast](https://coastonclojure.com) (1.0) | 51 126 | | |
| 53 | rust (1.39)| [gotham](https://gotham.rs) (0.4) | 49 683 | | |
| 54 | javascript (12.13)| [koa](https://koajs.com) (2.11) | 48 802 | | |
| 55 | javascript (12.13)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 48 083 | | |
| 56 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 47 148 | | |
| 57 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 46 986 | | |
| 58 | javascript (12.13)| [fastify](https://fastify.io) (2.11) | 46 501 | | |
| 59 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 44 704 | | |
| 60 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 44 502 | | |
| 61 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 43 720 | | |
| 62 | javascript (12.13)| [express](https://expressjs.com) (4.17) | 43 568 | | |
| 63 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 43 023 | | |
| 64 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 42 021 | | |
| 65 | javascript (12.13)| [0http](https://github.com/jkyberneees/0http) (2.2) | 40 922 | | |
| 66 | python (3.8)| [hug](https://hug.rest) (2.6) | 40 898 | | |
| 67 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 40 740 | | |
| 68 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 40 711 | | |
| 69 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 40 562 | | |
| 70 | python (3.8)| [starlette](https://starlette.io) (0.13) | 39 926 | | |
| 71 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 38 705 | | |
| 72 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 38 518 | | |
| 73 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 37 982 | | |
| 74 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 37 635 | | |
| 75 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 37 626 | | |
| 76 | php (7.4)| [slim](https://slimframework.com) (4.3) | 37 531 | | |
| 77 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 37 487 | | |
| 78 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 37 296 | | |
| 79 | php (7.4)| [symfony](https://symfony.com) (4.3) | 37 205 | | |
| 80 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 37 117 | | |
| 81 | php (7.4)| [imi](https://imiphp.com) (1.0) | 36 585 | | |
| 82 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 36 108 | | |
| 83 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 35 626 | | |
| 84 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 35 507 | | |
| 85 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.27) | 34 676 | | |
| 86 | javascript (12.13)| [restify](https://restify.com) (8.5) | 33 001 | | |
| 87 | php (7.4)| [swoft](https://swoft.org) (2.0) | 31 292 | | |
| 88 | rust (1.39)| [nickel](https://nickel-org.github.io) (0.11) | 31 123 | | |
| 89 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 31 031 | | |
| 90 | php (7.4)| [laravel](https://laravel.com) (6.9) | 30 612 | | |
| 91 | java (8)| [micronaut](https://micronaut.io) (1.2) | 27 275 | | |
| 92 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.45) | 26 173 | | |
| 93 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 25 951 | | |
| 94 | javascript (12.13)| [hapi](https://hapijs.com) (18.4) | 25 831 | | |
| 95 | python (3.8)| [responder](https://python-responder.org) (2.0) | 24 243 | | |
| 96 | javascript (12.13)| [moleculer](https://moleculer.services) (0.13) | 23 428 | | |
| 97 | javascript (12.13)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 656 | | |
| 98 | crystal (0.31)| [lucky](https://luckyframework.org) (0.18) | 22 645 | | |
| 99 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 21 554 | | |
| 100 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 21 509 | | |
| 101 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 21 040 | | |
| 102 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 20 783 | | |
| 103 | crystal (0.31)| [athena](https://github.com/blacksmoke16/athena) (0.7) | 20 597 | | |
| 104 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 20 429 | | |
| 105 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 19 632 | | |
| 106 | java (8)| [javalin](https://javalin.io) (3.5) | 19 055 | | |
| 107 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 18 880 | | |
| 108 | python (3.8)| [bocadillo](https://bocadilloproject.github.io) (0.18) | 18 470 | | |
| 109 | rust (1.39)| [iron](https://ironframework.io) (0.6) | 16 844 | | |
| 110 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 16 833 | | |
| 111 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.9) | 15 809 | | |
| 112 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 15 023 | | |
| 113 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 14 747 | | |
| 114 | ruby (2.6)| [grape](https://ruby-grape.org) (1.2) | 11 486 | | |
| 115 | javascript (12.13)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 11 320 | | |
| 116 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.10) | 10 101 | | |
| 117 | fsharp (7.3)| [suave](https://suave.io) (2.5) | 10 061 | | |
| 118 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 10 017 | | |
| 119 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 124 | | |
| 120 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 8 839 | | |
| 121 | python (3.8)| [django](https://djangoproject.com) (3.0) | 8 354 | | |
| 122 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 7 999 | | |
| 123 | crystal (0.31)| [onyx](https://onyxframework.org) (0.5) | 6 644 | | |
| 124 | python (3.8)| [masonite](https://masoniteproject.com) (2.2) | 6 247 | | |
| 125 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 3 481 | | |
| 126 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 405 | | |
| 127 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 089 | | |
| 128 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 477 | | |
| 129 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 325 | | |
| 130 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 942 | | |
| 131 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 910 | | |

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

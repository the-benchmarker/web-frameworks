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
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 191 573 | | |
| 2 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 185 350 | | |
| 3 | javascript (12.13)| [nanoexpress-pro](https://nanoexpress.js.org) (1.8) | 170 223 | | |
| 4 | c (11)| [kore](https://kore.io) (3.3) | 159 399 | | |
| 5 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 144 446 | | |
| 6 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 140 942 | | |
| 7 | go (1.13)| [router](https://github.com/fasthttp/router) (1.6) | 140 529 | | |
| 8 | javascript (12.13)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 137 424 | | |
| 9 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.7) | 136 656 | | |
| 10 | crystal (0.31)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 133 660 | | |
| 11 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (9.0) | 132 875 | | |
| 12 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.2) | 131 785 | | |
| 13 | crystal (0.31)| [toro](https://github.com/soveran/toro) (0.4) | 129 067 | | |
| 14 | crystal (0.31)| [raze](https://razecr.com) (0.3) | 128 026 | | |
| 15 | crystal (0.31)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 126 994 | | |
| 16 | crystal (0.31)| [kemal](https://kemalcr.com) (0.26) | 119 065 | | |
| 17 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 117 007 | | |
| 18 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.11) | 110 450 | | |
| 19 | crystal (0.31)| [amber](https://amberframework.org) (0.3) | 107 519 | | |
| 20 | javascript (12.13)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 106 938 | | |
| 21 | crystal (0.31)| [orion](https://github.com/obsidian/orion) (2.1) | 103 818 | | |
| 22 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 94 423 | | |
| 23 | php (7.4)| [workerman](https://github.com/workerman/workerman) (3.5) | 91 373 | | |
| 24 | csharp (7.3)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.0) | 87 841 | | |
| 25 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.2) | 87 461 | | |
| 26 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 84 938 | | |
| 27 | go (1.13)| [violetear](https://violetear.org) (7.0) | 84 756 | | |
| 28 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 84 677 | | |
| 29 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 84 295 | | |
| 30 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 83 432 | | |
| 31 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 81 856 | | |
| 32 | go (1.13)| [beego](https://beego.me) (1.12) | 81 767 | | |
| 33 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 81 423 | | |
| 34 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 78 414 | | |
| 35 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 76 699 | | |
| 36 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.0) | 74 221 | | |
| 37 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 68 999 | | |
| 38 | go (1.13)| [gf](https://goframe.org) (1.1) | 66 661 | | |
| 39 | javascript (12.13)| [restana](https://github.com/jkyberneees/ana) (4.0) | 66 291 | | |
| 40 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 62 783 | | |
| 41 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 56 377 | | |
| 42 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 54 012 | | |
| 43 | javascript (12.13)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 53 833 | | |
| 44 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 52 351 | | |
| 45 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 50 961 | | |
| 46 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 50 759 | | |
| 47 | rust (1.4)| [gotham](https://gotham.rs) (0.4) | 49 688 | | |
| 48 | clojure (1.10)| [coast](https://coastonclojure.com) (1.0) | 49 682 | | |
| 49 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 47 741 | | |
| 50 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 47 689 | | |
| 51 | javascript (12.13)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 47 212 | | |
| 52 | java (8)| [javalin](https://javalin.io) (3.5) | 46 660 | | |
| 53 | java (8)| [act](https://actframework.org) (1.8) | 44 981 | | |
| 54 | javascript (12.13)| [fastify](https://fastify.io) (2.11) | 44 978 | | |
| 55 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 44 379 | | |
| 56 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 42 658 | | |
| 57 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 41 912 | | |
| 58 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 41 253 | | |
| 59 | javascript (12.13)| [rayo](https://rayo.js.org) (1.3) | 40 820 | | |
| 60 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 40 116 | | |
| 61 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 39 782 | | |
| 62 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 38 499 | | |
| 63 | python (3.8)| [hug](https://hug.rest) (2.6) | 38 231 | | |
| 64 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 37 897 | | |
| 65 | php (7.4)| [slim](https://slimframework.com) (4.4) | 37 481 | | |
| 66 | python (3.8)| [starlette](https://starlette.io) (0.13) | 37 341 | | |
| 67 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 35 685 | | |
| 68 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 35 503 | | |
| 69 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 35 214 | | |
| 70 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 34 750 | | |
| 71 | php (7.4)| [imi](https://imiphp.com) (1.0) | 34 586 | | |
| 72 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 34 495 | | |
| 73 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 34 298 | | |
| 74 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.27) | 34 070 | | |
| 75 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 33 891 | | |
| 76 | php (7.4)| [symfony](https://symfony.com) (4.3) | 32 688 | | |
| 77 | rust (1.4)| [nickel](https://nickel-org.github.io) (0.11) | 32 092 | | |
| 78 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 31 884 | | |
| 79 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 31 455 | | |
| 80 | php (7.4)| [laravel](https://laravel.com) (6.9) | 31 130 | | |
| 81 | php (7.4)| [swoft](https://swoft.org) (2.0) | 30 205 | | |
| 82 | javascript (12.13)| [express](https://expressjs.com) (4.17) | 29 741 | | |
| 83 | javascript (12.13)| [koa](https://koajs.com) (2.11) | 26 863 | | |
| 84 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 26 834 | | |
| 85 | javascript (12.13)| [0http](https://github.com/jkyberneees/0http) (2.2) | 26 643 | | |
| 86 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 26 545 | | |
| 87 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.45) | 24 720 | | |
| 88 | javascript (12.13)| [polka](https://github.com/lukeed/polka) (0.5) | 24 225 | | |
| 89 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 23 774 | | |
| 90 | python (3.8)| [responder](https://python-responder.org) (2.0) | 22 968 | | |
| 91 | crystal (0.31)| [lucky](https://luckyframework.org) (0.18) | 22 457 | | |
| 92 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 21 882 | | |
| 93 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 21 532 | | |
| 94 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 21 473 | | |
| 95 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 21 446 | | |
| 96 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 20 443 | | |
| 97 | crystal (0.31)| [athena](https://github.com/blacksmoke16/athena) (0.7) | 19 763 | | |
| 98 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 19 121 | | |
| 99 | python (3.8)| [bocadillo](https://bocadilloproject.github.io) (0.18) | 17 601 | | |
| 100 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 17 205 | | |
| 101 | rust (1.4)| [iron](https://ironframework.io) (0.6) | 17 064 | | |
| 102 | javascript (12.13)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 16 985 | | |
| 103 | javascript (12.13)| [restify](https://restify.com) (8.5) | 16 738 | | |
| 104 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 16 396 | | |
| 105 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 15 781 | | |
| 106 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 15 334 | | |
| 107 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.9) | 15 092 | | |
| 108 | java (8)| [micronaut](https://micronaut.io) (1.2) | 14 755 | | |
| 109 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 14 116 | | |
| 110 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 14 107 | | |
| 111 | javascript (12.13)| [hapi](https://hapijs.com) (18.4) | 12 889 | | |
| 112 | javascript (12.13)| [moleculer](https://moleculer.services) (0.13) | 12 728 | | |
| 113 | ruby (2.6)| [grape](https://ruby-grape.org) (1.2) | 11 839 | | |
| 114 | javascript (12.13)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 10 462 | | |
| 115 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.10) | 10 015 | | |
| 116 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 038 | | |
| 117 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 8 625 | | |
| 118 | fsharp (7.3)| [suave](https://suave.io) (2.5) | 8 551 | | |
| 119 | python (3.8)| [django](https://djangoproject.com) (3.0) | 8 053 | | |
| 120 | crystal (0.31)| [onyx](https://onyxframework.org) (0.5) | 7 568 | | |
| 121 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 7 372 | | |
| 122 | python (3.8)| [masonite](https://masoniteproject.com) (2.2) | 5 869 | | |
| 123 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 3 368 | | |
| 124 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 3 305 | | |
| 125 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 274 | | |
| 126 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 211 | | |
| 127 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 452 | | |
| 128 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 241 | | |
| 129 | javascript (12.13)| [foxify](https://foxify.js.org) (0.1) | 1 019 | | |
| 130 | perl (5.30)| [dancer2](https://perldancer.org) (2.0) | 896 | | |

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

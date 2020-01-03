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
| 1 | javascript (12.13)| [nanoexpress-pro](https://nanoexpress.js.org) (1.8) | 209 354 | | |
| 2 | javascript (12.13)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 205 258 | | |
| 3 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 196 075 | | |
| 4 | javascript (12.13)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 195 053 | | |
| 5 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 181 671 | | |
| 6 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.7) | 176 365 | | |
| 7 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 175 429 | | |
| 8 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (9.0) | 167 213 | | |
| 9 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.2) | 164 990 | | |
| 10 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 164 696 | | |
| 11 | go (1.13)| [router](https://github.com/fasthttp/router) (1.6) | 161 554 | | |
| 12 | c (11)| [kore](https://kore.io) (3.3) | 160 793 | | |
| 13 | crystal (0.31)| [toro](https://github.com/soveran/toro) (0.4) | 153 805 | | |
| 14 | crystal (0.31)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 146 328 | | |
| 15 | crystal (0.31)| [kemal](https://kemalcr.com) (0.28) | 145 768 | | |
| 16 | crystal (0.31)| [spider-gazelle](https://spider-gazelle.net) (2.1) | 144 083 | | |
| 17 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 137 072 | | |
| 18 | crystal (0.31)| [raze](https://razecr.com) (0.3) | 136 792 | | |
| 19 | crystal (0.31)| [amber](https://amberframework.org) (0.3) | 128 318 | | |
| 20 | java (8)| [act](https://actframework.org) (1.8) | 128 119 | | |
| 21 | crystal (0.31)| [orion](https://github.com/obsidian/orion) (1.7) | 124 612 | | |
| 22 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.11) | 122 386 | | |
| 23 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 114 627 | | |
| 24 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.2) | 111 889 | | |
| 25 | php (7.4)| [workerman](https://github.com/workerman/workerman) (3.5) | 109 160 | | |
| 26 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 107 544 | | |
| 27 | csharp (7.3)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.0) | 106 968 | | |
| 28 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 104 858 | | |
| 29 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 104 372 | | |
| 30 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 103 407 | | |
| 31 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 103 083 | | |
| 32 | go (1.13)| [violetear](https://violetear.org) (7.0) | 102 665 | | |
| 33 | go (1.13)| [beego](https://beego.me) (1.12) | 102 345 | | |
| 34 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 100 568 | | |
| 35 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 99 587 | | |
| 36 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 96 692 | | |
| 37 | javascript (12.13)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 94 176 | | |
| 38 | javascript (12.13)| [0http](https://github.com/jkyberneees/0http) (1.2) | 93 317 | | |
| 39 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 90 160 | | |
| 40 | javascript (12.13)| [restana](https://github.com/jkyberneees/ana) (3.4) | 89 886 | | |
| 41 | go (1.13)| [gf](https://goframe.org) (1.1) | 82 992 | | |
| 42 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.0) | 82 165 | | |
| 43 | javascript (12.13)| [polka](https://github.com/lukeed/polka) (0.5) | 80 522 | | |
| 44 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 80 487 | | |
| 45 | javascript (12.13)| [rayo](https://rayo.js.org) (1.3) | 79 120 | | |
| 46 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 75 015 | | |
| 47 | javascript (12.13)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 71 440 | | |
| 48 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 69 620 | | |
| 49 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 66 064 | | |
| 50 | javascript (12.13)| [foxify](https://foxify.js.org) (0.1) | 65 966 | | |
| 51 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 60 995 | | |
| 52 | java (8)| [javalin](https://javalin.io) (3.5) | 60 962 | | |
| 53 | javascript (12.13)| [koa](https://koajs.com) (2.11) | 59 465 | | |
| 54 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 59 104 | | |
| 55 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 58 432 | | |
| 56 | javascript (12.13)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 57 572 | | |
| 57 | javascript (12.13)| [fastify](https://fastify.io) (2.11) | 55 487 | | |
| 58 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 55 101 | | |
| 59 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 54 566 | | |
| 60 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 52 996 | | |
| 61 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 52 566 | | |
| 62 | javascript (12.13)| [express](https://expressjs.com) (4.17) | 52 175 | | |
| 63 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 51 438 | | |
| 64 | rust (1.39)| [gotham](https://gotham.rs) (0.4) | 51 059 | | |
| 65 | clojure (1.10)| [coast](https://coastonclojure.com) (1.0) | 50 968 | | |
| 66 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 50 895 | | |
| 67 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 50 129 | | |
| 68 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 49 695 | | |
| 69 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 49 364 | | |
| 70 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 49 041 | | |
| 71 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 48 720 | | |
| 72 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 48 503 | | |
| 73 | python (3.8)| [hug](https://hug.rest) (2.6) | 47 704 | | |
| 74 | python (3.8)| [starlette](https://starlette.io) (0.13) | 46 564 | | |
| 75 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 45 551 | | |
| 76 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 45 545 | | |
| 77 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 44 863 | | |
| 78 | php (7.4)| [imi](https://imiphp.com) (1.0) | 44 473 | | |
| 79 | php (7.4)| [slim](https://slimframework.com) (4.3) | 44 200 | | |
| 80 | php (7.4)| [lumen](https://lumen.laravel.com) (6.2) | 43 916 | | |
| 81 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 43 856 | | |
| 82 | php (7.4)| [symfony](https://symfony.com) (4.3) | 43 504 | | |
| 83 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 42 554 | | |
| 84 | javascript (12.13)| [restify](https://restify.com) (8.5) | 42 411 | | |
| 85 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 42 141 | | |
| 86 | cpp (11)| [evhtp](https://criticalstack/libevhtp) (1.2) | 38 761 | | |
| 87 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 38 533 | | |
| 88 | java (8)| [micronaut](https://micronaut.io) (1.2) | 37 959 | | |
| 89 | php (7.4)| [laravel](https://laravel.com) (6.9) | 37 678 | | |
| 90 | php (7.4)| [swoft](https://swoft.org) (2.0) | 37 002 | | |
| 91 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.27) | 36 071 | | |
| 92 | javascript (12.13)| [hapi](https://hapijs.com) (18.4) | 35 065 | | |
| 93 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 34 622 | | |
| 94 | fsharp (7.3)| [suave](https://suave.io) (2.5) | 31 026 | | |
| 95 | javascript (12.13)| [moleculer](https://moleculer.services) (0.13) | 30 184 | | |
| 96 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.45) | 29 913 | | |
| 97 | rust (1.39)| [nickel](https://nickel-org.github.io) (0.11) | 29 551 | | |
| 98 | python (3.8)| [responder](https://python-responder.org) (2.0) | 29 233 | | |
| 99 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 28 669 | | |
| 100 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 27 242 | | |
| 101 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 26 366 | | |
| 102 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 26 345 | | |
| 103 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 25 515 | | |
| 104 | crystal (0.31)| [athena](https://github.com/blacksmoke16/athena) (0.7) | 24 582 | | |
| 105 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 24 543 | | |
| 106 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 23 513 | | |
| 107 | javascript (12.13)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 683 | | |
| 108 | crystal (0.31)| [lucky](https://luckyframework.org) (0.18) | 22 600 | | |
| 109 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 21 110 | | |
| 110 | python (3.8)| [bocadillo](https://bocadilloproject.github.io) (0.18) | 20 783 | | |
| 111 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.9) | 20 386 | | |
| 112 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 16 838 | | |
| 113 | rust (1.39)| [iron](https://ironframework.io) (0.6) | 16 277 | | |
| 114 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 16 243 | | |
| 115 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 14 425 | | |
| 116 | ruby (2.6)| [grape](https://ruby-grape.org) (1.2) | 13 513 | | |
| 117 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 12 581 | | |
| 118 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.10) | 11 124 | | |
| 119 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 10 250 | | |
| 120 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 201 | | |
| 121 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 179 | | |
| 122 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 9 055 | | |
| 123 | python (3.8)| [masonite](https://masoniteproject.com) (2.2) | 6 473 | | |
| 124 | crystal (0.31)| [onyx](https://onyxframework.org) (0.5) | 4 876 | | |
| 125 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 4 103 | | |
| 126 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 963 | | |
| 127 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 314 | | |
| 128 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 495 | | |
| 129 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 450 | | |
| 130 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 1 027 | | |

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

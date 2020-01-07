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
| 1 | javascript (12.13)| [nanoexpress-pro](https://nanoexpress.js.org) (1.9) | 199 314 | | |
| 2 | javascript (12.13)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 196 034 | | |
| 3 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 191 208 | | |
| 4 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 187 084 | | |
| 5 | javascript (12.13)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 185 315 | | |
| 6 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.7) | 167 906 | | |
| 7 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 165 319 | | |
| 8 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 162 318 | | |
| 9 | go (1.13)| [router](https://github.com/fasthttp/router) (1.6) | 161 414 | | |
| 10 | c (11)| [kore](https://kore.io) (3.3) | 160 870 | | |
| 11 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (9.0) | 158 847 | | |
| 12 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.2) | 157 829 | | |
| 13 | crystal (0.31)| [toro](https://github.com/soveran/toro) (0.4) | 149 104 | | |
| 14 | crystal (0.31)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 147 435 | | |
| 15 | crystal (0.31)| [raze](https://razecr.com) (0.3) | 143 987 | | |
| 16 | crystal (0.31)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 143 905 | | |
| 17 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 143 895 | | |
| 18 | crystal (0.31)| [kemal](https://kemalcr.com) (0.26) | 136 534 | | |
| 19 | crystal (0.31)| [amber](https://amberframework.org) (0.3) | 124 760 | | |
| 20 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.11) | 123 980 | | |
| 21 | java (8)| [act](https://actframework.org) (1.8) | 120 287 | | |
| 22 | crystal (0.31)| [orion](https://github.com/obsidian/orion) (2.1) | 119 105 | | |
| 23 | php (7.4)| [workerman](https://github.com/workerman/workerman) (3.5) | 112 376 | | |
| 24 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 108 566 | | |
| 25 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.2) | 105 289 | | |
| 26 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 102 125 | | |
| 27 | csharp (7.3)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.0) | 101 734 | | |
| 28 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 99 893 | | |
| 29 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 98 043 | | |
| 30 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 97 865 | | |
| 31 | go (1.13)| [violetear](https://violetear.org) (7.0) | 97 359 | | |
| 32 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 97 090 | | |
| 33 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 96 504 | | |
| 34 | go (1.13)| [beego](https://beego.me) (1.12) | 95 097 | | |
| 35 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 94 481 | | |
| 36 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 91 399 | | |
| 37 | javascript (12.13)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 88 225 | | |
| 38 | javascript (12.13)| [0http](https://github.com/jkyberneees/0http) (2.2) | 84 654 | | |
| 39 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 84 199 | | |
| 40 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.0) | 81 887 | | |
| 41 | go (1.13)| [gf](https://goframe.org) (1.1) | 78 601 | | |
| 42 | javascript (12.13)| [restana](https://github.com/jkyberneees/ana) (4.0) | 78 600 | | |
| 43 | javascript (12.13)| [polka](https://github.com/lukeed/polka) (0.5) | 76 581 | | |
| 44 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 75 394 | | |
| 45 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 74 847 | | |
| 46 | javascript (12.13)| [rayo](https://rayo.js.org) (1.3) | 74 047 | | |
| 47 | javascript (12.13)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 66 164 | | |
| 48 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 65 479 | | |
| 49 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 63 531 | | |
| 50 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 62 639 | | |
| 51 | javascript (12.13)| [foxify](https://foxify.js.org) (0.1) | 62 006 | | |
| 52 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 60 924 | | |
| 53 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 60 465 | | |
| 54 | java (8)| [javalin](https://javalin.io) (3.5) | 59 195 | | |
| 55 | javascript (12.13)| [koa](https://koajs.com) (2.11) | 55 524 | | |
| 56 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 54 277 | | |
| 57 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 54 226 | | |
| 58 | javascript (12.13)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 53 987 | | |
| 59 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 52 666 | | |
| 60 | clojure (1.10)| [coast](https://coastonclojure.com) (1.0) | 52 664 | | |
| 61 | javascript (12.13)| [fastify](https://fastify.io) (2.11) | 52 161 | | |
| 62 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 51 546 | | |
| 63 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 51 512 | | |
| 64 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 51 511 | | |
| 65 | rust (1.39)| [gotham](https://gotham.rs) (0.4) | 50 987 | | |
| 66 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 49 126 | | |
| 67 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 49 084 | | |
| 68 | javascript (12.13)| [express](https://expressjs.com) (4.17) | 49 040 | | |
| 69 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 48 650 | | |
| 70 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 48 252 | | |
| 71 | python (3.8)| [hug](https://hug.rest) (2.6) | 47 882 | | |
| 72 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 47 352 | | |
| 73 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 46 514 | | |
| 74 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 46 305 | | |
| 75 | python (3.8)| [starlette](https://starlette.io) (0.13) | 45 532 | | |
| 76 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 44 627 | | |
| 77 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 44 229 | | |
| 78 | php (7.4)| [slim](https://slimframework.com) (4.4) | 43 416 | | |
| 79 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 42 974 | | |
| 80 | php (7.4)| [imi](https://imiphp.com) (1.0) | 42 657 | | |
| 81 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 42 137 | | |
| 82 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 42 106 | | |
| 83 | php (7.4)| [symfony](https://symfony.com) (4.3) | 41 984 | | |
| 84 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 40 791 | | |
| 85 | javascript (12.13)| [restify](https://restify.com) (8.5) | 40 222 | | |
| 86 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 38 249 | | |
| 87 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 38 110 | | |
| 88 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.27) | 36 983 | | |
| 89 | php (7.4)| [laravel](https://laravel.com) (6.9) | 35 975 | | |
| 90 | php (7.4)| [swoft](https://swoft.org) (2.0) | 35 943 | | |
| 91 | java (8)| [micronaut](https://micronaut.io) (1.2) | 33 418 | | |
| 92 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 33 253 | | |
| 93 | javascript (12.13)| [hapi](https://hapijs.com) (18.4) | 32 443 | | |
| 94 | fsharp (7.3)| [suave](https://suave.io) (2.5) | 29 976 | | |
| 95 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.45) | 29 662 | | |
| 96 | rust (1.39)| [nickel](https://nickel-org.github.io) (0.11) | 29 596 | | |
| 97 | python (3.8)| [responder](https://python-responder.org) (2.0) | 29 263 | | |
| 98 | javascript (12.13)| [moleculer](https://moleculer.services) (0.13) | 28 149 | | |
| 99 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 27 813 | | |
| 100 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 26 485 | | |
| 101 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 26 062 | | |
| 102 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 25 980 | | |
| 103 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 25 763 | | |
| 104 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 24 881 | | |
| 105 | crystal (0.31)| [athena](https://github.com/blacksmoke16/athena) (0.7) | 24 002 | | |
| 106 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 22 673 | | |
| 107 | crystal (0.31)| [lucky](https://luckyframework.org) (0.18) | 22 492 | | |
| 108 | javascript (12.13)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 174 | | |
| 109 | python (3.8)| [bocadillo](https://bocadilloproject.github.io) (0.18) | 20 624 | | |
| 110 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 20 299 | | |
| 111 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.9) | 20 141 | | |
| 112 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 17 738 | | |
| 113 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 376 | | |
| 114 | rust (1.39)| [iron](https://ironframework.io) (0.6) | 15 962 | | |
| 115 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 13 746 | | |
| 116 | ruby (2.6)| [grape](https://ruby-grape.org) (1.2) | 12 816 | | |
| 117 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 941 | | |
| 118 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.10) | 11 385 | | |
| 119 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 439 | | |
| 120 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 10 077 | | |
| 121 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 919 | | |
| 122 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 9 256 | | |
| 123 | python (3.8)| [masonite](https://masoniteproject.com) (2.2) | 6 868 | | |
| 124 | crystal (0.31)| [onyx](https://onyxframework.org) (0.5) | 4 886 | | |
| 125 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 3 581 | | |
| 126 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 297 | | |
| 127 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 312 | | |
| 128 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 513 | | |
| 129 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 421 | | |
| 130 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 1 165 | | |

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

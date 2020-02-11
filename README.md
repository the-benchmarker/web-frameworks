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

:information_source:  Updated on **2020-02-11** :information_source:

> Benchmarking with [wrk](https://github.com/ioquatix/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 5s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Concurrency 64 (`req/s`) | Concurrency 512 (`req/s`) | Concurrency 4096 (`req/s`) |
|----|----------|-----------|----------------:|------------:|------------:|
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 164 950 | 185 952 | 70 525 |
| 2 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 163 730 | 184 575 | 69 351 |
| 3 | javascript (13.7)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 157 538 | 178 642 | 68 060 |
| 4 | go (1.13)| [fiber](https://fiber.wiki) (1.4) | 149 629 | 167 057 | 62 668 |
| 5 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.9) | 148 455 | 167 423 | 62 824 |
| 6 | go (1.13)| [router](https://github.com/fasthttp/router) (0.6) | 143 230 | 161 533 | 60 837 |
| 7 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 143 113 | 160 507 | 60 537 |
| 8 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.3) | 143 041 | 161 319 | 60 806 |
| 9 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 139 795 | 158 629 | 59 846 |
| 10 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 136 839 | 164 725 | 62 156 |
| 11 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 128 509 | 145 926 | 55 403 |
| 12 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (3.5) | 117 732 | 134 670 | 51 475 |
| 13 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 102 963 | 108 244 | 41 389 |
| 14 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 98 207 | 105 301 | 40 146 |
| 15 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 97 546 | 175 175 | 67 014 |
| 16 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 97 336 | 100 784 | 38 838 |
| 17 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 94 732 | 96 615 | 37 325 |
| 18 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 94 705 | 97 400 | 37 554 |
| 19 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 94 131 | 100 369 | 38 381 |
| 20 | crystal (0.32)| [toro](https://github.com/soveran/toro) (0.4) | 93 348 | 98 684 | 36 065 |
| 21 | go (1.13)| [violetear](https://violetear.org) (7.0) | 93 111 | 98 030 | 37 533 |
| 22 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 92 070 | 93 881 | 36 071 |
| 23 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.12) | 91 915 | 122 734 | 46 115 |
| 24 | crystal (0.32)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 91 325 | 95 960 | 35 244 |
| 25 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 90 985 | 96 613 | 37 016 |
| 26 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 90 822 | 98 699 | 37 218 |
| 27 | c (99)| [kore](https://kore.io) (3.3) | 90 446 | 125 635 | 44 263 |
| 28 | go (1.13)| [beego](https://beego.me) (1.12) | 90 401 | 98 377 | 37 718 |
| 29 | java (8)| [act](https://actframework.org) (1.8) | 88 311 | 125 357 | 48 019 |
| 30 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 88 260 | 91 763 | 35 307 |
| 31 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 87 840 | 101 291 | 37 926 |
| 32 | crystal (0.32)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 86 125 | 91 770 | 33 744 |
| 33 | crystal (0.32)| [raze](https://razecr.com) (0.3) | 84 695 | 88 715 | 32 545 |
| 34 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 81 368 | 84 587 | 32 841 |
| 35 | crystal (0.32)| [kemal](https://kemalcr.com) (0.26) | 78 640 | 82 171 | 30 270 |
| 36 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 77 756 | 97 971 | 35 885 |
| 37 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 77 301 | 87 715 | 34 309 |
| 38 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 74 826 | 93 530 | 34 556 |
| 39 | crystal (0.32)| [grip](https://github.com/grkek/grip) (0.27) | 74 666 | 78 331 | 29 280 |
| 40 | crystal (0.32)| [amber](https://amberframework.org) (0.3) | 71 423 | 74 516 | 27 869 |
| 41 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 70 286 | 82 665 | 31 125 |
| 42 | go (1.13)| [gf](https://goframe.org) (1.11) | 70 038 | 78 331 | 30 190 |
| 43 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 68 970 | 75 225 | 28 825 |
| 44 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 68 367 | 81 602 | 31 711 |
| 45 | crystal (0.32)| [orion](https://github.com/obsidian/orion) (2.1) | 67 192 | 67 854 | 24 936 |
| 46 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 67 075 | 79 714 | 29 811 |
| 47 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 62 886 | 64 399 | 24 875 |
| 48 | elixir (1.1)| [cowboy_stream](https://https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 59 590 | 62 045 | 23 583 |
| 49 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 57 908 | 70 683 | 26 898 |
| 50 | crystal (0.32)| [athena](https://github.com/athena-framework/athena) (0.8) | 56 858 | 57 624 | 21 466 |
| 51 | javascript (13.7)| [fastify](https://fastify.io) (2.12) | 56 740 | 67 174 | 25 304 |
| 52 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 56 038 | 60 905 | 23 507 |
| 53 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.12) | 55 689 | 61 517 | 23 020 |
| 54 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 55 225 | 60 482 | 23 021 |
| 55 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 54 998 | 66 127 | 25 027 |
| 56 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.0) | 54 557 | 82 604 | 30 983 |
| 57 | java (8)| [javalin](https://javalin.io) (3.5) | 52 816 | 75 093 | 28 596 |
| 58 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 51 068 | 55 641 | 20 635 |
| 59 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 49 244 | 59 415 | 22 409 |
| 60 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 48 785 | 50 700 | 19 553 |
| 61 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 48 672 | 57 681 | 21 977 |
| 62 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 48 257 | 49 313 | 19 036 |
| 63 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 47 806 | 52 593 | 19 568 |
| 64 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 46 927 | 66 160 | 27 877 |
| 65 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 46 789 | 48 988 | 18 689 |
| 66 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 45 962 | 76 579 | 29 248 |
| 67 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 45 327 | 48 920 | 18 742 |
| 68 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 45 047 | 53 095 | 20 623 |
| 69 | python (3.8)| [hug](https://hug.rest) (2.6) | 44 117 | 47 152 | 18 218 |
| 70 | python (3.8)| [starlette](https://starlette.io) (0.13) | 43 438 | 47 316 | 17 590 |
| 71 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 42 643 | 48 310 | 18 493 |
| 72 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 40 245 | 44 102 | 16 747 |
| 73 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 40 167 | 48 998 | 20 032 |
| 74 | elixir (1.1)| [cowboy](https://https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 39 468 | 42 006 | 15 908 |
| 75 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 39 058 | 40 806 | 15 687 |
| 76 | php (7.4)| [imi](https://imiphp.com) (1.0) | 38 305 | 41 944 | 16 094 |
| 77 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 37 177 | 52 747 | 20 290 |
| 78 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 36 871 | 38 173 | 14 885 |
| 79 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 35 323 | 72 607 | 28 143 |
| 80 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.28) | 35 001 | 37 040 | 14 445 |
| 81 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 34 816 | 36 502 | 13 879 |
| 82 | php (7.4)| [swoft](https://swoft.org) (2.0) | 33 789 | 34 838 | 13 439 |
| 83 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 32 627 | 41 788 | 16 385 |
| 84 | javascript (13.7)| [restify](https://restify.com) (8.5) | 32 094 | 39 497 | 15 309 |
| 85 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 31 273 | 32 073 | 12 587 |
| 86 | java (8)| [micronaut](https://micronaut.io) (1.2) | 31 227 | 67 183 | 25 768 |
| 87 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 30 302 | 31 646 | 12 059 |
| 88 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 29 526 | 57 188 | 22 789 |
| 89 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.48) | 28 601 | 30 240 | 11 217 |
| 90 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 28 105 | 15 465 | 11 595 |
| 91 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 28 048 | 32 097 | 12 163 |
| 92 | python (3.8)| [responder](https://python-responder.org) (2.0) | 27 906 | 29 930 | 11 108 |
| 93 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 27 424 | 27 790 | 11 025 |
| 94 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 25 619 | 26 142 | 10 134 |
| 95 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 24 603 | 26 016 | 10 066 |
| 96 | javascript (13.7)| [moleculer](https://moleculer.services) (0.13) | 24 491 | 31 497 | 12 086 |
| 97 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 24 072 | 27 178 | 10 143 |
| 98 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 23 991 | 23 904 | 9 473 |
| 99 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 22 992 | 23 909 | 9 165 |
| 100 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 22 497 | 23 758 | 9 169 |
| 101 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 244 | 19 979 | 7 926 |
| 102 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 20 687 | 20 404 | 8 013 |
| 103 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 19 925 | 20 518 | 7 626 |
| 104 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a1) | 19 277 | 19 271 | 6 924 |
| 105 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 19 058 | 20 519 | 6 962 |
| 106 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 17 866 | 55 618 | 23 475 |
| 107 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 17 512 | 11 946 | 6 764 |
| 108 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 16 781 | 17 254 | 6 781 |
| 109 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 15 707 | 17 094 | 6 697 |
| 110 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 14 625 | 15 320 | 6 093 |
| 111 | crystal (0.32)| [lucky](https://luckyframework.org) (0.18) | 13 282 | 11 971 | 5 142 |
| 112 | ruby (2.6)| [grape](https://ruby-grape.org) (1.3) | 11 729 | 11 719 | 4 602 |
| 113 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 11 604 | 11 137 | 4 085 |
| 114 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 962 | 10 113 | 3 923 |
| 115 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 681 | 9 750 | 3 873 |
| 116 | python (3.8)| [django](https://djangoproject.com) (3.0) | 8 710 | 8 760 | 3 467 |
| 117 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 8 533 | 8 883 | 3 506 |
| 118 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 252 | 11 303 | 18 184 |
| 119 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 7 875 | 7 712 | 3 184 |
| 120 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 7 253 | 9 613 | 14 678 |
| 121 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 7 150 | 2 278 | 1 343 |
| 122 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 6 911 | 9 483 | 18 629 |
| 123 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 6 893 | 9 370 | 13 658 |
| 124 | crystal (0.32)| [onyx](https://onyxframework.org) (0.5) | 5 729 | 6 121 | 2 456 |
| 125 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 088 | 7 059 | 14 501 |
| 126 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 3 940 | 7 077 | 14 651 |
| 127 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 3 887 | 6 934 | 14 031 |
| 128 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 3 569 | 3 459 | 1 458 |
| 129 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 179 | 6 497 | 13 689 |
| 130 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 128 | 6 589 | 14 573 |
| 131 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 035 | 4 855 | 1 930 |
| 132 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 313 | 2 350 | 960 |
| 133 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 1 790 | 4 364 | 12 409 |
| 134 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 1 761 | 5 371 | 13 424 |
| 135 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 497 | 1 558 | 624 |
| 136 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 474 | 1 464 | 602 |
| 137 | php (7.4)| [laravel](https://laravel.com) (6.14) | 486 | 3 149 | 10 176 |

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

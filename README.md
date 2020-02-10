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

:information_source:  Updated on **2020-02-10** :information_source:

> Benchmarking with [wrk](https://github.com/ioquatix/wrk)
   + Threads : 8
   + Timeout : 8
   + Connections : 64
   + Duration : 5s (seconds)

|    | Language | Framework | Concurrency 64 (`req/s`) | Concurrency 512 (`req/s`) | Concurrency 4096 (`req/s`) |
|----|----------|-----------|----------------:|------------:|------------:|
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 174 489 | 198 705 | 75 527 |
| 2 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 164 838 | 184 275 | 70 873 |
| 3 | javascript (13.7)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 158 721 | 179 317 | 67 702 |
| 4 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 102 015 | 174 045 | 71 325 |
| 5 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.9) | 150 896 | 169 511 | 63 905 |
| 6 | go (1.13)| [fiber](https://gofiber.github.io/fiber) (1.4) | 149 345 | 167 593 | 63 263 |
| 7 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 139 381 | 165 600 | 62 744 |
| 8 | go (1.13)| [router](https://github.com/fasthttp/router) (0.6) | 143 079 | 159 520 | 57 936 |
| 9 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 140 526 | 158 245 | 59 721 |
| 10 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 137 315 | 156 210 | 59 246 |
| 11 | c (11)| [kore](https://kore.io) (3.3) | 82 582 | 152 614 | 47 440 |
| 12 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 144 778 | 152 102 | 60 565 |
| 13 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.3) | 136 902 | 151 999 | 56 391 |
| 14 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (3.5) | 118 250 | 134 812 | 51 846 |
| 15 | java (8)| [act](https://actframework.org) (1.8) | 87 372 | 126 642 | 47 439 |
| 16 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.12) | 92 885 | 120 695 | 46 491 |
| 17 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 104 060 | 108 131 | 41 447 |
| 18 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 100 293 | 107 948 | 41 336 |
| 19 | crystal (0.32)| [toro](https://github.com/soveran/toro) (0.4) | 97 731 | 104 663 | 38 558 |
| 20 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 95 556 | 102 044 | 38 981 |
| 21 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 92 959 | 101 910 | 38 941 |
| 22 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 97 693 | 101 528 | 38 993 |
| 23 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 96 112 | 99 348 | 37 878 |
| 24 | go (1.13)| [violetear](https://violetear.org) (7.0) | 93 412 | 98 683 | 37 905 |
| 25 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 78 907 | 98 094 | 36 279 |
| 26 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 90 828 | 97 416 | 37 521 |
| 27 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 84 848 | 97 106 | 36 455 |
| 28 | crystal (0.32)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 90 873 | 96 837 | 35 689 |
| 29 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 75 647 | 93 822 | 34 538 |
| 30 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 91 207 | 93 687 | 35 477 |
| 31 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 88 842 | 92 086 | 35 243 |
| 32 | crystal (0.32)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 85 325 | 90 070 | 33 266 |
| 33 | crystal (0.32)| [raze](https://razecr.com) (0.3) | 80 963 | 89 350 | 33 048 |
| 34 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 81 088 | 88 755 | 33 676 |
| 35 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 86 448 | 88 682 | 34 263 |
| 36 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.0) | 70 448 | 88 337 | 32 581 |
| 37 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.0) | 76 733 | 87 667 | 33 549 |
| 38 | go (1.13)| [beego](https://beego.me) (1.12) | 87 413 | 84 685 | 37 352 |
| 39 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 71 188 | 84 176 | 31 194 |
| 40 | crystal (0.32)| [kemal](https://kemalcr.com) (0.26) | 79 003 | 83 208 | 30 621 |
| 41 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 71 152 | 82 404 | 32 084 |
| 42 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 68 760 | 81 403 | 30 413 |
| 43 | go (1.13)| [gf](https://goframe.org) (1.11) | 69 598 | 78 782 | 29 544 |
| 44 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 44 861 | 77 666 | 29 539 |
| 45 | java (8)| [javalin](https://javalin.io) (3.5) | 50 318 | 76 506 | 28 915 |
| 46 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 67 684 | 75 376 | 28 774 |
| 47 | crystal (0.32)| [amber](https://amberframework.org) (0.3) | 72 244 | 74 546 | 27 560 |
| 48 | crystal (0.32)| [orion](https://github.com/obsidian/orion) (2.1) | 71 445 | 74 180 | 27 289 |
| 49 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 58 799 | 71 910 | 26 891 |
| 50 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 32 200 | 71 128 | 28 453 |
| 51 | crystal (0.32)| [grip](https://github.com/grkek/grip) (0.27) | 65 500 | 70 002 | 25 739 |
| 52 | java (8)| [micronaut](https://micronaut.io) (1.2) | 37 448 | 67 815 | 25 670 |
| 53 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 45 164 | 67 479 | 27 553 |
| 54 | javascript (13.7)| [fastify](https://fastify.io) (2.12) | 57 164 | 67 287 | 25 057 |
| 55 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 56 152 | 66 433 | 25 039 |
| 56 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 63 139 | 64 131 | 24 837 |
| 57 | elixir (1.1)| [cowboy_stream](https://https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 60 147 | 63 128 | 24 008 |
| 58 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.12) | 55 332 | 62 195 | 23 048 |
| 59 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 52 626 | 60 859 | 23 304 |
| 60 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 54 041 | 60 780 | 23 208 |
| 61 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 49 857 | 59 604 | 22 608 |
| 62 | crystal (0.32)| [athena](https://github.com/athena-framework/athena) (0.8) | 57 652 | 57 987 | 21 348 |
| 63 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 48 838 | 57 308 | 22 025 |
| 64 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 50 049 | 56 216 | 21 034 |
| 65 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 723 | 53 734 | 20 810 |
| 66 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 17 313 | 53 390 | 23 983 |
| 67 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 43 829 | 53 376 | 20 310 |
| 68 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 49 649 | 51 687 | 19 809 |
| 69 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 49 266 | 50 865 | 19 827 |
| 70 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 40 892 | 49 591 | 20 055 |
| 71 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 46 917 | 49 182 | 18 772 |
| 72 | python (3.8)| [hug](https://hug.rest) (2.6) | 43 335 | 48 145 | 18 464 |
| 73 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 45 161 | 48 138 | 18 118 |
| 74 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 44 408 | 48 025 | 18 561 |
| 75 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 42 246 | 47 850 | 18 050 |
| 76 | python (3.8)| [starlette](https://starlette.io) (0.13) | 43 176 | 46 998 | 17 758 |
| 77 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 40 738 | 45 032 | 17 120 |
| 78 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 29 876 | 43 927 | 22 267 |
| 79 | php (7.4)| [imi](https://imiphp.com) (1.0) | 36 988 | 43 302 | 16 812 |
| 80 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 32 610 | 42 127 | 16 515 |
| 81 | elixir (1.1)| [cowboy](https://https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 40 030 | 41 581 | 15 980 |
| 82 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 38 645 | 41 229 | 15 804 |
| 83 | javascript (13.7)| [restify](https://restify.com) (8.5) | 32 537 | 39 727 | 15 521 |
| 84 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 36 770 | 37 735 | 14 863 |
| 85 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 35 379 | 37 080 | 14 031 |
| 86 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.28) | 36 177 | 37 030 | 14 463 |
| 87 | php (7.4)| [swoft](https://swoft.org) (2.0) | 34 049 | 32 628 | 13 003 |
| 88 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 32 512 | 33 627 | 13 105 |
| 89 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 29 238 | 31 847 | 12 441 |
| 90 | javascript (13.7)| [moleculer](https://moleculer.services) (0.13) | 24 480 | 30 786 | 12 081 |
| 91 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.48) | 28 002 | 30 485 | 11 499 |
| 92 | python (3.8)| [responder](https://python-responder.org) (2.0) | 28 025 | 30 370 | 11 361 |
| 93 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 29 947 | 21 955 | 11 700 |
| 94 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 28 812 | 28 935 | 12 940 |
| 95 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 27 630 | 28 550 | 11 074 |
| 96 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 24 894 | 27 090 | 10 192 |
| 97 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 25 723 | 26 841 | 10 353 |
| 98 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 24 717 | 26 267 | 10 149 |
| 99 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 24 055 | 24 409 | 9 653 |
| 100 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 22 979 | 24 174 | 9 311 |
| 101 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 22 458 | 23 550 | 9 267 |
| 102 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 137 | 20 205 | 7 917 |
| 103 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 20 233 | 20 840 | 7 733 |
| 104 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 20 011 | 20 664 | 8 195 |
| 105 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 19 502 | 20 484 | 6 847 |
| 106 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a1) | 19 502 | 19 866 | 7 097 |
| 107 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 15 779 | 17 263 | 6 708 |
| 108 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 17 144 | 11 821 | 6 669 |
| 109 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 16 193 | 16 633 | 6 534 |
| 110 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 235 | 10 769 | 16 455 |
| 111 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 6 973 | 10 033 | 16 272 |
| 112 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 334 | 6 773 | 14 996 |
| 113 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 3 933 | 6 976 | 14 860 |
| 114 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 6 895 | 9 559 | 14 573 |
| 115 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 1 882 | 4 608 | 14 436 |
| 116 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 6 886 | 9 428 | 14 337 |
| 117 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 3 989 | 6 833 | 14 084 |
| 118 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 14 057 | 13 678 | 5 918 |
| 119 | crystal (0.32)| [lucky](https://luckyframework.org) (0.18) | 13 972 | 12 476 | 4 908 |
| 120 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 184 | 6 655 | 13 912 |
| 121 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 361 | 6 459 | 13 145 |
| 122 | ruby (2.6)| [grape](https://ruby-grape.org) (1.3) | 12 037 | 12 059 | 4 746 |
| 123 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 1 652 | 4 770 | 11 899 |
| 124 | php (7.4)| [laravel](https://laravel.com) (6.14) | 453 | 3 081 | 11 590 |
| 125 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 11 586 | 10 941 | 4 153 |
| 126 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 041 | 10 104 | 4 024 |
| 127 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 724 | 9 876 | 3 862 |
| 128 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 331 | 9 785 | 3 925 |
| 129 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 8 880 | 8 997 | 3 532 |
| 130 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 7 930 | 8 097 | 2 989 |
| 131 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 7 259 | 1 167 | 1 196 |
| 132 | crystal (0.32)| [onyx](https://onyxframework.org) (0.5) | 5 260 | 5 640 | 2 266 |
| 133 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 161 | 4 926 | 1 959 |
| 134 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 3 963 | 3 888 | 1 559 |
| 135 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 315 | 2 362 | 946 |
| 136 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 507 | 1 556 | 649 |
| 137 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 472 | 1 055 | 603 |

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

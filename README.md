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

:information_source:  Updated on **2020-02-12** :information_source:

> Benchmarking with [wrk](https://github.com/ioquatix/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 5s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 164 369 | 184 768 | 184 919 | 70 977 | 71 159 |
| 2 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 161 955 | 183 125 | 183 864 | 70 006 | 68 959 |
| 3 | javascript (13.7)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 158 849 | 176 208 | 178 562 | 67 993 | 67 882 |
| 4 | go (1.13)| [fiber](https://fiber.wiki) (1.4) | 152 691 | 171 881 | 172 016 | 65 584 | 64 938 |
| 5 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.9) | 150 018 | 167 953 | 172 077 | 65 079 | 64 836 |
| 6 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 145 764 | 161 387 | 165 057 | 61 961 | 62 036 |
| 7 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.3) | 145 694 | 162 275 | 166 092 | 63 418 | 62 773 |
| 8 | go (1.13)| [router](https://github.com/fasthttp/router) (0.6) | 145 427 | 160 854 | 165 600 | 63 072 | 62 173 |
| 9 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 143 436 | 158 888 | 162 861 | 62 107 | 60 963 |
| 10 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 138 190 | 163 741 | 167 888 | 62 141 | 62 732 |
| 11 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 129 860 | 144 405 | 143 422 | 52 359 | 54 006 |
| 12 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (3.5) | 122 412 | 134 460 | 138 431 | 53 907 | 52 143 |
| 13 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 104 987 | 106 028 | 111 155 | 42 234 | 42 482 |
| 14 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 100 744 | 99 775 | 105 067 | 40 136 | 40 281 |
| 15 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 100 339 | 105 077 | 109 236 | 42 045 | 41 736 |
| 16 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 97 081 | 95 973 | 100 216 | 38 881 | 38 841 |
| 17 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.12) | 96 867 | 119 750 | 126 125 | 48 696 | 48 015 |
| 18 | c (99)| [kore](https://kore.io) (3.3) | 96 845 | 129 509 | 138 046 | 54 249 | 40 346 |
| 19 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 96 582 | 95 003 | 100 350 | 38 881 | 38 577 |
| 20 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 95 763 | 98 395 | 104 475 | 40 244 | 39 930 |
| 21 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 94 413 | 92 835 | 97 633 | 37 962 | 37 838 |
| 22 | go (1.13)| [beego](https://beego.me) (1.12) | 94 333 | 96 976 | 101 664 | 39 230 | 39 056 |
| 23 | go (1.13)| [violetear](https://violetear.org) (7.0) | 94 056 | 96 050 | 100 995 | 38 971 | 38 969 |
| 24 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 93 034 | 95 250 | 100 134 | 38 686 | 38 249 |
| 25 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 92 754 | 100 554 | 102 380 | 39 218 | 39 061 |
| 26 | crystal (0.32)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 92 424 | 98 399 | 96 784 | 35 798 | 35 871 |
| 27 | crystal (0.32)| [toro](https://github.com/soveran/toro) (0.4) | 92 253 | 100 660 | 98 245 | 36 317 | 36 097 |
| 28 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 90 981 | 88 487 | 94 109 | 36 461 | 36 611 |
| 29 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 88 080 | 100 536 | 101 268 | 38 681 | 38 738 |
| 30 | java (8)| [act](https://actframework.org) (1.8) | 88 066 | 125 885 | 120 497 | 46 944 | 47 920 |
| 31 | crystal (0.32)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 86 350 | 95 088 | 92 112 | 34 170 | 33 798 |
| 32 | crystal (0.32)| [raze](https://razecr.com) (0.3) | 85 833 | 92 560 | 90 377 | 33 742 | 33 432 |
| 33 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 82 778 | 84 416 | 87 590 | 34 276 | 33 847 |
| 34 | crystal (0.32)| [kemal](https://kemalcr.com) (0.26) | 79 793 | 84 138 | 76 553 | 30 493 | 30 256 |
| 35 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 78 716 | 98 837 | 98 564 | 36 316 | 36 185 |
| 36 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 76 295 | 87 014 | 89 560 | 34 332 | 34 337 |
| 37 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 74 975 | 94 734 | 94 055 | 34 820 | 34 881 |
| 38 | crystal (0.32)| [grip](https://github.com/grkek/grip) (0.27) | 74 737 | 79 245 | 76 697 | 28 616 | 28 387 |
| 39 | go (1.13)| [gf](https://goframe.org) (1.11) | 72 296 | 78 606 | 81 084 | 31 358 | 31 319 |
| 40 | crystal (0.32)| [amber](https://amberframework.org) (0.3) | 71 880 | 76 890 | 75 001 | 27 757 | 27 377 |
| 41 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.0) | 70 759 | 89 692 | 88 655 | 32 922 | 32 864 |
| 42 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 70 185 | 84 903 | 82 965 | 30 898 | 30 794 |
| 43 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 69 895 | 160 992 | 167 869 | 68 531 | 64 493 |
| 44 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 69 479 | 74 588 | 75 469 | 28 733 | 28 764 |
| 45 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 69 299 | 77 635 | 81 848 | 32 105 | 31 717 |
| 46 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 68 656 | 83 064 | 80 358 | 30 140 | 30 136 |
| 47 | crystal (0.32)| [orion](https://github.com/obsidian/orion) (2.1) | 68 215 | 72 117 | 68 141 | 24 908 | 24 971 |
| 48 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 64 699 | 62 595 | 65 891 | 25 627 | 25 522 |
| 49 | elixir (1.1)| [cowboy_stream](https://https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 60 672 | 63 854 | 63 717 | 24 286 | 24 165 |
| 50 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 58 411 | 73 278 | 71 566 | 27 109 | 26 847 |
| 51 | crystal (0.32)| [athena](https://github.com/athena-framework/athena) (0.8) | 57 594 | 60 234 | 57 058 | 21 423 | 21 104 |
| 52 | javascript (13.7)| [fastify](https://fastify.io) (2.12) | 57 386 | 70 552 | 68 347 | 26 127 | 25 789 |
| 53 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 56 143 | 60 887 | 61 322 | 23 566 | 23 706 |
| 54 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 56 002 | 60 234 | 60 633 | 23 091 | 23 222 |
| 55 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 55 171 | 67 747 | 66 076 | 24 873 | 25 036 |
| 56 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.12) | 54 604 | 62 578 | 62 408 | 23 424 | 23 388 |
| 57 | java (8)| [javalin](https://javalin.io) (3.5) | 53 666 | 78 944 | 74 958 | 28 584 | 29 282 |
| 58 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 52 224 | 73 107 | 65 299 | 28 674 | 28 207 |
| 59 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 51 360 | 57 737 | 56 664 | 21 500 | 21 236 |
| 60 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 49 684 | 59 852 | 60 718 | 23 130 | 21 474 |
| 61 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 48 840 | 50 506 | 50 258 | 19 491 | 19 461 |
| 62 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 48 520 | 51 578 | 50 858 | 19 626 | 19 630 |
| 63 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 48 490 | 59 274 | 57 324 | 21 907 | 21 951 |
| 64 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 47 088 | 53 415 | 52 446 | 19 915 | 19 700 |
| 65 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 47 044 | 49 380 | 48 997 | 18 681 | 18 739 |
| 66 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 46 082 | 55 009 | 55 232 | 21 277 | 21 172 |
| 67 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 45 043 | 47 421 | 47 983 | 18 759 | 18 648 |
| 68 | python (3.8)| [hug](https://hug.rest) (2.6) | 44 592 | 47 340 | 47 360 | 18 233 | 18 415 |
| 69 | javascript (13.7)| [moleculer](https://moleculer.services) (0.13) | 44 335 | 61 770 | 59 476 | 23 281 | 22 959 |
| 70 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 44 289 | 49 496 | 49 450 | 19 305 | 18 996 |
| 71 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 43 837 | 53 934 | 52 537 | 20 275 | 20 162 |
| 72 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 42 832 | 74 801 | 76 988 | 29 507 | 29 274 |
| 73 | python (3.8)| [starlette](https://starlette.io) (0.13) | 42 535 | 47 533 | 48 745 | 15 965 | 16 351 |
| 74 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 40 958 | 44 175 | 45 324 | 17 703 | 17 365 |
| 75 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 39 827 | 47 564 | 49 908 | 19 899 | 20 109 |
| 76 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 39 736 | 42 419 | 42 243 | 16 177 | 16 172 |
| 77 | elixir (1.1)| [cowboy](https://https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 39 504 | 41 931 | 41 395 | 15 387 | 15 695 |
| 78 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 37 562 | 40 064 | 39 002 | 15 015 | 15 236 |
| 79 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.28) | 36 958 | 39 194 | 38 460 | 14 724 | 14 901 |
| 80 | php (7.4)| [imi](https://imiphp.com) (1.0) | 36 762 | 41 187 | 42 542 | 16 511 | 16 448 |
| 81 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 36 317 | 72 421 | 74 729 | 28 705 | 28 523 |
| 82 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 35 141 | 37 172 | 36 672 | 14 069 | 13 853 |
| 83 | php (7.4)| [swoft](https://swoft.org) (2.0) | 34 713 | 36 565 | 35 711 | 13 805 | 13 802 |
| 84 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 33 936 | 45 308 | 44 192 | 17 033 | 17 069 |
| 85 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 33 638 | 34 126 | 34 042 | 13 322 | 13 288 |
| 86 | javascript (13.7)| [restify](https://restify.com) (8.5) | 30 968 | 40 888 | 39 460 | 15 219 | 15 153 |
| 87 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 30 590 | 57 097 | 58 538 | 22 947 | 23 048 |
| 88 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 30 349 | 20 287 | 19 084 | 11 713 | 11 702 |
| 89 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 30 312 | 31 517 | 31 018 | 11 933 | 11 926 |
| 90 | java (8)| [micronaut](https://micronaut.io) (1.2) | 29 617 | 68 757 | 69 670 | 26 457 | 26 526 |
| 91 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.48) | 28 722 | 30 205 | 30 693 | 11 388 | 11 221 |
| 92 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 28 280 | 28 765 | 30 735 | 13 555 | 13 303 |
| 93 | python (3.8)| [responder](https://python-responder.org) (2.0) | 27 761 | 31 396 | 30 925 | 11 548 | 11 517 |
| 94 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 27 694 | 28 587 | 28 160 | 11 029 | 11 134 |
| 95 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 25 088 | 25 346 | 25 290 | 9 896 | 9 903 |
| 96 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 24 769 | 25 751 | 25 759 | 9 933 | 10 018 |
| 97 | python (3.8)| [molten](https://moltenframework.com) (0.27) | 23 685 | 26 868 | 26 112 | 10 111 | 10 218 |
| 98 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 22 930 | 23 610 | 23 898 | 9 140 | 9 219 |
| 99 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 22 635 | 24 448 | 24 425 | 8 932 | 9 068 |
| 100 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 22 489 | 23 852 | 23 969 | 9 212 | 9 201 |
| 101 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 306 | 21 906 | 20 574 | 8 056 | 7 995 |
| 102 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 20 660 | 20 898 | 20 729 | 8 161 | 8 170 |
| 103 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 19 495 | 20 512 | 18 213 | 6 920 | 6 821 |
| 104 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 18 485 | 19 841 | 18 785 | 7 209 | 7 237 |
| 105 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 17 650 | 18 033 | 17 838 | 7 009 | 7 035 |
| 106 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 17 173 | 57 184 | 62 884 | 23 828 | 23 169 |
| 107 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a1) | 17 143 | 17 997 | 17 513 | 6 344 | 6 278 |
| 108 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 17 127 | 12 342 | 11 826 | 6 711 | 6 732 |
| 109 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 16 065 | 17 321 | 17 790 | 6 827 | 6 804 |
| 110 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 14 555 | 14 377 | 14 382 | 5 900 | 6 328 |
| 111 | crystal (0.32)| [lucky](https://luckyframework.org) (0.18) | 14 026 | 13 908 | 12 611 | 4 618 | 4 562 |
| 112 | ruby (2.6)| [grape](https://ruby-grape.org) (1.3) | 12 025 | 12 539 | 13 559 | 5 352 | 5 398 |
| 113 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 11 743 | 11 783 | 10 967 | 4 112 | 4 198 |
| 114 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 048 | 10 419 | 10 310 | 4 056 | 4 033 |
| 115 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 935 | 10 289 | 10 092 | 3 951 | 3 919 |
| 116 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 743 | 9 851 | 9 864 | 3 901 | 3 916 |
| 117 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 8 992 | 9 192 | 9 163 | 3 572 | 3 603 |
| 118 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 593 | 8 851 | 10 750 | 17 770 | 18 939 |
| 119 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 7 901 | 8 056 | 7 959 | 3 074 | 3 283 |
| 120 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 7 417 | 7 585 | 9 620 | 17 222 | 13 043 |
| 121 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 7 275 | 2 547 | 2 087 | 771 | 663 |
| 122 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 037 | 7 300 | 10 074 | 20 128 | 15 585 |
| 123 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 6 754 | 7 129 | 9 426 | 16 777 | 12 981 |
| 124 | crystal (0.32)| [onyx](https://onyxframework.org) (0.5) | 5 561 | 5 953 | 6 032 | 2 428 | 2 482 |
| 125 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 310 | 4 711 | 7 583 | 16 505 | 14 346 |
| 126 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 4 038 | 4 291 | 7 387 | 16 402 | 15 450 |
| 127 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 3 981 | 4 397 | 6 836 | 16 152 | 14 829 |
| 128 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 3 669 | 3 609 | 3 601 | 1 461 | 1 462 |
| 129 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 307 | 3 562 | 6 103 | 15 737 | 13 918 |
| 130 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 121 | 3 547 | 6 158 | 16 106 | 13 175 |
| 131 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 115 | 5 530 | 4 869 | 1 913 | 1 920 |
| 132 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 304 | 2 359 | 2 352 | 944 | 950 |
| 133 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 1 832 | 873 | 4 905 | 15 625 | 12 058 |
| 134 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 1 808 | 1 909 | 5 602 | 15 759 | 13 624 |
| 135 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 469 | 1 478 | 1 479 | 606 | 607 |
| 136 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 464 | 1 544 | 1 396 | 636 | 631 |
| 137 | php (7.4)| [laravel](https://laravel.com) (6.14) | 446 | 284 | 3 235 | 12 393 | 10 636 |

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

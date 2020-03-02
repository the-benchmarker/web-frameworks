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

:information_source:  Updated on **2020-03-02** :information_source:

> Benchmarking with [wrk](https://github.com/ioquatix/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 5s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 169 105 | 190 324 | 193 152 | 189 980 | 191 326 |
| 2 | javascript (13.7)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 160 157 | 180 992 | 183 937 | 180 897 | 180 264 |
| 3 | go (1.14)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.9) | 155 088 | 172 755 | 177 113 | 173 081 | 172 602 |
| 4 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.4) | 149 360 | 165 102 | 169 442 | 165 287 | 165 090 |
| 5 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 148 262 | 163 428 | 167 095 | 163 851 | 163 388 |
| 6 | go (1.14)| [router](https://github.com/fasthttp/router) (0.6) | 148 000 | 162 892 | 166 825 | 162 026 | 160 491 |
| 7 | go (1.14)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 147 415 | 161 877 | 165 873 | 160 938 | 160 776 |
| 8 | crystal (0.33)| [toro](https://github.com/soveran/toro) (0.4) | 146 233 | 162 205 | 161 980 | 152 794 | 152 959 |
| 9 | crystal (0.33)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 145 690 | 160 801 | 159 480 | 150 572 | 151 238 |
| 10 | go (1.14)| [fiber](https://fiber.wiki) (1.8) | 143 822 | 152 219 | 150 922 | 143 194 | 142 734 |
| 11 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 142 711 | 171 479 | 174 323 | 168 119 | 170 488 |
| 12 | crystal (0.33)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 140 456 | 156 276 | 154 448 | 147 775 | 148 820 |
| 13 | crystal (0.33)| [grip](https://github.com/grip-framework/grip) (0.27) | 134 898 | 148 304 | 146 788 | 137 812 | 138 635 |
| 14 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 134 869 | 152 598 | 156 100 | 152 376 | 151 836 |
| 15 | crystal (0.33)| [kemal](https://kemalcr.com) (0.26) | 133 580 | 150 188 | 150 027 | 140 266 | 140 772 |
| 16 | crystal (0.33)| [amber](https://amberframework.org) (0.33) | 131 914 | 143 745 | 142 049 | 133 969 | 134 657 |
| 17 | crystal (0.33)| [orion](https://github.com/obsidian/orion) (2.1) | 125 054 | 136 962 | 131 748 | 125 419 | 125 846 |
| 18 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (3.5) | 122 887 | 137 883 | 141 551 | 141 436 | 138 539 |
| 19 | crystal (0.33)| [athena](https://github.com/athena-framework/athena) (0.8) | 118 034 | 127 201 | 119 973 | 112 299 | 112 352 |
| 20 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 109 094 | 104 318 | 114 902 | 115 035 | 114 098 |
| 21 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 107 836 | 109 155 | 114 089 | 113 896 | 113 542 |
| 22 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 101 999 | 108 390 | 112 467 | 112 051 | 111 287 |
| 23 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.0) | 101 942 | 101 530 | 105 237 | 105 432 | 105 130 |
| 24 | go (1.14)| [violetear](https://violetear.org) (7.0) | 100 810 | 101 554 | 106 547 | 107 185 | 106 437 |
| 25 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 100 045 | 159 708 | 130 356 | 120 510 | 190 159 |
| 26 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 99 701 | 100 415 | 104 175 | 103 874 | 102 654 |
| 27 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 99 316 | 97 730 | 102 702 | 103 402 | 103 022 |
| 28 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 98 256 | 96 478 | 100 946 | 101 481 | 101 022 |
| 29 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 97 991 | 95 565 | 100 229 | 101 314 | 100 513 |
| 30 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 97 591 | 121 315 | 125 800 | 126 924 | 126 247 |
| 31 | go (1.14)| [beego](https://beego.me) (1.12) | 96 464 | 100 328 | 105 035 | 104 756 | 104 094 |
| 32 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 96 331 | 103 451 | 106 212 | 105 124 | 104 673 |
| 33 | go (1.14)| [gin](https://gin-gonic.com) (1.5) | 94 797 | 96 732 | 99 274 | 99 590 | 99 490 |
| 34 | c (99)| [kore](https://kore.io) (3.3) | 94 773 | 146 107 | 152 041 | 156 951 | 146 710 |
| 35 | java (8)| [act](https://actframework.org) (1.8) | 93 704 | 129 532 | 131 823 | 128 702 | 128 391 |
| 36 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 93 269 | 92 120 | 95 612 | 96 835 | 95 840 |
| 37 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 91 268 | 105 314 | 108 309 | 106 081 | 105 699 |
| 38 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 87 815 | 151 957 | 183 822 | 188 810 | 184 702 |
| 39 | go (1.14)| [air](https://github.com/aofei/air) (0.15) | 82 532 | 85 320 | 87 621 | 87 669 | 87 875 |
| 40 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 81 607 | 90 152 | 92 683 | 93 535 | 90 235 |
| 41 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 80 588 | 102 734 | 103 336 | 98 736 | 98 764 |
| 42 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 78 032 | 98 322 | 99 170 | 94 979 | 94 450 |
| 43 | go (1.14)| [gf](https://goframe.org) (1.11) | 75 286 | 82 486 | 85 117 | 86 027 | 85 704 |
| 44 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 73 126 | 89 005 | 87 924 | 84 450 | 84 479 |
| 45 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.1) | 72 865 | 92 902 | 92 737 | 88 692 | 88 571 |
| 46 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 71 661 | 86 925 | 85 829 | 82 959 | 81 623 |
| 47 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 71 384 | 80 167 | 84 643 | 85 558 | 84 567 |
| 48 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 69 393 | 77 951 | 78 870 | 71 763 | 77 654 |
| 49 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 65 083 | 63 239 | 66 718 | 67 201 | 67 149 |
| 50 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 62 776 | 66 991 | 66 692 | 65 263 | 65 087 |
| 51 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 60 785 | 76 688 | 75 581 | 73 088 | 72 842 |
| 52 | javascript (13.7)| [fastify](https://fastify.io) (2.12) | 58 485 | 71 554 | 70 185 | 67 833 | 67 340 |
| 53 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 57 876 | 64 142 | 63 985 | 63 032 | 62 889 |
| 54 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 57 276 | 70 527 | 69 118 | 67 433 | 67 185 |
| 55 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.12) | 56 923 | 65 480 | 65 498 | 62 733 | 62 537 |
| 56 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 55 315 | 62 067 | 62 843 | 62 316 | 61 909 |
| 57 | java (8)| [javalin](https://javalin.io) (3.5) | 53 225 | 82 464 | 80 724 | 78 288 | 78 863 |
| 58 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 51 766 | 64 196 | 62 024 | 61 308 | 61 510 |
| 59 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 50 233 | 53 456 | 52 788 | 51 793 | 52 015 |
| 60 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 50 216 | 51 800 | 51 863 | 51 105 | 51 412 |
| 61 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 50 070 | 62 138 | 60 552 | 58 874 | 58 603 |
| 62 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 49 561 | 80 673 | 83 310 | 82 746 | 83 096 |
| 63 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 49 366 | 55 533 | 55 070 | 52 944 | 52 682 |
| 64 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 48 886 | 72 150 | 70 672 | 71 263 | 71 969 |
| 65 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 47 632 | 50 820 | 50 598 | 49 641 | 49 685 |
| 66 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 47 381 | 53 258 | 53 160 | 51 249 | 51 259 |
| 67 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 46 893 | 50 951 | 51 288 | 50 914 | 50 872 |
| 68 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 45 594 | 56 736 | 55 149 | 54 064 | 55 027 |
| 69 | python (3.8)| [hug](https://hug.rest) (2.6) | 45 130 | 50 064 | 50 004 | 49 772 | 48 987 |
| 70 | python (3.8)| [starlette](https://starlette.io) (0.13) | 44 246 | 49 646 | 49 594 | 48 282 | 47 681 |
| 71 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 42 727 | 50 624 | 49 732 | 48 898 | 49 357 |
| 72 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 41 970 | 55 798 | 55 759 | 55 870 | 55 571 |
| 73 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 41 486 | 48 501 | 51 343 | 52 377 | 52 001 |
| 74 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 40 881 | 44 148 | 43 700 | 42 769 | 42 785 |
| 75 | javascript (13.7)| [moleculer](https://moleculer.services) (0.14) | 40 816 | 52 516 | 51 946 | 50 936 | 50 898 |
| 76 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 40 577 | 44 031 | 44 427 | 45 293 | 44 307 |
| 77 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 40 454 | 43 197 | 43 385 | 42 831 | 42 921 |
| 78 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 40 331 | 42 732 | 40 898 | 40 146 | 40 450 |
| 79 | php (7.4)| [imi](https://imiphp.com) (1.0) | 40 167 | 43 484 | 43 494 | 43 510 | 43 121 |
| 80 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 37 913 | 39 844 | 34 856 | 37 705 | 39 645 |
| 81 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a5) | 37 248 | 43 616 | 43 177 | 41 480 | 41 516 |
| 82 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.29) | 36 956 | 39 331 | 38 285 | 37 771 | 37 802 |
| 83 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 36 953 | 39 804 | 39 110 | 38 528 | 38 401 |
| 84 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 36 728 | 36 754 | 34 615 | 34 796 | 35 386 |
| 85 | php (7.4)| [swoft](https://swoft.org) (2.0) | 35 723 | 37 623 | 37 518 | 37 161 | 37 070 |
| 86 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 34 031 | 45 442 | 44 415 | 44 302 | 44 511 |
| 87 | javascript (13.7)| [restify](https://restify.com) (8.5) | 33 687 | 43 023 | 41 940 | 41 860 | 42 076 |
| 88 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 32 716 | 72 998 | 76 561 | 75 056 | 74 536 |
| 89 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 32 506 | 34 023 | 33 336 | 33 440 | 33 684 |
| 90 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 31 840 | 33 815 | 33 205 | 32 628 | 32 467 |
| 91 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 31 093 | 35 489 | 35 978 | 36 357 | 36 756 |
| 92 | java (8)| [micronaut](https://micronaut.io) (1.2) | 30 588 | 71 321 | 72 418 | 70 450 | 69 542 |
| 93 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.5) | 29 664 | 31 389 | 31 420 | 29 809 | 29 720 |
| 94 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 29 111 | 56 816 | 57 998 | 57 913 | 57 712 |
| 95 | python (3.8)| [responder](https://python-responder.org) (2.0) | 28 671 | 31 410 | 30 645 | 29 432 | 29 654 |
| 96 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 28 671 | 28 769 | 28 707 | 28 581 | 28 617 |
| 97 | crystal (0.33)| [lucky](https://luckyframework.org) (0.18) | 27 702 | 26 277 | 23 182 | 21 656 | 20 887 |
| 98 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 26 225 | 27 209 | 27 050 | 26 876 | 26 737 |
| 99 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 24 740 | 25 011 | 25 012 | 25 049 | 25 055 |
| 100 | dart (2.7)| [aqueduct](https://aqueduct.io) (3.2) | 24 226 | 27 325 | 27 166 | 25 316 | 26 457 |
| 101 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 23 209 | 24 684 | 25 002 | 24 629 | 24 582 |
| 102 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 22 798 | 24 705 | 24 809 | 24 561 | 24 811 |
| 103 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 22 702 | 25 013 | 24 681 | 23 272 | 23 365 |
| 104 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 281 | 21 803 | 20 311 | 20 204 | 20 293 |
| 105 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 21 332 | 22 269 | 22 097 | 21 537 | 21 509 |
| 106 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 20 211 | 21 925 | 21 811 | 20 997 | 20 926 |
| 107 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 18 040 | 58 301 | 66 464 | 63 595 | 65 308 |
| 108 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 17 251 | 17 296 | 17 392 | 17 152 | 17 344 |
| 109 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 16 555 | 17 676 | 17 993 | 17 818 | 17 877 |
| 110 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 15 600 | 16 220 | 16 235 | 16 367 | 14 717 |
| 111 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 14 729 | 15 142 | 15 301 | 15 274 | 15 232 |
| 112 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 13 526 | 13 749 | 13 800 | 13 780 | 13 807 |
| 113 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 13 200 | 13 167 | 13 058 | 12 967 | 13 008 |
| 114 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 12 225 | 12 253 | 11 582 | 10 989 | 11 056 |
| 115 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 11 681 | 11 784 | 11 705 | 11 679 | 11 706 |
| 116 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 10 535 | 10 901 | 10 514 | 10 305 | 10 443 |
| 117 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 370 | 10 749 | 10 670 | 10 659 | 10 339 |
| 118 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 9 040 | 9 271 | 9 274 | 9 116 | 9 232 |
| 119 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 571 | 9 101 | 11 339 | 53 649 | 51 525 |
| 120 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 7 893 | 8 215 | 8 211 | 8 012 | 8 167 |
| 121 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 7 625 | 2 047 | 1 499 | 1 201 | 1 622 |
| 122 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 7 545 | 7 815 | 10 519 | 45 822 | 42 531 |
| 123 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 235 | 7 369 | 10 189 | 51 936 | 43 433 |
| 124 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 047 | 7 352 | 10 516 | 45 699 | 40 666 |
| 125 | crystal (0.33)| [onyx](https://onyxframework.org) (0.5) | 4 512 | 4 788 | 4 805 | 4 766 | 4 787 |
| 126 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 317 | 4 647 | 7 636 | 43 148 | 33 622 |
| 127 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 4 131 | 4 307 | 6 917 | 42 264 | 33 971 |
| 128 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 098 | 4 471 | 7 386 | 44 446 | 36 029 |
| 129 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 4 062 | 3 969 | 3 913 | 3 903 | 3 888 |
| 130 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 376 | 3 709 | 6 623 | 43 101 | 38 929 |
| 131 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 307 | 3 620 | 6 650 | 43 782 | 38 028 |
| 132 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 2 928 | 4 025 | 2 066 | 1 818 | 1 904 |
| 133 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 193 | 2 216 | 2 182 | 2 133 | 2 157 |
| 134 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 1 836 | 1 909 | 5 933 | 40 783 | 41 173 |
| 135 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 1 781 | 907 | 7 748 | 38 156 | 32 848 |
| 136 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 530 | 1 556 | 1 522 | 1 524 | 1 529 |
| 137 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 518 | 1 501 | 1 466 | 1 410 | 1 452 |
| 138 | php (7.4)| [laravel](https://laravel.com) (6.17) | 493 | 266 | 10 755 | 25 433 | 21 895 |

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

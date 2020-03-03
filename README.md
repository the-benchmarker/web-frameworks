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

:information_source:  Updated on **2020-03-03** :information_source:

> Benchmarking with [wrk](https://github.com/ioquatix/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 5s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 185 146 | 198 485 | 199 450 | 195 417 | 195 800 |
| 2 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 180 785 | 194 003 | 194 265 | 190 839 | 191 333 |
| 3 | javascript (13.7)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 174 195 | 186 016 | 185 908 | 183 287 | 183 041 |
| 4 | go (1.14)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.9) | 162 145 | 171 100 | 173 299 | 168 177 | 167 901 |
| 5 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 158 338 | 174 195 | 174 080 | 168 959 | 168 583 |
| 6 | go (1.14)| [router](https://github.com/fasthttp/router) (0.6) | 157 466 | 165 562 | 168 239 | 163 849 | 163 678 |
| 7 | go (1.14)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 157 323 | 165 247 | 168 026 | 162 958 | 162 768 |
| 8 | crystal (0.33)| [toro](https://github.com/soveran/toro) (0.4) | 156 323 | 164 530 | 161 642 | 152 737 | 152 653 |
| 9 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.4) | 156 057 | 164 707 | 167 009 | 162 853 | 162 503 |
| 10 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 156 003 | 164 083 | 166 513 | 161 943 | 161 591 |
| 11 | crystal (0.33)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 155 081 | 163 063 | 160 372 | 151 160 | 150 745 |
| 12 | go (1.14)| [fiber](https://fiber.wiki) (1.8) | 153 287 | 155 085 | 152 199 | 143 460 | 143 608 |
| 13 | crystal (0.33)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 149 022 | 156 245 | 153 109 | 146 141 | 145 376 |
| 14 | crystal (0.33)| [kemal](https://kemalcr.com) (0.26) | 145 562 | 152 452 | 149 413 | 140 600 | 140 095 |
| 15 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 143 651 | 154 187 | 155 177 | 150 710 | 151 128 |
| 16 | crystal (0.33)| [grip](https://github.com/grip-framework/grip) (0.27) | 142 279 | 148 678 | 144 708 | 135 169 | 134 255 |
| 17 | crystal (0.33)| [amber](https://amberframework.org) (0.33) | 137 842 | 144 231 | 140 185 | 131 812 | 130 911 |
| 18 | crystal (0.33)| [orion](https://github.com/obsidian/orion) (2.1) | 133 695 | 138 061 | 132 813 | 122 036 | 120 901 |
| 19 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (3.5) | 131 344 | 140 699 | 141 912 | 140 115 | 139 629 |
| 20 | crystal (0.33)| [athena](https://github.com/athena-framework/athena) (0.8) | 126 088 | 129 523 | 122 990 | 111 369 | 111 029 |
| 21 | java (8)| [act](https://actframework.org) (1.8) | 119 942 | 133 582 | 133 364 | 130 320 | 130 035 |
| 22 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 111 059 | 110 330 | 113 249 | 113 345 | 113 368 |
| 23 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 109 363 | 108 311 | 111 926 | 111 973 | 111 343 |
| 24 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.0) | 105 648 | 104 284 | 107 229 | 107 018 | 106 838 |
| 25 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 104 700 | 108 149 | 110 546 | 109 819 | 109 502 |
| 26 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 103 801 | 103 292 | 105 918 | 105 671 | 104 974 |
| 27 | go (1.14)| [violetear](https://violetear.org) (7.0) | 102 066 | 101 690 | 104 938 | 105 085 | 105 015 |
| 28 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 100 083 | 98 661 | 101 963 | 102 569 | 102 076 |
| 29 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 99 616 | 98 181 | 101 523 | 102 048 | 101 790 |
| 30 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 99 260 | 120 820 | 124 403 | 124 449 | 124 345 |
| 31 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 99 198 | 102 854 | 104 294 | 102 442 | 102 272 |
| 32 | go (1.14)| [beego](https://beego.me) (1.12) | 96 852 | 99 821 | 103 003 | 102 901 | 102 718 |
| 33 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 96 837 | 103 903 | 104 121 | 101 497 | 101 879 |
| 34 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 96 712 | 95 121 | 98 187 | 98 794 | 98 435 |
| 35 | go (1.14)| [gin](https://gin-gonic.com) (1.5) | 95 439 | 98 277 | 100 474 | 100 850 | 100 512 |
| 36 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 95 095 | 94 069 | 97 039 | 98 101 | 97 721 |
| 37 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 95 056 | 104 273 | 103 573 | 98 925 | 98 636 |
| 38 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 91 983 | 100 654 | 100 289 | 96 132 | 95 808 |
| 39 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 88 696 | 91 898 | 93 215 | 92 024 | 93 222 |
| 40 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.1) | 86 757 | 95 085 | 94 216 | 90 304 | 90 188 |
| 41 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 86 595 | 90 629 | 88 542 | 85 899 | 85 414 |
| 42 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 86 434 | 144 582 | 184 936 | 184 769 | 181 456 |
| 43 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 83 992 | 88 633 | 86 431 | 83 421 | 83 207 |
| 44 | go (1.14)| [air](https://github.com/aofei/air) (0.15) | 83 158 | 86 712 | 88 949 | 89 103 | 88 774 |
| 45 | java (8)| [javalin](https://javalin.io) (3.5) | 78 686 | 83 459 | 81 545 | 80 275 | 80 244 |
| 46 | go (1.14)| [gf](https://goframe.org) (1.11) | 77 266 | 82 481 | 83 945 | 83 870 | 83 585 |
| 47 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 75 484 | 79 724 | 76 668 | 74 618 | 75 459 |
| 48 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 74 231 | 78 038 | 78 231 | 76 784 | 76 716 |
| 49 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 72 245 | 79 308 | 78 482 | 76 832 | 76 376 |
| 50 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 71 695 | 79 485 | 84 425 | 84 615 | 84 413 |
| 51 | javascript (13.7)| [fastify](https://fastify.io) (2.12) | 70 855 | 74 264 | 71 882 | 70 010 | 70 714 |
| 52 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 70 273 | 74 160 | 72 934 | 70 332 | 70 682 |
| 53 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 69 560 | 72 611 | 71 027 | 69 222 | 69 011 |
| 54 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 67 066 | 79 741 | 81 988 | 81 015 | 80 255 |
| 55 | c (99)| [kore](https://kore.io) (3.3) | 66 258 | 136 552 | 162 185 | 156 036 | 146 390 |
| 56 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 64 587 | 65 331 | 64 939 | 63 768 | 63 506 |
| 57 | java (8)| [micronaut](https://micronaut.io) (1.2) | 63 605 | 70 925 | 70 850 | 68 512 | 68 526 |
| 58 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 62 521 | 65 308 | 63 366 | 61 774 | 61 687 |
| 59 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 62 204 | 65 031 | 68 132 | 68 239 | 68 134 |
| 60 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 61 331 | 63 282 | 62 235 | 60 095 | 59 926 |
| 61 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 59 819 | 62 738 | 63 076 | 62 012 | 61 940 |
| 62 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 58 895 | 60 779 | 62 115 | 61 231 | 61 383 |
| 63 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.12) | 57 202 | 64 361 | 63 882 | 61 269 | 61 323 |
| 64 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 57 051 | 58 859 | 58 716 | 58 355 | 58 001 |
| 65 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 55 849 | 55 488 | 55 371 | 54 958 | 54 686 |
| 66 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 55 632 | 57 744 | 56 565 | 55 778 | 55 559 |
| 67 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 55 445 | 63 303 | 62 550 | 61 909 | 62 127 |
| 68 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 52 879 | 58 437 | 58 266 | 56 429 | 56 319 |
| 69 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 52 024 | 58 638 | 60 501 | 61 786 | 61 453 |
| 70 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 51 016 | 51 328 | 51 014 | 50 243 | 50 233 |
| 71 | javascript (13.7)| [moleculer](https://moleculer.services) (0.14) | 50 684 | 52 399 | 51 569 | 50 725 | 50 503 |
| 72 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 50 166 | 50 520 | 50 323 | 49 677 | 49 745 |
| 73 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 49 819 | 50 948 | 50 585 | 49 922 | 49 937 |
| 74 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 49 164 | 54 975 | 54 680 | 52 730 | 52 549 |
| 75 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 48 759 | 50 596 | 50 775 | 50 217 | 50 238 |
| 76 | python (3.8)| [hug](https://hug.rest) (2.6) | 47 017 | 49 795 | 49 173 | 48 672 | 48 791 |
| 77 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 46 633 | 50 429 | 49 947 | 49 193 | 49 502 |
| 78 | python (3.8)| [starlette](https://starlette.io) (0.13) | 45 376 | 48 674 | 48 300 | 46 868 | 46 737 |
| 79 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 45 263 | 45 004 | 45 317 | 45 192 | 44 180 |
| 80 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 43 685 | 46 666 | 45 975 | 45 771 | 45 466 |
| 81 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 42 631 | 43 668 | 43 321 | 42 184 | 42 290 |
| 82 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 42 177 | 49 315 | 50 983 | 53 356 | 53 232 |
| 83 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 41 802 | 43 383 | 40 970 | 40 277 | 40 549 |
| 84 | php (7.4)| [imi](https://imiphp.com) (1.0) | 41 800 | 43 382 | 43 757 | 43 430 | 43 505 |
| 85 | javascript (13.7)| [restify](https://restify.com) (8.5) | 41 482 | 43 603 | 42 646 | 42 246 | 42 391 |
| 86 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 41 085 | 43 025 | 42 701 | 42 104 | 42 186 |
| 87 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 40 324 | 42 595 | 39 447 | 39 472 | 39 961 |
| 88 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a5) | 39 910 | 42 893 | 42 438 | 41 007 | 40 932 |
| 89 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 37 911 | 38 860 | 38 333 | 37 482 | 37 403 |
| 90 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.29) | 37 754 | 38 491 | 37 045 | 36 337 | 36 554 |
| 91 | php (7.4)| [swoft](https://swoft.org) (2.0) | 35 455 | 36 903 | 36 536 | 36 088 | 36 028 |
| 92 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 33 799 | 35 231 | 34 142 | 34 243 | 34 128 |
| 93 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 33 506 | 33 817 | 35 770 | 34 787 | 35 990 |
| 94 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 32 514 | 32 846 | 31 478 | 30 588 | 31 897 |
| 95 | crystal (0.33)| [lucky](https://luckyframework.org) (0.18) | 31 607 | 29 758 | 27 900 | 22 349 | 21 685 |
| 96 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.5) | 29 451 | 31 352 | 31 021 | 29 770 | 29 793 |
| 97 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 28 828 | 29 144 | 28 471 | 28 506 | 28 538 |
| 98 | python (3.8)| [responder](https://python-responder.org) (2.0) | 28 806 | 31 384 | 31 060 | 29 842 | 29 672 |
| 99 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 26 795 | 27 294 | 27 502 | 27 217 | 26 995 |
| 100 | dart (2.7)| [aqueduct](https://aqueduct.io) (3.2) | 26 629 | 25 740 | 26 032 | 24 820 | 25 000 |
| 101 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 26 280 | 26 610 | 26 728 | 26 631 | 26 612 |
| 102 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 25 302 | 24 899 | 24 599 | 24 368 | 24 669 |
| 103 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 25 191 | 28 028 | 27 921 | 26 965 | 27 083 |
| 104 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 23 818 | 22 703 | 21 751 | 21 130 | 21 204 |
| 105 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 23 713 | 24 836 | 24 763 | 24 391 | 24 423 |
| 106 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 21 779 | 24 645 | 24 628 | 24 136 | 24 247 |
| 107 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 21 479 | 21 895 | 21 400 | 20 651 | 20 585 |
| 108 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 18 324 | 18 489 | 18 419 | 18 392 | 18 470 |
| 109 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 151 | 17 660 | 17 793 | 17 687 | 17 722 |
| 110 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 15 966 | 16 124 | 16 146 | 16 133 | 16 094 |
| 111 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 15 216 | 15 681 | 15 674 | 15 696 | 15 645 |
| 112 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 14 207 | 14 047 | 14 077 | 14 064 | 14 002 |
| 113 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 13 347 | 13 095 | 13 086 | 13 123 | 12 955 |
| 114 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 12 111 | 12 002 | 11 989 | 11 936 | 11 929 |
| 115 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 11 860 | 12 079 | 11 300 | 10 772 | 10 814 |
| 116 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 560 | 11 599 | 11 566 | 11 612 | 11 688 |
| 117 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 209 | 10 681 | 10 566 | 10 437 | 10 531 |
| 118 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 582 | 10 564 | 10 443 | 10 261 | 10 230 |
| 119 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 9 319 | 9 220 | 9 128 | 9 140 | 9 209 |
| 120 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 125 | 9 151 | 8 984 | 52 340 | 50 528 |
| 121 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 7 893 | 7 839 | 7 733 | 44 263 | 42 291 |
| 122 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 492 | 7 482 | 7 496 | 52 381 | 49 903 |
| 123 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 431 | 7 436 | 7 471 | 44 240 | 41 943 |
| 124 | crystal (0.33)| [onyx](https://onyxframework.org) (0.5) | 4 824 | 4 947 | 4 956 | 4 972 | 4 946 |
| 125 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 590 | 4 599 | 4 712 | 43 723 | 42 578 |
| 126 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 389 | 4 460 | 4 475 | 43 220 | 41 058 |
| 127 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 4 298 | 4 288 | 4 262 | 42 751 | 38 959 |
| 128 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 4 175 | 3 990 | 3 985 | 3 974 | 3 968 |
| 129 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 639 | 3 725 | 3 770 | 43 128 | 39 149 |
| 130 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 615 | 3 601 | 3 690 | 42 899 | 39 464 |
| 131 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 432 | 2 433 | 2 407 | 2 405 | 2 386 |
| 132 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 1 956 | 5 463 | 4 361 | 3 442 | 2 140 |
| 133 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 1 930 | 1 960 | 1 984 | 40 666 | 37 984 |
| 134 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 573 | 1 604 | 1 576 | 1 565 | 1 561 |
| 135 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 559 | 1 525 | 1 510 | 1 498 | 1 480 |
| 136 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 1 187 | 1 104 | 1 256 | 1 247 | 510 |
| 137 | php (7.4)| [laravel](https://laravel.com) (6.17) | 833 | 167 | 2 512 | 23 698 | 22 630 |
| 138 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 520 | 467 | 1 779 | 35 238 | 33 271 |

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

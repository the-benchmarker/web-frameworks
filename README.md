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

:information_source:  Updated on **2020-03-09** :information_source:

> Benchmarking with [wrk](https://github.com/ioquatix/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 5s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | javascript (13.7)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.1) | 185 775 | 198 291 | 197 341 | 195 295 | 195 520 |
| 2 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 181 529 | 194 183 | 193 995 | 191 003 | 191 449 |
| 3 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 179 444 | 198 345 | 198 864 | 194 958 | 189 057 |
| 4 | javascript (13.7)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (1.1) | 172 995 | 184 100 | 183 344 | 178 034 | 181 432 |
| 5 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.9) | 162 212 | 171 195 | 173 083 | 168 118 | 167 974 |
| 6 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 158 586 | 173 982 | 173 846 | 168 603 | 168 512 |
| 7 | go (1.14)| [router](https://github.com/fasthttp/router) (0.6) | 156 888 | 164 985 | 167 373 | 162 036 | 161 765 |
| 8 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 156 861 | 164 396 | 166 856 | 162 311 | 162 313 |
| 9 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.4) | 155 781 | 163 588 | 165 856 | 161 634 | 161 060 |
| 10 | crystal (0.33)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 155 215 | 162 626 | 159 844 | 150 581 | 150 367 |
| 11 | crystal (0.33)| [toro](https://github.com/soveran/toro) (0.4) | 154 688 | 163 275 | 160 304 | 150 374 | 150 214 |
| 12 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 154 016 | 162 035 | 164 153 | 159 599 | 158 936 |
| 13 | crystal (0.33)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 152 151 | 158 856 | 155 326 | 147 151 | 146 985 |
| 14 | go (1.14)| [fiber](https://fiber.wiki) (1.8) | 150 270 | 151 996 | 149 212 | 140 677 | 141 106 |
| 15 | crystal (0.33)| [grip](https://github.com/grip-framework/grip) (0.28) | 143 776 | 150 549 | 147 324 | 137 830 | 137 814 |
| 16 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 143 023 | 153 492 | 154 044 | 150 316 | 150 797 |
| 17 | crystal (0.33)| [kemal](https://kemalcr.com) (0.26) | 142 766 | 149 593 | 145 759 | 137 848 | 136 936 |
| 18 | crystal (0.33)| [amber](https://amberframework.org) (0.33) | 137 207 | 142 462 | 138 750 | 129 850 | 128 906 |
| 19 | crystal (0.33)| [lucky](https://luckyframework.org) (0.18) | 132 997 | 137 685 | 131 780 | 122 924 | 121 713 |
| 20 | crystal (0.33)| [orion](https://github.com/obsidian/orion) (2.1) | 132 876 | 137 524 | 132 360 | 123 475 | 122 180 |
| 21 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (3.5) | 131 582 | 140 348 | 141 840 | 139 798 | 139 085 |
| 22 | crystal (0.33)| [athena](https://github.com/athena-framework/athena) (0.8) | 125 256 | 127 727 | 121 904 | 110 612 | 110 743 |
| 23 | java (8)| [act](https://actframework.org) (1.8) | 120 312 | 133 643 | 133 232 | 130 125 | 129 740 |
| 24 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 110 038 | 109 074 | 112 601 | 111 955 | 111 594 |
| 25 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 108 894 | 107 870 | 111 148 | 110 804 | 110 378 |
| 26 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.0) | 104 837 | 103 062 | 106 136 | 105 699 | 105 748 |
| 27 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 104 584 | 107 829 | 110 514 | 109 418 | 109 137 |
| 28 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 102 872 | 102 202 | 105 036 | 104 564 | 104 442 |
| 29 | go (1.14)| [violetear](https://violetear.org) (7.0) | 101 459 | 100 953 | 104 067 | 104 126 | 103 878 |
| 30 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 99 861 | 98 360 | 101 661 | 101 753 | 101 876 |
| 31 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 99 583 | 103 057 | 104 414 | 102 728 | 102 465 |
| 32 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 98 810 | 97 446 | 100 484 | 100 797 | 100 286 |
| 33 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 97 926 | 117 764 | 122 201 | 122 346 | 122 129 |
| 34 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 96 763 | 94 767 | 98 033 | 98 274 | 98 095 |
| 35 | go (1.14)| [beego](https://beego.me) (1.12) | 96 762 | 99 232 | 102 053 | 102 157 | 102 201 |
| 36 | go (1.14)| [gin](https://gin-gonic.com) (1.5) | 95 743 | 98 734 | 101 124 | 100 794 | 100 039 |
| 37 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 94 806 | 104 185 | 103 509 | 99 026 | 98 355 |
| 38 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 94 577 | 101 616 | 102 075 | 99 237 | 99 725 |
| 39 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 94 557 | 93 341 | 96 693 | 97 265 | 96 949 |
| 40 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 91 287 | 100 061 | 99 243 | 94 656 | 94 869 |
| 41 | c (99)| [kore](https://kore.io) (3.3) | 89 072 | 162 602 | 159 540 | 154 863 | 158 015 |
| 42 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 87 995 | 91 922 | 93 198 | 92 957 | 92 261 |
| 43 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.1) | 86 722 | 94 748 | 93 606 | 89 816 | 89 387 |
| 44 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 86 251 | 90 419 | 88 435 | 85 594 | 84 543 |
| 45 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 84 634 | 88 976 | 86 713 | 83 798 | 83 774 |
| 46 | go (1.14)| [air](https://github.com/aofei/air) (0.15) | 82 604 | 86 511 | 88 512 | 88 570 | 88 397 |
| 47 | java (8)| [javalin](https://javalin.io) (3.5) | 77 950 | 82 907 | 81 356 | 80 169 | 79 734 |
| 48 | go (1.14)| [gf](https://goframe.org) (1.11) | 77 073 | 82 138 | 83 764 | 83 514 | 83 462 |
| 49 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 74 909 | 79 544 | 76 729 | 75 556 | 75 980 |
| 50 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 73 911 | 77 543 | 77 638 | 75 716 | 76 091 |
| 51 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 71 752 | 79 433 | 84 711 | 84 695 | 84 560 |
| 52 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 71 455 | 77 996 | 76 965 | 74 757 | 74 630 |
| 53 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 70 038 | 74 497 | 72 322 | 70 267 | 70 364 |
| 54 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 69 167 | 72 218 | 70 368 | 68 508 | 68 541 |
| 55 | javascript (13.7)| [fastify](https://fastify.io) (2.12) | 68 963 | 72 155 | 70 410 | 67 786 | 67 577 |
| 56 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 66 138 | 79 105 | 80 837 | 79 861 | 79 967 |
| 57 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 64 472 | 65 196 | 64 700 | 63 151 | 63 038 |
| 58 | java (8)| [micronaut](https://micronaut.io) (1.2) | 63 748 | 71 287 | 71 507 | 69 307 | 69 271 |
| 59 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 62 107 | 64 696 | 62 934 | 61 608 | 61 604 |
| 60 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 61 974 | 65 039 | 67 832 | 67 652 | 67 769 |
| 61 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 60 928 | 63 115 | 61 361 | 60 247 | 59 504 |
| 62 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 59 793 | 62 451 | 62 250 | 61 686 | 61 593 |
| 63 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 57 695 | 144 018 | 188 360 | 184 802 | 183 259 |
| 64 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 55 458 | 64 739 | 63 460 | 63 352 | 63 074 |
| 65 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 55 173 | 54 909 | 54 832 | 54 011 | 53 978 |
| 66 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 55 041 | 57 327 | 55 909 | 54 778 | 54 718 |
| 67 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 55 040 | 56 168 | 56 032 | 55 862 | 55 779 |
| 68 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 55 036 | 62 849 | 62 765 | 61 909 | 61 934 |
| 69 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.14) | 54 388 | 58 206 | 57 811 | 55 725 | 55 631 |
| 70 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 53 180 | 57 341 | 57 228 | 55 278 | 55 153 |
| 71 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 52 076 | 57 987 | 58 631 | 60 185 | 60 279 |
| 72 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 51 224 | 50 699 | 50 788 | 50 041 | 50 036 |
| 73 | javascript (13.7)| [moleculer](https://moleculer.services) (0.14) | 50 705 | 52 306 | 51 068 | 50 437 | 50 514 |
| 74 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 50 469 | 54 307 | 53 823 | 51 882 | 51 906 |
| 75 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 50 133 | 50 224 | 49 908 | 49 116 | 49 295 |
| 76 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 49 620 | 50 509 | 50 183 | 49 519 | 49 637 |
| 77 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 48 695 | 50 401 | 50 589 | 49 981 | 50 024 |
| 78 | python (3.8)| [hug](https://hug.rest) (2.6) | 46 887 | 49 509 | 49 182 | 48 706 | 48 560 |
| 79 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 46 841 | 49 815 | 49 476 | 49 214 | 49 005 |
| 80 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 46 240 | 45 680 | 46 204 | 46 154 | 44 647 |
| 81 | python (3.8)| [starlette](https://starlette.io) (0.13) | 44 930 | 49 005 | 48 735 | 47 148 | 46 961 |
| 82 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 43 944 | 46 537 | 45 572 | 45 176 | 45 172 |
| 83 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 42 260 | 43 400 | 42 866 | 41 938 | 41 832 |
| 84 | php (7.4)| [imi](https://imiphp.com) (1.0) | 41 758 | 43 552 | 43 777 | 43 222 | 43 461 |
| 85 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 41 644 | 49 311 | 51 616 | 53 120 | 53 172 |
| 86 | javascript (13.7)| [restify](https://restify.com) (8.5) | 41 225 | 43 228 | 42 229 | 42 015 | 42 071 |
| 87 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 41 108 | 43 045 | 41 191 | 40 402 | 40 602 |
| 88 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 39 862 | 43 325 | 43 148 | 42 575 | 42 751 |
| 89 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 38 725 | 42 586 | 39 782 | 39 668 | 40 263 |
| 90 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.29) | 38 305 | 39 131 | 37 429 | 37 045 | 37 467 |
| 91 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a5) | 38 086 | 43 130 | 42 705 | 41 107 | 41 302 |
| 92 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 37 463 | 38 389 | 37 969 | 36 929 | 36 904 |
| 93 | php (7.4)| [swoft](https://swoft.org) (2.0) | 35 648 | 36 979 | 36 765 | 36 274 | 36 232 |
| 94 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 34 331 | 34 136 | 36 893 | 36 710 | 34 271 |
| 95 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 33 630 | 33 934 | 32 828 | 32 530 | 32 873 |
| 96 | dart (2.7)| [aqueduct](https://aqueduct.io) (3.2) | 32 746 | 33 936 | 33 279 | 32 449 | 32 726 |
| 97 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 32 354 | 33 038 | 32 689 | 31 935 | 31 803 |
| 98 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.52) | 30 176 | 32 087 | 31 668 | 30 464 | 30 268 |
| 99 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 29 468 | 29 108 | 28 584 | 28 437 | 28 734 |
| 100 | python (3.8)| [responder](https://python-responder.org) (2.0) | 28 924 | 31 461 | 31 104 | 29 891 | 29 846 |
| 101 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 27 110 | 27 499 | 27 191 | 27 006 | 27 129 |
| 102 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 26 166 | 26 580 | 26 780 | 26 360 | 26 485 |
| 103 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 25 985 | 25 651 | 25 244 | 25 308 | 25 421 |
| 104 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 25 775 | 28 238 | 28 063 | 27 181 | 27 050 |
| 105 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 24 094 | 24 930 | 24 993 | 24 453 | 24 455 |
| 106 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 24 007 | 22 824 | 21 862 | 21 316 | 21 285 |
| 107 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 23 812 | 24 453 | 24 472 | 23 925 | 24 064 |
| 108 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 21 309 | 21 914 | 21 338 | 20 540 | 20 521 |
| 109 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 18 284 | 18 426 | 18 444 | 18 443 | 18 424 |
| 110 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 238 | 17 808 | 17 865 | 17 800 | 17 787 |
| 111 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 16 178 | 16 096 | 16 114 | 16 123 | 16 016 |
| 112 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 15 012 | 15 524 | 15 526 | 15 516 | 15 548 |
| 113 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 14 136 | 14 012 | 14 019 | 13 954 | 14 055 |
| 114 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 13 280 | 13 042 | 13 015 | 12 955 | 12 945 |
| 115 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 11 942 | 11 865 | 11 847 | 11 820 | 11 822 |
| 116 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 467 | 11 489 | 11 472 | 11 472 | 11 607 |
| 117 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 348 | 10 235 | 10 172 | 10 080 | 10 067 |
| 118 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 9 939 | 12 153 | 11 284 | 10 748 | 10 782 |
| 119 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 615 | 10 676 | 10 443 | 10 245 | 10 281 |
| 120 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 9 295 | 9 067 | 9 136 | 9 141 | 9 085 |
| 121 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 172 | 9 110 | 9 016 | 52 309 | 48 157 |
| 122 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 7 838 | 7 807 | 7 804 | 45 235 | 42 995 |
| 123 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 471 | 7 523 | 7 452 | 44 544 | 43 418 |
| 124 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 418 | 7 444 | 7 465 | 51 543 | 52 478 |
| 125 | crystal (0.33)| [onyx](https://onyxframework.org) (0.5) | 4 766 | 4 887 | 4 898 | 4 924 | 4 890 |
| 126 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 621 | 4 643 | 4 792 | 43 337 | 41 262 |
| 127 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 364 | 4 390 | 4 431 | 43 952 | 41 567 |
| 128 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 4 303 | 4 312 | 4 381 | 43 279 | 41 882 |
| 129 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 4 191 | 4 000 | 3 999 | 3 986 | 3 980 |
| 130 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 614 | 3 665 | 3 830 | 42 509 | 40 482 |
| 131 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 611 | 3 630 | 3 652 | 43 090 | 40 328 |
| 132 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 2 940 | 1 501 | 1 368 | 1 215 | 586 |
| 133 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 265 | 2 262 | 2 236 | 2 234 | 2 222 |
| 134 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 1 925 | 1 957 | 1 995 | 40 029 | 38 711 |
| 135 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 1 839 | 5 377 | 4 285 | 3 387 | 2 116 |
| 136 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 572 | 1 599 | 1 579 | 1 564 | 1 563 |
| 137 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 553 | 1 528 | 1 502 | 1 473 | 1 474 |
| 138 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 531 | 412 | 1 926 | 36 048 | 34 000 |
| 139 | php (7.4)| [laravel](https://laravel.com) (7.0) | 379 | 160 | 2 638 | 22 177 | 24 180 |

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

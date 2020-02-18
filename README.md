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

:information_source:  Updated on **2020-02-18** :information_source:

> Benchmarking with [wrk](https://github.com/ioquatix/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 5s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 180 128 | 201 196 | 202 661 | 200 861 | 201 719 |
| 2 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 175 851 | 197 172 | 196 942 | 197 842 | 196 975 |
| 3 | javascript (13.7)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 165 288 | 185 554 | 188 777 | 186 146 | 184 031 |
| 4 | go (1.13)| [fiber](https://fiber.wiki) (1.6) | 158 776 | 179 757 | 181 714 | 179 461 | 178 452 |
| 5 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.9) | 156 596 | 174 181 | 180 590 | 176 922 | 176 772 |
| 6 | go (1.13)| [router](https://github.com/fasthttp/router) (0.6) | 152 046 | 168 609 | 175 281 | 171 914 | 166 029 |
| 7 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.3) | 150 387 | 168 213 | 173 904 | 171 620 | 170 295 |
| 8 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 150 283 | 168 153 | 174 037 | 171 463 | 170 746 |
| 9 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 149 451 | 167 455 | 173 438 | 171 159 | 170 164 |
| 10 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 146 798 | 176 639 | 177 774 | 175 129 | 174 979 |
| 11 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 142 679 | 159 127 | 161 631 | 159 195 | 156 202 |
| 12 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (3.5) | 128 525 | 143 884 | 146 617 | 145 999 | 144 138 |
| 13 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 110 424 | 113 022 | 119 500 | 118 795 | 118 402 |
| 14 | go (1.13)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 108 482 | 110 948 | 117 154 | 116 926 | 116 615 |
| 15 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 105 231 | 105 661 | 111 544 | 111 150 | 110 608 |
| 16 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 104 984 | 110 529 | 115 601 | 114 756 | 114 858 |
| 17 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 101 737 | 101 039 | 107 485 | 107 874 | 107 269 |
| 18 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 101 648 | 104 718 | 111 807 | 111 204 | 110 946 |
| 19 | crystal (0.33)| [toro](https://github.com/soveran/toro) (0.4) | 101 324 | 109 660 | 108 535 | 104 294 | 103 918 |
| 20 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 100 917 | 100 548 | 106 478 | 106 094 | 105 625 |
| 21 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 99 678 | 97 965 | 104 074 | 104 279 | 104 006 |
| 22 | go (1.13)| [violetear](https://violetear.org) (7.0) | 99 672 | 101 321 | 108 006 | 107 637 | 107 443 |
| 23 | crystal (0.33)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 99 612 | 106 434 | 104 786 | 100 775 | 100 908 |
| 24 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 98 859 | 102 259 | 107 071 | 106 742 | 106 480 |
| 25 | go (1.13)| [beego](https://beego.me) (1.12) | 97 555 | 101 612 | 107 551 | 107 233 | 106 711 |
| 26 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 97 248 | 106 115 | 109 655 | 108 268 | 107 538 |
| 27 | crystal (0.33)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 95 692 | 102 545 | 101 266 | 96 370 | 96 484 |
| 28 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 95 510 | 109 481 | 111 583 | 109 803 | 110 574 |
| 29 | ruby (2.6)| [agoo](https://github.com/ohler55/agoo) (2.12) | 95 010 | 126 608 | 133 057 | 136 245 | 135 360 |
| 30 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 94 995 | 95 024 | 101 245 | 101 825 | 101 554 |
| 31 | crystal (0.33)| [raze](https://razecr.com) (0.3) | 93 189 | 100 521 | 98 894 | 95 547 | 94 780 |
| 32 | java (8)| [act](https://actframework.org) (1.8) | 92 332 | 135 144 | 137 832 | 134 351 | 132 842 |
| 33 | c (99)| [kore](https://kore.io) (3.3) | 87 208 | 150 057 | 155 829 | 161 708 | 157 484 |
| 34 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 86 985 | 106 129 | 107 557 | 102 720 | 102 580 |
| 35 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 86 489 | 88 801 | 93 190 | 93 345 | 92 576 |
| 36 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 86 078 | 187 406 | 193 682 | 190 985 | 155 488 |
| 37 | crystal (0.33)| [kemal](https://kemalcr.com) (0.26) | 85 893 | 90 691 | 89 651 | 85 432 | 84 886 |
| 38 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 82 149 | 101 470 | 102 669 | 98 441 | 97 976 |
| 39 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 82 040 | 91 603 | 94 734 | 93 479 | 94 280 |
| 40 | crystal (0.33)| [grip](https://github.com/grkek/grip) (0.27) | 81 544 | 87 179 | 84 881 | 82 061 | 82 099 |
| 41 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 77 767 | 91 467 | 89 472 | 86 658 | 86 047 |
| 42 | crystal (0.33)| [amber](https://amberframework.org) (0.3) | 77 013 | 82 676 | 81 222 | 77 721 | 78 122 |
| 43 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.0) | 76 721 | 96 564 | 96 422 | 92 833 | 91 879 |
| 44 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 75 524 | 89 653 | 88 504 | 84 907 | 84 538 |
| 45 | go (1.13)| [gf](https://goframe.org) (1.11) | 75 414 | 83 331 | 86 273 | 86 277 | 85 799 |
| 46 | crystal (0.33)| [orion](https://github.com/obsidian/orion) (2.1) | 74 422 | 78 302 | 76 019 | 72 570 | 73 201 |
| 47 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 74 255 | 81 124 | 81 312 | 80 250 | 80 231 |
| 48 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 73 663 | 81 907 | 87 522 | 88 353 | 86 655 |
| 49 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 67 167 | 65 563 | 69 769 | 70 254 | 70 376 |
| 50 | elixir (1.1)| [cowboy_stream](https://https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 66 039 | 69 328 | 69 198 | 67 697 | 67 779 |
| 51 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 64 328 | 78 771 | 77 219 | 75 234 | 74 783 |
| 52 | crystal (0.33)| [athena](https://github.com/athena-framework/athena) (0.8) | 61 754 | 64 691 | 62 915 | 60 582 | 60 600 |
| 53 | javascript (13.7)| [fastify](https://fastify.io) (2.12) | 61 199 | 73 655 | 71 660 | 69 838 | 69 634 |
| 54 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 60 135 | 72 659 | 72 124 | 69 183 | 69 533 |
| 55 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 59 852 | 64 446 | 65 102 | 64 202 | 64 095 |
| 56 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.12) | 59 625 | 67 158 | 67 919 | 64 899 | 65 278 |
| 57 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 56 770 | 61 623 | 63 366 | 64 435 | 64 388 |
| 58 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 54 439 | 60 130 | 60 508 | 58 500 | 58 710 |
| 59 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 54 261 | 66 734 | 64 974 | 63 583 | 62 799 |
| 60 | java (8)| [javalin](https://javalin.io) (3.5) | 54 098 | 83 712 | 82 699 | 80 886 | 81 353 |
| 61 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 53 141 | 63 532 | 62 556 | 60 729 | 60 134 |
| 62 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 51 793 | 78 774 | 79 366 | 77 428 | 79 850 |
| 63 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 51 048 | 53 465 | 53 139 | 52 127 | 51 915 |
| 64 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 49 954 | 52 177 | 52 670 | 51 974 | 51 559 |
| 65 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 49 223 | 57 240 | 57 515 | 55 024 | 55 130 |
| 66 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 49 158 | 52 048 | 51 542 | 51 416 | 50 939 |
| 67 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 48 093 | 52 419 | 52 786 | 52 481 | 51 960 |
| 68 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 48 093 | 51 242 | 48 517 | 51 356 | 50 621 |
| 69 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 47 752 | 58 382 | 57 064 | 56 017 | 55 912 |
| 70 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 47 545 | 55 031 | 55 689 | 55 183 | 55 357 |
| 71 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 47 234 | 81 376 | 84 698 | 83 405 | 83 924 |
| 72 | python (3.8)| [hug](https://hug.rest) (2.6) | 46 815 | 49 115 | 48 764 | 48 429 | 47 719 |
| 73 | python (3.8)| [starlette](https://starlette.io) (0.13) | 46 046 | 50 106 | 50 612 | 48 948 | 48 385 |
| 74 | elixir (1.1)| [cowboy](https://https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 43 857 | 46 688 | 46 412 | 45 316 | 45 023 |
| 75 | javascript (13.7)| [moleculer](https://moleculer.services) (0.14) | 43 337 | 54 813 | 53 228 | 52 570 | 53 062 |
| 76 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 41 680 | 44 370 | 44 580 | 44 095 | 44 247 |
| 77 | ruby (2.6)| [syro](https://github.com/soveran/syro) (3.1) | 40 413 | 42 478 | 41 692 | 40 989 | 41 133 |
| 78 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 40 267 | 43 914 | 44 479 | 44 538 | 43 682 |
| 79 | php (7.4)| [imi](https://imiphp.com) (1.0) | 39 698 | 42 486 | 42 946 | 42 823 | 43 110 |
| 80 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 39 187 | 79 036 | 80 113 | 78 346 | 78 483 |
| 81 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 38 860 | 41 252 | 40 598 | 39 896 | 40 147 |
| 82 | ruby (2.6)| [roda](https://roda.jeremyevans.net) (3.29) | 37 935 | 40 231 | 40 080 | 39 320 | 39 240 |
| 83 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 37 800 | 34 148 | 34 449 | 34 445 | 34 767 |
| 84 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 37 780 | 46 960 | 48 763 | 51 574 | 52 367 |
| 85 | java (8)| [micronaut](https://micronaut.io) (1.2) | 37 265 | 73 709 | 74 098 | 72 392 | 71 345 |
| 86 | php (7.4)| [swoft](https://swoft.org) (2.0) | 36 773 | 38 286 | 38 303 | 37 841 | 37 914 |
| 87 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 35 717 | 39 863 | 39 908 | 36 614 | 36 504 |
| 88 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 35 654 | 47 337 | 46 238 | 46 180 | 46 031 |
| 89 | ruby (2.6)| [cuba](https://cuba.is) (3.9) | 35 347 | 37 107 | 36 352 | 35 908 | 36 134 |
| 90 | javascript (13.7)| [restify](https://restify.com) (8.5) | 34 279 | 43 384 | 43 433 | 42 756 | 42 737 |
| 91 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 33 425 | 35 063 | 34 294 | 34 150 | 34 101 |
| 92 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 33 098 | 60 628 | 65 526 | 65 025 | 64 888 |
| 93 | nim (1.0)| [phoon](https://github.com/ducdetronquito/phoon) (0.1) | 30 112 | 27 014 | 24 777 | 23 176 | 21 007 |
| 94 | ruby (2.6)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 29 535 | 30 188 | 29 774 | 29 915 | 30 156 |
| 95 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.49) | 29 221 | 33 199 | 32 727 | 31 166 | 30 907 |
| 96 | python (3.8)| [responder](https://python-responder.org) (2.0) | 27 753 | 32 565 | 32 111 | 30 381 | 30 073 |
| 97 | ruby (2.6)| [camping](https://github.com/camping/camping) (2.1) | 26 385 | 26 712 | 26 555 | 26 520 | 26 787 |
| 98 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 26 030 | 27 174 | 27 015 | 26 887 | 26 528 |
| 99 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 25 933 | 27 600 | 26 973 | 26 806 | 26 913 |
| 100 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 25 920 | 28 332 | 28 019 | 26 972 | 26 899 |
| 101 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 24 097 | 24 732 | 24 487 | 24 824 | 24 974 |
| 102 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 911 | 22 633 | 20 980 | 21 314 | 20 923 |
| 103 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 22 843 | 24 817 | 24 781 | 24 565 | 24 448 |
| 104 | ruby (2.6)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 22 397 | 22 810 | 22 676 | 22 581 | 22 661 |
| 105 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 21 461 | 61 760 | 67 359 | 66 027 | 66 043 |
| 106 | ruby (2.6)| [plezi](https://github.com/boazsegev/plezi) (0.16) | 21 216 | 212 | 21 491 | 252 | 19 344 |
| 107 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 20 467 | 22 843 | 22 478 | 21 536 | 21 401 |
| 108 | ruby (2.6)| [hanami](https://hanamirb.org) (1.3) | 18 883 | 19 159 | 19 352 | 19 297 | 19 401 |
| 109 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a4) | 18 299 | 22 627 | 22 447 | 21 010 | 21 069 |
| 110 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 17 420 | 17 588 | 17 377 | 17 325 | 17 356 |
| 111 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 126 | 18 301 | 18 585 | 18 342 | 18 339 |
| 112 | ruby (2.6)| [sinatra](https://sinatrarb.com) (2.0) | 16 907 | 17 232 | 17 253 | 17 331 | 17 415 |
| 113 | crystal (0.33)| [lucky](https://luckyframework.org) (0.18) | 14 386 | 14 080 | 12 665 | 13 541 | 12 367 |
| 114 | ruby (2.6)| [grape](https://ruby-grape.org) (1.3) | 13 870 | 13 857 | 13 781 | 13 702 | 13 704 |
| 115 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 12 596 | 12 616 | 11 858 | 11 456 | 11 322 |
| 116 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 811 | 10 906 | 10 807 | 10 718 | 10 677 |
| 117 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 10 487 | 11 044 | 10 586 | 10 450 | 10 452 |
| 118 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 10 110 | 10 206 | 10 118 | 10 057 | 10 045 |
| 119 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 9 372 | 9 566 | 9 522 | 8 870 | 9 385 |
| 120 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 901 | 9 273 | 11 928 | 53 579 | 47 794 |
| 121 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 8 236 | 8 192 | 7 207 | 8 408 | 7 447 |
| 122 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 7 655 | 8 090 | 10 624 | 47 307 | 38 604 |
| 123 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 279 | 7 630 | 10 239 | 47 336 | 44 420 |
| 124 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 170 | 7 635 | 10 384 | 55 114 | 48 131 |
| 125 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 6 899 | 817 | 4 069 | 662 | 3 097 |
| 126 | crystal (0.33)| [onyx](https://onyxframework.org) (0.5) | 5 230 | 5 418 | 5 484 | 5 469 | 5 432 |
| 127 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 436 | 4 703 | 7 780 | 45 941 | 35 951 |
| 128 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 237 | 4 526 | 7 686 | 46 196 | 38 554 |
| 129 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 4 127 | 4 472 | 7 243 | 40 812 | 37 182 |
| 130 | ruby (2.6)| [rails](https://rubyonrails.org) (6.0) | 4 058 | 3 921 | 3 891 | 3 872 | 3 866 |
| 131 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 457 | 3 738 | 7 125 | 41 775 | 31 021 |
| 132 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 451 | 3 722 | 7 167 | 43 453 | 43 212 |
| 133 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 2 845 | 3 777 | 2 035 | 1 969 | 1 933 |
| 134 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 396 | 2 394 | 2 379 | 2 344 | 2 370 |
| 135 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 1 883 | 797 | 8 848 | 38 394 | 35 193 |
| 136 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 1 837 | 1 968 | 5 996 | 42 557 | 37 856 |
| 137 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 522 | 1 558 | 1 540 | 1 520 | 1 523 |
| 138 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 506 | 1 494 | 1 445 | 1 412 | 1 427 |
| 139 | php (7.4)| [laravel](https://laravel.com) (6.15) | 476 | 282 | 10 610 | 27 806 | 22 734 |

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

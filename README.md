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

:information_source:  Updated on **2020-03-07** :information_source:

> Benchmarking with [wrk](https://github.com/ioquatix/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 5s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | javascript (13.7)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.1) | 186 159 | 199 610 | 199 908 | 197 409 | 198 091 |
| 2 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 184 706 | 198 575 | 199 680 | 196 376 | 196 294 |
| 3 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 181 395 | 194 106 | 194 209 | 191 929 | 192 099 |
| 4 | javascript (13.7)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (1.1) | 172 240 | 184 557 | 184 087 | 181 309 | 181 690 |
| 5 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.9) | 165 091 | 174 778 | 178 262 | 174 983 | 174 526 |
| 6 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 159 484 | 168 433 | 172 224 | 168 807 | 168 098 |
| 7 | go (1.14)| [router](https://github.com/fasthttp/router) (0.6) | 159 112 | 168 309 | 171 985 | 168 180 | 167 469 |
| 8 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.4) | 158 864 | 168 091 | 172 129 | 168 750 | 168 314 |
| 9 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 158 243 | 174 201 | 174 970 | 170 266 | 170 125 |
| 10 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 157 540 | 166 250 | 170 063 | 166 463 | 165 960 |
| 11 | crystal (0.33)| [toro](https://github.com/soveran/toro) (0.4) | 156 862 | 165 306 | 163 628 | 154 987 | 154 178 |
| 12 | crystal (0.33)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 155 934 | 164 485 | 162 369 | 153 345 | 152 907 |
| 13 | go (1.14)| [fiber](https://fiber.wiki) (1.8) | 152 432 | 154 623 | 153 760 | 146 425 | 146 703 |
| 14 | crystal (0.33)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 152 018 | 159 903 | 156 614 | 147 983 | 147 670 |
| 15 | crystal (0.33)| [kemal](https://kemalcr.com) (0.26) | 144 670 | 151 150 | 148 519 | 139 436 | 138 856 |
| 16 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 144 556 | 155 926 | 157 040 | 153 004 | 153 324 |
| 17 | crystal (0.33)| [grip](https://github.com/grip-framework/grip) (0.28) | 140 657 | 148 070 | 145 522 | 135 462 | 135 422 |
| 18 | crystal (0.33)| [amber](https://amberframework.org) (0.33) | 138 830 | 145 915 | 141 216 | 133 057 | 133 079 |
| 19 | crystal (0.33)| [lucky](https://luckyframework.org) (0.18) | 137 313 | 142 727 | 138 326 | 128 803 | 128 611 |
| 20 | crystal (0.33)| [orion](https://github.com/obsidian/orion) (2.1) | 133 547 | 137 446 | 133 291 | 122 346 | 121 359 |
| 21 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (3.5) | 133 055 | 142 413 | 144 272 | 142 899 | 142 094 |
| 22 | crystal (0.33)| [athena](https://github.com/athena-framework/athena) (0.8) | 126 099 | 129 298 | 122 635 | 110 289 | 109 837 |
| 23 | java (8)| [act](https://actframework.org) (1.8) | 121 384 | 133 379 | 133 217 | 130 350 | 129 400 |
| 24 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 112 010 | 111 914 | 116 216 | 115 488 | 115 540 |
| 25 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 110 457 | 110 318 | 114 268 | 113 810 | 113 650 |
| 26 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.0) | 106 239 | 105 331 | 109 184 | 109 375 | 108 953 |
| 27 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 105 838 | 109 832 | 113 136 | 112 608 | 112 315 |
| 28 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 104 585 | 105 197 | 108 780 | 108 187 | 108 066 |
| 29 | go (1.14)| [violetear](https://violetear.org) (7.0) | 103 155 | 103 304 | 107 158 | 107 573 | 107 308 |
| 30 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 102 824 | 122 962 | 128 351 | 128 698 | 128 668 |
| 31 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 101 949 | 101 249 | 105 089 | 105 771 | 105 779 |
| 32 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 101 851 | 100 685 | 104 417 | 105 015 | 104 731 |
| 33 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 101 449 | 105 544 | 107 913 | 106 198 | 106 309 |
| 34 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 98 542 | 97 088 | 100 929 | 101 578 | 101 409 |
| 35 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 98 139 | 106 639 | 107 924 | 104 949 | 105 847 |
| 36 | go (1.14)| [gin](https://gin-gonic.com) (1.5) | 97 380 | 100 948 | 104 040 | 104 120 | 103 832 |
| 37 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 96 954 | 96 142 | 100 239 | 100 915 | 100 525 |
| 38 | go (1.14)| [beego](https://beego.me) (1.12) | 96 579 | 101 042 | 105 383 | 105 625 | 105 149 |
| 39 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 95 010 | 104 522 | 104 025 | 99 128 | 98 681 |
| 40 | c (99)| [kore](https://kore.io) (3.3) | 91 638 | 152 777 | 158 656 | 157 547 | 158 167 |
| 41 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 91 563 | 100 902 | 100 288 | 95 630 | 95 564 |
| 42 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.1) | 86 932 | 95 403 | 94 416 | 90 158 | 89 932 |
| 43 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 86 429 | 91 974 | 93 376 | 92 722 | 92 879 |
| 44 | go (1.14)| [air](https://github.com/aofei/air) (0.15) | 84 906 | 89 337 | 91 739 | 92 181 | 91 775 |
| 45 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 84 869 | 141 164 | 186 413 | 186 580 | 181 676 |
| 46 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 84 750 | 90 514 | 88 219 | 85 117 | 85 226 |
| 47 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 84 330 | 88 533 | 86 847 | 83 429 | 83 242 |
| 48 | go (1.14)| [gf](https://goframe.org) (1.11) | 78 918 | 84 231 | 86 349 | 86 530 | 86 240 |
| 49 | java (8)| [javalin](https://javalin.io) (3.5) | 78 129 | 83 392 | 82 256 | 79 735 | 79 933 |
| 50 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 76 742 | 82 942 | 79 450 | 78 263 | 79 090 |
| 51 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 73 771 | 78 124 | 78 163 | 76 601 | 76 659 |
| 52 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 71 216 | 78 460 | 77 511 | 76 011 | 75 767 |
| 53 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 71 128 | 79 443 | 84 439 | 84 813 | 84 309 |
| 54 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 70 571 | 74 166 | 72 641 | 70 444 | 70 450 |
| 55 | javascript (13.7)| [fastify](https://fastify.io) (2.12) | 68 949 | 71 642 | 70 590 | 68 331 | 67 741 |
| 56 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 68 749 | 71 891 | 70 387 | 68 535 | 68 621 |
| 57 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 66 762 | 79 514 | 81 358 | 80 234 | 80 042 |
| 58 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 64 685 | 65 590 | 65 060 | 63 797 | 63 694 |
| 59 | java (8)| [micronaut](https://micronaut.io) (1.2) | 63 690 | 71 680 | 71 932 | 69 635 | 69 598 |
| 60 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 63 037 | 66 680 | 69 791 | 69 948 | 69 645 |
| 61 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 63 011 | 65 209 | 63 445 | 62 193 | 62 190 |
| 62 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 60 396 | 62 687 | 61 169 | 59 383 | 59 195 |
| 63 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.14) | 59 168 | 65 920 | 66 019 | 63 380 | 63 431 |
| 64 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 57 597 | 62 019 | 63 487 | 62 983 | 63 001 |
| 65 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 57 591 | 62 255 | 61 967 | 61 342 | 61 114 |
| 66 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 56 827 | 65 207 | 64 703 | 63 614 | 63 382 |
| 67 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 56 610 | 55 697 | 56 029 | 54 971 | 55 553 |
| 68 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 55 198 | 57 447 | 56 123 | 55 261 | 55 077 |
| 69 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 54 807 | 59 902 | 59 606 | 57 439 | 57 529 |
| 70 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 54 533 | 61 692 | 62 779 | 63 903 | 64 148 |
| 71 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 54 132 | 55 390 | 55 143 | 54 817 | 54 846 |
| 72 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 51 004 | 50 766 | 49 399 | 48 785 | 47 637 |
| 73 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 50 959 | 55 702 | 55 429 | 53 586 | 53 545 |
| 74 | javascript (13.7)| [moleculer](https://moleculer.services) (0.14) | 50 782 | 52 651 | 51 461 | 50 837 | 50 764 |
| 75 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 49 382 | 49 508 | 49 345 | 48 580 | 48 629 |
| 76 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 48 943 | 50 631 | 50 123 | 49 547 | 49 516 |
| 77 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 48 895 | 51 085 | 51 586 | 51 170 | 51 057 |
| 78 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 48 102 | 51 025 | 50 707 | 50 376 | 50 002 |
| 79 | python (3.8)| [hug](https://hug.rest) (2.6) | 47 962 | 50 188 | 50 146 | 49 509 | 49 743 |
| 80 | python (3.8)| [starlette](https://starlette.io) (0.13) | 46 521 | 50 241 | 49 864 | 48 390 | 48 102 |
| 81 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 45 713 | 45 806 | 45 928 | 45 394 | 44 726 |
| 82 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 44 194 | 46 478 | 45 468 | 44 927 | 45 081 |
| 83 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 42 315 | 43 444 | 42 957 | 42 028 | 42 093 |
| 84 | php (7.4)| [imi](https://imiphp.com) (1.0) | 41 943 | 43 700 | 44 270 | 44 187 | 44 102 |
| 85 | javascript (13.7)| [restify](https://restify.com) (8.5) | 41 505 | 43 895 | 42 890 | 42 369 | 42 410 |
| 86 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 41 481 | 43 525 | 41 333 | 40 059 | 40 305 |
| 87 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 41 256 | 48 183 | 51 135 | 53 170 | 51 771 |
| 88 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 40 243 | 43 868 | 43 872 | 43 233 | 43 289 |
| 89 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.29) | 39 793 | 40 399 | 38 862 | 38 408 | 38 456 |
| 90 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 39 758 | 43 745 | 41 785 | 40 904 | 41 264 |
| 91 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a5) | 39 610 | 41 713 | 42 407 | 42 176 | 42 107 |
| 92 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 37 855 | 38 925 | 38 270 | 37 264 | 37 338 |
| 93 | php (7.4)| [swoft](https://swoft.org) (2.0) | 36 683 | 37 516 | 37 303 | 36 819 | 36 819 |
| 94 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 35 073 | 36 216 | 35 135 | 34 541 | 34 149 |
| 95 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 33 738 | 31 943 | 36 074 | 35 884 | 35 854 |
| 96 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 31 980 | 32 846 | 32 357 | 31 581 | 31 580 |
| 97 | python (3.8)| [responder](https://python-responder.org) (2.0) | 29 998 | 32 086 | 31 744 | 30 612 | 30 609 |
| 98 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 29 812 | 29 683 | 29 120 | 29 093 | 29 244 |
| 99 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.52) | 29 195 | 31 753 | 31 297 | 30 007 | 30 196 |
| 100 | dart (2.7)| [aqueduct](https://aqueduct.io) (3.2) | 26 678 | 25 671 | 26 503 | 25 260 | 24 655 |
| 101 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 26 394 | 27 699 | 27 583 | 27 170 | 27 019 |
| 102 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 26 385 | 27 266 | 27 216 | 26 772 | 26 665 |
| 103 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 26 193 | 26 197 | 25 634 | 25 661 | 25 923 |
| 104 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 26 063 | 28 084 | 28 036 | 27 118 | 27 144 |
| 105 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 24 112 | 24 976 | 24 681 | 24 285 | 24 573 |
| 106 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 23 242 | 22 117 | 20 963 | 20 613 | 20 472 |
| 107 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 21 640 | 21 832 | 21 203 | 20 491 | 20 479 |
| 108 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 20 588 | 22 386 | 22 823 | 23 261 | 23 065 |
| 109 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 18 281 | 18 425 | 18 388 | 18 453 | 18 186 |
| 110 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 460 | 17 988 | 18 099 | 18 036 | 18 016 |
| 111 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 16 172 | 15 801 | 15 817 | 15 838 | 15 825 |
| 112 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 15 190 | 15 675 | 15 642 | 15 625 | 15 645 |
| 113 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 14 113 | 13 920 | 13 891 | 13 843 | 13 721 |
| 114 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 13 457 | 13 238 | 13 213 | 13 089 | 13 027 |
| 115 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 12 570 | 12 472 | 12 416 | 12 460 | 12 412 |
| 116 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 403 | 11 401 | 11 396 | 11 442 | 11 494 |
| 117 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 11 302 | 11 665 | 11 021 | 10 582 | 10 598 |
| 118 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 758 | 10 296 | 10 303 | 10 261 | 10 130 |
| 119 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 319 | 9 255 | 9 188 | 53 270 | 51 967 |
| 120 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 8 886 | 9 381 | 9 395 | 9 265 | 9 315 |
| 121 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 8 038 | 7 994 | 8 021 | 45 226 | 45 725 |
| 122 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 7 803 | 10 584 | 10 349 | 10 256 | 10 140 |
| 123 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 646 | 7 608 | 7 648 | 51 499 | 51 616 |
| 124 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 617 | 7 620 | 7 546 | 45 775 | 44 739 |
| 125 | crystal (0.33)| [onyx](https://onyxframework.org) (0.5) | 4 765 | 4 930 | 4 911 | 4 911 | 4 885 |
| 126 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 710 | 4 697 | 4 786 | 44 473 | 42 869 |
| 127 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 468 | 4 526 | 4 527 | 44 755 | 42 682 |
| 128 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 4 415 | 4 420 | 4 451 | 44 193 | 39 289 |
| 129 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 4 228 | 3 996 | 3 980 | 3 957 | 3 954 |
| 130 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 691 | 3 689 | 3 734 | 44 407 | 40 210 |
| 131 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 677 | 3 692 | 3 725 | 43 950 | 40 511 |
| 132 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 429 | 2 448 | 2 418 | 2 416 | 2 399 |
| 133 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 1 937 | 1 973 | 1 992 | 41 308 | 39 297 |
| 134 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 1 876 | 5 337 | 4 201 | 3 329 | 2 053 |
| 135 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 571 | 1 599 | 1 573 | 1 566 | 1 556 |
| 136 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 549 | 1 521 | 1 506 | 1 472 | 1 478 |
| 137 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 1 047 | 1 607 | 1 552 | 1 483 | 1 012 |
| 138 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 488 | 488 | 870 | 34 539 | 37 113 |
| 139 | php (7.4)| [laravel](https://laravel.com) (7.0) | 260 | 135 | 2 797 | 23 664 | 22 426 |

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

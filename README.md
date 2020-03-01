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

:information_source:  Updated on **2020-03-01** :information_source:

> Benchmarking with [wrk](https://github.com/ioquatix/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 5s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 170 205 | 188 404 | 190 567 | 187 687 | 188 578 |
| 2 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 164 612 | 187 847 | 187 847 | 183 331 | 184 205 |
| 3 | javascript (13.7)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 157 984 | 177 555 | 178 720 | 174 437 | 174 091 |
| 4 | go (1.14)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.9) | 153 020 | 167 800 | 170 852 | 165 841 | 164 165 |
| 5 | go (1.14)| [router](https://github.com/fasthttp/router) (0.6) | 147 680 | 162 039 | 165 405 | 161 013 | 160 362 |
| 6 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.4) | 146 946 | 161 442 | 164 751 | 160 666 | 160 012 |
| 7 | go (1.14)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 146 897 | 161 167 | 164 997 | 160 763 | 160 454 |
| 8 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 145 759 | 158 811 | 162 635 | 157 545 | 156 603 |
| 9 | crystal (0.33)| [toro](https://github.com/soveran/toro) (0.4) | 144 130 | 159 402 | 157 474 | 150 038 | 146 934 |
| 10 | crystal (0.33)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 142 645 | 156 916 | 156 122 | 149 126 | 148 522 |
| 11 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 140 864 | 166 805 | 170 333 | 163 959 | 165 515 |
| 12 | go (1.14)| [fiber](https://fiber.wiki) (1.8) | 140 146 | 149 847 | 147 838 | 139 181 | 139 344 |
| 13 | crystal (0.33)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 139 274 | 153 637 | 152 615 | 144 134 | 144 322 |
| 14 | crystal (0.33)| [grip](https://github.com/grip-framework/grip) (0.27) | 133 796 | 146 806 | 143 948 | 135 784 | 136 488 |
| 15 | crystal (0.33)| [kemal](https://kemalcr.com) (0.26) | 133 080 | 146 317 | 144 226 | 133 778 | 134 076 |
| 16 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 131 719 | 146 964 | 149 782 | 145 604 | 145 723 |
| 17 | crystal (0.33)| [amber](https://amberframework.org) (0.33) | 127 705 | 137 744 | 134 003 | 126 761 | 126 562 |
| 18 | crystal (0.33)| [orion](https://github.com/obsidian/orion) (2.1) | 121 936 | 132 405 | 128 763 | 119 605 | 120 645 |
| 19 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (3.5) | 121 592 | 135 894 | 139 858 | 138 038 | 136 823 |
| 20 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 119 165 | 173 139 | 182 669 | 180 633 | 177 142 |
| 21 | crystal (0.33)| [athena](https://github.com/athena-framework/athena) (0.8) | 115 265 | 123 187 | 117 230 | 106 744 | 108 733 |
| 22 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 106 495 | 106 231 | 110 971 | 110 505 | 109 635 |
| 23 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 104 970 | 105 604 | 109 899 | 109 821 | 109 329 |
| 24 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.0) | 101 452 | 100 686 | 104 559 | 104 799 | 104 378 |
| 25 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 101 058 | 105 305 | 108 371 | 107 588 | 107 537 |
| 26 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 98 987 | 99 934 | 102 755 | 102 366 | 102 021 |
| 27 | go (1.14)| [violetear](https://violetear.org) (7.0) | 98 376 | 98 236 | 102 648 | 102 772 | 102 690 |
| 28 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 97 516 | 95 775 | 99 676 | 100 561 | 100 243 |
| 29 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 96 926 | 94 889 | 99 075 | 99 937 | 99 116 |
| 30 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 95 332 | 118 071 | 123 027 | 122 894 | 123 086 |
| 31 | go (1.14)| [gin](https://gin-gonic.com) (1.5) | 94 094 | 95 952 | 98 365 | 98 326 | 98 103 |
| 32 | go (1.14)| [beego](https://beego.me) (1.12) | 94 075 | 96 738 | 100 221 | 100 909 | 100 811 |
| 33 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 93 996 | 92 390 | 95 889 | 96 049 | 96 014 |
| 34 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 93 746 | 100 497 | 102 684 | 100 647 | 101 044 |
| 35 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 92 680 | 91 306 | 94 942 | 96 104 | 96 325 |
| 36 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 88 956 | 102 526 | 103 607 | 101 913 | 101 761 |
| 37 | java (8)| [act](https://actframework.org) (1.8) | 85 654 | 129 302 | 131 834 | 128 192 | 126 915 |
| 38 | c (99)| [kore](https://kore.io) (3.3) | 84 384 | 151 168 | 151 500 | 152 965 | 145 534 |
| 39 | go (1.14)| [air](https://github.com/aofei/air) (0.15) | 82 113 | 84 598 | 87 492 | 87 369 | 87 546 |
| 40 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 79 698 | 100 483 | 100 481 | 95 185 | 95 641 |
| 41 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 78 679 | 89 188 | 90 985 | 90 024 | 89 519 |
| 42 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 76 276 | 96 306 | 96 097 | 91 997 | 91 350 |
| 43 | go (1.14)| [gf](https://goframe.org) (1.11) | 73 065 | 80 531 | 82 136 | 82 231 | 81 823 |
| 44 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 70 676 | 85 769 | 84 875 | 81 487 | 81 430 |
| 45 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.1) | 70 493 | 90 871 | 90 473 | 86 549 | 86 469 |
| 46 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 70 469 | 79 004 | 83 476 | 83 317 | 83 037 |
| 47 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 68 541 | 83 661 | 81 458 | 75 104 | 78 022 |
| 48 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 68 435 | 76 153 | 76 617 | 75 668 | 74 834 |
| 49 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 64 193 | 62 996 | 66 791 | 66 989 | 66 737 |
| 50 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 60 615 | 63 951 | 63 956 | 62 571 | 62 429 |
| 51 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 58 923 | 74 542 | 73 207 | 70 454 | 69 842 |
| 52 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 57 543 | 60 800 | 61 789 | 61 162 | 61 243 |
| 53 | javascript (13.7)| [fastify](https://fastify.io) (2.12) | 56 720 | 69 529 | 66 650 | 65 309 | 66 089 |
| 54 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 55 892 | 68 806 | 67 360 | 65 670 | 65 433 |
| 55 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.12) | 55 316 | 62 610 | 62 753 | 60 532 | 59 951 |
| 56 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 54 850 | 61 081 | 61 505 | 60 921 | 60 688 |
| 57 | java (8)| [javalin](https://javalin.io) (3.5) | 52 639 | 79 284 | 77 299 | 75 412 | 76 649 |
| 58 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 50 234 | 62 356 | 60 200 | 59 088 | 59 574 |
| 59 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 49 648 | 54 719 | 57 450 | 55 167 | 55 209 |
| 60 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 49 303 | 51 353 | 51 517 | 50 012 | 50 366 |
| 61 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 48 863 | 60 006 | 58 973 | 57 122 | 56 794 |
| 62 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 48 012 | 53 896 | 53 521 | 51 648 | 51 256 |
| 63 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 47 993 | 50 572 | 50 050 | 49 492 | 49 589 |
| 64 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 46 865 | 50 177 | 49 900 | 48 813 | 48 970 |
| 65 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 46 343 | 77 314 | 79 520 | 79 475 | 79 935 |
| 66 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 46 272 | 71 971 | 69 553 | 70 206 | 73 558 |
| 67 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 45 489 | 53 303 | 53 611 | 52 654 | 53 035 |
| 68 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 45 369 | 49 587 | 50 024 | 49 191 | 49 407 |
| 69 | python (3.8)| [hug](https://hug.rest) (2.6) | 44 676 | 47 664 | 48 319 | 47 954 | 47 751 |
| 70 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 44 431 | 49 474 | 49 528 | 48 918 | 48 772 |
| 71 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 43 716 | 55 341 | 53 774 | 52 804 | 52 836 |
| 72 | python (3.8)| [starlette](https://starlette.io) (0.13) | 43 024 | 48 475 | 48 042 | 46 183 | 46 255 |
| 73 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 42 534 | 34 476 | 34 320 | 34 372 | 34 104 |
| 74 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 41 068 | 44 335 | 45 207 | 45 268 | 44 324 |
| 75 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 40 592 | 47 953 | 50 357 | 50 710 | 51 126 |
| 76 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 40 087 | 42 738 | 42 741 | 41 260 | 41 343 |
| 77 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 39 624 | 42 779 | 42 206 | 42 049 | 41 877 |
| 78 | javascript (13.7)| [moleculer](https://moleculer.services) (0.14) | 39 539 | 50 794 | 50 058 | 49 181 | 49 609 |
| 79 | php (7.4)| [imi](https://imiphp.com) (1.0) | 39 347 | 42 762 | 42 679 | 42 648 | 42 096 |
| 80 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 38 842 | 41 064 | 39 520 | 39 665 | 39 595 |
| 81 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 38 321 | 40 553 | 39 446 | 38 040 | 38 496 |
| 82 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 35 821 | 38 011 | 37 279 | 36 498 | 36 347 |
| 83 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a5) | 35 207 | 42 011 | 41 439 | 39 959 | 39 388 |
| 84 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.29) | 35 187 | 38 423 | 36 968 | 36 487 | 36 779 |
| 85 | php (7.4)| [swoft](https://swoft.org) (2.0) | 34 791 | 36 540 | 36 389 | 35 846 | 35 818 |
| 86 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 32 973 | 43 732 | 43 345 | 42 961 | 42 879 |
| 87 | javascript (13.7)| [restify](https://restify.com) (8.5) | 32 817 | 41 533 | 40 379 | 39 991 | 39 954 |
| 88 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 32 303 | 33 892 | 33 338 | 32 982 | 33 209 |
| 89 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 31 000 | 32 309 | 31 971 | 31 206 | 31 171 |
| 90 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 30 444 | 70 900 | 73 904 | 72 636 | 70 864 |
| 91 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 29 598 | 56 858 | 58 818 | 58 829 | 57 847 |
| 92 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.49) | 28 934 | 31 231 | 30 952 | 29 395 | 29 612 |
| 93 | python (3.8)| [responder](https://python-responder.org) (2.0) | 28 183 | 30 918 | 30 720 | 29 311 | 29 587 |
| 94 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 27 953 | 32 867 | 34 413 | 34 737 | 35 089 |
| 95 | crystal (0.33)| [lucky](https://luckyframework.org) (0.18) | 27 778 | 26 372 | 25 383 | 21 860 | 22 343 |
| 96 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 27 253 | 28 010 | 28 072 | 27 950 | 28 147 |
| 97 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 24 348 | 24 543 | 24 274 | 24 406 | 24 635 |
| 98 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 24 204 | 26 035 | 26 164 | 25 931 | 25 850 |
| 99 | dart (2.7)| [aqueduct](https://aqueduct.io) (3.2) | 23 754 | 27 096 | 26 583 | 24 744 | 26 197 |
| 100 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 23 510 | 24 686 | 24 599 | 24 434 | 24 172 |
| 101 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 22 704 | 24 593 | 24 231 | 24 180 | 24 150 |
| 102 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 22 668 | 24 231 | 23 963 | 23 387 | 23 710 |
| 103 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 22 572 | 24 531 | 24 042 | 23 196 | 23 035 |
| 104 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 553 | 21 900 | 20 345 | 20 655 | 20 548 |
| 105 | java (8)| [micronaut](https://micronaut.io) (1.2) | 21 994 | 67 166 | 69 461 | 67 154 | 66 906 |
| 106 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 20 211 | 21 030 | 20 798 | 20 222 | 20 283 |
| 107 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 17 458 | 54 569 | 61 970 | 60 695 | 60 714 |
| 108 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 17 251 | 17 462 | 17 285 | 17 332 | 17 429 |
| 109 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 16 031 | 17 190 | 17 361 | 17 236 | 17 397 |
| 110 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 15 265 | 15 704 | 15 794 | 15 804 | 15 861 |
| 111 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 14 838 | 15 321 | 15 385 | 15 439 | 15 389 |
| 112 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 12 792 | 12 809 | 12 627 | 12 658 | 12 490 |
| 113 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 12 547 | 12 880 | 13 009 | 13 004 | 13 159 |
| 114 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 12 077 | 12 180 | 11 447 | 10 724 | 10 795 |
| 115 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 11 554 | 11 618 | 11 587 | 11 593 | 11 556 |
| 116 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 029 | 10 588 | 10 445 | 10 396 | 10 245 |
| 117 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 876 | 10 568 | 10 322 | 10 088 | 10 167 |
| 118 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 8 738 | 9 112 | 9 009 | 8 893 | 8 601 |
| 119 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 409 | 8 779 | 11 253 | 51 310 | 41 635 |
| 120 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 7 672 | 7 605 | 8 177 | 7 993 | 8 010 |
| 121 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 7 405 | 7 487 | 9 916 | 44 041 | 34 536 |
| 122 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 7 335 | 1 964 | 2 895 | 2 193 | 3 287 |
| 123 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 034 | 7 261 | 9 736 | 41 524 | 37 085 |
| 124 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 007 | 7 362 | 10 547 | 51 828 | 42 897 |
| 125 | crystal (0.33)| [onyx](https://onyxframework.org) (0.5) | 4 751 | 4 963 | 4 988 | 5 001 | 4 983 |
| 126 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 306 | 4 523 | 7 742 | 39 732 | 36 398 |
| 127 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 3 958 | 4 319 | 7 367 | 41 968 | 32 215 |
| 128 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 3 922 | 4 261 | 7 203 | 42 384 | 36 639 |
| 129 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 878 | 3 729 | 3 751 | 3 703 | 3 625 |
| 130 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 296 | 3 558 | 6 673 | 41 477 | 40 564 |
| 131 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 186 | 3 568 | 6 904 | 42 291 | 35 724 |
| 132 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 2 787 | 3 803 | 1 963 | 1 794 | 1 788 |
| 133 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 329 | 2 365 | 2 336 | 2 315 | 2 281 |
| 134 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 1 795 | 934 | 6 351 | 36 714 | 35 015 |
| 135 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 1 794 | 1 861 | 5 790 | 39 761 | 32 192 |
| 136 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 531 | 1 559 | 1 526 | 1 518 | 1 520 |
| 137 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 511 | 1 488 | 1 455 | 1 380 | 1 408 |
| 138 | php (7.4)| [laravel](https://laravel.com) (6.17) | 465 | 297 | 9 443 | 24 238 | 17 373 |

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

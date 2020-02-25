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

:information_source:  Updated on **2020-02-25** :information_source:

> Benchmarking with [wrk](https://github.com/ioquatix/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 5s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 171 196 | 191 260 | 194 434 | 191 569 | 191 869 |
| 2 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 166 775 | 186 351 | 189 638 | 185 784 | 186 342 |
| 3 | javascript (13.7)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 162 801 | 183 704 | 187 419 | 181 887 | 183 332 |
| 4 | go (1.13)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.9) | 148 985 | 164 895 | 168 832 | 163 745 | 162 286 |
| 5 | go (1.13)| [fiber](https://fiber.wiki) (1.7) | 148 439 | 167 884 | 168 946 | 163 691 | 162 730 |
| 6 | go (1.13)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 144 563 | 159 391 | 163 090 | 158 060 | 155 947 |
| 7 | go (1.13)| [router](https://github.com/fasthttp/router) (0.6) | 144 022 | 158 987 | 162 590 | 158 874 | 158 465 |
| 8 | go (1.13)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.3) | 142 253 | 157 380 | 161 409 | 157 151 | 156 723 |
| 9 | go (1.13)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 141 799 | 156 302 | 160 070 | 156 619 | 155 719 |
| 10 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 139 766 | 165 892 | 166 939 | 162 244 | 162 599 |
| 11 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 130 251 | 146 725 | 149 628 | 145 775 | 144 755 |
| 12 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (3.5) | 120 655 | 135 768 | 138 382 | 137 999 | 136 038 |
| 13 | go (1.13)| [rte](https://github.com/jwilner/rte) (0.0) | 103 974 | 104 800 | 110 241 | 109 564 | 108 739 |
| 14 | go (1.13)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 102 428 | 102 942 | 107 313 | 107 173 | 107 139 |
| 15 | go (1.13)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 98 730 | 103 584 | 107 170 | 106 381 | 106 160 |
| 16 | go (1.13)| [chi](https://github.com/go-chi/chi) (4.0) | 98 633 | 98 102 | 101 949 | 102 274 | 101 809 |
| 17 | go (1.13)| [aero](https://github.com/aerogo/aero) (1.3) | 95 652 | 97 434 | 102 763 | 101 990 | 101 858 |
| 18 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 95 325 | 116 158 | 119 408 | 119 766 | 119 789 |
| 19 | go (1.13)| [echo](https://echo.labstack.com) (4.1) | 95 121 | 93 518 | 98 314 | 98 189 | 97 176 |
| 20 | go (1.13)| [goroute](https://goroute.github.io) (0.0) | 94 758 | 92 963 | 97 787 | 98 144 | 97 961 |
| 21 | crystal (0.33)| [toro](https://github.com/soveran/toro) (0.4) | 94 052 | 102 240 | 100 338 | 95 558 | 95 156 |
| 22 | go (1.13)| [violetear](https://violetear.org) (7.0) | 93 811 | 93 903 | 99 465 | 98 056 | 97 529 |
| 23 | go (1.13)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 93 615 | 91 491 | 96 193 | 95 551 | 94 856 |
| 24 | go (1.13)| [beego](https://beego.me) (1.12) | 91 843 | 93 769 | 98 715 | 98 546 | 98 285 |
| 25 | go (1.13)| [kami](https://github.com/guregu/kami) (2.2) | 91 622 | 98 947 | 101 588 | 99 936 | 99 350 |
| 26 | crystal (0.33)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 91 576 | 99 146 | 97 524 | 93 299 | 93 806 |
| 27 | go (1.13)| [gin](https://gin-gonic.com) (1.5) | 91 260 | 93 396 | 97 576 | 97 274 | 96 698 |
| 28 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 89 104 | 102 425 | 103 738 | 100 995 | 102 023 |
| 29 | go (1.13)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 88 796 | 87 895 | 92 857 | 93 126 | 91 551 |
| 30 | c (99)| [kore](https://kore.io) (3.3) | 88 014 | 149 651 | 146 373 | 148 800 | 143 629 |
| 31 | crystal (0.33)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 87 565 | 96 000 | 94 392 | 90 554 | 89 804 |
| 32 | java (8)| [act](https://actframework.org) (1.8) | 87 311 | 126 411 | 127 520 | 124 135 | 123 572 |
| 33 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 85 208 | 104 798 | 104 831 | 101 147 | 100 284 |
| 34 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 82 890 | 138 848 | 180 078 | 177 864 | 172 119 |
| 35 | go (1.13)| [air](https://github.com/aofei/air) (0.14) | 80 944 | 82 256 | 85 197 | 84 646 | 84 676 |
| 36 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 80 303 | 99 890 | 100 653 | 96 217 | 96 773 |
| 37 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 79 471 | 88 575 | 91 436 | 90 215 | 88 868 |
| 38 | crystal (0.33)| [kemal](https://kemalcr.com) (0.26) | 79 073 | 85 352 | 83 620 | 80 278 | 80 447 |
| 39 | crystal (0.33)| [grip](https://github.com/grkek/grip) (0.27) | 75 544 | 81 653 | 78 968 | 76 860 | 76 663 |
| 40 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 74 378 | 82 078 | 87 537 | 88 028 | 86 918 |
| 41 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 73 873 | 88 228 | 86 705 | 83 623 | 83 676 |
| 42 | crystal (0.33)| [amber](https://amberframework.org) (0.33) | 72 553 | 77 527 | 75 685 | 72 611 | 73 325 |
| 43 | go (1.13)| [gf](https://goframe.org) (1.11) | 70 554 | 77 512 | 79 224 | 79 464 | 78 913 |
| 44 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 69 902 | 89 287 | 81 590 | 84 798 | 85 575 |
| 45 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 69 464 | 76 228 | 76 094 | 75 187 | 75 405 |
| 46 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.0) | 69 141 | 89 741 | 88 589 | 84 918 | 84 394 |
| 47 | crystal (0.33)| [orion](https://github.com/obsidian/orion) (2.1) | 68 820 | 73 000 | 69 895 | 67 657 | 68 335 |
| 48 | go (1.13)| [mars](https://github.com/roblillack/mars) (1.0) | 62 318 | 61 461 | 64 834 | 65 185 | 65 161 |
| 49 | javascript (13.7)| [fastify](https://fastify.io) (2.12) | 60 606 | 72 828 | 71 035 | 69 290 | 69 665 |
| 50 | elixir (1.1)| [cowboy_stream](https://https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 60 467 | 64 146 | 64 032 | 62 981 | 63 032 |
| 51 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 59 732 | 72 432 | 71 000 | 68 561 | 68 643 |
| 52 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 59 376 | 75 088 | 73 141 | 71 310 | 71 395 |
| 53 | crystal (0.33)| [athena](https://github.com/athena-framework/athena) (0.8) | 59 211 | 62 993 | 61 242 | 58 661 | 59 778 |
| 54 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 57 157 | 62 297 | 61 853 | 61 029 | 60 566 |
| 55 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 55 262 | 61 015 | 60 899 | 60 940 | 60 801 |
| 56 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.12) | 53 608 | 64 928 | 63 305 | 60 415 | 58 960 |
| 57 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 52 783 | 65 551 | 60 194 | 62 703 | 62 101 |
| 58 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 51 136 | 57 009 | 57 259 | 55 304 | 55 293 |
| 59 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 50 944 | 61 330 | 60 288 | 57 681 | 55 257 |
| 60 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 49 923 | 51 948 | 49 930 | 49 935 | 50 660 |
| 61 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 49 510 | 52 313 | 52 225 | 51 316 | 51 367 |
| 62 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 48 775 | 51 559 | 51 108 | 50 678 | 50 963 |
| 63 | java (8)| [javalin](https://javalin.io) (3.5) | 48 433 | 80 115 | 76 607 | 76 588 | 76 520 |
| 64 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 47 325 | 53 348 | 52 821 | 50 799 | 50 654 |
| 65 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 46 556 | 72 744 | 70 262 | 71 615 | 72 968 |
| 66 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 45 759 | 49 077 | 49 678 | 49 004 | 49 054 |
| 67 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 45 329 | 49 584 | 49 216 | 48 737 | 47 986 |
| 68 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 827 | 53 604 | 55 313 | 55 087 | 55 399 |
| 69 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 44 309 | 76 762 | 79 281 | 78 014 | 79 450 |
| 70 | python (3.8)| [hug](https://hug.rest) (2.6) | 44 186 | 48 305 | 47 768 | 47 438 | 47 579 |
| 71 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 43 691 | 54 862 | 53 826 | 53 260 | 52 894 |
| 72 | javascript (13.7)| [moleculer](https://moleculer.services) (0.14) | 42 329 | 53 946 | 53 166 | 52 639 | 52 649 |
| 73 | python (3.8)| [starlette](https://starlette.io) (0.13) | 41 194 | 48 334 | 47 594 | 46 026 | 46 019 |
| 74 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 40 912 | 43 999 | 44 444 | 45 010 | 44 401 |
| 75 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 40 552 | 48 942 | 50 473 | 51 555 | 51 935 |
| 76 | elixir (1.1)| [cowboy](https://https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 40 016 | 42 690 | 42 369 | 41 480 | 41 591 |
| 77 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 39 456 | 42 412 | 42 200 | 41 811 | 41 774 |
| 78 | php (7.4)| [imi](https://imiphp.com) (1.0) | 38 677 | 43 020 | 42 901 | 42 615 | 42 712 |
| 79 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.1) | 37 900 | 40 214 | 38 894 | 38 556 | 38 407 |
| 80 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 37 535 | 39 908 | 38 870 | 38 008 | 38 549 |
| 81 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.29) | 36 130 | 38 627 | 37 892 | 37 175 | 37 382 |
| 82 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 35 929 | 38 159 | 37 850 | 36 959 | 36 674 |
| 83 | php (7.4)| [swoft](https://swoft.org) (2.0) | 34 966 | 36 822 | 36 540 | 36 384 | 35 810 |
| 84 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 34 901 | 45 677 | 45 595 | 45 148 | 45 250 |
| 85 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 33 922 | 31 514 | 31 380 | 31 181 | 35 524 |
| 86 | javascript (13.7)| [restify](https://restify.com) (8.5) | 33 790 | 43 048 | 42 356 | 41 944 | 42 047 |
| 87 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 33 479 | 34 673 | 34 064 | 33 803 | 34 146 |
| 88 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 31 196 | 71 556 | 74 712 | 73 013 | 73 162 |
| 89 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 30 543 | 32 014 | 31 603 | 30 894 | 31 321 |
| 90 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 30 525 | 55 951 | 57 269 | 57 902 | 58 640 |
| 91 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 29 793 | 34 486 | 36 071 | 35 862 | 33 132 |
| 92 | python (3.8)| [responder](https://python-responder.org) (2.0) | 28 749 | 30 502 | 30 426 | 28 862 | 28 800 |
| 93 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.49) | 28 538 | 30 998 | 30 549 | 29 276 | 29 039 |
| 94 | java (8)| [micronaut](https://micronaut.io) (1.2) | 28 172 | 67 650 | 68 252 | 66 240 | 66 508 |
| 95 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 27 233 | 28 124 | 28 024 | 27 905 | 27 945 |
| 96 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 25 818 | 27 243 | 27 054 | 26 519 | 26 389 |
| 97 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 25 057 | 25 226 | 25 161 | 25 044 | 25 219 |
| 98 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 24 943 | 27 264 | 27 234 | 26 285 | 26 217 |
| 99 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 747 | 24 774 | 24 693 | 24 273 | 24 438 |
| 100 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 23 042 | 22 514 | 21 048 | 20 974 | 20 793 |
| 101 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 22 596 | 23 696 | 24 020 | 23 473 | 23 440 |
| 102 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 22 569 | 24 175 | 23 993 | 23 782 | 23 638 |
| 103 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 20 310 | 20 853 | 20 755 | 20 030 | 19 886 |
| 104 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a5) | 19 747 | 20 996 | 20 198 | 18 716 | 19 072 |
| 105 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 18 549 | 56 581 | 63 227 | 62 870 | 62 294 |
| 106 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 17 548 | 17 468 | 17 234 | 17 212 | 17 207 |
| 107 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 15 765 | 16 050 | 16 137 | 16 227 | 16 214 |
| 108 | crystal (0.33)| [lucky](https://luckyframework.org) (0.18) | 13 762 | 13 580 | 12 162 | 12 750 | 12 885 |
| 109 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 12 626 | 12 729 | 12 824 | 12 698 | 12 596 |
| 110 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 11 967 | 11 852 | 11 710 | 11 640 | 11 890 |
| 111 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 11 546 | 11 962 | 11 908 | 11 910 | 11 911 |
| 112 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 11 415 | 12 221 | 11 232 | 10 594 | 10 455 |
| 113 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 10 169 | 10 519 | 10 168 | 10 096 | 9 849 |
| 114 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 985 | 10 473 | 10 380 | 10 177 | 10 270 |
| 115 | go (1.13)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 740 | 9 820 | 9 772 | 9 777 | 9 807 |
| 116 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 9 002 | 9 135 | 8 950 | 8 932 | 8 977 |
| 117 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 381 | 8 909 | 11 358 | 49 177 | 45 189 |
| 118 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 8 128 | 8 223 | 7 964 | 8 275 | 8 053 |
| 119 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 7 426 | 7 624 | 9 528 | 36 607 | 35 401 |
| 120 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 7 349 | 1 895 | 1 659 | 964 | 1 562 |
| 121 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 006 | 7 320 | 9 524 | 48 225 | 46 020 |
| 122 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 6 867 | 7 287 | 9 952 | 42 445 | 30 984 |
| 123 | crystal (0.33)| [onyx](https://onyxframework.org) (0.5) | 5 537 | 5 938 | 5 916 | 5 901 | 5 947 |
| 124 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 310 | 4 501 | 7 272 | 42 725 | 35 255 |
| 125 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 135 | 4 492 | 6 646 | 42 821 | 36 616 |
| 126 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 930 | 3 781 | 3 784 | 3 741 | 3 736 |
| 127 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 3 830 | 4 080 | 7 042 | 43 000 | 31 087 |
| 128 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 316 | 3 623 | 6 481 | 42 612 | 34 709 |
| 129 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 249 | 3 517 | 6 424 | 39 812 | 31 439 |
| 130 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 2 793 | 3 975 | 1 871 | 1 821 | 1 824 |
| 131 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 356 | 2 380 | 2 331 | 2 300 | 2 302 |
| 132 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 1 870 | 853 | 8 179 | 37 303 | 28 496 |
| 133 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 1 754 | 1 870 | 5 649 | 40 316 | 33 658 |
| 134 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 508 | 1 548 | 1 530 | 1 493 | 1 511 |
| 135 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 501 | 1 478 | 1 444 | 1 430 | 1 408 |
| 136 | php (7.4)| [laravel](https://laravel.com) (6.17) | 455 | 248 | 9 708 | 24 733 | 23 577 |

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

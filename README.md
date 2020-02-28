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

:information_source:  Updated on **2020-02-28** :information_source:

> Benchmarking with [wrk](https://github.com/ioquatix/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 5s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 169 852 | 190 198 | 193 804 | 190 235 | 189 281 |
| 2 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 166 886 | 185 480 | 189 656 | 184 789 | 185 460 |
| 3 | javascript (13.7)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 157 377 | 177 674 | 180 717 | 175 059 | 175 040 |
| 4 | go (1.14)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.9) | 154 772 | 171 337 | 175 852 | 171 568 | 170 936 |
| 5 | go (1.14)| [router](https://github.com/fasthttp/router) (0.6) | 149 616 | 165 236 | 169 410 | 165 213 | 165 439 |
| 6 | go (1.14)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 148 911 | 164 890 | 169 091 | 164 478 | 163 658 |
| 7 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.3) | 148 378 | 163 003 | 167 673 | 164 498 | 163 000 |
| 8 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 147 756 | 163 331 | 168 098 | 163 962 | 163 218 |
| 9 | crystal (0.33)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 145 841 | 160 880 | 160 039 | 153 026 | 152 347 |
| 10 | crystal (0.33)| [toro](https://github.com/soveran/toro) (0.4) | 145 654 | 161 632 | 160 000 | 153 386 | 152 044 |
| 11 | go (1.14)| [fiber](https://fiber.wiki) (1.8) | 144 734 | 152 410 | 151 838 | 144 341 | 143 913 |
| 12 | crystal (0.33)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 143 526 | 157 369 | 156 381 | 149 578 | 148 369 |
| 13 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 140 616 | 168 798 | 171 439 | 167 036 | 168 825 |
| 14 | crystal (0.33)| [kemal](https://kemalcr.com) (0.26) | 137 725 | 149 926 | 148 852 | 142 174 | 142 653 |
| 15 | crystal (0.33)| [grip](https://github.com/grip-framework/grip) (0.27) | 131 687 | 145 844 | 144 716 | 135 859 | 137 997 |
| 16 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 131 416 | 148 712 | 151 838 | 147 001 | 147 978 |
| 17 | crystal (0.33)| [amber](https://amberframework.org) (0.33) | 130 859 | 142 848 | 140 443 | 131 620 | 130 301 |
| 18 | crystal (0.33)| [orion](https://github.com/obsidian/orion) (2.1) | 123 111 | 135 884 | 130 575 | 121 456 | 121 237 |
| 19 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (3.5) | 122 068 | 138 024 | 140 848 | 140 335 | 137 715 |
| 20 | crystal (0.33)| [athena](https://github.com/athena-framework/athena) (0.8) | 117 126 | 127 915 | 120 764 | 110 295 | 110 759 |
| 21 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 108 445 | 109 511 | 114 429 | 114 350 | 113 759 |
| 22 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 106 846 | 108 282 | 112 459 | 112 675 | 112 044 |
| 23 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.0) | 103 077 | 102 799 | 107 549 | 108 058 | 107 605 |
| 24 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 102 845 | 107 662 | 111 433 | 111 341 | 110 818 |
| 25 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 101 625 | 134 464 | 186 306 | 184 963 | 181 641 |
| 26 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 101 600 | 103 227 | 106 846 | 106 725 | 106 523 |
| 27 | go (1.14)| [violetear](https://violetear.org) (7.0) | 100 190 | 100 900 | 105 078 | 106 146 | 105 886 |
| 28 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 99 871 | 98 611 | 103 261 | 103 924 | 103 545 |
| 29 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 98 929 | 97 763 | 102 424 | 102 941 | 102 472 |
| 30 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 97 009 | 120 104 | 125 038 | 125 868 | 126 232 |
| 31 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 96 655 | 94 798 | 99 019 | 99 678 | 99 514 |
| 32 | go (1.14)| [beego](https://beego.me) (1.12) | 95 919 | 98 628 | 103 602 | 103 568 | 103 246 |
| 33 | go (1.14)| [gin](https://gin-gonic.com) (1.5) | 95 736 | 98 234 | 101 075 | 101 606 | 101 292 |
| 34 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 95 183 | 102 872 | 105 656 | 104 243 | 103 823 |
| 35 | c (99)| [kore](https://kore.io) (3.3) | 94 224 | 149 347 | 151 573 | 155 908 | 144 216 |
| 36 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 94 223 | 93 470 | 97 919 | 99 174 | 99 110 |
| 37 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 91 039 | 105 495 | 107 530 | 104 603 | 104 706 |
| 38 | java (8)| [act](https://actframework.org) (1.8) | 90 193 | 131 440 | 134 241 | 131 082 | 129 498 |
| 39 | go (1.14)| [air](https://github.com/aofei/air) (0.15) | 83 633 | 86 911 | 89 978 | 90 239 | 89 479 |
| 40 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 79 911 | 89 812 | 92 307 | 92 403 | 91 885 |
| 41 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 79 144 | 100 790 | 100 969 | 95 615 | 95 997 |
| 42 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 75 821 | 96 046 | 95 666 | 91 507 | 91 004 |
| 43 | go (1.14)| [gf](https://goframe.org) (1.11) | 75 182 | 82 308 | 84 880 | 85 020 | 84 970 |
| 44 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.1) | 71 902 | 91 067 | 90 806 | 85 851 | 86 336 |
| 45 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 71 085 | 86 733 | 85 551 | 81 507 | 81 015 |
| 46 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 70 747 | 79 279 | 83 665 | 84 192 | 83 518 |
| 47 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 70 247 | 76 460 | 77 995 | 76 636 | 76 268 |
| 48 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 68 996 | 84 370 | 82 932 | 79 706 | 79 900 |
| 49 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 66 384 | 64 977 | 68 902 | 69 030 | 69 417 |
| 50 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 63 315 | 67 750 | 65 763 | 64 639 | 64 677 |
| 51 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 59 405 | 75 126 | 73 261 | 71 310 | 70 840 |
| 52 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 58 196 | 63 062 | 63 464 | 62 589 | 62 271 |
| 53 | javascript (13.7)| [fastify](https://fastify.io) (2.12) | 57 060 | 69 889 | 67 712 | 66 729 | 66 354 |
| 54 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 56 757 | 61 163 | 62 400 | 62 558 | 62 135 |
| 55 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 56 457 | 69 073 | 67 416 | 65 735 | 65 367 |
| 56 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.12) | 56 306 | 64 958 | 64 764 | 62 215 | 62 188 |
| 57 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 52 021 | 77 496 | 75 767 | 76 184 | 77 298 |
| 58 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 51 321 | 57 613 | 57 895 | 55 510 | 55 688 |
| 59 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 50 800 | 63 112 | 61 033 | 58 731 | 59 698 |
| 60 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 49 348 | 52 195 | 50 836 | 50 788 | 50 828 |
| 61 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 49 259 | 50 818 | 51 175 | 49 842 | 49 952 |
| 62 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 49 060 | 55 388 | 55 374 | 53 042 | 53 005 |
| 63 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 48 716 | 60 513 | 59 250 | 57 845 | 58 217 |
| 64 | java (8)| [javalin](https://javalin.io) (3.5) | 48 069 | 82 348 | 81 461 | 76 024 | 79 338 |
| 65 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 47 373 | 50 625 | 50 870 | 50 677 | 50 439 |
| 66 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 47 150 | 55 842 | 56 440 | 56 037 | 56 387 |
| 67 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 46 888 | 49 924 | 49 390 | 49 096 | 49 147 |
| 68 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 45 054 | 77 333 | 80 300 | 78 480 | 80 072 |
| 69 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 44 976 | 50 132 | 49 486 | 49 920 | 49 070 |
| 70 | python (3.8)| [hug](https://hug.rest) (2.6) | 44 846 | 49 736 | 49 656 | 49 488 | 49 142 |
| 71 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 43 979 | 54 906 | 53 747 | 53 125 | 52 901 |
| 72 | python (3.8)| [starlette](https://starlette.io) (0.13) | 43 977 | 49 574 | 49 299 | 47 579 | 47 598 |
| 73 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 41 179 | 44 383 | 44 001 | 42 950 | 42 872 |
| 74 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 40 845 | 43 903 | 44 806 | 45 223 | 44 678 |
| 75 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 40 346 | 43 209 | 43 506 | 42 613 | 42 572 |
| 76 | javascript (13.7)| [moleculer](https://moleculer.services) (0.14) | 39 889 | 51 038 | 49 637 | 49 250 | 49 243 |
| 77 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 39 456 | 48 878 | 51 011 | 51 967 | 52 345 |
| 78 | php (7.4)| [imi](https://imiphp.com) (1.0) | 39 079 | 42 617 | 43 027 | 43 297 | 43 321 |
| 79 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 39 023 | 41 287 | 39 596 | 38 985 | 38 771 |
| 80 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.1) | 37 887 | 40 647 | 39 467 | 38 673 | 38 914 |
| 81 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 37 803 | 37 291 | 37 457 | 36 528 | 36 771 |
| 82 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.29) | 36 975 | 38 933 | 37 903 | 37 292 | 37 597 |
| 83 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 36 898 | 39 089 | 39 244 | 38 160 | 38 354 |
| 84 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a5) | 36 281 | 42 576 | 42 642 | 40 988 | 40 546 |
| 85 | php (7.4)| [swoft](https://swoft.org) (2.0) | 35 218 | 36 856 | 36 710 | 36 472 | 36 264 |
| 86 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 35 190 | 71 986 | 75 937 | 74 457 | 73 123 |
| 87 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 34 126 | 34 922 | 34 315 | 34 177 | 34 335 |
| 88 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 32 918 | 43 656 | 43 277 | 42 862 | 42 568 |
| 89 | javascript (13.7)| [restify](https://restify.com) (8.5) | 32 708 | 40 920 | 41 382 | 40 141 | 40 194 |
| 90 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 31 824 | 33 212 | 32 754 | 32 407 | 32 366 |
| 91 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 30 727 | 34 280 | 34 247 | 36 847 | 34 022 |
| 92 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.49) | 29 426 | 31 729 | 30 644 | 30 211 | 30 706 |
| 93 | java (8)| [micronaut](https://micronaut.io) (1.2) | 29 383 | 68 998 | 70 445 | 67 648 | 67 877 |
| 94 | python (3.8)| [responder](https://python-responder.org) (2.0) | 29 234 | 31 812 | 31 354 | 29 958 | 30 230 |
| 95 | crystal (0.33)| [lucky](https://luckyframework.org) (0.18) | 28 671 | 24 734 | 26 234 | 22 701 | 21 836 |
| 96 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 28 482 | 28 763 | 28 738 | 28 549 | 28 577 |
| 97 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 28 278 | 56 765 | 59 445 | 58 926 | 59 312 |
| 98 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 26 051 | 27 477 | 27 363 | 27 220 | 27 323 |
| 99 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 25 279 | 26 295 | 26 611 | 26 639 | 26 692 |
| 100 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 24 873 | 25 485 | 25 385 | 25 371 | 25 374 |
| 101 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 23 560 | 25 930 | 25 691 | 24 685 | 25 153 |
| 102 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 23 400 | 24 411 | 24 661 | 24 384 | 24 223 |
| 103 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 22 741 | 24 684 | 24 771 | 24 536 | 24 098 |
| 104 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 403 | 21 899 | 20 056 | 20 443 | 20 222 |
| 105 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 21 019 | 22 005 | 21 451 | 20 616 | 20 437 |
| 106 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 19 016 | 57 307 | 64 026 | 63 364 | 62 903 |
| 107 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 17 357 | 17 382 | 17 523 | 17 261 | 17 298 |
| 108 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 16 955 | 17 832 | 18 049 | 17 847 | 17 923 |
| 109 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 15 232 | 15 524 | 15 552 | 15 671 | 15 571 |
| 110 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 14 986 | 15 492 | 15 555 | 15 611 | 15 560 |
| 111 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 13 739 | 13 941 | 14 082 | 14 046 | 14 020 |
| 112 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 13 079 | 13 257 | 13 193 | 13 100 | 13 018 |
| 113 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 12 137 | 12 568 | 11 607 | 11 035 | 11 040 |
| 114 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 11 503 | 11 653 | 11 679 | 11 712 | 11 705 |
| 115 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 10 604 | 10 763 | 10 425 | 10 246 | 10 103 |
| 116 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 969 | 10 806 | 10 678 | 10 638 | 10 611 |
| 117 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 9 126 | 9 250 | 9 257 | 9 094 | 9 105 |
| 118 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 477 | 9 019 | 11 546 | 51 504 | 46 595 |
| 119 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 8 128 | 8 229 | 8 279 | 8 383 | 8 159 |
| 120 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 7 642 | 7 817 | 10 475 | 45 377 | 39 965 |
| 121 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 7 356 | 2 037 | 2 719 | 896 | 1 435 |
| 122 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 207 | 7 499 | 10 318 | 53 148 | 40 410 |
| 123 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 6 999 | 7 436 | 9 502 | 45 484 | 41 865 |
| 124 | crystal (0.33)| [onyx](https://onyxframework.org) (0.5) | 4 642 | 4 865 | 4 901 | 4 894 | 4 871 |
| 125 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 431 | 4 763 | 7 847 | 43 462 | 35 434 |
| 126 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 187 | 4 320 | 7 270 | 43 217 | 32 610 |
| 127 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 4 100 | 4 307 | 7 413 | 42 937 | 35 646 |
| 128 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 4 009 | 3 938 | 3 932 | 3 912 | 3 897 |
| 129 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 419 | 3 600 | 7 078 | 42 184 | 38 282 |
| 130 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 225 | 3 588 | 7 027 | 43 963 | 32 788 |
| 131 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 2 777 | 3 928 | 1 869 | 1 825 | 1 826 |
| 132 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 358 | 2 378 | 2 353 | 2 303 | 2 299 |
| 133 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 1 910 | 929 | 8 318 | 39 263 | 30 480 |
| 134 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 1 839 | 1 915 | 5 816 | 40 851 | 38 672 |
| 135 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 523 | 1 499 | 1 481 | 1 409 | 1 438 |
| 136 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 488 | 1 533 | 1 396 | 1 489 | 1 485 |
| 137 | php (7.4)| [laravel](https://laravel.com) (6.17) | 494 | 246 | 10 229 | 28 073 | 22 140 |

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

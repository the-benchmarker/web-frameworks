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
~~~

Docker can be used to set up the database:

~~~sh
docker run -it --rm -d \
  -p 5432:5432 \
  -e POSTGRES_DB=benchmark \
  -e POSTGRES_HOST_AUTH_METHOD=trust \
  -v /tmp/pg-data:/var/lib/postgresql/data \
  --name pg postgres:12-alpine
~~~

Wait several seconds for the container to start, then inject the dump:

~~~sh
docker exec pg sh -c "echo \"$(cat .ci/dump.sql)\" | psql -U postgres -d benchmark"
~~~

After creating the database, export its URL:

~~~sh
export DATABASE_URL="postgresql://postgres@localhost/benchmark"
~~~

Docker can be used to set up the database:

~~~sh
docker run -it --rm -d \
  -p 5432:5432 \
  -e POSTGRES_DB=benchmark \
  -e POSTGRES_HOST_AUTH_METHOD=trust \
  -v /tmp/pg-data:/var/lib/postgresql/data \
  --name pg postgres:12-alpine
docker exec pg sh -c "echo \"$(cat .ci/dump.sql)\" | psql -U postgres -d benchmark"
~~~

After creating the database, export its URL: `export DATABASE_URL="postgresql://postgres@localhost/benchmark"`

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

:information_source:  Updated on **2020-03-13** :information_source:

> Benchmarking with [wrk](https://github.com/ioquatix/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 5s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | javascript (13.7)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.1) | 187 181 | 200 039 | 199 467 | 197 302 | 197 386 |
| 2 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 182 812 | 195 646 | 195 608 | 193 302 | 193 268 |
| 3 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 180 248 | 184 978 | 175 053 | 174 793 | 170 411 |
| 4 | javascript (13.7)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (1.1) | 173 631 | 185 369 | 185 406 | 182 446 | 182 266 |
| 5 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.9) | 163 076 | 171 859 | 174 779 | 170 055 | 169 679 |
| 6 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 158 918 | 174 594 | 174 682 | 169 598 | 169 903 |
| 7 | go (1.14)| [router](https://github.com/fasthttp/router) (0.6) | 158 154 | 166 201 | 168 378 | 164 391 | 164 488 |
| 8 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 157 823 | 165 880 | 168 500 | 164 075 | 163 530 |
| 9 | crystal (0.33)| [toro](https://github.com/soveran/toro) (0.4) | 157 477 | 165 407 | 162 551 | 154 000 | 153 705 |
| 10 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.4) | 156 478 | 165 072 | 167 989 | 163 455 | 163 382 |
| 11 | crystal (0.33)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 155 784 | 163 897 | 161 372 | 153 378 | 152 060 |
| 12 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 154 698 | 162 856 | 165 389 | 160 829 | 160 506 |
| 13 | crystal (0.33)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 152 151 | 159 242 | 156 642 | 147 620 | 147 466 |
| 14 | go (1.14)| [fiber](https://fiber.wiki) (1.8) | 151 544 | 152 539 | 151 068 | 143 483 | 143 500 |
| 15 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 148 028 | 159 185 | 160 557 | 159 006 | 158 784 |
| 16 | crystal (0.33)| [kemal](https://kemalcr.com) (0.26) | 145 924 | 152 857 | 150 116 | 140 907 | 140 253 |
| 17 | crystal (0.33)| [grip](https://github.com/grip-framework/grip) (0.28) | 144 709 | 151 905 | 147 639 | 139 576 | 138 707 |
| 18 | crystal (0.33)| [amber](https://amberframework.org) (0.33) | 139 585 | 144 922 | 141 599 | 132 894 | 132 169 |
| 19 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 138 306 | 149 013 | 149 183 | 146 117 | 144 644 |
| 20 | crystal (0.33)| [lucky](https://luckyframework.org) (0.18) | 136 074 | 140 639 | 135 236 | 124 975 | 125 624 |
| 21 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 135 019 | 171 435 | 158 815 | 185 880 | 182 491 |
| 22 | crystal (0.33)| [orion](https://github.com/obsidian/orion) (2.1) | 134 263 | 138 602 | 133 385 | 121 101 | 119 803 |
| 23 | crystal (0.33)| [athena](https://github.com/athena-framework/athena) (0.8) | 126 210 | 130 090 | 123 002 | 110 048 | 109 883 |
| 24 | java (8)| [act](https://actframework.org) (1.8) | 121 271 | 134 772 | 134 736 | 131 368 | 131 087 |
| 25 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 110 667 | 109 979 | 113 157 | 113 069 | 112 355 |
| 26 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 109 161 | 108 719 | 111 578 | 111 237 | 111 468 |
| 27 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.0) | 105 149 | 103 641 | 106 789 | 106 605 | 106 371 |
| 28 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 104 431 | 108 138 | 110 596 | 109 222 | 109 529 |
| 29 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 103 513 | 103 302 | 106 203 | 105 772 | 105 451 |
| 30 | go (1.14)| [violetear](https://violetear.org) (7.0) | 101 984 | 100 989 | 104 770 | 105 028 | 104 942 |
| 31 | c (99)| [kore](https://kore.io) (3.3) | 99 996 | 160 688 | 162 314 | 156 195 | 154 707 |
| 32 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 99 972 | 98 490 | 101 925 | 102 168 | 102 302 |
| 33 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 99 720 | 103 064 | 104 688 | 102 959 | 102 603 |
| 34 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 99 389 | 97 849 | 101 238 | 101 701 | 101 504 |
| 35 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 98 933 | 122 360 | 125 799 | 127 155 | 126 475 |
| 36 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 97 156 | 95 254 | 98 594 | 98 934 | 98 422 |
| 37 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 96 481 | 104 273 | 105 286 | 102 388 | 102 593 |
| 38 | go (1.14)| [beego](https://beego.me) (1.12) | 96 438 | 99 547 | 102 575 | 102 410 | 102 182 |
| 39 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 95 515 | 104 533 | 103 427 | 99 056 | 98 755 |
| 40 | go (1.14)| [gin](https://gin-gonic.com) (1.5) | 95 484 | 98 275 | 100 711 | 100 727 | 100 118 |
| 41 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 95 239 | 94 118 | 97 421 | 97 894 | 98 059 |
| 42 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 92 022 | 100 951 | 100 358 | 96 178 | 95 713 |
| 43 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 87 660 | 91 998 | 92 952 | 93 107 | 93 428 |
| 44 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.1) | 86 820 | 95 412 | 94 411 | 90 563 | 89 938 |
| 45 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 85 941 | 90 559 | 88 025 | 85 315 | 85 037 |
| 46 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 84 115 | 88 606 | 85 855 | 83 103 | 83 090 |
| 47 | go (1.14)| [air](https://github.com/aofei/air) (0.15) | 83 568 | 87 574 | 89 471 | 89 472 | 89 283 |
| 48 | java (8)| [javalin](https://javalin.io) (3.5) | 77 814 | 83 355 | 81 392 | 79 984 | 80 616 |
| 49 | go (1.14)| [gf](https://goframe.org) (1.11) | 76 798 | 81 983 | 83 591 | 83 792 | 83 716 |
| 50 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 74 044 | 79 836 | 77 346 | 75 176 | 76 033 |
| 51 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 73 370 | 79 295 | 78 611 | 76 932 | 76 728 |
| 52 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 71 240 | 79 075 | 84 102 | 84 641 | 84 173 |
| 53 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 70 407 | 74 448 | 72 850 | 70 406 | 70 406 |
| 54 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 69 129 | 72 077 | 70 363 | 68 609 | 68 427 |
| 55 | javascript (13.7)| [fastify](https://fastify.io) (2.12) | 69 129 | 72 561 | 70 397 | 69 125 | 68 796 |
| 56 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 67 685 | 80 792 | 82 747 | 81 473 | 81 188 |
| 57 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 65 947 | 77 092 | 77 004 | 75 603 | 75 674 |
| 58 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 63 824 | 64 591 | 64 204 | 62 491 | 62 660 |
| 59 | java (8)| [micronaut](https://micronaut.io) (1.2) | 63 480 | 71 598 | 71 835 | 69 502 | 69 676 |
| 60 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 62 459 | 64 962 | 63 005 | 61 753 | 61 785 |
| 61 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 61 699 | 64 886 | 67 743 | 68 031 | 67 726 |
| 62 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.14) | 60 552 | 66 588 | 66 641 | 64 291 | 64 183 |
| 63 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 60 535 | 62 781 | 61 241 | 59 744 | 59 647 |
| 64 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 59 706 | 62 277 | 62 462 | 59 511 | 61 531 |
| 65 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 58 713 | 62 534 | 62 662 | 62 144 | 61 870 |
| 66 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 56 440 | 64 531 | 64 191 | 63 165 | 63 226 |
| 67 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 54 882 | 57 467 | 55 588 | 54 639 | 54 699 |
| 68 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 53 857 | 55 020 | 54 361 | 53 818 | 53 817 |
| 69 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 52 190 | 55 404 | 55 480 | 55 140 | 55 007 |
| 70 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 52 022 | 58 973 | 59 086 | 59 059 | 61 104 |
| 71 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 51 250 | 56 532 | 56 132 | 54 187 | 53 955 |
| 72 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 51 106 | 51 398 | 51 174 | 50 637 | 50 558 |
| 73 | javascript (13.7)| [moleculer](https://moleculer.services) (0.14) | 50 891 | 52 754 | 51 636 | 50 819 | 50 917 |
| 74 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 49 648 | 49 867 | 49 730 | 48 988 | 48 801 |
| 75 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 49 418 | 50 617 | 50 281 | 49 551 | 49 490 |
| 76 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 48 931 | 50 725 | 50 809 | 50 233 | 50 333 |
| 77 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 47 600 | 54 026 | 53 873 | 52 106 | 52 032 |
| 78 | python (3.8)| [hug](https://hug.rest) (2.6) | 47 398 | 49 419 | 49 256 | 48 605 | 48 601 |
| 79 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 46 647 | 50 262 | 49 840 | 49 307 | 49 186 |
| 80 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 46 001 | 45 678 | 45 746 | 45 447 | 45 249 |
| 81 | python (3.8)| [starlette](https://starlette.io) (0.13) | 45 413 | 49 216 | 48 756 | 47 178 | 47 203 |
| 82 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 44 167 | 46 740 | 45 473 | 45 070 | 45 246 |
| 83 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 42 420 | 43 336 | 42 967 | 42 111 | 41 880 |
| 84 | php (7.4)| [imi](https://imiphp.com) (1.0) | 41 620 | 43 029 | 42 877 | 42 842 | 42 935 |
| 85 | javascript (13.7)| [restify](https://restify.com) (8.5) | 41 287 | 43 773 | 42 433 | 42 027 | 42 097 |
| 86 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 41 216 | 43 086 | 41 002 | 40 351 | 40 309 |
| 87 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 40 791 | 48 589 | 51 326 | 52 850 | 51 680 |
| 88 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 39 625 | 43 286 | 43 417 | 42 829 | 42 887 |
| 89 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 39 293 | 41 114 | 38 898 | 38 588 | 38 967 |
| 90 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a5) | 39 207 | 42 948 | 42 492 | 40 925 | 41 002 |
| 91 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 37 880 | 38 979 | 38 328 | 37 376 | 37 448 |
| 92 | php (7.4)| [swoft](https://swoft.org) (2.0) | 36 371 | 37 089 | 36 704 | 36 425 | 36 311 |
| 93 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.29) | 36 148 | 39 416 | 37 573 | 36 320 | 36 361 |
| 94 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 34 583 | 35 410 | 34 000 | 34 168 | 33 905 |
| 95 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 33 162 | 30 468 | 31 104 | 34 715 | 30 686 |
| 96 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 32 272 | 33 128 | 31 549 | 31 666 | 32 207 |
| 97 | dart (2.7)| [aqueduct](https://aqueduct.io) (3.2) | 31 747 | 30 939 | 31 707 | 30 033 | 29 855 |
| 98 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.52) | 29 831 | 31 847 | 31 363 | 30 105 | 30 206 |
| 99 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 28 768 | 29 284 | 28 562 | 28 418 | 28 806 |
| 100 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 26 988 | 27 589 | 27 547 | 27 238 | 26 757 |
| 101 | python (3.8)| [responder](https://python-responder.org) (2.0) | 26 871 | 28 540 | 28 481 | 26 651 | 26 867 |
| 102 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 26 398 | 25 801 | 25 223 | 25 359 | 25 410 |
| 103 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 26 012 | 26 770 | 26 543 | 26 382 | 25 783 |
| 104 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 25 313 | 27 316 | 27 458 | 25 875 | 26 427 |
| 105 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 24 386 | 24 617 | 24 699 | 22 500 | 23 976 |
| 106 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 23 280 | 22 223 | 21 046 | 20 739 | 20 678 |
| 107 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 22 712 | 24 280 | 24 259 | 23 880 | 24 013 |
| 108 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 21 638 | 21 926 | 21 332 | 20 487 | 20 582 |
| 109 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 18 305 | 18 298 | 18 434 | 18 422 | 18 197 |
| 110 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 908 | 18 291 | 18 373 | 18 225 | 18 223 |
| 111 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 16 383 | 16 395 | 16 283 | 16 350 | 16 304 |
| 112 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 14 990 | 15 386 | 15 439 | 15 384 | 15 400 |
| 113 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 13 817 | 13 406 | 13 520 | 13 520 | 13 698 |
| 114 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 13 243 | 13 195 | 13 160 | 13 149 | 13 079 |
| 115 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 12 109 | 12 049 | 12 039 | 12 065 | 12 060 |
| 116 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 667 | 11 722 | 11 696 | 11 666 | 11 744 |
| 117 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 9 750 | 10 610 | 10 097 | 9 731 | 9 736 |
| 118 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 211 | 9 168 | 9 063 | 52 937 | 49 489 |
| 119 | python (3.8)| [django](https://djangoproject.com) (3.0) | 8 732 | 9 147 | 9 019 | 8 983 | 8 942 |
| 120 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 8 328 | 9 166 | 9 056 | 9 108 | 9 101 |
| 121 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 7 963 | 7 893 | 7 801 | 43 233 | 44 308 |
| 122 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 7 569 | 10 522 | 10 274 | 10 041 | 9 948 |
| 123 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 548 | 7 500 | 7 471 | 52 584 | 52 446 |
| 124 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 489 | 7 498 | 7 416 | 44 967 | 41 618 |
| 125 | crystal (0.33)| [onyx](https://onyxframework.org) (0.5) | 4 794 | 4 913 | 4 913 | 4 919 | 4 909 |
| 126 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 634 | 4 651 | 4 722 | 44 142 | 39 442 |
| 127 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 369 | 4 372 | 4 596 | 43 540 | 42 376 |
| 128 | php (7.4)| [lumen](https://lumen.laravel.com) (7.0) | 4 295 | 4 328 | 4 393 | 44 075 | 41 136 |
| 129 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 708 | 3 648 | 3 627 | 3 598 | 3 573 |
| 130 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 629 | 3 643 | 3 757 | 43 022 | 40 034 |
| 131 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 607 | 3 618 | 3 724 | 42 379 | 39 660 |
| 132 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 430 | 2 447 | 2 427 | 2 405 | 2 404 |
| 133 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 1 926 | 1 981 | 1 997 | 41 019 | 38 349 |
| 134 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 1 889 | 5 421 | 4 356 | 3 422 | 2 193 |
| 135 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 584 | 1 600 | 1 579 | 1 573 | 1 578 |
| 136 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 579 | 1 537 | 1 515 | 1 489 | 1 495 |
| 137 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 1 097 | 955 | 1 615 | 1 285 | 850 |
| 138 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 565 | 464 | 1 453 | 35 266 | 34 868 |
| 139 | php (7.4)| [laravel](https://laravel.com) (7.1) | 352 | 156 | 2 563 | 23 356 | 23 091 |

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

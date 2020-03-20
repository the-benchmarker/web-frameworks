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

:information_source:  Updated on **2020-03-20** :information_source:

> Benchmarking with [wrk](https://github.com/ioquatix/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 182 612 | 195 101 | 195 728 | 191 715 | 192 361 |
| 2 | javascript (13.7)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.1) | 179 094 | 191 888 | 192 135 | 188 651 | 189 206 |
| 3 | javascript (13.7)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (1.1) | 169 010 | 179 283 | 181 873 | 176 097 | 177 030 |
| 4 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.9) | 164 766 | 174 047 | 177 706 | 173 484 | 173 375 |
| 5 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 159 385 | 167 537 | 171 094 | 166 947 | 166 896 |
| 6 | go (1.14)| [router](https://github.com/fasthttp/router) (0.6) | 159 179 | 168 351 | 171 951 | 167 860 | 167 542 |
| 7 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.4) | 159 173 | 168 189 | 171 327 | 164 468 | 167 189 |
| 8 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 158 582 | 173 728 | 174 236 | 169 000 | 168 492 |
| 9 | crystal (0.33)| [toro](https://github.com/soveran/toro) (0.4) | 158 311 | 166 913 | 164 661 | 156 361 | 155 822 |
| 10 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 157 820 | 166 068 | 169 525 | 165 583 | 165 435 |
| 11 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 155 059 | 175 831 | 175 606 | 178 921 | 176 546 |
| 12 | go (1.14)| [fiber](https://fiber.wiki) (1.8) | 153 915 | 166 925 | 165 912 | 158 335 | 157 518 |
| 13 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 148 923 | 159 505 | 161 163 | 160 337 | 159 607 |
| 14 | crystal (0.33)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 147 344 | 152 536 | 150 083 | 140 961 | 142 067 |
| 15 | crystal (0.33)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 142 199 | 148 946 | 147 188 | 138 351 | 138 410 |
| 16 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 141 881 | 152 648 | 153 036 | 149 292 | 149 219 |
| 17 | crystal (0.33)| [grip](https://github.com/grip-framework/grip) (0.28) | 139 590 | 146 600 | 142 767 | 132 826 | 133 940 |
| 18 | crystal (0.33)| [kemal](https://kemalcr.com) (0.26) | 139 058 | 144 171 | 139 687 | 132 943 | 134 810 |
| 19 | crystal (0.33)| [amber](https://amberframework.org) (0.33) | 136 094 | 141 139 | 137 044 | 127 459 | 127 080 |
| 20 | crystal (0.33)| [lucky](https://luckyframework.org) (0.18) | 126 873 | 131 441 | 126 442 | 114 663 | 114 690 |
| 21 | crystal (0.33)| [orion](https://github.com/obsidian/orion) (2.1) | 125 504 | 131 910 | 128 160 | 116 832 | 117 111 |
| 22 | java (8)| [act](https://actframework.org) (1.8) | 119 911 | 132 077 | 132 236 | 128 595 | 128 765 |
| 23 | crystal (0.33)| [athena](https://github.com/athena-framework/athena) (0.8) | 116 695 | 120 386 | 117 783 | 103 819 | 101 718 |
| 24 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 112 598 | 112 372 | 116 521 | 116 241 | 115 906 |
| 25 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 110 952 | 110 696 | 114 553 | 113 999 | 113 891 |
| 26 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.0) | 106 459 | 105 386 | 109 138 | 109 177 | 109 226 |
| 27 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 106 414 | 110 423 | 113 675 | 112 983 | 112 395 |
| 28 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 104 276 | 104 813 | 108 014 | 107 678 | 107 064 |
| 29 | go (1.14)| [violetear](https://violetear.org) (7.0) | 103 252 | 103 300 | 107 222 | 107 240 | 106 989 |
| 30 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 101 688 | 100 676 | 104 470 | 105 318 | 104 694 |
| 31 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 101 630 | 122 780 | 127 982 | 128 778 | 128 246 |
| 32 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 101 548 | 100 332 | 104 241 | 104 780 | 104 303 |
| 33 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 101 274 | 104 876 | 107 488 | 105 940 | 105 924 |
| 34 | go (1.14)| [beego](https://beego.me) (1.12) | 99 350 | 102 779 | 106 347 | 106 265 | 105 924 |
| 35 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 98 342 | 97 212 | 100 726 | 101 635 | 101 489 |
| 36 | go (1.14)| [gin](https://gin-gonic.com) (1.5) | 98 179 | 101 397 | 104 268 | 104 408 | 104 048 |
| 37 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 97 033 | 95 983 | 99 900 | 100 647 | 100 430 |
| 38 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 95 540 | 103 886 | 104 913 | 102 209 | 103 341 |
| 39 | c (99)| [kore](https://kore.io) (3.3) | 91 285 | 148 801 | 150 526 | 150 135 | 155 424 |
| 40 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 91 037 | 102 211 | 101 992 | 99 419 | 99 223 |
| 41 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 87 658 | 91 273 | 93 305 | 91 828 | 92 154 |
| 42 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 86 732 | 181 315 | 174 085 | 182 124 | 180 269 |
| 43 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 85 560 | 91 152 | 89 253 | 86 432 | 85 308 |
| 44 | go (1.14)| [air](https://github.com/aofei/air) (0.15) | 85 345 | 89 621 | 91 987 | 92 467 | 92 131 |
| 45 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 82 531 | 92 306 | 96 222 | 94 848 | 93 141 |
| 46 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.1) | 81 690 | 89 392 | 86 650 | 83 821 | 86 302 |
| 47 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 79 171 | 85 414 | 81 780 | 79 586 | 78 254 |
| 48 | go (1.14)| [gf](https://goframe.org) (1.11) | 77 995 | 83 694 | 85 695 | 86 002 | 85 727 |
| 49 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 77 059 | 83 350 | 80 137 | 78 180 | 79 157 |
| 50 | java (8)| [javalin](https://javalin.io) (3.5) | 74 299 | 78 182 | 76 567 | 75 428 | 73 560 |
| 51 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 67 378 | 73 045 | 73 733 | 71 304 | 68 314 |
| 52 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 65 852 | 69 471 | 70 696 | 70 091 | 69 919 |
| 53 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 65 778 | 70 327 | 68 537 | 66 552 | 67 591 |
| 54 | javascript (13.7)| [fastify](https://fastify.io) (2.12) | 65 167 | 67 240 | 66 261 | 65 847 | 66 144 |
| 55 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 64 934 | 77 514 | 79 445 | 77 625 | 77 896 |
| 56 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 63 993 | 65 981 | 61 047 | 58 873 | 60 073 |
| 57 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 63 988 | 64 644 | 64 489 | 63 058 | 62 917 |
| 58 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 63 585 | 66 820 | 70 000 | 70 138 | 69 986 |
| 59 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 63 356 | 71 821 | 72 618 | 71 274 | 71 435 |
| 60 | java (8)| [micronaut](https://micronaut.io) (1.2) | 62 213 | 70 250 | 70 111 | 67 676 | 67 850 |
| 61 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 61 991 | 65 734 | 61 947 | 60 581 | 61 671 |
| 62 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 60 468 | 64 080 | 63 892 | 62 907 | 63 146 |
| 63 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 58 490 | 61 458 | 59 435 | 57 431 | 57 389 |
| 64 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.14) | 57 383 | 64 066 | 64 162 | 61 747 | 61 821 |
| 65 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 56 681 | 55 982 | 56 829 | 56 224 | 56 436 |
| 66 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 55 798 | 64 549 | 63 964 | 62 915 | 62 482 |
| 67 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 54 575 | 55 961 | 55 797 | 55 448 | 55 670 |
| 68 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 54 312 | 59 519 | 59 349 | 57 338 | 57 322 |
| 69 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 53 568 | 60 653 | 60 955 | 63 057 | 63 408 |
| 70 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 52 611 | 54 628 | 53 201 | 52 434 | 51 281 |
| 71 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 52 445 | 65 072 | 71 935 | 71 904 | 70 506 |
| 72 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 51 443 | 53 620 | 53 549 | 52 883 | 52 796 |
| 73 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 50 896 | 55 560 | 55 568 | 53 721 | 53 559 |
| 74 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 49 644 | 51 645 | 51 705 | 51 122 | 51 206 |
| 75 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 48 997 | 47 762 | 49 209 | 47 477 | 48 233 |
| 76 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 48 948 | 50 038 | 48 726 | 48 483 | 48 593 |
| 77 | javascript (13.7)| [moleculer](https://moleculer.services) (0.14) | 48 850 | 50 297 | 49 393 | 49 529 | 50 495 |
| 78 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 47 677 | 49 364 | 49 210 | 48 728 | 48 475 |
| 79 | python (3.8)| [starlette](https://starlette.io) (0.13) | 46 279 | 49 760 | 49 525 | 48 170 | 47 958 |
| 80 | python (3.8)| [hug](https://hug.rest) (2.6) | 46 239 | 49 565 | 49 595 | 49 233 | 49 229 |
| 81 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 46 116 | 46 266 | 46 243 | 46 331 | 45 821 |
| 82 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 44 093 | 46 834 | 45 719 | 44 898 | 43 139 |
| 83 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 41 830 | 43 340 | 41 727 | 40 731 | 40 664 |
| 84 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 41 825 | 48 661 | 51 079 | 52 451 | 53 096 |
| 85 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 41 660 | 43 800 | 43 545 | 42 768 | 42 721 |
| 86 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 41 610 | 42 526 | 42 109 | 41 102 | 41 030 |
| 87 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0b1) | 40 821 | 43 154 | 43 008 | 41 335 | 41 488 |
| 88 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 40 554 | 43 310 | 41 442 | 40 588 | 41 269 |
| 89 | php (7.4)| [imi](https://imiphp.com) (1.0) | 40 504 | 43 557 | 43 927 | 43 999 | 43 841 |
| 90 | javascript (13.7)| [restify](https://restify.com) (8.5) | 40 485 | 42 227 | 41 274 | 41 124 | 41 159 |
| 91 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.3) | 38 008 | 40 264 | 38 599 | 38 042 | 38 492 |
| 92 | php (7.4)| [swoft](https://swoft.org) (2.0) | 36 302 | 37 927 | 37 725 | 37 222 | 37 176 |
| 93 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 36 301 | 37 065 | 36 296 | 35 688 | 35 557 |
| 94 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 36 002 | 36 176 | 34 968 | 34 530 | 34 914 |
| 95 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 31 930 | 32 387 | 32 039 | 31 285 | 31 253 |
| 96 | dart (2.7)| [aqueduct](https://aqueduct.io) (3.2) | 31 658 | 31 480 | 32 450 | 31 553 | 30 279 |
| 97 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.52) | 30 549 | 32 351 | 32 048 | 30 832 | 30 892 |
| 98 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 30 212 | 29 732 | 29 146 | 29 105 | 29 349 |
| 99 | python (3.8)| [responder](https://python-responder.org) (2.0) | 29 800 | 31 795 | 31 475 | 30 338 | 30 299 |
| 100 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 27 305 | 27 954 | 27 708 | 27 658 | 27 522 |
| 101 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 26 893 | 26 234 | 25 811 | 25 805 | 25 881 |
| 102 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 26 633 | 28 987 | 32 599 | 33 505 | 33 230 |
| 103 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 26 450 | 26 923 | 27 013 | 26 731 | 26 383 |
| 104 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 26 091 | 28 137 | 28 184 | 27 516 | 26 935 |
| 105 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 24 179 | 25 287 | 25 147 | 24 863 | 24 919 |
| 106 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 24 083 | 24 638 | 24 563 | 24 147 | 24 210 |
| 107 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 842 | 21 889 | 20 814 | 20 658 | 20 515 |
| 108 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 21 770 | 22 566 | 21 916 | 21 102 | 21 142 |
| 109 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 18 462 | 18 469 | 18 639 | 18 416 | 18 532 |
| 110 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 18 128 | 18 479 | 18 710 | 18 500 | 18 532 |
| 111 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 16 885 | 16 563 | 16 568 | 16 559 | 16 644 |
| 112 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 15 434 | 15 177 | 15 136 | 15 141 | 15 165 |
| 113 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 15 243 | 15 706 | 15 693 | 15 676 | 15 700 |
| 114 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 13 772 | 13 481 | 13 453 | 13 353 | 13 389 |
| 115 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 12 433 | 12 274 | 11 677 | 11 158 | 11 119 |
| 116 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 12 356 | 12 272 | 12 351 | 12 333 | 12 293 |
| 117 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 10 663 | 10 952 | 10 821 | 10 748 | 10 855 |
| 118 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 471 | 10 712 | 10 540 | 10 357 | 10 343 |
| 119 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 499 | 10 629 | 10 504 | 10 268 | 10 359 |
| 120 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 295 | 9 225 | 9 143 | 53 580 | 52 620 |
| 121 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 8 938 | 9 430 | 9 416 | 9 371 | 9 365 |
| 122 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 8 018 | 7 987 | 7 963 | 45 372 | 43 580 |
| 123 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 627 | 7 640 | 7 577 | 45 843 | 44 974 |
| 124 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 550 | 7 504 | 7 547 | 53 613 | 48 717 |
| 125 | crystal (0.33)| [onyx](https://onyxframework.org) (0.5) | 5 349 | 5 472 | 5 375 | 5 518 | 5 807 |
| 126 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 672 | 4 650 | 4 814 | 44 290 | 41 087 |
| 127 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 471 | 4 563 | 4 592 | 44 463 | 40 944 |
| 128 | php (7.4)| [lumen](https://lumen.laravel.com) (7.0) | 4 401 | 4 412 | 4 478 | 44 195 | 40 168 |
| 129 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 4 380 | 4 174 | 4 167 | 4 145 | 4 144 |
| 130 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 684 | 3 716 | 3 733 | 43 785 | 40 250 |
| 131 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 867 | 2 886 | 2 945 | 42 682 | 40 846 |
| 132 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 435 | 2 430 | 2 413 | 2 402 | 2 394 |
| 133 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 1 954 | 5 353 | 4 223 | 3 325 | 2 016 |
| 134 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 636 | 1 652 | 1 625 | 1 618 | 1 614 |
| 135 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 557 | 1 514 | 1 501 | 1 495 | 1 481 |
| 136 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 455 | 1 484 | 1 541 | 39 595 | 37 724 |
| 137 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 956 | 1 875 | 2 232 | 1 310 | 557 |
| 138 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 491 | 470 | 1 938 | 35 823 | 34 414 |
| 139 | php (7.4)| [laravel](https://laravel.com) (7.2) | 240 | 134 | 1 826 | 24 333 | 22 218 |

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

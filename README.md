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

:information_source:  Updated on **2020-03-28** :information_source:

> Benchmarking with [wrk](https://github.com/ioquatix/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | javascript (13.7)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.11) | 184 273 | 200 965 | 201 033 | 193 411 | 190 653 |
| 2 | javascript (13.7)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.0) | 181 594 | 193 499 | 194 418 | 191 995 | 191 560 |
| 3 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 179 770 | 192 039 | 191 659 | 187 825 | 187 735 |
| 4 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (2.2) | 176 417 | 194 606 | 196 187 | 192 173 | 190 308 |
| 5 | go (1.14)| [fiber](https://fiber.wiki) (1.8) | 160 763 | 164 984 | 162 292 | 152 960 | 152 258 |
| 6 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.9) | 159 610 | 168 327 | 171 407 | 167 076 | 166 623 |
| 7 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 155 050 | 165 372 | 169 363 | 165 914 | 165 193 |
| 8 | crystal (0.33)| [toro](https://github.com/soveran/toro) (0.4) | 153 733 | 160 654 | 158 451 | 149 558 | 149 589 |
| 9 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 153 711 | 162 642 | 165 814 | 161 315 | 160 762 |
| 10 | go (1.14)| [router](https://github.com/fasthttp/router) (0.7) | 153 534 | 162 817 | 166 045 | 161 590 | 160 911 |
| 11 | crystal (0.33)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 153 333 | 160 771 | 157 703 | 149 261 | 149 791 |
| 12 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.4) | 152 670 | 161 471 | 164 790 | 160 413 | 160 032 |
| 13 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 150 793 | 159 663 | 162 993 | 158 740 | 158 473 |
| 14 | crystal (0.33)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 150 645 | 157 761 | 154 309 | 146 166 | 145 763 |
| 15 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 144 473 | 155 310 | 156 493 | 154 692 | 153 751 |
| 16 | crystal (0.33)| [grip](https://github.com/grip-framework/grip) (0.28) | 143 874 | 148 990 | 146 119 | 137 174 | 136 817 |
| 17 | crystal (0.33)| [kemal](https://kemalcr.com) (0.26) | 143 839 | 149 772 | 146 629 | 139 174 | 137 742 |
| 18 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 142 287 | 152 759 | 153 363 | 149 173 | 148 907 |
| 19 | crystal (0.33)| [amber](https://amberframework.org) (0.33) | 136 417 | 142 064 | 138 384 | 131 212 | 130 440 |
| 20 | rust (1.41)| [actix](https://actix.rs) (2.0) | 134 980 | 136 329 | 136 692 | 134 536 | 132 794 |
| 21 | crystal (0.33)| [lucky](https://luckyframework.org) (0.19) | 133 506 | 137 651 | 132 790 | 124 983 | 124 951 |
| 22 | c (99)| [kore](https://kore.io) (3.3) | 132 937 | 160 094 | 142 763 | 144 230 | 156 491 |
| 23 | crystal (0.33)| [orion](https://github.com/obsidian/orion) (2.3) | 131 351 | 134 777 | 130 675 | 121 137 | 119 890 |
| 24 | crystal (0.33)| [athena](https://github.com/athena-framework/athena) (0.8) | 124 257 | 126 629 | 120 620 | 108 320 | 107 116 |
| 25 | java (8)| [act](https://actframework.org) (1.8) | 113 117 | 125 737 | 124 994 | 121 960 | 122 116 |
| 26 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 108 240 | 108 893 | 112 087 | 111 698 | 110 883 |
| 27 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 107 405 | 107 349 | 110 759 | 110 265 | 109 933 |
| 28 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 102 871 | 106 963 | 109 722 | 108 590 | 108 159 |
| 29 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.0) | 102 776 | 102 054 | 105 158 | 105 043 | 104 895 |
| 30 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 101 467 | 101 901 | 105 085 | 104 375 | 104 228 |
| 31 | go (1.14)| [violetear](https://violetear.org) (7.0) | 100 045 | 100 418 | 103 876 | 103 488 | 103 108 |
| 32 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 98 821 | 97 486 | 100 405 | 101 021 | 100 668 |
| 33 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 98 665 | 119 748 | 123 691 | 124 071 | 123 657 |
| 34 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 98 210 | 97 490 | 100 714 | 101 122 | 101 030 |
| 35 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 98 011 | 102 158 | 103 465 | 101 895 | 102 146 |
| 36 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 96 362 | 103 543 | 104 554 | 103 238 | 103 556 |
| 37 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 96 081 | 104 798 | 104 899 | 101 042 | 100 889 |
| 38 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 95 941 | 94 879 | 97 387 | 97 969 | 97 540 |
| 39 | go (1.14)| [beego](https://beego.me) (1.12) | 95 317 | 99 032 | 102 099 | 102 036 | 101 593 |
| 40 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 94 792 | 106 391 | 104 931 | 101 579 | 100 189 |
| 41 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 93 744 | 93 275 | 96 321 | 97 117 | 96 747 |
| 42 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 93 321 | 97 597 | 99 872 | 99 479 | 98 742 |
| 43 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 89 233 | 168 975 | 183 868 | 183 153 | 179 109 |
| 44 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.2) | 88 724 | 98 516 | 97 074 | 94 490 | 93 791 |
| 45 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 87 180 | 91 737 | 92 184 | 92 828 | 92 435 |
| 46 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 86 915 | 87 320 | 88 725 | 86 208 | 86 012 |
| 47 | go (1.14)| [air](https://github.com/aofei/air) (0.15) | 82 359 | 86 618 | 88 539 | 88 576 | 88 290 |
| 48 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 81 342 | 84 468 | 83 104 | 80 489 | 79 886 |
| 49 | go (1.14)| [gf](https://goframe.org) (1.11) | 75 586 | 80 842 | 82 085 | 82 221 | 82 004 |
| 50 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 74 025 | 78 479 | 77 759 | 75 102 | 75 303 |
| 51 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 73 619 | 77 492 | 75 061 | 72 621 | 73 175 |
| 52 | java (8)| [javalin](https://javalin.io) (3.5) | 72 775 | 78 621 | 76 973 | 76 197 | 75 538 |
| 53 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 72 064 | 78 257 | 82 584 | 82 753 | 82 070 |
| 54 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 71 120 | 74 164 | 75 607 | 73 967 | 74 523 |
| 55 | javascript (13.7)| [fastify](https://fastify.io) (2.13) | 69 685 | 73 474 | 71 596 | 70 261 | 70 477 |
| 56 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 67 767 | 73 947 | 74 403 | 73 672 | 73 973 |
| 57 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 65 878 | 71 447 | 69 312 | 69 491 | 67 291 |
| 58 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 65 852 | 76 884 | 79 007 | 78 632 | 78 608 |
| 59 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 63 508 | 66 484 | 65 003 | 63 266 | 63 192 |
| 60 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 63 479 | 64 063 | 63 158 | 61 726 | 61 445 |
| 61 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 62 977 | 67 752 | 68 158 | 67 707 | 67 334 |
| 62 | java (8)| [micronaut](https://micronaut.io) (1.2) | 61 815 | 68 896 | 66 345 | 64 536 | 66 372 |
| 63 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 61 226 | 64 558 | 67 403 | 67 221 | 66 801 |
| 64 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.14) | 58 754 | 63 314 | 63 135 | 60 558 | 60 618 |
| 65 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 58 156 | 61 530 | 60 701 | 57 944 | 57 429 |
| 66 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 58 068 | 60 898 | 59 510 | 58 044 | 58 281 |
| 67 | javascript (13.7)| [feathersjs](https://feathersjs.com) (4.5) | 57 193 | 60 106 | 58 994 | 58 049 | 57 749 |
| 68 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 56 074 | 56 372 | 56 224 | 55 976 | 55 602 |
| 69 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 54 692 | 55 925 | 55 818 | 55 537 | 55 348 |
| 70 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 53 956 | 61 293 | 61 041 | 59 822 | 59 905 |
| 71 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 52 594 | 59 749 | 62 295 | 62 381 | 63 246 |
| 72 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 52 136 | 58 408 | 60 397 | 59 849 | 58 930 |
| 73 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 51 771 | 56 139 | 55 957 | 54 158 | 54 016 |
| 74 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 50 416 | 49 116 | 49 430 | 48 665 | 49 893 |
| 75 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 49 914 | 49 422 | 49 315 | 48 644 | 48 546 |
| 76 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 49 510 | 50 649 | 50 352 | 49 705 | 49 722 |
| 77 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 49 320 | 51 941 | 52 147 | 51 204 | 51 215 |
| 78 | javascript (13.7)| [moleculer](https://moleculer.services) (0.14) | 48 841 | 50 329 | 48 617 | 46 864 | 47 911 |
| 79 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 48 707 | 53 253 | 53 136 | 51 335 | 51 201 |
| 80 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 48 162 | 49 905 | 49 955 | 49 312 | 49 378 |
| 81 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 46 602 | 46 443 | 46 588 | 45 635 | 44 819 |
| 82 | python (3.8)| [hug](https://hug.rest) (2.6) | 45 413 | 48 455 | 48 589 | 47 972 | 48 029 |
| 83 | python (3.8)| [starlette](https://starlette.io) (0.13) | 44 713 | 47 097 | 46 858 | 45 308 | 45 133 |
| 84 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 42 620 | 48 523 | 50 433 | 53 015 | 53 898 |
| 85 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 42 393 | 41 232 | 42 369 | 41 489 | 41 715 |
| 86 | php (7.4)| [imi](https://imiphp.com) (1.0) | 40 585 | 41 716 | 42 103 | 42 014 | 42 228 |
| 87 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 40 579 | 41 136 | 40 927 | 39 462 | 39 553 |
| 88 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 39 842 | 40 983 | 39 053 | 38 443 | 38 663 |
| 89 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 39 474 | 40 384 | 38 290 | 37 788 | 38 571 |
| 90 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 38 910 | 41 595 | 41 624 | 41 109 | 41 028 |
| 91 | javascript (13.7)| [restify](https://restify.com) (8.5) | 38 040 | 40 403 | 38 430 | 39 435 | 39 088 |
| 92 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0b1) | 37 221 | 42 504 | 41 742 | 40 771 | 40 218 |
| 93 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.3) | 36 564 | 37 500 | 35 969 | 35 736 | 35 893 |
| 94 | php (7.4)| [swoft](https://swoft.org) (2.0) | 36 331 | 37 317 | 37 236 | 36 800 | 36 792 |
| 95 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 36 213 | 36 966 | 36 423 | 35 109 | 35 110 |
| 96 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 33 897 | 33 893 | 32 514 | 32 545 | 32 623 |
| 97 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 32 687 | 31 819 | 33 854 | 34 339 | 34 927 |
| 98 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 31 149 | 31 549 | 31 147 | 30 011 | 30 195 |
| 99 | dart (2.7)| [aqueduct](https://aqueduct.io) (3.2) | 29 439 | 31 018 | 30 590 | 29 908 | 29 571 |
| 100 | python (3.8)| [responder](https://python-responder.org) (2.0) | 29 199 | 31 344 | 31 204 | 30 011 | 30 047 |
| 101 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 28 249 | 27 676 | 27 080 | 27 229 | 27 241 |
| 102 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.52) | 28 187 | 30 670 | 30 216 | 29 384 | 29 558 |
| 103 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 27 975 | 27 825 | 27 128 | 27 192 | 27 126 |
| 104 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 25 313 | 27 371 | 27 242 | 26 454 | 26 389 |
| 105 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 25 040 | 24 655 | 24 323 | 24 237 | 24 036 |
| 106 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 24 796 | 26 130 | 25 844 | 25 978 | 25 862 |
| 107 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 24 470 | 24 827 | 24 767 | 24 517 | 24 592 |
| 108 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 24 005 | 23 340 | 21 581 | 21 090 | 21 229 |
| 109 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 23 289 | 23 484 | 22 350 | 22 329 | 22 427 |
| 110 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 21 165 | 22 288 | 24 104 | 23 940 | 23 841 |
| 111 | php (7.4)| [slim](https://slimframework.com) (4.4) | 19 008 | 17 606 | 34 558 | 46 330 | 41 679 |
| 112 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 18 843 | 18 885 | 19 009 | 18 781 | 19 013 |
| 113 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 18 655 | 18 904 | 19 113 | 18 858 | 18 929 |
| 114 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 17 856 | 18 347 | 17 041 | 16 871 | 16 712 |
| 115 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 15 894 | 15 720 | 15 639 | 15 622 | 15 574 |
| 116 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 13 190 | 12 939 | 12 967 | 13 084 | 13 407 |
| 117 | javascript (13.7)| [sails](https://sailsjs.com) (1.2) | 12 703 | 13 265 | 13 172 | 13 015 | 12 674 |
| 118 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 11 607 | 11 308 | 10 781 | 10 258 | 10 566 |
| 119 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 11 543 | 11 309 | 11 313 | 11 281 | 11 222 |
| 120 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 486 | 11 556 | 11 546 | 11 501 | 11 582 |
| 121 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 194 | 9 140 | 9 075 | 51 786 | 49 123 |
| 122 | python (3.8)| [django](https://djangoproject.com) (3.0) | 8 470 | 9 727 | 9 690 | 8 856 | 9 609 |
| 123 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 8 407 | 9 092 | 8 983 | 8 925 | 9 004 |
| 124 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 8 008 | 7 242 | 7 221 | 7 220 | 10 637 |
| 125 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 7 969 | 7 901 | 7 917 | 43 664 | 43 865 |
| 126 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 7 730 | 2 016 | 0 | 0 | 4 184 |
| 127 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 583 | 7 554 | 7 520 | 51 341 | 49 168 |
| 128 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 521 | 7 444 | 7 479 | 43 230 | 42 488 |
| 129 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 6 595 | 9 399 | 9 244 | 9 072 | 8 941 |
| 130 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 5 655 | 5 682 | 5 643 | 43 484 | 41 328 |
| 131 | crystal (0.33)| [onyx](https://onyxframework.org) (0.5) | 4 921 | 5 051 | 5 054 | 5 092 | 5 062 |
| 132 | php (7.4)| [lumen](https://lumen.laravel.com) (7.0) | 4 266 | 4 310 | 4 372 | 42 803 | 37 979 |
| 133 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 146 | 4 101 | 4 274 | 41 385 | 41 944 |
| 134 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 842 | 3 651 | 3 646 | 3 631 | 3 620 |
| 135 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 173 | 3 162 | 3 229 | 41 231 | 40 248 |
| 136 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 823 | 2 827 | 2 881 | 41 534 | 38 331 |
| 137 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 469 | 2 444 | 2 434 | 2 403 | 2 419 |
| 138 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 602 | 1 633 | 1 603 | 1 607 | 1 602 |
| 139 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 532 | 1 498 | 1 501 | 1 482 | 1 471 |
| 140 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 1 459 | 4 341 | 4 317 | 3 427 | 2 172 |
| 141 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 417 | 1 434 | 1 504 | 37 611 | 37 319 |
| 142 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 890 | 1 633 | 1 853 | 1 251 | 441 |
| 143 | php (7.4)| [laravel](https://laravel.com) (7.3) | 666 | 152 | 3 146 | 23 616 | 24 137 |
| 144 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 513 | 439 | 1 458 | 36 540 | 34 021 |

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

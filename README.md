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

:information_source::information_source::information_source::information_source::information_source:

:warning: On `OSX` you need `docker-machine` to use `docker` containerization

~~~
brew install docker-machine
docker-machine create default
eval $(docker-machine env default)
~~~

:information_source::information_source::information_source::information_source::information_source:

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

:information_source:  Updated on **2020-05-06** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | javascript (13.14)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.11) | 178 426 | 190 773 | 190 234 | 181 841 | 185 128 |
| 2 | javascript (13.14)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.0) | 176 922 | 188 532 | 188 888 | 185 936 | 185 438 |
| 3 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 176 627 | 190 639 | 189 619 | 186 275 | 184 491 |
| 4 | php (7.4)| [simps](https://simps.io) (1.0) | 175 713 | 182 925 | 187 455 | 184 420 | 184 385 |
| 5 | java (8)| [jooby](https://jooby.io) (2.8) | 172 723 | 188 517 | 188 758 | 182 875 | 181 547 |
| 6 | javascript (13.14)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 170 492 | 180 146 | 183 412 | 182 390 | 182 012 |
| 7 | kotlin (1.3)| [kooby](https://jooby.io) (2.8) | 163 285 | 177 502 | 188 343 | 183 420 | 181 373 |
| 8 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.12) | 159 448 | 168 033 | 171 380 | 165 939 | 165 526 |
| 9 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 154 859 | 164 221 | 168 491 | 163 701 | 164 131 |
| 10 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.0) | 154 527 | 164 415 | 168 520 | 164 642 | 163 577 |
| 11 | crystal (0.34)| [toro](https://github.com/soveran/toro) (0.4) | 154 018 | 160 849 | 157 670 | 149 889 | 148 294 |
| 12 | go (1.14)| [fiber](https://gofiber.io) (1.9) | 153 075 | 156 059 | 154 587 | 146 875 | 145 933 |
| 13 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 153 046 | 170 419 | 170 046 | 165 036 | 163 471 |
| 14 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.0) | 152 968 | 161 712 | 164 173 | 160 546 | 160 301 |
| 15 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 152 349 | 161 705 | 164 720 | 159 562 | 159 305 |
| 16 | crystal (0.34)| [spider-gazelle](https://spider-gazelle.net) (3.0) | 150 989 | 157 314 | 154 133 | 145 980 | 145 957 |
| 17 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 150 342 | 161 155 | 163 085 | 161 121 | 160 207 |
| 18 | crystal (0.34)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 149 645 | 156 807 | 153 335 | 145 787 | 143 588 |
| 19 | crystal (0.34)| [kemal](https://kemalcr.com) (0.26) | 142 582 | 148 364 | 145 172 | 137 530 | 137 063 |
| 20 | crystal (0.34)| [grip](https://github.com/grip-framework/grip) (0.28) | 141 349 | 146 567 | 144 206 | 136 000 | 134 644 |
| 21 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 132 600 | 146 121 | 146 672 | 141 368 | 140 139 |
| 22 | rust (1.43)| [actix](https://actix.rs) (2.0) | 129 977 | 134 668 | 136 066 | 134 104 | 133 899 |
| 23 | crystal (0.34)| [orion](https://github.com/obsidian/orion) (2.3) | 129 617 | 134 603 | 127 922 | 118 986 | 118 429 |
| 24 | crystal (0.34)| [amber](https://amberframework.org) (0.34) | 123 933 | 135 740 | 130 175 | 123 632 | 123 733 |
| 25 | crystal (0.34)| [athena](https://github.com/athena-framework/athena) (0.8) | 121 067 | 127 610 | 120 198 | 112 669 | 111 833 |
| 26 | java (8)| [act](https://actframework.org) (1.8) | 118 526 | 132 969 | 129 166 | 126 720 | 129 264 |
| 27 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 108 809 | 108 625 | 111 823 | 111 048 | 111 009 |
| 28 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 107 040 | 107 114 | 109 464 | 109 180 | 109 020 |
| 29 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 102 899 | 106 707 | 108 920 | 107 442 | 108 126 |
| 30 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 102 449 | 101 614 | 104 321 | 103 900 | 103 234 |
| 31 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 100 486 | 101 455 | 104 528 | 103 510 | 102 930 |
| 32 | go (1.14)| [violetear](https://violetear.org) (7.0) | 99 662 | 99 696 | 102 692 | 103 033 | 102 145 |
| 33 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 98 689 | 97 267 | 100 364 | 100 283 | 100 010 |
| 34 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 98 606 | 97 594 | 100 144 | 100 104 | 100 047 |
| 35 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 97 564 | 102 129 | 103 654 | 101 834 | 101 436 |
| 36 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 97 252 | 116 518 | 119 255 | 120 301 | 119 552 |
| 37 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 95 867 | 100 531 | 103 264 | 101 980 | 102 433 |
| 38 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 95 377 | 93 981 | 96 602 | 96 832 | 96 837 |
| 39 | go (1.14)| [beego](https://beego.me) (1.12) | 95 366 | 98 601 | 101 701 | 101 340 | 100 956 |
| 40 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 93 937 | 101 521 | 102 481 | 99 986 | 100 476 |
| 41 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 93 137 | 92 887 | 95 047 | 95 820 | 95 371 |
| 42 | javascript (13.14)| [0http](https://github.com/jkyberneees/0http) (2.3) | 88 654 | 97 419 | 97 222 | 93 761 | 93 135 |
| 43 | javascript (13.14)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 87 792 | 98 626 | 97 945 | 93 820 | 93 646 |
| 44 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 87 175 | 91 917 | 93 279 | 91 576 | 93 025 |
| 45 | c (99)| [kore](https://kore.io) (3.3) | 85 687 | 122 507 | 147 374 | 152 672 | 142 512 |
| 46 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 83 436 | 169 641 | 179 684 | 179 261 | 176 406 |
| 47 | go (1.14)| [air](https://github.com/aofei/air) (0.16) | 79 633 | 85 550 | 87 198 | 86 920 | 86 438 |
| 48 | swift (5.2)| [perfect](https://perfect.org) (3.1) | 78 240 | 86 256 | 92 234 | 92 463 | 91 742 |
| 49 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 77 722 | 85 329 | 83 443 | 83 019 | 81 584 |
| 50 | javascript (13.14)| [restana](https://github.com/jkyberneees/ana) (4.3) | 76 923 | 85 262 | 85 632 | 82 528 | 82 367 |
| 51 | javascript (13.14)| [rayo](https://rayo.js.org) (1.3) | 76 847 | 79 499 | 77 890 | 75 362 | 75 903 |
| 52 | javascript (13.14)| [polka](https://github.com/lukeed/polka) (0.5) | 76 011 | 81 196 | 78 091 | 72 012 | 73 628 |
| 53 | go (1.14)| [gf](https://goframe.org) (1.12) | 75 129 | 80 583 | 81 606 | 81 981 | 81 886 |
| 54 | java (8)| [javalin](https://javalin.io) (3.8) | 70 193 | 78 446 | 79 085 | 78 228 | 78 236 |
| 55 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 67 496 | 78 043 | 79 541 | 78 993 | 76 805 |
| 56 | javascript (13.14)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 66 288 | 69 698 | 67 467 | 66 994 | 68 191 |
| 57 | php (7.4)| [hyperf](https://www.hyperf.io) (1.1) | 65 597 | 68 180 | 69 062 | 68 349 | 68 380 |
| 58 | javascript (13.14)| [foxify](https://foxify.js.org) (0.1) | 65 559 | 69 213 | 67 609 | 65 858 | 65 439 |
| 59 | javascript (13.14)| [fastify](https://fastify.io) (2.14) | 65 338 | 68 702 | 65 898 | 64 996 | 64 837 |
| 60 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 64 343 | 74 686 | 79 349 | 78 419 | 77 547 |
| 61 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 64 187 | 65 794 | 67 441 | 66 987 | 66 851 |
| 62 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 63 130 | 63 521 | 59 909 | 56 460 | 58 538 |
| 63 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.2) | 62 681 | 70 169 | 68 230 | 68 462 | 66 943 |
| 64 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 62 077 | 70 437 | 72 781 | 71 912 | 71 008 |
| 65 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 60 872 | 64 166 | 66 625 | 66 556 | 66 508 |
| 66 | java (8)| [micronaut](https://micronaut.io) (1.2) | 60 683 | 69 218 | 69 431 | 67 138 | 66 057 |
| 67 | javascript (13.14)| [koa](https://koajs.com) (2.11) | 59 385 | 62 917 | 60 814 | 59 773 | 59 623 |
| 68 | swift (5.2)| [kitura](https://kitura.io) (2.9) | 57 514 | 56 997 | 57 205 | 55 893 | 55 765 |
| 69 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 56 473 | 56 617 | 56 615 | 56 041 | 56 038 |
| 70 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 55 539 | 61 397 | 61 251 | 59 976 | 60 736 |
| 71 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.26) | 54 579 | 60 102 | 60 236 | 58 041 | 57 778 |
| 72 | rust (1.43)| [nickel](https://nickel-org.github.io) (0.11) | 53 172 | 53 374 | 53 070 | 52 809 | 53 385 |
| 73 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 52 645 | 55 475 | 53 959 | 52 229 | 51 112 |
| 74 | javascript (13.14)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 52 631 | 55 045 | 56 270 | 57 709 | 55 297 |
| 75 | javascript (13.14)| [nestjs-fastify](https://nestjs.com) (7.0) | 52 301 | 58 748 | 57 212 | 56 912 | 56 832 |
| 76 | javascript (13.14)| [feathersjs](https://feathersjs.com) (4.5) | 51 966 | 52 783 | 50 583 | 50 992 | 49 216 |
| 77 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 51 872 | 53 504 | 55 294 | 54 672 | 54 270 |
| 78 | javascript (13.14)| [express](https://expressjs.com) (4.17) | 51 781 | 54 311 | 54 542 | 50 363 | 51 804 |
| 79 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 51 753 | 57 593 | 57 326 | 55 020 | 54 741 |
| 80 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 50 970 | 59 644 | 60 738 | 61 363 | 60 870 |
| 81 | swift (5.2)| [vapor](https://vapor.codes) (4.5) | 50 148 | 52 088 | 51 955 | 51 367 | 50 908 |
| 82 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 48 907 | 53 719 | 52 505 | 51 272 | 50 788 |
| 83 | javascript (13.14)| [moleculer](https://moleculer.services) (0.14) | 48 592 | 49 744 | 48 734 | 48 239 | 48 440 |
| 84 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 48 415 | 50 778 | 51 090 | 50 481 | 50 411 |
| 85 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 46 568 | 46 295 | 46 450 | 46 190 | 45 925 |
| 86 | php (7.4)| [imi](https://imiphp.com) (1.2) | 46 469 | 47 523 | 47 876 | 47 516 | 47 603 |
| 87 | python (3.8)| [hug](https://hug.rest) (2.6) | 44 713 | 47 795 | 47 793 | 47 035 | 46 727 |
| 88 | python (3.8)| [starlette](https://starlette.io) (0.13) | 44 103 | 47 651 | 47 255 | 45 536 | 45 197 |
| 89 | rust (1.43)| [gotham](https://gotham.rs) (0.4) | 42 952 | 48 888 | 50 769 | 52 860 | 54 030 |
| 90 | javascript (13.14)| [hapi](https://hapijs.com) (19.1) | 42 507 | 44 752 | 44 027 | 43 441 | 43 185 |
| 91 | javascript (13.14)| [nestjs-express](https://nestjs.com) (7.0) | 41 719 | 42 881 | 42 185 | 41 597 | 41 755 |
| 92 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 39 900 | 40 708 | 40 673 | 39 079 | 39 332 |
| 93 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 39 742 | 40 723 | 39 316 | 38 472 | 38 545 |
| 94 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 39 115 | 40 105 | 38 462 | 37 980 | 38 809 |
| 95 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 38 748 | 41 270 | 41 089 | 40 524 | 40 509 |
| 96 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 37 933 | 40 746 | 40 437 | 38 802 | 38 326 |
| 97 | javascript (13.14)| [restify](https://restify.com) (8.5) | 36 981 | 38 365 | 38 383 | 38 345 | 38 831 |
| 98 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.31) | 35 915 | 37 163 | 35 701 | 35 334 | 35 315 |
| 99 | php (7.4)| [swoft](https://swoft.org) (2.0) | 35 774 | 36 806 | 36 632 | 36 066 | 36 091 |
| 100 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 34 899 | 35 426 | 34 921 | 33 647 | 33 802 |
| 101 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 34 632 | 35 756 | 35 534 | 35 177 | 34 979 |
| 102 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 33 890 | 34 441 | 33 304 | 33 077 | 33 236 |
| 103 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 32 180 | 33 183 | 33 037 | 32 633 | 32 899 |
| 104 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 30 140 | 30 566 | 30 049 | 29 216 | 29 006 |
| 105 | crystal (0.34)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.15) | 29 736 | 27 028 | 25 511 | 21 907 | 21 264 |
| 106 | dart (2.7)| [aqueduct](https://aqueduct.io) (3.3) | 29 418 | 28 746 | 29 661 | 28 099 | 28 051 |
| 107 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 28 454 | 30 259 | 31 980 | 33 219 | 30 656 |
| 108 | python (3.8)| [responder](https://python-responder.org) (2.0) | 27 921 | 30 834 | 30 467 | 29 421 | 29 425 |
| 109 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.54) | 27 803 | 28 348 | 27 606 | 27 349 | 27 924 |
| 110 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 27 100 | 26 839 | 26 361 | 26 384 | 26 574 |
| 111 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 26 793 | 26 649 | 26 102 | 25 870 | 26 012 |
| 112 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 25 383 | 25 057 | 24 926 | 24 117 | 24 721 |
| 113 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 25 077 | 25 352 | 25 075 | 24 643 | 22 881 |
| 114 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 24 869 | 24 490 | 24 034 | 23 958 | 23 927 |
| 115 | rust (1.43)| [iron](https://ironframework.io) (0.6) | 23 834 | 23 925 | 23 991 | 23 834 | 23 783 |
| 116 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 23 351 | 26 170 | 24 794 | 24 661 | 25 077 |
| 117 | javascript (13.14)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 23 222 | 22 339 | 20 952 | 20 673 | 20 612 |
| 118 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 22 386 | 23 047 | 22 876 | 23 281 | 23 170 |
| 119 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 22 369 | 22 237 | 22 232 | 21 720 | 22 136 |
| 120 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 18 099 | 18 654 | 19 095 | 18 885 | 18 731 |
| 121 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 17 560 | 16 948 | 15 619 | 16 251 | 15 668 |
| 122 | java (8)| [blade](https://lets-blade.com) (2.0) | 16 897 | 19 940 | 19 626 | 18 219 | 15 671 |
| 123 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 15 778 | 15 543 | 15 578 | 15 604 | 15 527 |
| 124 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 14 394 | 14 330 | 14 359 | 14 153 | 14 249 |
| 125 | javascript (13.14)| [sails](https://sailsjs.com) (1.2) | 12 052 | 12 504 | 12 780 | 12 417 | 12 522 |
| 126 | swift (5.2)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 867 | 11 941 | 11 844 | 11 883 | 11 909 |
| 127 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 574 | 11 852 | 11 851 | 6 145 | 6 003 |
| 128 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 11 328 | 11 128 | 11 045 | 11 058 | 10 900 |
| 129 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 11 268 | 11 836 | 11 253 | 10 682 | 10 689 |
| 130 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 10 763 | 10 654 | 10 661 | 10 638 | 10 598 |
| 131 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 621 | 10 054 | 10 046 | 9 851 | 9 340 |
| 132 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 097 | 9 007 | 8 975 | 49 695 | 47 684 |
| 133 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 8 592 | 9 011 | 8 991 | 8 978 | 8 948 |
| 134 | python (3.8)| [django](https://djangoproject.com) (3.0) | 8 266 | 9 633 | 9 523 | 9 349 | 9 523 |
| 135 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 7 877 | 7 846 | 7 723 | 43 707 | 42 648 |
| 136 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 7 621 | 7 583 | 7 499 | 44 159 | 41 845 |
| 137 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 7 579 | 5 092 | 5 065 | 1 390 | 5 001 |
| 138 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 511 | 7 427 | 7 502 | 48 761 | 48 563 |
| 139 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 187 | 6 185 | 6 148 | 48 979 | 46 677 |
| 140 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 5 580 | 5 547 | 5 565 | 43 128 | 39 236 |
| 141 | php (7.4)| [slim](https://slimframework.com) (4.5) | 4 568 | 4 551 | 4 648 | 41 044 | 40 453 |
| 142 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 4 384 | 4 468 | 4 600 | 42 020 | 38 555 |
| 143 | php (7.4)| [lumen](https://lumen.laravel.com) (7.1) | 4 267 | 4 229 | 4 387 | 41 838 | 41 829 |
| 144 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 111 | 4 111 | 4 074 | 41 056 | 38 630 |
| 145 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 808 | 3 586 | 3 582 | 3 543 | 3 517 |
| 146 | pony (0.34)| [jennet](https://github.com/Theodus/jennet) (0.1) | 3 602 | 6 030 | 350 | 4 956 | 5 017 |
| 147 | crystal (0.34)| [onyx](https://onyxframework.org) (0.5) | 3 341 | 2 538 | 1 723 | 2 417 | 3 952 |
| 148 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 167 | 3 183 | 3 225 | 41 489 | 38 726 |
| 149 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 824 | 2 843 | 2 866 | 41 727 | 39 370 |
| 150 | julia (1.4)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 2 536 | 8 101 | 6 508 | 5 038 | 3 025 |
| 151 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 438 | 2 471 | 2 419 | 2 369 | 2 406 |
| 152 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 567 | 1 513 | 1 483 | 1 493 | 1 497 |
| 153 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 558 | 1 528 | 1 495 | 1 506 | 1 494 |
| 154 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 418 | 1 443 | 1 520 | 38 570 | 36 297 |
| 155 | crystal (0.34)| [lucky](https://luckyframework.org) (0.21) | 1 415 | 1 386 | 884 | 887 | 928 |
| 156 | php (7.4)| [laravel](https://laravel.com) (7.9) | 707 | 173 | 2 591 | 23 753 | 22 647 |
| 157 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 596 | 498 | 1 914 | 33 743 | 34 334 |
| 158 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 458 | 783 | 1 023 | 1 438 | 529 |

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

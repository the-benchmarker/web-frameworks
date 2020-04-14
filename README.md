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

:information_source:  Updated on **2020-04-14** :information_source:

> Benchmarking with [wrk](https://github.com/ioquatix/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (2.2) | 181 909 | 194 450 | 195 118 | 190 290 | 190 628 |
| 2 | php (7.4)| [simps](https://simps.io) (preview) | 169 068 | 185 705 | 186 351 | 183 263 | 183 014 |
| 3 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.9) | 159 078 | 168 009 | 170 886 | 165 526 | 164 820 |
| 4 | javascript (13.7)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.11) | 156 498 | 169 215 | 167 921 | 166 555 | 163 500 |
| 5 | crystal (0.34)| [toro](https://github.com/soveran/toro) (0.4) | 153 545 | 160 329 | 156 443 | 148 714 | 147 993 |
| 6 | crystal (0.34)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 153 431 | 160 062 | 156 898 | 148 615 | 147 800 |
| 7 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 153 323 | 161 677 | 164 610 | 159 914 | 159 052 |
| 8 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 153 264 | 170 170 | 173 839 | 165 638 | 165 156 |
| 9 | go (1.14)| [fiber](https://fiber.wiki) (1.9) | 152 550 | 156 035 | 153 168 | 144 271 | 144 560 |
| 10 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.0) | 152 497 | 161 444 | 164 327 | 159 767 | 159 649 |
| 11 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 152 018 | 160 222 | 163 448 | 158 927 | 158 757 |
| 12 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.0) | 151 975 | 160 271 | 163 175 | 158 813 | 158 753 |
| 13 | crystal (0.34)| [ricr](https://ricr-web.github.io/ricr) (0.1) | 151 477 | 158 383 | 154 467 | 145 531 | 145 066 |
| 14 | crystal (0.34)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 149 680 | 153 649 | 147 380 | 138 615 | 144 339 |
| 15 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 149 492 | 157 210 | 157 317 | 154 414 | 154 873 |
| 16 | javascript (13.7)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.0) | 145 269 | 157 805 | 163 244 | 155 358 | 159 742 |
| 17 | crystal (0.34)| [grip](https://github.com/grip-framework/grip) (0.28) | 142 394 | 148 293 | 143 787 | 135 714 | 133 886 |
| 18 | crystal (0.34)| [kemal](https://kemalcr.com) (0.26) | 142 283 | 147 398 | 143 844 | 135 292 | 133 498 |
| 19 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 142 272 | 152 808 | 153 883 | 151 583 | 151 380 |
| 20 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 141 684 | 151 574 | 151 745 | 147 761 | 147 979 |
| 21 | rust (1.41)| [actix](https://actix.rs) (2.0) | 139 649 | 141 045 | 135 937 | 131 369 | 133 946 |
| 22 | crystal (0.34)| [amber](https://amberframework.org) (0.34) | 134 817 | 140 148 | 136 442 | 129 428 | 128 422 |
| 23 | crystal (0.34)| [orion](https://github.com/obsidian/orion) (2.3) | 130 930 | 135 151 | 129 230 | 119 301 | 117 367 |
| 24 | crystal (0.34)| [lucky](https://luckyframework.org) (0.2) | 129 659 | 133 824 | 128 073 | 118 351 | 118 207 |
| 25 | crystal (0.34)| [athena](https://github.com/athena-framework/athena) (0.8) | 121 841 | 123 582 | 117 422 | 105 059 | 103 505 |
| 26 | java (8)| [act](https://actframework.org) (1.8) | 108 779 | 118 254 | 122 878 | 121 993 | 121 010 |
| 27 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 107 921 | 108 153 | 111 326 | 110 523 | 109 996 |
| 28 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 106 438 | 106 312 | 109 406 | 109 073 | 108 635 |
| 29 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 101 726 | 100 921 | 103 727 | 103 158 | 103 267 |
| 30 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 101 579 | 105 878 | 108 348 | 107 125 | 106 775 |
| 31 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 100 046 | 101 191 | 103 515 | 103 225 | 102 946 |
| 32 | go (1.14)| [violetear](https://violetear.org) (7.0) | 99 554 | 99 610 | 102 459 | 102 284 | 102 074 |
| 33 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 99 366 | 118 355 | 122 010 | 121 976 | 121 657 |
| 34 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 98 260 | 97 070 | 100 210 | 100 125 | 99 980 |
| 35 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 97 875 | 101 941 | 103 484 | 101 437 | 101 287 |
| 36 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 97 553 | 96 672 | 99 660 | 99 449 | 99 382 |
| 37 | c (99)| [kore](https://kore.io) (3.3) | 96 528 | 117 750 | 147 560 | 155 560 | 139 864 |
| 38 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 95 431 | 93 829 | 96 593 | 96 745 | 96 410 |
| 39 | go (1.14)| [beego](https://beego.me) (1.12) | 94 167 | 97 416 | 100 106 | 99 604 | 99 524 |
| 40 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 93 648 | 97 512 | 99 572 | 99 223 | 99 099 |
| 41 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 93 023 | 92 466 | 95 580 | 95 784 | 95 128 |
| 42 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 92 622 | 98 971 | 99 697 | 97 847 | 98 258 |
| 43 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 86 928 | 89 406 | 90 977 | 91 191 | 90 387 |
| 44 | go (1.14)| [air](https://github.com/aofei/air) (0.15) | 81 146 | 85 338 | 86 990 | 86 861 | 86 708 |
| 45 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 78 457 | 91 197 | 91 046 | 87 287 | 86 754 |
| 46 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.3) | 75 875 | 86 859 | 86 698 | 82 485 | 80 412 |
| 47 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 75 768 | 87 596 | 90 919 | 87 779 | 85 131 |
| 48 | go (1.14)| [gf](https://goframe.org) (1.12) | 75 029 | 80 296 | 81 561 | 81 361 | 81 129 |
| 49 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 72 586 | 160 159 | 172 258 | 180 260 | 178 804 |
| 50 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 71 151 | 75 299 | 74 169 | 72 303 | 73 003 |
| 51 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 70 038 | 78 564 | 78 617 | 76 211 | 76 549 |
| 52 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 67 280 | 77 878 | 76 103 | 71 457 | 71 426 |
| 53 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 66 567 | 74 880 | 74 711 | 75 226 | 74 462 |
| 54 | java (8)| [javalin](https://javalin.io) (3.5) | 65 857 | 74 748 | 73 078 | 71 511 | 70 796 |
| 55 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 63 660 | 67 528 | 67 467 | 67 004 | 66 913 |
| 56 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 63 184 | 74 817 | 77 206 | 76 621 | 75 772 |
| 57 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 63 161 | 63 415 | 62 777 | 61 235 | 61 062 |
| 58 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 62 381 | 63 553 | 58 890 | 60 297 | 62 473 |
| 59 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 62 040 | 68 667 | 68 420 | 65 760 | 66 901 |
| 60 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 61 011 | 63 448 | 66 039 | 65 798 | 65 791 |
| 61 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 60 224 | 71 643 | 72 561 | 72 260 | 69 229 |
| 62 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 58 576 | 62 082 | 61 425 | 58 826 | 56 721 |
| 63 | java (8)| [micronaut](https://micronaut.io) (1.2) | 57 060 | 63 734 | 62 669 | 60 710 | 61 317 |
| 64 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.24) | 56 634 | 62 625 | 63 741 | 59 976 | 59 744 |
| 65 | javascript (13.7)| [fastify](https://fastify.io) (2.13) | 56 294 | 62 338 | 59 009 | 56 769 | 55 991 |
| 66 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 56 055 | 55 687 | 55 470 | 56 611 | 55 895 |
| 67 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 55 839 | 59 855 | 59 306 | 57 831 | 58 117 |
| 68 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 53 925 | 60 611 | 59 197 | 55 537 | 53 762 |
| 69 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 52 976 | 60 426 | 61 858 | 62 919 | 63 309 |
| 70 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 52 839 | 54 239 | 54 315 | 53 972 | 54 048 |
| 71 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 51 875 | 57 334 | 57 271 | 55 142 | 55 027 |
| 72 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 50 906 | 53 982 | 56 415 | 54 254 | 52 538 |
| 73 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 50 196 | 56 926 | 55 246 | 57 926 | 59 285 |
| 74 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 49 771 | 54 361 | 52 554 | 51 274 | 51 388 |
| 75 | javascript (13.7)| [feathersjs](https://feathersjs.com) (4.5) | 49 637 | 51 133 | 51 280 | 50 215 | 49 445 |
| 76 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 48 720 | 47 958 | 48 268 | 47 186 | 47 100 |
| 77 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 48 534 | 48 940 | 48 669 | 48 044 | 47 579 |
| 78 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 48 225 | 49 720 | 49 332 | 46 876 | 46 196 |
| 79 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 47 584 | 49 286 | 48 842 | 49 029 | 48 909 |
| 80 | javascript (13.7)| [moleculer](https://moleculer.services) (0.14) | 47 545 | 50 034 | 48 760 | 47 976 | 47 860 |
| 81 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 46 389 | 44 880 | 41 048 | 43 111 | 45 260 |
| 82 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 46 289 | 46 313 | 46 357 | 46 452 | 45 246 |
| 83 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 46 248 | 48 547 | 48 062 | 47 119 | 46 519 |
| 84 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 42 685 | 48 705 | 51 457 | 52 502 | 52 496 |
| 85 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 42 247 | 44 096 | 43 481 | 42 943 | 42 887 |
| 86 | python (3.8)| [hug](https://hug.rest) (2.6) | 42 090 | 43 373 | 46 136 | 44 642 | 47 413 |
| 87 | php (7.4)| [imi](https://imiphp.com) (1.0) | 40 745 | 41 698 | 42 114 | 41 813 | 41 873 |
| 88 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 40 511 | 41 238 | 40 931 | 39 605 | 39 672 |
| 89 | python (3.8)| [starlette](https://starlette.io) (0.13) | 39 410 | 47 222 | 46 781 | 44 649 | 42 451 |
| 90 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 39 220 | 39 530 | 37 676 | 36 995 | 37 953 |
| 91 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 39 026 | 40 518 | 38 476 | 38 128 | 38 267 |
| 92 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 38 588 | 41 686 | 41 744 | 41 253 | 41 187 |
| 93 | javascript (13.7)| [restify](https://restify.com) (8.5) | 37 078 | 38 706 | 37 568 | 37 338 | 37 896 |
| 94 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0b2) | 36 951 | 40 872 | 39 754 | 38 537 | 38 148 |
| 95 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 36 150 | 36 393 | 36 191 | 34 833 | 34 634 |
| 96 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.3) | 35 638 | 37 314 | 35 531 | 35 694 | 35 534 |
| 97 | php (7.4)| [swoft](https://swoft.org) (2.0) | 35 534 | 36 602 | 36 404 | 35 924 | 35 895 |
| 98 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 33 391 | 33 364 | 32 404 | 32 333 | 32 345 |
| 99 | dart (2.7)| [aqueduct](https://aqueduct.io) (3.3) | 31 373 | 30 595 | 31 499 | 30 812 | 30 992 |
| 100 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 31 086 | 31 250 | 30 968 | 29 878 | 29 970 |
| 101 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 28 190 | 27 682 | 27 072 | 27 177 | 27 054 |
| 102 | python (3.8)| [responder](https://python-responder.org) (2.0) | 27 816 | 30 392 | 30 247 | 29 195 | 29 193 |
| 103 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 27 531 | 27 504 | 26 908 | 26 819 | 26 889 |
| 104 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.54) | 26 541 | 29 793 | 28 820 | 27 196 | 26 811 |
| 105 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 26 457 | 28 506 | 28 550 | 27 473 | 27 487 |
| 106 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 25 584 | 27 164 | 26 925 | 26 529 | 26 595 |
| 107 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 25 112 | 24 398 | 23 994 | 23 892 | 23 914 |
| 108 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 23 611 | 23 935 | 23 636 | 23 591 | 23 726 |
| 109 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 22 571 | 23 046 | 23 983 | 23 731 | 23 426 |
| 110 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 372 | 21 335 | 19 724 | 20 046 | 19 895 |
| 111 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 21 403 | 22 387 | 22 968 | 20 409 | 22 429 |
| 112 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 18 667 | 18 809 | 18 781 | 18 648 | 18 595 |
| 113 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 18 180 | 18 342 | 18 435 | 18 359 | 18 469 |
| 114 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 17 358 | 23 842 | 32 745 | 33 859 | 32 846 |
| 115 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 15 665 | 15 436 | 15 395 | 15 451 | 15 394 |
| 116 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 15 477 | 15 525 | 13 798 | 14 438 | 14 336 |
| 117 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 14 193 | 14 397 | 14 317 | 14 290 | 14 346 |
| 118 | javascript (13.7)| [sails](https://sailsjs.com) (1.2) | 11 975 | 12 328 | 11 923 | 12 933 | 12 712 |
| 119 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 11 940 | 12 129 | 11 313 | 10 745 | 10 764 |
| 120 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 11 270 | 11 131 | 11 097 | 10 994 | 11 001 |
| 121 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 10 733 | 10 952 | 10 929 | 10 827 | 11 002 |
| 122 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 10 617 | 10 649 | 10 560 | 10 538 | 10 516 |
| 123 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 082 | 8 962 | 8 938 | 50 843 | 47 658 |
| 124 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 8 826 | 9 881 | 9 093 | 9 190 | 9 181 |
| 125 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 8 375 | 9 099 | 9 033 | 8 970 | 9 075 |
| 126 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 7 824 | 7 763 | 7 669 | 43 495 | 40 967 |
| 127 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 450 | 7 404 | 7 311 | 43 665 | 42 628 |
| 128 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 441 | 7 433 | 7 368 | 49 522 | 47 587 |
| 129 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 7 258 | 9 817 | 7 922 | 4 920 | 4 903 |
| 130 | python (3.8)| [django](https://djangoproject.com) (3.0) | 7 161 | 9 377 | 9 490 | 9 510 | 9 189 |
| 131 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 5 580 | 5 585 | 5 686 | 42 418 | 39 443 |
| 132 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 630 | 4 647 | 4 740 | 42 796 | 39 763 |
| 133 | php (7.4)| [lumen](https://lumen.laravel.com) (7.0) | 4 282 | 4 284 | 4 350 | 42 332 | 40 222 |
| 134 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 095 | 4 068 | 4 177 | 41 476 | 40 107 |
| 135 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 805 | 3 598 | 3 600 | 3 582 | 3 562 |
| 136 | crystal (0.34)| [onyx](https://onyxframework.org) (0.5) | 3 228 | 3 325 | 3 314 | 3 319 | 2 470 |
| 137 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 155 | 3 191 | 3 248 | 40 577 | 38 062 |
| 138 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 844 | 2 876 | 2 903 | 40 690 | 38 978 |
| 139 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 318 | 2 373 | 2 381 | 2 316 | 2 333 |
| 140 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 1 922 | 5 626 | 4 431 | 3 470 | 2 212 |
| 141 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 572 | 1 604 | 1 582 | 1 564 | 1 566 |
| 142 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 530 | 1 489 | 1 477 | 1 446 | 1 407 |
| 143 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 424 | 1 454 | 1 516 | 38 185 | 36 482 |
| 144 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 885 | 825 | 1 572 | 1 142 | 587 |
| 145 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 565 | 459 | 1 461 | 33 165 | 32 868 |
| 146 | php (7.4)| [laravel](https://laravel.com) (7.5) | 184 | 163 | 3 866 | 23 579 | 21 997 |

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

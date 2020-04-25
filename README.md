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

:warning::warning::warning::warning:

[Vapor](https://vapor.codes) is hidden due, see https://github.com/the-benchmarker/web-frameworks/issues/2575

:warning::warning::warning::warning:

:information_source:  Updated on **2020-04-25** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | javascript (13.13)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.11) | 189 060 | 202 174 | 203 477 | 200 578 | 201 275 |
| 2 | java (8)| [jooby](https://jooby.io) (2.8) | 177 169 | 192 009 | 192 372 | 187 952 | 186 744 |
| 3 | php (7.4)| [simps](https://simps.io) (1.0) | 175 340 | 188 260 | 188 754 | 186 367 | 186 112 |
| 4 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (2.2) | 174 113 | 183 620 | 189 253 | 181 532 | 176 198 |
| 5 | javascript (13.13)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.0) | 173 379 | 187 522 | 191 206 | 188 782 | 181 822 |
| 6 | javascript (13.13)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 171 506 | 182 071 | 177 896 | 177 850 | 179 866 |
| 7 | kotlin (1.3)| [kooby](https://jooby.io) (2.8) | 166 517 | 179 340 | 178 584 | 172 893 | 174 029 |
| 8 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.12) | 157 297 | 166 028 | 170 036 | 164 130 | 165 029 |
| 9 | crystal (0.34)| [toro](https://github.com/soveran/toro) (0.4) | 155 740 | 162 265 | 158 221 | 150 296 | 144 652 |
| 10 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 155 319 | 170 737 | 170 596 | 165 492 | 163 963 |
| 11 | go (1.14)| [fiber](https://fiber.wiki) (1.9) | 154 440 | 156 657 | 153 953 | 145 559 | 143 858 |
| 12 | crystal (0.34)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 153 641 | 160 439 | 157 012 | 148 385 | 148 241 |
| 13 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.0) | 152 808 | 161 500 | 164 613 | 159 551 | 159 193 |
| 14 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 152 309 | 161 081 | 164 241 | 158 572 | 158 726 |
| 15 | crystal (0.34)| [ricr](https://ricr-web.github.io/ricr) (0.1) | 151 385 | 157 842 | 154 245 | 145 201 | 145 038 |
| 16 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.0) | 151 366 | 160 771 | 163 650 | 159 225 | 157 821 |
| 17 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 150 680 | 160 804 | 163 363 | 158 717 | 158 364 |
| 18 | crystal (0.34)| [spider-gazelle](https://spider-gazelle.net) (2.4) | 150 292 | 156 759 | 152 674 | 144 641 | 144 460 |
| 19 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 150 253 | 162 295 | 163 717 | 161 432 | 160 828 |
| 20 | crystal (0.34)| [kemal](https://kemalcr.com) (0.26) | 143 804 | 149 795 | 146 678 | 139 006 | 138 021 |
| 21 | crystal (0.34)| [grip](https://github.com/grip-framework/grip) (0.28) | 139 993 | 145 587 | 142 003 | 133 362 | 132 163 |
| 22 | crystal (0.34)| [amber](https://amberframework.org) (0.34) | 136 214 | 141 399 | 136 314 | 129 734 | 129 393 |
| 23 | rust (1.43)| [actix](https://actix.rs) (2.0) | 135 759 | 135 732 | 135 910 | 127 947 | 125 941 |
| 24 | crystal (0.34)| [lucky](https://luckyframework.org) (0.20) | 131 195 | 134 749 | 129 946 | 121 007 | 120 864 |
| 25 | crystal (0.34)| [orion](https://github.com/obsidian/orion) (2.3) | 130 428 | 133 571 | 128 184 | 117 359 | 116 183 |
| 26 | crystal (0.34)| [athena](https://github.com/athena-framework/athena) (0.8) | 124 893 | 126 745 | 120 456 | 108 021 | 106 410 |
| 27 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 123 389 | 135 256 | 138 305 | 134 002 | 134 786 |
| 28 | java (8)| [act](https://actframework.org) (1.8) | 119 178 | 131 643 | 131 489 | 127 967 | 127 812 |
| 29 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 116 280 | 175 914 | 180 039 | 179 885 | 173 035 |
| 30 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 107 860 | 107 947 | 110 964 | 110 794 | 110 259 |
| 31 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 106 314 | 106 258 | 109 478 | 109 169 | 109 024 |
| 32 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 102 176 | 106 448 | 108 211 | 107 606 | 106 917 |
| 33 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 102 118 | 101 260 | 104 318 | 103 935 | 103 549 |
| 34 | c (99)| [kore](https://kore.io) (3.3) | 101 709 | 138 236 | 148 289 | 139 627 | 150 174 |
| 35 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 99 909 | 101 202 | 103 787 | 102 954 | 102 603 |
| 36 | go (1.14)| [violetear](https://violetear.org) (7.0) | 99 191 | 99 601 | 102 681 | 102 587 | 102 244 |
| 37 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 98 473 | 97 098 | 100 168 | 100 313 | 99 530 |
| 38 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 98 311 | 102 440 | 104 027 | 102 103 | 102 135 |
| 39 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 97 681 | 97 372 | 100 053 | 100 206 | 99 686 |
| 40 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 96 537 | 114 345 | 117 737 | 118 212 | 117 410 |
| 41 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 95 603 | 93 662 | 96 951 | 97 363 | 96 604 |
| 42 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 94 701 | 101 953 | 102 560 | 100 267 | 101 301 |
| 43 | go (1.14)| [beego](https://beego.me) (1.12) | 94 197 | 97 335 | 100 653 | 100 400 | 99 893 |
| 44 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 93 430 | 97 573 | 100 268 | 99 304 | 98 521 |
| 45 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 93 201 | 92 473 | 93 505 | 94 133 | 95 877 |
| 46 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 88 291 | 91 994 | 92 946 | 92 283 | 92 211 |
| 47 | javascript (13.13)| [0http](https://github.com/jkyberneees/0http) (2.2) | 85 205 | 92 906 | 93 557 | 88 176 | 86 124 |
| 48 | javascript (13.13)| [polka](https://github.com/lukeed/polka) (0.5) | 82 588 | 87 449 | 85 228 | 83 428 | 82 504 |
| 49 | javascript (13.13)| [restana](https://github.com/jkyberneees/ana) (4.3) | 81 821 | 90 649 | 91 853 | 89 284 | 88 131 |
| 50 | javascript (13.13)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 80 817 | 92 474 | 92 099 | 87 130 | 86 858 |
| 51 | go (1.14)| [air](https://github.com/aofei/air) (0.15) | 80 257 | 84 257 | 85 789 | 85 900 | 85 887 |
| 52 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 78 136 | 86 631 | 83 988 | 83 034 | 81 671 |
| 53 | javascript (13.13)| [rayo](https://rayo.js.org) (1.3) | 75 513 | 78 635 | 78 424 | 75 569 | 74 314 |
| 54 | go (1.14)| [gf](https://goframe.org) (1.12) | 75 416 | 80 673 | 82 228 | 81 999 | 81 255 |
| 55 | java (8)| [javalin](https://javalin.io) (3.8) | 73 926 | 78 921 | 78 268 | 77 290 | 77 729 |
| 56 | swift (5.2)| [perfect](https://perfect.org) (3.1) | 71 404 | 80 439 | 85 863 | 86 081 | 84 225 |
| 57 | javascript (13.13)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 68 572 | 73 154 | 71 932 | 69 963 | 69 890 |
| 58 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 68 043 | 75 656 | 75 950 | 74 336 | 74 458 |
| 59 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 65 724 | 76 664 | 78 316 | 77 520 | 77 258 |
| 60 | php (7.4)| [hyperf](https://www.hyperf.io) (1.1) | 65 635 | 68 650 | 69 162 | 68 689 | 68 739 |
| 61 | javascript (13.13)| [foxify](https://foxify.js.org) (0.1) | 65 502 | 68 990 | 67 622 | 65 821 | 65 479 |
| 62 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.2) | 65 495 | 71 774 | 70 658 | 69 632 | 69 020 |
| 63 | javascript (13.13)| [fastify](https://fastify.io) (2.13) | 65 376 | 71 486 | 69 053 | 66 226 | 66 805 |
| 64 | java (8)| [micronaut](https://micronaut.io) (1.2) | 62 511 | 68 702 | 68 274 | 65 698 | 65 750 |
| 65 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 61 398 | 67 368 | 67 478 | 67 141 | 66 843 |
| 66 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 60 746 | 63 792 | 65 879 | 66 456 | 66 121 |
| 67 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 59 667 | 61 738 | 60 174 | 58 949 | 59 158 |
| 68 | javascript (13.13)| [koa](https://koajs.com) (2.11) | 59 191 | 62 583 | 60 423 | 59 187 | 60 360 |
| 69 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 57 203 | 60 859 | 61 082 | 60 082 | 60 004 |
| 70 | javascript (13.13)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 56 232 | 60 630 | 59 427 | 56 048 | 55 444 |
| 71 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.26) | 56 173 | 61 192 | 61 252 | 58 709 | 58 692 |
| 72 | rust (1.43)| [nickel](https://nickel-org.github.io) (0.11) | 55 519 | 56 168 | 55 104 | 55 544 | 55 496 |
| 73 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 55 045 | 63 253 | 62 459 | 62 187 | 61 654 |
| 74 | javascript (13.13)| [feathersjs](https://feathersjs.com) (4.5) | 54 634 | 57 121 | 55 341 | 54 653 | 54 588 |
| 75 | javascript (13.13)| [express](https://expressjs.com) (4.17) | 54 379 | 57 259 | 55 270 | 54 485 | 54 256 |
| 76 | swift (5.2)| [kitura](https://kitura.io) (2.9) | 53 781 | 53 024 | 53 918 | 54 038 | 54 380 |
| 77 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 53 658 | 54 906 | 54 811 | 54 441 | 54 569 |
| 78 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 51 557 | 52 238 | 52 760 | 51 125 | 48 941 |
| 79 | javascript (13.13)| [nestjs-fastify](https://nestjs.com) (7.0) | 50 927 | 61 518 | 61 342 | 59 838 | 59 286 |
| 80 | scala (2.12)| [http4s](https://http4s.org) (0.21) | 50 456 | 55 733 | 53 409 | 51 699 | 51 000 |
| 81 | javascript (13.13)| [moleculer](https://moleculer.services) (0.14) | 49 516 | 51 223 | 49 902 | 49 469 | 48 396 |
| 82 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 48 909 | 50 904 | 50 712 | 50 105 | 50 071 |
| 83 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 48 515 | 53 995 | 53 945 | 52 060 | 51 654 |
| 84 | php (7.4)| [imi](https://imiphp.com) (1.2) | 46 313 | 48 531 | 48 848 | 48 480 | 48 351 |
| 85 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 46 255 | 46 190 | 46 451 | 46 188 | 45 423 |
| 86 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 45 651 | 53 218 | 52 878 | 50 745 | 50 842 |
| 87 | python (3.8)| [hug](https://hug.rest) (2.6) | 43 027 | 47 883 | 44 284 | 47 203 | 47 202 |
| 88 | rust (1.43)| [gotham](https://gotham.rs) (0.4) | 42 003 | 49 232 | 51 086 | 53 137 | 53 799 |
| 89 | javascript (13.13)| [hapi](https://hapijs.com) (19.1) | 41 161 | 43 888 | 42 957 | 43 295 | 42 621 |
| 90 | python (3.8)| [starlette](https://starlette.io) (0.13) | 40 929 | 43 577 | 43 163 | 41 701 | 41 667 |
| 91 | javascript (13.13)| [restify](https://restify.com) (8.5) | 40 511 | 40 931 | 41 042 | 40 909 | 41 523 |
| 92 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 39 558 | 40 617 | 38 735 | 38 463 | 38 254 |
| 93 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 39 551 | 39 491 | 38 212 | 37 694 | 38 356 |
| 94 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 39 237 | 41 355 | 41 171 | 40 707 | 40 811 |
| 95 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 39 055 | 37 003 | 37 839 | 38 789 | 38 839 |
| 96 | javascript (13.13)| [nestjs-express](https://nestjs.com) (7.0) | 38 952 | 40 161 | 39 781 | 39 481 | 39 660 |
| 97 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0b2) | 36 507 | 40 939 | 40 503 | 38 905 | 38 914 |
| 98 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.31) | 36 252 | 37 274 | 35 806 | 35 756 | 35 667 |
| 99 | php (7.4)| [swoft](https://swoft.org) (2.0) | 34 963 | 36 406 | 36 254 | 35 849 | 35 769 |
| 100 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 34 101 | 36 841 | 37 073 | 34 857 | 35 075 |
| 101 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 33 547 | 33 462 | 32 412 | 32 126 | 32 387 |
| 102 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 31 764 | 33 073 | 30 731 | 30 071 | 31 487 |
| 103 | dart (2.7)| [aqueduct](https://aqueduct.io) (3.3) | 30 643 | 29 819 | 30 617 | 30 179 | 29 956 |
| 104 | crystal (0.34)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.15) | 29 714 | 26 613 | 23 650 | 22 070 | 21 090 |
| 105 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.54) | 29 526 | 30 750 | 30 447 | 29 334 | 29 323 |
| 106 | python (3.8)| [responder](https://python-responder.org) (2.0) | 28 719 | 30 824 | 30 555 | 29 513 | 29 537 |
| 107 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 28 178 | 27 660 | 27 005 | 27 284 | 27 258 |
| 108 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 27 785 | 28 497 | 27 997 | 27 894 | 27 739 |
| 109 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 25 861 | 25 758 | 24 662 | 25 474 | 25 617 |
| 110 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 25 410 | 27 605 | 27 415 | 26 806 | 26 701 |
| 111 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 25 176 | 24 547 | 25 366 | 25 086 | 25 161 |
| 112 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 25 128 | 24 630 | 24 082 | 24 074 | 24 246 |
| 113 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 24 624 | 25 184 | 24 946 | 24 504 | 24 414 |
| 114 | rust (1.43)| [iron](https://ironframework.io) (0.6) | 23 920 | 23 934 | 23 871 | 24 061 | 23 887 |
| 115 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 23 760 | 23 009 | 23 940 | 23 821 | 22 845 |
| 116 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 23 281 | 23 696 | 23 503 | 23 095 | 23 124 |
| 117 | javascript (13.13)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 23 012 | 21 980 | 20 609 | 20 503 | 20 523 |
| 118 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 18 945 | 18 499 | 18 378 | 18 030 | 17 874 |
| 119 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 18 094 | 18 359 | 18 493 | 18 315 | 18 334 |
| 120 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 15 748 | 15 559 | 15 530 | 15 499 | 15 564 |
| 121 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 14 561 | 14 090 | 14 347 | 14 291 | 14 259 |
| 122 | javascript (13.13)| [sails](https://sailsjs.com) (1.2) | 13 036 | 13 560 | 13 417 | 13 291 | 12 804 |
| 123 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 12 156 | 12 009 | 11 259 | 10 752 | 10 708 |
| 124 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 11 476 | 11 249 | 11 219 | 11 130 | 11 119 |
| 125 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 462 | 11 746 | 11 757 | 11 759 | 7 890 |
| 126 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 10 672 | 10 561 | 10 626 | 10 565 | 10 577 |
| 127 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 878 | 14 850 | 15 024 | 10 725 | 10 026 |
| 128 | swift (5.2)| [swifter](https://github.com/httpswift/swifter) (1.4) | 9 830 | 10 600 | 10 903 | 11 109 | 10 246 |
| 129 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 197 | 9 153 | 8 958 | 51 470 | 49 640 |
| 130 | python (3.8)| [django](https://djangoproject.com) (3.0) | 8 999 | 9 892 | 9 320 | 9 660 | 9 672 |
| 131 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 8 023 | 9 119 | 9 042 | 9 014 | 9 059 |
| 132 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 7 868 | 7 843 | 7 702 | 42 570 | 40 132 |
| 133 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 519 | 7 549 | 7 599 | 49 776 | 48 515 |
| 134 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 499 | 7 441 | 7 403 | 42 504 | 41 696 |
| 135 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 7 173 | 8 431 | 8 263 | 8 287 | 8 296 |
| 136 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 300 | 6 190 | 6 211 | 50 405 | 47 794 |
| 137 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 5 596 | 5 571 | 5 689 | 42 996 | 39 791 |
| 138 | crystal (0.34)| [onyx](https://onyxframework.org) (0.5) | 4 971 | 5 090 | 5 085 | 5 150 | 5 077 |
| 139 | php (7.4)| [slim](https://slimframework.com) (4.5) | 4 604 | 4 626 | 4 689 | 42 778 | 39 698 |
| 140 | php (7.4)| [lumen](https://lumen.laravel.com) (7.0) | 4 332 | 4 354 | 4 384 | 42 445 | 40 180 |
| 141 | pony (0.33)| [jennet](https://github.com/Theodus/jennet) (0.1) | 4 209 | 8 526 | 9 105 | 8 433 | 8 492 |
| 142 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 109 | 4 072 | 4 246 | 42 297 | 39 011 |
| 143 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 868 | 3 640 | 3 616 | 3 370 | 3 544 |
| 144 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 156 | 3 160 | 3 210 | 41 378 | 39 513 |
| 145 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 825 | 2 837 | 2 907 | 41 157 | 38 802 |
| 146 | julia (1.4)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 2 693 | 8 069 | 6 593 | 5 045 | 3 043 |
| 147 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 388 | 2 409 | 2 377 | 2 387 | 2 377 |
| 148 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 603 | 1 624 | 1 600 | 1 591 | 1 584 |
| 149 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 542 | 1 504 | 1 495 | 1 462 | 1 463 |
| 150 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 424 | 1 459 | 1 486 | 38 302 | 36 166 |
| 151 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 874 | 1 425 | 1 555 | 1 252 | 604 |
| 152 | php (7.4)| [laravel](https://laravel.com) (7.7) | 823 | 158 | 2 250 | 23 612 | 21 491 |
| 153 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 568 | 463 | 1 528 | 33 202 | 33 380 |

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

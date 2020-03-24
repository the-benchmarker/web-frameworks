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

:information_source:  Updated on **2020-03-24** :information_source:

> Benchmarking with [wrk](https://github.com/ioquatix/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 180 195 | 194 848 | 195 765 | 192 452 | 192 008 |
| 2 | javascript (13.7)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.1) | 179 012 | 182 730 | 189 544 | 188 937 | 193 442 |
| 3 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 177 532 | 187 257 | 189 484 | 188 192 | 186 358 |
| 4 | javascript (13.7)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.0) | 168 633 | 184 416 | 183 496 | 182 149 | 175 118 |
| 5 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.9) | 164 913 | 173 989 | 177 162 | 169 394 | 171 745 |
| 6 | go (1.14)| [fiber](https://fiber.wiki) (1.8) | 164 101 | 166 986 | 165 497 | 157 895 | 157 421 |
| 7 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 159 678 | 168 241 | 171 362 | 167 375 | 166 506 |
| 8 | go (1.14)| [router](https://github.com/fasthttp/router) (0.7) | 159 199 | 167 768 | 171 404 | 166 661 | 167 246 |
| 9 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 158 412 | 167 102 | 174 353 | 168 774 | 168 650 |
| 10 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.4) | 158 166 | 166 962 | 170 293 | 166 330 | 165 881 |
| 11 | crystal (0.33)| [toro](https://github.com/soveran/toro) (0.4) | 158 078 | 166 261 | 164 620 | 155 432 | 154 430 |
| 12 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 157 453 | 166 095 | 169 422 | 165 004 | 164 994 |
| 13 | crystal (0.33)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 156 366 | 164 716 | 162 364 | 153 951 | 153 640 |
| 14 | crystal (0.33)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 153 906 | 162 122 | 160 009 | 151 320 | 151 319 |
| 15 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 147 032 | 157 624 | 158 729 | 156 886 | 156 376 |
| 16 | crystal (0.33)| [kemal](https://kemalcr.com) (0.26) | 146 883 | 154 174 | 151 353 | 141 985 | 141 861 |
| 17 | crystal (0.33)| [grip](https://github.com/grip-framework/grip) (0.28) | 146 423 | 154 337 | 151 295 | 142 925 | 141 190 |
| 18 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 143 347 | 154 382 | 155 233 | 150 517 | 151 214 |
| 19 | crystal (0.33)| [amber](https://amberframework.org) (0.33) | 139 578 | 147 114 | 143 896 | 134 613 | 134 841 |
| 20 | crystal (0.33)| [lucky](https://luckyframework.org) (0.18) | 137 368 | 142 804 | 138 037 | 126 887 | 126 608 |
| 21 | crystal (0.33)| [orion](https://github.com/obsidian/orion) (2.1) | 135 024 | 140 302 | 134 876 | 124 568 | 124 066 |
| 22 | crystal (0.33)| [athena](https://github.com/athena-framework/athena) (0.8) | 125 703 | 128 613 | 122 013 | 112 824 | 112 213 |
| 23 | java (8)| [act](https://actframework.org) (1.8) | 120 020 | 135 824 | 134 155 | 125 487 | 130 967 |
| 24 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 113 087 | 112 704 | 116 619 | 116 321 | 116 028 |
| 25 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 111 263 | 110 675 | 114 640 | 114 489 | 114 204 |
| 26 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.0) | 106 036 | 104 411 | 108 477 | 108 545 | 108 452 |
| 27 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 105 919 | 109 996 | 113 238 | 112 465 | 112 327 |
| 28 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 103 885 | 104 635 | 107 827 | 107 211 | 107 003 |
| 29 | go (1.14)| [violetear](https://violetear.org) (7.0) | 103 369 | 103 026 | 106 804 | 107 330 | 106 661 |
| 30 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 102 257 | 122 473 | 127 248 | 128 247 | 127 738 |
| 31 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 101 640 | 100 317 | 103 925 | 104 766 | 104 341 |
| 32 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 101 467 | 100 269 | 104 064 | 104 632 | 104 061 |
| 33 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 100 705 | 104 578 | 106 575 | 105 473 | 104 381 |
| 34 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 98 963 | 97 709 | 100 726 | 101 544 | 101 352 |
| 35 | c (99)| [kore](https://kore.io) (3.3) | 98 948 | 155 107 | 137 212 | 157 469 | 157 638 |
| 36 | go (1.14)| [beego](https://beego.me) (1.12) | 98 168 | 101 613 | 105 241 | 105 331 | 104 904 |
| 37 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 96 338 | 100 228 | 103 016 | 103 089 | 102 638 |
| 38 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 96 271 | 95 502 | 99 483 | 100 295 | 99 958 |
| 39 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 95 932 | 104 434 | 104 868 | 102 092 | 102 542 |
| 40 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 92 432 | 103 773 | 102 781 | 99 470 | 100 185 |
| 41 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 91 164 | 98 860 | 99 021 | 97 199 | 94 228 |
| 42 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 89 538 | 185 902 | 184 308 | 185 777 | 181 238 |
| 43 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.1) | 88 691 | 97 059 | 96 673 | 93 117 | 92 345 |
| 44 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 87 737 | 92 224 | 90 828 | 88 358 | 85 990 |
| 45 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 86 997 | 92 943 | 94 272 | 94 307 | 92 583 |
| 46 | go (1.14)| [air](https://github.com/aofei/air) (0.15) | 84 591 | 88 471 | 90 950 | 91 353 | 91 021 |
| 47 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 83 704 | 88 171 | 86 296 | 84 160 | 83 531 |
| 48 | java (8)| [javalin](https://javalin.io) (3.5) | 79 593 | 84 273 | 85 208 | 81 617 | 78 749 |
| 49 | go (1.14)| [gf](https://goframe.org) (1.11) | 77 724 | 83 019 | 84 855 | 85 211 | 84 966 |
| 50 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 75 827 | 82 007 | 79 192 | 77 578 | 77 980 |
| 51 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 71 999 | 76 094 | 74 794 | 72 653 | 72 489 |
| 52 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 71 567 | 74 130 | 73 186 | 70 841 | 70 080 |
| 53 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 69 417 | 76 479 | 77 089 | 75 388 | 74 844 |
| 54 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 69 062 | 79 872 | 83 841 | 83 944 | 79 774 |
| 55 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 66 519 | 71 157 | 71 710 | 72 180 | 73 557 |
| 56 | javascript (13.7)| [fastify](https://fastify.io) (2.13) | 65 009 | 70 789 | 68 568 | 68 296 | 68 195 |
| 57 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 63 748 | 65 403 | 63 469 | 62 112 | 61 111 |
| 58 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 63 744 | 64 989 | 64 588 | 63 113 | 63 169 |
| 59 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 63 370 | 70 066 | 70 269 | 69 224 | 69 095 |
| 60 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 63 038 | 66 267 | 69 590 | 69 588 | 69 449 |
| 61 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 62 554 | 65 545 | 63 306 | 61 647 | 61 861 |
| 62 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 62 350 | 80 437 | 78 668 | 80 360 | 77 297 |
| 63 | java (8)| [micronaut](https://micronaut.io) (1.2) | 62 098 | 70 464 | 69 429 | 67 167 | 68 224 |
| 64 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.14) | 58 669 | 64 028 | 64 061 | 61 530 | 61 516 |
| 65 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 57 383 | 59 595 | 60 474 | 60 335 | 60 591 |
| 66 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 56 686 | 58 798 | 58 602 | 58 182 | 58 244 |
| 67 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 56 431 | 58 938 | 57 200 | 56 360 | 56 173 |
| 68 | javascript (13.7)| [feathersjs](https://feathersjs.com) (4.5) | 56 417 | 58 259 | 57 446 | 56 417 | 55 984 |
| 69 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 56 303 | 64 158 | 63 932 | 62 965 | 63 816 |
| 70 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 54 832 | 55 400 | 55 169 | 55 196 | 54 530 |
| 71 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 54 049 | 60 824 | 62 179 | 62 953 | 63 098 |
| 72 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 53 905 | 58 586 | 58 423 | 56 387 | 56 376 |
| 73 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 51 631 | 51 515 | 51 678 | 50 544 | 50 623 |
| 74 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 50 951 | 51 130 | 51 463 | 50 358 | 50 446 |
| 75 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 50 880 | 54 500 | 54 016 | 52 282 | 52 068 |
| 76 | javascript (13.7)| [moleculer](https://moleculer.services) (0.14) | 50 306 | 52 740 | 51 498 | 50 956 | 51 064 |
| 77 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 49 798 | 51 324 | 51 219 | 50 424 | 50 241 |
| 78 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 49 405 | 50 972 | 51 167 | 50 625 | 50 487 |
| 79 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 47 152 | 53 190 | 52 701 | 52 062 | 51 875 |
| 80 | python (3.8)| [hug](https://hug.rest) (2.6) | 46 903 | 49 490 | 49 172 | 48 974 | 49 055 |
| 81 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 45 880 | 45 750 | 45 585 | 45 574 | 44 911 |
| 82 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 43 624 | 44 957 | 44 319 | 43 459 | 43 408 |
| 83 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 43 140 | 45 767 | 45 625 | 45 195 | 44 998 |
| 84 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 42 581 | 47 883 | 50 506 | 51 729 | 52 495 |
| 85 | php (7.4)| [imi](https://imiphp.com) (1.0) | 42 037 | 43 445 | 44 226 | 43 561 | 43 659 |
| 86 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 41 709 | 43 901 | 43 578 | 42 694 | 42 878 |
| 87 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 41 596 | 42 908 | 40 376 | 39 870 | 40 460 |
| 88 | python (3.8)| [starlette](https://starlette.io) (0.13) | 41 403 | 44 825 | 44 288 | 42 634 | 42 659 |
| 89 | javascript (13.7)| [restify](https://restify.com) (8.5) | 41 130 | 43 088 | 42 753 | 42 234 | 42 039 |
| 90 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 40 397 | 43 990 | 41 577 | 40 882 | 41 234 |
| 91 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0b1) | 39 812 | 42 001 | 41 653 | 40 087 | 39 986 |
| 92 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 38 749 | 39 766 | 39 212 | 38 115 | 38 231 |
| 93 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 38 298 | 37 153 | 37 539 | 37 516 | 37 780 |
| 94 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.3) | 37 420 | 40 399 | 38 666 | 38 248 | 38 603 |
| 95 | php (7.4)| [swoft](https://swoft.org) (2.0) | 36 131 | 37 662 | 37 434 | 36 861 | 36 937 |
| 96 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 34 431 | 35 737 | 34 491 | 34 305 | 34 651 |
| 97 | dart (2.7)| [aqueduct](https://aqueduct.io) (3.2) | 33 533 | 32 294 | 33 675 | 32 766 | 32 781 |
| 98 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 31 598 | 32 378 | 32 166 | 31 797 | 31 751 |
| 99 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 29 766 | 29 073 | 28 740 | 29 077 | 28 893 |
| 100 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.52) | 29 575 | 30 563 | 30 833 | 29 051 | 29 439 |
| 101 | python (3.8)| [responder](https://python-responder.org) (2.0) | 29 440 | 31 812 | 31 418 | 30 423 | 30 305 |
| 102 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 26 822 | 28 003 | 27 751 | 25 960 | 26 400 |
| 103 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 25 395 | 25 318 | 25 038 | 24 997 | 25 127 |
| 104 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 25 190 | 25 583 | 25 819 | 25 223 | 25 508 |
| 105 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 23 930 | 26 209 | 25 853 | 25 671 | 26 010 |
| 106 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 23 163 | 22 329 | 21 341 | 21 083 | 20 967 |
| 107 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 21 487 | 23 550 | 22 839 | 23 035 | 20 576 |
| 108 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 21 198 | 23 229 | 22 595 | 22 598 | 21 627 |
| 109 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 19 708 | 21 422 | 19 952 | 19 518 | 19 741 |
| 110 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 18 683 | 18 878 | 18 945 | 18 795 | 18 989 |
| 111 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 18 374 | 18 528 | 18 596 | 18 354 | 18 540 |
| 112 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 15 503 | 16 485 | 16 438 | 16 487 | 16 469 |
| 113 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 15 315 | 15 251 | 15 147 | 15 194 | 15 133 |
| 114 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 15 045 | 15 490 | 15 445 | 15 463 | 15 470 |
| 115 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 13 699 | 13 419 | 13 377 | 13 239 | 13 229 |
| 116 | javascript (13.7)| [sails](https://sailsjs.com) (1.2) | 13 218 | 13 878 | 13 805 | 13 820 | 13 491 |
| 117 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 12 368 | 12 280 | 12 255 | 12 208 | 12 195 |
| 118 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 11 786 | 12 311 | 11 366 | 10 788 | 10 850 |
| 119 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 750 | 11 678 | 11 715 | 11 721 | 11 653 |
| 120 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 245 | 9 966 | 10 005 | 9 693 | 10 070 |
| 121 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 237 | 9 184 | 9 055 | 53 475 | 52 259 |
| 122 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 8 869 | 9 353 | 9 236 | 9 217 | 9 121 |
| 123 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 8 630 | 10 646 | 10 400 | 10 255 | 10 320 |
| 124 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 7 963 | 7 898 | 7 796 | 43 980 | 42 581 |
| 125 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 579 | 7 511 | 7 626 | 45 491 | 42 710 |
| 126 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 533 | 7 532 | 7 507 | 53 336 | 52 428 |
| 127 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 5 658 | 5 612 | 5 759 | 45 057 | 42 385 |
| 128 | crystal (0.33)| [onyx](https://onyxframework.org) (0.5) | 4 844 | 4 951 | 4 960 | 4 962 | 4 941 |
| 129 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 651 | 4 695 | 4 836 | 44 346 | 41 677 |
| 130 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 370 | 4 364 | 4 488 | 43 225 | 40 775 |
| 131 | php (7.4)| [lumen](https://lumen.laravel.com) (7.0) | 4 348 | 4 298 | 4 501 | 43 516 | 40 871 |
| 132 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 4 244 | 4 164 | 4 156 | 4 142 | 4 142 |
| 133 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 631 | 3 680 | 3 745 | 43 017 | 41 267 |
| 134 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 870 | 2 875 | 2 984 | 42 951 | 39 396 |
| 135 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 453 | 2 484 | 2 429 | 2 422 | 2 430 |
| 136 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 2 067 | 5 539 | 4 317 | 3 445 | 2 213 |
| 137 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 586 | 1 621 | 1 596 | 1 582 | 1 601 |
| 138 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 573 | 1 527 | 1 517 | 1 479 | 1 454 |
| 139 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 1 560 | 1 852 | 1 934 | 1 715 | 769 |
| 140 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 441 | 1 474 | 1 505 | 39 791 | 37 451 |
| 141 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 479 | 431 | 1 011 | 36 434 | 35 562 |
| 142 | php (7.4)| [laravel](https://laravel.com) (7.2) | 261 | 145 | 2 943 | 23 542 | 22 556 |

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

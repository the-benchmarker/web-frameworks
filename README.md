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

:information_source:  Updated on **2020-03-21** :information_source:

> Benchmarking with [wrk](https://github.com/ioquatix/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 183 311 | 196 756 | 198 580 | 195 282 | 195 309 |
| 2 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 180 473 | 193 511 | 193 323 | 189 919 | 190 660 |
| 3 | javascript (13.7)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.1) | 174 528 | 186 721 | 182 928 | 187 247 | 182 666 |
| 4 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.9) | 163 475 | 171 750 | 173 918 | 169 992 | 169 576 |
| 5 | javascript (13.7)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (1.1) | 163 443 | 180 210 | 180 052 | 174 355 | 177 034 |
| 6 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 157 613 | 166 134 | 168 271 | 163 778 | 163 430 |
| 7 | go (1.14)| [router](https://github.com/fasthttp/router) (0.7) | 157 600 | 165 914 | 168 197 | 164 120 | 163 804 |
| 8 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.4) | 156 760 | 164 770 | 167 327 | 163 331 | 162 710 |
| 9 | go (1.14)| [fiber](https://fiber.wiki) (1.8) | 156 693 | 166 800 | 164 681 | 155 386 | 154 025 |
| 10 | crystal (0.33)| [toro](https://github.com/soveran/toro) (0.4) | 155 952 | 164 143 | 160 897 | 152 242 | 151 612 |
| 11 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 155 382 | 163 425 | 165 682 | 161 294 | 160 839 |
| 12 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 155 300 | 169 051 | 171 010 | 167 053 | 167 072 |
| 13 | crystal (0.33)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 155 294 | 162 885 | 160 659 | 151 157 | 151 055 |
| 14 | crystal (0.33)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 151 418 | 158 300 | 155 352 | 145 977 | 145 891 |
| 15 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 146 977 | 157 464 | 158 882 | 157 283 | 157 006 |
| 16 | crystal (0.33)| [grip](https://github.com/grip-framework/grip) (0.28) | 145 763 | 152 517 | 148 582 | 140 198 | 139 117 |
| 17 | crystal (0.33)| [kemal](https://kemalcr.com) (0.26) | 144 654 | 151 945 | 148 037 | 139 799 | 138 193 |
| 18 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 144 057 | 154 516 | 155 335 | 151 553 | 151 745 |
| 19 | crystal (0.33)| [amber](https://amberframework.org) (0.33) | 136 702 | 142 972 | 138 890 | 129 404 | 129 430 |
| 20 | crystal (0.33)| [lucky](https://luckyframework.org) (0.18) | 135 172 | 139 221 | 134 528 | 123 838 | 125 426 |
| 21 | crystal (0.33)| [orion](https://github.com/obsidian/orion) (2.1) | 132 718 | 136 796 | 131 319 | 120 902 | 120 148 |
| 22 | crystal (0.33)| [athena](https://github.com/athena-framework/athena) (0.8) | 126 854 | 129 440 | 123 129 | 110 867 | 110 924 |
| 23 | java (8)| [act](https://actframework.org) (1.8) | 119 062 | 130 618 | 130 711 | 127 746 | 127 284 |
| 24 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 111 097 | 110 181 | 113 492 | 113 325 | 113 070 |
| 25 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 109 322 | 108 482 | 111 757 | 111 644 | 111 172 |
| 26 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.0) | 104 669 | 103 122 | 106 196 | 106 042 | 105 482 |
| 27 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 104 012 | 107 875 | 110 677 | 109 771 | 109 521 |
| 28 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 103 642 | 103 223 | 105 915 | 104 967 | 104 813 |
| 29 | go (1.14)| [violetear](https://violetear.org) (7.0) | 102 144 | 101 539 | 105 017 | 105 104 | 104 802 |
| 30 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 100 257 | 98 774 | 101 913 | 102 211 | 102 004 |
| 31 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 100 126 | 98 379 | 101 378 | 101 962 | 101 742 |
| 32 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 99 758 | 103 430 | 104 985 | 103 298 | 103 001 |
| 33 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 98 417 | 120 215 | 125 072 | 125 176 | 124 597 |
| 34 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 97 314 | 95 604 | 98 713 | 99 235 | 98 902 |
| 35 | go (1.14)| [beego](https://beego.me) (1.12) | 97 016 | 100 360 | 103 549 | 103 221 | 102 790 |
| 36 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 96 821 | 104 852 | 105 447 | 102 601 | 103 044 |
| 37 | go (1.14)| [gin](https://gin-gonic.com) (1.5) | 96 127 | 99 270 | 101 595 | 101 793 | 101 530 |
| 38 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 94 837 | 93 571 | 96 997 | 97 541 | 97 266 |
| 39 | c (99)| [kore](https://kore.io) (3.3) | 91 570 | 157 941 | 157 570 | 156 352 | 147 974 |
| 40 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 89 819 | 100 738 | 101 326 | 96 351 | 96 820 |
| 41 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 88 286 | 91 531 | 92 457 | 93 056 | 92 279 |
| 42 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 84 696 | 93 695 | 95 814 | 91 279 | 89 801 |
| 43 | go (1.14)| [air](https://github.com/aofei/air) (0.15) | 83 223 | 87 023 | 89 244 | 89 249 | 89 032 |
| 44 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.1) | 83 198 | 90 425 | 89 107 | 85 369 | 85 132 |
| 45 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 83 171 | 87 320 | 84 916 | 82 269 | 81 566 |
| 46 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 82 224 | 87 898 | 85 434 | 82 197 | 79 916 |
| 47 | go (1.14)| [gf](https://goframe.org) (1.11) | 76 779 | 81 949 | 83 564 | 83 615 | 83 386 |
| 48 | java (8)| [javalin](https://javalin.io) (3.5) | 74 824 | 78 927 | 78 738 | 75 161 | 76 548 |
| 49 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 73 386 | 79 793 | 76 646 | 75 118 | 75 791 |
| 50 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 71 383 | 77 827 | 77 713 | 76 403 | 76 363 |
| 51 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 71 016 | 77 060 | 74 929 | 69 287 | 70 178 |
| 52 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 66 075 | 67 872 | 63 758 | 63 988 | 64 060 |
| 53 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 65 400 | 70 335 | 68 807 | 66 296 | 66 441 |
| 54 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 64 729 | 69 020 | 69 116 | 68 283 | 68 064 |
| 55 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 64 575 | 65 454 | 64 879 | 63 311 | 63 329 |
| 56 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 64 476 | 76 911 | 78 410 | 74 786 | 74 168 |
| 57 | javascript (13.7)| [fastify](https://fastify.io) (2.13) | 64 457 | 71 281 | 68 226 | 67 351 | 66 360 |
| 58 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 63 350 | 71 527 | 76 342 | 76 440 | 78 058 |
| 59 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 61 654 | 64 963 | 67 779 | 67 705 | 67 658 |
| 60 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 61 285 | 63 945 | 62 035 | 60 821 | 61 104 |
| 61 | java (8)| [micronaut](https://micronaut.io) (1.2) | 60 876 | 67 770 | 69 199 | 66 553 | 68 230 |
| 62 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 59 913 | 62 900 | 62 565 | 61 756 | 61 534 |
| 63 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 57 393 | 174 634 | 184 324 | 184 173 | 180 275 |
| 64 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.14) | 57 197 | 63 628 | 63 178 | 60 701 | 60 556 |
| 65 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 57 097 | 61 558 | 58 565 | 57 126 | 56 895 |
| 66 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 56 885 | 57 541 | 55 657 | 56 255 | 56 277 |
| 67 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 56 684 | 63 914 | 63 482 | 61 890 | 61 913 |
| 68 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 53 714 | 54 828 | 54 633 | 54 534 | 54 453 |
| 69 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 51 511 | 58 460 | 59 343 | 60 945 | 60 666 |
| 70 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 50 565 | 54 086 | 53 157 | 51 729 | 50 140 |
| 71 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 50 474 | 54 762 | 54 147 | 52 265 | 52 404 |
| 72 | javascript (13.7)| [moleculer](https://moleculer.services) (0.14) | 50 398 | 51 874 | 50 782 | 50 014 | 49 994 |
| 73 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 48 727 | 50 737 | 50 701 | 50 077 | 50 125 |
| 74 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 48 422 | 47 788 | 48 416 | 47 470 | 46 984 |
| 75 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 48 404 | 52 459 | 51 854 | 50 124 | 50 058 |
| 76 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 47 878 | 47 992 | 48 277 | 47 921 | 47 503 |
| 77 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 47 606 | 49 185 | 47 492 | 46 829 | 46 688 |
| 78 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 46 768 | 52 451 | 51 911 | 51 646 | 51 428 |
| 79 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 46 284 | 46 113 | 45 953 | 45 889 | 45 240 |
| 80 | python (3.8)| [starlette](https://starlette.io) (0.13) | 45 734 | 49 321 | 48 812 | 47 171 | 47 261 |
| 81 | python (3.8)| [hug](https://hug.rest) (2.6) | 44 720 | 46 672 | 46 373 | 45 550 | 45 887 |
| 82 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 43 557 | 45 374 | 44 272 | 44 197 | 43 951 |
| 83 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 42 503 | 43 536 | 43 083 | 42 197 | 42 043 |
| 84 | php (7.4)| [imi](https://imiphp.com) (1.0) | 41 478 | 43 033 | 43 406 | 43 153 | 43 198 |
| 85 | javascript (13.7)| [restify](https://restify.com) (8.5) | 41 372 | 43 070 | 41 746 | 41 730 | 41 701 |
| 86 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 41 250 | 43 279 | 42 975 | 42 389 | 42 317 |
| 87 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 41 192 | 49 295 | 50 829 | 52 961 | 52 792 |
| 88 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 40 547 | 42 158 | 39 707 | 39 165 | 38 947 |
| 89 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0b1) | 39 108 | 42 006 | 41 665 | 40 296 | 40 464 |
| 90 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 38 923 | 43 244 | 41 170 | 40 297 | 40 501 |
| 91 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.3) | 38 352 | 39 554 | 37 995 | 37 246 | 37 771 |
| 92 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 37 651 | 38 396 | 37 850 | 37 076 | 36 999 |
| 93 | php (7.4)| [swoft](https://swoft.org) (2.0) | 36 264 | 37 011 | 36 682 | 36 155 | 36 234 |
| 94 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 35 267 | 35 484 | 34 455 | 34 074 | 34 164 |
| 95 | dart (2.7)| [aqueduct](https://aqueduct.io) (3.2) | 33 335 | 32 357 | 32 878 | 31 809 | 31 940 |
| 96 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 30 789 | 32 803 | 32 406 | 31 796 | 31 751 |
| 97 | python (3.8)| [responder](https://python-responder.org) (2.0) | 29 727 | 31 424 | 31 028 | 29 822 | 29 869 |
| 98 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.52) | 29 508 | 31 294 | 31 023 | 29 737 | 29 749 |
| 99 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 29 114 | 27 415 | 33 419 | 32 469 | 31 665 |
| 100 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 28 066 | 28 381 | 27 896 | 27 830 | 27 984 |
| 101 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 26 925 | 27 693 | 27 209 | 27 223 | 26 973 |
| 102 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 26 092 | 26 556 | 26 420 | 26 408 | 26 310 |
| 103 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 25 803 | 25 475 | 25 122 | 25 043 | 25 280 |
| 104 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 25 730 | 28 028 | 27 905 | 26 842 | 27 124 |
| 105 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 24 025 | 24 833 | 24 668 | 24 355 | 23 845 |
| 106 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 23 811 | 24 706 | 24 393 | 24 018 | 24 235 |
| 107 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 23 061 | 21 118 | 20 503 | 20 470 | 20 405 |
| 108 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 21 529 | 21 840 | 21 195 | 20 542 | 20 394 |
| 109 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 18 322 | 18 492 | 18 389 | 18 508 | 18 260 |
| 110 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 639 | 18 028 | 18 125 | 17 986 | 18 077 |
| 111 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 16 170 | 16 094 | 16 052 | 16 030 | 16 010 |
| 112 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 14 956 | 15 430 | 15 386 | 15 330 | 15 397 |
| 113 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 14 870 | 14 746 | 14 698 | 14 641 | 14 657 |
| 114 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 13 393 | 13 114 | 13 117 | 13 071 | 13 033 |
| 115 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 12 026 | 11 978 | 11 957 | 11 934 | 11 980 |
| 116 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 11 238 | 12 166 | 11 400 | 10 867 | 10 627 |
| 117 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 10 854 | 11 141 | 11 074 | 10 809 | 11 210 |
| 118 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 204 | 10 348 | 10 289 | 10 238 | 10 250 |
| 119 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 104 | 9 010 | 9 127 | 51 769 | 52 415 |
| 120 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 8 042 | 8 353 | 8 254 | 8 202 | 8 219 |
| 121 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 7 868 | 7 844 | 7 774 | 43 856 | 43 118 |
| 122 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 7 627 | 10 514 | 10 184 | 10 092 | 10 118 |
| 123 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 463 | 7 471 | 7 524 | 52 154 | 48 362 |
| 124 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 459 | 7 463 | 7 515 | 43 808 | 44 781 |
| 125 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 5 605 | 5 570 | 5 591 | 44 556 | 42 802 |
| 126 | crystal (0.33)| [onyx](https://onyxframework.org) (0.5) | 4 794 | 4 898 | 4 917 | 4 950 | 4 918 |
| 127 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 612 | 4 622 | 4 736 | 44 003 | 41 856 |
| 128 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 326 | 4 362 | 4 416 | 43 618 | 40 804 |
| 129 | php (7.4)| [lumen](https://lumen.laravel.com) (7.0) | 4 304 | 4 317 | 4 375 | 43 353 | 41 555 |
| 130 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 981 | 3 783 | 3 774 | 3 748 | 3 730 |
| 131 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 607 | 3 628 | 3 669 | 42 672 | 38 941 |
| 132 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 812 | 2 832 | 2 899 | 41 735 | 40 543 |
| 133 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 425 | 2 449 | 2 420 | 2 409 | 2 393 |
| 134 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 2 131 | 5 475 | 4 352 | 3 467 | 1 783 |
| 135 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 641 | 1 653 | 1 624 | 1 608 | 1 620 |
| 136 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 529 | 1 498 | 1 463 | 1 460 | 1 468 |
| 137 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 426 | 1 457 | 1 509 | 39 093 | 38 381 |
| 138 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 887 | 702 | 1 357 | 1 625 | 486 |
| 139 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 482 | 515 | 2 500 | 35 114 | 33 763 |
| 140 | php (7.4)| [laravel](https://laravel.com) (7.2) | 274 | 158 | 2 278 | 23 201 | 21 877 |

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

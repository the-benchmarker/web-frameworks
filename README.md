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

:information_source:  Updated on **2020-04-17** :information_source:

> Benchmarking with [wrk](https://github.com/ioquatix/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (2.2) | 192 169 | 205 111 | 206 613 | 204 103 | 204 379 |
| 2 | javascript (13.12)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.11) | 180 619 | 193 235 | 191 953 | 188 442 | 189 058 |
| 3 | php (7.4)| [simps](https://simps.io) (1.0) | 174 627 | 186 057 | 187 838 | 185 407 | 185 371 |
| 4 | javascript (13.12)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.0) | 170 542 | 182 163 | 182 019 | 178 503 | 178 342 |
| 5 | javascript (13.12)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 168 331 | 173 032 | 181 641 | 179 924 | 180 822 |
| 6 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.1) | 159 485 | 169 040 | 172 132 | 167 238 | 167 197 |
| 7 | crystal (0.34)| [toro](https://github.com/soveran/toro) (0.4) | 153 719 | 160 140 | 158 804 | 150 865 | 149 687 |
| 8 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.0) | 153 219 | 161 937 | 165 143 | 160 925 | 161 044 |
| 9 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.0) | 152 677 | 161 664 | 165 469 | 160 879 | 160 457 |
| 10 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 152 478 | 161 979 | 165 406 | 160 540 | 160 466 |
| 11 | crystal (0.34)| [ricr](https://ricr-web.github.io/ricr) (0.1) | 152 423 | 159 153 | 155 157 | 147 257 | 146 240 |
| 12 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 152 328 | 158 268 | 165 050 | 159 737 | 158 936 |
| 13 | crystal (0.34)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 151 800 | 158 741 | 156 106 | 147 501 | 147 585 |
| 14 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 150 825 | 161 481 | 162 592 | 159 133 | 159 462 |
| 15 | crystal (0.34)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 149 437 | 156 362 | 153 392 | 145 053 | 144 424 |
| 16 | go (1.14)| [fiber](https://fiber.wiki) (1.9) | 145 769 | 156 494 | 153 772 | 145 749 | 146 151 |
| 17 | crystal (0.34)| [kemal](https://kemalcr.com) (0.26) | 143 325 | 150 039 | 145 983 | 138 471 | 137 303 |
| 18 | crystal (0.34)| [grip](https://github.com/grip-framework/grip) (0.28) | 143 181 | 149 768 | 145 599 | 137 536 | 135 822 |
| 19 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 142 725 | 154 433 | 156 053 | 153 756 | 153 305 |
| 20 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 142 028 | 158 419 | 161 180 | 162 537 | 159 496 |
| 21 | crystal (0.34)| [amber](https://amberframework.org) (0.34) | 136 180 | 141 277 | 137 718 | 129 471 | 128 679 |
| 22 | rust (1.42)| [actix](https://actix.rs) (2.0) | 134 197 | 136 409 | 138 322 | 134 614 | 131 010 |
| 23 | crystal (0.34)| [lucky](https://luckyframework.org) (0.2) | 131 391 | 134 957 | 129 044 | 119 699 | 119 260 |
| 24 | crystal (0.34)| [orion](https://github.com/obsidian/orion) (2.3) | 131 218 | 135 092 | 129 831 | 119 342 | 117 257 |
| 25 | c (99)| [kore](https://kore.io) (3.3) | 126 052 | 119 837 | 136 098 | 139 608 | 138 209 |
| 26 | crystal (0.34)| [athena](https://github.com/athena-framework/athena) (0.8) | 125 584 | 127 945 | 121 686 | 108 984 | 109 303 |
| 27 | java (8)| [act](https://actframework.org) (1.8) | 115 075 | 125 342 | 119 923 | 115 924 | 123 996 |
| 28 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 109 045 | 109 244 | 112 880 | 112 540 | 112 010 |
| 29 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 107 140 | 107 583 | 110 449 | 110 222 | 109 766 |
| 30 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 103 255 | 102 584 | 105 577 | 104 928 | 104 879 |
| 31 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 103 136 | 107 573 | 110 003 | 108 904 | 108 317 |
| 32 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 102 766 | 166 923 | 162 687 | 172 659 | 161 927 |
| 33 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 100 339 | 101 606 | 104 574 | 104 111 | 103 556 |
| 34 | go (1.14)| [violetear](https://violetear.org) (7.0) | 99 722 | 99 994 | 103 131 | 103 376 | 102 791 |
| 35 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 98 438 | 97 534 | 100 646 | 100 860 | 100 621 |
| 36 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 98 176 | 97 822 | 100 408 | 100 407 | 100 085 |
| 37 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 97 683 | 101 844 | 103 589 | 102 026 | 101 638 |
| 38 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 95 842 | 94 587 | 97 627 | 97 772 | 97 401 |
| 39 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 94 897 | 118 122 | 122 532 | 123 295 | 123 026 |
| 40 | go (1.14)| [beego](https://beego.me) (1.12) | 94 812 | 98 509 | 101 170 | 101 135 | 100 664 |
| 41 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 93 792 | 98 036 | 100 601 | 99 992 | 99 610 |
| 42 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 93 444 | 93 242 | 96 237 | 96 445 | 96 227 |
| 43 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 93 440 | 99 556 | 100 126 | 98 612 | 99 627 |
| 44 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 87 792 | 91 222 | 94 116 | 93 403 | 92 975 |
| 45 | javascript (13.12)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 86 353 | 95 692 | 94 841 | 91 076 | 90 598 |
| 46 | javascript (13.12)| [0http](https://github.com/jkyberneees/0http) (2.2) | 82 492 | 91 525 | 90 991 | 87 741 | 86 986 |
| 47 | go (1.14)| [air](https://github.com/aofei/air) (0.15) | 80 644 | 85 086 | 86 932 | 86 787 | 86 582 |
| 48 | javascript (13.12)| [restana](https://github.com/jkyberneees/ana) (4.3) | 79 275 | 86 738 | 84 162 | 84 010 | 83 796 |
| 49 | javascript (13.12)| [polka](https://github.com/lukeed/polka) (0.5) | 77 630 | 82 641 | 80 336 | 77 371 | 77 257 |
| 50 | javascript (13.12)| [rayo](https://rayo.js.org) (1.3) | 76 277 | 80 959 | 78 806 | 76 214 | 76 163 |
| 51 | go (1.14)| [gf](https://goframe.org) (1.12) | 75 280 | 81 061 | 82 619 | 82 036 | 81 601 |
| 52 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 74 040 | 79 101 | 75 665 | 74 287 | 75 495 |
| 53 | java (8)| [javalin](https://javalin.io) (3.5) | 70 925 | 76 527 | 77 817 | 76 894 | 76 390 |
| 54 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 70 635 | 75 758 | 76 244 | 75 013 | 74 581 |
| 55 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 70 112 | 79 261 | 84 327 | 84 040 | 84 362 |
| 56 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 65 241 | 75 645 | 77 473 | 76 394 | 76 086 |
| 57 | php (7.4)| [hyperf](https://www.hyperf.io) (1.1) | 65 010 | 68 710 | 68 988 | 68 450 | 68 612 |
| 58 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 64 989 | 69 684 | 67 229 | 66 053 | 67 623 |
| 59 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 63 964 | 66 923 | 67 404 | 67 008 | 66 737 |
| 60 | javascript (13.12)| [fastify](https://fastify.io) (2.13) | 63 877 | 67 289 | 64 937 | 64 532 | 63 233 |
| 61 | java (8)| [micronaut](https://micronaut.io) (1.2) | 63 536 | 70 960 | 70 921 | 67 858 | 68 425 |
| 62 | javascript (13.12)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 63 420 | 67 722 | 65 978 | 63 883 | 63 258 |
| 63 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 63 393 | 63 547 | 62 705 | 61 053 | 61 076 |
| 64 | javascript (13.12)| [foxify](https://foxify.js.org) (0.1) | 62 427 | 65 917 | 63 954 | 62 861 | 62 446 |
| 65 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 61 077 | 64 311 | 67 129 | 66 737 | 66 492 |
| 66 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 57 847 | 62 602 | 63 441 | 62 389 | 60 002 |
| 67 | javascript (13.12)| [koa](https://koajs.com) (2.11) | 55 941 | 59 235 | 56 648 | 56 775 | 56 597 |
| 68 | rust (1.42)| [nickel](https://nickel-org.github.io) (0.11) | 55 823 | 55 736 | 55 157 | 55 926 | 55 783 |
| 69 | javascript (13.12)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 55 766 | 58 457 | 56 557 | 55 133 | 55 044 |
| 70 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 54 360 | 55 693 | 55 577 | 55 156 | 55 293 |
| 71 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.25) | 54 208 | 56 754 | 57 195 | 56 824 | 56 656 |
| 72 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 53 477 | 59 317 | 59 507 | 59 503 | 58 694 |
| 73 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 52 200 | 57 562 | 57 577 | 55 594 | 55 419 |
| 74 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 52 185 | 60 137 | 62 400 | 62 451 | 63 372 |
| 75 | javascript (13.12)| [feathersjs](https://feathersjs.com) (4.5) | 50 760 | 52 873 | 51 711 | 50 852 | 50 644 |
| 76 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 49 664 | 50 001 | 48 935 | 49 749 | 49 690 |
| 77 | javascript (13.12)| [express](https://expressjs.com) (4.17) | 49 482 | 52 223 | 50 118 | 50 012 | 50 331 |
| 78 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 49 249 | 50 479 | 49 549 | 50 164 | 49 642 |
| 79 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 49 028 | 48 496 | 47 727 | 48 099 | 47 796 |
| 80 | javascript (13.12)| [moleculer](https://moleculer.services) (0.14) | 47 002 | 48 540 | 46 886 | 46 205 | 46 403 |
| 81 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 46 675 | 45 651 | 44 049 | 45 750 | 45 333 |
| 82 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 46 645 | 51 761 | 50 172 | 48 321 | 49 019 |
| 83 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 46 525 | 46 435 | 46 303 | 46 560 | 45 539 |
| 84 | python (3.8)| [hug](https://hug.rest) (2.6) | 45 221 | 48 106 | 48 191 | 47 818 | 47 986 |
| 85 | python (3.8)| [starlette](https://starlette.io) (0.13) | 43 748 | 47 909 | 47 800 | 46 464 | 46 139 |
| 86 | rust (1.42)| [gotham](https://gotham.rs) (0.4) | 42 186 | 50 412 | 52 019 | 53 192 | 53 706 |
| 87 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 40 820 | 41 530 | 41 182 | 39 922 | 40 024 |
| 88 | php (7.4)| [imi](https://imiphp.com) (1.0) | 40 119 | 41 669 | 42 237 | 42 301 | 41 999 |
| 89 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 39 983 | 40 808 | 38 928 | 38 276 | 38 835 |
| 90 | javascript (13.12)| [hapi](https://hapijs.com) (19.1) | 39 508 | 42 310 | 41 667 | 41 135 | 41 334 |
| 91 | javascript (13.12)| [restify](https://restify.com) (8.5) | 39 374 | 41 087 | 41 084 | 38 973 | 39 481 |
| 92 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 39 320 | 39 761 | 37 722 | 37 499 | 38 051 |
| 93 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0b2) | 38 973 | 41 404 | 41 212 | 39 562 | 39 855 |
| 94 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 38 604 | 41 446 | 41 537 | 41 073 | 41 074 |
| 95 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.31) | 36 802 | 37 483 | 35 935 | 35 613 | 35 750 |
| 96 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 36 045 | 36 522 | 36 096 | 35 119 | 34 890 |
| 97 | php (7.4)| [swoft](https://swoft.org) (2.0) | 35 253 | 36 533 | 36 543 | 36 110 | 36 104 |
| 98 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 32 505 | 32 700 | 31 708 | 31 548 | 31 717 |
| 99 | dart (2.7)| [aqueduct](https://aqueduct.io) (3.3) | 31 445 | 31 110 | 31 473 | 30 890 | 30 864 |
| 100 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 31 152 | 31 343 | 31 240 | 29 991 | 30 117 |
| 101 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 31 008 | 34 477 | 37 706 | 37 008 | 35 018 |
| 102 | python (3.8)| [responder](https://python-responder.org) (2.0) | 29 540 | 31 138 | 30 904 | 29 755 | 29 726 |
| 103 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 28 436 | 27 928 | 27 289 | 27 072 | 27 239 |
| 104 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 27 282 | 27 734 | 26 968 | 26 941 | 27 076 |
| 105 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.54) | 27 015 | 29 239 | 29 059 | 27 794 | 27 096 |
| 106 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 26 213 | 27 044 | 26 952 | 26 557 | 26 454 |
| 107 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 25 516 | 25 928 | 25 951 | 25 600 | 25 610 |
| 108 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 25 001 | 24 411 | 23 998 | 24 001 | 23 936 |
| 109 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 24 096 | 27 391 | 27 287 | 26 557 | 26 550 |
| 110 | rust (1.42)| [iron](https://ironframework.io) (0.6) | 23 962 | 23 900 | 23 918 | 23 726 | 23 822 |
| 111 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 23 654 | 24 220 | 24 139 | 23 553 | 22 878 |
| 112 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 23 235 | 21 946 | 23 933 | 23 774 | 23 543 |
| 113 | javascript (13.12)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 656 | 21 534 | 20 429 | 20 174 | 19 984 |
| 114 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 18 325 | 18 451 | 18 625 | 18 399 | 18 596 |
| 115 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 15 703 | 15 675 | 15 697 | 15 646 | 15 615 |
| 116 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 14 425 | 14 485 | 14 447 | 14 393 | 14 494 |
| 117 | javascript (13.12)| [sails](https://sailsjs.com) (1.2) | 12 778 | 13 128 | 12 949 | 12 763 | 12 635 |
| 118 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 12 025 | 10 890 | 9 558 | 10 379 | 11 282 |
| 119 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 11 473 | 11 172 | 11 178 | 11 163 | 11 157 |
| 120 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 376 | 11 373 | 11 585 | 11 079 | 11 338 |
| 121 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 10 915 | 12 074 | 11 331 | 10 722 | 10 748 |
| 122 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 10 733 | 10 676 | 10 587 | 10 592 | 10 598 |
| 123 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 144 | 9 067 | 8 907 | 51 207 | 49 102 |
| 124 | python (3.8)| [django](https://djangoproject.com) (3.0) | 8 864 | 9 659 | 9 286 | 9 132 | 9 193 |
| 125 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 8 555 | 10 316 | 10 157 | 9 991 | 10 014 |
| 126 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 8 403 | 9 111 | 9 026 | 8 189 | 8 901 |
| 127 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 7 943 | 7 878 | 7 696 | 44 601 | 41 414 |
| 128 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 514 | 7 464 | 7 403 | 50 687 | 50 444 |
| 129 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 514 | 7 481 | 7 437 | 44 114 | 43 756 |
| 130 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 5 638 | 5 573 | 5 607 | 43 980 | 40 528 |
| 131 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 4 965 | 5 060 | 8 806 | 10 015 | 2 786 |
| 132 | php (7.4)| [slim](https://slimframework.com) (4.5) | 4 652 | 4 667 | 4 719 | 42 716 | 38 975 |
| 133 | php (7.4)| [lumen](https://lumen.laravel.com) (7.0) | 4 309 | 4 333 | 4 391 | 42 424 | 39 240 |
| 134 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 134 | 4 165 | 4 281 | 42 636 | 39 849 |
| 135 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 782 | 3 612 | 3 601 | 3 552 | 3 549 |
| 136 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 182 | 3 213 | 3 259 | 41 060 | 38 514 |
| 137 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 858 | 2 872 | 2 948 | 39 373 | 39 613 |
| 138 | crystal (0.34)| [onyx](https://onyxframework.org) (0.5) | 2 831 | 3 336 | 3 334 | 3 349 | 3 441 |
| 139 | julia (1.4)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 2 711 | 7 846 | 6 495 | 4 991 | 2 997 |
| 140 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 441 | 2 448 | 2 432 | 2 413 | 2 408 |
| 141 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 545 | 1 504 | 1 487 | 1 464 | 1 470 |
| 142 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 442 | 1 418 | 1 464 | 1 416 | 1 308 |
| 143 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 425 | 1 459 | 1 512 | 38 950 | 36 201 |
| 144 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 833 | 466 | 1 200 | 34 281 | 34 058 |
| 145 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 523 | 604 | 1 290 | 1 502 | 1 306 |
| 146 | php (7.4)| [laravel](https://laravel.com) (7.6) | 334 | 163 | 2 019 | 21 588 | 22 755 |

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

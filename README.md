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

:information_source:  Updated on **2020-04-20** :information_source:

> Benchmarking with [wrk](https://github.com/ioquatix/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | javascript (13.12)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.0) | 176 480 | 188 358 | 188 735 | 185 362 | 186 495 |
| 2 | php (7.4)| [simps](https://simps.io) (1.0) | 175 011 | 184 006 | 185 336 | 183 139 | 183 520 |
| 3 | javascript (13.12)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.11) | 174 700 | 195 865 | 194 737 | 192 160 | 189 753 |
| 4 | javascript (13.12)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 172 624 | 185 454 | 184 938 | 183 964 | 179 356 |
| 5 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (2.2) | 170 899 | 190 062 | 188 898 | 180 806 | 178 524 |
| 6 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 159 160 | 175 603 | 177 087 | 173 003 | 173 583 |
| 7 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.11) | 158 037 | 167 193 | 170 783 | 166 167 | 165 804 |
| 8 | crystal (0.34)| [toro](https://github.com/soveran/toro) (0.4) | 153 642 | 161 116 | 157 831 | 150 055 | 149 362 |
| 9 | crystal (0.34)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 152 034 | 158 954 | 155 072 | 147 408 | 146 932 |
| 10 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 151 590 | 160 664 | 164 024 | 159 057 | 158 394 |
| 11 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.0) | 151 507 | 160 256 | 163 637 | 158 352 | 157 699 |
| 12 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.0) | 151 293 | 160 121 | 163 033 | 158 822 | 158 950 |
| 13 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 150 864 | 159 401 | 162 769 | 158 318 | 158 152 |
| 14 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 149 943 | 161 774 | 162 538 | 160 888 | 160 059 |
| 15 | crystal (0.34)| [ricr](https://ricr-web.github.io/ricr) (0.1) | 149 659 | 156 334 | 153 443 | 145 360 | 145 602 |
| 16 | crystal (0.34)| [spider-gazelle](https://spider-gazelle.net) (2.4) | 147 007 | 153 170 | 150 206 | 142 712 | 141 645 |
| 17 | go (1.14)| [fiber](https://fiber.wiki) (1.9) | 146 496 | 155 222 | 153 256 | 145 240 | 145 896 |
| 18 | crystal (0.34)| [kemal](https://kemalcr.com) (0.26) | 142 616 | 148 154 | 144 306 | 137 202 | 136 956 |
| 19 | crystal (0.34)| [grip](https://github.com/grip-framework/grip) (0.28) | 141 236 | 146 562 | 143 217 | 135 769 | 133 980 |
| 20 | rust (1.42)| [actix](https://actix.rs) (2.0) | 137 842 | 140 616 | 138 748 | 134 474 | 131 617 |
| 21 | crystal (0.34)| [amber](https://amberframework.org) (0.34) | 135 548 | 140 923 | 136 965 | 130 755 | 129 942 |
| 22 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 134 609 | 144 172 | 140 210 | 138 463 | 141 104 |
| 23 | crystal (0.34)| [orion](https://github.com/obsidian/orion) (2.3) | 130 205 | 133 417 | 127 882 | 119 018 | 117 671 |
| 24 | crystal (0.34)| [lucky](https://luckyframework.org) (0.20) | 128 518 | 132 287 | 126 471 | 117 683 | 116 672 |
| 25 | crystal (0.34)| [athena](https://github.com/athena-framework/athena) (0.8) | 123 688 | 126 159 | 119 153 | 107 094 | 107 007 |
| 26 | java (8)| [act](https://actframework.org) (1.8) | 117 627 | 126 512 | 131 558 | 129 414 | 129 922 |
| 27 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 110 947 | 173 013 | 154 557 | 178 089 | 178 808 |
| 28 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 107 816 | 107 895 | 111 192 | 110 913 | 110 709 |
| 29 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 106 310 | 106 445 | 109 465 | 108 887 | 108 996 |
| 30 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 101 951 | 105 880 | 108 497 | 107 320 | 106 410 |
| 31 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 101 868 | 101 171 | 104 288 | 103 876 | 103 099 |
| 32 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 100 028 | 100 942 | 103 730 | 102 991 | 103 125 |
| 33 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 98 790 | 117 828 | 121 786 | 122 307 | 121 220 |
| 34 | go (1.14)| [violetear](https://violetear.org) (7.0) | 98 553 | 98 978 | 102 169 | 102 086 | 102 086 |
| 35 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 98 210 | 96 895 | 100 171 | 100 663 | 100 499 |
| 36 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 97 764 | 102 020 | 103 638 | 101 596 | 101 161 |
| 37 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 97 751 | 96 808 | 99 612 | 99 919 | 99 592 |
| 38 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 94 585 | 93 144 | 96 179 | 96 649 | 96 357 |
| 39 | go (1.14)| [beego](https://beego.me) (1.12) | 94 506 | 98 005 | 100 921 | 100 590 | 100 916 |
| 40 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 93 329 | 97 230 | 99 266 | 99 329 | 98 836 |
| 41 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 92 732 | 92 216 | 95 206 | 95 734 | 95 544 |
| 42 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 91 571 | 98 459 | 99 249 | 97 903 | 98 203 |
| 43 | javascript (13.12)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 88 455 | 98 138 | 99 654 | 95 575 | 91 766 |
| 44 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 87 973 | 90 895 | 92 481 | 92 104 | 91 346 |
| 45 | javascript (13.12)| [0http](https://github.com/jkyberneees/0http) (2.2) | 85 589 | 97 823 | 98 409 | 94 424 | 90 408 |
| 46 | javascript (13.12)| [restana](https://github.com/jkyberneees/ana) (4.3) | 84 558 | 92 500 | 92 583 | 88 771 | 87 519 |
| 47 | javascript (13.12)| [polka](https://github.com/lukeed/polka) (0.5) | 81 334 | 83 534 | 82 916 | 81 545 | 81 379 |
| 48 | go (1.14)| [air](https://github.com/aofei/air) (0.15) | 80 150 | 84 672 | 86 190 | 86 081 | 86 186 |
| 49 | javascript (13.12)| [rayo](https://rayo.js.org) (1.3) | 79 565 | 85 123 | 82 992 | 80 288 | 79 129 |
| 50 | swift (5.2)| [perfect](https://perfect.org) (3.1) | 75 803 | 82 548 | 87 821 | 88 729 | 87 911 |
| 51 | java (8)| [javalin](https://javalin.io) (3.8) | 75 081 | 79 473 | 82 427 | 81 748 | 82 627 |
| 52 | go (1.14)| [gf](https://goframe.org) (1.12) | 74 485 | 79 783 | 81 141 | 81 154 | 81 108 |
| 53 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 74 020 | 77 582 | 74 163 | 73 604 | 74 429 |
| 54 | c (99)| [kore](https://kore.io) (3.3) | 72 311 | 109 633 | 140 002 | 141 963 | 131 942 |
| 55 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 71 597 | 74 774 | 76 223 | 75 211 | 75 111 |
| 56 | javascript (13.12)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 67 747 | 72 508 | 70 420 | 68 846 | 68 527 |
| 57 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.2) | 67 348 | 74 296 | 72 568 | 71 101 | 72 137 |
| 58 | php (7.4)| [hyperf](https://www.hyperf.io) (1.1) | 66 590 | 68 638 | 69 132 | 68 638 | 68 840 |
| 59 | javascript (13.12)| [fastify](https://fastify.io) (2.13) | 66 417 | 71 335 | 67 668 | 66 355 | 66 843 |
| 60 | javascript (13.12)| [foxify](https://foxify.js.org) (0.1) | 65 642 | 68 879 | 68 337 | 66 431 | 65 640 |
| 61 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 64 255 | 72 528 | 79 953 | 81 115 | 79 840 |
| 62 | java (8)| [micronaut](https://micronaut.io) (1.2) | 63 262 | 73 668 | 71 238 | 69 047 | 68 011 |
| 63 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 63 221 | 66 669 | 67 240 | 67 157 | 67 137 |
| 64 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 63 212 | 63 481 | 62 915 | 61 315 | 61 175 |
| 65 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 61 134 | 63 701 | 66 416 | 66 526 | 66 268 |
| 66 | javascript (13.12)| [koa](https://koajs.com) (2.11) | 59 767 | 62 271 | 60 865 | 59 989 | 58 280 |
| 67 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 58 934 | 61 810 | 62 239 | 60 875 | 61 433 |
| 68 | javascript (13.12)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 58 695 | 61 087 | 59 901 | 57 692 | 58 050 |
| 69 | swift (5.2)| [vapor](https://vapor.codes) (3.3) | 56 474 | 57 715 | 57 726 | 56 869 | 56 626 |
| 70 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 55 674 | 55 291 | 55 603 | 54 403 | 54 610 |
| 71 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.26) | 55 395 | 60 370 | 60 020 | 57 912 | 57 728 |
| 72 | rust (1.42)| [nickel](https://nickel-org.github.io) (0.11) | 55 224 | 54 270 | 53 596 | 54 322 | 54 445 |
| 73 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 54 962 | 63 594 | 63 558 | 63 586 | 63 404 |
| 74 | swift (5.2)| [kitura](https://kitura.io) (2.9) | 54 878 | 54 699 | 54 887 | 53 933 | 53 604 |
| 75 | javascript (13.12)| [express](https://expressjs.com) (4.17) | 54 464 | 56 950 | 55 707 | 54 534 | 54 250 |
| 76 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 54 270 | 55 317 | 55 258 | 54 871 | 54 910 |
| 77 | javascript (13.12)| [feathersjs](https://feathersjs.com) (4.5) | 53 621 | 56 520 | 54 711 | 54 450 | 53 904 |
| 78 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 52 787 | 59 651 | 61 914 | 62 317 | 62 487 |
| 79 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 49 496 | 53 417 | 53 332 | 51 425 | 51 305 |
| 80 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 48 925 | 51 305 | 51 747 | 50 822 | 50 688 |
| 81 | javascript (13.12)| [moleculer](https://moleculer.services) (0.14) | 48 265 | 49 667 | 46 743 | 46 337 | 47 788 |
| 82 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 47 824 | 57 397 | 57 414 | 55 495 | 55 500 |
| 83 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 46 672 | 46 761 | 46 855 | 46 200 | 45 789 |
| 84 | python (3.8)| [hug](https://hug.rest) (2.6) | 45 646 | 48 584 | 48 581 | 48 056 | 48 067 |
| 85 | python (3.8)| [starlette](https://starlette.io) (0.13) | 44 598 | 48 268 | 47 880 | 46 410 | 46 193 |
| 86 | rust (1.42)| [gotham](https://gotham.rs) (0.4) | 42 326 | 49 900 | 51 624 | 53 143 | 52 999 |
| 87 | javascript (13.12)| [hapi](https://hapijs.com) (19.1) | 41 197 | 43 479 | 42 876 | 43 182 | 41 871 |
| 88 | php (7.4)| [imi](https://imiphp.com) (1.0) | 40 611 | 42 143 | 42 346 | 42 489 | 42 654 |
| 89 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 39 557 | 38 710 | 39 113 | 36 615 | 36 271 |
| 90 | javascript (13.12)| [restify](https://restify.com) (8.5) | 39 300 | 41 416 | 40 522 | 40 618 | 40 908 |
| 91 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 39 050 | 40 891 | 39 048 | 38 352 | 38 801 |
| 92 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 38 875 | 39 061 | 37 047 | 36 727 | 37 133 |
| 93 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0b2) | 37 869 | 41 225 | 41 096 | 39 624 | 39 512 |
| 94 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 37 802 | 40 704 | 40 745 | 40 183 | 40 255 |
| 95 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.31) | 36 434 | 36 716 | 35 205 | 35 155 | 35 165 |
| 96 | php (7.4)| [swoft](https://swoft.org) (2.0) | 36 207 | 37 201 | 37 186 | 36 724 | 36 693 |
| 97 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 35 837 | 36 414 | 36 338 | 35 012 | 35 029 |
| 98 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 33 088 | 33 684 | 32 742 | 32 453 | 32 525 |
| 99 | dart (2.7)| [aqueduct](https://aqueduct.io) (3.3) | 31 498 | 30 734 | 31 466 | 30 901 | 30 909 |
| 100 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 31 035 | 31 348 | 30 786 | 30 043 | 30 007 |
| 101 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.54) | 29 661 | 31 347 | 31 241 | 30 019 | 29 981 |
| 102 | crystal (0.34)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.15) | 28 730 | 27 107 | 24 853 | 22 451 | 21 387 |
| 103 | python (3.8)| [responder](https://python-responder.org) (2.0) | 28 665 | 30 806 | 30 556 | 29 411 | 29 462 |
| 104 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 27 838 | 27 779 | 27 153 | 27 132 | 27 154 |
| 105 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 27 291 | 27 449 | 26 891 | 26 826 | 26 861 |
| 106 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 25 367 | 26 297 | 25 998 | 25 826 | 25 772 |
| 107 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 25 355 | 26 496 | 26 898 | 26 624 | 25 822 |
| 108 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 25 226 | 24 619 | 24 218 | 24 149 | 24 222 |
| 109 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 24 740 | 27 294 | 27 299 | 26 463 | 26 474 |
| 110 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 23 994 | 26 850 | 30 109 | 34 728 | 34 650 |
| 111 | rust (1.42)| [iron](https://ironframework.io) (0.6) | 23 902 | 23 908 | 24 084 | 23 832 | 23 863 |
| 112 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 23 744 | 24 716 | 24 600 | 24 196 | 24 021 |
| 113 | javascript (13.12)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 685 | 21 826 | 20 552 | 20 348 | 20 388 |
| 114 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 22 558 | 24 272 | 24 322 | 23 846 | 21 235 |
| 115 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 18 405 | 18 886 | 18 469 | 18 160 | 18 044 |
| 116 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 18 232 | 18 674 | 18 716 | 18 504 | 18 626 |
| 117 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 17 179 | 17 626 | 17 666 | 17 635 | 17 646 |
| 118 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 15 768 | 15 757 | 15 666 | 15 676 | 15 654 |
| 119 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 14 583 | 14 531 | 14 630 | 14 609 | 14 540 |
| 120 | javascript (13.12)| [sails](https://sailsjs.com) (1.2) | 12 544 | 12 985 | 13 012 | 12 929 | 13 009 |
| 121 | swift (5.2)| [swifter](https://github.com/httpswift/swifter) (1.4) | 12 204 | 12 176 | 12 112 | 12 163 | 12 236 |
| 122 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 11 516 | 11 293 | 11 288 | 11 265 | 11 189 |
| 123 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 10 725 | 10 638 | 10 638 | 10 579 | 10 598 |
| 124 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 10 524 | 11 002 | 9 702 | 9 562 | 9 720 |
| 125 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 919 | 9 566 | 9 873 | 8 889 | 9 694 |
| 126 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 192 | 9 073 | 8 869 | 51 167 | 49 026 |
| 127 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 8 447 | 10 019 | 10 011 | 9 740 | 9 899 |
| 128 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 7 944 | 7 947 | 7 778 | 43 169 | 41 620 |
| 129 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 7 647 | 9 162 | 9 089 | 8 986 | 9 020 |
| 130 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 542 | 7 513 | 7 423 | 51 408 | 48 966 |
| 131 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 498 | 7 505 | 7 481 | 43 986 | 43 970 |
| 132 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 6 165 | 9 865 | 7 575 | 3 820 | 0 |
| 133 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 5 619 | 5 609 | 5 793 | 43 571 | 42 651 |
| 134 | crystal (0.34)| [onyx](https://onyxframework.org) (0.5) | 4 912 | 5 034 | 5 026 | 5 109 | 5 149 |
| 135 | php (7.4)| [slim](https://slimframework.com) (4.5) | 4 658 | 4 712 | 4 732 | 43 220 | 40 718 |
| 136 | php (7.4)| [lumen](https://lumen.laravel.com) (7.0) | 4 323 | 4 318 | 4 407 | 41 646 | 37 498 |
| 137 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 142 | 4 122 | 4 204 | 42 437 | 39 424 |
| 138 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 747 | 3 540 | 3 540 | 3 511 | 3 523 |
| 139 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 183 | 3 187 | 3 216 | 41 467 | 39 102 |
| 140 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 775 | 2 789 | 2 847 | 39 311 | 40 397 |
| 141 | julia (1.4)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 2 655 | 8 211 | 6 495 | 5 121 | 3 216 |
| 142 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 434 | 2 428 | 2 428 | 2 430 | 2 373 |
| 143 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 615 | 1 632 | 1 614 | 1 605 | 1 602 |
| 144 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 585 | 1 526 | 1 515 | 1 510 | 1 518 |
| 145 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 1 505 | 1 533 | 1 044 | 1 727 | 1 063 |
| 146 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 433 | 1 467 | 1 504 | 38 293 | 36 990 |
| 147 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 464 | 421 | 2 543 | 35 786 | 32 772 |
| 148 | php (7.4)| [laravel](https://laravel.com) (7.6) | 209 | 154 | 2 571 | 22 917 | 21 052 |

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

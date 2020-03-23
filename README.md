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

:information_source:  Updated on **2020-03-22** :information_source:

> Benchmarking with [wrk](https://github.com/ioquatix/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | javascript (13.7)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.1) | 187 720 | 202 773 | 205 894 | 204 328 | 204 728 |
| 2 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 184 857 | 197 941 | 200 050 | 197 004 | 197 661 |
| 3 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 173 898 | 186 177 | 190 070 | 191 116 | 191 710 |
| 4 | javascript (13.7)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (1.1) | 172 170 | 187 277 | 189 933 | 187 412 | 187 251 |
| 5 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 159 401 | 168 448 | 171 437 | 167 690 | 167 318 |
| 6 | go (1.14)| [fiber](https://fiber.wiki) (1.8) | 159 179 | 159 785 | 162 650 | 155 651 | 155 180 |
| 7 | go (1.14)| [router](https://github.com/fasthttp/router) (0.7) | 159 107 | 168 350 | 171 703 | 167 663 | 167 686 |
| 8 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 157 966 | 173 356 | 173 841 | 168 227 | 168 619 |
| 9 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 152 875 | 162 351 | 166 159 | 159 445 | 163 184 |
| 10 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.9) | 152 430 | 158 187 | 161 974 | 159 900 | 160 637 |
| 11 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.4) | 151 802 | 160 921 | 164 876 | 158 794 | 159 787 |
| 12 | crystal (0.33)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 151 272 | 160 681 | 155 297 | 148 888 | 148 564 |
| 13 | crystal (0.33)| [toro](https://github.com/soveran/toro) (0.4) | 148 960 | 158 157 | 155 954 | 144 282 | 145 656 |
| 14 | crystal (0.33)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 144 485 | 157 700 | 154 058 | 141 628 | 137 236 |
| 15 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 143 303 | 154 311 | 155 569 | 152 187 | 152 698 |
| 16 | crystal (0.33)| [lucky](https://luckyframework.org) (0.18) | 133 893 | 140 538 | 132 219 | 124 694 | 116 445 |
| 17 | crystal (0.33)| [amber](https://amberframework.org) (0.33) | 132 914 | 144 449 | 139 929 | 130 692 | 129 739 |
| 18 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 132 753 | 153 279 | 156 005 | 159 003 | 158 053 |
| 19 | crystal (0.33)| [kemal](https://kemalcr.com) (0.26) | 131 656 | 143 581 | 140 797 | 132 380 | 133 582 |
| 20 | crystal (0.33)| [orion](https://github.com/obsidian/orion) (2.1) | 130 645 | 136 370 | 133 746 | 123 405 | 120 153 |
| 21 | crystal (0.33)| [grip](https://github.com/grip-framework/grip) (0.28) | 130 303 | 141 659 | 143 396 | 134 793 | 132 305 |
| 22 | java (8)| [act](https://actframework.org) (1.8) | 118 922 | 132 403 | 132 125 | 128 654 | 128 545 |
| 23 | crystal (0.33)| [athena](https://github.com/athena-framework/athena) (0.8) | 109 849 | 121 807 | 113 582 | 101 642 | 99 577 |
| 24 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 109 195 | 107 226 | 113 610 | 113 015 | 113 064 |
| 25 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 107 021 | 100 977 | 113 538 | 113 915 | 114 471 |
| 26 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.0) | 106 355 | 105 408 | 109 258 | 109 358 | 109 161 |
| 27 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 104 307 | 104 678 | 107 870 | 107 708 | 107 146 |
| 28 | go (1.14)| [violetear](https://violetear.org) (7.0) | 102 850 | 102 813 | 106 315 | 106 815 | 106 665 |
| 29 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 101 257 | 100 257 | 104 227 | 104 617 | 104 204 |
| 30 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 100 924 | 180 905 | 158 010 | 180 530 | 177 790 |
| 31 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 98 483 | 97 554 | 103 765 | 103 143 | 101 157 |
| 32 | go (1.14)| [beego](https://beego.me) (1.12) | 97 872 | 101 697 | 105 290 | 105 348 | 104 955 |
| 33 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 97 017 | 104 618 | 106 375 | 95 737 | 98 621 |
| 34 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 96 783 | 96 070 | 99 821 | 100 630 | 100 491 |
| 35 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 96 518 | 100 901 | 103 653 | 103 806 | 103 618 |
| 36 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 95 157 | 104 008 | 105 091 | 102 558 | 102 797 |
| 37 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 94 965 | 90 878 | 96 195 | 95 140 | 94 589 |
| 38 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 93 287 | 94 129 | 97 559 | 97 221 | 96 714 |
| 39 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 93 139 | 110 365 | 119 006 | 121 252 | 117 889 |
| 40 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 92 042 | 102 713 | 102 777 | 99 137 | 97 728 |
| 41 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 88 061 | 93 208 | 91 096 | 88 231 | 87 946 |
| 42 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.1) | 87 907 | 96 290 | 96 810 | 94 613 | 94 304 |
| 43 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 87 383 | 88 079 | 93 378 | 94 839 | 97 783 |
| 44 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 85 009 | 87 510 | 88 700 | 89 279 | 90 475 |
| 45 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 82 694 | 88 000 | 87 642 | 83 748 | 83 046 |
| 46 | go (1.14)| [air](https://github.com/aofei/air) (0.15) | 81 058 | 81 013 | 83 192 | 84 505 | 83 125 |
| 47 | java (8)| [javalin](https://javalin.io) (3.5) | 77 569 | 82 352 | 80 793 | 78 085 | 78 658 |
| 48 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 77 000 | 82 853 | 80 249 | 78 149 | 78 345 |
| 49 | go (1.14)| [gf](https://goframe.org) (1.11) | 75 572 | 82 616 | 83 110 | 81 927 | 84 624 |
| 50 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 72 894 | 76 471 | 75 089 | 73 870 | 73 664 |
| 51 | javascript (13.7)| [fastify](https://fastify.io) (2.13) | 70 850 | 74 003 | 72 016 | 70 446 | 69 704 |
| 52 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 70 453 | 76 315 | 75 112 | 73 712 | 71 390 |
| 53 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 68 723 | 71 749 | 70 740 | 69 879 | 69 877 |
| 54 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 65 895 | 79 068 | 81 344 | 79 880 | 80 280 |
| 55 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 64 358 | 75 093 | 77 251 | 81 928 | 81 470 |
| 56 | java (8)| [micronaut](https://micronaut.io) (1.2) | 63 882 | 70 083 | 69 915 | 67 776 | 67 838 |
| 57 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 63 357 | 67 364 | 66 173 | 62 977 | 62 801 |
| 58 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 62 739 | 66 383 | 69 413 | 69 667 | 69 311 |
| 59 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 62 199 | 67 948 | 68 839 | 68 248 | 68 228 |
| 60 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 60 825 | 72 418 | 71 838 | 71 248 | 68 898 |
| 61 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.14) | 60 754 | 66 423 | 66 862 | 64 405 | 64 632 |
| 62 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 60 539 | 60 806 | 62 122 | 59 918 | 59 439 |
| 63 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 59 389 | 62 003 | 62 140 | 61 254 | 61 158 |
| 64 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 58 423 | 62 038 | 62 158 | 60 618 | 59 918 |
| 65 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 56 398 | 54 886 | 54 885 | 54 208 | 54 418 |
| 66 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 55 753 | 64 072 | 63 447 | 61 729 | 62 324 |
| 67 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 54 450 | 55 994 | 55 839 | 55 527 | 55 331 |
| 68 | c (99)| [kore](https://kore.io) (3.3) | 53 963 | 131 502 | 160 755 | 136 038 | 157 449 |
| 69 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 53 828 | 53 766 | 53 850 | 53 296 | 53 896 |
| 70 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 51 934 | 51 271 | 51 759 | 50 952 | 50 766 |
| 71 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 51 336 | 51 445 | 51 800 | 50 895 | 49 788 |
| 72 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 51 272 | 57 523 | 58 873 | 56 867 | 56 556 |
| 73 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 51 242 | 56 454 | 57 605 | 58 893 | 58 702 |
| 74 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 50 090 | 51 791 | 51 619 | 50 699 | 50 430 |
| 75 | javascript (13.7)| [moleculer](https://moleculer.services) (0.14) | 49 751 | 52 112 | 51 701 | 50 822 | 51 346 |
| 76 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 46 957 | 52 550 | 53 367 | 52 284 | 52 320 |
| 77 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 46 358 | 46 800 | 46 574 | 45 719 | 45 398 |
| 78 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 46 200 | 49 298 | 47 824 | 47 407 | 47 628 |
| 79 | python (3.8)| [starlette](https://starlette.io) (0.13) | 45 482 | 48 721 | 46 247 | 44 893 | 45 733 |
| 80 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 45 205 | 48 604 | 47 013 | 46 681 | 48 087 |
| 81 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 44 132 | 45 339 | 44 749 | 45 015 | 45 305 |
| 82 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 41 453 | 43 930 | 40 964 | 40 513 | 41 108 |
| 83 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 41 191 | 49 684 | 52 243 | 53 012 | 51 634 |
| 84 | php (7.4)| [imi](https://imiphp.com) (1.0) | 40 696 | 42 242 | 41 019 | 41 975 | 43 112 |
| 85 | javascript (13.7)| [restify](https://restify.com) (8.5) | 40 625 | 43 960 | 42 809 | 41 432 | 41 570 |
| 86 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 39 689 | 44 059 | 41 786 | 41 299 | 41 348 |
| 87 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 39 578 | 43 001 | 42 798 | 42 001 | 42 171 |
| 88 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 39 246 | 40 854 | 40 531 | 39 775 | 40 590 |
| 89 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.3) | 37 790 | 39 545 | 38 260 | 37 866 | 37 748 |
| 90 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0b1) | 37 136 | 39 323 | 37 957 | 37 864 | 37 350 |
| 91 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 36 252 | 34 919 | 34 233 | 38 033 | 39 509 |
| 92 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 35 398 | 36 912 | 35 975 | 35 001 | 35 678 |
| 93 | python (3.8)| [hug](https://hug.rest) (2.6) | 34 250 | 46 962 | 44 278 | 45 533 | 46 217 |
| 94 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 33 765 | 36 049 | 34 886 | 34 480 | 34 813 |
| 95 | dart (2.7)| [aqueduct](https://aqueduct.io) (3.2) | 32 667 | 32 585 | 33 048 | 32 348 | 32 411 |
| 96 | php (7.4)| [swoft](https://swoft.org) (2.0) | 31 594 | 33 949 | 35 144 | 34 666 | 33 722 |
| 97 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.52) | 31 513 | 33 199 | 32 712 | 31 536 | 31 479 |
| 98 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 31 393 | 32 215 | 31 632 | 31 030 | 30 687 |
| 99 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 29 542 | 29 893 | 29 280 | 29 355 | 29 498 |
| 100 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 26 433 | 26 223 | 25 842 | 25 632 | 25 753 |
| 101 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 26 176 | 27 485 | 27 771 | 26 549 | 26 879 |
| 102 | python (3.8)| [responder](https://python-responder.org) (2.0) | 25 442 | 28 009 | 27 147 | 25 405 | 26 030 |
| 103 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 25 094 | 26 334 | 25 743 | 25 872 | 25 350 |
| 104 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 24 476 | 26 343 | 26 124 | 25 509 | 25 642 |
| 105 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 24 176 | 22 903 | 21 978 | 21 186 | 21 389 |
| 106 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 24 013 | 24 897 | 24 777 | 23 602 | 24 371 |
| 107 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 22 485 | 23 853 | 23 997 | 23 688 | 23 830 |
| 108 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 21 487 | 21 725 | 21 150 | 20 381 | 20 380 |
| 109 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 18 737 | 18 669 | 18 744 | 18 653 | 18 654 |
| 110 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 270 | 17 519 | 17 844 | 17 639 | 17 782 |
| 111 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 15 773 | 15 909 | 15 925 | 15 941 | 15 981 |
| 112 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 14 976 | 15 442 | 15 423 | 15 421 | 15 446 |
| 113 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 13 928 | 13 764 | 13 389 | 13 925 | 13 516 |
| 114 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 12 499 | 12 332 | 12 375 | 12 334 | 12 317 |
| 115 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 12 454 | 12 223 | 12 522 | 11 977 | 12 063 |
| 116 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 802 | 11 859 | 11 843 | 11 878 | 11 981 |
| 117 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 11 712 | 12 661 | 11 969 | 11 016 | 11 067 |
| 118 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 093 | 10 271 | 10 308 | 8 400 | 10 305 |
| 119 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 533 | 10 471 | 10 349 | 10 227 | 10 141 |
| 120 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 9 319 | 9 341 | 9 452 | 9 269 | 9 387 |
| 121 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 066 | 8 987 | 8 907 | 52 666 | 50 636 |
| 122 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 7 420 | 7 294 | 7 433 | 40 890 | 40 614 |
| 123 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 333 | 7 036 | 7 149 | 49 796 | 48 468 |
| 124 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 203 | 7 365 | 7 224 | 42 904 | 40 441 |
| 125 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 5 598 | 5 562 | 5 630 | 43 643 | 40 389 |
| 126 | crystal (0.33)| [onyx](https://onyxframework.org) (0.5) | 5 472 | 5 241 | 5 181 | 5 283 | 5 559 |
| 127 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 298 | 4 405 | 4 443 | 43 131 | 40 089 |
| 128 | php (7.4)| [lumen](https://lumen.laravel.com) (7.0) | 4 244 | 4 094 | 3 963 | 41 617 | 36 347 |
| 129 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 040 | 4 198 | 4 231 | 39 199 | 37 656 |
| 130 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 607 | 3 630 | 3 674 | 42 176 | 40 674 |
| 131 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 513 | 3 509 | 3 586 | 3 491 | 3 533 |
| 132 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 824 | 2 837 | 2 920 | 40 774 | 38 506 |
| 133 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 424 | 2 423 | 2 410 | 2 408 | 2 399 |
| 134 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 1 868 | 5 305 | 4 168 | 3 324 | 2 021 |
| 135 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 589 | 1 609 | 1 586 | 1 580 | 1 585 |
| 136 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 562 | 1 529 | 1 499 | 1 502 | 1 470 |
| 137 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 401 | 1 433 | 1 453 | 36 479 | 33 902 |
| 138 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 936 | 889 | 1 755 | 1 744 | 674 |
| 139 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 569 | 441 | 2 128 | 34 193 | 34 501 |
| 140 | php (7.4)| [laravel](https://laravel.com) (7.2) | 186 | 161 | 3 079 | 22 479 | 22 334 |

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

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

:information_source:  Updated on **2020-04-02** :information_source:

> Benchmarking with [wrk](https://github.com/ioquatix/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | javascript (13.7)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.11) | 191 330 | 204 548 | 205 033 | 203 105 | 202 901 |
| 2 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 189 340 | 201 105 | 202 179 | 200 382 | 200 346 |
| 3 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (2.2) | 175 595 | 188 025 | 188 795 | 184 786 | 184 740 |
| 4 | javascript (13.7)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.0) | 173 515 | 180 527 | 179 429 | 176 449 | 179 231 |
| 5 | go (1.14)| [fiber](https://fiber.wiki) (1.8) | 156 379 | 160 687 | 159 346 | 150 340 | 150 198 |
| 6 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.9) | 154 259 | 163 557 | 165 609 | 159 934 | 159 482 |
| 7 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 148 904 | 157 614 | 160 410 | 155 254 | 154 785 |
| 8 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.0) | 148 608 | 157 605 | 160 165 | 154 600 | 155 231 |
| 9 | crystal (0.33)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 147 933 | 154 407 | 151 725 | 144 629 | 143 395 |
| 10 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 147 722 | 156 196 | 159 190 | 153 677 | 153 442 |
| 11 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 147 324 | 160 009 | 164 388 | 160 375 | 161 605 |
| 12 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.0) | 146 924 | 155 395 | 157 840 | 152 626 | 152 448 |
| 13 | crystal (0.33)| [ricr](https://ricr-web.github.io/ricr) (0.1) | 146 787 | 153 675 | 150 365 | 142 380 | 142 742 |
| 14 | crystal (0.33)| [toro](https://github.com/soveran/toro) (0.4) | 146 636 | 152 242 | 149 911 | 141 755 | 142 153 |
| 15 | crystal (0.33)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 144 923 | 151 950 | 148 917 | 141 406 | 140 778 |
| 16 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 137 654 | 147 625 | 149 286 | 147 275 | 146 526 |
| 17 | crystal (0.33)| [grip](https://github.com/grip-framework/grip) (0.28) | 137 371 | 143 084 | 139 488 | 132 665 | 131 925 |
| 18 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 136 872 | 146 042 | 146 793 | 143 033 | 143 014 |
| 19 | crystal (0.33)| [kemal](https://kemalcr.com) (0.26) | 135 571 | 141 655 | 139 536 | 131 700 | 131 183 |
| 20 | crystal (0.33)| [amber](https://amberframework.org) (0.33) | 130 627 | 135 684 | 132 329 | 125 065 | 124 777 |
| 21 | rust (1.41)| [actix](https://actix.rs) (2.0) | 128 416 | 131 113 | 131 371 | 128 886 | 127 365 |
| 22 | crystal (0.33)| [lucky](https://luckyframework.org) (0.19) | 125 059 | 129 215 | 124 971 | 116 669 | 115 747 |
| 23 | crystal (0.33)| [orion](https://github.com/obsidian/orion) (2.3) | 123 194 | 126 382 | 122 005 | 112 561 | 111 741 |
| 24 | crystal (0.33)| [athena](https://github.com/athena-framework/athena) (0.8) | 119 434 | 120 522 | 112 517 | 104 637 | 104 499 |
| 25 | java (8)| [act](https://actframework.org) (1.8) | 111 855 | 125 493 | 121 756 | 121 889 | 123 973 |
| 26 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 105 129 | 105 111 | 107 764 | 107 250 | 106 481 |
| 27 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 103 838 | 103 867 | 107 000 | 106 055 | 105 875 |
| 28 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.0) | 100 081 | 98 982 | 101 621 | 101 410 | 100 973 |
| 29 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 98 630 | 102 573 | 104 838 | 103 730 | 103 098 |
| 30 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 97 795 | 98 177 | 100 692 | 100 183 | 99 545 |
| 31 | c (99)| [kore](https://kore.io) (3.3) | 97 724 | 144 333 | 149 100 | 148 478 | 143 385 |
| 32 | go (1.14)| [violetear](https://violetear.org) (7.0) | 96 484 | 96 383 | 99 563 | 99 443 | 98 540 |
| 33 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 95 091 | 112 906 | 115 943 | 116 044 | 115 812 |
| 34 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 94 817 | 93 528 | 96 320 | 96 169 | 95 721 |
| 35 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 94 525 | 94 045 | 96 926 | 96 840 | 95 553 |
| 36 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 94 399 | 98 122 | 99 432 | 97 767 | 97 504 |
| 37 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 92 898 | 103 580 | 102 668 | 100 122 | 98 697 |
| 38 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 92 505 | 91 180 | 93 621 | 93 522 | 93 061 |
| 39 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 92 482 | 98 997 | 98 436 | 97 884 | 98 763 |
| 40 | go (1.14)| [beego](https://beego.me) (1.12) | 91 263 | 94 961 | 97 025 | 96 505 | 96 508 |
| 41 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 90 038 | 89 424 | 92 182 | 92 527 | 92 214 |
| 42 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.2) | 89 591 | 97 422 | 97 269 | 93 993 | 93 168 |
| 43 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 89 280 | 93 196 | 95 376 | 94 758 | 93 915 |
| 44 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 87 993 | 92 125 | 91 680 | 87 967 | 87 587 |
| 45 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 86 572 | 89 759 | 90 826 | 90 972 | 90 829 |
| 46 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 84 328 | 95 010 | 94 742 | 90 420 | 92 196 |
| 47 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 82 918 | 87 597 | 83 942 | 80 979 | 80 258 |
| 48 | go (1.14)| [air](https://github.com/aofei/air) (0.15) | 78 553 | 82 358 | 84 142 | 83 928 | 83 582 |
| 49 | php (7.4)| [simps](https://simps.io) (preview) | 78 324 | 82 121 | 82 247 | 82 135 | 81 958 |
| 50 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 77 853 | 163 855 | 176 002 | 172 966 | 169 864 |
| 51 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 74 707 | 81 304 | 85 658 | 86 710 | 86 067 |
| 52 | java (8)| [javalin](https://javalin.io) (3.5) | 74 165 | 77 201 | 76 884 | 76 807 | 77 447 |
| 53 | go (1.14)| [gf](https://goframe.org) (1.12) | 73 020 | 78 201 | 79 681 | 79 303 | 78 799 |
| 54 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 71 954 | 75 759 | 72 941 | 71 334 | 72 412 |
| 55 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 71 713 | 76 079 | 75 054 | 72 574 | 72 483 |
| 56 | javascript (13.7)| [fastify](https://fastify.io) (2.13) | 69 830 | 73 861 | 71 092 | 69 506 | 69 716 |
| 57 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 66 517 | 73 143 | 71 874 | 69 068 | 69 961 |
| 58 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 65 357 | 68 696 | 70 065 | 68 927 | 68 759 |
| 59 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 62 000 | 64 885 | 63 405 | 61 550 | 61 692 |
| 60 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 61 995 | 65 455 | 65 809 | 65 031 | 65 123 |
| 61 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 61 198 | 71 850 | 70 603 | 75 462 | 69 573 |
| 62 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 60 540 | 61 144 | 60 438 | 59 102 | 59 060 |
| 63 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 60 047 | 62 480 | 61 058 | 61 186 | 61 249 |
| 64 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 59 698 | 68 525 | 66 862 | 63 457 | 63 550 |
| 65 | java (8)| [micronaut](https://micronaut.io) (1.2) | 58 861 | 66 843 | 65 410 | 65 137 | 61 138 |
| 66 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 58 737 | 62 143 | 64 519 | 63 996 | 63 901 |
| 67 | javascript (13.7)| [feathersjs](https://feathersjs.com) (4.5) | 56 522 | 59 088 | 57 802 | 56 434 | 56 710 |
| 68 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 56 153 | 59 530 | 57 902 | 57 029 | 56 036 |
| 69 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 55 287 | 58 789 | 59 251 | 58 866 | 59 579 |
| 70 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.14) | 55 009 | 61 703 | 61 487 | 58 984 | 58 727 |
| 71 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 54 057 | 53 980 | 53 584 | 53 670 | 53 595 |
| 72 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 53 450 | 54 879 | 54 561 | 54 362 | 54 175 |
| 73 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 52 505 | 59 855 | 60 746 | 60 973 | 61 670 |
| 74 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 51 274 | 50 076 | 50 759 | 50 312 | 49 010 |
| 75 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 50 802 | 55 576 | 55 600 | 53 355 | 53 307 |
| 76 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 50 244 | 57 289 | 58 139 | 58 625 | 58 781 |
| 77 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 50 145 | 50 814 | 50 843 | 50 215 | 49 812 |
| 78 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 49 600 | 49 919 | 50 138 | 49 342 | 46 895 |
| 79 | javascript (13.7)| [moleculer](https://moleculer.services) (0.14) | 49 456 | 52 176 | 50 511 | 51 102 | 50 979 |
| 80 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 47 936 | 52 291 | 51 855 | 49 917 | 50 000 |
| 81 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 47 298 | 49 774 | 49 658 | 48 805 | 48 762 |
| 82 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 46 907 | 47 060 | 47 180 | 46 686 | 46 353 |
| 83 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 46 509 | 48 391 | 48 447 | 47 876 | 47 845 |
| 84 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 46 399 | 49 320 | 48 037 | 47 537 | 47 268 |
| 85 | python (3.8)| [hug](https://hug.rest) (2.6) | 45 194 | 46 611 | 46 319 | 45 845 | 45 896 |
| 86 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 43 660 | 48 302 | 51 581 | 52 556 | 54 059 |
| 87 | javascript (13.7)| [restify](https://restify.com) (8.5) | 43 032 | 45 577 | 45 053 | 44 272 | 44 094 |
| 88 | python (3.8)| [starlette](https://starlette.io) (0.13) | 41 655 | 45 065 | 44 930 | 43 390 | 43 319 |
| 89 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 39 860 | 37 708 | 39 280 | 40 147 | 40 038 |
| 90 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 39 239 | 39 955 | 39 872 | 38 744 | 38 501 |
| 91 | php (7.4)| [imi](https://imiphp.com) (1.0) | 38 932 | 40 241 | 40 141 | 39 716 | 40 012 |
| 92 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 38 325 | 40 388 | 40 391 | 39 813 | 39 794 |
| 93 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 37 975 | 38 328 | 36 436 | 35 921 | 36 406 |
| 94 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 37 874 | 39 058 | 37 408 | 37 186 | 37 298 |
| 95 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0b1) | 37 446 | 39 686 | 39 223 | 37 649 | 37 545 |
| 96 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.3) | 35 873 | 36 083 | 34 645 | 34 740 | 34 750 |
| 97 | php (7.4)| [swoft](https://swoft.org) (2.0) | 34 960 | 35 967 | 35 755 | 35 368 | 35 334 |
| 98 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 34 887 | 35 550 | 35 230 | 34 061 | 34 028 |
| 99 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 32 019 | 32 054 | 31 231 | 31 297 | 31 296 |
| 100 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 29 788 | 30 335 | 30 027 | 28 969 | 28 945 |
| 101 | dart (2.7)| [aqueduct](https://aqueduct.io) (3.2) | 29 680 | 28 996 | 30 271 | 29 553 | 29 287 |
| 102 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.53) | 28 599 | 29 745 | 30 369 | 29 200 | 28 502 |
| 103 | python (3.8)| [responder](https://python-responder.org) (2.0) | 28 466 | 30 081 | 29 756 | 28 693 | 28 858 |
| 104 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 26 531 | 25 934 | 25 733 | 25 976 | 25 659 |
| 105 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 25 752 | 25 486 | 24 866 | 24 910 | 25 211 |
| 106 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 24 763 | 26 541 | 26 473 | 25 573 | 25 495 |
| 107 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 24 731 | 25 089 | 24 976 | 24 591 | 24 670 |
| 108 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 24 438 | 23 744 | 23 290 | 23 290 | 23 281 |
| 109 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 23 806 | 25 974 | 25 622 | 25 292 | 25 455 |
| 110 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 23 407 | 23 530 | 23 329 | 22 934 | 22 879 |
| 111 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 23 404 | 23 768 | 23 502 | 23 136 | 23 267 |
| 112 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 781 | 21 258 | 20 352 | 20 234 | 20 161 |
| 113 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 18 876 | 19 170 | 19 006 | 19 111 | 19 073 |
| 114 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 572 | 17 788 | 17 866 | 17 658 | 17 684 |
| 115 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 17 026 | 17 198 | 16 946 | 16 423 | 17 280 |
| 116 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 15 081 | 14 877 | 14 861 | 14 833 | 14 886 |
| 117 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 12 979 | 12 830 | 12 833 | 12 736 | 12 784 |
| 118 | javascript (13.7)| [sails](https://sailsjs.com) (1.2) | 12 312 | 13 491 | 12 959 | 12 813 | 12 938 |
| 119 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 11 670 | 11 707 | 10 971 | 10 497 | 10 518 |
| 120 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 11 097 | 10 863 | 10 838 | 10 772 | 10 769 |
| 121 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 10 371 | 10 313 | 10 304 | 10 275 | 10 225 |
| 122 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 9 844 | 10 289 | 10 327 | 10 208 | 10 393 |
| 123 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 630 | 9 802 | 9 423 | 4 967 | 4 926 |
| 124 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 074 | 10 225 | 9 711 | 9 601 | 9 688 |
| 125 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 897 | 8 889 | 8 783 | 47 279 | 47 192 |
| 126 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 8 847 | 8 922 | 8 814 | 8 455 | 6 997 |
| 127 | python (3.8)| [django](https://djangoproject.com) (3.0) | 8 105 | 9 252 | 9 320 | 9 141 | 7 437 |
| 128 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 7 676 | 7 634 | 7 594 | 42 536 | 39 879 |
| 129 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 312 | 7 331 | 7 303 | 49 670 | 46 442 |
| 130 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 265 | 7 299 | 7 171 | 40 152 | 39 087 |
| 131 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 5 500 | 5 510 | 5 457 | 41 677 | 41 604 |
| 132 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 481 | 4 539 | 4 658 | 41 184 | 37 312 |
| 133 | php (7.4)| [lumen](https://lumen.laravel.com) (7.0) | 4 128 | 4 141 | 4 222 | 40 616 | 39 014 |
| 134 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 3 973 | 3 976 | 4 020 | 40 801 | 37 398 |
| 135 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 629 | 3 465 | 3 449 | 3 436 | 3 436 |
| 136 | crystal (0.33)| [onyx](https://onyxframework.org) (0.5) | 3 289 | 3 675 | 3 690 | 3 684 | 3 700 |
| 137 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 051 | 3 060 | 3 115 | 39 271 | 38 647 |
| 138 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 772 | 2 781 | 2 882 | 40 321 | 36 751 |
| 139 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 390 | 2 437 | 2 385 | 2 373 | 2 370 |
| 140 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 1 828 | 5 427 | 4 233 | 3 319 | 1 965 |
| 141 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 652 | 1 664 | 1 634 | 1 622 | 1 614 |
| 142 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 512 | 1 484 | 1 462 | 1 428 | 1 432 |
| 143 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 382 | 1 421 | 1 470 | 36 827 | 34 761 |
| 144 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 814 | 1 670 | 1 476 | 1 723 | 566 |
| 145 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 618 | 488 | 1 110 | 34 444 | 32 540 |
| 146 | php (7.4)| [laravel](https://laravel.com) (7.4) | 173 | 161 | 2 730 | 22 944 | 20 950 |

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

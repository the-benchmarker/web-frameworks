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

:information_source:  Updated on **2020-03-04** :information_source:

> Benchmarking with [wrk](https://github.com/ioquatix/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 5s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 182 191 | 197 200 | 197 336 | 192 315 | 189 356 |
| 2 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 181 350 | 194 381 | 193 724 | 190 653 | 191 699 |
| 3 | javascript (13.7)| [nanoexpress](https://nanoexpress.js.org) (1.1) | 174 151 | 186 432 | 186 334 | 183 824 | 184 074 |
| 4 | go (1.14)| [fasthttp](https://godoc.org/github.com/valyala/fasthttp) (1.9) | 164 994 | 174 223 | 177 712 | 173 615 | 173 364 |
| 5 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 161 466 | 177 310 | 177 859 | 173 374 | 173 431 |
| 6 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.4) | 159 872 | 168 596 | 172 087 | 168 427 | 167 972 |
| 7 | go (1.14)| [router](https://github.com/fasthttp/router) (0.6) | 159 760 | 168 941 | 172 328 | 168 684 | 168 561 |
| 8 | go (1.14)| [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (0.1) | 159 338 | 168 391 | 171 771 | 167 871 | 167 502 |
| 9 | crystal (0.33)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 158 121 | 166 326 | 163 489 | 155 595 | 155 223 |
| 10 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 157 664 | 166 174 | 169 520 | 165 717 | 165 522 |
| 11 | crystal (0.33)| [toro](https://github.com/soveran/toro) (0.4) | 157 638 | 166 057 | 164 312 | 155 770 | 155 235 |
| 12 | crystal (0.33)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 154 276 | 161 871 | 158 771 | 150 927 | 150 837 |
| 13 | go (1.14)| [fiber](https://fiber.wiki) (1.8) | 154 065 | 155 758 | 154 366 | 146 916 | 147 253 |
| 14 | crystal (0.33)| [kemal](https://kemalcr.com) (0.26) | 147 192 | 154 447 | 151 025 | 142 471 | 142 492 |
| 15 | crystal (0.33)| [grip](https://github.com/grip-framework/grip) (0.27) | 143 693 | 150 608 | 148 011 | 138 569 | 138 766 |
| 16 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 141 446 | 153 738 | 153 203 | 146 643 | 145 398 |
| 17 | crystal (0.33)| [amber](https://amberframework.org) (0.33) | 140 374 | 146 905 | 143 974 | 133 557 | 133 123 |
| 18 | crystal (0.33)| [orion](https://github.com/obsidian/orion) (2.1) | 135 605 | 140 424 | 135 409 | 124 816 | 124 671 |
| 19 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (3.5) | 134 140 | 143 200 | 145 000 | 144 102 | 143 393 |
| 20 | crystal (0.33)| [athena](https://github.com/athena-framework/athena) (0.8) | 129 241 | 133 097 | 127 453 | 114 708 | 115 111 |
| 21 | java (8)| [act](https://actframework.org) (1.8) | 121 052 | 133 757 | 133 146 | 130 306 | 130 152 |
| 22 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 112 625 | 112 147 | 116 487 | 116 314 | 115 527 |
| 23 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 111 286 | 111 275 | 115 383 | 115 323 | 114 866 |
| 24 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.0) | 106 832 | 106 107 | 109 627 | 110 231 | 109 795 |
| 25 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 106 339 | 110 440 | 113 473 | 113 103 | 112 917 |
| 26 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 104 823 | 104 841 | 108 420 | 108 152 | 107 836 |
| 27 | go (1.14)| [violetear](https://violetear.org) (7.0) | 103 912 | 103 821 | 107 808 | 108 106 | 107 901 |
| 28 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 101 944 | 100 432 | 104 359 | 105 056 | 104 628 |
| 29 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 101 899 | 100 851 | 104 763 | 105 349 | 105 071 |
| 30 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 101 408 | 105 069 | 107 424 | 105 958 | 105 760 |
| 31 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 101 217 | 122 127 | 126 348 | 127 347 | 126 981 |
| 32 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 99 014 | 97 562 | 101 435 | 102 165 | 101 682 |
| 33 | go (1.14)| [beego](https://beego.me) (1.12) | 98 954 | 102 694 | 106 275 | 106 164 | 105 852 |
| 34 | go (1.14)| [gin](https://gin-gonic.com) (1.5) | 97 988 | 101 385 | 104 598 | 104 864 | 104 479 |
| 35 | c (99)| [kore](https://kore.io) (3.3) | 97 880 | 143 209 | 128 366 | 153 394 | 151 106 |
| 36 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 96 858 | 96 132 | 99 752 | 100 951 | 100 543 |
| 37 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 95 479 | 104 784 | 104 062 | 99 473 | 99 470 |
| 38 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 95 279 | 104 174 | 105 296 | 102 978 | 102 933 |
| 39 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 91 851 | 100 860 | 99 807 | 95 707 | 95 258 |
| 40 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 88 271 | 93 451 | 94 711 | 94 053 | 93 850 |
| 41 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.1) | 86 952 | 95 252 | 93 911 | 90 210 | 89 971 |
| 42 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 86 348 | 90 686 | 88 538 | 85 580 | 85 674 |
| 43 | go (1.14)| [air](https://github.com/aofei/air) (0.15) | 85 035 | 89 386 | 91 935 | 92 309 | 91 854 |
| 44 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 84 352 | 88 864 | 86 719 | 83 679 | 83 435 |
| 45 | go (1.14)| [gf](https://goframe.org) (1.11) | 79 280 | 84 575 | 86 566 | 86 745 | 86 537 |
| 46 | java (8)| [javalin](https://javalin.io) (3.5) | 77 728 | 83 006 | 81 984 | 79 252 | 78 886 |
| 47 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 77 501 | 83 834 | 80 917 | 78 579 | 79 785 |
| 48 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 75 302 | 165 851 | 166 881 | 188 958 | 184 373 |
| 49 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 74 156 | 77 890 | 78 381 | 76 817 | 76 964 |
| 50 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 70 926 | 78 362 | 83 173 | 83 188 | 82 811 |
| 51 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 70 525 | 76 531 | 75 769 | 74 170 | 73 860 |
| 52 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 70 083 | 74 236 | 72 611 | 70 169 | 69 721 |
| 53 | javascript (13.7)| [fastify](https://fastify.io) (2.12) | 69 490 | 72 738 | 70 540 | 68 228 | 68 538 |
| 54 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 69 062 | 71 890 | 70 159 | 68 639 | 68 634 |
| 55 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 66 744 | 80 012 | 82 120 | 80 514 | 80 899 |
| 56 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 63 736 | 66 942 | 70 136 | 70 483 | 70 463 |
| 57 | java (8)| [micronaut](https://micronaut.io) (1.2) | 63 428 | 71 571 | 71 392 | 69 375 | 69 435 |
| 58 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 63 188 | 64 997 | 66 123 | 64 615 | 63 998 |
| 59 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 62 427 | 64 902 | 62 784 | 61 427 | 61 656 |
| 60 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 60 763 | 63 100 | 61 444 | 59 944 | 59 914 |
| 61 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.12) | 59 369 | 65 618 | 65 554 | 62 967 | 63 097 |
| 62 | php (7.4)| [one](https://github.com/lizhichao/one) (1.9) | 59 217 | 63 130 | 63 542 | 62 972 | 62 864 |
| 63 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 57 665 | 57 482 | 56 795 | 57 222 | 56 650 |
| 64 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 57 503 | 65 108 | 64 351 | 63 508 | 63 939 |
| 65 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 55 575 | 57 687 | 56 605 | 55 824 | 55 758 |
| 66 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 55 521 | 57 005 | 56 920 | 56 657 | 56 528 |
| 67 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 54 323 | 56 919 | 58 825 | 57 839 | 57 892 |
| 68 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 53 727 | 58 705 | 58 592 | 56 574 | 56 372 |
| 69 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 53 450 | 60 331 | 61 777 | 62 953 | 62 791 |
| 70 | javascript (13.7)| [moleculer](https://moleculer.services) (0.14) | 50 733 | 52 786 | 51 452 | 50 958 | 50 727 |
| 71 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 50 375 | 50 353 | 50 039 | 49 376 | 49 237 |
| 72 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 49 840 | 51 533 | 51 752 | 51 353 | 51 354 |
| 73 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 49 833 | 54 493 | 54 014 | 52 241 | 52 241 |
| 74 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 49 808 | 51 094 | 50 646 | 49 922 | 49 909 |
| 75 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 49 135 | 49 234 | 49 500 | 48 809 | 48 769 |
| 76 | python (3.8)| [hug](https://hug.rest) (2.6) | 47 826 | 49 332 | 49 867 | 49 022 | 49 141 |
| 77 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 46 525 | 49 874 | 49 710 | 48 885 | 49 149 |
| 78 | python (3.8)| [starlette](https://starlette.io) (0.13) | 46 342 | 49 614 | 49 126 | 47 544 | 47 354 |
| 79 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 45 813 | 44 947 | 45 005 | 44 770 | 44 554 |
| 80 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 44 035 | 46 443 | 45 670 | 45 072 | 45 086 |
| 81 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 42 856 | 43 996 | 44 161 | 41 807 | 42 074 |
| 82 | php (7.4)| [imi](https://imiphp.com) (1.0) | 41 981 | 43 839 | 43 987 | 43 558 | 43 841 |
| 83 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 41 663 | 49 918 | 52 940 | 53 342 | 54 816 |
| 84 | javascript (13.7)| [restify](https://restify.com) (8.5) | 41 512 | 43 468 | 42 810 | 42 288 | 42 473 |
| 85 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 41 404 | 43 909 | 41 542 | 41 173 | 41 687 |
| 86 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 40 289 | 43 962 | 43 885 | 43 594 | 43 390 |
| 87 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0a5) | 40 014 | 43 433 | 43 164 | 41 691 | 41 752 |
| 88 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 39 833 | 43 080 | 41 012 | 40 634 | 40 658 |
| 89 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.29) | 39 072 | 40 465 | 38 565 | 38 464 | 38 705 |
| 90 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 37 908 | 39 388 | 39 129 | 38 487 | 37 923 |
| 91 | php (7.4)| [swoft](https://swoft.org) (2.0) | 36 409 | 37 626 | 37 304 | 36 842 | 36 861 |
| 92 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 35 802 | 36 409 | 35 325 | 34 977 | 35 187 |
| 93 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 33 320 | 30 654 | 35 306 | 35 750 | 32 831 |
| 94 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 32 732 | 33 703 | 33 258 | 32 530 | 32 707 |
| 95 | crystal (0.33)| [lucky](https://luckyframework.org) (0.18) | 31 619 | 28 470 | 26 809 | 23 152 | 21 771 |
| 96 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 30 020 | 29 835 | 29 344 | 29 280 | 29 404 |
| 97 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.5) | 29 502 | 32 042 | 31 622 | 30 301 | 30 299 |
| 98 | dart (2.7)| [aqueduct](https://aqueduct.io) (3.2) | 27 613 | 26 904 | 27 767 | 26 302 | 26 845 |
| 99 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 26 890 | 27 766 | 27 374 | 27 161 | 27 071 |
| 100 | python (3.8)| [responder](https://python-responder.org) (2.0) | 26 620 | 27 853 | 29 551 | 28 628 | 28 588 |
| 101 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 26 416 | 26 443 | 25 921 | 26 024 | 26 171 |
| 102 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 26 192 | 26 948 | 26 839 | 26 485 | 26 213 |
| 103 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 24 385 | 28 073 | 27 953 | 27 153 | 27 018 |
| 104 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 23 802 | 22 621 | 21 695 | 21 106 | 21 117 |
| 105 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 23 765 | 24 834 | 24 775 | 24 457 | 24 108 |
| 106 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 21 777 | 22 301 | 22 340 | 21 910 | 21 584 |
| 107 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 21 529 | 22 005 | 21 450 | 20 180 | 20 603 |
| 108 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 18 634 | 18 992 | 18 972 | 18 962 | 18 977 |
| 109 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 582 | 18 160 | 18 286 | 18 196 | 18 148 |
| 110 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 17 043 | 16 780 | 16 733 | 16 763 | 16 753 |
| 111 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 15 268 | 15 831 | 15 796 | 15 704 | 15 733 |
| 112 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 14 752 | 14 595 | 14 566 | 14 581 | 14 575 |
| 113 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 13 798 | 13 549 | 13 464 | 13 496 | 13 461 |
| 114 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 12 487 | 12 414 | 12 408 | 12 374 | 12 340 |
| 115 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 11 725 | 12 325 | 11 658 | 11 169 | 11 215 |
| 116 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 540 | 11 528 | 11 446 | 11 592 | 11 567 |
| 117 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 577 | 10 741 | 10 506 | 10 438 | 10 375 |
| 118 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 10 276 | 10 644 | 10 405 | 10 189 | 10 243 |
| 119 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 330 | 9 271 | 9 112 | 54 678 | 52 036 |
| 120 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 8 613 | 9 500 | 9 326 | 9 295 | 9 363 |
| 121 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (1.9) | 8 049 | 8 027 | 7 996 | 45 049 | 42 378 |
| 122 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 656 | 7 678 | 7 547 | 53 890 | 52 098 |
| 123 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 653 | 7 665 | 7 585 | 45 609 | 45 700 |
| 124 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 705 | 4 706 | 4 729 | 44 711 | 42 231 |
| 125 | crystal (0.33)| [onyx](https://onyxframework.org) (0.5) | 4 646 | 4 770 | 4 760 | 4 777 | 4 754 |
| 126 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 478 | 4 529 | 4 596 | 44 436 | 40 984 |
| 127 | php (7.4)| [lumen](https://lumen.laravel.com) (6.3) | 4 407 | 4 439 | 4 451 | 44 562 | 42 167 |
| 128 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 4 066 | 3 843 | 3 838 | 3 818 | 3 805 |
| 129 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 707 | 3 705 | 3 798 | 43 405 | 41 449 |
| 130 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 693 | 3 733 | 3 799 | 43 836 | 40 910 |
| 131 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 420 | 2 426 | 2 387 | 2 377 | 2 373 |
| 132 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 2 131 | 5 523 | 4 378 | 3 500 | 2 179 |
| 133 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 1 970 | 2 014 | 2 028 | 41 290 | 38 537 |
| 134 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 568 | 1 531 | 1 516 | 1 496 | 1 492 |
| 135 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 454 | 1 479 | 1 439 | 1 444 | 1 452 |
| 136 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 1 186 | 1 150 | 1 154 | 1 628 | 911 |
| 137 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 582 | 443 | 1 003 | 35 442 | 34 490 |
| 138 | php (7.4)| [laravel](https://laravel.com) (6.17) | 186 | 170 | 2 213 | 23 906 | 23 533 |

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

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

:information_source:  Updated on **2020-04-27** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 179 800 | 193 181 | 193 330 | 188 649 | 188 079 |
| 2 | javascript (13.13)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.11) | 178 904 | 192 811 | 195 297 | 185 587 | 194 241 |
| 3 | php (7.4)| [simps](https://simps.io) (1.0) | 175 787 | 184 525 | 186 318 | 183 226 | 183 188 |
| 4 | javascript (13.13)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.0) | 167 494 | 180 947 | 183 287 | 180 433 | 177 007 |
| 5 | kotlin (1.3)| [kooby](https://jooby.io) (2.8) | 161 814 | 175 077 | 174 952 | 181 261 | 181 713 |
| 6 | javascript (13.13)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 161 761 | 178 245 | 180 665 | 174 490 | 172 476 |
| 7 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 159 576 | 175 761 | 176 281 | 172 032 | 173 379 |
| 8 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.12) | 157 939 | 167 029 | 170 485 | 165 365 | 165 220 |
| 9 | java (8)| [jooby](https://jooby.io) (2.8) | 154 901 | 170 617 | 166 938 | 162 748 | 165 905 |
| 10 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.0) | 152 317 | 161 070 | 163 842 | 159 475 | 159 069 |
| 11 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 151 575 | 160 616 | 163 843 | 159 103 | 159 526 |
| 12 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 151 475 | 160 383 | 163 146 | 158 692 | 159 098 |
| 13 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.0) | 151 400 | 160 317 | 163 887 | 158 604 | 158 369 |
| 14 | go (1.14)| [fiber](https://fiber.wiki) (1.9) | 149 997 | 156 632 | 154 173 | 145 479 | 146 403 |
| 15 | crystal (0.34)| [ricr](https://ricr-web.github.io/ricr) (0.2) | 149 902 | 157 167 | 152 551 | 142 810 | 141 272 |
| 16 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 148 664 | 160 540 | 161 459 | 159 813 | 159 231 |
| 17 | rust (1.43)| [actix](https://actix.rs) (2.0) | 139 173 | 140 680 | 140 631 | 136 443 | 135 876 |
| 18 | crystal (0.34)| [toro](https://github.com/soveran/toro) (0.4) | 138 337 | 152 560 | 148 295 | 144 510 | 146 601 |
| 19 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 138 021 | 146 918 | 147 405 | 143 440 | 143 937 |
| 20 | crystal (0.34)| [grip](https://github.com/grip-framework/grip) (0.28) | 137 521 | 140 485 | 133 478 | 123 688 | 127 245 |
| 21 | crystal (0.34)| [lucky](https://luckyframework.org) (0.20) | 131 989 | 136 720 | 131 911 | 125 221 | 125 090 |
| 22 | crystal (0.34)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 130 511 | 136 571 | 131 975 | 125 941 | 124 800 |
| 23 | crystal (0.34)| [spider-gazelle](https://spider-gazelle.net) (3.0) | 127 736 | 133 155 | 129 442 | 122 556 | 122 523 |
| 24 | crystal (0.34)| [kemal](https://kemalcr.com) (0.26) | 123 787 | 125 749 | 122 223 | 117 237 | 114 815 |
| 25 | crystal (0.34)| [athena](https://github.com/athena-framework/athena) (0.8) | 114 077 | 118 689 | 110 149 | 102 192 | 101 991 |
| 26 | crystal (0.34)| [orion](https://github.com/obsidian/orion) (2.3) | 113 067 | 119 104 | 119 048 | 110 415 | 111 400 |
| 27 | java (8)| [act](https://actframework.org) (1.8) | 110 868 | 128 298 | 127 745 | 127 377 | 129 279 |
| 28 | crystal (0.34)| [amber](https://amberframework.org) (0.34) | 109 608 | 115 279 | 115 431 | 110 889 | 110 579 |
| 29 | c (99)| [kore](https://kore.io) (3.3) | 108 879 | 159 199 | 140 292 | 152 646 | 145 920 |
| 30 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 107 508 | 107 813 | 110 904 | 109 669 | 109 853 |
| 31 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 105 993 | 105 870 | 108 945 | 108 764 | 108 266 |
| 32 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 102 097 | 105 929 | 108 940 | 107 506 | 107 097 |
| 33 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 101 506 | 100 607 | 103 519 | 103 060 | 102 596 |
| 34 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 99 686 | 100 591 | 102 029 | 102 526 | 102 436 |
| 35 | go (1.14)| [violetear](https://violetear.org) (7.0) | 99 262 | 99 577 | 102 426 | 102 566 | 102 162 |
| 36 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 98 681 | 117 131 | 122 191 | 121 722 | 121 543 |
| 37 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 98 209 | 101 739 | 103 226 | 101 477 | 101 335 |
| 38 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 97 977 | 97 130 | 100 105 | 99 862 | 99 714 |
| 39 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 97 793 | 96 975 | 99 752 | 99 944 | 99 606 |
| 40 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 95 431 | 93 951 | 96 745 | 96 290 | 96 453 |
| 41 | go (1.14)| [beego](https://beego.me) (1.12) | 94 041 | 97 453 | 100 210 | 99 617 | 99 299 |
| 42 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 93 074 | 97 236 | 99 314 | 98 668 | 98 485 |
| 43 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 93 011 | 98 979 | 99 798 | 97 416 | 98 096 |
| 44 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 92 223 | 91 927 | 94 893 | 95 291 | 94 775 |
| 45 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 88 042 | 168 508 | 174 836 | 174 426 | 176 068 |
| 46 | javascript (13.13)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 83 987 | 97 078 | 97 845 | 95 158 | 93 468 |
| 47 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 83 397 | 86 686 | 87 726 | 88 401 | 87 638 |
| 48 | javascript (13.13)| [polka](https://github.com/lukeed/polka) (0.5) | 80 097 | 84 104 | 80 165 | 73 461 | 75 559 |
| 49 | go (1.14)| [air](https://github.com/aofei/air) (0.15) | 79 859 | 84 463 | 86 258 | 86 275 | 85 665 |
| 50 | javascript (13.13)| [0http](https://github.com/jkyberneees/0http) (2.2) | 79 656 | 93 578 | 93 694 | 88 388 | 87 024 |
| 51 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 78 561 | 85 334 | 83 059 | 81 849 | 81 476 |
| 52 | javascript (13.13)| [rayo](https://rayo.js.org) (1.3) | 76 590 | 79 398 | 79 088 | 77 845 | 74 353 |
| 53 | javascript (13.13)| [restana](https://github.com/jkyberneees/ana) (4.3) | 75 693 | 84 669 | 84 963 | 81 021 | 81 949 |
| 54 | go (1.14)| [gf](https://goframe.org) (1.12) | 74 939 | 80 312 | 81 442 | 81 438 | 81 193 |
| 55 | java (8)| [javalin](https://javalin.io) (3.8) | 73 393 | 78 293 | 78 516 | 77 942 | 77 154 |
| 56 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 70 914 | 75 386 | 75 560 | 74 456 | 74 401 |
| 57 | swift (5.2)| [perfect](https://perfect.org) (3.1) | 69 766 | 79 996 | 83 171 | 85 557 | 83 569 |
| 58 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 67 578 | 81 202 | 83 347 | 82 650 | 82 852 |
| 59 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.2) | 67 306 | 73 330 | 71 160 | 71 521 | 71 449 |
| 60 | php (7.4)| [hyperf](https://www.hyperf.io) (1.1) | 65 452 | 68 026 | 68 730 | 68 135 | 68 099 |
| 61 | javascript (13.13)| [fastify](https://fastify.io) (2.13) | 65 440 | 69 185 | 66 322 | 64 328 | 66 901 |
| 62 | javascript (13.13)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 65 276 | 69 190 | 67 865 | 66 978 | 67 761 |
| 63 | javascript (13.13)| [foxify](https://foxify.js.org) (0.1) | 64 334 | 67 480 | 62 597 | 61 963 | 63 446 |
| 64 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 63 353 | 65 324 | 66 775 | 66 542 | 66 502 |
| 65 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 62 352 | 62 773 | 61 998 | 60 575 | 60 586 |
| 66 | java (8)| [micronaut](https://micronaut.io) (1.2) | 62 348 | 69 910 | 70 519 | 67 906 | 67 315 |
| 67 | javascript (13.13)| [koa](https://koajs.com) (2.11) | 61 073 | 63 601 | 62 522 | 61 012 | 60 581 |
| 68 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 60 417 | 63 718 | 66 227 | 65 918 | 65 880 |
| 69 | javascript (13.13)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 58 904 | 61 749 | 60 584 | 58 736 | 58 504 |
| 70 | javascript (13.13)| [nestjs-fastify](https://nestjs.com) (7.0) | 58 830 | 67 033 | 63 620 | 60 512 | 62 525 |
| 71 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 56 854 | 64 606 | 64 558 | 63 891 | 63 604 |
| 72 | rust (1.43)| [nickel](https://nickel-org.github.io) (0.11) | 56 368 | 56 536 | 55 940 | 56 102 | 56 712 |
| 73 | javascript (13.13)| [feathersjs](https://feathersjs.com) (4.5) | 54 422 | 56 544 | 55 494 | 54 464 | 54 297 |
| 74 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 53 859 | 53 730 | 53 383 | 52 282 | 52 630 |
| 75 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 53 717 | 57 738 | 57 516 | 56 765 | 56 620 |
| 76 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 53 623 | 54 943 | 54 773 | 54 627 | 54 304 |
| 77 | swift (5.2)| [kitura](https://kitura.io) (2.9) | 53 218 | 54 011 | 53 296 | 52 761 | 52 797 |
| 78 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.26) | 53 202 | 60 346 | 60 506 | 58 017 | 57 708 |
| 79 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 52 034 | 56 421 | 56 557 | 54 634 | 54 388 |
| 80 | scala (2.12)| [http4s](https://http4s.org) (0.21) | 50 964 | 57 162 | 55 545 | 53 335 | 52 303 |
| 81 | javascript (13.13)| [express](https://expressjs.com) (4.17) | 50 672 | 53 651 | 51 070 | 50 084 | 52 336 |
| 82 | swift (5.2)| [vapor](https://vapor.codes) (4.4) | 49 883 | 50 995 | 50 611 | 49 927 | 49 884 |
| 83 | javascript (13.13)| [moleculer](https://moleculer.services) (0.14) | 48 214 | 50 343 | 49 157 | 48 390 | 47 794 |
| 84 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 46 342 | 53 130 | 52 926 | 51 144 | 50 919 |
| 85 | php (7.4)| [imi](https://imiphp.com) (1.2) | 46 305 | 47 598 | 47 755 | 47 718 | 47 945 |
| 86 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 45 871 | 47 606 | 47 699 | 47 044 | 46 975 |
| 87 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 45 709 | 45 764 | 45 941 | 45 870 | 45 401 |
| 88 | python (3.8)| [hug](https://hug.rest) (2.6) | 45 334 | 47 377 | 47 373 | 47 003 | 46 967 |
| 89 | javascript (13.13)| [hapi](https://hapijs.com) (19.1) | 43 656 | 45 967 | 44 720 | 44 070 | 44 247 |
| 90 | rust (1.43)| [gotham](https://gotham.rs) (0.4) | 42 742 | 48 276 | 51 169 | 52 881 | 54 237 |
| 91 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 40 236 | 40 877 | 40 561 | 39 362 | 39 212 |
| 92 | python (3.8)| [starlette](https://starlette.io) (0.13) | 40 065 | 44 066 | 43 799 | 42 182 | 42 316 |
| 93 | javascript (13.13)| [restify](https://restify.com) (8.5) | 39 794 | 41 361 | 40 948 | 39 745 | 39 075 |
| 94 | javascript (13.13)| [nestjs-express](https://nestjs.com) (7.0) | 39 293 | 41 590 | 40 080 | 36 127 | 35 483 |
| 95 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 38 760 | 40 323 | 38 796 | 38 104 | 38 053 |
| 96 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 38 055 | 39 644 | 37 715 | 37 095 | 37 974 |
| 97 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 38 005 | 40 574 | 40 492 | 40 099 | 39 878 |
| 98 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0b2) | 37 629 | 40 286 | 40 210 | 38 465 | 38 574 |
| 99 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 37 080 | 36 510 | 36 960 | 36 759 | 35 501 |
| 100 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.31) | 36 675 | 37 294 | 36 016 | 35 403 | 35 652 |
| 101 | php (7.4)| [swoft](https://swoft.org) (2.0) | 35 735 | 36 807 | 36 555 | 36 213 | 36 105 |
| 102 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 34 422 | 35 118 | 34 790 | 33 466 | 33 569 |
| 103 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 33 287 | 33 329 | 32 052 | 32 102 | 32 109 |
| 104 | dart (2.7)| [aqueduct](https://aqueduct.io) (3.3) | 30 941 | 30 659 | 31 623 | 30 944 | 30 203 |
| 105 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 30 403 | 30 786 | 30 454 | 29 676 | 29 677 |
| 106 | crystal (0.34)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.15) | 29 491 | 27 621 | 25 618 | 22 281 | 21 878 |
| 107 | python (3.8)| [responder](https://python-responder.org) (2.0) | 29 293 | 30 663 | 30 345 | 29 270 | 29 254 |
| 108 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 27 394 | 27 466 | 25 969 | 26 169 | 27 406 |
| 109 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.54) | 26 959 | 30 234 | 29 967 | 28 929 | 28 940 |
| 110 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 26 352 | 26 168 | 24 782 | 25 570 | 25 769 |
| 111 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 26 095 | 26 902 | 26 731 | 26 321 | 26 283 |
| 112 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 24 556 | 24 004 | 23 545 | 23 387 | 23 474 |
| 113 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 24 368 | 25 833 | 25 667 | 25 508 | 25 331 |
| 114 | rust (1.43)| [iron](https://ironframework.io) (0.6) | 23 646 | 23 719 | 23 769 | 23 746 | 23 764 |
| 115 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 23 275 | 23 942 | 23 813 | 23 494 | 23 568 |
| 116 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 22 971 | 27 160 | 27 190 | 26 304 | 26 413 |
| 117 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 22 873 | 24 249 | 23 755 | 23 773 | 22 118 |
| 118 | javascript (13.13)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 296 | 20 925 | 19 938 | 19 770 | 19 675 |
| 119 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 18 109 | 18 581 | 18 040 | 17 836 | 17 836 |
| 120 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 845 | 18 118 | 18 281 | 18 148 | 18 271 |
| 121 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 17 210 | 17 620 | 17 661 | 17 665 | 17 709 |
| 122 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 15 444 | 15 324 | 15 449 | 15 375 | 15 425 |
| 123 | javascript (13.13)| [sails](https://sailsjs.com) (1.2) | 13 293 | 13 814 | 13 594 | 13 471 | 13 519 |
| 124 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 12 859 | 12 750 | 12 300 | 11 926 | 11 680 |
| 125 | pony (0.33)| [jennet](https://github.com/Theodus/jennet) (0.1) | 12 039 | 15 298 | 16 511 | 17 439 | 12 685 |
| 126 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 12 020 | 11 882 | 11 280 | 10 730 | 10 656 |
| 127 | swift (5.2)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 378 | 11 803 | 11 724 | 11 621 | 11 777 |
| 128 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 11 047 | 10 212 | 10 299 | 10 547 | 10 752 |
| 129 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 10 576 | 10 584 | 10 349 | 10 554 | 10 471 |
| 130 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 079 | 9 013 | 9 006 | 51 451 | 48 745 |
| 131 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 8 310 | 9 062 | 9 011 | 8 918 | 7 578 |
| 132 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 8 189 | 10 304 | 10 161 | 9 978 | 9 989 |
| 133 | python (3.8)| [django](https://djangoproject.com) (3.0) | 7 924 | 9 780 | 9 702 | 9 376 | 9 613 |
| 134 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 7 822 | 7 838 | 7 738 | 42 999 | 41 915 |
| 135 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 504 | 7 481 | 7 484 | 51 006 | 50 291 |
| 136 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 455 | 7 418 | 7 385 | 43 704 | 42 033 |
| 137 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 252 | 6 158 | 6 166 | 49 740 | 45 846 |
| 138 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 5 569 | 5 542 | 5 560 | 41 622 | 40 393 |
| 139 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 4 970 | 5 024 | 6 817 | 9 945 | 6 438 |
| 140 | crystal (0.34)| [onyx](https://onyxframework.org) (0.5) | 4 933 | 5 060 | 5 039 | 5 070 | 5 066 |
| 141 | php (7.4)| [slim](https://slimframework.com) (4.5) | 4 579 | 4 638 | 4 648 | 42 802 | 41 284 |
| 142 | php (7.4)| [lumen](https://lumen.laravel.com) (7.0) | 4 247 | 4 310 | 4 434 | 42 500 | 39 402 |
| 143 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 080 | 4 105 | 4 256 | 42 118 | 41 209 |
| 144 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 551 | 3 491 | 3 481 | 3 320 | 3 018 |
| 145 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 161 | 3 193 | 3 215 | 41 156 | 39 693 |
| 146 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 734 | 2 735 | 2 824 | 40 963 | 38 429 |
| 147 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 432 | 2 423 | 2 416 | 2 410 | 2 400 |
| 148 | julia (1.4)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 2 406 | 8 004 | 6 511 | 4 888 | 2 810 |
| 149 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 606 | 1 625 | 1 596 | 1 598 | 1 592 |
| 150 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 530 | 1 501 | 1 466 | 1 457 | 1 459 |
| 151 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 423 | 1 458 | 1 509 | 38 203 | 36 252 |
| 152 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 902 | 1 265 | 1 408 | 1 194 | 573 |
| 153 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 583 | 407 | 1 058 | 35 651 | 35 735 |
| 154 | php (7.4)| [laravel](https://laravel.com) (7.7) | 207 | 156 | 4 586 | 22 944 | 20 580 |

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

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

:information_source:  Updated on **2020-03-17** :information_source:

> Benchmarking with [wrk](https://github.com/ioquatix/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 188 953 | 203 050 | 204 807 | 203 479 | 204 084 |
| 2 | javascript (13.7)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.1) | 186 702 | 199 437 | 199 380 | 196 690 | 197 550 |
| 3 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 180 857 | 193 024 | 193 499 | 190 642 | 191 046 |
| 4 | javascript (13.7)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (1.1) | 176 964 | 188 831 | 188 804 | 185 923 | 186 556 |
| 5 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.9) | 161 931 | 170 759 | 173 141 | 168 967 | 168 417 |
| 6 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 158 398 | 174 140 | 174 910 | 169 609 | 170 070 |
| 7 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 157 785 | 165 086 | 168 038 | 163 595 | 163 612 |
| 8 | crystal (0.33)| [toro](https://github.com/soveran/toro) (0.4) | 157 323 | 165 065 | 162 807 | 153 722 | 152 434 |
| 9 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.4) | 157 049 | 165 155 | 167 228 | 162 623 | 162 483 |
| 10 | go (1.14)| [router](https://github.com/fasthttp/router) (0.6) | 156 913 | 165 226 | 166 943 | 162 633 | 163 181 |
| 11 | crystal (0.33)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 155 413 | 163 611 | 160 539 | 151 449 | 151 037 |
| 12 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 155 003 | 163 085 | 165 583 | 160 534 | 160 227 |
| 13 | go (1.14)| [fiber](https://fiber.wiki) (1.8) | 152 957 | 157 550 | 155 979 | 148 064 | 148 619 |
| 14 | crystal (0.33)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 152 193 | 159 996 | 156 063 | 147 957 | 147 621 |
| 15 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 148 633 | 159 025 | 159 675 | 158 375 | 158 108 |
| 16 | crystal (0.33)| [grip](https://github.com/grip-framework/grip) (0.28) | 145 885 | 152 572 | 148 847 | 139 944 | 139 348 |
| 17 | crystal (0.33)| [kemal](https://kemalcr.com) (0.26) | 145 590 | 152 691 | 149 733 | 141 049 | 140 363 |
| 18 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 138 291 | 151 822 | 153 659 | 149 585 | 152 063 |
| 19 | crystal (0.33)| [amber](https://amberframework.org) (0.33) | 136 485 | 142 299 | 137 907 | 130 308 | 128 558 |
| 20 | crystal (0.33)| [lucky](https://luckyframework.org) (0.18) | 134 551 | 139 732 | 133 681 | 124 606 | 124 849 |
| 21 | crystal (0.33)| [orion](https://github.com/obsidian/orion) (2.1) | 133 326 | 137 795 | 132 462 | 121 686 | 120 886 |
| 22 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 129 513 | 168 837 | 185 123 | 179 671 | 180 828 |
| 23 | crystal (0.33)| [athena](https://github.com/athena-framework/athena) (0.8) | 126 007 | 129 545 | 122 668 | 111 751 | 110 449 |
| 24 | java (8)| [act](https://actframework.org) (1.8) | 121 173 | 134 394 | 133 467 | 130 686 | 130 419 |
| 25 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 110 019 | 109 545 | 112 624 | 112 051 | 112 416 |
| 26 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 108 570 | 107 535 | 111 094 | 110 667 | 110 446 |
| 27 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.0) | 104 946 | 103 021 | 105 896 | 105 830 | 105 345 |
| 28 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 104 260 | 107 531 | 110 178 | 109 356 | 108 777 |
| 29 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 102 700 | 102 470 | 105 339 | 104 851 | 104 721 |
| 30 | go (1.14)| [violetear](https://violetear.org) (7.0) | 101 541 | 101 025 | 104 173 | 104 659 | 104 194 |
| 31 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 99 578 | 97 928 | 100 826 | 101 720 | 101 524 |
| 32 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 99 137 | 102 650 | 103 900 | 102 156 | 101 850 |
| 33 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 98 731 | 97 113 | 100 316 | 100 973 | 100 707 |
| 34 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 98 634 | 122 134 | 126 888 | 127 245 | 126 450 |
| 35 | c (99)| [kore](https://kore.io) (3.3) | 97 603 | 162 248 | 160 871 | 156 815 | 157 311 |
| 36 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 96 848 | 105 453 | 105 445 | 103 202 | 103 396 |
| 37 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 96 730 | 94 667 | 97 890 | 98 648 | 98 391 |
| 38 | go (1.14)| [beego](https://beego.me) (1.12) | 95 690 | 99 522 | 102 350 | 102 008 | 102 252 |
| 39 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 95 162 | 104 379 | 103 489 | 99 625 | 98 511 |
| 40 | go (1.14)| [gin](https://gin-gonic.com) (1.5) | 94 652 | 97 675 | 100 057 | 99 777 | 99 763 |
| 41 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 94 482 | 93 121 | 96 682 | 96 945 | 96 520 |
| 42 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 90 776 | 99 618 | 98 664 | 93 739 | 93 894 |
| 43 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 88 902 | 88 315 | 89 706 | 88 811 | 89 259 |
| 44 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.1) | 86 221 | 94 667 | 93 505 | 89 946 | 89 862 |
| 45 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 85 872 | 90 081 | 88 049 | 84 826 | 84 709 |
| 46 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 83 707 | 88 275 | 84 837 | 82 692 | 82 555 |
| 47 | go (1.14)| [air](https://github.com/aofei/air) (0.15) | 82 749 | 86 623 | 88 544 | 88 528 | 88 437 |
| 48 | java (8)| [javalin](https://javalin.io) (3.5) | 78 319 | 83 077 | 81 524 | 79 779 | 80 937 |
| 49 | go (1.14)| [gf](https://goframe.org) (1.11) | 76 599 | 81 721 | 83 221 | 83 634 | 83 067 |
| 50 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 73 234 | 78 336 | 75 566 | 74 194 | 74 273 |
| 51 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 72 934 | 77 619 | 77 850 | 74 915 | 76 374 |
| 52 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 71 493 | 78 973 | 84 175 | 83 942 | 83 937 |
| 53 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 71 358 | 78 104 | 77 505 | 75 433 | 75 055 |
| 54 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 69 881 | 74 370 | 72 280 | 70 135 | 69 881 |
| 55 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 67 773 | 71 117 | 69 653 | 67 608 | 67 519 |
| 56 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 66 589 | 79 659 | 81 959 | 80 909 | 80 501 |
| 57 | javascript (13.7)| [fastify](https://fastify.io) (2.12) | 66 504 | 69 876 | 68 660 | 65 387 | 64 528 |
| 58 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 64 060 | 67 302 | 69 261 | 68 497 | 68 283 |
| 59 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 62 696 | 64 740 | 63 283 | 62 069 | 61 678 |
| 60 | java (8)| [micronaut](https://micronaut.io) (1.2) | 62 463 | 71 734 | 71 745 | 69 835 | 69 298 |
| 61 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 62 016 | 62 104 | 64 284 | 59 817 | 59 265 |
| 62 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 61 123 | 64 698 | 67 648 | 67 549 | 67 320 |
| 63 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 60 358 | 62 821 | 60 890 | 59 508 | 59 136 |
| 64 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 58 728 | 62 424 | 63 006 | 61 702 | 59 573 |
| 65 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.14) | 57 094 | 63 116 | 62 775 | 60 285 | 60 365 |
| 66 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 56 411 | 64 946 | 64 621 | 63 338 | 63 183 |
| 67 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 55 193 | 57 350 | 56 464 | 55 425 | 54 764 |
| 68 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 54 553 | 55 808 | 55 910 | 55 078 | 55 549 |
| 69 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 53 994 | 58 519 | 58 168 | 56 100 | 56 032 |
| 70 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 52 543 | 52 970 | 52 754 | 52 819 | 52 378 |
| 71 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 52 384 | 58 096 | 59 355 | 59 477 | 59 331 |
| 72 | javascript (13.7)| [moleculer](https://moleculer.services) (0.14) | 50 188 | 52 249 | 51 046 | 50 519 | 50 180 |
| 73 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 49 170 | 53 762 | 53 141 | 51 467 | 51 554 |
| 74 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 49 068 | 49 021 | 50 107 | 48 408 | 46 711 |
| 75 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 48 717 | 50 299 | 49 871 | 49 426 | 49 220 |
| 76 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 48 341 | 50 292 | 50 445 | 50 001 | 49 882 |
| 77 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 47 067 | 49 771 | 49 521 | 49 049 | 48 755 |
| 78 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 46 138 | 45 761 | 46 094 | 45 681 | 45 107 |
| 79 | python (3.8)| [hug](https://hug.rest) (2.6) | 46 106 | 47 279 | 49 200 | 48 976 | 48 588 |
| 80 | python (3.8)| [starlette](https://starlette.io) (0.13) | 44 613 | 48 977 | 48 467 | 46 771 | 46 760 |
| 81 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 43 867 | 46 050 | 45 158 | 44 581 | 44 784 |
| 82 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 41 596 | 49 417 | 51 408 | 53 553 | 53 631 |
| 83 | javascript (13.7)| [restify](https://restify.com) (8.5) | 41 182 | 43 231 | 42 431 | 42 055 | 41 938 |
| 84 | php (7.4)| [imi](https://imiphp.com) (1.0) | 40 951 | 43 269 | 43 587 | 43 202 | 43 309 |
| 85 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 40 717 | 42 718 | 40 380 | 39 435 | 40 021 |
| 86 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 39 907 | 41 603 | 45 237 | 42 868 | 40 215 |
| 87 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 39 311 | 42 757 | 42 646 | 42 104 | 42 048 |
| 88 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0b1) | 38 783 | 41 729 | 41 329 | 39 957 | 40 102 |
| 89 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 38 679 | 42 613 | 39 282 | 38 101 | 39 890 |
| 90 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 38 362 | 41 727 | 39 712 | 39 291 | 39 661 |
| 91 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 37 637 | 38 293 | 37 919 | 37 045 | 37 032 |
| 92 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.3) | 37 208 | 38 909 | 37 886 | 37 465 | 36 930 |
| 93 | php (7.4)| [swoft](https://swoft.org) (2.0) | 35 994 | 36 705 | 36 428 | 35 831 | 35 940 |
| 94 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 34 942 | 34 497 | 36 596 | 35 602 | 36 265 |
| 95 | dart (2.7)| [aqueduct](https://aqueduct.io) (3.2) | 33 219 | 32 466 | 33 431 | 32 532 | 31 653 |
| 96 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 32 632 | 33 505 | 32 652 | 32 222 | 32 561 |
| 97 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 30 631 | 31 722 | 30 342 | 29 575 | 29 008 |
| 98 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.52) | 28 850 | 31 617 | 31 313 | 30 078 | 30 055 |
| 99 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 28 590 | 28 928 | 27 913 | 28 011 | 27 910 |
| 100 | python (3.8)| [responder](https://python-responder.org) (2.0) | 27 508 | 31 659 | 31 268 | 30 145 | 29 994 |
| 101 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 26 725 | 27 023 | 27 377 | 26 605 | 26 883 |
| 102 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 26 037 | 27 909 | 27 798 | 26 941 | 26 663 |
| 103 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 26 010 | 26 901 | 26 657 | 25 978 | 26 193 |
| 104 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 25 459 | 24 923 | 24 452 | 24 963 | 24 914 |
| 105 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 24 225 | 24 617 | 24 759 | 24 411 | 24 186 |
| 106 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 24 041 | 22 724 | 21 631 | 21 087 | 21 020 |
| 107 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 23 952 | 24 737 | 24 792 | 24 430 | 24 342 |
| 108 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 21 285 | 21 709 | 21 171 | 20 568 | 20 502 |
| 109 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 18 612 | 18 572 | 18 519 | 18 537 | 18 277 |
| 110 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 727 | 18 076 | 18 160 | 17 991 | 18 022 |
| 111 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 15 029 | 15 485 | 15 565 | 15 549 | 15 557 |
| 112 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 14 999 | 14 930 | 15 066 | 15 031 | 15 122 |
| 113 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 14 975 | 14 778 | 14 785 | 14 813 | 14 763 |
| 114 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 13 344 | 12 977 | 13 046 | 12 989 | 12 957 |
| 115 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 12 065 | 12 044 | 12 002 | 11 762 | 11 685 |
| 116 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 172 | 11 420 | 11 511 | 11 426 | 11 602 |
| 117 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 10 908 | 12 241 | 11 455 | 10 862 | 10 864 |
| 118 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 441 | 10 355 | 10 245 | 10 209 | 10 162 |
| 119 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 100 | 9 054 | 9 004 | 52 494 | 49 862 |
| 120 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 8 414 | 9 252 | 9 119 | 9 154 | 9 152 |
| 121 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 7 876 | 7 792 | 7 747 | 44 888 | 41 348 |
| 122 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 7 649 | 10 549 | 10 333 | 10 165 | 9 991 |
| 123 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 418 | 7 360 | 7 297 | 44 079 | 43 048 |
| 124 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 342 | 7 390 | 7 388 | 50 331 | 49 835 |
| 125 | crystal (0.33)| [onyx](https://onyxframework.org) (0.5) | 4 799 | 4 911 | 4 942 | 4 939 | 4 931 |
| 126 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 576 | 4 559 | 4 705 | 43 765 | 41 481 |
| 127 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 361 | 4 365 | 4 362 | 43 473 | 40 206 |
| 128 | php (7.4)| [lumen](https://lumen.laravel.com) (7.0) | 4 278 | 4 298 | 4 302 | 43 077 | 41 069 |
| 129 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 4 044 | 3 805 | 3 807 | 3 768 | 3 757 |
| 130 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 620 | 3 614 | 3 721 | 42 683 | 40 831 |
| 131 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 607 | 3 616 | 3 672 | 42 260 | 39 922 |
| 132 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 423 | 2 458 | 2 429 | 2 424 | 2 401 |
| 133 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 2 044 | 5 485 | 4 340 | 3 423 | 2 005 |
| 134 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 1 916 | 1 937 | 1 974 | 40 782 | 38 821 |
| 135 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 575 | 1 605 | 1 566 | 1 563 | 1 564 |
| 136 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 421 | 1 392 | 1 376 | 1 367 | 1 366 |
| 137 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 1 064 | 1 005 | 1 623 | 1 941 | 1 133 |
| 138 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 530 | 449 | 1 933 | 35 007 | 33 017 |
| 139 | php (7.4)| [laravel](https://laravel.com) (7.1) | 250 | 157 | 3 294 | 24 276 | 22 812 |

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

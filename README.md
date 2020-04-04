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

:information_source:  Updated on **2020-04-04** :information_source:

> Benchmarking with [wrk](https://github.com/ioquatix/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (2.2) | 184 492 | 197 326 | 197 559 | 192 468 | 194 865 |
| 2 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 183 331 | 197 969 | 203 693 | 202 260 | 201 620 |
| 3 | javascript (13.7)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.11) | 163 647 | 186 964 | 192 585 | 179 776 | 179 791 |
| 4 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.9) | 158 335 | 167 782 | 170 814 | 165 942 | 165 662 |
| 5 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 158 109 | 170 321 | 172 904 | 166 838 | 167 438 |
| 6 | crystal (0.33)| [toro](https://github.com/soveran/toro) (0.4) | 154 046 | 161 967 | 158 177 | 151 593 | 150 296 |
| 7 | crystal (0.33)| [ricr](https://ricr-web.github.io/ricr) (0.1) | 153 367 | 156 722 | 152 775 | 147 881 | 149 763 |
| 8 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.0) | 153 089 | 161 538 | 164 860 | 159 778 | 159 963 |
| 9 | crystal (0.33)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 152 962 | 160 615 | 157 539 | 149 806 | 149 832 |
| 10 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 152 828 | 160 873 | 164 220 | 159 576 | 158 719 |
| 11 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.0) | 152 670 | 161 343 | 164 514 | 160 423 | 159 580 |
| 12 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 151 907 | 161 167 | 163 882 | 159 048 | 159 522 |
| 13 | go (1.14)| [fiber](https://fiber.wiki) (1.8) | 150 418 | 162 082 | 159 535 | 150 817 | 150 821 |
| 14 | crystal (0.33)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 150 252 | 156 704 | 153 033 | 144 856 | 143 864 |
| 15 | javascript (13.7)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.0) | 146 072 | 167 979 | 174 438 | 171 044 | 160 063 |
| 16 | crystal (0.33)| [kemal](https://kemalcr.com) (0.26) | 144 237 | 150 733 | 147 462 | 140 372 | 138 898 |
| 17 | crystal (0.33)| [grip](https://github.com/grip-framework/grip) (0.28) | 143 527 | 149 606 | 145 929 | 137 980 | 137 063 |
| 18 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 143 294 | 154 040 | 155 075 | 153 520 | 153 240 |
| 19 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 142 597 | 153 620 | 154 182 | 150 185 | 150 336 |
| 20 | crystal (0.33)| [amber](https://amberframework.org) (0.33) | 136 696 | 142 610 | 139 270 | 130 858 | 131 146 |
| 21 | crystal (0.33)| [orion](https://github.com/obsidian/orion) (2.3) | 130 242 | 134 277 | 128 771 | 117 102 | 117 720 |
| 22 | crystal (0.33)| [lucky](https://luckyframework.org) (0.19) | 129 660 | 133 965 | 127 797 | 119 833 | 118 298 |
| 23 | rust (1.41)| [actix](https://actix.rs) (2.0) | 128 398 | 132 935 | 134 693 | 127 243 | 128 260 |
| 24 | crystal (0.33)| [athena](https://github.com/athena-framework/athena) (0.8) | 123 646 | 125 801 | 120 206 | 106 699 | 108 223 |
| 25 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 123 218 | 181 245 | 167 556 | 183 890 | 181 718 |
| 26 | java (8)| [act](https://actframework.org) (1.8) | 118 241 | 132 103 | 131 554 | 128 255 | 128 188 |
| 27 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 108 533 | 108 803 | 112 059 | 111 329 | 110 922 |
| 28 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 106 735 | 106 970 | 110 084 | 109 761 | 108 941 |
| 29 | c (99)| [kore](https://kore.io) (3.3) | 106 501 | 142 625 | 162 211 | 156 621 | 151 584 |
| 30 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 102 484 | 102 032 | 104 983 | 104 412 | 103 686 |
| 31 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 101 560 | 105 429 | 107 897 | 106 976 | 107 014 |
| 32 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 100 827 | 101 419 | 104 220 | 103 700 | 103 386 |
| 33 | go (1.14)| [violetear](https://violetear.org) (7.0) | 99 871 | 99 973 | 103 145 | 102 743 | 102 530 |
| 34 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 98 163 | 117 248 | 122 399 | 122 491 | 121 907 |
| 35 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 97 885 | 96 980 | 100 154 | 100 078 | 99 453 |
| 36 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 97 778 | 97 503 | 100 229 | 100 055 | 99 782 |
| 37 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 97 315 | 101 315 | 102 660 | 100 874 | 100 887 |
| 38 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 95 261 | 94 063 | 96 921 | 97 187 | 96 942 |
| 39 | go (1.14)| [beego](https://beego.me) (1.12) | 94 748 | 98 711 | 101 624 | 100 739 | 100 304 |
| 40 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 93 228 | 97 538 | 99 245 | 99 004 | 98 858 |
| 41 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 92 911 | 92 284 | 95 148 | 95 927 | 95 422 |
| 42 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 92 896 | 100 609 | 94 472 | 91 406 | 88 095 |
| 43 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 92 781 | 99 192 | 99 799 | 98 009 | 98 127 |
| 44 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 87 780 | 91 029 | 92 713 | 92 846 | 92 741 |
| 45 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.2) | 85 716 | 93 799 | 93 489 | 90 323 | 89 249 |
| 46 | go (1.14)| [air](https://github.com/aofei/air) (0.15) | 81 873 | 85 693 | 87 800 | 87 676 | 87 078 |
| 47 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 79 643 | 99 020 | 93 785 | 85 806 | 90 834 |
| 48 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 78 506 | 82 804 | 80 327 | 75 370 | 72 853 |
| 49 | php (7.4)| [simps](https://simps.io) (preview) | 76 317 | 84 661 | 84 975 | 84 057 | 84 364 |
| 50 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 75 989 | 82 840 | 87 821 | 89 342 | 88 739 |
| 51 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 75 787 | 83 310 | 80 459 | 77 412 | 77 171 |
| 52 | go (1.14)| [gf](https://goframe.org) (1.12) | 75 390 | 80 799 | 82 135 | 81 832 | 81 690 |
| 53 | java (8)| [javalin](https://javalin.io) (3.5) | 70 885 | 76 356 | 79 091 | 77 206 | 76 126 |
| 54 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 69 726 | 75 361 | 75 695 | 74 455 | 74 221 |
| 55 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 69 518 | 74 480 | 73 280 | 70 885 | 71 265 |
| 56 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 69 432 | 73 884 | 71 166 | 69 775 | 70 623 |
| 57 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 66 739 | 71 168 | 71 118 | 70 126 | 70 277 |
| 58 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 66 197 | 72 239 | 70 726 | 68 051 | 67 766 |
| 59 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 66 103 | 77 611 | 79 631 | 78 272 | 78 388 |
| 60 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 65 115 | 68 327 | 66 941 | 65 746 | 65 116 |
| 61 | java (8)| [micronaut](https://micronaut.io) (1.2) | 64 038 | 71 464 | 71 527 | 69 010 | 68 946 |
| 62 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 63 682 | 67 108 | 67 632 | 66 673 | 66 541 |
| 63 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 63 556 | 64 092 | 63 329 | 62 069 | 61 606 |
| 64 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 61 101 | 64 023 | 66 783 | 66 419 | 66 264 |
| 65 | javascript (13.7)| [fastify](https://fastify.io) (2.13) | 59 295 | 67 498 | 65 164 | 60 968 | 56 450 |
| 66 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 58 047 | 60 953 | 61 319 | 60 339 | 60 315 |
| 67 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.14) | 57 953 | 62 201 | 63 005 | 60 550 | 60 632 |
| 68 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 56 302 | 60 077 | 57 501 | 54 935 | 55 464 |
| 69 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 55 676 | 57 292 | 57 232 | 56 920 | 56 707 |
| 70 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 55 003 | 57 299 | 56 055 | 54 954 | 54 525 |
| 71 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 54 727 | 62 968 | 62 673 | 62 253 | 61 395 |
| 72 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 54 600 | 55 334 | 54 461 | 55 003 | 54 350 |
| 73 | javascript (13.7)| [moleculer](https://moleculer.services) (0.14) | 53 540 | 55 526 | 54 363 | 53 606 | 53 408 |
| 74 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 53 076 | 60 084 | 61 503 | 62 173 | 62 254 |
| 75 | javascript (13.7)| [feathersjs](https://feathersjs.com) (4.5) | 52 050 | 54 535 | 52 237 | 51 137 | 51 232 |
| 76 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 51 139 | 56 199 | 56 117 | 54 017 | 53 917 |
| 77 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 50 955 | 51 324 | 51 086 | 50 154 | 49 883 |
| 78 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 50 509 | 50 588 | 50 301 | 49 298 | 49 406 |
| 79 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 50 160 | 51 113 | 50 960 | 50 503 | 50 330 |
| 80 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 48 773 | 51 341 | 51 126 | 50 534 | 48 914 |
| 81 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 48 424 | 49 873 | 50 066 | 49 169 | 49 155 |
| 82 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 47 790 | 53 700 | 53 446 | 51 433 | 51 407 |
| 83 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 46 646 | 46 580 | 46 358 | 46 447 | 45 732 |
| 84 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 46 633 | 48 769 | 47 838 | 48 202 | 47 149 |
| 85 | python (3.8)| [hug](https://hug.rest) (2.6) | 46 238 | 47 776 | 47 915 | 46 997 | 47 330 |
| 86 | javascript (13.7)| [restify](https://restify.com) (8.5) | 43 278 | 46 084 | 44 964 | 44 386 | 44 574 |
| 87 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 42 840 | 48 570 | 51 794 | 53 395 | 54 096 |
| 88 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 40 801 | 41 401 | 41 177 | 39 792 | 39 632 |
| 89 | php (7.4)| [imi](https://imiphp.com) (1.0) | 39 770 | 41 114 | 41 447 | 41 069 | 41 123 |
| 90 | python (3.8)| [starlette](https://starlette.io) (0.13) | 38 976 | 43 507 | 43 069 | 41 444 | 41 541 |
| 91 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 38 818 | 39 674 | 38 166 | 37 915 | 38 009 |
| 92 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 38 803 | 39 115 | 37 205 | 37 045 | 37 298 |
| 93 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0b2) | 38 332 | 42 941 | 43 072 | 41 456 | 41 433 |
| 94 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 38 274 | 41 584 | 41 610 | 41 155 | 41 190 |
| 95 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.3) | 36 667 | 37 383 | 35 980 | 35 438 | 35 781 |
| 96 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 36 158 | 36 735 | 36 362 | 35 112 | 35 115 |
| 97 | php (7.4)| [swoft](https://swoft.org) (2.0) | 35 108 | 36 406 | 36 244 | 35 805 | 35 803 |
| 98 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 33 622 | 33 479 | 35 071 | 35 156 | 35 381 |
| 99 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 32 556 | 33 619 | 32 462 | 32 366 | 32 525 |
| 100 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 30 973 | 31 158 | 30 872 | 29 737 | 29 809 |
| 101 | dart (2.7)| [aqueduct](https://aqueduct.io) (3.2) | 30 696 | 30 223 | 31 393 | 30 823 | 30 745 |
| 102 | python (3.8)| [responder](https://python-responder.org) (2.0) | 28 488 | 30 686 | 30 576 | 29 496 | 29 517 |
| 103 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.53) | 27 901 | 30 876 | 30 670 | 29 528 | 29 522 |
| 104 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 27 537 | 27 665 | 27 075 | 27 036 | 27 412 |
| 105 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 26 957 | 26 345 | 25 887 | 25 877 | 25 968 |
| 106 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 26 353 | 26 910 | 26 878 | 26 687 | 26 553 |
| 107 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 25 353 | 26 029 | 25 780 | 25 460 | 25 390 |
| 108 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 25 246 | 27 189 | 27 212 | 26 349 | 26 302 |
| 109 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 24 648 | 24 353 | 23 866 | 23 927 | 24 043 |
| 110 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 24 020 | 24 592 | 24 388 | 23 775 | 24 034 |
| 111 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 22 863 | 24 360 | 24 101 | 23 754 | 21 901 |
| 112 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 21 971 | 21 729 | 20 437 | 19 940 | 20 742 |
| 113 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 18 846 | 18 734 | 18 292 | 17 967 | 17 972 |
| 114 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 18 629 | 18 885 | 18 790 | 18 703 | 18 681 |
| 115 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 18 398 | 18 759 | 18 775 | 18 455 | 18 612 |
| 116 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 15 223 | 15 236 | 15 213 | 15 245 | 15 250 |
| 117 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 14 204 | 14 178 | 14 071 | 14 085 | 14 059 |
| 118 | javascript (13.7)| [sails](https://sailsjs.com) (1.2) | 13 312 | 13 954 | 13 730 | 13 562 | 13 580 |
| 119 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 773 | 11 807 | 11 780 | 11 752 | 11 854 |
| 120 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 11 445 | 11 252 | 11 266 | 11 148 | 11 076 |
| 121 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 11 165 | 12 091 | 11 240 | 10 681 | 10 710 |
| 122 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 10 701 | 10 654 | 10 590 | 10 594 | 10 544 |
| 123 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 468 | 9 882 | 9 846 | 9 386 | 9 752 |
| 124 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 130 | 9 140 | 8 922 | 49 347 | 47 513 |
| 125 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 7 886 | 7 770 | 7 684 | 44 383 | 40 806 |
| 126 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 7 674 | 9 533 | 9 501 | 9 264 | 9 052 |
| 127 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 531 | 7 496 | 7 327 | 43 916 | 42 817 |
| 128 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 500 | 7 548 | 7 494 | 50 821 | 48 333 |
| 129 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 6 874 | 9 061 | 9 024 | 8 948 | 7 927 |
| 130 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 5 609 | 5 583 | 5 611 | 42 549 | 40 250 |
| 131 | crystal (0.33)| [onyx](https://onyxframework.org) (0.5) | 5 058 | 5 192 | 5 184 | 5 223 | 5 178 |
| 132 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 592 | 4 628 | 4 578 | 43 198 | 38 535 |
| 133 | php (7.4)| [lumen](https://lumen.laravel.com) (7.0) | 4 268 | 4 255 | 4 344 | 42 687 | 42 159 |
| 134 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 118 | 4 074 | 4 219 | 41 493 | 38 226 |
| 135 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 826 | 3 640 | 3 617 | 3 574 | 3 567 |
| 136 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 124 | 3 142 | 3 193 | 40 776 | 39 362 |
| 137 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 844 | 2 904 | 2 890 | 40 761 | 38 324 |
| 138 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 433 | 2 447 | 2 432 | 2 418 | 2 411 |
| 139 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 625 | 1 655 | 1 628 | 1 626 | 1 614 |
| 140 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 560 | 1 512 | 1 507 | 1 488 | 1 472 |
| 141 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 1 524 | 1 133 | 1 144 | 1 244 | 1 122 |
| 142 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 1 464 | 4 255 | 4 194 | 3 342 | 2 025 |
| 143 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 423 | 1 454 | 1 496 | 38 340 | 36 375 |
| 144 | php (7.4)| [laravel](https://laravel.com) (7.4) | 502 | 180 | 2 856 | 23 133 | 23 531 |
| 145 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 499 | 439 | 1 159 | 33 621 | 34 246 |
| 146 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 0 | 0 | 3 558 | 4 893 | 3 443 |

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

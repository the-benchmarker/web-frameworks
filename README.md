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

:information_source:  Updated on **2020-06-25** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 201 699 | 215 546 | 216 709 |
| 2 | javascript (13.14)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.12) | 198 517 | 214 009 | 215 351 |
| 3 | nim (1.2)| [whip](https://github.com/mattaylor/whip) (0.2) | 189 999 | 201 240 | 201 469 |
| 4 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 179 620 | 190 595 | 193 248 |
| 5 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.0) | 177 849 | 191 769 | 192 988 |
| 6 | javascript (13.14)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 172 031 | 186 159 | 186 380 |
| 7 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 170 590 | 178 679 | 176 189 |
| 8 | kotlin (1.3)| [kooby](https://jooby.io) (2.8) | 168 329 | 190 002 | 191 350 |
| 9 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 167 317 | 175 821 | 173 269 |
| 10 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 166 619 | 183 139 | 183 889 |
| 11 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.2) | 166 163 | 176 991 | 182 453 |
| 12 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.1) | 165 620 | 173 904 | 171 023 |
| 13 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.14) | 164 995 | 177 139 | 179 682 |
| 14 | java (8)| [jooby](https://jooby.io) (2.8) | 164 711 | 183 264 | 189 002 |
| 15 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 162 226 | 174 683 | 175 654 |
| 16 | go (1.14)| [gearbox](https://gogearbox.com) (1.0) | 161 591 | 172 442 | 177 563 |
| 17 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 160 358 | 170 746 | 174 989 |
| 18 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.3) | 159 505 | 170 024 | 174 310 |
| 19 | crystal (0.35)| [kemal](https://kemalcr.com) (0.26) | 157 035 | 164 386 | 161 652 |
| 20 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (0.28) | 156 920 | 163 410 | 160 475 |
| 21 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 152 454 | 160 510 | 162 404 |
| 22 | go (1.14)| [fiber](https://gofiber.io) (1.12) | 152 281 | 170 606 | 169 715 |
| 23 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 149 431 | 155 443 | 151 860 |
| 24 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 145 898 | 162 199 | 170 895 |
| 25 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (2.3) | 142 073 | 146 044 | 139 230 |
| 26 | rust (1.44)| [actix](https://actix.rs) (2.0) | 142 053 | 141 587 | 143 715 |
| 27 | java (8)| [act](https://actframework.org) (1.8) | 126 849 | 139 882 | 140 364 |
| 28 | c (99)| [kore](https://kore.io) (3.3) | 117 488 | 121 084 | 110 005 |
| 29 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 111 457 | 111 769 | 116 227 |
| 30 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 111 429 | 115 241 | 120 316 |
| 31 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 111 328 | 111 274 | 115 950 |
| 32 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 110 831 | 110 017 | 114 752 |
| 33 | go (1.14)| [clevergo](https://clevergo.tech) (0.3) | 109 932 | 108 121 | 109 600 |
| 34 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 109 111 | 109 814 | 114 335 |
| 35 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 108 823 | 118 064 | 119 830 |
| 36 | fsharp (4.7)| [frank](https://github.com/frank-fs/frank) (6.1) | 106 390 | 113 668 | 115 209 |
| 37 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 106 379 | 104 915 | 108 972 |
| 38 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 105 994 | 108 514 | 113 709 |
| 39 | go (1.14)| [violetear](https://violetear.org) (7.0) | 105 893 | 101 997 | 111 175 |
| 40 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 104 783 | 101 728 | 106 702 |
| 41 | go (1.14)| [beego](https://beego.me) (1.12) | 103 778 | 106 844 | 111 172 |
| 42 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 103 040 | 102 430 | 106 301 |
| 43 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.9) | 102 850 | 115 497 | 113 348 |
| 44 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 101 650 | 108 094 | 108 083 |
| 45 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 97 678 | 103 580 | 104 766 |
| 46 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 96 969 | 103 599 | 102 361 |
| 47 | javascript (13.14)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 94 677 | 104 411 | 104 610 |
| 48 | php (7.4)| [hyperf](https://www.hyperf.io) (1.1) | 94 648 | 100 494 | 102 270 |
| 49 | fsharp (4.7)| [falco](https://github.com/pimbrouwers/Falco) (1.1) | 92 906 | 100 928 | 101 994 |
| 50 | fsharp (4.7)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 91 151 | 94 812 | 93 447 |
| 51 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 90 456 | 115 579 | 117 744 |
| 52 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 87 786 | 92 010 | 94 035 |
| 53 | go (1.14)| [air](https://github.com/aofei/air) (0.17) | 87 563 | 87 636 | 90 510 |
| 54 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 87 154 | 177 792 | 175 674 |
| 55 | javascript (13.14)| [0http](https://github.com/jkyberneees/0http) (2.5) | 84 620 | 93 554 | 94 387 |
| 56 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.1) | 84 013 | 81 461 | 71 965 |
| 57 | javascript (13.14)| [polka](https://github.com/lukeed/polka) (0.5) | 83 982 | 89 595 | 87 610 |
| 58 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 83 174 | 83 606 | 81 299 |
| 59 | javascript (13.14)| [restana](https://github.com/jkyberneees/ana) (4.6) | 82 682 | 93 083 | 93 346 |
| 60 | go (1.14)| [gf](https://goframe.org) (1.13) | 79 192 | 84 695 | 86 512 |
| 61 | swift (5.2)| [perfect](https://perfect.org) (3.1) | 78 532 | 86 659 | 92 469 |
| 62 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 78 044 | 90 508 | 91 961 |
| 63 | javascript (13.14)| [rayo](https://rayo.js.org) (1.3) | 77 665 | 83 113 | 81 266 |
| 64 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 77 356 | 83 787 | 82 800 |
| 65 | java (8)| [javalin](https://javalin.io) (3.9) | 75 708 | 80 243 | 81 156 |
| 66 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 74 985 | 78 989 | 79 496 |
| 67 | fsharp (4.7)| [saturn](https://saturnframework.org) (0.14) | 74 966 | 68 022 | 60 234 |
| 68 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 71 983 | 75 672 | 79 155 |
| 69 | javascript (13.14)| [fastify](https://fastify.io) (2.15) | 69 446 | 73 618 | 71 781 |
| 70 | javascript (13.14)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 69 413 | 74 381 | 72 707 |
| 71 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 69 024 | 68 542 | 72 036 |
| 72 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 68 925 | 75 389 | 74 522 |
| 73 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 64 931 | 67 767 | 74 166 |
| 74 | javascript (13.14)| [foxify](https://foxify.js.org) (0.1) | 64 164 | 67 481 | 65 203 |
| 75 | java (8)| [micronaut](https://micronaut.io) (1.2) | 63 892 | 72 111 | 71 999 |
| 76 | fsharp (4.7)| [websharper](https://websharper.com) (4.6) | 63 397 | 65 354 | 60 060 |
| 77 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 59 732 | 63 277 | 63 356 |
| 78 | javascript (13.14)| [nestjs-fastify](https://nestjs.com) (7.2) | 59 580 | 64 189 | 63 692 |
| 79 | javascript (13.14)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 59 528 | 60 775 | 59 753 |
| 80 | javascript (13.14)| [koa](https://koajs.com) (2.13) | 59 493 | 62 641 | 60 951 |
| 81 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 57 206 | 57 528 | 57 373 |
| 82 | rust (1.44)| [nickel](https://nickel-org.github.io) (0.11) | 56 986 | 56 748 | 56 842 |
| 83 | swift (5.2)| [kitura](https://kitura.io) (2.9) | 55 936 | 56 107 | 55 607 |
| 84 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 55 643 | 57 217 | 56 997 |
| 85 | javascript (13.14)| [express](https://expressjs.com) (4.17) | 55 396 | 57 730 | 56 225 |
| 86 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 55 310 | 63 587 | 63 607 |
| 87 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 52 874 | 58 525 | 59 095 |
| 88 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.26) | 52 647 | 62 700 | 62 749 |
| 89 | javascript (13.14)| [feathersjs](https://feathersjs.com) (4.5) | 51 959 | 54 645 | 52 347 |
| 90 | rust (1.44)| [gotham](https://gotham.rs) (0.4) | 51 669 | 59 164 | 61 535 |
| 91 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 51 488 | 51 948 | 52 317 |
| 92 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 51 078 | 53 360 | 51 344 |
| 93 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 50 963 | 51 997 | 52 670 |
| 94 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 50 463 | 55 616 | 55 330 |
| 95 | swift (5.2)| [vapor](https://vapor.codes) (4.12) | 49 070 | 50 306 | 50 264 |
| 96 | javascript (13.14)| [moleculer](https://moleculer.services) (0.14) | 48 523 | 50 741 | 48 891 |
| 97 | php (7.4)| [imi](https://imiphp.com) (1.2) | 47 815 | 50 741 | 50 891 |
| 98 | python (3.8)| [hug](https://hug.rest) (2.6) | 47 525 | 49 130 | 49 294 |
| 99 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 45 537 | 45 879 | 45 569 |
| 100 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.3) | 45 204 | 50 955 | 50 179 |
| 101 | javascript (13.14)| [hapi](https://hapijs.com) (19.1) | 44 584 | 46 321 | 45 095 |
| 102 | python (3.8)| [starlette](https://starlette.io) (0.13) | 44 334 | 49 210 | 49 434 |
| 103 | php (7.4)| [swoft](https://swoft.org) (2.0) | 43 412 | 45 849 | 45 948 |
| 104 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 43 200 | 43 884 | 43 948 |
| 105 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 41 722 | 44 294 | 44 165 |
| 106 | javascript (13.14)| [restify](https://restify.com) (8.5) | 41 507 | 43 427 | 42 974 |
| 107 | javascript (13.14)| [nestjs-express](https://nestjs.com) (7.2) | 39 938 | 41 038 | 39 638 |
| 108 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 38 939 | 41 242 | 40 957 |
| 109 | scala (2.13)| [play](https://playframework.com) (2.8) | 38 322 | 40 063 | 39 754 |
| 110 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.1) | 38 271 | 38 384 | 37 892 |
| 111 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 38 065 | 38 617 | 36 642 |
| 112 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 37 194 | 36 461 | 37 734 |
| 113 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 36 738 | 37 325 | 35 554 |
| 114 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 36 459 | 37 230 | 37 273 |
| 115 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 36 452 | 41 015 | 40 904 |
| 116 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.33) | 35 693 | 36 077 | 34 527 |
| 117 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 35 362 | 36 344 | 36 151 |
| 118 | dart (2.8)| [aqueduct](https://aqueduct.io) (3.3) | 32 315 | 32 254 | 31 888 |
| 119 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 31 802 | 32 364 | 31 050 |
| 120 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.15) | 31 409 | 30 352 | 27 745 |
| 121 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 30 174 | 29 721 | 28 669 |
| 122 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.58) | 29 243 | 32 132 | 31 848 |
| 123 | python (3.8)| [responder](https://python-responder.org) (2.0) | 28 837 | 31 849 | 31 558 |
| 124 | nim (1.2)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 28 695 | 25 123 | 22 723 |
| 125 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 27 096 | 27 135 | 26 365 |
| 126 | nim (1.2)| [akane](https://github.com/Ethosa/akane) (0.1) | 26 864 | 23 327 | 20 946 |
| 127 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 26 576 | 27 358 | 27 175 |
| 128 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 26 257 | 26 869 | 25 939 |
| 129 | go (1.14)| [macaron](https://go-macaron.com) (1.3) | 26 032 | 28 472 | 28 575 |
| 130 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 25 816 | 26 482 | 26 275 |
| 131 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 24 725 | 28 664 | 28 410 |
| 132 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 24 144 | 24 476 | 24 687 |
| 133 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 23 794 | 23 901 | 23 401 |
| 134 | rust (1.44)| [iron](https://ironframework.io) (0.6) | 23 508 | 23 486 | 23 601 |
| 135 | javascript (13.14)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 806 | 21 903 | 20 594 |
| 136 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 21 646 | 22 152 | 21 900 |
| 137 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 18 253 | 18 605 | 18 715 |
| 138 | java (8)| [blade](https://lets-blade.com) (2.0) | 17 797 | 20 960 | 19 625 |
| 139 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 17 682 | 17 944 | 18 020 |
| 140 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 15 358 | 15 335 | 15 252 |
| 141 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 15 065 | 15 433 | 15 394 |
| 142 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 14 444 | 15 625 | 15 964 |
| 143 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 14 247 | 14 319 | 14 233 |
| 144 | dart (2.8)| [start](https://github.com/lvivski/start) (0.4) | 13 965 | 13 682 | 12 828 |
| 145 | javascript (13.14)| [sails](https://sailsjs.com) (1.2) | 13 483 | 13 930 | 13 740 |
| 146 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (0.2) | 12 946 | 12 867 | 11 976 |
| 147 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.2) | 11 800 | 11 685 | 11 452 |
| 148 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 11 493 | 11 285 | 11 298 |
| 149 | pony (0.35)| [jennet](https://github.com/Theodus/jennet) (0.1) | 11 183 | 18 221 | 16 140 |
| 150 | swift (5.2)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 107 | 11 113 | 11 105 |
| 151 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.12) | 11 083 | 11 101 | 10 594 |
| 152 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 10 620 | 10 593 | 10 546 |
| 153 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 10 378 | 10 440 | 10 262 |
| 154 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 601 | 9 637 | 9 506 |
| 155 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 422 | 9 314 | 9 219 |
| 156 | java (8)| [struts2](https://struts.apache.org) (2.5) | 8 830 | 8 921 | 8 788 |
| 157 | python (3.8)| [django](https://djangoproject.com) (3.0) | 8 761 | 8 667 | 8 616 |
| 158 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 8 216 | 8 197 | 8 295 |
| 159 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 8 156 | 8 027 | 7 516 |
| 160 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 7 928 | 7 925 | 7 958 |
| 161 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 808 | 7 871 | 7 750 |
| 162 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 725 | 7 759 | 7 814 |
| 163 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 537 | 6 576 | 6 684 |
| 164 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 5 851 | 5 889 | 5 988 |
| 165 | php (7.4)| [slim](https://slimframework.com) (4.5) | 4 810 | 4 840 | 4 943 |
| 166 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 4 496 | 4 595 | 4 696 |
| 167 | php (7.4)| [lumen](https://lumen.laravel.com) (7.2) | 4 420 | 4 454 | 4 576 |
| 168 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 285 | 4 370 | 4 431 |
| 169 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 809 | 3 687 | 3 673 |
| 170 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 305 | 7 931 | 6 115 |
| 171 | php (7.4)| [symfony](https://symfony.com) (5.1) | 3 258 | 3 297 | 3 357 |
| 172 | perl (5.32)| [dancer2](https://perldancer.org) (2.0) | 3 144 | 3 078 | 1 567 |
| 173 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 2 978 | 2 308 | 1 334 |
| 174 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 896 | 2 953 | 3 024 |
| 175 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 214 | 2 205 | 2 176 |
| 176 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 199 | 2 223 | 2 168 |
| 177 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 629 | 1 642 | 1 615 |
| 178 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 563 | 1 523 | 1 512 |
| 179 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 467 | 1 494 | 1 511 |
| 180 | crystal (0.35)| [lucky](https://luckyframework.org) (0.22) | 1 201 | 1 224 | 1 219 |
| 181 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 560 | 499 | 1 911 |
| 182 | php (7.4)| [laravel](https://laravel.com) (7.17) | 502 | 172 | 2 194 |

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

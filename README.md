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

:information_source:  Updated on **2020-08-05** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 168 186 | 173 039 | 173 712 |
| 2 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 160 936 | 192 640 | 194 238 |
| 3 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.15) | 154 464 | 167 148 | 171 335 |
| 4 | nim (1.2)| [whip](https://github.com/mattaylor/whip) (0.2) | 152 353 | 179 381 | 180 907 |
| 5 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.2) | 150 595 | 162 655 | 165 872 |
| 6 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.5) | 150 209 | 161 450 | 165 789 |
| 7 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 150 086 | 161 366 | 164 373 |
| 8 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 148 374 | 159 332 | 163 777 |
| 9 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 146 237 | 174 212 | 174 966 |
| 10 | kotlin (1.3)| [kooby](https://jooby.io) (2.8) | 142 784 | 181 076 | 185 255 |
| 11 | java (11)| [jooby](https://jooby.io) (2.8) | 138 738 | 176 001 | 179 916 |
| 12 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 138 077 | 161 862 | 161 542 |
| 13 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 135 983 | 165 468 | 164 040 |
| 14 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 135 097 | 164 487 | 167 017 |
| 15 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.3) | 134 025 | 159 031 | 157 263 |
| 16 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 133 843 | 173 902 | 176 761 |
| 17 | javascript (14.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 132 157 | 172 274 | 179 984 |
| 18 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 131 125 | 152 492 | 154 396 |
| 19 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (0.28) | 129 391 | 152 126 | 151 346 |
| 20 | crystal (0.35)| [kemal](https://kemalcr.com) (0.26) | 127 515 | 151 017 | 149 601 |
| 21 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 121 910 | 143 589 | 142 266 |
| 22 | rust (1.45)| [actix](https://actix.rs) (2.0) | 120 503 | 132 959 | 132 398 |
| 23 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 104 281 | 159 816 | 175 020 |
| 24 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 103 662 | 105 056 | 109 358 |
| 25 | go (1.14)| [clevergo](https://clevergo.tech) (0.3) | 102 748 | 103 943 | 108 031 |
| 26 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 102 301 | 103 790 | 107 447 |
| 27 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 101 541 | 103 149 | 107 437 |
| 28 | java (11)| [act](https://actframework.org) (1.9) | 100 831 | 130 009 | 130 321 |
| 29 | go (1.14)| [fiber](https://gofiber.io) (1.12) | 100 806 | 144 278 | 154 913 |
| 30 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 100 002 | 105 545 | 109 177 |
| 31 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 98 047 | 98 011 | 102 136 |
| 32 | go (1.14)| [gearbox](https://gogearbox.com) (1.1) | 97 501 | 142 182 | 153 037 |
| 33 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 97 250 | 102 729 | 105 821 |
| 34 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 96 726 | 98 271 | 102 178 |
| 35 | go (1.14)| [violetear](https://violetear.org) (7.0) | 95 675 | 96 603 | 100 499 |
| 36 | fsharp (4.7)| [frank](https://github.com/frank-fs/frank) (6.1) | 95 449 | 108 328 | 111 625 |
| 37 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 93 614 | 94 567 | 98 268 |
| 38 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 93 294 | 92 599 | 96 007 |
| 39 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 92 547 | 98 675 | 100 706 |
| 40 | fsharp (4.7)| [falco](https://github.com/pimbrouwers/Falco) (1.2) | 92 400 | 104 700 | 107 037 |
| 41 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 91 522 | 89 424 | 93 868 |
| 42 | c (99)| [kore](https://kore.io) (3.3) | 90 312 | 133 514 | 144 421 |
| 43 | go (1.14)| [beego](https://beego.me) (1.12) | 89 224 | 94 923 | 97 914 |
| 44 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 88 962 | 102 590 | 104 967 |
| 45 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.9) | 86 477 | 104 161 | 103 444 |
| 46 | javascript (14.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 84 059 | 97 498 | 97 394 |
| 47 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 80 432 | 87 562 | 90 649 |
| 48 | go (1.14)| [air](https://github.com/aofei/air) (0.19) | 78 829 | 80 413 | 83 381 |
| 49 | javascript (14.7)| [rayo](https://rayo.js.org) (1.3) | 77 193 | 85 225 | 83 385 |
| 50 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 75 077 | 76 354 | 74 565 |
| 51 | javascript (14.7)| [polka](https://github.com/lukeed/polka) (0.5) | 74 548 | 81 260 | 81 496 |
| 52 | javascript (14.7)| [0http](https://github.com/jkyberneees/0http) (2.5) | 74 193 | 91 353 | 91 899 |
| 53 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.1) | 74 167 | 74 467 | 67 185 |
| 54 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 73 098 | 89 954 | 96 554 |
| 55 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.13) | 72 979 | 108 718 | 119 941 |
| 56 | swift (5.2)| [perfect](https://perfect.org) (3.1) | 71 867 | 81 626 | 87 237 |
| 57 | java (11)| [javalin](https://javalin.io) (3.9) | 71 811 | 78 212 | 77 990 |
| 58 | go (1.14)| [gf](https://goframe.org) (1.13) | 71 172 | 77 934 | 79 675 |
| 59 | javascript (14.7)| [fastify](https://fastify.io) (3.1) | 69 376 | 79 528 | 79 056 |
| 60 | php (7.4)| [hyperf](https://hyperf.io) (2.0) | 69 062 | 86 362 | 92 903 |
| 61 | javascript (14.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 68 213 | 75 808 | 74 364 |
| 62 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 67 863 | 77 697 | 74 414 |
| 63 | javascript (14.7)| [restana](https://github.com/jkyberneees/ana) (4.7) | 67 680 | 82 697 | 81 003 |
| 64 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 66 953 | 74 061 | 74 329 |
| 65 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 64 969 | 84 950 | 89 333 |
| 66 | fsharp (4.7)| [saturn](https://saturnframework.org) (0.14) | 64 589 | 61 169 | 54 383 |
| 67 | java (11)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 62 846 | 69 937 | 69 295 |
| 68 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.8) | 62 494 | 68 204 | 67 724 |
| 69 | javascript (14.7)| [foxify](https://foxify.js.org) (0.1) | 59 954 | 66 036 | 64 015 |
| 70 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 58 475 | 59 878 | 63 063 |
| 71 | javascript (14.7)| [koa](https://koajs.com) (2.13) | 57 661 | 61 735 | 59 661 |
| 72 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 57 123 | 174 670 | 185 633 |
| 73 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 55 908 | 62 309 | 66 430 |
| 74 | java (11)| [micronaut](https://micronaut.io) (1.2) | 55 507 | 64 022 | 63 642 |
| 75 | fsharp (4.7)| [websharper](https://websharper.com) (4.6) | 55 339 | 57 390 | 53 941 |
| 76 | javascript (14.7)| [nestjs-fastify](https://nestjs.com) (7.4) | 54 476 | 62 316 | 64 618 |
| 77 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 54 297 | 58 399 | 56 630 |
| 78 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 53 591 | 55 303 | 55 117 |
| 79 | javascript (14.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 52 223 | 56 430 | 54 271 |
| 80 | java (11)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 51 536 | 59 132 | 59 171 |
| 81 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 50 232 | 50 949 | 50 462 |
| 82 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 49 656 | 68 492 | 73 154 |
| 83 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.27) | 49 619 | 55 447 | 55 578 |
| 84 | swift (5.2)| [kitura](https://kitura.io) (2.9) | 49 257 | 50 849 | 50 786 |
| 85 | rust (1.45)| [nickel](https://nickel-org.github.io) (0.11) | 48 955 | 48 605 | 47 271 |
| 86 | javascript (14.7)| [moleculer](https://moleculer.services) (0.14) | 48 272 | 50 535 | 48 976 |
| 87 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 46 739 | 47 288 | 47 749 |
| 88 | rust (1.45)| [gotham](https://gotham.rs) (0.4) | 46 220 | 52 363 | 55 833 |
| 89 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 45 802 | 49 044 | 49 243 |
| 90 | swift (5.2)| [vapor](https://vapor.codes) (4.27) | 45 775 | 47 732 | 47 403 |
| 91 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 44 060 | 51 355 | 51 317 |
| 92 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 43 434 | 44 534 | 44 678 |
| 93 | python (3.8)| [hug](https://hug.rest) (2.6) | 43 221 | 45 958 | 46 194 |
| 94 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 42 328 | 47 741 | 46 861 |
| 95 | javascript (14.7)| [restify](https://restify.com) (8.5) | 40 784 | 43 330 | 42 656 |
| 96 | php (7.4)| [imi](https://imiphp.com) (1.2) | 40 173 | 45 130 | 46 087 |
| 97 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 39 647 | 40 281 | 40 678 |
| 98 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.6) | 39 563 | 42 579 | 42 130 |
| 99 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 39 372 | 43 776 | 43 570 |
| 100 | python (3.8)| [starlette](https://starlette.io) (0.13) | 38 043 | 43 664 | 43 278 |
| 101 | javascript (14.7)| [hapi](https://hapijs.com) (19.2) | 36 807 | 39 428 | 38 467 |
| 102 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 35 568 | 39 054 | 38 416 |
| 103 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.1) | 35 178 | 35 592 | 34 775 |
| 104 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 34 969 | 33 801 | 29 941 |
| 105 | scala (2.13)| [play](https://playframework.com) (2.8) | 33 315 | 34 321 | 35 819 |
| 106 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 33 224 | 34 046 | 34 369 |
| 107 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 30 815 | 31 842 | 31 619 |
| 108 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.15) | 30 372 | 29 644 | 26 577 |
| 109 | dart (2.8)| [aqueduct](https://aqueduct.io) (3.3) | 30 214 | 30 349 | 30 132 |
| 110 | fsharp (4.7)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 29 948 | 28 307 | 27 844 |
| 111 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 29 705 | 28 674 | 27 754 |
| 112 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 27 968 | 29 355 | 31 446 |
| 113 | php (7.4)| [swoft](https://swoft.org) (2.0) | 27 500 | 34 163 | 34 398 |
| 114 | nim (1.2)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 27 399 | 23 287 | 21 122 |
| 115 | nim (1.2)| [akane](https://github.com/Ethosa/akane) (0.1) | 27 004 | 23 876 | 21 243 |
| 116 | python (3.8)| [responder](https://python-responder.org) (2.0) | 26 812 | 28 874 | 28 910 |
| 117 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.6) | 26 368 | 28 594 | 28 199 |
| 118 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 26 072 | 31 920 | 33 222 |
| 119 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 25 774 | 31 224 | 31 586 |
| 120 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 24 861 | 27 334 | 26 144 |
| 121 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 24 624 | 28 007 | 27 009 |
| 122 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 24 600 | 29 637 | 29 825 |
| 123 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.34) | 24 565 | 27 963 | 27 011 |
| 124 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 24 536 | 25 127 | 24 706 |
| 125 | javascript (14.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 24 526 | 23 216 | 22 266 |
| 126 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 24 042 | 26 216 | 26 008 |
| 127 | javascript (14.7)| [express](https://expressjs.com) (4.17) | 23 755 | 24 871 | 24 813 |
| 128 | php (7.4)| [slim-swoole](https://slimframework.com) (4.5) | 23 546 | 28 272 | 28 240 |
| 129 | javascript (14.7)| [feathersjs](https://feathersjs.com) (4.5) | 23 544 | 24 417 | 24 418 |
| 130 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 491 | 23 955 | 23 896 |
| 131 | rust (1.45)| [iron](https://ironframework.io) (0.6) | 23 441 | 23 475 | 23 552 |
| 132 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 22 410 | 25 089 | 24 317 |
| 133 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 21 744 | 22 491 | 22 226 |
| 134 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 21 701 | 21 961 | 22 020 |
| 135 | go (1.14)| [macaron](https://go-macaron.com) (1.3) | 20 137 | 22 599 | 22 875 |
| 136 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 19 983 | 21 313 | 20 948 |
| 137 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 19 479 | 21 406 | 21 138 |
| 138 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 18 463 | 19 316 | 19 083 |
| 139 | javascript (14.7)| [nestjs-express](https://nestjs.com) (7.4) | 17 792 | 18 262 | 18 465 |
| 140 | java (11)| [blade](https://lets-blade.com) (2.0) | 14 820 | 17 318 | 16 832 |
| 141 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 14 043 | 14 552 | 14 573 |
| 142 | dart (2.8)| [start](https://github.com/lvivski/start) (0.4) | 13 936 | 13 597 | 12 684 |
| 143 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.5) | 12 782 | 13 022 | 12 890 |
| 144 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 12 511 | 12 772 | 12 765 |
| 145 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (0.2) | 12 508 | 12 412 | 12 013 |
| 146 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 12 156 | 12 277 | 12 385 |
| 147 | java (11)| [struts2](https://struts.apache.org) (2.5) | 12 059 | 12 981 | 12 873 |
| 148 | ruby (2.7)| [grape](https://ruby-grape.org) (1.4) | 11 479 | 11 840 | 11 834 |
| 149 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.13) | 11 177 | 11 186 | 10 518 |
| 150 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 9 848 | 10 461 | 10 486 |
| 151 | swift (5.2)| [swifter](https://github.com/httpswift/swifter) (1.4) | 9 723 | 9 871 | 9 790 |
| 152 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.5) | 9 688 | 10 393 | 10 352 |
| 153 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 561 | 9 714 | 9 557 |
| 154 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.4) | 9 554 | 9 380 | 9 229 |
| 155 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 278 | 9 259 | 9 152 |
| 156 | pony (0.36)| [jennet](https://github.com/Theodus/jennet) (0.1) | 8 925 | 16 928 | 15 720 |
| 157 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 8 695 | 8 619 | 8 580 |
| 158 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 8 131 | 8 010 | 7 876 |
| 159 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.3) | 7 843 | 7 731 | 7 607 |
| 160 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 7 775 | 7 944 | 7 928 |
| 161 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 7 724 | 7 855 | 7 809 |
| 162 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 6 684 | 6 537 | 6 571 |
| 163 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 6 487 | 6 352 | 6 099 |
| 164 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 5 959 | 5 873 | 5 886 |
| 165 | javascript (14.7)| [sails](https://sailsjs.com) (1.2) | 5 791 | 6 083 | 5 889 |
| 166 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 5 694 | 5 610 | 5 613 |
| 167 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 5 690 | 5 616 | 5 660 |
| 168 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 5 616 | 5 543 | 5 544 |
| 169 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.2) | 5 111 | 5 049 | 5 062 |
| 170 | php (7.4)| [ice](https://iceframework.org) (1.5) | 4 989 | 4 937 | 4 968 |
| 171 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.4) | 4 866 | 4 832 | 4 877 |
| 172 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 4 487 | 4 486 | 4 550 |
| 173 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 4 081 | 1 590 | 2 323 |
| 174 | php (7.4)| [slim](https://slimframework.com) (4.5) | 3 835 | 3 824 | 3 919 |
| 175 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 3 616 | 3 655 | 3 716 |
| 176 | php (7.4)| [lumen](https://lumen.laravel.com) (7.2) | 3 435 | 3 481 | 3 494 |
| 177 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 3 367 | 3 404 | 3 454 |
| 178 | perl (5.32)| [dancer2](https://perldancer.org) (2.0) | 3 308 | 3 789 | 2 053 |
| 179 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 285 | 3 110 | 3 151 |
| 180 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 281 | 7 692 | 5 934 |
| 181 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.4) | 3 077 | 3 110 | 3 172 |
| 182 | php (7.4)| [symfony](https://symfony.com) (5.1) | 2 708 | 2 738 | 2 792 |
| 183 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 493 | 2 529 | 2 562 |
| 184 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 259 | 2 255 | 2 222 |
| 185 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 571 | 1 569 | 1 572 |
| 186 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 527 | 1 487 | 1 466 |
| 187 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 330 | 1 339 | 1 366 |
| 188 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 017 | 1 039 | 1 095 |
| 189 | crystal (0.35)| [lucky](https://luckyframework.org) (0.23) | 315 | 340 | 330 |
| 190 | php (7.4)| [laravel](https://laravel.com) (7.22) | 314 | 164 | 3 427 |

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

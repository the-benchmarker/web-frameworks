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

:information_source:  Updated on **2020-07-17** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 166 812 | 198 814 | 201 607 |
| 2 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 166 477 | 170 714 | 170 731 |
| 3 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.15) | 163 762 | 179 212 | 185 942 |
| 4 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 159 389 | 172 340 | 179 573 |
| 5 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.2) | 158 618 | 172 263 | 178 690 |
| 6 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.4) | 157 297 | 172 702 | 179 954 |
| 7 | go (1.14)| [gearbox](https://gogearbox.com) (1.0) | 156 187 | 169 488 | 175 462 |
| 8 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 156 170 | 170 574 | 176 418 |
| 9 | javascript (13.14)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (2.2) | 155 930 | 196 487 | 199 377 |
| 10 | nim (1.2)| [whip](https://github.com/mattaylor/whip) (0.2) | 155 877 | 184 873 | 186 800 |
| 11 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 149 809 | 179 583 | 181 599 |
| 12 | javascript (13.14)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 149 729 | 185 241 | 187 830 |
| 13 | java (11)| [jooby](https://jooby.io) (2.8) | 146 668 | 186 604 | 186 539 |
| 14 | kotlin (1.3)| [kooby](https://jooby.io) (2.8) | 145 448 | 185 509 | 188 909 |
| 15 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 141 739 | 170 037 | 168 565 |
| 16 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 140 496 | 180 717 | 184 904 |
| 17 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 139 957 | 167 526 | 166 927 |
| 18 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 139 092 | 168 281 | 170 978 |
| 19 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.3) | 138 818 | 165 341 | 164 198 |
| 20 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (0.28) | 131 823 | 154 430 | 153 084 |
| 21 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 131 367 | 153 465 | 155 276 |
| 22 | crystal (0.35)| [kemal](https://kemalcr.com) (0.26) | 128 817 | 152 804 | 151 931 |
| 23 | rust (1.44)| [actix](https://actix.rs) (2.0) | 127 848 | 137 601 | 133 292 |
| 24 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 125 646 | 145 426 | 144 268 |
| 25 | go (1.14)| [clevergo](https://clevergo.tech) (0.3) | 109 419 | 112 425 | 117 950 |
| 26 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 109 093 | 111 823 | 117 892 |
| 27 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 108 426 | 111 040 | 116 962 |
| 28 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 108 415 | 110 542 | 116 175 |
| 29 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 106 943 | 177 179 | 178 468 |
| 30 | go (1.14)| [fiber](https://gofiber.io) (1.12) | 106 367 | 157 297 | 167 503 |
| 31 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 106 185 | 111 411 | 117 773 |
| 32 | java (11)| [act](https://actframework.org) (1.9) | 106 023 | 136 571 | 138 494 |
| 33 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 105 403 | 160 687 | 178 181 |
| 34 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 103 400 | 104 021 | 110 324 |
| 35 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 103 293 | 109 687 | 114 634 |
| 36 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 101 963 | 104 351 | 109 601 |
| 37 | c (99)| [kore](https://kore.io) (3.3) | 101 866 | 117 353 | 135 577 |
| 38 | go (1.14)| [violetear](https://violetear.org) (7.0) | 101 111 | 102 649 | 108 725 |
| 39 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 99 978 | 100 588 | 106 797 |
| 40 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 98 762 | 99 010 | 104 802 |
| 41 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 98 191 | 105 652 | 108 572 |
| 42 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 96 531 | 112 961 | 116 605 |
| 43 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 96 433 | 96 418 | 101 610 |
| 44 | go (1.14)| [beego](https://beego.me) (1.12) | 95 809 | 102 185 | 106 692 |
| 45 | fsharp (4.7)| [frank](https://github.com/frank-fs/frank) (6.1) | 94 808 | 108 449 | 111 709 |
| 46 | fsharp (4.7)| [falco](https://github.com/pimbrouwers/Falco) (1.2) | 93 731 | 109 063 | 111 753 |
| 47 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.9) | 87 095 | 104 864 | 104 967 |
| 48 | javascript (13.14)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 84 496 | 96 962 | 96 958 |
| 49 | go (1.14)| [air](https://github.com/aofei/air) (0.19) | 84 059 | 86 767 | 90 531 |
| 50 | javascript (13.14)| [0http](https://github.com/jkyberneees/0http) (2.5) | 80 408 | 92 737 | 92 927 |
| 51 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.1) | 78 175 | 80 376 | 72 299 |
| 52 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 77 797 | 85 441 | 88 469 |
| 53 | javascript (13.14)| [restana](https://github.com/jkyberneees/ana) (4.6) | 77 057 | 86 568 | 86 466 |
| 54 | javascript (13.14)| [polka](https://github.com/lukeed/polka) (0.5) | 76 887 | 83 330 | 81 249 |
| 55 | go (1.14)| [gf](https://goframe.org) (1.13) | 76 593 | 84 510 | 86 808 |
| 56 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 76 343 | 78 548 | 76 472 |
| 57 | swift (5.2)| [perfect](https://perfect.org) (3.1) | 75 710 | 86 099 | 92 764 |
| 58 | javascript (13.14)| [rayo](https://rayo.js.org) (1.3) | 75 314 | 82 203 | 80 504 |
| 59 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 74 612 | 90 500 | 98 353 |
| 60 | javascript (13.14)| [fastify](https://fastify.io) (3.0) | 74 572 | 81 697 | 79 811 |
| 61 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 73 922 | 85 604 | 82 925 |
| 62 | java (11)| [javalin](https://javalin.io) (3.9) | 73 715 | 81 379 | 81 750 |
| 63 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.13) | 72 037 | 105 532 | 116 988 |
| 64 | php (7.4)| [hyperf](https://hyperf.io) (2.0) | 69 482 | 84 318 | 95 400 |
| 65 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 68 521 | 76 879 | 77 552 |
| 66 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 67 696 | 88 839 | 91 858 |
| 67 | fsharp (4.7)| [saturn](https://saturnframework.org) (0.14) | 67 573 | 65 031 | 57 590 |
| 68 | javascript (13.14)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 66 927 | 73 194 | 71 791 |
| 69 | java (11)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 66 447 | 73 843 | 73 684 |
| 70 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.8) | 63 006 | 68 804 | 68 559 |
| 71 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 62 864 | 63 911 | 68 854 |
| 72 | javascript (13.14)| [foxify](https://foxify.js.org) (0.1) | 61 862 | 66 997 | 65 442 |
| 73 | java (11)| [micronaut](https://micronaut.io) (1.2) | 58 183 | 67 766 | 67 306 |
| 74 | javascript (13.14)| [koa](https://koajs.com) (2.13) | 57 159 | 61 692 | 59 711 |
| 75 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 56 693 | 63 480 | 67 177 |
| 76 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 55 820 | 60 833 | 59 884 |
| 77 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 55 216 | 56 939 | 56 806 |
| 78 | javascript (13.14)| [nestjs-fastify](https://nestjs.com) (7.3) | 54 606 | 62 008 | 60 553 |
| 79 | javascript (13.14)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 54 555 | 58 595 | 56 869 |
| 80 | java (11)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 53 412 | 62 092 | 61 948 |
| 81 | fsharp (4.7)| [websharper](https://websharper.com) (4.6) | 53 310 | 57 241 | 54 775 |
| 82 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.27) | 52 794 | 60 407 | 60 654 |
| 83 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 51 372 | 53 734 | 53 310 |
| 84 | javascript (13.14)| [feathersjs](https://feathersjs.com) (4.5) | 50 306 | 53 791 | 52 554 |
| 85 | javascript (13.14)| [express](https://expressjs.com) (4.17) | 49 672 | 53 529 | 52 054 |
| 86 | swift (5.2)| [kitura](https://kitura.io) (2.9) | 49 572 | 51 144 | 51 064 |
| 87 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 48 160 | 51 229 | 51 802 |
| 88 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 48 050 | 48 175 | 48 759 |
| 89 | swift (5.2)| [vapor](https://vapor.codes) (4.23) | 47 546 | 50 366 | 50 523 |
| 90 | rust (1.44)| [gotham](https://gotham.rs) (0.4) | 47 146 | 52 885 | 55 751 |
| 91 | javascript (13.14)| [moleculer](https://moleculer.services) (0.14) | 46 791 | 49 538 | 48 372 |
| 92 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 46 050 | 67 571 | 73 321 |
| 93 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 45 823 | 52 192 | 52 304 |
| 94 | python (3.8)| [hug](https://hug.rest) (2.6) | 44 930 | 47 759 | 47 322 |
| 95 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 44 428 | 45 047 | 45 260 |
| 96 | rust (1.44)| [nickel](https://nickel-org.github.io) (0.11) | 44 303 | 44 562 | 47 056 |
| 97 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.6) | 43 175 | 48 335 | 48 212 |
| 98 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 43 124 | 48 535 | 47 939 |
| 99 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 42 509 | 50 562 | 50 470 |
| 100 | php (7.4)| [imi](https://imiphp.com) (1.2) | 42 190 | 47 008 | 47 897 |
| 101 | python (3.8)| [starlette](https://starlette.io) (0.13) | 41 683 | 47 396 | 47 560 |
| 102 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 40 334 | 41 199 | 41 474 |
| 103 | javascript (13.14)| [hapi](https://hapijs.com) (19.1) | 39 578 | 42 783 | 41 860 |
| 104 | javascript (13.14)| [nestjs-express](https://nestjs.com) (7.3) | 38 536 | 41 057 | 40 183 |
| 105 | javascript (13.14)| [restify](https://restify.com) (8.5) | 36 801 | 40 038 | 39 833 |
| 106 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.1) | 36 631 | 37 806 | 36 636 |
| 107 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 36 348 | 40 827 | 40 611 |
| 108 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 35 149 | 33 349 | 31 356 |
| 109 | scala (2.13)| [play](https://playframework.com) (2.8) | 34 237 | 37 035 | 37 174 |
| 110 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 33 869 | 34 656 | 35 049 |
| 111 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 31 794 | 32 557 | 32 405 |
| 112 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 31 375 | 31 735 | 34 896 |
| 113 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.15) | 31 177 | 29 573 | 27 948 |
| 114 | dart (2.8)| [aqueduct](https://aqueduct.io) (3.3) | 30 865 | 31 125 | 30 257 |
| 115 | fsharp (4.7)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 29 567 | 28 611 | 28 050 |
| 116 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 28 432 | 27 713 | 27 672 |
| 117 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.59) | 28 260 | 30 716 | 30 450 |
| 118 | php (7.4)| [swoft](https://swoft.org) (2.0) | 27 857 | 34 082 | 35 227 |
| 119 | nim (1.2)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 27 838 | 23 797 | 21 675 |
| 120 | python (3.8)| [responder](https://python-responder.org) (2.0) | 26 868 | 29 820 | 29 623 |
| 121 | nim (1.2)| [akane](https://github.com/Ethosa/akane) (0.1) | 26 432 | 22 791 | 20 829 |
| 122 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 26 098 | 32 133 | 32 270 |
| 123 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 25 743 | 31 612 | 31 906 |
| 124 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 25 144 | 26 584 | 26 336 |
| 125 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 24 757 | 27 752 | 26 650 |
| 126 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 24 556 | 29 643 | 30 317 |
| 127 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 763 | 25 333 | 24 982 |
| 128 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 23 556 | 26 789 | 25 756 |
| 129 | rust (1.44)| [iron](https://ironframework.io) (0.6) | 23 277 | 23 570 | 23 442 |
| 130 | javascript (13.14)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 23 056 | 22 096 | 20 975 |
| 131 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.34) | 22 728 | 26 167 | 25 129 |
| 132 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 22 421 | 23 652 | 23 461 |
| 133 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 22 290 | 25 475 | 25 214 |
| 134 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 22 002 | 22 990 | 22 995 |
| 135 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 21 647 | 23 896 | 23 111 |
| 136 | go (1.14)| [macaron](https://go-macaron.com) (1.3) | 20 642 | 23 311 | 23 508 |
| 137 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 19 570 | 20 596 | 20 128 |
| 138 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 18 930 | 20 594 | 19 986 |
| 139 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 18 160 | 18 452 | 18 285 |
| 140 | java (11)| [blade](https://lets-blade.com) (2.0) | 15 402 | 17 706 | 17 529 |
| 141 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 14 434 | 14 962 | 15 035 |
| 142 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 13 360 | 13 589 | 13 632 |
| 143 | dart (2.8)| [start](https://github.com/lvivski/start) (0.4) | 12 970 | 12 751 | 12 379 |
| 144 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 12 966 | 13 304 | 13 278 |
| 145 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (0.2) | 12 642 | 12 513 | 11 893 |
| 146 | java (11)| [struts2](https://struts.apache.org) (2.5) | 12 494 | 13 309 | 13 346 |
| 147 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 12 232 | 12 426 | 12 486 |
| 148 | javascript (13.14)| [sails](https://sailsjs.com) (1.2) | 11 876 | 12 414 | 12 296 |
| 149 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.13) | 11 170 | 11 731 | 10 880 |
| 150 | ruby (2.7)| [grape](https://ruby-grape.org) (1.4) | 10 721 | 10 853 | 10 888 |
| 151 | swift (5.2)| [swifter](https://github.com/httpswift/swifter) (1.4) | 10 223 | 10 306 | 10 286 |
| 152 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 10 035 | 10 804 | 10 886 |
| 153 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.4) | 9 638 | 9 395 | 9 235 |
| 154 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 551 | 9 980 | 9 849 |
| 155 | pony (0.35)| [jennet](https://github.com/Theodus/jennet) (0.1) | 9 374 | 17 061 | 15 730 |
| 156 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 328 | 9 273 | 9 360 |
| 157 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 8 807 | 8 747 | 8 671 |
| 158 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.3) | 8 136 | 8 064 | 7 911 |
| 159 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 7 401 | 7 460 | 7 445 |
| 160 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 370 | 6 696 | 6 431 |
| 161 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 7 331 | 7 350 | 7 366 |
| 162 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 6 787 | 6 673 | 6 677 |
| 163 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 6 175 | 6 089 | 6 095 |
| 164 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 5 876 | 5 819 | 5 814 |
| 165 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 5 813 | 5 738 | 5 725 |
| 166 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 5 797 | 5 712 | 5 727 |
| 167 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.2) | 5 191 | 5 122 | 5 161 |
| 168 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.4) | 5 041 | 4 994 | 5 024 |
| 169 | php (7.4)| [ice](https://iceframework.org) (1.5) | 5 034 | 5 022 | 5 055 |
| 170 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 4 577 | 4 566 | 4 618 |
| 171 | php (7.4)| [slim](https://slimframework.com) (4.5) | 3 896 | 3 906 | 3 961 |
| 172 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 3 809 | 2 054 | 1 529 |
| 173 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 3 650 | 3 695 | 3 740 |
| 174 | php (7.4)| [lumen](https://lumen.laravel.com) (7.2) | 3 535 | 3 577 | 3 659 |
| 175 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 3 451 | 3 494 | 3 545 |
| 176 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 409 | 3 195 | 3 201 |
| 177 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 341 | 7 821 | 6 095 |
| 178 | perl (5.32)| [dancer2](https://perldancer.org) (2.0) | 3 204 | 2 393 | 1 172 |
| 179 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.4) | 3 159 | 3 231 | 3 282 |
| 180 | php (7.4)| [symfony](https://symfony.com) (5.1) | 2 847 | 2 887 | 2 907 |
| 181 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 518 | 2 555 | 2 590 |
| 182 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 435 | 2 401 | 2 429 |
| 183 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 1 745 | 2 161 | 2 136 |
| 184 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 607 | 1 633 | 1 605 |
| 185 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 551 | 1 509 | 1 501 |
| 186 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 380 | 1 400 | 1 415 |
| 187 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0.3) | 1 042 | 1 064 | 1 142 |
| 188 | php (7.4)| [laravel](https://laravel.com) (7.2) | 819 | 172 | 2 729 |
| 189 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 509 | 506 | 1 450 |
| 190 | crystal (0.35)| [lucky](https://luckyframework.org) (0.23) | 309 | 334 | 324 |

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

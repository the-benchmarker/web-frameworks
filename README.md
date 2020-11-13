nil
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
psql -U postgres -d benchmark < dump.sql
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
docker exec pg sh -c "echo \"$(cat dump.sql)\" | psql -U postgres -d benchmark"
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

:information_source:  Updated on **2020-11-13** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 182 927 | 204 968 | 206 931 |
| 2 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 181 712 | 189 594 | 189 792 |
| 3 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 179 963 | 210 498 | 212 184 |
| 4 | go (1.15)| [fiber](https://gofiber.io) (2.0) | 179 171 | 199 678 | 197 487 |
| 5 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 179 041 | 222 004 | 226 265 |
| 6 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.17) | 177 045 | 199 408 | 199 924 |
| 7 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 176 663 | 196 794 | 198 099 |
| 8 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.5) | 175 334 | 195 949 | 196 489 |
| 9 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 175 271 | 196 520 | 195 403 |
| 10 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 173 864 | 214 713 | 220 052 |
| 11 | kotlin (1.4)| [kooby](https://jooby.io) (2.8) | 165 552 | 206 793 | 214 592 |
| 12 | java (11)| [jooby](https://jooby.io) (2.8) | 164 988 | 209 615 | 215 790 |
| 13 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 164 685 | 200 988 | 204 999 |
| 14 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 163 972 | 200 664 | 205 414 |
| 15 | javascript (12.18)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 162 601 | 205 732 | 210 098 |
| 16 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 158 470 | 202 218 | 208 518 |
| 17 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 154 065 | 188 614 | 188 605 |
| 18 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 153 600 | 187 033 | 187 544 |
| 19 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 151 476 | 183 431 | 185 059 |
| 20 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.3) | 151 405 | 184 524 | 184 791 |
| 21 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 148 005 | 174 834 | 175 897 |
| 22 | crystal (0.35)| [kemal](https://kemalcr.com) (0.26) | 143 572 | 174 170 | 173 598 |
| 23 | java (11)| [act](https://actframework.org) (1.9) | 139 405 | 171 232 | 175 830 |
| 24 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 136 779 | 144 461 | 136 007 |
| 25 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 135 405 | 163 397 | 162 840 |
| 26 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.11) | 130 995 | 146 360 | 139 933 |
| 27 | javascript (12.18)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 129 174 | 150 665 | 152 289 |
| 28 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 124 091 | 124 065 | 128 426 |
| 29 | go (1.15)| [clevergo](https://clevergo.tech) (0.3) | 123 642 | 124 043 | 127 648 |
| 30 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 122 558 | 123 003 | 126 859 |
| 31 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 121 753 | 121 732 | 125 882 |
| 32 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 120 368 | 125 687 | 128 457 |
| 33 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 118 891 | 123 686 | 126 989 |
| 34 | go (1.15)| [violetear](https://violetear.org) (7.0) | 117 838 | 117 125 | 120 983 |
| 35 | go (1.15)| [chi](https://github.com/go-chi/chi) (4.1) | 117 666 | 114 448 | 119 564 |
| 36 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 117 266 | 178 709 | 199 336 |
| 37 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 116 089 | 115 194 | 118 900 |
| 38 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 114 116 | 112 402 | 116 040 |
| 39 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 113 703 | 119 933 | 120 974 |
| 40 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 112 632 | 108 265 | 113 430 |
| 41 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 110 399 | 105 747 | 110 896 |
| 42 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.1) | 109 964 | 127 624 | 131 525 |
| 43 | go (1.15)| [beego](https://beego.me) (1.12) | 109 402 | 113 433 | 116 975 |
| 44 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 109 256 | 127 501 | 131 885 |
| 45 | fsharp (5.0)| [falco](https://github.com/pimbrouwers/Falco) (1.2) | 108 845 | 127 809 | 132 387 |
| 46 | go (1.15)| [air](https://github.com/aofei/air) (0.20) | 98 046 | 95 127 | 99 441 |
| 47 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.1) | 96 332 | 106 992 | 106 593 |
| 48 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 95 872 | 139 471 | 151 876 |
| 49 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.4) | 94 758 | 93 763 | 97 974 |
| 50 | javascript (12.18)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 94 691 | 103 574 | 104 299 |
| 51 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 91 801 | 99 960 | 98 636 |
| 52 | c (99)| [kore](https://kore.io) (3.3) | 88 657 | 194 088 | 193 985 |
| 53 | javascript (12.18)| [0http](https://github.com/jkyberneees/0http) (3.0) | 87 936 | 98 038 | 98 364 |
| 54 | java (11)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 87 454 | 93 388 | 93 517 |
| 55 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 86 926 | 90 734 | 90 333 |
| 56 | javascript (12.18)| [polka](https://github.com/lukeed/polka) (0.5) | 86 598 | 93 326 | 91 555 |
| 57 | go (1.15)| [gf](https://goframe.org) (1.13) | 85 883 | 91 621 | 93 544 |
| 58 | java (11)| [spring-framework](https://spring.io/projects/spring-framework) (5.3) | 85 690 | 92 449 | 93 605 |
| 59 | javascript (12.18)| [restana](https://github.com/jkyberneees/ana) (4.7) | 85 545 | 94 634 | 93 870 |
| 60 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 84 634 | 93 790 | 101 333 |
| 61 | javascript (12.18)| [rayo](https://rayo.js.org) (1.3) | 84 560 | 90 982 | 89 322 |
| 62 | javascript (12.18)| [fastify](https://fastify.io) (3.7) | 83 808 | 91 736 | 89 440 |
| 63 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 82 829 | 104 555 | 113 447 |
| 64 | java (11)| [javalin](https://javalin.io) (3.9) | 82 770 | 88 305 | 89 855 |
| 65 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 80 864 | 96 914 | 95 430 |
| 66 | javascript (12.18)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 80 176 | 87 632 | 84 755 |
| 67 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.14) | 79 240 | 116 473 | 128 268 |
| 68 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 78 379 | 100 852 | 105 816 |
| 69 | fsharp (5.0)| [websharper](https://websharper.com) (4.6) | 76 580 | 85 887 | 87 164 |
| 70 | php (7.4)| [hyperf](https://hyperf.io) (2.0) | 75 590 | 100 477 | 108 722 |
| 71 | rust (1.47)| [actix](https://actix.rs) (3.2) | 73 519 | 82 645 | 86 529 |
| 72 | javascript (12.18)| [nestjs-fastify](https://nestjs.com) (7.4) | 73 162 | 80 813 | 78 317 |
| 73 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 72 373 | 80 765 | 81 461 |
| 74 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 71 715 | 81 170 | 87 377 |
| 75 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 69 258 | 68 877 | 73 202 |
| 76 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.8) | 68 896 | 75 045 | 75 911 |
| 77 | javascript (12.18)| [foxify](https://foxify.js.org) (0.1) | 67 837 | 73 068 | 71 676 |
| 78 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 65 931 | 66 681 | 64 767 |
| 79 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 62 605 | 68 540 | 68 989 |
| 80 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 62 040 | 67 426 | 68 352 |
| 81 | java (11)| [micronaut](https://micronaut.io) (1.2) | 60 480 | 73 330 | 72 693 |
| 82 | javascript (12.18)| [koa](https://koajs.com) (2.13) | 60 211 | 64 741 | 63 239 |
| 83 | swift (5.3)| [kitura](https://kitura.io) (2.9) | 57 546 | 56 698 | 57 132 |
| 84 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 57 119 | 59 121 | 60 310 |
| 85 | swift (5.3)| [kitura-nio](https://kitura.io) (2.9) | 55 092 | 54 217 | 54 718 |
| 86 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.27) | 54 815 | 60 627 | 62 005 |
| 87 | rust (1.47)| [nickel](https://nickel-org.github.io) (0.11) | 54 699 | 54 328 | 54 664 |
| 88 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 54 374 | 74 367 | 80 417 |
| 89 | javascript (12.18)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 52 940 | 56 370 | 55 495 |
| 90 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 52 710 | 57 276 | 57 165 |
| 91 | javascript (12.18)| [moleculer](https://moleculer.services) (0.14) | 52 321 | 55 449 | 53 376 |
| 92 | fsharp (5.0)| [suave](https://suave.io) (2.5) | 51 116 | 45 249 | 39 866 |
| 93 | javascript (12.18)| [hapi](https://hapijs.com) (20.0) | 50 637 | 52 661 | 51 183 |
| 94 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 50 000 | 53 212 | 53 874 |
| 95 | python (3.8)| [hug](https://hug.rest) (2.6) | 49 419 | 52 150 | 51 928 |
| 96 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 49 067 | 55 358 | 56 589 |
| 97 | javascript (12.18)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 48 446 | 51 202 | 53 451 |
| 98 | rust (1.47)| [gotham](https://gotham.rs) (0.4) | 47 690 | 51 744 | 52 957 |
| 99 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.9) | 47 561 | 51 680 | 52 604 |
| 100 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 46 953 | 53 863 | 54 662 |
| 101 | php (7.4)| [imi](https://imiphp.com) (1.2) | 44 979 | 52 243 | 52 804 |
| 102 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.1) | 43 682 | 48 440 | 48 085 |
| 103 | python (3.8)| [starlette](https://starlette.io) (0.13) | 43 139 | 49 697 | 50 339 |
| 104 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 42 882 | 54 470 | 55 341 |
| 105 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 42 624 | 45 811 | 44 556 |
| 106 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 42 551 | 34 931 | 30 881 |
| 107 | javascript (12.18)| [restify](https://restify.com) (8.5) | 41 764 | 45 684 | 44 888 |
| 108 | scala (2.13)| [play](https://playframework.com) (2.8) | 39 108 | 42 106 | 42 199 |
| 109 | javascript (12.18)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 38 391 | 42 640 | 41 191 |
| 110 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 38 359 | 36 056 | 32 097 |
| 111 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 36 088 | 42 282 | 42 185 |
| 112 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 34 698 | 38 375 | 37 312 |
| 113 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 34 123 | 32 947 | 32 883 |
| 114 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 33 484 | 32 813 | 31 275 |
| 115 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 32 946 | 33 031 | 32 298 |
| 116 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 32 505 | 32 555 | 32 004 |
| 117 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.16) | 31 986 | 31 579 | 29 674 |
| 118 | python (3.8)| [index.py](https://index-py.abersheeran.com) (0.12) | 31 526 | 39 223 | 39 297 |
| 119 | python (3.8)| [responder](https://python-responder.org) (2.0) | 31 262 | 33 516 | 33 133 |
| 120 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 29 641 | 34 683 | 34 696 |
| 121 | rust (1.47)| [iron](https://ironframework.io) (0.6) | 29 508 | 30 156 | 29 794 |
| 122 | javascript (12.18)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 29 100 | 27 878 | 26 833 |
| 123 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 29 074 | 35 676 | 37 310 |
| 124 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 28 648 | 31 676 | 30 820 |
| 125 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 28 112 | 25 344 | 23 779 |
| 126 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.61) | 27 788 | 32 836 | 32 747 |
| 127 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 27 247 | 29 252 | 29 111 |
| 128 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 26 616 | 25 759 | 25 071 |
| 129 | php (7.4)| [swoft](https://swoft.org) (2.0) | 26 589 | 31 577 | 31 924 |
| 130 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 25 748 | 25 048 | 24 441 |
| 131 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 25 688 | 22 891 | 20 880 |
| 132 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 25 471 | 29 121 | 30 304 |
| 133 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 25 313 | 30 787 | 30 796 |
| 134 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.36) | 24 356 | 24 598 | 23 905 |
| 135 | php (7.4)| [slim-swoole](https://slimframework.com) (4.5) | 24 228 | 28 416 | 28 738 |
| 136 | javascript (12.18)| [express](https://expressjs.com) (4.17) | 24 129 | 26 146 | 27 090 |
| 137 | javascript (12.18)| [feathersjs](https://feathersjs.com) (4.5) | 23 809 | 25 690 | 25 177 |
| 138 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 23 778 | 23 003 | 22 546 |
| 139 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 21 960 | 25 075 | 24 848 |
| 140 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 21 158 | 21 050 | 20 604 |
| 141 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 20 625 | 20 571 | 19 941 |
| 142 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 20 389 | 23 393 | 20 712 |
| 143 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 19 896 | 19 670 | 19 218 |
| 144 | go (1.15)| [macaron](https://go-macaron.com) (1.3) | 19 448 | 21 889 | 21 888 |
| 145 | javascript (12.18)| [nestjs-express](https://nestjs.com) (7.4) | 18 498 | 17 605 | 17 288 |
| 146 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 17 665 | 17 275 | 16 691 |
| 147 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 16 728 | 17 147 | 16 738 |
| 148 | java (11)| [blade](https://lets-blade.com) (2.0) | 16 426 | 20 559 | 19 928 |
| 149 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 15 530 | 15 940 | 16 054 |
| 150 | java (11)| [struts2](https://struts.apache.org) (2.5) | 15 442 | 16 034 | 15 972 |
| 151 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.4) | 15 237 | 15 721 | 15 939 |
| 152 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.5) | 14 617 | 15 295 | 15 473 |
| 153 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 14 423 | 15 046 | 15 212 |
| 154 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.6) | 14 316 | 14 691 | 14 960 |
| 155 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.1) | 13 437 | 13 241 | 13 202 |
| 156 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 13 042 | 13 400 | 13 330 |
| 157 | ruby (2.7)| [grape](https://ruby-grape.org) (1.5) | 12 931 | 12 432 | 12 413 |
| 158 | swift (5.3)| [swifter-framework](https://github.com/httpswift/swifter) (1.5) | 12 597 | 12 615 | 12 705 |
| 159 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.13) | 11 987 | 11 396 | 10 798 |
| 160 | python (3.8)| [tornado](https://tornadoweb.org) (6.1) | 10 148 | 9 938 | 9 652 |
| 161 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 350 | 9 344 | 9 150 |
| 162 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 9 159 | 9 028 | 8 821 |
| 163 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 8 915 | 8 818 | 8 571 |
| 164 | python (3.8)| [django](https://djangoproject.com) (3.1) | 8 655 | 8 682 | 8 514 |
| 165 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 750 | 7 849 | 7 689 |
| 166 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 7 631 | 18 346 | 17 628 |
| 167 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 7 579 | 7 413 | 7 501 |
| 168 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 7 135 | 7 069 | 7 107 |
| 169 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 6 537 | 6 466 | 6 496 |
| 170 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 306 | 6 202 | 6 290 |
| 171 | javascript (12.18)| [sails](https://sailsjs.com) (1.3) | 6 172 | 6 211 | 6 168 |
| 172 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 6 114 | 6 085 | 6 160 |
| 173 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.2) | 5 285 | 5 245 | 5 336 |
| 174 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.4) | 5 075 | 5 073 | 5 002 |
| 175 | php (7.4)| [ice](https://iceframework.org) (1.5) | 5 021 | 5 055 | 5 127 |
| 176 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 4 729 | 10 545 | 7 981 |
| 177 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 4 663 | 4 628 | 4 754 |
| 178 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 4 233 | 4 219 | 4 214 |
| 179 | php (7.4)| [slim](https://slimframework.com) (4.5) | 3 915 | 3 929 | 3 974 |
| 180 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 3 608 | 3 674 | 3 708 |
| 181 | php (7.4)| [lumen](https://lumen.laravel.com) (8.1) | 3 527 | 3 560 | 3 647 |
| 182 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 3 486 | 3 506 | 3 571 |
| 183 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 3 210 | 3 403 | 3 376 |
| 184 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.4) | 3 133 | 3 179 | 3 209 |
| 185 | php (7.4)| [symfony](https://symfony.com) (5.1) | 2 808 | 2 857 | 2 898 |
| 186 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 2 795 | 2 735 | 2 683 |
| 187 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 2 697 | 2 751 | 2 801 |
| 188 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 528 | 2 568 | 2 601 |
| 189 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 456 | 2 456 | 2 455 |
| 190 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 942 | 1 856 | 1 776 |
| 191 | python (3.8)| [django-ninja](https://django-ninja.rest-framework.com) (0.8) | 1 729 | 2 236 | 2 267 |
| 192 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 691 | 1 708 | 1 687 |
| 193 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 506 | 1 464 | 1 451 |
| 194 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 343 | 1 357 | 1 416 |
| 195 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 1 242 | 1 125 | 1 417 |
| 196 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 133 | 1 068 | 1 338 |
| 197 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 072 | 1 085 | 1 195 |
| 198 | crystal (0.35)| [lucky](https://luckyframework.org) (0.23) | 580 | 591 | 585 |
| 199 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 433 | 451 | 435 |
| 200 | php (7.4)| [laravel](https://laravel.com) (7.27) | 283 | 236 | 3 544 |

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

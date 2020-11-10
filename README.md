# Which is the fastest?

[![Build Status](https://the-benchmarker.semaphoreci.com/badges/web-frameworks/branches/master.svg)](https://the-benchmarker.semaphoreci.com/projects/web-frameworks)

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

:information_source:  Updated on **2020-11-05** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | go (1.15)| [fiber](https://gofiber.io) (2.0) | 169 728 | 181 862 | 183 418 |
| 2 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.16) | 169 038 | 183 813 | 184 384 |
| 3 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 168 006 | 172 705 | 172 622 |
| 4 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 167 604 | 177 253 | 177 806 |
| 5 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 167 276 | 181 708 | 179 071 |
| 6 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 166 242 | 180 922 | 179 163 |
| 7 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 166 147 | 199 087 | 201 467 |
| 8 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.5) | 165 818 | 180 873 | 180 462 |
| 9 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 162 464 | 193 489 | 186 348 |
| 10 | nim (1.2)| [whip](https://github.com/mattaylor/whip) (0.2) | 161 943 | 192 355 | 193 779 |
| 11 | java (11)| [jooby](https://jooby.io) (2.8) | 158 858 | 199 473 | 206 854 |
| 12 | javascript (12.18)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 154 386 | 194 178 | 195 460 |
| 13 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 153 823 | 193 652 | 198 295 |
| 14 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 150 971 | 178 514 | 181 225 |
| 15 | kotlin (1.4)| [kooby](https://jooby.io) (2.8) | 146 865 | 186 795 | 189 715 |
| 16 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 144 271 | 169 077 | 167 665 |
| 17 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 144 193 | 176 854 | 177 918 |
| 18 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 141 048 | 169 319 | 165 659 |
| 19 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.3) | 140 079 | 166 026 | 165 199 |
| 20 | java (11)| [act](https://actframework.org) (1.9) | 134 049 | 164 079 | 169 434 |
| 21 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 132 436 | 140 281 | 141 219 |
| 22 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 132 390 | 156 974 | 154 731 |
| 23 | crystal (0.35)| [kemal](https://kemalcr.com) (0.26) | 127 799 | 153 561 | 155 024 |
| 24 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 125 367 | 145 804 | 145 287 |
| 25 | c (99)| [kore](https://kore.io) (3.3) | 123 255 | 184 928 | 189 632 |
| 26 | javascript (12.18)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 119 972 | 143 556 | 143 850 |
| 27 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.9) | 115 794 | 131 125 | 120 210 |
| 28 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 115 700 | 115 575 | 118 922 |
| 29 | go (1.15)| [clevergo](https://clevergo.tech) (0.3) | 114 996 | 115 050 | 119 045 |
| 30 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 114 447 | 113 525 | 116 939 |
| 31 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 114 316 | 114 682 | 117 687 |
| 32 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 112 304 | 116 445 | 119 310 |
| 33 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 111 748 | 115 464 | 117 430 |
| 34 | rust (1.46)| [actix](https://actix.rs) (3.1) | 110 783 | 115 158 | 106 450 |
| 35 | go (1.15)| [chi](https://github.com/go-chi/chi) (4.1) | 110 000 | 106 608 | 110 798 |
| 36 | go (1.15)| [violetear](https://violetear.org) (7.0) | 109 860 | 109 274 | 113 235 |
| 37 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 109 245 | 106 991 | 111 010 |
| 38 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 108 668 | 166 150 | 181 713 |
| 39 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 107 303 | 104 847 | 108 944 |
| 40 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 105 662 | 112 320 | 113 686 |
| 41 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 104 771 | 100 765 | 104 725 |
| 42 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 103 807 | 98 155 | 103 369 |
| 43 | go (1.15)| [beego](https://beego.me) (1.12) | 101 686 | 104 852 | 108 363 |
| 44 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 97 248 | 118 985 | 117 064 |
| 45 | fsharp (4.7)| [falco](https://github.com/pimbrouwers/Falco) (1.2) | 97 192 | 111 765 | 115 443 |
| 46 | fsharp (4.7)| [frank](https://github.com/frank-fs/frank) (6.1) | 96 423 | 113 437 | 116 546 |
| 47 | go (1.15)| [air](https://github.com/aofei/air) (0.19) | 89 138 | 87 241 | 91 308 |
| 48 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 89 044 | 132 366 | 140 019 |
| 49 | javascript (12.18)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 88 041 | 97 685 | 98 281 |
| 50 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.1) | 86 588 | 85 359 | 89 957 |
| 51 | java (11)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 83 893 | 90 151 | 89 750 |
| 52 | javascript (12.18)| [0http](https://github.com/jkyberneees/0http) (3.0) | 83 209 | 93 595 | 94 160 |
| 53 | php (7.4)| [hyperf](https://hyperf.io) (2.0) | 82 731 | 85 087 | 92 148 |
| 54 | javascript (12.18)| [polka](https://github.com/lukeed/polka) (0.5) | 81 859 | 88 753 | 87 344 |
| 55 | java (11)| [javalin](https://javalin.io) (3.9) | 79 878 | 84 494 | 86 392 |
| 56 | java (11)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 79 877 | 88 301 | 89 396 |
| 57 | go (1.15)| [gf](https://goframe.org) (1.13) | 79 669 | 85 767 | 88 394 |
| 58 | javascript (12.18)| [restana](https://github.com/jkyberneees/ana) (4.7) | 78 959 | 89 408 | 88 791 |
| 59 | javascript (12.18)| [rayo](https://rayo.js.org) (1.3) | 78 812 | 85 096 | 84 152 |
| 60 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 78 294 | 88 235 | 95 031 |
| 61 | javascript (12.18)| [fastify](https://fastify.io) (3.7) | 78 039 | 86 384 | 84 820 |
| 62 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 77 969 | 79 844 | 78 394 |
| 63 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.1) | 77 102 | 81 970 | 74 482 |
| 64 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 76 582 | 103 030 | 105 567 |
| 65 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.13) | 76 025 | 107 866 | 118 380 |
| 66 | javascript (12.18)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 74 820 | 81 510 | 79 576 |
| 67 | fsharp (4.7)| [saturn](https://saturnframework.org) (0.14) | 74 303 | 76 460 | 68 728 |
| 68 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 73 927 | 85 451 | 82 910 |
| 69 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 69 559 | 73 490 | 76 953 |
| 70 | javascript (12.18)| [nestjs-fastify](https://nestjs.com) (7.4) | 69 195 | 72 603 | 72 333 |
| 71 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 68 148 | 86 493 | 89 921 |
| 72 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 66 358 | 77 973 | 77 780 |
| 73 | javascript (12.18)| [foxify](https://foxify.js.org) (0.1) | 64 783 | 69 925 | 68 625 |
| 74 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 64 739 | 63 801 | 68 534 |
| 75 | java (11)| [micronaut](https://micronaut.io) (1.2) | 64 181 | 71 716 | 71 114 |
| 76 | fsharp (4.7)| [websharper](https://websharper.com) (4.6) | 63 569 | 68 235 | 67 000 |
| 77 | javascript (12.18)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 63 344 | 69 584 | 67 521 |
| 78 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.8) | 62 521 | 68 366 | 67 816 |
| 79 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 61 975 | 64 152 | 64 507 |
| 80 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 59 629 | 170 443 | 194 965 |
| 81 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 59 189 | 61 033 | 59 159 |
| 82 | javascript (12.18)| [koa](https://koajs.com) (2.13) | 58 377 | 62 720 | 60 536 |
| 83 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 57 945 | 63 723 | 63 815 |
| 84 | rust (1.46)| [nickel](https://nickel-org.github.io) (0.11) | 54 215 | 56 178 | 53 040 |
| 85 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 53 231 | 79 265 | 85 067 |
| 86 | swift (5.3)| [kitura-nio](https://kitura.io) (2.9) | 52 808 | 53 380 | 53 367 |
| 87 | swift (5.3)| [kitura](https://kitura.io) (2.9) | 52 544 | 52 772 | 53 458 |
| 88 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 50 565 | 52 933 | 52 484 |
| 89 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 50 325 | 50 318 | 50 471 |
| 90 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 50 132 | 55 504 | 56 474 |
| 91 | javascript (12.18)| [moleculer](https://moleculer.services) (0.14) | 48 522 | 50 501 | 49 608 |
| 92 | javascript (12.18)| [hapi](https://hapijs.com) (20.0) | 47 889 | 49 078 | 48 829 |
| 93 | javascript (12.18)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 47 798 | 53 426 | 51 702 |
| 94 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 45 689 | 49 423 | 48 842 |
| 95 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.27) | 45 235 | 54 951 | 54 850 |
| 96 | javascript (12.18)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 44 593 | 46 667 | 47 551 |
| 97 | rust (1.46)| [gotham](https://gotham.rs) (0.4) | 43 992 | 49 075 | 50 596 |
| 98 | python (3.8)| [hug](https://hug.rest) (2.6) | 43 927 | 46 420 | 47 865 |
| 99 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 43 354 | 49 632 | 49 958 |
| 100 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 43 300 | 48 908 | 50 613 |
| 101 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 42 858 | 51 416 | 51 553 |
| 102 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.9) | 42 382 | 47 962 | 47 877 |
| 103 | python (3.8)| [starlette](https://starlette.io) (0.13) | 41 828 | 46 490 | 46 901 |
| 104 | php (7.4)| [imi](https://imiphp.com) (1.2) | 41 649 | 48 076 | 47 727 |
| 105 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 39 458 | 39 394 | 39 550 |
| 106 | javascript (12.18)| [restify](https://restify.com) (8.5) | 39 257 | 42 821 | 41 721 |
| 107 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.1) | 38 500 | 39 979 | 38 764 |
| 108 | javascript (12.18)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 37 461 | 40 898 | 40 613 |
| 109 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 37 445 | 37 747 | 37 803 |
| 110 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 35 669 | 32 897 | 33 568 |
| 111 | scala (2.13)| [play](https://playframework.com) (2.8) | 35 184 | 37 799 | 38 003 |
| 112 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 33 846 | 38 937 | 39 392 |
| 113 | python (3.8)| [index.py](https://index-py.abersheeran.com) (0.12) | 32 773 | 36 230 | 35 929 |
| 114 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 32 309 | 31 110 | 29 951 |
| 115 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 32 199 | 31 792 | 31 134 |
| 116 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 31 779 | 32 487 | 32 701 |
| 117 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 31 767 | 31 122 | 31 609 |
| 118 | nim (1.2)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 31 715 | 30 241 | 29 589 |
| 119 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.16) | 31 181 | 29 557 | 27 618 |
| 120 | rust (1.46)| [iron](https://ironframework.io) (0.6) | 29 297 | 29 435 | 29 972 |
| 121 | javascript (12.18)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 28 955 | 27 504 | 25 451 |
| 122 | php (7.4)| [swoft](https://swoft.org) (2.0) | 28 383 | 34 865 | 36 372 |
| 123 | python (3.8)| [responder](https://python-responder.org) (2.0) | 27 219 | 29 846 | 29 654 |
| 124 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 27 056 | 31 613 | 31 547 |
| 125 | nim (1.2)| [akane](https://github.com/Ethosa/akane) (0.1) | 27 031 | 24 156 | 22 493 |
| 126 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 26 727 | 32 858 | 34 835 |
| 127 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.61) | 26 628 | 30 132 | 29 506 |
| 128 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 26 293 | 30 896 | 31 730 |
| 129 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 26 117 | 28 409 | 28 511 |
| 130 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 25 764 | 31 386 | 32 736 |
| 131 | php (7.4)| [slim-swoole](https://slimframework.com) (4.5) | 25 565 | 30 794 | 31 543 |
| 132 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 24 369 | 24 385 | 23 461 |
| 133 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 24 120 | 23 595 | 22 983 |
| 134 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 23 971 | 26 644 | 26 902 |
| 135 | nim (1.2)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 23 825 | 21 007 | 18 874 |
| 136 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 416 | 25 443 | 24 850 |
| 137 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.36) | 23 274 | 22 554 | 22 333 |
| 138 | javascript (12.18)| [express](https://expressjs.com) (4.17) | 22 893 | 23 672 | 24 186 |
| 139 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 22 595 | 21 290 | 22 102 |
| 140 | javascript (12.18)| [feathersjs](https://feathersjs.com) (4.5) | 22 477 | 23 268 | 24 209 |
| 141 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 22 040 | 21 655 | 21 141 |
| 142 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 19 784 | 19 903 | 19 421 |
| 143 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 19 420 | 19 108 | 18 646 |
| 144 | go (1.15)| [macaron](https://go-macaron.com) (1.3) | 18 981 | 20 963 | 20 103 |
| 145 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 18 641 | 18 366 | 17 928 |
| 146 | javascript (12.18)| [nestjs-express](https://nestjs.com) (7.4) | 18 145 | 19 441 | 18 966 |
| 147 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 16 707 | 16 178 | 15 245 |
| 148 | java (11)| [blade](https://lets-blade.com) (2.0) | 15 705 | 19 475 | 19 158 |
| 149 | fsharp (4.7)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 15 339 | 15 489 | 14 959 |
| 150 | java (11)| [struts2](https://struts.apache.org) (2.5) | 14 701 | 15 244 | 14 947 |
| 151 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.4) | 14 234 | 15 008 | 15 041 |
| 152 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 14 054 | 14 900 | 14 562 |
| 153 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 13 921 | 14 422 | 14 347 |
| 154 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.6) | 13 775 | 14 228 | 14 114 |
| 155 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.5) | 13 383 | 13 787 | 13 877 |
| 156 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.1) | 13 029 | 12 789 | 12 701 |
| 157 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 12 472 | 13 142 | 13 208 |
| 158 | ruby (2.7)| [grape](https://ruby-grape.org) (1.5) | 12 063 | 11 671 | 11 529 |
| 159 | swift (5.3)| [swifter-framework](https://github.com/httpswift/swifter) (1.5) | 11 544 | 11 766 | 11 726 |
| 160 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.13) | 10 019 | 9 793 | 9 274 |
| 161 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 396 | 9 721 | 9 795 |
| 162 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 079 | 9 348 | 8 720 |
| 163 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 8 881 | 8 780 | 8 585 |
| 164 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 8 647 | 8 561 | 8 393 |
| 165 | python (3.8)| [django](https://djangoproject.com) (3.1) | 8 472 | 8 513 | 8 079 |
| 166 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 8 174 | 17 384 | 16 912 |
| 167 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 130 | 6 955 | 6 755 |
| 168 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 7 093 | 6 957 | 6 999 |
| 169 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 6 394 | 6 272 | 6 295 |
| 170 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 6 360 | 6 229 | 6 321 |
| 171 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 142 | 6 088 | 6 125 |
| 172 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 5 844 | 5 863 | 5 848 |
| 173 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 5 789 | 5 798 | 5 726 |
| 174 | javascript (12.18)| [sails](https://sailsjs.com) (1.3) | 5 726 | 5 773 | 5 849 |
| 175 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.2) | 5 071 | 4 996 | 5 038 |
| 176 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.4) | 4 904 | 4 915 | 4 997 |
| 177 | php (7.4)| [ice](https://iceframework.org) (1.5) | 4 699 | 4 658 | 4 755 |
| 178 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 4 609 | 9 393 | 7 249 |
| 179 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 4 236 | 4 246 | 4 296 |
| 180 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 887 | 3 891 | 3 930 |
| 181 | php (7.4)| [slim](https://slimframework.com) (4.5) | 3 613 | 3 655 | 3 718 |
| 182 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 3 337 | 3 349 | 3 399 |
| 183 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 3 255 | 3 415 | 3 385 |
| 184 | php (7.4)| [lumen](https://lumen.laravel.com) (8.1) | 3 228 | 3 260 | 3 353 |
| 185 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 3 218 | 3 229 | 3 269 |
| 186 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.4) | 3 036 | 3 080 | 3 149 |
| 187 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 2 919 | 2 988 | 2 943 |
| 188 | php (7.4)| [symfony](https://symfony.com) (5.1) | 2 767 | 2 784 | 2 824 |
| 189 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 2 754 | 2 652 | 2 582 |
| 190 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 421 | 2 456 | 2 482 |
| 191 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 416 | 2 414 | 2 402 |
| 192 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 912 | 1 814 | 1 736 |
| 193 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 630 | 1 651 | 1 612 |
| 194 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 534 | 1 513 | 1 493 |
| 195 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 352 | 1 378 | 1 439 |
| 196 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 1 165 | 2 025 | 5 365 |
| 197 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 016 | 1 030 | 1 112 |
| 198 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 970 | 1 841 | 3 256 |
| 199 | crystal (0.35)| [lucky](https://luckyframework.org) (0.23) | 655 | 666 | 654 |
| 200 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 429 | 438 | 432 |
| 201 | php (7.4)| [laravel](https://laravel.com) (7.27) | 289 | 161 | 2 659 |

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

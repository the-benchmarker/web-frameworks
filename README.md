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

:information_source:  Updated on **2020-07-18** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 167 900 | 172 088 | 173 349 |
| 2 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 165 808 | 197 548 | 200 123 |
| 3 | javascript (13.14)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (2.2) | 159 374 | 203 682 | 209 592 |
| 4 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.15) | 158 244 | 170 931 | 174 931 |
| 5 | nim (1.2)| [whip](https://github.com/mattaylor/whip) (0.2) | 157 852 | 185 577 | 186 965 |
| 6 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.4) | 153 402 | 165 386 | 169 576 |
| 7 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.2) | 152 854 | 164 684 | 169 379 |
| 8 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 152 460 | 164 763 | 168 679 |
| 9 | javascript (13.14)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 151 082 | 193 326 | 196 249 |
| 10 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 150 957 | 162 632 | 166 975 |
| 11 | go (1.14)| [gearbox](https://gogearbox.com) (1.0) | 149 964 | 161 740 | 165 613 |
| 12 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 148 731 | 176 813 | 177 967 |
| 13 | java (11)| [jooby](https://jooby.io) (2.8) | 144 563 | 187 072 | 191 087 |
| 14 | kotlin (1.3)| [kooby](https://jooby.io) (2.8) | 143 410 | 184 223 | 188 305 |
| 15 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 143 179 | 189 183 | 191 403 |
| 16 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 141 210 | 176 879 | 181 302 |
| 17 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 139 828 | 167 278 | 166 448 |
| 18 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 139 397 | 169 974 | 169 052 |
| 19 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.3) | 138 223 | 165 592 | 164 228 |
| 20 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 137 200 | 167 944 | 170 403 |
| 21 | crystal (0.35)| [kemal](https://kemalcr.com) (0.26) | 132 422 | 156 420 | 154 940 |
| 22 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (0.28) | 130 657 | 156 505 | 154 662 |
| 23 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 130 311 | 152 825 | 154 276 |
| 24 | rust (1.45)| [actix](https://actix.rs) (2.0) | 125 383 | 136 847 | 133 849 |
| 25 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 125 178 | 144 644 | 143 136 |
| 26 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 106 139 | 161 415 | 178 670 |
| 27 | go (1.14)| [clevergo](https://clevergo.tech) (0.3) | 104 631 | 106 188 | 110 306 |
| 28 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 104 620 | 106 149 | 110 679 |
| 29 | java (11)| [act](https://actframework.org) (1.9) | 104 185 | 134 428 | 135 251 |
| 30 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 103 125 | 105 277 | 109 307 |
| 31 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 103 050 | 104 779 | 108 820 |
| 32 | go (1.14)| [fiber](https://gofiber.io) (1.12) | 102 607 | 147 858 | 158 998 |
| 33 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 101 067 | 106 832 | 110 609 |
| 34 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 98 634 | 99 431 | 103 165 |
| 35 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 98 633 | 103 964 | 107 547 |
| 36 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 97 903 | 99 607 | 103 562 |
| 37 | go (1.14)| [violetear](https://violetear.org) (7.0) | 96 608 | 97 463 | 101 120 |
| 38 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 95 193 | 96 077 | 99 893 |
| 39 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 94 733 | 108 017 | 110 905 |
| 40 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 94 052 | 93 621 | 97 834 |
| 41 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 93 292 | 100 148 | 102 552 |
| 42 | fsharp (4.7)| [falco](https://github.com/pimbrouwers/Falco) (1.2) | 92 339 | 106 072 | 108 406 |
| 43 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 92 088 | 90 908 | 95 179 |
| 44 | fsharp (4.7)| [frank](https://github.com/frank-fs/frank) (6.1) | 91 990 | 104 069 | 106 111 |
| 45 | go (1.14)| [beego](https://beego.me) (1.12) | 91 186 | 96 653 | 100 384 |
| 46 | javascript (13.14)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 90 657 | 102 551 | 103 376 |
| 47 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.9) | 86 509 | 105 308 | 104 455 |
| 48 | c (99)| [kore](https://kore.io) (3.3) | 85 960 | 119 082 | 141 485 |
| 49 | javascript (13.14)| [0http](https://github.com/jkyberneees/0http) (2.5) | 85 365 | 98 058 | 99 126 |
| 50 | javascript (13.14)| [restana](https://github.com/jkyberneees/ana) (4.6) | 81 941 | 91 578 | 91 555 |
| 51 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 81 662 | 87 368 | 90 524 |
| 52 | javascript (13.14)| [polka](https://github.com/lukeed/polka) (0.5) | 80 601 | 88 104 | 87 295 |
| 53 | go (1.14)| [air](https://github.com/aofei/air) (0.19) | 80 321 | 81 983 | 85 383 |
| 54 | javascript (13.14)| [rayo](https://rayo.js.org) (1.3) | 78 392 | 85 836 | 84 595 |
| 55 | javascript (13.14)| [fastify](https://fastify.io) (3.1) | 77 280 | 86 412 | 85 020 |
| 56 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 77 117 | 79 037 | 77 251 |
| 57 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 76 042 | 91 735 | 96 299 |
| 58 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.13) | 74 106 | 110 573 | 123 845 |
| 59 | go (1.14)| [gf](https://goframe.org) (1.13) | 73 254 | 80 074 | 81 884 |
| 60 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 73 141 | 85 596 | 82 421 |
| 61 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.1) | 73 130 | 74 246 | 66 783 |
| 62 | swift (5.2)| [perfect](https://perfect.org) (3.1) | 72 461 | 82 137 | 87 742 |
| 63 | php (7.4)| [hyperf](https://hyperf.io) (2.0) | 71 889 | 90 300 | 94 927 |
| 64 | java (11)| [javalin](https://javalin.io) (3.9) | 71 495 | 79 141 | 78 295 |
| 65 | javascript (13.14)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 70 256 | 77 801 | 76 235 |
| 66 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 68 617 | 75 668 | 76 314 |
| 67 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 67 121 | 88 190 | 88 361 |
| 68 | fsharp (4.7)| [saturn](https://saturnframework.org) (0.14) | 66 497 | 62 731 | 55 831 |
| 69 | java (11)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 65 455 | 71 623 | 71 444 |
| 70 | javascript (13.14)| [foxify](https://foxify.js.org) (0.1) | 64 148 | 70 024 | 69 199 |
| 71 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.8) | 62 612 | 67 405 | 67 497 |
| 72 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 59 797 | 60 607 | 64 349 |
| 73 | javascript (13.14)| [koa](https://koajs.com) (2.13) | 59 674 | 65 111 | 63 278 |
| 74 | java (11)| [micronaut](https://micronaut.io) (1.2) | 57 662 | 66 616 | 65 711 |
| 75 | fsharp (4.7)| [websharper](https://websharper.com) (4.6) | 56 923 | 58 362 | 54 924 |
| 76 | javascript (13.14)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 56 004 | 58 967 | 59 745 |
| 77 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 55 879 | 62 428 | 66 645 |
| 78 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 55 116 | 60 823 | 60 007 |
| 79 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 53 021 | 54 762 | 54 992 |
| 80 | javascript (13.14)| [feathersjs](https://feathersjs.com) (4.5) | 53 012 | 56 219 | 55 119 |
| 81 | javascript (13.14)| [express](https://expressjs.com) (4.17) | 52 902 | 56 723 | 55 330 |
| 82 | rust (1.45)| [nickel](https://nickel-org.github.io) (0.11) | 52 618 | 48 793 | 51 696 |
| 83 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.27) | 51 515 | 59 053 | 59 192 |
| 84 | java (11)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 51 296 | 59 316 | 59 156 |
| 85 | javascript (13.14)| [nestjs-fastify](https://nestjs.com) (7.3) | 50 931 | 60 892 | 59 478 |
| 86 | swift (5.2)| [kitura](https://kitura.io) (2.9) | 50 405 | 50 872 | 50 863 |
| 87 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 49 712 | 51 150 | 51 070 |
| 88 | javascript (13.14)| [moleculer](https://moleculer.services) (0.14) | 49 456 | 52 663 | 51 879 |
| 89 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 47 798 | 48 484 | 48 750 |
| 90 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 47 764 | 50 253 | 50 442 |
| 91 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 47 627 | 54 254 | 54 501 |
| 92 | rust (1.45)| [gotham](https://gotham.rs) (0.4) | 46 373 | 52 337 | 54 878 |
| 93 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 45 565 | 68 053 | 74 899 |
| 94 | swift (5.2)| [vapor](https://vapor.codes) (4.23) | 45 341 | 48 039 | 47 892 |
| 95 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 44 967 | 50 418 | 50 507 |
| 96 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 43 647 | 44 683 | 44 202 |
| 97 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 42 326 | 48 277 | 47 692 |
| 98 | javascript (13.14)| [hapi](https://hapijs.com) (19.1) | 42 248 | 45 758 | 44 916 |
| 99 | python (3.8)| [hug](https://hug.rest) (2.6) | 42 168 | 44 293 | 44 976 |
| 100 | javascript (13.14)| [nestjs-express](https://nestjs.com) (7.3) | 41 140 | 43 789 | 42 970 |
| 101 | php (7.4)| [imi](https://imiphp.com) (1.2) | 41 017 | 47 086 | 47 684 |
| 102 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.6) | 40 439 | 48 110 | 47 946 |
| 103 | python (3.8)| [starlette](https://starlette.io) (0.13) | 40 394 | 44 210 | 44 588 |
| 104 | javascript (13.14)| [restify](https://restify.com) (8.5) | 39 954 | 43 417 | 42 549 |
| 105 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 39 654 | 40 583 | 40 747 |
| 106 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.1) | 36 547 | 37 086 | 35 917 |
| 107 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 35 263 | 39 568 | 39 663 |
| 108 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 34 914 | 33 918 | 29 528 |
| 109 | scala (2.13)| [play](https://playframework.com) (2.8) | 34 647 | 37 040 | 37 025 |
| 110 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 33 992 | 35 518 | 35 721 |
| 111 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 31 689 | 32 569 | 32 417 |
| 112 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.15) | 30 797 | 30 161 | 27 481 |
| 113 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 30 434 | 29 284 | 28 323 |
| 114 | fsharp (4.7)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 30 235 | 28 822 | 28 120 |
| 115 | dart (2.8)| [aqueduct](https://aqueduct.io) (3.3) | 29 971 | 30 096 | 29 962 |
| 116 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 29 197 | 29 977 | 31 845 |
| 117 | nim (1.2)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 27 634 | 23 877 | 21 571 |
| 118 | php (7.4)| [swoft](https://swoft.org) (2.0) | 27 634 | 33 470 | 34 678 |
| 119 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.59) | 26 912 | 29 394 | 29 387 |
| 120 | nim (1.2)| [akane](https://github.com/Ethosa/akane) (0.1) | 26 627 | 22 183 | 19 714 |
| 121 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 26 277 | 31 291 | 32 132 |
| 122 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 25 741 | 29 593 | 28 479 |
| 123 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 25 667 | 31 452 | 31 917 |
| 124 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 25 632 | 28 830 | 27 512 |
| 125 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.34) | 24 920 | 28 354 | 27 291 |
| 126 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 24 593 | 25 860 | 25 732 |
| 127 | python (3.8)| [responder](https://python-responder.org) (2.0) | 24 299 | 26 653 | 26 415 |
| 128 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 24 293 | 29 108 | 29 633 |
| 129 | rust (1.45)| [iron](https://ironframework.io) (0.6) | 24 175 | 24 341 | 24 340 |
| 130 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 23 664 | 26 132 | 26 490 |
| 131 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 404 | 25 203 | 25 107 |
| 132 | javascript (13.14)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 23 248 | 22 736 | 21 330 |
| 133 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 23 016 | 25 621 | 25 030 |
| 134 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 22 134 | 23 324 | 23 179 |
| 135 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 22 075 | 23 192 | 23 136 |
| 136 | go (1.14)| [macaron](https://go-macaron.com) (1.3) | 20 151 | 22 685 | 22 867 |
| 137 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 19 946 | 21 276 | 21 418 |
| 138 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 19 540 | 20 793 | 20 692 |
| 139 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 18 954 | 19 060 | 18 964 |
| 140 | java (11)| [blade](https://lets-blade.com) (2.0) | 15 292 | 17 610 | 17 259 |
| 141 | dart (2.8)| [start](https://github.com/lvivski/start) (0.4) | 14 224 | 13 842 | 12 911 |
| 142 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 14 146 | 14 645 | 14 701 |
| 143 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 13 078 | 13 360 | 13 230 |
| 144 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (0.2) | 13 002 | 12 826 | 11 896 |
| 145 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 12 825 | 12 867 | 12 936 |
| 146 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 12 526 | 12 784 | 12 771 |
| 147 | java (11)| [struts2](https://struts.apache.org) (2.5) | 12 128 | 13 157 | 12 915 |
| 148 | javascript (13.14)| [sails](https://sailsjs.com) (1.2) | 12 108 | 12 938 | 12 548 |
| 149 | ruby (2.7)| [grape](https://ruby-grape.org) (1.4) | 11 869 | 11 833 | 11 831 |
| 150 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.13) | 11 074 | 11 607 | 10 829 |
| 151 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 10 004 | 10 777 | 10 760 |
| 152 | swift (5.2)| [swifter](https://github.com/httpswift/swifter) (1.4) | 9 942 | 10 134 | 10 016 |
| 153 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.4) | 9 743 | 9 456 | 9 244 |
| 154 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 480 | 9 487 | 9 184 |
| 155 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 092 | 9 871 | 9 402 |
| 156 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 8 804 | 8 765 | 8 703 |
| 157 | pony (0.35)| [jennet](https://github.com/Theodus/jennet) (0.1) | 8 516 | 16 636 | 15 585 |
| 158 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 8 030 | 8 004 | 8 002 |
| 159 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.3) | 7 981 | 7 896 | 7 748 |
| 160 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 7 803 | 7 920 | 7 958 |
| 161 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 6 888 | 6 603 | 6 290 |
| 162 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 6 787 | 6 679 | 6 670 |
| 163 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 6 071 | 5 943 | 5 990 |
| 164 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 5 792 | 5 717 | 5 735 |
| 165 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 5 773 | 5 687 | 5 708 |
| 166 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 5 685 | 5 636 | 5 658 |
| 167 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.2) | 5 142 | 5 097 | 5 129 |
| 168 | php (7.4)| [ice](https://iceframework.org) (1.5) | 5 006 | 4 976 | 5 016 |
| 169 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.4) | 4 945 | 4 913 | 4 946 |
| 170 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 4 568 | 4 555 | 4 613 |
| 171 | php (7.4)| [slim](https://slimframework.com) (4.5) | 3 819 | 3 843 | 3 895 |
| 172 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 3 692 | 1 734 | 2 287 |
| 173 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 683 | 3 452 | 3 438 |
| 174 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 3 654 | 3 696 | 3 744 |
| 175 | php (7.4)| [lumen](https://lumen.laravel.com) (7.2) | 3 506 | 3 534 | 3 637 |
| 176 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 3 444 | 3 487 | 3 543 |
| 177 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 392 | 8 155 | 6 444 |
| 178 | perl (5.32)| [dancer2](https://perldancer.org) (2.0) | 3 235 | 2 528 | 1 359 |
| 179 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.4) | 3 130 | 3 177 | 3 226 |
| 180 | php (7.4)| [symfony](https://symfony.com) (5.1) | 2 770 | 2 801 | 2 846 |
| 181 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 488 | 2 525 | 2 546 |
| 182 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 416 | 2 416 | 2 412 |
| 183 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 286 | 2 275 | 2 293 |
| 184 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 588 | 1 609 | 1 600 |
| 185 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 523 | 1 487 | 1 468 |
| 186 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 339 | 1 358 | 1 380 |
| 187 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 039 | 1 059 | 1 122 |
| 188 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 586 | 465 | 548 |
| 189 | php (7.4)| [laravel](https://laravel.com) (7.2) | 320 | 159 | 3 655 |
| 190 | crystal (0.35)| [lucky](https://luckyframework.org) (0.23) | 306 | 332 | 323 |

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

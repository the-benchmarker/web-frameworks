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

:information_source:  Updated on **2020-06-16** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | javascript (13.14)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.11) | 199 895 | 214 260 | 215 564 |
| 2 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 191 590 | 203 958 | 204 366 |
| 3 | nim (1.2)| [whip](https://github.com/mattaylor/whip) (0.2) | 180 021 | 192 497 | 193 891 |
| 4 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 176 573 | 188 452 | 189 303 |
| 5 | javascript (13.14)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 167 461 | 183 147 | 186 802 |
| 6 | go (1.14)| [fiber](https://gofiber.io) (1.11) | 164 045 | 168 088 | 167 679 |
| 7 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.2) | 163 680 | 174 579 | 180 022 |
| 8 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 161 303 | 171 562 | 176 523 |
| 9 | java (8)| [jooby](https://jooby.io) (2.8) | 161 107 | 181 246 | 182 946 |
| 10 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 160 505 | 175 816 | 175 279 |
| 11 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.14) | 160 450 | 170 516 | 174 446 |
| 12 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.3) | 158 582 | 170 316 | 174 459 |
| 13 | kotlin (1.3)| [kooby](https://jooby.io) (2.8) | 157 774 | 188 062 | 191 579 |
| 14 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 157 362 | 169 052 | 169 045 |
| 15 | crystal (0.34)| [toro](https://github.com/soveran/toro) (0.4) | 156 739 | 164 535 | 161 158 |
| 16 | go (1.14)| [gearbox](https://gogearbox.com) (1.0) | 156 530 | 167 329 | 173 435 |
| 17 | crystal (0.34)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 156 418 | 162 712 | 159 456 |
| 18 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 153 728 | 162 893 | 166 131 |
| 19 | crystal (0.34)| [spider-gazelle](https://spider-gazelle.net) (3.0) | 152 025 | 157 831 | 154 430 |
| 20 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 145 805 | 157 027 | 157 988 |
| 21 | crystal (0.34)| [kemal](https://kemalcr.com) (0.26) | 145 614 | 151 077 | 147 678 |
| 22 | crystal (0.34)| [amber](https://amberframework.org) (0.34) | 135 933 | 141 347 | 137 790 |
| 23 | rust (1.44)| [actix](https://actix.rs) (2.0) | 135 383 | 140 147 | 139 326 |
| 24 | crystal (0.34)| [orion](https://github.com/obsidian/orion) (2.3) | 132 377 | 136 200 | 130 835 |
| 25 | crystal (0.34)| [athena](https://github.com/athena-framework/athena) (0.8) | 126 044 | 126 863 | 118 547 |
| 26 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 124 597 | 189 448 | 191 830 |
| 27 | java (8)| [act](https://actframework.org) (1.8) | 120 072 | 132 208 | 132 621 |
| 28 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 111 019 | 111 650 | 116 164 |
| 29 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 110 789 | 114 660 | 119 259 |
| 30 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 110 283 | 108 847 | 113 787 |
| 31 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 108 317 | 107 626 | 110 789 |
| 32 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 108 262 | 112 702 | 116 689 |
| 33 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 108 140 | 107 013 | 110 847 |
| 34 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 105 602 | 106 430 | 112 298 |
| 35 | go (1.14)| [violetear](https://violetear.org) (7.0) | 105 299 | 105 296 | 109 962 |
| 36 | c (99)| [kore](https://kore.io) (3.3) | 105 219 | 125 586 | 125 437 |
| 37 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 104 830 | 103 043 | 107 454 |
| 38 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 102 427 | 107 677 | 110 997 |
| 39 | fsharp (4.7)| [frank](https://github.com/frank-fs/frank) (6.1) | 101 903 | 107 266 | 109 009 |
| 40 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 101 595 | 108 504 | 109 700 |
| 41 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 101 001 | 98 744 | 103 568 |
| 42 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 99 481 | 98 321 | 101 629 |
| 43 | go (1.14)| [beego](https://beego.me) (1.12) | 97 685 | 98 419 | 100 909 |
| 44 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 97 369 | 122 230 | 124 819 |
| 45 | javascript (13.14)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 95 391 | 104 842 | 105 796 |
| 46 | fsharp (4.7)| [falco](https://github.com/pimbrouwers/Falco) (1.1) | 90 137 | 96 479 | 96 906 |
| 47 | fsharp (4.7)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 88 360 | 91 821 | 90 409 |
| 48 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 86 437 | 93 402 | 92 517 |
| 49 | javascript (13.14)| [0http](https://github.com/jkyberneees/0http) (2.5) | 86 383 | 93 669 | 94 434 |
| 50 | javascript (13.14)| [restana](https://github.com/jkyberneees/ana) (4.6) | 85 269 | 93 978 | 93 772 |
| 51 | javascript (13.14)| [polka](https://github.com/lukeed/polka) (0.5) | 85 151 | 89 929 | 88 904 |
| 52 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.1) | 84 023 | 81 168 | 71 501 |
| 53 | go (1.14)| [air](https://github.com/aofei/air) (0.17) | 83 526 | 84 284 | 86 452 |
| 54 | javascript (13.14)| [rayo](https://rayo.js.org) (1.3) | 79 779 | 85 167 | 82 773 |
| 55 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 78 524 | 80 277 | 75 488 |
| 56 | go (1.14)| [gf](https://goframe.org) (1.13) | 76 158 | 80 861 | 82 423 |
| 57 | swift (5.2)| [perfect](https://perfect.org) (3.1) | 75 317 | 83 956 | 88 744 |
| 58 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 75 308 | 85 576 | 79 388 |
| 59 | java (8)| [javalin](https://javalin.io) (3.8) | 74 959 | 79 821 | 79 392 |
| 60 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 71 956 | 75 276 | 76 080 |
| 61 | javascript (13.14)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 70 234 | 74 234 | 73 139 |
| 62 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 69 477 | 82 612 | 83 767 |
| 63 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 69 323 | 75 148 | 77 402 |
| 64 | javascript (13.14)| [fastify](https://fastify.io) (2.14) | 69 129 | 72 699 | 70 739 |
| 65 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 68 717 | 68 049 | 71 471 |
| 66 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 65 635 | 71 084 | 70 933 |
| 67 | javascript (13.14)| [koa](https://koajs.com) (2.12) | 64 103 | 66 832 | 66 065 |
| 68 | php (7.4)| [hyperf](https://www.hyperf.io) (1.1) | 63 885 | 68 370 | 68 944 |
| 69 | javascript (13.14)| [nestjs-fastify](https://nestjs.com) (7.1) | 63 571 | 68 038 | 66 397 |
| 70 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 62 952 | 65 035 | 68 185 |
| 71 | java (8)| [micronaut](https://micronaut.io) (1.2) | 62 226 | 70 364 | 70 239 |
| 72 | fsharp (4.7)| [websharper](https://websharper.com) (4.6) | 62 092 | 61 725 | 58 017 |
| 73 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 62 034 | 67 370 | 67 470 |
| 74 | javascript (13.14)| [foxify](https://foxify.js.org) (0.1) | 62 008 | 68 018 | 67 886 |
| 75 | javascript (13.14)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 60 970 | 64 016 | 62 140 |
| 76 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 58 789 | 61 797 | 61 755 |
| 77 | javascript (13.14)| [express](https://expressjs.com) (4.17) | 55 391 | 57 774 | 57 062 |
| 78 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 55 329 | 63 242 | 62 912 |
| 79 | swift (5.2)| [kitura](https://kitura.io) (2.9) | 55 019 | 55 798 | 55 371 |
| 80 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 54 729 | 54 393 | 54 218 |
| 81 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 54 495 | 55 991 | 55 671 |
| 82 | rust (1.44)| [nickel](https://nickel-org.github.io) (0.11) | 54 271 | 53 575 | 53 412 |
| 83 | javascript (13.14)| [feathersjs](https://feathersjs.com) (4.5) | 53 421 | 55 532 | 54 911 |
| 84 | rust (1.44)| [gotham](https://gotham.rs) (0.4) | 51 719 | 58 315 | 60 331 |
| 85 | javascript (13.14)| [moleculer](https://moleculer.services) (0.14) | 51 114 | 53 748 | 53 405 |
| 86 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 51 077 | 57 448 | 57 178 |
| 87 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.26) | 50 086 | 59 171 | 59 044 |
| 88 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 49 374 | 51 167 | 50 779 |
| 89 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 49 299 | 48 937 | 49 523 |
| 90 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 48 909 | 53 406 | 53 227 |
| 91 | swift (5.2)| [vapor](https://vapor.codes) (4.8) | 47 387 | 48 682 | 48 513 |
| 92 | python (3.8)| [hug](https://hug.rest) (2.6) | 46 472 | 47 735 | 47 763 |
| 93 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 46 281 | 46 438 | 46 442 |
| 94 | php (7.4)| [imi](https://imiphp.com) (1.2) | 46 192 | 48 160 | 48 436 |
| 95 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.3) | 45 578 | 49 167 | 48 495 |
| 96 | javascript (13.14)| [hapi](https://hapijs.com) (19.1) | 45 333 | 47 683 | 47 016 |
| 97 | python (3.8)| [starlette](https://starlette.io) (0.13) | 43 596 | 47 509 | 47 044 |
| 98 | javascript (13.14)| [nestjs-express](https://nestjs.com) (7.1) | 43 085 | 45 023 | 43 927 |
| 99 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 42 321 | 46 616 | 46 884 |
| 100 | javascript (13.14)| [restify](https://restify.com) (8.5) | 41 898 | 44 242 | 44 001 |
| 101 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 39 267 | 41 732 | 40 808 |
| 102 | scala (2.13)| [play](https://playframework.com) (2.8) | 38 962 | 41 207 | 40 384 |
| 103 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 38 439 | 38 780 | 36 608 |
| 104 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 38 276 | 40 273 | 40 121 |
| 105 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 38 081 | 40 162 | 38 403 |
| 106 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 37 671 | 41 034 | 40 731 |
| 107 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.32) | 37 380 | 38 143 | 36 413 |
| 108 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 35 513 | 35 658 | 35 074 |
| 109 | php (7.4)| [swoft](https://swoft.org) (2.0) | 35 322 | 36 493 | 36 299 |
| 110 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 34 594 | 35 651 | 35 576 |
| 111 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 34 278 | 34 045 | 32 907 |
| 112 | fsharp (4.7)| [saturn](https://saturnframework.org) (0.13) | 34 261 | 31 162 | 28 688 |
| 113 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 33 211 | 34 686 | 34 572 |
| 114 | dart (2.8)| [aqueduct](https://aqueduct.io) (3.3) | 32 129 | 31 986 | 31 637 |
| 115 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 31 812 | 31 453 | 34 289 |
| 116 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 30 130 | 29 333 | 28 302 |
| 117 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.58) | 29 007 | 30 678 | 30 591 |
| 118 | nim (1.2)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 28 528 | 24 977 | 22 455 |
| 119 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 28 317 | 28 018 | 27 173 |
| 120 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 28 209 | 28 266 | 27 062 |
| 121 | python (3.8)| [responder](https://python-responder.org) (2.0) | 27 002 | 30 941 | 30 613 |
| 122 | go (1.14)| [macaron](https://go-macaron.com) (1.3) | 26 198 | 28 777 | 28 840 |
| 123 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 26 174 | 26 946 | 26 642 |
| 124 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 25 237 | 24 713 | 23 849 |
| 125 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 24 870 | 25 694 | 25 509 |
| 126 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 24 602 | 27 199 | 27 172 |
| 127 | rust (1.44)| [iron](https://ironframework.io) (0.6) | 23 874 | 24 125 | 23 993 |
| 128 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 23 498 | 24 021 | 23 973 |
| 129 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 23 368 | 23 977 | 23 729 |
| 130 | javascript (13.14)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 450 | 22 212 | 20 725 |
| 131 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 695 | 18 123 | 18 284 |
| 132 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 17 522 | 17 742 | 17 852 |
| 133 | java (8)| [blade](https://lets-blade.com) (2.0) | 16 161 | 20 431 | 20 093 |
| 134 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 16 115 | 15 890 | 15 796 |
| 135 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 15 049 | 15 416 | 15 407 |
| 136 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 14 367 | 14 712 | 14 792 |
| 137 | dart (2.8)| [start](https://github.com/lvivski/start) (0.4) | 13 929 | 13 660 | 12 736 |
| 138 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 13 885 | 15 228 | 15 266 |
| 139 | javascript (13.14)| [sails](https://sailsjs.com) (1.2) | 13 499 | 13 881 | 13 745 |
| 140 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 11 787 | 11 600 | 11 497 |
| 141 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.12) | 11 647 | 11 802 | 11 021 |
| 142 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.2) | 11 507 | 11 343 | 11 208 |
| 143 | swift (5.2)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 039 | 11 059 | 11 024 |
| 144 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 10 991 | 10 566 | 9 995 |
| 145 | pony (0.35)| [jennet](https://github.com/Theodus/jennet) (0.1) | 10 905 | 17 742 | 15 674 |
| 146 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 10 156 | 10 195 | 10 079 |
| 147 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 768 | 9 713 | 9 643 |
| 148 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 307 | 9 303 | 9 239 |
| 149 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 276 | 9 180 | 9 113 |
| 150 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 7 995 | 8 060 | 8 058 |
| 151 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 7 754 | 7 799 | 7 837 |
| 152 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 754 | 7 570 | 7 169 |
| 153 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 652 | 7 658 | 7 724 |
| 154 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 563 | 7 651 | 7 570 |
| 155 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 434 | 6 479 | 6 501 |
| 156 | crystal (0.34)| [onyx](https://onyxframework.org) (0.5) | 5 972 | 6 097 | 6 097 |
| 157 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 5 705 | 5 763 | 5 700 |
| 158 | php (7.4)| [slim](https://slimframework.com) (4.5) | 4 598 | 4 701 | 4 765 |
| 159 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 4 369 | 4 425 | 4 551 |
| 160 | php (7.4)| [lumen](https://lumen.laravel.com) (7.1) | 4 303 | 4 367 | 4 430 |
| 161 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 166 | 4 262 | 4 326 |
| 162 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 4 039 | 3 820 | 3 811 |
| 163 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 335 | 7 878 | 6 002 |
| 164 | php (7.4)| [symfony](https://symfony.com) (5.1) | 3 193 | 3 244 | 3 292 |
| 165 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 2 855 | 1 574 | 1 987 |
| 166 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 843 | 2 922 | 2 944 |
| 167 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 2 449 | 1 342 | 780 |
| 168 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 423 | 2 448 | 2 422 |
| 169 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 589 | 1 613 | 1 597 |
| 170 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 520 | 1 481 | 1 463 |
| 171 | crystal (0.34)| [lucky](https://luckyframework.org) (0.21) | 1 437 | 1 469 | 1 464 |
| 172 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 428 | 1 457 | 1 466 |
| 173 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 842 | 459 | 1 390 |
| 174 | php (7.4)| [laravel](https://laravel.com) (7.15) | 276 | 153 | 3 211 |

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

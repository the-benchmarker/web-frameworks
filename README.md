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

:information_source:  Updated on **2020-06-23** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 190 437 | 204 285 | 204 376 |
| 2 | javascript (13.14)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.11) | 189 820 | 204 329 | 204 141 |
| 3 | nim (1.2)| [whip](https://github.com/mattaylor/whip) (0.2) | 178 650 | 190 743 | 190 855 |
| 4 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 175 070 | 185 630 | 189 379 |
| 5 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.0) | 174 702 | 188 286 | 188 773 |
| 6 | java (8)| [jooby](https://jooby.io) (2.8) | 167 672 | 187 682 | 190 320 |
| 7 | javascript (13.14)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 166 459 | 180 161 | 180 570 |
| 8 | kotlin (1.3)| [kooby](https://jooby.io) (2.8) | 165 243 | 190 132 | 193 087 |
| 9 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 165 128 | 175 651 | 181 369 |
| 10 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.14) | 163 621 | 172 722 | 176 728 |
| 11 | go (1.14)| [fiber](https://gofiber.io) (1.12) | 163 202 | 166 204 | 164 157 |
| 12 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 161 843 | 177 153 | 178 070 |
| 13 | crystal (0.34)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 158 275 | 165 623 | 162 069 |
| 14 | crystal (0.34)| [toro](https://github.com/soveran/toro) (0.4) | 157 483 | 166 150 | 162 764 |
| 15 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.2) | 157 045 | 165 800 | 169 897 |
| 16 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 155 837 | 164 971 | 168 569 |
| 17 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.3) | 155 483 | 168 986 | 175 211 |
| 18 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 155 337 | 166 182 | 167 751 |
| 19 | go (1.14)| [gearbox](https://gogearbox.com) (1.0) | 154 161 | 162 931 | 166 466 |
| 20 | crystal (0.34)| [spider-gazelle](https://spider-gazelle.net) (3.0) | 153 971 | 160 537 | 156 467 |
| 21 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 146 596 | 157 282 | 157 526 |
| 22 | crystal (0.34)| [kemal](https://kemalcr.com) (0.26) | 144 125 | 149 925 | 146 405 |
| 23 | crystal (0.34)| [amber](https://amberframework.org) (0.34) | 136 743 | 140 756 | 137 973 |
| 24 | rust (1.44)| [actix](https://actix.rs) (2.0) | 135 452 | 139 487 | 139 790 |
| 25 | crystal (0.34)| [orion](https://github.com/obsidian/orion) (2.3) | 134 172 | 137 612 | 132 625 |
| 26 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 127 112 | 157 538 | 184 956 |
| 27 | crystal (0.34)| [athena](https://github.com/athena-framework/athena) (0.8) | 126 909 | 128 422 | 120 901 |
| 28 | java (8)| [act](https://actframework.org) (1.8) | 122 397 | 135 204 | 135 478 |
| 29 | go (1.14)| [clevergo](https://clevergo.tech) (0.3) | 116 825 | 117 304 | 122 282 |
| 30 | c (99)| [kore](https://kore.io) (3.3) | 116 666 | 97 308 | 124 052 |
| 31 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 112 293 | 112 540 | 116 324 |
| 32 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 111 468 | 112 453 | 117 133 |
| 33 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 109 507 | 108 690 | 112 198 |
| 34 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 106 263 | 109 177 | 112 788 |
| 35 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 106 062 | 113 022 | 114 095 |
| 36 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 105 087 | 102 777 | 106 550 |
| 37 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 104 408 | 108 027 | 110 371 |
| 38 | fsharp (4.7)| [frank](https://github.com/frank-fs/frank) (6.1) | 103 425 | 110 641 | 111 734 |
| 39 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 103 300 | 103 520 | 107 655 |
| 40 | go (1.14)| [violetear](https://violetear.org) (7.0) | 102 102 | 99 037 | 103 466 |
| 41 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 100 705 | 98 301 | 101 255 |
| 42 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 100 180 | 102 981 | 109 045 |
| 43 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 98 831 | 103 253 | 104 685 |
| 44 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 98 525 | 95 532 | 98 244 |
| 45 | go (1.14)| [beego](https://beego.me) (1.12) | 98 099 | 100 130 | 103 907 |
| 46 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 96 624 | 102 045 | 103 324 |
| 47 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 96 217 | 115 390 | 119 456 |
| 48 | php (7.4)| [hyperf](https://www.hyperf.io) (1.1) | 93 145 | 100 366 | 102 041 |
| 49 | fsharp (4.7)| [falco](https://github.com/pimbrouwers/Falco) (1.1) | 91 705 | 98 441 | 98 937 |
| 50 | javascript (13.14)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 91 054 | 99 835 | 99 855 |
| 51 | fsharp (4.7)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 89 362 | 93 397 | 91 820 |
| 52 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 86 249 | 92 329 | 93 563 |
| 53 | go (1.14)| [air](https://github.com/aofei/air) (0.17) | 86 108 | 87 237 | 89 343 |
| 54 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.1) | 85 310 | 82 449 | 73 182 |
| 55 | javascript (13.14)| [0http](https://github.com/jkyberneees/0http) (2.5) | 82 311 | 93 207 | 93 473 |
| 56 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 81 419 | 81 964 | 79 016 |
| 57 | javascript (13.14)| [polka](https://github.com/lukeed/polka) (0.5) | 81 180 | 85 118 | 83 397 |
| 58 | javascript (13.14)| [restana](https://github.com/jkyberneees/ana) (4.6) | 80 369 | 88 212 | 87 544 |
| 59 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 78 269 | 91 547 | 92 293 |
| 60 | go (1.14)| [gf](https://goframe.org) (1.13) | 78 217 | 84 215 | 85 776 |
| 61 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 76 644 | 83 276 | 82 519 |
| 62 | javascript (13.14)| [rayo](https://rayo.js.org) (1.3) | 76 522 | 82 733 | 81 750 |
| 63 | java (8)| [javalin](https://javalin.io) (3.9) | 74 464 | 79 134 | 78 772 |
| 64 | swift (5.2)| [perfect](https://perfect.org) (3.1) | 73 920 | 84 733 | 90 914 |
| 65 | fsharp (4.7)| [saturn](https://saturnframework.org) (0.14) | 73 217 | 66 806 | 58 820 |
| 66 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 72 420 | 76 627 | 76 962 |
| 67 | javascript (13.14)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 69 602 | 73 708 | 72 778 |
| 68 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 68 314 | 78 160 | 80 763 |
| 69 | javascript (13.14)| [foxify](https://foxify.js.org) (0.1) | 66 444 | 70 020 | 67 644 |
| 70 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 66 214 | 73 185 | 72 479 |
| 71 | javascript (13.14)| [fastify](https://fastify.io) (2.15) | 65 357 | 69 390 | 66 330 |
| 72 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 64 596 | 66 072 | 70 218 |
| 73 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 64 397 | 63 689 | 66 603 |
| 74 | java (8)| [micronaut](https://micronaut.io) (1.2) | 64 276 | 70 806 | 70 712 |
| 75 | fsharp (4.7)| [websharper](https://websharper.com) (4.6) | 62 087 | 63 473 | 59 167 |
| 76 | javascript (13.14)| [nestjs-fastify](https://nestjs.com) (7.2) | 59 449 | 64 998 | 63 455 |
| 77 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 59 040 | 62 382 | 62 428 |
| 78 | javascript (13.14)| [koa](https://koajs.com) (2.13) | 58 742 | 61 431 | 60 038 |
| 79 | javascript (13.14)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 57 581 | 60 110 | 57 998 |
| 80 | rust (1.44)| [nickel](https://nickel-org.github.io) (0.11) | 56 711 | 55 778 | 55 138 |
| 81 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.26) | 56 304 | 61 173 | 61 160 |
| 82 | swift (5.2)| [kitura](https://kitura.io) (2.9) | 56 251 | 56 515 | 56 229 |
| 83 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 55 623 | 63 479 | 63 383 |
| 84 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 54 307 | 54 564 | 54 459 |
| 85 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 53 974 | 55 377 | 55 260 |
| 86 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 53 228 | 57 626 | 57 266 |
| 87 | javascript (13.14)| [express](https://expressjs.com) (4.17) | 52 470 | 54 568 | 53 362 |
| 88 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 51 542 | 54 146 | 51 710 |
| 89 | rust (1.44)| [gotham](https://gotham.rs) (0.4) | 51 054 | 59 444 | 60 906 |
| 90 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 50 095 | 50 173 | 50 317 |
| 91 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 49 613 | 51 352 | 51 542 |
| 92 | javascript (13.14)| [feathersjs](https://feathersjs.com) (4.5) | 49 231 | 53 096 | 51 724 |
| 93 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 48 661 | 54 007 | 53 948 |
| 94 | javascript (13.14)| [moleculer](https://moleculer.services) (0.14) | 48 152 | 49 722 | 48 691 |
| 95 | swift (5.2)| [vapor](https://vapor.codes) (4.10) | 47 553 | 49 282 | 48 977 |
| 96 | php (7.4)| [imi](https://imiphp.com) (1.2) | 47 231 | 50 105 | 50 164 |
| 97 | python (3.8)| [hug](https://hug.rest) (2.6) | 46 677 | 48 779 | 48 701 |
| 98 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 45 601 | 45 520 | 45 409 |
| 99 | python (3.8)| [starlette](https://starlette.io) (0.13) | 44 756 | 48 511 | 48 398 |
| 100 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.3) | 44 714 | 49 724 | 49 232 |
| 101 | php (7.4)| [swoft](https://swoft.org) (2.0) | 42 612 | 45 187 | 45 115 |
| 102 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 42 533 | 42 940 | 43 027 |
| 103 | javascript (13.14)| [hapi](https://hapijs.com) (19.1) | 42 172 | 43 858 | 43 284 |
| 104 | javascript (13.14)| [nestjs-express](https://nestjs.com) (7.2) | 41 866 | 41 184 | 40 937 |
| 105 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 41 287 | 44 274 | 44 198 |
| 106 | scala (2.13)| [play](https://playframework.com) (2.8) | 39 737 | 39 818 | 39 777 |
| 107 | javascript (13.14)| [restify](https://restify.com) (8.5) | 39 173 | 40 714 | 40 016 |
| 108 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 38 640 | 40 987 | 40 636 |
| 109 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 38 611 | 40 300 | 40 502 |
| 110 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.1) | 38 064 | 37 887 | 37 227 |
| 111 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 37 481 | 35 988 | 35 135 |
| 112 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 36 164 | 36 715 | 36 476 |
| 113 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.33) | 35 643 | 36 056 | 34 944 |
| 114 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 35 489 | 36 838 | 37 586 |
| 115 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 33 703 | 33 614 | 35 162 |
| 116 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 33 638 | 35 017 | 34 666 |
| 117 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 32 873 | 32 717 | 31 791 |
| 118 | dart (2.8)| [aqueduct](https://aqueduct.io) (3.3) | 32 329 | 32 224 | 31 697 |
| 119 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 30 728 | 30 141 | 28 531 |
| 120 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.58) | 29 062 | 31 115 | 30 808 |
| 121 | python (3.8)| [responder](https://python-responder.org) (2.0) | 28 795 | 30 827 | 30 406 |
| 122 | nim (1.2)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 27 811 | 23 945 | 21 895 |
| 123 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 27 472 | 27 333 | 27 352 |
| 124 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 26 947 | 26 990 | 26 288 |
| 125 | nim (1.2)| [akane](https://github.com/Ethosa/akane) (0.1) | 26 889 | 23 287 | 21 120 |
| 126 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 26 341 | 27 088 | 26 820 |
| 127 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 25 429 | 26 023 | 25 902 |
| 128 | go (1.14)| [macaron](https://go-macaron.com) (1.3) | 25 392 | 27 849 | 28 217 |
| 129 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 24 989 | 27 934 | 27 951 |
| 130 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 24 243 | 24 527 | 22 725 |
| 131 | rust (1.44)| [iron](https://ironframework.io) (0.6) | 24 015 | 24 005 | 23 919 |
| 132 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 23 675 | 24 092 | 24 033 |
| 133 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 23 600 | 24 095 | 24 072 |
| 134 | javascript (13.14)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 21 640 | 20 906 | 20 059 |
| 135 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 861 | 18 173 | 18 439 |
| 136 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 17 428 | 17 643 | 17 650 |
| 137 | java (8)| [blade](https://lets-blade.com) (2.0) | 17 110 | 20 640 | 19 751 |
| 138 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 14 937 | 14 761 | 15 065 |
| 139 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 14 711 | 15 080 | 14 977 |
| 140 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 14 262 | 15 423 | 15 609 |
| 141 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 14 258 | 14 448 | 14 380 |
| 142 | dart (2.8)| [start](https://github.com/lvivski/start) (0.4) | 13 929 | 13 672 | 12 760 |
| 143 | javascript (13.14)| [sails](https://sailsjs.com) (1.2) | 12 559 | 12 985 | 12 763 |
| 144 | crystal (0.34)| [runcobo](https://github.com/runcobo/runcobo) (0.2) | 12 300 | 12 152 | 11 641 |
| 145 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 11 643 | 11 013 | 10 909 |
| 146 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.2) | 11 531 | 11 384 | 11 224 |
| 147 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.12) | 11 478 | 12 026 | 11 222 |
| 148 | swift (5.2)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 043 | 11 119 | 11 093 |
| 149 | pony (0.35)| [jennet](https://github.com/Theodus/jennet) (0.1) | 10 836 | 17 897 | 15 998 |
| 150 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 10 717 | 10 051 | 10 861 |
| 151 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 932 | 9 750 | 9 674 |
| 152 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 513 | 10 147 | 10 031 |
| 153 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 409 | 9 410 | 9 380 |
| 154 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 263 | 9 136 | 9 176 |
| 155 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 8 047 | 8 040 | 8 050 |
| 156 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 900 | 7 607 | 7 207 |
| 157 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 7 773 | 7 805 | 7 806 |
| 158 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 670 | 7 692 | 7 733 |
| 159 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 663 | 7 685 | 7 731 |
| 160 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 484 | 6 547 | 6 593 |
| 161 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 5 734 | 5 764 | 5 859 |
| 162 | crystal (0.34)| [onyx](https://onyxframework.org) (0.5) | 5 706 | 5 852 | 5 904 |
| 163 | php (7.4)| [slim](https://slimframework.com) (4.5) | 4 708 | 4 809 | 4 899 |
| 164 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 4 403 | 4 460 | 4 613 |
| 165 | php (7.4)| [lumen](https://lumen.laravel.com) (7.1) | 4 336 | 4 336 | 4 424 |
| 166 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 191 | 4 278 | 4 385 |
| 167 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 561 | 3 454 | 3 464 |
| 168 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 239 | 7 699 | 5 980 |
| 169 | php (7.4)| [symfony](https://symfony.com) (5.1) | 3 177 | 3 209 | 3 262 |
| 170 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 2 910 | 2 103 | 1 854 |
| 171 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 851 | 2 920 | 2 986 |
| 172 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 424 | 2 426 | 2 424 |
| 173 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 201 | 2 227 | 2 235 |
| 174 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 1 660 | 1 765 | 797 |
| 175 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 635 | 1 649 | 1 624 |
| 176 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 556 | 1 511 | 1 498 |
| 177 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 438 | 1 466 | 1 474 |
| 178 | crystal (0.34)| [lucky](https://luckyframework.org) (0.21) | 1 432 | 1 450 | 1 451 |
| 179 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 614 | 504 | 1 679 |
| 180 | php (7.4)| [laravel](https://laravel.com) (7.16) | 283 | 165 | 2 257 |

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

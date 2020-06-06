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

:information_source:  Updated on **2020-06-06** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | javascript (13.14)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.11) | 199 901 | 213 969 | 214 803 |
| 2 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 197 814 | 210 382 | 211 857 |
| 3 | javascript (13.14)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.0) | 188 415 | 202 026 | 202 581 |
| 4 | php (7.4)| [simps](https://simps.io) (1.0) | 172 397 | 184 654 | 191 060 |
| 5 | javascript (13.14)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 166 586 | 183 989 | 187 319 |
| 6 | crystal (0.34)| [spider-gazelle](https://spider-gazelle.net) (3.0) | 163 583 | 171 148 | 169 162 |
| 7 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.14) | 160 476 | 169 997 | 173 322 |
| 8 | crystal (0.34)| [toro](https://github.com/soveran/toro) (0.4) | 160 198 | 168 290 | 164 984 |
| 9 | go (1.14)| [fiber](https://gofiber.io) (1.10) | 159 882 | 164 263 | 161 658 |
| 10 | kotlin (1.3)| [kooby](https://jooby.io) (2.8) | 159 746 | 190 308 | 195 677 |
| 11 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 158 132 | 177 749 | 172 808 |
| 12 | crystal (0.34)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 156 834 | 164 635 | 160 128 |
| 13 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.3) | 155 274 | 156 741 | 163 290 |
| 14 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 155 048 | 165 862 | 166 827 |
| 15 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.1) | 154 753 | 164 539 | 167 028 |
| 16 | java (8)| [jooby](https://jooby.io) (2.8) | 154 117 | 173 391 | 176 600 |
| 17 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 152 453 | 161 777 | 164 422 |
| 18 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 150 316 | 160 470 | 162 235 |
| 19 | crystal (0.34)| [grip](https://github.com/grip-framework/grip) (0.28) | 149 410 | 155 889 | 153 573 |
| 20 | crystal (0.34)| [kemal](https://kemalcr.com) (0.26) | 146 051 | 152 254 | 149 412 |
| 21 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 141 205 | 153 861 | 153 491 |
| 22 | crystal (0.34)| [orion](https://github.com/obsidian/orion) (2.3) | 136 565 | 139 912 | 134 946 |
| 23 | crystal (0.34)| [amber](https://amberframework.org) (0.34) | 135 721 | 140 193 | 135 989 |
| 24 | crystal (0.34)| [athena](https://github.com/athena-framework/athena) (0.8) | 133 342 | 130 716 | 124 876 |
| 25 | rust (1.44)| [actix](https://actix.rs) (2.0) | 131 809 | 135 797 | 136 658 |
| 26 | java (8)| [act](https://actframework.org) (1.8) | 128 885 | 139 589 | 139 991 |
| 27 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 108 275 | 195 168 | 196 630 |
| 28 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 108 001 | 106 968 | 110 838 |
| 29 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 106 864 | 106 286 | 109 459 |
| 30 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 103 787 | 107 268 | 109 524 |
| 31 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 103 348 | 99 423 | 109 387 |
| 32 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 102 626 | 101 065 | 104 219 |
| 33 | go (1.14)| [violetear](https://violetear.org) (7.0) | 102 604 | 99 505 | 106 646 |
| 34 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 101 585 | 108 628 | 111 856 |
| 35 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 99 452 | 100 532 | 103 328 |
| 36 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 98 375 | 95 562 | 98 525 |
| 37 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 97 495 | 121 808 | 126 133 |
| 38 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 96 965 | 101 123 | 102 435 |
| 39 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (4.0) | 96 347 | 95 217 | 97 963 |
| 40 | go (1.14)| [beego](https://beego.me) (1.12) | 95 103 | 97 395 | 100 149 |
| 41 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 93 507 | 89 679 | 94 997 |
| 42 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 92 375 | 93 430 | 94 668 |
| 43 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 88 921 | 92 717 | 95 976 |
| 44 | javascript (13.14)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 88 125 | 97 288 | 98 223 |
| 45 | javascript (13.14)| [restana](https://github.com/jkyberneees/ana) (4.6) | 85 170 | 93 533 | 94 209 |
| 46 | javascript (13.14)| [rayo](https://rayo.js.org) (1.3) | 82 832 | 87 515 | 86 447 |
| 47 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 82 399 | 85 237 | 82 409 |
| 48 | go (1.14)| [air](https://github.com/aofei/air) (0.16) | 81 623 | 83 876 | 85 045 |
| 49 | c (99)| [kore](https://kore.io) (3.3) | 78 445 | 127 316 | 145 532 |
| 50 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 77 299 | 85 436 | 82 019 |
| 51 | javascript (13.14)| [polka](https://github.com/lukeed/polka) (0.5) | 76 735 | 82 981 | 80 336 |
| 52 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 74 758 | 78 630 | 79 420 |
| 53 | swift (5.2)| [perfect](https://perfect.org) (3.1) | 74 742 | 83 078 | 88 853 |
| 54 | go (1.14)| [gf](https://goframe.org) (1.13) | 74 001 | 79 051 | 80 227 |
| 55 | javascript (13.14)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 73 946 | 78 995 | 76 925 |
| 56 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 72 234 | 85 937 | 86 093 |
| 57 | javascript (13.14)| [0http](https://github.com/jkyberneees/0http) (2.5) | 71 083 | 89 049 | 90 174 |
| 58 | java (8)| [javalin](https://javalin.io) (3.8) | 70 015 | 74 946 | 75 926 |
| 59 | javascript (13.14)| [fastify](https://fastify.io) (2.14) | 69 486 | 73 913 | 71 660 |
| 60 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 68 403 | 76 529 | 79 175 |
| 61 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.2) | 68 358 | 68 834 | 68 942 |
| 62 | javascript (13.14)| [foxify](https://foxify.js.org) (0.1) | 65 900 | 70 557 | 70 453 |
| 63 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 63 277 | 62 553 | 65 152 |
| 64 | php (7.4)| [hyperf](https://www.hyperf.io) (1.1) | 63 202 | 69 750 | 67 485 |
| 65 | javascript (13.14)| [nestjs-fastify](https://nestjs.com) (7.1) | 62 086 | 68 434 | 67 443 |
| 66 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 61 783 | 62 977 | 67 327 |
| 67 | java (8)| [micronaut](https://micronaut.io) (1.2) | 57 872 | 64 136 | 66 028 |
| 68 | javascript (13.14)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 56 772 | 60 177 | 61 478 |
| 69 | rust (1.44)| [nickel](https://nickel-org.github.io) (0.11) | 56 367 | 55 797 | 56 089 |
| 70 | javascript (13.14)| [koa](https://koajs.com) (2.12) | 56 167 | 59 468 | 60 908 |
| 71 | javascript (13.14)| [express](https://expressjs.com) (4.17) | 55 479 | 57 946 | 56 981 |
| 72 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 55 468 | 58 349 | 58 342 |
| 73 | javascript (13.14)| [feathersjs](https://feathersjs.com) (4.5) | 55 287 | 57 477 | 55 989 |
| 74 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 55 037 | 57 281 | 57 927 |
| 75 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 54 833 | 55 337 | 54 699 |
| 76 | swift (5.2)| [kitura](https://kitura.io) (2.9) | 54 287 | 53 775 | 53 388 |
| 77 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 54 034 | 55 496 | 54 293 |
| 78 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 53 513 | 56 097 | 54 553 |
| 79 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 53 075 | 57 840 | 59 559 |
| 80 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 52 677 | 53 331 | 53 173 |
| 81 | rust (1.44)| [gotham](https://gotham.rs) (0.4) | 52 509 | 58 849 | 60 660 |
| 82 | javascript (13.14)| [moleculer](https://moleculer.services) (0.14) | 51 510 | 53 419 | 52 353 |
| 83 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.26) | 51 238 | 57 193 | 56 837 |
| 84 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 49 440 | 50 855 | 51 757 |
| 85 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 48 683 | 53 251 | 52 860 |
| 86 | swift (5.2)| [vapor](https://vapor.codes) (4.8) | 48 378 | 49 752 | 49 422 |
| 87 | php (7.4)| [imi](https://imiphp.com) (1.2) | 47 293 | 50 988 | 51 732 |
| 88 | python (3.8)| [starlette](https://starlette.io) (0.13) | 45 876 | 50 506 | 50 216 |
| 89 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.3) | 44 741 | 49 374 | 46 884 |
| 90 | javascript (13.14)| [hapi](https://hapijs.com) (19.1) | 44 730 | 47 502 | 46 402 |
| 91 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 44 518 | 44 706 | 44 780 |
| 92 | python (3.8)| [hug](https://hug.rest) (2.6) | 44 236 | 45 090 | 43 856 |
| 93 | javascript (13.14)| [restify](https://restify.com) (8.5) | 41 869 | 44 342 | 43 982 |
| 94 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 41 423 | 44 618 | 44 063 |
| 95 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 40 366 | 42 659 | 42 759 |
| 96 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 40 327 | 41 243 | 41 265 |
| 97 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 40 207 | 41 359 | 39 298 |
| 98 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 39 466 | 39 881 | 37 670 |
| 99 | scala (2.13)| [play](https://playframework.com) (2.8) | 38 999 | 40 979 | 40 901 |
| 100 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 37 661 | 42 662 | 42 358 |
| 101 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 36 692 | 38 613 | 39 368 |
| 102 | php (7.4)| [swoft](https://swoft.org) (2.0) | 36 198 | 37 631 | 36 087 |
| 103 | javascript (13.14)| [nestjs-express](https://nestjs.com) (7.1) | 35 543 | 36 070 | 36 811 |
| 104 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 34 837 | 35 929 | 35 776 |
| 105 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.32) | 34 698 | 36 877 | 34 926 |
| 106 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 34 162 | 34 338 | 33 468 |
| 107 | dart (2.8)| [aqueduct](https://aqueduct.io) (3.3) | 31 620 | 31 677 | 31 151 |
| 108 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 31 582 | 32 853 | 32 741 |
| 109 | crystal (0.34)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.15) | 30 723 | 28 155 | 25 207 |
| 110 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 30 362 | 28 334 | 28 005 |
| 111 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 27 621 | 27 717 | 27 361 |
| 112 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 27 538 | 27 885 | 26 855 |
| 113 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 27 470 | 28 106 | 28 054 |
| 114 | python (3.8)| [responder](https://python-responder.org) (2.0) | 27 326 | 28 782 | 28 473 |
| 115 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 27 240 | 21 633 | 31 216 |
| 116 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.55) | 26 601 | 28 458 | 28 820 |
| 117 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 26 314 | 27 190 | 26 773 |
| 118 | go (1.14)| [macaron](https://go-macaron.com) (1.3) | 25 160 | 27 538 | 27 818 |
| 119 | rust (1.44)| [iron](https://ironframework.io) (0.6) | 24 186 | 24 241 | 24 178 |
| 120 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 23 939 | 25 645 | 25 529 |
| 121 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 23 084 | 23 767 | 23 930 |
| 122 | javascript (13.14)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 338 | 21 238 | 19 846 |
| 123 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 22 109 | 22 418 | 22 127 |
| 124 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 21 679 | 21 664 | 21 591 |
| 125 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 812 | 18 556 | 18 534 |
| 126 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 17 323 | 17 534 | 17 655 |
| 127 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 15 843 | 15 615 | 15 613 |
| 128 | java (8)| [blade](https://lets-blade.com) (2.0) | 15 677 | 16 875 | 18 774 |
| 129 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 15 672 | 15 510 | 15 674 |
| 130 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 14 541 | 14 828 | 14 868 |
| 131 | dart (2.8)| [start](https://github.com/lvivski/start) (0.4) | 13 934 | 13 692 | 12 847 |
| 132 | javascript (13.14)| [sails](https://sailsjs.com) (1.2) | 13 315 | 13 656 | 13 527 |
| 133 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.12) | 12 750 | 12 665 | 11 067 |
| 134 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 11 964 | 11 749 | 11 061 |
| 135 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 11 485 | 11 389 | 11 229 |
| 136 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.2) | 11 479 | 11 255 | 11 085 |
| 137 | pony (0.35)| [jennet](https://github.com/Theodus/jennet) (0.1) | 10 880 | 17 123 | 15 388 |
| 138 | swift (5.2)| [swifter](https://github.com/httpswift/swifter) (1.4) | 10 828 | 10 844 | 10 849 |
| 139 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 669 | 9 518 | 9 364 |
| 140 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 286 | 9 238 | 9 179 |
| 141 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 081 | 9 394 | 9 498 |
| 142 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 8 647 | 8 600 | 8 476 |
| 143 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 7 961 | 7 935 | 7 683 |
| 144 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 615 | 7 510 | 7 185 |
| 145 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 7 163 | 7 263 | 7 416 |
| 146 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 6 926 | 7 414 | 7 608 |
| 147 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 472 | 6 586 | 6 639 |
| 148 | crystal (0.34)| [onyx](https://onyxframework.org) (0.5) | 5 869 | 6 037 | 6 075 |
| 149 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 5 840 | 5 894 | 6 048 |
| 150 | php (7.4)| [slim](https://slimframework.com) (4.5) | 4 749 | 4 770 | 4 950 |
| 151 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 4 613 | 4 668 | 4 783 |
| 152 | php (7.4)| [lumen](https://lumen.laravel.com) (7.1) | 4 527 | 4 558 | 4 610 |
| 153 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 324 | 4 373 | 4 469 |
| 154 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 4 183 | 3 986 | 3 914 |
| 155 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 343 | 7 945 | 6 114 |
| 156 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 3 196 | 3 503 | 1 626 |
| 157 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 843 | 2 936 | 2 976 |
| 158 | php (7.4)| [symfony](https://symfony.com) (5.1) | 2 806 | 2 985 | 3 100 |
| 159 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 368 | 2 402 | 2 240 |
| 160 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 595 | 1 536 | 1 534 |
| 161 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 536 | 1 531 | 1 556 |
| 162 | crystal (0.34)| [lucky](https://luckyframework.org) (0.21) | 1 466 | 1 504 | 1 499 |
| 163 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 410 | 1 439 | 1 451 |
| 164 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 703 | 494 | 2 140 |
| 165 | php (7.4)| [laravel](https://laravel.com) (7.14) | 613 | 167 | 2 259 |

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

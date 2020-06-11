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

:information_source:  Updated on **2020-06-09** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | javascript (13.14)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.11) | 189 430 | 203 510 | 203 806 |
| 2 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 188 202 | 201 111 | 202 630 |
| 3 | php (7.4)| [simps](https://simps.io) (1.0) | 182 714 | 193 353 | 195 126 |
| 4 | javascript (13.14)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.0) | 178 215 | 191 305 | 191 947 |
| 5 | javascript (13.14)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 177 592 | 190 717 | 190 183 |
| 6 | kotlin (1.3)| [kooby](https://jooby.io) (2.8) | 167 887 | 188 125 | 189 999 |
| 7 | java (8)| [jooby](https://jooby.io) (2.8) | 164 958 | 184 422 | 187 137 |
| 8 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.14) | 163 996 | 173 869 | 177 604 |
| 9 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.3) | 160 711 | 169 474 | 171 901 |
| 10 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 160 630 | 176 516 | 176 459 |
| 11 | go (1.14)| [fiber](https://gofiber.io) (1.11) | 160 131 | 165 519 | 167 631 |
| 12 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.1) | 158 643 | 166 114 | 170 884 |
| 13 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 158 109 | 171 231 | 172 110 |
| 14 | crystal (0.34)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 158 056 | 165 597 | 162 308 |
| 15 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 157 879 | 166 976 | 171 004 |
| 16 | crystal (0.34)| [toro](https://github.com/soveran/toro) (0.4) | 157 664 | 164 912 | 161 384 |
| 17 | crystal (0.34)| [spider-gazelle](https://spider-gazelle.net) (3.0) | 154 999 | 161 746 | 158 450 |
| 18 | crystal (0.34)| [grip](https://github.com/grip-framework/grip) (0.28) | 148 479 | 154 450 | 151 062 |
| 19 | crystal (0.34)| [kemal](https://kemalcr.com) (0.26) | 145 452 | 151 361 | 147 772 |
| 20 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 143 817 | 154 245 | 154 637 |
| 21 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 140 603 | 155 054 | 164 274 |
| 22 | crystal (0.34)| [amber](https://amberframework.org) (0.34) | 139 625 | 144 992 | 141 041 |
| 23 | rust (1.44)| [actix](https://actix.rs) (2.0) | 138 199 | 139 565 | 139 160 |
| 24 | crystal (0.34)| [orion](https://github.com/obsidian/orion) (2.3) | 133 163 | 137 523 | 132 638 |
| 25 | crystal (0.34)| [athena](https://github.com/athena-framework/athena) (0.8) | 124 081 | 125 307 | 118 581 |
| 26 | java (8)| [act](https://actframework.org) (1.8) | 121 752 | 135 304 | 135 362 |
| 27 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 111 770 | 115 988 | 119 880 |
| 28 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 109 812 | 108 343 | 109 053 |
| 29 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 109 405 | 108 393 | 112 120 |
| 30 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 108 842 | 109 434 | 112 784 |
| 31 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 105 003 | 103 094 | 106 642 |
| 32 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 103 761 | 100 687 | 106 125 |
| 33 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 103 206 | 106 725 | 110 021 |
| 34 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 102 770 | 102 695 | 105 847 |
| 35 | fsharp (4.7)| [frank](https://github.com/frank-fs/frank) (6.0) | 101 900 | 109 250 | 112 410 |
| 36 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 101 337 | 102 172 | 103 348 |
| 37 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 101 209 | 107 750 | 105 295 |
| 38 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (4.0) | 100 445 | 99 701 | 102 956 |
| 39 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 98 953 | 96 279 | 96 652 |
| 40 | c (99)| [kore](https://kore.io) (3.3) | 98 208 | 110 597 | 136 471 |
| 41 | go (1.14)| [beego](https://beego.me) (1.12) | 97 372 | 99 756 | 102 982 |
| 42 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 95 935 | 120 197 | 123 520 |
| 43 | go (1.14)| [violetear](https://violetear.org) (7.0) | 92 559 | 87 115 | 92 062 |
| 44 | fsharp (4.7)| [falco](https://github.com/pimbrouwers/Falco) (1.1) | 92 324 | 101 172 | 103 084 |
| 45 | javascript (13.14)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 89 858 | 98 680 | 97 916 |
| 46 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 87 819 | 91 765 | 93 877 |
| 47 | fsharp (4.7)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 85 987 | 84 939 | 82 019 |
| 48 | javascript (13.14)| [0http](https://github.com/jkyberneees/0http) (2.5) | 85 979 | 95 009 | 94 430 |
| 49 | go (1.14)| [air](https://github.com/aofei/air) (0.17) | 84 820 | 86 096 | 88 055 |
| 50 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 80 943 | 80 976 | 78 642 |
| 51 | javascript (13.14)| [polka](https://github.com/lukeed/polka) (0.5) | 80 856 | 85 176 | 83 515 |
| 52 | javascript (13.14)| [rayo](https://rayo.js.org) (1.3) | 80 774 | 86 609 | 83 791 |
| 53 | javascript (13.14)| [restana](https://github.com/jkyberneees/ana) (4.6) | 79 817 | 87 938 | 86 889 |
| 54 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 77 296 | 83 830 | 80 969 |
| 55 | go (1.14)| [gf](https://goframe.org) (1.13) | 76 518 | 81 667 | 83 079 |
| 56 | swift (5.2)| [perfect](https://perfect.org) (3.1) | 76 458 | 84 424 | 89 853 |
| 57 | java (8)| [javalin](https://javalin.io) (3.8) | 74 698 | 79 946 | 79 454 |
| 58 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 72 868 | 77 248 | 77 142 |
| 59 | javascript (13.14)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 71 727 | 76 807 | 75 170 |
| 60 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 67 833 | 80 300 | 80 541 |
| 61 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.2) | 67 306 | 73 504 | 72 970 |
| 62 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 67 159 | 74 275 | 76 947 |
| 63 | php (7.4)| [hyperf](https://www.hyperf.io) (1.1) | 66 431 | 69 841 | 70 451 |
| 64 | javascript (13.14)| [fastify](https://fastify.io) (2.14) | 65 661 | 69 185 | 66 618 |
| 65 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 65 166 | 64 342 | 67 261 |
| 66 | javascript (13.14)| [foxify](https://foxify.js.org) (0.1) | 64 276 | 69 046 | 68 525 |
| 67 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 63 928 | 65 528 | 69 887 |
| 68 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 63 005 | 68 108 | 68 002 |
| 69 | java (8)| [micronaut](https://micronaut.io) (1.2) | 62 924 | 70 019 | 69 841 |
| 70 | javascript (13.14)| [koa](https://koajs.com) (2.12) | 61 072 | 63 648 | 61 845 |
| 71 | javascript (13.14)| [nestjs-fastify](https://nestjs.com) (7.1) | 59 850 | 64 632 | 62 892 |
| 72 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 58 403 | 62 402 | 62 444 |
| 73 | javascript (13.14)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 57 655 | 60 118 | 58 356 |
| 74 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 56 505 | 63 788 | 63 607 |
| 75 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 56 168 | 182 414 | 172 088 |
| 76 | javascript (13.14)| [feathersjs](https://feathersjs.com) (4.5) | 55 724 | 58 385 | 56 961 |
| 77 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.26) | 55 655 | 61 570 | 61 845 |
| 78 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 55 301 | 56 390 | 55 590 |
| 79 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 54 803 | 56 142 | 55 837 |
| 80 | swift (5.2)| [kitura](https://kitura.io) (2.9) | 54 658 | 54 498 | 54 170 |
| 81 | rust (1.44)| [nickel](https://nickel-org.github.io) (0.11) | 53 690 | 54 196 | 53 087 |
| 82 | javascript (13.14)| [express](https://expressjs.com) (4.17) | 52 490 | 54 566 | 53 583 |
| 83 | rust (1.44)| [gotham](https://gotham.rs) (0.4) | 51 953 | 58 449 | 60 603 |
| 84 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 51 851 | 57 062 | 56 958 |
| 85 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 51 443 | 52 752 | 50 695 |
| 86 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 49 913 | 50 093 | 50 442 |
| 87 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 49 432 | 51 532 | 51 289 |
| 88 | swift (5.2)| [vapor](https://vapor.codes) (4.8) | 48 415 | 50 269 | 49 918 |
| 89 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 48 401 | 53 110 | 53 262 |
| 90 | javascript (13.14)| [moleculer](https://moleculer.services) (0.14) | 48 288 | 50 113 | 49 232 |
| 91 | python (3.8)| [hug](https://hug.rest) (2.6) | 46 960 | 48 197 | 48 470 |
| 92 | php (7.4)| [imi](https://imiphp.com) (1.2) | 46 335 | 48 580 | 49 310 |
| 93 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.3) | 45 684 | 50 070 | 49 337 |
| 94 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 45 337 | 45 691 | 45 431 |
| 95 | python (3.8)| [starlette](https://starlette.io) (0.13) | 44 488 | 48 361 | 47 981 |
| 96 | javascript (13.14)| [hapi](https://hapijs.com) (19.1) | 42 091 | 44 240 | 43 595 |
| 97 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 41 272 | 41 916 | 41 852 |
| 98 | javascript (13.14)| [nestjs-express](https://nestjs.com) (7.1) | 41 097 | 42 135 | 41 527 |
| 99 | javascript (13.14)| [restify](https://restify.com) (8.5) | 39 739 | 41 541 | 41 151 |
| 100 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 39 059 | 41 161 | 40 948 |
| 101 | scala (2.13)| [play](https://playframework.com) (2.8) | 38 884 | 39 932 | 40 138 |
| 102 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 38 835 | 41 318 | 41 189 |
| 103 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 37 292 | 37 646 | 35 449 |
| 104 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 36 955 | 38 241 | 36 680 |
| 105 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 36 307 | 36 638 | 36 911 |
| 106 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.32) | 36 099 | 36 959 | 35 168 |
| 107 | php (7.4)| [swoft](https://swoft.org) (2.0) | 35 961 | 37 135 | 36 789 |
| 108 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 34 687 | 35 818 | 35 442 |
| 109 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 33 677 | 35 002 | 34 605 |
| 110 | fsharp (4.7)| [saturn](https://saturnframework.org) (0.13) | 32 531 | 28 911 | 26 237 |
| 111 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 31 997 | 28 676 | 31 950 |
| 112 | dart (2.8)| [aqueduct](https://aqueduct.io) (3.3) | 31 700 | 31 498 | 31 161 |
| 113 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 31 531 | 32 247 | 31 079 |
| 114 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 30 333 | 29 797 | 28 615 |
| 115 | crystal (0.34)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.15) | 30 190 | 27 501 | 26 282 |
| 116 | python (3.8)| [responder](https://python-responder.org) (2.0) | 28 959 | 31 164 | 31 086 |
| 117 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.55) | 28 304 | 31 342 | 31 086 |
| 118 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 27 097 | 27 121 | 26 572 |
| 119 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 27 026 | 27 016 | 26 147 |
| 120 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 26 312 | 26 915 | 26 518 |
| 121 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 25 451 | 26 243 | 26 107 |
| 122 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 25 443 | 27 469 | 27 429 |
| 123 | go (1.14)| [macaron](https://go-macaron.com) (1.3) | 25 265 | 27 940 | 28 178 |
| 124 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 24 262 | 23 820 | 23 206 |
| 125 | rust (1.44)| [iron](https://ironframework.io) (0.6) | 23 995 | 24 136 | 24 003 |
| 126 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 23 748 | 24 357 | 24 170 |
| 127 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 23 594 | 24 229 | 24 050 |
| 128 | javascript (13.14)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 182 | 21 377 | 20 366 |
| 129 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 878 | 18 279 | 18 350 |
| 130 | java (8)| [blade](https://lets-blade.com) (2.0) | 17 832 | 19 918 | 19 716 |
| 131 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 17 282 | 17 513 | 17 587 |
| 132 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 15 190 | 15 450 | 15 356 |
| 133 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 14 588 | 14 921 | 14 880 |
| 134 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 14 306 | 14 323 | 14 348 |
| 135 | dart (2.8)| [start](https://github.com/lvivski/start) (0.4) | 13 907 | 13 633 | 12 663 |
| 136 | javascript (13.14)| [sails](https://sailsjs.com) (1.2) | 12 793 | 13 198 | 13 144 |
| 137 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.12) | 12 056 | 11 892 | 11 258 |
| 138 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.2) | 11 498 | 11 427 | 11 354 |
| 139 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 11 444 | 11 218 | 11 141 |
| 140 | pony (0.35)| [jennet](https://github.com/Theodus/jennet) (0.1) | 11 294 | 17 581 | 15 969 |
| 141 | swift (5.2)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 048 | 11 119 | 11 097 |
| 142 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 10 592 | 10 549 | 10 465 |
| 143 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 10 016 | 10 159 | 10 010 |
| 144 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 345 | 9 372 | 9 366 |
| 145 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 281 | 9 329 | 9 170 |
| 146 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 214 | 9 051 | 8 977 |
| 147 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 8 111 | 8 095 | 8 073 |
| 148 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 7 807 | 7 850 | 7 816 |
| 149 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 689 | 7 700 | 7 695 |
| 150 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 612 | 7 697 | 7 707 |
| 151 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 387 | 6 427 | 6 522 |
| 152 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 5 737 | 5 750 | 5 840 |
| 153 | crystal (0.34)| [onyx](https://onyxframework.org) (0.5) | 5 621 | 5 767 | 5 801 |
| 154 | php (7.4)| [slim](https://slimframework.com) (4.5) | 4 715 | 4 796 | 4 946 |
| 155 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 4 406 | 4 466 | 4 592 |
| 156 | php (7.4)| [lumen](https://lumen.laravel.com) (7.1) | 4 311 | 4 339 | 4 464 |
| 157 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 198 | 4 307 | 4 363 |
| 158 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 809 | 3 599 | 3 593 |
| 159 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 380 | 7 914 | 6 121 |
| 160 | php (7.4)| [symfony](https://symfony.com) (5.1) | 3 202 | 3 245 | 3 322 |
| 161 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 875 | 2 958 | 2 980 |
| 162 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 2 537 | 2 702 | 1 122 |
| 163 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 420 | 2 422 | 2 420 |
| 164 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 624 | 1 636 | 1 627 |
| 165 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 557 | 1 507 | 1 502 |
| 166 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 450 | 1 478 | 1 485 |
| 167 | crystal (0.34)| [lucky](https://luckyframework.org) (0.21) | 1 413 | 1 435 | 1 434 |
| 168 | php (7.4)| [laravel](https://laravel.com) (7.14) | 586 | 162 | 1 970 |
| 169 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 548 | 478 | 1 410 |

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

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

:information_source:  Updated on **2020-06-15** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | javascript (13.14)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.11) | 189 592 | 202 653 | 202 760 |
| 2 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 188 721 | 201 420 | 202 295 |
| 3 | javascript (13.14)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.0) | 178 361 | 191 087 | 190 730 |
| 4 | javascript (13.14)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 177 166 | 190 473 | 190 146 |
| 5 | nim (1.2)| [whip](https://github.com/mattaylor/whip) (0.2) | 176 158 | 189 181 | 188 683 |
| 6 | php (7.4)| [simps](https://simps.io) (1.0) | 175 676 | 193 989 | 193 962 |
| 7 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.14) | 164 796 | 175 060 | 175 535 |
| 8 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 162 571 | 177 564 | 177 034 |
| 9 | java (8)| [jooby](https://jooby.io) (2.8) | 162 406 | 181 260 | 183 231 |
| 10 | kotlin (1.3)| [kooby](https://jooby.io) (2.8) | 161 537 | 187 932 | 190 200 |
| 11 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 159 673 | 170 791 | 171 899 |
| 12 | crystal (0.34)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 157 405 | 165 460 | 162 247 |
| 13 | crystal (0.34)| [toro](https://github.com/soveran/toro) (0.4) | 157 387 | 165 525 | 162 078 |
| 14 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.2) | 154 323 | 167 792 | 173 835 |
| 15 | crystal (0.34)| [spider-gazelle](https://spider-gazelle.net) (3.0) | 153 612 | 160 453 | 156 087 |
| 16 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 153 368 | 157 955 | 166 921 |
| 17 | go (1.14)| [fiber](https://gofiber.io) (1.11) | 152 056 | 164 536 | 163 930 |
| 18 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 151 727 | 158 788 | 171 050 |
| 19 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.3) | 149 530 | 146 485 | 158 579 |
| 20 | crystal (0.34)| [kemal](https://kemalcr.com) (0.26) | 145 247 | 151 223 | 147 157 |
| 21 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 141 941 | 152 254 | 153 155 |
| 22 | rust (1.44)| [actix](https://actix.rs) (2.0) | 139 943 | 143 224 | 141 532 |
| 23 | crystal (0.34)| [amber](https://amberframework.org) (0.34) | 139 027 | 144 345 | 141 083 |
| 24 | crystal (0.34)| [orion](https://github.com/obsidian/orion) (2.3) | 132 770 | 135 874 | 130 904 |
| 25 | crystal (0.34)| [athena](https://github.com/athena-framework/athena) (0.8) | 127 322 | 128 623 | 119 494 |
| 26 | java (8)| [act](https://actframework.org) (1.8) | 123 938 | 135 990 | 135 791 |
| 27 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 111 074 | 107 784 | 115 656 |
| 28 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 109 676 | 105 946 | 110 497 |
| 29 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 107 777 | 113 052 | 116 525 |
| 30 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 107 373 | 104 868 | 110 269 |
| 31 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 106 886 | 111 393 | 114 978 |
| 32 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 106 829 | 103 973 | 109 345 |
| 33 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 106 809 | 114 444 | 115 435 |
| 34 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 105 791 | 181 542 | 189 153 |
| 35 | fsharp (4.7)| [frank](https://github.com/frank-fs/frank) (6.1) | 105 059 | 110 950 | 112 052 |
| 36 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 103 722 | 99 601 | 105 523 |
| 37 | c (99)| [kore](https://kore.io) (3.3) | 102 475 | 102 672 | 124 981 |
| 38 | go (1.14)| [violetear](https://violetear.org) (7.0) | 101 925 | 96 108 | 102 560 |
| 39 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 101 301 | 104 456 | 109 605 |
| 40 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 100 744 | 106 770 | 105 247 |
| 41 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 100 228 | 97 954 | 100 502 |
| 42 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 97 543 | 121 148 | 126 223 |
| 43 | go (1.14)| [beego](https://beego.me) (1.12) | 90 867 | 90 984 | 96 098 |
| 44 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 89 987 | 89 729 | 90 789 |
| 45 | javascript (13.14)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 89 636 | 98 614 | 98 326 |
| 46 | fsharp (4.7)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 89 486 | 92 738 | 91 102 |
| 47 | go (1.14)| [air](https://github.com/aofei/air) (0.17) | 89 273 | 91 174 | 94 566 |
| 48 | fsharp (4.7)| [falco](https://github.com/pimbrouwers/Falco) (1.1) | 88 430 | 94 935 | 95 432 |
| 49 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 87 277 | 91 425 | 93 688 |
| 50 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.1) | 85 489 | 82 115 | 72 468 |
| 51 | javascript (13.14)| [0http](https://github.com/jkyberneees/0http) (2.5) | 85 397 | 94 025 | 94 275 |
| 52 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 80 515 | 81 015 | 78 525 |
| 53 | javascript (13.14)| [polka](https://github.com/lukeed/polka) (0.5) | 80 500 | 84 902 | 82 982 |
| 54 | javascript (13.14)| [restana](https://github.com/jkyberneees/ana) (4.6) | 80 353 | 87 846 | 87 438 |
| 55 | swift (5.2)| [perfect](https://perfect.org) (3.1) | 80 335 | 88 199 | 94 188 |
| 56 | javascript (13.14)| [rayo](https://rayo.js.org) (1.3) | 78 937 | 83 338 | 81 370 |
| 57 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 77 119 | 83 825 | 81 179 |
| 58 | java (8)| [javalin](https://javalin.io) (3.8) | 74 711 | 79 162 | 78 841 |
| 59 | go (1.14)| [gf](https://goframe.org) (1.13) | 72 598 | 81 475 | 77 726 |
| 60 | javascript (13.14)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 70 193 | 74 808 | 72 604 |
| 61 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 69 071 | 73 169 | 73 137 |
| 62 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 67 872 | 79 184 | 81 429 |
| 63 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 67 303 | 73 517 | 72 698 |
| 64 | php (7.4)| [hyperf](https://www.hyperf.io) (1.1) | 66 673 | 69 447 | 69 779 |
| 65 | javascript (13.14)| [fastify](https://fastify.io) (2.14) | 65 181 | 69 146 | 66 265 |
| 66 | javascript (13.14)| [foxify](https://foxify.js.org) (0.1) | 65 109 | 68 305 | 66 546 |
| 67 | java (8)| [micronaut](https://micronaut.io) (1.2) | 64 091 | 69 877 | 69 816 |
| 68 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 64 078 | 66 693 | 68 078 |
| 69 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 64 011 | 65 845 | 70 066 |
| 70 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 63 771 | 64 176 | 68 192 |
| 71 | fsharp (4.7)| [websharper](https://websharper.com) (4.6) | 62 743 | 62 525 | 59 415 |
| 72 | javascript (13.14)| [koa](https://koajs.com) (2.12) | 60 157 | 62 891 | 61 161 |
| 73 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 59 867 | 71 739 | 80 185 |
| 74 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 59 064 | 62 413 | 62 361 |
| 75 | javascript (13.14)| [nestjs-fastify](https://nestjs.com) (7.1) | 58 946 | 65 004 | 62 826 |
| 76 | rust (1.44)| [nickel](https://nickel-org.github.io) (0.11) | 56 636 | 56 100 | 55 272 |
| 77 | javascript (13.14)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 56 608 | 61 459 | 61 129 |
| 78 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 55 331 | 52 424 | 52 944 |
| 79 | swift (5.2)| [kitura](https://kitura.io) (2.9) | 54 963 | 53 440 | 50 120 |
| 80 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 54 856 | 62 644 | 62 427 |
| 81 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 54 571 | 56 092 | 56 049 |
| 82 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.26) | 54 470 | 61 088 | 61 300 |
| 83 | rust (1.44)| [gotham](https://gotham.rs) (0.4) | 52 635 | 58 144 | 61 187 |
| 84 | javascript (13.14)| [express](https://expressjs.com) (4.17) | 52 537 | 54 521 | 52 959 |
| 85 | javascript (13.14)| [feathersjs](https://feathersjs.com) (4.5) | 52 215 | 53 439 | 53 027 |
| 86 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 51 804 | 57 769 | 57 527 |
| 87 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 51 212 | 53 089 | 51 146 |
| 88 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 49 868 | 50 381 | 50 556 |
| 89 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 49 453 | 53 979 | 53 709 |
| 90 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 49 249 | 51 067 | 51 104 |
| 91 | swift (5.2)| [vapor](https://vapor.codes) (4.8) | 48 870 | 51 007 | 50 827 |
| 92 | javascript (13.14)| [moleculer](https://moleculer.services) (0.14) | 48 159 | 49 841 | 48 590 |
| 93 | python (3.8)| [hug](https://hug.rest) (2.6) | 46 737 | 48 061 | 47 999 |
| 94 | php (7.4)| [imi](https://imiphp.com) (1.2) | 46 406 | 47 934 | 48 290 |
| 95 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.3) | 45 428 | 49 750 | 48 884 |
| 96 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 45 378 | 45 774 | 45 396 |
| 97 | python (3.8)| [starlette](https://starlette.io) (0.13) | 44 966 | 48 142 | 48 264 |
| 98 | javascript (13.14)| [hapi](https://hapijs.com) (19.1) | 42 298 | 44 223 | 43 571 |
| 99 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 41 919 | 42 375 | 42 618 |
| 100 | javascript (13.14)| [nestjs-express](https://nestjs.com) (7.1) | 40 458 | 41 743 | 41 123 |
| 101 | javascript (13.14)| [restify](https://restify.com) (8.5) | 39 176 | 41 104 | 40 689 |
| 102 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 38 849 | 40 974 | 40 805 |
| 103 | scala (2.13)| [play](https://playframework.com) (2.8) | 38 802 | 40 253 | 40 134 |
| 104 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 37 155 | 38 047 | 36 003 |
| 105 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 37 125 | 40 054 | 39 754 |
| 106 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 37 114 | 37 541 | 35 602 |
| 107 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 36 338 | 36 627 | 36 797 |
| 108 | php (7.4)| [swoft](https://swoft.org) (2.0) | 35 987 | 37 202 | 36 950 |
| 109 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 35 075 | 36 070 | 35 957 |
| 110 | fsharp (4.7)| [saturn](https://saturnframework.org) (0.13) | 34 596 | 31 503 | 28 945 |
| 111 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 33 944 | 33 797 | 35 599 |
| 112 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 33 498 | 34 368 | 33 926 |
| 113 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.32) | 33 491 | 36 866 | 34 980 |
| 114 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 32 310 | 32 523 | 31 443 |
| 115 | dart (2.8)| [aqueduct](https://aqueduct.io) (3.3) | 30 009 | 30 015 | 29 828 |
| 116 | python (3.8)| [responder](https://python-responder.org) (2.0) | 29 302 | 30 890 | 30 553 |
| 117 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.57) | 29 243 | 30 670 | 30 457 |
| 118 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 29 120 | 28 239 | 27 058 |
| 119 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 27 114 | 27 006 | 26 133 |
| 120 | nim (1.2)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 26 954 | 23 326 | 21 375 |
| 121 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 26 497 | 26 240 | 25 743 |
| 122 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 26 334 | 26 868 | 26 674 |
| 123 | go (1.14)| [macaron](https://go-macaron.com) (1.3) | 25 962 | 28 608 | 28 905 |
| 124 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 25 491 | 26 088 | 25 892 |
| 125 | rust (1.44)| [iron](https://ironframework.io) (0.6) | 23 980 | 24 137 | 24 025 |
| 126 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 23 916 | 23 811 | 23 390 |
| 127 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 23 733 | 27 480 | 27 394 |
| 128 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 23 587 | 24 216 | 23 955 |
| 129 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 23 435 | 24 034 | 23 612 |
| 130 | javascript (13.14)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 23 002 | 21 968 | 20 946 |
| 131 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 790 | 18 194 | 18 408 |
| 132 | java (8)| [blade](https://lets-blade.com) (2.0) | 17 727 | 20 668 | 19 464 |
| 133 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 17 490 | 17 824 | 17 916 |
| 134 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 15 181 | 15 381 | 15 457 |
| 135 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 14 457 | 14 438 | 14 418 |
| 136 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 13 988 | 15 228 | 15 366 |
| 137 | dart (2.8)| [start](https://github.com/lvivski/start) (0.4) | 13 709 | 13 436 | 12 486 |
| 138 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 13 606 | 14 376 | 13 784 |
| 139 | javascript (13.14)| [sails](https://sailsjs.com) (1.2) | 12 580 | 12 985 | 13 034 |
| 140 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.2) | 11 476 | 11 397 | 11 213 |
| 141 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 11 468 | 11 249 | 11 131 |
| 142 | swift (5.2)| [swifter](https://github.com/httpswift/swifter) (1.4) | 10 955 | 11 001 | 10 875 |
| 143 | pony (0.35)| [jennet](https://github.com/Theodus/jennet) (0.1) | 10 718 | 17 922 | 15 984 |
| 144 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 10 685 | 10 619 | 10 621 |
| 145 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.12) | 10 639 | 10 381 | 9 945 |
| 146 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 831 | 9 813 | 9 690 |
| 147 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 629 | 9 710 | 9 612 |
| 148 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 392 | 9 357 | 9 302 |
| 149 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 178 | 9 050 | 8 996 |
| 150 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 8 040 | 8 056 | 8 014 |
| 151 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 7 776 | 7 821 | 7 855 |
| 152 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 658 | 7 742 | 7 669 |
| 153 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 604 | 7 637 | 7 625 |
| 154 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 442 | 6 498 | 6 484 |
| 155 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 5 766 | 5 761 | 5 919 |
| 156 | crystal (0.34)| [onyx](https://onyxframework.org) (0.5) | 5 691 | 5 812 | 5 893 |
| 157 | php (7.4)| [slim](https://slimframework.com) (4.5) | 4 715 | 4 777 | 4 870 |
| 158 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 4 378 | 4 478 | 4 589 |
| 159 | php (7.4)| [lumen](https://lumen.laravel.com) (7.1) | 4 310 | 4 362 | 4 445 |
| 160 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 153 | 4 241 | 4 363 |
| 161 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 624 | 3 511 | 3 488 |
| 162 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 324 | 7 832 | 5 978 |
| 163 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 3 258 | 1 895 | 1 787 |
| 164 | php (7.4)| [symfony](https://symfony.com) (5.1) | 3 185 | 3 221 | 3 252 |
| 165 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 2 887 | 1 859 | 1 194 |
| 166 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 848 | 2 921 | 2 953 |
| 167 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 227 | 2 244 | 2 207 |
| 168 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 617 | 1 632 | 1 621 |
| 169 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 545 | 1 502 | 1 495 |
| 170 | crystal (0.34)| [lucky](https://luckyframework.org) (0.21) | 1 441 | 1 471 | 1 467 |
| 171 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 435 | 1 465 | 1 467 |
| 172 | php (7.4)| [laravel](https://laravel.com) (7.15) | 919 | 181 | 3 126 |
| 173 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 499 | 444 | 1 330 |

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

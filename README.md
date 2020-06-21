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

:information_source:  Updated on **2020-06-21** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | javascript (13.14)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.11) | 190 210 | 205 607 | 206 446 |
| 2 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 189 668 | 205 134 | 205 541 |
| 3 | nim (1.2)| [whip](https://github.com/mattaylor/whip) (0.2) | 183 142 | 195 577 | 195 507 |
| 4 | javascript (13.14)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 178 366 | 192 478 | 193 568 |
| 5 | kotlin (1.3)| [kooby](https://jooby.io) (2.8) | 177 099 | 198 651 | 202 836 |
| 6 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.14) | 169 416 | 181 426 | 187 212 |
| 7 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 167 536 | 174 608 | 177 752 |
| 8 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.2) | 164 609 | 175 465 | 181 623 |
| 9 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.3) | 164 598 | 175 476 | 180 878 |
| 10 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 163 365 | 175 045 | 180 335 |
| 11 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 162 895 | 172 802 | 178 879 |
| 12 | go (1.14)| [fiber](https://gofiber.io) (1.12) | 162 442 | 167 506 | 168 400 |
| 13 | go (1.14)| [gearbox](https://gogearbox.com) (1.0) | 160 016 | 172 591 | 178 141 |
| 14 | crystal (0.34)| [toro](https://github.com/soveran/toro) (0.4) | 158 219 | 165 480 | 162 092 |
| 15 | crystal (0.34)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 157 937 | 164 624 | 161 434 |
| 16 | java (8)| [jooby](https://jooby.io) (2.8) | 153 520 | 173 417 | 177 082 |
| 17 | crystal (0.34)| [spider-gazelle](https://spider-gazelle.net) (3.0) | 152 719 | 158 616 | 155 730 |
| 18 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 152 167 | 167 037 | 165 565 |
| 19 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 151 010 | 161 126 | 162 122 |
| 20 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 150 081 | 161 305 | 161 282 |
| 21 | crystal (0.34)| [kemal](https://kemalcr.com) (0.26) | 145 543 | 152 512 | 149 125 |
| 22 | crystal (0.34)| [amber](https://amberframework.org) (0.34) | 138 946 | 144 090 | 140 238 |
| 23 | rust (1.44)| [actix](https://actix.rs) (2.0) | 138 586 | 138 184 | 138 170 |
| 24 | crystal (0.34)| [orion](https://github.com/obsidian/orion) (2.3) | 133 062 | 136 725 | 133 301 |
| 25 | crystal (0.34)| [athena](https://github.com/athena-framework/athena) (0.8) | 127 898 | 128 886 | 122 717 |
| 26 | c (99)| [kore](https://kore.io) (3.3) | 123 538 | 116 655 | 136 825 |
| 27 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 115 709 | 115 805 | 121 317 |
| 28 | java (8)| [act](https://actframework.org) (1.8) | 115 251 | 128 598 | 127 793 |
| 29 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 114 193 | 114 604 | 119 472 |
| 30 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 111 598 | 116 058 | 120 280 |
| 31 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 110 748 | 114 235 | 117 054 |
| 32 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 109 629 | 108 842 | 113 195 |
| 33 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 109 628 | 113 863 | 117 755 |
| 34 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 108 004 | 109 008 | 112 906 |
| 35 | go (1.14)| [violetear](https://violetear.org) (7.0) | 106 752 | 106 694 | 111 666 |
| 36 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 105 843 | 104 263 | 108 225 |
| 37 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 103 905 | 109 277 | 112 391 |
| 38 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 103 820 | 101 193 | 105 252 |
| 39 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 103 795 | 101 077 | 108 098 |
| 40 | go (1.14)| [beego](https://beego.me) (1.12) | 103 307 | 106 635 | 110 731 |
| 41 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 97 694 | 103 022 | 104 817 |
| 42 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 96 693 | 124 060 | 129 738 |
| 43 | go (1.14)| [air](https://github.com/aofei/air) (0.17) | 90 966 | 91 325 | 94 777 |
| 44 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 90 944 | 96 644 | 97 403 |
| 45 | javascript (13.14)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 90 846 | 100 103 | 100 351 |
| 46 | php (7.4)| [hyperf](https://www.hyperf.io) (1.1) | 89 561 | 95 442 | 96 182 |
| 47 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 88 321 | 181 527 | 177 078 |
| 48 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 88 265 | 93 790 | 95 909 |
| 49 | fsharp (4.7)| [falco](https://github.com/pimbrouwers/Falco) (1.1) | 84 704 | 91 083 | 91 436 |
| 50 | javascript (13.14)| [0http](https://github.com/jkyberneees/0http) (2.5) | 84 193 | 92 298 | 94 726 |
| 51 | fsharp (4.7)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 83 171 | 87 121 | 89 800 |
| 52 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 82 358 | 97 169 | 99 574 |
| 53 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 82 053 | 82 436 | 79 555 |
| 54 | swift (5.2)| [perfect](https://perfect.org) (3.1) | 81 701 | 89 649 | 94 943 |
| 55 | javascript (13.14)| [polka](https://github.com/lukeed/polka) (0.5) | 81 605 | 86 359 | 85 361 |
| 56 | go (1.14)| [gf](https://goframe.org) (1.13) | 81 197 | 87 334 | 89 158 |
| 57 | javascript (13.14)| [rayo](https://rayo.js.org) (1.3) | 80 090 | 84 318 | 83 526 |
| 58 | javascript (13.14)| [restana](https://github.com/jkyberneees/ana) (4.6) | 78 919 | 86 121 | 86 945 |
| 59 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 78 499 | 84 614 | 82 155 |
| 60 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.1) | 78 082 | 75 417 | 66 939 |
| 61 | fsharp (4.7)| [frank](https://github.com/frank-fs/frank) (6.1) | 77 086 | 92 926 | 85 499 |
| 62 | fsharp (4.7)| [saturn](https://saturnframework.org) (0.14) | 72 791 | 66 108 | 57 728 |
| 63 | javascript (13.14)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 71 262 | 76 677 | 74 929 |
| 64 | java (8)| [javalin](https://javalin.io) (3.8) | 70 483 | 75 759 | 75 543 |
| 65 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 68 621 | 68 270 | 71 745 |
| 66 | javascript (13.14)| [foxify](https://foxify.js.org) (0.1) | 66 571 | 70 081 | 69 108 |
| 67 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 64 688 | 66 701 | 72 310 |
| 68 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 64 260 | 72 978 | 74 095 |
| 69 | javascript (13.14)| [fastify](https://fastify.io) (2.15) | 63 475 | 66 568 | 67 715 |
| 70 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 62 544 | 68 564 | 67 600 |
| 71 | java (8)| [micronaut](https://micronaut.io) (1.2) | 60 973 | 67 724 | 67 694 |
| 72 | javascript (13.14)| [koa](https://koajs.com) (2.12) | 60 253 | 61 197 | 59 861 |
| 73 | javascript (13.14)| [nestjs-fastify](https://nestjs.com) (7.2) | 60 045 | 64 017 | 62 545 |
| 74 | javascript (13.14)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 58 785 | 60 546 | 59 004 |
| 75 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 57 098 | 57 915 | 59 133 |
| 76 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 56 292 | 74 506 | 75 369 |
| 77 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 55 776 | 56 660 | 56 864 |
| 78 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.26) | 55 441 | 62 100 | 62 009 |
| 79 | fsharp (4.7)| [websharper](https://websharper.com) (4.6) | 55 372 | 53 444 | 49 534 |
| 80 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 55 230 | 52 484 | 52 657 |
| 81 | swift (5.2)| [kitura](https://kitura.io) (2.9) | 54 824 | 55 250 | 54 593 |
| 82 | rust (1.44)| [nickel](https://nickel-org.github.io) (0.11) | 54 464 | 55 101 | 54 404 |
| 83 | javascript (13.14)| [express](https://expressjs.com) (4.17) | 53 914 | 55 605 | 54 953 |
| 84 | javascript (13.14)| [feathersjs](https://feathersjs.com) (4.5) | 52 859 | 55 640 | 54 614 |
| 85 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 52 142 | 53 851 | 51 609 |
| 86 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 51 806 | 59 926 | 59 421 |
| 87 | rust (1.44)| [gotham](https://gotham.rs) (0.4) | 51 034 | 57 763 | 59 222 |
| 88 | swift (5.2)| [vapor](https://vapor.codes) (4.10) | 50 819 | 52 163 | 52 124 |
| 89 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 50 340 | 50 458 | 50 350 |
| 90 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 50 312 | 57 377 | 57 964 |
| 91 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 49 400 | 50 586 | 50 501 |
| 92 | javascript (13.14)| [moleculer](https://moleculer.services) (0.14) | 48 698 | 50 368 | 48 835 |
| 93 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 46 789 | 53 771 | 52 261 |
| 94 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 46 286 | 45 436 | 45 843 |
| 95 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.3) | 45 953 | 48 984 | 48 914 |
| 96 | python (3.8)| [starlette](https://starlette.io) (0.13) | 43 895 | 47 924 | 47 908 |
| 97 | javascript (13.14)| [hapi](https://hapijs.com) (19.1) | 43 222 | 45 447 | 44 935 |
| 98 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 42 260 | 42 656 | 42 907 |
| 99 | scala (2.13)| [play](https://playframework.com) (2.8) | 42 072 | 41 851 | 41 947 |
| 100 | python (3.8)| [hug](https://hug.rest) (2.6) | 41 748 | 42 838 | 41 074 |
| 101 | javascript (13.14)| [nestjs-express](https://nestjs.com) (7.2) | 41 659 | 43 097 | 42 527 |
| 102 | javascript (13.14)| [restify](https://restify.com) (8.5) | 40 327 | 42 437 | 41 818 |
| 103 | php (7.4)| [imi](https://imiphp.com) (1.2) | 40 311 | 38 545 | 40 528 |
| 104 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 40 304 | 42 823 | 42 796 |
| 105 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 38 736 | 40 155 | 38 297 |
| 106 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 37 776 | 39 256 | 36 761 |
| 107 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 37 628 | 43 005 | 40 625 |
| 108 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.33) | 37 353 | 38 271 | 36 541 |
| 109 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 37 324 | 42 839 | 41 373 |
| 110 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 36 476 | 36 811 | 37 056 |
| 111 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 35 166 | 33 549 | 35 465 |
| 112 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.1) | 34 503 | 34 677 | 33 566 |
| 113 | php (7.4)| [swoft](https://swoft.org) (2.0) | 33 681 | 38 049 | 38 476 |
| 114 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 32 631 | 33 519 | 32 535 |
| 115 | dart (2.8)| [aqueduct](https://aqueduct.io) (3.3) | 31 037 | 30 867 | 30 948 |
| 116 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.58) | 30 625 | 32 725 | 32 376 |
| 117 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 30 348 | 29 320 | 28 386 |
| 118 | nim (1.2)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 29 131 | 25 828 | 23 024 |
| 119 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 28 725 | 29 751 | 32 409 |
| 120 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 28 607 | 28 286 | 27 475 |
| 121 | python (3.8)| [responder](https://python-responder.org) (2.0) | 28 416 | 30 725 | 30 182 |
| 122 | nim (1.2)| [akane](https://github.com/Ethosa/akane) (0.1) | 27 996 | 24 879 | 22 111 |
| 123 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 27 643 | 28 017 | 27 241 |
| 124 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 26 526 | 26 647 | 24 965 |
| 125 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 26 079 | 26 432 | 26 026 |
| 126 | go (1.14)| [macaron](https://go-macaron.com) (1.3) | 25 917 | 28 433 | 28 426 |
| 127 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 24 541 | 24 428 | 23 801 |
| 128 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 24 075 | 27 024 | 27 135 |
| 129 | rust (1.44)| [iron](https://ironframework.io) (0.6) | 23 875 | 23 978 | 23 986 |
| 130 | javascript (13.14)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 23 389 | 22 448 | 21 427 |
| 131 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 23 161 | 23 991 | 22 613 |
| 132 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 20 394 | 21 303 | 20 002 |
| 133 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 17 738 | 17 917 | 17 875 |
| 134 | java (8)| [blade](https://lets-blade.com) (2.0) | 16 955 | 18 687 | 19 041 |
| 135 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 16 677 | 17 822 | 18 386 |
| 136 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 16 078 | 15 848 | 15 821 |
| 137 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 15 087 | 15 446 | 15 397 |
| 138 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 15 017 | 14 901 | 14 797 |
| 139 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 13 976 | 15 158 | 15 526 |
| 140 | dart (2.8)| [start](https://github.com/lvivski/start) (0.4) | 13 511 | 13 427 | 12 486 |
| 141 | javascript (13.14)| [sails](https://sailsjs.com) (1.2) | 12 670 | 12 935 | 12 975 |
| 142 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.12) | 12 662 | 12 552 | 11 872 |
| 143 | crystal (0.34)| [runcobo](https://github.com/runcobo/runcobo) (0.2) | 12 232 | 11 997 | 11 753 |
| 144 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 11 811 | 11 500 | 11 506 |
| 145 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 11 005 | 10 907 | 10 865 |
| 146 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.2) | 10 983 | 10 878 | 10 636 |
| 147 | swift (5.2)| [swifter](https://github.com/httpswift/swifter) (1.4) | 10 845 | 10 940 | 11 089 |
| 148 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 534 | 9 390 | 9 304 |
| 149 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 742 | 8 167 | 7 716 |
| 150 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 8 508 | 8 711 | 8 790 |
| 151 | python (3.8)| [django](https://djangoproject.com) (3.0) | 8 307 | 8 138 | 8 307 |
| 152 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 782 | 7 548 | 7 132 |
| 153 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 7 739 | 7 653 | 7 762 |
| 154 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 514 | 7 487 | 7 508 |
| 155 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 7 466 | 7 435 | 7 407 |
| 156 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 238 | 7 004 | 7 029 |
| 157 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 422 | 6 470 | 6 512 |
| 158 | crystal (0.34)| [onyx](https://onyxframework.org) (0.5) | 5 861 | 6 038 | 6 074 |
| 159 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 5 743 | 5 765 | 5 882 |
| 160 | php (7.4)| [slim](https://slimframework.com) (4.5) | 4 719 | 4 778 | 4 878 |
| 161 | php (7.4)| [lumen](https://lumen.laravel.com) (7.1) | 4 602 | 4 628 | 4 707 |
| 162 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 4 414 | 4 482 | 4 632 |
| 163 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 189 | 4 274 | 4 330 |
| 164 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 4 041 | 3 841 | 3 821 |
| 165 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 472 | 7 987 | 6 099 |
| 166 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 3 078 | 3 177 | 1 333 |
| 167 | php (7.4)| [symfony](https://symfony.com) (5.1) | 3 049 | 3 122 | 3 176 |
| 168 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 852 | 2 943 | 3 020 |
| 169 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 2 770 | 2 608 | 1 331 |
| 170 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 220 | 2 254 | 2 235 |
| 171 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 553 | 1 575 | 1 576 |
| 172 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 546 | 1 512 | 1 498 |
| 173 | crystal (0.34)| [lucky](https://luckyframework.org) (0.21) | 1 450 | 1 468 | 1 467 |
| 174 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 392 | 1 421 | 1 436 |
| 175 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 722 | 436 | 2 086 |
| 176 | php (7.4)| [laravel](https://laravel.com) (7.16) | 719 | 160 | 2 219 |

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

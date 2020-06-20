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

:information_source:  Updated on **2020-06-20** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 190 114 | 202 829 | 203 125 |
| 2 | javascript (13.14)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.11) | 189 365 | 204 258 | 204 262 |
| 3 | nim (1.2)| [whip](https://github.com/mattaylor/whip) (0.2) | 178 574 | 190 828 | 190 606 |
| 4 | javascript (13.14)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 177 204 | 190 686 | 189 579 |
| 5 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 172 296 | 184 404 | 188 380 |
| 6 | kotlin (1.3)| [kooby](https://jooby.io) (2.8) | 170 019 | 190 717 | 193 009 |
| 7 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.14) | 163 459 | 173 243 | 176 500 |
| 8 | go (1.14)| [fiber](https://gofiber.io) (1.12) | 163 007 | 165 939 | 164 005 |
| 9 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 162 086 | 178 028 | 178 187 |
| 10 | java (8)| [jooby](https://jooby.io) (2.8) | 161 911 | 186 647 | 189 420 |
| 11 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.2) | 158 538 | 167 738 | 170 833 |
| 12 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 157 926 | 167 087 | 170 266 |
| 13 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.3) | 157 917 | 167 715 | 171 229 |
| 14 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 156 439 | 165 807 | 169 103 |
| 15 | go (1.14)| [gearbox](https://gogearbox.com) (1.0) | 154 622 | 164 122 | 167 683 |
| 16 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 151 314 | 167 301 | 172 113 |
| 17 | crystal (0.34)| [toro](https://github.com/soveran/toro) (0.4) | 149 275 | 154 110 | 159 954 |
| 18 | crystal (0.34)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 147 777 | 153 509 | 158 422 |
| 19 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 147 585 | 158 476 | 158 452 |
| 20 | crystal (0.34)| [spider-gazelle](https://spider-gazelle.net) (3.0) | 137 138 | 149 956 | 148 397 |
| 21 | crystal (0.34)| [kemal](https://kemalcr.com) (0.26) | 136 028 | 151 717 | 150 282 |
| 22 | rust (1.44)| [actix](https://actix.rs) (2.0) | 132 911 | 134 027 | 135 114 |
| 23 | crystal (0.34)| [amber](https://amberframework.org) (0.34) | 125 337 | 130 916 | 125 919 |
| 24 | crystal (0.34)| [orion](https://github.com/obsidian/orion) (2.3) | 124 403 | 126 478 | 130 304 |
| 25 | java (8)| [act](https://actframework.org) (1.8) | 122 571 | 134 527 | 134 419 |
| 26 | c (99)| [kore](https://kore.io) (3.3) | 116 497 | 122 548 | 134 371 |
| 27 | crystal (0.34)| [athena](https://github.com/athena-framework/athena) (0.8) | 113 393 | 119 380 | 115 044 |
| 28 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 112 566 | 165 514 | 181 204 |
| 29 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 109 742 | 109 231 | 113 052 |
| 30 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 109 480 | 109 958 | 113 416 |
| 31 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 108 967 | 108 551 | 111 855 |
| 32 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 106 607 | 109 731 | 112 741 |
| 33 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 105 416 | 108 715 | 111 347 |
| 34 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 105 303 | 103 368 | 106 927 |
| 35 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 105 232 | 129 702 | 135 120 |
| 36 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 105 111 | 105 482 | 109 331 |
| 37 | fsharp (4.7)| [frank](https://github.com/frank-fs/frank) (6.1) | 104 614 | 113 151 | 114 634 |
| 38 | go (1.14)| [violetear](https://violetear.org) (7.0) | 102 072 | 100 898 | 104 679 |
| 39 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 101 910 | 105 223 | 108 003 |
| 40 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 100 926 | 98 289 | 101 446 |
| 41 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 100 552 | 99 666 | 103 129 |
| 42 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 99 518 | 107 664 | 108 861 |
| 43 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 99 148 | 103 296 | 105 314 |
| 44 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 98 942 | 95 435 | 98 968 |
| 45 | go (1.14)| [beego](https://beego.me) (1.12) | 97 935 | 99 863 | 103 144 |
| 46 | fsharp (4.7)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 91 414 | 95 873 | 94 558 |
| 47 | php (7.4)| [hyperf](https://www.hyperf.io) (1.1) | 91 286 | 98 367 | 101 057 |
| 48 | fsharp (4.7)| [falco](https://github.com/pimbrouwers/Falco) (1.1) | 89 922 | 98 156 | 99 575 |
| 49 | javascript (13.14)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 89 820 | 99 111 | 98 347 |
| 50 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 87 676 | 91 316 | 92 480 |
| 51 | go (1.14)| [air](https://github.com/aofei/air) (0.17) | 86 334 | 87 545 | 90 421 |
| 52 | javascript (13.14)| [0http](https://github.com/jkyberneees/0http) (2.5) | 85 679 | 94 335 | 94 182 |
| 53 | javascript (13.14)| [polka](https://github.com/lukeed/polka) (0.5) | 80 753 | 85 382 | 84 056 |
| 54 | javascript (13.14)| [restana](https://github.com/jkyberneees/ana) (4.6) | 80 010 | 88 243 | 87 550 |
| 55 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 79 767 | 87 985 | 84 986 |
| 56 | javascript (13.14)| [rayo](https://rayo.js.org) (1.3) | 79 165 | 83 974 | 82 104 |
| 57 | go (1.14)| [gf](https://goframe.org) (1.13) | 76 924 | 82 406 | 83 373 |
| 58 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.1) | 76 550 | 73 947 | 69 463 |
| 59 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 76 041 | 80 233 | 74 693 |
| 60 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 76 014 | 91 682 | 92 752 |
| 61 | fsharp (4.7)| [saturn](https://saturnframework.org) (0.14) | 74 644 | 68 150 | 60 189 |
| 62 | java (8)| [javalin](https://javalin.io) (3.8) | 74 639 | 79 704 | 79 606 |
| 63 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 72 088 | 76 304 | 76 456 |
| 64 | javascript (13.14)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 69 774 | 74 570 | 72 867 |
| 65 | swift (5.2)| [perfect](https://perfect.org) (3.1) | 68 231 | 80 103 | 89 804 |
| 66 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 67 279 | 73 257 | 72 048 |
| 67 | javascript (13.14)| [fastify](https://fastify.io) (2.14) | 66 931 | 70 358 | 68 498 |
| 68 | javascript (13.14)| [foxify](https://foxify.js.org) (0.1) | 65 288 | 68 364 | 67 429 |
| 69 | java (8)| [micronaut](https://micronaut.io) (1.2) | 64 979 | 70 853 | 70 860 |
| 70 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 64 809 | 64 010 | 66 960 |
| 71 | fsharp (4.7)| [websharper](https://websharper.com) (4.6) | 64 566 | 64 583 | 61 239 |
| 72 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 64 321 | 66 086 | 69 626 |
| 73 | javascript (13.14)| [koa](https://koajs.com) (2.12) | 60 796 | 63 635 | 61 682 |
| 74 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 60 354 | 62 804 | 62 079 |
| 75 | javascript (13.14)| [nestjs-fastify](https://nestjs.com) (7.2) | 58 142 | 61 462 | 60 272 |
| 76 | javascript (13.14)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 57 395 | 60 126 | 58 342 |
| 77 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 57 179 | 74 265 | 80 371 |
| 78 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.26) | 56 174 | 64 893 | 65 232 |
| 79 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 54 828 | 55 999 | 55 943 |
| 80 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 54 672 | 62 657 | 62 484 |
| 81 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 54 647 | 60 500 | 61 296 |
| 82 | rust (1.44)| [nickel](https://nickel-org.github.io) (0.11) | 54 280 | 54 183 | 53 118 |
| 83 | javascript (13.14)| [feathersjs](https://feathersjs.com) (4.5) | 52 739 | 54 841 | 54 142 |
| 84 | javascript (13.14)| [express](https://expressjs.com) (4.17) | 52 703 | 54 581 | 53 916 |
| 85 | rust (1.44)| [gotham](https://gotham.rs) (0.4) | 51 993 | 57 867 | 60 591 |
| 86 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 51 007 | 57 154 | 57 520 |
| 87 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 50 718 | 49 519 | 48 572 |
| 88 | swift (5.2)| [vapor](https://vapor.codes) (4.10) | 49 827 | 51 414 | 51 354 |
| 89 | javascript (13.14)| [moleculer](https://moleculer.services) (0.14) | 48 355 | 49 947 | 49 166 |
| 90 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 47 176 | 50 068 | 49 178 |
| 91 | php (7.4)| [imi](https://imiphp.com) (1.2) | 46 637 | 49 009 | 49 384 |
| 92 | python (3.8)| [hug](https://hug.rest) (2.6) | 46 344 | 48 028 | 48 242 |
| 93 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 45 232 | 46 638 | 45 269 |
| 94 | swift (5.2)| [kitura](https://kitura.io) (2.9) | 44 888 | 45 091 | 43 656 |
| 95 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 44 738 | 45 793 | 44 056 |
| 96 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.3) | 44 642 | 51 016 | 50 260 |
| 97 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 43 456 | 43 931 | 44 484 |
| 98 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 42 367 | 45 101 | 45 238 |
| 99 | javascript (13.14)| [hapi](https://hapijs.com) (19.1) | 42 279 | 44 333 | 43 430 |
| 100 | python (3.8)| [starlette](https://starlette.io) (0.13) | 41 685 | 45 849 | 45 925 |
| 101 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 40 928 | 43 662 | 43 196 |
| 102 | javascript (13.14)| [nestjs-express](https://nestjs.com) (7.2) | 40 838 | 42 889 | 41 865 |
| 103 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 39 423 | 41 242 | 41 214 |
| 104 | javascript (13.14)| [restify](https://restify.com) (8.5) | 39 388 | 41 658 | 41 211 |
| 105 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.1) | 38 740 | 39 673 | 38 904 |
| 106 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 38 421 | 40 813 | 40 601 |
| 107 | php (7.4)| [swoft](https://swoft.org) (2.0) | 37 982 | 41 618 | 42 069 |
| 108 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 37 112 | 37 870 | 38 047 |
| 109 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 36 679 | 35 312 | 36 542 |
| 110 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 35 455 | 36 052 | 33 617 |
| 111 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 34 809 | 34 865 | 33 688 |
| 112 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 34 477 | 35 421 | 33 283 |
| 113 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 33 631 | 34 567 | 34 087 |
| 114 | scala (2.13)| [play](https://playframework.com) (2.8) | 32 728 | 35 441 | 35 214 |
| 115 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.33) | 32 524 | 34 199 | 32 913 |
| 116 | dart (2.8)| [aqueduct](https://aqueduct.io) (3.3) | 31 596 | 32 110 | 31 250 |
| 117 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 30 270 | 29 210 | 28 431 |
| 118 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.58) | 28 638 | 30 348 | 30 541 |
| 119 | nim (1.2)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 27 675 | 23 877 | 21 873 |
| 120 | python (3.8)| [responder](https://python-responder.org) (2.0) | 27 181 | 28 845 | 29 221 |
| 121 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 26 442 | 29 578 | 29 653 |
| 122 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 26 322 | 25 818 | 25 300 |
| 123 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 25 700 | 25 707 | 26 495 |
| 124 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 25 246 | 25 831 | 25 713 |
| 125 | go (1.14)| [macaron](https://go-macaron.com) (1.3) | 25 164 | 27 802 | 28 092 |
| 126 | rust (1.44)| [iron](https://ironframework.io) (0.6) | 24 005 | 24 082 | 23 910 |
| 127 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 23 697 | 24 526 | 23 313 |
| 128 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 23 577 | 24 186 | 23 848 |
| 129 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 23 162 | 24 648 | 23 733 |
| 130 | javascript (13.14)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 510 | 21 540 | 20 482 |
| 131 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 21 747 | 22 532 | 21 960 |
| 132 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 775 | 18 189 | 18 198 |
| 133 | java (8)| [blade](https://lets-blade.com) (2.0) | 17 717 | 19 772 | 20 018 |
| 134 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 17 454 | 17 733 | 17 790 |
| 135 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 15 710 | 16 385 | 16 506 |
| 136 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 14 559 | 14 922 | 14 881 |
| 137 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 14 113 | 15 342 | 15 518 |
| 138 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 14 033 | 13 239 | 12 890 |
| 139 | dart (2.8)| [start](https://github.com/lvivski/start) (0.4) | 13 354 | 13 212 | 12 321 |
| 140 | javascript (13.14)| [sails](https://sailsjs.com) (1.2) | 12 756 | 13 147 | 13 156 |
| 141 | crystal (0.34)| [runcobo](https://github.com/runcobo/runcobo) (0.2) | 12 492 | 12 164 | 11 605 |
| 142 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 12 165 | 11 927 | 11 909 |
| 143 | swift (5.2)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 560 | 11 592 | 11 544 |
| 144 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.2) | 11 532 | 11 446 | 11 246 |
| 145 | pony (0.35)| [jennet](https://github.com/Theodus/jennet) (0.1) | 11 366 | 18 151 | 15 981 |
| 146 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 10 777 | 11 098 | 10 672 |
| 147 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.12) | 10 755 | 11 015 | 11 014 |
| 148 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 815 | 9 825 | 9 678 |
| 149 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 813 | 9 883 | 9 679 |
| 150 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 282 | 9 159 | 9 097 |
| 151 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 245 | 9 268 | 9 182 |
| 152 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 7 903 | 8 039 | 7 907 |
| 153 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 7 766 | 7 826 | 7 810 |
| 154 | crystal (0.34)| [onyx](https://onyxframework.org) (0.5) | 7 681 | 8 919 | 9 092 |
| 155 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 641 | 7 719 | 7 730 |
| 156 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 514 | 7 589 | 7 152 |
| 157 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 6 730 | 6 044 | 6 040 |
| 158 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 326 | 6 435 | 6 441 |
| 159 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 5 762 | 5 776 | 5 849 |
| 160 | php (7.4)| [slim](https://slimframework.com) (4.5) | 4 720 | 4 792 | 4 911 |
| 161 | php (7.4)| [lumen](https://lumen.laravel.com) (7.1) | 4 378 | 4 464 | 4 508 |
| 162 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 4 319 | 4 474 | 4 443 |
| 163 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 177 | 4 226 | 4 318 |
| 164 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 646 | 3 678 | 3 668 |
| 165 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 305 | 7 698 | 6 015 |
| 166 | php (7.4)| [symfony](https://symfony.com) (5.1) | 2 979 | 3 089 | 3 102 |
| 167 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 2 943 | 2 256 | 1 694 |
| 168 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 820 | 2 975 | 3 027 |
| 169 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 2 647 | 2 161 | 1 056 |
| 170 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 437 | 2 420 | 2 444 |
| 171 | crystal (0.34)| [lucky](https://luckyframework.org) (0.21) | 1 656 | 1 666 | 1 760 |
| 172 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 583 | 1 591 | 1 545 |
| 173 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 508 | 1 479 | 1 441 |
| 174 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 460 | 1 490 | 1 513 |
| 175 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 574 | 469 | 1 299 |
| 176 | php (7.4)| [laravel](https://laravel.com) (7.16) | 359 | 179 | 2 257 |

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

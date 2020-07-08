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

:information_source:  Updated on **2020-07-05** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 166 366 | 170 370 | 170 656 |
| 2 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 162 933 | 194 455 | 196 925 |
| 3 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.14) | 155 746 | 168 434 | 172 012 |
| 4 | javascript (13.14)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (2.1) | 154 994 | 193 145 | 196 775 |
| 5 | nim (1.2)| [whip](https://github.com/mattaylor/whip) (0.2) | 154 666 | 182 000 | 183 630 |
| 6 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.3) | 150 863 | 163 014 | 166 803 |
| 7 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 150 861 | 162 000 | 165 576 |
| 8 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.2) | 150 702 | 162 565 | 165 259 |
| 9 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 149 177 | 161 487 | 165 094 |
| 10 | java (8)| [light-4j](https://doc.networknt.com) (1.6) | 148 319 | 174 959 | 175 112 |
| 11 | go (1.14)| [gearbox](https://gogearbox.com) (1.0) | 147 201 | 158 057 | 162 659 |
| 12 | javascript (13.14)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 145 600 | 181 110 | 183 660 |
| 13 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 143 987 | 170 910 | 172 274 |
| 14 | kotlin (1.3)| [kooby](https://jooby.io) (2.8) | 142 786 | 181 066 | 185 041 |
| 15 | java (8)| [jooby](https://jooby.io) (2.8) | 142 176 | 180 211 | 182 848 |
| 16 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 139 538 | 166 197 | 165 537 |
| 17 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 138 396 | 167 323 | 165 748 |
| 18 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 137 074 | 165 454 | 167 524 |
| 19 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.1) | 134 368 | 161 655 | 160 978 |
| 20 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (0.28) | 131 318 | 154 298 | 152 597 |
| 21 | crystal (0.35)| [kemal](https://kemalcr.com) (0.26) | 129 216 | 152 674 | 152 080 |
| 22 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 128 845 | 151 493 | 153 193 |
| 23 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 124 252 | 144 594 | 143 053 |
| 24 | rust (1.44)| [actix](https://actix.rs) (2.0) | 122 596 | 133 514 | 133 087 |
| 25 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 111 827 | 147 152 | 168 666 |
| 26 | c (99)| [kore](https://kore.io) (3.3) | 105 247 | 114 734 | 132 947 |
| 27 | java (8)| [act](https://actframework.org) (1.9) | 103 752 | 133 027 | 135 349 |
| 28 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 103 133 | 104 595 | 108 837 |
| 29 | go (1.14)| [clevergo](https://clevergo.tech) (0.3) | 102 264 | 104 310 | 108 233 |
| 30 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 101 984 | 104 445 | 108 987 |
| 31 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 101 251 | 102 895 | 107 202 |
| 32 | go (1.14)| [fiber](https://gofiber.io) (1.12) | 100 691 | 145 302 | 156 125 |
| 33 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 99 034 | 104 578 | 108 110 |
| 34 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 97 249 | 97 472 | 102 062 |
| 35 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 96 522 | 101 957 | 105 220 |
| 36 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 96 366 | 97 926 | 101 858 |
| 37 | go (1.14)| [violetear](https://violetear.org) (7.0) | 95 407 | 95 857 | 99 951 |
| 38 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 93 786 | 94 711 | 98 458 |
| 39 | fsharp (4.7)| [frank](https://github.com/frank-fs/frank) (6.1) | 92 954 | 105 084 | 107 589 |
| 40 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 92 795 | 92 508 | 96 797 |
| 41 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 91 805 | 98 630 | 100 773 |
| 42 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 90 794 | 89 459 | 94 101 |
| 43 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 89 966 | 102 241 | 105 080 |
| 44 | go (1.14)| [beego](https://beego.me) (1.12) | 88 895 | 94 630 | 97 754 |
| 45 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.9) | 86 269 | 104 448 | 105 401 |
| 46 | fsharp (4.7)| [falco](https://github.com/pimbrouwers/Falco) (1.2) | 85 182 | 95 680 | 97 446 |
| 47 | javascript (13.14)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 84 491 | 95 038 | 94 978 |
| 48 | go (1.14)| [air](https://github.com/aofei/air) (0.18) | 79 254 | 80 617 | 84 028 |
| 49 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 79 092 | 86 960 | 88 368 |
| 50 | javascript (13.14)| [0http](https://github.com/jkyberneees/0http) (2.5) | 79 039 | 92 520 | 92 205 |
| 51 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.1) | 75 703 | 76 694 | 68 914 |
| 52 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 75 684 | 77 408 | 75 828 |
| 53 | javascript (13.14)| [polka](https://github.com/lukeed/polka) (0.5) | 75 487 | 82 577 | 81 084 |
| 54 | javascript (13.14)| [restana](https://github.com/jkyberneees/ana) (4.6) | 74 177 | 85 351 | 84 696 |
| 55 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 74 089 | 91 490 | 94 882 |
| 56 | php (7.4)| [hyperf](https://www.hyperf.io) (1.1) | 73 882 | 88 424 | 95 937 |
| 57 | javascript (13.14)| [rayo](https://rayo.js.org) (1.3) | 73 448 | 81 068 | 79 027 |
| 58 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 72 480 | 103 750 | 112 990 |
| 59 | swift (5.2)| [perfect](https://perfect.org) (3.1) | 72 005 | 81 793 | 87 295 |
| 60 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 71 355 | 84 015 | 81 790 |
| 61 | go (1.14)| [gf](https://goframe.org) (1.13) | 70 470 | 78 038 | 79 965 |
| 62 | java (8)| [javalin](https://javalin.io) (3.9) | 68 915 | 75 288 | 74 728 |
| 63 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 66 912 | 74 275 | 75 044 |
| 64 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 66 493 | 87 133 | 90 464 |
| 65 | javascript (13.14)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 65 237 | 71 231 | 69 922 |
| 66 | fsharp (4.7)| [saturn](https://saturnframework.org) (0.14) | 64 653 | 61 290 | 54 812 |
| 67 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 62 913 | 70 502 | 69 541 |
| 68 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.7) | 61 750 | 67 550 | 66 866 |
| 69 | javascript (13.14)| [fastify](https://fastify.io) (2.15) | 61 446 | 67 773 | 64 337 |
| 70 | javascript (13.14)| [foxify](https://foxify.js.org) (0.1) | 60 182 | 65 032 | 63 488 |
| 71 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 59 064 | 59 643 | 63 551 |
| 72 | java (8)| [micronaut](https://micronaut.io) (1.2) | 56 698 | 65 384 | 65 954 |
| 73 | javascript (13.14)| [koa](https://koajs.com) (2.13) | 56 498 | 60 805 | 59 296 |
| 74 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 55 553 | 179 732 | 178 645 |
| 75 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 55 512 | 61 984 | 66 321 |
| 76 | fsharp (4.7)| [websharper](https://websharper.com) (4.6) | 55 126 | 57 221 | 54 483 |
| 77 | javascript (13.14)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 54 353 | 58 296 | 56 489 |
| 78 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 54 007 | 60 588 | 60 076 |
| 79 | javascript (13.14)| [nestjs-fastify](https://nestjs.com) (7.3) | 53 572 | 59 775 | 60 319 |
| 80 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 52 720 | 54 634 | 54 586 |
| 81 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.26) | 52 014 | 59 960 | 59 694 |
| 82 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 50 712 | 51 773 | 51 152 |
| 83 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 50 424 | 60 066 | 59 888 |
| 84 | javascript (13.14)| [express](https://expressjs.com) (4.17) | 50 097 | 52 654 | 51 388 |
| 85 | javascript (13.14)| [feathersjs](https://feathersjs.com) (4.5) | 49 438 | 52 727 | 51 544 |
| 86 | swift (5.2)| [kitura](https://kitura.io) (2.9) | 49 194 | 49 682 | 49 618 |
| 87 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 49 059 | 53 667 | 54 947 |
| 88 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 48 984 | 68 171 | 74 114 |
| 89 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 47 269 | 47 570 | 47 901 |
| 90 | rust (1.44)| [gotham](https://gotham.rs) (0.4) | 46 464 | 53 401 | 56 104 |
| 91 | javascript (13.14)| [moleculer](https://moleculer.services) (0.14) | 46 205 | 48 863 | 47 809 |
| 92 | swift (5.2)| [vapor](https://vapor.codes) (4.14) | 46 142 | 48 033 | 48 149 |
| 93 | rust (1.44)| [nickel](https://nickel-org.github.io) (0.11) | 46 104 | 47 730 | 45 973 |
| 94 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 46 053 | 49 807 | 49 882 |
| 95 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 45 192 | 50 675 | 51 599 |
| 96 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 44 246 | 44 904 | 45 192 |
| 97 | python (3.8)| [hug](https://hug.rest) (2.6) | 43 731 | 46 526 | 46 397 |
| 98 | python (3.8)| [starlette](https://starlette.io) (0.13) | 42 782 | 46 196 | 46 306 |
| 99 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 42 343 | 48 621 | 47 712 |
| 100 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.6) | 41 783 | 47 067 | 46 821 |
| 101 | php (7.4)| [imi](https://imiphp.com) (1.2) | 40 107 | 45 270 | 46 435 |
| 102 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 39 665 | 40 064 | 40 542 |
| 103 | javascript (13.14)| [hapi](https://hapijs.com) (19.1) | 39 259 | 42 157 | 41 859 |
| 104 | javascript (13.14)| [nestjs-express](https://nestjs.com) (7.3) | 38 288 | 40 176 | 39 056 |
| 105 | javascript (13.14)| [restify](https://restify.com) (8.5) | 36 925 | 39 741 | 38 974 |
| 106 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.1) | 35 761 | 36 570 | 35 621 |
| 107 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 34 487 | 33 041 | 30 440 |
| 108 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 33 926 | 38 611 | 38 372 |
| 109 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 33 844 | 34 765 | 35 093 |
| 110 | scala (2.13)| [play](https://playframework.com) (2.8) | 33 423 | 35 260 | 35 569 |
| 111 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 31 650 | 32 821 | 32 811 |
| 112 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.15) | 30 711 | 28 904 | 26 149 |
| 113 | dart (2.8)| [aqueduct](https://aqueduct.io) (3.3) | 30 308 | 30 390 | 30 315 |
| 114 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 29 932 | 31 190 | 33 755 |
| 115 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 29 696 | 28 688 | 27 658 |
| 116 | fsharp (4.7)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 29 011 | 27 787 | 27 165 |
| 117 | nim (1.2)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 27 919 | 23 469 | 21 564 |
| 118 | php (7.4)| [swoft](https://swoft.org) (2.0) | 27 651 | 34 614 | 35 108 |
| 119 | python (3.8)| [responder](https://python-responder.org) (2.0) | 27 425 | 29 563 | 29 484 |
| 120 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 27 068 | 33 334 | 33 971 |
| 121 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.58) | 26 821 | 29 643 | 29 502 |
| 122 | nim (1.2)| [akane](https://github.com/Ethosa/akane) (0.1) | 26 533 | 23 153 | 20 742 |
| 123 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 25 367 | 31 478 | 31 699 |
| 124 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 24 932 | 27 291 | 26 022 |
| 125 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 24 563 | 25 516 | 24 934 |
| 126 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 23 649 | 26 511 | 26 535 |
| 127 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 23 613 | 27 270 | 25 947 |
| 128 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 552 | 24 381 | 24 337 |
| 129 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.33) | 23 391 | 25 708 | 25 042 |
| 130 | rust (1.44)| [iron](https://ironframework.io) (0.6) | 23 210 | 23 326 | 23 323 |
| 131 | javascript (13.14)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 466 | 21 590 | 20 302 |
| 132 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 21 875 | 22 829 | 22 605 |
| 133 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 21 749 | 24 209 | 23 799 |
| 134 | go (1.14)| [macaron](https://go-macaron.com) (1.3) | 20 225 | 22 699 | 23 023 |
| 135 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 19 810 | 20 056 | 20 119 |
| 136 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 19 028 | 20 420 | 19 915 |
| 137 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 19 014 | 20 237 | 19 927 |
| 138 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 17 765 | 18 582 | 18 236 |
| 139 | java (8)| [blade](https://lets-blade.com) (2.0) | 14 939 | 19 143 | 18 582 |
| 140 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 14 020 | 14 454 | 14 615 |
| 141 | dart (2.8)| [start](https://github.com/lvivski/start) (0.4) | 13 671 | 13 420 | 12 563 |
| 142 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 12 918 | 13 229 | 13 111 |
| 143 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (0.2) | 12 696 | 12 466 | 11 855 |
| 144 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 12 585 | 12 847 | 12 909 |
| 145 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 12 062 | 12 211 | 12 311 |
| 146 | javascript (13.14)| [sails](https://sailsjs.com) (1.2) | 11 681 | 12 189 | 12 150 |
| 147 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.12) | 11 322 | 11 318 | 10 558 |
| 148 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 11 146 | 11 506 | 11 525 |
| 149 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.0) | 9 997 | 10 602 | 10 668 |
| 150 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 670 | 9 743 | 9 694 |
| 151 | swift (5.2)| [swifter](https://github.com/httpswift/swifter) (1.4) | 9 625 | 9 690 | 9 782 |
| 152 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.4) | 9 554 | 9 352 | 9 228 |
| 153 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 383 | 9 314 | 9 285 |
| 154 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 8 812 | 8 742 | 8 540 |
| 155 | pony (0.35)| [jennet](https://github.com/Theodus/jennet) (0.1) | 8 654 | 16 849 | 15 397 |
| 156 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.2) | 7 901 | 7 818 | 7 717 |
| 157 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 7 352 | 7 304 | 7 290 |
| 158 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 7 280 | 7 349 | 7 362 |
| 159 | java (8)| [struts2](https://struts.apache.org) (2.5) | 7 187 | 8 647 | 8 340 |
| 160 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 006 | 6 585 | 6 166 |
| 161 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 6 728 | 6 600 | 6 591 |
| 162 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 6 004 | 5 898 | 5 911 |
| 163 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 5 796 | 5 696 | 5 704 |
| 164 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 5 733 | 5 649 | 5 658 |
| 165 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 5 730 | 5 660 | 5 667 |
| 166 | php (7.4)| [ice](https://iceframework.org) (1.5) | 4 946 | 4 923 | 4 956 |
| 167 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.4) | 4 874 | 4 838 | 4 872 |
| 168 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.0) | 4 552 | 4 541 | 4 589 |
| 169 | php (7.4)| [slim](https://slimframework.com) (4.5) | 3 863 | 3 878 | 3 922 |
| 170 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 3 641 | 3 665 | 3 734 |
| 171 | php (7.4)| [lumen](https://lumen.laravel.com) (7.2) | 3 493 | 3 524 | 3 576 |
| 172 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 3 445 | 3 480 | 3 555 |
| 173 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 322 | 3 138 | 3 141 |
| 174 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 278 | 7 895 | 6 089 |
| 175 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.4) | 3 107 | 3 160 | 3 197 |
| 176 | perl (5.32)| [dancer2](https://perldancer.org) (2.0) | 2 882 | 1 554 | 1 429 |
| 177 | php (7.4)| [symfony](https://symfony.com) (5.1) | 2 742 | 2 775 | 2 821 |
| 178 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 2 728 | 1 822 | 1 795 |
| 179 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 508 | 2 538 | 2 562 |
| 180 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 419 | 2 417 | 2 409 |
| 181 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 1 971 | 2 125 | 2 107 |
| 182 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 598 | 1 624 | 1 604 |
| 183 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 529 | 1 490 | 1 473 |
| 184 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 338 | 1 356 | 1 385 |
| 185 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 717 | 462 | 828 |
| 186 | php (7.4)| [laravel](https://laravel.com) (7.18) | 418 | 162 | 2 991 |
| 187 | crystal (0.35)| [lucky](https://luckyframework.org) (0.23) | 316 | 344 | 329 |

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

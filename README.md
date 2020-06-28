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

:information_source:  Updated on **2020-08-06** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 167 687 | 171 413 | 172 108 |
| 2 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 163 348 | 194 799 | 196 969 |
| 3 | go (1.14)| [fiber](https://gofiber.io) (1.13) | 161 393 | 172 412 | 175 606 |
| 4 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.5) | 157 177 | 171 116 | 175 690 |
| 5 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.15) | 155 891 | 168 589 | 172 120 |
| 6 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 154 165 | 169 320 | 174 094 |
| 7 | javascript (14.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 154 028 | 196 301 | 200 982 |
| 8 | nim (1.2)| [whip](https://github.com/mattaylor/whip) (0.2) | 152 326 | 179 652 | 181 107 |
| 9 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 149 822 | 161 272 | 164 973 |
| 10 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.2) | 146 966 | 159 894 | 164 312 |
| 11 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 142 883 | 170 582 | 170 382 |
| 12 | java (11)| [jooby](https://jooby.io) (2.8) | 141 464 | 185 609 | 190 176 |
| 13 | kotlin (1.3)| [kooby](https://jooby.io) (2.8) | 139 571 | 181 294 | 186 057 |
| 14 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 139 415 | 167 287 | 166 543 |
| 15 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 137 983 | 165 408 | 163 673 |
| 16 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 137 889 | 173 832 | 178 946 |
| 17 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.3) | 136 456 | 163 188 | 162 035 |
| 18 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 135 687 | 165 076 | 167 225 |
| 19 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (0.28) | 130 188 | 149 298 | 153 046 |
| 20 | crystal (0.35)| [kemal](https://kemalcr.com) (0.26) | 130 074 | 151 565 | 150 099 |
| 21 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 127 553 | 148 481 | 150 413 |
| 22 | rust (1.45)| [actix](https://actix.rs) (2.0) | 125 052 | 134 080 | 133 326 |
| 23 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 124 130 | 143 971 | 141 380 |
| 24 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 112 272 | 155 541 | 152 611 |
| 25 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 108 698 | 111 169 | 116 843 |
| 26 | go (1.14)| [clevergo](https://clevergo.tech) (0.3) | 107 992 | 110 563 | 115 685 |
| 27 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 105 529 | 158 539 | 174 480 |
| 28 | c (99)| [kore](https://kore.io) (3.3) | 103 639 | 109 017 | 137 654 |
| 29 | go (1.14)| [gearbox](https://gogearbox.com) (1.1) | 103 471 | 152 999 | 166 295 |
| 30 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 103 219 | 104 640 | 109 085 |
| 31 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 102 670 | 104 395 | 108 246 |
| 32 | java (11)| [act](https://actframework.org) (1.9) | 101 599 | 130 515 | 132 140 |
| 33 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 99 998 | 109 751 | 115 103 |
| 34 | go (1.14)| [violetear](https://violetear.org) (7.0) | 98 851 | 102 374 | 107 708 |
| 35 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 97 601 | 98 556 | 102 327 |
| 36 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 96 912 | 103 694 | 108 517 |
| 37 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 96 824 | 93 468 | 94 702 |
| 38 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 96 014 | 102 798 | 105 358 |
| 39 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 95 152 | 96 159 | 101 928 |
| 40 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 94 415 | 94 939 | 99 208 |
| 41 | fsharp (4.7)| [frank](https://github.com/frank-fs/frank) (6.1) | 93 277 | 107 477 | 110 448 |
| 42 | fsharp (4.7)| [falco](https://github.com/pimbrouwers/Falco) (1.2) | 92 545 | 105 200 | 108 074 |
| 43 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 92 288 | 103 057 | 106 059 |
| 44 | go (1.14)| [beego](https://beego.me) (1.12) | 91 337 | 99 619 | 98 269 |
| 45 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 90 629 | 90 615 | 99 683 |
| 46 | javascript (14.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 88 580 | 100 055 | 101 405 |
| 47 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.9) | 85 462 | 104 694 | 106 107 |
| 48 | javascript (14.7)| [polka](https://github.com/lukeed/polka) (0.5) | 80 400 | 86 943 | 85 842 |
| 49 | javascript (14.7)| [0http](https://github.com/jkyberneees/0http) (2.5) | 79 808 | 95 537 | 92 069 |
| 50 | go (1.14)| [air](https://github.com/aofei/air) (0.19) | 78 961 | 80 203 | 83 725 |
| 51 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 78 680 | 85 199 | 87 951 |
| 52 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 76 656 | 93 036 | 98 657 |
| 53 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.1) | 75 652 | 76 685 | 68 673 |
| 54 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 75 406 | 77 525 | 74 847 |
| 55 | javascript (14.7)| [rayo](https://rayo.js.org) (1.3) | 72 841 | 83 563 | 81 045 |
| 56 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 72 387 | 83 842 | 81 697 |
| 57 | java (11)| [javalin](https://javalin.io) (3.9) | 71 991 | 78 140 | 77 932 |
| 58 | swift (5.2)| [perfect](https://perfect.org) (3.1) | 71 835 | 81 898 | 87 430 |
| 59 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.13) | 71 685 | 103 305 | 113 051 |
| 60 | php (7.4)| [hyperf](https://hyperf.io) (2.0) | 71 520 | 89 980 | 95 132 |
| 61 | go (1.14)| [gf](https://goframe.org) (1.13) | 71 263 | 78 113 | 80 266 |
| 62 | javascript (14.7)| [fastify](https://fastify.io) (3.1) | 69 627 | 75 186 | 72 184 |
| 63 | javascript (14.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 67 871 | 74 978 | 73 748 |
| 64 | javascript (14.7)| [restana](https://github.com/jkyberneees/ana) (4.7) | 67 368 | 79 662 | 78 915 |
| 65 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 66 478 | 75 165 | 75 566 |
| 66 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 65 912 | 86 291 | 89 913 |
| 67 | fsharp (4.7)| [saturn](https://saturnframework.org) (0.14) | 65 604 | 62 032 | 55 089 |
| 68 | java (11)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 64 618 | 71 898 | 71 675 |
| 69 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.8) | 62 182 | 67 595 | 67 400 |
| 70 | javascript (14.7)| [foxify](https://foxify.js.org) (0.1) | 62 035 | 67 154 | 67 142 |
| 71 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 61 557 | 63 325 | 66 455 |
| 72 | javascript (14.7)| [koa](https://koajs.com) (2.13) | 60 361 | 65 341 | 64 025 |
| 73 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 57 266 | 62 268 | 64 922 |
| 74 | java (11)| [micronaut](https://micronaut.io) (1.2) | 57 105 | 66 008 | 65 265 |
| 75 | fsharp (4.7)| [websharper](https://websharper.com) (4.6) | 56 216 | 57 279 | 54 791 |
| 76 | javascript (14.7)| [nestjs-fastify](https://nestjs.com) (7.4) | 54 510 | 58 066 | 58 673 |
| 77 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 54 102 | 58 688 | 56 820 |
| 78 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 53 486 | 55 248 | 55 393 |
| 79 | javascript (14.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 53 069 | 57 486 | 55 791 |
| 80 | java (11)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 51 271 | 58 787 | 58 487 |
| 81 | rust (1.45)| [nickel](https://nickel-org.github.io) (0.11) | 49 654 | 48 297 | 47 748 |
| 82 | swift (5.2)| [kitura](https://kitura.io) (2.9) | 49 346 | 50 133 | 50 097 |
| 83 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 49 114 | 50 035 | 49 767 |
| 84 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.27) | 48 161 | 55 525 | 55 294 |
| 85 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 47 228 | 51 188 | 51 818 |
| 86 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 47 062 | 48 013 | 48 323 |
| 87 | rust (1.45)| [gotham](https://gotham.rs) (0.4) | 46 724 | 53 252 | 55 457 |
| 88 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 46 457 | 49 978 | 49 606 |
| 89 | swift (5.2)| [vapor](https://vapor.codes) (4.27) | 46 299 | 48 477 | 48 352 |
| 90 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 45 957 | 67 943 | 72 658 |
| 91 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 44 061 | 45 027 | 45 232 |
| 92 | python (3.8)| [hug](https://hug.rest) (2.6) | 43 553 | 46 229 | 46 389 |
| 93 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 42 716 | 48 797 | 47 827 |
| 94 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 42 046 | 48 111 | 47 823 |
| 95 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.6) | 41 203 | 46 894 | 46 759 |
| 96 | php (7.4)| [imi](https://imiphp.com) (1.2) | 41 149 | 46 231 | 47 917 |
| 97 | javascript (14.7)| [moleculer](https://moleculer.services) (0.14) | 40 160 | 41 906 | 41 807 |
| 98 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 39 948 | 40 488 | 40 910 |
| 99 | javascript (14.7)| [restify](https://restify.com) (8.5) | 39 594 | 40 540 | 42 807 |
| 100 | python (3.8)| [starlette](https://starlette.io) (0.13) | 39 381 | 44 130 | 43 838 |
| 101 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 36 005 | 39 497 | 39 264 |
| 102 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.1) | 35 879 | 36 331 | 35 319 |
| 103 | javascript (14.7)| [hapi](https://hapijs.com) (19.2) | 35 261 | 37 581 | 37 959 |
| 104 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 35 119 | 33 157 | 29 685 |
| 105 | scala (2.13)| [play](https://playframework.com) (2.8) | 33 978 | 36 458 | 36 422 |
| 106 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 32 895 | 33 806 | 33 902 |
| 107 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 31 588 | 32 641 | 32 281 |
| 108 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.15) | 30 648 | 29 095 | 26 566 |
| 109 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 29 952 | 29 327 | 28 164 |
| 110 | fsharp (4.7)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 28 473 | 27 953 | 27 837 |
| 111 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 28 285 | 29 277 | 31 285 |
| 112 | dart (2.9)| [aqueduct](https://aqueduct.io) (3.3) | 27 637 | 27 605 | 27 693 |
| 113 | php (7.4)| [swoft](https://swoft.org) (2.0) | 27 519 | 33 857 | 34 708 |
| 114 | nim (1.2)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 27 234 | 23 234 | 21 258 |
| 115 | nim (1.2)| [akane](https://github.com/Ethosa/akane) (0.1) | 26 452 | 22 850 | 20 726 |
| 116 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 26 106 | 31 698 | 32 537 |
| 117 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.6) | 26 078 | 28 621 | 28 450 |
| 118 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 25 872 | 29 482 | 28 604 |
| 119 | python (3.8)| [responder](https://python-responder.org) (2.0) | 25 826 | 29 654 | 29 477 |
| 120 | javascript (14.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 25 762 | 24 974 | 23 662 |
| 121 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 25 516 | 31 629 | 31 882 |
| 122 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 24 491 | 29 157 | 29 391 |
| 123 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 24 332 | 25 638 | 25 234 |
| 124 | rust (1.45)| [iron](https://ironframework.io) (0.6) | 24 156 | 24 226 | 24 224 |
| 125 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 23 795 | 28 690 | 27 491 |
| 126 | javascript (14.7)| [express](https://expressjs.com) (4.17) | 23 777 | 25 199 | 24 733 |
| 127 | javascript (14.7)| [feathersjs](https://feathersjs.com) (4.5) | 23 678 | 25 317 | 25 058 |
| 128 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 558 | 24 731 | 24 290 |
| 129 | php (7.4)| [slim-swoole](https://slimframework.com) (4.5) | 23 491 | 27 761 | 28 484 |
| 130 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 23 467 | 26 145 | 26 133 |
| 131 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 22 008 | 22 949 | 22 764 |
| 132 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 21 928 | 23 044 | 22 629 |
| 133 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 21 758 | 24 544 | 23 046 |
| 134 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.34) | 21 501 | 26 936 | 26 241 |
| 135 | go (1.14)| [macaron](https://go-macaron.com) (1.3) | 20 414 | 23 026 | 23 193 |
| 136 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 19 517 | 20 584 | 20 083 |
| 137 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 19 318 | 20 991 | 20 614 |
| 138 | javascript (14.7)| [nestjs-express](https://nestjs.com) (7.4) | 18 421 | 19 860 | 19 488 |
| 139 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 17 059 | 18 462 | 18 593 |
| 140 | java (11)| [blade](https://lets-blade.com) (2.0) | 14 806 | 16 857 | 16 619 |
| 141 | dart (2.9)| [start](https://github.com/lvivski/start) (0.4) | 14 197 | 13 860 | 12 993 |
| 142 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 14 126 | 14 645 | 14 637 |
| 143 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.5) | 12 797 | 13 066 | 13 088 |
| 144 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 12 751 | 12 972 | 13 014 |
| 145 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (0.2) | 12 623 | 12 463 | 11 872 |
| 146 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 12 596 | 12 900 | 12 884 |
| 147 | java (11)| [struts2](https://struts.apache.org) (2.5) | 12 079 | 13 085 | 12 930 |
| 148 | ruby (2.7)| [grape](https://ruby-grape.org) (1.4) | 11 544 | 11 850 | 11 834 |
| 149 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.13) | 10 324 | 11 147 | 10 618 |
| 150 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 9 908 | 10 647 | 10 698 |
| 151 | swift (5.2)| [swifter](https://github.com/httpswift/swifter) (1.4) | 9 744 | 9 810 | 9 822 |
| 152 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.5) | 9 729 | 10 389 | 10 510 |
| 153 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.4) | 9 612 | 9 369 | 9 185 |
| 154 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 444 | 9 809 | 9 529 |
| 155 | pony (0.36)| [jennet](https://github.com/Theodus/jennet) (0.1) | 8 825 | 17 361 | 15 826 |
| 156 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 8 766 | 8 736 | 8 628 |
| 157 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 8 157 | 8 122 | 7 915 |
| 158 | python (3.8)| [django](https://djangoproject.com) (3.1) | 8 037 | 8 024 | 7 949 |
| 159 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.3) | 7 874 | 7 764 | 7 631 |
| 160 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 7 572 | 7 693 | 7 279 |
| 161 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 7 383 | 7 752 | 7 581 |
| 162 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 6 701 | 6 570 | 6 578 |
| 163 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 6 642 | 6 523 | 6 147 |
| 164 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 6 007 | 5 923 | 5 905 |
| 165 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 5 721 | 5 636 | 5 644 |
| 166 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 5 698 | 5 652 | 5 659 |
| 167 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 5 668 | 5 590 | 5 612 |
| 168 | javascript (14.7)| [sails](https://sailsjs.com) (1.2) | 5 512 | 5 591 | 5 477 |
| 169 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.2) | 5 108 | 5 060 | 5 073 |
| 170 | php (7.4)| [ice](https://iceframework.org) (1.5) | 4 977 | 4 943 | 4 982 |
| 171 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.4) | 4 897 | 4 886 | 4 856 |
| 172 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 4 529 | 4 508 | 4 552 |
| 173 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 3 871 | 2 198 | 1 639 |
| 174 | php (7.4)| [slim](https://slimframework.com) (4.5) | 3 846 | 3 859 | 3 920 |
| 175 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 3 576 | 3 594 | 3 659 |
| 176 | php (7.4)| [lumen](https://lumen.laravel.com) (7.2) | 3 479 | 3 500 | 3 601 |
| 177 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 3 406 | 3 426 | 3 498 |
| 178 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 351 | 8 196 | 6 254 |
| 179 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 246 | 3 131 | 3 139 |
| 180 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.4) | 3 091 | 3 139 | 3 186 |
| 181 | php (7.4)| [symfony](https://symfony.com) (5.1) | 2 730 | 2 769 | 2 815 |
| 182 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 496 | 2 539 | 2 566 |
| 183 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 404 | 2 413 | 2 372 |
| 184 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 071 | 2 112 | 1 850 |
| 185 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 591 | 1 616 | 1 592 |
| 186 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 535 | 1 502 | 1 478 |
| 187 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 332 | 1 355 | 1 368 |
| 188 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 024 | 1 046 | 1 122 |
| 189 | perl (5.32)| [dancer2](https://perldancer.org) (2.0) | 935 | 757 | 727 |
| 190 | php (7.4)| [laravel](https://laravel.com) (7.23) | 363 | 177 | 2 438 |
| 191 | crystal (0.35)| [lucky](https://luckyframework.org) (0.23) | 314 | 338 | 327 |

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


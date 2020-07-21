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

:information_source:  Updated on **2020-07-21** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 163 951 | 170 506 | 170 629 |
| 2 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 162 239 | 192 708 | 195 038 |
| 3 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.15) | 152 854 | 165 482 | 169 358 |
| 4 | nim (1.2)| [whip](https://github.com/mattaylor/whip) (0.2) | 152 670 | 179 457 | 181 285 |
| 5 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.4) | 150 823 | 163 079 | 166 661 |
| 6 | javascript (13.14)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (2.2) | 149 742 | 190 337 | 193 601 |
| 7 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.2) | 148 837 | 160 366 | 162 289 |
| 8 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 146 799 | 159 367 | 162 729 |
| 9 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 146 631 | 158 422 | 162 497 |
| 10 | javascript (13.14)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 144 992 | 177 223 | 179 662 |
| 11 | go (1.14)| [gearbox](https://gogearbox.com) (1.0) | 143 686 | 155 699 | 158 922 |
| 12 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 142 856 | 170 286 | 171 102 |
| 13 | java (11)| [jooby](https://jooby.io) (2.8) | 140 324 | 181 672 | 187 321 |
| 14 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 137 783 | 154 275 | 178 755 |
| 15 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 135 922 | 162 685 | 162 666 |
| 16 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 134 946 | 159 983 | 158 930 |
| 17 | kotlin (1.3)| [kooby](https://jooby.io) (2.8) | 134 333 | 175 691 | 183 367 |
| 18 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 133 670 | 162 207 | 163 840 |
| 19 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.3) | 133 257 | 160 022 | 158 198 |
| 20 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 131 519 | 171 769 | 175 925 |
| 21 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 127 455 | 147 913 | 149 220 |
| 22 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (0.28) | 126 962 | 151 352 | 149 777 |
| 23 | crystal (0.35)| [kemal](https://kemalcr.com) (0.26) | 126 517 | 149 778 | 148 437 |
| 24 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 121 529 | 142 544 | 139 065 |
| 25 | rust (1.45)| [actix](https://actix.rs) (2.0) | 120 313 | 129 840 | 130 523 |
| 26 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 103 283 | 157 099 | 171 860 |
| 27 | c (99)| [kore](https://kore.io) (3.3) | 102 607 | 134 424 | 118 562 |
| 28 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 102 177 | 102 827 | 107 356 |
| 29 | go (1.14)| [clevergo](https://clevergo.tech) (0.3) | 101 549 | 102 266 | 107 193 |
| 30 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 101 126 | 101 677 | 106 257 |
| 31 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 100 222 | 101 093 | 105 724 |
| 32 | java (11)| [act](https://actframework.org) (1.9) | 100 035 | 129 506 | 130 626 |
| 33 | go (1.14)| [fiber](https://gofiber.io) (1.12) | 98 672 | 141 336 | 151 785 |
| 34 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 98 206 | 102 334 | 106 832 |
| 35 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 96 828 | 95 964 | 100 039 |
| 36 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 96 027 | 101 234 | 104 809 |
| 37 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 94 485 | 95 203 | 99 339 |
| 38 | go (1.14)| [violetear](https://violetear.org) (7.0) | 93 503 | 94 883 | 98 672 |
| 39 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 92 584 | 93 474 | 97 188 |
| 40 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 90 807 | 90 174 | 94 410 |
| 41 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 90 598 | 96 519 | 99 148 |
| 42 | fsharp (4.7)| [falco](https://github.com/pimbrouwers/Falco) (1.2) | 90 442 | 102 527 | 105 010 |
| 43 | fsharp (4.7)| [frank](https://github.com/frank-fs/frank) (6.1) | 89 896 | 102 842 | 105 718 |
| 44 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 89 682 | 88 441 | 92 740 |
| 45 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 88 881 | 101 614 | 104 303 |
| 46 | go (1.14)| [beego](https://beego.me) (1.12) | 88 485 | 93 388 | 96 877 |
| 47 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.9) | 84 849 | 102 482 | 104 114 |
| 48 | javascript (13.14)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 82 150 | 94 592 | 95 362 |
| 49 | javascript (13.14)| [0http](https://github.com/jkyberneees/0http) (2.5) | 78 534 | 90 534 | 90 156 |
| 50 | go (1.14)| [air](https://github.com/aofei/air) (0.19) | 78 340 | 79 766 | 82 955 |
| 51 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 77 919 | 84 972 | 86 828 |
| 52 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.1) | 74 981 | 76 670 | 69 095 |
| 53 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 74 350 | 76 362 | 74 145 |
| 54 | javascript (13.14)| [polka](https://github.com/lukeed/polka) (0.5) | 73 857 | 81 658 | 79 745 |
| 55 | javascript (13.14)| [rayo](https://rayo.js.org) (1.3) | 73 002 | 79 766 | 77 932 |
| 56 | javascript (13.14)| [restana](https://github.com/jkyberneees/ana) (4.6) | 72 842 | 84 186 | 83 896 |
| 57 | javascript (13.14)| [fastify](https://fastify.io) (3.1) | 72 753 | 80 173 | 78 912 |
| 58 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.13) | 71 932 | 102 728 | 112 830 |
| 59 | go (1.14)| [gf](https://goframe.org) (1.13) | 70 825 | 76 669 | 78 588 |
| 60 | java (11)| [javalin](https://javalin.io) (3.9) | 70 164 | 76 766 | 75 570 |
| 61 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 69 747 | 89 728 | 96 494 |
| 62 | swift (5.2)| [perfect](https://perfect.org) (3.1) | 69 744 | 80 438 | 86 628 |
| 63 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 69 156 | 81 328 | 78 361 |
| 64 | php (7.4)| [hyperf](https://hyperf.io) (2.0) | 67 767 | 87 381 | 94 104 |
| 65 | java (11)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 66 235 | 72 753 | 72 249 |
| 66 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 66 203 | 72 471 | 73 726 |
| 67 | javascript (13.14)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 64 783 | 71 146 | 69 752 |
| 68 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 64 385 | 84 717 | 87 624 |
| 69 | fsharp (4.7)| [saturn](https://saturnframework.org) (0.14) | 64 128 | 61 927 | 55 023 |
| 70 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.8) | 60 953 | 66 533 | 65 978 |
| 71 | javascript (13.14)| [foxify](https://foxify.js.org) (0.1) | 60 949 | 66 196 | 63 768 |
| 72 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 57 934 | 59 421 | 62 832 |
| 73 | java (11)| [micronaut](https://micronaut.io) (1.2) | 55 729 | 64 803 | 64 415 |
| 74 | javascript (13.14)| [koa](https://koajs.com) (2.13) | 55 635 | 59 969 | 58 397 |
| 75 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 55 033 | 59 473 | 62 342 |
| 76 | javascript (13.14)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 54 109 | 57 433 | 55 232 |
| 77 | fsharp (4.7)| [websharper](https://websharper.com) (4.6) | 53 343 | 56 311 | 52 643 |
| 78 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 53 283 | 55 578 | 55 672 |
| 79 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 53 117 | 57 813 | 58 356 |
| 80 | java (11)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 50 677 | 58 062 | 58 064 |
| 81 | javascript (13.14)| [nestjs-fastify](https://nestjs.com) (7.3) | 50 491 | 60 931 | 59 880 |
| 82 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.27) | 49 847 | 57 761 | 57 689 |
| 83 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 49 236 | 50 448 | 50 069 |
| 84 | javascript (13.14)| [feathersjs](https://feathersjs.com) (4.5) | 49 230 | 51 974 | 51 168 |
| 85 | javascript (13.14)| [express](https://expressjs.com) (4.17) | 48 944 | 51 663 | 50 624 |
| 86 | swift (5.2)| [kitura](https://kitura.io) (2.9) | 48 931 | 49 958 | 49 650 |
| 87 | rust (1.45)| [nickel](https://nickel-org.github.io) (0.11) | 48 029 | 46 470 | 47 581 |
| 88 | rust (1.45)| [gotham](https://gotham.rs) (0.4) | 46 799 | 53 426 | 56 802 |
| 89 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 45 931 | 46 023 | 47 020 |
| 90 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 45 846 | 49 164 | 49 133 |
| 91 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 45 798 | 67 522 | 73 499 |
| 92 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 45 101 | 53 185 | 53 039 |
| 93 | javascript (13.14)| [moleculer](https://moleculer.services) (0.14) | 45 043 | 47 921 | 47 088 |
| 94 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 44 518 | 50 204 | 49 710 |
| 95 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 44 335 | 45 108 | 45 467 |
| 96 | swift (5.2)| [vapor](https://vapor.codes) (4.25) | 43 733 | 46 479 | 46 827 |
| 97 | python (3.8)| [hug](https://hug.rest) (2.6) | 42 404 | 45 633 | 45 862 |
| 98 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.6) | 41 928 | 47 079 | 46 607 |
| 99 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 41 053 | 47 579 | 46 310 |
| 100 | php (7.4)| [imi](https://imiphp.com) (1.2) | 40 339 | 45 769 | 47 399 |
| 101 | python (3.8)| [starlette](https://starlette.io) (0.13) | 39 438 | 45 241 | 45 063 |
| 102 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 38 839 | 39 849 | 40 192 |
| 103 | javascript (13.14)| [hapi](https://hapijs.com) (19.2) | 38 060 | 40 654 | 40 762 |
| 104 | javascript (13.14)| [nestjs-express](https://nestjs.com) (7.3) | 37 807 | 39 599 | 39 056 |
| 105 | javascript (13.14)| [restify](https://restify.com) (8.5) | 36 576 | 39 181 | 38 857 |
| 106 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.1) | 35 542 | 36 241 | 35 583 |
| 107 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 35 236 | 38 387 | 38 336 |
| 108 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 34 603 | 32 710 | 30 654 |
| 109 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 33 239 | 34 323 | 34 450 |
| 110 | scala (2.13)| [play](https://playframework.com) (2.8) | 32 516 | 35 951 | 35 885 |
| 111 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 31 011 | 32 296 | 31 918 |
| 112 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.15) | 30 334 | 29 291 | 27 016 |
| 113 | dart (2.8)| [aqueduct](https://aqueduct.io) (3.3) | 29 673 | 30 030 | 29 542 |
| 114 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 29 607 | 28 581 | 27 775 |
| 115 | fsharp (4.7)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 29 421 | 28 370 | 27 786 |
| 116 | nim (1.2)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 27 699 | 23 633 | 21 462 |
| 117 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 27 177 | 28 428 | 30 993 |
| 118 | php (7.4)| [swoft](https://swoft.org) (2.0) | 27 172 | 33 765 | 34 272 |
| 119 | nim (1.2)| [akane](https://github.com/Ethosa/akane) (0.1) | 26 517 | 23 224 | 20 982 |
| 120 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.59) | 26 156 | 28 904 | 28 758 |
| 121 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 25 692 | 31 845 | 31 594 |
| 122 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 25 316 | 31 355 | 31 737 |
| 123 | python (3.8)| [responder](https://python-responder.org) (2.0) | 25 191 | 27 412 | 27 314 |
| 124 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 24 276 | 27 822 | 26 888 |
| 125 | rust (1.45)| [iron](https://ironframework.io) (0.6) | 24 172 | 24 194 | 24 142 |
| 126 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 24 008 | 24 884 | 24 746 |
| 127 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 24 007 | 28 873 | 29 297 |
| 128 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.34) | 23 883 | 26 311 | 25 247 |
| 129 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 23 591 | 25 750 | 25 951 |
| 130 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 526 | 24 145 | 24 382 |
| 131 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 22 884 | 26 591 | 25 888 |
| 132 | javascript (13.14)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 011 | 21 148 | 20 124 |
| 133 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 21 914 | 22 529 | 22 476 |
| 134 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 21 739 | 22 687 | 22 647 |
| 135 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 21 731 | 23 872 | 23 211 |
| 136 | go (1.14)| [macaron](https://go-macaron.com) (1.3) | 20 389 | 22 964 | 23 219 |
| 137 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 18 844 | 20 076 | 19 829 |
| 138 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 18 752 | 20 327 | 19 783 |
| 139 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 17 021 | 17 929 | 17 540 |
| 140 | java (11)| [blade](https://lets-blade.com) (2.0) | 14 772 | 17 087 | 16 894 |
| 141 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 14 048 | 14 595 | 14 659 |
| 142 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.5) | 13 439 | 13 756 | 13 500 |
| 143 | dart (2.8)| [start](https://github.com/lvivski/start) (0.4) | 13 345 | 13 142 | 12 270 |
| 144 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (0.2) | 12 769 | 12 527 | 11 937 |
| 145 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 12 487 | 12 806 | 12 811 |
| 146 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 12 001 | 12 294 | 12 297 |
| 147 | java (11)| [struts2](https://struts.apache.org) (2.5) | 11 934 | 12 894 | 12 466 |
| 148 | javascript (13.14)| [sails](https://sailsjs.com) (1.2) | 11 708 | 12 314 | 12 100 |
| 149 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.13) | 10 846 | 11 394 | 10 708 |
| 150 | ruby (2.7)| [grape](https://ruby-grape.org) (1.4) | 10 631 | 10 905 | 10 972 |
| 151 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.4) | 9 852 | 9 611 | 9 461 |
| 152 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 9 752 | 10 584 | 10 646 |
| 153 | swift (5.2)| [swifter](https://github.com/httpswift/swifter) (1.4) | 9 619 | 9 807 | 9 762 |
| 154 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 481 | 9 651 | 9 487 |
| 155 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 244 | 9 294 | 9 207 |
| 156 | pony (0.35)| [jennet](https://github.com/Theodus/jennet) (0.1) | 8 724 | 16 359 | 15 655 |
| 157 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 8 684 | 8 629 | 8 606 |
| 158 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.3) | 7 827 | 7 721 | 7 528 |
| 159 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 7 151 | 7 293 | 7 246 |
| 160 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 7 135 | 7 212 | 7 192 |
| 161 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 6 654 | 6 464 | 6 529 |
| 162 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 6 428 | 6 322 | 5 991 |
| 163 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 5 910 | 5 824 | 5 823 |
| 164 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 5 637 | 5 582 | 5 594 |
| 165 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 5 622 | 5 513 | 5 490 |
| 166 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 5 592 | 5 511 | 5 501 |
| 167 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.2) | 5 086 | 5 030 | 5 072 |
| 168 | php (7.4)| [ice](https://iceframework.org) (1.5) | 4 867 | 4 835 | 4 870 |
| 169 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.4) | 4 828 | 4 792 | 4 829 |
| 170 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 4 472 | 4 455 | 4 488 |
| 171 | php (7.4)| [slim](https://slimframework.com) (4.5) | 3 806 | 3 811 | 3 891 |
| 172 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 3 576 | 3 617 | 3 644 |
| 173 | php (7.4)| [lumen](https://lumen.laravel.com) (7.2) | 3 437 | 3 467 | 3 520 |
| 174 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 3 356 | 3 399 | 3 449 |
| 175 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 320 | 3 126 | 3 105 |
| 176 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 3 291 | 2 792 | 1 877 |
| 177 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 229 | 7 696 | 6 069 |
| 178 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.4) | 3 048 | 3 091 | 3 147 |
| 179 | php (7.4)| [symfony](https://symfony.com) (5.1) | 2 687 | 2 721 | 2 767 |
| 180 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 483 | 2 510 | 2 546 |
| 181 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 420 | 2 404 | 2 399 |
| 182 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 141 | 2 107 | 2 112 |
| 183 | perl (5.32)| [dancer2](https://perldancer.org) (2.0) | 2 120 | 1 712 | 969 |
| 184 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 579 | 1 613 | 1 595 |
| 185 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 524 | 1 495 | 1 467 |
| 186 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 320 | 1 349 | 1 370 |
| 187 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 025 | 1 046 | 1 103 |
| 188 | php (7.4)| [laravel](https://laravel.com) (7.2) | 897 | 162 | 2 266 |
| 189 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 625 | 483 | 593 |
| 190 | crystal (0.35)| [lucky](https://luckyframework.org) (0.23) | 331 | 355 | 343 |

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

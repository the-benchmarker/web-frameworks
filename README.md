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

:information_source:  Updated on **2020-06-30** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 168 958 | 201 281 | 204 364 |
| 2 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.14) | 167 001 | 182 169 | 188 614 |
| 3 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.3) | 160 869 | 175 157 | 180 622 |
| 4 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 160 430 | 174 213 | 180 283 |
| 5 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 159 737 | 174 067 | 179 854 |
| 6 | nim (1.2)| [whip](https://github.com/mattaylor/whip) (0.2) | 159 450 | 187 616 | 188 439 |
| 7 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.2) | 155 401 | 169 336 | 175 413 |
| 8 | javascript (13.14)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (2.1) | 154 939 | 195 659 | 199 398 |
| 9 | javascript (13.14)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 148 191 | 185 243 | 187 226 |
| 10 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 148 084 | 175 210 | 177 381 |
| 11 | kotlin (1.3)| [kooby](https://jooby.io) (2.8) | 143 499 | 187 952 | 192 399 |
| 12 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 142 965 | 171 523 | 170 971 |
| 13 | java (8)| [jooby](https://jooby.io) (2.8) | 142 676 | 181 272 | 186 884 |
| 14 | go (1.14)| [gearbox](https://gogearbox.com) (1.0) | 142 419 | 158 150 | 158 788 |
| 15 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 142 118 | 169 728 | 169 309 |
| 16 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 139 759 | 169 468 | 172 195 |
| 17 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.1) | 138 609 | 166 022 | 164 989 |
| 18 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (0.28) | 134 698 | 157 661 | 155 440 |
| 19 | crystal (0.35)| [kemal](https://kemalcr.com) (0.26) | 134 537 | 157 618 | 155 662 |
| 20 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 132 725 | 155 840 | 157 355 |
| 21 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 126 261 | 147 453 | 145 975 |
| 22 | rust (1.44)| [actix](https://actix.rs) (2.0) | 125 149 | 135 027 | 136 992 |
| 23 | c (99)| [kore](https://kore.io) (3.3) | 114 065 | 105 776 | 131 938 |
| 24 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 111 527 | 149 957 | 171 348 |
| 25 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 110 508 | 112 992 | 119 202 |
| 26 | go (1.14)| [clevergo](https://clevergo.tech) (0.3) | 110 120 | 112 556 | 118 690 |
| 27 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 109 448 | 112 216 | 118 553 |
| 28 | java (8)| [act](https://actframework.org) (1.9) | 106 817 | 136 664 | 138 451 |
| 29 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 104 449 | 162 679 | 175 928 |
| 30 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 103 640 | 106 257 | 111 697 |
| 31 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 102 361 | 95 854 | 106 898 |
| 32 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 100 863 | 184 003 | 184 741 |
| 33 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 100 013 | 102 929 | 106 365 |
| 34 | go (1.14)| [beego](https://beego.me) (1.12) | 97 539 | 102 962 | 108 660 |
| 35 | go (1.14)| [violetear](https://violetear.org) (7.0) | 97 331 | 99 074 | 105 458 |
| 36 | fsharp (4.7)| [frank](https://github.com/frank-fs/frank) (6.1) | 97 239 | 111 580 | 114 910 |
| 37 | go (1.14)| [fiber](https://gofiber.io) (1.12) | 94 558 | 135 334 | 149 927 |
| 38 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 93 762 | 93 971 | 94 850 |
| 39 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 92 886 | 105 561 | 108 411 |
| 40 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 90 410 | 93 873 | 100 360 |
| 41 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.9) | 88 571 | 107 328 | 108 733 |
| 42 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 88 441 | 92 866 | 98 272 |
| 43 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 87 075 | 80 808 | 90 553 |
| 44 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 86 505 | 95 640 | 91 414 |
| 45 | go (1.14)| [air](https://github.com/aofei/air) (0.17) | 85 719 | 88 074 | 92 400 |
| 46 | javascript (13.14)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 85 215 | 97 131 | 97 101 |
| 47 | fsharp (4.7)| [falco](https://github.com/pimbrouwers/Falco) (1.2) | 85 174 | 95 209 | 96 880 |
| 48 | javascript (13.14)| [0http](https://github.com/jkyberneees/0http) (2.5) | 82 633 | 92 420 | 93 295 |
| 49 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 82 266 | 86 930 | 97 678 |
| 50 | fsharp (4.7)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 80 034 | 88 930 | 88 170 |
| 51 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 79 328 | 86 861 | 88 716 |
| 52 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 78 219 | 80 467 | 78 376 |
| 53 | javascript (13.14)| [restana](https://github.com/jkyberneees/ana) (4.6) | 77 599 | 87 276 | 86 565 |
| 54 | javascript (13.14)| [polka](https://github.com/lukeed/polka) (0.5) | 77 386 | 83 805 | 81 914 |
| 55 | php (7.4)| [hyperf](https://www.hyperf.io) (1.1) | 76 456 | 88 561 | 97 972 |
| 56 | go (1.14)| [gf](https://goframe.org) (1.13) | 75 845 | 83 316 | 85 219 |
| 57 | javascript (13.14)| [rayo](https://rayo.js.org) (1.3) | 75 643 | 82 261 | 80 653 |
| 58 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.1) | 75 120 | 76 238 | 68 585 |
| 59 | swift (5.2)| [perfect](https://perfect.org) (3.1) | 73 005 | 85 490 | 90 335 |
| 60 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 71 830 | 106 670 | 118 409 |
| 61 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 71 674 | 92 213 | 98 599 |
| 62 | java (8)| [javalin](https://javalin.io) (3.9) | 71 095 | 76 725 | 77 109 |
| 63 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 69 091 | 75 496 | 76 880 |
| 64 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 68 993 | 80 022 | 79 216 |
| 65 | javascript (13.14)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 67 852 | 74 595 | 73 314 |
| 66 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 66 910 | 88 136 | 91 107 |
| 67 | fsharp (4.7)| [saturn](https://saturnframework.org) (0.14) | 66 559 | 63 350 | 56 485 |
| 68 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.7) | 64 424 | 70 002 | 70 051 |
| 69 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 63 822 | 64 844 | 68 850 |
| 70 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 63 043 | 70 853 | 70 123 |
| 71 | javascript (13.14)| [foxify](https://foxify.js.org) (0.1) | 62 945 | 67 696 | 64 364 |
| 72 | javascript (13.14)| [fastify](https://fastify.io) (2.15) | 62 106 | 67 891 | 65 469 |
| 73 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 58 339 | 64 834 | 69 113 |
| 74 | java (8)| [micronaut](https://micronaut.io) (1.2) | 57 784 | 67 830 | 68 632 |
| 75 | fsharp (4.7)| [websharper](https://websharper.com) (4.6) | 57 497 | 59 883 | 57 117 |
| 76 | javascript (13.14)| [koa](https://koajs.com) (2.13) | 57 493 | 61 613 | 59 681 |
| 77 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 56 418 | 61 858 | 62 322 |
| 78 | javascript (13.14)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 54 845 | 58 369 | 56 481 |
| 79 | javascript (13.14)| [nestjs-fastify](https://nestjs.com) (7.2) | 54 225 | 62 616 | 60 942 |
| 80 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 53 168 | 54 771 | 54 686 |
| 81 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.26) | 52 235 | 60 410 | 61 048 |
| 82 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 51 827 | 60 436 | 60 534 |
| 83 | javascript (13.14)| [feathersjs](https://feathersjs.com) (4.5) | 50 444 | 53 595 | 52 572 |
| 84 | swift (5.2)| [kitura](https://kitura.io) (2.9) | 50 090 | 50 849 | 50 711 |
| 85 | javascript (13.14)| [express](https://expressjs.com) (4.17) | 49 767 | 53 234 | 52 120 |
| 86 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 48 833 | 55 845 | 56 677 |
| 87 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 48 746 | 50 102 | 49 754 |
| 88 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 48 667 | 49 114 | 49 756 |
| 89 | rust (1.44)| [gotham](https://gotham.rs) (0.4) | 47 292 | 51 975 | 54 106 |
| 90 | javascript (13.14)| [moleculer](https://moleculer.services) (0.14) | 46 775 | 49 163 | 48 361 |
| 91 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 46 436 | 66 937 | 72 634 |
| 92 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 45 760 | 52 310 | 52 852 |
| 93 | swift (5.2)| [vapor](https://vapor.codes) (4.14) | 45 702 | 48 534 | 48 352 |
| 94 | rust (1.44)| [nickel](https://nickel-org.github.io) (0.11) | 45 092 | 44 992 | 45 643 |
| 95 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 45 083 | 50 339 | 50 851 |
| 96 | python (3.8)| [hug](https://hug.rest) (2.6) | 43 919 | 47 421 | 48 013 |
| 97 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 43 808 | 44 666 | 44 724 |
| 98 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 43 548 | 49 398 | 48 585 |
| 99 | php (7.4)| [imi](https://imiphp.com) (1.2) | 42 338 | 46 602 | 47 899 |
| 100 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.3) | 41 881 | 48 282 | 48 567 |
| 101 | python (3.8)| [starlette](https://starlette.io) (0.13) | 40 724 | 47 335 | 47 606 |
| 102 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 40 547 | 41 704 | 42 370 |
| 103 | javascript (13.14)| [hapi](https://hapijs.com) (19.1) | 39 907 | 42 837 | 42 008 |
| 104 | javascript (13.14)| [nestjs-express](https://nestjs.com) (7.2) | 37 774 | 39 812 | 39 413 |
| 105 | javascript (13.14)| [restify](https://restify.com) (8.5) | 36 994 | 39 941 | 39 273 |
| 106 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.1) | 36 927 | 37 401 | 36 578 |
| 107 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 35 271 | 39 430 | 39 965 |
| 108 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 34 291 | 35 853 | 36 083 |
| 109 | scala (2.13)| [play](https://playframework.com) (2.8) | 34 237 | 36 268 | 36 707 |
| 110 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 33 814 | 32 972 | 29 874 |
| 111 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 31 987 | 33 101 | 32 870 |
| 112 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.15) | 31 758 | 30 281 | 28 042 |
| 113 | dart (2.8)| [aqueduct](https://aqueduct.io) (3.3) | 30 592 | 30 670 | 30 280 |
| 114 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 30 122 | 29 187 | 28 498 |
| 115 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 29 553 | 30 893 | 32 639 |
| 116 | python (3.8)| [responder](https://python-responder.org) (2.0) | 27 674 | 30 093 | 29 866 |
| 117 | php (7.4)| [swoft](https://swoft.org) (2.0) | 27 653 | 33 270 | 33 901 |
| 118 | nim (1.2)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 27 650 | 23 886 | 21 626 |
| 119 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.58) | 27 605 | 30 093 | 29 886 |
| 120 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 27 206 | 32 765 | 34 308 |
| 121 | nim (1.2)| [akane](https://github.com/Ethosa/akane) (0.1) | 26 558 | 23 543 | 21 154 |
| 122 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 25 889 | 31 300 | 32 344 |
| 123 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 24 811 | 27 926 | 26 747 |
| 124 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 24 559 | 27 468 | 25 964 |
| 125 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 24 405 | 26 304 | 26 238 |
| 126 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 24 242 | 26 772 | 27 067 |
| 127 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 816 | 25 078 | 24 966 |
| 128 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.33) | 23 101 | 26 870 | 25 836 |
| 129 | rust (1.44)| [iron](https://ironframework.io) (0.6) | 23 064 | 23 286 | 23 272 |
| 130 | javascript (13.14)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 879 | 22 169 | 20 706 |
| 131 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 22 152 | 23 124 | 23 159 |
| 132 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 22 051 | 23 394 | 23 228 |
| 133 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 21 860 | 23 976 | 23 815 |
| 134 | go (1.14)| [macaron](https://go-macaron.com) (1.3) | 20 207 | 22 946 | 22 997 |
| 135 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 18 949 | 20 987 | 20 428 |
| 136 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 18 892 | 20 449 | 19 867 |
| 137 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 17 839 | 18 719 | 18 585 |
| 138 | java (8)| [blade](https://lets-blade.com) (2.0) | 15 680 | 19 225 | 18 898 |
| 139 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 14 585 | 15 110 | 15 088 |
| 140 | dart (2.8)| [start](https://github.com/lvivski/start) (0.4) | 14 003 | 13 678 | 12 934 |
| 141 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 13 182 | 13 562 | 13 500 |
| 142 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 12 921 | 13 223 | 13 198 |
| 143 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (0.2) | 12 789 | 12 683 | 11 913 |
| 144 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 12 344 | 12 320 | 12 617 |
| 145 | javascript (13.14)| [sails](https://sailsjs.com) (1.2) | 11 691 | 12 231 | 12 083 |
| 146 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 11 564 | 11 626 | 11 657 |
| 147 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.12) | 10 995 | 11 628 | 10 734 |
| 148 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.0) | 10 209 | 10 936 | 11 043 |
| 149 | swift (5.2)| [swifter](https://github.com/httpswift/swifter) (1.4) | 10 006 | 10 243 | 10 222 |
| 150 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 399 | 9 338 | 9 273 |
| 151 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 246 | 9 509 | 9 579 |
| 152 | pony (0.35)| [jennet](https://github.com/Theodus/jennet) (0.1) | 9 027 | 17 270 | 15 639 |
| 153 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 8 844 | 8 747 | 8 813 |
| 154 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.2) | 8 052 | 7 928 | 7 849 |
| 155 | java (8)| [struts2](https://struts.apache.org) (2.5) | 7 596 | 8 828 | 8 154 |
| 156 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 7 460 | 7 556 | 7 543 |
| 157 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 7 428 | 7 487 | 7 484 |
| 158 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 6 995 | 6 542 | 6 232 |
| 159 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 6 857 | 6 764 | 6 739 |
| 160 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 6 113 | 6 014 | 6 036 |
| 161 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 5 921 | 5 836 | 5 849 |
| 162 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 5 871 | 5 779 | 5 811 |
| 163 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 5 826 | 5 739 | 5 746 |
| 164 | php (7.4)| [ice](https://iceframework.org) (1.5) | 5 074 | 5 042 | 5 081 |
| 165 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.4) | 4 971 | 4 938 | 4 976 |
| 166 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.0) | 4 631 | 4 626 | 4 658 |
| 167 | php (7.4)| [slim](https://slimframework.com) (4.5) | 3 927 | 3 906 | 4 002 |
| 168 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 3 684 | 3 721 | 3 768 |
| 169 | php (7.4)| [lumen](https://lumen.laravel.com) (7.2) | 3 567 | 3 584 | 3 677 |
| 170 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 3 526 | 3 545 | 3 617 |
| 171 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 431 | 3 249 | 3 236 |
| 172 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 338 | 7 883 | 6 043 |
| 173 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.4) | 3 142 | 3 200 | 3 233 |
| 174 | perl (5.32)| [dancer2](https://perldancer.org) (2.0) | 3 059 | 2 218 | 1 347 |
| 175 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 3 021 | 1 818 | 1 604 |
| 176 | php (7.4)| [symfony](https://symfony.com) (5.1) | 2 772 | 2 798 | 2 844 |
| 177 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 541 | 2 590 | 2 601 |
| 178 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 422 | 2 429 | 2 411 |
| 179 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 1 991 | 2 155 | 2 137 |
| 180 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 558 | 1 517 | 1 507 |
| 181 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 473 | 1 485 | 1 472 |
| 182 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 358 | 1 381 | 1 399 |
| 183 | crystal (0.35)| [lucky](https://luckyframework.org) (0.22) | 646 | 664 | 657 |
| 184 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 535 | 481 | 1 483 |
| 185 | php (7.4)| [laravel](https://laravel.com) (7.17) | 320 | 162 | 3 201 |

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

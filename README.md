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

:information_source:  Updated on **2020-07-01** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 167 884 | 200 367 | 203 133 |
| 2 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.2) | 161 840 | 175 264 | 181 637 |
| 3 | go (1.14)| [gearbox](https://gogearbox.com) (1.0) | 158 818 | 172 262 | 178 001 |
| 4 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.14) | 158 790 | 171 060 | 174 867 |
| 5 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 158 152 | 167 345 | 176 845 |
| 6 | nim (1.2)| [whip](https://github.com/mattaylor/whip) (0.2) | 157 886 | 186 217 | 188 235 |
| 7 | javascript (13.14)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (2.1) | 157 813 | 198 243 | 202 322 |
| 8 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 151 900 | 163 588 | 167 788 |
| 9 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 148 176 | 183 675 | 186 600 |
| 10 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 147 488 | 168 810 | 171 224 |
| 11 | javascript (13.14)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 147 449 | 186 128 | 188 355 |
| 12 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.3) | 145 800 | 164 051 | 170 889 |
| 13 | java (8)| [jooby](https://jooby.io) (2.8) | 144 788 | 183 168 | 187 167 |
| 14 | kotlin (1.3)| [kooby](https://jooby.io) (2.8) | 143 378 | 187 780 | 192 533 |
| 15 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 141 291 | 170 486 | 169 812 |
| 16 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 140 412 | 166 598 | 166 001 |
| 17 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 139 123 | 169 702 | 171 845 |
| 18 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.1) | 138 099 | 164 659 | 164 587 |
| 19 | crystal (0.35)| [kemal](https://kemalcr.com) (0.26) | 132 246 | 155 896 | 154 035 |
| 20 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (0.28) | 132 137 | 155 661 | 153 783 |
| 21 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 131 516 | 153 934 | 155 998 |
| 22 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 127 671 | 147 303 | 145 368 |
| 23 | rust (1.44)| [actix](https://actix.rs) (2.0) | 123 808 | 133 861 | 134 123 |
| 24 | c (99)| [kore](https://kore.io) (3.3) | 116 010 | 129 840 | 138 099 |
| 25 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 110 725 | 148 370 | 171 918 |
| 26 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 109 614 | 112 580 | 118 991 |
| 27 | go (1.14)| [fiber](https://gofiber.io) (1.12) | 108 138 | 160 070 | 172 880 |
| 28 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 106 593 | 113 166 | 118 534 |
| 29 | java (8)| [act](https://actframework.org) (1.9) | 105 685 | 135 865 | 137 673 |
| 30 | go (1.14)| [clevergo](https://clevergo.tech) (0.3) | 104 879 | 107 913 | 111 293 |
| 31 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 104 804 | 106 139 | 111 931 |
| 32 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 104 635 | 111 475 | 116 879 |
| 33 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 103 800 | 105 591 | 109 800 |
| 34 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 103 231 | 105 039 | 109 462 |
| 35 | go (1.14)| [violetear](https://violetear.org) (7.0) | 102 011 | 103 838 | 109 986 |
| 36 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 99 965 | 100 990 | 106 825 |
| 37 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 99 461 | 107 353 | 111 205 |
| 38 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 98 674 | 101 086 | 102 079 |
| 39 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 98 273 | 98 092 | 104 321 |
| 40 | go (1.14)| [beego](https://beego.me) (1.12) | 98 262 | 103 706 | 109 195 |
| 41 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 96 318 | 97 331 | 101 839 |
| 42 | fsharp (4.7)| [frank](https://github.com/frank-fs/frank) (6.1) | 92 372 | 105 402 | 108 053 |
| 43 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 91 748 | 104 597 | 107 126 |
| 44 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.9) | 87 770 | 106 512 | 107 463 |
| 45 | javascript (13.14)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 86 792 | 97 675 | 97 656 |
| 46 | fsharp (4.7)| [falco](https://github.com/pimbrouwers/Falco) (1.2) | 85 766 | 96 247 | 97 909 |
| 47 | fsharp (4.7)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 81 786 | 89 131 | 89 913 |
| 48 | javascript (13.14)| [0http](https://github.com/jkyberneees/0http) (2.5) | 81 333 | 93 331 | 93 374 |
| 49 | go (1.14)| [air](https://github.com/aofei/air) (0.17) | 80 548 | 82 161 | 85 701 |
| 50 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 79 922 | 85 113 | 87 832 |
| 51 | javascript (13.14)| [restana](https://github.com/jkyberneees/ana) (4.6) | 77 261 | 87 424 | 87 455 |
| 52 | javascript (13.14)| [polka](https://github.com/lukeed/polka) (0.5) | 77 164 | 84 336 | 82 473 |
| 53 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 76 441 | 152 711 | 182 066 |
| 54 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 75 925 | 77 989 | 75 722 |
| 55 | javascript (13.14)| [rayo](https://rayo.js.org) (1.3) | 74 462 | 81 736 | 80 249 |
| 56 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 73 942 | 105 796 | 115 506 |
| 57 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.1) | 73 290 | 74 165 | 66 891 |
| 58 | swift (5.2)| [perfect](https://perfect.org) (3.1) | 72 820 | 82 019 | 88 068 |
| 59 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 72 594 | 84 976 | 82 166 |
| 60 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 72 194 | 91 702 | 98 349 |
| 61 | go (1.14)| [gf](https://goframe.org) (1.13) | 71 541 | 78 957 | 80 876 |
| 62 | php (7.4)| [hyperf](https://www.hyperf.io) (1.1) | 70 416 | 90 411 | 96 189 |
| 63 | java (8)| [javalin](https://javalin.io) (3.9) | 70 069 | 75 367 | 76 234 |
| 64 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 68 914 | 76 443 | 76 396 |
| 65 | fsharp (4.7)| [saturn](https://saturnframework.org) (0.14) | 66 552 | 63 135 | 55 702 |
| 66 | javascript (13.14)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 66 400 | 73 621 | 72 554 |
| 67 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 66 351 | 89 318 | 92 439 |
| 68 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 65 132 | 72 264 | 71 931 |
| 69 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 63 505 | 64 787 | 69 593 |
| 70 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.7) | 62 485 | 67 771 | 67 648 |
| 71 | javascript (13.14)| [fastify](https://fastify.io) (2.15) | 62 309 | 68 619 | 65 982 |
| 72 | javascript (13.14)| [foxify](https://foxify.js.org) (0.1) | 61 710 | 66 781 | 64 723 |
| 73 | fsharp (4.7)| [websharper](https://websharper.com) (4.6) | 58 310 | 60 070 | 57 271 |
| 74 | java (8)| [micronaut](https://micronaut.io) (1.2) | 58 077 | 67 136 | 67 419 |
| 75 | javascript (13.14)| [koa](https://koajs.com) (2.13) | 57 872 | 62 181 | 60 509 |
| 76 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 57 324 | 64 535 | 68 822 |
| 77 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 56 463 | 61 417 | 61 375 |
| 78 | javascript (13.14)| [nestjs-fastify](https://nestjs.com) (7.2) | 55 381 | 64 975 | 63 556 |
| 79 | javascript (13.14)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 55 337 | 59 713 | 57 803 |
| 80 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.26) | 52 231 | 59 111 | 59 628 |
| 81 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 51 957 | 61 420 | 61 201 |
| 82 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 51 874 | 53 413 | 53 535 |
| 83 | javascript (13.14)| [express](https://expressjs.com) (4.17) | 50 298 | 53 367 | 52 578 |
| 84 | javascript (13.14)| [feathersjs](https://feathersjs.com) (4.5) | 50 221 | 53 594 | 52 598 |
| 85 | swift (5.2)| [kitura](https://kitura.io) (2.9) | 49 591 | 50 204 | 50 136 |
| 86 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 49 111 | 49 681 | 49 263 |
| 87 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 48 024 | 48 386 | 48 947 |
| 88 | rust (1.44)| [gotham](https://gotham.rs) (0.4) | 47 496 | 52 715 | 54 891 |
| 89 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 47 403 | 55 702 | 55 960 |
| 90 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 47 345 | 69 496 | 75 885 |
| 91 | javascript (13.14)| [moleculer](https://moleculer.services) (0.14) | 47 058 | 49 702 | 48 513 |
| 92 | swift (5.2)| [vapor](https://vapor.codes) (4.14) | 46 147 | 49 169 | 48 884 |
| 93 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 45 251 | 51 111 | 51 827 |
| 94 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 44 270 | 47 500 | 47 952 |
| 95 | rust (1.44)| [nickel](https://nickel-org.github.io) (0.11) | 43 883 | 43 386 | 44 912 |
| 96 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 43 702 | 49 261 | 48 204 |
| 97 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 43 528 | 44 533 | 44 846 |
| 98 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.6) | 42 953 | 47 775 | 47 801 |
| 99 | python (3.8)| [hug](https://hug.rest) (2.6) | 42 932 | 46 258 | 47 268 |
| 100 | python (3.8)| [starlette](https://starlette.io) (0.13) | 42 729 | 46 403 | 47 010 |
| 101 | php (7.4)| [imi](https://imiphp.com) (1.2) | 42 673 | 49 135 | 50 758 |
| 102 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 40 642 | 41 214 | 41 983 |
| 103 | javascript (13.14)| [hapi](https://hapijs.com) (19.1) | 39 609 | 43 005 | 42 344 |
| 104 | javascript (13.14)| [nestjs-express](https://nestjs.com) (7.2) | 38 553 | 40 995 | 40 082 |
| 105 | javascript (13.14)| [restify](https://restify.com) (8.5) | 37 172 | 40 744 | 39 866 |
| 106 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.1) | 36 604 | 37 658 | 36 475 |
| 107 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 35 182 | 33 655 | 31 587 |
| 108 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 34 935 | 39 975 | 39 806 |
| 109 | scala (2.13)| [play](https://playframework.com) (2.8) | 34 655 | 37 273 | 37 213 |
| 110 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 34 058 | 35 137 | 35 566 |
| 111 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 32 121 | 32 975 | 32 436 |
| 112 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.15) | 31 305 | 29 590 | 27 711 |
| 113 | dart (2.8)| [aqueduct](https://aqueduct.io) (3.3) | 30 110 | 30 181 | 30 003 |
| 114 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 30 109 | 29 400 | 28 310 |
| 115 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 29 147 | 30 220 | 32 272 |
| 116 | php (7.4)| [swoft](https://swoft.org) (2.0) | 27 897 | 33 789 | 34 970 |
| 117 | nim (1.2)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 27 639 | 23 971 | 21 541 |
| 118 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 27 287 | 34 030 | 35 069 |
| 119 | python (3.8)| [responder](https://python-responder.org) (2.0) | 27 133 | 29 884 | 29 983 |
| 120 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.58) | 26 700 | 30 096 | 30 056 |
| 121 | nim (1.2)| [akane](https://github.com/Ethosa/akane) (0.1) | 26 593 | 23 321 | 20 836 |
| 122 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 25 944 | 31 546 | 31 956 |
| 123 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 24 732 | 25 769 | 25 933 |
| 124 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 24 310 | 27 217 | 25 798 |
| 125 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 24 064 | 27 972 | 26 824 |
| 126 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 24 063 | 26 603 | 26 779 |
| 127 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.33) | 23 883 | 26 212 | 25 658 |
| 128 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 745 | 24 807 | 24 859 |
| 129 | rust (1.44)| [iron](https://ironframework.io) (0.6) | 23 178 | 23 483 | 23 517 |
| 130 | javascript (13.14)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 839 | 22 103 | 20 805 |
| 131 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 22 255 | 23 193 | 23 167 |
| 132 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 21 849 | 23 033 | 22 824 |
| 133 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 21 706 | 24 058 | 23 548 |
| 134 | go (1.14)| [macaron](https://go-macaron.com) (1.3) | 20 830 | 23 242 | 23 451 |
| 135 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 19 440 | 20 262 | 19 679 |
| 136 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 19 237 | 20 432 | 20 097 |
| 137 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 17 516 | 18 572 | 18 207 |
| 138 | java (8)| [blade](https://lets-blade.com) (2.0) | 15 994 | 19 608 | 18 168 |
| 139 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 14 248 | 14 721 | 14 793 |
| 140 | dart (2.8)| [start](https://github.com/lvivski/start) (0.4) | 14 016 | 13 655 | 12 798 |
| 141 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 13 128 | 13 367 | 13 368 |
| 142 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (0.2) | 12 879 | 12 644 | 11 997 |
| 143 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 12 666 | 12 979 | 13 074 |
| 144 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 12 185 | 12 341 | 12 529 |
| 145 | javascript (13.14)| [sails](https://sailsjs.com) (1.2) | 12 122 | 12 735 | 12 518 |
| 146 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 11 418 | 11 627 | 11 759 |
| 147 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.12) | 11 110 | 11 369 | 10 640 |
| 148 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.0) | 10 067 | 10 877 | 10 918 |
| 149 | swift (5.2)| [swifter](https://github.com/httpswift/swifter) (1.4) | 10 009 | 10 111 | 10 104 |
| 150 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.4) | 9 644 | 9 398 | 9 321 |
| 151 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 598 | 9 966 | 9 750 |
| 152 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 287 | 9 411 | 9 094 |
| 153 | pony (0.35)| [jennet](https://github.com/Theodus/jennet) (0.1) | 9 158 | 16 987 | 15 936 |
| 154 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 8 844 | 8 600 | 8 793 |
| 155 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.2) | 8 015 | 7 933 | 7 813 |
| 156 | java (8)| [struts2](https://struts.apache.org) (2.5) | 7 846 | 8 260 | 8 672 |
| 157 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 7 471 | 7 531 | 7 540 |
| 158 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 7 293 | 7 382 | 7 303 |
| 159 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 7 184 | 7 124 | 7 040 |
| 160 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 6 900 | 6 545 | 6 232 |
| 161 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 6 104 | 5 999 | 5 994 |
| 162 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 5 886 | 5 804 | 5 830 |
| 163 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 5 806 | 5 741 | 5 749 |
| 164 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 5 800 | 5 725 | 5 727 |
| 165 | php (7.4)| [ice](https://iceframework.org) (1.5) | 5 077 | 5 043 | 5 086 |
| 166 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.4) | 4 915 | 4 871 | 4 905 |
| 167 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.0) | 4 623 | 4 609 | 4 652 |
| 168 | php (7.4)| [slim](https://slimframework.com) (4.5) | 3 872 | 3 899 | 3 969 |
| 169 | php (7.4)| [lumen](https://lumen.laravel.com) (7.2) | 3 767 | 3 813 | 3 855 |
| 170 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 3 701 | 3 724 | 3 762 |
| 171 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 3 511 | 3 537 | 3 596 |
| 172 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 363 | 3 170 | 3 166 |
| 173 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 166 | 7 712 | 5 747 |
| 174 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.4) | 3 129 | 3 180 | 3 222 |
| 175 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 2 974 | 2 366 | 1 404 |
| 176 | php (7.4)| [symfony](https://symfony.com) (5.1) | 2 783 | 2 829 | 2 855 |
| 177 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 482 | 2 512 | 2 545 |
| 178 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 412 | 2 421 | 2 416 |
| 179 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 179 | 2 172 | 2 149 |
| 180 | perl (5.32)| [dancer2](https://perldancer.org) (2.0) | 2 013 | 1 833 | 924 |
| 181 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 628 | 1 633 | 1 613 |
| 182 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 562 | 1 517 | 1 506 |
| 183 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 347 | 1 375 | 1 390 |
| 184 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 752 | 509 | 1 365 |
| 185 | php (7.4)| [laravel](https://laravel.com) (7.18) | 390 | 159 | 2 629 |
| 186 | crystal (0.35)| [lucky](https://luckyframework.org) (0.23) | 302 | 330 | 317 |

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

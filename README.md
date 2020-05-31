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

:information_source:  Updated on **2020-05-31** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 188 245 | 202 502 | 202 640 |
| 2 | javascript (13.14)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.11) | 186 160 | 204 040 | 203 872 |
| 3 | javascript (13.14)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.0) | 177 994 | 192 987 | 196 343 |
| 4 | javascript (13.14)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 176 024 | 189 938 | 190 053 |
| 5 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 172 567 | 188 016 | 188 826 |
| 6 | php (7.4)| [simps](https://simps.io) (1.0) | 168 481 | 184 601 | 184 248 |
| 7 | java (8)| [jooby](https://jooby.io) (2.8) | 167 431 | 190 563 | 197 775 |
| 8 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.2) | 164 091 | 175 826 | 181 364 |
| 9 | kotlin (1.3)| [kooby](https://jooby.io) (2.8) | 163 334 | 188 777 | 191 289 |
| 10 | crystal (0.34)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 159 752 | 168 870 | 166 372 |
| 11 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 159 527 | 170 329 | 174 209 |
| 12 | crystal (0.34)| [spider-gazelle](https://spider-gazelle.net) (3.0) | 158 742 | 165 652 | 163 041 |
| 13 | go (1.14)| [fiber](https://gofiber.io) (1.1) | 156 780 | 165 376 | 163 434 |
| 14 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 152 234 | 161 725 | 168 260 |
| 15 | crystal (0.34)| [toro](https://github.com/soveran/toro) (0.4) | 151 811 | 144 364 | 152 583 |
| 16 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 150 183 | 161 269 | 163 853 |
| 17 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.13) | 147 643 | 165 463 | 172 817 |
| 18 | crystal (0.34)| [kemal](https://kemalcr.com) (0.26) | 145 725 | 152 332 | 147 350 |
| 19 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.1) | 145 141 | 151 941 | 160 889 |
| 20 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 142 828 | 153 466 | 154 910 |
| 21 | crystal (0.34)| [amber](https://amberframework.org) (0.34) | 138 772 | 144 927 | 140 332 |
| 22 | rust (1.43)| [actix](https://actix.rs) (2.0) | 133 510 | 136 230 | 135 536 |
| 23 | crystal (0.34)| [orion](https://github.com/obsidian/orion) (2.3) | 128 819 | 139 754 | 130 950 |
| 24 | crystal (0.34)| [athena](https://github.com/athena-framework/athena) (0.8) | 128 083 | 129 914 | 122 846 |
| 25 | crystal (0.34)| [grip](https://github.com/grip-framework/grip) (0.28) | 123 444 | 127 573 | 132 222 |
| 26 | java (8)| [act](https://actframework.org) (1.8) | 118 301 | 128 943 | 128 842 |
| 27 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 114 163 | 178 999 | 186 289 |
| 28 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 113 670 | 114 595 | 119 567 |
| 29 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 111 658 | 113 797 | 119 331 |
| 30 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 110 813 | 113 801 | 117 185 |
| 31 | c (99)| [kore](https://kore.io) (3.3) | 103 783 | 110 770 | 106 900 |
| 32 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 102 341 | 108 886 | 107 880 |
| 33 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 102 308 | 107 139 | 105 437 |
| 34 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 102 257 | 102 632 | 103 169 |
| 35 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 101 993 | 104 905 | 106 070 |
| 36 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (3.4) | 101 776 | 101 533 | 106 100 |
| 37 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 101 560 | 106 768 | 108 195 |
| 38 | go (1.14)| [violetear](https://violetear.org) (7.0) | 99 776 | 97 251 | 105 996 |
| 39 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 99 068 | 97 962 | 103 021 |
| 40 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 97 724 | 118 836 | 122 149 |
| 41 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 96 896 | 101 105 | 105 019 |
| 42 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 93 083 | 92 194 | 90 414 |
| 43 | go (1.14)| [beego](https://beego.me) (1.12) | 89 495 | 94 111 | 96 620 |
| 44 | javascript (13.14)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 89 246 | 98 457 | 97 665 |
| 45 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 87 771 | 91 845 | 92 935 |
| 46 | go (1.14)| [air](https://github.com/aofei/air) (0.16) | 87 009 | 90 193 | 93 068 |
| 47 | javascript (13.14)| [0http](https://github.com/jkyberneees/0http) (2.5) | 86 892 | 92 729 | 95 108 |
| 48 | javascript (13.14)| [restana](https://github.com/jkyberneees/ana) (4.5) | 82 524 | 90 622 | 91 149 |
| 49 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 81 283 | 81 686 | 79 295 |
| 50 | javascript (13.14)| [polka](https://github.com/lukeed/polka) (0.5) | 80 002 | 84 450 | 82 464 |
| 51 | javascript (13.14)| [fastify](https://fastify.io) (2.14) | 79 547 | 84 275 | 82 108 |
| 52 | swift (5.2)| [perfect](https://perfect.org) (3.1) | 79 435 | 87 573 | 92 955 |
| 53 | javascript (13.14)| [rayo](https://rayo.js.org) (1.3) | 78 662 | 83 439 | 81 177 |
| 54 | go (1.14)| [gf](https://goframe.org) (1.13) | 77 694 | 82 222 | 86 246 |
| 55 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 77 559 | 83 939 | 81 815 |
| 56 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 72 637 | 76 849 | 77 009 |
| 57 | java (8)| [javalin](https://javalin.io) (3.8) | 71 634 | 77 950 | 79 812 |
| 58 | javascript (13.14)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 69 584 | 74 631 | 72 512 |
| 59 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.2) | 69 230 | 76 622 | 76 221 |
| 60 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 68 432 | 80 790 | 81 747 |
| 61 | java (8)| [micronaut](https://micronaut.io) (1.2) | 68 005 | 74 275 | 74 275 |
| 62 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 66 842 | 76 762 | 79 935 |
| 63 | javascript (13.14)| [foxify](https://foxify.js.org) (0.1) | 65 211 | 69 082 | 67 047 |
| 64 | javascript (13.14)| [nestjs-fastify](https://nestjs.com) (7.1) | 64 597 | 71 428 | 69 515 |
| 65 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 62 875 | 60 735 | 64 721 |
| 66 | php (7.4)| [hyperf](https://www.hyperf.io) (1.1) | 62 292 | 65 932 | 68 965 |
| 67 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 61 009 | 64 874 | 66 393 |
| 68 | javascript (13.14)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 58 749 | 62 355 | 60 588 |
| 69 | rust (1.43)| [nickel](https://nickel-org.github.io) (0.11) | 58 270 | 57 287 | 57 651 |
| 70 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 57 924 | 67 231 | 67 052 |
| 71 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 56 563 | 57 076 | 56 592 |
| 72 | javascript (13.14)| [koa](https://koajs.com) (2.12) | 56 368 | 62 040 | 61 154 |
| 73 | swift (5.2)| [kitura](https://kitura.io) (2.9) | 56 134 | 57 004 | 57 003 |
| 74 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 54 566 | 60 864 | 59 412 |
| 75 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.26) | 54 216 | 60 213 | 60 024 |
| 76 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 53 013 | 53 053 | 54 067 |
| 77 | javascript (13.14)| [express](https://expressjs.com) (4.17) | 52 764 | 55 199 | 53 903 |
| 78 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 51 830 | 57 077 | 55 870 |
| 79 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 51 777 | 54 606 | 52 479 |
| 80 | rust (1.43)| [gotham](https://gotham.rs) (0.4) | 51 735 | 58 871 | 59 913 |
| 81 | swift (5.2)| [vapor](https://vapor.codes) (4.7) | 50 810 | 52 749 | 52 983 |
| 82 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 50 529 | 52 798 | 52 456 |
| 83 | javascript (13.14)| [feathersjs](https://feathersjs.com) (4.5) | 50 257 | 51 148 | 48 974 |
| 84 | javascript (13.14)| [moleculer](https://moleculer.services) (0.14) | 50 214 | 51 800 | 50 247 |
| 85 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 49 221 | 50 806 | 50 787 |
| 86 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 47 349 | 53 770 | 53 023 |
| 87 | python (3.8)| [hug](https://hug.rest) (2.6) | 46 238 | 48 003 | 47 694 |
| 88 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 45 759 | 45 809 | 45 440 |
| 89 | php (7.4)| [imi](https://imiphp.com) (1.2) | 45 671 | 48 478 | 48 968 |
| 90 | python (3.8)| [starlette](https://starlette.io) (0.13) | 43 839 | 47 497 | 46 956 |
| 91 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.3) | 43 801 | 49 074 | 48 620 |
| 92 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 41 741 | 42 488 | 42 553 |
| 93 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 41 735 | 39 323 | 40 856 |
| 94 | javascript (13.14)| [hapi](https://hapijs.com) (19.1) | 41 697 | 43 813 | 43 151 |
| 95 | javascript (13.14)| [nestjs-express](https://nestjs.com) (7.1) | 41 192 | 41 977 | 41 338 |
| 96 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 39 945 | 40 979 | 40 431 |
| 97 | javascript (13.14)| [restify](https://restify.com) (8.5) | 39 242 | 41 262 | 40 205 |
| 98 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 38 220 | 38 932 | 37 155 |
| 99 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 37 213 | 40 841 | 40 458 |
| 100 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.32) | 37 176 | 37 492 | 34 450 |
| 101 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 36 886 | 37 500 | 37 242 |
| 102 | php (7.4)| [swoft](https://swoft.org) (2.0) | 36 011 | 37 069 | 36 969 |
| 103 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 35 880 | 36 650 | 36 899 |
| 104 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 33 487 | 34 000 | 34 658 |
| 105 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 32 833 | 37 727 | 35 546 |
| 106 | dart (2.8)| [aqueduct](https://aqueduct.io) (3.3) | 32 733 | 33 054 | 32 613 |
| 107 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 32 253 | 33 451 | 31 339 |
| 108 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 30 964 | 30 456 | 29 188 |
| 109 | crystal (0.34)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.15) | 29 587 | 26 953 | 25 192 |
| 110 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.55) | 27 239 | 31 071 | 30 813 |
| 111 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 26 997 | 26 543 | 25 872 |
| 112 | python (3.8)| [responder](https://python-responder.org) (2.0) | 26 723 | 30 149 | 29 490 |
| 113 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 26 543 | 27 067 | 26 261 |
| 114 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 26 287 | 26 762 | 26 536 |
| 115 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 25 214 | 25 581 | 25 643 |
| 116 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 24 180 | 27 644 | 26 768 |
| 117 | rust (1.43)| [iron](https://ironframework.io) (0.6) | 23 996 | 24 056 | 24 033 |
| 118 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 23 966 | 23 591 | 23 024 |
| 119 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 23 508 | 24 082 | 23 631 |
| 120 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 23 390 | 23 883 | 23 816 |
| 121 | javascript (13.14)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 266 | 21 414 | 20 508 |
| 122 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 17 504 | 17 819 | 17 802 |
| 123 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 261 | 17 794 | 17 074 |
| 124 | java (8)| [blade](https://lets-blade.com) (2.0) | 16 843 | 19 851 | 18 611 |
| 125 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 15 404 | 15 518 | 15 510 |
| 126 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 14 861 | 15 185 | 14 845 |
| 127 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 14 465 | 14 419 | 14 331 |
| 128 | javascript (13.14)| [sails](https://sailsjs.com) (1.2) | 13 028 | 13 534 | 13 402 |
| 129 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.12) | 11 769 | 11 723 | 11 055 |
| 130 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 11 354 | 11 178 | 11 143 |
| 131 | swift (5.2)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 234 | 11 253 | 11 215 |
| 132 | pony (0.35)| [jennet](https://github.com/Theodus/jennet) (0.1) | 11 181 | 18 499 | 16 692 |
| 133 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.2) | 11 145 | 11 485 | 10 804 |
| 134 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 10 403 | 10 139 | 10 660 |
| 135 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 10 191 | 10 247 | 10 083 |
| 136 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 843 | 9 797 | 9 587 |
| 137 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 253 | 9 258 | 9 093 |
| 138 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 202 | 9 141 | 9 036 |
| 139 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 8 005 | 7 847 | 7 433 |
| 140 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 572 | 7 524 | 7 418 |
| 141 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 546 | 7 289 | 7 452 |
| 142 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 7 485 | 7 816 | 7 628 |
| 143 | crystal (0.34)| [onyx](https://onyxframework.org) (0.5) | 6 603 | 6 583 | 6 926 |
| 144 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 236 | 6 280 | 6 327 |
| 145 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 5 329 | 5 332 | 5 606 |
| 146 | php (7.4)| [slim](https://slimframework.com) (4.5) | 4 452 | 4 458 | 4 599 |
| 147 | php (7.4)| [lumen](https://lumen.laravel.com) (7.1) | 4 424 | 4 205 | 4 401 |
| 148 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 4 130 | 4 361 | 4 509 |
| 149 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 063 | 4 229 | 4 083 |
| 150 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 754 | 3 566 | 3 568 |
| 151 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 245 | 7 521 | 5 889 |
| 152 | php (7.4)| [symfony](https://symfony.com) (5.1) | 3 169 | 3 112 | 3 229 |
| 153 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 857 | 2 935 | 2 894 |
| 154 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 410 | 2 395 | 2 391 |
| 155 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 1 656 | 2 768 | 1 253 |
| 156 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 599 | 1 623 | 1 590 |
| 157 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 528 | 1 488 | 1 478 |
| 158 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 513 | 1 538 | 1 540 |
| 159 | crystal (0.34)| [lucky](https://luckyframework.org) (0.21) | 1 511 | 1 594 | 1 547 |
| 160 | php (7.4)| [laravel](https://laravel.com) (7.13) | 735 | 161 | 2 927 |
| 161 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 521 | 493 | 956 |

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

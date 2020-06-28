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

:information_source:  Updated on **2020-06-28** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | javascript (13.14)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.12) | 188 987 | 203 850 | 203 803 |
| 2 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 187 852 | 200 841 | 201 693 |
| 3 | nim (1.2)| [whip](https://github.com/mattaylor/whip) (0.2) | 176 646 | 187 984 | 187 440 |
| 4 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 176 547 | 187 381 | 194 457 |
| 5 | go (1.14)| [fiber](https://gofiber.io) (1.12) | 173 479 | 190 879 | 192 474 |
| 6 | java (8)| [jooby](https://jooby.io) (2.8) | 171 950 | 191 340 | 199 481 |
| 7 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.14) | 171 293 | 183 469 | 189 222 |
| 8 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 170 364 | 173 483 | 170 713 |
| 9 | javascript (13.14)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 168 848 | 181 799 | 181 537 |
| 10 | kotlin (1.3)| [kooby](https://jooby.io) (2.8) | 167 501 | 188 178 | 190 330 |
| 11 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.2) | 165 649 | 176 375 | 181 254 |
| 12 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 165 532 | 178 324 | 173 652 |
| 13 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 165 422 | 176 793 | 182 117 |
| 14 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 163 952 | 174 914 | 180 761 |
| 15 | go (1.14)| [gearbox](https://gogearbox.com) (1.0) | 162 704 | 173 612 | 178 783 |
| 16 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 160 650 | 181 797 | 182 176 |
| 17 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.3) | 160 173 | 170 247 | 179 322 |
| 18 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.1) | 159 554 | 167 414 | 164 369 |
| 19 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (0.28) | 157 736 | 166 656 | 162 929 |
| 20 | crystal (0.35)| [kemal](https://kemalcr.com) (0.26) | 156 337 | 164 639 | 161 834 |
| 21 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 155 632 | 177 189 | 182 683 |
| 22 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 147 062 | 147 323 | 149 975 |
| 23 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 146 503 | 155 851 | 155 998 |
| 24 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 145 977 | 154 076 | 165 615 |
| 25 | rust (1.44)| [actix](https://actix.rs) (2.0) | 142 255 | 143 649 | 141 368 |
| 26 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (2.3) | 124 486 | 131 288 | 132 690 |
| 27 | java (8)| [act](https://actframework.org) (1.8) | 118 803 | 131 074 | 134 807 |
| 28 | go (1.14)| [clevergo](https://clevergo.tech) (0.3) | 116 503 | 116 915 | 122 159 |
| 29 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 116 349 | 116 393 | 121 815 |
| 30 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 114 728 | 114 863 | 120 117 |
| 31 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 112 980 | 117 325 | 121 682 |
| 32 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 111 402 | 115 500 | 119 791 |
| 33 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 110 746 | 110 120 | 114 853 |
| 34 | c (99)| [kore](https://kore.io) (3.3) | 110 058 | 133 782 | 132 752 |
| 35 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 109 459 | 110 077 | 114 055 |
| 36 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 108 142 | 108 022 | 112 549 |
| 37 | go (1.14)| [violetear](https://violetear.org) (7.0) | 107 775 | 107 663 | 112 608 |
| 38 | fsharp (4.7)| [frank](https://github.com/frank-fs/frank) (6.1) | 107 181 | 115 462 | 120 237 |
| 39 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 106 940 | 104 873 | 109 479 |
| 40 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 105 847 | 183 366 | 178 909 |
| 41 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 105 317 | 116 263 | 120 345 |
| 42 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 104 976 | 110 195 | 113 326 |
| 43 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 104 069 | 100 853 | 106 005 |
| 44 | go (1.14)| [beego](https://beego.me) (1.12) | 103 799 | 106 849 | 111 300 |
| 45 | fsharp (4.7)| [falco](https://github.com/pimbrouwers/Falco) (1.2) | 103 451 | 110 894 | 111 098 |
| 46 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 98 112 | 103 525 | 105 091 |
| 47 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 98 009 | 99 451 | 96 851 |
| 48 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 97 781 | 119 478 | 123 191 |
| 49 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.9) | 97 556 | 109 869 | 107 474 |
| 50 | php (7.4)| [hyperf](https://www.hyperf.io) (1.1) | 96 194 | 104 482 | 104 712 |
| 51 | fsharp (4.7)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 93 049 | 97 918 | 94 042 |
| 52 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 88 672 | 90 649 | 93 185 |
| 53 | javascript (13.14)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 85 654 | 95 071 | 94 588 |
| 54 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.1) | 81 552 | 81 986 | 73 205 |
| 55 | javascript (13.14)| [0http](https://github.com/jkyberneees/0http) (2.5) | 81 153 | 89 777 | 89 360 |
| 56 | go (1.14)| [air](https://github.com/aofei/air) (0.17) | 80 553 | 78 760 | 78 667 |
| 57 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 80 547 | 87 924 | 83 992 |
| 58 | javascript (13.14)| [restana](https://github.com/jkyberneees/ana) (4.6) | 80 204 | 88 233 | 86 991 |
| 59 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 79 979 | 80 397 | 77 666 |
| 60 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 77 677 | 90 553 | 91 569 |
| 61 | javascript (13.14)| [polka](https://github.com/lukeed/polka) (0.5) | 76 631 | 81 448 | 79 642 |
| 62 | go (1.14)| [gf](https://goframe.org) (1.13) | 76 253 | 84 450 | 85 270 |
| 63 | java (8)| [javalin](https://javalin.io) (3.9) | 75 316 | 82 466 | 82 529 |
| 64 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 75 309 | 79 205 | 79 510 |
| 65 | javascript (13.14)| [rayo](https://rayo.js.org) (1.3) | 75 167 | 79 773 | 77 960 |
| 66 | fsharp (4.7)| [saturn](https://saturnframework.org) (0.14) | 75 055 | 69 668 | 61 050 |
| 67 | swift (5.2)| [perfect](https://perfect.org) (3.1) | 74 601 | 85 110 | 88 967 |
| 68 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 72 111 | 79 275 | 78 968 |
| 69 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 69 516 | 68 917 | 72 475 |
| 70 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.7) | 69 487 | 73 628 | 73 239 |
| 71 | java (8)| [micronaut](https://micronaut.io) (1.2) | 68 713 | 75 341 | 75 482 |
| 72 | javascript (13.14)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 66 953 | 71 183 | 70 525 |
| 73 | javascript (13.14)| [fastify](https://fastify.io) (2.15) | 66 214 | 70 685 | 68 022 |
| 74 | fsharp (4.7)| [websharper](https://websharper.com) (4.6) | 66 073 | 67 767 | 65 924 |
| 75 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 64 235 | 67 026 | 73 627 |
| 76 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 62 306 | 80 181 | 82 784 |
| 77 | javascript (13.14)| [foxify](https://foxify.js.org) (0.1) | 61 426 | 64 983 | 62 796 |
| 78 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 59 949 | 62 211 | 63 038 |
| 79 | javascript (13.14)| [nestjs-fastify](https://nestjs.com) (7.2) | 58 456 | 63 442 | 62 034 |
| 80 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 58 334 | 67 138 | 67 654 |
| 81 | javascript (13.14)| [koa](https://koajs.com) (2.13) | 57 975 | 60 057 | 58 782 |
| 82 | javascript (13.14)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 57 534 | 60 228 | 58 529 |
| 83 | rust (1.44)| [nickel](https://nickel-org.github.io) (0.11) | 55 998 | 55 558 | 56 305 |
| 84 | swift (5.2)| [kitura](https://kitura.io) (2.9) | 55 459 | 55 858 | 55 524 |
| 85 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 55 195 | 56 526 | 56 719 |
| 86 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 55 017 | 55 299 | 54 976 |
| 87 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.26) | 51 871 | 60 464 | 59 684 |
| 88 | rust (1.44)| [gotham](https://gotham.rs) (0.4) | 50 749 | 59 682 | 60 211 |
| 89 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 50 539 | 56 449 | 57 451 |
| 90 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 50 395 | 52 303 | 49 864 |
| 91 | javascript (13.14)| [feathersjs](https://feathersjs.com) (4.5) | 49 778 | 51 814 | 50 710 |
| 92 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 49 656 | 49 704 | 49 786 |
| 93 | javascript (13.14)| [moleculer](https://moleculer.services) (0.14) | 49 336 | 50 890 | 49 911 |
| 94 | javascript (13.14)| [express](https://expressjs.com) (4.17) | 49 322 | 51 824 | 51 259 |
| 95 | swift (5.2)| [vapor](https://vapor.codes) (4.14) | 48 803 | 50 286 | 50 435 |
| 96 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 48 486 | 50 607 | 50 718 |
| 97 | php (7.4)| [imi](https://imiphp.com) (1.2) | 47 541 | 48 477 | 50 163 |
| 98 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 47 293 | 50 770 | 50 939 |
| 99 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.3) | 46 346 | 50 116 | 49 917 |
| 100 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 45 344 | 46 190 | 46 185 |
| 101 | php (7.4)| [swoft](https://swoft.org) (2.0) | 43 412 | 45 848 | 43 794 |
| 102 | javascript (13.14)| [hapi](https://hapijs.com) (19.1) | 42 261 | 44 313 | 43 989 |
| 103 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 41 731 | 42 210 | 42 364 |
| 104 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 41 603 | 43 964 | 43 985 |
| 105 | python (3.8)| [starlette](https://starlette.io) (0.13) | 41 446 | 41 972 | 44 202 |
| 106 | javascript (13.14)| [restify](https://restify.com) (8.5) | 39 919 | 41 499 | 41 823 |
| 107 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 39 679 | 38 824 | 39 247 |
| 108 | javascript (13.14)| [nestjs-express](https://nestjs.com) (7.2) | 38 725 | 40 227 | 39 785 |
| 109 | scala (2.13)| [play](https://playframework.com) (2.8) | 38 479 | 40 140 | 39 883 |
| 110 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 38 358 | 40 889 | 41 324 |
| 111 | python (3.8)| [hug](https://hug.rest) (2.6) | 37 773 | 35 160 | 36 991 |
| 112 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.1) | 37 059 | 33 159 | 34 475 |
| 113 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 36 326 | 37 447 | 35 929 |
| 114 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.33) | 35 950 | 36 945 | 35 250 |
| 115 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 35 806 | 36 362 | 36 192 |
| 116 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 35 592 | 36 386 | 34 447 |
| 117 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 35 449 | 36 417 | 36 113 |
| 118 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 33 251 | 34 475 | 33 989 |
| 119 | dart (2.8)| [aqueduct](https://aqueduct.io) (3.3) | 32 058 | 32 370 | 31 674 |
| 120 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 31 724 | 32 655 | 31 320 |
| 121 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 31 611 | 30 510 | 28 778 |
| 122 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.15) | 31 198 | 29 950 | 27 676 |
| 123 | python (3.8)| [responder](https://python-responder.org) (2.0) | 29 899 | 31 280 | 31 266 |
| 124 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.58) | 29 837 | 30 691 | 30 898 |
| 125 | nim (1.2)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 27 743 | 23 793 | 21 656 |
| 126 | nim (1.2)| [akane](https://github.com/Ethosa/akane) (0.1) | 26 899 | 23 207 | 21 129 |
| 127 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 26 497 | 27 005 | 25 988 |
| 128 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 26 468 | 27 019 | 26 336 |
| 129 | go (1.14)| [macaron](https://go-macaron.com) (1.3) | 26 100 | 28 470 | 28 565 |
| 130 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 25 470 | 25 197 | 24 935 |
| 131 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 24 884 | 25 733 | 25 129 |
| 132 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 24 383 | 27 572 | 27 752 |
| 133 | rust (1.44)| [iron](https://ironframework.io) (0.6) | 24 064 | 24 344 | 24 105 |
| 134 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 23 392 | 23 907 | 23 337 |
| 135 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 23 208 | 23 577 | 23 156 |
| 136 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 23 123 | 23 446 | 23 727 |
| 137 | javascript (13.14)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 842 | 21 540 | 20 226 |
| 138 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 18 976 | 19 297 | 19 455 |
| 139 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 17 853 | 18 001 | 18 108 |
| 140 | java (8)| [blade](https://lets-blade.com) (2.0) | 16 230 | 20 527 | 20 325 |
| 141 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 15 065 | 15 477 | 15 417 |
| 142 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 14 600 | 14 521 | 14 412 |
| 143 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 14 423 | 14 397 | 14 269 |
| 144 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.0) | 14 405 | 15 864 | 15 750 |
| 145 | dart (2.8)| [start](https://github.com/lvivski/start) (0.4) | 14 099 | 13 820 | 13 019 |
| 146 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (0.2) | 12 731 | 12 588 | 12 066 |
| 147 | javascript (13.14)| [sails](https://sailsjs.com) (1.2) | 12 552 | 13 122 | 13 049 |
| 148 | swift (5.2)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 212 | 11 209 | 11 211 |
| 149 | pony (0.35)| [jennet](https://github.com/Theodus/jennet) (0.1) | 10 959 | 18 104 | 15 758 |
| 150 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.2) | 10 910 | 11 179 | 10 779 |
| 151 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 10 664 | 10 340 | 10 175 |
| 152 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.12) | 10 496 | 11 082 | 10 481 |
| 153 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 10 401 | 10 367 | 10 326 |
| 154 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 10 053 | 10 519 | 10 111 |
| 155 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 849 | 9 833 | 9 641 |
| 156 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 591 | 9 133 | 9 418 |
| 157 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 248 | 9 397 | 8 720 |
| 158 | java (8)| [struts2](https://struts.apache.org) (2.5) | 9 176 | 9 401 | 9 644 |
| 159 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 8 080 | 7 742 | 7 850 |
| 160 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 7 976 | 8 100 | 8 105 |
| 161 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 887 | 7 846 | 7 460 |
| 162 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 585 | 7 828 | 7 734 |
| 163 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 359 | 7 944 | 7 662 |
| 164 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 336 | 6 216 | 6 472 |
| 165 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.0) | 5 650 | 5 738 | 5 735 |
| 166 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 4 593 | 4 638 | 4 826 |
| 167 | php (7.4)| [slim](https://slimframework.com) (4.5) | 4 551 | 4 517 | 4 787 |
| 168 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 436 | 4 522 | 4 646 |
| 169 | php (7.4)| [lumen](https://lumen.laravel.com) (7.2) | 4 124 | 4 090 | 4 389 |
| 170 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 591 | 3 430 | 3 450 |
| 171 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 321 | 7 859 | 6 067 |
| 172 | php (7.4)| [symfony](https://symfony.com) (5.1) | 3 198 | 3 257 | 3 306 |
| 173 | perl (5.32)| [dancer2](https://perldancer.org) (2.0) | 3 048 | 1 663 | 1 055 |
| 174 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 883 | 2 905 | 2 988 |
| 175 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 2 794 | 2 671 | 1 412 |
| 176 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 505 | 2 508 | 2 495 |
| 177 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 229 | 2 194 | 2 163 |
| 178 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 555 | 1 603 | 1 573 |
| 179 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 535 | 1 502 | 1 498 |
| 180 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 416 | 1 473 | 1 461 |
| 181 | crystal (0.35)| [lucky](https://luckyframework.org) (0.22) | 1 152 | 1 176 | 1 182 |
| 182 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 996 | 503 | 923 |
| 183 | php (7.4)| [laravel](https://laravel.com) (7.17) | 273 | 158 | 2 001 |

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

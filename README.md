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

:information_source:  Updated on **2020-08-04** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 169 649 | 174 782 | 173 979 |
| 2 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 164 687 | 195 612 | 198 639 |
| 3 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.15) | 156 719 | 169 525 | 173 428 |
| 4 | javascript (14.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 155 045 | 196 291 | 201 351 |
| 5 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 154 648 | 166 557 | 170 185 |
| 6 | nim (1.2)| [whip](https://github.com/mattaylor/whip) (0.2) | 154 323 | 181 983 | 184 125 |
| 7 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.2) | 153 194 | 165 235 | 169 175 |
| 8 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 151 048 | 162 411 | 166 786 |
| 9 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.5) | 149 834 | 168 115 | 173 764 |
| 10 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 148 380 | 176 930 | 178 298 |
| 11 | java (11)| [jooby](https://jooby.io) (2.8) | 146 280 | 187 447 | 192 259 |
| 12 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 141 023 | 169 936 | 169 074 |
| 13 | kotlin (1.3)| [kooby](https://jooby.io) (2.8) | 140 253 | 184 406 | 189 013 |
| 14 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 137 591 | 164 419 | 163 417 |
| 15 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 137 206 | 166 501 | 168 393 |
| 16 | c (99)| [kore](https://kore.io) (3.3) | 136 605 | 123 987 | 134 434 |
| 17 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.3) | 134 677 | 165 269 | 163 388 |
| 18 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 132 942 | 175 821 | 180 833 |
| 19 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (0.28) | 131 653 | 154 708 | 152 620 |
| 20 | crystal (0.35)| [kemal](https://kemalcr.com) (0.26) | 130 534 | 153 685 | 152 420 |
| 21 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 128 174 | 150 818 | 152 747 |
| 22 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 126 368 | 147 094 | 144 101 |
| 23 | rust (1.45)| [actix](https://actix.rs) (2.0) | 123 632 | 133 586 | 134 501 |
| 24 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 116 589 | 164 015 | 183 665 |
| 25 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 106 029 | 159 981 | 177 916 |
| 26 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 104 445 | 105 242 | 110 097 |
| 27 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 103 866 | 109 945 | 116 074 |
| 28 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 103 444 | 105 131 | 109 315 |
| 29 | java (11)| [act](https://actframework.org) (1.9) | 103 335 | 134 172 | 136 036 |
| 30 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 102 277 | 107 741 | 107 071 |
| 31 | go (1.14)| [fiber](https://gofiber.io) (1.12) | 102 087 | 148 246 | 158 666 |
| 32 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 101 629 | 100 213 | 107 735 |
| 33 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 99 605 | 99 700 | 104 111 |
| 34 | go (1.14)| [gearbox](https://gogearbox.com) (1.1) | 99 324 | 137 681 | 159 993 |
| 35 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 98 466 | 99 739 | 103 702 |
| 36 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 98 108 | 103 196 | 108 381 |
| 37 | go (1.14)| [violetear](https://violetear.org) (7.0) | 98 068 | 98 244 | 104 944 |
| 38 | go (1.14)| [clevergo](https://clevergo.tech) (0.3) | 96 511 | 95 486 | 97 058 |
| 39 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 95 177 | 96 015 | 100 347 |
| 40 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 95 112 | 109 243 | 112 592 |
| 41 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 94 951 | 94 245 | 98 431 |
| 42 | fsharp (4.7)| [frank](https://github.com/frank-fs/frank) (6.1) | 94 915 | 109 078 | 112 495 |
| 43 | fsharp (4.7)| [falco](https://github.com/pimbrouwers/Falco) (1.2) | 92 692 | 106 874 | 109 385 |
| 44 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 92 472 | 91 266 | 95 567 |
| 45 | go (1.14)| [beego](https://beego.me) (1.12) | 90 619 | 95 941 | 99 477 |
| 46 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.9) | 86 490 | 105 183 | 104 762 |
| 47 | javascript (14.7)| [0http](https://github.com/jkyberneees/0http) (2.5) | 86 361 | 96 978 | 97 771 |
| 48 | go (1.14)| [air](https://github.com/aofei/air) (0.19) | 80 329 | 81 876 | 84 790 |
| 49 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 80 029 | 86 462 | 87 992 |
| 50 | javascript (14.7)| [rayo](https://rayo.js.org) (1.3) | 79 064 | 86 061 | 84 888 |
| 51 | javascript (14.7)| [fastify](https://fastify.io) (3.1) | 77 517 | 85 220 | 82 250 |
| 52 | javascript (14.7)| [restana](https://github.com/jkyberneees/ana) (4.7) | 77 501 | 86 788 | 88 482 |
| 53 | javascript (14.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 76 871 | 88 458 | 88 363 |
| 54 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 76 156 | 78 392 | 76 247 |
| 55 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.1) | 74 580 | 75 466 | 68 049 |
| 56 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 73 371 | 89 828 | 95 074 |
| 57 | php (7.4)| [hyperf](https://hyperf.io) (2.0) | 73 336 | 88 118 | 93 079 |
| 58 | java (11)| [javalin](https://javalin.io) (3.9) | 73 190 | 79 412 | 79 283 |
| 59 | swift (5.2)| [perfect](https://perfect.org) (3.1) | 72 535 | 82 286 | 87 905 |
| 60 | go (1.14)| [gf](https://goframe.org) (1.13) | 72 090 | 79 259 | 80 702 |
| 61 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.13) | 71 568 | 106 176 | 116 970 |
| 62 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 69 189 | 80 626 | 79 347 |
| 63 | javascript (14.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 68 527 | 75 688 | 74 035 |
| 64 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 67 432 | 74 729 | 75 465 |
| 65 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 66 375 | 87 369 | 91 280 |
| 66 | javascript (14.7)| [polka](https://github.com/lukeed/polka) (0.5) | 66 288 | 79 697 | 76 104 |
| 67 | java (11)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 65 827 | 72 882 | 73 073 |
| 68 | fsharp (4.7)| [saturn](https://saturnframework.org) (0.14) | 65 814 | 62 577 | 55 746 |
| 69 | javascript (14.7)| [foxify](https://foxify.js.org) (0.1) | 65 060 | 70 876 | 69 447 |
| 70 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.8) | 62 797 | 69 126 | 68 677 |
| 71 | javascript (14.7)| [koa](https://koajs.com) (2.13) | 59 679 | 64 771 | 63 108 |
| 72 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 59 128 | 60 572 | 64 096 |
| 73 | java (11)| [micronaut](https://micronaut.io) (1.2) | 57 852 | 66 918 | 66 388 |
| 74 | javascript (14.7)| [nestjs-fastify](https://nestjs.com) (7.4) | 57 809 | 64 097 | 59 259 |
| 75 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 56 785 | 62 216 | 66 575 |
| 76 | fsharp (4.7)| [websharper](https://websharper.com) (4.6) | 55 772 | 58 000 | 53 507 |
| 77 | javascript (14.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 55 120 | 58 102 | 56 214 |
| 78 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 54 334 | 56 363 | 56 305 |
| 79 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 54 033 | 58 916 | 56 642 |
| 80 | java (11)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 52 417 | 59 888 | 60 189 |
| 81 | rust (1.45)| [nickel](https://nickel-org.github.io) (0.11) | 50 940 | 49 470 | 52 227 |
| 82 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 49 792 | 50 785 | 50 976 |
| 83 | swift (5.2)| [kitura](https://kitura.io) (2.9) | 49 029 | 49 765 | 49 593 |
| 84 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.27) | 48 585 | 55 481 | 55 192 |
| 85 | rust (1.45)| [gotham](https://gotham.rs) (0.4) | 47 454 | 53 449 | 53 932 |
| 86 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 47 042 | 47 243 | 47 906 |
| 87 | swift (5.2)| [vapor](https://vapor.codes) (4.27) | 46 995 | 49 280 | 49 398 |
| 88 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 46 791 | 68 478 | 74 572 |
| 89 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 46 695 | 50 106 | 50 115 |
| 90 | javascript (14.7)| [moleculer](https://moleculer.services) (0.14) | 44 227 | 49 055 | 48 597 |
| 91 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 44 094 | 44 987 | 45 029 |
| 92 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 43 456 | 49 508 | 50 397 |
| 93 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 43 223 | 48 799 | 48 699 |
| 94 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 42 416 | 48 282 | 46 895 |
| 95 | php (7.4)| [imi](https://imiphp.com) (1.2) | 41 737 | 47 286 | 47 858 |
| 96 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.6) | 41 540 | 46 563 | 46 948 |
| 97 | javascript (14.7)| [restify](https://restify.com) (8.5) | 40 724 | 42 259 | 41 781 |
| 98 | python (3.8)| [hug](https://hug.rest) (2.6) | 39 925 | 42 791 | 42 777 |
| 99 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 39 774 | 40 706 | 40 999 |
| 100 | python (3.8)| [starlette](https://starlette.io) (0.13) | 36 180 | 39 519 | 39 544 |
| 101 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.1) | 35 661 | 36 588 | 35 853 |
| 102 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 35 287 | 39 099 | 39 267 |
| 103 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 35 213 | 34 180 | 31 540 |
| 104 | scala (2.13)| [play](https://playframework.com) (2.8) | 34 246 | 36 303 | 36 592 |
| 105 | javascript (14.7)| [hapi](https://hapijs.com) (19.2) | 33 590 | 35 794 | 37 307 |
| 106 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 33 404 | 34 218 | 34 708 |
| 107 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 31 683 | 32 623 | 32 417 |
| 108 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.15) | 30 872 | 29 687 | 26 508 |
| 109 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 30 432 | 29 507 | 28 444 |
| 110 | dart (2.8)| [aqueduct](https://aqueduct.io) (3.3) | 30 333 | 30 455 | 30 078 |
| 111 | fsharp (4.7)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 30 117 | 28 718 | 28 247 |
| 112 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 29 152 | 30 631 | 33 875 |
| 113 | nim (1.2)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 27 677 | 23 595 | 21 483 |
| 114 | php (7.4)| [swoft](https://swoft.org) (2.0) | 27 583 | 33 401 | 34 454 |
| 115 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 26 734 | 29 057 | 27 796 |
| 116 | python (3.8)| [responder](https://python-responder.org) (2.0) | 26 653 | 29 439 | 29 245 |
| 117 | nim (1.2)| [akane](https://github.com/Ethosa/akane) (0.1) | 26 646 | 23 336 | 21 147 |
| 118 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 26 302 | 32 115 | 32 606 |
| 119 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 26 113 | 31 822 | 32 544 |
| 120 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.6) | 25 696 | 28 787 | 28 746 |
| 121 | javascript (14.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 25 230 | 24 925 | 23 123 |
| 122 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 24 425 | 28 895 | 29 614 |
| 123 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 24 416 | 25 690 | 25 451 |
| 124 | rust (1.45)| [iron](https://ironframework.io) (0.6) | 24 215 | 24 450 | 24 398 |
| 125 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 23 915 | 29 501 | 27 971 |
| 126 | javascript (14.7)| [express](https://expressjs.com) (4.17) | 23 661 | 23 634 | 24 252 |
| 127 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 23 634 | 25 615 | 25 992 |
| 128 | javascript (14.7)| [feathersjs](https://feathersjs.com) (4.5) | 23 611 | 25 259 | 25 041 |
| 129 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 358 | 24 941 | 24 533 |
| 130 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.34) | 23 336 | 26 772 | 25 662 |
| 131 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 22 102 | 23 109 | 22 928 |
| 132 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 22 066 | 22 933 | 23 085 |
| 133 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 21 535 | 24 321 | 23 856 |
| 134 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 20 383 | 21 786 | 21 622 |
| 135 | go (1.14)| [macaron](https://go-macaron.com) (1.3) | 20 296 | 23 068 | 23 270 |
| 136 | javascript (14.7)| [nestjs-express](https://nestjs.com) (7.4) | 18 834 | 19 793 | 20 024 |
| 137 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 18 574 | 19 108 | 18 700 |
| 138 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 18 553 | 20 193 | 19 684 |
| 139 | java (11)| [blade](https://lets-blade.com) (2.0) | 14 820 | 17 466 | 17 080 |
| 140 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 14 210 | 14 750 | 14 813 |
| 141 | dart (2.8)| [start](https://github.com/lvivski/start) (0.4) | 13 915 | 13 594 | 12 695 |
| 142 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.5) | 13 001 | 13 286 | 13 213 |
| 143 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 12 843 | 12 851 | 12 911 |
| 144 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 12 580 | 12 875 | 12 884 |
| 145 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (0.2) | 12 539 | 12 485 | 11 880 |
| 146 | java (11)| [struts2](https://struts.apache.org) (2.5) | 12 352 | 13 324 | 13 172 |
| 147 | ruby (2.7)| [grape](https://ruby-grape.org) (1.4) | 11 201 | 11 378 | 11 450 |
| 148 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.13) | 10 654 | 11 273 | 10 623 |
| 149 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 9 943 | 10 751 | 10 745 |
| 150 | swift (5.2)| [swifter](https://github.com/httpswift/swifter) (1.4) | 9 916 | 10 079 | 9 966 |
| 151 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 732 | 9 911 | 9 767 |
| 152 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.4) | 9 674 | 9 507 | 9 261 |
| 153 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 419 | 9 427 | 9 304 |
| 154 | pony (0.36)| [jennet](https://github.com/Theodus/jennet) (0.1) | 8 931 | 16 691 | 15 618 |
| 155 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 8 712 | 8 696 | 8 576 |
| 156 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 8 222 | 8 151 | 7 963 |
| 157 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.3) | 7 932 | 7 859 | 7 754 |
| 158 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 7 888 | 7 897 | 7 869 |
| 159 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 7 295 | 7 468 | 7 450 |
| 160 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 6 782 | 6 662 | 6 667 |
| 161 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 6 472 | 6 100 | 5 863 |
| 162 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 6 056 | 5 959 | 5 971 |
| 163 | javascript (14.7)| [sails](https://sailsjs.com) (1.2) | 5 828 | 5 925 | 5 891 |
| 164 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 5 789 | 5 691 | 5 729 |
| 165 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 5 744 | 5 691 | 5 694 |
| 166 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 5 690 | 5 617 | 5 650 |
| 167 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.2) | 5 164 | 5 093 | 5 137 |
| 168 | php (7.4)| [ice](https://iceframework.org) (1.5) | 5 042 | 5 013 | 5 058 |
| 169 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.4) | 4 927 | 4 896 | 4 922 |
| 170 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 4 577 | 4 581 | 4 592 |
| 171 | php (7.4)| [slim](https://slimframework.com) (4.5) | 3 894 | 3 912 | 3 967 |
| 172 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 3 651 | 3 693 | 3 718 |
| 173 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 559 | 3 422 | 3 455 |
| 174 | php (7.4)| [lumen](https://lumen.laravel.com) (7.2) | 3 495 | 3 534 | 3 638 |
| 175 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 3 437 | 3 476 | 3 539 |
| 176 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 386 | 8 261 | 6 371 |
| 177 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.4) | 3 118 | 3 168 | 3 208 |
| 178 | php (7.4)| [symfony](https://symfony.com) (5.1) | 2 746 | 2 777 | 2 832 |
| 179 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 529 | 2 572 | 2 601 |
| 180 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 405 | 2 404 | 2 399 |
| 181 | perl (5.32)| [dancer2](https://perldancer.org) (2.0) | 1 765 | 1 338 | 506 |
| 182 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 602 | 1 632 | 1 599 |
| 183 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 535 | 1 498 | 1 487 |
| 184 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 348 | 1 373 | 1 373 |
| 185 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 036 | 1 057 | 1 122 |
| 186 | php (7.4)| [laravel](https://laravel.com) (7.22) | 630 | 182 | 2 287 |
| 187 | crystal (0.35)| [lucky](https://luckyframework.org) (0.23) | 308 | 334 | 324 |

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

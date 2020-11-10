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
psql -U postgres -d benchmark < dump.sql
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
docker exec pg sh -c "echo \"$(cat dump.sql)\" | psql -U postgres -d benchmark"
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

:information_source:  Updated on **2020-11-10** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 177 778 | 218 352 | 224 373 |
| 2 | go (1.15)| [fiber](https://gofiber.io) (2.0) | 177 275 | 195 309 | 194 029 |
| 3 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 176 194 | 184 782 | 184 906 |
| 4 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.17) | 175 521 | 196 155 | 195 555 |
| 5 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 173 976 | 212 938 | 217 770 |
| 6 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 173 385 | 193 438 | 194 081 |
| 7 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.5) | 172 635 | 191 925 | 193 183 |
| 8 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 171 183 | 191 228 | 191 591 |
| 9 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 166 492 | 202 767 | 203 996 |
| 10 | java (11)| [jooby](https://jooby.io) (2.8) | 165 601 | 206 174 | 212 641 |
| 11 | kotlin (1.4)| [kooby](https://jooby.io) (2.8) | 164 200 | 204 643 | 212 718 |
| 12 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 161 590 | 198 959 | 203 305 |
| 13 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 159 314 | 198 336 | 202 711 |
| 14 | javascript (12.18)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 158 487 | 202 517 | 206 839 |
| 15 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 157 587 | 198 916 | 205 859 |
| 16 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 154 379 | 186 078 | 186 615 |
| 17 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.3) | 152 377 | 183 220 | 185 013 |
| 18 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 151 369 | 185 293 | 185 218 |
| 19 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 150 056 | 183 210 | 183 317 |
| 20 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 145 474 | 171 003 | 170 951 |
| 21 | crystal (0.35)| [kemal](https://kemalcr.com) (0.26) | 142 394 | 171 126 | 171 265 |
| 22 | java (11)| [act](https://actframework.org) (1.9) | 139 373 | 172 220 | 175 805 |
| 23 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 134 133 | 139 915 | 136 331 |
| 24 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 133 751 | 161 225 | 161 095 |
| 25 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.11) | 129 748 | 143 382 | 133 755 |
| 26 | javascript (12.18)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 127 309 | 148 860 | 152 507 |
| 27 | c (99)| [kore](https://kore.io) (3.3) | 122 888 | 193 247 | 198 616 |
| 28 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 121 919 | 122 420 | 126 539 |
| 29 | go (1.15)| [clevergo](https://clevergo.tech) (0.3) | 120 649 | 121 608 | 125 790 |
| 30 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 119 944 | 120 808 | 124 645 |
| 31 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 119 674 | 119 965 | 123 736 |
| 32 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 117 601 | 123 199 | 126 484 |
| 33 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 116 979 | 121 800 | 124 845 |
| 34 | go (1.15)| [violetear](https://violetear.org) (7.0) | 115 127 | 115 246 | 118 995 |
| 35 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 115 106 | 177 034 | 194 756 |
| 36 | go (1.15)| [chi](https://github.com/go-chi/chi) (4.1) | 115 013 | 112 741 | 117 568 |
| 37 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 114 100 | 112 449 | 116 755 |
| 38 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 112 609 | 110 651 | 113 759 |
| 39 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 112 445 | 118 355 | 119 602 |
| 40 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 110 360 | 106 701 | 111 182 |
| 41 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 108 869 | 104 032 | 108 979 |
| 42 | go (1.15)| [beego](https://beego.me) (1.12) | 106 590 | 110 447 | 114 781 |
| 43 | fsharp (4.7)| [frank](https://github.com/frank-fs/frank) (6.1) | 101 770 | 118 339 | 122 946 |
| 44 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 100 798 | 118 226 | 121 379 |
| 45 | fsharp (4.7)| [falco](https://github.com/pimbrouwers/Falco) (1.2) | 98 661 | 115 297 | 118 998 |
| 46 | go (1.15)| [air](https://github.com/aofei/air) (0.20) | 96 068 | 93 752 | 97 496 |
| 47 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 95 191 | 138 341 | 149 024 |
| 48 | javascript (12.18)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 95 171 | 102 535 | 103 321 |
| 49 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.4) | 93 380 | 92 369 | 97 029 |
| 50 | javascript (12.18)| [0http](https://github.com/jkyberneees/0http) (3.0) | 89 561 | 99 235 | 99 635 |
| 51 | rust (1.47)| [actix](https://actix.rs) (3.2) | 87 026 | 85 813 | 85 915 |
| 52 | javascript (12.18)| [polka](https://github.com/lukeed/polka) (0.5) | 86 377 | 92 712 | 91 785 |
| 53 | java (11)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 86 297 | 92 912 | 92 327 |
| 54 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 85 939 | 89 884 | 88 533 |
| 55 | javascript (12.18)| [fastify](https://fastify.io) (3.7) | 85 066 | 92 058 | 88 768 |
| 56 | javascript (12.18)| [restana](https://github.com/jkyberneees/ana) (4.7) | 85 034 | 94 423 | 94 425 |
| 57 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.1) | 84 258 | 89 500 | 81 796 |
| 58 | java (11)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 84 103 | 91 338 | 92 829 |
| 59 | go (1.15)| [gf](https://goframe.org) (1.13) | 84 072 | 90 341 | 92 542 |
| 60 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 83 938 | 92 098 | 99 733 |
| 61 | java (11)| [javalin](https://javalin.io) (3.9) | 82 602 | 89 617 | 90 293 |
| 62 | javascript (12.18)| [rayo](https://rayo.js.org) (1.3) | 81 363 | 89 547 | 87 961 |
| 63 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 80 803 | 97 814 | 94 186 |
| 64 | javascript (12.18)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 80 726 | 86 961 | 86 163 |
| 65 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 79 853 | 104 519 | 112 524 |
| 66 | php (7.4)| [hyperf](https://hyperf.io) (2.0) | 78 044 | 90 238 | 107 586 |
| 67 | fsharp (4.7)| [saturn](https://saturnframework.org) (0.14) | 76 908 | 78 255 | 70 286 |
| 68 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 75 851 | 99 971 | 103 936 |
| 69 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.14) | 74 957 | 106 209 | 125 009 |
| 70 | javascript (12.18)| [nestjs-fastify](https://nestjs.com) (7.4) | 74 041 | 79 270 | 77 070 |
| 71 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 71 904 | 77 703 | 84 024 |
| 72 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 71 859 | 80 918 | 81 758 |
| 73 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.8) | 69 151 | 74 966 | 75 481 |
| 74 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 68 319 | 67 499 | 72 152 |
| 75 | fsharp (4.7)| [websharper](https://websharper.com) (4.6) | 67 496 | 73 953 | 72 583 |
| 76 | javascript (12.18)| [foxify](https://foxify.js.org) (0.1) | 67 254 | 73 197 | 71 154 |
| 77 | java (11)| [micronaut](https://micronaut.io) (1.2) | 66 997 | 75 290 | 75 275 |
| 78 | javascript (12.18)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 65 808 | 72 556 | 70 637 |
| 79 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 64 778 | 66 093 | 62 843 |
| 80 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 64 213 | 208 679 | 210 482 |
| 81 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 62 461 | 64 520 | 65 364 |
| 82 | javascript (12.18)| [koa](https://koajs.com) (2.13) | 61 985 | 65 574 | 64 966 |
| 83 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 61 018 | 67 286 | 67 501 |
| 84 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 55 957 | 57 715 | 59 274 |
| 85 | swift (5.3)| [kitura](https://kitura.io) (2.9) | 55 550 | 57 291 | 56 862 |
| 86 | swift (5.3)| [kitura-nio](https://kitura.io) (2.9) | 55 203 | 57 525 | 57 646 |
| 87 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 54 585 | 58 164 | 58 585 |
| 88 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 54 560 | 74 001 | 80 295 |
| 89 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.27) | 54 078 | 60 605 | 61 010 |
| 90 | javascript (12.18)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 51 863 | 57 288 | 55 574 |
| 91 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 51 455 | 56 190 | 56 903 |
| 92 | javascript (12.18)| [hapi](https://hapijs.com) (20.0) | 50 531 | 53 600 | 51 225 |
| 93 | javascript (12.18)| [moleculer](https://moleculer.services) (0.14) | 50 474 | 53 294 | 52 030 |
| 94 | rust (1.47)| [nickel](https://nickel-org.github.io) (0.11) | 50 372 | 57 970 | 55 329 |
| 95 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 50 272 | 53 250 | 54 096 |
| 96 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 47 404 | 53 152 | 54 756 |
| 97 | rust (1.47)| [gotham](https://gotham.rs) (0.4) | 46 979 | 51 352 | 52 610 |
| 98 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 46 860 | 53 126 | 53 328 |
| 99 | python (3.8)| [hug](https://hug.rest) (2.6) | 46 058 | 50 665 | 51 072 |
| 100 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.9) | 45 976 | 49 687 | 51 571 |
| 101 | javascript (12.18)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 45 668 | 49 164 | 49 172 |
| 102 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 45 466 | 53 797 | 55 250 |
| 103 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 43 973 | 42 770 | 45 112 |
| 104 | python (3.8)| [starlette](https://starlette.io) (0.13) | 43 318 | 49 109 | 48 851 |
| 105 | javascript (12.18)| [restify](https://restify.com) (8.5) | 42 218 | 46 177 | 45 752 |
| 106 | php (7.4)| [imi](https://imiphp.com) (1.2) | 42 113 | 50 983 | 52 011 |
| 107 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 41 968 | 29 638 | 29 848 |
| 108 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 41 950 | 44 978 | 44 803 |
| 109 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.1) | 41 186 | 43 447 | 42 660 |
| 110 | javascript (12.18)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 40 734 | 43 888 | 43 442 |
| 111 | scala (2.13)| [play](https://playframework.com) (2.8) | 39 138 | 41 434 | 40 805 |
| 112 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 37 617 | 37 404 | 31 362 |
| 113 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 36 638 | 41 563 | 41 904 |
| 114 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 34 875 | 33 029 | 33 624 |
| 115 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 34 595 | 38 211 | 37 186 |
| 116 | python (3.8)| [index.py](https://index-py.abersheeran.com) (0.12) | 33 958 | 39 026 | 38 568 |
| 117 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 33 607 | 32 329 | 31 387 |
| 118 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.16) | 33 520 | 32 075 | 30 633 |
| 119 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 33 342 | 32 257 | 31 615 |
| 120 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 32 881 | 32 369 | 32 340 |
| 121 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 31 069 | 33 954 | 34 200 |
| 122 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.61) | 30 363 | 31 386 | 31 394 |
| 123 | rust (1.47)| [iron](https://ironframework.io) (0.6) | 29 844 | 30 277 | 30 057 |
| 124 | javascript (12.18)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 29 424 | 28 260 | 26 749 |
| 125 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 28 787 | 25 316 | 23 708 |
| 126 | python (3.8)| [responder](https://python-responder.org) (2.0) | 27 620 | 32 249 | 32 055 |
| 127 | php (7.4)| [swoft](https://swoft.org) (2.0) | 27 342 | 34 226 | 33 734 |
| 128 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 27 243 | 29 955 | 30 426 |
| 129 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 26 098 | 31 277 | 31 830 |
| 130 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 25 779 | 23 367 | 20 769 |
| 131 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 25 753 | 25 827 | 24 865 |
| 132 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 25 577 | 31 447 | 31 732 |
| 133 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 25 438 | 24 871 | 24 020 |
| 134 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 25 417 | 30 386 | 30 868 |
| 135 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 25 103 | 28 897 | 28 362 |
| 136 | php (7.4)| [slim-swoole](https://slimframework.com) (4.5) | 24 562 | 28 107 | 28 936 |
| 137 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.36) | 24 522 | 24 311 | 23 538 |
| 138 | javascript (12.18)| [express](https://expressjs.com) (4.17) | 24 122 | 26 497 | 26 222 |
| 139 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 652 | 26 606 | 26 580 |
| 140 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 23 266 | 23 017 | 22 520 |
| 141 | javascript (12.18)| [feathersjs](https://feathersjs.com) (4.5) | 23 142 | 26 186 | 24 008 |
| 142 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 22 576 | 24 225 | 24 643 |
| 143 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 21 170 | 21 100 | 20 588 |
| 144 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 20 582 | 20 348 | 19 954 |
| 145 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 19 672 | 19 290 | 18 884 |
| 146 | go (1.15)| [macaron](https://go-macaron.com) (1.3) | 19 538 | 21 680 | 21 959 |
| 147 | javascript (12.18)| [nestjs-express](https://nestjs.com) (7.4) | 17 965 | 17 471 | 17 818 |
| 148 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 16 910 | 16 622 | 15 610 |
| 149 | java (11)| [blade](https://lets-blade.com) (2.0) | 16 390 | 20 452 | 19 280 |
| 150 | java (11)| [struts2](https://struts.apache.org) (2.5) | 15 801 | 16 391 | 15 874 |
| 151 | fsharp (4.7)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 15 766 | 15 930 | 15 534 |
| 152 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 15 036 | 15 593 | 15 884 |
| 153 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.4) | 14 953 | 15 434 | 15 847 |
| 154 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.5) | 14 561 | 15 369 | 15 299 |
| 155 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 14 460 | 15 097 | 15 310 |
| 156 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.6) | 13 999 | 14 364 | 14 730 |
| 157 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.1) | 13 805 | 13 647 | 13 504 |
| 158 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 13 125 | 13 432 | 13 354 |
| 159 | ruby (2.7)| [grape](https://ruby-grape.org) (1.5) | 13 058 | 12 610 | 12 273 |
| 160 | swift (5.3)| [swifter-framework](https://github.com/httpswift/swifter) (1.5) | 12 384 | 12 402 | 12 424 |
| 161 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.13) | 11 453 | 11 333 | 10 636 |
| 162 | python (3.8)| [tornado](https://tornadoweb.org) (6.1) | 10 003 | 9 921 | 10 095 |
| 163 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 654 | 9 316 | 9 230 |
| 164 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 9 080 | 8 986 | 8 823 |
| 165 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 8 906 | 8 799 | 8 672 |
| 166 | python (3.8)| [django](https://djangoproject.com) (3.1) | 7 915 | 7 547 | 7 563 |
| 167 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 7 886 | 17 378 | 17 229 |
| 168 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 872 | 7 642 | 7 431 |
| 169 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 7 589 | 7 436 | 7 438 |
| 170 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 7 009 | 6 862 | 7 009 |
| 171 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 6 512 | 6 481 | 6 495 |
| 172 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 279 | 6 295 | 6 242 |
| 173 | javascript (12.18)| [sails](https://sailsjs.com) (1.3) | 6 111 | 6 125 | 6 113 |
| 174 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 6 100 | 6 048 | 6 125 |
| 175 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 6 000 | 6 024 | 6 067 |
| 176 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.2) | 5 267 | 5 277 | 5 258 |
| 177 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.4) | 5 058 | 4 992 | 5 134 |
| 178 | php (7.4)| [ice](https://iceframework.org) (1.5) | 4 978 | 4 952 | 4 977 |
| 179 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 4 915 | 10 397 | 8 287 |
| 180 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 4 547 | 4 548 | 4 536 |
| 181 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 4 226 | 4 204 | 4 215 |
| 182 | php (7.4)| [slim](https://slimframework.com) (4.5) | 3 895 | 3 859 | 3 986 |
| 183 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 3 575 | 3 597 | 3 702 |
| 184 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 3 528 | 3 541 | 3 502 |
| 185 | php (7.4)| [lumen](https://lumen.laravel.com) (8.1) | 3 506 | 3 559 | 3 621 |
| 186 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 3 485 | 3 487 | 3 506 |
| 187 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.4) | 3 079 | 3 140 | 3 180 |
| 188 | php (7.4)| [symfony](https://symfony.com) (5.1) | 2 820 | 2 824 | 2 875 |
| 189 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 2 772 | 2 525 | 2 541 |
| 190 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 2 608 | 2 707 | 2 664 |
| 191 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 510 | 2 555 | 2 564 |
| 192 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 463 | 2 486 | 2 483 |
| 193 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 934 | 1 852 | 1 769 |
| 194 | python (3.8)| [django-ninja](https://django-ninja.rest-framework.com) (0.8) | 1 815 | 2 330 | 2 401 |
| 195 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 661 | 1 681 | 1 647 |
| 196 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 614 | 1 553 | 1 544 |
| 197 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 371 | 1 400 | 1 398 |
| 198 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 1 248 | 2 091 | 4 398 |
| 199 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 060 | 1 074 | 1 172 |
| 200 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 984 | 1 508 | 2 939 |
| 201 | crystal (0.35)| [lucky](https://luckyframework.org) (0.23) | 596 | 609 | 587 |
| 202 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 434 | 441 | 437 |
| 203 | php (7.4)| [laravel](https://laravel.com) (7.27) | 266 | 176 | 1 785 |

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

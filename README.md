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

:information_source:  Updated on **2020-07-02** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 165 977 | 196 909 | 200 041 |
| 2 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.14) | 158 114 | 170 983 | 174 840 |
| 3 | javascript (13.14)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (2.1) | 157 387 | 195 960 | 200 089 |
| 4 | nim (1.2)| [whip](https://github.com/mattaylor/whip) (0.2) | 156 128 | 184 671 | 186 261 |
| 5 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 152 852 | 164 464 | 167 844 |
| 6 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.2) | 152 814 | 164 954 | 168 519 |
| 7 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.3) | 152 137 | 164 788 | 168 050 |
| 8 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 151 758 | 163 147 | 167 596 |
| 9 | javascript (13.14)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 149 365 | 185 119 | 188 203 |
| 10 | go (1.14)| [gearbox](https://gogearbox.com) (1.0) | 148 951 | 161 144 | 165 505 |
| 11 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 148 393 | 181 982 | 184 634 |
| 12 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 145 719 | 172 562 | 174 622 |
| 13 | kotlin (1.3)| [kooby](https://jooby.io) (2.8) | 142 236 | 184 825 | 189 104 |
| 14 | java (8)| [jooby](https://jooby.io) (2.8) | 141 730 | 173 701 | 182 524 |
| 15 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 140 557 | 167 413 | 166 803 |
| 16 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 139 244 | 168 444 | 167 999 |
| 17 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 137 908 | 167 594 | 169 634 |
| 18 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.1) | 136 229 | 161 102 | 160 496 |
| 19 | crystal (0.35)| [kemal](https://kemalcr.com) (0.26) | 132 231 | 153 621 | 152 326 |
| 20 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 131 585 | 152 727 | 155 024 |
| 21 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (0.28) | 129 448 | 151 951 | 150 829 |
| 22 | rust (1.44)| [actix](https://actix.rs) (2.0) | 126 830 | 135 926 | 134 603 |
| 23 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 123 991 | 147 632 | 147 461 |
| 24 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 117 868 | 147 114 | 167 264 |
| 25 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 114 721 | 146 229 | 170 744 |
| 26 | java (8)| [act](https://actframework.org) (1.9) | 105 893 | 135 215 | 137 255 |
| 27 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 105 120 | 106 689 | 111 173 |
| 28 | go (1.14)| [clevergo](https://clevergo.tech) (0.3) | 105 037 | 106 716 | 111 431 |
| 29 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 104 148 | 105 801 | 109 937 |
| 30 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 103 506 | 104 728 | 109 364 |
| 31 | go (1.14)| [fiber](https://gofiber.io) (1.12) | 102 468 | 148 831 | 158 135 |
| 32 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 101 416 | 107 322 | 111 517 |
| 33 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 100 064 | 100 048 | 104 817 |
| 34 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 99 062 | 104 952 | 108 660 |
| 35 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 98 183 | 99 728 | 103 866 |
| 36 | go (1.14)| [violetear](https://violetear.org) (7.0) | 97 293 | 98 135 | 102 658 |
| 37 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 95 799 | 109 086 | 111 967 |
| 38 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 95 634 | 96 420 | 100 866 |
| 39 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 94 670 | 94 187 | 98 521 |
| 40 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 93 898 | 100 588 | 102 939 |
| 41 | fsharp (4.7)| [frank](https://github.com/frank-fs/frank) (6.1) | 93 259 | 105 178 | 108 114 |
| 42 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 92 705 | 91 743 | 96 124 |
| 43 | go (1.14)| [beego](https://beego.me) (1.12) | 91 105 | 96 647 | 100 809 |
| 44 | javascript (13.14)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 87 673 | 97 418 | 97 302 |
| 45 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.9) | 87 034 | 105 278 | 105 673 |
| 46 | javascript (13.14)| [0http](https://github.com/jkyberneees/0http) (2.5) | 82 332 | 93 213 | 93 187 |
| 47 | fsharp (4.7)| [falco](https://github.com/pimbrouwers/Falco) (1.2) | 82 011 | 91 818 | 93 546 |
| 48 | go (1.14)| [air](https://github.com/aofei/air) (0.17) | 80 638 | 81 879 | 85 959 |
| 49 | fsharp (4.7)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 79 895 | 87 301 | 88 211 |
| 50 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 78 541 | 87 247 | 89 513 |
| 51 | c (99)| [kore](https://kore.io) (3.3) | 77 907 | 122 140 | 146 276 |
| 52 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 77 526 | 79 774 | 75 202 |
| 53 | javascript (13.14)| [polka](https://github.com/lukeed/polka) (0.5) | 77 390 | 83 636 | 81 766 |
| 54 | javascript (13.14)| [restana](https://github.com/jkyberneees/ana) (4.6) | 77 248 | 86 704 | 86 092 |
| 55 | javascript (13.14)| [rayo](https://rayo.js.org) (1.3) | 75 649 | 82 081 | 81 401 |
| 56 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 74 647 | 90 247 | 97 015 |
| 57 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 74 594 | 87 058 | 86 618 |
| 58 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.1) | 74 280 | 75 106 | 67 288 |
| 59 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 72 960 | 103 961 | 116 476 |
| 60 | php (7.4)| [hyperf](https://www.hyperf.io) (1.1) | 72 590 | 89 430 | 96 174 |
| 61 | swift (5.2)| [perfect](https://perfect.org) (3.1) | 72 574 | 82 459 | 88 056 |
| 62 | go (1.14)| [gf](https://goframe.org) (1.13) | 71 806 | 78 976 | 80 362 |
| 63 | java (8)| [javalin](https://javalin.io) (3.9) | 70 410 | 75 888 | 76 176 |
| 64 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 68 327 | 89 541 | 91 952 |
| 65 | javascript (13.14)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 67 218 | 74 514 | 72 627 |
| 66 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 66 718 | 74 676 | 75 883 |
| 67 | fsharp (4.7)| [saturn](https://saturnframework.org) (0.14) | 65 441 | 62 287 | 54 855 |
| 68 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 63 670 | 71 194 | 70 707 |
| 69 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.7) | 63 242 | 69 560 | 69 108 |
| 70 | javascript (13.14)| [fastify](https://fastify.io) (2.15) | 63 198 | 68 934 | 66 730 |
| 71 | javascript (13.14)| [foxify](https://foxify.js.org) (0.1) | 62 355 | 66 486 | 65 063 |
| 72 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 59 983 | 60 986 | 64 846 |
| 73 | java (8)| [micronaut](https://micronaut.io) (1.2) | 59 388 | 68 676 | 69 830 |
| 74 | fsharp (4.7)| [websharper](https://websharper.com) (4.6) | 57 731 | 59 854 | 56 259 |
| 75 | javascript (13.14)| [koa](https://koajs.com) (2.13) | 57 447 | 61 793 | 60 041 |
| 76 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 57 418 | 63 837 | 68 078 |
| 77 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 55 943 | 61 222 | 61 433 |
| 78 | javascript (13.14)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 54 896 | 59 586 | 58 026 |
| 79 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 53 950 | 55 505 | 53 924 |
| 80 | javascript (13.14)| [nestjs-fastify](https://nestjs.com) (7.3) | 53 387 | 60 835 | 58 443 |
| 81 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 53 383 | 61 816 | 61 835 |
| 82 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.26) | 51 503 | 59 442 | 60 323 |
| 83 | javascript (13.14)| [feathersjs](https://feathersjs.com) (4.5) | 50 664 | 53 965 | 53 026 |
| 84 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 50 396 | 51 140 | 50 905 |
| 85 | javascript (13.14)| [express](https://expressjs.com) (4.17) | 50 244 | 53 722 | 52 523 |
| 86 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 49 944 | 67 983 | 75 292 |
| 87 | swift (5.2)| [kitura](https://kitura.io) (2.9) | 49 549 | 50 257 | 50 441 |
| 88 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 49 093 | 55 786 | 56 266 |
| 89 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 47 447 | 45 738 | 47 530 |
| 90 | rust (1.44)| [gotham](https://gotham.rs) (0.4) | 47 245 | 53 699 | 55 830 |
| 91 | rust (1.44)| [nickel](https://nickel-org.github.io) (0.11) | 47 026 | 45 740 | 44 538 |
| 92 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 46 823 | 49 866 | 50 371 |
| 93 | javascript (13.14)| [moleculer](https://moleculer.services) (0.14) | 46 813 | 49 391 | 48 712 |
| 94 | swift (5.2)| [vapor](https://vapor.codes) (4.14) | 46 449 | 48 631 | 49 252 |
| 95 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 45 354 | 51 694 | 52 311 |
| 96 | python (3.8)| [hug](https://hug.rest) (2.6) | 44 275 | 46 987 | 47 557 |
| 97 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 42 733 | 43 460 | 43 157 |
| 98 | python (3.8)| [starlette](https://starlette.io) (0.13) | 42 642 | 46 221 | 46 039 |
| 99 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.6) | 42 372 | 47 795 | 47 776 |
| 100 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 42 156 | 48 176 | 47 339 |
| 101 | php (7.4)| [imi](https://imiphp.com) (1.2) | 41 148 | 46 624 | 47 791 |
| 102 | javascript (13.14)| [hapi](https://hapijs.com) (19.1) | 40 363 | 43 181 | 42 672 |
| 103 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 40 299 | 41 171 | 41 250 |
| 104 | javascript (13.14)| [nestjs-express](https://nestjs.com) (7.3) | 38 514 | 40 717 | 40 400 |
| 105 | javascript (13.14)| [restify](https://restify.com) (8.5) | 37 259 | 40 144 | 39 493 |
| 106 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.1) | 36 684 | 37 088 | 35 884 |
| 107 | scala (2.13)| [play](https://playframework.com) (2.8) | 34 873 | 37 214 | 37 336 |
| 108 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 34 511 | 39 245 | 39 483 |
| 109 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 34 075 | 33 155 | 29 894 |
| 110 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 34 059 | 34 947 | 35 105 |
| 111 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 31 292 | 33 829 | 33 035 |
| 112 | dart (2.8)| [aqueduct](https://aqueduct.io) (3.3) | 30 403 | 30 470 | 30 225 |
| 113 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 30 350 | 29 084 | 28 324 |
| 114 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.15) | 29 126 | 28 032 | 23 241 |
| 115 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 28 911 | 30 142 | 31 684 |
| 116 | python (3.8)| [responder](https://python-responder.org) (2.0) | 28 070 | 29 653 | 29 730 |
| 117 | php (7.4)| [swoft](https://swoft.org) (2.0) | 27 846 | 34 170 | 35 503 |
| 118 | nim (1.2)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 27 520 | 23 777 | 21 457 |
| 119 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.58) | 27 403 | 29 950 | 29 882 |
| 120 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 27 399 | 33 748 | 35 036 |
| 121 | nim (1.2)| [akane](https://github.com/Ethosa/akane) (0.1) | 26 621 | 23 090 | 20 822 |
| 122 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 25 848 | 31 287 | 32 526 |
| 123 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 24 645 | 28 169 | 26 759 |
| 124 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 24 541 | 25 999 | 25 735 |
| 125 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 24 185 | 26 563 | 26 712 |
| 126 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.33) | 24 146 | 26 827 | 25 747 |
| 127 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 23 875 | 27 173 | 26 076 |
| 128 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 797 | 24 996 | 24 899 |
| 129 | javascript (13.14)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 23 086 | 22 152 | 21 021 |
| 130 | rust (1.44)| [iron](https://ironframework.io) (0.6) | 23 037 | 23 356 | 23 588 |
| 131 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 22 144 | 24 328 | 23 533 |
| 132 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 21 995 | 22 856 | 22 760 |
| 133 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 21 896 | 23 016 | 22 949 |
| 134 | go (1.14)| [macaron](https://go-macaron.com) (1.3) | 20 200 | 22 882 | 23 125 |
| 135 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 19 588 | 20 452 | 20 050 |
| 136 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 18 660 | 20 353 | 19 933 |
| 137 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 17 484 | 18 190 | 17 766 |
| 138 | java (8)| [blade](https://lets-blade.com) (2.0) | 16 081 | 19 506 | 18 066 |
| 139 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 14 278 | 14 749 | 14 735 |
| 140 | dart (2.8)| [start](https://github.com/lvivski/start) (0.4) | 13 609 | 13 296 | 12 426 |
| 141 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 13 205 | 13 460 | 13 483 |
| 142 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 12 457 | 12 735 | 12 751 |
| 143 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (0.2) | 12 399 | 12 259 | 11 934 |
| 144 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 12 160 | 12 254 | 12 369 |
| 145 | javascript (13.14)| [sails](https://sailsjs.com) (1.2) | 11 989 | 12 531 | 12 573 |
| 146 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 11 436 | 11 576 | 11 656 |
| 147 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.12) | 11 088 | 11 498 | 10 889 |
| 148 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.0) | 10 113 | 10 923 | 10 933 |
| 149 | swift (5.2)| [swifter](https://github.com/httpswift/swifter) (1.4) | 9 904 | 10 029 | 10 028 |
| 150 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.4) | 9 691 | 9 520 | 9 282 |
| 151 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 524 | 9 553 | 9 362 |
| 152 | pony (0.35)| [jennet](https://github.com/Theodus/jennet) (0.1) | 9 028 | 16 634 | 15 569 |
| 153 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 007 | 9 268 | 9 178 |
| 154 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 8 108 | 7 923 | 7 723 |
| 155 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.2) | 8 055 | 7 945 | 7 795 |
| 156 | java (8)| [struts2](https://struts.apache.org) (2.5) | 7 441 | 8 939 | 8 221 |
| 157 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 7 433 | 7 490 | 7 456 |
| 158 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 7 415 | 7 459 | 7 396 |
| 159 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 6 853 | 6 740 | 6 702 |
| 160 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 6 711 | 6 576 | 6 188 |
| 161 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 6 110 | 6 016 | 6 020 |
| 162 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 5 898 | 5 797 | 5 834 |
| 163 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 5 841 | 5 783 | 5 782 |
| 164 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 5 806 | 5 730 | 5 755 |
| 165 | php (7.4)| [ice](https://iceframework.org) (1.5) | 5 055 | 5 019 | 5 049 |
| 166 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.4) | 4 951 | 4 938 | 4 959 |
| 167 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.0) | 4 627 | 4 611 | 4 653 |
| 168 | php (7.4)| [slim](https://slimframework.com) (4.5) | 3 912 | 3 923 | 3 994 |
| 169 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 3 692 | 3 714 | 3 780 |
| 170 | php (7.4)| [lumen](https://lumen.laravel.com) (7.2) | 3 553 | 3 604 | 3 655 |
| 171 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 3 522 | 3 556 | 3 590 |
| 172 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 430 | 3 228 | 3 218 |
| 173 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 242 | 7 803 | 5 969 |
| 174 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.4) | 3 140 | 3 189 | 3 230 |
| 175 | perl (5.32)| [dancer2](https://perldancer.org) (2.0) | 3 111 | 2 377 | 1 079 |
| 176 | php (7.4)| [symfony](https://symfony.com) (5.1) | 2 792 | 2 838 | 2 852 |
| 177 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 2 728 | 1 822 | 1 795 |
| 178 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 544 | 2 585 | 2 610 |
| 179 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 439 | 2 443 | 2 433 |
| 180 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 176 | 2 178 | 2 155 |
| 181 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 618 | 1 642 | 1 615 |
| 182 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 527 | 1 500 | 1 492 |
| 183 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 353 | 1 373 | 1 383 |
| 184 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 781 | 518 | 1 019 |
| 185 | crystal (0.35)| [lucky](https://luckyframework.org) (0.23) | 304 | 330 | 317 |
| 186 | php (7.4)| [laravel](https://laravel.com) (7.18) | 270 | 166 | 2 077 |

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

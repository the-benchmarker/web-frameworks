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

:information_source:  Updated on **2020-06-07** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 187 194 | 199 770 | 200 472 |
| 2 | javascript (13.14)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.11) | 179 666 | 195 557 | 189 730 |
| 3 | php (7.4)| [simps](https://simps.io) (1.0) | 175 799 | 191 078 | 192 432 |
| 4 | javascript (13.14)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 170 130 | 185 206 | 189 772 |
| 5 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.14) | 162 949 | 172 822 | 175 894 |
| 6 | java (8)| [jooby](https://jooby.io) (2.8) | 160 456 | 181 791 | 183 543 |
| 7 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 159 637 | 174 917 | 175 031 |
| 8 | javascript (13.14)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.0) | 158 207 | 171 961 | 172 158 |
| 9 | kotlin (1.3)| [kooby](https://jooby.io) (2.8) | 158 154 | 186 320 | 188 450 |
| 10 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 157 628 | 170 695 | 171 630 |
| 11 | go (1.14)| [fiber](https://gofiber.io) (1.10) | 157 534 | 164 509 | 162 250 |
| 12 | crystal (0.34)| [toro](https://github.com/soveran/toro) (0.4) | 157 376 | 163 717 | 160 174 |
| 13 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.1) | 156 849 | 166 378 | 169 696 |
| 14 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 156 611 | 166 402 | 169 311 |
| 15 | crystal (0.34)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 156 319 | 163 551 | 159 593 |
| 16 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.3) | 155 325 | 166 331 | 170 705 |
| 17 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 154 573 | 164 118 | 167 454 |
| 18 | crystal (0.34)| [spider-gazelle](https://spider-gazelle.net) (3.0) | 152 780 | 159 921 | 156 463 |
| 19 | crystal (0.34)| [grip](https://github.com/grip-framework/grip) (0.28) | 147 173 | 152 873 | 148 687 |
| 20 | crystal (0.34)| [kemal](https://kemalcr.com) (0.26) | 144 775 | 151 860 | 148 220 |
| 21 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 142 221 | 152 088 | 153 070 |
| 22 | crystal (0.34)| [amber](https://amberframework.org) (0.34) | 137 534 | 141 684 | 139 078 |
| 23 | rust (1.44)| [actix](https://actix.rs) (2.0) | 135 049 | 137 762 | 139 488 |
| 24 | crystal (0.34)| [orion](https://github.com/obsidian/orion) (2.3) | 132 241 | 135 477 | 130 884 |
| 25 | crystal (0.34)| [athena](https://github.com/athena-framework/athena) (0.8) | 124 858 | 126 515 | 119 728 |
| 26 | java (8)| [act](https://actframework.org) (1.8) | 120 779 | 132 402 | 132 376 |
| 27 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 109 826 | 108 975 | 112 698 |
| 28 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 108 926 | 108 743 | 112 032 |
| 29 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 108 702 | 107 486 | 110 916 |
| 30 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 105 999 | 108 778 | 111 994 |
| 31 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 104 365 | 102 134 | 105 641 |
| 32 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 104 189 | 106 959 | 109 760 |
| 33 | fsharp (4.7)| [frank](https://github.com/frank-fs/frank) (6.0) | 102 691 | 109 611 | 111 141 |
| 34 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 102 269 | 102 221 | 105 211 |
| 35 | go (1.14)| [violetear](https://violetear.org) (7.0) | 101 123 | 99 962 | 103 792 |
| 36 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 99 261 | 97 129 | 100 102 |
| 37 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 98 135 | 94 847 | 98 054 |
| 38 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 97 858 | 102 054 | 103 869 |
| 39 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 96 424 | 119 233 | 123 228 |
| 40 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 96 156 | 102 179 | 102 316 |
| 41 | go (1.14)| [beego](https://beego.me) (1.12) | 95 973 | 98 990 | 101 788 |
| 42 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 89 209 | 93 729 | 97 267 |
| 43 | fsharp (4.7)| [falco](https://github.com/pimbrouwers/Falco) (1.1) | 88 585 | 94 545 | 95 288 |
| 44 | fsharp (4.7)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 86 737 | 89 969 | 88 602 |
| 45 | javascript (13.14)| [0http](https://github.com/jkyberneees/0http) (2.5) | 85 021 | 92 963 | 89 895 |
| 46 | go (1.14)| [air](https://github.com/aofei/air) (0.17) | 83 407 | 84 394 | 86 397 |
| 47 | javascript (13.14)| [polka](https://github.com/lukeed/polka) (0.5) | 81 667 | 86 654 | 82 928 |
| 48 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 79 967 | 79 836 | 78 165 |
| 49 | c (99)| [kore](https://kore.io) (3.3) | 79 515 | 135 948 | 130 080 |
| 50 | javascript (13.14)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 78 811 | 91 647 | 87 981 |
| 51 | javascript (13.14)| [restana](https://github.com/jkyberneees/ana) (4.6) | 75 986 | 83 157 | 84 910 |
| 52 | javascript (13.14)| [rayo](https://rayo.js.org) (1.3) | 75 496 | 80 301 | 81 948 |
| 53 | swift (5.2)| [perfect](https://perfect.org) (3.1) | 75 278 | 83 980 | 88 688 |
| 54 | go (1.14)| [gf](https://goframe.org) (1.13) | 75 196 | 80 603 | 81 818 |
| 55 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 74 268 | 80 208 | 77 118 |
| 56 | java (8)| [javalin](https://javalin.io) (3.8) | 73 984 | 78 403 | 78 700 |
| 57 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 70 558 | 77 476 | 77 463 |
| 58 | javascript (13.14)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 69 494 | 75 670 | 74 133 |
| 59 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 68 812 | 182 189 | 184 122 |
| 60 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 67 169 | 72 134 | 72 286 |
| 61 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 66 758 | 79 332 | 79 860 |
| 62 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.2) | 66 040 | 72 664 | 71 609 |
| 63 | php (7.4)| [hyperf](https://www.hyperf.io) (1.1) | 65 297 | 69 088 | 69 442 |
| 64 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 64 596 | 63 453 | 66 231 |
| 65 | java (8)| [micronaut](https://micronaut.io) (1.2) | 63 986 | 70 963 | 70 979 |
| 66 | javascript (13.14)| [foxify](https://foxify.js.org) (0.1) | 63 624 | 67 467 | 65 508 |
| 67 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 62 643 | 67 503 | 67 627 |
| 68 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 62 605 | 64 842 | 69 197 |
| 69 | javascript (13.14)| [nestjs-fastify](https://nestjs.com) (7.1) | 60 967 | 65 313 | 63 730 |
| 70 | javascript (13.14)| [koa](https://koajs.com) (2.12) | 60 730 | 62 195 | 60 120 |
| 71 | javascript (13.14)| [fastify](https://fastify.io) (2.14) | 60 551 | 65 254 | 62 459 |
| 72 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 59 075 | 61 568 | 61 765 |
| 73 | rust (1.44)| [nickel](https://nickel-org.github.io) (0.11) | 56 105 | 55 716 | 55 766 |
| 74 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 55 273 | 56 858 | 56 541 |
| 75 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 55 157 | 62 586 | 62 345 |
| 76 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.26) | 54 920 | 59 749 | 59 429 |
| 77 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 54 895 | 55 742 | 55 517 |
| 78 | swift (5.2)| [kitura](https://kitura.io) (2.9) | 54 053 | 53 173 | 53 422 |
| 79 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 53 201 | 57 291 | 57 204 |
| 80 | javascript (13.14)| [express](https://expressjs.com) (4.17) | 53 062 | 54 505 | 54 698 |
| 81 | javascript (13.14)| [feathersjs](https://feathersjs.com) (4.5) | 52 593 | 53 880 | 52 546 |
| 82 | rust (1.44)| [gotham](https://gotham.rs) (0.4) | 52 113 | 58 921 | 59 836 |
| 83 | javascript (13.14)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 50 925 | 55 034 | 54 292 |
| 84 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 50 905 | 53 598 | 51 967 |
| 85 | swift (5.2)| [vapor](https://vapor.codes) (4.8) | 48 620 | 50 708 | 50 168 |
| 86 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 48 448 | 48 422 | 49 335 |
| 87 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 48 027 | 53 757 | 53 650 |
| 88 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 46 810 | 48 020 | 48 014 |
| 89 | php (7.4)| [imi](https://imiphp.com) (1.2) | 46 752 | 48 820 | 48 672 |
| 90 | javascript (13.14)| [moleculer](https://moleculer.services) (0.14) | 46 358 | 47 295 | 47 908 |
| 91 | python (3.8)| [hug](https://hug.rest) (2.6) | 45 592 | 47 438 | 47 449 |
| 92 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.3) | 45 577 | 49 761 | 48 768 |
| 93 | python (3.8)| [starlette](https://starlette.io) (0.13) | 44 811 | 48 026 | 47 558 |
| 94 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 44 136 | 44 542 | 44 713 |
| 95 | javascript (13.14)| [hapi](https://hapijs.com) (19.1) | 41 677 | 43 273 | 42 825 |
| 96 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 40 131 | 41 216 | 41 488 |
| 97 | javascript (13.14)| [restify](https://restify.com) (8.5) | 38 995 | 41 387 | 40 114 |
| 98 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 38 707 | 41 050 | 41 004 |
| 99 | scala (2.13)| [play](https://playframework.com) (2.8) | 38 611 | 39 934 | 39 589 |
| 100 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 38 134 | 38 530 | 35 550 |
| 101 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 37 735 | 40 690 | 40 252 |
| 102 | javascript (13.14)| [nestjs-express](https://nestjs.com) (7.1) | 37 147 | 39 593 | 40 277 |
| 103 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 36 042 | 38 628 | 36 325 |
| 104 | php (7.4)| [swoft](https://swoft.org) (2.0) | 35 933 | 37 219 | 36 905 |
| 105 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 35 811 | 36 277 | 36 357 |
| 106 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.32) | 34 931 | 36 276 | 34 682 |
| 107 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 34 733 | 35 817 | 35 623 |
| 108 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 33 448 | 34 443 | 34 214 |
| 109 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 32 724 | 32 353 | 34 207 |
| 110 | dart (2.8)| [aqueduct](https://aqueduct.io) (3.3) | 32 062 | 32 020 | 31 212 |
| 111 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 32 048 | 32 223 | 31 090 |
| 112 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 30 474 | 29 529 | 28 708 |
| 113 | crystal (0.34)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.15) | 29 452 | 27 529 | 25 838 |
| 114 | python (3.8)| [responder](https://python-responder.org) (2.0) | 29 113 | 30 911 | 30 510 |
| 115 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.55) | 28 387 | 30 706 | 30 445 |
| 116 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 26 348 | 26 327 | 24 834 |
| 117 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 26 196 | 26 845 | 26 840 |
| 118 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 25 929 | 25 537 | 25 047 |
| 119 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 25 435 | 25 927 | 25 661 |
| 120 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 25 298 | 27 167 | 27 287 |
| 121 | go (1.14)| [macaron](https://go-macaron.com) (1.3) | 25 090 | 27 803 | 28 001 |
| 122 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 24 628 | 25 062 | 25 459 |
| 123 | rust (1.44)| [iron](https://ironframework.io) (0.6) | 24 033 | 24 214 | 24 129 |
| 124 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 23 457 | 24 034 | 23 728 |
| 125 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 23 411 | 24 014 | 23 822 |
| 126 | javascript (13.14)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 22 294 | 21 465 | 20 314 |
| 127 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 766 | 18 013 | 18 207 |
| 128 | java (8)| [blade](https://lets-blade.com) (2.0) | 17 462 | 20 522 | 19 621 |
| 129 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 17 336 | 17 642 | 17 666 |
| 130 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 15 276 | 14 672 | 14 687 |
| 131 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 14 555 | 14 934 | 14 825 |
| 132 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 14 147 | 13 995 | 14 592 |
| 133 | dart (2.8)| [start](https://github.com/lvivski/start) (0.4) | 14 067 | 13 758 | 12 878 |
| 134 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.12) | 11 918 | 11 769 | 11 037 |
| 135 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.2) | 11 414 | 11 265 | 11 152 |
| 136 | javascript (13.14)| [sails](https://sailsjs.com) (1.2) | 11 402 | 11 830 | 13 171 |
| 137 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 11 206 | 10 623 | 10 858 |
| 138 | swift (5.2)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 011 | 11 049 | 11 018 |
| 139 | pony (0.35)| [jennet](https://github.com/Theodus/jennet) (0.1) | 10 714 | 17 373 | 16 072 |
| 140 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 10 485 | 10 488 | 10 496 |
| 141 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 890 | 9 757 | 9 573 |
| 142 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 876 | 10 323 | 10 085 |
| 143 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 301 | 9 295 | 9 281 |
| 144 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 181 | 9 092 | 8 988 |
| 145 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 8 032 | 8 081 | 7 986 |
| 146 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 7 765 | 7 821 | 7 763 |
| 147 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 626 | 7 700 | 7 680 |
| 148 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 576 | 7 660 | 7 555 |
| 149 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 265 | 6 285 | 6 425 |
| 150 | crystal (0.34)| [onyx](https://onyxframework.org) (0.5) | 5 851 | 6 038 | 6 019 |
| 151 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 5 752 | 5 799 | 5 917 |
| 152 | php (7.4)| [slim](https://slimframework.com) (4.5) | 4 688 | 4 784 | 4 842 |
| 153 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 4 358 | 4 465 | 4 527 |
| 154 | php (7.4)| [lumen](https://lumen.laravel.com) (7.1) | 4 285 | 4 342 | 4 430 |
| 155 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 164 | 4 234 | 4 348 |
| 156 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 910 | 3 707 | 3 508 |
| 157 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 218 | 7 827 | 6 185 |
| 158 | php (7.4)| [symfony](https://symfony.com) (5.1) | 3 177 | 3 227 | 3 290 |
| 159 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 856 | 2 921 | 2 984 |
| 160 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 224 | 2 235 | 2 207 |
| 161 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 1 848 | 1 664 | 805 |
| 162 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 589 | 1 622 | 1 602 |
| 163 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 552 | 1 514 | 1 489 |
| 164 | crystal (0.34)| [lucky](https://luckyframework.org) (0.21) | 1 469 | 1 506 | 1 489 |
| 165 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 440 | 1 467 | 1 465 |
| 166 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 534 | 442 | 1 838 |
| 167 | php (7.4)| [laravel](https://laravel.com) (7.14) | 208 | 175 | 1 938 |
| 168 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (4.0) | 0 | 0 | 0 |

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

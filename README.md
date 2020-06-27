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

:information_source:  Updated on **2020-06-27** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 190 340 | 202 659 | 202 115 |
| 2 | javascript (13.14)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 185 556 | 199 141 | 200 556 |
| 3 | nim (1.2)| [whip](https://github.com/mattaylor/whip) (0.2) | 178 379 | 189 242 | 188 152 |
| 4 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.0) | 174 491 | 187 228 | 187 387 |
| 5 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 173 572 | 184 756 | 186 406 |
| 6 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.14) | 170 629 | 182 636 | 183 486 |
| 7 | javascript (13.14)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.12) | 169 607 | 183 880 | 193 657 |
| 8 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 166 822 | 174 088 | 172 099 |
| 9 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.2) | 165 390 | 175 802 | 181 657 |
| 10 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 165 226 | 175 439 | 180 839 |
| 11 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.3) | 164 909 | 175 445 | 181 208 |
| 12 | kotlin (1.3)| [kooby](https://jooby.io) (2.8) | 164 741 | 189 938 | 188 712 |
| 13 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 164 145 | 172 836 | 168 191 |
| 14 | go (1.14)| [fiber](https://gofiber.io) (1.12) | 162 811 | 168 435 | 167 845 |
| 15 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 160 943 | 171 941 | 165 275 |
| 16 | go (1.14)| [gearbox](https://gogearbox.com) (1.0) | 160 796 | 171 898 | 177 433 |
| 17 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.1) | 160 768 | 168 126 | 164 401 |
| 18 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 160 412 | 170 013 | 168 652 |
| 19 | java (8)| [jooby](https://jooby.io) (2.8) | 159 977 | 178 536 | 183 918 |
| 20 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 159 569 | 171 062 | 174 250 |
| 21 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (0.28) | 154 926 | 160 505 | 155 110 |
| 22 | crystal (0.35)| [kemal](https://kemalcr.com) (0.26) | 153 401 | 159 820 | 155 448 |
| 23 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 146 832 | 156 438 | 156 132 |
| 24 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 144 401 | 149 110 | 144 676 |
| 25 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (2.3) | 139 020 | 142 597 | 137 564 |
| 26 | java (8)| [act](https://actframework.org) (1.8) | 126 456 | 140 114 | 140 106 |
| 27 | rust (1.44)| [actix](https://actix.rs) (2.0) | 125 365 | 128 679 | 131 417 |
| 28 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 116 296 | 116 757 | 119 800 |
| 29 | go (1.14)| [clevergo](https://clevergo.tech) (0.3) | 116 040 | 116 608 | 121 773 |
| 30 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 114 726 | 114 949 | 119 711 |
| 31 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 112 375 | 116 756 | 120 932 |
| 32 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 110 491 | 114 760 | 119 128 |
| 33 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 110 370 | 109 293 | 113 872 |
| 34 | fsharp (4.7)| [frank](https://github.com/frank-fs/frank) (6.1) | 109 234 | 119 027 | 120 177 |
| 35 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 109 155 | 110 072 | 114 419 |
| 36 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 108 087 | 101 923 | 107 155 |
| 37 | go (1.14)| [violetear](https://violetear.org) (7.0) | 107 650 | 107 541 | 112 111 |
| 38 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 106 658 | 104 746 | 109 153 |
| 39 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 106 580 | 104 470 | 110 965 |
| 40 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 104 619 | 109 656 | 112 868 |
| 41 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 104 403 | 127 969 | 134 044 |
| 42 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 103 698 | 101 205 | 105 486 |
| 43 | go (1.14)| [beego](https://beego.me) (1.12) | 103 696 | 106 841 | 111 503 |
| 44 | c (99)| [kore](https://kore.io) (3.3) | 103 121 | 102 557 | 129 896 |
| 45 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 102 599 | 114 350 | 119 325 |
| 46 | fsharp (4.7)| [falco](https://github.com/pimbrouwers/Falco) (1.2) | 98 081 | 104 710 | 110 802 |
| 47 | fsharp (4.7)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 98 008 | 102 873 | 102 266 |
| 48 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.9) | 97 332 | 108 487 | 105 500 |
| 49 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 95 558 | 101 486 | 101 948 |
| 50 | javascript (13.14)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 95 505 | 104 561 | 105 243 |
| 51 | php (7.4)| [hyperf](https://www.hyperf.io) (1.1) | 94 449 | 98 961 | 100 887 |
| 52 | javascript (13.14)| [0http](https://github.com/jkyberneees/0http) (2.5) | 90 753 | 99 812 | 100 093 |
| 53 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 90 259 | 94 055 | 97 162 |
| 54 | go (1.14)| [air](https://github.com/aofei/air) (0.17) | 88 663 | 87 917 | 92 789 |
| 55 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 84 731 | 99 768 | 101 446 |
| 56 | javascript (13.14)| [polka](https://github.com/lukeed/polka) (0.5) | 84 582 | 89 051 | 87 637 |
| 57 | javascript (13.14)| [rayo](https://rayo.js.org) (1.3) | 82 927 | 88 799 | 85 121 |
| 58 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 81 531 | 89 184 | 85 867 |
| 59 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 79 852 | 80 159 | 78 281 |
| 60 | javascript (13.14)| [restana](https://github.com/jkyberneees/ana) (4.6) | 78 884 | 87 796 | 87 484 |
| 61 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.1) | 78 599 | 78 030 | 70 996 |
| 62 | fsharp (4.7)| [saturn](https://saturnframework.org) (0.14) | 76 803 | 69 755 | 61 297 |
| 63 | swift (5.2)| [perfect](https://perfect.org) (3.1) | 76 597 | 85 970 | 91 847 |
| 64 | javascript (13.14)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 73 777 | 78 485 | 77 110 |
| 65 | java (8)| [javalin](https://javalin.io) (3.9) | 73 356 | 77 832 | 78 185 |
| 66 | fsharp (4.7)| [websharper](https://websharper.com) (4.6) | 70 821 | 71 858 | 68 565 |
| 67 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 70 779 | 185 456 | 183 383 |
| 68 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 69 146 | 68 496 | 72 008 |
| 69 | javascript (13.14)| [foxify](https://foxify.js.org) (0.1) | 69 066 | 72 840 | 71 305 |
| 70 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 68 900 | 78 432 | 82 051 |
| 71 | javascript (13.14)| [fastify](https://fastify.io) (2.15) | 68 857 | 73 210 | 69 731 |
| 72 | go (1.14)| [gf](https://goframe.org) (1.13) | 67 824 | 72 300 | 82 353 |
| 73 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 67 066 | 73 372 | 72 632 |
| 74 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.7) | 66 835 | 70 357 | 70 149 |
| 75 | java (8)| [micronaut](https://micronaut.io) (1.2) | 66 794 | 73 387 | 73 539 |
| 76 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 64 000 | 66 971 | 72 644 |
| 77 | javascript (13.14)| [koa](https://koajs.com) (2.13) | 63 294 | 66 462 | 64 478 |
| 78 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 62 250 | 71 574 | 68 465 |
| 79 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 57 912 | 58 760 | 57 373 |
| 80 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.26) | 56 940 | 61 646 | 63 167 |
| 81 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 56 529 | 58 590 | 58 678 |
| 82 | javascript (13.14)| [express](https://expressjs.com) (4.17) | 55 699 | 57 636 | 56 067 |
| 83 | javascript (13.14)| [feathersjs](https://feathersjs.com) (4.5) | 55 678 | 57 656 | 56 892 |
| 84 | javascript (13.14)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 54 735 | 60 171 | 54 254 |
| 85 | javascript (13.14)| [nestjs-fastify](https://nestjs.com) (7.2) | 54 552 | 62 441 | 61 252 |
| 86 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 54 549 | 56 274 | 56 038 |
| 87 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 53 617 | 61 912 | 62 569 |
| 88 | rust (1.44)| [nickel](https://nickel-org.github.io) (0.11) | 51 880 | 51 313 | 51 918 |
| 89 | rust (1.44)| [gotham](https://gotham.rs) (0.4) | 50 928 | 58 478 | 60 466 |
| 90 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 50 149 | 52 746 | 51 080 |
| 91 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 49 883 | 49 778 | 50 111 |
| 92 | swift (5.2)| [kitura](https://kitura.io) (2.9) | 49 811 | 46 623 | 44 192 |
| 93 | swift (5.2)| [vapor](https://vapor.codes) (4.13) | 49 095 | 50 430 | 50 768 |
| 94 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 47 796 | 53 236 | 53 290 |
| 95 | php (7.4)| [imi](https://imiphp.com) (1.2) | 47 419 | 49 434 | 49 732 |
| 96 | python (3.8)| [hug](https://hug.rest) (2.6) | 46 738 | 48 016 | 48 914 |
| 97 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 46 285 | 54 397 | 53 964 |
| 98 | javascript (13.14)| [moleculer](https://moleculer.services) (0.14) | 45 796 | 49 122 | 48 466 |
| 99 | javascript (13.14)| [hapi](https://hapijs.com) (19.1) | 44 943 | 46 956 | 46 110 |
| 100 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 43 967 | 44 488 | 44 491 |
| 101 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 43 743 | 46 777 | 46 269 |
| 102 | javascript (13.14)| [nestjs-express](https://nestjs.com) (7.2) | 43 136 | 45 153 | 44 007 |
| 103 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 42 703 | 45 644 | 45 532 |
| 104 | php (7.4)| [swoft](https://swoft.org) (2.0) | 42 148 | 44 675 | 44 507 |
| 105 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.10) | 42 077 | 42 323 | 42 428 |
| 106 | javascript (13.14)| [restify](https://restify.com) (8.5) | 41 671 | 43 788 | 43 242 |
| 107 | python (3.8)| [starlette](https://starlette.io) (0.13) | 40 671 | 47 542 | 47 075 |
| 108 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 40 431 | 40 728 | 38 294 |
| 109 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 40 295 | 39 092 | 39 977 |
| 110 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 39 857 | 41 330 | 39 513 |
| 111 | scala (2.13)| [play](https://playframework.com) (2.8) | 39 394 | 40 863 | 40 365 |
| 112 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.3) | 39 192 | 41 044 | 39 395 |
| 113 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.1) | 38 859 | 38 385 | 39 093 |
| 114 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 38 420 | 40 747 | 40 777 |
| 115 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 37 610 | 42 177 | 41 913 |
| 116 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.33) | 36 439 | 38 202 | 36 466 |
| 117 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.5) | 35 744 | 36 198 | 36 472 |
| 118 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 34 183 | 35 130 | 35 022 |
| 119 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 33 701 | 34 765 | 33 602 |
| 120 | dart (2.8)| [aqueduct](https://aqueduct.io) (3.3) | 33 655 | 33 560 | 32 932 |
| 121 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 30 879 | 30 179 | 28 972 |
| 122 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.15) | 30 376 | 28 694 | 22 796 |
| 123 | python (3.8)| [responder](https://python-responder.org) (2.0) | 29 624 | 31 297 | 30 924 |
| 124 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 29 012 | 29 110 | 28 487 |
| 125 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.58) | 28 957 | 30 584 | 29 966 |
| 126 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 28 676 | 28 876 | 28 205 |
| 127 | nim (1.2)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 28 118 | 23 911 | 21 762 |
| 128 | nim (1.2)| [akane](https://github.com/Ethosa/akane) (0.1) | 26 743 | 23 358 | 20 966 |
| 129 | go (1.14)| [macaron](https://go-macaron.com) (1.3) | 26 113 | 28 321 | 28 551 |
| 130 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 25 309 | 25 290 | 24 833 |
| 131 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 24 981 | 24 830 | 24 483 |
| 132 | rust (1.44)| [iron](https://ironframework.io) (0.6) | 24 153 | 24 347 | 24 400 |
| 133 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 860 | 26 158 | 25 575 |
| 134 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 23 555 | 26 257 | 26 131 |
| 135 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 23 436 | 24 273 | 23 942 |
| 136 | javascript (13.14)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 23 394 | 22 741 | 21 575 |
| 137 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 22 204 | 23 296 | 23 311 |
| 138 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 18 892 | 19 303 | 19 444 |
| 139 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 17 451 | 17 368 | 17 167 |
| 140 | java (8)| [blade](https://lets-blade.com) (2.0) | 17 437 | 20 351 | 19 240 |
| 141 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 16 900 | 16 667 | 16 670 |
| 142 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 15 779 | 15 512 | 15 485 |
| 143 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 15 055 | 15 438 | 15 410 |
| 144 | dart (2.8)| [start](https://github.com/lvivski/start) (0.4) | 14 212 | 13 917 | 12 998 |
| 145 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.0) | 14 122 | 15 316 | 15 567 |
| 146 | javascript (13.14)| [sails](https://sailsjs.com) (1.2) | 12 777 | 13 268 | 13 128 |
| 147 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (0.2) | 12 354 | 12 302 | 11 880 |
| 148 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 12 299 | 12 035 | 11 901 |
| 149 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.12) | 11 728 | 11 797 | 11 308 |
| 150 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 11 552 | 11 497 | 11 404 |
| 151 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.2) | 11 467 | 11 355 | 11 035 |
| 152 | swift (5.2)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 364 | 11 366 | 11 345 |
| 153 | pony (0.35)| [jennet](https://github.com/Theodus/jennet) (0.1) | 11 206 | 17 998 | 15 897 |
| 154 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 099 | 10 040 | 9 823 |
| 155 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 744 | 9 934 | 9 483 |
| 156 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 408 | 9 365 | 9 285 |
| 157 | java (8)| [struts2](https://struts.apache.org) (2.5) | 8 785 | 9 040 | 9 716 |
| 158 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 8 622 | 8 616 | 9 113 |
| 159 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 8 019 | 8 102 | 8 126 |
| 160 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 8 006 | 8 064 | 8 057 |
| 161 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 7 740 | 7 788 | 7 718 |
| 162 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 634 | 7 656 | 7 652 |
| 163 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 6 822 | 6 059 | 5 322 |
| 164 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 438 | 6 501 | 6 514 |
| 165 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.0) | 5 708 | 5 720 | 5 914 |
| 166 | php (7.4)| [slim](https://slimframework.com) (4.5) | 4 705 | 4 730 | 4 899 |
| 167 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 4 378 | 4 425 | 4 544 |
| 168 | php (7.4)| [lumen](https://lumen.laravel.com) (7.2) | 4 307 | 4 348 | 4 463 |
| 169 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 4 146 | 4 033 | 3 988 |
| 170 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 125 | 4 201 | 4 312 |
| 171 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 311 | 8 087 | 6 405 |
| 172 | php (7.4)| [symfony](https://symfony.com) (5.1) | 3 188 | 3 239 | 3 290 |
| 173 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 2 923 | 1 770 | 1 753 |
| 174 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 854 | 2 912 | 2 976 |
| 175 | perl (5.32)| [dancer2](https://perldancer.org) (2.0) | 2 816 | 1 487 | 926 |
| 176 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 455 | 2 459 | 2 442 |
| 177 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 319 | 2 333 | 2 349 |
| 178 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 629 | 1 641 | 1 627 |
| 179 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 479 | 1 444 | 1 416 |
| 180 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 445 | 1 461 | 1 482 |
| 181 | crystal (0.35)| [lucky](https://luckyframework.org) (0.22) | 1 209 | 1 226 | 1 237 |
| 182 | php (7.4)| [laravel](https://laravel.com) (7.17) | 855 | 176 | 3 935 |
| 183 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 785 | 506 | 852 |

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

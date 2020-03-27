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

:information_source: you need `wrk` **stable**

~~~sh
git clone --branch 4.1.0 https://github.com/wg/wrk
~~~

:warning: `docker` is used for **development** purpose, `production` results will be computed on [DigitalOcean](https://www.digitalocean.com) :warning:

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

:information_source:  Updated on **2020-03-27** :information_source:

> Benchmarking with [wrk](https://github.com/ioquatix/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 183 647 | 197 283 | 196 742 | 195 569 | 193 794 |
| 2 | javascript (13.7)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.1) | 179 112 | 191 584 | 191 675 | 187 896 | 187 864 |
| 3 | javascript (13.7)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.0) | 170 051 | 181 682 | 181 320 | 178 148 | 177 995 |
| 4 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (2.2) | 168 932 | 180 582 | 178 859 | 176 551 | 181 523 |
| 5 | go (1.14)| [fiber](https://fiber.wiki) (1.8) | 160 146 | 162 270 | 161 958 | 152 205 | 151 235 |
| 6 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.9) | 155 690 | 163 680 | 166 323 | 160 932 | 161 068 |
| 7 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 152 293 | 166 799 | 166 777 | 162 178 | 161 485 |
| 8 | crystal (0.33)| [toro](https://github.com/soveran/toro) (0.4) | 151 234 | 157 769 | 154 645 | 145 747 | 144 937 |
| 9 | go (1.14)| [router](https://github.com/fasthttp/router) (0.7) | 150 018 | 157 945 | 161 385 | 155 926 | 156 010 |
| 10 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 149 410 | 158 459 | 161 145 | 155 663 | 155 264 |
| 11 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.4) | 149 120 | 157 977 | 160 701 | 155 977 | 155 441 |
| 12 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 146 886 | 155 835 | 157 976 | 153 268 | 152 703 |
| 13 | crystal (0.33)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 146 722 | 153 703 | 150 649 | 142 333 | 142 242 |
| 14 | crystal (0.33)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 146 307 | 153 236 | 149 977 | 142 439 | 142 070 |
| 15 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 141 921 | 151 633 | 152 618 | 150 479 | 149 636 |
| 16 | crystal (0.33)| [grip](https://github.com/grip-framework/grip) (0.28) | 140 170 | 145 935 | 142 433 | 134 000 | 133 848 |
| 17 | crystal (0.33)| [kemal](https://kemalcr.com) (0.26) | 139 848 | 145 038 | 141 466 | 134 800 | 134 173 |
| 18 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 137 441 | 146 401 | 147 380 | 142 014 | 141 417 |
| 19 | crystal (0.33)| [amber](https://amberframework.org) (0.33) | 133 350 | 137 960 | 135 152 | 126 846 | 126 228 |
| 20 | crystal (0.33)| [lucky](https://luckyframework.org) (0.19) | 129 301 | 133 136 | 127 937 | 121 425 | 120 166 |
| 21 | crystal (0.33)| [orion](https://github.com/obsidian/orion) (2.3) | 126 771 | 130 980 | 124 935 | 116 477 | 115 140 |
| 22 | crystal (0.33)| [athena](https://github.com/athena-framework/athena) (0.8) | 119 878 | 122 746 | 117 662 | 108 425 | 107 866 |
| 23 | java (8)| [act](https://actframework.org) (1.8) | 116 138 | 128 292 | 129 974 | 126 622 | 125 959 |
| 24 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 105 763 | 104 419 | 108 092 | 107 633 | 107 143 |
| 25 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 104 357 | 151 027 | 171 975 | 179 272 | 178 030 |
| 26 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 104 014 | 104 044 | 106 474 | 105 585 | 105 562 |
| 27 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.0) | 100 109 | 98 906 | 101 722 | 100 972 | 100 520 |
| 28 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 99 538 | 103 476 | 105 576 | 104 280 | 104 223 |
| 29 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 97 906 | 98 184 | 100 758 | 100 060 | 99 816 |
| 30 | go (1.14)| [violetear](https://violetear.org) (7.0) | 96 907 | 96 923 | 100 046 | 99 526 | 99 335 |
| 31 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 95 464 | 115 995 | 119 991 | 120 250 | 119 833 |
| 32 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 95 311 | 94 013 | 97 006 | 96 854 | 95 860 |
| 33 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 95 021 | 93 856 | 96 518 | 96 680 | 96 474 |
| 34 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 94 677 | 98 362 | 99 669 | 97 946 | 97 409 |
| 35 | c (99)| [kore](https://kore.io) (3.3) | 93 277 | 148 644 | 153 052 | 155 751 | 153 952 |
| 36 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 92 891 | 91 557 | 93 926 | 94 154 | 93 588 |
| 37 | go (1.14)| [beego](https://beego.me) (1.12) | 92 711 | 95 800 | 98 269 | 97 718 | 97 170 |
| 38 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 92 490 | 99 382 | 100 176 | 98 234 | 98 443 |
| 39 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 91 354 | 90 312 | 93 117 | 93 369 | 93 237 |
| 40 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 90 673 | 94 516 | 96 540 | 96 102 | 95 685 |
| 41 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 90 018 | 98 782 | 100 381 | 95 693 | 93 035 |
| 42 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 86 760 | 90 482 | 91 301 | 89 943 | 90 032 |
| 43 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 85 404 | 94 456 | 95 190 | 91 908 | 89 928 |
| 44 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 81 655 | 85 988 | 84 585 | 81 801 | 81 349 |
| 45 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.1) | 80 471 | 86 510 | 84 289 | 84 105 | 81 871 |
| 46 | go (1.14)| [air](https://github.com/aofei/air) (0.15) | 79 229 | 83 282 | 85 130 | 84 591 | 84 163 |
| 47 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 78 969 | 83 659 | 82 475 | 79 743 | 77 614 |
| 48 | java (8)| [javalin](https://javalin.io) (3.5) | 73 196 | 77 682 | 76 662 | 75 046 | 74 706 |
| 49 | go (1.14)| [gf](https://goframe.org) (1.11) | 73 169 | 78 398 | 79 731 | 79 587 | 79 271 |
| 50 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 72 795 | 80 041 | 85 077 | 84 059 | 83 516 |
| 51 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 72 242 | 75 539 | 73 199 | 71 090 | 72 069 |
| 52 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 69 086 | 73 991 | 74 308 | 72 556 | 72 457 |
| 53 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 68 356 | 75 395 | 74 677 | 72 927 | 72 699 |
| 54 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 66 790 | 71 006 | 69 605 | 67 288 | 67 103 |
| 55 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 65 843 | 69 121 | 67 273 | 65 714 | 65 225 |
| 56 | javascript (13.7)| [fastify](https://fastify.io) (2.13) | 64 065 | 69 772 | 66 615 | 65 951 | 65 211 |
| 57 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 63 757 | 75 116 | 77 343 | 76 675 | 76 910 |
| 58 | java (8)| [micronaut](https://micronaut.io) (1.2) | 62 778 | 69 712 | 69 947 | 67 939 | 67 768 |
| 59 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 62 507 | 63 753 | 63 405 | 61 932 | 61 927 |
| 60 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 61 437 | 65 325 | 65 532 | 64 856 | 64 541 |
| 61 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 59 303 | 62 423 | 64 541 | 64 239 | 64 258 |
| 62 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 58 697 | 61 456 | 59 506 | 57 634 | 56 980 |
| 63 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 57 834 | 56 649 | 56 736 | 57 078 | 56 889 |
| 64 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 57 659 | 60 465 | 58 545 | 57 093 | 56 794 |
| 65 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.14) | 55 243 | 61 393 | 61 128 | 58 688 | 58 409 |
| 66 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 54 002 | 60 064 | 60 017 | 59 194 | 58 950 |
| 67 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 53 657 | 55 026 | 54 720 | 54 678 | 53 700 |
| 68 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 53 549 | 60 894 | 60 625 | 60 170 | 59 673 |
| 69 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 52 793 | 60 506 | 61 290 | 61 311 | 60 819 |
| 70 | javascript (13.7)| [feathersjs](https://feathersjs.com) (4.5) | 52 699 | 54 782 | 53 440 | 52 378 | 52 481 |
| 71 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 52 311 | 54 434 | 53 171 | 52 179 | 52 099 |
| 72 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 50 465 | 55 668 | 55 290 | 53 406 | 53 270 |
| 73 | javascript (13.7)| [moleculer](https://moleculer.services) (0.14) | 48 543 | 51 217 | 50 310 | 50 459 | 49 442 |
| 74 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 47 944 | 48 648 | 47 345 | 46 664 | 46 471 |
| 75 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 47 607 | 47 914 | 48 977 | 48 160 | 46 282 |
| 76 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 47 535 | 48 029 | 48 211 | 45 869 | 45 899 |
| 77 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 46 933 | 48 254 | 50 003 | 49 193 | 49 395 |
| 78 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 46 466 | 48 310 | 48 720 | 47 922 | 47 967 |
| 79 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 46 320 | 50 171 | 50 249 | 48 509 | 48 137 |
| 80 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 46 038 | 46 249 | 46 167 | 45 534 | 45 709 |
| 81 | python (3.8)| [hug](https://hug.rest) (2.6) | 44 118 | 46 659 | 46 449 | 45 961 | 46 024 |
| 82 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 43 569 | 49 104 | 52 496 | 53 032 | 53 591 |
| 83 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 42 707 | 45 050 | 42 941 | 44 627 | 44 579 |
| 84 | javascript (13.7)| [restify](https://restify.com) (8.5) | 42 303 | 43 334 | 42 921 | 43 202 | 42 741 |
| 85 | python (3.8)| [starlette](https://starlette.io) (0.13) | 42 218 | 46 489 | 45 989 | 44 265 | 44 174 |
| 86 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 40 292 | 42 373 | 42 208 | 41 741 | 41 661 |
| 87 | php (7.4)| [imi](https://imiphp.com) (1.0) | 39 857 | 40 923 | 40 982 | 40 633 | 40 558 |
| 88 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 38 822 | 39 529 | 39 671 | 38 073 | 38 080 |
| 89 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 38 133 | 39 201 | 36 987 | 36 823 | 37 209 |
| 90 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0b1) | 37 475 | 40 084 | 39 694 | 37 916 | 37 800 |
| 91 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 36 784 | 38 828 | 36 650 | 37 190 | 37 314 |
| 92 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.3) | 35 565 | 36 467 | 35 045 | 34 935 | 35 102 |
| 93 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 34 801 | 35 518 | 35 230 | 33 989 | 33 910 |
| 94 | php (7.4)| [swoft](https://swoft.org) (2.0) | 34 631 | 35 857 | 35 554 | 35 273 | 35 170 |
| 95 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 32 967 | 32 813 | 31 695 | 31 581 | 31 890 |
| 96 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 30 686 | 31 083 | 30 888 | 29 974 | 29 826 |
| 97 | dart (2.7)| [aqueduct](https://aqueduct.io) (3.2) | 30 551 | 29 569 | 30 778 | 29 865 | 30 079 |
| 98 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.52) | 29 147 | 30 477 | 30 131 | 28 999 | 28 986 |
| 99 | python (3.8)| [responder](https://python-responder.org) (2.0) | 28 259 | 29 494 | 29 383 | 28 400 | 28 329 |
| 100 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 27 610 | 26 989 | 26 541 | 26 332 | 26 351 |
| 101 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 26 682 | 26 695 | 26 079 | 25 994 | 26 089 |
| 102 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 25 650 | 26 108 | 25 111 | 25 448 | 25 417 |
| 103 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 24 769 | 26 928 | 26 829 | 26 020 | 25 921 |
| 104 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 23 831 | 26 895 | 31 509 | 28 546 | 30 949 |
| 105 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 23 762 | 23 995 | 23 561 | 23 564 | 23 680 |
| 106 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 23 612 | 23 799 | 23 536 | 23 479 | 23 210 |
| 107 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 294 | 25 344 | 25 090 | 24 646 | 24 559 |
| 108 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 23 161 | 23 467 | 23 199 | 22 831 | 22 890 |
| 109 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 23 035 | 21 633 | 20 697 | 20 626 | 20 446 |
| 110 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 18 882 | 18 994 | 19 058 | 18 956 | 19 017 |
| 111 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 18 174 | 17 846 | 17 442 | 17 159 | 17 184 |
| 112 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 614 | 17 891 | 17 955 | 17 604 | 17 718 |
| 113 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 14 595 | 15 030 | 14 910 | 15 014 | 14 959 |
| 114 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 13 844 | 13 734 | 13 590 | 13 623 | 13 583 |
| 115 | javascript (13.7)| [sails](https://sailsjs.com) (1.2) | 12 169 | 12 758 | 12 070 | 12 787 | 12 605 |
| 116 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 11 237 | 11 056 | 11 054 | 10 930 | 10 961 |
| 117 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 11 203 | 11 039 | 11 347 | 11 050 | 11 122 |
| 118 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 11 135 | 11 393 | 11 079 | 10 554 | 10 535 |
| 119 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 10 356 | 10 308 | 10 291 | 10 287 | 10 325 |
| 120 | python (3.8)| [django](https://djangoproject.com) (3.0) | 9 526 | 9 712 | 9 591 | 8 472 | 9 541 |
| 121 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 272 | 10 086 | 9 911 | 9 816 | 9 689 |
| 122 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 976 | 8 919 | 8 787 | 48 880 | 48 174 |
| 123 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 8 441 | 8 916 | 8 811 | 8 804 | 8 754 |
| 124 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 7 770 | 7 658 | 7 649 | 42 759 | 42 233 |
| 125 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 354 | 7 327 | 7 308 | 48 286 | 46 552 |
| 126 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 339 | 7 358 | 7 298 | 40 877 | 40 013 |
| 127 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 6 258 | 4 807 | 0 | 0 | 279 |
| 128 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 5 513 | 5 531 | 5 496 | 42 274 | 41 521 |
| 129 | crystal (0.33)| [onyx](https://onyxframework.org) (0.5) | 5 319 | 5 468 | 5 465 | 5 498 | 5 464 |
| 130 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 543 | 4 526 | 4 614 | 42 283 | 38 687 |
| 131 | php (7.4)| [lumen](https://lumen.laravel.com) (7.0) | 4 168 | 4 166 | 4 182 | 40 055 | 37 648 |
| 132 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 3 986 | 3 950 | 4 045 | 41 175 | 40 343 |
| 133 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 837 | 3 676 | 3 672 | 3 662 | 3 662 |
| 134 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 467 | 3 483 | 3 570 | 39 729 | 37 817 |
| 135 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 793 | 2 842 | 2 915 | 39 711 | 37 385 |
| 136 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 407 | 2 420 | 2 361 | 2 385 | 2 369 |
| 137 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 1 809 | 5 514 | 4 416 | 3 390 | 2 057 |
| 138 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 610 | 1 616 | 1 601 | 1 598 | 1 598 |
| 139 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 519 | 1 491 | 1 470 | 1 446 | 1 441 |
| 140 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 1 506 | 1 982 | 1 303 | 1 174 | 877 |
| 141 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 399 | 1 436 | 1 482 | 37 831 | 34 318 |
| 142 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 513 | 428 | 1 252 | 34 246 | 31 520 |
| 143 | php (7.4)| [laravel](https://laravel.com) (7.3) | 230 | 151 | 3 016 | 22 673 | 21 715 |

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

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

:information_source:  Updated on **2020-03-18** :information_source:

> Benchmarking with [wrk](https://github.com/ioquatix/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |
|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|
| 1 | javascript (13.7)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.1) | 183 728 | 197 134 | 196 947 | 194 578 | 194 773 |
| 2 | nim (1.0)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 180 611 | 193 482 | 195 386 | 192 742 | 193 068 |
| 3 | javascript (13.7)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (1.1) | 174 356 | 186 232 | 186 246 | 183 496 | 182 929 |
| 4 | javascript (13.7)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 167 365 | 180 755 | 180 270 | 177 241 | 178 413 |
| 5 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.9) | 163 725 | 173 226 | 176 657 | 170 952 | 168 921 |
| 6 | java (8)| [rapidoid](https://rapidoid.org) (5.5) | 158 044 | 173 085 | 173 352 | 167 764 | 168 295 |
| 7 | go (1.14)| [router](https://github.com/fasthttp/router) (0.6) | 157 867 | 167 150 | 170 613 | 166 241 | 166 073 |
| 8 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 157 526 | 166 128 | 169 262 | 165 697 | 165 704 |
| 9 | go (1.14)| [fiber](https://fiber.wiki) (1.8) | 157 099 | 164 796 | 164 369 | 156 427 | 157 015 |
| 10 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (10.4) | 157 068 | 165 874 | 168 996 | 165 092 | 165 341 |
| 11 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 155 150 | 164 337 | 167 334 | 163 077 | 162 974 |
| 12 | crystal (0.33)| [toro](https://github.com/soveran/toro) (0.4) | 151 474 | 158 944 | 155 750 | 146 367 | 146 059 |
| 13 | crystal (0.33)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 151 209 | 157 671 | 153 778 | 145 169 | 145 982 |
| 14 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 148 813 | 158 781 | 158 745 | 160 224 | 159 404 |
| 15 | crystal (0.33)| [spider-gazelle](https://spider-gazelle.net) (2.3) | 148 003 | 154 174 | 150 911 | 141 764 | 135 594 |
| 16 | crystal (0.33)| [kemal](https://kemalcr.com) (0.26) | 140 763 | 147 091 | 143 946 | 136 199 | 136 466 |
| 17 | crystal (0.33)| [grip](https://github.com/grip-framework/grip) (0.28) | 140 602 | 146 968 | 143 367 | 133 944 | 133 810 |
| 18 | nim (1.0)| [jester](https://github.com/dom96/jester) (0.4) | 139 376 | 149 787 | 149 236 | 147 595 | 147 023 |
| 19 | crystal (0.33)| [amber](https://amberframework.org) (0.33) | 132 856 | 138 320 | 134 297 | 126 445 | 125 026 |
| 20 | crystal (0.33)| [lucky](https://luckyframework.org) (0.18) | 131 275 | 136 020 | 130 284 | 120 647 | 120 035 |
| 21 | crystal (0.33)| [orion](https://github.com/obsidian/orion) (2.1) | 128 720 | 132 771 | 127 946 | 117 794 | 116 746 |
| 22 | crystal (0.33)| [athena](https://github.com/athena-framework/athena) (0.8) | 120 733 | 118 538 | 118 090 | 105 004 | 104 555 |
| 23 | java (8)| [act](https://actframework.org) (1.8) | 119 324 | 132 273 | 130 906 | 126 218 | 127 867 |
| 24 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 111 084 | 110 493 | 114 503 | 114 000 | 113 957 |
| 25 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 109 872 | 109 081 | 113 069 | 112 547 | 112 699 |
| 26 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.0) | 105 375 | 104 085 | 107 731 | 107 587 | 107 296 |
| 27 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 105 219 | 108 963 | 111 876 | 111 112 | 110 846 |
| 28 | c (99)| [kore](https://kore.io) (3.3) | 103 876 | 154 598 | 159 368 | 149 108 | 152 822 |
| 29 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 103 243 | 103 171 | 106 511 | 105 826 | 105 712 |
| 30 | go (1.14)| [violetear](https://violetear.org) (7.0) | 102 225 | 102 232 | 105 964 | 106 143 | 105 703 |
| 31 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 100 618 | 99 559 | 102 856 | 103 534 | 103 247 |
| 32 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 100 374 | 122 138 | 126 136 | 127 092 | 126 226 |
| 33 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 99 959 | 98 242 | 102 237 | 102 458 | 102 189 |
| 34 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 99 816 | 103 759 | 105 433 | 103 959 | 103 959 |
| 35 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 97 391 | 95 631 | 99 060 | 99 643 | 99 158 |
| 36 | go (1.14)| [beego](https://beego.me) (1.12) | 97 244 | 100 645 | 104 137 | 103 830 | 103 722 |
| 37 | go (1.14)| [gin](https://gin-gonic.com) (1.5) | 96 516 | 99 526 | 102 476 | 102 868 | 102 193 |
| 38 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 95 608 | 102 855 | 103 588 | 100 779 | 101 756 |
| 39 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (3.0) | 95 274 | 94 446 | 98 062 | 98 889 | 98 635 |
| 40 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 89 551 | 175 437 | 181 344 | 176 345 | 171 634 |
| 41 | javascript (13.7)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 89 304 | 97 873 | 95 955 | 92 592 | 92 256 |
| 42 | javascript (13.7)| [0http](https://github.com/jkyberneees/0http) (2.2) | 88 125 | 96 620 | 95 909 | 92 003 | 92 067 |
| 43 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 85 474 | 82 622 | 88 772 | 88 330 | 88 782 |
| 44 | javascript (13.7)| [restana](https://github.com/jkyberneees/ana) (4.1) | 85 317 | 93 948 | 93 168 | 89 434 | 89 149 |
| 45 | javascript (13.7)| [polka](https://github.com/lukeed/polka) (0.5) | 85 202 | 89 550 | 87 139 | 84 798 | 83 953 |
| 46 | go (1.14)| [air](https://github.com/aofei/air) (0.15) | 83 220 | 87 799 | 90 224 | 90 358 | 90 232 |
| 47 | java (8)| [javalin](https://javalin.io) (3.5) | 77 187 | 81 715 | 81 382 | 78 985 | 79 039 |
| 48 | go (1.14)| [gf](https://goframe.org) (1.11) | 76 836 | 82 483 | 84 476 | 84 830 | 84 475 |
| 49 | scala (2.12)| [akkahttp](https://akka.io) (10.1) | 75 028 | 80 749 | 78 536 | 76 614 | 76 346 |
| 50 | javascript (13.7)| [rayo](https://rayo.js.org) (1.3) | 74 332 | 81 215 | 81 969 | 77 757 | 75 634 |
| 51 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 74 148 | 78 302 | 78 296 | 76 597 | 76 886 |
| 52 | java (8)| [spring-boot](https://spring.io/projects/spring-boot) (2.1) | 71 111 | 77 489 | 76 203 | 74 423 | 74 530 |
| 53 | javascript (13.7)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 69 166 | 73 428 | 71 393 | 69 361 | 69 507 |
| 54 | kotlin (1.3)| [ktor](https://ktor.io) (1.2) | 67 446 | 80 629 | 82 386 | 81 042 | 81 413 |
| 55 | javascript (13.7)| [fastify](https://fastify.io) (2.12) | 67 161 | 71 260 | 69 557 | 67 336 | 67 783 |
| 56 | javascript (13.7)| [foxify](https://foxify.js.org) (0.1) | 66 624 | 69 887 | 66 266 | 64 513 | 64 157 |
| 57 | java (8)| [micronaut](https://micronaut.io) (1.2) | 65 146 | 73 011 | 73 278 | 71 161 | 71 301 |
| 58 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 64 347 | 68 883 | 69 133 | 68 587 | 68 649 |
| 59 | swift (5.1)| [perfect](https://perfect.org) (3.1) | 64 171 | 74 208 | 79 100 | 78 361 | 77 258 |
| 60 | elixir (1.1)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.7/guide/streams/) (2.7) | 63 085 | 64 178 | 61 106 | 60 516 | 62 355 |
| 61 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 62 181 | 65 691 | 68 937 | 69 045 | 68 855 |
| 62 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 60 561 | 63 219 | 63 020 | 60 039 | 61 734 |
| 63 | javascript (13.7)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 60 161 | 62 622 | 60 857 | 59 198 | 59 132 |
| 64 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.14) | 57 541 | 64 507 | 64 020 | 61 673 | 61 616 |
| 65 | javascript (13.7)| [koa](https://koajs.com) (2.11) | 56 802 | 59 897 | 59 517 | 57 868 | 57 769 |
| 66 | rust (1.41)| [nickel](https://nickel-org.github.io) (0.11) | 55 491 | 55 555 | 55 323 | 55 538 | 55 876 |
| 67 | javascript (13.7)| [express](https://expressjs.com) (4.17) | 54 574 | 56 717 | 55 035 | 54 345 | 53 902 |
| 68 | java (8)| [spring-framework](https://spring.io/projects/spring-framework) (5.2) | 54 165 | 63 394 | 62 968 | 61 973 | 61 748 |
| 69 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 53 950 | 55 336 | 55 162 | 55 111 | 55 071 |
| 70 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 52 932 | 58 895 | 58 410 | 56 366 | 56 407 |
| 71 | scala (2.12)| [http4s](https://http4s.org) (0.18) | 51 602 | 57 986 | 58 241 | 58 547 | 59 231 |
| 72 | swift (5.1)| [kitura-nio](https://kitura.io) (2.8) | 49 829 | 49 619 | 49 768 | 46 434 | 46 872 |
| 73 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 49 445 | 55 262 | 54 833 | 52 954 | 52 985 |
| 74 | php (7.4)| [hyperf](https://www.hyperf.io) (1.0) | 48 973 | 50 593 | 50 577 | 50 284 | 50 275 |
| 75 | swift (5.1)| [kitura](https://kitura.io) (2.8) | 48 178 | 48 989 | 49 197 | 47 677 | 47 719 |
| 76 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 47 867 | 49 547 | 49 169 | 48 949 | 48 644 |
| 77 | swift (5.1)| [vapor](https://vapor.codes) (3.3) | 47 112 | 48 080 | 47 860 | 47 120 | 47 384 |
| 78 | python (3.8)| [hug](https://hug.rest) (2.6) | 47 065 | 48 870 | 49 268 | 48 824 | 48 937 |
| 79 | javascript (13.7)| [moleculer](https://moleculer.services) (0.14) | 46 811 | 49 894 | 48 833 | 47 749 | 47 842 |
| 80 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 46 308 | 46 206 | 46 398 | 45 867 | 46 050 |
| 81 | rust (1.41)| [gotham](https://gotham.rs) (0.4) | 43 148 | 48 394 | 50 292 | 52 330 | 52 355 |
| 82 | python (3.8)| [starlette](https://starlette.io) (0.13) | 43 020 | 49 249 | 48 817 | 47 299 | 47 235 |
| 83 | javascript (13.7)| [hapi](https://hapijs.com) (19.1) | 42 913 | 45 151 | 44 107 | 43 589 | 43 680 |
| 84 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 42 039 | 43 966 | 41 755 | 41 011 | 41 437 |
| 85 | php (7.4)| [imi](https://imiphp.com) (1.0) | 40 957 | 43 111 | 43 562 | 42 859 | 43 154 |
| 86 | elixir (1.1)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.7/guide/) (2.7) | 40 822 | 41 991 | 41 778 | 40 975 | 40 970 |
| 87 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 40 494 | 42 308 | 40 136 | 39 363 | 39 419 |
| 88 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 39 994 | 43 685 | 43 548 | 42 788 | 42 820 |
| 89 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0.0b1) | 38 603 | 42 371 | 42 198 | 40 915 | 41 042 |
| 90 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.3) | 38 082 | 39 787 | 38 323 | 37 670 | 37 867 |
| 91 | javascript (13.7)| [restify](https://restify.com) (8.5) | 37 362 | 39 422 | 39 181 | 38 642 | 39 305 |
| 92 | php (7.4)| [swoft](https://swoft.org) (2.0) | 36 348 | 37 071 | 36 729 | 36 190 | 36 189 |
| 93 | elixir (1.1)| [plug](https://hexdocs.pm/plug) (1.8) | 35 006 | 36 957 | 36 300 | 35 843 | 34 299 |
| 94 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 33 585 | 35 770 | 34 322 | 34 469 | 34 285 |
| 95 | dart (2.7)| [aqueduct](https://aqueduct.io) (3.2) | 33 414 | 34 149 | 33 723 | 32 692 | 32 717 |
| 96 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 33 334 | 33 784 | 35 711 | 35 715 | 35 716 |
| 97 | elixir (1.1)| [phoenix](https://phoenixframework.org) (1.4) | 31 166 | 31 770 | 31 342 | 30 673 | 30 616 |
| 98 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.52) | 30 353 | 31 914 | 31 479 | 30 226 | 30 310 |
| 99 | python (3.8)| [responder](https://python-responder.org) (2.0) | 29 673 | 31 263 | 31 062 | 29 890 | 29 914 |
| 100 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 27 831 | 28 494 | 27 955 | 27 995 | 27 963 |
| 101 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 26 069 | 26 691 | 26 506 | 26 359 | 26 052 |
| 102 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 25 945 | 25 986 | 25 539 | 25 438 | 25 617 |
| 103 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 25 704 | 27 678 | 27 473 | 25 349 | 26 605 |
| 104 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 25 471 | 28 205 | 28 212 | 27 352 | 27 208 |
| 105 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 24 257 | 22 264 | 24 652 | 24 432 | 24 350 |
| 106 | javascript (13.7)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 23 649 | 22 428 | 21 331 | 21 044 | 20 940 |
| 107 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 23 007 | 23 808 | 23 517 | 23 174 | 22 911 |
| 108 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (19.12) | 18 832 | 20 336 | 18 889 | 17 821 | 19 462 |
| 109 | rust (1.41)| [iron](https://ironframework.io) (0.6) | 18 427 | 18 532 | 18 569 | 18 711 | 18 535 |
| 110 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.4) | 17 880 | 18 215 | 18 440 | 18 245 | 18 287 |
| 111 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 16 515 | 16 347 | 16 275 | 16 356 | 16 422 |
| 112 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 15 136 | 15 582 | 15 624 | 15 578 | 15 618 |
| 113 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 12 758 | 12 294 | 12 429 | 12 222 | 12 600 |
| 114 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 12 510 | 12 525 | 13 299 | 13 126 | 13 199 |
| 115 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 12 358 | 12 256 | 12 275 | 12 237 | 12 199 |
| 116 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.11) | 11 306 | 12 240 | 11 348 | 10 818 | 10 856 |
| 117 | swift (5.1)| [swifter](https://github.com/httpswift/swifter) (1.4) | 10 538 | 10 399 | 10 774 | 10 698 | 10 633 |
| 118 | python (3.8)| [django](https://djangoproject.com) (3.0) | 10 338 | 10 560 | 10 315 | 10 175 | 10 355 |
| 119 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 9 635 | 10 660 | 10 445 | 10 324 | 10 382 |
| 120 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 9 183 | 9 131 | 9 132 | 52 124 | 48 311 |
| 121 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.5) | 8 159 | 8 341 | 8 317 | 8 294 | 8 299 |
| 122 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 7 903 | 7 904 | 7 878 | 45 036 | 42 716 |
| 123 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 7 550 | 7 557 | 7 586 | 51 048 | 49 979 |
| 124 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 7 510 | 7 460 | 7 452 | 44 133 | 40 933 |
| 125 | crystal (0.33)| [onyx](https://onyxframework.org) (0.5) | 5 081 | 5 197 | 5 265 | 5 231 | 5 306 |
| 126 | php (7.4)| [slim](https://slimframework.com) (4.4) | 4 605 | 4 585 | 4 796 | 43 816 | 40 426 |
| 127 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 4 411 | 4 356 | 4 470 | 43 192 | 40 093 |
| 128 | php (7.4)| [lumen](https://lumen.laravel.com) (7.0) | 4 295 | 4 291 | 4 350 | 43 533 | 41 348 |
| 129 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 3 893 | 3 770 | 3 746 | 3 756 | 3 753 |
| 130 | php (7.4)| [zend-expressive](https://zendframework.github.io/zend-expressive) (3.2) | 3 665 | 3 650 | 3 791 | 43 315 | 39 514 |
| 131 | php (7.4)| [symfony](https://symfony.com) (4.3) | 3 583 | 3 625 | 3 685 | 42 498 | 41 628 |
| 132 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 435 | 2 440 | 2 404 | 2 406 | 2 390 |
| 133 | php (7.4)| [zend-framework](https://framework.zend.com) (3.1) | 1 944 | 1 972 | 2 015 | 40 856 | 38 956 |
| 134 | julia (1.3)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 1 887 | 5 392 | 4 339 | 3 312 | 1 867 |
| 135 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 1 571 | 1 595 | 1 568 | 1 551 | 1 545 |
| 136 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 1 548 | 1 511 | 1 498 | 1 468 | 1 469 |
| 137 | perl (5.3)| [dancer2](https://perldancer.org) (2.0) | 1 423 | 1 913 | 1 816 | 1 299 | 823 |
| 138 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 499 | 456 | 2 163 | 34 401 | 36 077 |
| 139 | php (7.4)| [laravel](https://laravel.com) (7.2) | 195 | 165 | 3 175 | 23 775 | 22 175 |

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

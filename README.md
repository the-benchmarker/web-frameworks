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

:information_source:  Updated on **2020-06-03** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (256) | Speed (512) | Speed (1024) | 
|----|----------|-----------|-----------:|------------:|------------:|------------:|-------------:| 
| 1 | rust (1.43)| [actix](https://actix.rs) (2.0) | 125 539 | 111 609 | 106 173 | 103 973 | 101 087 | 
| 2 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 114 232 | 109 407 | 102 542 | 99 327 | 99 535 | 
| 3 | crystal (0.34)| [orion](https://github.com/obsidian/orion) (2.3) | 106 287 | 100 272 | 92 787 | 89 729 | 89 487 | 
| 4 | nim (1.2)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 105 974 | 95 082 | 92 334 | 89 281 | 86 | 
| 5 | javascript (13.14)| [restana](https://github.com/jkyberneees/ana) (4.6) | 104 115 | 104 898 | 102 900 | 100 724 | 102 949 | 
| 6 | crystal (0.34)| [grip](https://github.com/grip-framework/grip) (0.28) | 103 483 | 98 642 | 93 012 | 91 521 | 91 263 | 
| 7 | go (1.14)| [rte](https://github.com/jwilner/rte) (0.0) | 103 323 | 96 195 | 92 609 | 90 712 | 91 384 | 
| 8 | nim (1.2)| [jester](https://github.com/dom96/jester) (0.4) | 102 410 | 99 200 | 90 039 | 88 338 | 92 | 
| 9 | javascript (13.14)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 100 191 | 93 845 | 104 538 | 101 476 | 100 769 | 
| 10 | go (1.14)| [gf](https://goframe.org) (1.13) | 99 935 | 98 235 | 100 942 | 104 197 | 104 537 | 
| 11 | go (1.14)| [echo](https://echo.labstack.com) (4.1) | 98 286 | 94 348 | 93 063 | 95 094 | 97 016 | 
| 12 | javascript (13.14)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 97 544 | 93 336 | 93 538 | 95 760 | 92 887 | 
| 13 | javascript (13.14)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.11) | 97 422 | 106 399 | 101 960 | 102 538 | 100 803 | 
| 14 | javascript (13.14)| [polka](https://github.com/lukeed/polka) (0.5) | 96 512 | 89 207 | 91 992 | 90 583 | 90 164 | 
| 15 | crystal (0.34)| [athena](https://github.com/athena-framework/athena) (0.8) | 96 340 | 87 141 | 78 384 | 77 484 | 73 944 | 
| 16 | javascript (13.14)| [rayo](https://rayo.js.org) (1.3) | 96 329 | 104 546 | 106 556 | 101 851 | 101 156 | 
| 17 | go (1.14)| [fiber](https://gofiber.io) (1.1) | 96 202 | 86 667 | 92 109 | 89 612 | 90 092 | 
| 18 | go (1.14)| [chi](https://github.com/go-chi/chi) (4.1) | 95 642 | 97 027 | 97 558 | 100 797 | 102 697 | 
| 19 | javascript (13.14)| [0http](https://github.com/jkyberneees/0http) (2.5) | 95 384 | 86 152 | 82 723 | 80 270 | 78 706 | 
| 20 | crystal (0.34)| [kemal](https://kemalcr.com) (0.26) | 95 013 | 90 382 | 87 381 | 84 744 | 82 861 | 
| 21 | go (1.14)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.1) | 94 583 | 86 536 | 83 332 | 83 950 | 83 608 | 
| 22 | python (3.8)| [hug](https://www.hug.rest) (2.6) | 94 138 | 90 342 | 102 248 | 92 833 | 99 771 | 
| 23 | crystal (0.34)| [onyx](https://onyxframework.org) (0.5) | 92 613 | 96 054 | 86 460 | 87 962 | 83 604 | 
| 24 | javascript (13.14)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.0) | 91 806 | 82 991 | 82 381 | 81 583 | 82 348 | 
| 25 | go (1.14)| [fasthttprouter](https://pkg.go.dev/github.com/buaazp/fasthttprouter) (0.1) | 91 393 | 84 143 | 88 480 | 88 445 | 89 211 | 
| 26 | cpp (14/17)| [drogon](https://github.com/an-tao/drogon) (1.0) | 90 582 | 84 696 | 79 998 | 77 825 | 76 901 | 
| 27 | go (1.14)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 90 251 | 84 164 | 81 636 | 80 879 | 84 884 | 
| 28 | go (1.14)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.13) | 89 725 | 83 984 | 82 642 | 81 306 | 82 850 | 
| 29 | javascript (13.14)| [nestjs-fastify](https://nestjs.com) (7.1) | 89 445 | 92 653 | 103 407 | 101 478 | 98 205 | 
| 30 | go (1.14)| [gin](https://gin-gonic.com) (1.6) | 89 256 | 84 704 | 82 454 | 80 298 | 83 056 | 
| 31 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.3) | 89 060 | 81 045 | 78 616 | 76 246 | 79 551 | 
| 32 | rust (1.43)| [iron](https://ironframework.io) (0.6) | 88 646 | 89 891 | 85 103 | 89 321 | 85 259 | 
| 33 | javascript (13.14)| [koa](https://koajs.com) (2.12) | 87 942 | 89 822 | 92 201 | 96 440 | 96 637 | 
| 34 | go (1.14)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.7) | 87 776 | 82 203 | 80 656 | 81 141 | 82 499 | 
| 35 | go (1.14)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.2) | 86 243 | 76 499 | 77 324 | 75 048 | 76 411 | 
| 36 | go (1.14)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 86 043 | 84 006 | 90 207 | 87 327 | 85 473 | 
| 37 | c (99)| [kore](https://kore.io) (3.3) | 85 970 | 79 569 | 78 113 | 75 650 | 75 541 | 
| 38 | crystal (0.34)| [spider-gazelle](https://spider-gazelle.net) (3.0) | 85 312 | 83 462 | 80 400 | 79 385 | 78 860 | 
| 39 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 83 704 | 82 900 | 79 166 | 79 844 | 76 254 | 
| 40 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 83 526 | 84 639 | 81 121 | 79 026 | 76 899 | 
| 41 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (3.1) | 83 076 | 77 558 | 70 764 | 66 649 | 65 896 | 
| 42 | javascript (13.14)| [fastify](https://fastify.io) (2.14) | 82 709 | 81 699 | 75 040 | 74 322 | 71 539 | 
| 43 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 81 719 | 77 895 | 77 091 | 75 774 | 75 402 | 
| 44 | javascript (13.14)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 81 565 | 83 167 | 80 777 | 81 741 | 81 797 | 
| 45 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 81 393 | 77 164 | 77 314 | 75 187 | 75 614 | 
| 46 | rust (1.43)| [gotham](https://gotham.rs) (0.4) | 80 807 | 73 903 | 65 826 | 65 427 | 61 063 | 
| 47 | crystal (0.34)| [lucky](https://luckyframework.org) (0.21) | 80 709 | 79 296 | 76 445 | 78 139 | 75 896 | 
| 48 | php (7.4)| [simps](https://simps.io) (1.0) | 80 657 | 73 889 | 70 307 | 72 477 | 73 074 | 
| 49 | crystal (0.34)| [amber](https://amberframework.org) (0.34) | 80 277 | 75 962 | 72 548 | 69 710 | 68 071 | 
| 50 | go (1.14)| [beego](https://beego.me) (1.12) | 79 530 | 71 181 | 61 366 | 60 953 | 58 858 | 
| 51 | go (1.14)| [aero](https://github.com/aerogo/aero) (1.3) | 79 168 | 71 370 | 64 422 | 60 166 | 59 080 | 
| 52 | javascript (13.14)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 79 116 | 75 753 | 73 092 | 73 658 | 71 683 | 
| 53 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 78 824 | 74 476 | 71 302 | 70 816 | 69 029 | 
| 54 | go (1.14)| [violetear](https://violetear.org) (7.0) | 77 750 | 69 868 | 61 472 | 60 731 | 60 517 | 
| 55 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 77 707 | 83 315 | 84 596 | 80 530 | 76 536 | 
| 56 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 76 583 | 77 459 | 71 226 | 68 914 | 66 641 | 
| 57 | go (1.14)| [mars](https://github.com/roblillack/mars) (1.0) | 75 948 | 74 075 | 73 612 | 72 931 | 72 036 | 
| 58 | swift (5.2)| [kitura-nio](https://kitura.io) (2.9) | 75 857 | 72 240 | 74 110 | 73 141 | 73 191 | 
| 59 | rust (1.43)| [nickel](https://nickel-org.github.io) (0.11) | 73 989 | 72 319 | 73 791 | 71 690 | 73 935 | 
| 60 | fsharp (4.7)| [suave](https://suave.io) (2.5) | 72 325 | 66 979 | 67 419 | 65 045 | 63 945 | 
| 61 | javascript (13.14)| [express](https://expressjs.com) (4.17) | 71 434 | 70 234 | 75 810 | 73 293 | 72 129 | 
| 62 | javascript (13.14)| [foxify](https://foxify.js.org) (0.1) | 71 061 | 69 996 | 68 472 | 68 071 | 67 643 | 
| 63 | javascript (13.14)| [feathersjs](https://feathersjs.com) (4.5) | 70 466 | 75 582 | 76 306 | 76 494 | 77 000 | 
| 64 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 70 260 | 66 430 | 67 563 | 65 368 | 63 810 | 
| 65 | javascript (13.14)| [hapi](https://hapijs.com) (19.1) | 70 229 | 71 862 | 72 072 | 71 677 | 71 228 | 
| 66 | javascript (13.14)| [moleculer](https://moleculer.services) (0.14) | 65 847 | 65 744 | 65 129 | 64 489 | 63 307 | 
| 67 | crystal (0.34)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.15) | 62 962 | 63 578 | 51 009 | 56 969 | 53 616 | 
| 68 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 60 388 | 58 992 | 60 007 | 59 938 | 59 904 | 
| 69 | javascript (13.14)| [restify](https://restify.com) (8.5) | 60 148 | 61 588 | 61 114 | 62 453 | 62 299 | 
| 70 | go (1.14)| [kami](https://github.com/guregu/kami) (2.2) | 58 569 | 62 476 | 62 275 | 62 862 | 63 879 | 
| 71 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 57 601 | 57 936 | 57 978 | 57 789 | 57 922 | 
| 72 | go (1.14)| [goroute](https://goroute.github.io) (0.0) | 55 560 | 59 256 | 59 647 | 60 826 | 61 373 | 
| 73 | go (1.14)| [air](https://github.com/aofei/air) (0.16) | 55 295 | 56 495 | 57 236 | 57 577 | 59 736 | 
| 74 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 52 841 | 52 057 | 51 952 | 50 491 | 50 466 | 
| 75 | javascript (13.14)| [turbo-polka](https://github.com/mafintosh/turbo-http) (0.3) | 52 265 | 51 696 | 56 414 | 55 811 | 49 744 | 
| 76 | javascript (13.14)| [nestjs-express](https://nestjs.com) (7.1) | 52 224 | 54 368 | 54 371 | 54 275 | 54 048 | 
| 77 | go (1.14)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.4) | 50 677 | 55 118 | 53 223 | 55 945 | 57 418 | 
| 78 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 50 533 | 57 046 | 60 527 | 63 227 | 65 244 | 
| 79 | go (1.14)| [webgo](https://github.com/bnkamalesh/webgo) (3.4) | 49 834 | 51 908 | 55 238 | 55 611 | 56 481 | 
| 80 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.26) | 49 218 | 52 151 | 54 230 | 55 456 | 55 897 | 
| 81 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 49 175 | 53 106 | 54 630 | 56 136 | 56 683 | 
| 82 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 47 067 | 47 340 | 47 396 | 47 486 | 47 160 | 
| 83 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 44 356 | 46 543 | 48 317 | 48 938 | 49 244 | 
| 84 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 43 865 | 46 736 | 47 814 | 49 160 | 50 067 | 
| 85 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 42 856 | 41 317 | 42 488 | 43 095 | 42 709 | 
| 86 | python (3.8)| [starlette](https://starlette.io) (0.13) | 42 329 | 44 816 | 46 370 | 47 137 | 47 280 | 
| 87 | go (1.14)| [gramework](https://github.com/gramework/gramework) (1.7) | 41 182 | 45 370 | 44 000 | 44 907 | 42 280 | 
| 88 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 38 660 | 41 475 | 42 322 | 42 413 | 43 994 | 
| 89 | go (1.14)| [tango](https://gitea.com/lunny/tango) (0.6) | 38 618 | 43 516 | 40 850 | 40 890 | 39 122 | 
| 90 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 36 753 | 38 681 | 39 647 | 40 471 | 40 378 | 
| 91 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 35 447 | 37 554 | 37 356 | 36 777 | 36 770 | 
| 92 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.32) | 35 166 | 37 111 | 37 617 | 37 715 | 37 814 | 
| 93 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 31 924 | 33 331 | 33 399 | 32 791 | 32 731 | 
| 94 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.55) | 29 763 | 30 877 | 31 657 | 31 964 | 31 883 | 
| 95 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 29 625 | 30 655 | 30 964 | 29 903 | 77 725 | 
| 96 | python (3.8)| [responder](https://python-responder.org) (2.0) | 29 452 | 31 048 | 31 741 | 32 074 | 31 512 | 
| 97 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 27 665 | 28 770 | 28 399 | 28 254 | 27 993 | 
| 98 | ruby (2.7)| [rack-app](https://rack-app.com) (7.6) | 26 631 | 27 570 | 27 192 | 26 999 | 26 767 | 
| 99 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 25 493 | 26 169 | 25 793 | 25 763 | 25 641 | 
| 100 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 22 342 | 22 187 | 22 268 | 22 786 | 66 938 | 
| 101 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 21 261 | 23 159 | 21 941 | 22 454 | 21 588 | 
| 102 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 20 526 | 20 500 | 20 508 | 20 518 | 68 540 | 
| 103 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 19 340 | 19 595 | 19 644 | 20 343 | 66 488 | 
| 104 | javascript (13.14)| [sails](https://sailsjs.com) (1.2) | 19 211 | 19 453 | 19 364 | 18 909 | 18 844 | 
| 105 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.12) | 16 078 | 17 305 | 17 443 | 17 134 | 16 125 | 
| 106 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 16 072 | 16 647 | 16 917 | 16 637 | 16 159 | 
| 107 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 15 973 | 16 767 | 16 066 | 16 328 | 16 153 | 
| 108 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 15 753 | 16 273 | 16 118 | 16 106 | 16 033 | 
| 109 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 15 647 | 15 653 | 15 731 | 15 635 | 15 560 | 
| 110 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 14 566 | 14 462 | 14 439 | 15 451 | 15 963 | 
| 111 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 13 743 | 13 966 | 13 884 | 13 882 | 13 890 | 
| 112 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (2.8) | 13 303 | 13 602 | 13 566 | 13 536 | 63 861 | 
| 113 | php (7.4)| [symfony](https://symfony.com) (5.1) | 12 302 | 12 356 | 12 204 | 11 952 | 58 666 | 
| 114 | php (7.4)| [slim](https://slimframework.com) (4.5) | 10 997 | 10 816 | 10 816 | 10 570 | 60 680 | 
| 115 | pony (0.35)| [jennet](https://github.com/Theodus/jennet) (0.1) | 10 182 | 11 909 | 8 592 | 92 | 74 | 
| 116 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 9 260 | 9 191 | 9 163 | 9 021 | 53 487 | 
| 117 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 4 819 | 4 516 | 4 513 | 4 689 | 4 639 | 
| 118 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 3 141 | 3 025 | 3 050 | 3 061 | 3 048 | 
| 119 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 3 061 | 2 962 | 2 937 | 2 881 | 2 861 | 
| 120 | python (3.8)| [django](https://djangoproject.com) (3.0) | 133 | 145 | 141 | 134 | 119 | 

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

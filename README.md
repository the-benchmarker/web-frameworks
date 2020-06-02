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

:information_source:  Updated on **2020-06-02** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (256) | Speed (512) | Speed (1024) | 
|----|----------|-----------|-----------:|------------:|------------:|------------:|-------------:| 
| 1 | javascript (13.14)| [restana](https://github.com/jkyberneees/ana) (4.6) | 104 115 | 104 898 | 102 900 | 100 724 | 102 949 | 
| 2 | javascript (13.14)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 100 191 | 93 845 | 104 538 | 101 476 | 100 769 | 
| 3 | javascript (13.14)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 97 544 | 93 336 | 93 538 | 95 760 | 92 887 | 
| 4 | javascript (13.14)| [nanoexpress-pro](https://github.com/nanoexpress/pro) (1.11) | 97 422 | 106 399 | 101 960 | 102 538 | 100 803 | 
| 5 | javascript (13.14)| [polka](https://github.com/lukeed/polka) (0.5) | 96 512 | 89 207 | 91 992 | 90 583 | 90 164 | 
| 6 | javascript (13.14)| [rayo](https://rayo.js.org) (1.3) | 96 329 | 104 546 | 106 556 | 101 851 | 101 156 | 
| 7 | javascript (13.14)| [0http](https://github.com/jkyberneees/0http) (2.5) | 95 384 | 86 152 | 82 723 | 80 270 | 78 706 | 
| 8 | python (3.8)| [hug](https://hug.rest) (2.6) | 94 138 | 90 342 | 102 248 | 92 833 | 99 771 | 
| 9 | javascript (13.14)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.0) | 91 806 | 82 991 | 82 381 | 81 583 | 82 348 | 
| 10 | javascript (13.14)| [nestjs-fastify](https://nestjs.com) (7.1) | 89 445 | 92 653 | 103 407 | 101 478 | 98 205 | 
| 11 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.3) | 89 060 | 81 045 | 78 616 | 76 246 | 79 551 | 
| 12 | javascript (13.14)| [koa](https://koajs.com) (2.12) | 87 942 | 89 822 | 92 201 | 96 440 | 96 637 | 
| 13 | javascript (13.14)| [fastify](https://fastify.io) (2.14) | 82 709 | 81 699 | 75 040 | 74 322 | 71 539 | 
| 14 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 81 719 | 77 895 | 77 091 | 75 774 | 75 402 | 
| 15 | javascript (13.14)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 81 565 | 83 167 | 80 777 | 81 741 | 81 797 | 
| 16 | php (7.4)| [simps](https://simps.io) (1.0) | 80 657 | 73 889 | 70 307 | 72 477 | 73 074 | 
| 17 | javascript (13.14)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 79 116 | 75 753 | 73 092 | 73 658 | 71 683 | 
| 18 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 78 824 | 74 476 | 71 302 | 70 816 | 69 029 | 
| 19 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.12) | 77 707 | 83 315 | 84 596 | 80 530 | 76 536 | 
| 20 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 76 583 | 77 459 | 71 226 | 68 914 | 66 641 | 
| 21 | javascript (13.14)| [express](https://expressjs.com) (4.17) | 71 434 | 70 234 | 75 810 | 73 293 | 72 129 | 
| 22 | javascript (13.14)| [foxify](https://foxify.js.org) (0.1) | 71 061 | 69 996 | 68 472 | 68 071 | 67 643 | 
| 23 | javascript (13.14)| [feathersjs](https://feathersjs.com) (4.5) | 70 466 | 75 582 | 76 306 | 76 494 | 77 000 | 
| 24 | javascript (13.14)| [hapi](https://hapijs.com) (19.1) | 70 229 | 71 862 | 72 072 | 71 677 | 71 228 | 
| 25 | javascript (13.14)| [moleculer](https://moleculer.services) (0.14) | 65 847 | 65 744 | 65 129 | 64 489 | 63 307 | 
| 26 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 60 388 | 58 992 | 60 007 | 59 938 | 59 904 | 
| 27 | javascript (13.14)| [restify](https://restify.com) (8.5) | 60 148 | 61 588 | 61 114 | 62 453 | 62 299 | 
| 28 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 57 601 | 57 936 | 57 978 | 57 789 | 57 922 | 
| 29 | javascript (13.14)| [turbo-polka](https://github.com/mafintosh/turbo-http) (0.3) | 52 265 | 51 696 | 56 414 | 55 811 | 49 744 | 
| 30 | javascript (13.14)| [nestjs-express](https://nestjs.com) (7.1) | 52 224 | 54 368 | 54 371 | 54 275 | 54 048 | 
| 31 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 50 533 | 57 046 | 60 527 | 63 227 | 65 244 | 
| 32 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.26) | 49 218 | 52 151 | 54 230 | 55 456 | 55 897 | 
| 33 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 47 067 | 47 340 | 47 396 | 47 486 | 47 160 | 
| 34 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 44 356 | 46 543 | 48 317 | 48 938 | 49 244 | 
| 35 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.7) | 43 865 | 46 736 | 47 814 | 49 160 | 50 067 | 
| 36 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 42 856 | 41 317 | 42 488 | 43 095 | 42 709 | 
| 37 | python (3.8)| [starlette](https://starlette.io) (0.13) | 42 329 | 44 816 | 46 370 | 47 137 | 47 280 | 
| 38 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 38 660 | 41 475 | 42 322 | 42 413 | 43 994 | 
| 39 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 36 753 | 38 681 | 39 647 | 40 471 | 40 378 | 
| 40 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 35 447 | 37 554 | 37 356 | 36 777 | 36 770 | 
| 41 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.32) | 35 166 | 37 111 | 37 617 | 37 715 | 37 814 | 
| 42 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 31 924 | 33 331 | 33 399 | 32 791 | 32 731 | 
| 43 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.55) | 29 763 | 30 877 | 31 657 | 31 964 | 31 883 | 
| 44 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 29 625 | 30 655 | 30 964 | 29 903 | 77 725 | 
| 45 | python (3.8)| [responder](https://python-responder.org) (2.0) | 29 452 | 31 048 | 31 741 | 32 074 | 31 512 | 
| 46 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 27 665 | 28 770 | 28 399 | 28 254 | 27 993 | 
| 47 | ruby (2.7)| [rack-app](https://rack-app.com) (7.6) | 26 631 | 27 570 | 27 192 | 26 999 | 26 767 | 
| 48 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 25 493 | 26 169 | 25 793 | 25 763 | 25 641 | 
| 49 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 21 261 | 23 159 | 21 941 | 22 454 | 21 588 | 
| 50 | php (7.4)| [phalcon](https://phalcon.io) (4.0) | 20 526 | 20 500 | 20 508 | 20 518 | 68 540 | 
| 51 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 19 340 | 19 595 | 19 644 | 20 343 | 66 488 | 
| 52 | javascript (13.14)| [sails](https://sailsjs.com) (1.2) | 19 211 | 19 453 | 19 364 | 18 909 | 18 844 | 
| 53 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.12) | 16 078 | 17 305 | 17 443 | 17 134 | 16 125 | 
| 54 | python (3.8)| [tornado](https://tornadoweb.org) (6.0) | 16 072 | 16 647 | 16 917 | 16 637 | 16 159 | 
| 55 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 15 973 | 16 767 | 16 066 | 16 328 | 16 153 | 
| 56 | ruby (2.7)| [grape](https://ruby-grape.org) (1.3) | 15 753 | 16 273 | 16 118 | 16 106 | 16 033 | 
| 57 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 15 647 | 15 653 | 15 731 | 15 635 | 15 560 | 
| 58 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.0) | 14 566 | 14 462 | 14 439 | 15 451 | 15 963 | 
| 59 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 13 743 | 13 966 | 13 884 | 13 882 | 13 890 | 
| 60 | php (7.4)| [symfony](https://symfony.com) (5.1) | 12 302 | 12 356 | 12 204 | 11 952 | 58 666 | 
| 61 | php (7.4)| [slim](https://slimframework.com) (4.5) | 10 977 | 10 857 | 10 869 | 10 529 | 60 277 | 
| 62 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 9 260 | 9 191 | 9 163 | 9 021 | 53 487 | 
| 63 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 4 819 | 4 516 | 4 513 | 4 689 | 4 639 | 
| 64 | python (3.8)| [klein](https://github.com/twisted/klein) (19.6) | 3 141 | 3 025 | 3 050 | 3 061 | 3 048 | 
| 65 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.12) | 3 061 | 2 962 | 2 937 | 2 881 | 2 861 | 
| 66 | python (3.8)| [django](https://djangoproject.com) (3.0) | 133 | 145 | 141 | 134 | 119 | 

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

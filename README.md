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

+ Initialize `sqlite` database

~~~sh
bin/db init
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

|    | Language | Framework | Speed (`req/s`) | Horizontal scale (parallelism) | Vertical scale (concurrency) |
|----|----------|-----------|----------------:|-------------|-------------|
| 1 | javascript (12.13)| nanoexpress-pro (1.8) | 202 352 | | |
| 2 | javascript (12.13)| sifrr (0.0) | 199 208 | | |
| 3 | javascript (12.13)| nanoexpress (1.1) | 188 562 | | |
| 4 | nim (1.0)| httpbeast (0.2) | 188 432 | | |
| 5 | c (11)| agoo-c (0.7) | 182 336 | | |
| 6 | go (1.13)| fasthttp (1.7) | 167 722 | | |
| 7 | go (1.13)| fasthttprouter (0.1) | 165 151 | | |
| 8 | java (8)| rapidoid (5.5) | 163 544 | | |
| 9 | go (1.13)| gorouter-fasthttp (4.2) | 162 800 | | |
| 10 | go (1.13)| router (1.6) | 159 965 | | |
| 11 | go (1.13)| atreugo (9.0) | 154 496 | | |
| 12 | c (11)| kore (3.3) | 152 766 | | |
| 13 | crystal (0.31)| router.cr (0.2) | 145 584 | | |
| 14 | crystal (0.31)| toro (0.4) | 145 030 | | |
| 15 | crystal (0.31)| spider-gazelle (2.1) | 141 550 | | |
| 16 | nim (1.0)| jester (0.4) | 141 041 | | |
| 17 | crystal (0.31)| raze (0.3) | 140 481 | | |
| 18 | crystal (0.31)| kemal (0.28) | 136 477 | | |
| 19 | java (8)| act (1.8) | 125 309 | | |
| 20 | crystal (0.31)| amber (0.3) | 124 253 | | |
| 21 | ruby (2.6)| agoo (2.11) | 119 852 | | |
| 22 | php (7.4)| workerman (3.5) | 109 655 | | |
| 23 | go (1.13)| gorouter (4.2) | 109 431 | | |
| 24 | go (1.13)| rte (0.0) | 107 483 | | |
| 25 | rust (1.39)| actix-web (1.0) | 106 177 | | |
| 26 | go (1.13)| chi (4.0) | 105 251 | | |
| 27 | go (1.13)| aero (1.3) | 102 214 | | |
| 28 | crystal (0.31)| orion (1.7) | 101 916 | | |
| 29 | go (1.13)| kami (2.2) | 101 287 | | |
| 30 | go (1.13)| goroute (0.0) | 100 718 | | |
| 31 | go (1.13)| echo (4.1) | 98 925 | | |
| 32 | go (1.13)| violetear (7.0) | 98 241 | | |
| 33 | csharp (7.3)| aspnetcore (3.0) | 97 953 | | |
| 34 | go (1.13)| beego (1.12) | 96 734 | | |
| 35 | go (1.13)| gin (1.5) | 96 300 | | |
| 36 | go (1.13)| gorilla-mux (1.7) | 92 900 | | |
| 37 | javascript (12.13)| polkadot (1.0) | 92 677 | | |
| 38 | go (1.13)| webgo (3.0) | 92 082 | | |
| 39 | javascript (12.13)| 0http (1.2) | 90 498 | | |
| 40 | javascript (12.13)| restana (3.4) | 88 121 | | |
| 41 | go (1.13)| air (0.14) | 84 302 | | |
| 42 | cpp (11)| drogon (1.0) | 82 666 | | |
| 43 | go (1.13)| gf (1.1) | 82 292 | | |
| 44 | swift (5.1)| perfect (3.1) | 80 085 | | |
| 45 | javascript (12.13)| polka (0.5) | 79 168 | | |
| 46 | javascript (12.13)| rayo (1.3) | 78 595 | | |
| 47 | python (3.8)| falcon (2.0) | 76 091 | | |
| 48 | javascript (12.13)| muneem (2.4) | 69 978 | | |
| 49 | kotlin (1.3)| ktor (1.2) | 68 888 | | |
| 50 | javascript (12.13)| foxify (0.1) | 65 547 | | |
| 51 | go (1.13)| mars (1.0) | 63 260 | | |
| 52 | python (3.8)| bottle (0.12) | 62 003 | | |
| 53 | java (8)| javalin (3.5) | 60 911 | | |
| 54 | php (7.4)| one (1.9) | 60 276 | | |
| 55 | javascript (12.13)| koa (2.11) | 58 666 | | |
| 56 | scala (2.12)| akkahttp (10.1) | 57 641 | | |
| 57 | javascript (12.13)| iotjs-express (0.0) | 56 389 | | |
| 58 | python (3.8)| asgineer (0.7) | 54 425 | | |
| 59 | php (7.4)| ubiquity (2.3) | 54 281 | | |
| 60 | javascript (12.13)| fastify (2.11) | 54 143 | | |
| 61 | java (8)| spring-boot (2.1) | 54 125 | | |
| 62 | ruby (2.6)| plezi (0.16) | 52 100 | | |
| 63 | javascript (12.13)| express (4.17) | 51 895 | | |
| 64 | scala (2.12)| http4s (0.18) | 51 867 | | |
| 65 | clojure (1.10)| coast (1.0) | 51 798 | | |
| 66 | python (3.8)| blacksheep (0.2) | 51 687 | | |
| 67 | php (7.4)| phalcon (4.0) | 51 432 | | |
| 68 | swift (5.1)| vapor (3.3) | 50 674 | | |
| 69 | rust (1.39)| gotham (0.4) | 50 653 | | |
| 70 | swift (5.1)| kitura-nio (2.8) | 49 337 | | |
| 71 | php (7.4)| hyperf (1.0) | 49 221 | | |
| 72 | python (3.8)| hug (2.6) | 48 569 | | |
| 73 | python (3.8)| pyramid (1.1) | 48 444 | | |
| 74 | swift (5.1)| kitura (2.8) | 48 089 | | |
| 75 | python (3.8)| starlette (0.13) | 46 169 | | |
| 76 | php (7.4)| hamlet (3.2) | 44 965 | | |
| 77 | php (7.4)| one-fpm (1.9) | 44 548 | | |
| 78 | php (7.4)| basicphp (0.9) | 43 791 | | |
| 79 | php (7.4)| symfony (4.3) | 43 036 | | |
| 80 | php (7.4)| slim (4.3) | 42 936 | | |
| 81 | php (7.4)| zend-expressive (3.2) | 42 682 | | |
| 82 | php (7.4)| lumen (6.2) | 42 067 | | |
| 83 | javascript (12.13)| restify (8.5) | 41 909 | | |
| 84 | php (7.4)| zend-framework (3.1) | 41 321 | | |
| 85 | php (7.4)| sw-fw-less (preview) | 40 957 | | |
| 86 | php (7.4)| imi (1.0) | 40 068 | | |
| 87 | java (8)| micronaut (1.2) | 38 758 | | |
| 88 | cpp (11)| evhtp (1.2) | 38 037 | | |
| 89 | ruby (2.6)| syro (3.1) | 37 256 | | |
| 90 | ruby (2.6)| roda (3.27) | 34 821 | | |
| 91 | javascript (12.13)| hapi (18.4) | 34 773 | | |
| 92 | fsharp (7.3)| suave (2.5) | 34 763 | | |
| 93 | php (7.4)| swoft (2.0) | 33 952 | | |
| 94 | php (7.4)| laravel (6.9) | 33 835 | | |
| 95 | ruby (2.6)| cuba (3.9) | 33 482 | | |
| 96 | java (8)| spring-framework (5.2) | 32 022 | | |
| 97 | javascript (12.13)| moleculer (0.13) | 30 010 | | |
| 98 | rust (1.39)| nickel (0.11) | 28 957 | | |
| 99 | python (3.8)| fastapi (0.45) | 27 274 | | |
| 100 | ruby (2.6)| rack-routing (0.0) | 27 215 | | |
| 101 | python (3.8)| molten (0.27) | 26 907 | | |
| 102 | python (3.8)| aiohttp (3.6) | 26 297 | | |
| 103 | python (3.8)| responder (2.0) | 25 187 | | |
| 104 | python (3.8)| clastic (19.9) | 25 181 | | |
| 105 | python (3.8)| flask (1.1) | 24 762 | | |
| 106 | crystal (0.31)| athena (0.7) | 23 562 | | |
| 107 | javascript (12.13)| turbo_polka (0.3) | 22 066 | | |
| 108 | crystal (0.31)| lucky (0.18) | 21 235 | | |
| 109 | ruby (2.6)| camping (2.1) | 20 942 | | |
| 110 | python (3.8)| sanic (19.9) | 20 868 | | |
| 111 | ruby (2.6)| flame (4.18) | 20 517 | | |
| 112 | python (3.8)| bocadillo (0.18) | 18 548 | | |
| 113 | php (7.4)| spiral (2.4) | 16 741 | | |
| 114 | rust (1.39)| iron (0.6) | 16 076 | | |
| 115 | ruby (2.6)| hanami (1.3) | 16 024 | | |
| 116 | ruby (2.6)| sinatra (2.0) | 15 082 | | |
| 117 | swift (5.1)| swifter (1.4) | 12 088 | | |
| 118 | ruby (2.6)| grape (1.2) | 11 903 | | |
| 119 | python (3.8)| tornado (6.0) | 10 345 | | |
| 120 | go (1.13)| gramework (1.7) | 10 266 | | |
| 121 | python (3.8)| quart (0.10) | 10 236 | | |
| 122 | python (3.8)| django (3.0) | 9 640 | | |
| 123 | python (3.8)| cherrypy (18.5) | 8 951 | | |
| 124 | python (3.8)| masonite (2.2) | 7 092 | | |
| 125 | crystal (0.31)| onyx (0.5) | 5 832 | | |
| 126 | ruby (2.6)| rails (6.0) | 3 848 | | |
| 127 | julia (1.3)| merly (0.2) | 3 308 | | |
| 128 | python (3.8)| cyclone (1.3) | 2 348 | | |
| 129 | python (3.8)| klein (19.6) | 1 447 | | |
| 130 | python (3.8)| nameko (2.12) | 1 422 | | |
| 131 | perl (5.3)| dancer2 (2.0) | 1 415 | | |

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

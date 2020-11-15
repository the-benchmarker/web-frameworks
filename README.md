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

:information_source:  Updated on **2020-11-15** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 187 218 | 205 150 | 207 124 |
| 2 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 181 194 | 189 668 | 189 175 |
| 3 | go (1.15)| [fiber](https://gofiber.io) (2.0) | 179 329 | 199 248 | 197 670 |
| 4 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.17) | 177 391 | 199 944 | 200 657 |
| 5 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.5) | 174 803 | 196 280 | 198 490 |
| 6 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 174 555 | 196 690 | 197 223 |
| 7 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 174 134 | 197 894 | 198 631 |
| 8 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 168 832 | 204 781 | 206 574 |
| 9 | java (11)| [jooby](https://jooby.io) (2.8) | 164 432 | 209 860 | 216 445 |
| 10 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 160 377 | 203 697 | 207 573 |
| 11 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 153 580 | 185 944 | 187 299 |
| 12 | java (11)| [act](https://actframework.org) (1.9) | 141 146 | 175 016 | 179 271 |
| 13 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 123 916 | 124 309 | 128 323 |
| 14 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 122 737 | 123 516 | 127 203 |
| 15 | go (1.15)| [clevergo](https://clevergo.tech) (0.3) | 122 691 | 123 190 | 126 874 |
| 16 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 121 999 | 121 789 | 125 781 |
| 17 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 120 660 | 126 141 | 128 683 |
| 18 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 119 460 | 123 698 | 127 189 |
| 19 | go (1.15)| [chi](https://github.com/go-chi/chi) (4.1) | 117 783 | 115 185 | 119 772 |
| 20 | go (1.15)| [violetear](https://violetear.org) (7.0) | 117 586 | 117 206 | 120 672 |
| 21 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 116 406 | 114 668 | 118 477 |
| 22 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 115 691 | 179 348 | 198 086 |
| 23 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 114 241 | 112 921 | 116 258 |
| 24 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 114 016 | 120 343 | 121 465 |
| 25 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 112 215 | 108 255 | 113 114 |
| 26 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 111 339 | 106 443 | 111 317 |
| 27 | go (1.15)| [beego](https://beego.me) (1.12) | 109 258 | 112 420 | 116 248 |
| 28 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 107 897 | 125 413 | 129 139 |
| 29 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.1) | 107 809 | 126 038 | 130 183 |
| 30 | fsharp (5.0)| [falco](https://github.com/pimbrouwers/Falco) (1.2) | 105 100 | 120 865 | 124 765 |
| 31 | go (1.15)| [air](https://github.com/aofei/air) (0.20) | 97 857 | 95 167 | 99 505 |
| 32 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.1) | 96 365 | 107 575 | 107 059 |
| 33 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.4) | 95 558 | 93 970 | 98 541 |
| 34 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 94 627 | 139 994 | 151 063 |
| 35 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 91 058 | 98 416 | 97 344 |
| 36 | java (11)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 87 105 | 93 332 | 93 378 |
| 37 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 87 104 | 105 536 | 114 469 |
| 38 | go (1.15)| [gf](https://goframe.org) (1.13) | 86 233 | 91 923 | 94 233 |
| 39 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 85 060 | 93 524 | 101 544 |
| 40 | java (11)| [spring-framework](https://spring.io/projects/spring-framework) (5.3) | 83 419 | 90 819 | 92 646 |
| 41 | java (11)| [javalin](https://javalin.io) (3.9) | 82 653 | 88 410 | 89 070 |
| 42 | php (7.4)| [hyperf](https://hyperf.io) (2.0) | 79 570 | 100 870 | 106 904 |
| 43 | fsharp (5.0)| [websharper](https://websharper.com) (4.6) | 75 758 | 85 359 | 85 919 |
| 44 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 75 088 | 83 223 | 84 355 |
| 45 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 71 737 | 80 470 | 87 349 |
| 46 | rust (1.47)| [actix](https://actix.rs) (3.2) | 71 025 | 84 325 | 81 973 |
| 47 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 69 925 | 68 845 | 73 148 |
| 48 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 65 784 | 66 232 | 65 027 |
| 49 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 63 105 | 67 208 | 68 097 |
| 50 | java (11)| [micronaut](https://micronaut.io) (1.2) | 62 925 | 75 464 | 75 131 |
| 51 | rust (1.47)| [nickel](https://nickel-org.github.io) (0.11) | 62 749 | 62 272 | 62 754 |
| 52 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 62 337 | 68 980 | 69 642 |
| 53 | swift (5.3)| [kitura-nio](https://kitura.io) (2.9) | 56 509 | 56 050 | 55 536 |
| 54 | swift (5.3)| [kitura](https://kitura.io) (2.9) | 55 705 | 55 987 | 56 108 |
| 55 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 54 491 | 75 460 | 79 256 |
| 56 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 53 763 | 58 143 | 58 164 |
| 57 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.27) | 53 141 | 56 146 | 57 149 |
| 58 | fsharp (5.0)| [suave](https://suave.io) (2.5) | 50 049 | 44 344 | 41 153 |
| 59 | python (3.8)| [hug](https://hug.rest) (2.6) | 49 266 | 51 769 | 51 952 |
| 60 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.9) | 48 889 | 51 663 | 52 432 |
| 61 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 48 517 | 54 712 | 55 632 |
| 62 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 48 309 | 54 412 | 55 001 |
| 63 | rust (1.47)| [gotham](https://gotham.rs) (0.4) | 47 053 | 51 677 | 52 742 |
| 64 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.1) | 43 623 | 48 265 | 47 843 |
| 65 | php (7.4)| [imi](https://imiphp.com) (1.2) | 43 403 | 51 810 | 53 681 |
| 66 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 42 123 | 32 666 | 31 287 |
| 67 | python (3.8)| [starlette](https://starlette.io) (0.13) | 40 598 | 48 933 | 49 251 |
| 68 | python (3.8)| [emmett](https://github.com/emmett-framework/emmett) (2.0) | 36 672 | 42 772 | 42 592 |
| 69 | python (3.8)| [index.py](https://index-py.abersheeran.com) (0.12) | 35 145 | 39 608 | 39 166 |
| 70 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 33 506 | 32 908 | 31 618 |
| 71 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 33 407 | 32 286 | 31 871 |
| 72 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 32 311 | 32 728 | 32 277 |
| 73 | rust (1.47)| [iron](https://ironframework.io) (0.6) | 29 615 | 30 250 | 29 988 |
| 74 | php (7.4)| [swoft](https://swoft.org) (2.0) | 26 763 | 31 540 | 31 910 |
| 75 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 26 453 | 35 591 | 34 528 |
| 76 | python (3.8)| [responder](https://python-responder.org) (2.0) | 25 528 | 33 791 | 33 502 |
| 77 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 25 337 | 29 982 | 30 787 |
| 78 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.61) | 25 270 | 32 768 | 32 679 |
| 79 | php (7.4)| [slim-swoole](https://slimframework.com) (4.5) | 24 373 | 28 802 | 28 684 |
| 80 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 24 189 | 25 161 | 25 110 |
| 81 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 24 079 | 30 781 | 30 831 |
| 82 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 23 199 | 26 439 | 26 189 |
| 83 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 22 541 | 25 076 | 25 086 |
| 84 | go (1.15)| [macaron](https://go-macaron.com) (1.3) | 19 400 | 21 479 | 21 978 |
| 85 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 17 205 | 16 956 | 16 364 |
| 86 | java (11)| [blade](https://lets-blade.com) (2.0) | 16 270 | 20 375 | 19 615 |
| 87 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 15 964 | 16 976 | 16 278 |
| 88 | java (11)| [struts2](https://struts.apache.org) (2.5) | 15 459 | 15 951 | 15 768 |
| 89 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.4) | 15 141 | 15 846 | 15 909 |
| 90 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 14 529 | 15 054 | 15 203 |
| 91 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.6) | 14 266 | 14 637 | 14 583 |
| 92 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 13 200 | 13 434 | 13 418 |
| 93 | swift (5.3)| [swifter-framework](https://github.com/httpswift/swifter) (1.5) | 12 665 | 12 668 | 12 636 |
| 94 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.13) | 10 069 | 11 462 | 10 902 |
| 95 | python (3.8)| [tornado](https://tornadoweb.org) (6.1) | 9 895 | 9 852 | 9 715 |
| 96 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 431 | 9 386 | 9 234 |
| 97 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 9 100 | 9 082 | 8 605 |
| 98 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 8 859 | 8 738 | 8 654 |
| 99 | python (3.8)| [django](https://djangoproject.com) (3.1) | 8 667 | 8 678 | 8 526 |
| 100 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 8 123 | 17 524 | 17 081 |
| 101 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 805 | 7 836 | 7 523 |
| 102 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 6 515 | 6 487 | 6 509 |
| 103 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 307 | 6 174 | 6 264 |
| 104 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 6 153 | 6 071 | 6 091 |
| 105 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.2) | 5 259 | 5 259 | 5 229 |
| 106 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.4) | 5 047 | 5 058 | 5 105 |
| 107 | php (7.4)| [lumen](https://lumen.laravel.com) (8.1) | 3 522 | 3 504 | 3 641 |
| 108 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.4) | 3 112 | 3 176 | 3 189 |
| 109 | php (7.4)| [symfony](https://symfony.com) (5.1) | 2 823 | 2 801 | 2 912 |
| 110 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 2 795 | 2 680 | 2 592 |
| 111 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 515 | 2 575 | 2 580 |
| 112 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 373 | 2 386 | 2 362 |
| 113 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 933 | 1 854 | 1 763 |
| 114 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 666 | 1 710 | 1 679 |
| 115 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 607 | 1 568 | 1 547 |
| 116 | python (3.8)| [django-ninja](https://django-ninja.rest-framework.com) (0.8) | 1 432 | 2 142 | 2 202 |
| 117 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 364 | 1 383 | 1 435 |
| 118 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 1 301 | 1 167 | 2 098 |
| 119 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 054 | 1 070 | 1 157 |
| 120 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 436 | 453 | 441 |

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

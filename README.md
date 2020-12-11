# Which is the fastest?

[![Build Status](https://the-benchmarker.semaphoreci.com/badges/web-frameworks/branches/master.svg)](https://the-benchmarker.semaphoreci.com/projects/web-frameworks)

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

+ [Ruby](https://ruby-lang.org) as `built-in` tools are made in this language
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

... to be documented ...

feel free to create an issue if you want to try this project

## Results

:information_source:  Updated on **2020-12-11** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 148 995.25 | 182 003.41 | 184 168.61 |
| 2 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 121 039.97 | 124 074.41 | 123 701.49 |
| 3 | go (1.15)| [fiber](https://gofiber.io) (2.2) | 119 962.62 | 130 497.32 | 129 525.38 |
| 4 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.5) | 117 801.53 | 129 565.96 | 128 757.82 |
| 5 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 117 772.30 | 147 343.30 | 149 861.85 |
| 6 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 117 117.57 | 127 822.15 | 127 015.66 |
| 7 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 116 945.76 | 128 747.99 | 127 835.10 |
| 8 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.17) | 116 544.11 | 130 547.38 | 131 014.70 |
| 9 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 114 736.68 | 134 562.31 | 135 769.00 |
| 10 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 114 734.28 | 143 051.01 | 145 997.57 |
| 11 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 112 285.83 | 111 246.65 | 114 589.12 |
| 12 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 110 100.51 | 137 015.62 | 142 632.99 |
| 13 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 109 481.91 | 135 535.56 | 138 440.87 |
| 14 | java (11)| [jooby](https://jooby.io) (2.9) | 108 890.14 | 136 295.40 | 141 345.10 |
| 15 | javascript (14.15)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 108 806.90 | 139 357.61 | 143 021.22 |
| 16 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 108 234.51 | 104 472.89 | 109 011.59 |
| 17 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 107 538.70 | 136 222.97 | 139 911.58 |
| 18 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 106 725.28 | 132 328.60 | 135 479.83 |
| 19 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.4) | 101 942.16 | 120 758.48 | 121 412.65 |
| 20 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 99 735.79 | 121 596.82 | 122 239.88 |
| 21 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 96 587.27 | 121 856.33 | 121 437.06 |
| 22 | java (11)| [act](https://actframework.org) (1.9) | 96 339.73 | 117 835.96 | 120 819.42 |
| 23 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 95 575.63 | 140 212.60 | 151 437.99 |
| 24 | c (11)| [kore](https://kore.io) (3.3) | 94 796.49 | 168 472.15 | 194 007.96 |
| 25 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 93 781.89 | 114 316.16 | 114 621.89 |
| 26 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 91 010.41 | 107 348.96 | 107 302.08 |
| 27 | crystal (0.35)| [kemal](https://kemalcr.com) (0.27) | 90 293.24 | 110 953.99 | 110 591.31 |
| 28 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.1) | 84 536.61 | 82 122.92 | 85 137.72 |
| 29 | javascript (14.15)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 84 082.76 | 100 623.58 | 100 516.07 |
| 30 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 82 572.40 | 97 772.76 | 94 590.46 |
| 31 | go (1.15)| [gf](https://goframe.org) (1.14) | 82 512.11 | 89 335.07 | 91 384.80 |
| 32 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.11) | 81 912.02 | 95 038.73 | 91 050.51 |
| 33 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 81 123.76 | 81 914.91 | 84 287.38 |
| 34 | go (1.15)| [clevergo](https://clevergo.tech) (0.5) | 80 910.02 | 82 022.84 | 84 162.71 |
| 35 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 79 990.50 | 81 222.82 | 83 247.04 |
| 36 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 79 760.69 | 80 724.09 | 83 005.93 |
| 37 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 79 336.54 | 111 496.36 | 122 394.40 |
| 38 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 78 511.44 | 82 223.78 | 83 887.94 |
| 39 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 475.23 | 82 881.26 | 84 098.18 |
| 40 | go (1.15)| [chi](https://github.com/go-chi/chi) (4.1) | 76 753.76 | 75 812.69 | 78 657.82 |
| 41 | go (1.15)| [violetear](https://violetear.org) (7.0) | 75 640.35 | 77 137.29 | 79 394.21 |
| 42 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 75 351.98 | 79 333.70 | 80 068.17 |
| 43 | fsharp (5.0)| [falco](https://github.com/pimbrouwers/Falco) (3.0) | 75 325.69 | 88 227.94 | 91 403.32 |
| 44 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 75 088.33 | 74 456.95 | 76 681.52 |
| 45 | java (11)| [restheart](https://restheart.org) (5.1) | 74 795.82 | 77 422.08 | 78 536.39 |
| 46 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 73 672.80 | 72 021.75 | 74 872.72 |
| 47 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 72 895.87 | 84 529.33 | 87 164.38 |
| 48 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 72 875.27 | 80 034.63 | 81 610.01 |
| 49 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 72 257.42 | 83 860.78 | 86 318.58 |
| 50 | go (1.15)| [beego](https://beego.me) (1.12) | 71 643.29 | 74 405.46 | 76 511.20 |
| 51 | javascript (14.15)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 69 124.61 | 79 291.87 | 80 571.24 |
| 52 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 65 320.22 | 73 173.97 | 73 555.73 |
| 53 | go (1.15)| [air](https://github.com/aofei/air) (0.20) | 64 642.13 | 63 264.15 | 65 711.27 |
| 54 | rust (1.48)| [actix](https://actix.rs) (3.3) | 63 409.39 | 58 555.30 | 60 210.08 |
| 55 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.4) | 62 826.32 | 62 575.44 | 65 527.04 |
| 56 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 62 230.90 | 67 745.63 | 67 320.09 |
| 57 | javascript (14.15)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 61 837.25 | 68 602.42 | 70 202.33 |
| 58 | javascript (14.15)| [0http](https://github.com/jkyberneees/0http) (3.0) | 60 252.49 | 65 543.54 | 67 010.16 |
| 59 | javascript (14.15)| [restana](https://github.com/jkyberneees/ana) (4.7) | 58 153.90 | 63 441.45 | 64 364.46 |
| 60 | javascript (14.15)| [polka](https://github.com/lukeed/polka) (0.5) | 57 162.63 | 61 163.86 | 61 272.57 |
| 61 | java (11)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 56 425.58 | 60 436.64 | 60 391.74 |
| 62 | javascript (14.15)| [rayo](https://rayo.js.org) (1.3) | 54 588.25 | 59 659.29 | 59 017.16 |
| 63 | java (11)| [javalin](https://javalin.io) (3.9) | 54 284.74 | 57 317.96 | 57 986.40 |
| 64 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 53 886.26 | 57 353.16 | 55 766.60 |
| 65 | javascript (14.15)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 53 881.17 | 58 210.71 | 57 933.66 |
| 66 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.14) | 53 841.88 | 74 206.05 | 82 355.03 |
| 67 | javascript (14.15)| [fastify](https://fastify.io) (3.7) | 53 684.16 | 59 976.15 | 58 540.23 |
| 68 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 53 172.11 | 61 264.75 | 63 886.83 |
| 69 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 52 634.30 | 63 869.04 | 67 851.20 |
| 70 | php (7.4)| [hyperf](https://hyperf.io) (2.0) | 51 997.60 | 58 381.43 | 66 264.00 |
| 71 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 51 283.85 | 58 141.45 | 58 280.41 |
| 72 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 51 184.27 | 66 256.81 | 68 537.13 |
| 73 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 51 145.17 | 56 543.85 | 63 463.84 |
| 74 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 50 529.40 | 57 440.52 | 57 734.37 |
| 75 | javascript (14.15)| [nestjs-fastify](https://nestjs.com) (7.4) | 47 256.87 | 52 371.89 | 50 989.46 |
| 76 | java (11)| [spring-framework](https://spring.io/projects/spring-framework) (5.3) | 46 565.49 | 50 591.98 | 51 533.83 |
| 77 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 46 185.37 | 46 628.09 | 49 541.81 |
| 78 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 45 659.86 | 49 609.72 | 51 276.40 |
| 79 | javascript (14.15)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 45 569.93 | 48 625.00 | 48 530.63 |
| 80 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 45 008.69 | 51 980.90 | 52 797.57 |
| 81 | javascript (14.15)| [foxify](https://foxify.js.org) (0.1) | 44 947.41 | 49 297.05 | 49 187.72 |
| 82 | java (11)| [micronaut](https://micronaut.io) (1.2) | 44 811.85 | 50 400.63 | 50 747.78 |
| 83 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.8) | 44 773.86 | 48 822.87 | 49 923.45 |
| 84 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 766.46 | 45 876.58 | 45 450.68 |
| 85 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 43 009.70 | 32 988.82 | 32 958.37 |
| 86 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 41 876.18 | 45 253.98 | 46 598.79 |
| 87 | javascript (14.15)| [koa](https://koajs.com) (2.13) | 41 480.07 | 45 033.05 | 43 142.00 |
| 88 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 41 471.28 | 136 900.86 | 139 492.55 |
| 89 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 40 344.03 | 43 252.75 | 43 813.62 |
| 90 | javascript (14.15)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 36 840.77 | 39 983.27 | 39 344.10 |
| 91 | swift (5.3)| [vapor](https://vapor.codes) (4.36) | 36 197.15 | 37 805.09 | 37 722.72 |
| 92 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 36 188.87 | 37 611.46 | 37 727.64 |
| 93 | swift (5.3)| [kitura-nio](https://kitura.io) (2.9) | 36 086.94 | 36 153.15 | 36 022.78 |
| 94 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 35 371.78 | 49 219.38 | 52 504.60 |
| 95 | javascript (14.15)| [hapi](https://hapijs.com) (20.0) | 35 302.09 | 35 696.12 | 35 463.82 |
| 96 | javascript (14.15)| [moleculer](https://moleculer.services) (0.14) | 35 052.73 | 35 985.35 | 35 520.46 |
| 97 | python (3.8)| [emmett](https://emmett.sh) (2.1) | 34 930.97 | 40 565.41 | 41 701.23 |
| 98 | fsharp (5.0)| [suave](https://suave.io) (2.5) | 34 732.34 | 30 777.29 | 25 237.90 |
| 99 | rust (1.48)| [nickel](https://nickel-org.github.io) (0.11) | 34 469.50 | 35 040.97 | 36 328.61 |
| 100 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 33 736.14 | 37 581.21 | 38 787.26 |
| 101 | python (3.8)| [hug](https://hug.rest) (2.6) | 32 769.93 | 35 108.78 | 34 973.81 |
| 102 | swift (5.3)| [kitura](https://kitura.io) (2.9) | 32 514.83 | 36 345.80 | 35 970.21 |
| 103 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 32 260.43 | 36 643.30 | 36 816.89 |
| 104 | javascript (14.15)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 31 532.70 | 33 392.88 | 49 515.35 |
| 105 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 31 312.37 | 31 457.24 | 31 237.12 |
| 106 | rust (1.48)| [gotham](https://gotham.rs) (0.4) | 30 624.04 | 34 074.83 | 34 797.54 |
| 107 | php (7.4)| [imi](https://imiphp.com) (1.2) | 29 408.28 | 32 777.32 | 34 262.42 |
| 108 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.9) | 28 897.67 | 33 291.05 | 33 943.51 |
| 109 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 28 881.80 | 31 863.98 | 31 763.34 |
| 110 | javascript (14.15)| [restify](https://restify.com) (8.5) | 28 798.53 | 31 250.05 | 29 801.66 |
| 111 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 28 608.97 | 32 620.60 | 32 867.48 |
| 112 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 27 894.12 | 29 126.80 | 29 124.15 |
| 113 | javascript (14.15)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 27 813.73 | 29 954.15 | 28 475.44 |
| 114 | python (3.8)| [starlette](https://starlette.io) (0.14) | 27 462.96 | 31 684.27 | 32 315.79 |
| 115 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 26 913.30 | 29 115.98 | 28 576.66 |
| 116 | scala (2.13)| [play](https://playframework.com) (2.8) | 25 675.43 | 27 882.99 | 28 729.87 |
| 117 | python (3.8)| [index.py](https://index-py.abersheeran.com) (0.15) | 25 520.19 | 28 982.85 | 29 670.40 |
| 118 | python (3.8)| [responder](https://python-responder.org) (2.0) | 24 970.11 | 31 295.44 | 32 502.26 |
| 119 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 23 941.36 | 26 110.58 | 25 643.42 |
| 120 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 23 734.99 | 23 924.10 | 21 321.10 |
| 121 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 22 499.40 | 21 932.15 | 20 617.09 |
| 122 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 22 305.43 | 21 961.33 | 21 720.45 |
| 123 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 22 034.84 | 21 513.80 | 20 898.38 |
| 124 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 21 807.86 | 28 197.00 | 27 893.07 |
| 125 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.16) | 21 149.75 | 21 291.00 | 19 315.20 |
| 126 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 20 846.51 | 20 307.93 | 20 610.48 |
| 127 | javascript (14.15)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 18 823.37 | 17 941.09 | 17 013.94 |
| 128 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 18 592.82 | 22 880.95 | 24 016.45 |
| 129 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 635.47 | 15 636.84 | 14 552.22 |
| 130 | php (7.4)| [swoft](https://swoft.org) (2.0) | 17 354.40 | 20 936.48 | 21 795.92 |
| 131 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 17 158.31 | 16 880.21 | 16 490.43 |
| 132 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 17 139.46 | 22 714.47 | 23 519.39 |
| 133 | rust (1.48)| [iron](https://ironframework.io) (0.6) | 17 117.39 | 16 759.61 | 16 976.55 |
| 134 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 16 626.40 | 16 475.36 | 15 891.47 |
| 135 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 16 472.03 | 20 997.06 | 20 811.91 |
| 136 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 16 321.35 | 19 618.17 | 20 085.97 |
| 137 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.62) | 16 085.54 | 20 764.42 | 21 273.84 |
| 138 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 16 077.55 | 14 214.56 | 13 261.66 |
| 139 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.38) | 16 058.78 | 15 866.92 | 15 243.04 |
| 140 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 16 035.94 | 20 103.68 | 20 071.30 |
| 141 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 473.53 | 18 091.86 | 18 533.63 |
| 142 | go (1.15)| [macaron](https://go-macaron.com) (1.4) | 15 420.07 | 16 923.69 | 16 937.15 |
| 143 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 15 377.19 | 18 006.79 | 18 129.80 |
| 144 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 15 339.81 | 14 922.44 | 14 645.17 |
| 145 | javascript (14.15)| [express](https://expressjs.com) (4.17) | 14 974.49 | 18 017.75 | 17 773.20 |
| 146 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 14 563.98 | 17 297.84 | 17 208.79 |
| 147 | javascript (14.15)| [feathersjs](https://feathersjs.com) (4.5) | 14 289.23 | 17 710.08 | 17 563.08 |
| 148 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 14 122.64 | 13 695.20 | 13 713.97 |
| 149 | java (11)| [struts2](https://struts.apache.org) (2.5) | 13 914.61 | 14 168.83 | 14 253.32 |
| 150 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 13 817.59 | 13 367.59 | 13 205.40 |
| 151 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 13 538.21 | 14 438.67 | 21 489.86 |
| 152 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 12 923.02 | 12 685.69 | 12 392.19 |
| 153 | java (11)| [blade](https://lets-blade.com) (2.0) | 12 844.55 | 15 686.72 | 14 683.05 |
| 154 | javascript (14.15)| [nestjs-express](https://nestjs.com) (7.4) | 11 671.91 | 13 622.49 | 13 430.74 |
| 155 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 348.42 | 11 610.63 | 11 673.69 |
| 156 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 11 135.89 | 10 929.88 | 10 489.83 |
| 157 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 10 262.19 | 10 442.04 | 10 559.46 |
| 158 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.4) | 10 233.41 | 10 453.43 | 10 596.89 |
| 159 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 992.65 | 9 853.44 | 9 357.82 |
| 160 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 878.26 | 10 078.08 | 10 061.84 |
| 161 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 9 792.33 | 10 019.26 | 10 088.83 |
| 162 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.6) | 9 653.22 | 9 687.68 | 9 779.89 |
| 163 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.1) | 9 070.36 | 8 834.29 | 8 781.91 |
| 164 | ruby (2.7)| [grape](https://ruby-grape.org) (1.5) | 8 806.83 | 8 565.18 | 8 506.07 |
| 165 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 7 759.94 | 13 569.20 | 12 696.96 |
| 166 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.13) | 7 566.33 | 7 242.62 | 6 926.83 |
| 167 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 6 737.18 | 6 681.18 | 6 597.08 |
| 168 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 655.34 | 6 571.94 | 6 499.81 |
| 169 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 6 573.57 | 6 497.52 | 6 374.11 |
| 170 | python (3.8)| [tornado](https://tornadoweb.org) (6.1) | 6 524.69 | 6 554.35 | 6 670.06 |
| 171 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 6 419.88 | 6 326.65 | 6 221.47 |
| 172 | python (3.8)| [django](https://djangoproject.com) (3.1) | 6 381.64 | 6 262.84 | 5 635.10 |
| 173 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 261.36 | 6 188.78 | 6 148.42 |
| 174 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 5 720.14 | 5 654.46 | 5 676.40 |
| 175 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 5 600.37 | 6 504.30 | 6 652.11 |
| 176 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 5 382.09 | 5 307.03 | 5 252.77 |
| 177 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 5 286.93 | 5 186.01 | 5 233.11 |
| 178 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 4 988.06 | 4 706.26 | 4 506.01 |
| 179 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 4 715.98 | 4 684.19 | 4 592.81 |
| 180 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 492.37 | 4 450.32 | 4 389.56 |
| 181 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 455.92 | 4 420.07 | 4 415.61 |
| 182 | javascript (14.15)| [sails](https://sailsjs.com) (1.4) | 4 241.52 | 4 221.58 | 4 178.05 |
| 183 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 4 150.55 | 4 126.58 | 4 150.81 |
| 184 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.2) | 4 005.14 | 3 946.84 | 3 933.41 |
| 185 | php (7.4)| [lumen](https://lumen.laravel.com) (8.1) | 3 859.22 | 3 844.26 | 3 875.49 |
| 186 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.4) | 3 825.97 | 3 773.45 | 3 767.12 |
| 187 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 3 542.07 | 3 506.35 | 3 508.95 |
| 188 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 3 426.81 | 3 819.78 | 1 579.03 |
| 189 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 2 768.14 | 2 744.69 | 2 749.13 |
| 190 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 638.38 | 2 631.59 | 2 645.26 |
| 191 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 378.00 | 2 411.69 | 2 407.61 |
| 192 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.4) | 2 349.52 | 2 346.10 | 2 359.03 |
| 193 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 2 156.93 | 6 969.67 | 5 010.66 |
| 194 | php (7.4)| [symfony](https://symfony.com) (5.1) | 2 072.01 | 2 090.28 | 2 093.66 |
| 195 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 1 940.36 | 1 934.34 | 1 913.39 |
| 196 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 913.89 | 1 846.13 | 1 755.67 |
| 197 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 891.18 | 1 855.23 | 1 815.10 |
| 198 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 1 883.58 | 1 877.35 | 1 884.34 |
| 199 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 1 663.81 | 1 645.62 | 1 644.82 |
| 200 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 607.65 | 1 620.88 | 1 594.86 |
| 201 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 495.02 | 1 513.73 | 1 510.99 |
| 202 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 348.41 | 624.91 | 955.76 |
| 203 | python (3.8)| [django-ninja](https://django-ninja.rest-framework.com) (0.9) | 1 280.02 | 1 619.34 | 1 658.50 |
| 204 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 106.85 | 1 127.64 | 1 125.88 |
| 205 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 043.44 | 1 016.18 | 1 004.39 |
| 206 | crystal (0.35)| [lucky](https://luckyframework.org) (0.23) | 439.12 | 444.29 | 421.48 |
| 207 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 286.39 | 299.24 | -89.56 |
| 208 | php (7.4)| [laravel](https://laravel.com) (7.27) | 144.84 | 44.90 | 1.79 |

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

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

:information_source:  Updated on **2020-12-14** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 145 449.20 | 182 976.30 | 176 461.11 |
| 2 | go (1.15)| [fiber](https://gofiber.io) (2.2) | 121 564.59 | 129 766.78 | 128 957.18 |
| 3 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 120 722.32 | 124 337.40 | 123 614.44 |
| 4 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 118 769.65 | 146 582.08 | 149 811.39 |
| 5 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 117 210.46 | 128 764.88 | 127 987.57 |
| 6 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.5) | 116 980.12 | 128 329.38 | 127 935.25 |
| 7 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 116 492.39 | 129 566.65 | 128 605.99 |
| 8 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.18) | 116 319.37 | 129 769.18 | 130 025.35 |
| 9 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 114 750.23 | 141 940.78 | 145 363.43 |
| 10 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 113 685.65 | 134 495.68 | 136 518.21 |
| 11 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 112 190.96 | 111 982.08 | 114 904.10 |
| 12 | javascript (14.15)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 109 730.80 | 140 107.44 | 142 910.48 |
| 13 | java (11)| [jooby](https://jooby.io) (2.9) | 109 570.29 | 137 826.65 | 141 866.50 |
| 14 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 108 888.85 | 133 958.47 | 140 749.95 |
| 15 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 108 397.67 | 104 797.26 | 109 257.30 |
| 16 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 107 283.94 | 134 261.05 | 138 146.31 |
| 17 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 107 206.93 | 130 022.23 | 133 315.63 |
| 18 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 105 965.97 | 131 896.31 | 135 440.74 |
| 19 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 99 758.99 | 122 265.01 | 122 541.17 |
| 20 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.4) | 98 425.23 | 120 815.30 | 121 183.74 |
| 21 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 97 558.82 | 120 846.09 | 121 039.29 |
| 22 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 95 641.88 | 116 666.55 | 119 757.85 |
| 23 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 94 658.17 | 113 554.13 | 112 521.81 |
| 24 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 93 747.74 | 136 101.45 | 146 375.33 |
| 25 | c (11)| [kore](https://kore.io) (3.3) | 93 516.02 | 180 777.39 | 179 043.33 |
| 26 | crystal (0.35)| [kemal](https://kemalcr.com) (0.27) | 92 623.23 | 111 622.93 | 111 328.57 |
| 27 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 89 318.88 | 106 807.45 | 107 148.18 |
| 28 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.11) | 83 158.05 | 95 210.07 | 90 297.77 |
| 29 | javascript (14.15)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 82 719.31 | 98 910.51 | 99 636.01 |
| 30 | go (1.15)| [gf](https://goframe.org) (1.14) | 82 672.86 | 89 230.36 | 91 268.53 |
| 31 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 80 988.48 | 81 628.79 | 83 668.13 |
| 32 | go (1.15)| [clevergo](https://clevergo.tech) (0.5) | 80 357.29 | 81 519.91 | 83 468.44 |
| 33 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 80 057.58 | 81 022.86 | 83 109.75 |
| 34 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 79 913.08 | 80 891.97 | 82 499.57 |
| 35 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 78 755.14 | 93 177.17 | 90 562.93 |
| 36 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 067.43 | 82 528.12 | 83 712.94 |
| 37 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 77 646.26 | 81 624.52 | 83 012.44 |
| 38 | go (1.15)| [chi](https://github.com/go-chi/chi) (1.5) | 77 633.41 | 76 783.69 | 79 272.47 |
| 39 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 77 107.91 | 117 211.83 | 127 813.98 |
| 40 | go (1.15)| [violetear](https://violetear.org) (7.0) | 76 946.50 | 77 035.10 | 78 983.96 |
| 41 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 75 129.17 | 78 688.35 | 79 365.59 |
| 42 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 74 862.73 | 74 158.48 | 76 686.96 |
| 43 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.2) | 74 697.00 | 77 577.40 | 75 003.66 |
| 44 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 73 993.06 | 81 358.49 | 82 435.72 |
| 45 | javascript (14.15)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 73 798.42 | 80 459.05 | 80 325.39 |
| 46 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 73 312.64 | 71 181.47 | 74 212.14 |
| 47 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 72 543.09 | 84 110.17 | 86 695.33 |
| 48 | go (1.15)| [beego](https://beego.me) (1.12) | 71 864.31 | 75 129.72 | 76 871.40 |
| 49 | java (11)| [restheart](https://restheart.org) (5.1) | 71 672.19 | 74 267.07 | 74 808.65 |
| 50 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 71 620.86 | 83 260.94 | 85 861.61 |
| 51 | fsharp (5.0)| [falco](https://github.com/pimbrouwers/Falco) (3.0) | 70 716.10 | 81 547.22 | 83 753.72 |
| 52 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 65 234.98 | 72 777.71 | 73 062.23 |
| 53 | go (1.15)| [air](https://github.com/aofei/air) (0.21) | 64 601.09 | 63 089.64 | 65 452.19 |
| 54 | javascript (14.15)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 64 169.64 | 69 551.10 | 70 598.96 |
| 55 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.4) | 62 407.02 | 62 336.72 | 64 971.59 |
| 56 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 62 028.43 | 68 269.33 | 67 846.06 |
| 57 | javascript (14.15)| [0http](https://github.com/jkyberneees/0http) (3.0) | 60 246.77 | 66 217.21 | 67 802.53 |
| 58 | javascript (14.15)| [polka](https://github.com/lukeed/polka) (0.5) | 58 326.02 | 62 284.93 | 62 505.37 |
| 59 | rust (1.48)| [actix](https://actix.rs) (3.3) | 58 216.99 | 60 934.48 | 62 086.55 |
| 60 | java (11)| [spring-boot](https://spring.io/projects/spring-boot) (2.4) | 58 207.25 | 61 862.69 | 62 226.25 |
| 61 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 57 428.69 | 125 561.02 | 140 774.95 |
| 62 | javascript (14.15)| [rayo](https://rayo.js.org) (1.3) | 55 360.01 | 61 292.18 | 59 586.17 |
| 63 | javascript (14.15)| [fastify](https://fastify.io) (3.7) | 55 336.68 | 61 013.27 | 59 441.23 |
| 64 | javascript (14.15)| [restana](https://github.com/jkyberneees/ana) (4.7) | 54 857.93 | 62 827.97 | 63 360.18 |
| 65 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 53 530.26 | 56 495.81 | 55 273.83 |
| 66 | java (11)| [javalin](https://javalin.io) (3.9) | 53 396.81 | 57 138.99 | 57 553.73 |
| 67 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 53 113.92 | 58 182.18 | 58 260.82 |
| 68 | php (7.4)| [hyperf](https://hyperf.io) (2.0) | 52 503.04 | 60 519.21 | 68 483.34 |
| 69 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 52 412.74 | 61 027.20 | 63 346.40 |
| 70 | javascript (14.15)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 52 000.47 | 58 846.71 | 57 789.30 |
| 71 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 51 880.40 | 67 451.21 | 69 895.22 |
| 72 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 51 841.26 | 57 429.21 | 64 159.47 |
| 73 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.14) | 51 307.06 | 75 325.34 | 84 568.44 |
| 74 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 50 722.84 | 62 565.71 | 70 953.21 |
| 75 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 50 624.30 | 57 331.08 | 57 477.10 |
| 76 | java (11)| [micronaut](https://micronaut.io) (1.2) | 50 476.71 | 56 590.25 | 56 360.37 |
| 77 | javascript (14.15)| [nestjs-fastify](https://nestjs.com) (7.4) | 48 891.67 | 52 540.39 | 50 994.87 |
| 78 | java (11)| [spring-framework](https://spring.io/projects/spring-framework) (5.3) | 47 644.51 | 51 293.09 | 52 525.87 |
| 79 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 46 189.30 | 46 607.24 | 49 622.79 |
| 80 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 46 106.97 | 49 791.32 | 51 232.83 |
| 81 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 46 059.92 | 50 954.91 | 52 274.93 |
| 82 | javascript (14.15)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 45 955.65 | 49 091.77 | 47 717.86 |
| 83 | javascript (14.15)| [foxify](https://foxify.js.org) (0.1) | 45 747.18 | 49 932.35 | 49 457.69 |
| 84 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.8) | 44 871.00 | 49 221.45 | 49 912.28 |
| 85 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 558.63 | 45 699.46 | 45 715.96 |
| 86 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 42 576.28 | 31 829.11 | 31 018.10 |
| 87 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 41 847.25 | 45 278.35 | 46 353.51 |
| 88 | javascript (14.15)| [koa](https://koajs.com) (2.13) | 40 684.15 | 44 094.70 | 43 782.99 |
| 89 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 40 100.01 | 43 349.54 | 44 131.08 |
| 90 | javascript (14.15)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 36 458.23 | 39 769.07 | 39 018.35 |
| 91 | python (3.8)| [emmett](https://emmett.sh) (2.1) | 36 276.17 | 40 278.17 | 40 840.68 |
| 92 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 36 166.94 | 37 444.06 | 37 687.08 |
| 93 | rust (1.48)| [nickel](https://nickel-org.github.io) (0.11) | 36 161.15 | 33 662.88 | 36 256.93 |
| 94 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 36 106.65 | 36 677.65 | 36 571.98 |
| 95 | javascript (14.15)| [moleculer](https://moleculer.services) (0.14) | 35 902.93 | 36 713.61 | 35 319.47 |
| 96 | swift (5.3)| [vapor](https://vapor.codes) (4.36) | 35 653.64 | 36 970.70 | 36 843.30 |
| 97 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 35 575.96 | 49 740.52 | 52 395.58 |
| 98 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 35 422.57 | 35 775.61 | 36 031.34 |
| 99 | javascript (14.15)| [hapi](https://hapijs.com) (20.0) | 35 192.13 | 35 301.83 | 34 839.54 |
| 100 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 34 123.00 | 38 348.39 | 39 050.52 |
| 101 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 32 769.44 | 36 131.14 | 36 564.34 |
| 102 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 32 056.56 | 28 799.96 | 24 250.88 |
| 103 | python (3.8)| [hug](https://hug.rest) (2.6) | 31 473.06 | 34 968.51 | 34 987.74 |
| 104 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 31 395.45 | 31 561.90 | 30 909.99 |
| 105 | javascript (14.15)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 30 477.44 | 48 365.09 | 32 151.26 |
| 106 | rust (1.48)| [gotham](https://gotham.rs) (0.5) | 30 437.78 | 33 918.43 | 34 706.56 |
| 107 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 30 036.76 | 34 186.32 | 34 404.96 |
| 108 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.9) | 29 517.30 | 33 421.35 | 33 755.18 |
| 109 | javascript (14.15)| [restify](https://restify.com) (8.5) | 29 477.63 | 31 314.80 | 29 690.13 |
| 110 | python (3.8)| [starlette](https://starlette.io) (0.14) | 29 298.08 | 31 080.14 | 32 274.13 |
| 111 | php (7.4)| [imi](https://imiphp.com) (1.2) | 29 233.09 | 33 038.53 | 33 697.31 |
| 112 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 29 085.02 | 32 143.79 | 31 979.01 |
| 113 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 27 833.76 | 28 973.17 | 29 473.76 |
| 114 | javascript (14.15)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 27 580.27 | 30 708.67 | 29 237.90 |
| 115 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 27 101.01 | 29 384.44 | 28 756.21 |
| 116 | scala (2.13)| [play](https://playframework.com) (2.8) | 26 738.83 | 28 457.88 | 27 893.30 |
| 117 | python (3.8)| [responder](https://python-responder.org) (2.0) | 25 427.41 | 31 663.39 | 32 242.98 |
| 118 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 24 189.18 | 24 696.47 | 21 664.25 |
| 119 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 24 184.64 | 26 329.84 | 25 716.62 |
| 120 | python (3.8)| [index.py](https://index-py.abersheeran.com) (0.15) | 23 958.14 | 28 743.97 | 29 037.07 |
| 121 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 22 972.65 | 21 792.65 | 20 693.95 |
| 122 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 22 023.97 | 21 521.29 | 21 206.50 |
| 123 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 21 936.43 | 27 835.00 | 27 665.40 |
| 124 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 21 883.27 | 19 925.68 | 19 645.92 |
| 125 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.16) | 21 326.67 | 21 552.43 | 19 493.02 |
| 126 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 20 934.19 | 20 271.32 | 20 655.84 |
| 127 | javascript (14.15)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 18 824.30 | 18 165.39 | 17 109.26 |
| 128 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 18 787.25 | 20 946.69 | 20 759.65 |
| 129 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 18 643.92 | 22 438.78 | 23 308.85 |
| 130 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 18 499.16 | 22 921.87 | 24 356.47 |
| 131 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 570.34 | 15 605.64 | 14 689.71 |
| 132 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 17 333.00 | 17 258.48 | 16 291.03 |
| 133 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 17 128.87 | 20 357.42 | 20 303.56 |
| 134 | php (7.4)| [swoft](https://swoft.org) (2.0) | 17 107.10 | 20 964.20 | 21 553.07 |
| 135 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 16 630.72 | 16 974.26 | 15 785.07 |
| 136 | rust (1.48)| [iron](https://ironframework.io) (0.6) | 16 392.71 | 16 445.73 | 16 794.98 |
| 137 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 16 372.68 | 19 403.95 | 19 837.04 |
| 138 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 15 955.73 | 14 205.84 | 13 287.92 |
| 139 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.38) | 15 864.86 | 15 608.58 | 15 170.48 |
| 140 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.62) | 15 794.03 | 20 220.76 | 20 648.15 |
| 141 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 15 499.20 | 17 564.57 | 17 924.07 |
| 142 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 423.02 | 17 890.02 | 18 250.16 |
| 143 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 15 321.27 | 14 966.20 | 14 642.69 |
| 144 | go (1.15)| [macaron](https://go-macaron.com) (1.4) | 15 262.77 | 16 814.52 | 16 819.83 |
| 145 | javascript (14.15)| [express](https://expressjs.com) (4.17) | 15 159.88 | 18 165.45 | 17 519.97 |
| 146 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 14 171.19 | 13 706.84 | 13 555.61 |
| 147 | javascript (14.15)| [feathersjs](https://feathersjs.com) (4.5) | 13 940.19 | 15 962.23 | 17 735.83 |
| 148 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 13 860.32 | 13 486.80 | 13 183.50 |
| 149 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 13 796.16 | 13 579.54 | 13 488.23 |
| 150 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 13 680.66 | 17 437.07 | 17 323.99 |
| 151 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 13 266.68 | 12 791.73 | 12 657.58 |
| 152 | java (11)| [blade](https://lets-blade.com) (2.0) | 12 905.88 | 16 206.03 | 15 106.68 |
| 153 | javascript (14.15)| [nestjs-express](https://nestjs.com) (7.4) | 11 522.86 | 12 705.22 | 12 735.23 |
| 154 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 390.03 | 11 680.91 | 11 712.13 |
| 155 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 11 045.75 | 10 892.46 | 10 459.25 |
| 156 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.4) | 10 206.67 | 10 444.01 | 10 544.87 |
| 157 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 10 176.14 | 10 511.97 | 10 612.89 |
| 158 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 802.28 | 10 030.06 | 10 011.97 |
| 159 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 9 727.91 | 10 050.73 | 10 109.82 |
| 160 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.6) | 9 642.96 | 9 757.74 | 9 829.66 |
| 161 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.1) | 9 088.09 | 8 794.55 | 8 781.80 |
| 162 | ruby (2.7)| [grape](https://ruby-grape.org) (1.5) | 8 795.24 | 8 556.43 | 8 532.48 |
| 163 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 8 683.70 | 8 660.83 | 8 232.24 |
| 164 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 7 383.65 | 7 092.26 | 6 839.84 |
| 165 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 7 348.83 | 13 238.07 | 12 680.68 |
| 166 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 6 753.58 | 6 704.97 | 6 587.21 |
| 167 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 641.11 | 6 569.59 | 6 524.40 |
| 168 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 6 554.67 | 6 503.81 | 6 302.72 |
| 169 | python (3.8)| [tornado](https://tornadoweb.org) (6.1) | 6 457.93 | 6 627.24 | 6 412.18 |
| 170 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 6 404.73 | 6 358.15 | 6 266.33 |
| 171 | python (3.8)| [django](https://djangoproject.com) (3.1) | 6 377.03 | 5 797.71 | 5 669.66 |
| 172 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 277.95 | 6 195.36 | 6 167.69 |
| 173 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 5 744.25 | 5 651.12 | 5 660.23 |
| 174 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 5 334.14 | 5 265.19 | 5 223.04 |
| 175 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 5 280.06 | 5 182.44 | 5 240.20 |
| 176 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 5 242.72 | 6 138.34 | 6 541.92 |
| 177 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 4 731.80 | 4 673.50 | 4 611.28 |
| 178 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 4 666.50 | 4 393.20 | 4 157.42 |
| 179 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 4 530.90 | 1 962.15 | 1 436.30 |
| 180 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 474.82 | 4 429.67 | 4 393.40 |
| 181 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 467.57 | 4 416.78 | 4 448.67 |
| 182 | javascript (14.15)| [sails](https://sailsjs.com) (1.4) | 4 265.29 | 4 253.44 | 4 195.80 |
| 183 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 4 144.53 | 4 100.21 | 4 142.33 |
| 184 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.2) | 4 002.79 | 3 945.63 | 3 933.97 |
| 185 | php (7.4)| [lumen](https://lumen.laravel.com) (8.1) | 3 858.36 | 3 841.33 | 3 885.88 |
| 186 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.4) | 3 786.66 | 3 726.80 | 3 736.46 |
| 187 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 3 529.09 | 3 483.60 | 3 498.63 |
| 188 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 010.59 | 7 022.13 | 5 151.75 |
| 189 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 2 866.78 | 2 847.19 | 2 856.77 |
| 190 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 644.70 | 2 635.07 | 2 653.16 |
| 191 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 544.53 | 2 541.30 | 2 534.55 |
| 192 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.4) | 2 356.38 | 2 359.55 | 2 366.11 |
| 193 | php (7.4)| [symfony](https://symfony.com) (5.1) | 2 079.03 | 2 101.10 | 2 099.24 |
| 194 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 1 946.70 | 1 955.15 | 1 917.20 |
| 195 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 923.16 | 1 866.26 | 1 771.38 |
| 196 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 904.31 | 1 824.77 | 1 771.53 |
| 197 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 1 883.95 | 1 871.87 | 1 887.87 |
| 198 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 1 646.18 | 1 643.46 | 1 630.97 |
| 199 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 617.71 | 1 640.19 | 1 609.81 |
| 200 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 470.83 | 1 490.98 | 1 492.27 |
| 201 | python (3.8)| [django-ninja](https://django-ninja.rest-framework.com) (0.9) | 1 331.99 | 1 611.96 | 1 644.39 |
| 202 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 131.46 | 528.67 | 502.76 |
| 203 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 107.19 | 1 128.16 | 1 127.27 |
| 204 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 041.67 | 1 015.09 | 1 007.42 |
| 205 | crystal (0.35)| [lucky](https://luckyframework.org) (0.23) | 435.90 | 446.88 | 418.82 |
| 206 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 287.95 | 302.50 | -82.71 |
| 207 | php (7.4)| [laravel](https://laravel.com) (7.27) | 141.39 | 47.87 | 3.80 |

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

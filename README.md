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

:information_source:  Updated on **2020-11-29** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 148 892.84 | 182 506.75 | 184 165.88 |
| 2 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 121 014.97 | 123 207.24 | 122 678.78 |
| 3 | go (1.15)| [fiber](https://gofiber.io) (2.2) | 120 719.74 | 130 021.45 | 128 951.49 |
| 4 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 118 147.54 | 146 225.13 | 149 207.73 |
| 5 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.5) | 116 513.77 | 128 600.17 | 128 104.90 |
| 6 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.17) | 116 509.44 | 130 307.01 | 130 195.03 |
| 7 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 116 212.19 | 127 898.74 | 127 310.11 |
| 8 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 115 691.16 | 128 952.46 | 128 760.15 |
| 9 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 114 399.26 | 141 738.58 | 144 248.77 |
| 10 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 114 319.90 | 134 914.96 | 136 061.53 |
| 11 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 112 737.43 | 112 066.29 | 115 443.19 |
| 12 | java (11)| [jooby](https://jooby.io) (2.8) | 110 295.48 | 139 001.75 | 143 505.07 |
| 13 | kotlin (1.4)| [kooby](https://jooby.io) (2.8) | 109 982.40 | 136 922.20 | 141 385.57 |
| 14 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 109 082.99 | 134 346.39 | 136 908.27 |
| 15 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 107 719.90 | 104 576.47 | 108 998.66 |
| 16 | javascript (14.15)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 107 294.87 | 138 347.23 | 140 763.51 |
| 17 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 107 293.06 | 132 871.06 | 136 265.45 |
| 18 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 106 052.64 | 134 492.35 | 138 387.80 |
| 19 | c (11)| [kore](https://kore.io) (3.3) | 106 041.49 | 180 557.88 | 194 747.18 |
| 20 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 98 536.91 | 121 314.54 | 121 874.17 |
| 21 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 98 513.16 | 120 357.14 | 120 895.16 |
| 22 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.3) | 97 698.41 | 120 089.62 | 120 387.58 |
| 23 | java (11)| [act](https://actframework.org) (1.9) | 95 276.77 | 115 257.81 | 118 392.24 |
| 24 | php (7.4)| [nano](https://) (0.0.9) | 94 772.82 | 139 939.73 | 150 637.24 |
| 25 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 93 548.02 | 112 663.14 | 112 568.27 |
| 26 | crystal (0.35)| [kemal](https://kemalcr.com) (0.26) | 92 760.36 | 111 436.11 | 111 643.51 |
| 27 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 88 186.57 | 104 676.61 | 105 083.18 |
| 28 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.1) | 84 543.02 | 81 806.48 | 88 116.49 |
| 29 | javascript (14.15)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 83 679.49 | 96 835.50 | 99 345.98 |
| 30 | go (1.15)| [gf](https://goframe.org) (1.14) | 82 711.35 | 89 572.05 | 91 450.14 |
| 31 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.11) | 82 580.25 | 94 387.43 | 89 726.00 |
| 32 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 81 915.15 | 96 383.36 | 93 717.69 |
| 33 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 80 836.69 | 81 747.49 | 84 048.61 |
| 34 | go (1.15)| [clevergo](https://clevergo.tech) (0.3) | 80 737.11 | 81 953.95 | 83 922.82 |
| 35 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 80 452.29 | 132 267.36 | 124 059.72 |
| 36 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 80 045.24 | 80 752.54 | 82 798.14 |
| 37 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 79 833.52 | 80 996.00 | 82 787.32 |
| 38 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 78 373.16 | 82 077.23 | 83 669.95 |
| 39 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 050.99 | 82 577.13 | 83 694.73 |
| 40 | go (1.15)| [violetear](https://violetear.org) (7.0) | 77 255.98 | 77 515.90 | 79 563.25 |
| 41 | go (1.15)| [chi](https://github.com/go-chi/chi) (4.1) | 76 655.12 | 75 734.27 | 78 313.10 |
| 42 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 76 490.53 | 113 059.84 | 129 768.28 |
| 43 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 75 204.34 | 79 459.73 | 79 778.67 |
| 44 | fsharp (5.0)| [falco](https://github.com/pimbrouwers/Falco) (3.0) | 74 997.60 | 87 963.06 | 90 920.76 |
| 45 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 74 824.59 | 74 410.60 | 76 548.03 |
| 46 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 73 527.69 | 71 606.45 | 74 402.44 |
| 47 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 72 888.24 | 84 521.40 | 86 732.12 |
| 48 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 72 110.69 | 79 912.09 | 81 996.92 |
| 49 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 71 512.20 | 82 800.90 | 85 447.23 |
| 50 | go (1.15)| [beego](https://beego.me) (1.12) | 71 221.60 | 74 309.39 | 76 268.73 |
| 51 | go (1.15)| [air](https://github.com/aofei/air) (0.20) | 64 775.63 | 63 362.03 | 65 501.01 |
| 52 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 63 256.60 | 70 111.63 | 70 294.11 |
| 53 | rust (1.48)| [actix](https://actix.rs) (3.3) | 62 864.30 | 59 641.04 | 60 798.95 |
| 54 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.4) | 62 646.78 | 62 504.43 | 65 240.66 |
| 55 | java (11)| [restheart](https://restheart.org) (5.1) | 62 387.35 | 64 962.21 | 66 161.76 |
| 56 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 62 338.29 | 67 819.47 | 67 646.20 |
| 57 | javascript (14.15)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 62 027.00 | 68 434.24 | 69 564.35 |
| 58 | javascript (14.15)| [0http](https://github.com/jkyberneees/0http) (3.0) | 60 506.29 | 66 045.35 | 67 515.33 |
| 59 | java (11)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 58 110.29 | 61 632.09 | 62 029.46 |
| 60 | javascript (14.15)| [restana](https://github.com/jkyberneees/ana) (4.7) | 57 947.47 | 63 377.76 | 64 584.65 |
| 61 | javascript (14.15)| [polka](https://github.com/lukeed/polka) (0.5) | 57 459.71 | 62 380.68 | 60 997.84 |
| 62 | javascript (14.15)| [rayo](https://rayo.js.org) (1.3) | 55 573.59 | 60 420.82 | 59 441.78 |
| 63 | java (11)| [javalin](https://javalin.io) (3.9) | 54 223.15 | 57 424.63 | 57 863.28 |
| 64 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 54 098.65 | 60 573.66 | 67 303.87 |
| 65 | javascript (14.15)| [fastify](https://fastify.io) (3.7) | 54 077.23 | 59 771.68 | 58 741.92 |
| 66 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 54 050.74 | 57 333.93 | 55 785.68 |
| 67 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.14) | 54 006.97 | 74 919.62 | 81 680.60 |
| 68 | javascript (14.15)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 53 901.16 | 58 470.46 | 58 821.94 |
| 69 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 52 809.67 | 61 260.72 | 63 783.40 |
| 70 | php (7.4)| [hyperf](https://hyperf.io) (2.0) | 52 475.21 | 61 124.73 | 66 841.70 |
| 71 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 51 512.49 | 56 645.18 | 63 612.60 |
| 72 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 51 076.27 | 54 579.13 | 54 697.89 |
| 73 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 50 735.60 | 66 456.80 | 68 964.33 |
| 74 | fsharp (5.0)| [websharper](https://websharper.com) (4.6) | 49 066.91 | 55 091.77 | 55 179.52 |
| 75 | javascript (14.15)| [nestjs-fastify](https://nestjs.com) (7.4) | 48 101.10 | 51 981.08 | 50 375.49 |
| 76 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 46 753.17 | 49 338.16 | 51 465.32 |
| 77 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 46 041.25 | 46 428.08 | 49 565.65 |
| 78 | javascript (14.15)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 45 701.43 | 48 737.35 | 47 608.22 |
| 79 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 45 183.34 | 51 454.15 | 52 751.14 |
| 80 | java (11)| [micronaut](https://micronaut.io) (1.2) | 45 135.41 | 51 204.76 | 51 205.18 |
| 81 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.8) | 44 439.83 | 49 174.86 | 49 760.18 |
| 82 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 265.80 | 45 311.55 | 45 221.32 |
| 83 | javascript (14.15)| [foxify](https://foxify.js.org) (0.1) | 43 374.12 | 49 096.58 | 47 969.69 |
| 84 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 42 976.00 | 32 637.81 | 32 550.25 |
| 85 | javascript (14.15)| [koa](https://koajs.com) (2.13) | 42 005.27 | 44 455.84 | 42 943.71 |
| 86 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 40 937.59 | 45 428.67 | 46 144.46 |
| 87 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 40 636.46 | 43 584.96 | 44 644.68 |
| 88 | javascript (14.15)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 36 974.27 | 38 902.93 | 38 860.91 |
| 89 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 36 269.92 | 37 364.23 | 37 578.48 |
| 90 | swift (5.3)| [vapor](https://vapor.codes) (4.36) | 35 327.77 | 36 951.21 | 36 800.85 |
| 91 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 35 321.81 | 49 418.13 | 53 219.06 |
| 92 | javascript (14.15)| [moleculer](https://moleculer.services) (0.14) | 35 220.28 | 35 880.24 | 35 163.47 |
| 93 | python (3.8)| [emmett](https://emmett.sh) (2.1) | 35 161.10 | 40 870.17 | 41 892.15 |
| 94 | swift (5.3)| [kitura-nio](https://kitura.io) (2.9) | 34 658.91 | 36 214.50 | 36 145.10 |
| 95 | javascript (14.15)| [hapi](https://hapijs.com) (20.0) | 34 606.70 | 35 525.58 | 34 834.05 |
| 96 | rust (1.48)| [nickel](https://nickel-org.github.io) (0.11) | 33 826.46 | 33 468.01 | 34 497.34 |
| 97 | python (3.8)| [hug](https://hug.rest) (2.6) | 32 871.91 | 35 122.24 | 52 724.86 |
| 98 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 32 665.07 | 36 826.43 | 37 190.73 |
| 99 | fsharp (5.0)| [suave](https://suave.io) (2.5) | 32 485.34 | 30 180.84 | 25 365.62 |
| 100 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 31 371.57 | 31 278.35 | 30 831.01 |
| 101 | javascript (14.15)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 31 165.55 | 49 616.63 | 51 021.68 |
| 102 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 30 792.86 | 34 674.23 | 35 528.24 |
| 103 | rust (1.48)| [gotham](https://gotham.rs) (0.4) | 30 600.66 | 34 058.81 | 34 850.49 |
| 104 | php (7.4)| [imi](https://imiphp.com) (1.2) | 29 885.73 | 33 118.01 | 34 502.49 |
| 105 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.9) | 29 617.35 | 34 264.06 | 34 562.98 |
| 106 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 29 149.61 | 32 514.41 | 34 031.12 |
| 107 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 29 023.89 | 32 099.86 | 32 156.48 |
| 108 | javascript (14.15)| [restify](https://restify.com) (8.5) | 28 283.04 | 30 020.75 | 29 186.47 |
| 109 | javascript (14.15)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 28 004.84 | 29 354.50 | 28 897.26 |
| 110 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 27 813.43 | 28 981.99 | 29 009.06 |
| 111 | python (3.8)| [starlette](https://starlette.io) (0.14) | 27 183.17 | 31 638.32 | 32 423.83 |
| 112 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.10) | 26 957.15 | 29 244.79 | 28 952.65 |
| 113 | scala (2.13)| [play](https://playframework.com) (2.8) | 26 847.70 | 29 118.10 | 28 857.30 |
| 114 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 24 930.83 | 23 991.53 | 20 241.27 |
| 115 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 24 183.70 | 26 460.06 | 25 834.36 |
| 116 | python (3.8)| [responder](https://python-responder.org) (2.0) | 23 343.51 | 30 092.75 | 30 321.40 |
| 117 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 23 080.52 | 21 561.65 | 20 451.06 |
| 118 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 22 614.84 | 27 199.66 | 26 981.19 |
| 119 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 22 263.69 | 21 990.64 | 21 748.57 |
| 120 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 22 042.09 | 21 478.65 | 20 567.35 |
| 121 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.16) | 21 544.89 | 20 640.25 | 18 214.51 |
| 122 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 21 025.07 | 20 325.68 | 20 896.81 |
| 123 | python (3.8)| [index.py](https://index-py.abersheeran.com) (0.12) | 20 277.45 | 24 735.99 | 25 015.83 |
| 124 | javascript (14.15)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 19 008.35 | 18 042.02 | 16 880.25 |
| 125 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 18 437.49 | 22 741.78 | 24 475.69 |
| 126 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 18 055.80 | 23 271.64 | 23 387.88 |
| 127 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 718.62 | 15 711.98 | 14 688.18 |
| 128 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 17 411.81 | 20 121.27 | 20 039.76 |
| 129 | php (7.4)| [swoft](https://swoft.org) (2.0) | 17 238.72 | 20 449.03 | 21 366.15 |
| 130 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 17 176.18 | 16 671.93 | 16 395.82 |
| 131 | rust (1.48)| [iron](https://ironframework.io) (0.6) | 17 062.02 | 16 896.37 | 16 457.58 |
| 132 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 16 581.84 | 16 185.71 | 15 839.75 |
| 133 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 16 399.76 | 19 358.63 | 19 932.34 |
| 134 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.38) | 16 296.76 | 15 963.62 | 15 587.96 |
| 135 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 16 079.65 | 14 236.09 | 13 335.70 |
| 136 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.61) | 16 050.41 | 20 614.01 | 20 996.43 |
| 137 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 728.47 | 18 525.65 | 18 262.43 |
| 138 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 15 515.77 | 14 990.22 | 14 777.63 |
| 139 | javascript (14.15)| [feathersjs](https://feathersjs.com) (4.5) | 15 502.39 | 17 568.94 | 17 381.34 |
| 140 | php (7.4)| [slim-swoole](https://slimframework.com) (4.5) | 15 475.34 | 18 101.32 | 18 042.42 |
| 141 | go (1.15)| [macaron](https://go-macaron.com) (1.4) | 15 340.78 | 16 936.31 | 16 981.04 |
| 142 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 14 997.60 | 19 269.43 | 19 157.25 |
| 143 | javascript (14.15)| [express](https://expressjs.com) (4.17) | 14 656.39 | 15 964.93 | 17 463.31 |
| 144 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 14 231.96 | 13 774.81 | 13 592.20 |
| 145 | java (11)| [struts2](https://struts.apache.org) (2.5) | 14 067.61 | 14 362.53 | 14 426.67 |
| 146 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 13 800.06 | 13 391.62 | 13 236.54 |
| 147 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 13 330.08 | 14 084.56 | 21 066.83 |
| 148 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 13 298.27 | 12 857.68 | 12 643.32 |
| 149 | java (11)| [blade](https://lets-blade.com) (2.0) | 13 004.39 | 15 831.65 | 14 939.37 |
| 150 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 12 459.82 | 15 475.95 | 13 795.43 |
| 151 | javascript (14.15)| [nestjs-express](https://nestjs.com) (7.4) | 11 677.69 | 14 334.29 | 12 676.04 |
| 152 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 381.99 | 11 682.05 | 11 709.13 |
| 153 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 11 328.28 | 11 116.15 | 10 730.11 |
| 154 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 10 177.40 | 10 410.90 | 10 552.14 |
| 155 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.4) | 10 176.44 | 10 470.90 | 10 576.13 |
| 156 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 852.09 | 10 047.50 | 10 010.64 |
| 157 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.5) | 9 829.82 | 10 088.00 | 10 207.93 |
| 158 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.6) | 9 615.95 | 9 757.41 | 9 768.77 |
| 159 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 373.45 | 9 372.17 | 9 173.04 |
| 160 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.1) | 9 310.15 | 9 036.67 | 8 994.84 |
| 161 | ruby (2.7)| [grape](https://ruby-grape.org) (1.5) | 8 675.97 | 8 379.68 | 8 340.98 |
| 162 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 8 117.43 | 13 484.74 | 12 598.12 |
| 163 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.13) | 7 194.46 | 6 984.31 | 6 593.81 |
| 164 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 6 769.85 | 6 696.56 | 6 625.12 |
| 165 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 641.97 | 6 566.94 | 6 503.15 |
| 166 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 6 567.16 | 6 507.76 | 6 371.62 |
| 167 | python (3.8)| [tornado](https://tornadoweb.org) (6.1) | 6 493.63 | 6 771.57 | 6 372.01 |
| 168 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 6 420.08 | 6 367.64 | 6 188.83 |
| 169 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 262.49 | 6 199.35 | 6 130.42 |
| 170 | python (3.8)| [django](https://djangoproject.com) (3.1) | 6 067.88 | 6 058.65 | 5 809.36 |
| 171 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 5 760.09 | 5 635.40 | 5 641.39 |
| 172 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 5 429.65 | 6 378.46 | 6 603.15 |
| 173 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 5 231.25 | 5 143.47 | 5 289.30 |
| 174 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 4 881.64 | 4 700.48 | 4 423.49 |
| 175 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 4 716.92 | 4 666.34 | 4 604.94 |
| 176 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 477.07 | 4 419.86 | 4 368.36 |
| 177 | php (7.4)| [slim](https://slimframework.com) (4.6) | 4 475.37 | 4 454.03 | 4 433.66 |
| 178 | javascript (14.15)| [sails](https://sailsjs.com) (1.4) | 4 231.44 | 4 216.35 | 4 169.21 |
| 179 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 4 144.09 | 4 129.86 | 4 157.60 |
| 180 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.2) | 4 010.61 | 3 955.05 | 3 945.69 |
| 181 | php (7.4)| [lumen](https://lumen.laravel.com) (8.1) | 3 868.70 | 3 873.73 | 3 895.89 |
| 182 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.4) | 3 832.36 | 3 770.90 | 3 776.36 |
| 183 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 3 558.23 | 3 519.42 | 3 504.23 |
| 184 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 3 237.75 | 2 049.31 | 2 258.23 |
| 185 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 2 780.86 | 2 753.75 | 2 773.34 |
| 186 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 635.80 | 2 630.26 | 2 639.40 |
| 187 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 462.15 | 2 562.12 | 2 540.65 |
| 188 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.4) | 2 345.59 | 2 342.94 | 2 350.64 |
| 189 | php (7.4)| [symfony](https://symfony.com) (5.1) | 2 084.71 | 2 099.45 | 2 111.07 |
| 190 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 1 949.71 | 6 970.17 | 5 213.93 |
| 191 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 1 935.36 | 1 925.72 | 1 916.62 |
| 192 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 931.58 | 1 851.74 | 1 766.21 |
| 193 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 919.68 | 1 824.04 | 1 825.26 |
| 194 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 1 869.51 | 1 873.98 | 1 875.40 |
| 195 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 1 640.13 | 1 647.70 | 1 626.92 |
| 196 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 482.49 | 1 505.22 | 1 480.35 |
| 197 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 467.38 | 494.39 | 444.63 |
| 198 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 441.80 | 1 461.78 | 1 464.07 |
| 199 | python (3.8)| [django-ninja](https://django-ninja.rest-framework.com) (0.9) | 1 110.28 | 1 452.66 | 1 477.39 |
| 200 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 108.10 | 1 127.50 | 1 123.39 |
| 201 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 047.39 | 1 019.01 | 1 003.06 |
| 202 | crystal (0.35)| [lucky](https://luckyframework.org) (0.23) | 432.63 | 443.51 | 425.06 |
| 203 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 288.73 | 302.14 | -92.43 |
| 204 | php (7.4)| [laravel](https://laravel.com) (7.27) | 119.70 | 27.38 | -0.00 |
| 205 | swift (5.3)| [kitura](https://kitura.io) (2.9) | 0.00 | 0.00 | 0.00 |
| 206 | java (11)| [spring-framework](https://spring.io/projects/spring-framework) (5.3) | 0.00 | 0.00 | 0.00 |
| 207 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 0.00 | 0.00 | 0.00 |
| 208 | nim (1.4)| [basolato](https://github.com/itsumura-h/nim-basolato) (0.7) | 0.00 | 0.00 | 0.00 |

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

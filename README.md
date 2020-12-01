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

:information_source:  Updated on **2020-12-01** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 149 789.83 | 182 209.63 | 183 268.18 |
| 2 | go (1.15)| [fiber](https://gofiber.io) (2.2) | 122 289.56 | 129 620.89 | 128 668.99 |
| 3 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 120 938.53 | 122 803.31 | 122 553.43 |
| 4 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 118 117.13 | 146 993.69 | 150 150.47 |
| 5 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.5) | 118 004.46 | 129 164.83 | 128 572.71 |
| 6 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.17) | 116 558.83 | 130 045.64 | 130 206.51 |
| 7 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 116 356.96 | 126 118.85 | 122 742.81 |
| 8 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 116 026.33 | 128 229.18 | 127 359.43 |
| 9 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 115 881.27 | 134 234.12 | 135 473.12 |
| 10 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 115 157.73 | 142 557.38 | 145 757.57 |
| 11 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 112 605.33 | 111 500.86 | 114 989.40 |
| 12 | kotlin (1.4)| [kooby](https://jooby.io) (2.8) | 110 807.43 | 136 977.93 | 141 758.58 |
| 13 | java (11)| [jooby](https://jooby.io) (2.8) | 110 540.98 | 138 804.03 | 143 364.10 |
| 14 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 109 885.03 | 135 760.17 | 138 472.71 |
| 15 | javascript (14.15)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 109 219.57 | 138 795.82 | 141 700.98 |
| 16 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 108 519.80 | 134 415.06 | 138 501.89 |
| 17 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 108 334.03 | 104 863.54 | 109 300.20 |
| 18 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 106 778.09 | 132 699.23 | 136 506.86 |
| 19 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.4) | 100 707.17 | 120 142.16 | 120 707.87 |
| 20 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 99 399.11 | 121 358.89 | 121 527.76 |
| 21 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 98 170.32 | 120 718.76 | 120 689.46 |
| 22 | java (11)| [act](https://actframework.org) (1.9) | 95 524.88 | 117 490.07 | 120 164.44 |
| 23 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 95 160.95 | 138 900.02 | 149 448.40 |
| 24 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 94 461.44 | 114 861.02 | 114 477.71 |
| 25 | crystal (0.35)| [kemal](https://kemalcr.com) (0.27) | 92 768.29 | 113 105.71 | 112 828.75 |
| 26 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 88 284.24 | 104 839.29 | 104 603.90 |
| 27 | javascript (14.15)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 83 896.58 | 97 736.23 | 99 811.43 |
| 28 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.1) | 83 450.60 | 81 001.25 | 89 460.65 |
| 29 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.11) | 83 216.97 | 92 896.67 | 89 947.98 |
| 30 | go (1.15)| [gf](https://goframe.org) (1.14) | 82 385.79 | 89 170.35 | 91 423.86 |
| 31 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 81 677.47 | 96 393.95 | 95 106.29 |
| 32 | go (1.15)| [clevergo](https://clevergo.tech) (0.3) | 80 788.37 | 81 956.76 | 83 878.57 |
| 33 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 80 569.63 | 81 613.23 | 83 671.77 |
| 34 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 80 039.76 | 80 944.47 | 82 956.68 |
| 35 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 79 516.96 | 80 365.84 | 82 127.28 |
| 36 | c (11)| [kore](https://kore.io) (3.3) | 79 429.22 | 190 683.30 | 179 608.09 |
| 37 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 177.01 | 82 679.76 | 83 862.27 |
| 38 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 77 719.23 | 81 554.03 | 83 152.67 |
| 39 | go (1.15)| [chi](https://github.com/go-chi/chi) (4.1) | 76 965.39 | 75 961.59 | 78 583.21 |
| 40 | go (1.15)| [violetear](https://violetear.org) (7.0) | 76 798.22 | 76 905.73 | 79 044.58 |
| 41 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 76 372.19 | 113 043.03 | 129 758.97 |
| 42 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 74 949.29 | 78 841.58 | 79 601.85 |
| 43 | fsharp (5.0)| [falco](https://github.com/pimbrouwers/Falco) (3.0) | 74 916.25 | 87 967.97 | 91 096.81 |
| 44 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 74 872.79 | 134 894.50 | 118 036.11 |
| 45 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 74 866.86 | 74 186.57 | 76 416.52 |
| 46 | java (11)| [restheart](https://restheart.org) (5.1) | 73 962.25 | 76 680.26 | 77 611.09 |
| 47 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 73 351.33 | 71 624.27 | 74 347.76 |
| 48 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 72 667.59 | 84 249.60 | 86 837.71 |
| 49 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 72 047.27 | 83 226.40 | 85 819.99 |
| 50 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 71 661.59 | 80 450.02 | 81 563.87 |
| 51 | go (1.15)| [beego](https://beego.me) (1.12) | 71 229.16 | 74 468.19 | 76 365.15 |
| 52 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 65 498.32 | 73 085.98 | 73 500.00 |
| 53 | go (1.15)| [air](https://github.com/aofei/air) (0.20) | 64 425.90 | 63 071.80 | 65 461.27 |
| 54 | javascript (14.15)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 63 360.87 | 68 345.62 | 70 091.69 |
| 55 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.4) | 62 241.61 | 62 061.90 | 64 824.54 |
| 56 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 62 118.09 | 67 842.90 | 67 482.12 |
| 57 | rust (1.48)| [actix](https://actix.rs) (3.3) | 60 165.35 | 58 062.56 | 62 795.28 |
| 58 | javascript (14.15)| [0http](https://github.com/jkyberneees/0http) (3.0) | 59 647.70 | 66 052.41 | 67 504.29 |
| 59 | javascript (14.15)| [polka](https://github.com/lukeed/polka) (0.5) | 58 742.82 | 62 545.66 | 61 929.74 |
| 60 | java (11)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 58 518.45 | 62 387.09 | 62 817.99 |
| 61 | javascript (14.15)| [restana](https://github.com/jkyberneees/ana) (4.7) | 56 650.56 | 62 540.28 | 63 380.16 |
| 62 | javascript (14.15)| [rayo](https://rayo.js.org) (1.3) | 56 067.80 | 60 407.33 | 59 245.69 |
| 63 | javascript (14.15)| [fastify](https://fastify.io) (3.7) | 55 313.48 | 59 729.26 | 59 429.95 |
| 64 | java (11)| [javalin](https://javalin.io) (3.9) | 54 459.89 | 57 424.51 | 58 270.30 |
| 65 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 53 745.50 | 57 065.62 | 55 259.76 |
| 66 | javascript (14.15)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 53 679.29 | 58 668.25 | 57 348.75 |
| 67 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.14) | 53 280.85 | 73 962.12 | 82 881.52 |
| 68 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 53 266.86 | 61 618.94 | 64 391.08 |
| 69 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 52 300.99 | 57 314.73 | 57 455.72 |
| 70 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 52 117.27 | 62 917.43 | 68 820.61 |
| 71 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 51 840.72 | 57 190.42 | 64 323.67 |
| 72 | php (7.4)| [hyperf](https://hyperf.io) (2.0) | 51 051.21 | 60 996.08 | 65 473.10 |
| 73 | fsharp (5.0)| [websharper](https://websharper.com) (4.6) | 50 991.06 | 57 819.79 | 57 851.95 |
| 74 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 50 821.35 | 67 178.28 | 69 582.44 |
| 75 | javascript (14.15)| [nestjs-fastify](https://nestjs.com) (7.4) | 48 484.22 | 52 109.11 | 50 814.32 |
| 76 | java (11)| [spring-framework](https://spring.io/projects/spring-framework) (5.3) | 46 208.32 | 49 958.18 | 51 259.75 |
| 77 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 45 961.54 | 50 960.56 | 52 489.09 |
| 78 | javascript (14.15)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 45 822.51 | 48 774.22 | 47 767.99 |
| 79 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 45 759.86 | 49 160.83 | 51 627.76 |
| 80 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 45 539.23 | 46 489.81 | 49 306.73 |
| 81 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.8) | 45 121.60 | 48 872.07 | 49 791.75 |
| 82 | java (11)| [micronaut](https://micronaut.io) (1.2) | 45 017.97 | 50 733.71 | 51 113.85 |
| 83 | javascript (14.15)| [foxify](https://foxify.js.org) (0.1) | 44 632.83 | 48 809.15 | 48 283.64 |
| 84 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 522.21 | 45 731.96 | 45 733.87 |
| 85 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 43 485.14 | 33 194.75 | 34 145.07 |
| 86 | javascript (14.15)| [koa](https://koajs.com) (2.13) | 42 110.47 | 44 021.29 | 42 938.52 |
| 87 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 42 040.96 | 45 683.70 | 46 427.42 |
| 88 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 40 502.77 | 43 245.59 | 43 979.19 |
| 89 | javascript (14.15)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 37 214.36 | 39 648.27 | 39 124.24 |
| 90 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 36 346.21 | 37 205.85 | 37 813.03 |
| 91 | swift (5.3)| [vapor](https://vapor.codes) (4.36) | 36 249.33 | 37 688.43 | 37 544.58 |
| 92 | swift (5.3)| [kitura-nio](https://kitura.io) (2.9) | 35 708.26 | 35 328.18 | 35 200.28 |
| 93 | javascript (14.15)| [moleculer](https://moleculer.services) (0.14) | 35 640.52 | 36 679.48 | 34 937.95 |
| 94 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 35 196.10 | 49 947.98 | 52 635.66 |
| 95 | rust (1.48)| [nickel](https://nickel-org.github.io) (0.11) | 35 128.98 | 33 879.93 | 35 446.99 |
| 96 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 34 370.12 | 37 846.17 | 39 365.75 |
| 97 | javascript (14.15)| [hapi](https://hapijs.com) (20.0) | 34 313.25 | 35 767.22 | 34 304.66 |
| 98 | fsharp (5.0)| [suave](https://suave.io) (2.5) | 33 646.29 | 31 040.52 | 26 630.01 |
| 99 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 32 956.47 | 37 779.34 | 37 317.20 |
| 100 | python (3.8)| [emmett](https://emmett.sh) (2.1) | 32 944.32 | 39 998.99 | 41 310.28 |
| 101 | python (3.8)| [hug](https://hug.rest) (2.6) | 32 559.72 | 34 686.75 | 52 368.21 |
| 102 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 31 724.93 | 31 471.85 | 31 044.32 |
| 103 | javascript (14.15)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 31 604.05 | 32 665.93 | 49 324.49 |
| 104 | rust (1.48)| [gotham](https://gotham.rs) (0.4) | 30 717.26 | 34 058.12 | 34 898.36 |
| 105 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.9) | 30 464.19 | 33 514.45 | 34 240.58 |
| 106 | php (7.4)| [imi](https://imiphp.com) (1.2) | 29 683.35 | 34 005.37 | 35 058.92 |
| 107 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 29 280.86 | 33 904.13 | 34 502.75 |
| 108 | javascript (14.15)| [restify](https://restify.com) (8.5) | 29 141.68 | 30 726.25 | 29 917.09 |
| 109 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 28 940.30 | 31 998.92 | 31 929.55 |
| 110 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 27 951.78 | 28 897.53 | 29 441.30 |
| 111 | python (3.8)| [starlette](https://starlette.io) (0.14) | 27 864.61 | 31 790.75 | 32 345.02 |
| 112 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.10) | 27 274.56 | 29 322.88 | 28 802.10 |
| 113 | scala (2.13)| [play](https://playframework.com) (2.8) | 27 102.43 | 28 738.79 | 28 791.92 |
| 114 | javascript (14.15)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 26 791.42 | 29 718.36 | 28 707.12 |
| 115 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 25 135.03 | 24 687.54 | 22 702.48 |
| 116 | python (3.8)| [index.py](https://index-py.abersheeran.com) (0.15) | 25 061.68 | 29 390.69 | 29 618.67 |
| 117 | python (3.8)| [responder](https://python-responder.org) (2.0) | 25 059.04 | 32 131.61 | 32 088.87 |
| 118 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 23 875.61 | 26 025.80 | 25 266.79 |
| 119 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 23 479.19 | 21 364.19 | 20 772.96 |
| 120 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 22 808.74 | 28 084.55 | 27 939.68 |
| 121 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 22 271.55 | 21 830.20 | 21 735.47 |
| 122 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.16) | 21 869.10 | 21 114.50 | 19 728.81 |
| 123 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 21 622.77 | 19 630.40 | 19 928.28 |
| 124 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 20 767.26 | 20 063.10 | 20 650.15 |
| 125 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 19 327.65 | 23 050.60 | 23 402.20 |
| 126 | javascript (14.15)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 19 091.37 | 18 257.73 | 17 053.67 |
| 127 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 18 671.47 | 21 070.18 | 20 870.45 |
| 128 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 18 509.26 | 22 527.45 | 23 943.37 |
| 129 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 614.02 | 15 708.63 | 14 780.80 |
| 130 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 17 194.43 | 16 729.02 | 16 335.61 |
| 131 | php (7.4)| [swoft](https://swoft.org) (2.0) | 17 187.18 | 20 453.14 | 21 272.08 |
| 132 | rust (1.48)| [iron](https://ironframework.io) (0.6) | 16 826.45 | 16 516.32 | 16 615.67 |
| 133 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.62) | 16 706.45 | 20 100.66 | 20 883.65 |
| 134 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 16 439.03 | 15 872.11 | 15 836.99 |
| 135 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 16 349.13 | 19 330.63 | 19 900.72 |
| 136 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 16 318.06 | 19 918.53 | 19 544.96 |
| 137 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 16 106.65 | 14 308.43 | 13 288.59 |
| 138 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.38) | 16 019.94 | 15 765.37 | 15 310.40 |
| 139 | swift (5.3)| [kitura](https://kitura.io) (2.9) | 16 011.39 | 28 084.93 | 24 074.18 |
| 140 | javascript (14.15)| [express](https://expressjs.com) (4.17) | 15 576.13 | 18 175.25 | 17 986.70 |
| 141 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 518.65 | 18 053.67 | 18 561.69 |
| 142 | php (7.4)| [slim-swoole](https://slimframework.com) (4.5) | 15 498.26 | 17 662.11 | 18 042.20 |
| 143 | javascript (14.15)| [feathersjs](https://feathersjs.com) (4.5) | 15 477.29 | 17 583.60 | 15 308.65 |
| 144 | go (1.15)| [macaron](https://go-macaron.com) (1.4) | 15 299.74 | 16 791.46 | 16 822.19 |
| 145 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 15 293.73 | 14 812.97 | 14 612.65 |
| 146 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 14 137.23 | 13 743.31 | 13 536.86 |
| 147 | java (11)| [struts2](https://struts.apache.org) (2.5) | 13 935.79 | 14 205.92 | 14 213.43 |
| 148 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 13 800.98 | 13 362.12 | 13 179.09 |
| 149 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 13 575.23 | 15 305.91 | 20 702.55 |
| 150 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 13 406.83 | 17 398.80 | 17 345.40 |
| 151 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 13 217.06 | 12 827.58 | 12 596.48 |
| 152 | java (11)| [blade](https://lets-blade.com) (2.0) | 12 935.19 | 16 243.88 | 14 674.57 |
| 153 | javascript (14.15)| [nestjs-express](https://nestjs.com) (7.4) | 11 959.26 | 13 624.46 | 13 967.30 |
| 154 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 408.36 | 11 616.05 | 11 648.43 |
| 155 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 10 878.26 | 10 919.93 | 10 614.08 |
| 156 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.4) | 10 206.55 | 10 431.79 | 10 502.90 |
| 157 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 10 155.67 | 10 401.33 | 10 534.06 |
| 158 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.5) | 9 799.95 | 10 009.48 | 10 097.06 |
| 159 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 776.24 | 9 975.25 | 9 904.89 |
| 160 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.6) | 9 648.48 | 9 700.26 | 9 738.28 |
| 161 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 234.97 | 9 515.84 | 9 246.29 |
| 162 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.1) | 9 050.91 | 8 828.81 | 8 780.59 |
| 163 | ruby (2.7)| [grape](https://ruby-grape.org) (1.5) | 8 690.56 | 8 349.86 | 8 321.98 |
| 164 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 7 949.95 | 13 182.75 | 12 395.26 |
| 165 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.13) | 7 513.18 | 7 277.58 | 6 893.45 |
| 166 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 6 715.63 | 6 661.82 | 6 555.08 |
| 167 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 621.14 | 6 561.39 | 6 478.58 |
| 168 | python (3.8)| [tornado](https://tornadoweb.org) (6.1) | 6 588.01 | 6 528.12 | 6 286.81 |
| 169 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 6 583.84 | 6 507.86 | 6 396.44 |
| 170 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 6 392.57 | 6 329.13 | 6 236.23 |
| 171 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 230.30 | 6 149.96 | 6 130.72 |
| 172 | python (3.8)| [django](https://djangoproject.com) (3.1) | 6 020.34 | 6 308.51 | 5 860.22 |
| 173 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 5 754.15 | 5 627.61 | 5 651.48 |
| 174 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 5 706.71 | 6 350.11 | 6 558.82 |
| 175 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 5 364.44 | 5 319.28 | 5 228.00 |
| 176 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 5 213.55 | 5 177.95 | 5 212.72 |
| 177 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 4 991.10 | 4 854.96 | 4 586.21 |
| 178 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 4 713.61 | 4 686.27 | 4 600.37 |
| 179 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 486.14 | 4 439.97 | 4 371.66 |
| 180 | php (7.4)| [slim](https://slimframework.com) (4.6) | 4 458.00 | 4 431.40 | 4 413.71 |
| 181 | javascript (14.15)| [sails](https://sailsjs.com) (1.4) | 4 213.05 | 4 246.22 | 4 171.89 |
| 182 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 4 139.26 | 4 097.42 | 4 130.23 |
| 183 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.2) | 3 998.24 | 3 927.25 | 3 917.49 |
| 184 | php (7.4)| [lumen](https://lumen.laravel.com) (8.1) | 3 847.95 | 3 837.07 | 3 873.75 |
| 185 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.4) | 3 822.75 | 3 777.17 | 3 752.34 |
| 186 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 3 559.14 | 2 165.32 | 3 217.70 |
| 187 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 3 532.94 | 3 492.11 | 3 479.01 |
| 188 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 062.15 | 6 972.76 | 5 075.39 |
| 189 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 2 859.44 | 2 853.47 | 2 851.73 |
| 190 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 631.56 | 2 631.63 | 2 632.79 |
| 191 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 460.12 | 2 537.63 | 2 519.20 |
| 192 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.4) | 2 341.42 | 2 358.28 | 2 367.38 |
| 193 | php (7.4)| [symfony](https://symfony.com) (5.1) | 2 081.95 | 2 094.93 | 2 101.17 |
| 194 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 927.38 | 1 839.80 | 1 821.76 |
| 195 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 904.47 | 1 847.05 | 1 769.47 |
| 196 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 1 873.30 | 1 882.43 | 1 888.18 |
| 197 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 1 808.92 | 1 844.88 | 1 838.96 |
| 198 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 1 665.58 | 1 649.44 | 1 625.44 |
| 199 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 595.04 | 1 603.40 | 1 574.68 |
| 200 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 546.67 | 628.78 | 236.22 |
| 201 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 485.38 | 1 504.12 | 1 501.78 |
| 202 | python (3.8)| [django-ninja](https://django-ninja.rest-framework.com) (0.9) | 1 241.16 | 1 621.05 | 1 645.98 |
| 203 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 111.70 | 1 131.75 | 1 125.64 |
| 204 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 042.65 | 1 015.05 | 1 005.19 |
| 205 | crystal (0.35)| [lucky](https://luckyframework.org) (0.23) | 431.74 | 443.80 | 420.40 |
| 206 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 286.74 | 301.39 | -96.35 |
| 207 | php (7.4)| [laravel](https://laravel.com) (7.27) | 115.94 | 51.88 | 0.00 |

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

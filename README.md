# Which is the fastest?

[![Build Status](https://the-benchmarker.semaphoreci.com/badges/web-frameworks/branches/master.svg)](https://the-benchmarker.semaphoreci.com/projects/web-frameworks)

[![Chat with US](https://img.shields.io/badge/slack-Chat_with_us-blueviolet)](https://thebenchmarker.slack.com)

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

:information_source:  Updated on **2020-12-31** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 147 451.93 | 182 575.83 | 183 851.83 |
| 2 | c (11)| [kore](https://kore.io) (3.3) | 124 261.29 | 180 953.13 | 192 971.01 |
| 3 | go (1.15)| [fiber](https://gofiber.io) (2.3) | 122 518.64 | 128 757.28 | 128 160.51 |
| 4 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 121 661.05 | 136 016.07 | 137 418.50 |
| 5 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 121 299.27 | 124 142.50 | 123 436.84 |
| 6 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 118 372.28 | 145 923.51 | 148 274.14 |
| 7 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.5) | 117 299.32 | 128 440.81 | 127 583.95 |
| 8 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 117 292.67 | 127 540.75 | 126 788.24 |
| 9 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 115 953.52 | 128 334.19 | 127 305.28 |
| 10 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.18) | 115 869.23 | 129 508.32 | 129 526.24 |
| 11 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 114 127.71 | 141 913.35 | 144 472.01 |
| 12 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 112 175.17 | 111 702.45 | 114 678.32 |
| 13 | javascript (14.15)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.4) | 110 903.88 | 142 407.07 | 145 160.69 |
| 14 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 110 355.51 | 135 863.56 | 141 464.00 |
| 15 | java (11)| [jooby](https://jooby.io) (2.9) | 110 265.82 | 137 219.94 | 142 590.52 |
| 16 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 109 283.01 | 135 245.05 | 137 288.90 |
| 17 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 107 898.02 | 104 214.01 | 108 820.64 |
| 18 | javascript (14.15)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 107 646.22 | 137 630.48 | 140 672.68 |
| 19 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 107 462.25 | 134 132.04 | 137 868.32 |
| 20 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 105 789.85 | 131 820.87 | 135 015.16 |
| 21 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 99 204.93 | 120 845.36 | 121 241.47 |
| 22 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 99 086.90 | 120 403.49 | 121 261.19 |
| 23 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.4) | 96 967.86 | 119 899.46 | 119 944.97 |
| 24 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 96 240.03 | 140 192.18 | 151 693.23 |
| 25 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 95 174.57 | 113 273.67 | 112 915.21 |
| 26 | crystal (0.35)| [kemal](https://kemalcr.com) (0.27) | 94 460.60 | 113 322.29 | 113 044.66 |
| 27 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 93 785.02 | 114 674.96 | 117 456.73 |
| 28 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 89 854.70 | 106 867.39 | 105 916.18 |
| 29 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.12) | 81 738.71 | 91 252.86 | 86 979.94 |
| 30 | javascript (14.15)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 81 381.73 | 97 271.06 | 99 650.13 |
| 31 | go (1.15)| [gf](https://goframe.org) (1.14) | 81 048.09 | 88 625.01 | 91 046.89 |
| 32 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 80 755.08 | 81 727.40 | 83 600.62 |
| 33 | go (1.15)| [clevergo](https://clevergo.tech) (0.5) | 80 582.14 | 81 876.08 | 83 558.90 |
| 34 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 79 911.61 | 94 661.53 | 91 029.13 |
| 35 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 79 833.25 | 80 700.27 | 82 790.80 |
| 36 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 79 506.85 | 80 017.49 | 82 081.88 |
| 37 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 78 201.97 | 116 884.73 | 131 101.45 |
| 38 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 163.93 | 82 318.74 | 83 989.29 |
| 39 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 77 734.44 | 81 533.96 | 83 046.20 |
| 40 | go (1.15)| [chi](https://github.com/go-chi/chi) (1.5) | 77 172.87 | 76 654.72 | 78 979.90 |
| 41 | go (1.15)| [violetear](https://violetear.org) (7.0) | 76 803.62 | 76 815.45 | 79 114.00 |
| 42 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 74 950.58 | 78 950.59 | 79 447.64 |
| 43 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.2) | 74 872.34 | 70 884.65 | 74 075.04 |
| 44 | fsharp (5.0)| [falco](https://github.com/pimbrouwers/Falco) (3.0) | 74 842.68 | 87 591.27 | 90 282.09 |
| 45 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 74 815.78 | 74 249.26 | 76 342.35 |
| 46 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 74 666.82 | 87 538.84 | 90 185.06 |
| 47 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 73 559.64 | 81 202.58 | 82 615.60 |
| 48 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 73 280.80 | 71 373.38 | 74 049.31 |
| 49 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 72 124.22 | 83 828.11 | 86 150.00 |
| 50 | go (1.15)| [beego](https://beego.me) (1.12) | 71 582.17 | 74 816.34 | 76 675.09 |
| 51 | javascript (14.15)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 70 986.19 | 78 884.90 | 79 811.28 |
| 52 | java (11)| [restheart](https://restheart.org) (5.1) | 70 724.56 | 73 125.07 | 74 180.53 |
| 53 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 64 688.78 | 71 666.16 | 72 129.69 |
| 54 | go (1.15)| [air](https://github.com/aofei/air) (0.21) | 64 390.63 | 62 795.83 | 65 518.82 |
| 55 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.6) | 62 342.08 | 62 086.69 | 64 867.70 |
| 56 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 62 214.09 | 68 222.13 | 67 534.42 |
| 57 | javascript (14.15)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 61 557.80 | 68 735.38 | 69 561.35 |
| 58 | rust (1.48)| [actix](https://actix.rs) (3.3) | 61 179.83 | 58 015.42 | 61 963.24 |
| 59 | javascript (14.15)| [0http](https://github.com/jkyberneees/0http) (3.0) | 60 228.05 | 66 343.78 | 67 584.94 |
| 60 | javascript (14.15)| [polka](https://github.com/lukeed/polka) (0.5) | 57 764.78 | 62 081.58 | 61 011.14 |
| 61 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 57 227.96 | 60 827.76 | 61 637.75 |
| 62 | javascript (14.15)| [rayo](https://rayo.js.org) (1.3) | 56 644.81 | 61 283.33 | 59 845.92 |
| 63 | javascript (14.15)| [restana](https://github.com/jkyberneees/ana) (4.7) | 56 412.27 | 62 929.01 | 63 977.07 |
| 64 | php (7.4)| [one](https://github.com/lizhichao/one) (2.2) | 55 495.61 | 61 068.37 | 70 564.20 |
| 65 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 54 232.35 | 57 171.70 | 55 274.29 |
| 66 | javascript (14.15)| [fastify](https://fastify.io) (3.7) | 53 952.93 | 58 888.63 | 58 066.56 |
| 67 | java (11)| [javalin](https://javalin.io) (3.9) | 53 599.89 | 56 957.42 | 57 762.39 |
| 68 | javascript (14.15)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 53 260.59 | 57 696.92 | 56 511.63 |
| 69 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 52 775.67 | 61 229.68 | 63 456.13 |
| 70 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 52 203.73 | 66 179.51 | 68 351.99 |
| 71 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 51 973.06 | 57 655.78 | 58 016.64 |
| 72 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 51 610.67 | 56 841.97 | 63 918.40 |
| 73 | php (7.4)| [hyperf](https://hyperf.io) (2.1) | 51 339.09 | 61 584.15 | 63 621.14 |
| 74 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.14) | 50 697.18 | 73 761.31 | 81 838.02 |
| 75 | java (11)| [micronaut](https://micronaut.io) (1.2) | 50 546.93 | 56 804.82 | 56 723.26 |
| 76 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 49 808.92 | 56 289.43 | 56 671.62 |
| 77 | javascript (14.15)| [nestjs-fastify](https://nestjs.com) (7.4) | 48 322.34 | 52 179.96 | 50 946.50 |
| 78 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 46 242.54 | 49 074.11 | 51 562.48 |
| 79 | javascript (14.15)| [foxify](https://foxify.js.org) (0.1) | 46 052.22 | 49 041.57 | 47 999.83 |
| 80 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 46 036.68 | 46 460.45 | 49 049.84 |
| 81 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 45 799.59 | 51 127.36 | 52 569.76 |
| 82 | javascript (14.15)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 45 151.89 | 49 162.15 | 47 810.48 |
| 83 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.8) | 44 722.70 | 48 491.99 | 49 603.47 |
| 84 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 43 822.19 | 44 750.66 | 44 641.35 |
| 85 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 43 192.45 | 32 709.63 | 31 263.25 |
| 86 | javascript (14.15)| [koa](https://koajs.com) (2.13) | 41 915.87 | 44 501.34 | 43 245.93 |
| 87 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 41 888.73 | 45 115.73 | 46 546.02 |
| 88 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 40 331.51 | 134 986.16 | 116 078.83 |
| 89 | javascript (14.15)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 37 417.11 | 39 583.07 | 38 990.21 |
| 90 | rust (1.48)| [nickel](https://nickel-org.github.io) (0.11) | 37 401.89 | 35 893.93 | 37 075.06 |
| 91 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 36 496.01 | 37 761.70 | 38 016.51 |
| 92 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 36 433.22 | 36 310.23 | 36 535.64 |
| 93 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 35 660.13 | 35 118.36 | 34 964.46 |
| 94 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 35 621.30 | 37 813.01 | 38 059.90 |
| 95 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 35 573.44 | 50 201.12 | 52 971.98 |
| 96 | swift (5.3)| [vapor](https://vapor.codes) (4.36) | 35 456.05 | 37 544.86 | 37 207.66 |
| 97 | javascript (14.15)| [moleculer](https://moleculer.services) (0.14) | 35 255.45 | 36 038.89 | 35 627.18 |
| 98 | javascript (14.15)| [hapi](https://hapijs.com) (20.0) | 34 755.75 | 35 209.07 | 34 466.74 |
| 99 | python (3.8)| [emmett](https://emmett.sh) (2.1) | 34 074.80 | 40 762.30 | 41 389.21 |
| 100 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 33 943.06 | 38 201.61 | 39 002.15 |
| 101 | python (3.8)| [hug](https://hug.rest) (2.6) | 32 978.44 | 35 298.10 | 35 219.63 |
| 102 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 32 911.08 | 27 877.88 | 23 518.56 |
| 103 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 32 670.57 | 37 049.19 | 37 215.73 |
| 104 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 31 738.33 | 31 549.88 | 31 367.82 |
| 105 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.12) | 30 955.10 | 35 010.24 | 35 691.24 |
| 106 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.3) | 30 266.22 | 33 620.19 | 34 393.57 |
| 107 | python (3.8)| [responder](https://python-responder.org) (2.0) | 29 384.23 | 32 040.02 | 32 473.79 |
| 108 | php (7.4)| [imi](https://imiphp.com) (1.2) | 29 144.71 | 32 923.07 | 34 718.61 |
| 109 | javascript (14.15)| [restify](https://restify.com) (8.5) | 29 104.12 | 30 886.49 | 29 829.70 |
| 110 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 29 093.47 | 32 251.10 | 32 134.64 |
| 111 | javascript (14.15)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 28 916.30 | 46 792.05 | 46 611.25 |
| 112 | rust (1.48)| [salvo](https://github.com/kenorld/salvo) (0.3) | 28 196.12 | 29 683.21 | 29 714.28 |
| 113 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 27 638.29 | 29 358.00 | 29 411.24 |
| 114 | python (3.8)| [starlette](https://starlette.io) (0.14) | 27 311.50 | 31 123.56 | 32 186.19 |
| 115 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 26 956.46 | 29 305.06 | 28 542.29 |
| 116 | scala (2.13)| [play](https://playframework.com) (2.8) | 26 360.84 | 27 681.46 | 27 623.99 |
| 117 | javascript (14.15)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 26 255.17 | 28 648.44 | 27 735.91 |
| 118 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 24 649.20 | 25 057.24 | 23 428.48 |
| 119 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 23 707.17 | 25 937.99 | 25 335.91 |
| 120 | python (3.8)| [index.py](https://index-py.abersheeran.com) (0.15) | 23 507.94 | 28 860.46 | 29 426.09 |
| 121 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 23 259.17 | 21 636.08 | 20 880.97 |
| 122 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 22 788.22 | 28 136.11 | 27 909.26 |
| 123 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 21 450.28 | 21 009.12 | 20 825.89 |
| 124 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.16) | 21 363.89 | 21 637.79 | 19 295.18 |
| 125 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 20 050.54 | 19 650.23 | 19 648.30 |
| 126 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 18 992.31 | 20 603.22 | 20 531.35 |
| 127 | javascript (14.15)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 18 887.93 | 18 148.31 | 17 018.54 |
| 128 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 18 473.94 | 23 294.29 | 23 583.55 |
| 129 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.62) | 17 645.87 | 20 704.88 | 21 039.34 |
| 130 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 598.59 | 15 708.22 | 14 605.68 |
| 131 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 17 285.27 | 17 884.61 | 16 272.70 |
| 132 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 17 270.44 | 21 153.36 | 21 006.37 |
| 133 | php (7.4)| [swoft](https://swoft.org) (2.0) | 17 226.45 | 21 063.90 | 21 195.64 |
| 134 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 17 154.14 | 20 097.45 | 20 006.44 |
| 135 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 16 570.16 | 19 277.83 | 19 517.97 |
| 136 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 16 557.39 | 15 995.99 | 15 791.70 |
| 137 | rust (1.48)| [iron](https://ironframework.io) (0.6) | 16 406.88 | 16 578.79 | 16 827.74 |
| 138 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 16 052.68 | 14 143.93 | 13 201.90 |
| 139 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.39) | 16 025.30 | 15 607.86 | 15 240.42 |
| 140 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 15 990.48 | 18 541.96 | 19 009.52 |
| 141 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 15 427.88 | 17 879.99 | 18 170.38 |
| 142 | javascript (14.15)| [express](https://expressjs.com) (4.17) | 15 383.19 | 17 879.41 | 17 680.71 |
| 143 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 341.45 | 17 695.84 | 17 837.96 |
| 144 | go (1.15)| [macaron](https://go-macaron.com) (1.4) | 15 315.82 | 16 887.65 | 16 960.75 |
| 145 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 15 294.50 | 14 976.67 | 14 636.84 |
| 146 | javascript (14.15)| [feathersjs](https://feathersjs.com) (4.5) | 15 087.34 | 17 811.40 | 17 450.39 |
| 147 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 14 182.47 | 13 674.26 | 13 521.40 |
| 148 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 14 181.64 | 17 144.14 | 16 941.23 |
| 149 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 13 818.43 | 13 967.75 | 20 442.54 |
| 150 | java (11)| [struts2](https://struts.apache.org) (2.5) | 13 791.91 | 14 084.19 | 14 063.41 |
| 151 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 13 594.71 | 13 165.40 | 12 971.85 |
| 152 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 13 083.87 | 12 729.23 | 12 527.72 |
| 153 | java (11)| [blade](https://lets-blade.com) (2.0) | 12 898.30 | 15 498.86 | 14 676.47 |
| 154 | javascript (14.15)| [nestjs-express](https://nestjs.com) (7.4) | 11 678.85 | 13 181.47 | 12 761.93 |
| 155 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 451.17 | 11 676.39 | 11 708.39 |
| 156 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 11 362.92 | 11 063.27 | 10 779.21 |
| 157 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 10 219.67 | 9 933.80 | 9 425.04 |
| 158 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.4) | 10 191.54 | 10 464.49 | 10 592.06 |
| 159 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 10 075.04 | 10 257.04 | 10 388.31 |
| 160 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 883.14 | 10 096.02 | 10 057.09 |
| 161 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 9 780.33 | 10 004.38 | 10 100.33 |
| 162 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.6) | 9 563.61 | 9 732.53 | 9 796.89 |
| 163 | python (3.8)| [guillotina](https://guillotina.io) (6.0) | 9 287.64 | 8 881.69 | 8 911.04 |
| 164 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.1) | 8 636.63 | 8 438.02 | 8 421.04 |
| 165 | ruby (2.7)| [grape](https://ruby-grape.org) (1.5) | 8 476.44 | 8 201.01 | 8 149.56 |
| 166 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 7 593.12 | 7 299.31 | 6 876.52 |
| 167 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 7 570.68 | 13 543.85 | 12 553.94 |
| 168 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 6 779.57 | 6 734.20 | 6 639.23 |
| 169 | python (3.8)| [tornado](https://tornadoweb.org) (6.1) | 6 677.27 | 6 785.50 | 6 464.72 |
| 170 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 648.97 | 6 584.26 | 6 507.31 |
| 171 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 6 562.57 | 6 524.73 | 6 390.35 |
| 172 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 6 419.45 | 6 370.15 | 6 203.54 |
| 173 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 250.07 | 6 174.68 | 6 163.88 |
| 174 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 6 066.40 | 6 809.24 | 6 886.55 |
| 175 | python (3.8)| [django](https://djangoproject.com) (3.1) | 6 051.72 | 6 036.34 | 5 677.22 |
| 176 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 5 750.91 | 5 664.98 | 5 668.95 |
| 177 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 5 306.25 | 5 266.87 | 5 189.92 |
| 178 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 5 276.51 | 5 180.85 | 5 205.53 |
| 179 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 4 967.09 | 4 762.06 | 4 376.82 |
| 180 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.2) | 4 721.06 | 4 671.95 | 4 586.10 |
| 181 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 490.36 | 4 441.67 | 4 382.30 |
| 182 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 406.74 | 4 379.53 | 4 401.80 |
| 183 | javascript (14.15)| [sails](https://sailsjs.com) (1.4) | 4 251.80 | 4 250.63 | 4 193.00 |
| 184 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 4 160.92 | 4 138.31 | 4 167.85 |
| 185 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.2) | 4 003.95 | 3 942.29 | 3 938.83 |
| 186 | php (7.4)| [lumen](https://lumen.laravel.com) (8.1) | 3 834.74 | 3 834.26 | 3 872.91 |
| 187 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.4) | 3 814.69 | 3 763.34 | 3 752.83 |
| 188 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 3 537.52 | 3 513.89 | 2 400.02 |
| 189 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 3 464.04 | 3 420.99 | 3 422.32 |
| 190 | ruby (2.7)| [rails](https://rubyonrails.org) (6.1) | 2 827.99 | 2 819.73 | 2 824.74 |
| 191 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 636.39 | 2 621.41 | 2 644.34 |
| 192 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 2 424.50 | 7 052.55 | 5 355.38 |
| 193 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.4) | 2 335.76 | 2 325.63 | 2 344.08 |
| 194 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 117.37 | 2 450.98 | 2 430.18 |
| 195 | php (7.4)| [symfony](https://symfony.com) (5.1) | 2 081.91 | 2 093.61 | 2 096.19 |
| 196 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 1 944.08 | 1 978.18 | 1 940.83 |
| 197 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 924.52 | 1 846.10 | 1 768.13 |
| 198 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 910.23 | 1 848.36 | 1 865.49 |
| 199 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 1 856.10 | 1 847.63 | 1 865.06 |
| 200 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 1 652.37 | 1 648.37 | 1 630.48 |
| 201 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 617.39 | 1 636.62 | 1 614.95 |
| 202 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 496.42 | 1 508.16 | 1 518.60 |
| 203 | python (3.8)| [django-ninja](https://django-ninja.rest-framework.com) (0.9) | 1 348.65 | 1 623.95 | 1 648.35 |
| 204 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 142.08 | 620.54 | 750.64 |
| 205 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 107.48 | 1 124.78 | 1 125.95 |
| 206 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 035.69 | 1 003.55 | 995.47 |
| 207 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 287.14 | 305.36 | -98.62 |
| 208 | php (7.4)| [laravel](https://laravel.com) (7.27) | 122.76 | 20.27 | 0.00 |

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

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

:information_source:  Updated on **2020-12-15** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 150 702.77 | 182 964.21 | 185 245.19 |
| 2 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 122 538.73 | 137 094.68 | 138 150.15 |
| 3 | go (1.15)| [fiber](https://gofiber.io) (2.2) | 122 164.42 | 129 822.56 | 129 239.54 |
| 4 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 121 370.59 | 123 829.61 | 123 730.74 |
| 5 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.5) | 117 829.22 | 129 127.46 | 128 618.50 |
| 6 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 117 701.81 | 146 852.18 | 149 510.60 |
| 7 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 117 171.95 | 128 713.04 | 128 025.31 |
| 8 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.18) | 117 004.22 | 130 406.18 | 130 560.83 |
| 9 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 116 076.03 | 129 011.84 | 128 255.79 |
| 10 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 114 459.95 | 142 444.32 | 145 778.31 |
| 11 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 113 215.14 | 112 449.66 | 115 700.19 |
| 12 | javascript (14.15)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 110 999.29 | 138 583.28 | 142 266.64 |
| 13 | java (11)| [jooby](https://jooby.io) (2.9) | 109 423.42 | 137 313.40 | 142 228.73 |
| 14 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 109 406.24 | 134 030.01 | 137 395.81 |
| 15 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 109 057.52 | 137 449.13 | 141 923.47 |
| 16 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 108 326.83 | 104 548.67 | 109 136.55 |
| 17 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 107 244.75 | 133 247.37 | 137 529.11 |
| 18 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 106 197.70 | 131 554.78 | 135 401.36 |
| 19 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 98 291.39 | 122 121.55 | 121 983.72 |
| 20 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 97 935.76 | 121 259.74 | 121 219.79 |
| 21 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 97 917.01 | 140 599.00 | 151 206.65 |
| 22 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.4) | 97 796.59 | 119 049.15 | 119 021.96 |
| 23 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 95 221.83 | 116 425.68 | 119 038.58 |
| 24 | c (11)| [kore](https://kore.io) (3.3) | 95 162.11 | 166 532.76 | 183 208.69 |
| 25 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 94 453.72 | 114 477.81 | 113 195.80 |
| 26 | crystal (0.35)| [kemal](https://kemalcr.com) (0.27) | 94 112.76 | 112 365.25 | 112 587.44 |
| 27 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 90 855.50 | 106 400.82 | 106 602.91 |
| 28 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.12) | 83 816.02 | 95 200.59 | 91 541.08 |
| 29 | javascript (14.15)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 82 833.95 | 98 817.28 | 100 142.59 |
| 30 | go (1.15)| [gf](https://goframe.org) (1.14) | 82 740.31 | 89 599.45 | 91 772.96 |
| 31 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 82 031.75 | 97 354.93 | 96 381.61 |
| 32 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 81 186.40 | 81 959.40 | 84 136.99 |
| 33 | go (1.15)| [clevergo](https://clevergo.tech) (0.5) | 80 590.16 | 81 824.21 | 83 809.01 |
| 34 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 80 173.07 | 80 991.54 | 83 347.69 |
| 35 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 79 699.09 | 80 579.14 | 82 504.54 |
| 36 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 381.53 | 82 953.61 | 83 962.24 |
| 37 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 77 999.16 | 81 666.02 | 83 350.23 |
| 38 | go (1.15)| [chi](https://github.com/go-chi/chi) (1.5) | 77 383.99 | 76 764.64 | 79 292.56 |
| 39 | go (1.15)| [violetear](https://violetear.org) (7.0) | 77 144.50 | 77 016.66 | 79 302.78 |
| 40 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 75 904.60 | 131 545.48 | 137 291.69 |
| 41 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 75 471.69 | 88 484.80 | 91 408.84 |
| 42 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 75 463.12 | 79 540.96 | 79 878.49 |
| 43 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 75 315.65 | 115 699.43 | 129 702.24 |
| 44 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 75 271.33 | 74 850.16 | 76 926.40 |
| 45 | java (11)| [restheart](https://restheart.org) (5.1) | 74 060.93 | 76 780.29 | 73 478.29 |
| 46 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 73 506.86 | 81 462.19 | 83 093.99 |
| 47 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 73 245.16 | 71 397.16 | 74 202.94 |
| 48 | javascript (14.15)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 72 729.79 | 79 147.35 | 80 785.83 |
| 49 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 72 709.84 | 84 711.52 | 87 352.49 |
| 50 | go (1.15)| [beego](https://beego.me) (1.12) | 72 047.84 | 74 993.60 | 76 862.87 |
| 51 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.2) | 71 963.79 | 71 542.60 | 77 283.87 |
| 52 | fsharp (5.0)| [falco](https://github.com/pimbrouwers/Falco) (3.0) | 71 558.12 | 82 679.93 | 84 687.44 |
| 53 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 65 720.79 | 73 739.59 | 74 015.83 |
| 54 | go (1.15)| [air](https://github.com/aofei/air) (0.21) | 64 791.55 | 63 349.37 | 65 803.28 |
| 55 | rust (1.48)| [actix](https://actix.rs) (3.3) | 63 427.42 | 56 958.50 | 60 039.38 |
| 56 | javascript (14.15)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 63 022.52 | 69 326.53 | 70 268.37 |
| 57 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.4) | 62 889.04 | 62 721.84 | 65 525.05 |
| 58 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 61 602.29 | 67 391.71 | 66 533.37 |
| 59 | java (11)| [spring-boot](https://spring.io/projects/spring-boot) (2.4) | 60 291.27 | 63 691.54 | 64 051.67 |
| 60 | javascript (14.15)| [0http](https://github.com/jkyberneees/0http) (3.0) | 58 495.65 | 65 988.88 | 67 712.62 |
| 61 | javascript (14.15)| [rayo](https://rayo.js.org) (1.3) | 56 321.73 | 59 770.65 | 59 934.49 |
| 62 | javascript (14.15)| [polka](https://github.com/lukeed/polka) (0.5) | 56 244.94 | 62 881.57 | 61 708.15 |
| 63 | javascript (14.15)| [fastify](https://fastify.io) (3.7) | 55 320.03 | 59 873.25 | 59 104.12 |
| 64 | javascript (14.15)| [restana](https://github.com/jkyberneees/ana) (4.7) | 55 247.86 | 62 541.24 | 63 140.25 |
| 65 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 54 332.98 | 57 172.66 | 55 954.92 |
| 66 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 54 108.05 | 63 011.31 | 70 458.60 |
| 67 | javascript (14.15)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 54 098.39 | 58 558.48 | 57 633.22 |
| 68 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.14) | 53 015.58 | 76 285.99 | 83 536.25 |
| 69 | java (11)| [javalin](https://javalin.io) (3.9) | 52 927.72 | 56 799.73 | 57 382.40 |
| 70 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 52 925.40 | 56 965.08 | 57 161.24 |
| 71 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 52 688.97 | 61 243.09 | 63 984.55 |
| 72 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 52 014.65 | 57 234.98 | 64 336.59 |
| 73 | php (7.4)| [hyperf](https://hyperf.io) (2.0) | 51 662.79 | 61 127.27 | 65 754.69 |
| 74 | java (11)| [micronaut](https://micronaut.io) (1.2) | 51 167.87 | 57 630.11 | 57 520.08 |
| 75 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 50 861.81 | 65 667.30 | 68 358.39 |
| 76 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 50 648.45 | 57 550.85 | 57 788.18 |
| 77 | javascript (14.15)| [nestjs-fastify](https://nestjs.com) (7.4) | 48 780.67 | 52 706.66 | 50 807.17 |
| 78 | java (11)| [spring-framework](https://spring.io/projects/spring-framework) (5.3) | 46 328.74 | 49 889.29 | 51 255.21 |
| 79 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 46 007.91 | 46 507.52 | 49 401.59 |
| 80 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 45 758.25 | 49 206.20 | 51 436.61 |
| 81 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 44 707.37 | 50 727.36 | 52 486.54 |
| 82 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 652.20 | 45 577.29 | 45 454.54 |
| 83 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.8) | 44 632.99 | 48 723.87 | 49 542.84 |
| 84 | javascript (14.15)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 43 831.70 | 48 910.81 | 47 961.36 |
| 85 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 43 617.20 | 33 167.52 | 31 743.27 |
| 86 | javascript (14.15)| [koa](https://koajs.com) (2.13) | 42 291.31 | 44 120.34 | 43 602.29 |
| 87 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 42 057.75 | 45 534.82 | 46 304.27 |
| 88 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 40 566.38 | 43 492.77 | 44 330.86 |
| 89 | javascript (14.15)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 37 436.43 | 39 479.77 | 38 713.85 |
| 90 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 36 403.22 | 37 691.34 | 37 787.71 |
| 91 | swift (5.3)| [vapor](https://vapor.codes) (4.36) | 36 123.13 | 37 773.20 | 37 668.81 |
| 92 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 35 468.86 | 49 414.36 | 52 168.91 |
| 93 | javascript (14.15)| [hapi](https://hapijs.com) (20.0) | 35 135.48 | 35 720.32 | 34 383.67 |
| 94 | javascript (14.15)| [moleculer](https://moleculer.services) (0.14) | 34 170.25 | 35 808.80 | 34 672.01 |
| 95 | python (3.8)| [emmett](https://emmett.sh) (2.1) | 34 062.19 | 39 194.32 | 40 063.34 |
| 96 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 34 020.05 | 35 511.36 | 35 173.76 |
| 97 | rust (1.48)| [nickel](https://nickel-org.github.io) (0.11) | 33 798.10 | 34 255.79 | 36 183.26 |
| 98 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 33 472.93 | 38 616.45 | 39 139.41 |
| 99 | javascript (14.15)| [foxify](https://foxify.js.org) (0.1) | 32 812.08 | 48 551.63 | 48 160.27 |
| 100 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 32 666.38 | 36 526.18 | 36 805.34 |
| 101 | python (3.8)| [hug](https://hug.rest) (2.6) | 32 511.19 | 35 184.93 | 52 524.35 |
| 102 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 31 553.05 | 31 894.32 | 31 191.52 |
| 103 | rust (1.48)| [gotham](https://gotham.rs) (0.5) | 31 031.34 | 34 102.61 | 34 870.43 |
| 104 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 29 533.09 | 29 137.76 | 26 225.51 |
| 105 | javascript (14.15)| [restify](https://restify.com) (8.5) | 29 460.60 | 30 843.10 | 30 280.35 |
| 106 | php (7.4)| [imi](https://imiphp.com) (1.2) | 29 458.91 | 33 990.93 | 34 535.10 |
| 107 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 29 195.25 | 32 207.07 | 32 065.92 |
| 108 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.9) | 29 031.77 | 34 039.43 | 34 428.50 |
| 109 | python (3.8)| [starlette](https://starlette.io) (0.14) | 27 980.67 | 31 541.42 | 32 136.45 |
| 110 | javascript (14.15)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 27 835.43 | 29 392.43 | 28 339.62 |
| 111 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 27 815.79 | 28 971.84 | 28 844.91 |
| 112 | javascript (14.15)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 27 496.06 | 44 751.40 | 46 598.19 |
| 113 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 27 486.91 | 34 051.32 | 34 754.98 |
| 114 | python (3.8)| [index.py](https://index-py.abersheeran.com) (0.15) | 27 205.52 | 29 027.47 | 29 518.46 |
| 115 | scala (2.13)| [play](https://playframework.com) (2.8) | 26 879.61 | 28 218.17 | 28 472.76 |
| 116 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 26 757.68 | 28 855.37 | 28 428.71 |
| 117 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 24 949.21 | 25 125.21 | 23 474.29 |
| 118 | python (3.8)| [responder](https://python-responder.org) (2.0) | 24 490.41 | 32 206.88 | 32 189.26 |
| 119 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 23 956.79 | 26 392.89 | 25 802.65 |
| 120 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 487.35 | 28 206.17 | 28 057.45 |
| 121 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 23 186.87 | 21 423.45 | 20 666.61 |
| 122 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.16) | 22 039.55 | 20 879.00 | 19 647.68 |
| 123 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 21 933.53 | 21 021.81 | 20 684.77 |
| 124 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 21 793.89 | 21 695.79 | 21 438.28 |
| 125 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 20 778.37 | 19 893.67 | 21 032.56 |
| 126 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 19 422.30 | 23 614.15 | 23 575.84 |
| 127 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 18 466.83 | 22 633.00 | 23 719.97 |
| 128 | javascript (14.15)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 18 444.29 | 17 886.76 | 16 695.45 |
| 129 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 18 400.64 | 20 966.14 | 20 873.78 |
| 130 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 565.34 | 15 674.55 | 14 684.20 |
| 131 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 17 298.85 | 16 819.70 | 16 570.00 |
| 132 | php (7.4)| [swoft](https://swoft.org) (2.0) | 17 133.30 | 20 990.34 | 21 260.54 |
| 133 | rust (1.48)| [iron](https://ironframework.io) (0.6) | 16 901.54 | 16 830.18 | 16 666.65 |
| 134 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.62) | 16 647.32 | 19 381.57 | 19 436.02 |
| 135 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 16 599.57 | 20 317.92 | 20 279.08 |
| 136 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 16 564.23 | 16 060.18 | 15 812.41 |
| 137 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 16 365.07 | 19 036.82 | 20 148.19 |
| 138 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 16 093.38 | 14 100.25 | 13 227.04 |
| 139 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.38) | 16 087.78 | 15 792.09 | 15 320.56 |
| 140 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 554.73 | 17 991.43 | 18 430.54 |
| 141 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 15 430.07 | 17 889.10 | 17 726.88 |
| 142 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 15 362.41 | 15 039.59 | 14 665.78 |
| 143 | go (1.15)| [macaron](https://go-macaron.com) (1.4) | 15 324.61 | 16 789.70 | 16 739.60 |
| 144 | javascript (14.15)| [feathersjs](https://feathersjs.com) (4.5) | 15 013.50 | 17 460.18 | 17 209.64 |
| 145 | javascript (14.15)| [express](https://expressjs.com) (4.17) | 14 553.36 | 18 157.43 | 18 026.97 |
| 146 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 14 240.68 | 13 719.26 | 13 531.53 |
| 147 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 13 798.25 | 17 246.29 | 15 075.50 |
| 148 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 13 785.54 | 14 299.71 | 21 167.56 |
| 149 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 13 654.07 | 13 287.79 | 12 973.08 |
| 150 | java (11)| [struts2](https://struts.apache.org) (2.5) | 13 648.65 | 14 001.26 | 14 020.78 |
| 151 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 13 059.89 | 12 614.41 | 12 459.28 |
| 152 | java (11)| [blade](https://lets-blade.com) (2.0) | 13 035.72 | 15 419.26 | 14 341.61 |
| 153 | javascript (14.15)| [nestjs-express](https://nestjs.com) (7.4) | 11 951.53 | 14 443.53 | 14 258.97 |
| 154 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 417.88 | 11 652.63 | 11 716.02 |
| 155 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 11 183.35 | 10 943.48 | 10 498.45 |
| 156 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.4) | 10 247.10 | 10 488.39 | 10 588.18 |
| 157 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 10 241.32 | 10 432.88 | 10 543.94 |
| 158 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 10 178.50 | 9 838.04 | 9 020.68 |
| 159 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 891.33 | 10 045.53 | 10 091.57 |
| 160 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 9 775.91 | 9 939.26 | 10 034.38 |
| 161 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.6) | 9 666.89 | 9 750.35 | 9 795.10 |
| 162 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.1) | 9 055.07 | 8 787.16 | 8 821.01 |
| 163 | ruby (2.7)| [grape](https://ruby-grape.org) (1.5) | 8 639.02 | 8 340.40 | 8 322.65 |
| 164 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 8 601.51 | 13 516.22 | 12 587.11 |
| 165 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 6 788.29 | 6 731.22 | 6 664.36 |
| 166 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 6 760.27 | 6 466.21 | 6 186.61 |
| 167 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 648.37 | 6 574.08 | 6 506.25 |
| 168 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 6 557.85 | 6 509.71 | 6 369.09 |
| 169 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 6 415.57 | 6 334.52 | 6 162.92 |
| 170 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 253.03 | 6 175.74 | 6 115.00 |
| 171 | python (3.8)| [tornado](https://tornadoweb.org) (6.1) | 6 178.99 | 6 237.55 | 6 249.60 |
| 172 | python (3.8)| [django](https://djangoproject.com) (3.1) | 6 045.66 | 6 025.20 | 5 658.42 |
| 173 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 5 775.14 | 5 609.55 | 5 634.17 |
| 174 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 5 343.30 | 5 265.18 | 5 191.18 |
| 175 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 5 323.88 | 6 276.30 | 6 488.57 |
| 176 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 5 175.91 | 5 139.89 | 5 184.36 |
| 177 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 4 992.00 | 4 874.67 | 4 517.33 |
| 178 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 4 686.57 | 4 649.88 | 4 568.27 |
| 179 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 490.00 | 4 452.07 | 4 395.95 |
| 180 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 467.95 | 4 432.10 | 4 446.73 |
| 181 | javascript (14.15)| [sails](https://sailsjs.com) (1.4) | 4 239.21 | 4 230.03 | 4 167.42 |
| 182 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 4 133.48 | 4 125.18 | 4 126.18 |
| 183 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.2) | 4 011.96 | 3 945.36 | 3 944.03 |
| 184 | php (7.4)| [lumen](https://lumen.laravel.com) (8.1) | 3 860.41 | 3 853.74 | 3 886.81 |
| 185 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.4) | 3 830.09 | 3 767.65 | 3 768.82 |
| 186 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 3 714.85 | 1 952.03 | 1 738.71 |
| 187 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 3 547.20 | 3 500.16 | 3 499.04 |
| 188 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 2 877.23 | 7 194.95 | 5 214.24 |
| 189 | ruby (2.7)| [rails](https://rubyonrails.org) (6.0) | 2 870.96 | 2 853.93 | 2 856.10 |
| 190 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 645.40 | 2 627.34 | 2 654.30 |
| 191 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 537.10 | 2 538.26 | 2 519.46 |
| 192 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.4) | 2 351.53 | 2 344.90 | 2 359.94 |
| 193 | php (7.4)| [symfony](https://symfony.com) (5.1) | 2 084.02 | 2 091.06 | 2 110.47 |
| 194 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 920.29 | 1 844.72 | 1 759.33 |
| 195 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 903.13 | 1 834.27 | 1 832.41 |
| 196 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 1 851.38 | 1 858.81 | 1 820.39 |
| 197 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 1 845.33 | 1 855.26 | 1 852.73 |
| 198 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 1 651.00 | 1 640.68 | 1 627.56 |
| 199 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 620.77 | 1 633.90 | 1 596.26 |
| 200 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 496.28 | 1 502.20 | 1 513.83 |
| 201 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 378.00 | 573.22 | 357.74 |
| 202 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 098.00 | 1 120.65 | 1 112.39 |
| 203 | python (3.8)| [django-ninja](https://django-ninja.rest-framework.com) (0.9) | 1 070.66 | 1 424.85 | 1 462.75 |
| 204 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 040.39 | 1 013.26 | 1 001.29 |
| 205 | crystal (0.35)| [lucky](https://luckyframework.org) (0.23) | 432.17 | 442.10 | 398.44 |
| 206 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 286.66 | 304.42 | -103.77 |
| 207 | php (7.4)| [laravel](https://laravel.com) (7.27) | 124.93 | 55.84 | 2.92 |

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

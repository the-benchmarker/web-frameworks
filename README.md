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

:information_source:  Updated on **2020-12-21** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 149 711.27 | 183 113.06 | 185 579.62 |
| 2 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 123 005.99 | 136 021.11 | 137 163.40 |
| 3 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 121 061.83 | 123 366.77 | 123 145.82 |
| 4 | go (1.15)| [fiber](https://gofiber.io) (2.3) | 119 633.73 | 130 905.01 | 130 228.87 |
| 5 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 118 242.34 | 146 370.61 | 149 169.10 |
| 6 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.5) | 116 642.10 | 128 966.36 | 128 611.34 |
| 7 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.18) | 116 636.42 | 130 620.50 | 130 319.28 |
| 8 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 115 924.06 | 128 716.07 | 128 314.67 |
| 9 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 115 888.59 | 128 515.85 | 127 928.87 |
| 10 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 114 972.43 | 142 814.64 | 145 407.14 |
| 11 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 112 487.44 | 111 500.52 | 114 631.46 |
| 12 | java (11)| [jooby](https://jooby.io) (2.9) | 110 075.38 | 136 657.89 | 141 660.49 |
| 13 | javascript (14.15)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 109 494.01 | 139 135.40 | 141 539.88 |
| 14 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 108 893.78 | 134 537.65 | 137 310.33 |
| 15 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 108 421.04 | 105 228.61 | 109 848.76 |
| 16 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 108 355.60 | 136 696.38 | 141 203.47 |
| 17 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 107 955.32 | 133 677.83 | 137 852.41 |
| 18 | c (11)| [kore](https://kore.io) (3.3) | 107 868.05 | 166 191.83 | 194 767.88 |
| 19 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 106 370.19 | 131 580.25 | 134 875.66 |
| 20 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 98 374.29 | 119 380.24 | 120 020.42 |
| 21 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 98 112.11 | 121 382.46 | 121 073.34 |
| 22 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.4) | 97 375.62 | 120 192.29 | 120 614.07 |
| 23 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 95 510.74 | 140 248.74 | 151 389.05 |
| 24 | crystal (0.35)| [kemal](https://kemalcr.com) (0.27) | 95 056.63 | 112 606.66 | 112 910.77 |
| 25 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 94 913.73 | 115 770.21 | 118 366.96 |
| 26 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 94 020.98 | 114 510.25 | 113 965.83 |
| 27 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 89 710.53 | 107 187.19 | 107 275.52 |
| 28 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.12) | 83 217.08 | 93 927.85 | 88 669.12 |
| 29 | javascript (14.15)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 82 831.80 | 97 242.95 | 100 347.91 |
| 30 | go (1.15)| [gf](https://goframe.org) (1.14) | 81 019.04 | 88 971.62 | 91 261.90 |
| 31 | go (1.15)| [clevergo](https://clevergo.tech) (0.5) | 80 931.68 | 81 975.20 | 84 079.59 |
| 32 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 80 869.53 | 81 706.97 | 83 845.44 |
| 33 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 80 822.29 | 96 101.02 | 93 098.98 |
| 34 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 79 907.44 | 81 132.44 | 83 085.62 |
| 35 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 79 831.44 | 80 696.69 | 82 691.33 |
| 36 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 682.05 | 83 173.34 | 84 397.34 |
| 37 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 78 228.09 | 82 035.97 | 83 546.49 |
| 38 | go (1.15)| [chi](https://github.com/go-chi/chi) (1.5) | 77 589.62 | 76 990.42 | 79 188.64 |
| 39 | go (1.15)| [violetear](https://violetear.org) (7.0) | 77 033.28 | 77 183.53 | 79 344.63 |
| 40 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 76 464.15 | 118 009.13 | 131 078.69 |
| 41 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.2) | 75 726.14 | 77 066.34 | 73 377.55 |
| 42 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 75 260.41 | 87 898.63 | 90 870.41 |
| 43 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 75 174.70 | 74 445.18 | 76 747.40 |
| 44 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 75 057.82 | 79 170.58 | 79 952.30 |
| 45 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 73 472.83 | 71 768.11 | 74 668.49 |
| 46 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 72 804.01 | 84 439.47 | 86 813.19 |
| 47 | javascript (14.15)| [nestjs-fastify](https://nestjs.com) (7.4) | 72 432.74 | 78 694.81 | 77 515.99 |
| 48 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 72 313.38 | 79 933.68 | 82 124.50 |
| 49 | go (1.15)| [beego](https://beego.me) (1.12) | 71 637.46 | 74 881.98 | 76 664.00 |
| 50 | fsharp (5.0)| [falco](https://github.com/pimbrouwers/Falco) (3.0) | 71 488.56 | 83 510.90 | 86 203.80 |
| 51 | java (11)| [restheart](https://restheart.org) (5.1) | 71 021.72 | 73 401.34 | 74 228.08 |
| 52 | javascript (14.15)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 71 007.88 | 80 039.53 | 80 769.67 |
| 53 | go (1.15)| [air](https://github.com/aofei/air) (0.21) | 65 012.36 | 63 593.27 | 66 065.79 |
| 54 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 64 752.98 | 72 307.81 | 72 415.19 |
| 55 | javascript (14.15)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 63 308.16 | 69 642.87 | 70 201.96 |
| 56 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.5) | 62 880.82 | 62 875.58 | 65 353.20 |
| 57 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 62 174.78 | 68 216.16 | 67 899.68 |
| 58 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 60 705.39 | 63 932.05 | 64 822.22 |
| 59 | rust (1.48)| [actix](https://actix.rs) (3.3) | 59 855.59 | 61 162.15 | 62 632.35 |
| 60 | javascript (14.15)| [0http](https://github.com/jkyberneees/0http) (3.0) | 59 450.19 | 65 178.21 | 67 093.02 |
| 61 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 59 140.84 | 136 426.53 | 136 476.11 |
| 62 | javascript (14.15)| [polka](https://github.com/lukeed/polka) (0.5) | 57 576.88 | 62 286.86 | 61 156.90 |
| 63 | javascript (14.15)| [fastify](https://fastify.io) (3.7) | 55 410.14 | 60 037.77 | 58 551.91 |
| 64 | javascript (14.15)| [rayo](https://rayo.js.org) (1.3) | 54 350.36 | 59 849.38 | 58 835.33 |
| 65 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 54 251.12 | 57 305.26 | 55 505.67 |
| 66 | javascript (14.15)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 53 652.12 | 57 839.21 | 58 198.10 |
| 67 | java (11)| [javalin](https://javalin.io) (3.9) | 53 626.75 | 57 172.74 | 57 534.47 |
| 68 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 53 034.17 | 58 027.76 | 58 268.03 |
| 69 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 53 024.29 | 61 144.04 | 64 246.38 |
| 70 | php (7.4)| [one](https://github.com/lizhichao/one) (2.1) | 52 601.65 | 63 658.59 | 69 887.93 |
| 71 | php (7.4)| [hyperf](https://hyperf.io) (2.0) | 51 818.81 | 63 713.99 | 69 318.25 |
| 72 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 51 439.25 | 56 805.66 | 63 664.22 |
| 73 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.14) | 51 214.15 | 74 569.09 | 81 173.69 |
| 74 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 50 979.44 | 57 454.32 | 58 056.43 |
| 75 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 50 780.70 | 66 654.75 | 69 197.02 |
| 76 | java (11)| [micronaut](https://micronaut.io) (1.2) | 50 764.89 | 56 613.36 | 57 186.89 |
| 77 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 46 112.49 | 49 159.84 | 51 703.54 |
| 78 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 46 105.67 | 46 542.89 | 49 398.07 |
| 79 | javascript (14.15)| [foxify](https://foxify.js.org) (0.1) | 44 981.02 | 48 997.97 | 48 319.01 |
| 80 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 758.86 | 45 920.31 | 46 141.01 |
| 81 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.8) | 44 740.43 | 48 748.89 | 49 837.72 |
| 82 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 44 144.42 | 50 827.38 | 52 323.94 |
| 83 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 42 970.04 | 32 791.62 | 31 802.97 |
| 84 | javascript (14.15)| [koa](https://koajs.com) (2.13) | 42 377.83 | 44 358.79 | 43 459.73 |
| 85 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 41 420.51 | 45 591.82 | 46 240.96 |
| 86 | rust (1.48)| [nickel](https://nickel-org.github.io) (0.11) | 37 358.65 | 37 134.90 | 37 541.35 |
| 87 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 35 885.03 | 36 795.53 | 37 310.14 |
| 88 | swift (5.3)| [vapor](https://vapor.codes) (4.36) | 35 789.02 | 37 546.31 | 37 263.87 |
| 89 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 35 655.55 | 37 837.49 | 37 975.07 |
| 90 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 35 261.81 | 49 600.06 | 51 283.80 |
| 91 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 34 862.41 | 34 913.45 | 35 021.74 |
| 92 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 34 674.81 | 35 271.30 | 35 156.35 |
| 93 | javascript (14.15)| [hapi](https://hapijs.com) (20.0) | 34 070.06 | 35 563.42 | 34 513.83 |
| 94 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 33 852.33 | 38 245.67 | 39 112.86 |
| 95 | python (3.8)| [emmett](https://emmett.sh) (2.1) | 33 040.37 | 39 901.31 | 41 241.63 |
| 96 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 32 491.91 | 36 605.35 | 36 997.90 |
| 97 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 32 125.11 | 28 579.95 | 22 879.47 |
| 98 | python (3.8)| [hug](https://hug.rest) (2.6) | 31 883.55 | 34 718.02 | 52 178.71 |
| 99 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 31 770.61 | 31 263.97 | 31 004.11 |
| 100 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.3) | 31 545.44 | 34 001.10 | 34 707.56 |
| 101 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.9) | 31 539.83 | 33 863.56 | 34 269.49 |
| 102 | rust (1.48)| [gotham](https://gotham.rs) (0.5) | 31 493.98 | 34 123.71 | 34 959.13 |
| 103 | javascript (14.15)| [restify](https://restify.com) (8.5) | 30 078.77 | 31 747.73 | 30 980.76 |
| 104 | php (7.4)| [imi](https://imiphp.com) (1.2) | 29 534.84 | 31 680.09 | 34 411.48 |
| 105 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 29 025.67 | 32 010.30 | 31 721.28 |
| 106 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 27 838.93 | 29 109.99 | 29 317.47 |
| 107 | javascript (14.15)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 27 755.29 | 29 451.09 | 28 372.45 |
| 108 | scala (2.13)| [play](https://playframework.com) (2.8) | 27 522.11 | 29 273.94 | 29 063.16 |
| 109 | javascript (14.15)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 27 491.80 | 44 127.75 | 45 017.73 |
| 110 | python (3.8)| [starlette](https://starlette.io) (0.14) | 27 417.92 | 31 333.34 | 31 899.56 |
| 111 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 27 069.58 | 29 165.36 | 28 622.58 |
| 112 | python (3.8)| [index.py](https://index-py.abersheeran.com) (0.15) | 26 268.28 | 28 475.48 | 29 639.61 |
| 113 | python (3.8)| [responder](https://python-responder.org) (2.0) | 26 073.36 | 30 722.11 | 31 979.37 |
| 114 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 25 364.83 | 25 240.31 | 23 994.33 |
| 115 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 24 287.29 | 26 324.78 | 25 757.13 |
| 116 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 23 327.21 | 21 562.17 | 20 519.87 |
| 117 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 22 467.90 | 21 309.62 | 21 122.68 |
| 118 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 22 402.12 | 28 015.39 | 27 943.05 |
| 119 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 22 043.22 | 21 797.43 | 21 762.34 |
| 120 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.16) | 21 916.00 | 21 200.26 | 19 919.97 |
| 121 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 20 753.40 | 20 219.41 | 20 599.45 |
| 122 | javascript (14.15)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 18 358.92 | 17 472.97 | 16 620.35 |
| 123 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 607.13 | 15 634.80 | 14 636.93 |
| 124 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 17 575.18 | 22 858.48 | 23 434.10 |
| 125 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 17 547.30 | 21 115.39 | 21 026.22 |
| 126 | php (7.4)| [swoft](https://swoft.org) (2.0) | 17 157.59 | 20 417.40 | 21 074.80 |
| 127 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 17 108.31 | 17 153.99 | 16 450.06 |
| 128 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.62) | 16 818.19 | 20 708.43 | 20 567.52 |
| 129 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 16 686.07 | 20 270.98 | 20 077.37 |
| 130 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 16 590.58 | 16 120.87 | 15 853.46 |
| 131 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 16 393.37 | 19 303.79 | 19 464.27 |
| 132 | rust (1.48)| [iron](https://ironframework.io) (0.6) | 16 366.52 | 16 667.27 | 16 659.24 |
| 133 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 16 080.69 | 14 227.68 | 13 189.80 |
| 134 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.39) | 16 052.05 | 15 477.16 | 15 242.50 |
| 135 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 15 870.14 | 18 724.13 | 18 835.10 |
| 136 | javascript (14.15)| [feathersjs](https://feathersjs.com) (4.5) | 15 711.98 | 18 131.33 | 17 879.28 |
| 137 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 15 445.41 | 17 977.41 | 17 802.26 |
| 138 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 421.14 | 17 760.00 | 17 846.48 |
| 139 | go (1.15)| [macaron](https://go-macaron.com) (1.4) | 15 358.18 | 16 952.10 | 16 861.70 |
| 140 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 15 346.73 | 14 918.02 | 14 579.38 |
| 141 | javascript (14.15)| [express](https://expressjs.com) (4.17) | 14 709.93 | 17 778.57 | 17 680.86 |
| 142 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 14 195.75 | 17 379.62 | 17 351.78 |
| 143 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 14 183.45 | 13 603.27 | 13 598.86 |
| 144 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 13 848.99 | 13 445.67 | 13 564.70 |
| 145 | ruby (2.7)| [rack_app](https://rack-app.com) (7.6) | 13 817.88 | 13 334.96 | 13 163.14 |
| 146 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 13 195.17 | 12 812.75 | 12 657.83 |
| 147 | java (11)| [blade](https://lets-blade.com) (2.0) | 13 000.35 | 16 579.16 | 14 742.82 |
| 148 | javascript (14.15)| [nestjs-express](https://nestjs.com) (7.4) | 11 705.36 | 13 071.80 | 13 736.42 |
| 149 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 11 490.64 | 11 113.05 | 10 712.23 |
| 150 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 378.45 | 11 679.20 | 11 689.59 |
| 151 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.4) | 10 189.67 | 10 445.65 | 10 609.97 |
| 152 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 10 060.65 | 10 331.40 | 10 429.66 |
| 153 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 977.27 | 9 937.70 | 9 853.59 |
| 154 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 813.24 | 10 002.81 | 10 000.68 |
| 155 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 9 752.85 | 9 983.25 | 10 108.98 |
| 156 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.6) | 9 616.52 | 9 712.78 | 9 823.03 |
| 157 | python (3.8)| [guillotina](https://guillotina.io) (6.0) | 9 108.82 | 9 046.56 | 8 917.38 |
| 158 | ruby (2.7)| [grape](https://ruby-grape.org) (1.5) | 8 844.72 | 8 523.38 | 8 515.20 |
| 159 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.1) | 8 753.54 | 8 541.64 | 8 519.49 |
| 160 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 8 604.92 | 13 289.03 | 12 546.30 |
| 161 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 7 483.53 | 7 144.65 | 6 883.84 |
| 162 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 6 738.59 | 6 673.52 | 6 595.55 |
| 163 | python (3.8)| [tornado](https://tornadoweb.org) (6.1) | 6 672.98 | 6 586.56 | 6 916.85 |
| 164 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 646.83 | 6 571.51 | 6 517.97 |
| 165 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 6 565.49 | 6 511.68 | 6 343.01 |
| 166 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 6 378.53 | 6 319.91 | 6 202.75 |
| 167 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 248.45 | 6 178.75 | 6 125.83 |
| 168 | python (3.8)| [django](https://djangoproject.com) (3.1) | 6 103.28 | 6 404.28 | 5 671.52 |
| 169 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 5 990.18 | 6 758.45 | 6 663.90 |
| 170 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 5 766.96 | 5 654.39 | 5 670.61 |
| 171 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 5 337.24 | 5 279.95 | 5 185.21 |
| 172 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 5 270.21 | 5 193.40 | 5 213.59 |
| 173 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 5 081.54 | 4 965.87 | 4 474.22 |
| 174 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.1) | 4 719.00 | 4 670.57 | 4 588.95 |
| 175 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 474.31 | 4 443.73 | 4 372.60 |
| 176 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 452.62 | 4 425.61 | 4 421.90 |
| 177 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 4 381.38 | 3 201.91 | 2 904.87 |
| 178 | javascript (14.15)| [sails](https://sailsjs.com) (1.4) | 4 269.98 | 4 267.44 | 4 212.30 |
| 179 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 4 134.99 | 4 112.11 | 4 139.96 |
| 180 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.2) | 4 002.15 | 3 931.38 | 3 924.66 |
| 181 | php (7.4)| [lumen](https://lumen.laravel.com) (8.1) | 3 861.68 | 3 845.55 | 3 879.26 |
| 182 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.4) | 3 829.41 | 3 767.43 | 3 754.24 |
| 183 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 3 430.08 | 3 393.40 | 3 396.03 |
| 184 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 057.46 | 7 095.92 | 5 164.04 |
| 185 | ruby (2.7)| [rails](https://rubyonrails.org) (6.1) | 2 818.32 | 2 802.84 | 2 791.95 |
| 186 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 642.81 | 2 627.33 | 2 639.40 |
| 187 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 470.10 | 2 543.72 | 2 545.94 |
| 188 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.4) | 2 333.49 | 2 348.41 | 2 347.46 |
| 189 | php (7.4)| [symfony](https://symfony.com) (5.1) | 2 081.46 | 2 092.91 | 2 103.71 |
| 190 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 2 019.27 | 2 018.38 | 2 051.71 |
| 191 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 919.34 | 1 846.43 | 1 782.74 |
| 192 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 904.68 | 1 818.29 | 1 827.33 |
| 193 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 1 870.92 | 1 871.57 | 1 873.40 |
| 194 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 1 658.35 | 1 678.11 | 1 641.48 |
| 195 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 598.42 | 1 622.72 | 1 596.37 |
| 196 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 499.16 | 1 503.14 | 1 511.19 |
| 197 | python (3.8)| [django-ninja](https://django-ninja.rest-framework.com) (0.9) | 1 277.54 | 1 582.85 | 1 654.09 |
| 198 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 108.64 | 497.04 | 543.18 |
| 199 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 106.66 | 1 124.17 | 1 122.86 |
| 200 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 036.32 | 1 007.12 | 994.41 |
| 201 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 282.49 | 300.31 | -95.28 |
| 202 | php (7.4)| [laravel](https://laravel.com) (7.27) | 127.24 | 34.18 | -0.00 |

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

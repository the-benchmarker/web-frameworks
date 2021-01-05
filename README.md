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

:information_source:  Updated on **2021-01-05** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 150 595.62 | 182 620.50 | 184 968.89 |
| 2 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 123 031.95 | 135 652.73 | 137 051.79 |
| 3 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 120 725.96 | 123 884.24 | 122 998.52 |
| 4 | go (1.15)| [fiber](https://gofiber.io) (2.3) | 120 229.12 | 130 071.86 | 128 847.52 |
| 5 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 117 410.11 | 145 417.76 | 148 604.13 |
| 6 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.5) | 117 261.27 | 128 283.94 | 128 097.82 |
| 7 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.19) | 116 081.98 | 130 302.01 | 130 170.53 |
| 8 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 115 954.51 | 128 131.50 | 127 690.60 |
| 9 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 115 734.93 | 128 789.69 | 128 001.50 |
| 10 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 114 734.22 | 140 989.07 | 144 261.34 |
| 11 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 112 666.44 | 111 725.15 | 115 084.99 |
| 12 | javascript (14.15)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.4) | 110 558.26 | 140 664.92 | 144 207.62 |
| 13 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 109 519.30 | 136 189.95 | 140 717.40 |
| 14 | java (11)| [jooby](https://jooby.io) (2.9) | 108 912.55 | 136 795.72 | 141 782.33 |
| 15 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 108 770.68 | 131 124.53 | 134 148.59 |
| 16 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 108 471.81 | 104 670.94 | 109 274.19 |
| 17 | javascript (14.15)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 108 308.02 | 138 220.55 | 140 541.47 |
| 18 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 106 542.41 | 134 377.74 | 138 277.34 |
| 19 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 105 725.24 | 131 295.65 | 134 093.51 |
| 20 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 98 798.93 | 120 243.46 | 121 163.07 |
| 21 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.4) | 98 067.53 | 119 815.07 | 120 026.54 |
| 22 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 97 925.97 | 118 394.77 | 119 611.20 |
| 23 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 95 145.99 | 116 719.70 | 119 581.83 |
| 24 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 94 539.13 | 139 040.31 | 149 674.81 |
| 25 | c (11)| [kore](https://kore.io) (3.3) | 93 221.58 | 156 498.29 | 181 393.85 |
| 26 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 92 619.09 | 112 788.83 | 113 160.40 |
| 27 | crystal (0.35)| [kemal](https://kemalcr.com) (0.27) | 90 936.55 | 110 653.81 | 110 290.42 |
| 28 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 89 863.67 | 105 943.71 | 106 290.08 |
| 29 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 82 849.59 | 96 907.00 | 95 044.15 |
| 30 | javascript (14.15)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 82 492.03 | 97 827.44 | 100 063.74 |
| 31 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.12) | 81 956.38 | 93 193.42 | 89 212.90 |
| 32 | go (1.15)| [gf](https://goframe.org) (1.15) | 81 441.59 | 88 909.91 | 91 487.38 |
| 33 | go (1.15)| [clevergo](https://clevergo.tech) (0.5) | 81 142.98 | 82 146.44 | 84 266.51 |
| 34 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 80 943.32 | 81 459.71 | 83 747.43 |
| 35 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 79 892.78 | 81 152.43 | 83 041.70 |
| 36 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 79 547.52 | 80 269.00 | 82 223.52 |
| 37 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 464.26 | 82 710.63 | 84 170.85 |
| 38 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 78 153.58 | 81 788.57 | 83 440.79 |
| 39 | go (1.15)| [chi](https://github.com/go-chi/chi) (1.5) | 77 723.91 | 76 965.09 | 79 662.43 |
| 40 | go (1.15)| [violetear](https://violetear.org) (7.0) | 76 938.42 | 77 073.65 | 79 260.55 |
| 41 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 76 744.51 | 115 695.13 | 129 402.87 |
| 42 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 75 148.45 | 79 098.12 | 79 630.68 |
| 43 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 74 973.63 | 87 824.86 | 90 601.12 |
| 44 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.2) | 74 886.07 | 73 824.50 | 74 103.17 |
| 45 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 74 821.03 | 74 539.22 | 76 722.37 |
| 46 | fsharp (5.0)| [falco](https://github.com/pimbrouwers/Falco) (3.0) | 74 813.99 | 85 739.03 | 85 979.54 |
| 47 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 73 664.03 | 80 517.35 | 82 462.48 |
| 48 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 73 418.45 | 71 514.97 | 74 455.55 |
| 49 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 72 559.87 | 83 719.61 | 86 120.05 |
| 50 | java (11)| [restheart](https://restheart.org) (5.1) | 71 759.35 | 74 413.17 | 75 533.60 |
| 51 | go (1.15)| [beego](https://beego.me) (1.12) | 71 577.14 | 74 520.50 | 76 247.51 |
| 52 | javascript (14.15)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 71 391.11 | 78 721.42 | 79 781.31 |
| 53 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 65 229.42 | 73 017.21 | 73 213.03 |
| 54 | go (1.15)| [air](https://github.com/aofei/air) (0.21) | 64 824.83 | 63 385.80 | 65 963.10 |
| 55 | javascript (14.15)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 63 128.17 | 68 470.34 | 70 269.09 |
| 56 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.6) | 62 789.44 | 62 639.47 | 65 530.30 |
| 57 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 61 123.01 | 66 660.58 | 66 242.56 |
| 58 | javascript (14.15)| [0http](https://github.com/jkyberneees/0http) (3.0) | 60 146.33 | 65 814.59 | 67 332.90 |
| 59 | scala (2.13)| [finch](https://finagle.github.io/finch/) (0.32) | 59 609.32 | 65 988.77 | 66 111.52 |
| 60 | rust (1.49)| [actix](https://actix.rs) (3.3) | 59 433.68 | 57 894.24 | 62 805.74 |
| 61 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 59 431.30 | 62 996.96 | 63 211.65 |
| 62 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 58 525.05 | 121 351.38 | 137 824.69 |
| 63 | javascript (14.15)| [rayo](https://rayo.js.org) (1.3) | 56 145.37 | 60 987.62 | 59 538.48 |
| 64 | javascript (14.15)| [restana](https://github.com/jkyberneees/ana) (4.7) | 55 757.08 | 62 150.79 | 63 124.42 |
| 65 | javascript (14.15)| [polka](https://github.com/lukeed/polka) (0.5) | 55 326.65 | 61 443.94 | 60 926.81 |
| 66 | javascript (14.15)| [fastify](https://fastify.io) (3.7) | 54 507.02 | 59 446.56 | 59 101.89 |
| 67 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.14) | 54 476.38 | 72 810.84 | 81 064.28 |
| 68 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 54 136.94 | 57 074.75 | 55 726.11 |
| 69 | javascript (14.15)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 53 491.70 | 57 486.91 | 57 131.24 |
| 70 | java (11)| [javalin](https://javalin.io) (3.9) | 53 292.23 | 56 720.90 | 57 438.39 |
| 71 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 52 991.70 | 61 771.07 | 64 228.82 |
| 72 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 52 592.75 | 57 898.09 | 57 941.17 |
| 73 | php (7.4)| [one](https://github.com/lizhichao/one) (2.2) | 52 111.98 | 62 896.73 | 65 303.65 |
| 74 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 52 102.90 | 56 957.58 | 63 847.75 |
| 75 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 50 625.06 | 66 523.93 | 68 594.41 |
| 76 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 50 567.53 | 57 244.87 | 57 577.66 |
| 77 | java (11)| [micronaut](https://micronaut.io) (1.2) | 50 112.19 | 56 373.55 | 56 369.52 |
| 78 | php (7.4)| [hyperf](https://hyperf.io) (2.1) | 49 158.12 | 61 082.83 | 64 600.78 |
| 79 | javascript (14.15)| [nestjs-fastify](https://nestjs.com) (7.6) | 48 466.66 | 52 745.36 | 50 794.39 |
| 80 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 47 200.90 | 50 692.34 | 52 178.42 |
| 81 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 46 319.71 | 49 319.92 | 50 775.89 |
| 82 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 46 094.56 | 46 618.77 | 49 724.04 |
| 83 | javascript (14.15)| [foxify](https://foxify.js.org) (0.1) | 45 899.56 | 49 238.47 | 48 455.03 |
| 84 | javascript (14.15)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 45 437.69 | 48 464.70 | 48 061.44 |
| 85 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.8) | 44 897.55 | 49 064.78 | 49 702.87 |
| 86 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 704.31 | 45 726.45 | 45 838.69 |
| 87 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 42 999.93 | 31 745.25 | 31 241.67 |
| 88 | javascript (14.15)| [koa](https://koajs.com) (2.13) | 42 322.00 | 44 551.98 | 43 670.80 |
| 89 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 41 804.34 | 46 074.66 | 46 475.21 |
| 90 | javascript (14.15)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 36 292.14 | 39 447.72 | 38 683.27 |
| 91 | swift (5.3)| [vapor](https://vapor.codes) (4.36) | 36 285.77 | 37 976.84 | 37 852.25 |
| 92 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 35 620.90 | 36 975.08 | 37 072.96 |
| 93 | javascript (14.15)| [moleculer](https://moleculer.services) (0.14) | 35 487.65 | 36 300.33 | 35 525.10 |
| 94 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 35 433.35 | 35 209.47 | 34 747.19 |
| 95 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 35 297.63 | 49 425.76 | 52 690.14 |
| 96 | javascript (14.15)| [hapi](https://hapijs.com) (20.0) | 35 124.70 | 35 407.42 | 34 330.78 |
| 97 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 34 356.08 | 37 678.34 | 37 531.88 |
| 98 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 33 596.30 | 38 243.93 | 38 941.44 |
| 99 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 33 553.38 | 33 825.27 | 34 134.02 |
| 100 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 33 268.29 | 37 279.93 | 37 499.35 |
| 101 | rust (1.49)| [nickel](https://nickel-org.github.io) (0.11) | 33 078.90 | 32 593.55 | 33 258.29 |
| 102 | python (3.8)| [hug](https://hug.rest) (2.6) | 32 857.78 | 34 169.31 | 52 953.52 |
| 103 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 32 367.43 | 28 368.62 | 24 559.24 |
| 104 | python (3.8)| [emmett](https://emmett.sh) (2.1) | 32 337.04 | 40 357.48 | 41 340.78 |
| 105 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 31 685.03 | 31 386.10 | 31 047.05 |
| 106 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.12) | 30 480.69 | 34 221.72 | 35 598.36 |
| 107 | php (7.4)| [imi](https://imiphp.com) (1.2) | 29 210.10 | 32 107.13 | 33 775.91 |
| 108 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 28 994.10 | 32 010.00 | 31 819.94 |
| 109 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.3) | 28 797.48 | 33 715.42 | 34 622.67 |
| 110 | javascript (14.15)| [restify](https://restify.com) (8.5) | 28 473.57 | 30 529.95 | 29 903.77 |
| 111 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 27 893.80 | 29 182.25 | 29 011.56 |
| 112 | rust (1.49)| [salvo](https://github.com/kenorld/salvo) (0.3) | 27 800.68 | 29 635.34 | 29 745.69 |
| 113 | javascript (14.15)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 27 556.87 | 29 782.38 | 28 354.75 |
| 114 | python (3.8)| [starlette](https://starlette.io) (0.14) | 27 305.82 | 31 489.78 | 32 417.24 |
| 115 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 27 036.94 | 29 254.48 | 28 667.37 |
| 116 | scala (2.13)| [play](https://playframework.com) (2.8) | 27 000.06 | 28 213.60 | 28 350.32 |
| 117 | javascript (14.15)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 26 748.07 | 28 464.51 | 29 259.84 |
| 118 | rust (1.49)| [gotham](https://gotham.rs) (0.5) | 26 018.34 | 29 074.02 | 30 262.08 |
| 119 | python (3.8)| [index.py](https://index-py.abersheeran.com) (0.15) | 25 100.96 | 28 642.44 | 29 388.33 |
| 120 | python (3.8)| [responder](https://python-responder.org) (2.0) | 24 999.90 | 32 193.87 | 31 988.52 |
| 121 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 24 110.07 | 26 303.67 | 25 704.44 |
| 122 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 610.78 | 27 991.79 | 27 787.08 |
| 123 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 23 372.25 | 23 484.13 | 21 917.06 |
| 124 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 22 845.18 | 21 454.80 | 20 461.04 |
| 125 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.16) | 21 864.31 | 21 106.58 | 19 628.29 |
| 126 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 21 380.89 | 21 126.45 | 20 723.92 |
| 127 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 20 625.67 | 20 050.15 | 20 595.09 |
| 128 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 19 997.40 | 23 525.22 | 23 731.50 |
| 129 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 19 144.37 | 20 982.02 | 20 772.88 |
| 130 | javascript (14.15)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 19 017.11 | 18 043.81 | 16 818.68 |
| 131 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.63) | 17 632.23 | 20 722.60 | 20 682.33 |
| 132 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 536.87 | 15 586.43 | 14 492.09 |
| 133 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 17 235.40 | 16 689.99 | 16 272.75 |
| 134 | php (7.4)| [swoft](https://swoft.org) (2.0) | 17 170.73 | 20 873.78 | 21 168.87 |
| 135 | rust (1.49)| [iron](https://ironframework.io) (0.6) | 16 764.26 | 16 533.68 | 16 599.02 |
| 136 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 16 662.15 | 16 154.64 | 15 858.81 |
| 137 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 16 617.29 | 20 081.49 | 18 869.85 |
| 138 | javascript (14.15)| [express](https://expressjs.com) (4.17) | 16 354.79 | 17 828.17 | 18 014.65 |
| 139 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 16 318.23 | 19 228.83 | 19 708.98 |
| 140 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 16 026.09 | 14 185.22 | 13 186.91 |
| 141 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.39) | 15 998.96 | 15 499.53 | 15 294.62 |
| 142 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 15 978.37 | 18 582.77 | 19 336.95 |
| 143 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 421.34 | 17 607.85 | 17 976.50 |
| 144 | go (1.15)| [macaron](https://go-macaron.com) (1.4) | 15 410.74 | 16 896.61 | 16 895.45 |
| 145 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 15 384.94 | 14 957.70 | 14 729.83 |
| 146 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 15 334.62 | 17 740.28 | 17 712.40 |
| 147 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 15 031.79 | 15 162.82 | 17 078.05 |
| 148 | javascript (14.15)| [feathersjs](https://feathersjs.com) (4.5) | 14 753.65 | 17 473.24 | 17 253.90 |
| 149 | java (11)| [struts2](https://struts.apache.org) (2.5) | 13 934.10 | 14 216.27 | 14 202.29 |
| 150 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 13 909.71 | 13 590.52 | 21 519.38 |
| 151 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 13 894.11 | 13 362.15 | 13 268.66 |
| 152 | ruby (2.7)| [rack_app](https://rack-app.com) (7.7) | 13 626.43 | 13 185.06 | 13 034.29 |
| 153 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 13 036.36 | 12 600.68 | 12 445.31 |
| 154 | java (11)| [blade](https://lets-blade.com) (2.0) | 12 931.97 | 16 141.69 | 14 581.34 |
| 155 | javascript (14.15)| [nestjs-express](https://nestjs.com) (7.6) | 11 645.18 | 12 838.99 | 12 684.74 |
| 156 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 405.99 | 11 698.42 | 11 763.04 |
| 157 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 11 405.78 | 11 289.21 | 10 892.02 |
| 158 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.5) | 10 158.87 | 10 436.51 | 10 573.66 |
| 159 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 10 065.64 | 10 317.45 | 10 322.17 |
| 160 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 942.26 | 10 151.07 | 10 129.09 |
| 161 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 9 728.31 | 9 937.42 | 10 062.11 |
| 162 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.7) | 9 621.83 | 9 719.17 | 9 780.95 |
| 163 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 539.92 | 9 129.53 | 9 382.41 |
| 164 | python (3.8)| [guillotina](https://guillotina.io) (6.0) | 9 071.00 | 9 343.74 | 8 670.39 |
| 165 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.1) | 8 705.57 | 8 489.52 | 8 419.14 |
| 166 | ruby (2.7)| [grape](https://ruby-grape.org) (1.5) | 8 661.03 | 8 272.74 | 8 294.43 |
| 167 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 8 124.40 | 13 530.30 | 12 767.18 |
| 168 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 7 577.46 | 7 286.13 | 6 887.30 |
| 169 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 6 767.48 | 6 704.15 | 6 618.91 |
| 170 | python (3.8)| [tornado](https://tornadoweb.org) (6.1) | 6 651.23 | 6 505.35 | 6 404.15 |
| 171 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 634.71 | 6 565.34 | 6 493.81 |
| 172 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 6 540.86 | 6 506.89 | 6 322.46 |
| 173 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 6 376.70 | 6 360.56 | 6 204.42 |
| 174 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 267.31 | 6 204.03 | 6 121.01 |
| 175 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 6 092.52 | 6 743.96 | 6 758.10 |
| 176 | python (3.8)| [django](https://djangoproject.com) (3.1) | 6 063.14 | 5 710.98 | 5 648.85 |
| 177 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 5 739.17 | 5 660.37 | 5 667.65 |
| 178 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 5 273.47 | 5 174.13 | 5 227.80 |
| 179 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 5 052.79 | 4 778.49 | 4 479.61 |
| 180 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.2) | 4 722.76 | 4 687.76 | 4 593.23 |
| 181 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 449.82 | 4 415.47 | 4 436.13 |
| 182 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 446.98 | 4 404.99 | 4 360.20 |
| 183 | javascript (14.15)| [sails](https://sailsjs.com) (1.4) | 4 257.17 | 4 245.30 | 4 189.53 |
| 184 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.3) | 3 995.15 | 3 938.27 | 3 916.26 |
| 185 | php (7.4)| [nette](https://nette.org/en/) (3.1) | 3 876.70 | 3 856.69 | 3 873.92 |
| 186 | php (7.4)| [lumen](https://lumen.laravel.com) (8.1) | 3 850.16 | 3 857.38 | 3 872.38 |
| 187 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.5) | 3 820.98 | 3 758.40 | 3 765.43 |
| 188 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 3 467.97 | 3 419.61 | 3 421.93 |
| 189 | ruby (2.7)| [rails](https://rubyonrails.org) (6.1) | 2 821.88 | 2 793.70 | 2 797.16 |
| 190 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 628.70 | 2 610.88 | 2 636.24 |
| 191 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 472.24 | 2 542.20 | 2 521.87 |
| 192 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 2 371.22 | 7 086.42 | 5 096.17 |
| 193 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.5) | 2 336.07 | 2 338.77 | 2 341.87 |
| 194 | php (7.4)| [symfony](https://symfony.com) (5.1) | 2 073.95 | 2 093.65 | 2 096.53 |
| 195 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 948.06 | 1 855.72 | 1 788.35 |
| 196 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 910.65 | 1 827.34 | 1 829.49 |
| 197 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 1 883.51 | 1 877.30 | 1 898.49 |
| 198 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 1 750.40 | 1 771.53 | 1 750.28 |
| 199 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 1 669.80 | 1 676.09 | 1 648.77 |
| 200 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 1 635.92 | 1 214.42 | 1 290.86 |
| 201 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 601.35 | 1 627.01 | 1 600.20 |
| 202 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 490.63 | 1 499.91 | 1 504.58 |
| 203 | python (3.8)| [django-ninja](https://django-ninja.rest-framework.com) (0.9) | 1 288.53 | 1 610.64 | 1 647.49 |
| 204 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 180.76 | 400.59 | 305.38 |
| 205 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 114.64 | 1 134.15 | 1 127.58 |
| 206 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 035.70 | 1 011.60 | 999.96 |
| 207 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 287.86 | 304.95 | -86.17 |
| 208 | cpp (11)| [nawa](https://github.com/jatofg/nawa) (0.6) | 242.80 | NaN | NaN |
| 209 | php (7.4)| [laravel](https://laravel.com) (7.27) | 121.48 | 24.53 | 5.24 |

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

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

:information_source:  Updated on **2021-01-03** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 149 537.22 | 182 390.46 | 184 595.51 |
| 2 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 122 305.26 | 135 004.59 | 136 441.03 |
| 3 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 119 260.32 | 123 926.11 | 123 470.67 |
| 4 | go (1.15)| [fiber](https://gofiber.io) (2.3) | 119 087.64 | 130 089.73 | 129 341.72 |
| 5 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 117 756.60 | 145 787.74 | 148 575.60 |
| 6 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.18) | 115 865.01 | 129 352.67 | 129 598.69 |
| 7 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.5) | 115 783.67 | 128 469.41 | 128 230.12 |
| 8 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 115 532.12 | 128 443.87 | 127 831.59 |
| 9 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 115 053.50 | 128 271.81 | 128 416.26 |
| 10 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 114 193.16 | 141 577.37 | 144 565.63 |
| 11 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 112 244.26 | 111 449.41 | 114 620.19 |
| 12 | java (11)| [jooby](https://jooby.io) (2.9) | 110 472.46 | 137 314.73 | 142 099.46 |
| 13 | javascript (14.15)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.4) | 110 025.28 | 142 067.98 | 144 826.71 |
| 14 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 109 644.83 | 136 243.49 | 141 138.02 |
| 15 | javascript (14.15)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 109 330.87 | 138 171.31 | 141 208.21 |
| 16 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 109 079.75 | 134 851.98 | 137 666.52 |
| 17 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 108 155.46 | 104 576.76 | 108 974.78 |
| 18 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 107 415.40 | 134 221.84 | 138 614.30 |
| 19 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 105 919.12 | 131 120.90 | 134 520.58 |
| 20 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 98 398.62 | 120 774.52 | 122 533.94 |
| 21 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 97 570.48 | 118 968.29 | 119 705.18 |
| 22 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.4) | 97 139.81 | 120 032.24 | 120 817.09 |
| 23 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 95 977.89 | 139 922.57 | 150 701.52 |
| 24 | crystal (0.35)| [kemal](https://kemalcr.com) (0.27) | 94 810.75 | 112 244.40 | 112 424.71 |
| 25 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 94 771.28 | 115 751.39 | 118 649.08 |
| 26 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 93 889.46 | 112 465.37 | 111 662.79 |
| 27 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 89 294.03 | 106 456.22 | 106 209.28 |
| 28 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.12) | 83 002.36 | 94 211.22 | 88 967.57 |
| 29 | javascript (14.15)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 82 021.30 | 97 996.17 | 98 544.67 |
| 30 | go (1.15)| [gf](https://goframe.org) (1.15) | 81 036.06 | 88 727.11 | 91 187.81 |
| 31 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 80 912.43 | 81 810.02 | 83 779.93 |
| 32 | go (1.15)| [clevergo](https://clevergo.tech) (0.5) | 80 441.80 | 82 113.47 | 83 900.06 |
| 33 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 80 347.86 | 95 251.03 | 92 892.28 |
| 34 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 79 795.64 | 80 834.87 | 82 861.62 |
| 35 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 79 523.81 | 80 315.30 | 82 239.00 |
| 36 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 555.71 | 82 852.90 | 84 388.84 |
| 37 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 78 165.05 | 82 174.69 | 83 387.62 |
| 38 | go (1.15)| [chi](https://github.com/go-chi/chi) (1.5) | 77 476.82 | 76 943.81 | 79 571.91 |
| 39 | go (1.15)| [violetear](https://violetear.org) (7.0) | 76 744.05 | 77 358.45 | 79 347.92 |
| 40 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.2) | 76 032.35 | 75 339.66 | 75 123.60 |
| 41 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 75 945.22 | 115 868.67 | 128 342.94 |
| 42 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 75 820.44 | 88 534.38 | 91 430.63 |
| 43 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 75 126.87 | 74 370.70 | 76 614.96 |
| 44 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 75 058.48 | 88 131.69 | 90 925.65 |
| 45 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 74 910.17 | 78 935.81 | 79 385.16 |
| 46 | fsharp (5.0)| [falco](https://github.com/pimbrouwers/Falco) (3.0) | 74 536.46 | 87 277.17 | 90 055.63 |
| 47 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 73 918.73 | 80 718.37 | 82 120.21 |
| 48 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 73 029.79 | 71 392.88 | 74 176.87 |
| 49 | go (1.15)| [beego](https://beego.me) (1.12) | 71 350.29 | 74 416.92 | 76 306.13 |
| 50 | java (11)| [restheart](https://restheart.org) (5.1) | 70 746.56 | 68 880.03 | 64 518.96 |
| 51 | javascript (14.15)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 69 857.63 | 79 169.12 | 80 048.86 |
| 52 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 65 296.70 | 73 357.21 | 73 427.03 |
| 53 | go (1.15)| [air](https://github.com/aofei/air) (0.21) | 65 043.82 | 63 397.78 | 65 944.21 |
| 54 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.6) | 62 757.30 | 62 450.21 | 65 361.66 |
| 55 | javascript (14.15)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 62 690.13 | 68 350.07 | 70 116.71 |
| 56 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 61 026.08 | 66 269.37 | 65 485.54 |
| 57 | rust (1.49)| [actix](https://actix.rs) (3.3) | 59 872.17 | 60 146.86 | 62 925.96 |
| 58 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 58 951.42 | 62 345.26 | 63 056.43 |
| 59 | javascript (14.15)| [0http](https://github.com/jkyberneees/0http) (3.0) | 58 509.80 | 65 842.12 | 67 006.15 |
| 60 | javascript (14.15)| [restana](https://github.com/jkyberneees/ana) (4.7) | 57 340.09 | 63 331.64 | 64 332.85 |
| 61 | javascript (14.15)| [polka](https://github.com/lukeed/polka) (0.5) | 56 895.28 | 62 394.03 | 61 261.81 |
| 62 | javascript (14.15)| [fastify](https://fastify.io) (3.7) | 55 761.69 | 60 308.14 | 58 895.35 |
| 63 | javascript (14.15)| [rayo](https://rayo.js.org) (1.3) | 55 158.38 | 60 649.40 | 59 639.09 |
| 64 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 54 193.71 | 58 035.47 | 58 451.45 |
| 65 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 54 050.03 | 57 059.09 | 55 096.19 |
| 66 | java (11)| [javalin](https://javalin.io) (3.9) | 53 765.20 | 56 929.44 | 57 553.72 |
| 67 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.14) | 53 264.65 | 74 090.43 | 83 254.12 |
| 68 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 52 996.53 | 58 053.56 | 62 786.32 |
| 69 | php (7.4)| [hyperf](https://hyperf.io) (2.1) | 52 607.98 | 60 272.91 | 66 605.66 |
| 70 | php (7.4)| [one](https://github.com/lizhichao/one) (2.2) | 52 062.78 | 63 802.96 | 71 361.15 |
| 71 | javascript (14.15)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 51 837.94 | 58 347.76 | 57 079.85 |
| 72 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 51 557.01 | 57 036.94 | 63 822.88 |
| 73 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 51 308.81 | 66 542.60 | 68 910.04 |
| 74 | java (11)| [micronaut](https://micronaut.io) (1.2) | 51 067.76 | 57 638.52 | 57 820.64 |
| 75 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 49 079.48 | 54 866.22 | 55 192.88 |
| 76 | javascript (14.15)| [nestjs-fastify](https://nestjs.com) (7.6) | 48 235.44 | 53 057.31 | 51 179.46 |
| 77 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 45 866.34 | 46 610.48 | 49 587.39 |
| 78 | javascript (14.15)| [foxify](https://foxify.js.org) (0.1) | 45 815.76 | 49 363.33 | 48 729.17 |
| 79 | javascript (14.15)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 45 348.27 | 48 822.81 | 47 533.46 |
| 80 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 45 183.59 | 48 724.33 | 51 103.23 |
| 81 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 44 994.60 | 51 415.89 | 51 938.89 |
| 82 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.8) | 44 898.73 | 48 579.07 | 49 653.99 |
| 83 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 297.63 | 45 091.19 | 45 664.86 |
| 84 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 42 742.90 | 33 635.97 | 31 723.65 |
| 85 | javascript (14.15)| [koa](https://koajs.com) (2.13) | 41 524.16 | 44 217.62 | 43 366.57 |
| 86 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 41 235.35 | 45 498.35 | 46 216.04 |
| 87 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 40 274.26 | 134 105.00 | 121 703.42 |
| 88 | javascript (14.15)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 36 578.00 | 39 474.06 | 39 434.64 |
| 89 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 36 438.20 | 37 340.90 | 38 231.71 |
| 90 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 35 707.17 | 36 369.90 | 36 402.49 |
| 91 | swift (5.3)| [vapor](https://vapor.codes) (4.36) | 35 568.47 | 37 293.27 | 37 146.65 |
| 92 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 35 507.05 | 49 282.92 | 52 447.81 |
| 93 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 35 334.84 | 37 466.39 | 38 146.44 |
| 94 | javascript (14.15)| [moleculer](https://moleculer.services) (0.14) | 35 112.36 | 35 992.12 | 34 904.87 |
| 95 | python (3.8)| [emmett](https://emmett.sh) (2.1) | 35 075.46 | 41 028.93 | 41 391.63 |
| 96 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 34 970.14 | 34 487.31 | 34 698.92 |
| 97 | javascript (14.15)| [hapi](https://hapijs.com) (20.0) | 34 741.82 | 36 205.28 | 34 547.75 |
| 98 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 33 818.32 | 38 640.18 | 39 447.86 |
| 99 | python (3.8)| [hug](https://hug.rest) (2.6) | 32 936.81 | 35 300.53 | 52 775.29 |
| 100 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 32 680.67 | 36 432.02 | 36 535.27 |
| 101 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 32 016.42 | 27 588.79 | 23 484.93 |
| 102 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 31 967.21 | 31 544.27 | 30 913.13 |
| 103 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.12) | 31 858.37 | 35 295.91 | 35 915.86 |
| 104 | rust (1.49)| [nickel](https://nickel-org.github.io) (0.11) | 30 372.74 | 30 105.34 | 30 457.48 |
| 105 | javascript (14.15)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 30 119.86 | 31 429.27 | 31 964.02 |
| 106 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.3) | 29 908.39 | 33 564.32 | 34 414.06 |
| 107 | javascript (14.15)| [restify](https://restify.com) (8.5) | 29 517.72 | 30 559.92 | 30 257.99 |
| 108 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 29 045.90 | 32 132.83 | 32 134.43 |
| 109 | php (7.4)| [imi](https://imiphp.com) (1.2) | 28 438.98 | 32 069.54 | 33 583.72 |
| 110 | python (3.8)| [starlette](https://starlette.io) (0.14) | 27 864.98 | 31 572.82 | 32 075.75 |
| 111 | rust (1.49)| [salvo](https://github.com/kenorld/salvo) (0.3) | 27 814.64 | 29 622.17 | 29 858.28 |
| 112 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 27 794.19 | 29 196.22 | 29 400.97 |
| 113 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 27 172.15 | 29 487.47 | 28 607.55 |
| 114 | python (3.8)| [responder](https://python-responder.org) (2.0) | 26 539.06 | 31 066.67 | 32 203.11 |
| 115 | scala (2.13)| [play](https://playframework.com) (2.8) | 26 433.36 | 27 992.12 | 28 331.91 |
| 116 | rust (1.49)| [gotham](https://gotham.rs) (0.5) | 25 584.56 | 29 036.90 | 30 203.54 |
| 117 | python (3.8)| [index.py](https://index-py.abersheeran.com) (0.15) | 24 922.91 | 29 165.19 | 29 819.32 |
| 118 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 24 521.21 | 24 626.99 | 22 705.54 |
| 119 | javascript (14.15)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 24 366.90 | 26 767.32 | 26 031.17 |
| 120 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 24 069.79 | 26 101.09 | 25 530.71 |
| 121 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 22 672.59 | 21 689.14 | 20 718.02 |
| 122 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 22 437.01 | 27 839.97 | 27 641.05 |
| 123 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 22 322.51 | 21 643.88 | 21 161.50 |
| 124 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.16) | 21 746.95 | 20 639.40 | 18 408.47 |
| 125 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 21 267.66 | 21 147.01 | 20 788.15 |
| 126 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 20 383.64 | 19 995.23 | 20 600.39 |
| 127 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 19 830.41 | 23 002.45 | 23 585.24 |
| 128 | javascript (14.15)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 18 428.85 | 17 436.02 | 16 510.94 |
| 129 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 499.20 | 15 538.06 | 14 449.36 |
| 130 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 17 306.97 | 16 599.07 | 16 312.16 |
| 131 | rust (1.49)| [iron](https://ironframework.io) (0.6) | 17 221.36 | 16 532.79 | 16 435.87 |
| 132 | php (7.4)| [swoft](https://swoft.org) (2.0) | 16 963.95 | 20 019.25 | 21 027.54 |
| 133 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 16 683.40 | 16 408.33 | 15 845.96 |
| 134 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.63) | 16 611.68 | 21 346.27 | 21 022.76 |
| 135 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 16 347.40 | 19 312.66 | 19 944.56 |
| 136 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.39) | 15 982.09 | 15 579.35 | 15 251.27 |
| 137 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 15 979.92 | 14 088.53 | 13 225.52 |
| 138 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 15 951.39 | 20 062.47 | 19 863.81 |
| 139 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 15 911.88 | 18 957.84 | 18 971.04 |
| 140 | javascript (14.15)| [feathersjs](https://feathersjs.com) (4.5) | 15 677.86 | 17 519.63 | 17 689.77 |
| 141 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 15 675.20 | 20 901.47 | 20 671.99 |
| 142 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 442.96 | 17 970.94 | 18 063.35 |
| 143 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 15 396.77 | 18 132.01 | 17 901.16 |
| 144 | go (1.15)| [macaron](https://go-macaron.com) (1.4) | 15 341.39 | 16 980.40 | 16 940.47 |
| 145 | javascript (14.15)| [express](https://expressjs.com) (4.17) | 15 338.74 | 18 045.90 | 18 001.07 |
| 146 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 15 301.56 | 14 892.86 | 14 573.70 |
| 147 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 14 179.55 | 13 820.66 | 13 606.40 |
| 148 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 13 751.00 | 17 248.40 | 17 015.85 |
| 149 | java (11)| [struts2](https://struts.apache.org) (2.5) | 13 733.50 | 14 111.26 | 14 140.97 |
| 150 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 13 575.85 | 13 842.91 | 20 927.22 |
| 151 | ruby (2.7)| [rack_app](https://rack-app.com) (7.7) | 13 570.23 | 13 350.07 | 12 961.65 |
| 152 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 13 305.17 | 12 888.50 | 12 649.56 |
| 153 | java (11)| [blade](https://lets-blade.com) (2.0) | 13 024.24 | 15 585.98 | 14 342.14 |
| 154 | javascript (14.15)| [nestjs-express](https://nestjs.com) (7.6) | 11 999.77 | 11 733.02 | 12 829.29 |
| 155 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 433.29 | 11 717.79 | 11 747.05 |
| 156 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 11 327.76 | 11 175.32 | 10 875.38 |
| 157 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.4) | 10 192.40 | 10 438.42 | 10 530.84 |
| 158 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 10 062.81 | 10 347.43 | 10 408.77 |
| 159 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 863.06 | 10 057.91 | 10 019.77 |
| 160 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 9 734.72 | 9 966.64 | 10 111.82 |
| 161 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.6) | 9 660.54 | 9 718.71 | 9 877.49 |
| 162 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 565.19 | 9 216.15 | 9 254.82 |
| 163 | python (3.8)| [guillotina](https://guillotina.io) (6.0) | 9 259.35 | 8 950.39 | 8 866.10 |
| 164 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.1) | 9 135.31 | 8 807.82 | 8 789.34 |
| 165 | ruby (2.7)| [grape](https://ruby-grape.org) (1.5) | 8 716.57 | 8 396.84 | 8 364.79 |
| 166 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 8 626.55 | 13 357.61 | 12 496.45 |
| 167 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 7 511.39 | 7 215.29 | 6 896.71 |
| 168 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 6 771.26 | 6 714.91 | 6 638.35 |
| 169 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 654.42 | 6 587.86 | 6 515.90 |
| 170 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 6 567.54 | 6 507.76 | 6 356.71 |
| 171 | python (3.8)| [tornado](https://tornadoweb.org) (6.1) | 6 481.47 | 6 516.26 | 6 397.91 |
| 172 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 6 408.95 | 6 323.55 | 6 205.03 |
| 173 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 289.53 | 6 216.88 | 6 162.10 |
| 174 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 6 149.74 | 6 936.57 | 6 990.45 |
| 175 | python (3.8)| [django](https://djangoproject.com) (3.1) | 6 028.97 | 5 816.87 | 5 612.43 |
| 176 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 5 750.82 | 5 661.65 | 5 671.83 |
| 177 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 5 256.90 | 5 206.53 | 5 209.42 |
| 178 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 5 084.55 | 4 852.04 | 4 574.83 |
| 179 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.2) | 4 748.70 | 4 695.82 | 4 648.09 |
| 180 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 494.48 | 4 439.03 | 4 397.51 |
| 181 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 458.36 | 4 438.95 | 4 450.28 |
| 182 | javascript (14.15)| [sails](https://sailsjs.com) (1.4) | 4 257.93 | 4 262.72 | 4 185.78 |
| 183 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.2) | 4 010.77 | 3 945.66 | 3 920.23 |
| 184 | php (7.4)| [nette](https://nette.org/en/) (3.1) | 3 993.92 | 3 959.82 | 3 994.60 |
| 185 | php (7.4)| [lumen](https://lumen.laravel.com) (8.1) | 3 861.10 | 3 865.16 | 3 882.91 |
| 186 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.4) | 3 828.06 | 3 778.10 | 3 766.92 |
| 187 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 3 470.64 | 2 374.05 | 3 143.98 |
| 188 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 3 461.96 | 3 439.98 | 3 422.31 |
| 189 | ruby (2.7)| [rails](https://rubyonrails.org) (6.1) | 2 829.79 | 2 826.51 | 2 819.46 |
| 190 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 2 812.43 | 7 076.18 | 5 167.28 |
| 191 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 634.20 | 2 628.44 | 2 645.48 |
| 192 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 442.34 | 2 499.70 | 2 497.05 |
| 193 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.4) | 2 335.31 | 2 344.27 | 2 349.62 |
| 194 | php (7.4)| [symfony](https://symfony.com) (5.1) | 2 080.79 | 2 092.73 | 2 096.91 |
| 195 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 936.96 | 1 859.14 | 1 761.71 |
| 196 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 903.86 | 1 836.35 | 1 786.38 |
| 197 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 1 872.34 | 1 878.86 | 1 882.92 |
| 198 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 1 678.89 | 1 701.88 | 1 665.20 |
| 199 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 620.79 | 1 632.14 | 1 606.29 |
| 200 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 1 504.02 | 1 494.99 | 1 477.47 |
| 201 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 497.06 | 1 518.33 | 1 517.68 |
| 202 | python (3.8)| [django-ninja](https://django-ninja.rest-framework.com) (0.9) | 1 291.66 | 1 604.31 | 1 650.05 |
| 203 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 140.51 | 541.39 | 512.79 |
| 204 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 107.66 | 1 124.68 | 1 125.33 |
| 205 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 041.53 | 1 007.52 | 997.45 |
| 206 | cpp (11)| [nawa](https://github.com/jatofg/nawa) (0.6) | 537.20 | NaN | NaN |
| 207 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 286.82 | 303.38 | -85.75 |
| 208 | php (7.4)| [laravel](https://laravel.com) (7.27) | 137.10 | 62.96 | 0.00 |

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

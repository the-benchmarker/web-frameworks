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

:information_source:  Updated on **2021-01-08** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | java (11)| [activej](https://activej.io) (3.0.2) | 172 823.60 | 214 761.39 | 219 580.40 |
| 2 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 151 426.03 | 185 090.43 | 187 087.70 |
| 3 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 122 800.57 | 136 559.21 | 137 956.15 |
| 4 | go (1.15)| [fiber](https://gofiber.io) (2.3) | 122 579.51 | 129 700.78 | 129 155.02 |
| 5 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 120 817.59 | 123 697.17 | 123 259.10 |
| 6 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 118 445.73 | 147 203.55 | 149 595.43 |
| 7 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.5) | 117 965.55 | 129 288.70 | 128 691.91 |
| 8 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.19) | 117 253.45 | 131 158.63 | 131 336.31 |
| 9 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 117 212.59 | 126 863.54 | 127 007.58 |
| 10 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 116 556.38 | 128 665.30 | 128 536.87 |
| 11 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 115 199.62 | 143 193.92 | 145 937.23 |
| 12 | javascript (14.15)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.4) | 112 671.38 | 142 951.17 | 145 982.85 |
| 13 | java (11)| [undertow](https://undertow.io) (2.2) | 112 321.29 | 137 535.84 | 139 374.72 |
| 14 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 111 894.23 | 111 096.83 | 114 027.95 |
| 15 | java (11)| [jooby](https://jooby.io) (2.9) | 110 397.59 | 138 514.20 | 143 437.14 |
| 16 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 110 272.79 | 135 561.96 | 138 775.07 |
| 17 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 110 025.03 | 137 807.08 | 142 611.88 |
| 18 | javascript (14.15)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 108 680.85 | 138 029.79 | 141 170.51 |
| 19 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 108 570.58 | 105 316.12 | 109 597.72 |
| 20 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 107 582.73 | 135 416.26 | 139 447.23 |
| 21 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 106 998.01 | 131 684.69 | 135 364.68 |
| 22 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 104 481.41 | 122 138.11 | 124 265.26 |
| 23 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 99 933.31 | 121 474.22 | 122 158.81 |
| 24 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 99 451.79 | 121 368.94 | 122 354.76 |
| 25 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.4) | 99 072.31 | 121 045.53 | 121 618.02 |
| 26 | crystal (0.35)| [kemal](https://kemalcr.com) (0.27) | 96 325.37 | 112 960.76 | 112 602.67 |
| 27 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 95 962.78 | 140 241.50 | 151 015.90 |
| 28 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 94 790.84 | 116 258.18 | 119 054.76 |
| 29 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 93 991.90 | 114 573.46 | 114 895.12 |
| 30 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 88 406.46 | 105 809.43 | 105 885.75 |
| 31 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 82 722.00 | 96 882.09 | 95 315.29 |
| 32 | javascript (14.15)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 82 226.69 | 98 544.98 | 99 866.59 |
| 33 | go (1.15)| [gf](https://goframe.org) (1.15) | 81 115.20 | 88 883.94 | 91 248.93 |
| 34 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 81 034.12 | 81 968.09 | 84 137.84 |
| 35 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.12) | 80 802.35 | 93 670.83 | 89 839.17 |
| 36 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 79 578.74 | 80 954.35 | 82 877.05 |
| 37 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 79 470.53 | 80 408.77 | 82 682.01 |
| 38 | go (1.15)| [clevergo](https://clevergo.tech) (0.5) | 79 052.66 | 81 851.30 | 83 633.57 |
| 39 | c (11)| [kore](https://kore.io) (3.3) | 78 868.85 | 178 825.28 | 191 954.83 |
| 40 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 78 531.54 | 82 292.70 | 83 954.98 |
| 41 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 355.58 | 82 647.94 | 83 932.66 |
| 42 | go (1.15)| [chi](https://github.com/go-chi/chi) (1.5) | 77 632.66 | 76 812.52 | 79 520.69 |
| 43 | go (1.15)| [violetear](https://violetear.org) (7.0) | 77 072.01 | 77 273.99 | 79 495.90 |
| 44 | fsharp (5.0)| [falco](https://falcoframework.com) (3.0) | 75 217.15 | 88 350.70 | 91 398.87 |
| 45 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 75 122.54 | 117 371.42 | 131 269.35 |
| 46 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 75 059.61 | 79 180.16 | 79 488.68 |
| 47 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 74 592.35 | 73 977.23 | 76 307.49 |
| 48 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.2) | 74 247.79 | 73 181.20 | 74 310.58 |
| 49 | python (3.9)| [falcon](https://falconframework.org) (2.0) | 73 714.60 | 80 230.58 | 82 439.51 |
| 50 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 73 472.01 | 71 814.99 | 74 690.15 |
| 51 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 73 331.88 | 84 832.46 | 87 437.03 |
| 52 | java (11)| [restheart](https://restheart.org) (5.1) | 72 249.55 | 75 051.12 | 76 196.52 |
| 53 | javascript (14.15)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 71 866.68 | 78 533.33 | 79 506.57 |
| 54 | go (1.15)| [beego](https://beego.me) (1.12) | 71 725.73 | 74 564.56 | 76 614.17 |
| 55 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 71 418.34 | 82 812.50 | 85 269.34 |
| 56 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 65 541.93 | 73 702.43 | 73 984.24 |
| 57 | go (1.15)| [air](https://github.com/aofei/air) (0.21) | 65 124.09 | 63 664.02 | 66 329.94 |
| 58 | javascript (14.15)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 63 575.20 | 68 626.38 | 70 385.39 |
| 59 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.6) | 62 705.05 | 62 673.59 | 65 201.82 |
| 60 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 61 750.53 | 67 676.10 | 67 246.22 |
| 61 | scala (2.13)| [finch](https://finagle.github.io/finch/) (0.32) | 60 363.93 | 67 146.90 | 67 093.01 |
| 62 | javascript (14.15)| [0http](https://github.com/jkyberneees/0http) (3.0) | 59 930.74 | 66 549.75 | 67 660.22 |
| 63 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 59 040.09 | 62 754.74 | 63 199.07 |
| 64 | javascript (14.15)| [polka](https://github.com/lukeed/polka) (0.5) | 58 131.52 | 62 599.15 | 61 110.24 |
| 65 | rust (1.49)| [actix](https://actix.rs) (3.3) | 58 090.01 | 59 727.02 | 64 429.03 |
| 66 | javascript (14.15)| [restana](https://github.com/jkyberneees/ana) (4.7) | 57 852.44 | 63 876.17 | 64 490.55 |
| 67 | javascript (14.15)| [fastify](https://fastify.io) (3.7) | 55 783.30 | 60 248.28 | 59 148.88 |
| 68 | javascript (14.15)| [rayo](https://rayo.js.org) (1.3) | 55 530.95 | 61 233.03 | 60 093.53 |
| 69 | php (7.4)| [one](https://github.com/lizhichao/one) (2.2) | 55 179.52 | 62 097.88 | 69 319.45 |
| 70 | java (11)| [javalin](https://javalin.io) (3.9) | 54 718.92 | 57 600.38 | 58 181.84 |
| 71 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 53 905.42 | 56 802.28 | 55 263.54 |
| 72 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 53 632.85 | 62 210.23 | 64 736.62 |
| 73 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.14) | 53 561.88 | 75 823.74 | 83 681.70 |
| 74 | javascript (14.15)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 53 425.14 | 58 762.58 | 57 039.63 |
| 75 | python (3.9)| [pyramid](https://trypyramid.com) (1.1) | 53 038.17 | 58 405.90 | 58 716.15 |
| 76 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 51 003.60 | 66 276.60 | 68 587.81 |
| 77 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 50 725.37 | 57 497.03 | 57 595.12 |
| 78 | java (11)| [micronaut](https://micronaut.io) (1.2) | 50 583.19 | 56 862.90 | 57 076.13 |
| 79 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 50 317.41 | 55 342.54 | 62 114.72 |
| 80 | java (11)| [spark](https://sparkjava.com) (2.9) | 49 546.39 | 54 287.52 | 55 428.17 |
| 81 | javascript (14.15)| [nestjs-fastify](https://nestjs.com) (7.6) | 48 054.73 | 52 353.09 | 50 417.14 |
| 82 | python (3.9)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 46 598.67 | 51 729.66 | 52 843.22 |
| 83 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 46 300.01 | 46 609.66 | 49 642.18 |
| 84 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 46 224.55 | 48 741.06 | 51 256.90 |
| 85 | javascript (14.15)| [foxify](https://foxify.js.org) (0.1) | 45 447.52 | 48 371.52 | 48 166.27 |
| 86 | javascript (14.15)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 45 381.85 | 48 329.16 | 47 200.26 |
| 87 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.8) | 45 105.34 | 48 586.00 | 49 416.48 |
| 88 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 317.46 | 45 253.77 | 45 081.03 |
| 89 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 43 020.68 | 32 819.78 | 30 794.83 |
| 90 | python (3.9)| [bottle](https://bottlepy.org) (0.12) | 42 245.13 | 45 387.40 | 46 225.29 |
| 91 | javascript (14.15)| [koa](https://koajs.com) (2.13) | 41 934.01 | 44 947.05 | 44 086.66 |
| 92 | javascript (14.15)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 37 802.35 | 39 655.65 | 39 337.73 |
| 93 | rust (1.49)| [nickel](https://nickel-org.github.io) (0.11) | 37 299.50 | 36 338.06 | 34 909.14 |
| 94 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 36 291.76 | 37 429.40 | 37 883.52 |
| 95 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 35 892.25 | 35 202.42 | 34 515.76 |
| 96 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 35 349.61 | 37 380.42 | 38 111.84 |
| 97 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 34 871.63 | 51 312.61 | 55 581.09 |
| 98 | swift (5.3)| [vapor](https://vapor.codes) (4.37) | 34 803.73 | 36 675.21 | 36 419.77 |
| 99 | javascript (14.15)| [moleculer](https://moleculer.services) (0.14) | 34 729.77 | 35 937.32 | 34 707.95 |
| 100 | javascript (14.15)| [hapi](https://hapijs.com) (20.0) | 34 602.29 | 35 697.72 | 34 318.95 |
| 101 | python (3.9)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 34 569.14 | 37 999.14 | 39 527.23 |
| 102 | python (3.9)| [emmett](https://emmett.sh) (2.1) | 34 413.50 | 41 375.61 | 41 768.10 |
| 103 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 34 287.52 | 35 126.55 | 35 122.50 |
| 104 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 33 766.42 | 38 722.71 | 39 482.43 |
| 105 | python (3.9)| [hug](https://hug.rest) (2.6) | 32 713.07 | 35 708.37 | 53 913.57 |
| 106 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 32 020.13 | 28 267.89 | 24 346.44 |
| 107 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 31 277.80 | 31 475.10 | 30 683.78 |
| 108 | python (3.9)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.3) | 30 631.96 | 33 045.71 | 33 908.46 |
| 109 | php (7.4)| [imi](https://imiphp.com) (1.2) | 30 323.08 | 33 613.69 | 33 541.01 |
| 110 | python (3.9)| [sanic](https://github.com/huge-success/sanic) (20.12) | 29 858.79 | 34 246.33 | 35 107.79 |
| 111 | python (3.9)| [starlette](https://starlette.io) (0.14) | 29 089.62 | 32 037.27 | 32 633.10 |
| 112 | javascript (14.15)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 29 058.47 | 30 920.99 | 46 817.95 |
| 113 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 28 613.61 | 31 723.49 | 31 655.12 |
| 114 | javascript (14.15)| [restify](https://restify.com) (8.5) | 28 604.08 | 30 403.26 | 29 555.75 |
| 115 | rust (1.49)| [salvo](https://github.com/kenorld/salvo) (0.3) | 28 085.94 | 29 819.61 | 29 713.76 |
| 116 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 27 946.33 | 29 046.83 | 28 706.34 |
| 117 | javascript (14.15)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 27 812.74 | 29 367.77 | 28 320.23 |
| 118 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 27 063.89 | 29 057.04 | 28 698.49 |
| 119 | rust (1.49)| [gotham](https://gotham.rs) (0.5) | 25 707.32 | 28 887.21 | 30 044.64 |
| 120 | scala (2.13)| [play](https://playframework.com) (2.8) | 25 666.91 | 28 385.88 | 28 312.76 |
| 121 | python (3.9)| [index.py](https://index-py.abersheeran.com) (0.15) | 25 115.38 | 29 916.82 | 30 523.94 |
| 122 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 24 908.81 | 24 256.64 | 22 806.58 |
| 123 | python (3.9)| [responder](https://python-responder.org) (2.0) | 24 361.61 | 31 454.15 | 31 710.31 |
| 124 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 24 269.22 | 26 183.10 | 25 626.09 |
| 125 | python (3.9)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 292.98 | 28 275.13 | 28 369.92 |
| 126 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 23 225.89 | 21 136.71 | 20 851.34 |
| 127 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 22 205.44 | 22 000.65 | 21 679.82 |
| 128 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 20 932.79 | 19 921.06 | 20 669.27 |
| 129 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.16) | 20 880.39 | 21 323.63 | 19 410.14 |
| 130 | python (3.9)| [molten](https://moltenframework.com) (1.0) | 19 453.61 | 22 386.93 | 22 222.66 |
| 131 | javascript (14.15)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 18 911.29 | 18 374.48 | 16 742.40 |
| 132 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 623.82 | 15 661.58 | 14 553.87 |
| 133 | php (7.4)| [swoft](https://swoft.org) (2.0) | 17 089.89 | 20 795.00 | 21 012.69 |
| 134 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 17 069.10 | 16 486.08 | 16 144.96 |
| 135 | python (3.9)| [fastapi](https://fastapi.tiangolo.com) (0.63) | 16 760.67 | 22 415.25 | 22 473.89 |
| 136 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 16 686.87 | 16 098.10 | 15 843.52 |
| 137 | python (3.9)| [masonite](https://masoniteproject.com) (2.3) | 16 475.94 | 17 681.76 | 20 434.29 |
| 138 | python (3.9)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 16 387.97 | 21 128.84 | 21 127.65 |
| 139 | rust (1.49)| [iron](https://ironframework.io) (0.6) | 16 344.72 | 16 434.61 | 16 694.91 |
| 140 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 16 309.64 | 19 496.92 | 19 889.73 |
| 141 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 16 172.33 | 14 348.73 | 13 309.85 |
| 142 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.39) | 15 992.35 | 15 625.58 | 15 309.34 |
| 143 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 15 846.15 | 18 263.61 | 18 879.49 |
| 144 | go (1.15)| [macaron](https://go-macaron.com) (1.4) | 15 298.65 | 16 869.83 | 16 821.66 |
| 145 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 15 262.44 | 14 743.40 | 14 671.47 |
| 146 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 15 218.79 | 17 704.42 | 18 035.14 |
| 147 | javascript (14.15)| [express](https://expressjs.com) (4.17) | 15 207.18 | 18 021.00 | 17 868.56 |
| 148 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 156.33 | 17 300.24 | 17 976.78 |
| 149 | javascript (14.15)| [feathersjs](https://feathersjs.com) (4.5) | 14 551.65 | 17 361.17 | 17 219.32 |
| 150 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 14 171.86 | 13 804.31 | 13 534.83 |
| 151 | ruby (2.7)| [rack_app](https://rack-app.com) (7.7) | 13 815.23 | 13 385.07 | 13 138.83 |
| 152 | java (11)| [struts2](https://struts.apache.org) (2.5) | 13 789.99 | 14 083.30 | 14 135.96 |
| 153 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 13 617.87 | 13 934.88 | 20 362.13 |
| 154 | python (3.9)| [flask](https://flask.pocoo.org) (1.1) | 13 446.16 | 13 463.72 | 17 289.17 |
| 155 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 13 232.91 | 12 852.76 | 12 641.53 |
| 156 | java (11)| [blade](https://lets-blade.com) (2.0) | 13 103.10 | 15 542.98 | 14 591.07 |
| 157 | javascript (14.15)| [nestjs-express](https://nestjs.com) (7.6) | 12 122.85 | 12 995.47 | 12 263.46 |
| 158 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 384.26 | 11 681.37 | 11 735.59 |
| 159 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 11 317.27 | 11 050.56 | 10 830.98 |
| 160 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.5) | 10 143.09 | 10 453.99 | 10 472.25 |
| 161 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 10 077.82 | 10 262.10 | 10 435.57 |
| 162 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 868.31 | 10 100.14 | 10 032.60 |
| 163 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 9 727.47 | 9 981.16 | 10 024.76 |
| 164 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.7) | 9 585.50 | 9 632.16 | 9 762.87 |
| 165 | python (3.9)| [guillotina](https://guillotina.io) (6.0) | 9 239.25 | 9 325.74 | 8 937.74 |
| 166 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.1) | 9 080.10 | 8 812.52 | 8 828.35 |
| 167 | ruby (2.7)| [grape](https://ruby-grape.org) (1.5) | 8 711.15 | 8 437.69 | 8 336.63 |
| 168 | python (3.9)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 8 693.42 | 8 364.96 | 8 236.70 |
| 169 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 8 678.99 | 13 326.48 | 12 518.33 |
| 170 | python (3.9)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 7 483.89 | 7 114.82 | 6 767.83 |
| 171 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 6 694.54 | 6 635.39 | 6 561.67 |
| 172 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 641.75 | 6 561.16 | 6 501.44 |
| 173 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 6 561.07 | 6 513.16 | 6 359.44 |
| 174 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 6 398.66 | 6 326.18 | 6 236.84 |
| 175 | python (3.9)| [tornado](https://tornadoweb.org) (6.1) | 6 388.23 | 6 554.62 | 6 718.03 |
| 176 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 253.21 | 6 178.58 | 6 154.45 |
| 177 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 6 163.90 | 6 933.39 | 6 934.57 |
| 178 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 5 758.37 | 5 651.52 | 5 659.39 |
| 179 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 5 283.14 | 5 233.64 | 5 174.24 |
| 180 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 5 254.20 | 5 202.29 | 5 222.99 |
| 181 | python (3.9)| [django](https://djangoproject.com) (3.1) | 5 186.74 | 5 124.81 | 5 282.28 |
| 182 | python (3.9)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 5 101.04 | 4 916.61 | 4 575.69 |
| 183 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.2) | 4 736.38 | 4 671.17 | 4 602.46 |
| 184 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 468.39 | 4 413.93 | 4 366.32 |
| 185 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 465.58 | 4 420.40 | 4 437.84 |
| 186 | javascript (14.15)| [sails](https://sailsjs.com) (1.4) | 4 284.91 | 4 280.29 | 4 204.69 |
| 187 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 4 052.61 | 2 769.96 | 2 108.85 |
| 188 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.3) | 3 990.73 | 3 911.56 | 3 904.90 |
| 189 | php (7.4)| [nette](https://nette.org/en/) (3.1) | 3 879.20 | 3 860.83 | 3 885.79 |
| 190 | php (7.4)| [lumen](https://lumen.laravel.com) (8.1) | 3 854.86 | 3 849.14 | 3 884.44 |
| 191 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.5) | 3 835.48 | 3 778.36 | 3 778.11 |
| 192 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.2) | 3 454.74 | 3 411.08 | 3 422.93 |
| 193 | ruby (2.7)| [rails](https://rubyonrails.org) (6.1) | 2 825.31 | 2 798.95 | 2 812.84 |
| 194 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 631.10 | 2 626.57 | 2 625.19 |
| 195 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 460.01 | 2 545.99 | 2 526.57 |
| 196 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.5) | 2 341.53 | 2 336.42 | 2 355.89 |
| 197 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 2 303.22 | 7 017.88 | 5 178.71 |
| 198 | php (7.4)| [symfony](https://symfony.com) (5.1) | 2 077.81 | 2 096.26 | 2 099.74 |
| 199 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 939.88 | 1 848.20 | 1 784.88 |
| 200 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 876.52 | 1 807.67 | 1 790.66 |
| 201 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 1 865.74 | 1 873.71 | 1 875.91 |
| 202 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 1 736.37 | 1 743.65 | 1 759.91 |
| 203 | python (3.9)| [cyclone](https://cyclone.io) (1.3) | 1 672.20 | 1 648.29 | 1 637.20 |
| 204 | python (3.9)| [klein](https://github.com/twisted/klein) (20.6) | 1 618.06 | 1 638.10 | 1 600.07 |
| 205 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 497.67 | 1 510.07 | 1 518.76 |
| 206 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 407.73 | 645.34 | 562.51 |
| 207 | python (3.9)| [django-ninja](https://django-ninja.rest-framework.com) (0.9) | 1 351.51 | 1 639.65 | 1 673.65 |
| 208 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 104.47 | 1 121.21 | 1 118.86 |
| 209 | python (3.9)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 048.30 | 1 012.78 | 1 010.98 |
| 210 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 287.60 | 304.16 | -86.41 |
| 211 | cpp (11)| [nawa](https://github.com/jatofg/nawa) (0.6) | 248.02 | NaN | NaN |
| 212 | php (7.4)| [laravel](https://laravel.com) (7.27) | 126.86 | 33.89 | 4.11 |

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

# Which is the fastest?

![CI](https://github.com/the-benchmarker/web-frameworks/workflows/CI/badge.svg)

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

:information_source:  Updated on **2021-01-22** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | java (11)| [activej](https://activej.io) (3.0) | 172 874.98 | 214 039.81 | 219 335.33 |
| 2 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 149 415.01 | 182 618.83 | 184 760.71 |
| 3 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 122 745.29 | 135 463.52 | 136 443.44 |
| 4 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 121 267.09 | 124 517.21 | 123 998.74 |
| 5 | go (1.15)| [fiber](https://gofiber.io) (2.3) | 120 760.89 | 129 919.56 | 128 975.11 |
| 6 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 117 678.99 | 146 365.75 | 149 138.24 |
| 7 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.6) | 117 400.97 | 128 555.26 | 127 466.93 |
| 8 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.19) | 116 313.79 | 130 394.06 | 130 416.34 |
| 9 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 116 185.27 | 128 662.89 | 128 222.12 |
| 10 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 115 811.83 | 128 681.38 | 128 586.42 |
| 11 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 114 332.56 | 142 037.79 | 145 188.64 |
| 12 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 112 645.03 | 112 137.52 | 115 331.38 |
| 13 | java (11)| [undertow](https://undertow.io) (2.2) | 112 639.77 | 137 587.26 | 139 710.77 |
| 14 | clojure (1.1)| [donkey](https://github.com/AppsFlyer/donkey) (0.4) | 111 357.38 | 113 087.65 | 115 268.66 |
| 15 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 110 960.06 | 134 924.68 | 138 165.85 |
| 16 | javascript (14.15)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.4) | 110 757.65 | 141 765.32 | 145 820.12 |
| 17 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 110 221.65 | 137 018.57 | 142 192.92 |
| 18 | java (11)| [jooby](https://jooby.io) (2.9) | 109 641.33 | 138 111.93 | 142 905.99 |
| 19 | javascript (14.15)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 108 572.79 | 137 911.33 | 140 563.89 |
| 20 | c (11)| [kore](https://kore.io) (3.3) | 108 005.26 | 180 300.55 | 191 658.61 |
| 21 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 108 002.73 | 104 385.33 | 109 127.99 |
| 22 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 107 291.67 | 133 436.81 | 137 234.30 |
| 23 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 106 325.77 | 132 009.27 | 134 976.40 |
| 24 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 101 226.43 | 120 595.41 | 120 917.12 |
| 25 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 99 151.16 | 121 889.20 | 121 693.74 |
| 26 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.4) | 97 137.72 | 120 543.93 | 120 587.11 |
| 27 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 95 449.05 | 138 517.02 | 149 045.59 |
| 28 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 95 362.93 | 117 108.58 | 120 092.15 |
| 29 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 92 815.25 | 112 177.58 | 112 402.70 |
| 30 | crystal (0.35)| [kemal](https://kemalcr.com) (0.27) | 92 313.46 | 110 926.57 | 111 017.48 |
| 31 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 87 688.89 | 105 291.75 | 104 455.97 |
| 32 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.12) | 82 733.36 | 93 818.27 | 88 964.96 |
| 33 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 82 499.24 | 108 614.45 | 111 623.77 |
| 34 | javascript (14.15)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 81 810.97 | 98 502.83 | 99 727.76 |
| 35 | scala (2.13)| [akkahttp](https://akka.io) (10.2) | 81 677.79 | 95 543.52 | 94 140.89 |
| 36 | go (1.15)| [gf](https://goframe.org) (1.15) | 81 402.66 | 88 756.74 | 91 415.31 |
| 37 | go (1.15)| [clevergo](https://clevergo.tech) (0.5) | 81 078.29 | 82 399.33 | 84 275.82 |
| 38 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 80 958.49 | 82 047.54 | 83 991.58 |
| 39 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 80 272.16 | 81 564.24 | 83 424.42 |
| 40 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 79 856.10 | 80 798.72 | 82 911.60 |
| 41 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 78 568.55 | 82 569.19 | 83 936.20 |
| 42 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 498.07 | 83 115.24 | 84 245.62 |
| 43 | go (1.15)| [chi](https://github.com/go-chi/chi) (1.5) | 77 751.33 | 77 136.78 | 79 916.47 |
| 44 | go (1.15)| [violetear](https://violetear.org) (7.0) | 77 018.19 | 77 283.21 | 79 288.02 |
| 45 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 75 641.81 | 88 714.30 | 91 428.05 |
| 46 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 75 299.14 | 79 233.17 | 79 806.89 |
| 47 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 74 849.68 | 74 643.97 | 76 720.90 |
| 48 | java (11)| [restheart](https://restheart.org) (5.1) | 74 398.46 | 76 679.88 | 77 709.47 |
| 49 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 73 163.62 | 71 537.23 | 74 317.48 |
| 50 | python (3.9)| [falcon](https://falconframework.org) (2.0) | 73 061.79 | 80 611.19 | 82 022.82 |
| 51 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 72 971.47 | 84 328.31 | 87 031.20 |
| 52 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.2) | 71 967.47 | 65 907.68 | 64 892.53 |
| 53 | fsharp (5.0)| [falco](https://falcoframework.com) (3.0) | 71 944.09 | 83 099.20 | 85 522.53 |
| 54 | go (1.15)| [beego](https://beego.me) (1.12) | 71 848.84 | 74 648.97 | 76 654.62 |
| 55 | javascript (14.15)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 70 175.64 | 78 754.32 | 79 569.76 |
| 56 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 67 367.02 | 129 803.72 | 129 518.76 |
| 57 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 65 115.67 | 73 141.87 | 73 409.75 |
| 58 | go (1.15)| [air](https://github.com/aofei/air) (0.21) | 65 013.94 | 63 591.61 | 66 118.69 |
| 59 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.6) | 62 866.38 | 62 739.04 | 65 441.23 |
| 60 | javascript (14.15)| [0http](https://github.com/jkyberneees/0http) (3.0) | 61 490.07 | 66 243.77 | 67 454.30 |
| 61 | scala (2.13)| [finch](https://finagle.github.io/finch/) (0.32) | 60 382.10 | 67 389.77 | 67 277.02 |
| 62 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 59 668.74 | 62 940.24 | 63 663.19 |
| 63 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 59 346.72 | 64 485.74 | 63 581.03 |
| 64 | javascript (14.15)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 58 765.86 | 67 613.86 | 69 056.33 |
| 65 | javascript (14.15)| [polka](https://github.com/lukeed/polka) (0.5) | 57 803.42 | 62 332.73 | 61 211.39 |
| 66 | javascript (14.15)| [restana](https://github.com/jkyberneees/ana) (4.7) | 56 573.82 | 63 578.67 | 64 693.22 |
| 67 | javascript (14.15)| [rayo](https://rayo.js.org) (1.3) | 55 855.37 | 60 789.16 | 59 451.39 |
| 68 | javascript (14.15)| [fastify](https://fastify.io) (3.10) | 54 731.78 | 59 228.80 | 58 120.92 |
| 69 | php (7.4)| [one](https://github.com/lizhichao/one) (2.2) | 54 003.54 | 60 155.67 | 69 861.95 |
| 70 | javascript (14.15)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 53 393.32 | 58 881.06 | 57 055.15 |
| 71 | python (3.9)| [pyramid](https://trypyramid.com) (1.1) | 53 301.53 | 58 303.62 | 58 930.98 |
| 72 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 53 226.88 | 55 721.14 | 54 535.84 |
| 73 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.14) | 52 550.72 | 76 339.36 | 83 436.76 |
| 74 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 52 291.97 | 60 234.82 | 62 622.75 |
| 75 | php (7.4)| [hyperf](https://hyperf.io) (2.1) | 51 965.30 | 63 456.65 | 64 847.68 |
| 76 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 51 728.89 | 57 062.22 | 64 154.32 |
| 77 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 50 646.28 | 57 365.15 | 57 721.85 |
| 78 | java (11)| [javalin](https://javalin.io) (3.9) | 50 483.51 | 54 580.79 | 54 742.99 |
| 79 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 50 137.35 | 65 715.76 | 68 729.04 |
| 80 | java (11)| [spark](https://sparkjava.com) (2.9) | 49 863.96 | 54 475.77 | 55 683.15 |
| 81 | java (11)| [micronaut](https://micronaut.io) (1.2) | 49 284.20 | 57 493.62 | 57 243.89 |
| 82 | javascript (14.15)| [nestjs-fastify](https://nestjs.com) (7.6) | 48 600.74 | 51 674.11 | 50 609.60 |
| 83 | rust (1.49)| [actix](https://actix.rs) (3.3) | 48 003.86 | 50 697.37 | 50 414.53 |
| 84 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 46 206.70 | 48 913.76 | 50 442.04 |
| 85 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 46 168.37 | 46 713.99 | 49 774.92 |
| 86 | javascript (14.15)| [foxify](https://foxify.js.org) (0.1) | 45 131.05 | 49 467.97 | 47 950.19 |
| 87 | python (3.9)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 45 006.13 | 51 096.95 | 52 889.91 |
| 88 | javascript (14.15)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 44 812.19 | 48 789.49 | 48 371.21 |
| 89 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 756.53 | 45 810.95 | 46 040.69 |
| 90 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.8) | 44 480.60 | 47 996.07 | 48 941.23 |
| 91 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 43 058.04 | 32 304.72 | 30 842.56 |
| 92 | python (3.9)| [bottle](https://bottlepy.org) (0.12) | 42 853.26 | 44 964.34 | 46 232.34 |
| 93 | javascript (14.15)| [koa](https://koajs.com) (2.13) | 40 242.52 | 44 044.32 | 42 876.92 |
| 94 | javascript (14.15)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 37 063.45 | 39 510.81 | 38 981.11 |
| 95 | python (3.9)| [emmett](https://emmett.sh) (2.1) | 36 993.55 | 41 429.92 | 42 408.80 |
| 96 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 36 371.91 | 37 636.49 | 37 587.04 |
| 97 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 36 015.25 | 35 010.91 | 35 022.60 |
| 98 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 35 521.68 | 35 506.62 | 35 462.83 |
| 99 | javascript (14.15)| [hapi](https://hapijs.com) (20.0) | 34 791.84 | 35 485.86 | 34 317.71 |
| 100 | javascript (14.15)| [moleculer](https://moleculer.services) (0.14) | 34 736.54 | 35 772.06 | 34 686.40 |
| 101 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.3) | 34 562.47 | 37 068.64 | 36 744.62 |
| 102 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 34 512.95 | 50 012.16 | 53 663.66 |
| 103 | swift (5.3)| [vapor](https://vapor.codes) (4.38) | 34 199.88 | 36 411.15 | 36 050.07 |
| 104 | rust (1.49)| [nickel](https://nickel-org.github.io) (0.11) | 34 100.96 | 34 514.93 | 33 340.84 |
| 105 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 33 883.69 | 34 798.06 | 33 191.16 |
| 106 | python (3.9)| [hug](https://hug.rest) (2.6) | 33 272.32 | 35 299.08 | 35 586.93 |
| 107 | java (11)| [jersey3-grizzly2](https://eclipse-ee4j.github.io/jersey) (3) | 33 253.98 | 38 280.15 | 37 869.76 |
| 108 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 33 237.87 | 37 679.81 | 38 199.73 |
| 109 | java (11)| [jersey-grizzly2](https://eclipse-ee4j.github.io/jersey) (2.33) | 33 126.27 | 38 158.80 | 37 742.53 |
| 110 | python (3.9)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 32 791.16 | 38 668.46 | 39 594.98 |
| 111 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 32 539.34 | 28 246.55 | 25 229.38 |
| 112 | rust (1.49)| [gotham](https://gotham.rs) (0.5) | 32 104.24 | 35 524.07 | 36 513.38 |
| 113 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 31 257.27 | 31 196.21 | 30 876.48 |
| 114 | python (3.9)| [sanic](https://github.com/huge-success/sanic) (20.12) | 30 982.00 | 33 597.06 | 34 484.73 |
| 115 | rust (1.49)| [salvo](https://github.com/kenorld/salvo) (0.5) | 30 482.71 | 32 366.39 | 32 990.26 |
| 116 | python (3.9)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.3) | 30 049.56 | 34 284.53 | 34 577.99 |
| 117 | javascript (14.15)| [restify](https://restify.com) (8.5) | 30 039.10 | 30 511.67 | 29 575.56 |
| 118 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 28 920.28 | 31 862.60 | 31 735.90 |
| 119 | python (3.9)| [starlette](https://starlette.io) (0.14) | 28 047.81 | 32 338.76 | 32 643.54 |
| 120 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 28 026.15 | 31 878.20 | 33 138.83 |
| 121 | javascript (14.15)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 27 500.59 | 29 414.37 | 29 879.50 |
| 122 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 26 957.98 | 29 093.94 | 28 527.28 |
| 123 | scala (2.13)| [play](https://playframework.com) (2.8) | 26 720.72 | 28 454.53 | 28 310.29 |
| 124 | python (3.9)| [index.py](https://index-py.abersheeran.com) (0.16) | 26 030.01 | 29 601.10 | 30 794.73 |
| 125 | python (3.9)| [responder](https://python-responder.org) (2.0) | 25 483.58 | 30 414.82 | 31 234.91 |
| 126 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 24 252.70 | 23 387.60 | 20 138.83 |
| 127 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 24 214.10 | 26 088.18 | 25 561.19 |
| 128 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 22 868.22 | 21 771.92 | 20 591.00 |
| 129 | python (3.9)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 22 189.47 | 28 527.64 | 28 337.38 |
| 130 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.16) | 22 098.47 | 21 106.55 | 19 423.77 |
| 131 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 22 074.66 | 21 834.77 | 21 594.71 |
| 132 | clojure (1.1)| [luminus](https://luminusweb.com) (3.91) | 21 885.66 | 21 083.59 | 18 659.93 |
| 133 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 21 158.60 | 20 578.11 | 20 258.44 |
| 134 | javascript (14.15)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 21 113.18 | 22 906.42 | 22 426.61 |
| 135 | python (3.9)| [molten](https://moltenframework.com) (1.0) | 19 498.00 | 22 123.98 | 21 899.97 |
| 136 | javascript (14.15)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 18 707.07 | 17 570.21 | 16 840.46 |
| 137 | python (3.9)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 18 523.55 | 23 872.60 | 24 068.37 |
| 138 | python (3.9)| [fastapi](https://fastapi.tiangolo.com) (0.63) | 18 069.67 | 21 975.72 | 21 920.76 |
| 139 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 558.22 | 15 722.48 | 14 727.92 |
| 140 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 17 186.48 | 16 730.40 | 16 260.52 |
| 141 | rust (1.49)| [iron](https://ironframework.io) (0.6) | 17 055.03 | 16 788.04 | 16 931.80 |
| 142 | php (7.4)| [swoft](https://swoft.org) (2.0) | 16 921.95 | 20 519.81 | 21 331.31 |
| 143 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 16 628.25 | 16 010.17 | 15 801.52 |
| 144 | python (3.9)| [masonite](https://masoniteproject.com) (2.3) | 16 228.90 | 20 279.31 | 20 208.65 |
| 145 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 16 150.75 | 14 290.21 | 13 243.37 |
| 146 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.40) | 15 995.95 | 15 669.07 | 15 258.80 |
| 147 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.3) | 15 755.89 | 18 554.46 | 19 010.29 |
| 148 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 399.67 | 17 924.84 | 18 240.25 |
| 149 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 15 384.59 | 14 818.63 | 14 606.48 |
| 150 | go (1.15)| [macaron](https://go-macaron.com) (1.4) | 15 380.88 | 16 930.64 | 16 884.56 |
| 151 | javascript (14.15)| [feathersjs](https://feathersjs.com) (4.5) | 15 155.74 | 18 032.73 | 18 065.92 |
| 152 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 15 068.82 | 17 359.22 | 17 669.76 |
| 153 | javascript (14.15)| [express](https://expressjs.com) (4.17) | 15 015.73 | 18 078.91 | 17 476.24 |
| 154 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 14 175.69 | 13 766.93 | 13 511.93 |
| 155 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 13 948.63 | 13 969.32 | 14 460.17 |
| 156 | java (11)| [struts2](https://struts.apache.org) (2.5) | 13 917.74 | 14 198.26 | 14 251.73 |
| 157 | python (3.9)| [flask](https://flask.pocoo.org) (1.1) | 13 816.83 | 15 391.80 | 17 152.78 |
| 158 | ruby (2.7)| [rack_app](https://rack-app.com) (7.7) | 13 766.93 | 13 345.86 | 13 103.59 |
| 159 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 13 280.24 | 12 791.02 | 12 627.69 |
| 160 | java (11)| [blade](https://lets-blade.com) (2.0) | 13 036.94 | 15 297.77 | 14 424.41 |
| 161 | javascript (14.15)| [nestjs-express](https://nestjs.com) (7.6) | 11 817.02 | 11 742.27 | 11 531.21 |
| 162 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 373.31 | 11 604.28 | 11 625.10 |
| 163 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 11 056.47 | 10 853.99 | 10 659.16 |
| 164 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.5) | 10 224.14 | 10 472.95 | 10 557.53 |
| 165 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.3) | 9 999.56 | 10 263.18 | 10 358.44 |
| 166 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 810.75 | 9 996.68 | 9 937.54 |
| 167 | python (3.9)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 789.74 | 9 651.19 | 9 507.05 |
| 168 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 9 783.73 | 10 013.59 | 10 080.87 |
| 169 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.7) | 9 614.24 | 9 727.76 | 9 873.65 |
| 170 | python (3.9)| [guillotina](https://guillotina.io) (6.1) | 9 225.95 | 9 579.87 | 8 847.15 |
| 171 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.1) | 9 091.10 | 8 854.24 | 8 818.27 |
| 172 | ruby (2.7)| [grape](https://ruby-grape.org) (1.5) | 8 722.40 | 8 406.15 | 8 345.13 |
| 173 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 7 780.14 | 13 129.07 | 12 468.31 |
| 174 | python (3.9)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 7 519.78 | 7 193.65 | 6 939.15 |
| 175 | python (3.9)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 408.60 | 7 071.57 | 6 519.12 |
| 176 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 6 780.91 | 6 719.60 | 6 663.73 |
| 177 | python (3.9)| [tornado](https://tornadoweb.org) (6.1) | 6 672.67 | 6 644.60 | 6 406.59 |
| 178 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 575.50 | 6 512.06 | 6 459.26 |
| 179 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 6 503.07 | 6 431.66 | 6 303.40 |
| 180 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 6 320.91 | 6 283.11 | 6 156.90 |
| 181 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 213.95 | 6 137.08 | 6 112.87 |
| 182 | python (3.9)| [django](https://djangoproject.com) (3.1) | 6 127.34 | 5 826.39 | 5 699.35 |
| 183 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 5 983.98 | 6 689.29 | 6 672.45 |
| 184 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 5 707.67 | 5 621.63 | 5 645.54 |
| 185 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 5 276.98 | 5 181.00 | 5 235.03 |
| 186 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 5 220.60 | 5 194.92 | 5 142.64 |
| 187 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.2) | 4 670.50 | 4 620.34 | 4 584.17 |
| 188 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 443.39 | 4 393.56 | 4 355.21 |
| 189 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 438.80 | 4 408.98 | 4 408.56 |
| 190 | javascript (14.15)| [sails](https://sailsjs.com) (1.4) | 4 296.13 | 4 252.85 | 4 252.99 |
| 191 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 4 164.45 | 2 268.26 | 2 267.63 |
| 192 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.3) | 3 956.47 | 3 899.55 | 3 886.76 |
| 193 | php (7.4)| [nette](https://nette.org/en/) (3.1) | 3 862.24 | 3 853.15 | 3 866.96 |
| 194 | php (7.4)| [lumen](https://lumen.laravel.com) (8.1) | 3 827.48 | 3 836.92 | 3 863.93 |
| 195 | julia (1.6)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 791.22 | 6 792.09 | 5 757.87 |
| 196 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.5) | 3 780.90 | 3 722.21 | 3 722.77 |
| 197 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.3) | 3 423.68 | 3 396.54 | 3 386.92 |
| 198 | php (7.4)| [symfony](https://symfony.com) (5.2) | 2 974.99 | 2 993.67 | 3 010.97 |
| 199 | ruby (2.7)| [rails](https://rubyonrails.org) (6.1) | 2 819.34 | 2 813.50 | 2 797.32 |
| 200 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 618.36 | 2 594.91 | 2 617.84 |
| 201 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.5) | 2 297.32 | 2 286.11 | 2 316.50 |
| 202 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 239.44 | 2 572.37 | 2 537.41 |
| 203 | php (7.4)| [antidot](https://antidotfw.io) (0.1) | 2 116.52 | 2 139.31 | 1 665.30 |
| 204 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 934.74 | 1 850.81 | 1 854.20 |
| 205 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 931.24 | 1 837.16 | 1 761.46 |
| 206 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 1 898.41 | 1 956.18 | 1 944.38 |
| 207 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 1 859.32 | 1 856.95 | 1 862.77 |
| 208 | php (7.4)| [driftphp](https://github.com/driftphp/driftphp) (0.1) | 1 825.75 | 1 787.27 | 1 393.16 |
| 209 | python (3.9)| [klein](https://github.com/twisted/klein) (20.6) | 1 662.78 | 1 661.79 | 1 627.73 |
| 210 | python (3.9)| [cyclone](https://cyclone.io) (1.3) | 1 659.70 | 1 657.21 | 1 631.34 |
| 211 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 582.62 | 1 591.30 | 1 603.03 |
| 212 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 361.73 | 646.32 | 532.78 |
| 213 | python (3.9)| [django-ninja](https://django-ninja.rest-framework.com) (0.10) | 1 317.94 | 1 624.75 | 1 653.77 |
| 214 | python (3.9)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 245.27 | 1 176.47 | 1 182.44 |
| 215 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 092.27 | 1 109.07 | 1 107.24 |
| 216 | php (7.4)| [laravel](https://laravel.com) (8.24) | 904.68 | 909.29 | 907.74 |
| 217 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 287.60 | 302.64 | -86.01 |
| 218 | cpp (11)| [nawa](https://github.com/jatofg/nawa) (0.6) | 265.70 | NaN | NaN |

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

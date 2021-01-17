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

:information_source:  Updated on **2021-01-17** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | java (11)| [activej](https://activej.io) (3.0) | 172 707.44 | 214 890.18 | 219 744.98 |
| 2 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 149 332.03 | 182 293.46 | 184 213.32 |
| 3 | go (1.15)| [fiber](https://gofiber.io) (2.3) | 124 034.97 | 130 614.65 | 129 324.78 |
| 4 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 123 757.24 | 136 064.52 | 137 880.79 |
| 5 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 121 772.68 | 124 237.05 | 123 921.64 |
| 6 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.6) | 121 554.85 | 128 250.04 | 127 874.20 |
| 7 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 117 943.58 | 147 049.87 | 150 010.63 |
| 8 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.19) | 117 344.91 | 131 044.91 | 130 701.85 |
| 9 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 117 328.30 | 129 133.86 | 128 237.47 |
| 10 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 116 731.77 | 129 131.17 | 128 560.06 |
| 11 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 114 408.51 | 142 772.26 | 146 158.64 |
| 12 | java (11)| [undertow](https://undertow.io) (2.2) | 113 509.91 | 137 070.97 | 139 057.71 |
| 13 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 112 964.46 | 112 046.14 | 115 070.07 |
| 14 | javascript (14.15)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.4) | 110 635.24 | 142 131.49 | 145 550.78 |
| 15 | java (11)| [jooby](https://jooby.io) (2.9) | 110 511.00 | 138 168.81 | 143 168.67 |
| 16 | javascript (14.15)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 109 425.11 | 138 716.78 | 141 282.22 |
| 17 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 109 306.53 | 136 912.56 | 141 340.98 |
| 18 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 108 832.07 | 105 112.24 | 109 555.75 |
| 19 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 108 641.45 | 135 521.99 | 138 271.52 |
| 20 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 107 404.73 | 134 553.20 | 138 843.42 |
| 21 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 106 458.98 | 132 205.61 | 135 465.77 |
| 22 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 99 372.68 | 122 053.72 | 122 833.48 |
| 23 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 98 957.92 | 122 283.31 | 121 965.94 |
| 24 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.4) | 97 654.57 | 118 162.21 | 119 059.41 |
| 25 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 95 113.91 | 114 117.62 | 113 711.31 |
| 26 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 94 528.25 | 117 174.97 | 120 253.95 |
| 27 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 93 247.28 | 140 133.69 | 151 046.37 |
| 28 | crystal (0.35)| [kemal](https://kemalcr.com) (0.27) | 92 028.20 | 111 169.18 | 111 464.03 |
| 29 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 90 209.64 | 107 711.69 | 108 607.57 |
| 30 | c (11)| [kore](https://kore.io) (3.3) | 84 051.96 | 192 805.96 | 195 003.53 |
| 31 | javascript (14.15)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 82 602.07 | 98 617.81 | 100 004.50 |
| 32 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.12) | 82 275.53 | 94 324.39 | 89 443.98 |
| 33 | go (1.15)| [gf](https://goframe.org) (1.15) | 81 381.58 | 88 996.84 | 91 519.72 |
| 34 | go (1.15)| [clevergo](https://clevergo.tech) (0.5) | 81 305.48 | 82 442.51 | 84 515.01 |
| 35 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 81 254.84 | 82 217.54 | 84 163.10 |
| 36 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 80 260.00 | 96 331.32 | 94 006.38 |
| 37 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 80 195.17 | 81 160.53 | 83 094.38 |
| 38 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 79 848.17 | 80 593.68 | 82 800.00 |
| 39 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 806.38 | 83 626.66 | 84 200.64 |
| 40 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 78 394.86 | 101 721.62 | 111 722.64 |
| 41 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 78 303.89 | 82 305.94 | 83 864.82 |
| 42 | go (1.15)| [chi](https://github.com/go-chi/chi) (1.5) | 77 639.43 | 77 119.11 | 79 611.10 |
| 43 | go (1.15)| [violetear](https://violetear.org) (7.0) | 77 139.89 | 77 367.03 | 79 342.71 |
| 44 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 75 586.62 | 88 097.91 | 90 553.02 |
| 45 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 75 233.28 | 74 853.92 | 76 820.58 |
| 46 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 75 127.18 | 78 853.97 | 79 594.89 |
| 47 | fsharp (5.0)| [falco](https://falcoframework.com) (3.0) | 74 883.87 | 87 806.74 | 90 716.07 |
| 48 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 73 298.88 | 71 372.60 | 74 314.34 |
| 49 | java (11)| [restheart](https://restheart.org) (5.1) | 73 138.26 | 75 683.80 | 71 844.68 |
| 50 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.2) | 73 054.27 | 65 460.65 | 65 280.30 |
| 51 | python (3.9)| [falcon](https://falconframework.org) (2.0) | 72 933.09 | 80 292.15 | 81 868.50 |
| 52 | go (1.15)| [beego](https://beego.me) (1.12) | 71 648.69 | 74 518.49 | 76 458.39 |
| 53 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 71 630.65 | 83 075.68 | 85 245.44 |
| 54 | javascript (14.15)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 69 981.05 | 79 327.23 | 80 070.77 |
| 55 | go (1.15)| [air](https://github.com/aofei/air) (0.21) | 65 069.94 | 63 620.86 | 66 098.23 |
| 56 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 64 504.97 | 71 697.23 | 72 070.47 |
| 57 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.6) | 63 183.13 | 63 023.84 | 65 795.67 |
| 58 | javascript (14.15)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 62 678.24 | 68 236.42 | 69 662.12 |
| 59 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 61 965.50 | 67 775.08 | 67 390.56 |
| 60 | scala (2.13)| [finch](https://finagle.github.io/finch/) (0.32) | 60 731.63 | 67 367.21 | 67 430.24 |
| 61 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 59 767.47 | 63 159.55 | 64 252.15 |
| 62 | javascript (14.15)| [polka](https://github.com/lukeed/polka) (0.5) | 57 463.74 | 62 721.68 | 62 507.72 |
| 63 | javascript (14.15)| [0http](https://github.com/jkyberneees/0http) (3.0) | 56 920.58 | 65 161.11 | 66 135.59 |
| 64 | javascript (14.15)| [restana](https://github.com/jkyberneees/ana) (4.7) | 56 902.11 | 64 215.81 | 64 510.04 |
| 65 | javascript (14.15)| [rayo](https://rayo.js.org) (1.3) | 55 041.94 | 60 526.72 | 58 891.01 |
| 66 | python (3.9)| [pyramid](https://trypyramid.com) (1.1) | 54 858.88 | 57 996.36 | 58 523.79 |
| 67 | javascript (14.15)| [fastify](https://fastify.io) (3.10) | 54 593.66 | 59 570.32 | 58 264.98 |
| 68 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 54 095.85 | 57 341.32 | 56 033.68 |
| 69 | javascript (14.15)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 54 077.76 | 58 354.32 | 57 894.32 |
| 70 | php (7.4)| [hyperf](https://hyperf.io) (2.1) | 54 021.74 | 65 431.81 | 67 307.22 |
| 71 | java (11)| [javalin](https://javalin.io) (3.9) | 53 823.91 | 57 240.70 | 57 639.55 |
| 72 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.14) | 53 534.55 | 75 960.85 | 83 828.15 |
| 73 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 52 767.25 | 61 023.45 | 63 593.46 |
| 74 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 51 999.16 | 57 236.56 | 64 233.63 |
| 75 | php (7.4)| [one](https://github.com/lizhichao/one) (2.2) | 51 436.95 | 63 935.01 | 69 389.07 |
| 76 | rust (1.49)| [actix](https://actix.rs) (3.3) | 51 408.40 | 50 081.05 | 52 281.82 |
| 77 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 50 626.85 | 66 492.81 | 68 321.08 |
| 78 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 50 503.87 | 56 820.16 | 57 404.43 |
| 79 | java (11)| [spark](https://sparkjava.com) (2.9) | 50 117.57 | 54 565.92 | 55 646.64 |
| 80 | java (11)| [micronaut](https://micronaut.io) (1.2) | 49 874.11 | 55 692.10 | 56 097.22 |
| 81 | javascript (14.15)| [nestjs-fastify](https://nestjs.com) (7.6) | 47 794.47 | 51 623.21 | 51 015.57 |
| 82 | python (3.9)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 46 494.06 | 51 076.33 | 52 739.60 |
| 83 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 46 242.94 | 46 661.25 | 49 844.66 |
| 84 | javascript (14.15)| [foxify](https://foxify.js.org) (0.1) | 45 632.77 | 48 661.71 | 48 324.42 |
| 85 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 45 205.29 | 49 902.60 | 51 610.07 |
| 86 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 897.19 | 45 814.45 | 46 282.68 |
| 87 | javascript (14.15)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 44 763.40 | 48 289.89 | 47 157.49 |
| 88 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.8) | 43 861.95 | 48 234.59 | 49 238.57 |
| 89 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 43 045.35 | 34 223.56 | 31 628.31 |
| 90 | javascript (14.15)| [koa](https://koajs.com) (2.13) | 41 975.13 | 44 243.68 | 43 840.28 |
| 91 | python (3.9)| [bottle](https://bottlepy.org) (0.12) | 41 874.43 | 45 291.39 | 46 115.76 |
| 92 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 40 349.70 | 133 000.90 | 136 151.53 |
| 93 | javascript (14.15)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 37 463.79 | 39 720.62 | 38 928.95 |
| 94 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 36 582.11 | 37 723.05 | 37 932.74 |
| 95 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 36 371.44 | 36 691.76 | 36 437.74 |
| 96 | javascript (14.15)| [moleculer](https://moleculer.services) (0.14) | 35 501.12 | 35 982.74 | 35 202.50 |
| 97 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 35 308.08 | 35 621.17 | 35 529.46 |
| 98 | swift (5.3)| [vapor](https://vapor.codes) (4.37) | 35 202.57 | 36 815.13 | 36 497.01 |
| 99 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.3) | 34 975.99 | 37 456.59 | 37 613.16 |
| 100 | rust (1.49)| [nickel](https://nickel-org.github.io) (0.11) | 34 938.81 | 32 385.34 | 33 752.18 |
| 101 | python (3.9)| [emmett](https://emmett.sh) (2.1) | 34 684.62 | 40 968.29 | 41 908.48 |
| 102 | javascript (14.15)| [hapi](https://hapijs.com) (20.0) | 34 561.33 | 34 855.40 | 34 174.41 |
| 103 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 34 512.93 | 49 796.30 | 54 192.19 |
| 104 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 33 858.12 | 34 897.86 | 33 547.05 |
| 105 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 33 577.94 | 37 910.36 | 38 368.75 |
| 106 | python (3.9)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 33 136.81 | 38 396.43 | 39 119.94 |
| 107 | java (11)| [jersey-grizzly2](https://eclipse-ee4j.github.io/jersey) (2.33) | 33 130.22 | 37 551.68 | 37 019.09 |
| 108 | java (11)| [jersey3-grizzly2](https://eclipse-ee4j.github.io/jersey) (3) | 33 056.81 | 37 835.44 | 37 133.28 |
| 109 | python (3.9)| [hug](https://hug.rest) (2.6) | 32 718.42 | 35 508.35 | 53 206.40 |
| 110 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 32 355.82 | 28 747.13 | 25 014.33 |
| 111 | python (3.9)| [sanic](https://github.com/huge-success/sanic) (20.12) | 31 853.76 | 34 645.06 | 35 208.57 |
| 112 | rust (1.49)| [gotham](https://gotham.rs) (0.5) | 31 392.87 | 35 426.88 | 36 371.53 |
| 113 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 31 280.27 | 31 353.20 | 31 065.37 |
| 114 | rust (1.49)| [salvo](https://github.com/kenorld/salvo) (0.3) | 30 218.54 | 32 357.55 | 33 008.64 |
| 115 | python (3.9)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.3) | 29 596.48 | 34 062.43 | 34 865.72 |
| 116 | javascript (14.15)| [restify](https://restify.com) (8.5) | 29 352.40 | 30 616.73 | 29 466.32 |
| 117 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 29 042.52 | 32 057.58 | 31 929.07 |
| 118 | javascript (14.15)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 27 604.46 | 29 566.83 | 30 938.98 |
| 119 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 27 553.47 | 31 474.48 | 32 151.62 |
| 120 | python (3.9)| [starlette](https://starlette.io) (0.14) | 27 528.81 | 32 168.33 | 32 483.43 |
| 121 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 27 085.31 | 29 183.23 | 27 635.74 |
| 122 | scala (2.13)| [play](https://playframework.com) (2.8) | 26 488.29 | 28 415.33 | 27 802.52 |
| 123 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 25 016.08 | 24 112.59 | 20 233.43 |
| 124 | python (3.9)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 24 860.74 | 28 344.85 | 27 883.72 |
| 125 | python (3.9)| [responder](https://python-responder.org) (2.0) | 24 839.84 | 31 975.43 | 32 204.87 |
| 126 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 23 881.08 | 26 387.26 | 25 493.42 |
| 127 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 22 671.18 | 21 306.61 | 20 637.88 |
| 128 | python (3.9)| [index.py](https://index-py.abersheeran.com) (0.15) | 22 445.14 | 27 394.96 | 27 511.91 |
| 129 | clojure (1.1)| [luminus](https://luminusweb.com) (3.91) | 22 207.00 | 20 941.85 | 21 008.42 |
| 130 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 21 920.21 | 21 795.16 | 21 655.87 |
| 131 | javascript (14.15)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 21 743.50 | 24 021.60 | 23 393.72 |
| 132 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 21 026.93 | 20 217.76 | 20 817.70 |
| 133 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.16) | 21 011.37 | 20 612.74 | 17 980.60 |
| 134 | python (3.9)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 19 772.22 | 23 191.09 | 23 721.95 |
| 135 | javascript (14.15)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 18 871.72 | 18 181.48 | 16 599.74 |
| 136 | python (3.9)| [molten](https://moltenframework.com) (1.0) | 18 725.93 | 22 180.17 | 22 100.51 |
| 137 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 577.19 | 15 630.36 | 14 615.84 |
| 138 | python (3.9)| [fastapi](https://fastapi.tiangolo.com) (0.63) | 17 390.16 | 22 004.76 | 22 313.85 |
| 139 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 17 190.10 | 16 788.81 | 16 347.41 |
| 140 | php (7.4)| [swoft](https://swoft.org) (2.0) | 16 919.18 | 20 372.44 | 21 199.23 |
| 141 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 16 676.99 | 16 172.93 | 15 751.77 |
| 142 | javascript (14.15)| [express](https://expressjs.com) (4.17) | 16 508.74 | 18 084.93 | 17 567.52 |
| 143 | rust (1.49)| [iron](https://ironframework.io) (0.6) | 16 486.29 | 16 470.10 | 16 873.85 |
| 144 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.40) | 16 084.45 | 15 466.76 | 15 272.70 |
| 145 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 15 894.76 | 14 210.52 | 13 271.42 |
| 146 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.3) | 15 669.22 | 18 469.86 | 18 805.34 |
| 147 | go (1.15)| [macaron](https://go-macaron.com) (1.4) | 15 454.59 | 16 853.49 | 16 846.05 |
| 148 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 15 309.06 | 14 905.51 | 14 583.62 |
| 149 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 279.41 | 17 598.80 | 18 053.46 |
| 150 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 15 121.14 | 17 574.21 | 17 468.22 |
| 151 | python (3.9)| [masonite](https://masoniteproject.com) (2.3) | 14 939.56 | 18 295.72 | 16 075.81 |
| 152 | javascript (14.15)| [feathersjs](https://feathersjs.com) (4.5) | 14 200.83 | 17 793.67 | 17 400.99 |
| 153 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 14 176.56 | 13 745.33 | 13 574.15 |
| 154 | java (11)| [struts2](https://struts.apache.org) (2.5) | 13 830.38 | 14 077.06 | 14 098.53 |
| 155 | ruby (2.7)| [rack_app](https://rack-app.com) (7.7) | 13 801.89 | 13 353.88 | 13 143.04 |
| 156 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 13 452.28 | 14 114.04 | 21 635.74 |
| 157 | python (3.9)| [flask](https://flask.pocoo.org) (1.1) | 13 413.16 | 17 383.44 | 17 236.79 |
| 158 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 13 264.12 | 12 952.73 | 12 666.44 |
| 159 | java (11)| [blade](https://lets-blade.com) (2.0) | 12 906.39 | 15 832.65 | 14 732.99 |
| 160 | javascript (14.15)| [nestjs-express](https://nestjs.com) (7.6) | 11 622.06 | 12 764.03 | 11 311.76 |
| 161 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 439.57 | 11 701.95 | 11 725.52 |
| 162 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 10 919.49 | 10 722.06 | 10 320.90 |
| 163 | python (3.9)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 10 458.91 | 9 674.22 | 9 643.55 |
| 164 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.5) | 10 218.72 | 10 417.60 | 10 512.49 |
| 165 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.3) | 10 076.26 | 10 252.58 | 10 306.15 |
| 166 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 927.94 | 10 108.55 | 10 067.01 |
| 167 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 9 739.83 | 9 953.51 | 10 103.82 |
| 168 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.7) | 9 624.65 | 9 824.94 | 9 847.39 |
| 169 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.1) | 9 102.87 | 8 825.45 | 8 817.48 |
| 170 | python (3.9)| [guillotina](https://guillotina.io) (6.1) | 8 743.39 | 9 168.00 | 8 727.32 |
| 171 | ruby (2.7)| [grape](https://ruby-grape.org) (1.5) | 8 707.50 | 8 383.39 | 8 328.30 |
| 172 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 7 770.10 | 13 548.09 | 12 778.27 |
| 173 | python (3.9)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 7 483.28 | 7 292.96 | 6 816.09 |
| 174 | python (3.9)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 482.84 | 7 083.56 | 6 511.14 |
| 175 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 6 752.07 | 6 683.08 | 6 625.65 |
| 176 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 573.65 | 6 490.21 | 6 437.67 |
| 177 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 6 499.08 | 6 483.41 | 6 328.56 |
| 178 | python (3.9)| [tornado](https://tornadoweb.org) (6.1) | 6 491.30 | 6 474.01 | 6 359.81 |
| 179 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 6 330.70 | 6 278.31 | 6 168.14 |
| 180 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 215.15 | 6 147.03 | 6 115.83 |
| 181 | python (3.9)| [django](https://djangoproject.com) (3.1) | 6 157.26 | 6 113.17 | 5 737.10 |
| 182 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 6 064.27 | 6 758.37 | 6 811.31 |
| 183 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 5 794.50 | 5 644.78 | 5 681.63 |
| 184 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 5 249.39 | 5 233.08 | 5 126.22 |
| 185 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 5 150.95 | 5 102.14 | 5 132.42 |
| 186 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.2) | 4 696.48 | 4 643.04 | 4 593.21 |
| 187 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 453.40 | 4 409.42 | 4 370.92 |
| 188 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 431.80 | 4 414.89 | 4 431.45 |
| 189 | javascript (14.15)| [sails](https://sailsjs.com) (1.4) | 4 245.68 | 4 220.09 | 4 171.44 |
| 190 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.3) | 3 951.81 | 3 901.08 | 3 895.42 |
| 191 | php (7.4)| [nette](https://nette.org/en/) (3.1) | 3 865.92 | 3 853.85 | 3 872.87 |
| 192 | php (7.4)| [lumen](https://lumen.laravel.com) (8.1) | 3 830.53 | 3 828.64 | 3 856.84 |
| 193 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.5) | 3 804.59 | 3 752.03 | 3 754.43 |
| 194 | julia (1.6)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 605.60 | 6 985.17 | 5 889.56 |
| 195 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.3) | 3 424.85 | 3 387.68 | 3 394.35 |
| 196 | php (7.4)| [symfony](https://symfony.com) (5.2) | 2 959.48 | 2 974.59 | 2 994.76 |
| 197 | ruby (2.7)| [rails](https://rubyonrails.org) (6.1) | 2 772.16 | 2 751.88 | 2 749.41 |
| 198 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 609.38 | 2 598.03 | 2 614.87 |
| 199 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.5) | 2 277.01 | 2 267.77 | 2 285.83 |
| 200 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 2 247.82 | 573.34 | 560.47 |
| 201 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 221.50 | 2 546.35 | 2 525.74 |
| 202 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 926.16 | 1 844.66 | 1 760.83 |
| 203 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 891.64 | 1 833.05 | 1 815.73 |
| 204 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 1 858.74 | 1 861.64 | 1 867.13 |
| 205 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 1 835.56 | 1 826.23 | 1 828.82 |
| 206 | python (3.9)| [cyclone](https://cyclone.io) (1.3) | 1 657.18 | 1 644.84 | 1 629.69 |
| 207 | python (3.9)| [klein](https://github.com/twisted/klein) (20.6) | 1 627.08 | 1 642.40 | 1 602.22 |
| 208 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 567.07 | 1 569.63 | 1 586.29 |
| 209 | python (3.9)| [django-ninja](https://django-ninja.rest-framework.com) (0.10) | 1 336.15 | 1 625.25 | 1 664.94 |
| 210 | python (3.9)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 239.86 | 1 165.31 | 1 181.47 |
| 211 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 081.47 | 1 099.94 | 1 098.29 |
| 212 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 560.79 | 1 211.76 | 1 450.12 |
| 213 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 288.37 | 302.28 | -83.77 |
| 214 | cpp (11)| [nawa](https://github.com/jatofg/nawa) (0.6) | 259.93 | NaN | NaN |
| 215 | php (7.4)| [laravel](https://laravel.com) (7.27) | 133.59 | 41.28 | 4.49 |

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

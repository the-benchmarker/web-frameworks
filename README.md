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

:information_source:  Updated on **2021-01-12** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | java (11)| [activej](https://activej.io) (3.0) | 174 565.75 | 215 212.42 | 220 533.33 |
| 2 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 149 474.56 | 182 197.29 | 184 381.58 |
| 3 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 123 303.60 | 135 644.48 | 136 855.47 |
| 4 | go (1.15)| [fiber](https://gofiber.io) (2.3) | 122 010.02 | 129 704.20 | 128 884.87 |
| 5 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 121 289.64 | 124 518.26 | 124 101.14 |
| 6 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.6) | 118 216.22 | 128 219.57 | 127 675.62 |
| 7 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 117 608.57 | 146 009.40 | 149 333.10 |
| 8 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.19) | 116 552.92 | 129 788.59 | 130 066.37 |
| 9 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 116 238.66 | 128 879.43 | 127 957.80 |
| 10 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 116 171.34 | 128 259.76 | 128 064.90 |
| 11 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 114 055.92 | 142 111.15 | 144 922.77 |
| 12 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 112 476.43 | 112 096.81 | 115 324.35 |
| 13 | java (11)| [undertow](https://undertow.io) (2.2) | 111 986.69 | 136 832.04 | 139 167.96 |
| 14 | javascript (14.15)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.4) | 110 559.15 | 142 186.32 | 145 380.54 |
| 15 | java (11)| [jooby](https://jooby.io) (2.9) | 110 026.65 | 137 214.78 | 141 795.02 |
| 16 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 109 920.25 | 134 886.85 | 137 359.38 |
| 17 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 109 325.38 | 135 888.02 | 140 650.55 |
| 18 | javascript (14.15)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 108 493.71 | 138 407.47 | 140 971.89 |
| 19 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 108 389.02 | 104 763.11 | 109 389.18 |
| 20 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 107 439.48 | 134 350.83 | 138 649.20 |
| 21 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 103 960.08 | 129 315.23 | 132 733.83 |
| 22 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.4) | 102 138.61 | 119 737.39 | 120 048.68 |
| 23 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 98 121.89 | 120 395.37 | 121 114.13 |
| 24 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 97 947.14 | 120 372.30 | 121 130.91 |
| 25 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 95 577.52 | 117 531.67 | 119 823.99 |
| 26 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 95 295.76 | 139 958.45 | 152 046.32 |
| 27 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 93 904.70 | 113 931.75 | 112 728.52 |
| 28 | crystal (0.35)| [kemal](https://kemalcr.com) (0.27) | 92 639.94 | 112 264.51 | 112 881.53 |
| 29 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 90 768.45 | 107 013.22 | 106 483.12 |
| 30 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.12) | 83 325.23 | 94 078.78 | 88 766.66 |
| 31 | javascript (14.15)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 83 151.04 | 97 354.61 | 99 932.09 |
| 32 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 81 661.98 | 96 607.59 | 94 342.06 |
| 33 | go (1.15)| [gf](https://goframe.org) (1.15) | 81 449.26 | 88 932.24 | 91 674.66 |
| 34 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 81 212.61 | 82 065.19 | 84 199.10 |
| 35 | go (1.15)| [clevergo](https://clevergo.tech) (0.5) | 80 897.20 | 82 008.15 | 83 863.05 |
| 36 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 79 973.66 | 81 081.43 | 83 128.18 |
| 37 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 79 804.71 | 80 475.18 | 82 649.97 |
| 38 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 525.28 | 82 948.41 | 84 212.09 |
| 39 | go (1.15)| [chi](https://github.com/go-chi/chi) (1.5) | 78 035.93 | 77 448.37 | 79 889.45 |
| 40 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 78 020.97 | 81 801.80 | 83 442.40 |
| 41 | go (1.15)| [violetear](https://violetear.org) (7.0) | 77 378.20 | 77 586.79 | 79 618.80 |
| 42 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 75 770.06 | 88 452.75 | 91 014.46 |
| 43 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.2) | 75 615.09 | 73 257.03 | 76 630.03 |
| 44 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 75 147.73 | 74 618.90 | 76 890.70 |
| 45 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 75 070.76 | 88 292.89 | 91 254.49 |
| 46 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 74 910.37 | 79 215.94 | 79 768.08 |
| 47 | fsharp (5.0)| [falco](https://falcoframework.com) (3.0) | 74 328.91 | 86 717.89 | 89 461.59 |
| 48 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 74 237.07 | 115 427.51 | 129 030.08 |
| 49 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 73 657.82 | 71 818.46 | 74 903.90 |
| 50 | python (3.9)| [falcon](https://falconframework.org) (2.0) | 73 378.87 | 81 092.93 | 82 463.23 |
| 51 | javascript (14.15)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 72 923.69 | 79 633.57 | 79 769.04 |
| 52 | java (11)| [restheart](https://restheart.org) (5.1) | 72 210.81 | 72 038.53 | 66 562.03 |
| 53 | go (1.15)| [beego](https://beego.me) (1.12) | 71 735.71 | 74 788.65 | 76 623.65 |
| 54 | go (1.15)| [air](https://github.com/aofei/air) (0.21) | 64 950.11 | 63 455.70 | 66 017.04 |
| 55 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 64 563.76 | 72 056.71 | 72 523.15 |
| 56 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.6) | 62 946.27 | 62 781.70 | 65 482.78 |
| 57 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 62 219.61 | 68 005.62 | 67 334.50 |
| 58 | javascript (14.15)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 61 972.58 | 67 894.81 | 68 626.77 |
| 59 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 60 057.03 | 63 485.78 | 64 028.32 |
| 60 | javascript (14.15)| [0http](https://github.com/jkyberneees/0http) (3.0) | 59 624.69 | 65 281.89 | 67 552.24 |
| 61 | scala (2.13)| [finch](https://finagle.github.io/finch/) (0.32) | 59 524.15 | 66 134.61 | 66 094.63 |
| 62 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 57 177.52 | 119 724.02 | 141 341.03 |
| 63 | javascript (14.15)| [polka](https://github.com/lukeed/polka) (0.5) | 56 315.40 | 61 528.76 | 60 523.26 |
| 64 | javascript (14.15)| [restana](https://github.com/jkyberneees/ana) (4.7) | 56 161.99 | 63 596.85 | 64 368.53 |
| 65 | javascript (14.15)| [rayo](https://rayo.js.org) (1.3) | 55 601.01 | 60 789.91 | 59 236.72 |
| 66 | javascript (14.15)| [fastify](https://fastify.io) (3.7) | 55 561.10 | 60 667.82 | 59 640.99 |
| 67 | php (7.4)| [one](https://github.com/lizhichao/one) (2.2) | 55 448.32 | 66 725.65 | 70 912.36 |
| 68 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.14) | 54 982.65 | 75 351.72 | 83 956.33 |
| 69 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 54 231.86 | 57 463.89 | 55 333.83 |
| 70 | javascript (14.15)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 54 178.35 | 59 463.55 | 57 053.71 |
| 71 | java (11)| [javalin](https://javalin.io) (3.9) | 53 593.32 | 57 549.60 | 57 478.44 |
| 72 | python (3.9)| [pyramid](https://trypyramid.com) (1.1) | 53 204.25 | 58 217.38 | 58 619.60 |
| 73 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 52 491.38 | 60 984.45 | 63 139.53 |
| 74 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 51 978.74 | 57 226.16 | 64 415.18 |
| 75 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 51 031.47 | 66 382.89 | 68 915.04 |
| 76 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 50 635.48 | 57 209.90 | 57 663.86 |
| 77 | rust (1.49)| [actix](https://actix.rs) (3.3) | 49 790.97 | 47 982.24 | 48 838.02 |
| 78 | java (11)| [spark](https://sparkjava.com) (2.9) | 49 707.71 | 54 470.35 | 55 452.41 |
| 79 | java (11)| [micronaut](https://micronaut.io) (1.2) | 48 940.98 | 55 674.56 | 56 058.67 |
| 80 | javascript (14.15)| [nestjs-fastify](https://nestjs.com) (7.6) | 47 275.54 | 52 222.35 | 50 675.63 |
| 81 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 46 304.07 | 49 978.90 | 51 570.65 |
| 82 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 46 208.21 | 46 625.75 | 49 623.08 |
| 83 | python (3.9)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 45 449.20 | 51 357.27 | 51 938.19 |
| 84 | javascript (14.15)| [foxify](https://foxify.js.org) (0.1) | 45 421.87 | 49 150.11 | 47 390.07 |
| 85 | javascript (14.15)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 44 988.79 | 48 370.87 | 47 435.23 |
| 86 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 944.16 | 45 358.91 | 45 620.82 |
| 87 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.8) | 44 505.07 | 48 283.44 | 49 030.90 |
| 88 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 43 606.65 | 31 036.06 | 33 630.35 |
| 89 | python (3.9)| [bottle](https://bottlepy.org) (0.12) | 41 952.18 | 45 586.75 | 46 502.05 |
| 90 | javascript (14.15)| [koa](https://koajs.com) (2.13) | 41 639.93 | 43 779.26 | 43 168.43 |
| 91 | javascript (14.15)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 37 306.84 | 39 863.93 | 39 405.58 |
| 92 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 36 320.45 | 37 258.71 | 37 741.66 |
| 93 | swift (5.3)| [vapor](https://vapor.codes) (4.37) | 35 861.91 | 37 575.40 | 37 317.92 |
| 94 | python (3.9)| [emmett](https://emmett.sh) (2.1) | 35 390.05 | 41 703.59 | 42 352.29 |
| 95 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.3) | 35 390.00 | 37 415.72 | 37 731.77 |
| 96 | javascript (14.15)| [moleculer](https://moleculer.services) (0.14) | 35 368.12 | 36 098.18 | 34 929.33 |
| 97 | javascript (14.15)| [hapi](https://hapijs.com) (20.0) | 34 874.05 | 35 235.93 | 34 640.59 |
| 98 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 34 338.14 | 34 718.30 | 34 997.24 |
| 99 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 34 283.32 | 35 108.33 | 34 727.86 |
| 100 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 34 201.59 | 50 493.06 | 52 417.68 |
| 101 | rust (1.49)| [nickel](https://nickel-org.github.io) (0.11) | 33 492.58 | 32 720.21 | 32 692.21 |
| 102 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 33 422.27 | 38 303.93 | 38 303.27 |
| 103 | java (11)| [jersey-grizzly2](https://eclipse-ee4j.github.io/jersey) (2.33) | 33 207.56 | 37 604.53 | 37 262.65 |
| 104 | java (11)| [jersey3-grizzly2](https://eclipse-ee4j.github.io/jersey) (3) | 33 083.14 | 38 055.34 | 37 478.78 |
| 105 | python (3.9)| [hug](https://hug.rest) (2.6) | 32 735.46 | 35 723.52 | 53 636.86 |
| 106 | rust (1.49)| [gotham](https://gotham.rs) (0.5) | 31 664.51 | 35 456.97 | 36 524.16 |
| 107 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 31 248.13 | 30 596.16 | 30 552.46 |
| 108 | python (3.9)| [sanic](https://github.com/huge-success/sanic) (20.12) | 30 897.85 | 35 305.04 | 35 481.74 |
| 109 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 30 885.95 | 28 569.77 | 25 066.82 |
| 110 | python (3.9)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 30 501.90 | 35 019.03 | 35 580.94 |
| 111 | javascript (14.15)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 30 476.85 | 31 959.06 | 32 402.16 |
| 112 | rust (1.49)| [salvo](https://github.com/kenorld/salvo) (0.3) | 30 305.14 | 32 478.24 | 32 962.32 |
| 113 | php (7.4)| [imi](https://imiphp.com) (1.2) | 29 159.10 | 31 598.34 | 33 103.88 |
| 114 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 29 027.74 | 32 047.81 | 31 962.65 |
| 115 | javascript (14.15)| [restify](https://restify.com) (8.5) | 28 646.13 | 30 382.35 | 29 562.41 |
| 116 | python (3.9)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.3) | 28 241.25 | 33 550.29 | 34 467.36 |
| 117 | javascript (14.15)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 28 092.60 | 29 456.08 | 28 836.14 |
| 118 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 27 869.19 | 29 023.13 | 28 682.46 |
| 119 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 26 897.82 | 29 198.05 | 28 615.72 |
| 120 | python (3.9)| [responder](https://python-responder.org) (2.0) | 26 690.53 | 31 526.14 | 31 765.47 |
| 121 | python (3.9)| [index.py](https://index-py.abersheeran.com) (0.15) | 25 429.61 | 30 246.64 | 30 448.12 |
| 122 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 24 750.27 | 25 430.91 | 23 285.43 |
| 123 | python (3.9)| [starlette](https://starlette.io) (0.14) | 24 482.88 | 29 351.53 | 29 823.57 |
| 124 | scala (2.13)| [play](https://playframework.com) (2.8) | 24 307.33 | 27 469.59 | 27 585.58 |
| 125 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 24 177.11 | 26 384.16 | 25 406.12 |
| 126 | python (3.9)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 691.56 | 28 512.20 | 28 280.70 |
| 127 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 23 186.31 | 21 442.61 | 20 706.90 |
| 128 | clojure (1.1)| [luminus](https://luminusweb.com) (3.91) | 22 175.84 | 21 473.60 | 21 044.55 |
| 129 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 22 085.93 | 22 119.21 | 21 778.01 |
| 130 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.16) | 21 672.15 | 20 691.03 | 18 088.44 |
| 131 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 20 694.52 | 20 316.36 | 20 920.66 |
| 132 | python (3.9)| [molten](https://moltenframework.com) (1.0) | 19 171.51 | 22 470.67 | 22 256.49 |
| 133 | javascript (14.15)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 18 998.54 | 18 488.85 | 16 939.61 |
| 134 | python (3.9)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 18 205.57 | 23 941.37 | 23 729.57 |
| 135 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 728.92 | 15 724.90 | 14 710.38 |
| 136 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 17 361.93 | 16 775.20 | 16 323.45 |
| 137 | php (7.4)| [swoft](https://swoft.org) (2.0) | 17 103.60 | 20 768.75 | 21 321.24 |
| 138 | python (3.9)| [masonite](https://masoniteproject.com) (2.3) | 16 730.36 | 20 236.12 | 20 140.37 |
| 139 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 16 630.94 | 16 032.91 | 15 808.93 |
| 140 | rust (1.49)| [iron](https://ironframework.io) (0.6) | 16 312.74 | 16 476.66 | 16 538.40 |
| 141 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 16 239.21 | 19 334.42 | 19 696.69 |
| 142 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 16 058.39 | 14 310.63 | 13 178.84 |
| 143 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.39) | 15 873.86 | 15 376.33 | 15 114.60 |
| 144 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.3) | 15 836.84 | 18 447.00 | 19 051.87 |
| 145 | python (3.9)| [fastapi](https://fastapi.tiangolo.com) (0.63) | 15 615.88 | 19 871.35 | 19 937.64 |
| 146 | javascript (14.15)| [feathersjs](https://feathersjs.com) (4.5) | 15 560.31 | 17 834.27 | 15 433.56 |
| 147 | go (1.15)| [macaron](https://go-macaron.com) (1.4) | 15 436.31 | 16 898.52 | 16 908.94 |
| 148 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 15 374.91 | 14 895.28 | 14 605.50 |
| 149 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 329.82 | 17 914.00 | 18 101.62 |
| 150 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 15 304.49 | 17 780.06 | 17 822.33 |
| 151 | javascript (14.15)| [express](https://expressjs.com) (4.17) | 15 258.10 | 17 935.50 | 18 098.38 |
| 152 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 14 330.49 | 14 840.02 | 21 933.98 |
| 153 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 14 133.58 | 13 742.82 | 13 492.15 |
| 154 | java (11)| [struts2](https://struts.apache.org) (2.5) | 13 832.82 | 14 161.71 | 14 173.95 |
| 155 | ruby (2.7)| [rack_app](https://rack-app.com) (7.7) | 13 601.26 | 13 114.49 | 12 964.91 |
| 156 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 13 192.30 | 12 786.74 | 12 611.12 |
| 157 | java (11)| [blade](https://lets-blade.com) (2.0) | 12 974.32 | 15 814.99 | 14 518.10 |
| 158 | python (3.9)| [flask](https://flask.pocoo.org) (1.1) | 12 877.27 | 15 998.59 | 16 307.10 |
| 159 | javascript (14.15)| [nestjs-express](https://nestjs.com) (7.6) | 11 568.54 | 14 037.90 | 11 298.97 |
| 160 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 11 463.13 | 11 182.60 | 10 736.92 |
| 161 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 416.08 | 11 655.88 | 11 704.99 |
| 162 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.5) | 10 152.34 | 10 401.25 | 10 494.49 |
| 163 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.3) | 10 046.07 | 10 308.06 | 10 437.20 |
| 164 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 767.86 | 9 963.34 | 9 944.11 |
| 165 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 9 724.11 | 9 895.35 | 10 040.32 |
| 166 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.7) | 9 580.39 | 9 746.84 | 9 808.15 |
| 167 | python (3.9)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 263.11 | 9 216.43 | 9 097.26 |
| 168 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.1) | 9 117.62 | 8 849.86 | 8 815.09 |
| 169 | ruby (2.7)| [grape](https://ruby-grape.org) (1.5) | 8 688.31 | 8 421.53 | 8 330.51 |
| 170 | python (3.9)| [guillotina](https://guillotina.io) (6.1) | 8 384.92 | 8 314.69 | 7 973.24 |
| 171 | python (3.9)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 7 547.84 | 7 130.20 | 6 831.64 |
| 172 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 7 495.70 | 13 610.00 | 12 690.05 |
| 173 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 6 793.56 | 6 743.46 | 6 668.21 |
| 174 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 647.40 | 6 565.52 | 6 488.58 |
| 175 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 6 609.44 | 6 536.17 | 6 414.41 |
| 176 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 6 427.77 | 6 333.42 | 6 216.55 |
| 177 | python (3.9)| [tornado](https://tornadoweb.org) (6.1) | 6 360.65 | 6 791.21 | 6 479.07 |
| 178 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 289.60 | 6 203.37 | 6 180.86 |
| 179 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 5 983.65 | 6 662.07 | 6 682.56 |
| 180 | python (3.9)| [django](https://djangoproject.com) (3.1) | 5 807.21 | 5 798.11 | 5 802.97 |
| 181 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 5 751.98 | 5 652.27 | 5 655.33 |
| 182 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 5 282.16 | 5 233.15 | 5 143.90 |
| 183 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 5 252.23 | 5 201.14 | 5 212.32 |
| 184 | python (3.9)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 5 046.75 | 4 949.61 | 4 615.63 |
| 185 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.2) | 4 723.89 | 4 677.85 | 4 607.86 |
| 186 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 487.32 | 4 463.99 | 4 392.84 |
| 187 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 458.19 | 4 429.21 | 4 442.07 |
| 188 | javascript (14.15)| [sails](https://sailsjs.com) (1.4) | 4 254.68 | 4 266.94 | 4 145.23 |
| 189 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 4 115.22 | 2 563.96 | 2 477.99 |
| 190 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.3) | 4 010.09 | 3 928.93 | 3 940.01 |
| 191 | php (7.4)| [nette](https://nette.org/en/) (3.1) | 3 887.04 | 3 875.37 | 3 891.25 |
| 192 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.5) | 3 853.45 | 3 783.10 | 3 794.64 |
| 193 | php (7.4)| [lumen](https://lumen.laravel.com) (8.1) | 3 853.04 | 3 858.63 | 3 878.46 |
| 194 | julia (1.6)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 512.21 | 6 801.44 | 5 744.13 |
| 195 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.3) | 3 429.07 | 3 387.36 | 3 390.18 |
| 196 | php (7.4)| [symfony](https://symfony.com) (5.2) | 2 979.67 | 2 989.54 | 3 000.69 |
| 197 | ruby (2.7)| [rails](https://rubyonrails.org) (6.1) | 2 826.37 | 2 801.23 | 2 810.61 |
| 198 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 634.53 | 2 619.67 | 2 636.06 |
| 199 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.5) | 2 316.40 | 2 316.42 | 2 332.20 |
| 200 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 056.60 | 2 439.87 | 2 418.59 |
| 201 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 930.97 | 1 850.25 | 1 775.87 |
| 202 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 884.10 | 1 832.96 | 1 850.29 |
| 203 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 1 875.12 | 1 876.33 | 1 889.35 |
| 204 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 1 742.71 | 1 751.54 | 1 779.37 |
| 205 | python (3.9)| [cyclone](https://cyclone.io) (1.3) | 1 664.10 | 1 641.78 | 1 624.40 |
| 206 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 601.39 | 1 619.03 | 1 617.55 |
| 207 | python (3.9)| [klein](https://github.com/twisted/klein) (20.6) | 1 492.69 | 1 507.43 | 1 476.20 |
| 208 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 205.91 | 644.19 | 747.69 |
| 209 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 107.60 | 1 129.13 | 1 127.47 |
| 210 | python (3.9)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 047.54 | 1 022.49 | 1 015.30 |
| 211 | python (3.9)| [django-ninja](https://django-ninja.rest-framework.com) (0.9) | 909.49 | 1 626.93 | 1 665.68 |
| 212 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 287.30 | 303.41 | -93.04 |
| 213 | cpp (11)| [nawa](https://github.com/jatofg/nawa) (0.6) | 248.26 | NaN | NaN |
| 214 | php (7.4)| [laravel](https://laravel.com) (7.27) | 130.42 | 25.10 | 0.00 |

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


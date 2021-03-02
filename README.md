# Which is the fastest ?
----------
#### Simple framework comparison 
----------

<p align="center">
  <img src="https://img.shields.io/badge/status-beta-green?style=for-the-badge">
</p>

<hr/>

<p align="center">
   <a href="https://the-benchmarker.semaphoreci.com/badges/web-frameworks/branches/master.svg">
      <img src="https://the-benchmarker.semaphoreci.com/badges/web-frameworks/branches/master.svg" alt="Build Status">
   </a>
   <a href="https://join.slack.com/t/thebenchmarker/shared_invite/zt-fcyy1ybq-A7T1SedewiVMEtJQGEyQYw" target="_blank">
      <img src="https://img.shields.io/badge/slack-chat_with_us-green" alt="Chat with us">
   </a>
   <a href="https://github.com/the-benchmarker/web-frameworks/blob/master/LICENSE" target="_blank">
      <img src="https://img.shields.io/github/license/the-benchmarker/web-frameworks" alt="License">
   </a>
</p>

## Motivation

There are many frameworks, each one comes with its own advantages and drawbacks. The purpose of this project is to identify them and attempt to measure their differences (performance is only one metric).

#### What is a framework ?

A framework is a set of components working together. The main intention behind a framework is to facilitate (app or service) creation. The way a framework help any developer could vary from one to an other.

A majority of frameworks could be splitted in 2 parts :

+ **full-stack** meaning it provides all aspects (-stacks-) from data layer to sometimes deployment
+ **micro** meaning it provides only the routing part, and let the developer choose any other component for the others

## Requirements

+ `ruby`, all tools are made in `ruby`
+ `wrk`, results are collected using `wrk`
+ `postgresql`, results are stored in `postgresql`
+ `docker`, each implementation is implemented in an isolated **container**
+ `jq`, processing `docker` metadata
+ `docker-machine` if you are on `macos`

## Usage

+ Setup

```
bundle install
bundle exec rake config
```

+ Build

:warning: On `macos`, you need to use `docker-machine` to allow `docker` usage for each framework :warning:

```
docker-machine rm default --force
docker-machine create default
eval $(docker-machine env default)
```

```
export FRAMEWORK=php/lumen
cd ${FRAMEWORK} 
make -f .Makefile build 
```

+ Run

```
make -f ${FRAMEWORK}/.Makefile collect
```

:warning: You need to be on the project main directory :warning:

## Results (2021-03-02)



<details open>
  <summary><strong>Technical details</strong></summary>
  <ul>
   <li>CPU : 8 Cores (AMD FX-8320E Eight-Core Processor)</li>
   <li>RAM : 16 Gb</li>
   <li>OS : Fedora</li>
   <li><pre>Docker version 20.10.0-rc1, build 5cc2396
</pre></li>
  </ul>
</details>

<details open>
  <summary><strong>Datatable</strong></summary>
<a id="results"> Computed with https://github.com/wg/wrk
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | java (11)| [activej](https://activej.io) (4.0) | 172 927.90 | 210 354.07 | 213 421.65 |
| 2 | php (7.4)| [webman](https://github.com/walkor/webman) (1.0) | 153 368.49 | 164 666.78 | 168 154.51 |
| 3 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 150 739.95 | 182 966.31 | 185 804.79 |
| 4 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 122 197.04 | 135 126.68 | 136 735.13 |
| 5 | go (1.16)| [gearbox](https://gogearbox.com) (1.2) | 120 584.45 | 125 658.26 | 125 518.60 |
| 6 | go (1.16)| [fiber](https://gofiber.io) (2.5) | 118 834.16 | 131 386.97 | 130 992.35 |
| 7 | go (1.16)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.21) | 117 259.88 | 131 220.76 | 132 614.64 |
| 8 | go (1.16)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.6) | 116 654.15 | 128 924.20 | 129 139.76 |
| 9 | go (1.16)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 116 384.78 | 129 416.42 | 129 003.26 |
| 10 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 116 110.82 | 145 504.97 | 148 122.11 |
| 11 | go (1.16)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 116 029.51 | 129 088.20 | 128 675.33 |
| 12 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 113 486.60 | 140 561.43 | 143 762.25 |
| 13 | java (11)| [restheart](https://restheart.org) (5.3) | 113 052.19 | 117 987.53 | 118 661.08 |
| 14 | java (11)| [undertow](https://undertow.io) (2.2) | 112 907.02 | 137 200.81 | 138 540.27 |
| 15 | javascript (14.16)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 112 736.94 | 138 388.62 | 140 709.43 |
| 16 | javascript (14.16)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.4) | 110 133.07 | 142 259.56 | 145 423.82 |
| 17 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 109 410.41 | 134 943.25 | 137 953.27 |
| 18 | java (11)| [jooby](https://jooby.io) (2.9) | 109 266.09 | 137 022.37 | 142 112.39 |
| 19 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 108 667.14 | 136 290.26 | 141 107.52 |
| 20 | go (1.16)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 108 291.99 | 104 684.68 | 109 135.84 |
| 21 | clojure (1.1)| [donkey](https://github.com/AppsFlyer/donkey) (0.5) | 107 928.77 | 112 146.78 | 113 189.19 |
| 22 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 106 738.51 | 133 054.08 | 137 003.93 |
| 23 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 106 337.26 | 130 731.69 | 134 035.91 |
| 24 | java (11)| [vertx](https://vertx.io/docs/vertx-core/java/) (4.0) | 104 939.04 | 124 957.11 | 126 789.38 |
| 25 | java (11)| [vertx4web](https://vertx.io/docs/vertx-web/java/) (4.0) | 104 758.95 | 124 121.54 | 125 749.60 |
| 26 | crystal (0.36)| [grip](https://github.com/grip-framework/grip) (4.0) | 100 866.65 | 114 762.28 | 115 524.30 |
| 27 | crystal (0.36)| [spider-gazelle](https://spider-gazelle.net) (4.2) | 99 513.30 | 120 488.25 | 121 136.10 |
| 28 | crystal (0.36)| [toro](https://github.com/soveran/toro) (0.4) | 99 271.86 | 122 566.43 | 123 009.97 |
| 29 | crystal (0.36)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 98 804.47 | 121 028.70 | 121 873.58 |
| 30 | php (7.4)| [swoole](https://github.com/swoole/swoole-src) (4.6) | 97 767.78 | 119 461.12 | 138 040.73 |
| 31 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 96 369.55 | 140 344.21 | 150 863.66 |
| 32 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 94 562.96 | 115 195.40 | 117 705.81 |
| 33 | crystal (0.36)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 94 249.67 | 117 207.47 | 118 080.33 |
| 34 | crystal (0.36)| [kemal](https://kemalcr.com) (0.27) | 93 851.31 | 114 144.00 | 113 772.05 |
| 35 | c (11)| [kore](https://kore.io) (3.3) | 93 115.76 | 183 178.75 | 195 101.01 |
| 36 | crystal (0.36)| [amber](https://amberframework.org) (0.36) | 90 053.77 | 107 910.87 | 107 582.90 |
| 37 | php (7.4)| [swoole-coroutine](https://github.com/swoole/swoole-src) (4.6) | 87 193.98 | 126 211.93 | 136 423.28 |
| 38 | java (11)| [quarkus](https://quarkus.io) (1.12) | 86 465.68 | 104 376.28 | 107 080.18 |
| 39 | javascript (14.16)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 84 039.99 | 97 273.61 | 98 708.70 |
| 40 | scala (2.13)| [akkahttp](https://akka.io) (10.2) | 83 285.14 | 97 821.76 | 95 004.69 |
| 41 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 82 558.75 | 104 528.05 | 113 417.90 |
| 42 | go (1.16)| [gf](https://goframe.org) (1.15) | 82 525.81 | 90 113.81 | 92 418.98 |
| 43 | go (1.16)| [rte](https://github.com/jwilner/rte) (0.0) | 81 424.67 | 82 223.93 | 84 442.30 |
| 44 | go (1.16)| [clevergo](https://clevergo.tech) (0.5) | 80 927.29 | 81 831.15 | 83 843.38 |
| 45 | go (1.16)| [echo](https://echo.labstack.com) (4.2) | 80 256.59 | 81 582.70 | 83 512.46 |
| 46 | go (1.16)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 80 254.96 | 81 273.94 | 83 041.49 |
| 47 | go (1.16)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 780.30 | 83 354.72 | 84 235.71 |
| 48 | go (1.16)| [gin](https://gin-gonic.com) (1.6) | 78 754.86 | 82 694.49 | 84 051.33 |
| 49 | go (1.16)| [violetear](https://violetear.org) (7.0) | 77 537.06 | 77 629.71 | 79 943.49 |
| 50 | go (1.16)| [chi](https://github.com/go-chi/chi) (1.5) | 76 763.48 | 76 333.57 | 78 765.28 |
| 51 | go (1.16)| [aero](https://github.com/aerogo/aero) (1.3) | 76 680.27 | 77 450.03 | 79 721.36 |
| 52 | go (1.16)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 76 187.69 | 75 726.64 | 77 875.23 |
| 53 | crystal (0.36)| [athena](https://github.com/athena-framework/athena) (0.13) | 75 848.60 | 85 883.06 | 81 410.20 |
| 54 | go (1.16)| [kami](https://github.com/guregu/kami) (2.2) | 75 730.73 | 79 876.67 | 80 382.31 |
| 55 | scala (2.13)| [finatra](https://twitter.github.io/finatra/) (21.1) | 75 337.12 | 86 530.69 | 87 111.03 |
| 56 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 74 809.39 | 87 695.78 | 90 434.10 |
| 57 | python (3.9)| [falcon](https://falconframework.org) (2.0) | 74 413.93 | 81 673.05 | 83 309.53 |
| 58 | go (1.16)| [goroute](https://goroute.github.io) (0.0) | 73 627.57 | 72 226.48 | 75 033.59 |
| 59 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 72 955.04 | 84 713.62 | 87 319.54 |
| 60 | javascript (14.16)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 72 678.55 | 79 848.24 | 80 501.93 |
| 61 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.3) | 72 669.94 | 65 811.04 | 65 253.98 |
| 62 | go (1.16)| [beego](https://beego.me) (1.12) | 71 535.55 | 74 517.20 | 76 799.22 |
| 63 | fsharp (5.0)| [falco](https://falcoframework.com) (3.0) | 71 445.26 | 82 575.63 | 84 975.74 |
| 64 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 66 297.46 | 73 246.53 | 73 993.74 |
| 65 | go (1.16)| [air](https://github.com/aofei/air) (0.21) | 65 540.73 | 64 436.85 | 66 726.18 |
| 66 | go (1.16)| [goyave](https://github.com/go-goyave/goyave) (3.7) | 64 660.91 | 64 252.73 | 67 524.91 |
| 67 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 62 023.31 | 67 461.67 | 65 954.30 |
| 68 | javascript (14.16)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 61 445.07 | 67 364.14 | 69 373.03 |
| 69 | scala (2.13)| [finch](https://finagle.github.io/finch/) (0.32) | 61 432.80 | 68 727.71 | 68 573.58 |
| 70 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 60 369.44 | 63 507.65 | 64 483.99 |
| 71 | javascript (14.16)| [0http](https://github.com/jkyberneees/0http) (3.0) | 59 369.68 | 64 780.71 | 66 002.52 |
| 72 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 58 528.96 | 124 570.60 | 130 766.10 |
| 73 | javascript (14.16)| [polka](https://github.com/lukeed/polka) (0.5) | 57 449.47 | 62 569.00 | 61 345.89 |
| 74 | javascript (14.16)| [restana](https://github.com/jkyberneees/ana) (4.7) | 56 819.70 | 62 694.79 | 63 664.32 |
| 75 | javascript (14.16)| [fastify](https://fastify.io) (3.12) | 56 404.57 | 59 678.55 | 59 309.98 |
| 76 | javascript (14.16)| [rayo](https://rayo.js.org) (1.3) | 55 206.85 | 60 986.29 | 59 429.13 |
| 77 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 54 487.15 | 57 827.13 | 56 421.92 |
| 78 | php (7.4)| [one](https://github.com/lizhichao/one) (2.2) | 53 571.25 | 62 629.62 | 66 371.74 |
| 79 | python (3.9)| [pyramid](https://trypyramid.com) (1.1) | 53 475.64 | 57 267.42 | 57 907.39 |
| 80 | javascript (14.16)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 53 394.63 | 58 591.90 | 57 943.92 |
| 81 | rust (1.50)| [salvo](https://github.com/kenorld/salvo) (0.6) | 52 958.61 | 56 924.98 | 57 831.90 |
| 82 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 52 344.77 | 60 491.36 | 62 380.15 |
| 83 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 51 912.54 | 68 199.85 | 70 721.59 |
| 84 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 51 758.74 | 57 197.94 | 63 979.69 |
| 85 | java (11)| [micronaut](https://micronaut.io) (1.2) | 51 370.30 | 57 811.13 | 56 790.36 |
| 86 | java (11)| [javalin](https://javalin.io) (3.9) | 51 083.62 | 54 446.01 | 54 776.93 |
| 87 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 50 905.67 | 57 509.98 | 57 830.50 |
| 88 | java (11)| [spark](https://sparkjava.com) (2.9) | 49 926.68 | 54 454.55 | 55 633.62 |
| 89 | php (7.4)| [hyperf](https://hyperf.io) (2.1) | 49 617.44 | 61 965.92 | 68 683.69 |
| 90 | ruby (3.0)| [agoo](https://github.com/ohler55/agoo) (2.14) | 48 848.31 | 72 383.31 | 80 614.79 |
| 91 | javascript (14.16)| [nestjs-fastify](https://nestjs.com) (7.6) | 48 420.47 | 51 883.94 | 51 511.60 |
| 92 | go (1.16)| [mars](https://github.com/roblillack/mars) (1.0) | 46 536.35 | 46 695.77 | 49 914.70 |
| 93 | rust (1.50)| [actix](https://actix.rs) (3.3) | 46 130.02 | 45 093.25 | 46 512.84 |
| 94 | javascript (14.16)| [foxify](https://foxify.js.org) (0.1) | 45 714.90 | 48 819.40 | 48 036.02 |
| 95 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 45 641.36 | 48 479.97 | 51 124.84 |
| 96 | python (3.9)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 45 045.66 | 51 484.86 | 52 351.65 |
| 97 | javascript (14.16)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 45 001.84 | 48 629.25 | 47 433.97 |
| 98 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 43 702.40 | 44 534.11 | 44 976.17 |
| 99 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 43 239.04 | 32 148.29 | 30 933.86 |
| 100 | python (3.9)| [bottle](https://bottlepy.org) (0.12) | 42 540.06 | 46 226.42 | 47 098.99 |
| 101 | javascript (14.16)| [koa](https://koajs.com) (2.13) | 41 404.18 | 44 037.71 | 43 324.73 |
| 102 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 36 598.48 | 37 867.45 | 37 892.20 |
| 103 | javascript (14.16)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 36 075.73 | 38 852.02 | 38 475.97 |
| 104 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 35 740.25 | 35 598.28 | 34 890.64 |
| 105 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 35 584.43 | 37 339.77 | 37 770.11 |
| 106 | swift (5.3)| [vapor](https://vapor.codes) (4.41) | 35 337.88 | 37 402.45 | 37 086.37 |
| 107 | python (3.9)| [emmett](https://emmett.sh) (2.1) | 35 111.30 | 40 637.41 | 41 964.87 |
| 108 | javascript (14.16)| [moleculer](https://moleculer.services) (0.14) | 35 059.05 | 35 766.25 | 34 653.30 |
| 109 | javascript (14.16)| [hapi](https://hapijs.com) (20.1) | 34 875.10 | 35 967.42 | 35 456.76 |
| 110 | java (11)| [jersey3-grizzly2](https://eclipse-ee4j.github.io/jersey) (3) | 34 635.52 | 39 860.84 | 39 360.67 |
| 111 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 34 244.33 | 34 215.09 | 34 306.20 |
| 112 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 33 975.15 | 35 012.03 | 33 613.14 |
| 113 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 33 699.23 | 43 990.05 | 47 957.03 |
| 114 | java (11)| [jersey-grizzly2](https://eclipse-ee4j.github.io/jersey) (2.33) | 33 405.95 | 38 149.96 | 37 681.95 |
| 115 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 33 394.16 | 38 554.78 | 38 405.15 |
| 116 | python (3.9)| [hug](https://hug.rest) (2.6) | 33 135.01 | 35 554.41 | 53 573.82 |
| 117 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 31 899.70 | 27 801.51 | 23 553.61 |
| 118 | python (3.9)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 31 844.88 | 36 502.04 | 36 876.15 |
| 119 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 31 542.35 | 31 421.58 | 30 966.56 |
| 120 | rust (1.50)| [nickel](https://nickel-org.github.io) (0.11) | 31 454.49 | 32 042.77 | 32 215.68 |
| 121 | python (3.9)| [sanic](https://github.com/huge-success/sanic) (20.12) | 30 177.64 | 34 773.53 | 35 100.34 |
| 122 | php (7.4)| [imi](https://imiphp.com) (1.2) | 29 654.65 | 32 201.63 | 33 770.67 |
| 123 | python (3.9)| [blacksheep](https://github.com/Neoteroi/BlackSheep) (1.0) | 29 064.16 | 33 340.24 | 34 779.67 |
| 124 | javascript (14.16)| [restify](https://restify.com) (8.5) | 29 059.89 | 30 541.14 | 29 880.13 |
| 125 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 28 980.47 | 31 938.01 | 31 859.91 |
| 126 | scala (2.13)| [play](https://playframework.com) (2.8) | 28 189.28 | 29 916.94 | 29 821.38 |
| 127 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 27 711.26 | 30 243.87 | 32 223.15 |
| 128 | python (3.9)| [starlette](https://starlette.io) (0.14) | 27 598.18 | 31 098.51 | 32 173.70 |
| 129 | javascript (14.16)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 27 433.31 | 28 324.57 | 28 479.96 |
| 130 | php (7.4)| [comet](https://github.com/gotzmann/comet) (1.2) | 27 374.37 | 27 475.58 | 26 745.59 |
| 131 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 27 205.81 | 29 185.90 | 28 720.97 |
| 132 | python (3.9)| [responder](https://python-responder.org) (2.0) | 26 797.78 | 31 593.83 | 31 603.47 |
| 133 | rust (1.50)| [gotham](https://gotham.rs) (0.5) | 25 855.05 | 29 733.62 | 30 917.44 |
| 134 | python (3.9)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 24 964.97 | 28 872.55 | 28 471.51 |
| 135 | crystal (0.36)| [orion](https://github.com/obsidian/orion) (3.0) | 24 904.26 | 24 243.65 | 22 531.65 |
| 136 | python (3.9)| [index.py](https://index-py.abersheeran.com) (0.16) | 24 636.91 | 29 389.59 | 30 345.12 |
| 137 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 24 445.04 | 26 246.44 | 25 670.46 |
| 138 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 23 492.24 | 21 647.73 | 20 778.32 |
| 139 | clojure (1.1)| [luminus](https://luminusweb.com) (3.97) | 22 604.64 | 22 067.21 | 21 536.28 |
| 140 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 22 276.68 | 21 988.07 | 21 771.08 |
| 141 | javascript (14.16)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 21 939.49 | 23 788.60 | 23 228.41 |
| 142 | crystal (0.36)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.17) | 21 177.69 | 21 336.79 | 18 953.60 |
| 143 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 20 306.38 | 20 071.79 | 20 292.43 |
| 144 | python (3.9)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 19 938.35 | 22 810.01 | 23 804.77 |
| 145 | javascript (14.16)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 18 930.06 | 17 919.19 | 17 051.33 |
| 146 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 665.97 | 15 641.79 | 14 644.19 |
| 147 | python (3.9)| [molten](https://moltenframework.com) (1.0) | 17 377.78 | 20 685.68 | 20 602.24 |
| 148 | php (7.4)| [swoft](https://swoft.org) (2.0) | 17 020.02 | 20 339.25 | 21 554.69 |
| 149 | ruby (3.0)| [hanami-api](https://hanamirb.org) (0.1) | 16 706.40 | 16 334.78 | 15 983.15 |
| 150 | python (3.9)| [fastapi](https://fastapi.tiangolo.com) (0.63) | 16 557.53 | 21 445.75 | 21 089.93 |
| 151 | rust (1.50)| [iron](https://github.com/iron/iron) (0.6) | 16 271.89 | 16 416.01 | 16 698.45 |
| 152 | ruby (3.0)| [syro](https://github.com/soveran/syro) (3.2) | 16 270.91 | 15 693.76 | 15 378.55 |
| 153 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 15 940.52 | 14 099.84 | 13 133.03 |
| 154 | ruby (3.0)| [roda](https://roda.jeremyevans.net) (3.41) | 15 670.24 | 15 100.15 | 14 827.11 |
| 155 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 15 600.13 | 18 230.80 | 18 951.21 |
| 156 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 310.95 | 17 467.69 | 17 907.49 |
| 157 | go (1.16)| [macaron](https://go-macaron.com) (1.4) | 15 191.86 | 16 739.43 | 16 740.51 |
| 158 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 15 053.15 | 17 277.40 | 17 740.19 |
| 159 | javascript (14.16)| [express](https://expressjs.com) (4.17) | 15 013.87 | 17 674.64 | 18 163.97 |
| 160 | javascript (14.16)| [feathersjs](https://feathersjs.com) (4.5) | 14 987.53 | 17 878.55 | 17 580.26 |
| 161 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 14 022.37 | 13 809.23 | 20 754.85 |
| 162 | python (3.9)| [flask](https://flask.pocoo.org) (1.1) | 13 923.47 | 13 145.76 | 16 543.00 |
| 163 | ruby (3.0)| [cuba](https://cuba.is) (3.9) | 13 826.66 | 13 375.58 | 13 298.06 |
| 164 | java (11)| [struts2](https://struts.apache.org) (2.5) | 13 770.38 | 14 059.68 | 14 080.48 |
| 165 | ruby (3.0)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 13 604.71 | 13 262.90 | 13 000.74 |
| 166 | java (11)| [blade](https://lets-blade.com) (2.0) | 13 080.07 | 15 577.92 | 14 490.38 |
| 167 | ruby (3.0)| [camping](https://github.com/camping/camping) (2.1) | 12 898.80 | 12 456.98 | 12 360.96 |
| 168 | ruby (3.0)| [rack_app](https://rack-app.com) (7.7) | 12 855.20 | 12 291.56 | 11 938.71 |
| 169 | javascript (14.16)| [nestjs-express](https://nestjs.com) (7.6) | 11 460.34 | 12 523.83 | 12 543.31 |
| 170 | go (1.16)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 398.01 | 11 752.12 | 11 763.12 |
| 171 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 10 694.30 | 10 629.97 | 10 244.35 |
| 172 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 10 140.46 | 10 410.37 | 10 425.16 |
| 173 | go (1.16)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 914.29 | 10 189.66 | 10 155.84 |
| 174 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.6) | 9 844.25 | 10 075.46 | 10 226.85 |
| 175 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.7) | 9 761.73 | 9 812.21 | 9 948.72 |
| 176 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 9 759.79 | 9 969.71 | 10 085.95 |
| 177 | python (3.9)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 476.95 | 9 180.20 | 8 746.17 |
| 178 | php (7.4)| [laravel-s-lumen](https://github.com/hhxsv5/laravel-s) (3.7) | 9 340.05 | 9 960.20 | 9 877.27 |
| 179 | python (3.9)| [guillotina](https://guillotina.io) (6.1) | 9 243.95 | 9 329.81 | 8 874.55 |
| 180 | pony (0.39)| [jennet](https://github.com/Theodus/jennet) (0.1) | 8 803.00 | 13 323.81 | 12 738.43 |
| 181 | ruby (3.0)| [sinatra](https://sinatrarb.com) (2.1) | 8 627.09 | 8 391.81 | 8 401.54 |
| 182 | ruby (3.0)| [grape](https://ruby-grape.org) (1.5) | 7 623.98 | 7 395.94 | 7 361.32 |
| 183 | python (3.9)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 7 600.93 | 7 209.01 | 6 849.42 |
| 184 | python (3.9)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 419.86 | 6 907.41 | 6 404.15 |
| 185 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 7 208.18 | 7 530.56 | 7 495.75 |
| 186 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 7 068.65 | 7 545.12 | 7 428.06 |
| 187 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 6 942.37 | 7 330.95 | 7 241.18 |
| 188 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 896.59 | 7 197.07 | 7 104.98 |
| 189 | php (7.4)| [laravel-s-laravel](https://github.com/hhxsv5/laravel-s) (3.7) | 6 391.80 | 6 440.34 | 6 269.40 |
| 190 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 368.38 | 6 683.06 | 6 662.12 |
| 191 | python (3.9)| [django](https://djangoproject.com) (3.1) | 6 298.96 | 5 903.29 | 5 879.34 |
| 192 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 5 935.35 | 6 549.16 | 6 644.59 |
| 193 | python (3.9)| [tornado](https://tornadoweb.org) (6.1) | 5 904.67 | 5 761.67 | 5 676.64 |
| 194 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 5 766.45 | 5 982.82 | 5 893.63 |
| 195 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.2) | 5 095.46 | 5 237.02 | 5 227.20 |
| 196 | php (7.4)| [fatfree](https://fatfreeframework.com) (3.7) | 5 006.35 | 5 217.46 | 5 124.50 |
| 197 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 786.78 | 4 916.81 | 4 880.17 |
| 198 | php (7.4)| [cubex](https://cubex.io) (4.16) | 4 633.85 | 4 831.16 | 4 815.41 |
| 199 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 364.90 | 4 533.76 | 4 518.01 |
| 200 | javascript (14.16)| [sails](https://sailsjs.com) (1.4) | 4 186.16 | 4 211.14 | 4 168.99 |
| 201 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.3) | 4 111.17 | 4 245.50 | 4 269.25 |
| 202 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.6) | 3 925.07 | 3 954.03 | 3 949.16 |
| 203 | php (7.4)| [nette](https://nette.org/en/) (3.1) | 3 857.86 | 3 967.20 | 3 958.34 |
| 204 | php (7.4)| [lumen](https://lumen.laravel.com) (8.2) | 3 828.58 | 3 891.58 | 3 933.14 |
| 205 | julia (1.6)| [merly](https://github.com/codeneomatrix/Merly.jl) (1.0) | 3 709.40 | 6 303.47 | 4 047.46 |
| 206 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 3 446.52 | 3 508.18 | 3 501.36 |
| 207 | php (7.4)| [symfony](https://symfony.com) (5.2) | 3 013.57 | 3 054.85 | 3 047.34 |
| 208 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.6) | 2 969.01 | 3 019.44 | 3 035.51 |
| 209 | ruby (3.0)| [rails](https://rubyonrails.org) (6.1) | 2 788.70 | 2 778.27 | 2 770.92 |
| 210 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 504.46 | 2 544.92 | 2 551.86 |
| 211 | python (3.9)| [masonite](https://masoniteproject.com) (3.0) | 2 203.02 | 2 216.48 | 2 216.81 |
| 212 | php (7.4)| [driftphp](https://github.com/driftphp/driftphp) (0.1) | 1 934.05 | 661.50 | 1 874.94 |
| 213 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 932.99 | 1 838.02 | 1 782.21 |
| 214 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 916.37 | 1 865.52 | 1 845.84 |
| 215 | php (7.4)| [antidot](https://github.com/antidot-framework/react-framework) (0.2) | 1 881.21 | 668.09 | 1 987.56 |
| 216 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.3) | 1 853.90 | 1 873.08 | 1 852.46 |
| 217 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 686.37 | 1 703.06 | 1 706.15 |
| 218 | python (3.9)| [cyclone](https://cyclone.io) (1.3) | 1 582.95 | 1 583.39 | 1 558.57 |
| 219 | python (3.9)| [klein](https://github.com/twisted/klein) (20.6) | 1 517.57 | 1 545.76 | 1 517.22 |
| 220 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 507.27 | 647.35 | 266.87 |
| 221 | python (3.9)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 233.51 | 1 162.66 | 1 173.65 |
| 222 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.1) | 1 175.76 | 1 199.79 | 1 193.42 |
| 223 | python (3.9)| [django-ninja](https://django-ninja.rest-framework.com) (0.10) | 1 165.02 | 1 642.75 | 1 673.05 |
| 224 | php (7.4)| [laravel](https://laravel.com) (8.29) | 992.99 | 998.38 | 994.52 |
| 225 | php (7.4)| [unic](https://unicframework.github.io/docs) (1.0) | 489.73 | 475.09 | 172.82 |
| 226 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 285.52 | 302.20 | -84.17 |
| 227 | cpp (11)| [nawa](https://github.com/jatofg/nawa) (0.6) | 264.41 | NaN | NaN |
</a>

</details>

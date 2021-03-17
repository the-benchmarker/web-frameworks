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

## Results (2021-03-17)



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
| 1 | java (11)| [activej](https://activej.io) (4.0) | 175 743.49 | 214 934.04 | 218 965.03 |
| 2 | php (7.4)| [webman](https://github.com/walkor/webman) (1.0) | 157 478.09 | 168 873.78 | 171 703.85 |
| 3 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 152 402.94 | 179 151.31 | 185 200.10 |
| 4 | go (1.16)| [gearbox](https://gogearbox.com) (1.2) | 121 344.15 | 125 667.98 | 125 289.89 |
| 5 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 120 911.21 | 135 574.99 | 137 480.62 |
| 6 | go (1.16)| [fiber](https://gofiber.io) (2.6) | 119 189.48 | 130 790.23 | 130 899.41 |
| 7 | go (1.16)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.22) | 117 494.81 | 131 552.46 | 132 366.95 |
| 8 | go (1.16)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.6) | 117 311.08 | 128 709.24 | 128 684.01 |
| 9 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 117 130.86 | 145 029.10 | 148 004.92 |
| 10 | go (1.16)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 115 919.07 | 129 492.79 | 128 661.81 |
| 11 | go (1.16)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 115 531.78 | 129 015.67 | 128 273.42 |
| 12 | java (11)| [restheart](https://restheart.org) (5.3) | 113 159.61 | 118 688.75 | 118 869.30 |
| 13 | java (11)| [undertow](https://undertow.io) (2.2) | 112 638.08 | 137 387.32 | 139 834.04 |
| 14 | javascript (14.16)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.4) | 112 426.84 | 142 947.70 | 146 328.19 |
| 15 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 112 376.11 | 140 771.28 | 143 677.04 |
| 16 | clojure (1.1)| [donkey](https://github.com/AppsFlyer/donkey) (0.5) | 111 049.33 | 113 145.26 | 115 297.68 |
| 17 | java (11)| [jooby](https://jooby.io) (2.9) | 109 799.20 | 136 271.99 | 140 751.28 |
| 18 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 109 033.80 | 136 594.32 | 141 954.71 |
| 19 | go (1.16)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 108 610.10 | 105 411.34 | 109 538.08 |
| 20 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 107 786.80 | 132 400.62 | 131 977.02 |
| 21 | javascript (14.16)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 107 716.98 | 138 756.24 | 140 958.12 |
| 22 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 106 000.31 | 133 205.60 | 136 936.59 |
| 23 | java (11)| [vertx](https://vertx.io/docs/vertx-core/java/) (4.0) | 105 815.31 | 126 423.56 | 128 637.96 |
| 24 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 105 083.20 | 131 115.11 | 134 417.88 |
| 25 | java (11)| [vertx4web](https://vertx.io/docs/vertx-web/java/) (4.0) | 103 801.86 | 122 877.64 | 124 242.06 |
| 26 | crystal (0.36)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 101 461.80 | 120 551.04 | 121 603.86 |
| 27 | crystal (0.36)| [toro](https://github.com/soveran/toro) (0.4) | 99 563.85 | 122 263.98 | 122 856.46 |
| 28 | php (7.4)| [swoole](https://github.com/swoole/swoole-src) (4.6) | 98 348.32 | 122 172.28 | 135 096.72 |
| 29 | crystal (0.36)| [spider-gazelle](https://spider-gazelle.net) (4.2) | 97 262.33 | 120 410.73 | 121 014.18 |
| 30 | crystal (0.36)| [grip](https://github.com/grip-framework/grip) (4.0) | 95 307.99 | 115 174.30 | 114 472.55 |
| 31 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 94 518.43 | 115 236.35 | 118 586.89 |
| 32 | crystal (0.36)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 94 265.90 | 116 029.46 | 116 318.52 |
| 33 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 93 756.97 | 139 871.18 | 150 151.51 |
| 34 | crystal (0.36)| [kemal](https://kemalcr.com) (0.27) | 93 609.62 | 112 662.55 | 113 368.10 |
| 35 | php (7.4)| [swoole-coroutine](https://github.com/swoole/swoole-src) (4.6) | 89 908.46 | 127 103.22 | 137 864.64 |
| 36 | java (11)| [quarkus](https://quarkus.io) (1.12) | 87 212.60 | 105 129.36 | 108 135.92 |
| 37 | scala (2.13)| [akkahttp](https://akka.io) (10.2) | 84 849.89 | 100 344.85 | 95 912.88 |
| 38 | javascript (14.16)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 84 080.60 | 98 497.06 | 99 945.86 |
| 39 | c (11)| [kore](https://kore.io) (3.3) | 83 117.46 | 168 756.86 | 191 651.91 |
| 40 | go (1.16)| [gf](https://goframe.org) (1.15) | 82 779.03 | 90 517.76 | 92 757.99 |
| 41 | go (1.16)| [rte](https://github.com/jwilner/rte) (0.0) | 81 525.65 | 82 390.67 | 84 702.59 |
| 42 | go (1.16)| [clevergo](https://clevergo.tech) (0.5) | 80 563.29 | 82 185.23 | 83 941.03 |
| 43 | go (1.16)| [echo](https://echo.labstack.com) (4.2) | 80 321.87 | 81 607.64 | 83 454.97 |
| 44 | go (1.16)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 80 101.79 | 81 224.43 | 83 023.19 |
| 45 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 79 400.46 | 104 081.63 | 116 412.00 |
| 46 | go (1.16)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 825.78 | 83 092.93 | 84 447.67 |
| 47 | go (1.16)| [gin](https://gin-gonic.com) (1.6) | 78 437.66 | 82 302.26 | 83 699.37 |
| 48 | go (1.16)| [violetear](https://violetear.org) (7.0) | 77 199.31 | 77 577.96 | 79 461.05 |
| 49 | go (1.16)| [aero](https://github.com/aerogo/aero) (1.3) | 77 050.49 | 77 743.25 | 79 535.77 |
| 50 | go (1.16)| [chi](https://github.com/go-chi/chi) (1.5) | 76 917.03 | 76 371.35 | 78 639.58 |
| 51 | go (1.16)| [webgo](https://github.com/bnkamalesh/webgo) (5.0) | 75 875.98 | 75 698.20 | 77 696.64 |
| 52 | scala (2.13)| [finatra](https://twitter.github.io/finatra/) (21.1) | 75 725.66 | 87 825.82 | 88 324.49 |
| 53 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 75 182.33 | 88 356.05 | 91 078.01 |
| 54 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 73 640.39 | 119 422.26 | 133 068.30 |
| 55 | go (1.16)| [goroute](https://goroute.github.io) (0.0) | 73 639.10 | 72 297.96 | 75 014.30 |
| 56 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 72 382.44 | 84 385.52 | 86 654.97 |
| 57 | go (1.16)| [beego](https://beego.me) (1.12) | 72 271.23 | 75 186.92 | 77 111.17 |
| 58 | fsharp (5.0)| [falco](https://falcoframework.com) (3.0) | 71 808.58 | 82 717.49 | 85 047.81 |
| 59 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.4) | 71 706.16 | 65 397.87 | 63 604.09 |
| 60 | python (3.9)| [falcon](https://falconframework.org) (2.0) | 71 117.57 | 81 061.08 | 82 386.00 |
| 61 | go (1.16)| [goyave](https://github.com/go-goyave/goyave) (3.7) | 70 920.42 | 71 085.72 | 73 208.86 |
| 62 | javascript (14.16)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 69 761.18 | 79 443.67 | 80 490.59 |
| 63 | go (1.16)| [air](https://github.com/aofei/air) (0.21) | 65 232.95 | 63 751.84 | 66 176.74 |
| 64 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 64 304.75 | 71 051.05 | 71 868.75 |
| 65 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 62 028.13 | 67 940.21 | 67 702.83 |
| 66 | scala (2.13)| [finch](https://finagle.github.io/finch/) (0.32) | 61 298.66 | 68 510.33 | 68 438.76 |
| 67 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 61 040.20 | 63 684.96 | 64 948.63 |
| 68 | javascript (14.16)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 60 418.46 | 68 060.93 | 70 051.68 |
| 69 | javascript (14.16)| [0http](https://github.com/jkyberneees/0http) (3.0) | 59 540.56 | 66 501.08 | 67 827.14 |
| 70 | rust (1.50)| [salvo](https://github.com/salvo-rs/salvo) (0.8) | 58 202.27 | 62 481.24 | 63 754.04 |
| 71 | javascript (14.16)| [restana](https://github.com/jkyberneees/ana) (4.7) | 57 746.06 | 63 442.09 | 64 807.43 |
| 72 | javascript (14.16)| [polka](https://github.com/lukeed/polka) (0.5) | 57 532.72 | 62 608.38 | 61 611.94 |
| 73 | javascript (14.16)| [rayo](https://rayo.js.org) (1.3) | 55 735.49 | 60 465.06 | 59 786.00 |
| 74 | javascript (14.16)| [fastify](https://fastify.io) (3.14) | 54 375.57 | 59 566.33 | 57 969.85 |
| 75 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 54 141.10 | 57 135.65 | 55 665.19 |
| 76 | javascript (14.16)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 53 660.79 | 58 489.88 | 57 749.92 |
| 77 | swift (5.3)| [hummingbird](https://github.com/hummingbird-project/hummingbird) (0.7) | 53 443.37 | 59 201.96 | 59 614.39 |
| 78 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 53 127.75 | 61 740.24 | 64 015.51 |
| 79 | python (3.9)| [pyramid](https://trypyramid.com) (2.0) | 52 988.66 | 56 898.54 | 56 965.92 |
| 80 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 52 268.92 | 67 292.28 | 69 977.00 |
| 81 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 52 004.23 | 57 240.09 | 64 280.40 |
| 82 | ruby (3.0)| [agoo](https://github.com/ohler55/agoo) (2.14) | 51 499.20 | 73 528.68 | 80 683.78 |
| 83 | php (7.4)| [one](https://github.com/lizhichao/one) (2.2) | 51 203.20 | 62 409.93 | 69 013.71 |
| 84 | java (11)| [javalin](https://javalin.io) (3.9) | 51 133.34 | 54 573.38 | 54 699.10 |
| 85 | java (11)| [micronaut](https://micronaut.io) (1.2) | 50 542.29 | 57 291.13 | 57 320.17 |
| 86 | java (11)| [spark](https://sparkjava.com) (2.9) | 49 919.76 | 54 692.66 | 55 466.84 |
| 87 | php (7.4)| [hyperf](https://hyperf.io) (2.1) | 49 783.86 | 60 293.54 | 67 001.64 |
| 88 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 49 127.21 | 55 109.76 | 55 507.24 |
| 89 | javascript (14.16)| [nestjs-fastify](https://nestjs.com) (7.6) | 47 718.81 | 51 135.67 | 50 703.54 |
| 90 | rust (1.50)| [actix](https://actix.rs) (3.3) | 46 419.77 | 45 925.93 | 46 702.64 |
| 91 | go (1.16)| [mars](https://github.com/roblillack/mars) (1.0) | 46 262.85 | 46 655.55 | 49 757.20 |
| 92 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 46 107.69 | 49 203.36 | 50 502.80 |
| 93 | javascript (14.16)| [foxify](https://foxify.js.org) (0.1) | 45 660.74 | 48 615.54 | 47 855.43 |
| 94 | javascript (14.16)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 45 638.66 | 48 505.77 | 47 327.46 |
| 95 | python (3.9)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 44 818.56 | 49 300.22 | 50 985.17 |
| 96 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 630.42 | 45 758.90 | 45 578.27 |
| 97 | python (3.9)| [bottle](https://bottlepy.org) (0.12) | 43 314.64 | 46 366.65 | 47 121.65 |
| 98 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 42 577.29 | 33 157.18 | 30 633.18 |
| 99 | javascript (14.16)| [koa](https://koajs.com) (2.13) | 41 693.63 | 44 108.24 | 43 050.43 |
| 100 | dart (2.12)| [aqueduct](https://aqueduct.io) (3.3) | 39 528.58 | 39 415.96 | 38 764.21 |
| 101 | javascript (14.16)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (3.0) | 38 486.19 | 41 056.44 | 60 328.27 |
| 102 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 36 773.21 | 35 973.56 | 36 092.84 |
| 103 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 36 622.45 | 37 776.66 | 38 337.89 |
| 104 | swift (5.3)| [vapor](https://vapor.codes) (4.41) | 36 262.08 | 37 991.53 | 37 788.99 |
| 105 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 36 151.93 | 36 241.93 | 35 947.41 |
| 106 | python (3.9)| [emmett](https://emmett.sh) (2.2) | 36 120.30 | 41 124.89 | 42 256.72 |
| 107 | javascript (14.16)| [moleculer](https://moleculer.services) (0.14) | 35 029.67 | 36 248.94 | 35 340.14 |
| 108 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 34 918.80 | 36 025.73 | 37 555.69 |
| 109 | javascript (14.16)| [hapi](https://hapijs.com) (20.1) | 34 344.35 | 34 815.86 | 33 934.98 |
| 110 | java (11)| [jersey3-grizzly2](https://eclipse-ee4j.github.io/jersey) (3) | 34 184.89 | 39 108.40 | 38 154.12 |
| 111 | python (3.9)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 33 872.31 | 38 116.91 | 39 165.57 |
| 112 | javascript (14.16)| [fyrejet](https://github.com/fyrejet/fyrejet) (3.0) | 33 852.75 | 35 601.18 | 35 446.07 |
| 113 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 33 544.99 | 44 729.01 | 48 867.86 |
| 114 | python (3.9)| [hug](https://hug.rest) (2.6) | 33 456.56 | 35 736.85 | 53 815.45 |
| 115 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 33 225.57 | 34 280.69 | 32 471.59 |
| 116 | java (11)| [jersey-grizzly2](https://eclipse-ee4j.github.io/jersey) (2.33) | 33 001.16 | 37 996.79 | 37 424.48 |
| 117 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 32 832.05 | 28 471.01 | 25 768.35 |
| 118 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 32 214.87 | 37 810.02 | 37 784.29 |
| 119 | python (3.9)| [blacksheep](https://github.com/Neoteroi/BlackSheep) (1.0) | 30 888.14 | 33 721.18 | 34 624.28 |
| 120 | python (3.9)| [sanic](https://github.com/huge-success/sanic) (20.12) | 30 657.74 | 34 364.49 | 35 464.26 |
| 121 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 29 212.79 | 32 124.25 | 31 993.92 |
| 122 | javascript (14.16)| [restify](https://restify.com) (8.5) | 29 094.97 | 30 648.91 | 29 516.08 |
| 123 | python (3.9)| [responder](https://python-responder.org) (2.0) | 28 619.58 | 32 043.38 | 32 262.30 |
| 124 | php (7.4)| [imi](https://imiphp.com) (1.2) | 28 544.24 | 32 514.42 | 33 087.03 |
| 125 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 28 227.54 | 31 528.34 | 32 574.74 |
| 126 | php (7.4)| [comet](https://github.com/gotzmann/comet) (1.2) | 27 474.47 | 27 567.82 | 26 965.08 |
| 127 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 27 328.30 | 29 538.93 | 29 001.53 |
| 128 | scala (2.13)| [play](https://playframework.com) (2.8) | 26 622.96 | 28 538.27 | 28 132.30 |
| 129 | python (3.9)| [index.py](https://index-py.abersheeran.com) (0.16) | 26 201.11 | 29 824.32 | 30 771.77 |
| 130 | python (3.9)| [starlette](https://starlette.io) (0.14) | 24 970.40 | 28 756.06 | 29 069.22 |
| 131 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 24 163.42 | 26 381.19 | 25 912.69 |
| 132 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 22 918.18 | 21 590.28 | 20 582.96 |
| 133 | python (3.9)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 22 673.32 | 28 421.70 | 28 640.82 |
| 134 | clojure (1.1)| [luminus](https://luminusweb.com) (3.98) | 22 172.02 | 21 471.90 | 20 952.03 |
| 135 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 21 978.74 | 21 794.08 | 21 630.11 |
| 136 | crystal (0.36)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.17) | 20 962.69 | 21 328.82 | 19 200.27 |
| 137 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 20 322.05 | 20 222.83 | 20 014.86 |
| 138 | javascript (14.16)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 18 772.99 | 18 007.49 | 17 163.27 |
| 139 | python (3.9)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 18 715.91 | 22 192.77 | 23 445.40 |
| 140 | python (3.9)| [fastapi](https://fastapi.tiangolo.com) (0.63) | 17 666.68 | 21 563.60 | 22 194.69 |
| 141 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 583.36 | 15 715.13 | 14 584.98 |
| 142 | python (3.9)| [molten](https://moltenframework.com) (1.0) | 17 040.59 | 20 725.66 | 20 566.46 |
| 143 | php (7.4)| [swoft](https://swoft.org) (2.0) | 16 945.62 | 20 209.39 | 20 681.45 |
| 144 | ruby (3.0)| [hanami-api](https://hanamirb.org) (0.1) | 16 775.49 | 16 277.14 | 15 913.37 |
| 145 | ruby (3.0)| [syro](https://github.com/soveran/syro) (3.2) | 16 193.22 | 15 829.45 | 15 427.76 |
| 146 | rust (1.50)| [iron](https://iron/iron) (0.6) | 16 049.66 | 16 346.11 | 16 004.77 |
| 147 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 15 897.29 | 18 277.44 | 18 820.72 |
| 148 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 15 837.64 | 14 097.19 | 13 106.90 |
| 149 | ruby (3.0)| [roda](https://roda.jeremyevans.net) (3.42) | 15 756.66 | 15 236.62 | 14 890.48 |
| 150 | go (1.16)| [macaron](https://go-macaron.com) (1.4) | 15 341.67 | 16 763.82 | 16 801.17 |
| 151 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 251.43 | 17 711.24 | 17 867.26 |
| 152 | javascript (14.16)| [express](https://expressjs.com) (4.17) | 15 132.96 | 15 653.44 | 17 729.16 |
| 153 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 15 065.33 | 17 670.61 | 17 657.12 |
| 154 | javascript (14.16)| [feathersjs](https://feathersjs.com) (4.5) | 14 675.59 | 17 581.39 | 17 830.99 |
| 155 | ruby (3.0)| [cuba](https://cuba.is) (3.9) | 14 126.41 | 13 671.68 | 13 485.55 |
| 156 | python (3.9)| [flask](https://flask.pocoo.org) (1.1) | 13 898.89 | 15 578.55 | 17 549.54 |
| 157 | java (11)| [struts2](https://struts.apache.org) (2.5) | 13 807.69 | 14 154.13 | 14 221.27 |
| 158 | ruby (3.0)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 13 766.24 | 13 295.17 | 13 114.82 |
| 159 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 13 687.27 | 13 710.46 | 13 611.45 |
| 160 | java (11)| [blade](https://lets-blade.com) (2.0) | 12 910.94 | 15 845.33 | 14 845.49 |
| 161 | ruby (3.0)| [rack_app](https://rack-app.com) (7.7) | 12 799.53 | 12 287.36 | 11 894.61 |
| 162 | ruby (3.0)| [camping](https://github.com/camping/camping) (2.1) | 12 612.96 | 12 229.15 | 12 123.57 |
| 163 | javascript (14.16)| [nestjs-express](https://nestjs.com) (7.6) | 12 017.95 | 12 632.95 | 13 562.13 |
| 164 | dart (2.12)| [start](https://github.com/lvivski/start) (0.4) | 11 603.70 | 11 091.99 | 10 793.26 |
| 165 | go (1.16)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 425.03 | 11 738.75 | 11 771.91 |
| 166 | python (3.9)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 10 360.68 | 9 652.76 | 9 926.72 |
| 167 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 10 090.60 | 10 349.49 | 10 386.95 |
| 168 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.6) | 9 866.60 | 10 017.26 | 10 165.42 |
| 169 | go (1.16)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 850.01 | 10 105.64 | 10 097.38 |
| 170 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.7) | 9 701.20 | 9 812.22 | 9 884.99 |
| 171 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 9 689.76 | 9 908.28 | 10 128.80 |
| 172 | python (3.9)| [guillotina](https://guillotina.io) (6.2) | 9 445.60 | 8 885.13 | 8 870.78 |
| 173 | php (7.4)| [laravel-s-lumen](https://github.com/hhxsv5/laravel-s) (3.7) | 9 141.68 | 9 893.37 | 9 858.93 |
| 174 | pony (0.39)| [jennet](https://github.com/Theodus/jennet) (0.1) | 8 625.37 | 13 296.67 | 12 628.45 |
| 175 | ruby (3.0)| [sinatra](https://sinatrarb.com) (2.1) | 8 620.66 | 8 402.76 | 8 386.36 |
| 176 | ruby (3.0)| [grape](https://ruby-grape.org) (1.5) | 7 842.15 | 7 579.37 | 7 558.20 |
| 177 | python (3.9)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 7 562.76 | 7 236.27 | 6 933.06 |
| 178 | python (3.9)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 382.92 | 6 958.83 | 6 469.16 |
| 179 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 7 096.13 | 7 561.32 | 7 453.35 |
| 180 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 7 003.57 | 7 431.63 | 7 395.31 |
| 181 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 6 951.03 | 7 312.73 | 7 246.59 |
| 182 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 822.68 | 7 171.39 | 7 147.44 |
| 183 | php (7.4)| [laravel-s-laravel](https://github.com/hhxsv5/laravel-s) (3.7) | 6 407.50 | 6 359.10 | 6 284.86 |
| 184 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 314.92 | 6 648.09 | 6 633.06 |
| 185 | python (3.9)| [django](https://djangoproject.com) (3.1) | 6 248.28 | 5 917.83 | 5 792.30 |
| 186 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 6 111.05 | 6 837.68 | 6 893.17 |
| 187 | python (3.9)| [tornado](https://tornadoweb.org) (6.1) | 5 906.85 | 5 764.74 | 5 658.40 |
| 188 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 5 784.10 | 5 952.55 | 5 896.73 |
| 189 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.2) | 5 108.15 | 5 342.06 | 5 248.99 |
| 190 | php (7.4)| [fatfree](https://fatfreeframework.com) (3.7) | 5 070.49 | 5 167.17 | 5 095.47 |
| 191 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 755.93 | 4 909.40 | 4 837.64 |
| 192 | php (7.4)| [cubex](https://cubex.io) (4.16) | 4 693.46 | 4 786.53 | 4 800.46 |
| 193 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 364.62 | 4 528.43 | 4 543.38 |
| 194 | javascript (14.16)| [sails](https://sailsjs.com) (1.4) | 4 272.33 | 4 226.43 | 4 135.08 |
| 195 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.3) | 4 085.73 | 4 262.08 | 4 241.33 |
| 196 | julia (1.6)| [merly](https://github.com/codeneomatrix/Merly.jl) (1.0) | 3 956.92 | 6 385.05 | 3 970.43 |
| 197 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.6) | 3 880.49 | 3 971.54 | 3 940.72 |
| 198 | php (7.4)| [nette](https://nette.org/en/) (3.1) | 3 873.94 | 3 978.66 | 3 984.14 |
| 199 | php (7.4)| [lumen](https://lumen.laravel.com) (8.2) | 3 813.96 | 3 900.22 | 3 913.24 |
| 200 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 3 459.08 | 3 540.78 | 3 520.66 |
| 201 | php (7.4)| [symfony](https://symfony.com) (5.2) | 3 017.37 | 3 036.88 | 3 057.48 |
| 202 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.6) | 2 997.14 | 3 014.28 | 3 020.81 |
| 203 | ruby (3.0)| [rails](https://rubyonrails.org) (6.1) | 2 772.36 | 2 743.93 | 2 747.87 |
| 204 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 531.35 | 2 562.70 | 2 564.61 |
| 205 | python (3.9)| [masonite](https://masoniteproject.com) (3.0) | 2 487.32 | 2 477.48 | 2 478.45 |
| 206 | php (7.4)| [driftphp](https://github.com/driftphp/driftphp) (0.1) | 2 093.74 | 667.95 | 1 154.05 |
| 207 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 923.67 | 1 853.13 | 1 832.27 |
| 208 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 916.14 | 1 846.89 | 1 759.05 |
| 209 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.3) | 1 863.34 | 1 870.87 | 1 870.86 |
| 210 | php (7.4)| [antidot](https://github.com/antidot-framework/react-framework) (0.2) | 1 849.01 | 667.30 | 1 909.42 |
| 211 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 715.65 | 1 738.90 | 1 741.88 |
| 212 | python (3.9)| [cyclone](https://cyclone.io) (1.3) | 1 584.85 | 1 590.54 | 1 576.67 |
| 213 | python (3.9)| [klein](https://github.com/twisted/klein) (20.6) | 1 512.07 | 1 537.18 | 1 506.93 |
| 214 | python (3.9)| [django-ninja](https://django-ninja.rest-framework.com) (0.11) | 1 330.15 | 1 652.44 | 1 679.55 |
| 215 | python (3.9)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 217.49 | 1 157.28 | 1 169.48 |
| 216 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.1) | 1 177.81 | 1 200.38 | 1 194.43 |
| 217 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 028.00 | 617.90 | 513.59 |
| 218 | php (7.4)| [laravel](https://laravel.com) (8.32) | 994.72 | 989.13 | 985.77 |
| 219 | php (7.4)| [unic](https://unicframework.github.io/docs) (1.0) | 595.31 | 478.18 | 182.05 |
| 220 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 287.49 | 304.57 | -95.99 |
| 221 | cpp (11)| [nawa](https://github.com/jatofg/nawa) (0.6) | 262.90 | NaN | NaN |
</a>

</details>

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

## Results (2021-04-03)



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
| 1 | go (1.16)| [fiber](https://gofiber.io) (2.7) | 182 485.36 | 192 580.26 | 191 645.23 |
| 2 | go (1.16)| [gearbox](https://gogearbox.com) (1.2) | 178 843.77 | 184 329.63 | 183 878.40 |
| 3 | go (1.16)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.6) | 178 095.36 | 188 126.68 | 187 872.96 |
| 4 | go (1.16)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 177 729.18 | 188 156.15 | 187 578.67 |
| 5 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 177 089.34 | 202 588.38 | 205 391.79 |
| 6 | go (1.16)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 176 528.03 | 187 032.66 | 186 075.04 |
| 7 | go (1.16)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.23) | 174 812.26 | 194 350.93 | 194 893.36 |
| 8 | java (11)| [activej](https://activej.io) (4.1) | 172 177.81 | 211 001.26 | 214 033.19 |
| 9 | javascript (14.16)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.4) | 163 534.06 | 208 834.30 | 214 590.58 |
| 10 | java (11)| [undertow](https://undertow.io) (2.2) | 163 266.62 | 199 907.69 | 204 511.01 |
| 11 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 163 028.26 | 199 073.08 | 203 587.94 |
| 12 | javascript (14.16)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 159 134.10 | 201 890.86 | 205 121.20 |
| 13 | rust (1.51)| [actix](https://actix.rs) (3.3) | 156 869.00 | 191 338.33 | 193 873.82 |
| 14 | php (7.4)| [webman](https://github.com/walkor/webman) (1.0) | 155 118.89 | 166 905.11 | 169 997.67 |
| 15 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 154 914.37 | 191 230.53 | 194 857.63 |
| 16 | java (11)| [vertx](https://vertx.io/docs/vertx-core/java/) (4.0) | 154 484.53 | 184 908.67 | 187 375.95 |
| 17 | java (11)| [jooby](https://jooby.io) (2.9) | 152 730.24 | 194 551.39 | 200 018.29 |
| 18 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 151 500.53 | 185 037.28 | 189 643.53 |
| 19 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 150 685.35 | 190 310.00 | 197 322.25 |
| 20 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 149 525.56 | 178 992.26 | 183 089.33 |
| 21 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 146 834.03 | 187 010.94 | 193 725.11 |
| 22 | java (11)| [vertx4web](https://vertx.io/docs/vertx-web/java/) (4.0) | 146 715.25 | 174 714.03 | 177 473.31 |
| 23 | crystal (0.36)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 144 911.25 | 175 022.67 | 174 705.46 |
| 24 | crystal (0.36)| [toro](https://github.com/soveran/toro) (0.4) | 144 331.77 | 175 167.31 | 175 433.51 |
| 25 | crystal (0.36)| [spider-gazelle](https://spider-gazelle.net) (4.2) | 142 228.87 | 172 353.72 | 171 513.65 |
| 26 | crystal (0.36)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 137 551.60 | 166 208.95 | 165 691.27 |
| 27 | crystal (0.36)| [kemal](https://kemalcr.com) (0.27) | 137 121.88 | 162 214.71 | 162 623.41 |
| 28 | crystal (0.36)| [grip](https://github.com/grip-framework/grip) (1.0) | 134 623.38 | 158 027.85 | 155 895.60 |
| 29 | crystal (0.36)| [amber](https://amberframework.org) (0.36) | 127 565.21 | 151 039.01 | 149 720.34 |
| 30 | java (11)| [quarkus](https://quarkus.io) (1.13) | 125 789.70 | 154 790.19 | 158 954.33 |
| 31 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 121 807.05 | 145 672.03 | 149 969.98 |
| 32 | javascript (14.16)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 119 843.47 | 136 876.48 | 138 971.69 |
| 33 | go (1.16)| [rte](https://github.com/jwilner/rte) (0.0) | 118 919.60 | 119 777.17 | 123 093.26 |
| 34 | go (1.16)| [clevergo](https://clevergo.tech) (0.5) | 118 606.60 | 120 431.70 | 123 309.41 |
| 35 | go (1.16)| [echo](https://echo.labstack.com) (4.2) | 117 757.61 | 119 399.46 | 122 403.48 |
| 36 | go (1.16)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 117 366.63 | 117 976.63 | 121 076.33 |
| 37 | clojure (1.1)| [donkey](https://github.com/AppsFlyer/donkey) (0.5) | 115 006.85 | 121 987.84 | 123 416.15 |
| 38 | go (1.16)| [gin](https://gin-gonic.com) (1.6) | 114 922.16 | 120 638.40 | 122 958.99 |
| 39 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 114 827.91 | 151 740.44 | 175 749.77 |
| 40 | go (1.16)| [aero](https://github.com/aerogo/aero) (1.3) | 113 052.19 | 113 437.38 | 116 644.98 |
| 41 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 112 870.29 | 145 223.36 | 149 267.33 |
| 42 | go (1.16)| [chi](https://github.com/go-chi/chi) (1.5) | 112 308.96 | 110 875.59 | 114 446.63 |
| 43 | go (1.16)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 111 657.28 | 118 738.66 | 120 658.38 |
| 44 | go (1.16)| [violetear](https://violetear.org) (7.0) | 110 001.95 | 108 555.21 | 112 341.66 |
| 45 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 109 867.13 | 128 776.81 | 132 863.15 |
| 46 | go (1.16)| [webgo](https://github.com/bnkamalesh/webgo) (5.0) | 109 273.93 | 107 697.24 | 111 182.74 |
| 47 | crystal (0.36)| [athena](https://github.com/athena-framework/athena) (0.13) | 109 268.65 | 119 919.66 | 112 594.26 |
| 48 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 109 244.42 | 127 689.77 | 131 689.34 |
| 49 | go (1.16)| [goroute](https://goroute.github.io) (0.0) | 108 882.29 | 106 262.13 | 110 661.91 |
| 50 | go (1.16)| [kami](https://github.com/guregu/kami) (2.2) | 108 535.54 | 113 457.25 | 114 155.91 |
| 51 | rust (1.51)| [iron](https://iron/iron) (0.6) | 107 834.20 | 102 285.02 | 104 511.48 |
| 52 | scala (2.13)| [finatra](https://twitter.github.io/finatra/) (21.1) | 107 102.03 | 124 871.80 | 125 242.13 |
| 53 | javascript (14.16)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 105 560.80 | 117 230.00 | 117 735.69 |
| 54 | go (1.16)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 105 297.22 | 101 337.77 | 106 039.47 |
| 55 | java (11)| [restheart](https://restheart.org) (5.3) | 104 634.37 | 110 054.03 | 111 042.23 |
| 56 | fsharp (5.0)| [falco](https://falcoframework.com) (3.0) | 104 445.51 | 120 571.57 | 124 057.37 |
| 57 | c (11)| [kore](https://kore.io) (3.3) | 104 296.32 | 131 367.42 | 159 226.19 |
| 58 | rust (1.51)| [salvo](https://github.com/salvo-rs/salvo) (0.9) | 103 715.12 | 129 047.79 | 140 327.47 |
| 59 | go (1.16)| [beego](https://beego.me) (1.12) | 102 091.56 | 105 652.07 | 108 868.89 |
| 60 | rust (1.51)| [gotham](https://gotham.rs) (0.6) | 100 350.63 | 125 083.70 | 136 285.92 |
| 61 | php (7.4)| [swoole](https://github.com/swoole/swoole-src) (4.6) | 98 676.80 | 118 526.77 | 132 210.83 |
| 62 | go (1.16)| [goyave](https://github.com/go-goyave/goyave) (3.7) | 95 753.96 | 96 216.83 | 100 065.98 |
| 63 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 94 683.26 | 105 500.64 | 105 117.52 |
| 64 | go (1.16)| [air](https://github.com/aofei/air) (0.21) | 93 960.16 | 92 621.16 | 96 151.45 |
| 65 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 93 925.64 | 138 750.52 | 149 397.65 |
| 66 | rust (1.51)| [nickel](https://nickel-org.github.io) (0.11) | 93 508.28 | 89 751.87 | 90 522.48 |
| 67 | php (7.4)| [swoole-coroutine](https://github.com/swoole/swoole-src) (4.6) | 87 580.51 | 124 797.96 | 136 462.40 |
| 68 | javascript (14.16)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 87 126.89 | 97 658.43 | 99 107.93 |
| 69 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 86 396.34 | 92 594.01 | 89 982.28 |
| 70 | scala (2.13)| [finch](https://finagle.github.io/finch/) (0.32) | 85 021.00 | 97 583.21 | 97 313.01 |
| 71 | javascript (14.16)| [0http](https://github.com/jkyberneees/0http) (3.1) | 81 283.21 | 92 444.47 | 93 552.33 |
| 72 | go (1.16)| [gf](https://goframe.org) (1.15) | 80 764.38 | 88 577.69 | 90 736.46 |
| 73 | scala (2.13)| [akkahttp](https://akka.io) (10.2) | 78 623.13 | 93 590.60 | 90 548.63 |
| 74 | javascript (14.16)| [restana](https://github.com/jkyberneees/ana) (4.7) | 78 169.56 | 89 178.98 | 89 706.42 |
| 75 | javascript (14.16)| [polka](https://github.com/lukeed/polka) (0.5) | 77 990.06 | 84 515.93 | 82 657.53 |
| 76 | javascript (14.16)| [rayo](https://rayo.js.org) (1.3) | 76 009.07 | 82 579.88 | 81 370.94 |
| 77 | swift (5.3)| [hummingbird](https://github.com/hummingbird-project/hummingbird) (0.8) | 75 680.20 | 83 263.38 | 84 049.19 |
| 78 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 75 617.45 | 79 349.73 | 77 988.71 |
| 79 | php (7.4)| [one](https://github.com/lizhichao/one) (2.2) | 75 439.33 | 88 060.03 | 95 014.78 |
| 80 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 74 976.46 | 88 227.09 | 91 847.12 |
| 81 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 73 962.26 | 182 624.86 | 190 965.21 |
| 82 | ruby (3.0)| [agoo](https://github.com/ohler55/agoo) (2.14) | 73 350.50 | 103 680.53 | 113 767.58 |
| 83 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 72 904.90 | 82 119.02 | 81 724.86 |
| 84 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 72 870.13 | 80 768.27 | 90 214.97 |
| 85 | php (7.4)| [hyperf](https://hyperf.io) (2.1) | 72 839.99 | 89 213.45 | 95 156.56 |
| 86 | java (11)| [javalin](https://javalin.io) (3.9) | 72 430.85 | 79 430.54 | 79 946.20 |
| 87 | java (11)| [spark](https://sparkjava.com) (2.9) | 72 331.23 | 80 034.14 | 81 924.36 |
| 88 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 70 289.50 | 92 476.15 | 94 782.25 |
| 89 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.4) | 69 770.26 | 63 601.12 | 62 153.10 |
| 90 | python (3.9)| [falcon](https://falconframework.org) (2.0) | 69 540.94 | 76 056.72 | 77 315.03 |
| 91 | javascript (14.16)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 66 597.72 | 73 940.55 | 71 916.39 |
| 92 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 65 882.24 | 75 849.89 | 76 172.54 |
| 93 | go (1.16)| [mars](https://github.com/roblillack/mars) (1.0) | 65 674.49 | 66 522.70 | 71 247.52 |
| 94 | javascript (14.16)| [foxify](https://foxify.js.org) (0.1) | 64 486.51 | 69 122.67 | 67 719.72 |
| 95 | javascript (14.16)| [nestjs-fastify](https://nestjs.com) (7.6) | 61 797.00 | 67 655.13 | 66 300.95 |
| 96 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 61 375.28 | 65 753.43 | 72 592.54 |
| 97 | javascript (14.16)| [koa](https://koajs.com) (2.13) | 59 898.68 | 62 859.50 | 61 518.37 |
| 98 | python (3.9)| [bottle](https://bottlepy.org) (0.12) | 58 414.75 | 63 240.92 | 64 089.13 |
| 99 | javascript (14.16)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 56 349.30 | 59 763.48 | 59 608.95 |
| 100 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 54 214.63 | 55 939.05 | 55 911.64 |
| 101 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 50 057.98 | 53 106.84 | 54 134.83 |
| 102 | javascript (14.16)| [fastify](https://fastify.io) (3.14) | 49 989.91 | 54 289.60 | 52 979.26 |
| 103 | python (3.9)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 49 760.81 | 56 294.26 | 56 731.86 |
| 104 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 49 635.16 | 50 058.76 | 49 874.14 |
| 105 | python (3.9)| [pyramid](https://trypyramid.com) (2.0) | 49 151.79 | 53 327.06 | 53 249.98 |
| 106 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 49 098.30 | 48 734.65 | 48 850.85 |
| 107 | python (3.9)| [hug](https://hug.rest) (2.6) | 49 021.27 | 52 171.19 | 52 058.74 |
| 108 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 48 740.55 | 50 134.93 | 50 409.02 |
| 109 | swift (5.3)| [vapor](https://vapor.codes) (4.41) | 48 477.56 | 52 035.91 | 51 630.46 |
| 110 | javascript (14.16)| [moleculer](https://moleculer.services) (0.14) | 47 498.40 | 50 802.35 | 49 486.22 |
| 111 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 46 839.87 | 57 783.98 | 60 224.34 |
| 112 | java (11)| [micronaut](https://micronaut.io) (1.2) | 46 627.40 | 55 291.03 | 55 523.48 |
| 113 | javascript (14.16)| [fyrejet](https://github.com/fyrejet/fyrejet) (3.1) | 45 691.93 | 49 372.35 | 48 383.36 |
| 114 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 43 252.20 | 44 005.12 | 42 569.93 |
| 115 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 43 240.77 | 50 552.38 | 50 239.95 |
| 116 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 43 106.54 | 38 406.78 | 33 044.16 |
| 117 | php (7.4)| [imi](https://imiphp.com) (1.2) | 42 759.56 | 48 205.34 | 49 646.31 |
| 118 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 41 946.39 | 46 061.19 | 45 693.07 |
| 119 | python (3.9)| [blacksheep](https://github.com/Neoteroi/BlackSheep) (1.0) | 41 073.45 | 47 843.93 | 49 094.24 |
| 120 | javascript (14.16)| [restify](https://restify.com) (8.5) | 41 059.11 | 44 430.60 | 43 273.07 |
| 121 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 41 007.60 | 45 689.85 | 47 823.61 |
| 122 | python (3.9)| [sanic](https://github.com/sanic-org/sanic) (21.3) | 40 829.27 | 46 381.95 | 46 930.61 |
| 123 | javascript (14.16)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (3.1) | 40 385.93 | 42 454.64 | 43 678.81 |
| 124 | python (3.9)| [starlette](https://starlette.io) (0.14) | 40 176.68 | 45 875.90 | 46 131.56 |
| 125 | python (3.9)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 39 420.57 | 44 749.47 | 45 891.74 |
| 126 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 37 321.93 | 40 349.68 | 39 571.54 |
| 127 | php (7.4)| [comet](https://github.com/gotzmann/comet) (1.2) | 37 090.74 | 37 708.29 | 37 205.56 |
| 128 | scala (2.13)| [play](https://playframework.com) (2.8) | 36 716.47 | 40 223.79 | 40 237.47 |
| 129 | javascript (14.16)| [hapi](https://hapijs.com) (20.1) | 35 871.27 | 38 732.44 | 38 105.48 |
| 130 | clojure (1.1)| [luminus](https://luminusweb.com) (3.98) | 34 569.40 | 36 796.31 | 36 717.08 |
| 131 | crystal (0.36)| [orion](https://github.com/obsidian/orion) (3.0) | 34 535.69 | 34 901.53 | 31 772.42 |
| 132 | dart (2.12)| [aqueduct](https://aqueduct.io) (3.3) | 34 420.78 | 35 174.29 | 34 196.41 |
| 133 | python (3.9)| [emmett](https://emmett.sh) (2.2) | 34 279.55 | 39 870.27 | 41 285.47 |
| 134 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 32 589.72 | 26 573.46 | 27 317.56 |
| 135 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 32 569.66 | 32 521.09 | 32 236.98 |
| 136 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 31 944.50 | 35 672.85 | 35 325.99 |
| 137 | java (11)| [jersey3-grizzly2](https://eclipse-ee4j.github.io/jersey) (3) | 31 340.95 | 36 673.17 | 36 234.24 |
| 138 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 31 221.55 | 29 907.59 | 28 362.53 |
| 139 | crystal (0.36)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.17) | 30 263.71 | 29 309.23 | 27 519.64 |
| 140 | python (3.9)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 30 240.95 | 33 154.08 | 33 933.39 |
| 141 | java (11)| [jersey-grizzly2](https://eclipse-ee4j.github.io/jersey) (2.33) | 29 198.20 | 35 930.68 | 35 469.15 |
| 142 | python (3.9)| [index.py](https://index-py.abersheeran.com) (0.17) | 27 310.42 | 32 732.00 | 32 176.15 |
| 143 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 27 167.65 | 26 002.38 | 27 060.38 |
| 144 | javascript (14.16)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 26 251.12 | 25 058.47 | 23 904.31 |
| 145 | python (3.9)| [fastapi](https://fastapi.tiangolo.com) (0.63) | 25 709.37 | 30 791.70 | 31 068.31 |
| 146 | python (3.9)| [molten](https://moltenframework.com) (1.0) | 25 315.07 | 30 036.65 | 29 714.39 |
| 147 | php (7.4)| [swoft](https://swoft.org) (2.0) | 25 072.02 | 29 864.20 | 31 202.17 |
| 148 | python (3.9)| [responder](https://python-responder.org) (2.0) | 23 932.47 | 31 188.48 | 31 277.21 |
| 149 | ruby (3.0)| [syro](https://github.com/soveran/syro) (3.2) | 23 393.11 | 22 697.43 | 22 381.13 |
| 150 | python (3.9)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 204.57 | 25 870.74 | 28 119.82 |
| 151 | ruby (3.0)| [hanami-api](https://hanamirb.org) (0.1) | 23 115.13 | 22 471.26 | 22 116.14 |
| 152 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 22 977.86 | 26 700.60 | 26 880.49 |
| 153 | go (1.16)| [macaron](https://go-macaron.com) (1.4) | 22 855.62 | 25 192.54 | 25 339.30 |
| 154 | ruby (3.0)| [roda](https://roda.jeremyevans.net) (3.42) | 22 375.62 | 21 795.07 | 21 403.89 |
| 155 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 21 721.23 | 19 275.97 | 17 889.95 |
| 156 | javascript (14.16)| [feathersjs](https://feathersjs.com) (4.5) | 21 681.03 | 25 970.18 | 25 798.54 |
| 157 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 21 636.56 | 24 774.99 | 25 982.51 |
| 158 | javascript (14.16)| [express](https://expressjs.com) (4.17) | 21 543.17 | 26 693.21 | 26 268.64 |
| 159 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 20 617.10 | 24 109.08 | 24 560.63 |
| 160 | python (3.9)| [flask](https://flask.pocoo.org) (1.1) | 20 498.43 | 25 792.35 | 25 707.26 |
| 161 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 19 790.21 | 18 040.69 | 16 640.75 |
| 162 | ruby (3.0)| [cuba](https://cuba.is) (3.9) | 19 708.22 | 19 158.30 | 18 938.57 |
| 163 | ruby (3.0)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 19 455.60 | 18 925.66 | 18 651.17 |
| 164 | ruby (3.0)| [rack_app](https://rack-app.com) (7.7) | 18 424.34 | 17 477.10 | 17 104.97 |
| 165 | ruby (3.0)| [camping](https://github.com/camping/camping) (2.1) | 17 935.44 | 17 335.78 | 17 181.77 |
| 166 | java (11)| [blade](https://lets-blade.com) (2.0) | 17 700.80 | 22 076.77 | 20 560.33 |
| 167 | go (1.16)| [tango](https://gitea.com/lunny/tango) (0.6) | 16 916.26 | 17 361.51 | 17 435.65 |
| 168 | javascript (14.16)| [nestjs-express](https://nestjs.com) (7.6) | 16 129.62 | 17 154.29 | 16 989.11 |
| 169 | dart (2.12)| [start](https://github.com/lvivski/start) (0.4) | 15 076.06 | 14 781.83 | 14 179.93 |
| 170 | go (1.16)| [gramework](https://github.com/gramework/gramework) (1.7) | 14 890.31 | 15 217.60 | 15 172.57 |
| 171 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 14 566.58 | 14 931.18 | 15 057.06 |
| 172 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.6) | 14 144.77 | 14 442.58 | 14 539.44 |
| 173 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 14 111.16 | 14 327.48 | 14 584.05 |
| 174 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.7) | 14 023.45 | 14 182.87 | 14 346.57 |
| 175 | python (3.9)| [guillotina](https://guillotina.io) (6.2) | 13 632.68 | 13 152.67 | 12 971.29 |
| 176 | php (7.4)| [laravel-s-lumen](https://github.com/hhxsv5/laravel-s) (3.7) | 13 235.47 | 13 942.01 | 13 830.58 |
| 177 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 12 482.43 | 13 032.26 | 12 461.03 |
| 178 | java (11)| [struts2](https://struts.apache.org) (2.5) | 12 352.82 | 13 064.18 | 13 069.41 |
| 179 | pony (0.39)| [jennet](https://github.com/Theodus/jennet) (0.1) | 12 249.29 | 19 982.61 | 18 840.53 |
| 180 | ruby (3.0)| [sinatra](https://sinatrarb.com) (2.1) | 12 240.50 | 11 948.92 | 11 908.68 |
| 181 | python (3.9)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 11 317.29 | 10 720.77 | 10 176.30 |
| 182 | ruby (3.0)| [grape](https://ruby-grape.org) (1.5) | 10 939.39 | 10 635.15 | 10 606.94 |
| 183 | python (3.9)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 10 920.86 | 10 196.19 | 9 516.87 |
| 184 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 10 124.09 | 10 924.23 | 10 835.63 |
| 185 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 9 790.15 | 10 614.10 | 10 441.41 |
| 186 | python (3.9)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 757.73 | 9 905.94 | 9 213.22 |
| 187 | php (7.4)| [laravel-s-laravel](https://github.com/hhxsv5/laravel-s) (3.7) | 9 480.75 | 9 453.74 | 9 419.23 |
| 188 | python (3.9)| [django](https://djangoproject.com) (3.1) | 9 014.78 | 9 197.04 | 8 729.88 |
| 189 | python (3.9)| [tornado](https://tornadoweb.org) (6.1) | 8 720.25 | 8 619.61 | 8 536.70 |
| 190 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 320.00 | 8 757.21 | 8 673.38 |
| 191 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 7 868.58 | 9 148.43 | 9 155.27 |
| 192 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.2) | 7 279.44 | 7 735.20 | 7 645.16 |
| 193 | php (7.4)| [fatfree](https://fatfreeframework.com) (3.7) | 7 190.62 | 7 589.77 | 7 543.36 |
| 194 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 7 087.41 | 7 489.18 | 7 434.89 |
| 195 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 6 887.51 | 7 209.82 | 7 181.45 |
| 196 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 639.15 | 6 968.19 | 6 946.89 |
| 197 | javascript (14.16)| [sails](https://sailsjs.com) (1.4) | 6 087.97 | 6 146.67 | 6 066.03 |
| 198 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.3) | 5 905.93 | 6 236.57 | 6 192.66 |
| 199 | php (7.4)| [ice](https://iceframework.org) (1.5) | 5 788.58 | 6 079.30 | 6 032.24 |
| 200 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.6) | 5 484.59 | 5 753.50 | 5 774.03 |
| 201 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 5 008.81 | 5 220.88 | 5 208.27 |
| 202 | php (7.4)| [cubex](https://cubex.io) (4.16) | 4 508.65 | 4 604.20 | 4 563.96 |
| 203 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.6) | 4 330.44 | 4 467.37 | 4 489.82 |
| 204 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 329.52 | 4 439.39 | 4 438.03 |
| 205 | ruby (3.0)| [rails](https://rubyonrails.org) (6.1) | 3 978.38 | 3 966.03 | 3 970.67 |
| 206 | php (7.4)| [lumen](https://lumen.laravel.com) (8.2) | 3 900.34 | 3 937.46 | 3 975.82 |
| 207 | php (7.4)| [nette](https://nette.org/en/) (3.1) | 3 836.11 | 3 947.88 | 3 955.68 |
| 208 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 3 717.79 | 3 849.22 | 3 843.29 |
| 209 | python (3.9)| [masonite](https://masoniteproject.com) (3.0) | 3 658.68 | 3 582.41 | 3 583.08 |
| 210 | julia (1.6)| [merly](https://github.com/codeneomatrix/Merly.jl) (1.0) | 3 506.55 | 8 524.07 | 5 126.28 |
| 211 | php (7.4)| [symfony](https://symfony.com) (5.2) | 3 026.07 | 3 033.93 | 3 032.59 |
| 212 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.3) | 2 734.75 | 2 767.95 | 2 773.71 |
| 213 | php (7.4)| [antidot](https://github.com/antidot-framework/react-framework) (0.2) | 2 516.75 | 4 053.02 | 1 724.68 |
| 214 | python (3.9)| [cyclone](https://cyclone.io) (1.3) | 2 376.65 | 2 374.38 | 2 353.03 |
| 215 | php (7.4)| [driftphp](https://github.com/driftphp/driftphp) (0.1) | 2 310.06 | 3 877.59 | 1 568.21 |
| 216 | python (3.9)| [django-ninja](https://django-ninja.rest-framework.com) (0.12) | 1 982.66 | 2 443.33 | 2 516.59 |
| 217 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 816.17 | 1 764.20 | 1 768.29 |
| 218 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 802.93 | 1 748.08 | 1 679.18 |
| 219 | python (3.9)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 781.48 | 1 708.15 | 1 712.20 |
| 220 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 690.15 | 1 572.52 | 979.10 |
| 221 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 685.22 | 1 704.46 | 1 709.66 |
| 222 | python (3.9)| [klein](https://github.com/twisted/klein) (20.6) | 1 486.96 | 1 517.00 | 1 495.85 |
| 223 | php (7.4)| [laravel](https://laravel.com) (8.35) | 1 468.49 | 1 475.56 | 1 477.01 |
| 224 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.1) | 1 184.25 | 1 204.79 | 1 203.18 |
| 225 | php (7.4)| [unic](https://unicframework.github.io/docs) (1.0) | 529.86 | 439.13 | 403.32 |
| 226 | r (4.0)| [plumber](https://rplumber.io) (1.1) | 422.54 | 448.90 | 433.58 |
| 227 | cpp (11)| [nawa](https://github.com/jatofg/nawa) (0.6) | 264.55 | NaN | NaN |
</a>

</details>

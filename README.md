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

## Results (2021-02-26)



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
| 1 | java (11)| [activej](https://activej.io) (4.0) | 171 633.18 | 210 643.81 | 214 736.59 |
| 2 | php (7.4)| [webman](https://github.com/walkor/webman) (1.0) | 155 484.95 | 168 637.31 | 171 752.01 |
| 3 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 151 466.64 | 184 117.13 | 186 783.07 |
| 4 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 123 587.57 | 135 556.00 | 137 389.69 |
| 5 | go (1.16)| [gearbox](https://gogearbox.com) (1.2) | 120 194.72 | 126 420.09 | 125 834.69 |
| 6 | go (1.16)| [fiber](https://gofiber.io) (2.5) | 119 271.88 | 131 804.44 | 131 546.98 |
| 7 | go (1.16)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.21) | 117 658.86 | 131 967.76 | 132 618.38 |
| 8 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 117 491.70 | 144 894.32 | 147 813.26 |
| 9 | go (1.16)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 116 517.67 | 129 886.13 | 129 308.17 |
| 10 | go (1.16)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 116 446.27 | 130 295.40 | 129 819.53 |
| 11 | go (1.16)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.6) | 116 097.59 | 129 255.90 | 128 859.65 |
| 12 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 113 393.93 | 140 956.87 | 143 969.92 |
| 13 | java (11)| [restheart](https://restheart.org) (5.3) | 113 099.17 | 117 858.80 | 117 869.91 |
| 14 | java (11)| [undertow](https://undertow.io) (2.2) | 112 863.31 | 138 577.68 | 141 191.80 |
| 15 | clojure (1.1)| [donkey](https://github.com/AppsFlyer/donkey) (0.5) | 111 383.61 | 113 278.31 | 115 082.74 |
| 16 | javascript (14.16)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.4) | 110 898.09 | 142 690.41 | 146 477.05 |
| 17 | java (11)| [jooby](https://jooby.io) (2.9) | 110 042.44 | 138 025.94 | 142 591.07 |
| 18 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 109 406.91 | 135 338.42 | 138 417.13 |
| 19 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 108 489.40 | 136 618.96 | 141 864.22 |
| 20 | go (1.16)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 108 359.73 | 105 144.65 | 109 497.64 |
| 21 | javascript (14.16)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 107 654.11 | 138 006.94 | 140 536.21 |
| 22 | java (11)| [vertx](https://vertx.io/docs/vertx-core/java/) (4.0) | 107 183.68 | 127 548.37 | 129 127.96 |
| 23 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 105 616.44 | 133 578.33 | 138 085.97 |
| 24 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 105 511.31 | 131 003.04 | 134 575.15 |
| 25 | java (11)| [vertx4web](https://vertx.io/docs/vertx-web/java/) (4.0) | 104 617.93 | 124 399.92 | 125 951.98 |
| 26 | php (7.4)| [swoole](https://github.com/swoole/swoole-src) (4.6) | 101 309.87 | 125 869.64 | 139 271.73 |
| 27 | crystal (0.36)| [toro](https://github.com/soveran/toro) (0.4) | 99 747.57 | 122 358.80 | 123 492.40 |
| 28 | crystal (0.36)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 99 656.07 | 122 880.41 | 123 036.35 |
| 29 | crystal (0.36)| [spider-gazelle](https://spider-gazelle.net) (4.2) | 97 083.20 | 121 159.18 | 121 825.09 |
| 30 | crystal (0.36)| [grip](https://github.com/grip-framework/grip) (4.0) | 96 394.39 | 115 119.83 | 115 871.67 |
| 31 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 95 520.68 | 117 815.34 | 120 519.74 |
| 32 | crystal (0.36)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 94 412.84 | 118 144.78 | 118 193.91 |
| 33 | crystal (0.36)| [kemal](https://kemalcr.com) (0.27) | 94 268.52 | 113 664.57 | 114 342.77 |
| 34 | c (11)| [kore](https://kore.io) (3.3) | 94 136.96 | 193 392.56 | 196 657.09 |
| 35 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 93 226.32 | 139 161.32 | 149 867.23 |
| 36 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 90 706.76 | 111 057.26 | 134 407.62 |
| 37 | crystal (0.36)| [amber](https://amberframework.org) (0.36) | 90 533.64 | 108 261.78 | 107 835.95 |
| 38 | php (7.4)| [swoole-coroutine](https://github.com/swoole/swoole-src) (4.6) | 89 995.88 | 124 776.23 | 138 491.94 |
| 39 | java (11)| [quarkus](https://quarkus.io) (1.12) | 85 850.34 | 103 608.89 | 106 385.23 |
| 40 | javascript (14.16)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 84 376.10 | 98 441.84 | 99 928.89 |
| 41 | scala (2.13)| [akkahttp](https://akka.io) (10.2) | 83 165.01 | 98 525.68 | 95 508.30 |
| 42 | go (1.16)| [gf](https://goframe.org) (1.15) | 82 571.79 | 90 593.71 | 92 414.73 |
| 43 | go (1.16)| [rte](https://github.com/jwilner/rte) (0.0) | 81 283.67 | 82 390.72 | 84 465.53 |
| 44 | go (1.16)| [clevergo](https://clevergo.tech) (0.5) | 80 674.27 | 82 093.65 | 84 107.40 |
| 45 | go (1.16)| [echo](https://echo.labstack.com) (4.2) | 80 619.82 | 81 643.36 | 83 860.65 |
| 46 | go (1.16)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 80 185.34 | 81 181.70 | 83 001.33 |
| 47 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 79 100.97 | 101 007.79 | 116 904.63 |
| 48 | go (1.16)| [gin](https://gin-gonic.com) (1.6) | 79 062.43 | 83 006.87 | 84 086.59 |
| 49 | go (1.16)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 783.29 | 83 355.63 | 84 500.63 |
| 50 | go (1.16)| [aero](https://github.com/aerogo/aero) (1.3) | 77 543.95 | 78 043.36 | 80 120.52 |
| 51 | go (1.16)| [chi](https://github.com/go-chi/chi) (1.5) | 76 953.38 | 76 246.73 | 78 761.41 |
| 52 | crystal (0.36)| [athena](https://github.com/athena-framework/athena) (0.13) | 76 913.90 | 85 030.71 | 80 698.08 |
| 53 | go (1.16)| [violetear](https://violetear.org) (7.0) | 76 785.39 | 77 410.89 | 79 466.71 |
| 54 | go (1.16)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 76 192.89 | 76 463.06 | 77 982.95 |
| 55 | go (1.16)| [kami](https://github.com/guregu/kami) (2.2) | 75 362.96 | 79 286.73 | 79 892.58 |
| 56 | go (1.16)| [goroute](https://goroute.github.io) (0.0) | 73 781.73 | 71 841.56 | 74 895.93 |
| 57 | python (3.9)| [falcon](https://falconframework.org) (2.0) | 73 741.71 | 81 041.13 | 82 904.21 |
| 58 | scala (2.13)| [finatra](https://twitter.github.io/finatra/) (21.1) | 73 336.98 | 84 423.87 | 84 853.59 |
| 59 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.3) | 73 159.76 | 65 489.49 | 64 124.22 |
| 60 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 72 597.33 | 84 100.01 | 86 619.77 |
| 61 | go (1.16)| [beego](https://beego.me) (1.12) | 72 468.90 | 75 320.85 | 77 350.30 |
| 62 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 71 878.00 | 83 573.73 | 85 914.19 |
| 63 | javascript (14.16)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 71 767.92 | 79 511.64 | 80 530.78 |
| 64 | fsharp (5.0)| [falco](https://falcoframework.com) (3.0) | 69 703.20 | 82 514.10 | 81 521.95 |
| 65 | go (1.16)| [air](https://github.com/aofei/air) (0.21) | 65 384.25 | 63 964.08 | 66 199.69 |
| 66 | go (1.16)| [goyave](https://github.com/System-Glitch/goyave) (3.6) | 65 065.45 | 64 469.48 | 67 424.37 |
| 67 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 65 055.24 | 72 702.35 | 73 109.70 |
| 68 | javascript (14.16)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 64 706.10 | 69 168.05 | 70 475.07 |
| 69 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 61 964.18 | 67 778.98 | 67 244.76 |
| 70 | scala (2.13)| [finch](https://finagle.github.io/finch/) (0.32) | 60 829.20 | 67 938.78 | 67 785.91 |
| 71 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 59 094.68 | 62 032.98 | 63 238.02 |
| 72 | javascript (14.16)| [0http](https://github.com/jkyberneees/0http) (3.0) | 58 888.05 | 66 119.91 | 67 734.10 |
| 73 | rust (1.50)| [salvo](https://github.com/kenorld/salvo) (0.5) | 58 506.95 | 62 529.03 | 63 861.81 |
| 74 | javascript (14.16)| [polka](https://github.com/lukeed/polka) (0.5) | 57 603.42 | 62 395.90 | 61 273.30 |
| 75 | javascript (14.16)| [restana](https://github.com/jkyberneees/ana) (4.7) | 56 017.94 | 63 672.97 | 64 307.77 |
| 76 | javascript (14.16)| [fastify](https://fastify.io) (3.12) | 55 701.86 | 60 689.01 | 59 607.01 |
| 77 | javascript (14.16)| [rayo](https://rayo.js.org) (1.3) | 55 573.99 | 60 652.88 | 59 393.02 |
| 78 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 54 583.04 | 57 577.54 | 56 138.32 |
| 79 | python (3.9)| [pyramid](https://trypyramid.com) (1.1) | 53 635.79 | 57 631.66 | 57 916.76 |
| 80 | javascript (14.16)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 53 410.26 | 58 011.98 | 57 324.26 |
| 81 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 52 915.65 | 61 269.29 | 63 724.36 |
| 82 | php (7.4)| [one](https://github.com/lizhichao/one) (2.2) | 51 836.03 | 60 335.11 | 67 146.27 |
| 83 | java (11)| [javalin](https://javalin.io) (3.9) | 51 758.29 | 54 678.45 | 54 893.17 |
| 84 | php (7.4)| [hyperf](https://hyperf.io) (2.1) | 51 615.49 | 60 990.61 | 65 846.28 |
| 85 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 51 460.25 | 56 417.28 | 63 437.13 |
| 86 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 51 237.59 | 66 225.20 | 69 343.20 |
| 87 | java (11)| [micronaut](https://micronaut.io) (1.2) | 51 182.52 | 57 643.93 | 57 928.65 |
| 88 | ruby (3.0)| [agoo](https://github.com/ohler55/agoo) (2.14) | 51 009.14 | 73 137.85 | 79 873.23 |
| 89 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 49 920.39 | 56 387.53 | 56 776.38 |
| 90 | java (11)| [spark](https://sparkjava.com) (2.9) | 49 824.59 | 54 186.73 | 55 242.14 |
| 91 | javascript (14.16)| [nestjs-fastify](https://nestjs.com) (7.6) | 48 543.16 | 52 979.76 | 51 360.02 |
| 92 | go (1.16)| [mars](https://github.com/roblillack/mars) (1.0) | 46 388.51 | 46 470.19 | 49 709.25 |
| 93 | rust (1.50)| [actix](https://actix.rs) (3.3) | 46 321.11 | 45 826.22 | 46 902.84 |
| 94 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 45 430.47 | 48 853.20 | 51 347.95 |
| 95 | javascript (14.16)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 44 654.63 | 47 736.89 | 47 299.99 |
| 96 | javascript (14.16)| [foxify](https://foxify.js.org) (0.1) | 44 632.03 | 48 865.63 | 47 726.29 |
| 97 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 278.03 | 44 969.19 | 45 441.67 |
| 98 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 42 946.91 | 33 030.47 | 32 748.95 |
| 99 | javascript (14.16)| [koa](https://koajs.com) (2.13) | 41 440.89 | 44 010.31 | 43 201.19 |
| 100 | python (3.9)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 41 064.63 | 45 853.50 | 46 913.63 |
| 101 | python (3.9)| [bottle](https://bottlepy.org) (0.12) | 39 764.87 | 43 347.35 | 43 930.87 |
| 102 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 36 692.01 | 37 650.39 | 38 391.20 |
| 103 | javascript (14.16)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 36 437.91 | 39 796.40 | 38 876.88 |
| 104 | swift (5.3)| [vapor](https://vapor.codes) (4.41) | 36 119.34 | 37 703.40 | 37 643.25 |
| 105 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 35 125.82 | 37 383.53 | 37 621.82 |
| 106 | javascript (14.16)| [moleculer](https://moleculer.services) (0.14) | 35 113.79 | 35 901.22 | 35 478.47 |
| 107 | javascript (14.16)| [hapi](https://hapijs.com) (20.1) | 34 652.36 | 35 692.14 | 34 485.67 |
| 108 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 34 493.95 | 33 816.26 | 33 730.06 |
| 109 | python (3.9)| [emmett](https://emmett.sh) (2.1) | 34 313.42 | 40 940.23 | 42 074.16 |
| 110 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 34 203.67 | 35 375.54 | 35 230.13 |
| 111 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 33 956.92 | 34 861.99 | 33 747.17 |
| 112 | java (11)| [jersey3-grizzly2](https://eclipse-ee4j.github.io/jersey) (3) | 33 801.74 | 38 643.48 | 37 877.03 |
| 113 | java (11)| [jersey-grizzly2](https://eclipse-ee4j.github.io/jersey) (2.33) | 33 523.25 | 38 395.38 | 37 680.41 |
| 114 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 33 498.50 | 43 176.62 | 45 257.20 |
| 115 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 33 437.31 | 37 977.47 | 38 121.25 |
| 116 | python (3.9)| [hug](https://hug.rest) (2.6) | 33 285.94 | 35 810.11 | 53 555.95 |
| 117 | python (3.9)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 32 689.74 | 38 874.86 | 39 614.14 |
| 118 | rust (1.50)| [nickel](https://nickel-org.github.io) (0.11) | 31 997.24 | 31 670.83 | 31 684.06 |
| 119 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 31 730.19 | 27 153.91 | 22 439.42 |
| 120 | python (3.9)| [sanic](https://github.com/huge-success/sanic) (20.12) | 31 590.00 | 34 682.58 | 34 614.61 |
| 121 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 30 936.47 | 31 217.71 | 30 848.79 |
| 122 | javascript (14.16)| [restify](https://restify.com) (8.5) | 29 425.67 | 31 541.30 | 30 118.94 |
| 123 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 29 269.45 | 32 139.91 | 32 039.49 |
| 124 | php (7.4)| [imi](https://imiphp.com) (1.2) | 29 224.51 | 33 310.75 | 33 532.05 |
| 125 | javascript (14.16)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 28 639.30 | 46 428.74 | 46 855.54 |
| 126 | python (3.9)| [blacksheep](https://github.com/Neoteroi/BlackSheep) (1.0) | 28 192.58 | 32 269.66 | 33 763.60 |
| 127 | scala (2.13)| [play](https://playframework.com) (2.8) | 28 021.88 | 30 108.78 | 29 919.90 |
| 128 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 27 973.33 | 30 624.94 | 33 253.67 |
| 129 | php (7.4)| [comet](https://github.com/gotzmann/comet) (1.2) | 27 281.46 | 27 576.10 | 26 751.44 |
| 130 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 27 211.55 | 29 455.59 | 28 783.90 |
| 131 | python (3.9)| [starlette](https://starlette.io) (0.14) | 27 142.47 | 31 377.50 | 32 496.37 |
| 132 | rust (1.50)| [gotham](https://gotham.rs) (0.5) | 26 298.87 | 29 805.17 | 31 170.12 |
| 133 | python (3.9)| [responder](https://python-responder.org) (2.0) | 26 172.72 | 32 046.90 | 32 436.80 |
| 134 | crystal (0.36)| [orion](https://github.com/obsidian/orion) (3.0) | 25 235.73 | 24 402.82 | 23 893.44 |
| 135 | python (3.9)| [index.py](https://index-py.abersheeran.com) (0.16) | 25 175.80 | 28 621.18 | 30 276.41 |
| 136 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 24 260.30 | 26 515.45 | 25 902.28 |
| 137 | python (3.9)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 24 084.58 | 26 072.26 | 28 306.65 |
| 138 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 22 654.21 | 21 458.22 | 20 509.24 |
| 139 | clojure (1.1)| [luminus](https://luminusweb.com) (3.97) | 22 623.58 | 22 346.77 | 21 802.19 |
| 140 | python (3.9)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 22 262.14 | 23 553.55 | 24 218.13 |
| 141 | crystal (0.36)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.17) | 22 044.45 | 21 608.66 | 19 990.75 |
| 142 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 21 551.00 | 21 387.73 | 20 979.76 |
| 143 | javascript (14.16)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 20 342.05 | 22 746.69 | 22 343.68 |
| 144 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 20 241.40 | 19 668.77 | 20 223.23 |
| 145 | javascript (14.16)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 19 268.55 | 18 567.02 | 16 933.06 |
| 146 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 577.09 | 15 870.15 | 14 615.97 |
| 147 | python (3.9)| [molten](https://moltenframework.com) (1.0) | 17 209.13 | 21 195.42 | 21 014.77 |
| 148 | python (3.9)| [fastapi](https://fastapi.tiangolo.com) (0.63) | 17 171.32 | 21 933.53 | 22 084.97 |
| 149 | php (7.4)| [swoft](https://swoft.org) (2.0) | 16 814.81 | 20 411.00 | 20 718.70 |
| 150 | ruby (3.0)| [hanami-api](https://hanamirb.org) (0.1) | 16 700.52 | 16 577.22 | 15 941.70 |
| 151 | rust (1.50)| [iron](https://ironframework.io) (0.6) | 16 301.21 | 16 376.10 | 16 498.48 |
| 152 | ruby (3.0)| [syro](https://github.com/soveran/syro) (3.2) | 16 201.23 | 15 736.24 | 15 419.62 |
| 153 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 15 863.98 | 14 038.22 | 13 096.23 |
| 154 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 15 705.68 | 18 598.46 | 18 495.11 |
| 155 | go (1.16)| [macaron](https://go-macaron.com) (1.4) | 15 330.27 | 16 839.60 | 16 865.20 |
| 156 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 320.14 | 17 561.85 | 17 977.18 |
| 157 | ruby (3.0)| [roda](https://roda.jeremyevans.net) (3.41) | 15 264.83 | 14 801.64 | 14 621.89 |
| 158 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 15 149.29 | 17 390.24 | 17 760.09 |
| 159 | ruby (3.0)| [cuba](https://cuba.is) (3.9) | 14 139.62 | 13 681.12 | 13 444.57 |
| 160 | javascript (14.16)| [express](https://expressjs.com) (4.17) | 14 030.20 | 17 828.85 | 18 052.15 |
| 161 | javascript (14.16)| [feathersjs](https://feathersjs.com) (4.5) | 13 941.57 | 17 447.36 | 17 473.24 |
| 162 | python (3.9)| [flask](https://flask.pocoo.org) (1.1) | 13 930.81 | 17 770.44 | 16 999.08 |
| 163 | java (11)| [struts2](https://struts.apache.org) (2.5) | 13 700.13 | 14 049.11 | 14 074.01 |
| 164 | ruby (3.0)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 13 634.25 | 13 466.82 | 13 139.95 |
| 165 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 13 621.66 | 14 477.94 | 21 369.48 |
| 166 | ruby (3.0)| [camping](https://github.com/camping/camping) (2.1) | 12 937.34 | 12 594.66 | 12 315.47 |
| 167 | java (11)| [blade](https://lets-blade.com) (2.0) | 12 852.14 | 16 281.59 | 14 858.85 |
| 168 | ruby (3.0)| [rack_app](https://rack-app.com) (7.7) | 12 779.48 | 12 070.98 | 11 797.93 |
| 169 | javascript (14.16)| [nestjs-express](https://nestjs.com) (7.6) | 11 506.52 | 12 948.64 | 11 291.49 |
| 170 | go (1.16)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 374.06 | 11 674.64 | 11 707.86 |
| 171 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 10 925.31 | 10 770.56 | 10 415.61 |
| 172 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 10 103.33 | 10 310.67 | 10 425.07 |
| 173 | go (1.16)| [gramework](https://github.com/gramework/gramework) (1.7) | 10 025.13 | 10 193.49 | 10 165.72 |
| 174 | python (3.9)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 859.81 | 9 795.05 | 9 694.57 |
| 175 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.6) | 9 836.09 | 10 032.51 | 10 148.65 |
| 176 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 9 787.23 | 9 955.19 | 9 795.72 |
| 177 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.7) | 9 744.87 | 9 848.88 | 9 976.39 |
| 178 | python (3.9)| [guillotina](https://guillotina.io) (6.1) | 9 145.59 | 8 966.35 | 8 761.30 |
| 179 | php (7.4)| [laravel-s-lumen](https://github.com/hhxsv5/laravel-s) (3.7) | 8 916.14 | 9 827.93 | 9 906.67 |
| 180 | ruby (3.0)| [sinatra](https://sinatrarb.com) (2.1) | 8 613.45 | 8 397.89 | 8 410.77 |
| 181 | ruby (3.0)| [grape](https://ruby-grape.org) (1.5) | 7 799.92 | 7 572.39 | 7 553.94 |
| 182 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 7 785.29 | 13 358.91 | 12 564.06 |
| 183 | python (3.9)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 7 444.62 | 7 019.81 | 6 743.93 |
| 184 | python (3.9)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 233.88 | 6 976.77 | 6 439.95 |
| 185 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 7 095.63 | 7 535.51 | 7 436.33 |
| 186 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 7 094.22 | 7 548.47 | 7 450.56 |
| 187 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 6 919.57 | 7 326.89 | 7 195.96 |
| 188 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 892.02 | 7 175.33 | 7 155.08 |
| 189 | php (7.4)| [laravel-s-laravel](https://github.com/hhxsv5/laravel-s) (3.7) | 6 406.22 | 6 383.32 | 6 289.61 |
| 190 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 360.76 | 6 750.46 | 6 654.60 |
| 191 | python (3.9)| [django](https://djangoproject.com) (3.1) | 6 289.17 | 5 924.44 | 5 903.13 |
| 192 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 6 091.20 | 6 755.45 | 6 799.00 |
| 193 | python (3.9)| [tornado](https://tornadoweb.org) (6.1) | 5 845.72 | 5 765.69 | 5 641.28 |
| 194 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 5 831.75 | 5 967.68 | 5 921.05 |
| 195 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.2) | 5 102.47 | 5 326.11 | 5 253.80 |
| 196 | php (7.4)| [fatfree](https://fatfreeframework.com) (3.7) | 5 046.47 | 5 140.18 | 5 106.82 |
| 197 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 716.48 | 4 911.46 | 4 862.17 |
| 198 | php (7.4)| [cubex](https://cubex.io) (4.16) | 4 620.98 | 4 805.96 | 4 795.51 |
| 199 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 392.84 | 4 545.55 | 4 521.30 |
| 200 | javascript (14.16)| [sails](https://sailsjs.com) (1.4) | 4 211.73 | 4 265.63 | 4 174.04 |
| 201 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.3) | 4 143.64 | 4 233.82 | 4 272.96 |
| 202 | julia (1.6)| [merly](https://github.com/codeneomatrix/Merly.jl) (1.0) | 4 132.34 | 6 641.57 | 4 307.84 |
| 203 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.6) | 3 867.35 | 3 938.01 | 3 952.57 |
| 204 | php (7.4)| [nette](https://nette.org/en/) (3.1) | 3 842.67 | 3 954.50 | 3 956.85 |
| 205 | php (7.4)| [lumen](https://lumen.laravel.com) (8.2) | 3 824.11 | 3 919.69 | 3 911.79 |
| 206 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 3 484.84 | 3 562.50 | 3 561.42 |
| 207 | php (7.4)| [symfony](https://symfony.com) (5.2) | 3 037.07 | 3 055.33 | 3 055.34 |
| 208 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.6) | 2 968.31 | 3 010.55 | 3 018.90 |
| 209 | ruby (3.0)| [rails](https://rubyonrails.org) (6.1) | 2 788.73 | 2 758.43 | 2 774.38 |
| 210 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 515.81 | 2 572.25 | 2 593.22 |
| 211 | python (3.9)| [masonite](https://masoniteproject.com) (3.0) | 2 327.08 | 2 472.08 | 2 474.08 |
| 212 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 915.81 | 1 851.02 | 1 773.39 |
| 213 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 909.67 | 1 843.40 | 1 822.04 |
| 214 | php (7.4)| [driftphp](https://github.com/driftphp/driftphp) (0.1) | 1 903.03 | 663.62 | 1 277.04 |
| 215 | php (7.4)| [antidot](https://github.com/antidot-framework/react-framework) (0.2) | 1 878.66 | 671.01 | 2 017.03 |
| 216 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.3) | 1 860.85 | 1 870.47 | 1 856.61 |
| 217 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 712.36 | 1 730.83 | 1 732.70 |
| 218 | python (3.9)| [klein](https://github.com/twisted/klein) (20.6) | 1 588.82 | 1 607.67 | 1 577.86 |
| 219 | python (3.9)| [cyclone](https://cyclone.io) (1.3) | 1 584.88 | 1 589.49 | 1 564.42 |
| 220 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 439.85 | 645.09 | 509.09 |
| 221 | python (3.9)| [django-ninja](https://django-ninja.rest-framework.com) (0.10) | 1 385.54 | 1 617.41 | 1 669.56 |
| 222 | python (3.9)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 218.73 | 1 175.52 | 1 185.24 |
| 223 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.1) | 1 188.21 | 1 210.26 | 1 209.24 |
| 224 | php (7.4)| [laravel](https://laravel.com) (8.29) | 978.71 | 983.14 | 985.82 |
| 225 | php (7.4)| [unic](https://unicframework.github.io/docs) (1.0) | 506.30 | 423.05 | 242.10 |
| 226 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 287.33 | 302.55 | -96.03 |
| 227 | cpp (11)| [nawa](https://github.com/jatofg/nawa) (0.6) | 257.63 | NaN | NaN |
</a>

</details>

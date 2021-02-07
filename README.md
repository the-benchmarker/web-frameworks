# Which is the fastest ?
----------
#### Simple framework comparison
----------
<p align="center">
   <a href="https://github.com/the-benchmarker/web-frameworks/actions?query=workflow%3ACI" target="_blank">
      <img src="https://github.com/the-benchmarker/web-frameworks/workflows/CI/badge.svg" alt="Test">
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

## Results (2021-02-16)



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
| 1 | java (11)| [activej](https://activej.io) (3.0) | 174 308.59 | 216 441.42 | 221 693.11 |
| 2 | php (7.4)| [webman](https://github.com/walkor/webman) (1.0) | 158 674.91 | 170 699.01 | 172 941.85 |
| 3 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 150 794.34 | 186 318.22 | 186 651.14 |
| 4 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 123 887.12 | 136 537.47 | 138 175.82 |
| 5 | go (1.15)| [fiber](https://gofiber.io) (2.5) | 121 724.16 | 130 491.66 | 129 377.71 |
| 6 | go (1.15)| [gearbox](https://gogearbox.com) (1.2) | 120 695.44 | 123 499.54 | 123 196.20 |
| 7 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 118 508.87 | 147 144.25 | 150 071.98 |
| 8 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 118 427.29 | 127 755.80 | 126 616.99 |
| 9 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.6) | 117 634.31 | 128 254.80 | 127 407.35 |
| 10 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.2) | 116 565.36 | 130 328.67 | 129 872.73 |
| 11 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 116 241.60 | 128 748.54 | 128 373.34 |
| 12 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 115 481.61 | 142 300.07 | 145 457.86 |
| 13 | java (11)| [undertow](https://undertow.io) (2.2) | 113 046.22 | 137 819.03 | 139 997.60 |
| 14 | javascript (14.15)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.4) | 111 871.98 | 143 007.45 | 146 461.42 |
| 15 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 109 441.02 | 136 625.08 | 141 694.47 |
| 16 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 109 183.54 | 136 236.51 | 139 178.75 |
| 17 | java (11)| [jooby](https://jooby.io) (2.9) | 109 152.59 | 138 007.24 | 142 396.62 |
| 18 | javascript (14.15)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 108 457.10 | 138 395.57 | 140 963.20 |
| 19 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 108 201.93 | 104 829.32 | 109 350.15 |
| 20 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 107 592.66 | 134 059.56 | 137 443.52 |
| 21 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 107 507.80 | 131 136.18 | 135 411.95 |
| 22 | java (11)| [vertx](https://vertx.io/docs/vertx-core/java/) (4.0) | 106 607.74 | 127 531.22 | 128 786.60 |
| 23 | clojure (1.1)| [donkey](https://github.com/AppsFlyer/donkey) (0.4) | 106 572.60 | 113 280.59 | 114 252.75 |
| 24 | java (11)| [vertx4web](https://vertx.io/docs/vertx-web/java/) (4.0) | 104 974.24 | 121 914.99 | 125 386.69 |
| 25 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 99 834.14 | 121 597.45 | 122 243.75 |
| 26 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (4.1) | 98 956.47 | 121 089.22 | 121 918.32 |
| 27 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 98 678.85 | 121 119.54 | 121 367.05 |
| 28 | php (7.4)| [swoole](https://github.com/swoole/swoole-src) (4.6) | 98 587.35 | 117 862.22 | 132 587.63 |
| 29 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 95 148.79 | 139 767.48 | 150 612.01 |
| 30 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 94 741.69 | 117 796.64 | 120 345.53 |
| 31 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 94 500.70 | 113 655.41 | 113 198.86 |
| 32 | crystal (0.35)| [kemal](https://kemalcr.com) (0.27) | 92 966.89 | 112 108.17 | 112 509.20 |
| 33 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 91 309.19 | 106 398.14 | 106 344.53 |
| 34 | php (7.4)| [swoole-coroutine](https://github.com/swoole/swoole-src) (4.6) | 88 159.34 | 125 366.77 | 135 590.13 |
| 35 | java (11)| [quarkus](https://quarkus.io) (1.11) | 86 871.53 | 104 797.45 | 107 512.17 |
| 36 | javascript (14.15)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 83 544.67 | 98 845.84 | 100 032.84 |
| 37 | scala (2.13)| [akkahttp](https://akka.io) (10.2) | 83 199.12 | 98 194.09 | 95 149.84 |
| 38 | c (11)| [kore](https://kore.io) (3.3) | 83 138.48 | 180 353.00 | 191 909.03 |
| 39 | go (1.15)| [gf](https://goframe.org) (1.15) | 82 503.66 | 89 980.39 | 92 119.94 |
| 40 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.12) | 82 492.60 | 92 791.41 | 90 111.99 |
| 41 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 80 877.65 | 103 544.77 | 112 746.25 |
| 42 | go (1.15)| [echo](https://echo.labstack.com) (4.2) | 80 552.65 | 81 662.01 | 83 833.88 |
| 43 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 80 469.99 | 81 244.66 | 83 168.89 |
| 44 | go (1.15)| [clevergo](https://clevergo.tech) (0.5) | 80 362.12 | 81 015.23 | 82 853.24 |
| 45 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 80 055.53 | 80 785.35 | 82 821.30 |
| 46 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 852.33 | 83 116.23 | 84 766.76 |
| 47 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 78 643.11 | 82 312.25 | 83 913.62 |
| 48 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 77 409.08 | 128 177.84 | 133 660.20 |
| 49 | go (1.15)| [violetear](https://violetear.org) (7.0) | 76 647.71 | 76 873.14 | 78 968.95 |
| 50 | go (1.15)| [chi](https://github.com/go-chi/chi) (1.5) | 76 538.63 | 75 809.08 | 77 841.77 |
| 51 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 76 018.74 | 75 654.89 | 77 796.87 |
| 52 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 75 275.67 | 87 920.56 | 90 791.59 |
| 53 | scala (2.13)| [finatra](https://twitter.github.io/finatra/) (21.1) | 75 164.40 | 86 831.80 | 87 165.66 |
| 54 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 74 844.38 | 74 726.11 | 76 547.05 |
| 55 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 74 626.82 | 78 626.31 | 78 937.84 |
| 56 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 73 172.71 | 71 291.58 | 74 350.72 |
| 57 | python (3.9)| [falcon](https://falconframework.org) (2.0) | 72 912.64 | 81 378.02 | 82 609.11 |
| 58 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 72 231.38 | 83 825.87 | 86 382.54 |
| 59 | go (1.15)| [beego](https://beego.me) (1.12) | 72 036.01 | 74 866.18 | 76 926.28 |
| 60 | javascript (14.15)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 71 936.24 | 79 134.48 | 79 164.47 |
| 61 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.3) | 71 899.41 | 65 173.31 | 65 434.91 |
| 62 | fsharp (5.0)| [falco](https://falcoframework.com) (3.0) | 70 553.34 | 80 956.30 | 83 089.01 |
| 63 | go (1.15)| [air](https://github.com/aofei/air) (0.21) | 65 012.05 | 63 603.59 | 65 941.62 |
| 64 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 64 553.93 | 71 728.72 | 71 888.18 |
| 65 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.6) | 61 882.77 | 62 490.68 | 65 096.32 |
| 66 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 61 515.68 | 67 218.45 | 66 625.92 |
| 67 | javascript (14.15)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 61 327.23 | 69 041.73 | 70 482.65 |
| 68 | scala (2.13)| [finch](https://finagle.github.io/finch/) (0.32) | 61 154.67 | 68 135.20 | 68 155.12 |
| 69 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 60 054.21 | 63 261.28 | 64 501.16 |
| 70 | rust (1.50)| [salvo](https://github.com/kenorld/salvo) (0.5) | 57 997.07 | 62 614.44 | 63 640.95 |
| 71 | javascript (14.15)| [0http](https://github.com/jkyberneees/0http) (3.0) | 57 434.28 | 65 618.66 | 66 555.03 |
| 72 | javascript (14.15)| [polka](https://github.com/lukeed/polka) (0.5) | 56 425.56 | 61 891.47 | 60 682.46 |
| 73 | javascript (14.15)| [fastify](https://fastify.io) (3.12) | 55 403.52 | 60 214.85 | 58 881.41 |
| 74 | javascript (14.15)| [restana](https://github.com/jkyberneees/ana) (4.7) | 55 393.06 | 62 333.20 | 63 689.36 |
| 75 | javascript (14.15)| [rayo](https://rayo.js.org) (1.3) | 55 299.79 | 60 642.96 | 59 807.49 |
| 76 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 54 431.79 | 57 437.76 | 56 036.75 |
| 77 | javascript (14.15)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 54 319.70 | 58 941.78 | 58 172.43 |
| 78 | python (3.9)| [pyramid](https://trypyramid.com) (1.1) | 53 476.83 | 58 750.71 | 58 640.27 |
| 79 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 53 189.03 | 61 644.09 | 63 856.44 |
| 80 | php (7.4)| [one](https://github.com/lizhichao/one) (2.2) | 52 702.96 | 62 257.04 | 67 396.97 |
| 81 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 52 074.60 | 67 382.51 | 70 071.76 |
| 82 | php (7.4)| [hyperf](https://hyperf.io) (2.1) | 52 065.40 | 58 649.34 | 67 453.60 |
| 83 | ruby (3.0)| [agoo](https://github.com/ohler55/agoo) (2.14) | 51 627.40 | 72 479.55 | 80 582.44 |
| 84 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 51 537.77 | 56 529.91 | 63 772.80 |
| 85 | java (11)| [javalin](https://javalin.io) (3.9) | 51 332.32 | 54 630.04 | 54 502.62 |
| 86 | java (11)| [micronaut](https://micronaut.io) (1.2) | 50 945.35 | 57 504.64 | 57 792.70 |
| 87 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 50 630.42 | 57 269.90 | 57 711.63 |
| 88 | java (11)| [spark](https://sparkjava.com) (2.9) | 50 162.16 | 54 960.32 | 55 636.09 |
| 89 | rust (1.50)| [actix](https://actix.rs) (3.3) | 49 045.39 | 47 477.71 | 44 505.51 |
| 90 | javascript (14.15)| [nestjs-fastify](https://nestjs.com) (7.6) | 48 480.34 | 52 399.83 | 51 516.39 |
| 91 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 45 717.06 | 46 479.31 | 49 623.65 |
| 92 | javascript (14.15)| [foxify](https://foxify.js.org) (0.1) | 45 598.06 | 48 951.63 | 47 833.18 |
| 93 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 45 570.96 | 48 906.33 | 51 259.95 |
| 94 | javascript (14.15)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 45 104.88 | 48 709.84 | 47 741.78 |
| 95 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 532.90 | 45 486.95 | 45 396.99 |
| 96 | python (3.9)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 44 122.35 | 51 299.38 | 52 546.74 |
| 97 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 42 888.81 | 33 576.50 | 32 358.34 |
| 98 | python (3.9)| [bottle](https://bottlepy.org) (0.12) | 42 295.98 | 46 239.04 | 46 892.43 |
| 99 | javascript (14.15)| [koa](https://koajs.com) (2.13) | 41 925.64 | 44 033.35 | 42 872.41 |
| 100 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 36 632.03 | 37 521.26 | 38 172.66 |
| 101 | javascript (14.15)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 36 618.34 | 39 784.04 | 39 109.96 |
| 102 | java (11)| [restheart](https://restheart.org) (5.3) | 36 069.52 | 36 207.19 | 36 357.94 |
| 103 | javascript (14.15)| [moleculer](https://moleculer.services) (0.14) | 35 330.89 | 36 559.06 | 35 196.89 |
| 104 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 35 305.49 | 37 609.68 | 37 460.68 |
| 105 | python (3.9)| [emmett](https://emmett.sh) (2.1) | 35 220.79 | 41 528.79 | 42 270.18 |
| 106 | swift (5.3)| [vapor](https://vapor.codes) (4.40) | 35 107.05 | 36 691.50 | 36 510.55 |
| 107 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 35 070.16 | 35 180.27 | 35 351.26 |
| 108 | java (11)| [jersey3-grizzly2](https://eclipse-ee4j.github.io/jersey) (3) | 34 788.66 | 40 061.03 | 39 723.99 |
| 109 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 34 530.79 | 34 996.79 | 35 431.04 |
| 110 | javascript (14.15)| [hapi](https://hapijs.com) (20.1) | 34 488.78 | 35 376.01 | 34 728.39 |
| 111 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 33 887.86 | 34 768.86 | 33 251.62 |
| 112 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 33 692.49 | 44 098.81 | 47 412.29 |
| 113 | java (11)| [jersey-grizzly2](https://eclipse-ee4j.github.io/jersey) (2.33) | 33 251.85 | 37 773.88 | 37 080.79 |
| 114 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 33 183.39 | 37 961.57 | 37 934.55 |
| 115 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 32 398.69 | 29 258.31 | 25 922.30 |
| 116 | python (3.9)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 32 185.54 | 38 613.16 | 39 436.20 |
| 117 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 31 626.07 | 31 369.36 | 31 172.46 |
| 118 | rust (1.50)| [nickel](https://nickel-org.github.io) (0.11) | 31 560.85 | 30 476.79 | 31 155.46 |
| 119 | python (3.9)| [sanic](https://github.com/huge-success/sanic) (20.12) | 31 014.14 | 34 318.11 | 35 124.86 |
| 120 | python (3.9)| [hug](https://hug.rest) (2.6) | 30 634.98 | 33 400.86 | 49 956.43 |
| 121 | javascript (14.15)| [restify](https://restify.com) (8.5) | 29 528.27 | 30 795.56 | 29 695.90 |
| 122 | python (3.9)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.3) | 29 497.67 | 33 866.86 | 34 790.40 |
| 123 | php (7.4)| [imi](https://imiphp.com) (1.2) | 29 330.63 | 32 350.28 | 33 787.86 |
| 124 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 28 441.25 | 31 209.51 | 30 966.47 |
| 125 | scala (2.13)| [play](https://playframework.com) (2.8) | 28 144.71 | 29 799.89 | 29 878.37 |
| 126 | javascript (14.15)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 27 722.10 | 44 363.80 | 45 054.78 |
| 127 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 27 578.90 | 30 741.78 | 32 059.84 |
| 128 | python (3.9)| [starlette](https://starlette.io) (0.14) | 27 463.43 | 31 584.09 | 32 577.36 |
| 129 | php (7.4)| [comet](https://github.com/gotzmann/comet) (1.2) | 27 412.74 | 27 481.72 | 26 793.75 |
| 130 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 27 133.79 | 29 128.93 | 28 863.09 |
| 131 | python (3.9)| [index.py](https://index-py.abersheeran.com) (0.16) | 26 435.13 | 29 737.53 | 30 385.91 |
| 132 | rust (1.50)| [gotham](https://gotham.rs) (0.5) | 26 131.24 | 29 711.44 | 31 038.90 |
| 133 | python (3.9)| [responder](https://python-responder.org) (2.0) | 25 374.72 | 31 554.98 | 31 696.11 |
| 134 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 24 683.53 | 25 030.49 | 23 238.51 |
| 135 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 24 103.54 | 26 354.78 | 25 698.95 |
| 136 | python (3.9)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 625.50 | 27 605.24 | 27 508.15 |
| 137 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 22 676.85 | 21 126.54 | 20 269.27 |
| 138 | clojure (1.1)| [luminus](https://luminusweb.com) (3.96) | 22 089.58 | 21 246.81 | 20 673.49 |
| 139 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 21 980.84 | 21 682.09 | 21 629.48 |
| 140 | javascript (14.15)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 21 829.86 | 23 558.91 | 23 254.34 |
| 141 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.16) | 21 743.02 | 20 624.13 | 18 522.69 |
| 142 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 20 927.23 | 20 538.26 | 20 244.33 |
| 143 | python (3.9)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 20 155.93 | 23 476.58 | 24 089.98 |
| 144 | javascript (14.15)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 19 147.24 | 17 682.60 | 16 918.82 |
| 145 | python (3.9)| [molten](https://moltenframework.com) (1.0) | 19 135.94 | 22 461.23 | 22 070.08 |
| 146 | python (3.9)| [fastapi](https://fastapi.tiangolo.com) (0.63) | 18 345.04 | 21 573.15 | 22 034.75 |
| 147 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 653.29 | 15 626.06 | 14 671.92 |
| 148 | php (7.4)| [swoft](https://swoft.org) (2.0) | 16 925.53 | 20 437.57 | 20 881.86 |
| 149 | ruby (3.0)| [hanami-api](https://hanamirb.org) (0.1) | 16 713.42 | 16 227.05 | 15 950.25 |
| 150 | rust (1.50)| [iron](https://ironframework.io) (0.6) | 16 319.57 | 16 760.16 | 16 641.30 |
| 151 | ruby (3.0)| [syro](https://github.com/soveran/syro) (3.2) | 16 150.17 | 15 666.83 | 15 420.68 |
| 152 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 16 021.26 | 14 098.69 | 13 140.25 |
| 153 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 15 813.92 | 18 376.06 | 18 823.08 |
| 154 | javascript (14.15)| [express](https://expressjs.com) (4.17) | 15 651.74 | 17 531.59 | 17 552.71 |
| 155 | ruby (3.0)| [roda](https://roda.jeremyevans.net) (3.40) | 15 641.86 | 15 136.37 | 14 896.59 |
| 156 | go (1.15)| [macaron](https://go-macaron.com) (1.4) | 15 344.47 | 16 935.43 | 16 900.17 |
| 157 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 254.52 | 17 525.11 | 17 871.74 |
| 158 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 15 230.83 | 17 534.42 | 17 514.91 |
| 159 | javascript (14.15)| [feathersjs](https://feathersjs.com) (4.5) | 14 514.49 | 17 793.39 | 17 232.49 |
| 160 | ruby (3.0)| [cuba](https://cuba.is) (3.9) | 14 004.80 | 13 608.42 | 13 422.96 |
| 161 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 14 001.06 | 15 009.98 | 22 727.56 |
| 162 | python (3.9)| [flask](https://flask.pocoo.org) (1.1) | 13 946.52 | 15 659.24 | 17 535.44 |
| 163 | java (11)| [struts2](https://struts.apache.org) (2.5) | 13 852.11 | 14 114.82 | 14 170.55 |
| 164 | ruby (3.0)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 13 744.40 | 13 411.05 | 13 158.88 |
| 165 | java (11)| [blade](https://lets-blade.com) (2.0) | 13 064.51 | 16 546.59 | 14 731.85 |
| 166 | ruby (3.0)| [camping](https://github.com/camping/camping) (2.1) | 13 020.83 | 12 477.14 | 12 360.34 |
| 167 | ruby (3.0)| [rack_app](https://rack-app.com) (7.7) | 12 618.00 | 12 030.99 | 11 717.24 |
| 168 | javascript (14.15)| [nestjs-express](https://nestjs.com) (7.6) | 12 535.89 | 12 047.74 | 11 350.48 |
| 169 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 11 391.52 | 11 183.64 | 10 856.94 |
| 170 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 387.51 | 11 648.93 | 11 682.49 |
| 171 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 10 123.36 | 10 334.21 | 10 376.98 |
| 172 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.6) | 9 894.11 | 10 075.42 | 10 200.25 |
| 173 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 882.09 | 10 074.28 | 10 025.48 |
| 174 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 9 833.59 | 9 956.73 | 10 132.77 |
| 175 | python (3.9)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 734.39 | 9 794.25 | 9 647.40 |
| 176 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.7) | 9 669.92 | 9 705.88 | 9 860.14 |
| 177 | php (7.4)| [laravel-s-lumen](https://github.com/hhxsv5/laravel-s) (3.7) | 9 356.55 | 9 411.18 | 9 928.16 |
| 178 | python (3.9)| [guillotina](https://guillotina.io) (6.1) | 9 134.40 | 9 278.07 | 8 762.42 |
| 179 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 8 762.22 | 13 409.30 | 12 600.07 |
| 180 | ruby (3.0)| [sinatra](https://sinatrarb.com) (2.1) | 8 746.82 | 8 515.88 | 8 501.82 |
| 181 | ruby (3.0)| [grape](https://ruby-grape.org) (1.5) | 7 833.41 | 7 598.22 | 7 569.55 |
| 182 | python (3.9)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 458.45 | 6 991.37 | 6 458.88 |
| 183 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 7 149.94 | 7 542.49 | 7 448.73 |
| 184 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 7 082.55 | 7 489.28 | 7 443.82 |
| 185 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 6 948.96 | 7 338.00 | 7 236.01 |
| 186 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 838.68 | 7 194.01 | 7 108.05 |
| 187 | python (3.9)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 6 816.17 | 6 623.49 | 6 207.98 |
| 188 | php (7.4)| [laravel-s-laravel](https://github.com/hhxsv5/laravel-s) (3.7) | 6 442.17 | 6 416.67 | 6 399.97 |
| 189 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 356.21 | 6 725.26 | 6 731.73 |
| 190 | python (3.9)| [tornado](https://tornadoweb.org) (6.1) | 5 879.46 | 5 729.83 | 5 660.77 |
| 191 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 5 765.54 | 6 014.82 | 5 932.65 |
| 192 | python (3.9)| [django](https://djangoproject.com) (3.1) | 5 518.69 | 5 391.43 | 5 296.88 |
| 193 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.2) | 5 087.01 | 5 286.00 | 5 257.08 |
| 194 | php (7.4)| [fatfree](https://fatfreeframework.com) (3.7) | 5 053.31 | 5 216.02 | 5 120.10 |
| 195 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 721.78 | 4 927.63 | 4 834.73 |
| 196 | php (7.4)| [cubex](https://cubex.io) (4.16) | 4 479.04 | 4 597.18 | 4 593.00 |
| 197 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 335.53 | 4 456.01 | 4 461.07 |
| 198 | javascript (14.15)| [sails](https://sailsjs.com) (1.4) | 4 233.90 | 4 219.64 | 4 186.98 |
| 199 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.3) | 4 191.84 | 4 284.99 | 4 268.12 |
| 200 | julia (1.6)| [merly](https://github.com/codeneomatrix/Merly.jl) (1.0) | 3 964.76 | 6 211.35 | 3 940.54 |
| 201 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.6) | 3 854.20 | 3 940.23 | 3 963.87 |
| 202 | php (7.4)| [lumen](https://lumen.laravel.com) (8.2) | 3 833.50 | 3 912.23 | 3 898.04 |
| 203 | php (7.4)| [nette](https://nette.org/en/) (3.1) | 3 827.03 | 3 954.46 | 3 971.17 |
| 204 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 3 472.10 | 3 503.11 | 3 532.72 |
| 205 | php (7.4)| [symfony](https://symfony.com) (5.2) | 3 011.15 | 3 047.88 | 3 039.44 |
| 206 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.6) | 2 961.57 | 3 024.32 | 3 025.42 |
| 207 | ruby (3.0)| [rails](https://rubyonrails.org) (6.1) | 2 767.91 | 2 764.08 | 2 765.71 |
| 208 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 543.49 | 2 583.79 | 2 579.92 |
| 209 | python (3.9)| [masonite](https://masoniteproject.com) (3.0) | 2 464.96 | 2 480.94 | 2 474.40 |
| 210 | php (7.4)| [driftphp](https://github.com/driftphp/driftphp) (0.1) | 2 003.75 | 660.09 | 1 382.79 |
| 211 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 917.34 | 1 853.31 | 1 869.32 |
| 212 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 911.31 | 1 842.64 | 1 750.22 |
| 213 | php (7.4)| [antidot](https://github.com/antidot-framework/react-framework) (0.2) | 1 869.34 | 671.16 | 1 995.39 |
| 214 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 1 839.53 | 1 867.67 | 1 843.67 |
| 215 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.3) | 1 833.63 | 1 842.54 | 1 841.11 |
| 216 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 708.65 | 1 731.95 | 1 733.92 |
| 217 | python (3.9)| [cyclone](https://cyclone.io) (1.3) | 1 596.92 | 1 591.19 | 1 579.76 |
| 218 | python (3.9)| [klein](https://github.com/twisted/klein) (20.6) | 1 562.67 | 1 580.48 | 1 553.40 |
| 219 | python (3.9)| [django-ninja](https://django-ninja.rest-framework.com) (0.10) | 1 372.91 | 1 627.83 | 1 687.91 |
| 220 | python (3.9)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 230.96 | 1 163.81 | 1 149.61 |
| 221 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.1) | 1 173.49 | 1 191.94 | 1 204.33 |
| 222 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 984.74 | 508.02 | 663.40 |
| 223 | php (7.4)| [laravel](https://laravel.com) (8.28) | 982.32 | 984.47 | 982.39 |
| 224 | php (7.4)| [unic](https://unicframework.github.io/docs) (1.0) | 544.42 | 480.32 | 155.48 |
| 225 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 283.83 | 304.14 | -95.37 |
| 226 | cpp (11)| [nawa](https://github.com/jatofg/nawa) (0.6) | 262.99 | NaN | NaN |
</a>

</details>

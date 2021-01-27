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

A framework is a set of components working together. The main intention behind a framework is to faciliate (app or service) creation. The way a framework help any developer could vary from one to an other.

A majority of frameworks could be splitted in 2 parts :

+ **full-stack** meaning it provides all aspects (-stacks-) from data layer to sometimes deployment
+ **micro** meaning it provides only the routing part, and let the developer choose any other component for the others

## Requirements

+ `ruby`, all tools are made in `ruby`
+ `wrk`, results are collected using `wrk`
+ `postgresql`, results are stored in `postgresql`
+ `docker`, each implementation is implemented in an isolated **container**
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

## Results (2021-01-27)



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
<a id="results"> Computed with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | java (11)| [activej](https://activej.io) (3.0) | 172 394.80 | 213 774.51 | 219 271.59 |
| 2 | php (7.4)| [webman](https://github.com/walkor/webman) (1.0) | 157 082.38 | 168 073.66 | 170 445.91 |
| 3 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 149 040.49 | 180 938.88 | 183 413.81 |
| 4 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 122 682.92 | 135 291.90 | 136 352.10 |
| 5 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 120 720.52 | 123 663.63 | 123 234.33 |
| 6 | go (1.15)| [fiber](https://gofiber.io) (2.3) | 120 235.53 | 129 963.58 | 129 192.20 |
| 7 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 117 370.12 | 146 058.44 | 148 892.00 |
| 8 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.6) | 116 839.51 | 128 623.77 | 127 871.36 |
| 9 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 116 122.56 | 128 043.58 | 127 608.03 |
| 10 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.19) | 116 110.99 | 130 126.41 | 130 320.40 |
| 11 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 116 024.15 | 128 461.86 | 127 824.08 |
| 12 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 114 525.13 | 141 543.00 | 144 708.24 |
| 13 | java (11)| [undertow](https://undertow.io) (2.2) | 113 077.55 | 137 801.17 | 139 780.72 |
| 14 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 112 600.03 | 112 015.28 | 115 136.80 |
| 15 | clojure (1.1)| [donkey](https://github.com/AppsFlyer/donkey) (0.4) | 111 269.40 | 113 661.47 | 115 202.56 |
| 16 | javascript (14.15)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.4) | 110 373.50 | 141 798.78 | 145 491.38 |
| 17 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 109 950.40 | 136 691.44 | 141 841.53 |
| 18 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 109 705.98 | 134 941.34 | 138 429.13 |
| 19 | java (11)| [jooby](https://jooby.io) (2.9) | 109 694.22 | 137 159.00 | 142 303.11 |
| 20 | javascript (14.15)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 108 915.27 | 137 693.56 | 140 564.67 |
| 21 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 107 882.98 | 104 520.16 | 109 033.22 |
| 22 | c (11)| [kore](https://kore.io) (3.3) | 107 316.81 | 186 537.59 | 187 148.73 |
| 23 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 107 188.98 | 133 353.61 | 137 242.90 |
| 24 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 106 302.24 | 131 795.97 | 135 018.70 |
| 25 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 99 673.62 | 120 836.34 | 121 362.80 |
| 26 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 98 051.02 | 120 732.08 | 120 663.68 |
| 27 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.4) | 97 515.60 | 119 460.20 | 119 870.17 |
| 28 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 95 168.77 | 116 665.76 | 119 494.53 |
| 29 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 94 917.12 | 138 792.15 | 149 159.56 |
| 30 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 93 355.55 | 113 176.74 | 112 883.61 |
| 31 | crystal (0.35)| [kemal](https://kemalcr.com) (0.27) | 92 472.38 | 111 869.50 | 111 382.76 |
| 32 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 89 004.19 | 105 895.82 | 105 380.70 |
| 33 | php (7.4)| [swoole-process-coroutine](https://github.com/swoole/swoole-src) (4.6) | 88 827.54 | 126 018.57 | 137 100.44 |
| 34 | java (11)| [quarkus](https://quarkus.io) (1.11) | 86 897.92 | 105 604.16 | 108 452.17 |
| 35 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 84 790.18 | 107 102.87 | 113 462.53 |
| 36 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.12) | 83 167.28 | 93 706.89 | 89 079.25 |
| 37 | scala (2.13)| [akkahttp](https://akka.io) (10.2) | 82 236.16 | 94 492.64 | 92 946.60 |
| 38 | javascript (14.15)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 81 862.86 | 97 805.46 | 99 196.81 |
| 39 | go (1.15)| [gf](https://goframe.org) (1.15) | 81 307.83 | 88 874.08 | 91 427.15 |
| 40 | go (1.15)| [clevergo](https://clevergo.tech) (0.5) | 81 042.52 | 82 335.86 | 84 200.85 |
| 41 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 80 834.19 | 81 841.27 | 83 998.10 |
| 42 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 79 961.35 | 81 131.55 | 82 992.19 |
| 43 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 79 820.56 | 80 665.96 | 82 810.47 |
| 44 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 039.15 | 82 583.60 | 83 774.81 |
| 45 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 78 005.51 | 81 917.72 | 83 441.98 |
| 46 | go (1.15)| [chi](https://github.com/go-chi/chi) (1.5) | 77 370.71 | 76 748.65 | 79 435.78 |
| 47 | go (1.15)| [violetear](https://violetear.org) (7.0) | 76 970.74 | 77 287.74 | 79 316.51 |
| 48 | scala (2.13)| [finatra](https://twitter.github.io/finatra/) (21.1) | 76 207.28 | 87 869.26 | 88 596.85 |
| 49 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 75 288.91 | 88 291.33 | 90 990.65 |
| 50 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 75 244.98 | 79 203.92 | 79 741.71 |
| 51 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 74 952.79 | 74 437.31 | 76 514.11 |
| 52 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 73 289.47 | 71 688.46 | 74 456.40 |
| 53 | python (3.9)| [falcon](https://falconframework.org) (2.0) | 73 061.79 | 80 611.19 | 82 022.82 |
| 54 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 72 684.62 | 84 009.36 | 86 571.25 |
| 55 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.2) | 72 459.85 | 68 488.02 | 65 168.61 |
| 56 | fsharp (5.0)| [falco](https://falcoframework.com) (3.0) | 72 071.69 | 83 355.12 | 85 732.63 |
| 57 | go (1.15)| [beego](https://beego.me) (1.12) | 71 551.27 | 74 506.64 | 76 415.88 |
| 58 | javascript (14.15)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 70 068.86 | 78 339.95 | 79 288.19 |
| 59 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 65 028.93 | 72 797.02 | 73 056.46 |
| 60 | go (1.15)| [air](https://github.com/aofei/air) (0.21) | 64 802.75 | 63 537.30 | 65 972.23 |
| 61 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 64 480.67 | 130 606.88 | 131 105.97 |
| 62 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.6) | 62 546.06 | 62 509.12 | 65 009.14 |
| 63 | scala (2.13)| [finch](https://finagle.github.io/finch/) (0.32) | 61 032.14 | 67 906.61 | 67 916.01 |
| 64 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 60 741.65 | 66 246.74 | 65 581.16 |
| 65 | javascript (14.15)| [0http](https://github.com/jkyberneees/0http) (3.0) | 60 485.31 | 66 077.49 | 67 352.62 |
| 66 | javascript (14.15)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 60 463.03 | 68 110.62 | 69 343.53 |
| 67 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 60 442.29 | 63 778.51 | 64 637.04 |
| 68 | javascript (14.15)| [polka](https://github.com/lukeed/polka) (0.5) | 57 364.10 | 61 878.15 | 60 915.53 |
| 69 | javascript (14.15)| [restana](https://github.com/jkyberneees/ana) (4.7) | 56 285.77 | 63 629.16 | 64 685.16 |
| 70 | java (11)| [restheart](https://restheart.org) (5.1) | 56 186.11 | 57 433.90 | 58 186.65 |
| 71 | javascript (14.15)| [rayo](https://rayo.js.org) (1.3) | 55 892.66 | 60 803.79 | 59 631.89 |
| 72 | javascript (14.15)| [fastify](https://fastify.io) (3.11) | 55 050.58 | 59 870.80 | 58 507.46 |
| 73 | php (7.4)| [one](https://github.com/lizhichao/one) (2.2) | 54 319.88 | 61 608.92 | 69 984.49 |
| 74 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 53 723.86 | 56 561.25 | 54 950.46 |
| 75 | python (3.9)| [pyramid](https://trypyramid.com) (1.1) | 53 649.22 | 58 218.21 | 58 681.87 |
| 76 | javascript (14.15)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 53 496.99 | 59 024.72 | 57 195.14 |
| 77 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 52 720.85 | 60 967.91 | 63 449.53 |
| 78 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 51 750.74 | 57 015.25 | 64 010.59 |
| 79 | php (7.4)| [hyperf](https://hyperf.io) (2.1) | 51 045.34 | 61 724.20 | 65 077.00 |
| 80 | java (11)| [javalin](https://javalin.io) (3.9) | 51 010.31 | 54 656.95 | 54 819.93 |
| 81 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.14) | 50 993.19 | 74 337.64 | 82 949.52 |
| 82 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 50 883.40 | 66 456.90 | 69 208.33 |
| 83 | java (11)| [micronaut](https://micronaut.io) (1.2) | 50 259.10 | 57 593.54 | 57 561.58 |
| 84 | java (11)| [spark](https://sparkjava.com) (2.9) | 49 888.25 | 54 503.01 | 55 693.98 |
| 85 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 49 861.35 | 56 231.44 | 56 484.89 |
| 86 | javascript (14.15)| [nestjs-fastify](https://nestjs.com) (7.6) | 48 550.94 | 51 914.97 | 50 777.31 |
| 87 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 46 033.34 | 46 483.25 | 49 543.25 |
| 88 | rust (1.49)| [actix](https://actix.rs) (3.3) | 45 997.06 | 48 586.03 | 50 357.56 |
| 89 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 45 545.56 | 48 773.98 | 50 821.28 |
| 90 | javascript (14.15)| [foxify](https://foxify.js.org) (0.1) | 45 319.32 | 49 052.73 | 47 794.71 |
| 91 | javascript (14.15)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 44 953.41 | 48 506.45 | 48 220.18 |
| 92 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 790.49 | 45 880.16 | 45 994.70 |
| 93 | php (7.4)| [comet](https://github.com/gotzmann/comet) (1.1) | 44 480.30 | 48 318.96 | 49 210.49 |
| 94 | python (3.9)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 44 307.32 | 50 634.78 | 52 265.51 |
| 95 | rust (1.49)| [salvo](https://github.com/kenorld/salvo) (0.5) | 44 094.34 | 47 141.37 | 47 940.40 |
| 96 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 43 023.57 | 32 617.96 | 30 949.12 |
| 97 | python (3.9)| [bottle](https://bottlepy.org) (0.12) | 42 434.66 | 45 348.01 | 46 208.33 |
| 98 | javascript (14.15)| [koa](https://koajs.com) (2.13) | 40 740.58 | 43 880.23 | 42 680.88 |
| 99 | javascript (14.15)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 37 089.76 | 39 711.52 | 38 869.10 |
| 100 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 36 374.10 | 37 639.40 | 37 728.66 |
| 101 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 36 361.14 | 35 916.10 | 35 503.68 |
| 102 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 35 784.07 | 35 604.17 | 35 411.10 |
| 103 | python (3.9)| [emmett](https://emmett.sh) (2.1) | 35 501.16 | 41 455.86 | 42 322.21 |
| 104 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 35 228.17 | 37 231.38 | 37 089.14 |
| 105 | javascript (14.15)| [moleculer](https://moleculer.services) (0.14) | 35 020.16 | 36 034.99 | 34 845.04 |
| 106 | javascript (14.15)| [hapi](https://hapijs.com) (20.1) | 34 720.09 | 35 399.65 | 34 291.74 |
| 107 | swift (5.3)| [vapor](https://vapor.codes) (4.39) | 34 644.00 | 36 854.85 | 36 512.11 |
| 108 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 34 551.16 | 49 411.99 | 52 044.82 |
| 109 | rust (1.49)| [nickel](https://nickel-org.github.io) (0.11) | 34 295.36 | 34 288.89 | 33 981.04 |
| 110 | java (11)| [jersey3-grizzly2](https://eclipse-ee4j.github.io/jersey) (3) | 33 912.84 | 39 059.86 | 38 373.08 |
| 111 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 33 885.45 | 34 797.56 | 33 330.33 |
| 112 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 33 533.38 | 37 973.77 | 38 485.53 |
| 113 | java (11)| [jersey-grizzly2](https://eclipse-ee4j.github.io/jersey) (2.33) | 33 283.68 | 38 211.83 | 37 878.50 |
| 114 | python (3.9)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 33 079.31 | 38 426.14 | 39 141.90 |
| 115 | python (3.9)| [hug](https://hug.rest) (2.6) | 32 630.20 | 35 642.97 | 35 671.58 |
| 116 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 32 376.58 | 28 114.13 | 24 941.85 |
| 117 | rust (1.49)| [gotham](https://gotham.rs) (0.5) | 31 880.09 | 35 369.98 | 36 481.79 |
| 118 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 31 629.79 | 31 280.95 | 31 348.47 |
| 119 | python (3.9)| [sanic](https://github.com/huge-success/sanic) (20.12) | 31 097.59 | 34 180.68 | 34 737.12 |
| 120 | python (3.9)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.3) | 30 621.44 | 34 193.37 | 34 307.50 |
| 121 | php (7.4)| [swoole-coroutine](https://github.com/swoole/swoole-src) (4.6) | 30 463.78 | 30 936.77 | 29 139.23 |
| 122 | javascript (14.15)| [restify](https://restify.com) (8.5) | 29 616.56 | 30 653.36 | 29 659.92 |
| 123 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 28 914.64 | 31 910.71 | 31 883.28 |
| 124 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 28 449.96 | 31 430.88 | 33 308.65 |
| 125 | javascript (14.15)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 28 069.43 | 29 858.31 | 36 576.43 |
| 126 | python (3.9)| [starlette](https://starlette.io) (0.14) | 28 047.81 | 32 338.76 | 32 643.54 |
| 127 | scala (2.13)| [play](https://playframework.com) (2.8) | 27 532.17 | 29 590.84 | 29 351.84 |
| 128 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 26 940.01 | 29 182.05 | 28 576.79 |
| 129 | python (3.9)| [index.py](https://index-py.abersheeran.com) (0.16) | 26 393.95 | 29 589.90 | 30 305.47 |
| 130 | python (3.9)| [responder](https://python-responder.org) (2.0) | 24 947.94 | 30 782.88 | 31 442.32 |
| 131 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 24 346.19 | 24 212.30 | 21 558.85 |
| 132 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 24 212.85 | 26 182.21 | 25 650.26 |
| 133 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 22 755.52 | 21 731.51 | 20 562.14 |
| 134 | clojure (1.1)| [luminus](https://luminusweb.com) (3.93) | 22 334.78 | 21 725.14 | 20 237.35 |
| 135 | python (3.9)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 22 189.47 | 28 527.64 | 28 337.38 |
| 136 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 21 996.40 | 21 834.12 | 21 639.15 |
| 137 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.16) | 21 864.21 | 21 254.77 | 19 413.98 |
| 138 | javascript (14.15)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 21 181.17 | 22 864.11 | 22 390.58 |
| 139 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 20 998.57 | 20 391.71 | 20 598.65 |
| 140 | python (3.9)| [molten](https://moltenframework.com) (1.0) | 19 498.00 | 22 123.98 | 21 899.97 |
| 141 | python (3.9)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 19 421.33 | 23 624.96 | 24 043.78 |
| 142 | javascript (14.15)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 18 835.86 | 17 703.48 | 16 897.05 |
| 143 | python (3.9)| [fastapi](https://fastapi.tiangolo.com) (0.63) | 17 729.22 | 21 932.01 | 21 955.31 |
| 144 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 484.42 | 15 702.15 | 14 659.99 |
| 145 | php (7.4)| [swoft](https://swoft.org) (2.0) | 16 974.76 | 20 479.23 | 21 257.97 |
| 146 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 16 967.14 | 16 479.55 | 16 071.07 |
| 147 | rust (1.49)| [iron](https://ironframework.io) (0.6) | 16 797.90 | 16 675.44 | 16 897.10 |
| 148 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 16 509.54 | 16 000.41 | 15 803.50 |
| 149 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 16 174.34 | 14 317.32 | 13 250.65 |
| 150 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.40) | 15 870.30 | 15 572.23 | 15 247.29 |
| 151 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 15 644.07 | 18 406.70 | 18 904.72 |
| 152 | go (1.15)| [macaron](https://go-macaron.com) (1.4) | 15 398.82 | 16 902.57 | 16 936.06 |
| 153 | javascript (14.15)| [express](https://expressjs.com) (4.17) | 15 259.08 | 17 131.22 | 17 561.22 |
| 154 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 15 246.73 | 14 685.77 | 14 517.50 |
| 155 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 15 178.58 | 17 367.96 | 17 706.35 |
| 156 | javascript (14.15)| [feathersjs](https://feathersjs.com) (4.5) | 15 170.86 | 16 867.59 | 17 669.23 |
| 157 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 109.84 | 17 567.72 | 17 513.71 |
| 158 | python (3.9)| [flask](https://flask.pocoo.org) (1.1) | 14 322.12 | 16 375.48 | 17 151.79 |
| 159 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 13 903.44 | 13 518.25 | 13 255.68 |
| 160 | java (11)| [struts2](https://struts.apache.org) (2.5) | 13 902.13 | 14 177.11 | 14 197.72 |
| 161 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 13 795.00 | 13 756.54 | 16 858.13 |
| 162 | ruby (2.7)| [rack_app](https://rack-app.com) (7.7) | 13 634.37 | 13 177.94 | 12 982.29 |
| 163 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 13 157.12 | 12 734.07 | 12 586.07 |
| 164 | java (11)| [blade](https://lets-blade.com) (2.0) | 12 939.53 | 15 537.96 | 14 536.24 |
| 165 | javascript (14.15)| [nestjs-express](https://nestjs.com) (7.6) | 11 769.80 | 13 231.81 | 12 221.70 |
| 166 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 379.67 | 11 654.62 | 11 684.23 |
| 167 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 11 110.82 | 10 880.99 | 10 533.50 |
| 168 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.5) | 10 187.47 | 10 423.00 | 10 525.90 |
| 169 | python (3.9)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 10 134.23 | 9 653.58 | 9 511.10 |
| 170 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 10 021.14 | 10 242.98 | 10 372.02 |
| 171 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 840.49 | 10 027.34 | 10 014.52 |
| 172 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 9 757.72 | 9 964.87 | 10 073.46 |
| 173 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.7) | 9 616.34 | 9 703.90 | 9 801.91 |
| 174 | python (3.9)| [guillotina](https://guillotina.io) (6.1) | 9 184.10 | 9 335.68 | 8 833.33 |
| 175 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.1) | 8 962.05 | 8 729.06 | 8 714.94 |
| 176 | ruby (2.7)| [grape](https://ruby-grape.org) (1.5) | 8 671.78 | 8 391.22 | 8 359.83 |
| 177 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 7 941.08 | 13 213.13 | 12 513.56 |
| 178 | python (3.9)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 408.60 | 7 071.57 | 6 519.12 |
| 179 | python (3.9)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 7 406.52 | 7 176.11 | 6 870.50 |
| 180 | python (3.9)| [masonite](https://masoniteproject.com) (3.0) | 7 006.34 | 11 394.80 | 11 359.53 |
| 181 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 6 748.08 | 6 684.68 | 6 626.74 |
| 182 | python (3.9)| [tornado](https://tornadoweb.org) (6.1) | 6 672.67 | 6 644.60 | 6 406.59 |
| 183 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 566.50 | 6 500.31 | 6 453.64 |
| 184 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 6 504.89 | 6 411.72 | 6 319.31 |
| 185 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 6 329.31 | 6 275.03 | 6 155.03 |
| 186 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 199.07 | 6 116.77 | 6 098.98 |
| 187 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 6 045.62 | 6 747.95 | 6 664.06 |
| 188 | python (3.9)| [django](https://djangoproject.com) (3.1) | 5 973.82 | 5 814.99 | 5 734.18 |
| 189 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 5 659.06 | 5 565.07 | 5 555.72 |
| 190 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 5 230.92 | 5 196.78 | 5 136.02 |
| 191 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 5 184.13 | 5 104.93 | 5 150.53 |
| 192 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.2) | 4 674.27 | 4 632.54 | 4 583.51 |
| 193 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 453.82 | 4 406.61 | 4 365.48 |
| 194 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 441.18 | 4 413.41 | 4 419.69 |
| 195 | javascript (14.15)| [sails](https://sailsjs.com) (1.4) | 4 278.53 | 4 259.47 | 4 223.43 |
| 196 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 4 195.93 | 2 352.50 | 2 723.02 |
| 197 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.3) | 3 953.22 | 3 894.27 | 3 890.64 |
| 198 | php (7.4)| [nette](https://nette.org/en/) (3.1) | 3 877.06 | 3 861.62 | 3 884.12 |
| 199 | php (7.4)| [lumen](https://lumen.laravel.com) (8.2) | 3 833.51 | 3 840.28 | 3 880.98 |
| 200 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.5) | 3 785.58 | 3 732.06 | 3 729.28 |
| 201 | julia (1.6)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 722.78 | 6 875.45 | 5 823.67 |
| 202 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 3 424.56 | 3 390.75 | 3 396.33 |
| 203 | php (7.4)| [symfony](https://symfony.com) (5.2) | 2 971.32 | 2 981.89 | 2 999.83 |
| 204 | ruby (2.7)| [rails](https://rubyonrails.org) (6.1) | 2 786.44 | 2 770.54 | 2 771.03 |
| 205 | php (7.4)| [fatfree](https://fatfreeframework.com) (3.7) | 2 768.19 | 2 739.54 | 2 906.26 |
| 206 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 618.75 | 2 602.75 | 2 625.02 |
| 207 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.5) | 2 292.01 | 2 287.50 | 2 309.39 |
| 208 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 289.31 | 2 535.78 | 2 511.53 |
| 209 | php (7.4)| [antidot](https://antidotfw.io) (0.1) | 2 002.48 | 1 904.37 | 1 391.65 |
| 210 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 925.08 | 1 848.11 | 1 838.39 |
| 211 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 922.57 | 1 839.08 | 1 769.31 |
| 212 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 1 862.91 | 1 918.38 | 1 907.67 |
| 213 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.3) | 1 855.88 | 1 853.19 | 1 859.66 |
| 214 | php (7.4)| [driftphp](https://github.com/driftphp/driftphp) (0.1) | 1 825.75 | 1 787.27 | 1 393.16 |
| 215 | python (3.9)| [cyclone](https://cyclone.io) (1.3) | 1 659.70 | 1 657.21 | 1 631.34 |
| 216 | python (3.9)| [klein](https://github.com/twisted/klein) (20.6) | 1 610.12 | 1 612.30 | 1 580.43 |
| 217 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 582.21 | 1 592.51 | 1 598.58 |
| 218 | python (3.9)| [django-ninja](https://django-ninja.rest-framework.com) (0.10) | 1 346.57 | 1 624.42 | 1 662.12 |
| 219 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 257.57 | 636.88 | 400.15 |
| 220 | python (3.9)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 245.27 | 1 176.47 | 1 182.44 |
| 221 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 090.76 | 1 107.42 | 1 107.43 |
| 222 | php (7.4)| [laravel](https://laravel.com) (8.25) | 905.66 | 912.23 | 909.09 |
| 223 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 287.80 | 302.39 | -86.12 |
| 224 | cpp (11)| [nawa](https://github.com/jatofg/nawa) (0.6) | 260.63 | NaN | NaN |
</a>

</details>

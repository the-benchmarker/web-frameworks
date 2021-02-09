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

## Results (2021-02-09)



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
| 1 | java (11)| [activej](https://activej.io) (3.0) | 169 224.28 | 209 243.46 | 213 911.35 |
| 2 | php (7.4)| [webman](https://github.com/walkor/webman) (1.0) | 155 741.37 | 166 588.74 | 168 858.33 |
| 3 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 144 207.54 | 176 700.77 | 178 762.36 |
| 4 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 121 431.72 | 133 172.38 | 134 660.53 |
| 5 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 118 645.46 | 121 935.27 | 121 944.54 |
| 6 | go (1.15)| [fiber](https://gofiber.io) (2.5) | 117 943.39 | 128 742.68 | 127 972.76 |
| 7 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 116 001.58 | 142 858.47 | 145 835.99 |
| 8 | c (11)| [kore](https://kore.io) (3.3) | 115 458.95 | 187 527.34 | 192 149.51 |
| 9 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.6) | 114 890.58 | 126 343.02 | 125 766.51 |
| 10 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.2) | 114 234.80 | 127 479.09 | 127 484.93 |
| 11 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 113 864.19 | 125 419.83 | 124 453.84 |
| 12 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 113 783.31 | 126 939.13 | 126 419.74 |
| 13 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 112 100.24 | 138 770.03 | 141 768.22 |
| 14 | java (11)| [undertow](https://undertow.io) (2.2) | 111 498.73 | 135 608.50 | 137 045.34 |
| 15 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 111 073.60 | 110 533.63 | 113 553.45 |
| 16 | clojure (1.1)| [donkey](https://github.com/AppsFlyer/donkey) (0.4) | 111 000.13 | 113 426.14 | 113 975.55 |
| 17 | javascript (14.15)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.4) | 109 089.59 | 139 823.66 | 142 151.20 |
| 18 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 108 174.27 | 132 484.85 | 134 686.17 |
| 19 | java (11)| [jooby](https://jooby.io) (2.9) | 107 743.79 | 134 556.35 | 139 263.76 |
| 20 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 107 597.93 | 133 446.80 | 138 328.71 |
| 21 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 107 567.29 | 104 100.42 | 108 419.57 |
| 22 | javascript (14.15)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 107 122.64 | 135 841.30 | 138 090.95 |
| 23 | java (11)| [vertx](https://vertx.io/docs/vertx-core/java/) (4.0) | 105 445.86 | 125 110.33 | 126 149.86 |
| 24 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 105 105.50 | 130 597.89 | 135 245.19 |
| 25 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 104 835.10 | 128 966.93 | 132 268.30 |
| 26 | java (11)| [vertx4web](https://vertx.io/docs/vertx-web/java/) (4.0) | 103 233.40 | 122 064.30 | 123 005.99 |
| 27 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 98 290.25 | 119 839.67 | 120 053.24 |
| 28 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 98 277.95 | 117 968.89 | 118 857.80 |
| 29 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.4) | 97 035.20 | 117 242.65 | 117 449.52 |
| 30 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 94 784.60 | 105 960.19 | 105 507.55 |
| 31 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 94 627.00 | 125 792.88 | 130 050.23 |
| 32 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 93 793.91 | 137 818.99 | 147 574.70 |
| 33 | php (7.4)| [swoole](https://github.com/swoole/swoole-src) (4.6) | 93 763.86 | 118 391.54 | 133 952.05 |
| 34 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 93 456.43 | 114 324.26 | 117 194.67 |
| 35 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 93 319.32 | 110 908.86 | 110 019.54 |
| 36 | crystal (0.35)| [kemal](https://kemalcr.com) (0.27) | 91 221.11 | 110 087.64 | 110 574.55 |
| 37 | php (7.4)| [swoole-coroutine](https://github.com/swoole/swoole-src) (4.6) | 88 411.48 | 127 005.09 | 136 663.74 |
| 38 | java (11)| [quarkus](https://quarkus.io) (1.11) | 85 960.75 | 103 501.98 | 105 765.79 |
| 39 | go (1.15)| [gf](https://goframe.org) (1.15) | 82 070.32 | 89 004.19 | 91 527.83 |
| 40 | javascript (14.15)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 81 754.54 | 96 487.89 | 97 658.71 |
| 41 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.12) | 80 449.08 | 92 300.93 | 88 540.28 |
| 42 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 79 907.81 | 80 716.63 | 82 734.59 |
| 43 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 79 567.03 | 80 719.93 | 82 772.58 |
| 44 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 79 418.55 | 80 302.97 | 82 297.74 |
| 45 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 79 389.79 | 102 689.14 | 109 786.63 |
| 46 | go (1.15)| [clevergo](https://clevergo.tech) (0.5) | 79 252.25 | 80 724.22 | 82 602.82 |
| 47 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 391.13 | 82 560.19 | 83 688.09 |
| 48 | scala (2.13)| [akkahttp](https://akka.io) (10.2) | 78 333.49 | 92 079.66 | 89 788.13 |
| 49 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 77 672.11 | 81 521.96 | 82 916.74 |
| 50 | go (1.15)| [violetear](https://violetear.org) (7.0) | 76 281.18 | 76 187.90 | 78 230.75 |
| 51 | go (1.15)| [chi](https://github.com/go-chi/chi) (1.5) | 76 258.20 | 75 635.55 | 77 975.48 |
| 52 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 74 376.57 | 73 584.23 | 75 996.34 |
| 53 | fsharp (5.0)| [falco](https://falcoframework.com) (3.0) | 74 181.38 | 86 726.27 | 89 161.43 |
| 54 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 74 133.28 | 78 045.98 | 78 778.50 |
| 55 | python (3.9)| [falcon](https://falconframework.org) (2.0) | 73 565.70 | 80 826.63 | 82 553.94 |
| 56 | scala (2.13)| [finatra](https://twitter.github.io/finatra/) (21.1) | 73 272.72 | 84 664.28 | 85 080.46 |
| 57 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 72 260.56 | 83 470.05 | 85 817.13 |
| 58 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 71 494.07 | 82 423.23 | 84 754.06 |
| 59 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.3) | 71 376.76 | 66 386.83 | 64 652.48 |
| 60 | go (1.15)| [beego](https://beego.me) (1.12) | 71 119.02 | 74 113.39 | 75 928.82 |
| 61 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 70 547.55 | 67 145.32 | 73 715.32 |
| 62 | javascript (14.15)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 68 971.13 | 77 842.86 | 78 520.16 |
| 63 | go (1.15)| [air](https://github.com/aofei/air) (0.21) | 64 520.42 | 63 066.61 | 65 609.26 |
| 64 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 64 273.69 | 71 262.01 | 71 535.76 |
| 65 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.6) | 62 329.57 | 62 192.17 | 64 993.74 |
| 66 | javascript (14.15)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 61 189.98 | 67 863.77 | 69 228.89 |
| 67 | javascript (14.15)| [0http](https://github.com/jkyberneees/0http) (3.0) | 60 778.22 | 65 648.77 | 66 593.17 |
| 68 | scala (2.13)| [finch](https://finagle.github.io/finch/) (0.32) | 60 696.46 | 67 541.36 | 67 553.11 |
| 69 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 60 013.87 | 62 736.76 | 63 754.63 |
| 70 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 59 431.43 | 63 656.63 | 63 513.59 |
| 71 | javascript (14.15)| [polka](https://github.com/lukeed/polka) (0.5) | 57 037.88 | 62 096.57 | 60 792.06 |
| 72 | javascript (14.15)| [restana](https://github.com/jkyberneees/ana) (4.7) | 56 979.45 | 62 696.34 | 63 533.78 |
| 73 | javascript (14.15)| [rayo](https://rayo.js.org) (1.3) | 55 551.54 | 60 114.22 | 58 958.55 |
| 74 | python (3.9)| [pyramid](https://trypyramid.com) (1.1) | 54 956.39 | 58 313.71 | 58 834.13 |
| 75 | javascript (14.15)| [fastify](https://fastify.io) (3.11) | 54 225.84 | 59 336.97 | 58 306.57 |
| 76 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 53 824.31 | 56 929.58 | 55 413.54 |
| 77 | javascript (14.15)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 53 743.42 | 58 205.84 | 56 797.97 |
| 78 | rust (1.49)| [salvo](https://github.com/kenorld/salvo) (0.5) | 53 507.03 | 57 253.67 | 58 091.06 |
| 79 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 52 737.91 | 61 120.04 | 62 653.14 |
| 80 | php (7.4)| [one](https://github.com/lizhichao/one) (2.2) | 52 135.20 | 63 261.79 | 66 853.25 |
| 81 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 51 888.11 | 66 755.00 | 69 310.19 |
| 82 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 51 652.51 | 57 007.70 | 63 921.37 |
| 83 | java (11)| [javalin](https://javalin.io) (3.9) | 51 437.69 | 54 665.27 | 54 935.16 |
| 84 | php (7.4)| [hyperf](https://hyperf.io) (2.1) | 50 903.89 | 62 787.89 | 64 465.62 |
| 85 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.14) | 50 094.26 | 72 625.31 | 80 712.21 |
| 86 | java (11)| [micronaut](https://micronaut.io) (1.2) | 49 303.43 | 56 862.10 | 56 998.18 |
| 87 | java (11)| [spark](https://sparkjava.com) (2.9) | 49 261.42 | 53 940.25 | 54 758.47 |
| 88 | rust (1.49)| [actix](https://actix.rs) (3.3) | 48 475.92 | 49 573.89 | 49 404.94 |
| 89 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 48 206.22 | 53 848.72 | 54 330.15 |
| 90 | javascript (14.15)| [nestjs-fastify](https://nestjs.com) (7.6) | 48 045.99 | 51 598.12 | 50 584.73 |
| 91 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 45 842.58 | 46 146.39 | 49 178.99 |
| 92 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 45 509.90 | 48 420.60 | 50 549.36 |
| 93 | javascript (14.15)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 45 444.29 | 48 761.70 | 47 701.83 |
| 94 | javascript (14.15)| [foxify](https://foxify.js.org) (0.1) | 44 941.78 | 48 676.51 | 48 274.37 |
| 95 | python (3.9)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 44 933.95 | 51 138.51 | 52 617.92 |
| 96 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 157.47 | 45 109.80 | 45 180.50 |
| 97 | php (7.4)| [comet](https://github.com/gotzmann/comet) (1.1) | 43 906.30 | 47 692.10 | 47 473.69 |
| 98 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 42 702.46 | 32 627.11 | 31 681.52 |
| 99 | python (3.9)| [bottle](https://bottlepy.org) (0.12) | 41 723.41 | 46 049.37 | 46 877.89 |
| 100 | javascript (14.15)| [koa](https://koajs.com) (2.13) | 41 076.74 | 43 998.16 | 43 444.69 |
| 101 | python (3.9)| [emmett](https://emmett.sh) (2.1) | 37 330.58 | 40 447.47 | 41 533.68 |
| 102 | javascript (14.15)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 37 169.06 | 39 230.72 | 38 612.66 |
| 103 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 36 624.75 | 37 589.98 | 37 794.59 |
| 104 | swift (5.3)| [vapor](https://vapor.codes) (4.39) | 36 318.47 | 37 653.93 | 37 529.48 |
| 105 | java (11)| [restheart](https://restheart.org) (5.3) | 35 632.16 | 35 768.96 | 35 903.80 |
| 106 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 35 174.95 | 36 938.04 | 37 490.30 |
| 107 | javascript (14.15)| [moleculer](https://moleculer.services) (0.14) | 35 014.01 | 36 370.65 | 35 436.20 |
| 108 | rust (1.49)| [nickel](https://nickel-org.github.io) (0.11) | 34 664.30 | 33 196.11 | 34 881.22 |
| 109 | javascript (14.15)| [hapi](https://hapijs.com) (20.1) | 34 391.42 | 35 566.82 | 34 755.48 |
| 110 | java (11)| [jersey3-grizzly2](https://eclipse-ee4j.github.io/jersey) (3) | 33 955.51 | 38 999.63 | 38 279.09 |
| 111 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 33 927.32 | 35 543.38 | 36 036.80 |
| 112 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 33 640.44 | 34 832.16 | 33 692.47 |
| 113 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 33 561.06 | 44 142.09 | 47 507.46 |
| 114 | python (3.9)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 33 551.79 | 38 681.37 | 38 803.51 |
| 115 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 33 409.78 | 37 592.24 | 37 802.68 |
| 116 | python (3.9)| [hug](https://hug.rest) (2.6) | 33 239.64 | 35 467.75 | 53 110.19 |
| 117 | java (11)| [jersey-grizzly2](https://eclipse-ee4j.github.io/jersey) (2.33) | 33 060.03 | 37 689.58 | 37 180.15 |
| 118 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 32 669.96 | 33 132.06 | 33 540.25 |
| 119 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 32 035.75 | 28 326.65 | 25 172.29 |
| 120 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 31 599.31 | 31 178.77 | 31 186.68 |
| 121 | rust (1.49)| [gotham](https://gotham.rs) (0.5) | 31 377.48 | 34 925.44 | 36 239.62 |
| 122 | php (7.4)| [imi](https://imiphp.com) (1.2) | 29 912.32 | 33 273.41 | 34 558.55 |
| 123 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 28 917.38 | 31 938.03 | 31 770.15 |
| 124 | javascript (14.15)| [restify](https://restify.com) (8.5) | 28 800.80 | 30 922.53 | 29 745.19 |
| 125 | python (3.9)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.3) | 28 692.16 | 33 942.26 | 34 137.30 |
| 126 | python (3.9)| [sanic](https://github.com/huge-success/sanic) (20.12) | 28 575.08 | 32 696.23 | 32 749.66 |
| 127 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 28 202.20 | 31 281.02 | 32 976.72 |
| 128 | scala (2.13)| [play](https://playframework.com) (2.8) | 28 169.77 | 29 940.25 | 29 882.81 |
| 129 | python (3.9)| [starlette](https://starlette.io) (0.14) | 28 137.08 | 31 702.25 | 32 739.27 |
| 130 | javascript (14.15)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 27 644.49 | 30 302.59 | 30 764.13 |
| 131 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 27 198.69 | 29 186.52 | 28 828.74 |
| 132 | python (3.9)| [index.py](https://index-py.abersheeran.com) (0.16) | 25 660.65 | 29 163.14 | 30 759.17 |
| 133 | python (3.9)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 24 470.58 | 28 355.61 | 27 804.14 |
| 134 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 24 324.20 | 26 325.56 | 25 666.59 |
| 135 | python (3.9)| [responder](https://python-responder.org) (2.0) | 24 247.97 | 31 955.94 | 31 329.97 |
| 136 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 24 166.83 | 24 682.16 | 22 734.63 |
| 137 | clojure (1.1)| [luminus](https://luminusweb.com) (3.96) | 22 699.96 | 22 450.01 | 21 423.34 |
| 138 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 22 596.54 | 21 391.98 | 20 100.94 |
| 139 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 22 264.88 | 22 117.84 | 21 882.77 |
| 140 | javascript (14.15)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 22 100.21 | 23 976.08 | 23 046.48 |
| 141 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.16) | 20 934.89 | 20 285.62 | 18 559.10 |
| 142 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 20 827.76 | 20 346.80 | 20 463.14 |
| 143 | javascript (14.15)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 18 450.04 | 17 351.66 | 16 858.55 |
| 144 | python (3.9)| [molten](https://moltenframework.com) (1.0) | 18 370.19 | 22 302.81 | 22 224.59 |
| 145 | python (3.9)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 18 145.30 | 22 565.04 | 23 693.60 |
| 146 | python (3.9)| [fastapi](https://fastapi.tiangolo.com) (0.63) | 17 798.74 | 21 811.35 | 21 931.47 |
| 147 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 383.98 | 15 501.46 | 14 395.71 |
| 148 | rust (1.49)| [iron](https://ironframework.io) (0.6) | 16 988.13 | 16 584.81 | 16 751.40 |
| 149 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 16 913.95 | 16 482.85 | 16 026.13 |
| 150 | php (7.4)| [swoft](https://swoft.org) (2.0) | 16 830.82 | 19 997.29 | 20 628.15 |
| 151 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 16 407.96 | 16 096.95 | 15 610.48 |
| 152 | javascript (14.15)| [express](https://expressjs.com) (4.17) | 15 822.66 | 18 534.62 | 18 064.39 |
| 153 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 15 770.02 | 14 028.57 | 12 979.17 |
| 154 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 15 610.42 | 18 236.92 | 18 766.83 |
| 155 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.40) | 15 590.32 | 15 361.39 | 14 822.39 |
| 156 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 353.47 | 17 629.33 | 17 547.39 |
| 157 | go (1.15)| [macaron](https://go-macaron.com) (1.4) | 15 341.06 | 16 799.09 | 16 813.47 |
| 158 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 15 214.46 | 17 655.44 | 17 777.40 |
| 159 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 15 083.45 | 14 864.37 | 14 419.53 |
| 160 | javascript (14.15)| [feathersjs](https://feathersjs.com) (4.5) | 14 692.62 | 15 876.30 | 17 416.88 |
| 161 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 13 863.43 | 13 595.09 | 13 282.32 |
| 162 | java (11)| [struts2](https://struts.apache.org) (2.5) | 13 805.17 | 14 139.48 | 14 129.34 |
| 163 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 13 650.10 | 14 575.69 | 21 652.16 |
| 164 | ruby (2.7)| [rack_app](https://rack-app.com) (7.7) | 13 494.01 | 13 073.49 | 12 873.13 |
| 165 | python (3.9)| [flask](https://flask.pocoo.org) (1.1) | 13 450.41 | 17 782.98 | 14 968.32 |
| 166 | java (11)| [blade](https://lets-blade.com) (2.0) | 13 027.83 | 15 727.39 | 14 670.27 |
| 167 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 12 919.43 | 12 571.31 | 12 372.59 |
| 168 | javascript (14.15)| [nestjs-express](https://nestjs.com) (7.6) | 11 563.22 | 13 510.00 | 12 492.42 |
| 169 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 398.00 | 11 664.24 | 11 684.89 |
| 170 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 11 321.92 | 11 079.42 | 10 716.15 |
| 171 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 10 076.53 | 10 340.14 | 10 359.14 |
| 172 | python (3.9)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 10 014.98 | 10 007.69 | 9 819.43 |
| 173 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 922.90 | 10 175.53 | 10 081.16 |
| 174 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.6) | 9 916.78 | 10 117.08 | 10 102.74 |
| 175 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 9 743.28 | 9 997.50 | 10 101.96 |
| 176 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.7) | 9 627.36 | 9 741.11 | 9 733.36 |
| 177 | php (7.4)| [laravel-s-lumen](https://github.com/hhxsv5/laravel-s) (3.7) | 9 507.61 | 9 738.59 | 9 915.39 |
| 178 | python (3.9)| [guillotina](https://guillotina.io) (6.1) | 9 091.53 | 9 047.18 | 8 728.53 |
| 179 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.1) | 8 810.34 | 8 605.83 | 8 554.82 |
| 180 | ruby (2.7)| [grape](https://ruby-grape.org) (1.5) | 8 632.13 | 8 358.86 | 8 347.88 |
| 181 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 8 628.14 | 13 534.36 | 12 574.86 |
| 182 | python (3.9)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 7 624.62 | 7 290.70 | 6 856.71 |
| 183 | python (3.9)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 446.68 | 6 936.01 | 6 475.95 |
| 184 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 7 122.51 | 7 543.87 | 7 449.73 |
| 185 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 7 033.11 | 7 458.15 | 7 425.26 |
| 186 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 6 946.86 | 7 316.95 | 7 253.50 |
| 187 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 775.54 | 7 190.08 | 7 113.57 |
| 188 | php (7.4)| [laravel-s-laravel](https://github.com/hhxsv5/laravel-s) (3.7) | 6 466.97 | 6 369.14 | 6 376.08 |
| 189 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 399.74 | 6 679.69 | 6 694.91 |
| 190 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 6 082.66 | 6 857.55 | 6 853.80 |
| 191 | python (3.9)| [django](https://djangoproject.com) (3.1) | 5 867.70 | 6 061.49 | 5 955.16 |
| 192 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 5 849.65 | 6 012.30 | 5 967.06 |
| 193 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 5 663.99 | 5 556.62 | 5 592.79 |
| 194 | python (3.9)| [tornado](https://tornadoweb.org) (6.1) | 5 346.70 | 5 375.39 | 5 256.71 |
| 195 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.2) | 5 153.75 | 5 279.19 | 5 269.26 |
| 196 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 5 064.74 | 5 018.77 | 5 085.73 |
| 197 | php (7.4)| [fatfree](https://fatfreeframework.com) (3.7) | 4 944.66 | 5 154.99 | 5 113.28 |
| 198 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 739.99 | 4 901.93 | 4 880.53 |
| 199 | php (7.4)| [cubex](https://cubex.io) (4.15) | 4 482.91 | 4 612.99 | 4 645.97 |
| 200 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 364.45 | 4 542.83 | 4 528.68 |
| 201 | javascript (14.15)| [sails](https://sailsjs.com) (1.4) | 4 230.86 | 4 232.55 | 4 177.01 |
| 202 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.3) | 4 116.24 | 4 244.59 | 4 289.49 |
| 203 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.6) | 3 873.64 | 3 950.47 | 3 964.94 |
| 204 | php (7.4)| [lumen](https://lumen.laravel.com) (8.2) | 3 841.79 | 3 873.95 | 3 874.44 |
| 205 | php (7.4)| [nette](https://nette.org/en/) (3.1) | 3 824.62 | 3 910.92 | 3 921.16 |
| 206 | julia (1.6)| [merly](https://github.com/codeneomatrix/Merly.jl) (1.0) | 3 748.01 | 6 306.46 | 3 944.49 |
| 207 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 3 422.44 | 3 518.26 | 3 527.32 |
| 208 | php (7.4)| [symfony](https://symfony.com) (5.2) | 2 991.68 | 3 008.96 | 3 006.97 |
| 209 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.6) | 2 960.91 | 3 008.07 | 3 014.54 |
| 210 | ruby (2.7)| [rails](https://rubyonrails.org) (6.1) | 2 752.77 | 2 739.17 | 2 743.44 |
| 211 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 522.88 | 2 554.58 | 2 565.59 |
| 212 | python (3.9)| [masonite](https://masoniteproject.com) (3.0) | 2 519.86 | 2 490.97 | 2 497.46 |
| 213 | php (7.4)| [antidot](https://github.com/antidot-framework/react-framework) (0.2) | 2 171.08 | 670.73 | 1 183.06 |
| 214 | php (7.4)| [driftphp](https://github.com/driftphp/driftphp) (0.1) | 1 945.44 | 662.20 | 1 517.49 |
| 215 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 915.71 | 1 826.96 | 1 853.04 |
| 216 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 904.93 | 1 823.27 | 1 757.52 |
| 217 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.3) | 1 850.78 | 1 869.16 | 1 867.64 |
| 218 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 1 764.71 | 1 748.87 | 1 745.02 |
| 219 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 717.04 | 1 735.60 | 1 736.40 |
| 220 | python (3.9)| [cyclone](https://cyclone.io) (1.3) | 1 590.18 | 1 588.78 | 1 579.44 |
| 221 | python (3.9)| [klein](https://github.com/twisted/klein) (20.6) | 1 572.18 | 1 597.46 | 1 568.59 |
| 222 | python (3.9)| [django-ninja](https://django-ninja.rest-framework.com) (0.10) | 1 275.14 | 1 648.45 | 1 673.92 |
| 223 | python (3.9)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 216.56 | 1 166.55 | 1 151.60 |
| 224 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.1) | 1 189.46 | 1 209.34 | 1 206.83 |
| 225 | php (7.4)| [laravel](https://laravel.com) (8.26) | 991.88 | 992.35 | 991.98 |
| 226 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 673.05 | 885.08 | 359.23 |
| 227 | php (7.4)| [unic](https://unicframework.github.io/docs) (1.0) | 502.47 | 398.18 | 137.28 |
| 228 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 288.09 | 301.53 | -93.46 |
| 229 | cpp (11)| [nawa](https://github.com/jatofg/nawa) (0.6) | 260.46 | NaN | NaN |
</a>

</details>

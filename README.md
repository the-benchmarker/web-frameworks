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

A framework is a set of components working together. The main intention behind a framework is to facilitate (app or service) creation. The way a framework helps any developer may vary from one to another.

A majority of frameworks could be split in 2 parts :

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

## Results (2021-04-28)



<details open>
  <summary><strong>Technical details</strong></summary>
  <ul>
   <li>CPU : 8 Cores (AMD FX-8320E Eight-Core Processor)</li>
   <li>RAM : 16 Gb</li>
   <li>OS : Fedora</li>
   <li><pre>Docker version 20.10.5, build 55c4c88
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
| 1 | go (1.16)| [fiber](https://gofiber.io) (2.8) | 181 559.05 | 191 938.03 | 191 711.13 |
| 2 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 178 110.99 | 200 181.33 | 203 113.15 |
| 3 | go (1.16)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.6) | 177 835.25 | 187 575.57 | 187 006.80 |
| 4 | go (1.16)| [gearbox](https://gogearbox.com) (1.2) | 175 991.58 | 184 498.55 | 184 360.95 |
| 5 | go (1.16)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 175 983.64 | 188 426.96 | 187 520.37 |
| 6 | go (1.16)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 175 602.93 | 186 172.54 | 186 107.41 |
| 7 | go (1.16)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.23) | 174 303.54 | 193 072.51 | 193 906.60 |
| 8 | java (11)| [activej](https://activej.io) (4.1) | 170 399.57 | 207 857.02 | 211 467.63 |
| 9 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 167 067.04 | 202 156.56 | 206 830.16 |
| 10 | java (11)| [undertow](https://undertow.io) (2.2) | 164 368.98 | 199 244.39 | 202 245.36 |
| 11 | javascript (14.16)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.4) | 163 687.69 | 209 285.98 | 214 356.54 |
| 12 | javascript (14.16)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 159 445.68 | 199 809.11 | 204 490.63 |
| 13 | php (7.4)| [webman](https://github.com/walkor/webman) (1.0) | 157 596.41 | 168 979.11 | 171 846.96 |
| 14 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 156 158.96 | 191 238.83 | 195 197.79 |
| 15 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 155 997.14 | 186 686.04 | 192 060.51 |
| 16 | rust (1.51)| [actix](https://actix.rs) (3.3) | 155 428.48 | 187 600.59 | 192 225.42 |
| 17 | java (11)| [vertx](https://vertx.io/docs/vertx-core/java/) (4.0) | 154 236.60 | 183 997.45 | 185 748.50 |
| 18 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 153 244.29 | 192 602.83 | 200 324.29 |
| 19 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 151 824.86 | 180 206.28 | 186 730.50 |
| 20 | java (11)| [jooby](https://jooby.io) (2.9) | 150 210.94 | 187 882.42 | 195 483.59 |
| 21 | java (11)| [vertx4web](https://vertx.io/docs/vertx-web/java/) (4.0) | 149 839.23 | 175 425.80 | 177 740.23 |
| 22 | crystal (0.36)| [toro](https://github.com/soveran/toro) (0.4) | 146 006.75 | 176 846.96 | 176 605.11 |
| 23 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 145 288.32 | 183 191.06 | 188 825.93 |
| 24 | crystal (0.36)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 143 994.48 | 172 226.01 | 173 731.44 |
| 25 | crystal (0.36)| [spider-gazelle](https://spider-gazelle.net) (4.2) | 141 125.87 | 171 927.97 | 172 043.42 |
| 26 | crystal (0.36)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 138 954.12 | 164 315.17 | 164 507.01 |
| 27 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (1.1) | 137 262.90 | 164 142.90 | 167 197.49 |
| 28 | crystal (0.36)| [kemal](https://kemalcr.com) (0.27) | 135 681.83 | 161 411.11 | 161 151.08 |
| 29 | crystal (0.36)| [grip](https://github.com/grip-framework/grip) (1.0) | 132 361.79 | 154 422.27 | 153 458.10 |
| 30 | crystal (0.36)| [amber](https://amberframework.org) (0.36) | 131 430.84 | 149 358.72 | 147 756.75 |
| 31 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 125 133.93 | 148 686.70 | 153 362.30 |
| 32 | java (11)| [quarkus](https://quarkus.io) (1.13) | 124 920.23 | 154 398.37 | 157 862.58 |
| 33 | go (1.16)| [rte](https://github.com/jwilner/rte) (0.0) | 120 684.67 | 121 284.74 | 124 993.84 |
| 34 | go (1.16)| [clevergo](https://clevergo.tech) (0.5) | 119 697.13 | 121 003.17 | 124 336.88 |
| 35 | go (1.16)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 118 392.39 | 118 764.35 | 122 292.56 |
| 36 | go (1.16)| [echo](https://echo.labstack.com) (4.2) | 117 750.70 | 118 628.16 | 121 723.74 |
| 37 | javascript (14.16)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 117 628.51 | 135 801.27 | 137 565.10 |
| 38 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 116 055.05 | 151 784.92 | 161 930.86 |
| 39 | go (1.16)| [gin](https://gin-gonic.com) (1.7) | 115 685.77 | 121 702.25 | 123 964.03 |
| 40 | clojure (1.1)| [donkey](https://github.com/AppsFlyer/donkey) (0.5) | 115 620.92 | 124 235.67 | 125 084.51 |
| 41 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 113 792.98 | 189 340.42 | 192 834.77 |
| 42 | go (1.16)| [aero](https://github.com/aerogo/aero) (1.3) | 113 680.61 | 113 677.73 | 116 916.73 |
| 43 | go (1.16)| [chi](https://github.com/go-chi/chi) (1.5) | 113 216.69 | 111 523.54 | 115 422.87 |
| 44 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 113 129.41 | 145 786.49 | 148 517.83 |
| 45 | go (1.16)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 111 659.06 | 119 327.44 | 121 040.53 |
| 46 | go (1.16)| [violetear](https://violetear.org) (7.0) | 111 410.73 | 109 824.16 | 113 322.71 |
| 47 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 110 062.57 | 127 670.66 | 132 064.07 |
| 48 | go (1.16)| [webgo](https://github.com/bnkamalesh/webgo) (5.0) | 110 056.47 | 108 581.27 | 112 479.21 |
| 49 | rust (1.51)| [salvo](https://github.com/salvo-rs/salvo) (0.11) | 109 759.28 | 135 538.45 | 146 051.22 |
| 50 | crystal (0.36)| [athena](https://github.com/athena-framework/athena) (0.13) | 109 235.47 | 118 620.89 | 110 571.06 |
| 51 | go (1.16)| [goroute](https://goroute.github.io) (0.0) | 109 147.76 | 106 597.92 | 110 968.46 |
| 52 | go (1.16)| [kami](https://github.com/guregu/kami) (2.2) | 108 705.90 | 113 689.04 | 114 449.19 |
| 53 | fsharp (5.0)| [falco](https://www.falcoframework.com) (3.0) | 108 079.50 | 125 639.31 | 129 657.31 |
| 54 | rust (1.51)| [iron](https://iron/iron) (0.6) | 107 102.66 | 102 089.08 | 102 740.22 |
| 55 | go (1.16)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 105 912.56 | 102 163.20 | 106 698.79 |
| 56 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 105 203.63 | 121 449.94 | 125 125.38 |
| 57 | scala (2.13)| [finatra](https://twitter.github.io/finatra/) (21.3) | 104 978.79 | 123 809.61 | 124 496.76 |
| 58 | javascript (14.16)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 104 522.97 | 116 619.94 | 117 932.13 |
| 59 | rust (1.51)| [gotham](https://gotham.rs) (0.6) | 104 394.68 | 128 264.58 | 138 123.39 |
| 60 | go (1.16)| [beego](https://beego.me) (1.12) | 101 576.29 | 105 524.38 | 108 855.28 |
| 61 | java (11)| [restheart](https://restheart.org) (5.3) | 95 780.04 | 102 093.09 | 101 952.75 |
| 62 | go (1.16)| [goyave](https://github.com/go-goyave/goyave) (3.7) | 95 390.33 | 95 962.70 | 99 899.71 |
| 63 | php (7.4)| [swoole](https://github.com/swoole/swoole-src) (4.6) | 94 572.97 | 119 326.15 | 135 246.01 |
| 64 | go (1.16)| [air](https://github.com/aofei/air) (0.22) | 93 392.67 | 92 297.64 | 96 213.36 |
| 65 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 93 101.42 | 103 780.58 | 104 888.71 |
| 66 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 93 095.42 | 138 539.36 | 149 546.85 |
| 67 | rust (1.51)| [nickel](https://nickel-org.github.io) (0.11) | 91 130.47 | 73 792.43 | 92 442.07 |
| 68 | php (7.4)| [swoole-coroutine](https://github.com/swoole/swoole-src) (4.6) | 88 340.46 | 122 984.97 | 134 831.74 |
| 69 | javascript (14.16)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 86 636.22 | 96 434.25 | 98 337.21 |
| 70 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 85 265.18 | 92 271.75 | 91 307.45 |
| 71 | scala (2.13)| [finch](https://finagle.github.io/finch/) (0.32) | 82 978.09 | 95 882.55 | 95 490.22 |
| 72 | javascript (14.16)| [0http](https://github.com/jkyberneees/0http) (3.1) | 80 409.98 | 92 449.78 | 94 863.22 |
| 73 | scala (2.13)| [akkahttp](https://akka.io) (10.2) | 80 244.68 | 96 125.43 | 92 695.76 |
| 74 | go (1.16)| [gf](https://goframe.org) (1.15) | 80 158.22 | 87 953.09 | 90 280.81 |
| 75 | swift (5.3)| [hummingbird](https://github.com/hummingbird-project/hummingbird) (0.11) | 78 587.90 | 85 983.33 | 90 039.11 |
| 76 | javascript (14.16)| [restana](https://github.com/jkyberneees/ana) (4.7) | 77 024.11 | 88 380.34 | 88 941.03 |
| 77 | javascript (14.16)| [polka](https://github.com/lukeed/polka) (0.5) | 76 669.54 | 83 822.18 | 83 666.28 |
| 78 | javascript (14.16)| [rayo](https://rayo.js.org) (1.3) | 75 706.43 | 82 164.76 | 81 349.28 |
| 79 | elixir (1.12)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 75 598.04 | 79 776.36 | 78 157.43 |
| 80 | php (7.4)| [one](https://github.com/lizhichao/one) (2.2) | 75 076.66 | 88 473.50 | 96 290.93 |
| 81 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 74 920.37 | 84 190.79 | 93 750.24 |
| 82 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 73 951.14 | 88 469.39 | 90 434.79 |
| 83 | php (7.4)| [hyperf](https://hyperf.io) (2.1) | 73 294.72 | 85 473.49 | 91 434.12 |
| 84 | java (11)| [javalin](https://javalin.io) (3.9) | 73 271.87 | 80 215.88 | 81 287.70 |
| 85 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 72 988.68 | 82 678.38 | 82 317.28 |
| 86 | java (11)| [spark](https://sparkjava.com) (2.9) | 72 673.34 | 79 983.08 | 81 936.67 |
| 87 | python (3.9)| [falcon](https://falconframework.org) (3.0) | 72 561.68 | 80 688.49 | 82 140.99 |
| 88 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.5) | 70 948.72 | 68 037.66 | 63 782.37 |
| 89 | c (11)| [kore](https://kore.io) (3.3) | 70 939.35 | 108 267.11 | 153 205.05 |
| 90 | ruby (3.0)| [agoo](https://github.com/ohler55/agoo) (2.14) | 70 433.23 | 102 359.07 | 112 467.16 |
| 91 | kotlin (1.4)| [ktor](https://ktor.io) (1.5) | 69 358.21 | 89 904.41 | 92 877.58 |
| 92 | javascript (14.16)| [tinyhttp](https://tinyhttp.v1rtl.site) (1.2) | 68 355.02 | 77 501.71 | 76 214.58 |
| 93 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 68 244.89 | 85 646.20 | 89 936.15 |
| 94 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 68 033.77 | 76 741.74 | 76 840.26 |
| 95 | javascript (14.16)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 66 632.83 | 73 248.55 | 72 407.98 |
| 96 | go (1.16)| [mars](https://github.com/roblillack/mars) (1.0) | 65 349.43 | 66 508.32 | 71 468.00 |
| 97 | javascript (14.16)| [foxify](https://foxify.js.org) (0.1) | 64 852.15 | 68 694.96 | 68 696.43 |
| 98 | javascript (14.16)| [nestjs-fastify](https://nestjs.com) (7.6) | 61 377.29 | 66 235.06 | 65 735.53 |
| 99 | javascript (14.16)| [koa](https://koajs.com) (2.13) | 60 047.97 | 62 848.51 | 61 914.65 |
| 100 | python (3.9)| [bottle](https://bottlepy.org) (0.12) | 59 473.70 | 63 308.93 | 63 999.63 |
| 101 | javascript (14.16)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 57 302.29 | 59 835.67 | 59 581.99 |
| 102 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 53 556.75 | 55 648.32 | 55 080.53 |
| 103 | swift (5.3)| [vapor](https://vapor.codes) (4.44) | 53 286.30 | 55 388.65 | 55 006.18 |
| 104 | python (3.9)| [pyramid](https://trypyramid.com) (2.0) | 49 791.90 | 53 429.87 | 53 536.96 |
| 105 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 49 528.86 | 49 898.46 | 49 736.28 |
| 106 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 49 483.14 | 50 134.53 | 50 227.17 |
| 107 | javascript (14.16)| [fastify](https://fastify.io) (3.15) | 49 461.22 | 53 646.99 | 54 054.37 |
| 108 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 49 271.26 | 53 579.24 | 53 776.92 |
| 109 | javascript (14.16)| [moleculer](https://moleculer.services) (0.14) | 48 582.54 | 50 862.24 | 49 295.30 |
| 110 | elixir (1.12)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 48 403.02 | 50 780.07 | 50 925.33 |
| 111 | python (3.9)| [hug](https://hug.rest) (2.6) | 48 227.19 | 51 875.27 | 51 585.28 |
| 112 | javascript (14.16)| [fyrejet](https://github.com/fyrejet/fyrejet) (3.1) | 48 092.89 | 51 501.08 | 49 474.76 |
| 113 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 46 906.44 | 56 281.44 | 60 045.18 |
| 114 | python (3.9)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 44 676.42 | 49 682.77 | 51 786.11 |
| 115 | python (3.9)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 44 578.72 | 50 726.73 | 51 123.79 |
| 116 | java (11)| [micronaut](https://micronaut.io) (1.2) | 44 316.71 | 54 668.09 | 53 686.79 |
| 117 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 44 260.18 | 51 929.07 | 52 019.22 |
| 118 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 42 807.29 | 42 920.34 | 42 008.41 |
| 119 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 42 507.26 | 47 286.46 | 47 061.39 |
| 120 | javascript (14.16)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (3.1) | 42 320.58 | 45 234.32 | 45 884.44 |
| 121 | php (7.4)| [imi](https://imiphp.com) (1.2) | 42 250.06 | 46 981.64 | 49 193.18 |
| 122 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 42 152.82 | 39 870.51 | 33 098.19 |
| 123 | python (3.9)| [blacksheep](https://github.com/Neoteroi/BlackSheep) (1.0) | 42 053.84 | 47 553.00 | 49 019.14 |
| 124 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 40 421.24 | 47 225.84 | 47 205.44 |
| 125 | python (3.9)| [sanic](https://github.com/sanic-org/sanic) (21.3) | 40 403.36 | 46 543.08 | 46 815.16 |
| 126 | javascript (14.16)| [restify](https://restify.com) (8.5) | 40 196.68 | 44 364.93 | 42 632.34 |
| 127 | python (3.9)| [starlette](https://starlette.io) (0.14) | 38 132.55 | 45 582.47 | 45 938.82 |
| 128 | php (7.4)| [comet](https://github.com/gotzmann/comet) (1.2) | 38 016.91 | 38 351.00 | 38 479.71 |
| 129 | elixir (1.12)| [plug](https://hexdocs.pm/plug) (1.11) | 37 372.20 | 40 084.76 | 39 351.50 |
| 130 | javascript (14.16)| [hapi](https://hapijs.com) (20.1) | 36 895.08 | 38 578.51 | 39 152.66 |
| 131 | scala (2.13)| [play](https://playframework.com) (2.8) | 35 766.03 | 38 846.59 | 38 666.48 |
| 132 | python (3.9)| [emmett](https://emmett.sh) (2.2) | 35 529.17 | 41 181.42 | 41 166.39 |
| 133 | crystal (0.36)| [orion](https://github.com/obsidian/orion) (3.0) | 35 357.30 | 35 194.59 | 30 558.26 |
| 134 | dart (2.12)| [aqueduct](https://aqueduct.io) (3.3) | 34 930.24 | 34 677.93 | 34 412.84 |
| 135 | clojure (1.1)| [luminus](https://luminusweb.com) (4.0) | 34 495.49 | 36 515.91 | 36 449.86 |
| 136 | haskell (8.8)| [servant](https://docs.servant.dev) (0.18) | 32 680.31 | 29 974.66 | 28 842.49 |
| 137 | elixir (1.12)| [phoenix](https://phoenixframework.org) (1.5) | 32 441.41 | 35 619.56 | 35 147.57 |
| 138 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 32 229.81 | 31 923.52 | 31 675.88 |
| 139 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 31 999.09 | 24 618.44 | 24 587.59 |
| 140 | java (11)| [jersey3-grizzly2](https://eclipse-ee4j.github.io/jersey) (3) | 31 096.47 | 36 993.38 | 36 634.35 |
| 141 | java (11)| [jersey-grizzly2](https://eclipse-ee4j.github.io/jersey) (2.33) | 30 216.78 | 35 607.65 | 35 238.36 |
| 142 | python (3.9)| [index.py](https://index-py.abersheeran.com) (0.17) | 30 061.51 | 34 641.36 | 36 443.16 |
| 143 | crystal (0.36)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.17) | 29 837.86 | 29 202.50 | 25 819.77 |
| 144 | python (3.9)| [molten](https://moltenframework.com) (1.0) | 26 552.45 | 29 797.53 | 28 950.97 |
| 145 | python (3.9)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 26 485.85 | 33 674.24 | 33 695.31 |
| 146 | python (3.9)| [fastapi](https://fastapi.tiangolo.com) (0.63) | 25 845.55 | 31 760.72 | 31 861.76 |
| 147 | javascript (14.16)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 25 700.86 | 24 733.27 | 23 463.20 |
| 148 | php (7.4)| [swoft](https://swoft.org) (2.0) | 25 083.25 | 29 177.75 | 29 718.55 |
| 149 | python (3.9)| [responder](https://python-responder.org) (2.0) | 24 892.61 | 29 025.22 | 31 865.89 |
| 150 | python (3.9)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 687.48 | 28 547.05 | 28 454.93 |
| 151 | javascript (14.16)| [feathersjs](https://feathersjs.com) (4.5) | 23 499.65 | 23 940.86 | 25 582.96 |
| 152 | ruby (3.0)| [syro](https://github.com/soveran/syro) (3.2) | 23 314.08 | 22 469.02 | 22 357.49 |
| 153 | go (1.16)| [macaron](https://go-macaron.com) (1.4) | 23 190.69 | 25 396.35 | 25 557.03 |
| 154 | ruby (3.0)| [hanami-api](https://hanamirb.org) (0.1) | 23 148.03 | 22 603.88 | 22 165.75 |
| 155 | python (3.9)| [flask](https://flask.pocoo.org) (1.1) | 22 827.79 | 25 334.55 | 25 693.82 |
| 156 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 22 798.36 | 20 072.82 | 18 735.62 |
| 157 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 22 698.32 | 25 501.96 | 26 012.10 |
| 158 | ruby (3.0)| [roda](https://roda.jeremyevans.net) (3.43) | 22 383.83 | 21 727.67 | 21 345.67 |
| 159 | javascript (14.16)| [express](https://expressjs.com) (4.17) | 21 548.61 | 26 591.66 | 25 914.59 |
| 160 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 21 213.41 | 24 220.51 | 24 568.79 |
| 161 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 20 545.09 | 18 647.80 | 17 204.29 |
| 162 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 20 284.79 | 23 308.45 | 23 328.65 |
| 163 | ruby (3.0)| [cuba](https://cuba.is) (3.9) | 19 813.85 | 19 200.63 | 19 024.32 |
| 164 | ruby (3.0)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 19 411.49 | 18 719.85 | 18 626.84 |
| 165 | ruby (3.0)| [rack_app](https://rack-app.com) (7.7) | 18 257.19 | 17 810.95 | 17 419.86 |
| 166 | java (11)| [blade](https://lets-blade.com) (2.0) | 18 045.08 | 21 620.34 | 20 771.87 |
| 167 | ruby (3.0)| [camping](https://github.com/camping/camping) (2.1) | 17 918.79 | 17 333.12 | 17 163.37 |
| 168 | php (7.4)| [antidot](https://github.com/antidot-framework/react-framework) (0.2) | 17 749.05 | 19 094.78 | 18 908.47 |
| 169 | go (1.16)| [tango](https://gitea.com/lunny/tango) (0.6) | 16 920.47 | 17 433.45 | 17 460.37 |
| 170 | javascript (14.16)| [nestjs-express](https://nestjs.com) (7.6) | 16 264.40 | 16 727.79 | 17 672.10 |
| 171 | php (7.4)| [driftphp](https://github.com/driftphp/driftphp) (0.1) | 16 112.54 | 17 247.35 | 16 633.87 |
| 172 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 14 821.12 | 15 149.60 | 15 383.66 |
| 173 | go (1.16)| [gramework](https://github.com/gramework/gramework) (1.7) | 14 767.65 | 15 164.77 | 15 112.81 |
| 174 | dart (2.12)| [start](https://github.com/lvivski/start) (0.4) | 14 706.85 | 14 504.48 | 14 037.77 |
| 175 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.6) | 14 417.31 | 14 680.66 | 14 847.22 |
| 176 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 14 380.75 | 14 705.84 | 14 805.44 |
| 177 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.7) | 14 269.18 | 14 375.27 | 14 573.07 |
| 178 | python (3.9)| [guillotina](https://guillotina.io) (6.3) | 13 420.52 | 13 051.98 | 12 544.37 |
| 179 | php (7.4)| [laravel-s-lumen](https://github.com/hhxsv5/laravel-s) (3.7) | 13 057.95 | 13 832.79 | 13 598.29 |
| 180 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 12 933.83 | 13 332.04 | 12 759.46 |
| 181 | java (11)| [struts2](https://struts.apache.org) (2.5) | 12 548.80 | 13 212.04 | 13 228.99 |
| 182 | ruby (3.0)| [sinatra](https://sinatrarb.com) (2.1) | 12 108.03 | 11 831.55 | 11 846.67 |
| 183 | pony (0.39)| [jennet](https://github.com/Theodus/jennet) (0.1) | 11 968.42 | 19 856.33 | 18 586.83 |
| 184 | python (3.9)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 11 150.24 | 10 724.62 | 10 101.01 |
| 185 | python (3.9)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 10 900.82 | 10 205.53 | 9 371.68 |
| 186 | ruby (3.0)| [grape](https://ruby-grape.org) (1.5) | 10 842.33 | 10 561.83 | 10 540.42 |
| 187 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 10 180.14 | 11 052.59 | 10 884.87 |
| 188 | python (3.9)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 10 030.38 | 10 129.65 | 9 185.60 |
| 189 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 9 847.46 | 10 679.11 | 10 547.48 |
| 190 | php (7.4)| [laravel-s-laravel](https://github.com/hhxsv5/laravel-s) (3.7) | 9 371.38 | 9 247.55 | 9 155.18 |
| 191 | python (3.9)| [tornado](https://tornadoweb.org) (6.1) | 8 533.08 | 8 554.34 | 8 427.08 |
| 192 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 297.08 | 8 835.86 | 8 770.66 |
| 193 | python (3.9)| [django](https://djangoproject.com) (3.2) | 7 814.19 | 7 703.61 | 7 453.86 |
| 194 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 7 339.02 | 8 454.09 | 8 575.56 |
| 195 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.2) | 7 324.90 | 7 754.99 | 7 648.12 |
| 196 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 7 151.12 | 7 475.03 | 7 463.12 |
| 197 | php (7.4)| [fatfree](https://fatfreeframework.com) (3.7) | 7 076.35 | 7 553.84 | 7 450.16 |
| 198 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 6 803.75 | 7 179.46 | 7 160.47 |
| 199 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 659.46 | 6 958.44 | 6 858.77 |
| 200 | javascript (14.16)| [sails](https://sailsjs.com) (1.4) | 6 076.18 | 6 149.84 | 6 068.38 |
| 201 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.3) | 6 003.25 | 6 247.96 | 6 247.70 |
| 202 | php (7.4)| [ice](https://iceframework.org) (1.5) | 5 857.69 | 6 142.16 | 6 155.77 |
| 203 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.6) | 5 553.79 | 5 752.38 | 5 778.12 |
| 204 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 5 102.84 | 5 257.00 | 5 274.92 |
| 205 | php (7.4)| [cubex](https://cubex.io) (4.16) | 4 512.26 | 4 610.60 | 4 614.09 |
| 206 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.6) | 4 338.18 | 4 467.35 | 4 475.88 |
| 207 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 278.68 | 4 456.50 | 4 436.51 |
| 208 | ruby (3.0)| [rails](https://rubyonrails.org) (6.1) | 3 978.50 | 3 930.11 | 3 927.17 |
| 209 | php (7.4)| [lumen](https://lumen.laravel.com) (8.2) | 3 874.13 | 3 955.22 | 3 959.40 |
| 210 | php (7.4)| [nette](https://nette.org/en/) (3.1) | 3 845.98 | 3 954.27 | 3 968.05 |
| 211 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 3 720.82 | 3 828.03 | 3 831.28 |
| 212 | python (3.9)| [masonite](https://masoniteproject.com) (3.0) | 3 668.39 | 3 581.23 | 3 584.20 |
| 213 | julia (1.6)| [merly](https://github.com/codeneomatrix/Merly.jl) (1.0) | 3 647.92 | 8 932.50 | 5 460.18 |
| 214 | php (7.4)| [symfony](https://symfony.com) (5.2) | 3 012.95 | 3 056.67 | 3 061.86 |
| 215 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.4) | 2 666.00 | 2 712.76 | 2 712.41 |
| 216 | python (3.9)| [cyclone](https://cyclone.io) (1.3) | 2 336.46 | 2 325.16 | 2 318.68 |
| 217 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 978.25 | 1 548.72 | 1 076.35 |
| 218 | r (4.0)| [restrserve](https://restrserve.org) (0.4) | 1 846.99 | 1 757.70 | 1 757.45 |
| 219 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.6) | 1 834.11 | 1 758.77 | 1 697.82 |
| 220 | python (3.9)| [django-ninja](https://django-ninja.rest-framework.com) (0.12) | 1 816.22 | 2 519.53 | 2 446.46 |
| 221 | python (3.9)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 764.03 | 1 692.97 | 1 680.20 |
| 222 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 734.25 | 1 765.90 | 1 770.05 |
| 223 | php (7.4)| [laravel](https://laravel.com) (8.39) | 1 499.97 | 1 507.74 | 1 511.92 |
| 224 | python (3.9)| [klein](https://github.com/twisted/klein) (20.6) | 1 373.63 | 1 398.49 | 1 379.69 |
| 225 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.1) | 1 223.91 | 1 253.29 | 1 250.39 |
| 226 | php (7.4)| [unic](https://unicframework.github.io/docs) (1.0) | 504.73 | 461.42 | 369.87 |
| 227 | r (4.0)| [plumber](https://rplumber.io) (1.1) | 423.58 | 449.97 | 435.20 |
| 228 | cpp (11)| [nawa](https://github.com/jatofg/nawa) (0.6) | 271.35 | NaN | NaN |
</a>

</details>

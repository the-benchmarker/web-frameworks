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

## Results (2021-04-20)



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
| 1 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 178 526.24 | 198 712.31 | 201 441.33 |
| 2 | go (1.16)| [fiber](https://gofiber.io) (2.8) | 178 395.23 | 190 781.01 | 189 718.71 |
| 3 | go (1.16)| [gearbox](https://gogearbox.com) (1.2) | 175 309.80 | 183 068.54 | 182 606.32 |
| 4 | go (1.16)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.6) | 175 161.14 | 186 987.27 | 186 113.39 |
| 5 | go (1.16)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.23) | 174 870.57 | 193 878.19 | 194 671.51 |
| 6 | go (1.16)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 174 637.57 | 185 504.70 | 185 315.96 |
| 7 | go (1.16)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 174 410.42 | 188 567.08 | 187 876.93 |
| 8 | java (11)| [activej](https://activej.io) (4.1) | 172 152.68 | 209 113.85 | 212 338.62 |
| 9 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 169 355.98 | 203 081.82 | 208 601.06 |
| 10 | java (11)| [undertow](https://undertow.io) (2.2) | 164 161.98 | 200 658.45 | 203 162.93 |
| 11 | javascript (14.16)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.4) | 162 057.88 | 205 312.06 | 210 471.39 |
| 12 | php (7.4)| [webman](https://github.com/walkor/webman) (1.0) | 160 963.89 | 170 054.23 | 171 415.37 |
| 13 | javascript (14.16)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 159 123.15 | 197 280.65 | 202 453.42 |
| 14 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 156 746.87 | 196 936.27 | 204 262.27 |
| 15 | java (11)| [vertx](https://vertx.io/docs/vertx-core/java/) (4.0) | 155 899.94 | 185 017.21 | 187 116.80 |
| 16 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 155 843.67 | 192 325.89 | 195 701.87 |
| 17 | java (11)| [jooby](https://jooby.io) (2.9) | 152 956.67 | 192 745.51 | 198 981.08 |
| 18 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 152 248.37 | 183 448.59 | 188 185.44 |
| 19 | rust (1.51)| [actix](https://actix.rs) (3.3) | 152 202.15 | 187 438.71 | 191 109.19 |
| 20 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 152 045.12 | 182 192.04 | 185 763.42 |
| 21 | java (11)| [vertx4web](https://vertx.io/docs/vertx-web/java/) (4.0) | 150 146.83 | 176 695.84 | 178 639.63 |
| 22 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 146 208.24 | 184 221.57 | 192 080.44 |
| 23 | crystal (0.36)| [toro](https://github.com/soveran/toro) (0.4) | 143 755.32 | 176 142.91 | 176 232.88 |
| 24 | crystal (0.36)| [spider-gazelle](https://spider-gazelle.net) (4.2) | 143 284.95 | 170 272.21 | 171 937.29 |
| 25 | crystal (0.36)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 142 391.25 | 172 249.87 | 173 327.84 |
| 26 | crystal (0.36)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 138 408.58 | 165 981.78 | 165 926.48 |
| 27 | crystal (0.36)| [kemal](https://kemalcr.com) (0.27) | 136 516.42 | 159 971.84 | 160 740.44 |
| 28 | crystal (0.36)| [grip](https://github.com/grip-framework/grip) (1.0) | 132 575.85 | 155 085.24 | 154 819.59 |
| 29 | crystal (0.36)| [amber](https://amberframework.org) (0.36) | 127 899.99 | 149 638.96 | 147 416.21 |
| 30 | java (11)| [quarkus](https://quarkus.io) (1.13) | 125 554.34 | 154 399.78 | 158 271.59 |
| 31 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 122 841.07 | 145 372.48 | 148 686.40 |
| 32 | go (1.16)| [rte](https://github.com/jwilner/rte) (0.0) | 119 813.56 | 120 457.35 | 123 664.05 |
| 33 | go (1.16)| [clevergo](https://clevergo.tech) (0.5) | 118 818.57 | 120 142.27 | 123 388.77 |
| 34 | go (1.16)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 118 313.63 | 118 800.12 | 122 136.49 |
| 35 | javascript (14.16)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 118 018.33 | 134 981.55 | 137 649.81 |
| 36 | go (1.16)| [echo](https://echo.labstack.com) (4.2) | 117 442.32 | 118 486.97 | 121 429.51 |
| 37 | clojure (1.1)| [donkey](https://github.com/AppsFlyer/donkey) (0.5) | 116 788.13 | 124 550.99 | 125 390.15 |
| 38 | go (1.16)| [gin](https://gin-gonic.com) (1.7) | 115 735.41 | 122 037.18 | 123 925.57 |
| 39 | go (1.16)| [aero](https://github.com/aerogo/aero) (1.3) | 113 852.62 | 113 954.72 | 117 309.04 |
| 40 | go (1.16)| [chi](https://github.com/go-chi/chi) (1.5) | 112 699.17 | 110 966.41 | 115 144.93 |
| 41 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 112 608.96 | 156 338.13 | 162 786.89 |
| 42 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 112 010.39 | 146 585.46 | 149 703.24 |
| 43 | go (1.16)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 111 827.17 | 119 057.75 | 121 069.37 |
| 44 | go (1.16)| [violetear](https://violetear.org) (7.0) | 111 228.61 | 109 582.03 | 113 133.78 |
| 45 | go (1.16)| [webgo](https://github.com/bnkamalesh/webgo) (5.0) | 110 657.83 | 108 817.76 | 112 427.17 |
| 46 | rust (1.51)| [salvo](https://github.com/salvo-rs/salvo) (0.11) | 109 412.52 | 134 986.53 | 145 335.59 |
| 47 | go (1.16)| [goroute](https://goroute.github.io) (0.0) | 108 639.41 | 106 328.71 | 110 641.03 |
| 48 | go (1.16)| [kami](https://github.com/guregu/kami) (2.2) | 108 455.72 | 113 090.60 | 114 136.53 |
| 49 | fsharp (5.0)| [falco](https://www.falcoframework.com) (3.0) | 108 135.14 | 125 798.60 | 129 710.19 |
| 50 | crystal (0.36)| [athena](https://github.com/athena-framework/athena) (0.13) | 107 844.80 | 117 528.34 | 110 725.86 |
| 51 | rust (1.51)| [iron](https://iron/iron) (0.6) | 106 880.47 | 103 912.52 | 104 480.60 |
| 52 | scala (2.13)| [finatra](https://twitter.github.io/finatra/) (21.3) | 106 716.52 | 125 324.07 | 126 297.06 |
| 53 | go (1.16)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 105 791.65 | 101 657.24 | 106 747.14 |
| 54 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 105 149.22 | 121 441.48 | 125 419.25 |
| 55 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 105 039.57 | 121 920.16 | 125 836.67 |
| 56 | rust (1.51)| [gotham](https://gotham.rs) (0.6) | 104 755.78 | 128 478.84 | 138 169.14 |
| 57 | javascript (14.16)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 103 262.90 | 116 517.68 | 119 006.37 |
| 58 | go (1.16)| [beego](https://beego.me) (1.12) | 101 380.08 | 105 414.15 | 108 639.86 |
| 59 | php (7.4)| [swoole](https://github.com/swoole/swoole-src) (4.6) | 101 286.42 | 125 113.05 | 128 191.04 |
| 60 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 101 012.88 | 183 756.60 | 190 388.27 |
| 61 | go (1.16)| [goyave](https://github.com/go-goyave/goyave) (3.7) | 95 430.07 | 96 146.57 | 100 165.80 |
| 62 | java (11)| [restheart](https://restheart.org) (5.3) | 93 915.02 | 100 636.20 | 100 955.43 |
| 63 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 93 790.23 | 105 109.72 | 106 189.11 |
| 64 | go (1.16)| [air](https://github.com/aofei/air) (0.22) | 92 993.56 | 91 860.59 | 95 650.02 |
| 65 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 92 719.29 | 138 325.56 | 148 462.74 |
| 66 | php (7.4)| [swoole-coroutine](https://github.com/swoole/swoole-src) (4.6) | 89 695.80 | 123 334.38 | 135 218.64 |
| 67 | javascript (14.16)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 88 787.62 | 96 647.54 | 99 018.93 |
| 68 | rust (1.51)| [nickel](https://nickel-org.github.io) (0.11) | 86 883.71 | 84 012.30 | 89 078.41 |
| 69 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 86 473.40 | 93 293.82 | 92 027.74 |
| 70 | scala (2.13)| [finch](https://finagle.github.io/finch/) (0.32) | 84 645.37 | 97 271.62 | 97 092.17 |
| 71 | javascript (14.16)| [0http](https://github.com/jkyberneees/0http) (3.1) | 83 085.28 | 93 991.73 | 94 479.90 |
| 72 | go (1.16)| [gf](https://goframe.org) (1.15) | 80 177.04 | 87 924.95 | 90 119.69 |
| 73 | scala (2.13)| [akkahttp](https://akka.io) (10.2) | 79 951.02 | 94 982.24 | 92 545.22 |
| 74 | javascript (14.16)| [polka](https://github.com/lukeed/polka) (0.5) | 77 494.22 | 84 731.93 | 83 758.03 |
| 75 | javascript (14.16)| [restana](https://github.com/jkyberneees/ana) (4.7) | 76 763.99 | 86 521.04 | 88 232.86 |
| 76 | javascript (14.16)| [rayo](https://rayo.js.org) (1.3) | 76 334.66 | 82 182.78 | 81 035.57 |
| 77 | elixir (1.12)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 75 790.11 | 79 732.29 | 78 144.21 |
| 78 | php (7.4)| [hyperf](https://hyperf.io) (2.1) | 74 833.36 | 87 087.58 | 95 584.33 |
| 79 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 74 027.01 | 88 710.20 | 91 049.87 |
| 80 | java (11)| [javalin](https://javalin.io) (3.9) | 73 308.36 | 80 689.15 | 80 508.55 |
| 81 | php (7.4)| [one](https://github.com/lizhichao/one) (2.2) | 73 123.24 | 87 719.44 | 96 862.67 |
| 82 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 72 970.65 | 82 836.93 | 82 737.12 |
| 83 | python (3.9)| [falcon](https://falconframework.org) (3.0) | 72 543.42 | 80 144.27 | 81 792.96 |
| 84 | swift (5.3)| [hummingbird](https://github.com/hummingbird-project/hummingbird) (0.10) | 71 916.80 | 78 541.45 | 79 217.88 |
| 85 | java (11)| [spark](https://sparkjava.com) (2.9) | 71 847.47 | 79 661.63 | 81 329.59 |
| 86 | ruby (3.0)| [agoo](https://github.com/ohler55/agoo) (2.14) | 71 130.30 | 104 657.49 | 116 988.31 |
| 87 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 70 264.48 | 81 908.42 | 89 070.48 |
| 88 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.5) | 69 831.14 | 69 459.98 | 62 882.93 |
| 89 | kotlin (1.4)| [ktor](https://ktor.io) (1.5) | 69 547.28 | 90 498.71 | 93 480.21 |
| 90 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 68 367.11 | 85 945.04 | 90 547.48 |
| 91 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 68 046.97 | 77 253.22 | 76 894.30 |
| 92 | javascript (14.16)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 66 711.16 | 73 563.13 | 72 283.71 |
| 93 | c (11)| [kore](https://kore.io) (3.3) | 65 984.93 | 146 546.59 | 156 313.38 |
| 94 | go (1.16)| [mars](https://github.com/roblillack/mars) (1.0) | 65 659.10 | 66 320.27 | 71 296.67 |
| 95 | javascript (14.16)| [foxify](https://foxify.js.org) (0.1) | 63 821.62 | 69 183.76 | 69 138.65 |
| 96 | javascript (14.16)| [nestjs-fastify](https://nestjs.com) (7.6) | 61 621.98 | 67 201.63 | 65 379.87 |
| 97 | python (3.9)| [bottle](https://bottlepy.org) (0.12) | 60 642.08 | 65 514.31 | 66 559.10 |
| 98 | javascript (14.16)| [koa](https://koajs.com) (2.13) | 59 510.13 | 62 707.78 | 61 422.05 |
| 99 | javascript (14.16)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 57 390.89 | 61 467.90 | 60 591.11 |
| 100 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 54 417.97 | 56 165.08 | 56 096.94 |
| 101 | swift (5.3)| [vapor](https://vapor.codes) (4.44) | 50 016.84 | 52 615.04 | 52 308.39 |
| 102 | python (3.9)| [pyramid](https://trypyramid.com) (2.0) | 49 710.45 | 52 632.11 | 52 867.63 |
| 103 | python (3.9)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 49 478.17 | 56 557.05 | 57 598.87 |
| 104 | javascript (14.16)| [fastify](https://fastify.io) (3.14) | 48 989.47 | 53 788.21 | 53 257.57 |
| 105 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 48 978.44 | 53 618.78 | 53 849.64 |
| 106 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 48 919.23 | 49 098.42 | 49 083.89 |
| 107 | elixir (1.12)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 48 599.97 | 51 222.99 | 51 289.14 |
| 108 | javascript (14.16)| [fyrejet](https://github.com/fyrejet/fyrejet) (3.1) | 48 205.18 | 50 520.43 | 49 359.16 |
| 109 | javascript (14.16)| [moleculer](https://moleculer.services) (0.14) | 48 174.48 | 50 553.55 | 49 264.55 |
| 110 | python (3.9)| [hug](https://hug.rest) (2.6) | 48 163.63 | 52 265.45 | 52 041.08 |
| 111 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 47 599.38 | 48 447.67 | 48 269.83 |
| 112 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 47 196.65 | 56 300.70 | 58 669.76 |
| 113 | java (11)| [micronaut](https://micronaut.io) (1.2) | 47 140.12 | 54 933.89 | 55 652.24 |
| 114 | python (3.9)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 45 449.60 | 49 947.58 | 50 814.41 |
| 115 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 43 666.01 | 51 127.64 | 50 984.83 |
| 116 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 43 008.50 | 43 299.56 | 42 290.53 |
| 117 | php (7.4)| [imi](https://imiphp.com) (1.2) | 42 915.12 | 48 173.09 | 50 448.16 |
| 118 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 42 549.53 | 47 528.97 | 47 036.70 |
| 119 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 42 408.98 | 39 396.06 | 31 292.75 |
| 120 | python (3.9)| [blacksheep](https://github.com/Neoteroi/BlackSheep) (1.0) | 42 070.17 | 49 020.50 | 49 040.81 |
| 121 | javascript (14.16)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (3.1) | 41 417.60 | 44 987.47 | 45 813.79 |
| 122 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 41 322.97 | 45 779.44 | 47 764.10 |
| 123 | javascript (14.16)| [restify](https://restify.com) (8.5) | 40 328.20 | 43 844.30 | 43 075.87 |
| 124 | python (3.9)| [sanic](https://github.com/sanic-org/sanic) (21.3) | 40 234.09 | 46 481.27 | 46 642.48 |
| 125 | python (3.9)| [starlette](https://starlette.io) (0.14) | 39 193.70 | 43 360.03 | 45 900.70 |
| 126 | php (7.4)| [comet](https://github.com/gotzmann/comet) (1.2) | 37 823.69 | 38 599.13 | 38 360.90 |
| 127 | elixir (1.12)| [plug](https://hexdocs.pm/plug) (1.11) | 37 482.98 | 40 676.84 | 39 702.91 |
| 128 | scala (2.13)| [play](https://playframework.com) (2.8) | 36 355.95 | 39 991.74 | 39 551.85 |
| 129 | crystal (0.36)| [orion](https://github.com/obsidian/orion) (3.0) | 35 048.10 | 33 998.81 | 31 086.75 |
| 130 | javascript (14.16)| [hapi](https://hapijs.com) (20.1) | 34 785.03 | 37 440.39 | 37 314.05 |
| 131 | dart (2.12)| [aqueduct](https://aqueduct.io) (3.3) | 34 547.49 | 34 429.35 | 34 104.96 |
| 132 | python (3.9)| [emmett](https://emmett.sh) (2.2) | 34 284.17 | 39 543.99 | 40 325.97 |
| 133 | clojure (1.1)| [luminus](https://luminusweb.com) (4.0) | 34 211.40 | 37 003.90 | 36 942.82 |
| 134 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 33 871.85 | 25 281.73 | 25 283.20 |
| 135 | haskell (8.8)| [servant](https://docs.servant.dev) (0.18) | 32 787.03 | 30 861.02 | 29 320.92 |
| 136 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 32 239.32 | 31 823.80 | 31 563.86 |
| 137 | elixir (1.12)| [phoenix](https://phoenixframework.org) (1.5) | 32 061.15 | 35 839.94 | 35 251.92 |
| 138 | java (11)| [jersey3-grizzly2](https://eclipse-ee4j.github.io/jersey) (3) | 30 835.90 | 36 509.26 | 36 106.49 |
| 139 | python (3.9)| [index.py](https://index-py.abersheeran.com) (0.17) | 30 521.99 | 36 839.14 | 36 092.22 |
| 140 | crystal (0.36)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.17) | 30 249.41 | 30 486.24 | 26 852.38 |
| 141 | java (11)| [jersey-grizzly2](https://eclipse-ee4j.github.io/jersey) (2.33) | 29 667.26 | 35 429.58 | 34 872.61 |
| 142 | python (3.9)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 28 258.70 | 32 122.27 | 33 709.83 |
| 143 | python (3.9)| [molten](https://moltenframework.com) (1.0) | 26 781.74 | 30 286.36 | 29 976.36 |
| 144 | python (3.9)| [responder](https://python-responder.org) (2.0) | 26 575.49 | 29 677.64 | 31 348.78 |
| 145 | javascript (14.16)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 25 966.72 | 24 086.68 | 22 891.05 |
| 146 | php (7.4)| [swoft](https://swoft.org) (2.0) | 24 992.68 | 29 506.99 | 29 621.06 |
| 147 | python (3.9)| [fastapi](https://fastapi.tiangolo.com) (0.63) | 24 978.16 | 29 579.04 | 31 761.59 |
| 148 | ruby (3.0)| [syro](https://github.com/soveran/syro) (3.2) | 23 371.27 | 22 748.55 | 22 344.93 |
| 149 | ruby (3.0)| [hanami-api](https://hanamirb.org) (0.1) | 23 229.98 | 22 467.88 | 22 045.46 |
| 150 | go (1.16)| [macaron](https://go-macaron.com) (1.4) | 22 989.27 | 25 276.51 | 25 423.12 |
| 151 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 22 846.87 | 25 972.52 | 26 028.46 |
| 152 | javascript (14.16)| [feathersjs](https://feathersjs.com) (4.5) | 22 560.02 | 25 941.37 | 25 761.53 |
| 153 | python (3.9)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 22 393.13 | 28 188.58 | 28 182.56 |
| 154 | ruby (3.0)| [roda](https://roda.jeremyevans.net) (3.43) | 22 370.63 | 21 622.68 | 21 290.96 |
| 155 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 22 222.31 | 19 390.85 | 18 067.75 |
| 156 | javascript (14.16)| [express](https://expressjs.com) (4.17) | 21 615.73 | 26 348.22 | 25 586.39 |
| 157 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 21 359.66 | 24 248.80 | 24 624.24 |
| 158 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 20 415.53 | 22 925.97 | 23 341.74 |
| 159 | ruby (3.0)| [cuba](https://cuba.is) (3.9) | 19 705.75 | 19 173.33 | 18 972.77 |
| 160 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 19 613.92 | 17 818.62 | 16 565.15 |
| 161 | python (3.9)| [flask](https://flask.pocoo.org) (1.1) | 19 462.78 | 21 569.92 | 23 036.39 |
| 162 | ruby (3.0)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 19 163.62 | 18 584.73 | 18 479.50 |
| 163 | ruby (3.0)| [rack_app](https://rack-app.com) (7.7) | 18 514.29 | 17 721.77 | 17 405.47 |
| 164 | ruby (3.0)| [camping](https://github.com/camping/camping) (2.1) | 17 910.74 | 17 317.95 | 17 157.13 |
| 165 | php (7.4)| [antidot](https://github.com/antidot-framework/react-framework) (0.2) | 17 880.92 | 20 280.72 | 19 545.10 |
| 166 | java (11)| [blade](https://lets-blade.com) (2.0) | 17 038.81 | 22 813.35 | 20 870.87 |
| 167 | go (1.16)| [tango](https://gitea.com/lunny/tango) (0.6) | 16 981.37 | 17 481.02 | 17 509.72 |
| 168 | javascript (14.16)| [nestjs-express](https://nestjs.com) (7.6) | 16 459.41 | 17 659.96 | 17 288.28 |
| 169 | php (7.4)| [driftphp](https://github.com/driftphp/driftphp) (0.1) | 16 146.25 | 18 009.92 | 16 468.11 |
| 170 | go (1.16)| [gramework](https://github.com/gramework/gramework) (1.7) | 14 879.87 | 15 252.19 | 15 191.35 |
| 171 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 14 862.90 | 15 105.63 | 15 317.02 |
| 172 | dart (2.12)| [start](https://github.com/lvivski/start) (0.4) | 14 555.71 | 14 329.17 | 13 787.98 |
| 173 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.6) | 14 445.46 | 14 806.86 | 14 907.71 |
| 174 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 14 327.57 | 14 677.73 | 14 794.39 |
| 175 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.7) | 14 204.05 | 14 410.43 | 14 510.99 |
| 176 | python (3.9)| [guillotina](https://guillotina.io) (6.2) | 13 380.82 | 12 834.28 | 12 364.04 |
| 177 | php (7.4)| [laravel-s-lumen](https://github.com/hhxsv5/laravel-s) (3.7) | 13 195.93 | 13 424.43 | 13 104.42 |
| 178 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 12 295.38 | 13 125.38 | 13 165.05 |
| 179 | java (11)| [struts2](https://struts.apache.org) (2.5) | 12 185.56 | 13 043.19 | 13 049.41 |
| 180 | ruby (3.0)| [sinatra](https://sinatrarb.com) (2.1) | 12 060.98 | 11 809.16 | 11 800.03 |
| 181 | pony (0.39)| [jennet](https://github.com/Theodus/jennet) (0.1) | 11 484.12 | 20 179.44 | 18 834.66 |
| 182 | python (3.9)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 11 191.37 | 10 559.05 | 10 194.92 |
| 183 | python (3.9)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 10 877.07 | 10 281.69 | 9 520.69 |
| 184 | ruby (3.0)| [grape](https://ruby-grape.org) (1.5) | 10 544.61 | 10 260.57 | 10 260.37 |
| 185 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 10 228.59 | 11 045.60 | 10 903.37 |
| 186 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 9 866.58 | 10 654.40 | 10 533.08 |
| 187 | python (3.9)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 711.70 | 9 793.96 | 9 570.70 |
| 188 | php (7.4)| [laravel-s-laravel](https://github.com/hhxsv5/laravel-s) (3.7) | 9 392.38 | 9 278.21 | 9 250.39 |
| 189 | python (3.9)| [django](https://djangoproject.com) (3.2) | 8 856.58 | 8 801.67 | 8 387.95 |
| 190 | python (3.9)| [tornado](https://tornadoweb.org) (6.1) | 8 679.02 | 8 616.64 | 8 408.69 |
| 191 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 340.74 | 8 816.54 | 8 717.46 |
| 192 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.2) | 7 339.82 | 7 723.15 | 7 634.18 |
| 193 | php (7.4)| [fatfree](https://fatfreeframework.com) (3.7) | 7 157.43 | 7 592.62 | 7 506.76 |
| 194 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 7 074.01 | 7 502.67 | 7 434.14 |
| 195 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 7 067.50 | 8 308.42 | 8 459.33 |
| 196 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 6 766.77 | 7 192.44 | 7 106.36 |
| 197 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 618.90 | 6 996.87 | 6 950.45 |
| 198 | javascript (14.16)| [sails](https://sailsjs.com) (1.4) | 6 133.36 | 6 155.43 | 6 069.66 |
| 199 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.3) | 5 926.85 | 6 271.07 | 6 261.06 |
| 200 | php (7.4)| [ice](https://iceframework.org) (1.5) | 5 877.98 | 6 101.76 | 6 112.00 |
| 201 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.6) | 5 505.64 | 5 792.21 | 5 827.41 |
| 202 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 5 037.03 | 5 255.45 | 5 278.48 |
| 203 | php (7.4)| [cubex](https://cubex.io) (4.16) | 4 488.34 | 4 593.47 | 4 582.58 |
| 204 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.6) | 4 325.36 | 4 464.13 | 4 471.28 |
| 205 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 313.54 | 4 439.23 | 4 476.82 |
| 206 | ruby (3.0)| [rails](https://rubyonrails.org) (6.1) | 3 954.88 | 3 933.94 | 3 916.30 |
| 207 | julia (1.6)| [merly](https://github.com/codeneomatrix/Merly.jl) (1.0) | 3 907.99 | 8 708.33 | 5 263.85 |
| 208 | php (7.4)| [lumen](https://lumen.laravel.com) (8.2) | 3 896.15 | 3 954.71 | 3 978.31 |
| 209 | php (7.4)| [nette](https://nette.org/en/) (3.1) | 3 880.84 | 3 965.69 | 3 953.61 |
| 210 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 3 742.49 | 3 830.17 | 3 818.62 |
| 211 | python (3.9)| [masonite](https://masoniteproject.com) (3.0) | 3 642.75 | 3 557.06 | 3 566.06 |
| 212 | php (7.4)| [symfony](https://symfony.com) (5.2) | 3 021.09 | 3 052.31 | 3 038.03 |
| 213 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.3) | 2 743.28 | 2 804.83 | 2 788.15 |
| 214 | python (3.9)| [cyclone](https://cyclone.io) (1.3) | 2 332.82 | 2 333.23 | 2 328.92 |
| 215 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 2 050.65 | 2 152.99 | 987.07 |
| 216 | python (3.9)| [django-ninja](https://django-ninja.rest-framework.com) (0.12) | 2 022.99 | 2 411.15 | 2 478.24 |
| 217 | r (4.0)| [restrserve](https://restrserve.org) (0.4) | 1 836.86 | 1 758.19 | 1 752.42 |
| 218 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 810.87 | 1 758.78 | 1 685.29 |
| 219 | python (3.9)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 757.45 | 1 685.17 | 1 699.55 |
| 220 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 736.57 | 1 767.20 | 1 768.44 |
| 221 | php (7.4)| [laravel](https://laravel.com) (8.38) | 1 501.13 | 1 515.37 | 1 516.46 |
| 222 | python (3.9)| [klein](https://github.com/twisted/klein) (20.6) | 1 321.71 | 1 346.57 | 1 334.38 |
| 223 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.1) | 1 210.84 | 1 230.40 | 1 231.24 |
| 224 | php (7.4)| [unic](https://unicframework.github.io/docs) (1.0) | 494.08 | 477.95 | 385.74 |
| 225 | r (4.0)| [plumber](https://rplumber.io) (1.1) | 420.94 | 446.00 | 431.76 |
| 226 | cpp (11)| [nawa](https://github.com/jatofg/nawa) (0.6) | 269.20 | NaN | NaN |
</a>

</details>

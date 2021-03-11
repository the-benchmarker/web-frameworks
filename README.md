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

## Results (2021-03-10)



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
| 1 | java (11)| [activej](https://activej.io) (4.0) | 172 615.83 | 210 093.20 | 213 370.57 |
| 2 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 148 394.68 | 178 247.00 | 182 907.60 |
| 3 | go (1.16)| [gearbox](https://gogearbox.com) (1.2) | 120 856.29 | 124 715.10 | 124 444.98 |
| 4 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 120 699.35 | 135 092.07 | 137 388.56 |
| 5 | go (1.16)| [fiber](https://gofiber.io) (2.5) | 118 637.26 | 130 929.40 | 129 984.10 |
| 6 | go (1.16)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.6) | 118 154.79 | 129 384.46 | 128 721.56 |
| 7 | go (1.16)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.22) | 116 265.63 | 130 110.08 | 130 592.86 |
| 8 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 116 194.12 | 145 329.58 | 147 989.01 |
| 9 | go (1.16)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 115 625.81 | 127 803.55 | 126 981.75 |
| 10 | go (1.16)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 115 561.35 | 128 400.35 | 128 276.72 |
| 11 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 113 450.86 | 140 434.14 | 143 666.37 |
| 12 | java (11)| [restheart](https://restheart.org) (5.3) | 113 428.06 | 118 059.17 | 118 685.67 |
| 13 | java (11)| [undertow](https://undertow.io) (2.2) | 111 808.83 | 136 680.68 | 138 517.60 |
| 14 | javascript (14.16)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.4) | 110 904.17 | 141 439.01 | 144 766.50 |
| 15 | clojure (1.1)| [donkey](https://github.com/AppsFlyer/donkey) (0.5) | 110 531.90 | 113 121.89 | 114 721.66 |
| 16 | java (11)| [jooby](https://jooby.io) (2.9) | 109 328.45 | 135 433.24 | 140 013.96 |
| 17 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 109 267.74 | 136 412.44 | 141 336.03 |
| 18 | javascript (14.16)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 108 321.20 | 136 833.12 | 140 589.11 |
| 19 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 108 020.50 | 134 621.23 | 137 157.81 |
| 20 | c (11)| [kore](https://kore.io) (3.3) | 107 625.48 | 188 888.06 | 195 637.01 |
| 21 | go (1.16)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 107 596.20 | 104 299.44 | 108 755.09 |
| 22 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 106 251.92 | 132 257.48 | 135 419.32 |
| 23 | java (11)| [vertx](https://vertx.io/docs/vertx-core/java/) (4.0) | 105 888.66 | 125 747.88 | 127 208.69 |
| 24 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 105 607.16 | 130 859.67 | 134 000.39 |
| 25 | java (11)| [vertx4web](https://vertx.io/docs/vertx-web/java/) (4.0) | 104 707.78 | 123 648.49 | 125 142.76 |
| 26 | php (7.4)| [swoole](https://github.com/swoole/swoole-src) (4.6) | 100 000.19 | 126 444.42 | 129 348.17 |
| 27 | crystal (0.36)| [toro](https://github.com/soveran/toro) (0.4) | 99 286.23 | 122 022.49 | 122 932.04 |
| 28 | crystal (0.36)| [spider-gazelle](https://spider-gazelle.net) (4.2) | 97 821.88 | 121 611.95 | 122 584.99 |
| 29 | crystal (0.36)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 97 349.18 | 121 704.66 | 122 588.08 |
| 30 | crystal (0.36)| [grip](https://github.com/grip-framework/grip) (4.0) | 95 265.56 | 114 144.78 | 115 243.57 |
| 31 | crystal (0.36)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 93 830.19 | 117 176.94 | 117 643.04 |
| 32 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 93 806.34 | 114 684.67 | 117 564.12 |
| 33 | crystal (0.36)| [kemal](https://kemalcr.com) (0.27) | 92 358.16 | 113 379.84 | 113 669.53 |
| 34 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 92 066.83 | 138 886.28 | 149 899.99 |
| 35 | crystal (0.36)| [amber](https://amberframework.org) (0.36) | 88 864.56 | 107 163.90 | 106 946.85 |
| 36 | php (7.4)| [swoole-coroutine](https://github.com/swoole/swoole-src) (4.6) | 88 634.21 | 126 751.97 | 136 898.80 |
| 37 | java (11)| [quarkus](https://quarkus.io) (1.12) | 85 982.57 | 104 242.70 | 106 879.76 |
| 38 | scala (2.13)| [akkahttp](https://akka.io) (10.2) | 83 945.38 | 99 461.76 | 96 442.36 |
| 39 | javascript (14.16)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 82 515.88 | 97 203.32 | 99 383.81 |
| 40 | go (1.16)| [gf](https://goframe.org) (1.15) | 82 106.24 | 89 839.30 | 91 903.56 |
| 41 | go (1.16)| [rte](https://github.com/jwilner/rte) (0.0) | 81 037.89 | 82 101.29 | 84 128.40 |
| 42 | go (1.16)| [clevergo](https://clevergo.tech) (0.5) | 80 630.52 | 82 066.64 | 84 145.44 |
| 43 | go (1.16)| [echo](https://echo.labstack.com) (4.2) | 80 030.89 | 81 240.58 | 83 022.87 |
| 44 | go (1.16)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 79 069.08 | 80 078.58 | 81 720.55 |
| 45 | go (1.16)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 683.05 | 83 283.34 | 84 376.31 |
| 46 | go (1.16)| [gin](https://gin-gonic.com) (1.6) | 78 473.85 | 82 439.31 | 83 952.73 |
| 47 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 78 388.93 | 106 438.09 | 115 672.24 |
| 48 | crystal (0.36)| [athena](https://github.com/athena-framework/athena) (0.13) | 77 242.94 | 85 704.61 | 80 282.22 |
| 49 | go (1.16)| [aero](https://github.com/aerogo/aero) (1.3) | 76 951.10 | 77 812.73 | 79 517.00 |
| 50 | go (1.16)| [violetear](https://violetear.org) (7.0) | 76 792.28 | 77 029.69 | 78 896.27 |
| 51 | go (1.16)| [chi](https://github.com/go-chi/chi) (1.5) | 76 259.90 | 75 319.90 | 77 664.51 |
| 52 | go (1.16)| [webgo](https://github.com/bnkamalesh/webgo) (5.0) | 75 638.76 | 75 340.18 | 77 368.03 |
| 53 | scala (2.13)| [finatra](https://twitter.github.io/finatra/) (21.1) | 75 067.74 | 86 985.68 | 87 397.14 |
| 54 | go (1.16)| [kami](https://github.com/guregu/kami) (2.2) | 74 894.24 | 78 977.55 | 79 489.49 |
| 55 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 73 970.00 | 108 118.68 | 135 103.30 |
| 56 | go (1.16)| [goroute](https://goroute.github.io) (0.0) | 73 017.82 | 71 735.32 | 74 250.64 |
| 57 | python (3.9)| [falcon](https://falconframework.org) (2.0) | 72 864.58 | 79 354.57 | 82 114.44 |
| 58 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 72 322.32 | 83 742.67 | 86 312.30 |
| 59 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 72 226.47 | 83 488.43 | 85 683.29 |
| 60 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.4) | 71 974.65 | 67 725.45 | 56 628.00 |
| 61 | go (1.16)| [beego](https://beego.me) (1.12) | 71 722.82 | 74 442.08 | 76 323.93 |
| 62 | fsharp (5.0)| [falco](https://falcoframework.com) (3.0) | 71 012.69 | 82 280.68 | 84 534.14 |
| 63 | go (1.16)| [goyave](https://github.com/go-goyave/goyave) (3.7) | 70 567.34 | 70 935.30 | 73 273.03 |
| 64 | javascript (14.16)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 69 824.03 | 79 331.73 | 79 008.06 |
| 65 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 64 664.71 | 72 034.66 | 72 552.32 |
| 66 | go (1.16)| [air](https://github.com/aofei/air) (0.21) | 64 575.18 | 63 422.27 | 65 710.70 |
| 67 | javascript (14.16)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 63 144.60 | 68 854.22 | 70 406.40 |
| 68 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 61 754.03 | 67 526.09 | 67 172.70 |
| 69 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 61 490.33 | 64 433.00 | 65 402.11 |
| 70 | scala (2.13)| [finch](https://finagle.github.io/finch/) (0.32) | 60 517.38 | 67 449.16 | 67 355.56 |
| 71 | javascript (14.16)| [0http](https://github.com/jkyberneees/0http) (3.0) | 60 318.65 | 65 881.01 | 67 589.79 |
| 72 | javascript (14.16)| [restana](https://github.com/jkyberneees/ana) (4.7) | 56 512.78 | 63 097.86 | 64 146.78 |
| 73 | swift (5.3)| [hummingbird](https://github.com/hummingbird-project/hummingbird) (0.6) | 55 155.94 | 60 788.28 | 61 252.53 |
| 74 | javascript (14.16)| [polka](https://github.com/lukeed/polka) (0.5) | 55 049.66 | 60 631.79 | 60 593.89 |
| 75 | javascript (14.16)| [rayo](https://rayo.js.org) (1.3) | 54 981.93 | 60 321.85 | 59 211.32 |
| 76 | javascript (14.16)| [fastify](https://fastify.io) (3.13) | 54 070.95 | 58 949.51 | 57 505.84 |
| 77 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 53 912.54 | 56 941.83 | 55 192.22 |
| 78 | rust (1.50)| [salvo](https://github.com/kenorld/salvo) (0.7) | 53 390.16 | 57 393.51 | 58 336.83 |
| 79 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 52 712.17 | 60 911.10 | 63 584.66 |
| 80 | python (3.9)| [pyramid](https://trypyramid.com) (2.0) | 52 673.06 | 56 042.85 | 56 149.52 |
| 81 | javascript (14.16)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 52 457.15 | 58 497.04 | 56 770.93 |
| 82 | ruby (3.0)| [agoo](https://github.com/ohler55/agoo) (2.14) | 52 065.38 | 72 327.25 | 80 260.12 |
| 83 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 51 075.21 | 56 550.10 | 63 349.64 |
| 84 | rust (1.50)| [actix](https://actix.rs) (3.3) | 50 638.98 | 46 079.50 | 46 680.35 |
| 85 | java (11)| [javalin](https://javalin.io) (3.9) | 50 604.27 | 54 378.81 | 54 551.77 |
| 86 | php (7.4)| [hyperf](https://hyperf.io) (2.1) | 50 385.35 | 59 208.49 | 66 133.45 |
| 87 | java (11)| [spark](https://sparkjava.com) (2.9) | 49 917.15 | 54 430.41 | 55 578.35 |
| 88 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 49 888.27 | 65 788.34 | 69 121.16 |
| 89 | java (11)| [micronaut](https://micronaut.io) (1.2) | 49 600.93 | 57 301.27 | 57 388.63 |
| 90 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 49 285.18 | 55 371.48 | 55 755.19 |
| 91 | php (7.4)| [one](https://github.com/lizhichao/one) (2.2) | 49 108.96 | 58 353.40 | 68 418.68 |
| 92 | javascript (14.16)| [nestjs-fastify](https://nestjs.com) (7.6) | 48 121.36 | 51 756.43 | 50 889.28 |
| 93 | go (1.16)| [mars](https://github.com/roblillack/mars) (1.0) | 46 356.82 | 46 499.76 | 49 700.18 |
| 94 | javascript (14.16)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 45 841.59 | 49 207.79 | 47 472.00 |
| 95 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 45 783.23 | 48 850.15 | 51 568.08 |
| 96 | javascript (14.16)| [foxify](https://foxify.js.org) (0.1) | 45 541.44 | 49 029.19 | 47 719.27 |
| 97 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 565.76 | 45 402.54 | 45 972.58 |
| 98 | python (3.9)| [bottle](https://bottlepy.org) (0.12) | 43 046.73 | 46 189.60 | 46 724.84 |
| 99 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 42 927.18 | 32 717.89 | 31 761.34 |
| 100 | javascript (14.16)| [koa](https://koajs.com) (2.13) | 40 785.74 | 43 590.14 | 42 527.52 |
| 101 | python (3.9)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 40 207.81 | 44 835.57 | 45 756.63 |
| 102 | dart (2.12)| [aqueduct](https://aqueduct.io) (3.3) | 39 102.45 | 38 628.36 | 37 860.74 |
| 103 | javascript (14.16)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (3.0) | 38 583.00 | 40 398.74 | 59 601.65 |
| 104 | swift (5.3)| [vapor](https://vapor.codes) (4.41) | 36 603.27 | 38 225.01 | 37 927.82 |
| 105 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 36 151.35 | 37 457.03 | 37 690.88 |
| 106 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 35 145.42 | 36 889.96 | 37 644.80 |
| 107 | javascript (14.16)| [hapi](https://hapijs.com) (20.1) | 34 808.24 | 35 817.30 | 34 587.60 |
| 108 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 34 775.08 | 34 561.31 | 34 843.25 |
| 109 | python (3.9)| [emmett](https://emmett.sh) (2.2) | 34 332.57 | 41 184.28 | 41 803.58 |
| 110 | javascript (14.16)| [moleculer](https://moleculer.services) (0.14) | 34 329.10 | 35 495.83 | 34 736.27 |
| 111 | java (11)| [jersey3-grizzly2](https://eclipse-ee4j.github.io/jersey) (3) | 34 159.27 | 38 959.40 | 38 396.14 |
| 112 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 33 842.27 | 42 593.80 | 45 664.77 |
| 113 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 33 801.33 | 34 110.51 | 34 184.91 |
| 114 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 33 263.10 | 34 162.92 | 33 176.95 |
| 115 | python (3.9)| [hug](https://hug.rest) (2.6) | 33 222.36 | 35 036.61 | 53 664.67 |
| 116 | java (11)| [jersey-grizzly2](https://eclipse-ee4j.github.io/jersey) (2.33) | 33 164.75 | 37 840.04 | 37 194.93 |
| 117 | javascript (14.16)| [fyrejet](https://github.com/fyrejet/fyrejet) (3.0) | 32 933.92 | 36 157.17 | 35 732.22 |
| 118 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 32 413.10 | 37 010.94 | 37 284.19 |
| 119 | python (3.9)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 32 055.96 | 38 856.92 | 39 073.19 |
| 120 | python (3.9)| [sanic](https://github.com/huge-success/sanic) (20.12) | 31 354.17 | 34 731.04 | 35 082.92 |
| 121 | rust (1.50)| [nickel](https://nickel-org.github.io) (0.11) | 31 314.74 | 32 681.24 | 29 513.18 |
| 122 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 30 991.76 | 29 116.01 | 25 096.03 |
| 123 | python (3.9)| [blacksheep](https://github.com/Neoteroi/BlackSheep) (1.0) | 29 859.47 | 33 774.70 | 34 277.41 |
| 124 | javascript (14.16)| [restify](https://restify.com) (8.5) | 29 290.39 | 30 595.06 | 29 384.85 |
| 125 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 28 910.45 | 32 028.60 | 31 711.60 |
| 126 | php (7.4)| [imi](https://imiphp.com) (1.2) | 28 331.17 | 32 628.83 | 33 119.56 |
| 127 | scala (2.13)| [play](https://playframework.com) (2.8) | 28 001.73 | 29 569.49 | 29 583.94 |
| 128 | python (3.9)| [starlette](https://starlette.io) (0.14) | 27 688.92 | 31 407.21 | 32 372.93 |
| 129 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 27 549.77 | 30 987.08 | 32 553.98 |
| 130 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 27 289.95 | 28 970.08 | 28 544.30 |
| 131 | rust (1.50)| [gotham](https://gotham.rs) (0.5) | 27 031.85 | 29 956.94 | 30 866.51 |
| 132 | php (7.4)| [comet](https://github.com/gotzmann/comet) (1.2) | 27 024.35 | 27 396.46 | 26 688.38 |
| 133 | python (3.9)| [index.py](https://index-py.abersheeran.com) (0.16) | 25 562.99 | 29 841.93 | 30 521.28 |
| 134 | crystal (0.36)| [orion](https://github.com/obsidian/orion) (3.0) | 24 817.66 | 24 318.49 | 23 729.17 |
| 135 | python (3.9)| [responder](https://python-responder.org) (2.0) | 24 664.17 | 32 276.67 | 31 304.23 |
| 136 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 24 293.63 | 26 176.67 | 25 707.30 |
| 137 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 22 693.19 | 21 496.76 | 20 670.71 |
| 138 | clojure (1.1)| [luminus](https://luminusweb.com) (3.98) | 22 402.68 | 21 784.39 | 21 221.20 |
| 139 | crystal (0.36)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.17) | 21 842.36 | 21 919.97 | 19 515.05 |
| 140 | python (3.9)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 21 659.43 | 23 971.41 | 26 191.87 |
| 141 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 21 632.95 | 22 087.12 | 22 147.73 |
| 142 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 20 471.17 | 20 089.25 | 20 366.55 |
| 143 | python (3.9)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 20 075.70 | 23 468.60 | 23 968.03 |
| 144 | python (3.9)| [molten](https://moltenframework.com) (1.0) | 19 094.91 | 22 463.92 | 22 262.17 |
| 145 | javascript (14.16)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 18 265.29 | 17 419.52 | 16 405.56 |
| 146 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 311.37 | 15 510.42 | 14 446.49 |
| 147 | php (7.4)| [swoft](https://swoft.org) (2.0) | 16 918.30 | 20 585.66 | 20 866.38 |
| 148 | ruby (3.0)| [hanami-api](https://hanamirb.org) (0.1) | 16 761.31 | 16 363.60 | 15 942.00 |
| 149 | python (3.9)| [fastapi](https://fastapi.tiangolo.com) (0.63) | 16 583.26 | 21 290.63 | 21 603.38 |
| 150 | rust (1.50)| [iron](https://iron/iron) (0.6) | 16 354.98 | 16 627.39 | 16 444.44 |
| 151 | ruby (3.0)| [syro](https://github.com/soveran/syro) (3.2) | 16 224.16 | 15 699.92 | 15 445.28 |
| 152 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 15 847.56 | 14 059.56 | 13 117.77 |
| 153 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 15 636.41 | 18 260.07 | 18 663.17 |
| 154 | javascript (14.16)| [express](https://expressjs.com) (4.17) | 15 523.01 | 17 772.31 | 17 631.75 |
| 155 | ruby (3.0)| [roda](https://roda.jeremyevans.net) (3.41) | 15 348.29 | 14 846.78 | 14 685.63 |
| 156 | javascript (14.16)| [feathersjs](https://feathersjs.com) (4.5) | 15 309.79 | 17 599.50 | 17 394.37 |
| 157 | go (1.16)| [macaron](https://go-macaron.com) (1.4) | 15 284.18 | 16 760.26 | 16 808.74 |
| 158 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 213.83 | 17 605.58 | 18 070.07 |
| 159 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 15 024.46 | 17 383.06 | 17 747.33 |
| 160 | ruby (3.0)| [cuba](https://cuba.is) (3.9) | 14 080.54 | 13 633.38 | 13 470.67 |
| 161 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 13 932.52 | 14 754.55 | 20 017.57 |
| 162 | java (11)| [struts2](https://struts.apache.org) (2.5) | 13 766.97 | 13 995.08 | 14 035.10 |
| 163 | ruby (3.0)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 13 688.22 | 13 394.74 | 13 080.59 |
| 164 | java (11)| [blade](https://lets-blade.com) (2.0) | 13 261.73 | 15 554.41 | 14 505.27 |
| 165 | ruby (3.0)| [camping](https://github.com/camping/camping) (2.1) | 12 965.21 | 12 490.18 | 12 313.03 |
| 166 | ruby (3.0)| [rack_app](https://rack-app.com) (7.7) | 12 770.28 | 12 092.40 | 11 863.75 |
| 167 | python (3.9)| [flask](https://flask.pocoo.org) (1.1) | 12 686.98 | 14 396.86 | 13 089.87 |
| 168 | javascript (14.16)| [nestjs-express](https://nestjs.com) (7.6) | 11 547.29 | 11 488.63 | 14 090.38 |
| 169 | go (1.16)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 384.95 | 11 698.63 | 11 715.71 |
| 170 | dart (2.12)| [start](https://github.com/lvivski/start) (0.4) | 11 141.84 | 11 013.98 | 10 672.81 |
| 171 | python (3.9)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 10 263.32 | 9 929.62 | 9 642.36 |
| 172 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 10 073.35 | 10 306.44 | 10 416.24 |
| 173 | go (1.16)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 926.91 | 10 122.75 | 10 129.42 |
| 174 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.6) | 9 813.66 | 9 968.96 | 10 182.33 |
| 175 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 9 717.94 | 9 914.27 | 10 017.71 |
| 176 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.7) | 9 697.13 | 9 842.62 | 9 914.25 |
| 177 | php (7.4)| [laravel-s-lumen](https://github.com/hhxsv5/laravel-s) (3.7) | 9 594.35 | 9 934.55 | 9 724.64 |
| 178 | python (3.9)| [guillotina](https://guillotina.io) (6.2) | 9 076.78 | 9 398.42 | 8 894.83 |
| 179 | ruby (3.0)| [sinatra](https://sinatrarb.com) (2.1) | 8 598.81 | 8 348.59 | 8 333.45 |
| 180 | pony (0.39)| [jennet](https://github.com/Theodus/jennet) (0.1) | 8 363.85 | 13 365.85 | 12 638.68 |
| 181 | ruby (3.0)| [grape](https://ruby-grape.org) (1.5) | 7 592.63 | 7 380.46 | 7 356.21 |
| 182 | python (3.9)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 7 576.66 | 7 262.92 | 6 832.35 |
| 183 | python (3.9)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 395.60 | 6 948.86 | 6 348.29 |
| 184 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 7 182.67 | 7 485.89 | 7 481.62 |
| 185 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 7 095.68 | 7 529.16 | 7 393.25 |
| 186 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 7 038.85 | 7 234.47 | 7 239.83 |
| 187 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 790.06 | 7 168.45 | 7 078.47 |
| 188 | php (7.4)| [laravel-s-laravel](https://github.com/hhxsv5/laravel-s) (3.7) | 6 403.63 | 6 409.97 | 6 322.56 |
| 189 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 384.64 | 6 725.16 | 6 704.88 |
| 190 | python (3.9)| [django](https://djangoproject.com) (3.1) | 6 198.90 | 5 835.09 | 5 746.99 |
| 191 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 5 991.23 | 6 764.83 | 6 513.46 |
| 192 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 5 851.40 | 5 948.96 | 5 855.13 |
| 193 | python (3.9)| [tornado](https://tornadoweb.org) (6.1) | 5 795.89 | 5 966.89 | 5 623.56 |
| 194 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.2) | 5 089.07 | 5 264.94 | 5 253.85 |
| 195 | php (7.4)| [fatfree](https://fatfreeframework.com) (3.7) | 4 957.20 | 5 174.99 | 5 084.01 |
| 196 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 734.15 | 4 909.87 | 4 875.59 |
| 197 | php (7.4)| [cubex](https://cubex.io) (4.16) | 4 678.96 | 4 841.44 | 4 821.19 |
| 198 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 377.63 | 4 530.61 | 4 528.42 |
| 199 | javascript (14.16)| [sails](https://sailsjs.com) (1.4) | 4 214.82 | 4 214.02 | 4 153.02 |
| 200 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.3) | 4 097.65 | 4 279.74 | 4 259.70 |
| 201 | julia (1.6)| [merly](https://github.com/codeneomatrix/Merly.jl) (1.0) | 3 943.97 | 6 505.08 | 4 159.69 |
| 202 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.6) | 3 881.29 | 3 955.50 | 3 941.23 |
| 203 | php (7.4)| [nette](https://nette.org/en/) (3.1) | 3 839.37 | 3 943.39 | 3 951.88 |
| 204 | php (7.4)| [lumen](https://lumen.laravel.com) (8.2) | 3 833.71 | 3 889.30 | 3 904.63 |
| 205 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 3 460.20 | 3 570.98 | 3 561.05 |
| 206 | php (7.4)| [symfony](https://symfony.com) (5.2) | 3 031.12 | 3 046.35 | 3 057.91 |
| 207 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.6) | 2 970.31 | 3 017.39 | 3 019.20 |
| 208 | ruby (3.0)| [rails](https://rubyonrails.org) (6.1) | 2 780.08 | 2 754.07 | 2 769.29 |
| 209 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 513.95 | 2 550.08 | 2 561.24 |
| 210 | python (3.9)| [masonite](https://masoniteproject.com) (3.0) | 2 438.59 | 2 478.19 | 2 476.38 |
| 211 | php (7.4)| [antidot](https://github.com/antidot-framework/react-framework) (0.2) | 2 099.46 | 696.97 | 1 293.71 |
| 212 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 918.37 | 1 870.43 | 1 869.27 |
| 213 | php (7.4)| [driftphp](https://github.com/driftphp/driftphp) (0.1) | 1 914.63 | 661.69 | 1 584.98 |
| 214 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 911.61 | 1 857.24 | 1 783.75 |
| 215 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 720.48 | 1 734.95 | 1 733.42 |
| 216 | python (3.9)| [cyclone](https://cyclone.io) (1.3) | 1 592.82 | 1 588.07 | 1 564.75 |
| 217 | python (3.9)| [klein](https://github.com/twisted/klein) (20.6) | 1 491.97 | 1 520.48 | 1 500.71 |
| 218 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 370.14 | 661.87 | 389.66 |
| 219 | python (3.9)| [django-ninja](https://django-ninja.rest-framework.com) (0.11) | 1 258.54 | 1 631.94 | 1 677.37 |
| 220 | python (3.9)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 214.75 | 1 168.97 | 1 181.42 |
| 221 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.1) | 1 190.52 | 1 206.06 | 1 204.94 |
| 222 | php (7.4)| [laravel](https://laravel.com) (8.32) | 993.48 | 994.32 | 988.17 |
| 223 | php (7.4)| [unic](https://unicframework.github.io/docs) (1.0) | 549.41 | 498.23 | 159.59 |
| 224 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 285.03 | 302.85 | -94.91 |
| 225 | cpp (11)| [nawa](https://github.com/jatofg/nawa) (0.6) | 270.49 | NaN | NaN |
</a>

</details>

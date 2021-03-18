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

## Results (2021-03-18)



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
| 1 | java (11)| [activej](https://activej.io) (4.0) | 173 306.29 | 212 181.60 | 215 351.10 |
| 2 | php (7.4)| [webman](https://github.com/walkor/webman) (1.0) | 157 181.84 | 168 860.46 | 171 152.65 |
| 3 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 155 714.38 | 180 467.59 | 185 373.99 |
| 4 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 124 278.32 | 134 868.98 | 137 298.25 |
| 5 | go (1.16)| [fiber](https://gofiber.io) (2.6) | 121 501.16 | 131 367.52 | 131 045.12 |
| 6 | go (1.16)| [gearbox](https://gogearbox.com) (1.2) | 120 973.05 | 125 950.45 | 125 341.88 |
| 7 | go (1.16)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.22) | 117 942.13 | 132 208.41 | 132 416.17 |
| 8 | go (1.16)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.6) | 117 723.97 | 128 926.96 | 128 571.21 |
| 9 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 117 332.22 | 145 947.09 | 148 507.06 |
| 10 | go (1.16)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 117 094.45 | 129 190.26 | 128 948.95 |
| 11 | go (1.16)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 116 316.40 | 129 538.02 | 128 822.76 |
| 12 | java (11)| [restheart](https://restheart.org) (5.3) | 113 965.21 | 118 356.51 | 119 452.86 |
| 13 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 112 902.70 | 140 108.46 | 143 699.81 |
| 14 | java (11)| [undertow](https://undertow.io) (2.2) | 112 484.21 | 137 287.25 | 137 135.04 |
| 15 | javascript (14.16)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.4) | 109 812.74 | 142 093.77 | 145 350.39 |
| 16 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 109 648.56 | 136 009.76 | 140 481.47 |
| 17 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 109 471.51 | 135 167.80 | 137 483.59 |
| 18 | java (11)| [jooby](https://jooby.io) (2.9) | 108 824.86 | 137 913.13 | 142 715.92 |
| 19 | clojure (1.1)| [donkey](https://github.com/AppsFlyer/donkey) (0.5) | 108 598.83 | 111 955.81 | 113 297.34 |
| 20 | go (1.16)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 108 403.54 | 105 267.13 | 109 338.01 |
| 21 | javascript (14.16)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 108 239.85 | 138 434.14 | 141 016.70 |
| 22 | c (11)| [kore](https://kore.io) (3.3) | 107 247.18 | 193 174.59 | 184 923.84 |
| 23 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 106 623.24 | 133 473.92 | 137 658.61 |
| 24 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 106 496.91 | 131 139.88 | 134 295.14 |
| 25 | java (11)| [vertx](https://vertx.io/docs/vertx-core/java/) (4.0) | 105 970.72 | 127 223.35 | 128 295.96 |
| 26 | java (11)| [vertx4web](https://vertx.io/docs/vertx-web/java/) (4.0) | 104 889.32 | 123 049.87 | 124 805.60 |
| 27 | crystal (0.36)| [toro](https://github.com/soveran/toro) (0.4) | 99 018.87 | 121 752.11 | 121 893.38 |
| 28 | crystal (0.36)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 98 929.19 | 120 875.67 | 122 478.21 |
| 29 | php (7.4)| [swoole](https://github.com/swoole/swoole-src) (4.6) | 97 440.22 | 123 414.50 | 133 038.97 |
| 30 | crystal (0.36)| [spider-gazelle](https://spider-gazelle.net) (4.2) | 97 417.89 | 120 737.65 | 121 608.91 |
| 31 | crystal (0.36)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 95 847.69 | 115 380.29 | 116 222.82 |
| 32 | crystal (0.36)| [grip](https://github.com/grip-framework/grip) (4.0) | 95 843.54 | 113 890.35 | 113 303.83 |
| 33 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 95 552.81 | 139 016.55 | 149 745.50 |
| 34 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 94 543.22 | 117 195.09 | 119 905.98 |
| 35 | crystal (0.36)| [kemal](https://kemalcr.com) (0.27) | 94 127.63 | 112 984.28 | 113 387.67 |
| 36 | php (7.4)| [swoole-coroutine](https://github.com/swoole/swoole-src) (4.6) | 89 642.39 | 126 673.37 | 138 143.90 |
| 37 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 88 861.92 | 129 892.78 | 131 114.36 |
| 38 | java (11)| [quarkus](https://quarkus.io) (1.12) | 86 874.66 | 105 062.72 | 107 924.81 |
| 39 | javascript (14.16)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 83 085.03 | 97 733.07 | 98 836.37 |
| 40 | go (1.16)| [gf](https://goframe.org) (1.15) | 82 156.27 | 89 852.86 | 92 142.08 |
| 41 | go (1.16)| [rte](https://github.com/jwilner/rte) (0.0) | 81 344.80 | 82 708.80 | 84 724.24 |
| 42 | go (1.16)| [clevergo](https://clevergo.tech) (0.5) | 81 145.77 | 82 384.69 | 84 279.89 |
| 43 | go (1.16)| [echo](https://echo.labstack.com) (4.2) | 80 520.09 | 81 806.81 | 83 939.80 |
| 44 | go (1.16)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 80 117.49 | 81 120.30 | 83 205.51 |
| 45 | scala (2.13)| [akkahttp](https://akka.io) (10.2) | 79 481.76 | 94 057.85 | 90 308.90 |
| 46 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 79 439.40 | 107 498.93 | 106 639.52 |
| 47 | go (1.16)| [gin](https://gin-gonic.com) (1.6) | 79 097.33 | 83 084.60 | 84 336.28 |
| 48 | go (1.16)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 79 045.01 | 83 560.64 | 84 962.28 |
| 49 | go (1.16)| [violetear](https://violetear.org) (7.0) | 77 415.66 | 77 739.65 | 79 797.49 |
| 50 | go (1.16)| [chi](https://github.com/go-chi/chi) (1.5) | 76 984.92 | 76 412.33 | 78 735.33 |
| 51 | go (1.16)| [aero](https://github.com/aerogo/aero) (1.3) | 76 796.35 | 77 502.52 | 79 686.29 |
| 52 | go (1.16)| [webgo](https://github.com/bnkamalesh/webgo) (5.0) | 76 211.48 | 75 874.60 | 78 197.96 |
| 53 | fsharp (5.0)| [falco](https://falcoframework.com) (3.0) | 74 836.64 | 87 462.29 | 90 487.72 |
| 54 | scala (2.13)| [finatra](https://twitter.github.io/finatra/) (21.1) | 74 801.04 | 87 420.72 | 87 921.89 |
| 55 | go (1.16)| [goroute](https://goroute.github.io) (0.0) | 73 905.05 | 72 623.42 | 75 412.09 |
| 56 | python (3.9)| [falcon](https://falconframework.org) (2.0) | 72 872.80 | 81 445.90 | 82 867.82 |
| 57 | javascript (14.16)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 72 855.08 | 79 022.05 | 79 907.86 |
| 58 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 72 806.34 | 84 066.39 | 86 529.10 |
| 59 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 72 357.94 | 83 534.75 | 85 932.72 |
| 60 | go (1.16)| [beego](https://beego.me) (1.12) | 71 915.40 | 74 881.65 | 76 988.51 |
| 61 | go (1.16)| [goyave](https://github.com/go-goyave/goyave) (3.7) | 70 785.01 | 71 132.65 | 73 313.91 |
| 62 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.4) | 70 238.37 | 66 718.91 | 63 291.78 |
| 63 | go (1.16)| [air](https://github.com/aofei/air) (0.21) | 65 209.13 | 63 695.42 | 66 339.65 |
| 64 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 65 190.45 | 73 028.32 | 73 617.58 |
| 65 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 62 428.05 | 68 747.21 | 68 392.96 |
| 66 | scala (2.13)| [finch](https://finagle.github.io/finch/) (0.32) | 61 672.62 | 68 998.82 | 68 796.15 |
| 67 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 60 428.73 | 64 336.41 | 64 841.76 |
| 68 | javascript (14.16)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 60 337.58 | 68 332.86 | 70 537.27 |
| 69 | javascript (14.16)| [0http](https://github.com/jkyberneees/0http) (3.0) | 58 964.29 | 66 017.43 | 67 392.40 |
| 70 | rust (1.50)| [salvo](https://github.com/salvo-rs/salvo) (0.8) | 57 900.45 | 63 313.48 | 63 334.30 |
| 71 | javascript (14.16)| [polka](https://github.com/lukeed/polka) (0.5) | 57 709.69 | 62 518.94 | 60 860.14 |
| 72 | javascript (14.16)| [restana](https://github.com/jkyberneees/ana) (4.7) | 56 034.59 | 63 318.29 | 64 431.70 |
| 73 | javascript (14.16)| [rayo](https://rayo.js.org) (1.3) | 55 966.65 | 60 476.91 | 59 536.99 |
| 74 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 54 367.23 | 57 737.82 | 55 929.13 |
| 75 | javascript (14.16)| [fastify](https://fastify.io) (3.14) | 53 921.41 | 58 638.89 | 57 270.94 |
| 76 | javascript (14.16)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 53 432.42 | 59 329.75 | 57 390.07 |
| 77 | swift (5.3)| [hummingbird](https://github.com/hummingbird-project/hummingbird) (0.7) | 53 347.60 | 60 267.01 | 60 539.26 |
| 78 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 52 981.29 | 61 658.22 | 63 541.76 |
| 79 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 51 874.05 | 57 245.94 | 64 322.91 |
| 80 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 51 764.65 | 67 048.36 | 69 678.50 |
| 81 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 50 915.13 | 57 604.62 | 57 595.22 |
| 82 | ruby (3.0)| [agoo](https://github.com/ohler55/agoo) (2.14) | 50 817.93 | 72 092.43 | 80 404.07 |
| 83 | php (7.4)| [one](https://github.com/lizhichao/one) (2.2) | 50 350.30 | 60 548.06 | 67 999.87 |
| 84 | java (11)| [javalin](https://javalin.io) (3.9) | 50 150.13 | 54 270.52 | 54 786.94 |
| 85 | java (11)| [spark](https://sparkjava.com) (2.9) | 49 824.06 | 54 288.09 | 55 350.90 |
| 86 | php (7.4)| [hyperf](https://hyperf.io) (2.1) | 49 776.73 | 60 518.44 | 66 279.30 |
| 87 | java (11)| [micronaut](https://micronaut.io) (1.2) | 49 064.49 | 56 074.06 | 56 417.51 |
| 88 | python (3.9)| [pyramid](https://trypyramid.com) (2.0) | 48 671.66 | 52 906.19 | 52 859.78 |
| 89 | javascript (14.16)| [nestjs-fastify](https://nestjs.com) (7.6) | 47 552.39 | 51 596.75 | 49 896.05 |
| 90 | go (1.16)| [mars](https://github.com/roblillack/mars) (1.0) | 46 962.63 | 46 900.88 | 50 262.72 |
| 91 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 45 809.03 | 49 449.75 | 51 061.70 |
| 92 | javascript (14.16)| [foxify](https://foxify.js.org) (0.1) | 45 429.84 | 49 136.36 | 47 655.82 |
| 93 | rust (1.50)| [actix](https://actix.rs) (3.3) | 44 854.55 | 45 685.66 | 46 940.23 |
| 94 | javascript (14.16)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 44 701.93 | 47 802.82 | 46 889.99 |
| 95 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 284.41 | 45 360.90 | 45 314.63 |
| 96 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 43 230.45 | 32 953.38 | 30 568.38 |
| 97 | javascript (14.16)| [koa](https://koajs.com) (2.13) | 42 198.12 | 43 984.45 | 43 277.49 |
| 98 | python (3.9)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 42 096.73 | 51 062.75 | 52 385.41 |
| 99 | dart (2.12)| [aqueduct](https://aqueduct.io) (3.3) | 39 499.39 | 39 136.77 | 38 966.43 |
| 100 | python (3.9)| [bottle](https://bottlepy.org) (0.12) | 39 138.46 | 43 723.98 | 44 320.60 |
| 101 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 36 521.67 | 37 374.43 | 37 058.58 |
| 102 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 36 209.23 | 35 914.73 | 35 617.36 |
| 103 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 36 014.67 | 34 901.37 | 34 940.39 |
| 104 | swift (5.3)| [vapor](https://vapor.codes) (4.41) | 35 841.37 | 37 976.33 | 37 850.35 |
| 105 | python (3.9)| [emmett](https://emmett.sh) (2.2) | 35 418.81 | 41 163.35 | 41 925.30 |
| 106 | javascript (14.16)| [moleculer](https://moleculer.services) (0.14) | 35 197.89 | 35 842.00 | 35 469.00 |
| 107 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 35 114.12 | 36 717.07 | 37 464.02 |
| 108 | java (11)| [jersey3-grizzly2](https://eclipse-ee4j.github.io/jersey) (3) | 34 177.43 | 39 321.81 | 38 877.28 |
| 109 | javascript (14.16)| [fyrejet](https://github.com/fyrejet/fyrejet) (3.1) | 34 143.54 | 36 182.98 | 36 064.48 |
| 110 | javascript (14.16)| [hapi](https://hapijs.com) (20.1) | 33 946.95 | 35 046.98 | 33 989.30 |
| 111 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 33 828.04 | 34 836.20 | 33 338.10 |
| 112 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 33 733.64 | 44 557.63 | 46 738.28 |
| 113 | python (3.9)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 33 411.75 | 38 026.71 | 39 192.58 |
| 114 | python (3.9)| [hug](https://hug.rest) (2.6) | 33 374.88 | 35 664.10 | 53 621.02 |
| 115 | java (11)| [jersey-grizzly2](https://eclipse-ee4j.github.io/jersey) (2.33) | 33 130.62 | 37 736.48 | 37 166.88 |
| 116 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 32 395.75 | 37 707.59 | 37 523.21 |
| 117 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 31 388.61 | 27 028.19 | 24 194.76 |
| 118 | python (3.9)| [sanic](https://github.com/huge-success/sanic) (20.12) | 30 115.38 | 34 067.10 | 34 737.72 |
| 119 | rust (1.50)| [nickel](https://nickel-org.github.io) (0.11) | 29 621.40 | 30 385.61 | 31 136.52 |
| 120 | python (3.9)| [blacksheep](https://github.com/Neoteroi/BlackSheep) (1.0) | 29 245.42 | 33 726.84 | 34 539.53 |
| 121 | javascript (14.16)| [restify](https://restify.com) (8.5) | 29 215.01 | 30 477.59 | 29 536.56 |
| 122 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 29 130.47 | 32 184.80 | 32 006.00 |
| 123 | php (7.4)| [imi](https://imiphp.com) (1.2) | 28 385.53 | 32 278.18 | 33 056.74 |
| 124 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 28 136.19 | 31 413.81 | 32 936.67 |
| 125 | javascript (14.16)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (3.1) | 27 972.63 | 29 621.07 | 30 028.63 |
| 126 | scala (2.13)| [play](https://playframework.com) (2.8) | 27 887.38 | 29 594.97 | 29 590.10 |
| 127 | python (3.9)| [starlette](https://starlette.io) (0.14) | 27 863.56 | 31 300.00 | 31 952.06 |
| 128 | python (3.9)| [responder](https://python-responder.org) (2.0) | 27 775.31 | 30 903.83 | 32 202.57 |
| 129 | php (7.4)| [comet](https://github.com/gotzmann/comet) (1.2) | 27 274.51 | 27 542.52 | 26 925.06 |
| 130 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 26 969.89 | 29 037.62 | 28 493.77 |
| 131 | rust (1.50)| [gotham](https://gotham.rs) (0.5) | 25 906.70 | 29 965.20 | 30 870.05 |
| 132 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 24 114.32 | 25 058.58 | 25 490.12 |
| 133 | python (3.9)| [index.py](https://index-py.abersheeran.com) (0.16) | 23 877.35 | 29 666.50 | 30 696.14 |
| 134 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 23 424.17 | 21 643.13 | 20 758.14 |
| 135 | clojure (1.1)| [luminus](https://luminusweb.com) (3.98) | 22 572.16 | 21 887.03 | 21 201.17 |
| 136 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 22 474.01 | 22 130.64 | 21 805.10 |
| 137 | python (3.9)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 22 070.41 | 27 385.68 | 27 277.31 |
| 138 | crystal (0.36)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.17) | 21 474.52 | 21 402.25 | 18 851.69 |
| 139 | python (3.9)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 20 911.96 | 23 269.91 | 23 906.36 |
| 140 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 20 196.89 | 19 610.12 | 20 255.17 |
| 141 | python (3.9)| [molten](https://moltenframework.com) (1.0) | 19 623.08 | 22 417.24 | 22 240.61 |
| 142 | javascript (14.16)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 18 707.08 | 17 595.80 | 16 779.42 |
| 143 | rust (1.50)| [iron](https://iron/iron) (0.6) | 17 436.43 | 16 714.33 | 16 608.76 |
| 144 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 303.97 | 15 650.81 | 14 525.14 |
| 145 | python (3.9)| [fastapi](https://fastapi.tiangolo.com) (0.63) | 17 062.45 | 22 582.80 | 21 854.12 |
| 146 | php (7.4)| [swoft](https://swoft.org) (2.0) | 16 868.14 | 20 599.93 | 21 261.06 |
| 147 | ruby (3.0)| [hanami-api](https://hanamirb.org) (0.1) | 16 707.11 | 16 384.77 | 15 876.44 |
| 148 | ruby (3.0)| [syro](https://github.com/soveran/syro) (3.2) | 16 201.52 | 15 773.27 | 15 448.58 |
| 149 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 15 790.67 | 13 931.65 | 12 987.15 |
| 150 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 15 604.68 | 18 086.06 | 18 788.27 |
| 151 | javascript (14.16)| [express](https://expressjs.com) (4.17) | 15 531.39 | 17 462.33 | 17 489.83 |
| 152 | ruby (3.0)| [roda](https://roda.jeremyevans.net) (3.42) | 15 466.40 | 15 107.73 | 14 826.98 |
| 153 | go (1.16)| [macaron](https://go-macaron.com) (1.4) | 15 317.50 | 16 917.70 | 16 912.56 |
| 154 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 15 126.79 | 17 462.89 | 18 026.00 |
| 155 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 125.12 | 17 508.78 | 17 986.79 |
| 156 | ruby (3.0)| [cuba](https://cuba.is) (3.9) | 14 065.33 | 13 843.24 | 13 505.59 |
| 157 | python (3.9)| [flask](https://flask.pocoo.org) (1.1) | 13 813.29 | 17 464.88 | 17 476.35 |
| 158 | java (11)| [struts2](https://struts.apache.org) (2.5) | 13 739.89 | 14 051.21 | 14 065.65 |
| 159 | ruby (3.0)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 13 657.52 | 13 306.13 | 13 099.49 |
| 160 | javascript (14.16)| [feathersjs](https://feathersjs.com) (4.5) | 13 619.66 | 17 800.66 | 17 690.74 |
| 161 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 13 462.28 | 14 860.44 | 21 852.87 |
| 162 | ruby (3.0)| [camping](https://github.com/camping/camping) (2.1) | 12 927.00 | 12 436.99 | 12 317.34 |
| 163 | java (11)| [blade](https://lets-blade.com) (2.0) | 12 877.56 | 15 396.77 | 14 460.98 |
| 164 | ruby (3.0)| [rack_app](https://rack-app.com) (7.7) | 12 814.98 | 12 202.73 | 11 913.19 |
| 165 | javascript (14.16)| [nestjs-express](https://nestjs.com) (7.6) | 11 946.80 | 12 644.29 | 11 270.82 |
| 166 | go (1.16)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 408.39 | 11 751.62 | 11 792.91 |
| 167 | dart (2.12)| [start](https://github.com/lvivski/start) (0.4) | 11 347.71 | 11 149.05 | 10 869.30 |
| 168 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 9 995.69 | 10 248.77 | 10 371.22 |
| 169 | go (1.16)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 941.45 | 10 178.99 | 10 137.41 |
| 170 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.6) | 9 876.13 | 10 032.54 | 10 144.89 |
| 171 | python (3.9)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 804.20 | 9 949.05 | 9 620.67 |
| 172 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 9 761.94 | 9 938.84 | 10 090.99 |
| 173 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.7) | 9 653.76 | 9 704.21 | 9 936.42 |
| 174 | python (3.9)| [guillotina](https://guillotina.io) (6.2) | 9 449.34 | 9 065.33 | 8 782.82 |
| 175 | php (7.4)| [laravel-s-lumen](https://github.com/hhxsv5/laravel-s) (3.7) | 9 416.15 | 9 256.55 | 9 790.21 |
| 176 | ruby (3.0)| [sinatra](https://sinatrarb.com) (2.1) | 8 268.76 | 8 060.63 | 8 059.55 |
| 177 | pony (0.39)| [jennet](https://github.com/Theodus/jennet) (0.1) | 7 946.65 | 13 307.89 | 12 646.71 |
| 178 | ruby (3.0)| [grape](https://ruby-grape.org) (1.5) | 7 769.56 | 7 511.85 | 7 517.53 |
| 179 | python (3.9)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 7 608.27 | 7 256.74 | 6 864.07 |
| 180 | python (3.9)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 332.35 | 6 968.88 | 6 526.94 |
| 181 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 7 191.56 | 7 531.53 | 7 485.90 |
| 182 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 7 095.56 | 7 468.37 | 7 412.00 |
| 183 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 6 932.14 | 7 341.34 | 7 248.02 |
| 184 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 873.15 | 7 244.38 | 7 142.54 |
| 185 | php (7.4)| [laravel-s-laravel](https://github.com/hhxsv5/laravel-s) (3.7) | 6 390.19 | 6 381.19 | 6 289.67 |
| 186 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 329.02 | 6 704.12 | 6 747.49 |
| 187 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 6 032.29 | 6 703.45 | 6 697.42 |
| 188 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 5 865.24 | 5 945.17 | 5 895.72 |
| 189 | python (3.9)| [tornado](https://tornadoweb.org) (6.1) | 5 849.30 | 5 790.80 | 5 670.51 |
| 190 | python (3.9)| [django](https://djangoproject.com) (3.1) | 5 468.33 | 5 224.79 | 5 159.54 |
| 191 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.2) | 5 126.48 | 5 299.02 | 5 214.58 |
| 192 | php (7.4)| [fatfree](https://fatfreeframework.com) (3.7) | 4 969.33 | 5 159.86 | 5 090.57 |
| 193 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 676.30 | 4 917.39 | 4 832.46 |
| 194 | php (7.4)| [cubex](https://cubex.io) (4.16) | 4 644.20 | 4 818.62 | 4 803.51 |
| 195 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 383.24 | 4 539.28 | 4 549.21 |
| 196 | javascript (14.16)| [sails](https://sailsjs.com) (1.4) | 4 264.65 | 4 223.07 | 4 178.72 |
| 197 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.3) | 4 101.37 | 4 254.36 | 4 263.51 |
| 198 | julia (1.6)| [merly](https://github.com/codeneomatrix/Merly.jl) (1.0) | 3 927.90 | 6 423.11 | 4 125.02 |
| 199 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.6) | 3 886.11 | 3 956.14 | 3 942.37 |
| 200 | php (7.4)| [nette](https://nette.org/en/) (3.1) | 3 880.83 | 3 948.42 | 3 987.27 |
| 201 | php (7.4)| [lumen](https://lumen.laravel.com) (8.2) | 3 840.77 | 3 894.71 | 3 894.13 |
| 202 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 3 446.59 | 3 536.86 | 3 541.13 |
| 203 | php (7.4)| [symfony](https://symfony.com) (5.2) | 3 034.91 | 3 042.71 | 3 057.78 |
| 204 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.6) | 2 969.15 | 3 018.74 | 3 025.42 |
| 205 | ruby (3.0)| [rails](https://rubyonrails.org) (6.1) | 2 785.69 | 2 760.89 | 2 749.28 |
| 206 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 515.84 | 2 558.03 | 2 553.13 |
| 207 | python (3.9)| [masonite](https://masoniteproject.com) (3.0) | 2 494.74 | 2 480.49 | 2 483.17 |
| 208 | php (7.4)| [antidot](https://github.com/antidot-framework/react-framework) (0.2) | 1 984.49 | 673.54 | 1 739.89 |
| 209 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 910.77 | 1 832.34 | 1 761.53 |
| 210 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 867.30 | 1 817.65 | 1 845.71 |
| 211 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.3) | 1 844.20 | 1 855.09 | 1 866.59 |
| 212 | php (7.4)| [driftphp](https://github.com/driftphp/driftphp) (0.1) | 1 803.66 | 662.02 | 1 690.96 |
| 213 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 726.28 | 1 742.92 | 1 741.75 |
| 214 | python (3.9)| [cyclone](https://cyclone.io) (1.3) | 1 596.89 | 1 585.23 | 1 569.35 |
| 215 | python (3.9)| [klein](https://github.com/twisted/klein) (20.6) | 1 493.27 | 1 514.00 | 1 488.83 |
| 216 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 342.90 | 593.51 | 623.43 |
| 217 | python (3.9)| [django-ninja](https://django-ninja.rest-framework.com) (0.11) | 1 335.28 | 1 635.34 | 1 662.62 |
| 218 | python (3.9)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 230.17 | 1 169.43 | 1 152.99 |
| 219 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.1) | 1 180.88 | 1 201.09 | 1 199.85 |
| 220 | php (7.4)| [laravel](https://laravel.com) (8.33) | 984.64 | 986.10 | 983.95 |
| 221 | php (7.4)| [unic](https://unicframework.github.io/docs) (1.0) | 519.65 | 406.66 | 270.66 |
| 222 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 287.33 | 302.83 | -103.43 |
| 223 | cpp (11)| [nawa](https://github.com/jatofg/nawa) (0.6) | 258.76 | NaN | NaN |
</a>

</details>

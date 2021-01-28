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

## Results (2021-01-28)



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
| 1 | java (11)| [activej](https://activej.io) (3.0) | 172 489.04 | 214 122.41 | 219 373.11 |
| 2 | php (7.4)| [webman](https://github.com/walkor/webman) (1.0) | 156 997.93 | 168 186.85 | 171 132.92 |
| 3 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 149 507.25 | 181 650.32 | 183 923.43 |
| 4 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 122 504.46 | 135 292.20 | 136 549.05 |
| 5 | go (1.15)| [fiber](https://gofiber.io) (2.3) | 121 279.06 | 129 903.09 | 129 011.27 |
| 6 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 120 797.27 | 123 737.87 | 123 304.20 |
| 7 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 117 370.12 | 146 058.44 | 148 892.00 |
| 8 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.6) | 116 854.81 | 128 532.01 | 127 779.73 |
| 9 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.19) | 116 326.97 | 130 391.37 | 130 567.54 |
| 10 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 116 232.57 | 128 534.74 | 127 901.44 |
| 11 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 116 170.50 | 127 703.94 | 127 549.20 |
| 12 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 114 525.13 | 141 543.00 | 144 708.24 |
| 13 | java (11)| [undertow](https://undertow.io) (2.2) | 112 835.11 | 138 017.25 | 139 858.22 |
| 14 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 112 780.71 | 112 143.05 | 115 102.80 |
| 15 | clojure (1.1)| [donkey](https://github.com/AppsFlyer/donkey) (0.4) | 111 212.10 | 113 673.95 | 115 075.21 |
| 16 | javascript (14.15)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.4) | 110 373.50 | 141 798.78 | 145 491.38 |
| 17 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 109 950.40 | 136 691.44 | 141 841.53 |
| 18 | java (11)| [jooby](https://jooby.io) (2.9) | 109 742.11 | 137 428.21 | 142 450.00 |
| 19 | javascript (14.15)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 108 915.27 | 137 693.56 | 140 564.67 |
| 20 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 108 546.32 | 133 852.57 | 136 199.53 |
| 21 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 108 047.40 | 104 692.32 | 109 244.00 |
| 22 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 107 373.07 | 133 926.78 | 138 009.76 |
| 23 | c (11)| [kore](https://kore.io) (3.3) | 107 316.81 | 186 537.59 | 187 148.73 |
| 24 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 106 302.24 | 131 795.97 | 135 018.70 |
| 25 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 99 905.65 | 121 190.71 | 121 846.92 |
| 26 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 98 051.02 | 120 732.08 | 120 663.68 |
| 27 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.4) | 97 515.60 | 119 460.20 | 119 870.17 |
| 28 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 95 244.75 | 116 492.64 | 119 492.42 |
| 29 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 94 581.31 | 138 851.56 | 149 135.62 |
| 30 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 93 355.55 | 113 176.74 | 112 883.61 |
| 31 | crystal (0.35)| [kemal](https://kemalcr.com) (0.27) | 92 472.38 | 111 869.50 | 111 382.76 |
| 32 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 89 004.19 | 105 895.82 | 105 380.70 |
| 33 | java (11)| [quarkus](https://quarkus.io) (1.11) | 86 712.97 | 105 236.65 | 108 001.71 |
| 34 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.12) | 83 167.28 | 93 706.89 | 89 079.25 |
| 35 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 82 309.38 | 106 469.02 | 112 819.05 |
| 36 | scala (2.13)| [akkahttp](https://akka.io) (10.2) | 82 236.16 | 94 492.64 | 92 946.60 |
| 37 | go (1.15)| [gf](https://goframe.org) (1.15) | 81 915.66 | 89 355.15 | 91 764.20 |
| 38 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 81 887.83 | 85 762.32 | 83 322.07 |
| 39 | javascript (14.15)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 81 862.86 | 97 805.46 | 99 196.81 |
| 40 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 80 810.79 | 81 710.14 | 83 924.88 |
| 41 | go (1.15)| [clevergo](https://clevergo.tech) (0.5) | 80 701.12 | 81 952.80 | 83 829.73 |
| 42 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 79 997.88 | 81 146.12 | 83 020.51 |
| 43 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 79 988.94 | 80 796.69 | 82 929.89 |
| 44 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 029.41 | 82 562.11 | 83 749.90 |
| 45 | go (1.15)| [chi](https://github.com/go-chi/chi) (1.5) | 77 367.69 | 76 818.37 | 79 386.04 |
| 46 | go (1.15)| [violetear](https://violetear.org) (7.0) | 76 874.43 | 77 042.81 | 79 169.53 |
| 47 | scala (2.13)| [finatra](https://twitter.github.io/finatra/) (21.1) | 76 207.28 | 87 869.26 | 88 596.85 |
| 48 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 75 151.81 | 78 994.81 | 79 457.87 |
| 49 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 75 038.29 | 74 528.22 | 76 659.48 |
| 50 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 74 238.64 | 86 785.18 | 89 381.59 |
| 51 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 73 353.52 | 71 730.47 | 74 550.82 |
| 52 | python (3.9)| [falcon](https://falconframework.org) (2.0) | 73 197.72 | 80 872.97 | 82 323.61 |
| 53 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 72 803.05 | 84 211.01 | 86 699.56 |
| 54 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.3) | 72 459.85 | 68 488.02 | 65 168.61 |
| 55 | fsharp (5.0)| [falco](https://falcoframework.com) (3.0) | 71 798.90 | 83 081.39 | 85 533.39 |
| 56 | go (1.15)| [beego](https://beego.me) (1.12) | 71 598.44 | 74 560.19 | 76 404.37 |
| 57 | javascript (14.15)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 70 068.86 | 78 339.95 | 79 288.19 |
| 58 | php (7.4)| [swoole-coroutine](https://github.com/swoole/swoole-src) (4.6) | 69 907.39 | 95 358.19 | 101 717.45 |
| 59 | go (1.15)| [air](https://github.com/aofei/air) (0.21) | 64 815.15 | 63 548.65 | 65 966.79 |
| 60 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 64 747.32 | 72 271.08 | 72 673.15 |
| 61 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 64 480.67 | 130 606.88 | 131 105.97 |
| 62 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.6) | 62 450.18 | 62 372.57 | 65 055.19 |
| 63 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 61 210.17 | 66 647.24 | 66 224.24 |
| 64 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 61 100.56 | 64 377.69 | 65 383.98 |
| 65 | scala (2.13)| [finch](https://finagle.github.io/finch/) (0.32) | 61 032.14 | 67 906.61 | 67 916.01 |
| 66 | javascript (14.15)| [0http](https://github.com/jkyberneees/0http) (3.0) | 60 485.31 | 66 077.49 | 67 352.62 |
| 67 | javascript (14.15)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 60 463.03 | 68 110.62 | 69 343.53 |
| 68 | javascript (14.15)| [polka](https://github.com/lukeed/polka) (0.5) | 57 364.10 | 61 878.15 | 60 915.53 |
| 69 | javascript (14.15)| [restana](https://github.com/jkyberneees/ana) (4.7) | 56 285.77 | 63 629.16 | 64 685.16 |
| 70 | javascript (14.15)| [rayo](https://rayo.js.org) (1.3) | 55 892.66 | 60 803.79 | 59 631.89 |
| 71 | javascript (14.15)| [fastify](https://fastify.io) (3.11) | 55 050.58 | 59 870.80 | 58 507.46 |
| 72 | python (3.9)| [pyramid](https://trypyramid.com) (1.1) | 53 980.92 | 58 449.32 | 58 794.05 |
| 73 | php (7.4)| [one](https://github.com/lizhichao/one) (2.2) | 53 953.68 | 60 979.96 | 69 264.81 |
| 74 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 53 723.86 | 56 561.25 | 54 950.46 |
| 75 | javascript (14.15)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 53 496.99 | 59 024.72 | 57 195.14 |
| 76 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 52 720.85 | 60 967.91 | 63 449.53 |
| 77 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 51 868.77 | 57 081.66 | 64 039.58 |
| 78 | java (11)| [javalin](https://javalin.io) (3.9) | 51 089.17 | 54 623.06 | 54 714.79 |
| 79 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.14) | 50 993.19 | 74 337.64 | 82 949.52 |
| 80 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 50 883.40 | 66 456.90 | 69 208.33 |
| 81 | php (7.4)| [hyperf](https://hyperf.io) (2.1) | 50 649.24 | 62 387.45 | 65 699.21 |
| 82 | java (11)| [micronaut](https://micronaut.io) (1.2) | 50 373.56 | 57 734.73 | 57 780.37 |
| 83 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 50 089.10 | 56 567.85 | 56 844.36 |
| 84 | java (11)| [spark](https://sparkjava.com) (2.9) | 50 030.71 | 54 617.26 | 55 763.17 |
| 85 | java (11)| [restheart](https://restheart.org) (5.1) | 49 989.09 | 50 868.68 | 51 440.71 |
| 86 | rust (1.49)| [salvo](https://github.com/kenorld/salvo) (0.5) | 48 862.75 | 52 214.36 | 53 140.28 |
| 87 | javascript (14.15)| [nestjs-fastify](https://nestjs.com) (7.6) | 48 550.94 | 51 914.97 | 50 777.31 |
| 88 | rust (1.49)| [actix](https://actix.rs) (3.3) | 46 446.94 | 48 857.90 | 50 287.36 |
| 89 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 45 960.25 | 46 388.14 | 49 416.04 |
| 90 | php (7.4)| [swoole](https://github.com/swoole/swoole-src) (4.6) | 45 797.46 | 59 984.77 | 69 587.10 |
| 91 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 45 576.13 | 48 574.49 | 50 991.45 |
| 92 | javascript (14.15)| [foxify](https://foxify.js.org) (0.1) | 45 319.32 | 49 052.73 | 47 794.71 |
| 93 | javascript (14.15)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 44 953.41 | 48 506.45 | 48 220.18 |
| 94 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 616.87 | 45 619.42 | 45 858.37 |
| 95 | python (3.9)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 44 518.20 | 50 769.82 | 52 108.81 |
| 96 | php (7.4)| [comet](https://github.com/gotzmann/comet) (1.1) | 44 481.58 | 48 370.37 | 49 292.96 |
| 97 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 42 939.20 | 32 780.98 | 31 204.05 |
| 98 | python (3.9)| [bottle](https://bottlepy.org) (0.12) | 42 025.68 | 45 379.14 | 46 213.86 |
| 99 | javascript (14.15)| [koa](https://koajs.com) (2.13) | 40 740.58 | 43 880.23 | 42 680.88 |
| 100 | javascript (14.15)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 37 089.76 | 39 711.52 | 38 869.10 |
| 101 | python (3.9)| [emmett](https://emmett.sh) (2.1) | 36 591.71 | 41 284.01 | 42 138.27 |
| 102 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 36 374.10 | 37 639.40 | 37 728.66 |
| 103 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 36 084.53 | 35 954.84 | 35 498.06 |
| 104 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 35 153.76 | 37 274.08 | 37 344.53 |
| 105 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 35 095.02 | 35 213.46 | 35 158.93 |
| 106 | swift (5.3)| [vapor](https://vapor.codes) (4.39) | 35 080.01 | 37 200.86 | 36 863.42 |
| 107 | javascript (14.15)| [moleculer](https://moleculer.services) (0.14) | 35 020.16 | 36 034.99 | 34 845.04 |
| 108 | javascript (14.15)| [hapi](https://hapijs.com) (20.1) | 34 720.09 | 35 399.65 | 34 291.74 |
| 109 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 34 263.75 | 47 739.77 | 50 503.02 |
| 110 | rust (1.49)| [nickel](https://nickel-org.github.io) (0.11) | 34 148.83 | 34 062.44 | 33 453.40 |
| 111 | java (11)| [jersey3-grizzly2](https://eclipse-ee4j.github.io/jersey) (3) | 34 027.53 | 39 197.12 | 38 437.65 |
| 112 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 33 885.45 | 34 797.56 | 33 330.33 |
| 113 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 33 533.38 | 37 973.77 | 38 485.53 |
| 114 | java (11)| [jersey-grizzly2](https://eclipse-ee4j.github.io/jersey) (2.33) | 33 409.87 | 38 363.82 | 38 061.36 |
| 115 | python (3.9)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 33 265.23 | 38 203.15 | 39 095.65 |
| 116 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 32 050.50 | 27 968.54 | 24 184.15 |
| 117 | python (3.9)| [hug](https://hug.rest) (2.6) | 32 040.61 | 34 769.16 | 34 804.11 |
| 118 | rust (1.49)| [gotham](https://gotham.rs) (0.5) | 31 904.99 | 35 428.36 | 36 452.98 |
| 119 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 31 598.73 | 31 210.35 | 31 354.97 |
| 120 | python (3.9)| [sanic](https://github.com/huge-success/sanic) (20.12) | 30 801.73 | 34 371.87 | 34 956.68 |
| 121 | python (3.9)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.3) | 30 271.08 | 34 223.69 | 34 494.17 |
| 122 | javascript (14.15)| [restify](https://restify.com) (8.5) | 29 616.56 | 30 653.36 | 29 659.92 |
| 123 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 28 934.55 | 31 970.84 | 31 880.42 |
| 124 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 28 099.82 | 31 773.81 | 33 218.58 |
| 125 | javascript (14.15)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 28 069.43 | 29 858.31 | 36 576.43 |
| 126 | scala (2.13)| [play](https://playframework.com) (2.8) | 27 532.17 | 29 590.84 | 29 351.84 |
| 127 | python (3.9)| [starlette](https://starlette.io) (0.14) | 27 339.14 | 32 109.29 | 32 609.60 |
| 128 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 26 940.01 | 29 182.05 | 28 576.79 |
| 129 | python (3.9)| [index.py](https://index-py.abersheeran.com) (0.16) | 26 709.58 | 29 734.32 | 30 396.12 |
| 130 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 24 346.19 | 24 212.30 | 21 558.85 |
| 131 | python (3.9)| [responder](https://python-responder.org) (2.0) | 24 270.37 | 29 760.93 | 30 317.43 |
| 132 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 24 212.85 | 26 182.21 | 25 650.26 |
| 133 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 22 885.07 | 21 756.06 | 20 563.91 |
| 134 | clojure (1.1)| [luminus](https://luminusweb.com) (3.93) | 22 527.08 | 21 982.77 | 20 724.11 |
| 135 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 21 896.94 | 21 733.68 | 21 491.19 |
| 136 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.16) | 21 864.21 | 21 254.77 | 19 413.98 |
| 137 | python (3.9)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 21 615.79 | 25 489.01 | 27 197.36 |
| 138 | javascript (14.15)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 21 181.17 | 22 864.11 | 22 390.58 |
| 139 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 20 998.57 | 20 391.71 | 20 598.65 |
| 140 | python (3.9)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 19 109.42 | 23 397.73 | 23 886.05 |
| 141 | javascript (14.15)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 18 835.86 | 17 703.48 | 16 897.05 |
| 142 | python (3.9)| [molten](https://moltenframework.com) (1.0) | 18 658.63 | 22 253.15 | 22 107.45 |
| 143 | python (3.9)| [fastapi](https://fastapi.tiangolo.com) (0.63) | 18 248.79 | 21 857.01 | 21 998.03 |
| 144 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 484.42 | 15 702.15 | 14 659.99 |
| 145 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 16 967.14 | 16 479.55 | 16 071.07 |
| 146 | php (7.4)| [swoft](https://swoft.org) (2.0) | 16 934.79 | 20 560.24 | 21 233.54 |
| 147 | rust (1.49)| [iron](https://ironframework.io) (0.6) | 16 783.22 | 16 614.84 | 16 748.67 |
| 148 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 16 509.54 | 16 000.41 | 15 803.50 |
| 149 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 16 174.34 | 14 317.32 | 13 250.65 |
| 150 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.40) | 15 870.30 | 15 572.23 | 15 247.29 |
| 151 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 15 691.09 | 18 616.90 | 18 871.77 |
| 152 | go (1.15)| [macaron](https://go-macaron.com) (1.4) | 15 370.25 | 16 875.99 | 16 907.52 |
| 153 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 264.13 | 17 869.44 | 17 772.38 |
| 154 | javascript (14.15)| [express](https://expressjs.com) (4.17) | 15 259.08 | 17 131.22 | 17 561.22 |
| 155 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 15 246.73 | 14 685.77 | 14 517.50 |
| 156 | javascript (14.15)| [feathersjs](https://feathersjs.com) (4.5) | 15 170.86 | 16 867.59 | 17 669.23 |
| 157 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 15 169.06 | 17 455.24 | 17 794.28 |
| 158 | python (3.9)| [flask](https://flask.pocoo.org) (1.1) | 14 213.62 | 16 498.03 | 16 546.10 |
| 159 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 13 903.44 | 13 518.25 | 13 255.68 |
| 160 | java (11)| [struts2](https://struts.apache.org) (2.5) | 13 902.63 | 14 173.11 | 14 202.43 |
| 161 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 13 737.14 | 13 645.04 | 18 046.97 |
| 162 | ruby (2.7)| [rack_app](https://rack-app.com) (7.7) | 13 634.37 | 13 177.94 | 12 982.29 |
| 163 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 13 157.12 | 12 734.07 | 12 586.07 |
| 164 | java (11)| [blade](https://lets-blade.com) (2.0) | 12 977.40 | 15 620.58 | 14 420.70 |
| 165 | javascript (14.15)| [nestjs-express](https://nestjs.com) (7.6) | 11 769.80 | 13 231.81 | 12 221.70 |
| 166 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 390.20 | 11 674.36 | 11 696.56 |
| 167 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 11 097.91 | 10 872.34 | 10 533.82 |
| 168 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.5) | 10 199.29 | 10 425.60 | 10 517.24 |
| 169 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 10 058.12 | 10 284.06 | 10 390.24 |
| 170 | python (3.9)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 906.84 | 9 648.46 | 9 506.69 |
| 171 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 850.39 | 10 029.30 | 10 015.81 |
| 172 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 9 772.43 | 9 977.40 | 10 086.13 |
| 173 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.7) | 9 597.56 | 9 721.35 | 9 811.71 |
| 174 | php (7.4)| [laravel-s-lumen](https://github.com/hhxsv5/laravel-s) (3.7) | 9 337.15 | 9 999.62 | 9 880.99 |
| 175 | python (3.9)| [guillotina](https://guillotina.io) (6.1) | 9 155.37 | 9 363.99 | 8 872.01 |
| 176 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.1) | 8 962.05 | 8 729.06 | 8 714.94 |
| 177 | ruby (2.7)| [grape](https://ruby-grape.org) (1.5) | 8 671.78 | 8 391.22 | 8 359.83 |
| 178 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 8 028.81 | 13 300.69 | 12 541.27 |
| 179 | python (3.9)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 7 438.09 | 7 173.07 | 6 881.52 |
| 180 | python (3.9)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 431.33 | 7 046.06 | 6 547.27 |
| 181 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 6 741.60 | 6 684.61 | 6 620.08 |
| 182 | python (3.9)| [tornado](https://tornadoweb.org) (6.1) | 6 582.50 | 6 509.54 | 6 618.94 |
| 183 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 565.17 | 6 502.82 | 6 448.78 |
| 184 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 6 510.35 | 6 425.23 | 6 325.23 |
| 185 | php (7.4)| [laravel-s-laravel](https://github.com/hhxsv5/laravel-s) (3.7) | 6 489.99 | 6 412.15 | 6 378.22 |
| 186 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 6 337.82 | 6 285.95 | 6 165.02 |
| 187 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 203.45 | 6 122.04 | 6 104.00 |
| 188 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 6 031.42 | 6 724.28 | 6 672.87 |
| 189 | python (3.9)| [django](https://djangoproject.com) (3.1) | 6 025.30 | 5 870.86 | 5 795.79 |
| 190 | python (3.9)| [masonite](https://masoniteproject.com) (3.0) | 5 781.84 | 8 423.68 | 8 404.93 |
| 191 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 5 659.06 | 5 565.07 | 5 555.72 |
| 192 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 5 233.65 | 5 194.91 | 5 140.07 |
| 193 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 5 184.13 | 5 104.93 | 5 150.53 |
| 194 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.2) | 4 675.45 | 4 634.77 | 4 580.18 |
| 195 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 452.91 | 4 407.50 | 4 362.95 |
| 196 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 434.34 | 4 409.25 | 4 416.38 |
| 197 | javascript (14.15)| [sails](https://sailsjs.com) (1.4) | 4 278.53 | 4 259.47 | 4 223.43 |
| 198 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 4 124.67 | 2 403.50 | 2 273.21 |
| 199 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.3) | 3 952.66 | 3 892.43 | 3 884.84 |
| 200 | php (7.4)| [nette](https://nette.org/en/) (3.1) | 3 877.45 | 3 861.28 | 3 883.52 |
| 201 | php (7.4)| [lumen](https://lumen.laravel.com) (8.2) | 3 832.73 | 3 836.87 | 3 874.13 |
| 202 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.5) | 3 790.34 | 3 739.20 | 3 737.82 |
| 203 | julia (1.6)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 722.78 | 6 875.45 | 5 823.67 |
| 204 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 3 427.00 | 3 389.11 | 3 392.61 |
| 205 | php (7.4)| [fatfree](https://fatfreeframework.com) (3.7) | 3 077.05 | 3 045.89 | 3 193.30 |
| 206 | php (7.4)| [symfony](https://symfony.com) (5.2) | 2 974.50 | 2 982.04 | 3 001.11 |
| 207 | ruby (2.7)| [rails](https://rubyonrails.org) (6.1) | 2 786.44 | 2 770.54 | 2 771.03 |
| 208 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 617.76 | 2 603.01 | 2 624.53 |
| 209 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.5) | 2 291.48 | 2 285.19 | 2 305.05 |
| 210 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 289.31 | 2 535.78 | 2 511.53 |
| 211 | php (7.4)| [antidot](https://antidotfw.io) (0.1) | 2 022.13 | 1 490.20 | 1 578.08 |
| 212 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 923.81 | 1 841.40 | 1 771.75 |
| 213 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 912.21 | 1 849.89 | 1 836.03 |
| 214 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 1 862.91 | 1 918.38 | 1 907.67 |
| 215 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.3) | 1 853.95 | 1 851.30 | 1 860.70 |
| 216 | php (7.4)| [driftphp](https://github.com/driftphp/driftphp) (0.1) | 1 825.75 | 1 787.27 | 1 393.16 |
| 217 | python (3.9)| [cyclone](https://cyclone.io) (1.3) | 1 660.91 | 1 656.86 | 1 637.36 |
| 218 | python (3.9)| [klein](https://github.com/twisted/klein) (20.6) | 1 616.57 | 1 620.34 | 1 590.70 |
| 219 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 576.18 | 1 589.16 | 1 594.54 |
| 220 | python (3.9)| [django-ninja](https://django-ninja.rest-framework.com) (0.10) | 1 290.74 | 1 629.98 | 1 663.42 |
| 221 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 257.57 | 636.88 | 400.15 |
| 222 | python (3.9)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 247.33 | 1 175.82 | 1 181.56 |
| 223 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 091.44 | 1 107.34 | 1 107.71 |
| 224 | php (7.4)| [laravel](https://laravel.com) (8.25) | 903.64 | 909.92 | 907.22 |
| 225 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 287.92 | 302.35 | -88.22 |
| 226 | cpp (11)| [nawa](https://github.com/jatofg/nawa) (0.6) | 260.63 | NaN | NaN |
</a>

</details>

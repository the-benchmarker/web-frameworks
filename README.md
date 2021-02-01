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

## Results (2021-02-01)



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
| 1 | java (11)| [activej](https://activej.io) (3.0) | 172 654.47 | 214 491.47 | 219 548.28 |
| 2 | php (7.4)| [webman](https://github.com/walkor/webman) (1.0) | 158 566.41 | 168 840.35 | 171 636.94 |
| 3 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 149 198.64 | 181 906.42 | 184 240.10 |
| 4 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 122 511.16 | 135 379.67 | 136 744.92 |
| 5 | go (1.15)| [fiber](https://gofiber.io) (2.4) | 121 549.98 | 130 131.31 | 129 234.81 |
| 6 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 121 051.19 | 123 828.03 | 123 370.92 |
| 7 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 117 552.27 | 146 450.47 | 149 201.55 |
| 8 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.6) | 117 032.14 | 128 849.40 | 128 174.03 |
| 9 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.19) | 116 559.40 | 130 490.95 | 130 783.85 |
| 10 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 116 369.08 | 128 440.44 | 127 894.11 |
| 11 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 116 343.90 | 128 839.96 | 128 134.31 |
| 12 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 114 553.15 | 141 952.17 | 145 017.41 |
| 13 | java (11)| [undertow](https://undertow.io) (2.2) | 113 068.61 | 138 290.98 | 140 258.08 |
| 14 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 112 917.01 | 112 179.64 | 115 277.63 |
| 15 | clojure (1.1)| [donkey](https://github.com/AppsFlyer/donkey) (0.4) | 111 586.66 | 114 235.23 | 115 523.86 |
| 16 | javascript (14.15)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.4) | 110 303.18 | 141 796.02 | 145 339.99 |
| 17 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 110 095.20 | 136 715.24 | 141 687.09 |
| 18 | java (11)| [jooby](https://jooby.io) (2.9) | 109 783.24 | 137 511.44 | 142 486.40 |
| 19 | javascript (14.15)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 108 816.96 | 138 126.55 | 141 032.11 |
| 20 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 108 245.80 | 104 775.44 | 109 331.90 |
| 21 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 107 618.08 | 134 274.36 | 138 140.02 |
| 22 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 107 460.87 | 133 191.83 | 135 627.98 |
| 23 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 106 240.11 | 131 816.34 | 135 119.39 |
| 24 | c (11)| [kore](https://kore.io) (3.3) | 103 469.14 | 188 437.97 | 189 560.88 |
| 25 | crystal (0.35)| [kemal](https://kemalcr.com) (0.27) | 100 746.30 | 122 499.09 | 122 203.65 |
| 26 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 99 684.07 | 121 521.37 | 122 099.55 |
| 27 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.4) | 98 317.35 | 119 833.99 | 120 221.33 |
| 28 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 97 993.10 | 120 919.28 | 120 759.25 |
| 29 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 95 285.59 | 116 729.62 | 119 654.80 |
| 30 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 94 323.82 | 139 024.99 | 149 677.20 |
| 31 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 93 615.40 | 113 425.46 | 112 868.74 |
| 32 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 89 494.89 | 106 440.88 | 105 524.24 |
| 33 | java (11)| [quarkus](https://quarkus.io) (1.11) | 86 687.53 | 105 096.82 | 107 820.15 |
| 34 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.12) | 82 941.52 | 93 355.22 | 89 092.96 |
| 35 | scala (2.13)| [akkahttp](https://akka.io) (10.2) | 82 722.70 | 95 505.24 | 93 898.95 |
| 36 | javascript (14.15)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 82 381.55 | 98 124.14 | 99 456.01 |
| 37 | go (1.15)| [gf](https://goframe.org) (1.15) | 82 322.42 | 89 671.92 | 91 992.19 |
| 38 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 81 107.67 | 105 402.10 | 113 685.77 |
| 39 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 80 836.66 | 81 736.25 | 83 920.53 |
| 40 | go (1.15)| [clevergo](https://clevergo.tech) (0.5) | 80 661.98 | 81 854.77 | 83 761.39 |
| 41 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 80 142.66 | 81 204.54 | 83 222.31 |
| 42 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 80 014.12 | 83 808.24 | 83 242.63 |
| 43 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 79 928.10 | 80 794.43 | 82 897.38 |
| 44 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 157.54 | 82 656.29 | 83 856.95 |
| 45 | go (1.15)| [chi](https://github.com/go-chi/chi) (1.5) | 77 361.49 | 76 718.44 | 79 333.46 |
| 46 | php (7.4)| [swoole-coroutine](https://github.com/swoole/swoole-src) (4.6) | 77 171.13 | 107 382.65 | 116 136.96 |
| 47 | go (1.15)| [violetear](https://violetear.org) (7.0) | 76 939.11 | 77 058.24 | 79 161.76 |
| 48 | scala (2.13)| [finatra](https://twitter.github.io/finatra/) (21.1) | 75 257.63 | 87 084.01 | 87 572.09 |
| 49 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 75 175.36 | 78 380.51 | 79 270.50 |
| 50 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 75 125.15 | 74 609.57 | 76 864.26 |
| 51 | python (3.9)| [falcon](https://falconframework.org) (2.0) | 73 606.56 | 80 856.00 | 82 449.99 |
| 52 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 73 546.40 | 85 793.98 | 88 401.78 |
| 53 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 73 434.83 | 71 683.26 | 74 542.62 |
| 54 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 73 329.00 | 84 994.73 | 87 596.04 |
| 55 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 73 148.62 | 130 751.04 | 131 700.90 |
| 56 | php (7.4)| [swoole](https://github.com/swoole/swoole-src) (4.6) | 72 722.44 | 89 964.00 | 104 095.85 |
| 57 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.3) | 72 257.61 | 67 107.19 | 64 779.70 |
| 58 | fsharp (5.0)| [falco](https://falcoframework.com) (3.0) | 71 976.05 | 83 313.58 | 85 806.30 |
| 59 | go (1.15)| [beego](https://beego.me) (1.12) | 71 610.38 | 74 567.06 | 76 510.77 |
| 60 | javascript (14.15)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 70 593.00 | 78 688.06 | 79 577.32 |
| 61 | go (1.15)| [air](https://github.com/aofei/air) (0.21) | 64 860.57 | 63 448.47 | 65 906.75 |
| 62 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 64 831.83 | 72 366.23 | 72 460.09 |
| 63 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.6) | 62 398.61 | 62 342.36 | 65 085.02 |
| 64 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 61 495.10 | 64 699.89 | 65 818.00 |
| 65 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 61 390.59 | 67 044.55 | 66 467.49 |
| 66 | scala (2.13)| [finch](https://finagle.github.io/finch/) (0.32) | 61 218.33 | 68 187.37 | 68 101.43 |
| 67 | javascript (14.15)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 61 165.23 | 68 500.30 | 69 696.65 |
| 68 | javascript (14.15)| [0http](https://github.com/jkyberneees/0http) (3.0) | 59 754.36 | 66 006.62 | 67 260.94 |
| 69 | javascript (14.15)| [polka](https://github.com/lukeed/polka) (0.5) | 57 454.29 | 62 055.13 | 60 802.14 |
| 70 | javascript (14.15)| [restana](https://github.com/jkyberneees/ana) (4.7) | 56 607.11 | 63 787.77 | 64 710.56 |
| 71 | javascript (14.15)| [rayo](https://rayo.js.org) (1.3) | 55 697.31 | 60 964.64 | 59 791.29 |
| 72 | javascript (14.15)| [fastify](https://fastify.io) (3.11) | 55 025.61 | 60 131.68 | 58 758.12 |
| 73 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 53 794.85 | 56 548.80 | 55 005.27 |
| 74 | python (3.9)| [pyramid](https://trypyramid.com) (1.1) | 53 768.70 | 58 309.96 | 58 673.71 |
| 75 | php (7.4)| [one](https://github.com/lizhichao/one) (2.2) | 53 559.13 | 62 307.53 | 69 438.22 |
| 76 | javascript (14.15)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 53 198.41 | 58 637.62 | 57 006.24 |
| 77 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 52 901.27 | 61 193.17 | 63 629.38 |
| 78 | rust (1.49)| [salvo](https://github.com/kenorld/salvo) (0.5) | 52 446.02 | 56 388.42 | 57 178.78 |
| 79 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 51 935.43 | 57 158.14 | 64 075.12 |
| 80 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 51 201.80 | 66 890.51 | 69 554.26 |
| 81 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.14) | 51 147.04 | 74 043.38 | 82 557.25 |
| 82 | java (11)| [javalin](https://javalin.io) (3.9) | 51 026.76 | 54 521.08 | 54 822.88 |
| 83 | php (7.4)| [hyperf](https://hyperf.io) (2.1) | 50 618.24 | 62 137.85 | 65 647.30 |
| 84 | java (11)| [micronaut](https://micronaut.io) (1.2) | 50 495.31 | 57 672.85 | 57 814.93 |
| 85 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 50 111.13 | 56 516.59 | 56 819.99 |
| 86 | java (11)| [spark](https://sparkjava.com) (2.9) | 50 103.74 | 54 712.23 | 55 809.02 |
| 87 | javascript (14.15)| [nestjs-fastify](https://nestjs.com) (7.6) | 48 525.13 | 52 010.78 | 50 756.45 |
| 88 | rust (1.49)| [actix](https://actix.rs) (3.3) | 46 725.03 | 48 893.12 | 50 577.58 |
| 89 | java (11)| [restheart](https://restheart.org) (5.3) | 45 962.08 | 47 825.40 | 48 425.98 |
| 90 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 45 861.32 | 46 395.61 | 49 383.52 |
| 91 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 45 556.27 | 48 865.82 | 51 225.03 |
| 92 | javascript (14.15)| [foxify](https://foxify.js.org) (0.1) | 45 311.82 | 49 012.12 | 47 759.05 |
| 93 | javascript (14.15)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 45 281.81 | 48 598.88 | 48 126.37 |
| 94 | php (7.4)| [comet](https://github.com/gotzmann/comet) (1.1) | 44 391.10 | 48 274.07 | 49 184.69 |
| 95 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 380.43 | 45 372.80 | 45 589.89 |
| 96 | python (3.9)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 44 182.33 | 50 916.39 | 52 188.46 |
| 97 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 42 942.32 | 32 942.24 | 31 431.77 |
| 98 | python (3.9)| [bottle](https://bottlepy.org) (0.12) | 41 774.31 | 45 457.88 | 46 312.50 |
| 99 | javascript (14.15)| [koa](https://koajs.com) (2.13) | 41 022.75 | 44 068.56 | 42 713.46 |
| 100 | javascript (14.15)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 36 630.12 | 39 465.87 | 38 698.94 |
| 101 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 36 487.13 | 37 665.38 | 37 864.40 |
| 102 | python (3.9)| [emmett](https://emmett.sh) (2.1) | 36 102.65 | 41 457.09 | 42 179.56 |
| 103 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 35 604.16 | 35 551.83 | 35 339.64 |
| 104 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 35 438.76 | 35 458.95 | 35 262.29 |
| 105 | swift (5.3)| [vapor](https://vapor.codes) (4.39) | 35 360.07 | 37 281.00 | 36 998.45 |
| 106 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 35 301.19 | 37 357.26 | 37 508.04 |
| 107 | javascript (14.15)| [moleculer](https://moleculer.services) (0.14) | 34 975.45 | 35 907.47 | 34 753.25 |
| 108 | javascript (14.15)| [hapi](https://hapijs.com) (20.1) | 34 573.21 | 35 420.89 | 34 705.78 |
| 109 | java (11)| [jersey3-grizzly2](https://eclipse-ee4j.github.io/jersey) (3) | 34 189.52 | 39 366.34 | 38 647.04 |
| 110 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 34 171.51 | 47 125.21 | 49 931.61 |
| 111 | rust (1.49)| [nickel](https://nickel-org.github.io) (0.11) | 34 167.86 | 34 301.52 | 33 763.38 |
| 112 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 33 889.46 | 34 747.98 | 33 247.81 |
| 113 | python (3.9)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 33 669.03 | 38 313.24 | 39 066.37 |
| 114 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 33 367.23 | 37 912.31 | 38 287.30 |
| 115 | java (11)| [jersey-grizzly2](https://eclipse-ee4j.github.io/jersey) (2.33) | 33 321.93 | 38 143.73 | 37 719.73 |
| 116 | python (3.9)| [hug](https://hug.rest) (2.6) | 32 573.46 | 35 214.54 | 40 657.71 |
| 117 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 32 310.44 | 28 154.61 | 24 375.69 |
| 118 | rust (1.49)| [gotham](https://gotham.rs) (0.5) | 31 753.08 | 35 440.19 | 36 407.22 |
| 119 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 31 635.66 | 31 402.12 | 31 224.25 |
| 120 | python (3.9)| [sanic](https://github.com/huge-success/sanic) (20.12) | 30 139.83 | 34 347.19 | 34 703.01 |
| 121 | javascript (14.15)| [restify](https://restify.com) (8.5) | 29 455.68 | 30 591.76 | 29 656.56 |
| 122 | python (3.9)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.3) | 29 259.43 | 33 337.69 | 33 717.40 |
| 123 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 28 937.04 | 31 905.95 | 31 805.12 |
| 124 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 28 227.93 | 31 632.34 | 33 194.29 |
| 125 | javascript (14.15)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 27 759.67 | 29 741.20 | 34 150.97 |
| 126 | scala (2.13)| [play](https://playframework.com) (2.8) | 27 623.26 | 29 603.32 | 29 523.28 |
| 127 | python (3.9)| [starlette](https://starlette.io) (0.14) | 27 107.77 | 31 484.82 | 31 938.00 |
| 128 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 26 985.41 | 28 959.46 | 28 680.39 |
| 129 | python (3.9)| [index.py](https://index-py.abersheeran.com) (0.16) | 26 225.83 | 29 887.97 | 30 525.49 |
| 130 | python (3.9)| [responder](https://python-responder.org) (2.0) | 24 708.92 | 30 593.22 | 30 894.96 |
| 131 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 24 511.38 | 24 529.12 | 22 238.01 |
| 132 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 24 303.93 | 26 283.26 | 25 731.90 |
| 133 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 23 037.45 | 21 700.04 | 20 774.03 |
| 134 | python (3.9)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 22 834.97 | 27 335.90 | 27 835.10 |
| 135 | clojure (1.1)| [luminus](https://luminusweb.com) (3.93) | 22 495.91 | 21 963.64 | 20 997.14 |
| 136 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.16) | 21 945.06 | 21 250.23 | 18 934.52 |
| 137 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 21 936.41 | 21 692.66 | 21 459.27 |
| 138 | javascript (14.15)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 21 094.17 | 22 838.49 | 22 355.86 |
| 139 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 20 941.58 | 20 380.11 | 20 679.71 |
| 140 | python (3.9)| [molten](https://moltenframework.com) (1.0) | 19 061.51 | 22 317.07 | 22 087.90 |
| 141 | python (3.9)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 18 981.95 | 23 547.18 | 23 963.97 |
| 142 | javascript (14.15)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 18 948.11 | 17 716.58 | 16 817.88 |
| 143 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 537.35 | 15 708.42 | 14 655.13 |
| 144 | python (3.9)| [fastapi](https://fastapi.tiangolo.com) (0.63) | 17 281.56 | 21 404.24 | 21 550.64 |
| 145 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 16 952.70 | 16 456.16 | 16 051.97 |
| 146 | php (7.4)| [swoft](https://swoft.org) (2.0) | 16 942.72 | 20 507.67 | 21 256.92 |
| 147 | rust (1.49)| [iron](https://ironframework.io) (0.6) | 16 753.21 | 16 547.26 | 16 740.45 |
| 148 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 16 460.39 | 16 011.33 | 15 734.56 |
| 149 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 16 144.93 | 14 296.95 | 13 262.05 |
| 150 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.40) | 15 883.22 | 15 493.49 | 15 204.17 |
| 151 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 15 706.26 | 18 535.87 | 18 826.47 |
| 152 | go (1.15)| [macaron](https://go-macaron.com) (1.4) | 15 384.76 | 16 897.61 | 16 904.53 |
| 153 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 382.07 | 17 986.80 | 17 948.98 |
| 154 | javascript (14.15)| [express](https://expressjs.com) (4.17) | 15 373.38 | 17 413.66 | 17 652.41 |
| 155 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 15 215.11 | 14 681.30 | 14 494.51 |
| 156 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 15 164.14 | 17 570.92 | 17 809.50 |
| 157 | javascript (14.15)| [feathersjs](https://feathersjs.com) (4.5) | 14 975.24 | 16 608.31 | 17 614.26 |
| 158 | python (3.9)| [flask](https://flask.pocoo.org) (1.1) | 14 112.28 | 16 468.17 | 16 018.32 |
| 159 | java (11)| [struts2](https://struts.apache.org) (2.5) | 13 905.85 | 14 203.31 | 14 234.12 |
| 160 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 13 888.32 | 13 542.58 | 13 244.52 |
| 161 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 13 694.45 | 13 927.70 | 18 127.29 |
| 162 | ruby (2.7)| [rack_app](https://rack-app.com) (7.7) | 13 586.95 | 13 142.04 | 12 935.85 |
| 163 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 13 112.58 | 12 684.51 | 12 532.97 |
| 164 | java (11)| [blade](https://lets-blade.com) (2.0) | 12 999.75 | 15 620.67 | 14 447.89 |
| 165 | javascript (14.15)| [nestjs-express](https://nestjs.com) (7.6) | 11 781.21 | 13 099.87 | 11 925.22 |
| 166 | php (7.4)| [imi](https://imiphp.com) (1.2) | 11 660.51 | 13 060.40 | 13 822.65 |
| 167 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 402.38 | 11 712.52 | 11 724.52 |
| 168 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 11 197.65 | 10 962.30 | 10 644.70 |
| 169 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.5) | 10 192.00 | 10 421.99 | 10 543.76 |
| 170 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 10 050.53 | 10 273.18 | 10 374.00 |
| 171 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 863.89 | 10 070.23 | 10 042.72 |
| 172 | python (3.9)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 827.78 | 9 647.76 | 9 514.97 |
| 173 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 9 775.41 | 9 973.56 | 10 088.55 |
| 174 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.7) | 9 586.95 | 9 725.63 | 9 825.03 |
| 175 | php (7.4)| [laravel-s-lumen](https://github.com/hhxsv5/laravel-s) (3.7) | 9 386.98 | 10 024.18 | 9 721.80 |
| 176 | python (3.9)| [guillotina](https://guillotina.io) (6.1) | 9 147.70 | 9 167.20 | 8 876.82 |
| 177 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.1) | 8 939.87 | 8 751.41 | 8 693.40 |
| 178 | ruby (2.7)| [grape](https://ruby-grape.org) (1.5) | 8 656.64 | 8 385.21 | 8 367.81 |
| 179 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 8 081.02 | 13 324.18 | 12 536.57 |
| 180 | python (3.9)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 7 432.17 | 7 191.61 | 6 861.65 |
| 181 | python (3.9)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 426.62 | 7 071.50 | 6 524.38 |
| 182 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 6 728.08 | 6 667.04 | 6 602.72 |
| 183 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 564.08 | 6 499.37 | 6 449.68 |
| 184 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 6 496.30 | 6 425.16 | 6 318.24 |
| 185 | php (7.4)| [laravel-s-laravel](https://github.com/hhxsv5/laravel-s) (3.7) | 6 467.49 | 6 405.60 | 6 369.62 |
| 186 | python (3.9)| [tornado](https://tornadoweb.org) (6.1) | 6 404.53 | 6 309.36 | 6 367.24 |
| 187 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 6 341.57 | 6 298.04 | 6 172.06 |
| 188 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 206.05 | 6 128.64 | 6 104.32 |
| 189 | python (3.9)| [django](https://djangoproject.com) (3.1) | 6 171.40 | 5 898.03 | 5 762.90 |
| 190 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 5 938.09 | 6 705.49 | 6 687.70 |
| 191 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 5 642.33 | 5 534.48 | 5 536.22 |
| 192 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 5 233.83 | 5 188.11 | 5 139.76 |
| 193 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 5 169.32 | 5 085.71 | 5 131.08 |
| 194 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.2) | 4 667.47 | 4 624.89 | 4 571.82 |
| 195 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 452.32 | 4 410.98 | 4 368.42 |
| 196 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 431.71 | 4 408.68 | 4 419.38 |
| 197 | python (3.9)| [masonite](https://masoniteproject.com) (3.0) | 4 291.86 | 5 446.35 | 5 447.11 |
| 198 | javascript (14.15)| [sails](https://sailsjs.com) (1.4) | 4 277.49 | 4 265.59 | 4 217.23 |
| 199 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 3 959.00 | 2 440.78 | 2 138.50 |
| 200 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.3) | 3 952.82 | 3 891.57 | 3 885.42 |
| 201 | php (7.4)| [nette](https://nette.org/en/) (3.1) | 3 877.19 | 3 859.31 | 3 883.00 |
| 202 | php (7.4)| [lumen](https://lumen.laravel.com) (8.2) | 3 832.00 | 3 837.56 | 3 875.59 |
| 203 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.5) | 3 788.71 | 3 741.84 | 3 741.70 |
| 204 | julia (1.6)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 719.08 | 6 887.57 | 5 818.90 |
| 205 | php (7.4)| [fatfree](https://fatfreeframework.com) (3.7) | 3 459.31 | 3 423.91 | 3 535.06 |
| 206 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 3 425.55 | 3 386.80 | 3 393.98 |
| 207 | php (7.4)| [symfony](https://symfony.com) (5.2) | 2 970.38 | 2 978.52 | 3 001.28 |
| 208 | ruby (2.7)| [rails](https://rubyonrails.org) (6.1) | 2 771.85 | 2 753.69 | 2 752.84 |
| 209 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 616.21 | 2 603.03 | 2 621.75 |
| 210 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 329.16 | 2 522.57 | 2 499.96 |
| 211 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.5) | 2 289.04 | 2 283.24 | 2 304.33 |
| 212 | php (7.4)| [antidot](https://antidotfw.io) (0.1) | 2 050.66 | 1 286.17 | 1 476.03 |
| 213 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 919.85 | 1 840.29 | 1 764.49 |
| 214 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 901.78 | 1 843.60 | 1 833.37 |
| 215 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.3) | 1 855.59 | 1 851.46 | 1 862.01 |
| 216 | php (7.4)| [driftphp](https://github.com/driftphp/driftphp) (0.1) | 1 825.75 | 1 787.27 | 1 393.16 |
| 217 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 1 825.22 | 1 863.30 | 1 863.64 |
| 218 | python (3.9)| [cyclone](https://cyclone.io) (1.3) | 1 660.80 | 1 649.06 | 1 629.83 |
| 219 | python (3.9)| [klein](https://github.com/twisted/klein) (20.6) | 1 628.36 | 1 633.91 | 1 603.63 |
| 220 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 568.85 | 1 580.88 | 1 587.50 |
| 221 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 273.97 | 645.48 | 414.15 |
| 222 | python (3.9)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 250.42 | 1 183.75 | 1 181.04 |
| 223 | python (3.9)| [django-ninja](https://django-ninja.rest-framework.com) (0.10) | 1 183.19 | 1 534.80 | 1 563.20 |
| 224 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 094.84 | 1 110.59 | 1 111.20 |
| 225 | php (7.4)| [laravel](https://laravel.com) (8.25) | 901.89 | 907.54 | 905.11 |
| 226 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 285.83 | 301.38 | -90.22 |
| 227 | cpp (11)| [nawa](https://github.com/jatofg/nawa) (0.6) | 261.89 | NaN | NaN |
</a>

</details>

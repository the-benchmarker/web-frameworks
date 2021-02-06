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

## Results (2021-02-04)



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
| 1 | java (11)| [activej](https://activej.io) (3.0) | 172 233.04 | 214 764.89 | 219 395.10 |
| 2 | php (7.4)| [webman](https://github.com/walkor/webman) (1.0) | 159 515.82 | 169 693.28 | 172 189.10 |
| 3 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 151 080.90 | 182 392.92 | 184 424.30 |
| 4 | go (1.15)| [fiber](https://gofiber.io) (2.4) | 122 918.90 | 128 858.23 | 127 813.73 |
| 5 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 122 477.06 | 134 933.20 | 136 032.90 |
| 6 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 120 696.67 | 124 005.36 | 123 462.57 |
| 7 | c (11)| [kore](https://kore.io) (3.3) | 120 045.54 | 191 531.60 | 192 946.99 |
| 8 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.6) | 118 079.31 | 128 901.77 | 128 077.46 |
| 9 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 117 748.97 | 146 282.24 | 149 046.77 |
| 10 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 116 922.74 | 128 462.70 | 128 094.25 |
| 11 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.19) | 116 812.14 | 130 597.63 | 131 390.59 |
| 12 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 116 486.89 | 129 605.94 | 128 605.28 |
| 13 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 114 184.20 | 142 275.25 | 145 006.82 |
| 14 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 113 200.47 | 112 433.04 | 115 683.18 |
| 15 | java (11)| [undertow](https://undertow.io) (2.2) | 113 164.64 | 138 236.66 | 139 816.62 |
| 16 | clojure (1.1)| [donkey](https://github.com/AppsFlyer/donkey) (0.4) | 110 612.58 | 114 123.93 | 114 978.62 |
| 17 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 110 287.59 | 134 683.40 | 137 629.10 |
| 18 | java (11)| [jooby](https://jooby.io) (2.9) | 109 821.72 | 138 035.70 | 142 242.18 |
| 19 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 109 337.49 | 136 212.87 | 141 140.21 |
| 20 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 108 641.78 | 105 079.28 | 109 486.49 |
| 21 | javascript (14.15)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.4) | 107 926.79 | 141 851.29 | 144 605.61 |
| 22 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 107 271.55 | 133 077.40 | 137 633.71 |
| 23 | javascript (14.15)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 107 093.50 | 137 451.67 | 140 930.21 |
| 24 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 105 308.04 | 131 281.27 | 134 505.78 |
| 25 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 99 331.99 | 121 522.90 | 122 215.48 |
| 26 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 99 240.07 | 121 885.34 | 122 054.04 |
| 27 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.4) | 98 729.87 | 119 920.47 | 120 528.54 |
| 28 | php (7.4)| [swoole](https://github.com/swoole/swoole-src) (4.6) | 95 781.92 | 123 041.14 | 132 972.20 |
| 29 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 95 168.73 | 139 489.69 | 151 513.41 |
| 30 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 94 934.57 | 116 700.89 | 119 047.49 |
| 31 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 94 888.55 | 113 948.20 | 113 322.94 |
| 32 | crystal (0.35)| [kemal](https://kemalcr.com) (0.27) | 93 690.93 | 111 422.63 | 111 702.86 |
| 33 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 90 384.31 | 106 987.76 | 105 762.44 |
| 34 | php (7.4)| [swoole-coroutine](https://github.com/swoole/swoole-src) (4.6) | 89 171.01 | 124 649.75 | 136 859.75 |
| 35 | java (11)| [quarkus](https://quarkus.io) (1.11) | 85 669.66 | 103 619.42 | 106 197.27 |
| 36 | scala (2.13)| [akkahttp](https://akka.io) (10.2) | 83 266.20 | 98 161.19 | 95 204.35 |
| 37 | javascript (14.15)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 82 750.05 | 97 965.11 | 99 787.73 |
| 38 | go (1.15)| [gf](https://goframe.org) (1.15) | 82 468.71 | 89 820.40 | 91 926.22 |
| 39 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.12) | 81 249.97 | 92 799.52 | 88 870.08 |
| 40 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 80 963.60 | 82 065.60 | 84 086.74 |
| 41 | go (1.15)| [clevergo](https://clevergo.tech) (0.5) | 80 913.00 | 81 778.04 | 83 941.18 |
| 42 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 80 491.62 | 127 099.12 | 101 423.46 |
| 43 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 80 390.36 | 81 291.00 | 83 261.15 |
| 44 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 80 201.20 | 81 060.13 | 83 073.35 |
| 45 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 436.19 | 82 902.60 | 84 222.71 |
| 46 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 78 231.87 | 81 899.05 | 83 151.96 |
| 47 | go (1.15)| [chi](https://github.com/go-chi/chi) (1.5) | 77 560.53 | 77 085.05 | 79 479.18 |
| 48 | go (1.15)| [violetear](https://violetear.org) (7.0) | 77 482.79 | 77 700.81 | 79 831.99 |
| 49 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 75 632.14 | 88 621.42 | 91 467.30 |
| 50 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 75 430.70 | 74 774.00 | 77 240.46 |
| 51 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 75 345.21 | 79 328.96 | 79 996.85 |
| 52 | fsharp (5.0)| [falco](https://falcoframework.com) (3.0) | 75 039.40 | 87 879.30 | 90 739.67 |
| 53 | scala (2.13)| [finatra](https://twitter.github.io/finatra/) (21.1) | 74 947.35 | 86 888.51 | 87 435.90 |
| 54 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 73 368.06 | 71 437.26 | 74 539.68 |
| 55 | python (3.9)| [falcon](https://falconframework.org) (2.0) | 73 359.62 | 81 115.35 | 82 201.82 |
| 56 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 71 881.62 | 83 390.02 | 85 916.74 |
| 57 | go (1.15)| [beego](https://beego.me) (1.12) | 71 541.97 | 74 205.78 | 76 253.91 |
| 58 | javascript (14.15)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 70 668.86 | 78 996.01 | 79 673.94 |
| 59 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 66 036.69 | 73 470.67 | 74 118.29 |
| 60 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.3) | 65 261.81 | 66 167.29 | 65 021.08 |
| 61 | go (1.15)| [air](https://github.com/aofei/air) (0.21) | 64 970.23 | 63 461.37 | 65 862.00 |
| 62 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.6) | 62 694.01 | 62 578.79 | 65 215.36 |
| 63 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 61 689.80 | 64 413.45 | 65 805.93 |
| 64 | scala (2.13)| [finch](https://finagle.github.io/finch/) (0.32) | 61 476.56 | 66 836.85 | 64 762.48 |
| 65 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 61 431.47 | 66 932.43 | 66 938.66 |
| 66 | rust (1.49)| [salvo](https://github.com/kenorld/salvo) (0.5) | 58 322.33 | 62 429.57 | 63 397.80 |
| 67 | javascript (14.15)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 58 247.04 | 66 942.64 | 68 763.03 |
| 68 | javascript (14.15)| [0http](https://github.com/jkyberneees/0http) (3.0) | 57 403.35 | 64 465.21 | 66 038.39 |
| 69 | javascript (14.15)| [polka](https://github.com/lukeed/polka) (0.5) | 57 163.96 | 62 302.79 | 61 584.30 |
| 70 | javascript (14.15)| [restana](https://github.com/jkyberneees/ana) (4.7) | 57 012.85 | 63 570.65 | 64 770.28 |
| 71 | javascript (14.15)| [rayo](https://rayo.js.org) (1.3) | 56 020.68 | 60 961.07 | 59 652.01 |
| 72 | javascript (14.15)| [fastify](https://fastify.io) (3.11) | 55 174.70 | 59 675.83 | 58 232.98 |
| 73 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 54 372.11 | 57 467.52 | 56 217.05 |
| 74 | javascript (14.15)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 54 185.11 | 58 303.01 | 57 316.16 |
| 75 | python (3.9)| [pyramid](https://trypyramid.com) (1.1) | 53 201.51 | 58 353.72 | 58 480.24 |
| 76 | php (7.4)| [one](https://github.com/lizhichao/one) (2.2) | 53 066.47 | 63 443.56 | 69 640.64 |
| 77 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 52 732.13 | 61 285.99 | 62 826.08 |
| 78 | php (7.4)| [hyperf](https://hyperf.io) (2.1) | 52 024.00 | 59 102.40 | 67 205.17 |
| 79 | java (11)| [micronaut](https://micronaut.io) (1.2) | 51 815.68 | 58 638.14 | 58 790.46 |
| 80 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 51 753.17 | 67 319.13 | 69 617.37 |
| 81 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.14) | 51 525.35 | 73 462.49 | 81 781.48 |
| 82 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 50 519.89 | 57 359.17 | 57 467.81 |
| 83 | java (11)| [spark](https://sparkjava.com) (2.9) | 50 088.86 | 54 817.23 | 55 886.64 |
| 84 | java (11)| [javalin](https://javalin.io) (3.9) | 50 082.52 | 54 214.78 | 54 698.02 |
| 85 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 49 822.70 | 55 193.87 | 61 917.26 |
| 86 | rust (1.49)| [actix](https://actix.rs) (3.3) | 49 766.49 | 47 160.84 | 50 318.63 |
| 87 | javascript (14.15)| [nestjs-fastify](https://nestjs.com) (7.6) | 48 340.61 | 52 422.19 | 50 507.00 |
| 88 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 46 585.32 | 49 023.10 | 51 900.47 |
| 89 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 45 987.35 | 46 329.42 | 49 660.21 |
| 90 | javascript (14.15)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 45 152.83 | 48 106.37 | 46 864.21 |
| 91 | javascript (14.15)| [foxify](https://foxify.js.org) (0.1) | 45 028.15 | 48 749.10 | 47 791.62 |
| 92 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 580.59 | 45 720.40 | 45 976.64 |
| 93 | php (7.4)| [comet](https://github.com/gotzmann/comet) (1.1) | 44 132.99 | 47 824.32 | 48 867.93 |
| 94 | python (3.9)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 43 953.11 | 50 321.93 | 51 431.70 |
| 95 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 43 473.77 | 34 188.14 | 32 221.24 |
| 96 | python (3.9)| [bottle](https://bottlepy.org) (0.12) | 42 508.73 | 45 514.38 | 46 179.70 |
| 97 | javascript (14.15)| [koa](https://koajs.com) (2.13) | 41 837.34 | 44 832.52 | 43 663.66 |
| 98 | javascript (14.15)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 37 128.22 | 39 230.66 | 38 930.30 |
| 99 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 36 582.03 | 37 987.19 | 37 839.95 |
| 100 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 36 061.17 | 37 615.10 | 38 019.19 |
| 101 | java (11)| [restheart](https://restheart.org) (5.3) | 35 991.95 | 36 237.79 | 36 253.87 |
| 102 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 35 846.04 | 36 201.72 | 35 730.07 |
| 103 | javascript (14.15)| [hapi](https://hapijs.com) (20.1) | 34 847.04 | 35 354.84 | 34 562.05 |
| 104 | swift (5.3)| [vapor](https://vapor.codes) (4.39) | 34 839.37 | 36 889.67 | 36 668.16 |
| 105 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 34 792.55 | 35 433.17 | 35 653.84 |
| 106 | javascript (14.15)| [moleculer](https://moleculer.services) (0.14) | 34 723.46 | 35 991.95 | 35 389.62 |
| 107 | python (3.9)| [emmett](https://emmett.sh) (2.1) | 34 247.86 | 41 769.25 | 42 296.56 |
| 108 | java (11)| [jersey3-grizzly2](https://eclipse-ee4j.github.io/jersey) (3) | 34 052.62 | 39 241.32 | 38 411.95 |
| 109 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 33 848.33 | 34 961.63 | 33 048.72 |
| 110 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 33 807.62 | 44 448.42 | 47 038.74 |
| 111 | rust (1.49)| [nickel](https://nickel-org.github.io) (0.11) | 33 756.37 | 35 544.28 | 35 485.16 |
| 112 | python (3.9)| [hug](https://hug.rest) (2.6) | 33 702.92 | 35 967.24 | 53 963.80 |
| 113 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 33 393.95 | 38 730.93 | 38 662.15 |
| 114 | java (11)| [jersey-grizzly2](https://eclipse-ee4j.github.io/jersey) (2.33) | 33 306.49 | 38 033.14 | 37 493.77 |
| 115 | python (3.9)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 33 218.93 | 38 497.65 | 39 381.11 |
| 116 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 33 183.42 | 29 106.05 | 25 916.70 |
| 117 | rust (1.49)| [gotham](https://gotham.rs) (0.5) | 31 947.22 | 35 358.19 | 36 375.60 |
| 118 | python (3.9)| [sanic](https://github.com/huge-success/sanic) (20.12) | 31 533.05 | 34 584.11 | 35 021.08 |
| 119 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 31 482.14 | 30 829.95 | 30 857.39 |
| 120 | javascript (14.15)| [restify](https://restify.com) (8.5) | 29 357.22 | 30 196.22 | 29 395.67 |
| 121 | php (7.4)| [imi](https://imiphp.com) (1.2) | 29 176.56 | 32 314.08 | 34 509.52 |
| 122 | python (3.9)| [starlette](https://starlette.io) (0.14) | 29 124.80 | 31 834.08 | 32 197.34 |
| 123 | python (3.9)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.3) | 29 060.86 | 33 570.11 | 34 329.77 |
| 124 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 29 055.29 | 31 870.57 | 31 853.32 |
| 125 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 28 635.78 | 30 110.71 | 32 572.62 |
| 126 | javascript (14.15)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 28 512.82 | 30 410.72 | 30 554.62 |
| 127 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 27 150.72 | 29 240.00 | 28 821.11 |
| 128 | scala (2.13)| [play](https://playframework.com) (2.8) | 26 990.22 | 27 626.53 | 27 422.22 |
| 129 | python (3.9)| [responder](https://python-responder.org) (2.0) | 25 449.81 | 31 886.24 | 32 301.87 |
| 130 | python (3.9)| [index.py](https://index-py.abersheeran.com) (0.16) | 24 610.55 | 26 635.34 | 27 111.58 |
| 131 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 24 044.52 | 24 180.10 | 21 704.35 |
| 132 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 23 989.72 | 26 166.42 | 25 257.34 |
| 133 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 23 178.28 | 21 510.93 | 20 344.83 |
| 134 | python (3.9)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 110.96 | 28 408.47 | 28 114.62 |
| 135 | clojure (1.1)| [luminus](https://luminusweb.com) (3.95) | 22 255.15 | 21 661.86 | 21 086.46 |
| 136 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.16) | 22 041.91 | 21 262.95 | 19 827.80 |
| 137 | javascript (14.15)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 21 604.46 | 23 735.75 | 23 186.66 |
| 138 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 21 468.02 | 21 582.06 | 21 220.95 |
| 139 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 20 901.14 | 20 422.59 | 19 703.38 |
| 140 | python (3.9)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 19 505.15 | 22 029.47 | 22 648.33 |
| 141 | javascript (14.15)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 18 680.35 | 17 800.01 | 16 754.94 |
| 142 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 683.93 | 15 815.31 | 14 680.96 |
| 143 | python (3.9)| [molten](https://moltenframework.com) (1.0) | 17 282.10 | 21 324.12 | 21 196.07 |
| 144 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 16 908.29 | 16 649.86 | 16 204.28 |
| 145 | php (7.4)| [swoft](https://swoft.org) (2.0) | 16 896.17 | 20 345.24 | 20 767.38 |
| 146 | python (3.9)| [fastapi](https://fastapi.tiangolo.com) (0.63) | 16 690.43 | 21 748.55 | 22 123.71 |
| 147 | javascript (14.15)| [express](https://expressjs.com) (4.17) | 16 634.07 | 18 860.87 | 17 396.01 |
| 148 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 16 463.03 | 16 006.11 | 15 636.91 |
| 149 | rust (1.49)| [iron](https://ironframework.io) (0.6) | 16 370.41 | 16 618.18 | 16 580.75 |
| 150 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 16 137.83 | 14 193.44 | 13 228.69 |
| 151 | javascript (14.15)| [feathersjs](https://feathersjs.com) (4.5) | 16 108.66 | 17 377.16 | 17 702.12 |
| 152 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.40) | 15 670.56 | 15 288.13 | 14 986.86 |
| 153 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 15 628.47 | 18 050.52 | 19 017.27 |
| 154 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 513.98 | 18 231.18 | 18 438.62 |
| 155 | go (1.15)| [macaron](https://go-macaron.com) (1.4) | 15 389.32 | 16 916.05 | 16 893.93 |
| 156 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 15 223.45 | 17 662.99 | 17 670.00 |
| 157 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 15 115.29 | 14 674.47 | 14 401.78 |
| 158 | python (3.9)| [flask](https://flask.pocoo.org) (1.1) | 14 529.51 | 15 531.08 | 17 423.68 |
| 159 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 14 054.15 | 14 193.08 | 20 857.84 |
| 160 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 13 840.31 | 13 407.90 | 13 264.08 |
| 161 | java (11)| [struts2](https://struts.apache.org) (2.5) | 13 667.58 | 14 003.54 | 14 036.04 |
| 162 | ruby (2.7)| [rack_app](https://rack-app.com) (7.7) | 13 500.91 | 13 064.22 | 12 876.57 |
| 163 | java (11)| [blade](https://lets-blade.com) (2.0) | 13 073.34 | 16 141.81 | 14 700.05 |
| 164 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 12 769.97 | 12 301.33 | 12 224.19 |
| 165 | javascript (14.15)| [nestjs-express](https://nestjs.com) (7.6) | 12 272.76 | 14 427.79 | 11 242.86 |
| 166 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 452.54 | 11 733.42 | 11 785.43 |
| 167 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 11 257.77 | 10 987.35 | 10 564.28 |
| 168 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.6) | 10 183.43 | 10 432.34 | 10 448.87 |
| 169 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 10 143.42 | 10 331.43 | 10 418.90 |
| 170 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 906.43 | 10 073.64 | 10 100.45 |
| 171 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 9 736.57 | 9 979.11 | 10 137.98 |
| 172 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.7) | 9 638.02 | 9 804.36 | 9 807.60 |
| 173 | php (7.4)| [laravel-s-lumen](https://github.com/hhxsv5/laravel-s) (3.7) | 9 325.79 | 10 072.66 | 9 955.27 |
| 174 | python (3.9)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 8 885.97 | 8 892.74 | 8 691.07 |
| 175 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.1) | 8 785.51 | 8 595.60 | 8 607.90 |
| 176 | ruby (2.7)| [grape](https://ruby-grape.org) (1.5) | 8 573.74 | 8 365.43 | 8 318.50 |
| 177 | python (3.9)| [guillotina](https://guillotina.io) (6.1) | 8 269.50 | 8 093.52 | 7 915.44 |
| 178 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 8 094.30 | 13 395.74 | 12 464.96 |
| 179 | python (3.9)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 7 569.83 | 7 283.18 | 6 808.85 |
| 180 | python (3.9)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 470.19 | 7 119.86 | 6 558.64 |
| 181 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 6 767.61 | 6 715.61 | 6 654.93 |
| 182 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 568.76 | 6 497.16 | 6 450.14 |
| 183 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 6 478.86 | 6 434.58 | 6 308.41 |
| 184 | php (7.4)| [laravel-s-laravel](https://github.com/hhxsv5/laravel-s) (3.7) | 6 442.41 | 6 482.54 | 6 349.77 |
| 185 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 6 340.09 | 6 290.26 | 6 174.76 |
| 186 | python (3.9)| [tornado](https://tornadoweb.org) (6.1) | 6 306.45 | 6 301.04 | 6 302.44 |
| 187 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 6 205.80 | 6 966.21 | 7 022.06 |
| 188 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 188.48 | 6 120.19 | 6 087.71 |
| 189 | python (3.9)| [django](https://djangoproject.com) (3.1) | 5 812.33 | 5 813.01 | 5 798.43 |
| 190 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 5 680.70 | 5 595.86 | 5 583.20 |
| 191 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 5 242.71 | 5 182.36 | 5 114.12 |
| 192 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 5 035.02 | 4 970.50 | 5 027.51 |
| 193 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.2) | 4 649.13 | 4 611.82 | 4 548.00 |
| 194 | php (7.4)| [fatfree](https://fatfreeframework.com) (3.7) | 4 590.35 | 4 553.17 | 4 501.82 |
| 195 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 456.64 | 4 411.75 | 4 384.31 |
| 196 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 413.38 | 4 383.07 | 4 400.47 |
| 197 | javascript (14.15)| [sails](https://sailsjs.com) (1.4) | 4 253.47 | 4 246.02 | 4 170.44 |
| 198 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.3) | 3 943.28 | 3 880.27 | 3 872.47 |
| 199 | php (7.4)| [nette](https://nette.org/en/) (3.1) | 3 885.84 | 3 871.66 | 3 893.87 |
| 200 | php (7.4)| [lumen](https://lumen.laravel.com) (8.2) | 3 810.12 | 3 825.83 | 3 852.78 |
| 201 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.6) | 3 786.19 | 3 740.91 | 3 730.34 |
| 202 | julia (1.6)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 658.64 | 6 846.95 | 5 740.10 |
| 203 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 3 425.91 | 3 394.08 | 3 384.56 |
| 204 | php (7.4)| [symfony](https://symfony.com) (5.2) | 2 962.69 | 2 973.19 | 2 984.66 |
| 205 | ruby (2.7)| [rails](https://rubyonrails.org) (6.1) | 2 725.42 | 2 724.89 | 2 718.83 |
| 206 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 600.11 | 2 587.94 | 2 608.85 |
| 207 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 428.33 | 2 493.62 | 2 492.57 |
| 208 | python (3.9)| [masonite](https://masoniteproject.com) (3.0) | 2 412.47 | 2 494.39 | 2 489.92 |
| 209 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.6) | 2 241.32 | 2 237.04 | 2 250.93 |
| 210 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 935.62 | 1 843.38 | 1 844.82 |
| 211 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 921.95 | 1 835.89 | 1 763.46 |
| 212 | php (7.4)| [driftphp](https://github.com/driftphp/driftphp) (0.1) | 1 896.56 | 662.15 | 1 663.33 |
| 213 | php (7.4)| [antidot](https://antidotfw.io) (0.1) | 1 892.96 | 662.34 | 1 542.27 |
| 214 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 1 772.80 | 1 779.32 | 1 754.53 |
| 215 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.3) | 1 749.18 | 1 739.68 | 1 756.93 |
| 216 | python (3.9)| [cyclone](https://cyclone.io) (1.3) | 1 644.78 | 1 637.21 | 1 622.89 |
| 217 | python (3.9)| [klein](https://github.com/twisted/klein) (20.6) | 1 633.55 | 1 641.21 | 1 602.80 |
| 218 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 494.24 | 1 510.74 | 1 516.79 |
| 219 | python (3.9)| [django-ninja](https://django-ninja.rest-framework.com) (0.10) | 1 347.75 | 1 625.56 | 1 657.76 |
| 220 | python (3.9)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 234.57 | 1 172.40 | 1 177.99 |
| 221 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.1) | 1 089.40 | 1 107.47 | 1 102.25 |
| 222 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 008.68 | 609.75 | 390.51 |
| 223 | php (7.4)| [laravel](https://laravel.com) (8.26) | 891.39 | 899.26 | 899.75 |
| 224 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 286.22 | 302.43 | -92.88 |
| 225 | cpp (11)| [nawa](https://github.com/jatofg/nawa) (0.6) | 262.58 | NaN | NaN |
</a>

</details>

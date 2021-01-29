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

## Results (2021-01-29)



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
| 1 | java (11)| [activej](https://activej.io) (3.0) | 172 589.71 | 214 442.40 | 219 497.79 |
| 2 | php (7.4)| [webman](https://github.com/walkor/webman) (1.0) | 156 997.93 | 168 186.85 | 171 132.92 |
| 3 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 149 224.89 | 181 997.87 | 184 237.89 |
| 4 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 122 504.46 | 135 292.20 | 136 549.05 |
| 5 | go (1.15)| [fiber](https://gofiber.io) (2.3) | 121 394.47 | 130 042.30 | 129 250.30 |
| 6 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 120 962.52 | 123 826.76 | 123 334.41 |
| 7 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 117 370.12 | 146 058.44 | 148 892.00 |
| 8 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.6) | 117 042.92 | 128 784.27 | 128 121.89 |
| 9 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.19) | 116 412.42 | 130 381.29 | 130 600.50 |
| 10 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 116 344.36 | 128 722.06 | 128 017.68 |
| 11 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 116 339.40 | 128 230.51 | 127 760.34 |
| 12 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 114 525.13 | 141 543.00 | 144 708.24 |
| 13 | java (11)| [undertow](https://undertow.io) (2.2) | 112 933.69 | 138 182.20 | 140 091.83 |
| 14 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 112 931.28 | 112 150.61 | 115 256.06 |
| 15 | clojure (1.1)| [donkey](https://github.com/AppsFlyer/donkey) (0.4) | 111 425.99 | 114 167.27 | 115 537.26 |
| 16 | javascript (14.15)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.4) | 110 373.50 | 141 798.78 | 145 491.38 |
| 17 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 109 950.40 | 136 691.44 | 141 841.53 |
| 18 | java (11)| [jooby](https://jooby.io) (2.9) | 109 604.18 | 137 420.72 | 142 376.95 |
| 19 | javascript (14.15)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 108 915.27 | 137 693.56 | 140 564.67 |
| 20 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 108 209.17 | 104 778.26 | 109 328.09 |
| 21 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 107 541.83 | 134 134.71 | 138 088.64 |
| 22 | c (11)| [kore](https://kore.io) (3.3) | 107 316.81 | 186 537.59 | 187 148.73 |
| 23 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 107 088.85 | 132 524.63 | 135 075.74 |
| 24 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 106 302.24 | 131 795.97 | 135 018.70 |
| 25 | crystal (0.35)| [kemal](https://kemalcr.com) (0.27) | 103 864.55 | 126 245.81 | 125 706.82 |
| 26 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 99 905.65 | 121 190.71 | 121 846.92 |
| 27 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 98 051.02 | 120 732.08 | 120 663.68 |
| 28 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.4) | 97 515.60 | 119 460.20 | 119 870.17 |
| 29 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 95 201.92 | 116 648.41 | 119 585.86 |
| 30 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 94 562.98 | 138 982.40 | 149 135.62 |
| 31 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 93 355.55 | 113 176.74 | 112 883.61 |
| 32 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 89 004.19 | 105 895.82 | 105 380.70 |
| 33 | java (11)| [quarkus](https://quarkus.io) (1.11) | 86 672.84 | 105 129.80 | 107 837.20 |
| 34 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.12) | 83 167.28 | 93 706.89 | 89 079.25 |
| 35 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 82 309.38 | 106 469.02 | 112 819.05 |
| 36 | go (1.15)| [gf](https://goframe.org) (1.15) | 82 254.43 | 89 573.23 | 91 932.58 |
| 37 | scala (2.13)| [akkahttp](https://akka.io) (10.2) | 82 236.16 | 94 492.64 | 92 946.60 |
| 38 | javascript (14.15)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 81 862.86 | 97 805.46 | 99 196.81 |
| 39 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 80 829.26 | 81 719.89 | 83 894.23 |
| 40 | go (1.15)| [clevergo](https://clevergo.tech) (0.5) | 80 676.57 | 81 895.98 | 83 790.98 |
| 41 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 80 436.96 | 84 232.00 | 83 256.86 |
| 42 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 80 131.55 | 81 198.97 | 83 167.27 |
| 43 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 79 940.17 | 80 774.63 | 82 875.29 |
| 44 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 092.85 | 82 598.05 | 83 810.96 |
| 45 | go (1.15)| [chi](https://github.com/go-chi/chi) (1.5) | 77 361.13 | 76 755.02 | 79 357.42 |
| 46 | go (1.15)| [violetear](https://violetear.org) (7.0) | 76 932.70 | 77 038.33 | 79 175.22 |
| 47 | scala (2.13)| [finatra](https://twitter.github.io/finatra/) (21.1) | 76 207.28 | 87 869.26 | 88 596.85 |
| 48 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 75 205.79 | 79 058.90 | 79 545.21 |
| 49 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 75 116.73 | 74 595.54 | 76 849.36 |
| 50 | php (7.4)| [swoole-coroutine](https://github.com/swoole/swoole-src) (4.6) | 74 610.71 | 103 008.26 | 110 786.61 |
| 51 | python (3.9)| [falcon](https://falconframework.org) (2.0) | 73 517.52 | 80 805.20 | 82 389.17 |
| 52 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 73 375.97 | 71 681.07 | 74 551.15 |
| 53 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 73 212.26 | 85 354.42 | 87 927.01 |
| 54 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 72 737.76 | 84 103.30 | 86 570.71 |
| 55 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.3) | 72 459.85 | 68 488.02 | 65 168.61 |
| 56 | fsharp (5.0)| [falco](https://falcoframework.com) (3.0) | 71 984.00 | 83 245.13 | 85 691.80 |
| 57 | go (1.15)| [beego](https://beego.me) (1.12) | 71 598.91 | 74 512.60 | 76 457.98 |
| 58 | javascript (14.15)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 70 068.86 | 78 339.95 | 79 288.19 |
| 59 | go (1.15)| [air](https://github.com/aofei/air) (0.21) | 64 855.29 | 63 461.15 | 65 940.42 |
| 60 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 64 848.41 | 72 361.37 | 72 455.58 |
| 61 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 64 480.67 | 130 606.88 | 131 105.97 |
| 62 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.6) | 62 416.93 | 62 350.10 | 65 072.42 |
| 63 | php (7.4)| [swoole](https://github.com/swoole/swoole-src) (4.6) | 62 026.63 | 81 004.46 | 94 290.33 |
| 64 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 61 410.16 | 67 055.75 | 66 523.01 |
| 65 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 61 268.55 | 64 563.49 | 65 569.73 |
| 66 | scala (2.13)| [finch](https://finagle.github.io/finch/) (0.32) | 61 032.14 | 67 906.61 | 67 916.01 |
| 67 | javascript (14.15)| [0http](https://github.com/jkyberneees/0http) (3.0) | 60 485.31 | 66 077.49 | 67 352.62 |
| 68 | javascript (14.15)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 60 463.03 | 68 110.62 | 69 343.53 |
| 69 | javascript (14.15)| [polka](https://github.com/lukeed/polka) (0.5) | 57 364.10 | 61 878.15 | 60 915.53 |
| 70 | javascript (14.15)| [restana](https://github.com/jkyberneees/ana) (4.7) | 56 285.77 | 63 629.16 | 64 685.16 |
| 71 | javascript (14.15)| [rayo](https://rayo.js.org) (1.3) | 55 892.66 | 60 803.79 | 59 631.89 |
| 72 | javascript (14.15)| [fastify](https://fastify.io) (3.11) | 55 050.58 | 59 870.80 | 58 507.46 |
| 73 | php (7.4)| [one](https://github.com/lizhichao/one) (2.2) | 53 958.83 | 61 636.75 | 69 008.75 |
| 74 | python (3.9)| [pyramid](https://trypyramid.com) (1.1) | 53 740.39 | 58 424.75 | 58 685.70 |
| 75 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 53 723.86 | 56 561.25 | 54 950.46 |
| 76 | javascript (14.15)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 53 496.99 | 59 024.72 | 57 195.14 |
| 77 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 52 720.85 | 60 967.91 | 63 449.53 |
| 78 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 51 887.93 | 57 096.20 | 64 042.22 |
| 79 | java (11)| [javalin](https://javalin.io) (3.9) | 51 162.92 | 54 620.13 | 54 822.06 |
| 80 | rust (1.49)| [salvo](https://github.com/kenorld/salvo) (0.5) | 51 140.95 | 54 865.15 | 55 695.24 |
| 81 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.14) | 50 993.19 | 74 337.64 | 82 949.52 |
| 82 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 50 883.40 | 66 456.90 | 69 208.33 |
| 83 | php (7.4)| [hyperf](https://hyperf.io) (2.1) | 50 794.84 | 62 411.51 | 65 801.28 |
| 84 | java (11)| [micronaut](https://micronaut.io) (1.2) | 50 714.18 | 57 848.43 | 57 935.74 |
| 85 | java (11)| [spark](https://sparkjava.com) (2.9) | 50 093.82 | 54 710.98 | 55 788.62 |
| 86 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 49 956.01 | 56 325.57 | 56 614.73 |
| 87 | javascript (14.15)| [nestjs-fastify](https://nestjs.com) (7.6) | 48 550.94 | 51 914.97 | 50 777.31 |
| 88 | java (11)| [restheart](https://restheart.org) (5.1) | 47 151.61 | 47 941.85 | 48 384.54 |
| 89 | rust (1.49)| [actix](https://actix.rs) (3.3) | 46 455.86 | 48 790.64 | 50 260.57 |
| 90 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 45 901.30 | 46 407.73 | 49 420.69 |
| 91 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 45 595.63 | 48 886.98 | 51 140.12 |
| 92 | javascript (14.15)| [foxify](https://foxify.js.org) (0.1) | 45 319.32 | 49 052.73 | 47 794.71 |
| 93 | javascript (14.15)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 44 953.41 | 48 506.45 | 48 220.18 |
| 94 | php (7.4)| [comet](https://github.com/gotzmann/comet) (1.1) | 44 481.58 | 48 370.37 | 49 292.96 |
| 95 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 366.98 | 45 388.56 | 45 578.59 |
| 96 | python (3.9)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 44 053.83 | 50 981.72 | 52 195.88 |
| 97 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 42 924.80 | 33 060.19 | 31 363.37 |
| 98 | python (3.9)| [bottle](https://bottlepy.org) (0.12) | 42 039.63 | 45 416.36 | 46 292.85 |
| 99 | javascript (14.15)| [koa](https://koajs.com) (2.13) | 40 740.58 | 43 880.23 | 42 680.88 |
| 100 | javascript (14.15)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 37 089.76 | 39 711.52 | 38 869.10 |
| 101 | python (3.9)| [emmett](https://emmett.sh) (2.1) | 36 470.50 | 41 381.74 | 42 096.48 |
| 102 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 36 374.10 | 37 639.40 | 37 728.66 |
| 103 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 35 633.75 | 35 504.59 | 35 236.26 |
| 104 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 35 559.27 | 35 313.51 | 35 114.72 |
| 105 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 35 323.01 | 37 331.44 | 37 399.10 |
| 106 | swift (5.3)| [vapor](https://vapor.codes) (4.39) | 35 186.00 | 37 171.07 | 36 857.92 |
| 107 | javascript (14.15)| [moleculer](https://moleculer.services) (0.14) | 35 020.16 | 36 034.99 | 34 845.04 |
| 108 | javascript (14.15)| [hapi](https://hapijs.com) (20.1) | 34 720.09 | 35 399.65 | 34 291.74 |
| 109 | rust (1.49)| [nickel](https://nickel-org.github.io) (0.11) | 34 303.57 | 34 387.64 | 33 413.33 |
| 110 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 34 263.75 | 47 739.77 | 50 503.02 |
| 111 | java (11)| [jersey3-grizzly2](https://eclipse-ee4j.github.io/jersey) (3) | 34 090.50 | 39 263.52 | 38 463.93 |
| 112 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 33 885.45 | 34 797.56 | 33 330.33 |
| 113 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 33 533.38 | 37 973.77 | 38 485.53 |
| 114 | java (11)| [jersey-grizzly2](https://eclipse-ee4j.github.io/jersey) (2.33) | 33 360.87 | 38 253.38 | 37 868.85 |
| 115 | python (3.9)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 33 299.19 | 38 116.20 | 38 901.38 |
| 116 | python (3.9)| [hug](https://hug.rest) (2.6) | 32 320.72 | 35 030.44 | 38 244.91 |
| 117 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 32 203.85 | 27 981.56 | 24 314.50 |
| 118 | rust (1.49)| [gotham](https://gotham.rs) (0.5) | 31 762.49 | 35 443.69 | 36 447.23 |
| 119 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 31 644.72 | 31 405.05 | 31 268.85 |
| 120 | python (3.9)| [sanic](https://github.com/huge-success/sanic) (20.12) | 30 003.84 | 34 131.86 | 34 563.58 |
| 121 | javascript (14.15)| [restify](https://restify.com) (8.5) | 29 616.56 | 30 653.36 | 29 659.92 |
| 122 | python (3.9)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.3) | 29 468.49 | 33 399.14 | 33 754.60 |
| 123 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 28 915.37 | 31 916.21 | 31 832.49 |
| 124 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 28 211.64 | 31 710.17 | 33 121.05 |
| 125 | javascript (14.15)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 28 069.43 | 29 858.31 | 36 576.43 |
| 126 | scala (2.13)| [play](https://playframework.com) (2.8) | 27 532.17 | 29 590.84 | 29 351.84 |
| 127 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 26 940.01 | 29 182.05 | 28 576.79 |
| 128 | python (3.9)| [starlette](https://starlette.io) (0.14) | 26 887.21 | 31 396.69 | 31 801.70 |
| 129 | python (3.9)| [index.py](https://index-py.abersheeran.com) (0.16) | 26 635.48 | 29 902.95 | 30 478.02 |
| 130 | python (3.9)| [responder](https://python-responder.org) (2.0) | 24 653.94 | 30 564.10 | 30 813.06 |
| 131 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 24 346.19 | 24 212.30 | 21 558.85 |
| 132 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 24 212.85 | 26 182.21 | 25 650.26 |
| 133 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 22 914.78 | 21 693.14 | 20 727.36 |
| 134 | python (3.9)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 22 590.07 | 26 990.28 | 27 726.14 |
| 135 | clojure (1.1)| [luminus](https://luminusweb.com) (3.93) | 22 545.05 | 22 054.20 | 21 062.13 |
| 136 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 21 896.25 | 21 678.24 | 21 459.51 |
| 137 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.16) | 21 864.21 | 21 254.77 | 19 413.98 |
| 138 | javascript (14.15)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 21 181.17 | 22 864.11 | 22 390.58 |
| 139 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 20 998.57 | 20 391.71 | 20 598.65 |
| 140 | python (3.9)| [molten](https://moltenframework.com) (1.0) | 19 091.94 | 22 280.32 | 22 051.87 |
| 141 | python (3.9)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 18 925.50 | 23 578.97 | 24 026.62 |
| 142 | javascript (14.15)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 18 835.86 | 17 703.48 | 16 897.05 |
| 143 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 484.42 | 15 702.15 | 14 659.99 |
| 144 | python (3.9)| [fastapi](https://fastapi.tiangolo.com) (0.63) | 17 369.57 | 21 231.32 | 21 459.55 |
| 145 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 16 967.14 | 16 479.55 | 16 071.07 |
| 146 | php (7.4)| [swoft](https://swoft.org) (2.0) | 16 934.79 | 20 560.24 | 21 233.54 |
| 147 | rust (1.49)| [iron](https://ironframework.io) (0.6) | 16 691.65 | 16 566.53 | 16 762.41 |
| 148 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 16 509.54 | 16 000.41 | 15 803.50 |
| 149 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 16 174.34 | 14 317.32 | 13 250.65 |
| 150 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.40) | 15 870.30 | 15 572.23 | 15 247.29 |
| 151 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 15 691.09 | 18 616.90 | 18 871.77 |
| 152 | go (1.15)| [macaron](https://go-macaron.com) (1.4) | 15 405.92 | 16 902.72 | 16 930.58 |
| 153 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 264.13 | 17 869.44 | 17 772.38 |
| 154 | javascript (14.15)| [express](https://expressjs.com) (4.17) | 15 259.08 | 17 131.22 | 17 561.22 |
| 155 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 15 246.73 | 14 685.77 | 14 517.50 |
| 156 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 15 171.32 | 17 531.23 | 17 796.29 |
| 157 | javascript (14.15)| [feathersjs](https://feathersjs.com) (4.5) | 15 170.86 | 16 867.59 | 17 669.23 |
| 158 | python (3.9)| [flask](https://flask.pocoo.org) (1.1) | 14 118.51 | 16 711.65 | 15 738.56 |
| 159 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 13 903.44 | 13 518.25 | 13 255.68 |
| 160 | java (11)| [struts2](https://struts.apache.org) (2.5) | 13 902.76 | 14 194.49 | 14 235.03 |
| 161 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 13 657.37 | 13 930.53 | 17 664.51 |
| 162 | ruby (2.7)| [rack_app](https://rack-app.com) (7.7) | 13 634.37 | 13 177.94 | 12 982.29 |
| 163 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 13 157.12 | 12 734.07 | 12 586.07 |
| 164 | java (11)| [blade](https://lets-blade.com) (2.0) | 13 002.38 | 15 596.17 | 14 455.74 |
| 165 | javascript (14.15)| [nestjs-express](https://nestjs.com) (7.6) | 11 769.80 | 13 231.81 | 12 221.70 |
| 166 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 391.75 | 11 703.23 | 11 712.13 |
| 167 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 11 203.77 | 10 969.73 | 10 638.35 |
| 168 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.5) | 10 201.98 | 10 435.77 | 10 537.81 |
| 169 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 10 058.12 | 10 284.06 | 10 390.24 |
| 170 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 876.96 | 10 073.45 | 10 046.68 |
| 171 | python (3.9)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 872.89 | 9 654.66 | 9 526.53 |
| 172 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 9 772.43 | 9 977.40 | 10 086.13 |
| 173 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.7) | 9 589.50 | 9 720.91 | 9 818.53 |
| 174 | php (7.4)| [laravel-s-lumen](https://github.com/hhxsv5/laravel-s) (3.7) | 9 337.15 | 9 999.62 | 9 880.99 |
| 175 | python (3.9)| [guillotina](https://guillotina.io) (6.1) | 9 148.51 | 9 242.84 | 8 890.69 |
| 176 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.1) | 8 962.05 | 8 729.06 | 8 714.94 |
| 177 | ruby (2.7)| [grape](https://ruby-grape.org) (1.5) | 8 671.78 | 8 391.22 | 8 359.83 |
| 178 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 8 192.40 | 13 342.26 | 12 558.30 |
| 179 | python (3.9)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 454.06 | 7 084.91 | 6 538.34 |
| 180 | python (3.9)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 7 430.98 | 7 196.52 | 6 864.46 |
| 181 | php (7.4)| [imi](https://imiphp.com) (1.2) | 7 265.42 | 8 188.39 | 8 588.20 |
| 182 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 6 733.27 | 6 670.53 | 6 605.13 |
| 183 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 562.10 | 6 498.55 | 6 447.95 |
| 184 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 6 499.10 | 6 423.17 | 6 331.68 |
| 185 | php (7.4)| [laravel-s-laravel](https://github.com/hhxsv5/laravel-s) (3.7) | 6 456.81 | 6 435.08 | 6 393.87 |
| 186 | python (3.9)| [tornado](https://tornadoweb.org) (6.1) | 6 372.98 | 6 303.53 | 6 409.79 |
| 187 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 6 341.30 | 6 292.04 | 6 172.54 |
| 188 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 203.45 | 6 122.04 | 6 104.00 |
| 189 | python (3.9)| [django](https://djangoproject.com) (3.1) | 6 168.90 | 5 915.11 | 5 765.93 |
| 190 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 5 904.46 | 6 671.73 | 6 685.25 |
| 191 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 5 659.06 | 5 565.07 | 5 555.72 |
| 192 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 5 233.65 | 5 194.91 | 5 140.07 |
| 193 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 5 184.13 | 5 104.93 | 5 150.53 |
| 194 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.2) | 4 672.32 | 4 630.48 | 4 576.96 |
| 195 | python (3.9)| [masonite](https://masoniteproject.com) (3.0) | 4 615.09 | 6 038.56 | 6 039.07 |
| 196 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 452.91 | 4 407.50 | 4 362.95 |
| 197 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 434.34 | 4 409.25 | 4 416.38 |
| 198 | javascript (14.15)| [sails](https://sailsjs.com) (1.4) | 4 278.53 | 4 259.47 | 4 223.43 |
| 199 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 4 005.98 | 2 488.65 | 2 212.62 |
| 200 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.3) | 3 952.66 | 3 892.43 | 3 884.84 |
| 201 | php (7.4)| [nette](https://nette.org/en/) (3.1) | 3 877.45 | 3 861.28 | 3 883.52 |
| 202 | php (7.4)| [lumen](https://lumen.laravel.com) (8.2) | 3 832.73 | 3 836.87 | 3 874.13 |
| 203 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.5) | 3 789.83 | 3 742.42 | 3 741.07 |
| 204 | julia (1.6)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 722.78 | 6 875.45 | 5 823.67 |
| 205 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 3 427.00 | 3 389.11 | 3 392.61 |
| 206 | php (7.4)| [fatfree](https://fatfreeframework.com) (3.7) | 3 295.92 | 3 260.79 | 3 389.92 |
| 207 | php (7.4)| [symfony](https://symfony.com) (5.2) | 2 971.24 | 2 977.94 | 2 999.29 |
| 208 | ruby (2.7)| [rails](https://rubyonrails.org) (6.1) | 2 786.44 | 2 770.54 | 2 771.03 |
| 209 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 617.76 | 2 603.01 | 2 624.53 |
| 210 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.5) | 2 290.64 | 2 284.11 | 2 305.16 |
| 211 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 289.31 | 2 535.78 | 2 511.53 |
| 212 | php (7.4)| [antidot](https://antidotfw.io) (0.1) | 2 022.13 | 1 490.20 | 1 578.08 |
| 213 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 922.41 | 1 841.99 | 1 768.34 |
| 214 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 904.21 | 1 842.66 | 1 834.23 |
| 215 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 1 862.91 | 1 918.38 | 1 907.67 |
| 216 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.3) | 1 853.95 | 1 851.30 | 1 860.70 |
| 217 | php (7.4)| [driftphp](https://github.com/driftphp/driftphp) (0.1) | 1 825.75 | 1 787.27 | 1 393.16 |
| 218 | python (3.9)| [cyclone](https://cyclone.io) (1.3) | 1 658.93 | 1 650.32 | 1 628.34 |
| 219 | python (3.9)| [klein](https://github.com/twisted/klein) (20.6) | 1 626.11 | 1 630.94 | 1 601.69 |
| 220 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 569.41 | 1 580.39 | 1 586.97 |
| 221 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 257.57 | 636.88 | 400.15 |
| 222 | python (3.9)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 248.87 | 1 181.34 | 1 180.97 |
| 223 | python (3.9)| [django-ninja](https://django-ninja.rest-framework.com) (0.10) | 1 200.13 | 1 553.57 | 1 582.24 |
| 224 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 091.44 | 1 107.34 | 1 107.71 |
| 225 | php (7.4)| [laravel](https://laravel.com) (8.25) | 903.64 | 909.92 | 907.22 |
| 226 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 287.41 | 302.17 | -87.00 |
| 227 | cpp (11)| [nawa](https://github.com/jatofg/nawa) (0.6) | 260.63 | NaN | NaN |
</a>

</details>

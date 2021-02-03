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

## Results (2021-02-03)



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
| 1 | java (11)| [activej](https://activej.io) (3.0) | 172 633.05 | 214 422.85 | 219 445.13 |
| 2 | php (7.4)| [webman](https://github.com/walkor/webman) (1.0) | 158 473.96 | 168 872.82 | 171 597.43 |
| 3 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 149 244.79 | 182 043.82 | 184 509.86 |
| 4 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 122 716.88 | 135 480.31 | 136 911.52 |
| 5 | go (1.15)| [fiber](https://gofiber.io) (2.4) | 121 616.43 | 130 117.69 | 129 166.22 |
| 6 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 121 064.24 | 123 801.94 | 123 352.24 |
| 7 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 117 830.48 | 146 489.86 | 149 302.72 |
| 8 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.6) | 116 928.83 | 128 849.12 | 128 141.80 |
| 9 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.19) | 116 653.81 | 130 493.26 | 130 667.38 |
| 10 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 116 384.28 | 128 523.29 | 127 939.56 |
| 11 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 116 361.44 | 128 846.48 | 128 141.62 |
| 12 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 114 610.14 | 142 120.10 | 145 139.23 |
| 13 | java (11)| [undertow](https://undertow.io) (2.2) | 113 037.60 | 138 118.22 | 140 113.47 |
| 14 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 112 970.53 | 112 175.50 | 115 300.18 |
| 15 | clojure (1.1)| [donkey](https://github.com/AppsFlyer/donkey) (0.4) | 111 533.41 | 114 411.18 | 115 607.47 |
| 16 | javascript (14.15)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.4) | 110 554.72 | 142 033.60 | 145 517.08 |
| 17 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 109 910.93 | 136 500.33 | 141 641.63 |
| 18 | java (11)| [jooby](https://jooby.io) (2.9) | 109 638.74 | 137 411.44 | 142 422.88 |
| 19 | javascript (14.15)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 108 795.40 | 138 157.92 | 141 065.39 |
| 20 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 108 237.62 | 104 683.32 | 109 210.51 |
| 21 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 107 780.44 | 133 615.70 | 136 114.15 |
| 22 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 107 375.64 | 134 281.82 | 138 384.48 |
| 23 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 106 394.78 | 131 875.94 | 135 152.05 |
| 24 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 99 853.58 | 121 593.51 | 122 294.44 |
| 25 | crystal (0.35)| [kemal](https://kemalcr.com) (0.27) | 99 133.23 | 120 106.34 | 120 177.35 |
| 26 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.4) | 98 535.17 | 119 892.99 | 119 529.74 |
| 27 | c (11)| [kore](https://kore.io) (3.3) | 98 090.20 | 189 914.16 | 187 976.25 |
| 28 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 98 067.72 | 121 046.45 | 121 166.76 |
| 29 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 94 994.32 | 116 291.56 | 119 454.83 |
| 30 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 94 836.78 | 139 102.25 | 149 726.72 |
| 31 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 93 855.13 | 113 596.75 | 113 108.78 |
| 32 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 89 718.46 | 106 640.69 | 105 992.36 |
| 33 | java (11)| [quarkus](https://quarkus.io) (1.11) | 86 382.35 | 104 872.15 | 107 599.64 |
| 34 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.12) | 82 657.99 | 93 149.00 | 89 179.99 |
| 35 | scala (2.13)| [akkahttp](https://akka.io) (10.2) | 82 648.87 | 95 830.61 | 93 831.20 |
| 36 | javascript (14.15)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 82 630.71 | 98 040.71 | 99 578.46 |
| 37 | go (1.15)| [gf](https://goframe.org) (1.15) | 82 388.79 | 89 720.39 | 92 001.62 |
| 38 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 80 820.46 | 81 719.19 | 83 875.42 |
| 39 | go (1.15)| [clevergo](https://clevergo.tech) (0.5) | 80 625.48 | 81 784.81 | 83 709.04 |
| 40 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 80 299.59 | 104 287.23 | 113 092.06 |
| 41 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 80 170.97 | 81 224.00 | 83 222.37 |
| 42 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 79 952.91 | 80 744.62 | 82 839.75 |
| 43 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 79 723.41 | 83 451.67 | 83 173.34 |
| 44 | php (7.4)| [swoole-coroutine](https://github.com/swoole/swoole-src) (4.6) | 79 570.88 | 110 769.64 | 119 904.26 |
| 45 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 78 848.65 | 131 483.86 | 131 864.77 |
| 46 | php (7.4)| [swoole](https://github.com/swoole/swoole-src) (4.6) | 78 531.15 | 96 519.55 | 110 050.37 |
| 47 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 211.28 | 82 647.34 | 83 856.36 |
| 48 | go (1.15)| [chi](https://github.com/go-chi/chi) (1.5) | 77 379.86 | 76 679.06 | 79 281.20 |
| 49 | go (1.15)| [violetear](https://violetear.org) (7.0) | 76 950.80 | 77 003.79 | 79 110.44 |
| 50 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 75 171.06 | 74 603.94 | 76 820.20 |
| 51 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 75 145.45 | 78 384.82 | 79 260.35 |
| 52 | scala (2.13)| [finatra](https://twitter.github.io/finatra/) (21.1) | 74 482.19 | 86 587.19 | 87 134.33 |
| 53 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 73 681.72 | 86 059.37 | 88 682.57 |
| 54 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 73 507.90 | 71 672.57 | 74 572.74 |
| 55 | python (3.9)| [falcon](https://falconframework.org) (2.0) | 73 192.02 | 80 739.02 | 82 390.39 |
| 56 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 73 177.94 | 84 934.28 | 87 499.03 |
| 57 | fsharp (5.0)| [falco](https://falcoframework.com) (3.0) | 71 991.23 | 83 326.76 | 85 775.34 |
| 58 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.3) | 71 938.65 | 66 822.04 | 64 992.87 |
| 59 | go (1.15)| [beego](https://beego.me) (1.12) | 71 668.16 | 74 560.04 | 76 523.97 |
| 60 | javascript (14.15)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 70 644.27 | 78 881.75 | 79 938.11 |
| 61 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 64 932.92 | 72 502.42 | 72 617.72 |
| 62 | go (1.15)| [air](https://github.com/aofei/air) (0.21) | 64 874.98 | 63 396.39 | 65 875.96 |
| 63 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.6) | 62 462.55 | 62 342.10 | 65 054.73 |
| 64 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 61 428.04 | 67 054.64 | 66 523.23 |
| 65 | javascript (14.15)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 61 271.43 | 68 560.40 | 69 865.82 |
| 66 | scala (2.13)| [finch](https://finagle.github.io/finch/) (0.32) | 60 891.85 | 67 876.15 | 67 947.28 |
| 67 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 60 542.62 | 64 614.72 | 65 741.50 |
| 68 | javascript (14.15)| [0http](https://github.com/jkyberneees/0http) (3.0) | 59 847.48 | 66 027.13 | 67 288.95 |
| 69 | javascript (14.15)| [polka](https://github.com/lukeed/polka) (0.5) | 57 340.61 | 61 991.80 | 60 663.92 |
| 70 | javascript (14.15)| [restana](https://github.com/jkyberneees/ana) (4.7) | 56 432.68 | 63 686.01 | 64 625.28 |
| 71 | javascript (14.15)| [rayo](https://rayo.js.org) (1.3) | 55 678.16 | 60 604.44 | 59 651.68 |
| 72 | javascript (14.15)| [fastify](https://fastify.io) (3.11) | 54 883.76 | 59 975.54 | 58 565.11 |
| 73 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 54 023.68 | 56 882.41 | 55 262.59 |
| 74 | php (7.4)| [one](https://github.com/lizhichao/one) (2.2) | 53 689.04 | 62 425.12 | 69 617.46 |
| 75 | rust (1.49)| [salvo](https://github.com/kenorld/salvo) (0.5) | 53 501.86 | 57 372.36 | 58 262.15 |
| 76 | python (3.9)| [pyramid](https://trypyramid.com) (1.1) | 53 481.96 | 57 825.24 | 58 135.68 |
| 77 | javascript (14.15)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 53 263.46 | 58 451.21 | 56 912.02 |
| 78 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 53 020.64 | 61 277.46 | 63 726.89 |
| 79 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 51 951.96 | 57 138.23 | 64 038.89 |
| 80 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 51 209.44 | 67 085.80 | 69 944.88 |
| 81 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.14) | 51 116.59 | 73 939.47 | 82 504.72 |
| 82 | java (11)| [javalin](https://javalin.io) (3.9) | 50 993.98 | 54 422.92 | 54 741.77 |
| 83 | php (7.4)| [hyperf](https://hyperf.io) (2.1) | 50 268.78 | 61 983.59 | 65 804.76 |
| 84 | java (11)| [micronaut](https://micronaut.io) (1.2) | 50 210.06 | 57 507.61 | 57 754.22 |
| 85 | java (11)| [spark](https://sparkjava.com) (2.9) | 50 093.14 | 54 714.45 | 55 848.71 |
| 86 | javascript (14.15)| [nestjs-fastify](https://nestjs.com) (7.6) | 48 269.34 | 51 929.85 | 50 687.99 |
| 87 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 46 979.43 | 52 982.07 | 54 927.74 |
| 88 | rust (1.49)| [actix](https://actix.rs) (3.3) | 46 848.80 | 48 958.89 | 50 526.74 |
| 89 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 45 903.02 | 46 365.33 | 49 365.96 |
| 90 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 45 533.36 | 48 860.07 | 50 957.73 |
| 91 | javascript (14.15)| [foxify](https://foxify.js.org) (0.1) | 45 280.92 | 48 936.24 | 47 731.25 |
| 92 | javascript (14.15)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 45 086.27 | 48 580.56 | 48 050.71 |
| 93 | java (11)| [restheart](https://restheart.org) (5.3) | 45 015.24 | 46 751.14 | 47 295.76 |
| 94 | php (7.4)| [comet](https://github.com/gotzmann/comet) (1.1) | 44 367.49 | 48 170.72 | 49 075.21 |
| 95 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 330.04 | 45 391.37 | 45 610.99 |
| 96 | python (3.9)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 44 233.79 | 50 986.36 | 52 238.44 |
| 97 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 43 110.15 | 33 162.39 | 31 614.99 |
| 98 | python (3.9)| [bottle](https://bottlepy.org) (0.12) | 41 795.02 | 45 490.26 | 46 363.39 |
| 99 | javascript (14.15)| [koa](https://koajs.com) (2.13) | 41 141.57 | 44 103.93 | 42 917.32 |
| 100 | javascript (14.15)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 36 705.97 | 39 466.28 | 38 742.29 |
| 101 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 36 541.29 | 37 725.12 | 37 988.23 |
| 102 | python (3.9)| [emmett](https://emmett.sh) (2.1) | 35 986.75 | 41 384.01 | 42 094.98 |
| 103 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 35 461.79 | 35 436.63 | 35 154.41 |
| 104 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 35 447.80 | 35 358.65 | 35 136.03 |
| 105 | swift (5.3)| [vapor](https://vapor.codes) (4.39) | 35 429.98 | 37 294.66 | 37 020.83 |
| 106 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 35 385.86 | 37 336.65 | 37 504.85 |
| 107 | javascript (14.15)| [moleculer](https://moleculer.services) (0.14) | 34 814.47 | 35 843.56 | 34 865.62 |
| 108 | javascript (14.15)| [hapi](https://hapijs.com) (20.1) | 34 586.73 | 35 445.73 | 34 739.84 |
| 109 | rust (1.49)| [nickel](https://nickel-org.github.io) (0.11) | 34 229.57 | 34 119.19 | 33 656.97 |
| 110 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 34 063.74 | 46 329.78 | 49 403.63 |
| 111 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 33 856.04 | 34 793.20 | 33 371.46 |
| 112 | python (3.9)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 33 507.49 | 38 218.42 | 38 970.39 |
| 113 | java (11)| [jersey3-grizzly2](https://eclipse-ee4j.github.io/jersey) (3) | 33 219.02 | 39 184.50 | 38 510.12 |
| 114 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 32 781.27 | 37 816.66 | 38 062.27 |
| 115 | python (3.9)| [hug](https://hug.rest) (2.6) | 32 578.01 | 35 307.15 | 39 710.70 |
| 116 | java (11)| [jersey-grizzly2](https://eclipse-ee4j.github.io/jersey) (2.33) | 32 364.83 | 38 053.99 | 37 619.84 |
| 117 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 32 222.70 | 28 139.07 | 24 446.67 |
| 118 | rust (1.49)| [gotham](https://gotham.rs) (0.5) | 31 688.79 | 35 382.45 | 36 358.78 |
| 119 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 31 653.56 | 31 426.67 | 31 157.60 |
| 120 | python (3.9)| [sanic](https://github.com/huge-success/sanic) (20.12) | 30 162.19 | 34 369.63 | 34 756.37 |
| 121 | javascript (14.15)| [restify](https://restify.com) (8.5) | 29 290.86 | 30 620.99 | 29 757.69 |
| 122 | python (3.9)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.3) | 29 163.06 | 33 328.51 | 33 771.06 |
| 123 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 28 919.75 | 31 866.68 | 31 780.92 |
| 124 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 28 261.37 | 31 478.22 | 33 241.79 |
| 125 | scala (2.13)| [play](https://playframework.com) (2.8) | 27 246.45 | 29 069.38 | 28 964.04 |
| 126 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 27 061.07 | 29 056.84 | 28 725.06 |
| 127 | python (3.9)| [starlette](https://starlette.io) (0.14) | 27 024.80 | 31 545.99 | 31 942.82 |
| 128 | javascript (14.15)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 26 723.19 | 28 768.97 | 32 108.70 |
| 129 | python (3.9)| [index.py](https://index-py.abersheeran.com) (0.16) | 25 607.00 | 29 453.79 | 30 043.70 |
| 130 | python (3.9)| [responder](https://python-responder.org) (2.0) | 24 829.44 | 30 768.79 | 31 042.74 |
| 131 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 24 400.68 | 24 521.34 | 22 545.67 |
| 132 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 24 288.85 | 26 322.38 | 25 764.40 |
| 133 | python (3.9)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 090.94 | 27 511.06 | 27 905.74 |
| 134 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 22 974.68 | 21 691.29 | 20 779.67 |
| 135 | clojure (1.1)| [luminus](https://luminusweb.com) (3.95) | 22 231.28 | 22 032.38 | 21 139.19 |
| 136 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 21 946.72 | 21 721.53 | 21 476.73 |
| 137 | javascript (14.15)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 21 535.24 | 23 292.10 | 22 804.02 |
| 138 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.16) | 21 379.13 | 21 144.97 | 18 990.24 |
| 139 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 20 928.13 | 20 359.50 | 20 696.38 |
| 140 | python (3.9)| [molten](https://moltenframework.com) (1.0) | 18 998.76 | 22 315.85 | 22 121.52 |
| 141 | python (3.9)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 18 858.20 | 23 559.89 | 23 886.45 |
| 142 | javascript (14.15)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 18 816.65 | 17 723.61 | 16 698.93 |
| 143 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 510.30 | 15 717.89 | 14 625.25 |
| 144 | python (3.9)| [fastapi](https://fastapi.tiangolo.com) (0.63) | 17 257.33 | 21 428.71 | 21 608.34 |
| 145 | php (7.4)| [swoft](https://swoft.org) (2.0) | 16 966.28 | 20 538.78 | 21 232.09 |
| 146 | ruby (2.7)| [hanami-api](https://hanamirb.org) (0.1) | 16 959.81 | 16 516.91 | 16 064.67 |
| 147 | rust (1.49)| [iron](https://ironframework.io) (0.6) | 16 691.96 | 16 532.01 | 16 751.75 |
| 148 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 16 434.33 | 15 971.68 | 15 707.32 |
| 149 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 16 153.24 | 14 263.74 | 13 255.14 |
| 150 | ruby (2.7)| [roda](https://roda.jeremyevans.net) (3.40) | 15 897.12 | 15 456.28 | 15 181.40 |
| 151 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 15 707.15 | 18 464.89 | 18 734.14 |
| 152 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 424.01 | 18 026.26 | 18 058.09 |
| 153 | go (1.15)| [macaron](https://go-macaron.com) (1.4) | 15 398.06 | 16 900.33 | 16 907.30 |
| 154 | javascript (14.15)| [express](https://expressjs.com) (4.17) | 15 299.19 | 17 314.17 | 17 642.19 |
| 155 | ruby (2.7)| [cuba](https://cuba.is) (3.9) | 15 179.64 | 14 679.73 | 14 476.31 |
| 156 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 15 164.93 | 17 536.65 | 17 821.26 |
| 157 | javascript (14.15)| [feathersjs](https://feathersjs.com) (4.5) | 14 851.34 | 16 950.73 | 17 632.19 |
| 158 | php (7.4)| [imi](https://imiphp.com) (1.2) | 14 634.84 | 16 312.31 | 17 193.56 |
| 159 | python (3.9)| [flask](https://flask.pocoo.org) (1.1) | 14 216.12 | 16 595.71 | 15 893.41 |
| 160 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 13 885.78 | 13 509.25 | 13 258.19 |
| 161 | java (11)| [struts2](https://struts.apache.org) (2.5) | 13 863.06 | 14 212.20 | 14 242.53 |
| 162 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 13 732.19 | 14 066.62 | 17 441.52 |
| 163 | ruby (2.7)| [rack_app](https://rack-app.com) (7.7) | 13 559.35 | 13 156.80 | 12 913.89 |
| 164 | java (11)| [blade](https://lets-blade.com) (2.0) | 13 120.46 | 15 725.09 | 14 537.27 |
| 165 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 13 076.09 | 12 638.47 | 12 515.63 |
| 166 | javascript (14.15)| [nestjs-express](https://nestjs.com) (7.6) | 11 716.31 | 13 286.66 | 12 155.98 |
| 167 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 396.51 | 11 702.64 | 11 714.82 |
| 168 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 11 203.92 | 10 964.64 | 10 625.22 |
| 169 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.6) | 10 188.87 | 10 431.59 | 10 549.70 |
| 170 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 10 050.83 | 10 277.97 | 10 367.07 |
| 171 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 875.60 | 10 072.35 | 10 045.78 |
| 172 | python (3.9)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 868.42 | 9 710.00 | 9 525.05 |
| 173 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 9 771.15 | 9 961.78 | 10 085.24 |
| 174 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.7) | 9 585.22 | 9 731.19 | 9 817.75 |
| 175 | php (7.4)| [laravel-s-lumen](https://github.com/hhxsv5/laravel-s) (3.7) | 9 468.96 | 10 010.40 | 9 754.55 |
| 176 | python (3.9)| [guillotina](https://guillotina.io) (6.1) | 9 194.78 | 9 182.46 | 8 871.41 |
| 177 | ruby (2.7)| [sinatra](https://sinatrarb.com) (2.1) | 8 925.50 | 8 712.19 | 8 673.29 |
| 178 | ruby (2.7)| [grape](https://ruby-grape.org) (1.5) | 8 646.82 | 8 374.18 | 8 359.97 |
| 179 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 8 098.91 | 13 302.53 | 12 542.17 |
| 180 | python (3.9)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 7 442.55 | 7 181.48 | 6 851.46 |
| 181 | python (3.9)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 408.42 | 7 061.53 | 6 525.78 |
| 182 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 6 733.46 | 6 674.83 | 6 608.73 |
| 183 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 569.15 | 6 499.18 | 6 449.99 |
| 184 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 6 495.65 | 6 428.99 | 6 320.73 |
| 185 | php (7.4)| [laravel-s-laravel](https://github.com/hhxsv5/laravel-s) (3.7) | 6 459.28 | 6 386.78 | 6 307.44 |
| 186 | python (3.9)| [tornado](https://tornadoweb.org) (6.1) | 6 399.25 | 6 314.19 | 6 323.17 |
| 187 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 6 338.85 | 6 293.56 | 6 177.05 |
| 188 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 205.15 | 6 127.84 | 6 104.17 |
| 189 | python (3.9)| [django](https://djangoproject.com) (3.1) | 6 158.67 | 5 890.20 | 5 761.01 |
| 190 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 5 856.85 | 6 610.56 | 6 660.68 |
| 191 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 5 631.97 | 5 538.14 | 5 538.42 |
| 192 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 5 232.41 | 5 185.90 | 5 141.76 |
| 193 | ruby (2.7)| [hanami](https://hanamirb.org) (1.3) | 5 142.48 | 5 057.80 | 5 124.32 |
| 194 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.2) | 4 663.69 | 4 622.27 | 4 571.30 |
| 195 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 448.28 | 4 406.68 | 4 362.66 |
| 196 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 429.03 | 4 406.44 | 4 416.36 |
| 197 | javascript (14.15)| [sails](https://sailsjs.com) (1.4) | 4 236.98 | 4 260.83 | 4 205.71 |
| 198 | python (3.9)| [masonite](https://masoniteproject.com) (3.0) | 4 060.66 | 5 022.78 | 5 021.44 |
| 199 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.3) | 3 952.58 | 3 890.32 | 3 884.13 |
| 200 | php (7.4)| [nette](https://nette.org/en/) (3.1) | 3 878.61 | 3 858.19 | 3 883.02 |
| 201 | php (7.4)| [lumen](https://lumen.laravel.com) (8.2) | 3 832.56 | 3 835.39 | 3 873.10 |
| 202 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.6) | 3 788.44 | 3 738.65 | 3 741.85 |
| 203 | julia (1.6)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 718.27 | 6 926.35 | 5 848.78 |
| 204 | php (7.4)| [fatfree](https://fatfreeframework.com) (3.7) | 3 587.42 | 3 550.35 | 3 648.91 |
| 205 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 3 424.35 | 3 386.97 | 3 394.83 |
| 206 | php (7.4)| [symfony](https://symfony.com) (5.2) | 2 964.29 | 2 972.24 | 2 994.29 |
| 207 | ruby (2.7)| [rails](https://rubyonrails.org) (6.1) | 2 760.90 | 2 739.50 | 2 744.27 |
| 208 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 616.61 | 2 601.40 | 2 621.65 |
| 209 | ruby (2.7)| [pakyow](https://pakyow.com) (1.0) | 2 350.44 | 2 508.13 | 2 493.14 |
| 210 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.6) | 2 289.97 | 2 285.91 | 2 304.60 |
| 211 | php (7.4)| [antidot](https://antidotfw.io) (0.1) | 1 947.62 | 1 605.13 | 1 308.94 |
| 212 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 919.57 | 1 841.39 | 1 765.09 |
| 213 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 900.80 | 1 842.43 | 1 831.35 |
| 214 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.3) | 1 853.71 | 1 851.38 | 1 861.89 |
| 215 | php (7.4)| [driftphp](https://github.com/driftphp/driftphp) (0.1) | 1 825.75 | 1 787.27 | 1 393.16 |
| 216 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 1 802.58 | 1 834.72 | 1 838.64 |
| 217 | python (3.9)| [cyclone](https://cyclone.io) (1.3) | 1 660.16 | 1 645.84 | 1 629.44 |
| 218 | python (3.9)| [klein](https://github.com/twisted/klein) (20.6) | 1 612.92 | 1 618.23 | 1 591.10 |
| 219 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 569.72 | 1 580.13 | 1 587.75 |
| 220 | python (3.9)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 250.85 | 1 182.05 | 1 180.95 |
| 221 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 231.78 | 784.95 | 419.11 |
| 222 | python (3.9)| [django-ninja](https://django-ninja.rest-framework.com) (0.10) | 1 209.07 | 1 545.59 | 1 574.96 |
| 223 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.1) | 1 096.99 | 1 113.14 | 1 112.77 |
| 224 | php (7.4)| [laravel](https://laravel.com) (8.26) | 901.98 | 905.75 | 904.11 |
| 225 | cpp (11)| [nawa](https://github.com/jatofg/nawa) (0.6) | 336.26 | 238.48 | NaN |
| 226 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 288.34 | 301.84 | -89.59 |
</a>

</details>

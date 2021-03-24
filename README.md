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

## Results (2021-03-23)



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
| 1 | java (11)| [activej](https://activej.io) (4.1) | 175 575.37 | 214 314.96 | 219 282.93 |
| 2 | php (7.4)| [webman](https://github.com/walkor/webman) (1.0) | 157 939.75 | 168 675.71 | 171 454.01 |
| 3 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 151 503.96 | 181 214.83 | 184 745.52 |
| 4 | go (1.16)| [gearbox](https://gogearbox.com) (1.2) | 120 900.31 | 125 485.16 | 124 791.06 |
| 5 | go (1.16)| [fiber](https://gofiber.io) (2.6) | 120 441.91 | 131 480.71 | 131 093.31 |
| 6 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 119 381.30 | 135 354.35 | 138 210.84 |
| 7 | go (1.16)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.6) | 118 176.09 | 129 649.42 | 128 978.63 |
| 8 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 117 683.62 | 145 983.33 | 148 618.10 |
| 9 | go (1.16)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.22) | 117 215.91 | 131 125.03 | 131 372.24 |
| 10 | go (1.16)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 116 674.15 | 129 143.06 | 128 576.27 |
| 11 | go (1.16)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 116 118.84 | 128 877.77 | 128 512.95 |
| 12 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 113 356.48 | 141 552.81 | 143 983.14 |
| 13 | java (11)| [undertow](https://undertow.io) (2.2) | 113 268.58 | 136 696.83 | 138 323.86 |
| 14 | clojure (1.1)| [donkey](https://github.com/AppsFlyer/donkey) (0.5) | 111 881.78 | 113 748.69 | 116 058.96 |
| 15 | java (11)| [restheart](https://restheart.org) (5.3) | 111 852.69 | 116 361.51 | 116 543.13 |
| 16 | javascript (14.16)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.4) | 110 621.99 | 143 020.34 | 146 528.26 |
| 17 | java (11)| [jooby](https://jooby.io) (2.9) | 109 697.97 | 138 146.71 | 142 529.71 |
| 18 | c (11)| [kore](https://kore.io) (3.3) | 109 582.57 | 183 115.34 | 193 190.77 |
| 19 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 108 853.84 | 136 846.71 | 141 005.01 |
| 20 | javascript (14.16)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 108 786.92 | 139 125.78 | 142 238.52 |
| 21 | go (1.16)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 108 629.22 | 104 937.09 | 109 286.47 |
| 22 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 108 534.75 | 135 710.03 | 138 311.60 |
| 23 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 106 871.87 | 133 570.09 | 137 203.82 |
| 24 | java (11)| [vertx](https://vertx.io/docs/vertx-core/java/) (4.0) | 106 590.76 | 126 946.56 | 128 475.49 |
| 25 | rust (1.50)| [actix](https://actix.rs) (3.3) | 106 374.88 | 131 697.66 | 133 103.60 |
| 26 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 105 814.99 | 130 995.15 | 134 446.37 |
| 27 | java (11)| [vertx4web](https://vertx.io/docs/vertx-web/java/) (4.0) | 103 953.48 | 122 142.91 | 123 753.26 |
| 28 | php (7.4)| [swoole](https://github.com/swoole/swoole-src) (4.6) | 102 433.78 | 118 060.22 | 136 462.85 |
| 29 | crystal (0.36)| [toro](https://github.com/soveran/toro) (0.4) | 101 767.69 | 123 074.86 | 123 472.90 |
| 30 | crystal (0.36)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 98 876.37 | 121 134.17 | 122 037.00 |
| 31 | crystal (0.36)| [spider-gazelle](https://spider-gazelle.net) (4.2) | 98 818.64 | 119 850.68 | 120 041.39 |
| 32 | crystal (0.36)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 96 594.83 | 116 908.66 | 117 335.06 |
| 33 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 95 346.75 | 115 998.03 | 119 283.27 |
| 34 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 94 728.22 | 139 541.72 | 150 479.84 |
| 35 | crystal (0.36)| [grip](https://github.com/grip-framework/grip) (4.0) | 94 121.26 | 111 846.72 | 112 083.48 |
| 36 | crystal (0.36)| [kemal](https://kemalcr.com) (0.27) | 92 715.73 | 112 422.62 | 112 744.04 |
| 37 | php (7.4)| [swoole-coroutine](https://github.com/swoole/swoole-src) (4.6) | 92 097.08 | 128 021.62 | 138 334.22 |
| 38 | crystal (0.36)| [amber](https://amberframework.org) (0.36) | 89 243.49 | 108 264.66 | 107 940.22 |
| 39 | java (11)| [quarkus](https://quarkus.io) (1.12) | 86 516.80 | 105 041.29 | 108 102.13 |
| 40 | go (1.16)| [gf](https://goframe.org) (1.15) | 82 432.32 | 89 918.50 | 92 272.77 |
| 41 | javascript (14.16)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 82 347.97 | 98 789.92 | 98 665.33 |
| 42 | go (1.16)| [rte](https://github.com/jwilner/rte) (0.0) | 81 381.29 | 82 259.30 | 84 167.46 |
| 43 | go (1.16)| [clevergo](https://clevergo.tech) (0.5) | 80 743.29 | 82 395.73 | 84 400.47 |
| 44 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 80 447.72 | 100 918.50 | 109 115.52 |
| 45 | go (1.16)| [echo](https://echo.labstack.com) (4.2) | 79 961.96 | 81 325.69 | 83 246.19 |
| 46 | go (1.16)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 79 884.15 | 80 978.32 | 82 820.91 |
| 47 | scala (2.13)| [akkahttp](https://akka.io) (10.2) | 79 823.18 | 93 177.31 | 90 500.86 |
| 48 | go (1.16)| [gin](https://gin-gonic.com) (1.6) | 78 779.18 | 82 701.04 | 84 212.28 |
| 49 | go (1.16)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 515.63 | 83 008.62 | 84 237.00 |
| 50 | go (1.16)| [aero](https://github.com/aerogo/aero) (1.3) | 77 587.68 | 78 060.52 | 79 889.55 |
| 51 | go (1.16)| [violetear](https://violetear.org) (7.0) | 77 280.12 | 77 463.72 | 79 810.17 |
| 52 | go (1.16)| [chi](https://github.com/go-chi/chi) (1.5) | 76 860.25 | 75 965.31 | 78 210.80 |
| 53 | go (1.16)| [webgo](https://github.com/bnkamalesh/webgo) (5.0) | 76 274.33 | 75 761.93 | 77 895.43 |
| 54 | crystal (0.36)| [athena](https://github.com/athena-framework/athena) (0.13) | 75 105.40 | 83 945.28 | 79 413.76 |
| 55 | fsharp (5.0)| [falco](https://falcoframework.com) (3.0) | 74 992.16 | 87 894.79 | 91 028.78 |
| 56 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 74 933.56 | 87 847.50 | 90 700.73 |
| 57 | rust (1.50)| [iron](https://iron/iron) (0.6) | 74 786.19 | 72 048.88 | 72 101.53 |
| 58 | python (3.9)| [falcon](https://falconframework.org) (2.0) | 74 256.04 | 81 538.76 | 82 897.69 |
| 59 | scala (2.13)| [finatra](https://twitter.github.io/finatra/) (21.1) | 74 159.56 | 85 918.40 | 86 206.07 |
| 60 | go (1.16)| [goroute](https://goroute.github.io) (0.0) | 74 107.45 | 72 573.98 | 74 905.87 |
| 61 | rust (1.50)| [salvo](https://github.com/salvo-rs/salvo) (0.9) | 73 858.28 | 92 724.14 | 101 711.36 |
| 62 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.4) | 72 618.58 | 66 149.25 | 62 422.83 |
| 63 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 72 449.66 | 84 013.20 | 86 410.35 |
| 64 | go (1.16)| [beego](https://beego.me) (1.12) | 71 610.06 | 74 415.75 | 76 458.88 |
| 65 | javascript (14.16)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 70 905.72 | 80 816.93 | 81 076.18 |
| 66 | go (1.16)| [goyave](https://github.com/go-goyave/goyave) (3.7) | 70 700.57 | 70 970.82 | 72 989.77 |
| 67 | rust (1.50)| [gotham](https://gotham.rs) (0.6) | 69 067.13 | 86 130.21 | 94 536.28 |
| 68 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 65 364.29 | 73 162.26 | 73 335.66 |
| 69 | go (1.16)| [air](https://github.com/aofei/air) (0.21) | 64 922.88 | 63 741.52 | 65 944.72 |
| 70 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 62 089.60 | 68 037.68 | 67 860.74 |
| 71 | scala (2.13)| [finch](https://finagle.github.io/finch/) (0.32) | 61 199.24 | 68 343.67 | 67 948.93 |
| 72 | javascript (14.16)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 60 441.74 | 67 299.29 | 69 348.87 |
| 73 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 60 063.91 | 63 248.85 | 64 460.22 |
| 74 | rust (1.50)| [nickel](https://nickel-org.github.io) (0.11) | 59 964.06 | 60 125.58 | 50 529.48 |
| 75 | javascript (14.16)| [0http](https://github.com/jkyberneees/0http) (3.1) | 59 532.72 | 66 718.33 | 67 911.27 |
| 76 | javascript (14.16)| [polka](https://github.com/lukeed/polka) (0.5) | 58 498.04 | 62 462.26 | 61 841.55 |
| 77 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 57 626.83 | 131 186.65 | 130 177.21 |
| 78 | javascript (14.16)| [restana](https://github.com/jkyberneees/ana) (4.7) | 57 349.85 | 63 545.80 | 64 274.03 |
| 79 | javascript (14.16)| [rayo](https://rayo.js.org) (1.3) | 55 535.55 | 61 326.04 | 59 891.82 |
| 80 | swift (5.3)| [hummingbird](https://github.com/hummingbird-project/hummingbird) (0.7) | 54 558.38 | 60 632.74 | 60 720.03 |
| 81 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 54 248.05 | 57 375.34 | 55 756.91 |
| 82 | javascript (14.16)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 53 990.94 | 58 334.94 | 56 923.22 |
| 83 | javascript (14.16)| [fastify](https://fastify.io) (3.14) | 53 896.88 | 58 572.39 | 57 179.16 |
| 84 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 53 111.06 | 61 395.46 | 63 842.30 |
| 85 | php (7.4)| [hyperf](https://hyperf.io) (2.1) | 52 280.64 | 62 670.52 | 63 804.26 |
| 86 | php (7.4)| [one](https://github.com/lizhichao/one) (2.2) | 51 899.45 | 65 935.37 | 69 936.99 |
| 87 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 51 893.79 | 57 168.10 | 64 395.46 |
| 88 | python (3.9)| [pyramid](https://trypyramid.com) (2.0) | 51 298.30 | 56 850.32 | 57 128.55 |
| 89 | ruby (3.0)| [agoo](https://github.com/ohler55/agoo) (2.14) | 51 225.18 | 72 385.48 | 80 363.86 |
| 90 | java (11)| [javalin](https://javalin.io) (3.9) | 51 203.09 | 54 486.63 | 54 363.07 |
| 91 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 51 187.71 | 58 165.25 | 58 256.42 |
| 92 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 49 905.35 | 64 487.89 | 67 437.27 |
| 93 | java (11)| [spark](https://sparkjava.com) (2.9) | 49 448.82 | 54 061.36 | 55 112.76 |
| 94 | java (11)| [micronaut](https://micronaut.io) (1.2) | 49 178.22 | 57 623.25 | 57 561.89 |
| 95 | javascript (14.16)| [nestjs-fastify](https://nestjs.com) (7.6) | 48 075.64 | 51 522.25 | 50 507.95 |
| 96 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 46 640.38 | 49 250.28 | 50 270.34 |
| 97 | go (1.16)| [mars](https://github.com/roblillack/mars) (1.0) | 46 515.52 | 46 798.12 | 50 038.25 |
| 98 | javascript (14.16)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 45 767.05 | 48 414.74 | 47 698.93 |
| 99 | javascript (14.16)| [foxify](https://foxify.js.org) (0.1) | 45 129.95 | 48 491.41 | 47 472.75 |
| 100 | python (3.9)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 44 745.54 | 51 318.30 | 52 105.43 |
| 101 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 44 048.38 | 32 861.75 | 33 334.30 |
| 102 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 43 427.93 | 44 225.50 | 44 018.62 |
| 103 | javascript (14.16)| [koa](https://koajs.com) (2.13) | 41 187.18 | 44 292.70 | 43 143.61 |
| 104 | python (3.9)| [bottle](https://bottlepy.org) (0.12) | 39 690.41 | 42 590.65 | 44 329.21 |
| 105 | dart (2.12)| [aqueduct](https://aqueduct.io) (3.3) | 39 588.39 | 38 651.99 | 38 887.50 |
| 106 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 36 295.68 | 37 803.86 | 37 753.28 |
| 107 | python (3.9)| [emmett](https://emmett.sh) (2.2) | 35 983.95 | 41 270.86 | 42 295.95 |
| 108 | swift (5.3)| [vapor](https://vapor.codes) (4.41) | 35 871.83 | 37 729.15 | 37 600.74 |
| 109 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 35 744.50 | 36 127.03 | 36 120.92 |
| 110 | javascript (14.16)| [moleculer](https://moleculer.services) (0.14) | 35 357.87 | 36 091.56 | 34 731.17 |
| 111 | javascript (14.16)| [hapi](https://hapijs.com) (20.1) | 34 968.23 | 34 970.06 | 34 382.10 |
| 112 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 34 955.70 | 35 743.59 | 35 104.82 |
| 113 | javascript (14.16)| [fyrejet](https://github.com/fyrejet/fyrejet) (3.1) | 34 182.91 | 35 843.83 | 35 568.81 |
| 114 | java (11)| [jersey3-grizzly2](https://eclipse-ee4j.github.io/jersey) (3) | 34 177.81 | 39 358.20 | 38 895.23 |
| 115 | python (3.9)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 34 119.60 | 38 263.87 | 38 707.73 |
| 116 | python (3.9)| [hug](https://hug.rest) (2.6) | 34 040.91 | 35 909.39 | 53 828.77 |
| 117 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 33 999.72 | 36 971.44 | 37 512.02 |
| 118 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 33 622.60 | 34 646.70 | 33 237.87 |
| 119 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 33 572.36 | 42 537.94 | 45 615.94 |
| 120 | java (11)| [jersey-grizzly2](https://eclipse-ee4j.github.io/jersey) (2.33) | 33 270.76 | 38 222.92 | 37 846.04 |
| 121 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 32 708.03 | 28 805.22 | 24 605.95 |
| 122 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 32 368.45 | 36 707.55 | 36 956.65 |
| 123 | php (7.4)| [imi](https://imiphp.com) (1.2) | 29 182.71 | 33 287.41 | 34 065.02 |
| 124 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 29 096.25 | 32 069.06 | 31 978.95 |
| 125 | python (3.9)| [blacksheep](https://github.com/Neoteroi/BlackSheep) (1.0) | 28 887.22 | 33 204.44 | 34 356.25 |
| 126 | javascript (14.16)| [restify](https://restify.com) (8.5) | 28 880.04 | 30 805.27 | 29 538.17 |
| 127 | scala (2.13)| [play](https://playframework.com) (2.8) | 28 707.97 | 30 874.74 | 30 660.45 |
| 128 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 27 984.72 | 31 572.80 | 32 485.26 |
| 129 | python (3.9)| [index.py](https://index-py.abersheeran.com) (0.16) | 27 273.81 | 29 282.48 | 30 258.63 |
| 130 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 27 197.43 | 29 385.43 | 28 594.83 |
| 131 | python (3.9)| [starlette](https://starlette.io) (0.14) | 26 693.66 | 31 709.52 | 31 302.99 |
| 132 | javascript (14.16)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (3.1) | 26 588.70 | 27 940.91 | 28 717.57 |
| 133 | php (7.4)| [comet](https://github.com/gotzmann/comet) (1.2) | 26 398.81 | 26 960.54 | 26 369.98 |
| 134 | python (3.9)| [responder](https://python-responder.org) (2.0) | 26 092.65 | 31 092.58 | 32 175.53 |
| 135 | python (3.9)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 25 651.88 | 28 936.99 | 28 781.05 |
| 136 | python (3.9)| [sanic](https://github.com/sanic-org/sanic) (21.3) | 25 446.08 | 28 833.98 | 29 194.50 |
| 137 | crystal (0.36)| [orion](https://github.com/obsidian/orion) (3.0) | 24 544.57 | 24 648.07 | 21 485.78 |
| 138 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 24 464.13 | 26 448.44 | 25 733.68 |
| 139 | clojure (1.1)| [luminus](https://luminusweb.com) (3.98) | 22 334.37 | 21 703.07 | 21 235.60 |
| 140 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 22 292.60 | 21 960.62 | 21 641.70 |
| 141 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 22 270.24 | 21 451.15 | 20 333.35 |
| 142 | crystal (0.36)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.17) | 20 912.88 | 20 491.29 | 19 429.79 |
| 143 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 20 446.66 | 19 846.43 | 20 203.57 |
| 144 | javascript (14.16)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 18 813.51 | 17 646.74 | 16 632.76 |
| 145 | python (3.9)| [molten](https://moltenframework.com) (1.0) | 18 055.01 | 21 858.74 | 21 906.60 |
| 146 | python (3.9)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 17 837.07 | 23 359.98 | 24 128.26 |
| 147 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 17 557.23 | 15 658.50 | 14 696.51 |
| 148 | php (7.4)| [swoft](https://swoft.org) (2.0) | 16 895.06 | 20 266.16 | 21 470.01 |
| 149 | python (3.9)| [fastapi](https://fastapi.tiangolo.com) (0.63) | 16 701.72 | 22 402.72 | 22 042.29 |
| 150 | ruby (3.0)| [hanami-api](https://hanamirb.org) (0.1) | 16 677.61 | 16 336.47 | 15 946.76 |
| 151 | ruby (3.0)| [syro](https://github.com/soveran/syro) (3.2) | 15 976.12 | 15 459.75 | 15 231.86 |
| 152 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 15 801.08 | 14 051.87 | 12 977.26 |
| 153 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 15 687.81 | 17 965.66 | 18 494.73 |
| 154 | ruby (3.0)| [roda](https://roda.jeremyevans.net) (3.42) | 15 510.36 | 15 132.27 | 14 766.26 |
| 155 | javascript (14.16)| [express](https://expressjs.com) (4.17) | 15 374.60 | 17 971.03 | 17 618.89 |
| 156 | go (1.16)| [macaron](https://go-macaron.com) (1.4) | 15 306.91 | 16 778.55 | 16 806.59 |
| 157 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 15 160.95 | 17 419.30 | 17 712.80 |
| 158 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 15 099.69 | 17 442.35 | 17 745.43 |
| 159 | javascript (14.16)| [feathersjs](https://feathersjs.com) (4.5) | 14 538.63 | 17 486.47 | 17 857.56 |
| 160 | java (11)| [struts2](https://struts.apache.org) (2.5) | 14 020.64 | 14 321.54 | 14 279.01 |
| 161 | ruby (3.0)| [cuba](https://cuba.is) (3.9) | 14 015.19 | 13 681.38 | 13 423.34 |
| 162 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 13 628.68 | 15 174.44 | 21 728.75 |
| 163 | ruby (3.0)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 13 537.73 | 13 154.64 | 12 918.84 |
| 164 | python (3.9)| [flask](https://flask.pocoo.org) (1.1) | 12 901.82 | 16 427.23 | 16 521.94 |
| 165 | java (11)| [blade](https://lets-blade.com) (2.0) | 12 872.70 | 16 612.92 | 14 665.40 |
| 166 | ruby (3.0)| [rack_app](https://rack-app.com) (7.7) | 12 788.41 | 12 208.51 | 11 902.36 |
| 167 | ruby (3.0)| [camping](https://github.com/camping/camping) (2.1) | 12 650.81 | 12 314.43 | 12 132.91 |
| 168 | javascript (14.16)| [nestjs-express](https://nestjs.com) (7.6) | 11 765.97 | 11 454.54 | 11 294.14 |
| 169 | dart (2.12)| [start](https://github.com/lvivski/start) (0.4) | 11 516.04 | 11 294.73 | 10 928.13 |
| 170 | go (1.16)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 411.87 | 11 660.96 | 11 712.78 |
| 171 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 10 097.57 | 10 332.62 | 10 380.89 |
| 172 | go (1.16)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 882.50 | 10 130.44 | 10 138.03 |
| 173 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.6) | 9 847.14 | 10 098.98 | 10 198.69 |
| 174 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 9 682.50 | 9 936.40 | 10 088.18 |
| 175 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.7) | 9 682.45 | 9 841.02 | 9 913.63 |
| 176 | python (3.9)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 328.09 | 9 396.33 | 8 832.55 |
| 177 | python (3.9)| [guillotina](https://guillotina.io) (6.2) | 9 152.33 | 8 843.65 | 8 742.31 |
| 178 | php (7.4)| [laravel-s-lumen](https://github.com/hhxsv5/laravel-s) (3.7) | 9 017.70 | 9 744.92 | 9 672.64 |
| 179 | ruby (3.0)| [sinatra](https://sinatrarb.com) (2.1) | 8 630.27 | 8 404.41 | 8 373.15 |
| 180 | pony (0.39)| [jennet](https://github.com/Theodus/jennet) (0.1) | 8 125.68 | 13 472.65 | 12 449.02 |
| 181 | python (3.9)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 7 571.28 | 7 501.08 | 6 888.88 |
| 182 | ruby (3.0)| [grape](https://ruby-grape.org) (1.5) | 7 514.23 | 7 294.92 | 7 313.41 |
| 183 | python (3.9)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 363.12 | 6 948.61 | 6 344.40 |
| 184 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 7 117.36 | 7 488.15 | 7 430.21 |
| 185 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 7 103.24 | 7 484.24 | 7 414.76 |
| 186 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 6 955.62 | 7 334.53 | 7 224.48 |
| 187 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 876.02 | 7 185.74 | 7 105.97 |
| 188 | php (7.4)| [laravel-s-laravel](https://github.com/hhxsv5/laravel-s) (3.7) | 6 543.48 | 6 433.59 | 6 423.13 |
| 189 | php (7.4)| [ice](https://iceframework.org) (1.5) | 6 362.58 | 6 685.52 | 6 714.95 |
| 190 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 6 182.28 | 6 951.18 | 6 873.50 |
| 191 | python (3.9)| [django](https://djangoproject.com) (3.1) | 5 918.73 | 6 572.13 | 6 150.29 |
| 192 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 5 789.17 | 5 980.22 | 5 909.48 |
| 193 | python (3.9)| [tornado](https://tornadoweb.org) (6.1) | 5 722.03 | 5 728.67 | 5 624.07 |
| 194 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.2) | 5 090.00 | 5 297.20 | 5 232.10 |
| 195 | php (7.4)| [fatfree](https://fatfreeframework.com) (3.7) | 5 033.76 | 5 180.87 | 5 017.53 |
| 196 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 4 790.01 | 4 896.76 | 4 890.59 |
| 197 | php (7.4)| [cubex](https://cubex.io) (4.16) | 4 675.00 | 4 778.79 | 4 796.42 |
| 198 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 376.58 | 4 519.76 | 4 546.55 |
| 199 | javascript (14.16)| [sails](https://sailsjs.com) (1.4) | 4 239.64 | 4 244.94 | 4 179.74 |
| 200 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.3) | 4 115.76 | 4 203.52 | 4 221.94 |
| 201 | julia (1.6)| [merly](https://github.com/codeneomatrix/Merly.jl) (1.0) | 3 941.36 | 6 355.95 | 4 145.46 |
| 202 | php (7.4)| [nette](https://nette.org/en/) (3.1) | 3 855.90 | 3 948.03 | 3 954.67 |
| 203 | php (7.4)| [lumen](https://lumen.laravel.com) (8.2) | 3 834.34 | 3 903.96 | 3 901.05 |
| 204 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.6) | 3 832.95 | 3 929.99 | 3 904.15 |
| 205 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 3 440.61 | 3 569.28 | 3 513.07 |
| 206 | php (7.4)| [symfony](https://symfony.com) (5.2) | 2 994.68 | 3 042.22 | 3 038.90 |
| 207 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.6) | 2 938.37 | 2 985.60 | 3 001.55 |
| 208 | ruby (3.0)| [rails](https://rubyonrails.org) (6.1) | 2 809.64 | 2 766.30 | 2 774.49 |
| 209 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 2 506.87 | 2 566.75 | 2 581.33 |
| 210 | python (3.9)| [masonite](https://masoniteproject.com) (3.0) | 2 485.30 | 2 477.63 | 2 477.29 |
| 211 | php (7.4)| [driftphp](https://github.com/driftphp/driftphp) (0.1) | 1 963.98 | 663.69 | 1 545.84 |
| 212 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 938.98 | 1 849.13 | 1 786.08 |
| 213 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 923.19 | 1 863.49 | 1 895.95 |
| 214 | php (7.4)| [antidot](https://github.com/antidot-framework/react-framework) (0.2) | 1 863.69 | 665.13 | 1 670.00 |
| 215 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.3) | 1 845.80 | 1 870.66 | 1 864.01 |
| 216 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 722.92 | 1 737.85 | 1 743.41 |
| 217 | python (3.9)| [cyclone](https://cyclone.io) (1.3) | 1 597.57 | 1 591.42 | 1 577.40 |
| 218 | python (3.9)| [klein](https://github.com/twisted/klein) (20.6) | 1 501.11 | 1 538.24 | 1 513.71 |
| 219 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 256.11 | 664.47 | 642.76 |
| 220 | python (3.9)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 213.25 | 1 164.90 | 1 142.60 |
| 221 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.1) | 1 186.72 | 1 206.01 | 1 203.15 |
| 222 | python (3.9)| [django-ninja](https://django-ninja.rest-framework.com) (0.11) | 1 065.37 | 1 478.17 | 1 496.42 |
| 223 | php (7.4)| [laravel](https://laravel.com) (8.34) | 990.46 | 986.28 | 989.90 |
| 224 | php (7.4)| [unic](https://unicframework.github.io/docs) (1.0) | 509.61 | 463.05 | 240.42 |
| 225 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 286.11 | 298.58 | -84.76 |
| 226 | cpp (11)| [nawa](https://github.com/jatofg/nawa) (0.6) | 263.95 | NaN | NaN |
</a>

</details>

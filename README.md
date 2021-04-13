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

## Results (2021-04-13)



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
| 1 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 180 399.05 | 199 648.58 | 203 299.50 |
| 2 | go (1.16)| [fiber](https://gofiber.io) (2.7) | 180 036.08 | 193 413.58 | 192 348.44 |
| 3 | go (1.16)| [gearbox](https://gogearbox.com) (1.2) | 177 012.27 | 183 413.15 | 183 688.88 |
| 4 | go (1.16)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.6) | 176 498.90 | 188 350.50 | 188 315.50 |
| 5 | go (1.16)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 176 141.56 | 186 666.08 | 186 300.55 |
| 6 | go (1.16)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 175 784.57 | 188 258.04 | 187 793.39 |
| 7 | go (1.16)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.23) | 174 979.93 | 193 891.65 | 194 423.04 |
| 8 | java (11)| [activej](https://activej.io) (4.1) | 165 677.52 | 196 657.47 | 200 255.01 |
| 9 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 163 378.30 | 195 861.92 | 199 874.33 |
| 10 | rust (1.51)| [actix](https://actix.rs) (3.3) | 160 985.58 | 184 838.80 | 186 306.91 |
| 11 | php (7.4)| [webman](https://github.com/walkor/webman) (1.0) | 155 853.13 | 168 014.23 | 170 595.41 |
| 12 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 153 899.25 | 191 620.53 | 197 626.83 |
| 13 | crystal (0.36)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 151 924.25 | 172 990.61 | 173 533.01 |
| 14 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 150 939.47 | 179 336.79 | 183 348.21 |
| 15 | java (11)| [vertx](https://vertx.io/docs/vertx-core/java/) (4.0) | 149 561.08 | 174 263.47 | 175 494.63 |
| 16 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 148 900.21 | 177 720.81 | 182 130.39 |
| 17 | java (11)| [undertow](https://undertow.io) (2.2) | 146 381.94 | 174 936.27 | 178 796.51 |
| 18 | crystal (0.36)| [spider-gazelle](https://spider-gazelle.net) (4.2) | 141 710.78 | 168 756.74 | 169 974.77 |
| 19 | crystal (0.36)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 140 915.88 | 165 338.70 | 165 595.37 |
| 20 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 137 295.04 | 166 592.33 | 170 066.82 |
| 21 | java (11)| [jooby](https://jooby.io) (2.9) | 135 373.46 | 174 950.11 | 183 720.93 |
| 22 | crystal (0.36)| [kemal](https://kemalcr.com) (0.27) | 135 193.29 | 160 799.25 | 160 611.60 |
| 23 | java (11)| [vertx4web](https://vertx.io/docs/vertx-web/java/) (4.0) | 134 298.44 | 156 100.61 | 158 143.09 |
| 24 | crystal (0.36)| [grip](https://github.com/grip-framework/grip) (1.0) | 132 603.67 | 154 430.23 | 152 195.67 |
| 25 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 131 103.77 | 169 200.80 | 173 996.21 |
| 26 | crystal (0.36)| [amber](https://amberframework.org) (0.36) | 131 054.68 | 150 918.80 | 149 302.02 |
| 27 | javascript (14.16)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 130 925.12 | 168 525.55 | 174 072.23 |
| 28 | javascript (14.16)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.4) | 125 557.05 | 164 974.15 | 170 903.31 |
| 29 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 120 740.33 | 143 667.00 | 147 234.79 |
| 30 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 119 943.76 | 148 754.20 | 163 546.55 |
| 31 | go (1.16)| [rte](https://github.com/jwilner/rte) (0.0) | 119 847.25 | 120 841.32 | 124 162.35 |
| 32 | go (1.16)| [clevergo](https://clevergo.tech) (0.5) | 119 090.48 | 120 149.46 | 123 373.65 |
| 33 | go (1.16)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 118 150.14 | 118 409.32 | 121 692.00 |
| 34 | go (1.16)| [echo](https://echo.labstack.com) (4.2) | 118 092.56 | 118 743.89 | 122 105.26 |
| 35 | go (1.16)| [gin](https://gin-gonic.com) (1.7) | 115 333.15 | 121 013.19 | 123 748.48 |
| 36 | clojure (1.1)| [donkey](https://github.com/AppsFlyer/donkey) (0.5) | 113 901.73 | 124 084.83 | 124 996.27 |
| 37 | go (1.16)| [aero](https://github.com/aerogo/aero) (1.3) | 113 896.13 | 113 879.15 | 117 030.97 |
| 38 | java (11)| [quarkus](https://quarkus.io) (1.13) | 113 460.06 | 143 434.52 | 146 517.83 |
| 39 | go (1.16)| [chi](https://github.com/go-chi/chi) (1.5) | 112 880.57 | 111 294.14 | 115 392.51 |
| 40 | go (1.16)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 111 474.31 | 118 486.08 | 120 619.33 |
| 41 | go (1.16)| [violetear](https://violetear.org) (7.0) | 111 199.97 | 109 504.02 | 113 377.76 |
| 42 | go (1.16)| [webgo](https://github.com/bnkamalesh/webgo) (5.0) | 109 786.10 | 107 919.12 | 111 617.55 |
| 43 | go (1.16)| [goroute](https://goroute.github.io) (0.0) | 109 366.09 | 106 797.78 | 111 118.42 |
| 44 | go (1.16)| [kami](https://github.com/guregu/kami) (2.2) | 109 239.29 | 114 032.34 | 114 909.78 |
| 45 | crystal (0.36)| [athena](https://github.com/athena-framework/athena) (0.13) | 107 607.50 | 117 222.41 | 110 998.35 |
| 46 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 107 320.04 | 182 170.28 | 186 124.95 |
| 47 | rust (1.51)| [salvo](https://github.com/salvo-rs/salvo) (0.10) | 106 509.89 | 133 221.19 | 142 053.58 |
| 48 | scala (2.13)| [finatra](https://twitter.github.io/finatra/) (21.3) | 105 893.77 | 124 352.22 | 124 573.22 |
| 49 | go (1.16)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 105 645.17 | 101 622.05 | 106 590.36 |
| 50 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 104 991.67 | 121 724.15 | 126 649.98 |
| 51 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 104 224.07 | 123 005.09 | 127 618.20 |
| 52 | rust (1.51)| [iron](https://iron/iron) (0.6) | 103 245.31 | 103 395.08 | 103 488.15 |
| 53 | rust (1.51)| [gotham](https://gotham.rs) (0.6) | 101 914.79 | 126 560.87 | 134 795.52 |
| 54 | go (1.16)| [beego](https://beego.me) (1.12) | 101 451.32 | 105 465.17 | 108 875.59 |
| 55 | php (7.4)| [swoole](https://github.com/swoole/swoole-src) (4.6) | 97 346.54 | 122 423.53 | 137 500.60 |
| 56 | fsharp (5.0)| [falco](https://www.falcoframework.com) (3.0) | 96 114.84 | 112 209.05 | 116 429.72 |
| 57 | go (1.16)| [goyave](https://github.com/go-goyave/goyave) (3.7) | 95 473.97 | 96 541.51 | 100 336.16 |
| 58 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 93 836.20 | 126 108.05 | 129 020.76 |
| 59 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 93 420.70 | 138 089.03 | 149 234.75 |
| 60 | go (1.16)| [air](https://github.com/aofei/air) (0.21) | 93 383.99 | 92 234.32 | 95 970.96 |
| 61 | javascript (14.16)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 91 889.66 | 111 894.56 | 115 131.76 |
| 62 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 88 835.02 | 99 277.88 | 100 100.73 |
| 63 | java (11)| [restheart](https://restheart.org) (5.3) | 88 640.30 | 95 004.33 | 94 407.63 |
| 64 | php (7.4)| [swoole-coroutine](https://github.com/swoole/swoole-src) (4.6) | 87 054.38 | 125 878.86 | 136 587.08 |
| 65 | rust (1.51)| [nickel](https://nickel-org.github.io) (0.11) | 85 191.99 | 87 979.53 | 91 733.37 |
| 66 | scala (2.13)| [finch](https://finagle.github.io/finch/) (0.32) | 84 680.05 | 97 607.52 | 97 587.70 |
| 67 | javascript (14.16)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 80 555.94 | 95 246.43 | 97 513.85 |
| 68 | go (1.16)| [gf](https://goframe.org) (1.15) | 80 066.51 | 87 995.06 | 90 575.75 |
| 69 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 79 027.95 | 86 173.01 | 85 380.74 |
| 70 | swift (5.3)| [hummingbird](https://github.com/hummingbird-project/hummingbird) (0.9) | 76 453.42 | 82 532.43 | 82 826.51 |
| 71 | scala (2.13)| [akkahttp](https://akka.io) (10.2) | 76 361.38 | 90 519.39 | 87 828.60 |
| 72 | elixir (1.12)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 75 513.33 | 78 806.85 | 77 142.17 |
| 73 | php (7.4)| [one](https://github.com/lizhichao/one) (2.2) | 73 695.75 | 86 334.02 | 96 700.48 |
| 74 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 73 119.27 | 82 413.04 | 91 600.14 |
| 75 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 72 636.57 | 85 800.67 | 88 877.28 |
| 76 | php (7.4)| [hyperf](https://hyperf.io) (2.1) | 72 275.12 | 88 738.27 | 96 934.50 |
| 77 | python (3.9)| [falcon](https://falconframework.org) (3.0) | 71 490.22 | 78 381.04 | 79 377.53 |
| 78 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 68 287.71 | 85 287.64 | 89 606.50 |
| 79 | kotlin (1.4)| [ktor](https://ktor.io) (1.5) | 67 959.08 | 89 479.83 | 91 703.89 |
| 80 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 67 801.97 | 77 428.32 | 77 646.72 |
| 81 | c (11)| [kore](https://kore.io) (3.3) | 67 554.78 | 134 908.55 | 136 439.33 |
| 82 | java (11)| [spark](https://sparkjava.com) (2.9) | 67 349.69 | 75 615.51 | 77 219.85 |
| 83 | javascript (14.16)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 67 258.01 | 81 678.75 | 82 687.32 |
| 84 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.5) | 66 580.12 | 66 511.45 | 60 729.82 |
| 85 | go (1.16)| [mars](https://github.com/roblillack/mars) (1.0) | 65 512.71 | 66 347.12 | 71 421.58 |
| 86 | javascript (14.16)| [0http](https://github.com/jkyberneees/0http) (3.1) | 65 493.68 | 78 929.87 | 79 811.57 |
| 87 | javascript (14.16)| [rayo](https://rayo.js.org) (1.3) | 64 260.44 | 72 702.18 | 72 081.32 |
| 88 | java (11)| [javalin](https://javalin.io) (3.9) | 63 777.60 | 69 794.84 | 71 122.61 |
| 89 | javascript (14.16)| [polka](https://github.com/lukeed/polka) (0.5) | 63 184.56 | 71 960.48 | 70 965.34 |
| 90 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 60 814.86 | 72 985.02 | 72 417.72 |
| 91 | ruby (3.0)| [agoo](https://github.com/ohler55/agoo) (2.14) | 60 285.33 | 88 819.57 | 100 319.27 |
| 92 | python (3.9)| [bottle](https://bottlepy.org) (0.12) | 60 154.29 | 65 185.28 | 65 516.47 |
| 93 | javascript (14.16)| [restana](https://github.com/jkyberneees/ana) (4.7) | 58 054.53 | 70 274.88 | 70 307.67 |
| 94 | javascript (14.16)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 56 882.00 | 64 025.68 | 63 193.92 |
| 95 | javascript (14.16)| [foxify](https://foxify.js.org) (0.1) | 53 206.87 | 59 264.32 | 58 973.89 |
| 96 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 52 784.29 | 53 927.21 | 53 733.39 |
| 97 | swift (5.3)| [vapor](https://vapor.codes) (4.44) | 49 945.17 | 52 270.39 | 52 133.94 |
| 98 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 49 550.41 | 53 141.74 | 53 783.54 |
| 99 | python (3.9)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 49 159.57 | 55 680.73 | 56 136.82 |
| 100 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 48 727.99 | 48 343.75 | 47 945.49 |
| 101 | elixir (1.12)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 48 650.00 | 50 255.29 | 51 036.83 |
| 102 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 48 371.07 | 48 506.81 | 48 790.80 |
| 103 | javascript (14.16)| [nestjs-fastify](https://nestjs.com) (7.6) | 48 138.19 | 53 940.75 | 53 161.21 |
| 104 | javascript (14.16)| [koa](https://koajs.com) (2.13) | 47 554.58 | 52 912.88 | 52 093.77 |
| 105 | python (3.9)| [hug](https://hug.rest) (2.6) | 46 289.02 | 50 985.08 | 50 850.40 |
| 106 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 45 909.59 | 57 089.97 | 63 600.30 |
| 107 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 45 723.55 | 45 931.37 | 45 172.57 |
| 108 | python (3.9)| [pyramid](https://trypyramid.com) (2.0) | 45 626.22 | 48 023.27 | 48 128.96 |
| 109 | python (3.9)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 43 694.85 | 50 258.08 | 50 990.20 |
| 110 | javascript (14.16)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 43 664.95 | 48 321.87 | 47 230.57 |
| 111 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 43 381.12 | 50 563.16 | 50 687.22 |
| 112 | php (7.4)| [imi](https://imiphp.com) (1.2) | 42 407.17 | 49 312.82 | 50 473.50 |
| 113 | python (3.9)| [blacksheep](https://github.com/Neoteroi/BlackSheep) (1.0) | 41 825.71 | 48 770.52 | 47 747.27 |
| 114 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 41 436.43 | 44 981.70 | 44 461.19 |
| 115 | java (11)| [micronaut](https://micronaut.io) (1.2) | 40 872.96 | 49 977.84 | 50 641.10 |
| 116 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 40 475.59 | 44 689.36 | 47 691.38 |
| 117 | python (3.9)| [sanic](https://github.com/sanic-org/sanic) (21.3) | 40 458.75 | 41 790.44 | 43 648.50 |
| 118 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 39 860.39 | 33 122.26 | 26 998.77 |
| 119 | javascript (14.16)| [fastify](https://fastify.io) (3.14) | 38 899.73 | 43 813.96 | 42 811.66 |
| 120 | javascript (14.16)| [fyrejet](https://github.com/fyrejet/fyrejet) (3.1) | 38 181.64 | 41 204.63 | 40 512.73 |
| 121 | python (3.9)| [starlette](https://starlette.io) (0.14) | 37 884.50 | 41 161.98 | 42 186.55 |
| 122 | elixir (1.12)| [plug](https://hexdocs.pm/plug) (1.11) | 36 795.84 | 40 040.48 | 39 103.71 |
| 123 | php (7.4)| [comet](https://github.com/gotzmann/comet) (1.2) | 36 210.20 | 37 140.07 | 36 835.13 |
| 124 | scala (2.13)| [play](https://playframework.com) (2.8) | 35 915.92 | 39 389.06 | 38 892.59 |
| 125 | javascript (14.16)| [moleculer](https://moleculer.services) (0.14) | 35 805.74 | 38 994.40 | 38 199.33 |
| 126 | clojure (1.1)| [luminus](https://luminusweb.com) (4.0) | 35 410.16 | 37 449.94 | 37 376.46 |
| 127 | crystal (0.36)| [orion](https://github.com/obsidian/orion) (3.0) | 34 954.74 | 35 362.67 | 31 837.74 |
| 128 | dart (2.12)| [aqueduct](https://aqueduct.io) (3.3) | 34 631.08 | 34 577.60 | 34 248.84 |
| 129 | javascript (14.16)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (3.1) | 33 710.12 | 35 616.21 | 35 582.18 |
| 130 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 32 923.37 | 24 123.66 | 25 732.92 |
| 131 | python (3.9)| [index.py](https://index-py.abersheeran.com) (0.17) | 32 892.74 | 31 839.42 | 30 637.71 |
| 132 | python (3.9)| [emmett](https://emmett.sh) (2.2) | 32 632.01 | 39 464.96 | 37 371.58 |
| 133 | elixir (1.12)| [phoenix](https://phoenixframework.org) (1.5) | 32 184.54 | 34 945.19 | 34 907.13 |
| 134 | haskell (8.8)| [servant](https://docs.servant.dev) (0.18) | 32 031.60 | 29 984.80 | 28 618.47 |
| 135 | javascript (14.16)| [restify](https://restify.com) (8.5) | 31 487.68 | 34 384.67 | 34 088.69 |
| 136 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 29 657.39 | 29 563.18 | 29 059.56 |
| 137 | crystal (0.36)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.17) | 29 501.80 | 28 178.90 | 22 664.88 |
| 138 | javascript (14.16)| [hapi](https://hapijs.com) (20.1) | 29 136.34 | 30 879.14 | 30 445.88 |
| 139 | python (3.9)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 27 955.51 | 31 876.56 | 31 166.15 |
| 140 | php (7.4)| [swoft](https://swoft.org) (2.0) | 26 799.78 | 34 191.19 | 33 980.37 |
| 141 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 26 679.87 | 26 234.61 | 26 189.93 |
| 142 | python (3.9)| [responder](https://python-responder.org) (2.0) | 25 838.67 | 27 953.04 | 27 630.20 |
| 143 | python (3.9)| [molten](https://moltenframework.com) (1.0) | 24 630.83 | 28 142.90 | 25 257.15 |
| 144 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 24 409.39 | 30 119.79 | 30 177.42 |
| 145 | python (3.9)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 871.08 | 26 860.83 | 26 081.04 |
| 146 | python (3.9)| [fastapi](https://fastapi.tiangolo.com) (0.63) | 23 628.68 | 23 781.04 | 22 947.37 |
| 147 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 23 312.86 | 28 523.31 | 29 183.76 |
| 148 | javascript (14.16)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 23 276.36 | 21 966.69 | 20 861.01 |
| 149 | go (1.16)| [macaron](https://go-macaron.com) (1.4) | 23 240.76 | 25 351.44 | 25 490.95 |
| 150 | java (11)| [jersey3-grizzly2](https://eclipse-ee4j.github.io/jersey) (3) | 23 033.75 | 32 045.91 | 31 882.71 |
| 151 | java (11)| [jersey-grizzly2](https://eclipse-ee4j.github.io/jersey) (2.33) | 22 893.81 | 30 753.54 | 30 641.14 |
| 152 | python (3.9)| [flask](https://flask.pocoo.org) (1.1) | 22 602.40 | 25 350.25 | 24 415.15 |
| 153 | ruby (3.0)| [roda](https://roda.jeremyevans.net) (3.43) | 22 190.69 | 21 640.45 | 21 341.36 |
| 154 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 21 691.18 | 25 602.80 | 25 385.42 |
| 155 | ruby (3.0)| [syro](https://github.com/soveran/syro) (3.2) | 21 549.33 | 20 730.14 | 20 911.76 |
| 156 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 21 480.32 | 18 782.16 | 17 456.16 |
| 157 | ruby (3.0)| [hanami-api](https://hanamirb.org) (0.1) | 21 364.36 | 20 501.10 | 20 534.54 |
| 158 | javascript (14.16)| [feathersjs](https://feathersjs.com) (4.5) | 19 622.62 | 19 618.15 | 19 412.10 |
| 159 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 19 278.22 | 17 247.14 | 16 095.88 |
| 160 | javascript (14.16)| [express](https://expressjs.com) (4.17) | 19 117.54 | 19 059.48 | 18 661.69 |
| 161 | ruby (3.0)| [cuba](https://cuba.is) (3.9) | 18 386.03 | 17 857.93 | 17 614.03 |
| 162 | php (7.4)| [antidot](https://github.com/antidot-framework/react-framework) (0.2) | 18 320.25 | 22 572.59 | 16 445.41 |
| 163 | ruby (3.0)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 17 842.02 | 17 110.92 | 17 119.84 |
| 164 | ruby (3.0)| [rack_app](https://rack-app.com) (7.7) | 17 191.10 | 16 416.73 | 16 199.73 |
| 165 | java (11)| [blade](https://lets-blade.com) (2.0) | 16 962.47 | 20 651.30 | 19 649.32 |
| 166 | go (1.16)| [tango](https://gitea.com/lunny/tango) (0.6) | 16 852.13 | 17 312.77 | 17 379.91 |
| 167 | ruby (3.0)| [camping](https://github.com/camping/camping) (2.1) | 16 438.46 | 15 860.33 | 15 996.47 |
| 168 | dart (2.12)| [start](https://github.com/lvivski/start) (0.4) | 15 282.89 | 14 828.78 | 14 483.51 |
| 169 | go (1.16)| [gramework](https://github.com/gramework/gramework) (1.7) | 14 882.50 | 15 202.26 | 15 211.77 |
| 170 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 14 557.51 | 14 790.36 | 14 926.78 |
| 171 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.7) | 14 277.61 | 14 454.27 | 14 525.63 |
| 172 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.6) | 14 131.07 | 14 392.21 | 14 534.58 |
| 173 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 14 123.95 | 14 313.72 | 14 469.88 |
| 174 | javascript (14.16)| [nestjs-express](https://nestjs.com) (7.6) | 13 925.40 | 14 202.78 | 14 107.22 |
| 175 | python (3.9)| [guillotina](https://guillotina.io) (6.2) | 13 516.50 | 12 740.37 | 12 105.62 |
| 176 | php (7.4)| [laravel-s-lumen](https://github.com/hhxsv5/laravel-s) (3.7) | 13 322.45 | 13 891.16 | 14 017.70 |
| 177 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 12 644.18 | 12 858.24 | 13 325.30 |
| 178 | python (3.9)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 11 506.62 | 10 670.01 | 10 208.36 |
| 179 | java (11)| [struts2](https://struts.apache.org) (2.5) | 11 358.53 | 12 035.00 | 12 106.00 |
| 180 | pony (0.39)| [jennet](https://github.com/Theodus/jennet) (0.1) | 11 142.97 | 19 495.27 | 18 036.62 |
| 181 | ruby (3.0)| [sinatra](https://sinatrarb.com) (2.1) | 10 983.09 | 10 693.42 | 10 701.72 |
| 182 | python (3.9)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 10 950.85 | 10 252.90 | 9 501.98 |
| 183 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 10 174.45 | 11 047.09 | 10 878.99 |
| 184 | python (3.9)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 973.78 | 9 433.71 | 9 410.09 |
| 185 | ruby (3.0)| [grape](https://ruby-grape.org) (1.5) | 9 931.73 | 9 672.95 | 9 648.92 |
| 186 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 9 918.71 | 10 619.70 | 10 515.59 |
| 187 | php (7.4)| [laravel-s-laravel](https://github.com/hhxsv5/laravel-s) (3.7) | 9 697.70 | 9 589.25 | 9 551.41 |
| 188 | python (3.9)| [tornado](https://tornadoweb.org) (6.1) | 8 580.85 | 8 668.14 | 8 314.25 |
| 189 | python (3.9)| [django](https://djangoproject.com) (3.2) | 8 469.82 | 8 148.59 | 7 982.05 |
| 190 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 349.40 | 8 781.92 | 8 746.85 |
| 191 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.2) | 7 393.77 | 7 741.25 | 7 683.20 |
| 192 | php (7.4)| [fatfree](https://fatfreeframework.com) (3.7) | 7 125.11 | 7 563.78 | 7 559.21 |
| 193 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 7 095.84 | 8 385.57 | 8 637.71 |
| 194 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 7 093.08 | 7 502.74 | 7 402.44 |
| 195 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 6 794.38 | 7 185.11 | 7 157.68 |
| 196 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 620.95 | 7 002.21 | 6 938.23 |
| 197 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.3) | 5 923.02 | 6 300.41 | 6 294.76 |
| 198 | php (7.4)| [ice](https://iceframework.org) (1.5) | 5 816.35 | 6 138.73 | 6 079.25 |
| 199 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.6) | 5 529.42 | 5 749.38 | 5 777.06 |
| 200 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 5 055.81 | 5 241.32 | 5 226.36 |
| 201 | javascript (14.16)| [sails](https://sailsjs.com) (1.4) | 5 025.51 | 5 127.94 | 5 093.85 |
| 202 | php (7.4)| [cubex](https://cubex.io) (4.16) | 4 526.06 | 4 604.52 | 4 627.63 |
| 203 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.6) | 4 349.73 | 4 454.07 | 4 472.87 |
| 204 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 299.62 | 4 429.45 | 4 483.43 |
| 205 | php (7.4)| [lumen](https://lumen.laravel.com) (8.2) | 3 877.83 | 3 926.90 | 3 957.93 |
| 206 | php (7.4)| [nette](https://nette.org/en/) (3.1) | 3 848.96 | 3 968.97 | 3 953.07 |
| 207 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 3 735.00 | 3 819.60 | 3 835.23 |
| 208 | php (7.4)| [driftphp](https://github.com/driftphp/driftphp) (0.1) | 3 718.53 | 4 617.75 | 4 925.58 |
| 209 | python (3.9)| [masonite](https://masoniteproject.com) (3.0) | 3 514.67 | 3 444.85 | 3 434.66 |
| 210 | ruby (3.0)| [rails](https://rubyonrails.org) (6.1) | 3 511.98 | 3 490.66 | 3 487.98 |
| 211 | php (7.4)| [symfony](https://symfony.com) (5.2) | 3 025.39 | 3 035.91 | 3 044.85 |
| 212 | julia (1.6)| [merly](https://github.com/codeneomatrix/Merly.jl) (1.0) | 3 008.97 | 8 245.98 | 4 368.51 |
| 213 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.3) | 2 742.17 | 2 793.25 | 2 790.51 |
| 214 | python (3.9)| [cyclone](https://cyclone.io) (1.3) | 2 338.34 | 2 315.88 | 2 297.83 |
| 215 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 2 001.81 | 1 993.11 | 853.75 |
| 216 | python (3.9)| [django-ninja](https://django-ninja.rest-framework.com) (0.12) | 1 961.98 | 2 599.55 | 2 511.67 |
| 217 | r (4.0)| [restrserve](https://restrserve.org) (0.4) | 1 820.13 | 1 775.33 | 1 784.65 |
| 218 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 810.73 | 1 741.67 | 1 684.50 |
| 219 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 741.32 | 1 763.24 | 1 765.97 |
| 220 | python (3.9)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 693.27 | 1 633.58 | 1 615.32 |
| 221 | php (7.4)| [laravel](https://laravel.com) (8.37) | 1 494.34 | 1 503.23 | 1 504.42 |
| 222 | python (3.9)| [klein](https://github.com/twisted/klein) (20.6) | 1 478.95 | 1 511.37 | 1 496.63 |
| 223 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.1) | 1 206.07 | 1 228.74 | 1 227.87 |
| 224 | php (7.4)| [unic](https://unicframework.github.io/docs) (1.0) | 466.73 | 495.17 | 425.34 |
| 225 | r (4.0)| [plumber](https://rplumber.io) (1.1) | 421.16 | 446.93 | 435.65 |
| 226 | cpp (11)| [nawa](https://github.com/jatofg/nawa) (0.6) | 362.61 | NaN | NaN |
</a>

</details>

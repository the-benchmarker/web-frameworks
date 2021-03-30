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

## Results (2021-03-30)



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
| 1 | go (1.16)| [fiber](https://gofiber.io) (2.7) | 181 738.93 | 193 051.05 | 191 956.56 |
| 2 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 179 357.94 | 202 575.49 | 205 324.74 |
| 3 | go (1.16)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.6) | 178 539.18 | 187 758.31 | 187 422.86 |
| 4 | go (1.16)| [gearbox](https://gogearbox.com) (1.2) | 178 461.29 | 181 710.21 | 181 467.14 |
| 5 | go (1.16)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 176 439.89 | 186 124.47 | 185 736.67 |
| 6 | go (1.16)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 176 035.32 | 186 591.07 | 186 216.31 |
| 7 | go (1.16)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.23) | 173 349.51 | 192 724.32 | 193 976.17 |
| 8 | java (11)| [activej](https://activej.io) (4.1) | 170 279.55 | 209 374.97 | 212 739.70 |
| 9 | javascript (14.16)| [nanoexpress](https://github.com/nanoexpress/nanoexpress) (2.4) | 161 627.72 | 208 550.32 | 213 164.64 |
| 10 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 160 836.36 | 198 123.18 | 202 580.93 |
| 11 | javascript (14.16)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 158 132.68 | 200 966.04 | 204 230.15 |
| 12 | java (11)| [undertow](https://undertow.io) (2.2) | 157 555.59 | 193 895.74 | 197 300.15 |
| 13 | php (7.4)| [webman](https://github.com/walkor/webman) (1.0) | 156 786.57 | 168 652.75 | 171 309.03 |
| 14 | java (11)| [jooby](https://jooby.io) (2.9) | 156 202.01 | 198 689.89 | 205 704.06 |
| 15 | java (11)| [vertx](https://vertx.io/docs/vertx-core/java/) (4.0) | 155 094.88 | 185 523.41 | 187 698.36 |
| 16 | rust (1.51)| [actix](https://actix.rs) (3.3) | 154 992.39 | 191 867.59 | 193 299.97 |
| 17 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 154 355.50 | 175 881.83 | 181 595.67 |
| 18 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 152 969.60 | 188 822.54 | 192 962.62 |
| 19 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 152 673.89 | 184 154.73 | 188 996.85 |
| 20 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 150 229.90 | 189 929.84 | 196 511.34 |
| 21 | kotlin (1.4)| [kooby](https://jooby.io) (2.9) | 149 834.53 | 190 024.69 | 205 470.41 |
| 22 | java (11)| [vertx4web](https://vertx.io/docs/vertx-web/java/) (4.0) | 149 774.23 | 177 458.69 | 180 043.09 |
| 23 | crystal (0.36)| [kemal](https://kemalcr.com) (0.27) | 144 801.08 | 161 516.67 | 162 060.61 |
| 24 | crystal (0.36)| [toro](https://github.com/soveran/toro) (0.4) | 143 453.04 | 175 765.48 | 176 617.62 |
| 25 | crystal (0.36)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 141 328.19 | 170 081.98 | 173 926.69 |
| 26 | crystal (0.36)| [spider-gazelle](https://spider-gazelle.net) (4.2) | 140 528.92 | 168 241.43 | 168 783.89 |
| 27 | crystal (0.36)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 138 980.65 | 167 389.83 | 167 148.94 |
| 28 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 134 781.16 | 171 931.22 | 181 639.82 |
| 29 | crystal (0.36)| [amber](https://amberframework.org) (0.36) | 127 957.52 | 150 332.20 | 148 778.80 |
| 30 | java (11)| [quarkus](https://quarkus.io) (1.12) | 123 715.85 | 153 149.66 | 156 861.52 |
| 31 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 122 178.27 | 145 106.64 | 149 180.33 |
| 32 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 120 014.03 | 155 564.33 | 167 339.17 |
| 33 | go (1.16)| [clevergo](https://clevergo.tech) (0.5) | 118 306.60 | 119 870.59 | 122 990.24 |
| 34 | go (1.16)| [rte](https://github.com/jwilner/rte) (0.0) | 118 275.33 | 119 425.08 | 122 658.57 |
| 35 | go (1.16)| [echo](https://echo.labstack.com) (4.2) | 117 228.82 | 118 738.93 | 121 562.28 |
| 36 | javascript (14.16)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 116 787.06 | 135 775.48 | 138 369.80 |
| 37 | go (1.16)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 116 725.62 | 117 386.39 | 120 509.95 |
| 38 | clojure (1.1)| [donkey](https://github.com/AppsFlyer/donkey) (0.5) | 115 413.89 | 122 160.81 | 123 364.42 |
| 39 | go (1.16)| [gin](https://gin-gonic.com) (1.6) | 114 512.77 | 119 982.16 | 122 277.18 |
| 40 | java (11)| [act](https://github.com/actframework/actframework) (1.9) | 114 186.21 | 145 301.21 | 149 039.55 |
| 41 | go (1.16)| [aero](https://github.com/aerogo/aero) (1.3) | 113 271.46 | 113 600.44 | 116 458.34 |
| 42 | go (1.16)| [chi](https://github.com/go-chi/chi) (1.5) | 112 104.35 | 110 307.67 | 114 355.79 |
| 43 | go (1.16)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 111 440.48 | 118 981.77 | 120 328.47 |
| 44 | go (1.16)| [violetear](https://violetear.org) (7.0) | 110 413.00 | 109 261.17 | 112 505.41 |
| 45 | go (1.16)| [webgo](https://github.com/bnkamalesh/webgo) (5.0) | 109 113.84 | 107 620.94 | 110 811.29 |
| 46 | go (1.16)| [goroute](https://goroute.github.io) (0.0) | 108 720.22 | 105 971.92 | 110 074.18 |
| 47 | go (1.16)| [kami](https://github.com/guregu/kami) (2.2) | 108 222.83 | 112 937.61 | 113 761.01 |
| 48 | rust (1.51)| [iron](https://iron/iron) (0.6) | 107 927.23 | 103 162.53 | 104 469.99 |
| 49 | fsharp (5.0)| [falco](https://falcoframework.com) (3.0) | 107 808.79 | 126 108.89 | 129 574.74 |
| 50 | scala (2.13)| [finatra](https://twitter.github.io/finatra/) (21.1) | 107 165.87 | 127 393.13 | 128 044.53 |
| 51 | crystal (0.36)| [athena](https://github.com/athena-framework/athena) (0.13) | 106 966.05 | 119 203.70 | 112 537.67 |
| 52 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 105 566.30 | 122 653.03 | 126 270.23 |
| 53 | go (1.16)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 104 896.08 | 100 450.84 | 105 109.34 |
| 54 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 104 395.02 | 120 658.41 | 124 148.05 |
| 55 | javascript (14.16)| [low-http-server](https://github.com/jkyberneees/low-http-server) (2.1) | 103 969.31 | 117 024.14 | 117 670.00 |
| 56 | rust (1.51)| [salvo](https://github.com/salvo-rs/salvo) (0.9) | 103 584.40 | 128 829.43 | 140 056.37 |
| 57 | php (7.4)| [swoole](https://github.com/swoole/swoole-src) (4.6) | 101 295.72 | 124 132.16 | 137 300.67 |
| 58 | go (1.16)| [beego](https://beego.me) (1.12) | 101 186.01 | 105 023.14 | 108 295.28 |
| 59 | rust (1.51)| [gotham](https://gotham.rs) (0.6) | 100 136.37 | 124 511.71 | 135 842.80 |
| 60 | go (1.16)| [goyave](https://github.com/go-goyave/goyave) (3.7) | 94 967.37 | 95 453.86 | 99 383.14 |
| 61 | java (11)| [restheart](https://restheart.org) (5.3) | 93 994.82 | 100 318.13 | 100 162.64 |
| 62 | go (1.16)| [air](https://github.com/aofei/air) (0.21) | 93 779.23 | 92 472.13 | 96 206.59 |
| 63 | php (7.4)| [nano](https://gitlab.com/x.laylatichy.x/nano) (0.0.9) | 93 591.06 | 137 798.37 | 149 800.56 |
| 64 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 92 837.30 | 102 596.65 | 102 063.42 |
| 65 | c (11)| [kore](https://kore.io) (3.3) | 88 733.02 | 145 325.05 | 159 885.40 |
| 66 | php (7.4)| [swoole-coroutine](https://github.com/swoole/swoole-src) (4.6) | 88 545.77 | 126 317.06 | 137 253.53 |
| 67 | javascript (14.16)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 88 103.73 | 97 476.10 | 98 959.22 |
| 68 | rust (1.51)| [nickel](https://nickel-org.github.io) (0.11) | 86 952.74 | 94 842.50 | 96 616.29 |
| 69 | javascript (14.16)| [0http](https://github.com/jkyberneees/0http) (3.1) | 84 639.35 | 93 984.41 | 95 244.26 |
| 70 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 84 283.12 | 90 224.49 | 87 643.93 |
| 71 | scala (2.13)| [finch](https://finagle.github.io/finch/) (0.32) | 83 620.28 | 96 103.68 | 96 043.88 |
| 72 | scala (2.13)| [akkahttp](https://akka.io) (10.2) | 80 186.65 | 94 976.55 | 92 014.42 |
| 73 | go (1.16)| [gf](https://goframe.org) (1.15) | 80 055.23 | 87 644.60 | 89 561.14 |
| 74 | javascript (14.16)| [restana](https://github.com/jkyberneees/ana) (4.7) | 78 494.26 | 88 573.51 | 89 241.32 |
| 75 | swift (5.3)| [hummingbird](https://github.com/hummingbird-project/hummingbird) (0.7) | 76 535.10 | 83 651.51 | 84 415.35 |
| 76 | javascript (14.16)| [rayo](https://rayo.js.org) (1.3) | 76 243.04 | 83 648.53 | 82 282.36 |
| 77 | javascript (14.16)| [polka](https://github.com/lukeed/polka) (0.5) | 76 227.66 | 83 739.98 | 82 441.42 |
| 78 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 75 716.16 | 79 516.56 | 77 769.21 |
| 79 | php (7.4)| [one](https://github.com/lizhichao/one) (2.2) | 75 585.95 | 88 703.33 | 99 135.67 |
| 80 | php (7.4)| [hyperf](https://hyperf.io) (2.1) | 74 644.84 | 86 138.10 | 96 855.51 |
| 81 | ruby (3.0)| [agoo](https://github.com/ohler55/agoo) (2.14) | 74 520.30 | 104 998.16 | 116 349.92 |
| 82 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 73 964.67 | 88 570.30 | 91 767.51 |
| 83 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 73 815.02 | 81 505.21 | 91 193.50 |
| 84 | python (3.9)| [falcon](https://falconframework.org) (2.0) | 73 069.11 | 79 366.47 | 81 667.13 |
| 85 | java (11)| [javalin](https://javalin.io) (3.9) | 72 779.68 | 79 541.54 | 79 412.44 |
| 86 | fsharp (5.0)| [websharper](https://websharper.com) (4.7) | 72 648.19 | 81 943.41 | 81 536.84 |
| 87 | java (11)| [spark](https://sparkjava.com) (2.9) | 71 526.67 | 79 787.96 | 81 180.11 |
| 88 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 71 007.85 | 93 619.78 | 96 867.70 |
| 89 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.4) | 70 093.97 | 63 766.68 | 62 912.46 |
| 90 | javascript (14.16)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 68 201.74 | 74 844.85 | 73 247.47 |
| 91 | java (11)| [spring](https://spring.io/projects/spring-boot) (2.4) | 67 425.20 | 77 131.58 | 77 785.11 |
| 92 | go (1.16)| [mars](https://github.com/roblillack/mars) (1.0) | 65 301.75 | 66 692.41 | 71 338.81 |
| 93 | javascript (14.16)| [foxify](https://foxify.js.org) (0.1) | 65 119.79 | 69 394.76 | 67 997.64 |
| 94 | javascript (14.16)| [nestjs-fastify](https://nestjs.com) (7.6) | 62 143.48 | 67 330.23 | 66 589.55 |
| 95 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 60 864.99 | 66 128.61 | 73 451.43 |
| 96 | python (3.9)| [bottle](https://bottlepy.org) (0.12) | 60 639.41 | 64 766.09 | 66 550.90 |
| 97 | javascript (14.16)| [koa](https://koajs.com) (2.13) | 59 302.28 | 62 785.56 | 61 682.77 |
| 98 | javascript (14.16)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 57 279.51 | 60 449.98 | 58 964.66 |
| 99 | python (3.9)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 50 705.66 | 55 999.47 | 56 978.11 |
| 100 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 49 706.32 | 52 354.43 | 53 467.60 |
| 101 | swift (5.3)| [vapor](https://vapor.codes) (4.41) | 49 468.54 | 52 618.53 | 52 199.62 |
| 102 | javascript (14.16)| [moleculer](https://moleculer.services) (0.14) | 49 322.49 | 51 839.51 | 50 256.15 |
| 103 | javascript (14.16)| [fastify](https://fastify.io) (3.14) | 49 176.25 | 53 721.72 | 52 553.57 |
| 104 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 48 966.29 | 50 212.87 | 50 520.05 |
| 105 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 48 774.28 | 50 357.55 | 50 006.87 |
| 106 | swift (5.3)| [kitura](https://kitura.dev) (2.9) | 48 392.73 | 49 564.84 | 49 681.77 |
| 107 | python (3.9)| [hug](https://hug.rest) (2.6) | 48 122.70 | 51 676.97 | 51 775.43 |
| 108 | javascript (14.16)| [fyrejet](https://github.com/fyrejet/fyrejet) (3.1) | 47 891.39 | 50 736.10 | 49 743.19 |
| 109 | python (3.9)| [pyramid](https://trypyramid.com) (2.0) | 47 798.63 | 52 816.39 | 52 750.95 |
| 110 | java (11)| [micronaut](https://micronaut.io) (1.2) | 47 091.22 | 54 448.84 | 54 516.41 |
| 111 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 46 924.56 | 56 839.47 | 60 450.67 |
| 112 | python (3.9)| [blacksheep](https://github.com/Neoteroi/BlackSheep) (1.0) | 44 301.08 | 48 653.76 | 50 497.87 |
| 113 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 43 388.25 | 50 723.76 | 50 629.11 |
| 114 | python (3.9)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 43 150.48 | 49 788.09 | 51 485.79 |
| 115 | php (7.4)| [imi](https://imiphp.com) (1.2) | 42 372.91 | 47 209.38 | 50 341.99 |
| 116 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 42 331.31 | 43 181.66 | 42 186.67 |
| 117 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 41 868.75 | 46 129.04 | 45 788.19 |
| 118 | python (3.9)| [sanic](https://github.com/sanic-org/sanic) (21.3) | 41 729.60 | 45 603.50 | 46 817.19 |
| 119 | javascript (14.16)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (3.1) | 41 720.01 | 44 704.13 | 45 531.28 |
| 120 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 41 299.99 | 46 064.83 | 46 106.47 |
| 121 | fsharp (5.0)| [suave](https://suave.io) (2.6) | 40 894.03 | 39 728.66 | 34 813.58 |
| 122 | javascript (14.16)| [restify](https://restify.com) (8.5) | 40 049.36 | 44 302.23 | 42 869.30 |
| 123 | python (3.9)| [starlette](https://starlette.io) (0.14) | 38 892.62 | 45 524.99 | 46 536.99 |
| 124 | php (7.4)| [comet](https://github.com/gotzmann/comet) (1.2) | 37 366.79 | 38 039.01 | 37 199.08 |
| 125 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.11) | 37 168.35 | 40 420.61 | 39 570.58 |
| 126 | scala (2.13)| [play](https://playframework.com) (2.8) | 37 132.05 | 40 001.88 | 40 222.99 |
| 127 | javascript (14.16)| [hapi](https://hapijs.com) (20.1) | 35 374.31 | 38 617.27 | 37 701.66 |
| 128 | dart (2.12)| [aqueduct](https://aqueduct.io) (3.3) | 34 857.44 | 34 852.91 | 34 355.29 |
| 129 | crystal (0.36)| [orion](https://github.com/obsidian/orion) (3.0) | 34 326.47 | 33 759.84 | 30 746.76 |
| 130 | clojure (1.1)| [luminus](https://luminusweb.com) (3.98) | 34 026.37 | 36 052.40 | 35 816.69 |
| 131 | python (3.9)| [emmett](https://emmett.sh) (2.2) | 33 358.29 | 40 226.23 | 40 997.48 |
| 132 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 32 747.92 | 30 729.78 | 29 621.44 |
| 133 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 32 389.13 | 25 429.08 | 27 622.94 |
| 134 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 32 379.32 | 32 170.28 | 31 521.79 |
| 135 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 31 958.53 | 35 662.06 | 34 980.42 |
| 136 | java (11)| [jersey3-grizzly2](https://eclipse-ee4j.github.io/jersey) (3) | 31 621.94 | 37 299.72 | 36 998.03 |
| 137 | java (11)| [jersey-grizzly2](https://eclipse-ee4j.github.io/jersey) (2.33) | 30 025.97 | 35 663.65 | 35 200.81 |
| 138 | python (3.9)| [index.py](https://index-py.abersheeran.com) (0.17) | 29 977.70 | 36 731.14 | 36 321.14 |
| 139 | crystal (0.36)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.17) | 29 623.60 | 29 331.68 | 26 223.43 |
| 140 | python (3.9)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 28 949.00 | 33 255.03 | 34 040.23 |
| 141 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 27 180.15 | 26 760.20 | 27 477.56 |
| 142 | python (3.9)| [responder](https://python-responder.org) (2.0) | 26 097.21 | 31 275.00 | 30 828.51 |
| 143 | javascript (14.16)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 25 695.54 | 24 910.66 | 23 548.39 |
| 144 | python (3.9)| [molten](https://moltenframework.com) (1.0) | 25 279.76 | 30 279.92 | 29 974.19 |
| 145 | php (7.4)| [swoft](https://swoft.org) (2.0) | 25 053.93 | 30 067.43 | 31 414.55 |
| 146 | python (3.9)| [fastapi](https://fastapi.tiangolo.com) (0.63) | 24 803.12 | 31 921.66 | 31 523.56 |
| 147 | python (3.9)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 24 215.81 | 28 207.32 | 28 070.98 |
| 148 | ruby (3.0)| [syro](https://github.com/soveran/syro) (3.2) | 23 458.76 | 22 703.65 | 22 360.65 |
| 149 | ruby (3.0)| [hanami-api](https://hanamirb.org) (0.1) | 23 252.86 | 22 710.24 | 22 275.13 |
| 150 | go (1.16)| [macaron](https://go-macaron.com) (1.4) | 22 976.23 | 25 148.74 | 25 200.26 |
| 151 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 22 855.60 | 26 568.70 | 26 741.50 |
| 152 | ruby (3.0)| [roda](https://roda.jeremyevans.net) (3.42) | 22 373.69 | 21 817.24 | 21 438.99 |
| 153 | python (3.9)| [flask](https://flask.pocoo.org) (1.1) | 21 771.82 | 25 802.37 | 23 584.90 |
| 154 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 21 665.23 | 19 154.03 | 17 867.38 |
| 155 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 21 300.44 | 24 570.37 | 25 776.04 |
| 156 | javascript (14.16)| [express](https://expressjs.com) (4.17) | 21 241.01 | 26 099.87 | 25 984.55 |
| 157 | javascript (14.16)| [feathersjs](https://feathersjs.com) (4.5) | 20 811.00 | 26 547.03 | 25 779.80 |
| 158 | php (7.4)| [slim-swoole](https://slimframework.com) (4.7) | 20 734.94 | 24 088.42 | 24 188.14 |
| 159 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 19 817.38 | 17 879.98 | 16 532.47 |
| 160 | ruby (3.0)| [cuba](https://cuba.is) (3.9) | 19 810.49 | 19 430.86 | 19 040.19 |
| 161 | ruby (3.0)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 19 268.38 | 18 786.75 | 18 508.68 |
| 162 | java (11)| [blade](https://lets-blade.com) (2.0) | 18 926.13 | 22 382.18 | 19 994.08 |
| 163 | ruby (3.0)| [rack_app](https://rack-app.com) (7.7) | 18 407.10 | 17 542.90 | 17 092.46 |
| 164 | ruby (3.0)| [camping](https://github.com/camping/camping) (2.1) | 17 607.00 | 17 117.63 | 16 949.98 |
| 165 | go (1.16)| [tango](https://gitea.com/lunny/tango) (0.6) | 16 872.50 | 17 307.22 | 17 377.64 |
| 166 | javascript (14.16)| [nestjs-express](https://nestjs.com) (7.6) | 15 686.75 | 17 842.39 | 17 982.35 |
| 167 | dart (2.12)| [start](https://github.com/lvivski/start) (0.4) | 15 132.48 | 14 844.58 | 14 434.41 |
| 168 | go (1.16)| [gramework](https://github.com/gramework/gramework) (1.7) | 14 670.18 | 15 004.27 | 15 006.37 |
| 169 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 14 559.51 | 14 884.29 | 15 033.84 |
| 170 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.7) | 14 075.84 | 14 373.19 | 14 501.16 |
| 171 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.6) | 14 067.87 | 14 509.34 | 14 590.28 |
| 172 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.7) | 13 995.73 | 14 165.88 | 14 287.35 |
| 173 | python (3.9)| [guillotina](https://guillotina.io) (6.2) | 13 223.68 | 13 061.75 | 12 685.41 |
| 174 | php (7.4)| [laravel-s-lumen](https://github.com/hhxsv5/laravel-s) (3.7) | 13 058.48 | 13 779.23 | 13 475.73 |
| 175 | java (11)| [struts2](https://struts.apache.org) (2.5) | 12 446.38 | 13 154.62 | 13 157.92 |
| 176 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 12 376.60 | 12 405.74 | 12 474.66 |
| 177 | ruby (3.0)| [sinatra](https://sinatrarb.com) (2.1) | 12 245.97 | 11 937.10 | 11 910.38 |
| 178 | pony (0.39)| [jennet](https://github.com/Theodus/jennet) (0.1) | 11 631.52 | 19 818.41 | 18 774.08 |
| 179 | python (3.9)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 10 916.05 | 10 258.68 | 9 578.51 |
| 180 | ruby (3.0)| [grape](https://ruby-grape.org) (1.5) | 10 891.49 | 10 599.61 | 10 584.05 |
| 181 | python (3.9)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 10 811.99 | 10 608.05 | 10 136.71 |
| 182 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 10 151.17 | 10 978.83 | 10 867.69 |
| 183 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 9 900.87 | 10 649.55 | 10 477.82 |
| 184 | python (3.9)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 650.12 | 10 332.85 | 9 525.27 |
| 185 | php (7.4)| [laravel-s-laravel](https://github.com/hhxsv5/laravel-s) (3.7) | 9 532.43 | 9 506.85 | 9 394.13 |
| 186 | python (3.9)| [django](https://djangoproject.com) (3.1) | 8 742.48 | 8 391.62 | 8 271.70 |
| 187 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 8 264.15 | 8 811.04 | 8 703.32 |
| 188 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 8 069.86 | 9 434.31 | 9 498.29 |
| 189 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.2) | 7 318.92 | 7 761.42 | 7 582.63 |
| 190 | php (7.4)| [fatfree](https://fatfreeframework.com) (3.7) | 7 236.74 | 7 537.30 | 7 523.69 |
| 191 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 7 075.08 | 7 437.58 | 7 409.16 |
| 192 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 6 896.67 | 7 172.76 | 7 130.20 |
| 193 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 566.36 | 6 988.22 | 6 920.27 |
| 194 | javascript (14.16)| [sails](https://sailsjs.com) (1.4) | 6 098.56 | 6 137.36 | 6 087.52 |
| 195 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.3) | 5 899.65 | 6 272.55 | 6 246.24 |
| 196 | php (7.4)| [ice](https://iceframework.org) (1.5) | 5 880.71 | 6 061.67 | 6 044.81 |
| 197 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.6) | 5 435.95 | 5 746.04 | 5 768.90 |
| 198 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.4) | 5 073.26 | 5 249.49 | 5 259.25 |
| 199 | php (7.4)| [cubex](https://cubex.io) (4.16) | 4 492.90 | 4 602.10 | 4 593.55 |
| 200 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.6) | 4 327.97 | 4 465.69 | 4 438.64 |
| 201 | php (7.4)| [slim](https://slimframework.com) (4.7) | 4 317.30 | 4 462.44 | 4 473.56 |
| 202 | ruby (3.0)| [rails](https://rubyonrails.org) (6.1) | 3 979.91 | 3 975.75 | 3 976.76 |
| 203 | php (7.4)| [lumen](https://lumen.laravel.com) (8.2) | 3 892.75 | 3 927.83 | 3 940.45 |
| 204 | php (7.4)| [nette](https://nette.org/en/) (3.1) | 3 827.65 | 3 959.58 | 3 956.12 |
| 205 | julia (1.6)| [merly](https://github.com/codeneomatrix/Merly.jl) (1.0) | 3 781.65 | 8 556.05 | 5 280.68 |
| 206 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 3 705.78 | 3 822.29 | 3 824.19 |
| 207 | python (3.9)| [masonite](https://masoniteproject.com) (3.0) | 3 651.08 | 3 569.59 | 3 577.65 |
| 208 | php (7.4)| [symfony](https://symfony.com) (5.2) | 2 975.09 | 2 996.91 | 3 006.47 |
| 209 | php (7.4)| [antidot](https://github.com/antidot-framework/react-framework) (0.2) | 2 840.51 | 3 574.86 | 1 986.71 |
| 210 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.3) | 2 731.77 | 2 768.79 | 2 761.83 |
| 211 | python (3.9)| [cyclone](https://cyclone.io) (1.3) | 2 374.96 | 2 367.20 | 2 330.90 |
| 212 | php (7.4)| [driftphp](https://github.com/driftphp/driftphp) (0.1) | 2 203.71 | 3 973.69 | 2 117.54 |
| 213 | python (3.9)| [django-ninja](https://django-ninja.rest-framework.com) (0.12) | 1 888.57 | 2 463.62 | 2 498.72 |
| 214 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 1 832.68 | 1 771.57 | 1 760.00 |
| 215 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 792.94 | 1 541.58 | 807.05 |
| 216 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 791.00 | 1 739.09 | 1 670.88 |
| 217 | python (3.9)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 771.61 | 1 686.55 | 1 700.14 |
| 218 | php (7.4)| [laminas](https://getlaminas.org) (3.2) | 1 712.66 | 1 727.86 | 1 731.63 |
| 219 | python (3.9)| [klein](https://github.com/twisted/klein) (20.6) | 1 490.57 | 1 512.16 | 1 498.60 |
| 220 | php (7.4)| [laravel](https://laravel.com) (8.34) | 1 471.95 | 1 483.63 | 1 481.52 |
| 221 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.1) | 1 177.13 | 1 197.58 | 1 198.94 |
| 222 | php (7.4)| [unic](https://unicframework.github.io/docs) (1.0) | 510.83 | 457.63 | 447.35 |
| 223 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 417.76 | 445.45 | 431.93 |
| 224 | cpp (11)| [nawa](https://github.com/jatofg/nawa) (0.6) | 265.17 | NaN | NaN |
</a>

</details>

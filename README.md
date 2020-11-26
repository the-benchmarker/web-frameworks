# Which is the fastest?

[![Build Status](https://the-benchmarker.semaphoreci.com/badges/web-frameworks/branches/master.svg)](https://the-benchmarker.semaphoreci.com/projects/web-frameworks)

This project aims to be a load benchmarking suite, no more, no less

> Measuring response times (routing times) for each framework (middleware).


<div align="center">
:warning::warning::warning::warning::warning::warning::warning::warning:
</div>

<div align="center">Results are not <b>production-ready</b> <i>yet</i></div>

<div align="center">
:warning::warning::warning::warning::warning::warning::warning::warning:
</div>

### Additional purposes :

+ Helping decide between languages, depending on use case
+ Learning languages, best practices, devops culture ...
+ Having fun :heart:

## Requirements

+ [Ruby](https://ruby-lang.org) as `built-in` tools are made in this language
+ [Docker](https://www.docker.com) as **frameworks** are `isolated` into _containers_
+ [wrk](https://github.com/wg/wrk) as benchmarking tool, `>= 4.1.0`
+ [postgresql](https://www.postgresql.org) to store data, `>= 10`

:information_source::information_source::information_source::information_source::information_source:

:warning: On `OSX` you need `docker-machine` to use `docker` containerization

~~~
brew install docker-machine
docker-machine create default
eval $(docker-machine env default)
~~~

:information_source::information_source::information_source::information_source::information_source:

## Usage

... to be documented ...

feel free to create an issue if you want to try this project

## Results

:information_source:  Updated on **2020-11-26** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | go (1.15)| [fiber](https://gofiber.io) (2.2) | 185 462.94 | 194 998.67 | 193 232.53 |
| 2 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 181 634.88 | 184 591.82 | 183 869.82 |
| 3 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 176 973.72 | 218 862.21 | 223 415.43 |
| 4 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 176 015.15 | 192 350.97 | 191 598.97 |
| 5 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.5) | 175 927.85 | 193 100.68 | 191 584.90 |
| 6 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.17) | 175 461.54 | 194 887.23 | 195 563.55 |
| 7 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 174 405.20 | 192 877.41 | 190 996.40 |
| 8 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 172 162.09 | 201 733.33 | 203 629.12 |
| 9 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 172 143.31 | 212 904.46 | 217 018.50 |
| 10 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 164 913.13 | 203 632.00 | 206 822.14 |
| 11 | java (11)| [jooby](https://jooby.io) (2.8) | 164 803.07 | 208 790.89 | 215 759.03 |
| 12 | javascript (14.15)| [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 164 301.51 | 208 193.39 | 212 855.12 |
| 13 | kotlin (1.4)| [kooby](https://jooby.io) (2.8) | 164 297.30 | 205 569.47 | 212 325.44 |
| 14 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 160 961.42 | 199 001.91 | 203 983.34 |
| 15 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 159 979.03 | 201 449.56 | 208 312.88 |
| 16 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 150 320.77 | 181 076.69 | 182 382.44 |
| 17 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 149 997.92 | 181 878.61 | 180 908.04 |
| 18 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 148 634.67 | 180 293.65 | 183 110.17 |
| 19 | crystal (0.35)| [spider-gazelle](https://spider-gazelle.net) (3.3) | 145 021.43 | 178 224.26 | 179 760.03 |
| 20 | java (11)| [act](https://actframework.org) (1.9) | 141 582.60 | 174 592.53 | 179 036.99 |
| 21 | crystal (0.35)| [kemal](https://kemalcr.com) (0.26) | 139 629.42 | 168 209.18 | 168 286.62 |
| 22 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 138 892.84 | 171 093.13 | 168 809.68 |
| 23 | crystal (0.35)| [amber](https://amberframework.org) (0.35) | 133 161.39 | 158 362.08 | 158 130.49 |
| 24 | javascript (14.15)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 125 669.39 | 148 681.94 | 150 270.64 |
| 25 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.11) | 122 782.76 | 142 711.29 | 135 928.16 |
| 26 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.1) | 122 361.36 | 124 388.34 | 128 723.99 |
| 27 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 122 268.04 | 123 038.23 | 126 206.01 |
| 28 | go (1.15)| [clevergo](https://clevergo.tech) (0.3) | 121 403.68 | 122 478.17 | 125 834.97 |
| 29 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 120 531.77 | 121 431.55 | 124 961.73 |
| 30 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 120 093.75 | 121 231.46 | 124 558.54 |
| 31 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 117 728.58 | 122 778.45 | 125 439.60 |
| 32 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 117 565.52 | 123 571.23 | 125 741.57 |
| 33 | go (1.15)| [violetear](https://violetear.org) (7.0) | 116 397.13 | 115 901.71 | 119 429.04 |
| 34 | go (1.15)| [chi](https://github.com/go-chi/chi) (4.1) | 115 777.81 | 114 089.86 | 117 797.06 |
| 35 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 115 731.38 | 198 074.91 | 167 562.46 |
| 36 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 114 775.92 | 113 754.05 | 117 318.15 |
| 37 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 113 225.46 | 118 713.99 | 119 384.25 |
| 38 | fsharp (5.0)| [falco](https://github.com/pimbrouwers/Falco) (2.1) | 113 186.58 | 132 046.14 | 136 841.56 |
| 39 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 113 015.10 | 111 355.92 | 114 699.83 |
| 40 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 112 620.05 | 131 111.85 | 135 606.98 |
| 41 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 111 648.49 | 177 755.93 | 195 044.31 |
| 42 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 110 943.16 | 107 182.58 | 112 209.95 |
| 43 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 109 914.22 | 106 300.91 | 110 655.73 |
| 44 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.2) | 108 957.18 | 126 345.24 | 130 037.77 |
| 45 | go (1.15)| [beego](https://beego.me) (1.12) | 107 981.10 | 111 724.75 | 114 933.78 |
| 46 | go (1.15)| [air](https://github.com/aofei/air) (0.20) | 98 222.15 | 93 952.97 | 98 241.16 |
| 47 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 96 116.54 | 107 938.81 | 108 493.27 |
| 48 | php (7.4)| [nano](https://) (0.0.9) | 95 171.23 | 138 851.51 | 149 972.33 |
| 49 | rust (1.48)| [actix](https://actix.rs) (3.3) | 94 532.37 | 92 710.74 | 99 354.93 |
| 50 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.4) | 94 446.27 | 93 097.75 | 97 451.19 |
| 51 | javascript (14.15)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 93 745.89 | 102 144.26 | 102 623.83 |
| 52 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 92 713.22 | 100 916.37 | 100 499.31 |
| 53 | javascript (14.15)| [polka](https://github.com/lukeed/polka) (0.5) | 87 649.69 | 94 849.81 | 93 665.44 |
| 54 | javascript (14.15)| [0http](https://github.com/jkyberneees/0http) (3.0) | 87 411.20 | 99 564.31 | 101 526.62 |
| 55 | javascript (14.15)| [restana](https://github.com/jkyberneees/ana) (4.7) | 86 289.18 | 95 689.78 | 97 643.02 |
| 56 | java (11)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 84 126.86 | 89 910.36 | 90 950.91 |
| 57 | c (11)| [kore](https://kore.io) (3.3) | 83 754.60 | 122 560.50 | 194 376.93 |
| 58 | go (1.15)| [gf](https://goframe.org) (1.14) | 83 674.23 | 90 605.36 | 92 837.66 |
| 59 | javascript (14.15)| [fastify](https://fastify.io) (3.7) | 83 346.20 | 90 232.63 | 88 447.66 |
| 60 | javascript (14.15)| [rayo](https://rayo.js.org) (1.3) | 82 879.38 | 91 347.60 | 88 573.64 |
| 61 | javascript (14.15)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 81 555.77 | 88 746.84 | 86 757.80 |
| 62 | elixir (1.11)| [cowboy_stream](https://ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 80 735.32 | 85 847.04 | 83 336.77 |
| 63 | java (11)| [javalin](https://javalin.io) (3.9) | 80 126.49 | 85 515.20 | 86 061.03 |
| 64 | kotlin (1.4)| [http4k](https://http4k.org) (3.275) | 79 573.51 | 91 920.75 | 96 300.89 |
| 65 | scala (2.13)| [akkahttp](https://akka.io) (10.1) | 78 440.67 | 91 892.80 | 87 217.63 |
| 66 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 77 944.34 | 85 778.26 | 96 025.97 |
| 67 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 77 914.16 | 94 213.21 | 107 047.35 |
| 68 | kotlin (1.4)| [ktor](https://ktor.io) (1.4) | 77 889.62 | 100 100.27 | 103 042.45 |
| 69 | php (7.4)| [hyperf](https://hyperf.io) (2.0) | 76 698.42 | 95 535.74 | 105 799.92 |
| 70 | fsharp (5.0)| [websharper](https://websharper.com) (4.6) | 75 786.58 | 85 528.34 | 86 243.47 |
| 71 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 75 083.74 | 82 197.74 | 83 072.04 |
| 72 | java (11)| [restheart](https://restheart.org) (5.1) | 72 574.30 | 91 691.92 | 97 407.88 |
| 73 | javascript (14.15)| [nestjs-fastify](https://nestjs.com) (7.4) | 72 345.14 | 78 235.24 | 76 033.21 |
| 74 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 70 398.60 | 69 411.56 | 74 449.81 |
| 75 | javascript (14.15)| [foxify](https://foxify.js.org) (0.1) | 68 816.30 | 73 182.40 | 73 071.95 |
| 76 | javascript (14.15)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 67 976.96 | 73 004.07 | 71 055.53 |
| 77 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 67 682.45 | 68 810.05 | 69 028.28 |
| 78 | java (11)| [micronaut](https://micronaut.io) (1.2) | 66 801.97 | 75 962.01 | 75 408.55 |
| 79 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.8) | 66 406.04 | 71 328.85 | 72 575.45 |
| 80 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 66 383.68 | 73 135.90 | 76 998.33 |
| 81 | javascript (14.15)| [koa](https://koajs.com) (2.13) | 62 159.98 | 65 797.46 | 64 620.83 |
| 82 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 61 702.06 | 66 538.00 | 68 667.37 |
| 83 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 60 550.85 | 65 119.48 | 66 723.50 |
| 84 | swift (5.3)| [kitura-nio](https://kitura.io) (2.9) | 54 870.99 | 54 816.89 | 54 294.23 |
| 85 | javascript (14.15)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 53 990.65 | 58 513.32 | 56 736.15 |
| 86 | elixir (1.11)| [cowboy](https://ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 53 945.71 | 55 618.82 | 56 762.39 |
| 87 | swift (5.3)| [vapor](https://vapor.codes) (4.35) | 53 575.74 | 56 111.60 | 55 870.15 |
| 88 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 53 530.80 | 73 546.07 | 77 970.07 |
| 89 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 53 021.40 | 58 204.19 | 58 255.48 |
| 90 | swift (5.3)| [kitura](https://kitura.io) (2.9) | 53 003.73 | 53 310.76 | 52 397.57 |
| 91 | javascript (14.15)| [moleculer](https://moleculer.services) (0.14) | 52 783.87 | 54 037.17 | 52 574.38 |
| 92 | fsharp (5.0)| [suave](https://suave.io) (2.5) | 52 377.74 | 47 193.98 | 40 599.07 |
| 93 | javascript (14.15)| [hapi](https://hapijs.com) (20.0) | 52 149.03 | 52 786.06 | 51 659.39 |
| 94 | python (3.8)| [hug](https://hug.rest) (2.6) | 49 445.29 | 50 707.57 | 50 993.40 |
| 95 | scala (2.13)| [http4s](https://http4s.org) (0.21) | 48 851.19 | 55 443.81 | 57 010.80 |
| 96 | rust (1.48)| [nickel](https://nickel-org.github.io) (0.11) | 48 596.03 | 51 956.58 | 51 860.43 |
| 97 | javascript (14.15)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 48 208.10 | 50 772.79 | 50 186.50 |
| 98 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 47 792.42 | 51 279.79 | 53 234.34 |
| 99 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 45 753.06 | 50 088.91 | 52 075.10 |
| 100 | rust (1.48)| [gotham](https://gotham.rs) (0.4) | 45 697.00 | 51 183.06 | 52 017.71 |
| 101 | php (7.4)| [imi](https://imiphp.com) (1.2) | 45 677.18 | 51 757.02 | 51 467.93 |
| 102 | javascript (14.15)| [restify](https://restify.com) (8.5) | 44 810.14 | 46 080.91 | 44 258.08 |
| 103 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.9) | 42 970.60 | 49 377.78 | 50 850.88 |
| 104 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 42 948.79 | 47 341.54 | 47 465.92 |
| 105 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 42 521.48 | 31 935.17 | 31 073.97 |
| 106 | javascript (14.15)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 42 367.62 | 45 397.32 | 44 994.19 |
| 107 | cpp (11)| [evhtp](https://criticalstack.com) (1.2) | 41 672.08 | 43 625.20 | 43 873.66 |
| 108 | python (3.8)| [starlette](https://starlette.io) (0.14) | 40 881.70 | 46 891.35 | 47 908.82 |
| 109 | scala (2.13)| [play](https://playframework.com) (2.8) | 40 548.52 | 43 008.64 | 43 073.31 |
| 110 | elixir (1.11)| [plug](https://hexdocs.pm/plug) (1.10) | 40 418.46 | 43 900.42 | 42 893.11 |
| 111 | elixir (1.11)| [phoenix](https://phoenixframework.org) (1.5) | 36 318.10 | 39 663.46 | 38 603.29 |
| 112 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 36 210.28 | 37 302.43 | 34 634.36 |
| 113 | python (3.8)| [emmett](https://emmett.sh) (2.1) | 35 688.52 | 40 030.21 | 41 434.76 |
| 114 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 35 132.03 | 32 447.48 | 30 677.59 |
| 115 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 32 858.60 | 32 368.12 | 32 161.62 |
| 116 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 32 733.05 | 32 056.17 | 31 163.45 |
| 117 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 32 469.23 | 31 677.33 | 31 521.91 |
| 118 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.16) | 32 432.63 | 31 020.25 | 29 570.53 |
| 119 | python (3.8)| [index.py](https://index-py.abersheeran.com) (0.12) | 32 175.83 | 36 826.25 | 37 941.00 |
| 120 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 31 019.31 | 30 813.10 | 29 714.92 |
| 121 | javascript (14.15)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 28 383.32 | 25 842.28 | 24 899.15 |
| 122 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 27 808.50 | 33 859.93 | 35 700.49 |
| 123 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 27 761.69 | 31 799.29 | 35 201.23 |
| 124 | rust (1.48)| [iron](https://ironframework.io) (0.6) | 26 724.41 | 24 936.71 | 25 031.66 |
| 125 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 26 526.42 | 23 656.66 | 21 942.42 |
| 126 | python (3.8)| [responder](https://python-responder.org) (2.0) | 25 622.45 | 31 773.50 | 32 582.40 |
| 127 | php (7.4)| [swoft](https://swoft.org) (2.0) | 25 603.72 | 30 509.18 | 31 874.28 |
| 128 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 24 782.08 | 31 388.71 | 31 450.11 |
| 129 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.61) | 24 459.69 | 31 723.42 | 30 904.47 |
| 130 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 24 163.80 | 28 268.80 | 28 743.24 |
| 131 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 23 907.30 | 21 359.89 | 19 590.10 |
| 132 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 23 504.18 | 26 647.29 | 27 435.05 |
| 133 | go (1.15)| [macaron](https://go-macaron.com) (1.4) | 23 338.00 | 25 291.12 | 25 209.80 |
| 134 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 22 975.82 | 29 915.50 | 30 295.90 |
| 135 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 22 968.95 | 28 189.58 | 28 179.04 |
| 136 | php (7.4)| [slim-swoole](https://slimframework.com) (4.5) | 22 931.13 | 27 092.12 | 26 912.93 |
| 137 | javascript (14.15)| [express](https://expressjs.com) (4.17) | 21 620.51 | 28 299.19 | 26 588.01 |
| 138 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 21 592.19 | 25 983.94 | 25 848.66 |
| 139 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 21 131.68 | 19 947.60 | 20 368.69 |
| 140 | javascript (14.15)| [feathersjs](https://feathersjs.com) (4.5) | 20 871.07 | 26 240.06 | 26 639.53 |
| 141 | java (11)| [blade](https://lets-blade.com) (2.0) | 20 550.92 | 25 376.64 | 24 383.72 |
| 142 | javascript (14.15)| [nestjs-express](https://nestjs.com) (7.4) | 17 388.56 | 17 354.89 | 20 389.58 |
| 143 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 17 186.93 | 17 492.94 | 17 655.71 |
| 144 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 16 996.02 | 16 917.91 | 16 092.42 |
| 145 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.4) | 15 145.37 | 15 456.09 | 15 827.59 |
| 146 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 15 140.88 | 15 484.12 | 15 644.96 |
| 147 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 14 937.75 | 15 165.69 | 15 125.72 |
| 148 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.5) | 14 629.80 | 14 920.08 | 15 166.12 |
| 149 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.6) | 14 317.42 | 14 666.68 | 14 740.58 |
| 150 | java (11)| [struts2](https://struts.apache.org) (2.5) | 14 117.34 | 14 215.04 | 14 157.34 |
| 151 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 12 479.77 | 19 800.21 | 18 641.34 |
| 152 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.13) | 11 049.86 | 10 873.59 | 10 244.12 |
| 153 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 9 875.47 | 9 750.16 | 9 592.87 |
| 154 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 9 657.23 | 9 491.08 | 9 350.31 |
| 155 | python (3.8)| [tornado](https://tornadoweb.org) (6.1) | 9 623.79 | 9 755.25 | 9 480.50 |
| 156 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 415.50 | 9 368.21 | 10 036.58 |
| 157 | python (3.8)| [django](https://djangoproject.com) (3.1) | 8 677.94 | 8 578.06 | 8 574.84 |
| 158 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 643.37 | 7 211.20 | 6 858.88 |
| 159 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 7 084.48 | 6 987.04 | 6 890.24 |
| 160 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 7 081.44 | 8 657.73 | 9 902.33 |
| 161 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 885.87 | 6 801.22 | 6 726.45 |
| 162 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 6 749.30 | 6 662.44 | 6 598.43 |
| 163 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 6 708.74 | 6 647.31 | 6 566.34 |
| 164 | javascript (14.15)| [sails](https://sailsjs.com) (1.4) | 6 360.63 | 6 393.25 | 6 348.38 |
| 165 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.2) | 6 018.96 | 5 941.00 | 5 873.67 |
| 166 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.4) | 5 759.22 | 5 679.06 | 5 638.34 |
| 167 | php (7.4)| [ice](https://iceframework.org) (1.5) | 5 721.73 | 5 661.05 | 5 617.93 |
| 168 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 5 350.17 | 5 277.66 | 5 274.32 |
| 169 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 5 188.75 | 10 401.02 | 7 719.56 |
| 170 | php (7.4)| [slim](https://slimframework.com) (4.6) | 4 449.05 | 4 429.14 | 4 425.36 |
| 171 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 4 160.55 | 4 114.57 | 4 131.09 |
| 172 | php (7.4)| [lumen](https://lumen.laravel.com) (8.1) | 3 950.11 | 3 951.21 | 3 949.28 |
| 173 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 3 946.81 | 3 951.36 | 3 952.20 |
| 174 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.4) | 3 522.84 | 3 524.55 | 3 554.46 |
| 175 | php (7.4)| [symfony](https://symfony.com) (5.1) | 3 136.20 | 3 142.17 | 3 163.47 |
| 176 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 3 094.46 | 3 125.93 | 3 090.48 |
| 177 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 2 843.89 | 2 758.20 | 2 831.43 |
| 178 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 826.21 | 2 823.58 | 2 824.25 |
| 179 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 218.18 | 2 214.73 | 2 182.81 |
| 180 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 951.98 | 1 872.72 | 1 797.53 |
| 181 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 659.43 | 1 665.95 | 1 648.77 |
| 182 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 579.88 | 1 534.54 | 1 511.46 |
| 183 | python (3.8)| [django-ninja](https://django-ninja.rest-framework.com) (0.9) | 1 572.16 | 2 438.88 | 2 476.93 |
| 184 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 499.77 | 1 516.51 | 1 510.04 |
| 185 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 1 487.08 | 1 187.27 | 2 525.63 |
| 186 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 117.39 | 1 130.98 | 1 121.69 |
| 187 | perl (5.32)| [dancer2](https://perldancer.org) (0.3) | 1 109.99 | 978.89 | 1 747.10 |
| 188 | crystal (0.35)| [lucky](https://luckyframework.org) (0.23) | 655.47 | 664.95 | 634.40 |
| 189 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 438.38 | 456.71 | 43.23 |
| 190 | php (7.4)| [laravel](https://laravel.com) (7.27) | 285.44 | 77.74 | 0.07 |
| 191 | nim (1.4)| [basolato](https://github.com/itsumura-h/nim-basolato) (0.7) | 0.00 | 0.00 | 0.00 |
| 192 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 0.00 | 0.00 | 0.00 |
| 193 | java (11)| [spring-framework](https://spring.io/projects/spring-framework) (5.3) | 0.00 | 0.00 | 0.00 |

## How to contribute ?

In any way you want ...

+ Request a framework addition
+ Report a bug (on any implementation)
+ Suggest an idea
+ ...

Any kind of idea is :heart:

## Contributors

- [Taichiro Suzuki](https://github.com/tbrand) - Author | Maintainer
- [OvermindDL1](https://github.com/OvermindDL1) - Maintainer
- [Marwan Rabbâa](https://github.com/waghanza) - Maintainer

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

+ [Crystal](https://crystal-lang.org) as `built-in` tools are made in this language
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

+ Install all dependencies

~~~sh
shards install
~~~

+ Build internal tools

~~~sh
shards build
~~~

+ Create and initialize the database

~~~sh
createdb -U postgres benchmark
psql -U postgres -d benchmark < dump.sql
~~~

Docker can be used to set up the database:

~~~sh
docker run -it --rm -d \
-p 5432:5432 \
-e POSTGRES_DB=benchmark \
-e POSTGRES_HOST_AUTH_METHOD=trust \
-v /tmp/pg-data:/var/lib/postgresql/data \
--name pg postgres:12-alpine
~~~

Wait several seconds for the container to start, then inject the dump:

~~~sh
docker exec pg sh -c "echo \"$(cat dump.sql)\" | psql -U postgres -d benchmark"
~~~

After creating the database, export its URL:

~~~sh
export DATABASE_URL="postgresql://postgres@localhost/benchmark"
~~~

+ Make configuration

~~~sh
bin/make config
~~~

+ Build containers

> jobs are either languages (example : crystal) or frameworks (example : router.cr)

~~~sh
bin/neph [job1] [job2] [job3] ...
~~~

+ Export all results readme

~~~sh
bin/db to_readme
~~~

## Results

:information_source:  Updated on **2020-11-17** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | go (1.15)| [fiber](gofiber.io) (2.2) | 185 103.19 | 196 413.36 | 195 245.89 |
| 2 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 184 527.48 | 204 273.31 | 206 849.71 |
| 3 | go (1.15)| [gearbox](gogearbox.com) (1.1) | 181 041.20 | 184 724.79 | 184 629.37 |
| 4 | go (1.15)| [router](pkg.go.dev/github.com/fasthttp/router) (1.3) | 179 515.14 | 192 131.84 | 191 505.30 |
| 5 | go (1.15)| [gorouter-fasthttp](github.com/vardius/gorouter/wiki) (4.4) | 179 176.54 | 190 858.77 | 189 798.80 |
| 6 | go (1.15)| [atreugo](github.com/savsgio/atreugo/blob/master/docs/README.md) (11.5) | 178 760.80 | 193 054.80 | 192 355.82 |
| 7 | go (1.15)| [fasthttp](pkg.go.dev/github.com/valyala/fasthttp) (1.17) | 177 244.69 | 197 390.65 | 197 823.11 |
| 8 | nim (1.4)| [httpbeast](https://github.com/dom96/httpbeast) (0.2) | 170 543.20 | 211 769.23 | 216 054.68 |
| 9 | javascript (12.18)| [sifrr](sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (0.0) | 160 374.34 | 201 373.20 | 205 046.47 |
| 10 | nim (1.4)| [whip](https://github.com/mattaylor/whip) (0.2) | 159 521.37 | 197 313.03 | 202 033.21 |
| 11 | java (11)| [rapidoid](rapidoid.org) (5.5) | 157 917.00 | 194 460.16 | 199 374.23 |
| 12 | java (11)| [jooby](jooby.io) (2.8) | 156 094.69 | 199 416.53 | 206 906.16 |
| 13 | kotlin (1.4)| [kooby](jooby.io) (2.8) | 154 763.00 | 204 214.74 | 211 406.79 |
| 14 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 150 536.36 | 184 342.02 | 186 352.92 |
| 15 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 149 586.26 | 183 496.39 | 186 520.71 |
| 16 | java (11)| [light-4j](doc.networknt.com) (2.0) | 149 192.52 | 196 636.31 | 203 579.07 |
| 17 | crystal (0.35)| [router.cr](https://github.com/tbrand/router.cr) (0.2) | 147 935.89 | 183 726.33 | 184 111.74 |
| 18 | crystal (0.35)| [spider-gazelle](spider-gazelle.net) (3.3) | 146 670.84 | 179 027.70 | 180 512.24 |
| 19 | crystal (0.35)| [kemal](kemalcr.com) (0.26) | 143 395.54 | 170 795.76 | 169 912.17 |
| 20 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 142 573.44 | 173 989.55 | 173 475.32 |
| 21 | crystal (0.35)| [amber](amberframework.org) (0.35) | 135 602.39 | 160 834.61 | 159 979.48 |
| 22 | nim (1.4)| [jester](https://github.com/dom96/jester) (0.5) | 135 016.80 | 159 063.26 | 164 177.76 |
| 23 | crystal (0.35)| [athena](https://github.com/athena-framework/athena) (0.11) | 124 416.04 | 141 849.70 | 134 981.77 |
| 24 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 121 723.82 | 123 060.83 | 126 310.78 |
| 25 | go (1.15)| [clevergo](clevergo.tech) (0.3) | 121 173.43 | 122 736.86 | 125 634.07 |
| 26 | go (1.15)| [echo](echo.labstack.com) (4.1) | 120 847.87 | 122 506.91 | 125 656.81 |
| 27 | javascript (12.18)| [naturaljs-router](https://github.com/jesusvilla/natural) (0.1.1) | 120 464.68 | 144 322.71 | 145 433.11 |
| 28 | go (1.15)| [httprouter](pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 120 084.73 | 121 059.95 | 124 217.70 |
| 29 | go (1.15)| [gin](gin-gonic.com) (1.6) | 117 547.24 | 123 686.95 | 125 945.40 |
| 30 | go (1.15)| [chi](https://github.com/go-chi/chi) (4.1) | 116 178.29 | 115 038.65 | 119 228.87 |
| 31 | java (11)| [act](actframework.org) (1.9) | 115 463.59 | 150 637.86 | 152 527.96 |
| 32 | go (1.15)| [gorouter](github.com/vardius/gorouter/wiki) (4.5) | 114 252.74 | 120 568.08 | 124 122.88 |
| 33 | go (1.15)| [violetear](violetear.org) (7.0) | 113 112.26 | 112 064.56 | 115 518.33 |
| 34 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 112 998.12 | 111 870.79 | 115 546.06 |
| 35 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 112 993.83 | 171 229.56 | 191 313.90 |
| 36 | c (11)| [kore](kore.io) (3.3) | 112 436.90 | 174 449.13 | 175 668.74 |
| 37 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 112 238.20 | 110 884.88 | 114 522.70 |
| 38 | fsharp (5.0)| [falco](https://github.com/pimbrouwers/Falco) (2.1) | 111 671.90 | 130 294.83 | 134 389.32 |
| 39 | go (1.15)| [goroute](goroute.github.io) (0.0) | 111 254.19 | 109 101.50 | 113 146.38 |
| 40 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.1) | 110 908.73 | 129 821.03 | 133 959.57 |
| 41 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 110 705.99 | 115 680.64 | 116 393.68 |
| 42 | go (1.15)| [gorilla-mux](www.gorillatoolkit.org/pkg/mux) (1.8) | 108 490.63 | 104 785.78 | 109 285.82 |
| 43 | csharp (8.0)| [aspnetcore](docs.microsoft.com/en-us/aspnet/index) (5.0) | 106 680.09 | 123 624.85 | 127 294.16 |
| 44 | go (1.15)| [beego](beego.me) (1.12) | 106 391.88 | 110 470.21 | 113 633.68 |
| 45 | go (1.15)| [air](https://github.com/aofei/air) (0.20) | 95 049.08 | 94 009.85 | 97 683.55 |
| 46 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 95 038.10 | 104 909.67 | 104 827.34 |
| 47 | php (7.4)| [nano]() (0.0.9) | 93 647.63 | 140 160.71 | 152 394.64 |
| 48 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.4) | 91 251.44 | 91 612.14 | 96 069.01 |
| 49 | javascript (12.18)| [polkadot](https://github.com/lukeed/polkadot) (1.0) | 88 518.35 | 99 452.05 | 100 252.17 |
| 50 | fsharp (5.0)| [saturn](saturnframework.org) (0.14) | 86 962.26 | 92 484.44 | 90 208.12 |
| 51 | java (11)| [spring-framework](spring.io/projects/spring-framework) (5.3) | 85 941.42 | 91 924.74 | 93 447.19 |
| 52 | cpp (11)| [drogon](https://github.com/an-tao/drogon) (1.1) | 85 138.05 | 86 768.96 | 87 645.27 |
| 53 | elixir (1.1)| [cowboy_stream](ninenines.eu/docs/en/cowboy/2.8/guide/streams/) (2.8) | 84 990.64 | 90 017.03 | 87 861.58 |
| 54 | ruby (2.7)| [agoo](https://github.com/ohler55/agoo) (2.14) | 83 710.76 | 112 972.76 | 125 592.04 |
| 55 | go (1.15)| [gf](goframe.org) (1.14) | 83 397.63 | 90 154.07 | 92 134.05 |
| 56 | javascript (12.18)| [0http](https://github.com/jkyberneees/0http) (3.0) | 83 138.64 | 95 641.62 | 95 840.91 |
| 57 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 80 749.84 | 91 223.65 | 104 318.84 |
| 58 | javascript (12.18)| [fastify](fastify.io) (3.7) | 79 656.40 | 87 338.85 | 85 002.42 |
| 59 | scala (2.13)| [akkahttp](akka.io) (10.1) | 79 367.83 | 94 743.13 | 92 302.29 |
| 60 | javascript (12.18)| [restana](https://github.com/jkyberneees/ana) (4.7) | 78 837.80 | 88 852.26 | 88 664.93 |
| 61 | swift (5.3)| [perfect](perfect.org) (3.1) | 78 728.51 | 86 895.28 | 97 110.53 |
| 62 | java (11)| [javalin](javalin.io) (3.9) | 78 671.10 | 85 689.81 | 86 618.10 |
| 63 | javascript (12.18)| [polka](https://github.com/lukeed/polka) (0.5) | 77 705.07 | 85 188.35 | 83 851.72 |
| 64 | php (7.4)| [hyperf](hyperf.io) (2.0) | 76 947.65 | 90 438.56 | 100 286.77 |
| 65 | kotlin (1.4)| [http4k](http4k.org) (3.275) | 75 846.71 | 90 011.18 | 93 016.55 |
| 66 | javascript (12.18)| [rayo](rayo.js.org) (1.3) | 75 737.55 | 83 255.68 | 81 702.76 |
| 67 | c (11)| [agoo-c](https://github.com/ohler55/agoo-c) (0.7) | 74 614.25 | 186 342.05 | 203 182.89 |
| 68 | python (3.8)| [falcon](falconframework.org) (2.0) | 74 473.87 | 82 152.60 | 83 676.93 |
| 69 | fsharp (5.0)| [websharper](websharper.com) (4.6) | 74 389.59 | 83 300.62 | 82 856.24 |
| 70 | java (11)| [spring-boot](spring.io/projects/spring-boot) (2.3) | 74 152.88 | 82 421.44 | 82 949.17 |
| 71 | kotlin (1.4)| [ktor](ktor.io) (1.4) | 72 163.19 | 94 665.18 | 97 385.70 |
| 72 | rust (1.47)| [actix](actix.rs) (3.2) | 71 930.58 | 72 682.76 | 75 204.54 |
| 73 | java (11)| [restheart](restheart.org) (5.1) | 70 384.51 | 89 370.91 | 95 056.39 |
| 74 | javascript (12.18)| [muneem](https://github.com/node-muneem/muneem) (2.4) | 68 927.65 | 75 758.00 | 74 132.28 |
| 75 | javascript (12.18)| [nestjs-fastify](nestjs.com) (7.4) | 67 969.87 | 74 127.66 | 72 509.62 |
| 76 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 67 961.63 | 69 150.33 | 73 463.43 |
| 77 | php (7.4)| [ubiquity](ubiquity.kobject.net) (2.3) | 67 173.10 | 70 165.62 | 67 519.35 |
| 78 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.8) | 66 046.87 | 72 789.40 | 73 790.66 |
| 79 | javascript (12.18)| [foxify](foxify.js.org) (0.1) | 64 389.65 | 70 116.75 | 68 772.56 |
| 80 | haskell (8.8)| [scotty](hackage.haskell.org/package/scotty) (0.12) | 64 024.05 | 69 035.60 | 75 195.52 |
| 81 | rust (1.47)| [nickel](nickel-org.github.io) (0.11) | 61 641.35 | 63 143.40 | 64 163.03 |
| 82 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 61 198.66 | 65 690.14 | 67 004.97 |
| 83 | java (11)| [micronaut](micronaut.io) (1.2) | 61 096.61 | 74 130.90 | 75 045.30 |
| 84 | python (3.8)| [bottle](bottlepy.org) (0.12) | 59 823.02 | 66 003.37 | 66 465.96 |
| 85 | javascript (12.18)| [koa](koajs.com) (2.13) | 58 872.41 | 64 096.26 | 63 010.44 |
| 86 | clojure (1.1)| [coast](coastonclojure.com) (1.0) | 58 627.13 | 60 511.79 | 60 238.70 |
| 87 | javascript (12.18)| [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (0.0) | 57 209.62 | 61 186.59 | 60 428.64 |
| 88 | elixir (1.1)| [cowboy](ninenines.eu/docs/en/cowboy/2.8/guide/) (2.8) | 53 837.19 | 55 702.42 | 56 245.61 |
| 89 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.27) | 53 742.32 | 61 183.72 | 62 339.49 |
| 90 | swift (5.3)| [kitura-nio](kitura.io) (2.9) | 53 547.64 | 53 130.93 | 53 402.20 |
| 91 | swift (5.3)| [kitura](kitura.io) (2.9) | 52 637.28 | 52 850.60 | 52 822.42 |
| 92 | php (7.4)| [siler-swoole](siler.leocavalcante.dev) (1.7) | 51 659.92 | 65 115.73 | 69 046.89 |
| 93 | python (3.8)| [pyramid](trypyramid.com) (1.1) | 51 188.17 | 54 312.63 | 55 159.62 |
| 94 | javascript (12.18)| [fyrejet-api](https://github.com/fyrejet/fyrejet) (2.1) | 50 808.66 | 54 608.87 | 53 971.44 |
| 95 | javascript (12.18)| [moleculer](moleculer.services) (0.14) | 50 426.30 | 53 183.43 | 51 941.78 |
| 96 | swift (5.3)| [vapor](vapor.codes) (4.35) | 49 740.82 | 52 555.03 | 52 563.49 |
| 97 | scala (2.13)| [http4s](http4s.org) (0.21) | 48 087.23 | 54 732.02 | 54 717.49 |
| 98 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 47 165.53 | 52 704.03 | 53 402.13 |
| 99 | javascript (12.18)| [fyrejet-uwebsockets](https://github.com/fyrejet/fyrejet) (2.1) | 47 042.44 | 49 958.33 | 49 739.64 |
| 100 | python (3.8)| [asgineer](asgineer.readthedocs.io) (0.8) | 46 853.84 | 53 591.72 | 55 342.51 |
| 101 | python (3.8)| [hug](hug.rest) (2.6) | 46 534.70 | 51 520.32 | 51 581.55 |
| 102 | fsharp (5.0)| [suave](suave.io) (2.5) | 45 389.55 | 43 642.99 | 38 463.21 |
| 103 | php (7.4)| [imi](imiphp.com) (1.2) | 44 861.52 | 50 755.54 | 51 772.83 |
| 104 | cpp (11)| [evhtp](criticalstack.com) (1.2) | 44 026.18 | 45 607.88 | 45 881.38 |
| 105 | python (3.8)| [starlette](starlette.io) (0.13) | 43 153.05 | 48 035.00 | 48 844.40 |
| 106 | javascript (12.18)| [hapi](hapijs.com) (20.0) | 42 133.81 | 45 360.24 | 44 400.69 |
| 107 | elixir (1.1)| [plug](hexdocs.pm/plug) (1.10) | 41 689.45 | 44 273.40 | 43 349.21 |
| 108 | javascript (12.18)| [restify](restify.com) (8.5) | 41 256.68 | 45 313.40 | 44 003.01 |
| 109 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.9) | 40 910.05 | 47 790.25 | 49 654.98 |
| 110 | csharp (8.0)| [simplify.web](web.simplifynet.dev) (4.2) | 40 805.15 | 44 819.00 | 43 990.35 |
| 111 | rust (1.47)| [gotham](gotham.rs) (0.4) | 40 004.13 | 44 681.30 | 45 951.93 |
| 112 | scala (2.13)| [play](playframework.com) (2.8) | 39 246.05 | 42 005.89 | 42 251.07 |
| 113 | javascript (12.18)| [fyrejet](https://github.com/fyrejet/fyrejet) (2.1) | 38 418.69 | 42 476.12 | 41 114.77 |
| 114 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 37 295.29 | 36 768.76 | 34 313.57 |
| 115 | clojure (1.1)| [luminus](luminusweb.com) (1.0) | 36 675.45 | 38 604.55 | 38 433.63 |
| 116 | elixir (1.1)| [phoenix](phoenixframework.org) (1.5) | 35 185.83 | 38 507.59 | 38 050.56 |
| 117 | python (3.8)| [emmett](github.com/emmett-framework/emmett) (2.0) | 34 694.78 | 40 530.19 | 41 079.10 |
| 118 | haskell (8.8)| [servant](servant.dev) (0.17) | 33 461.15 | 32 627.84 | 30 971.21 |
| 119 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 33 044.95 | 32 869.99 | 32 107.07 |
| 120 | nim (1.4)| [mike](https://github.com/ire4ever1190/mike) (0.5) | 32 061.33 | 31 504.85 | 32 000.99 |
| 121 | crystal (0.35)| [shivneri](https://github.com/ujjwalguptaofficial/shivneri) (0.16) | 31 858.78 | 31 771.41 | 28 962.92 |
| 122 | r (4.0)| [rserve](rforge.net/Rserve/) (1.7) | 31 594.93 | 27 940.16 | 26 920.39 |
| 123 | dart (2.10)| [aqueduct](aqueduct.io) (3.3) | 29 194.55 | 29 062.64 | 28 895.72 |
| 124 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 28 650.08 | 34 576.62 | 35 316.58 |
| 125 | javascript (12.18)| [turbo_polka](https://github.com/mafintosh/turbo-http) (0.3) | 28 525.26 | 27 514.51 | 26 687.20 |
| 126 | python (3.8)| [aiohttp](aiohttp.readthedocs.io) (3.6) | 27 742.01 | 31 480.06 | 33 287.57 |
| 127 | python (3.8)| [index.py](index-py.abersheeran.com) (0.12) | 27 672.79 | 35 797.82 | 36 276.89 |
| 128 | php (7.4)| [swoft](swoft.org) (2.0) | 25 979.01 | 31 070.42 | 31 913.64 |
| 129 | python (3.8)| [responder](python-responder.org) (2.0) | 25 567.94 | 31 423.05 | 32 172.10 |
| 130 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 24 727.26 | 30 144.22 | 31 029.73 |
| 131 | ruby (2.7)| [syro](https://github.com/soveran/syro) (3.2) | 24 648.71 | 24 952.30 | 23 476.36 |
| 132 | php (7.4)| [yii-swoole](yiiframework.com) (2.0) | 24 541.73 | 29 964.27 | 29 998.51 |
| 133 | ruby (2.7)| [hanami-api](hanamirb.org) (0.1) | 24 472.75 | 23 764.89 | 23 380.23 |
| 134 | python (3.8)| [fastapi](fastapi.tiangolo.com) (0.61) | 24 345.61 | 30 324.25 | 30 617.07 |
| 135 | python (3.8)| [molten](moltenframework.com) (1.0) | 24 149.49 | 28 006.70 | 26 429.13 |
| 136 | nim (1.4)| [rosencrantz](https://github.com/andreaferretti/rosencrantz) (0.4) | 24 060.31 | 21 446.60 | 19 783.12 |
| 137 | rust (1.47)| [iron](ironframework.io) (0.6) | 23 925.96 | 23 207.73 | 23 464.27 |
| 138 | nim (1.4)| [akane](https://github.com/Ethosa/akane) (0.1) | 23 811.36 | 21 507.88 | 19 644.34 |
| 139 | ruby (2.7)| [roda](roda.jeremyevans.net) (3.36) | 23 794.28 | 23 234.04 | 22 621.03 |
| 140 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 775.10 | 27 197.91 | 25 162.26 |
| 141 | go (1.15)| [macaron](go-macaron.com) (1.4) | 23 038.62 | 25 456.11 | 25 430.01 |
| 142 | ruby (2.7)| [cuba](cuba.is) (3.9) | 22 243.76 | 21 679.11 | 21 278.54 |
| 143 | php (7.4)| [slim-swoole](slimframework.com) (4.5) | 22 002.29 | 25 770.20 | 26 416.55 |
| 144 | javascript (12.18)| [feathersjs](feathersjs.com) (4.5) | 21 986.49 | 25 988.59 | 25 799.45 |
| 145 | javascript (12.18)| [express](expressjs.com) (4.17) | 21 812.87 | 25 711.98 | 25 692.49 |
| 146 | python (3.8)| [masonite](masoniteproject.com) (2.3) | 21 204.07 | 23 874.13 | 24 469.21 |
| 147 | python (3.8)| [flask](flask.pocoo.org) (1.1) | 20 583.58 | 24 406.51 | 24 950.21 |
| 148 | ruby (2.7)| [rack-routing](https://github.com/georgeu2000/rack-routing) (0.0) | 20 491.46 | 19 947.10 | 19 615.10 |
| 149 | ruby (2.7)| [rack_app](rack-app.com) (7.6) | 20 334.23 | 19 704.80 | 19 376.59 |
| 150 | ruby (2.7)| [camping](https://github.com/camping/camping) (2.1) | 18 753.36 | 18 124.12 | 17 961.51 |
| 151 | java (11)| [blade](lets-blade.com) (2.0) | 17 848.06 | 22 214.25 | 20 653.22 |
| 152 | javascript (12.18)| [nestjs-express](nestjs.com) (7.4) | 17 320.30 | 18 203.08 | 17 921.31 |
| 153 | go (1.15)| [tango](gitea.com/lunny/tango) (0.6) | 17 051.42 | 17 405.68 | 17 472.14 |
| 154 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 15 575.82 | 15 754.36 | 16 123.44 |
| 155 | php (7.4)| [slim-roadrunner](slimframework.com) (4.5) | 15 027.84 | 15 318.68 | 15 445.42 |
| 156 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 14 950.63 | 14 762.22 | 14 374.32 |
| 157 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 14 734.91 | 15 077.69 | 15 028.06 |
| 158 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.6) | 14 504.72 | 14 585.32 | 14 845.99 |
| 159 | ruby (2.7)| [sinatra](sinatrarb.com) (2.1) | 13 324.71 | 12 989.32 | 12 942.35 |
| 160 | java (11)| [struts2](struts.apache.org) (2.5) | 12 638.62 | 13 324.58 | 13 420.53 |
| 161 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 12 607.38 | 12 743.47 | 12 722.43 |
| 162 | ruby (2.7)| [grape](ruby-grape.org) (1.5) | 12 272.86 | 11 885.04 | 11 790.29 |
| 163 | python (3.8)| [quart](pgjones.gitlab.io/quart) (0.13) | 11 335.34 | 11 340.16 | 10 649.10 |
| 164 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 10 979.66 | 19 635.98 | 18 549.90 |
| 165 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.4) | 10 844.21 | 10 494.76 | 10 217.31 |
| 166 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 9 688.05 | 9 562.62 | 9 327.08 |
| 167 | python (3.8)| [tornado](tornadoweb.org) (6.1) | 9 654.96 | 9 998.10 | 9 653.50 |
| 168 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 9 355.24 | 9 254.74 | 8 970.45 |
| 169 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 8 927.93 | 8 919.47 | 8 977.63 |
| 170 | python (3.8)| [django](djangoproject.com) (3.1) | 8 379.79 | 8 178.25 | 8 071.78 |
| 171 | ruby (2.7)| [flame](https://github.com/AlexWayfer/flame) (4.18) | 8 037.53 | 7 925.44 | 7 945.92 |
| 172 | ruby (2.7)| [hanami](hanamirb.org) (1.3) | 7 700.31 | 7 599.95 | 7 642.46 |
| 173 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 527.83 | 7 117.66 | 6 792.77 |
| 174 | clojure (1.1)| [yada](juxt.pro/yada/) (1.2) | 7 402.20 | 8 636.41 | 9 014.01 |
| 175 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 6 947.06 | 6 860.06 | 6 752.42 |
| 176 | php (7.4)| [phalcon](phalcon.io) (4.1) | 6 730.12 | 6 657.90 | 6 566.48 |
| 177 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 6 680.69 | 6 593.28 | 6 519.18 |
| 178 | php (7.4)| [siler](siler.leocavalcante.dev) (1.7) | 6 526.14 | 6 434.93 | 6 378.77 |
| 179 | javascript (12.18)| [sails](sailsjs.com) (1.3) | 5 958.28 | 6 031.98 | 6 013.23 |
| 180 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.2) | 5 934.42 | 5 823.70 | 5 810.76 |
| 181 | php (7.4)| [ice](iceframework.org) (1.5) | 5 717.06 | 5 655.62 | 5 623.59 |
| 182 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.4) | 5 672.90 | 5 582.70 | 5 580.10 |
| 183 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 5 305.74 | 5 253.32 | 5 225.77 |
| 184 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 5 262.22 | 3 094.81 | 2 992.56 |
| 185 | php (7.4)| [slim](slimframework.com) (4.6) | 4 400.12 | 4 367.14 | 4 354.77 |
| 186 | ruby (2.7)| [rails](rubyonrails.org) (6.0) | 4 217.02 | 4 201.63 | 4 217.59 |
| 187 | php (7.4)| [nette](nette.org/en/) (3.0) | 4 135.39 | 4 113.02 | 4 116.67 |
| 188 | php (7.4)| [yii](yiiframework.com) (2.0) | 3 953.67 | 3 945.84 | 3 941.78 |
| 189 | php (7.4)| [lumen](lumen.laravel.com) (8.1) | 3 857.64 | 3 856.85 | 3 893.65 |
| 190 | julia (1.5)| [merly](https://github.com/codeneomatrix/Merly.jl) (0.2) | 3 775.30 | 8 492.97 | 6 415.57 |
| 191 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.4) | 3 496.88 | 3 499.26 | 3 512.43 |
| 192 | php (7.4)| [symfony](symfony.com) (5.1) | 3 107.28 | 3 120.86 | 3 143.90 |
| 193 | crystal (0.35)| [runcobo](https://github.com/runcobo/runcobo) (1.0) | 2 856.67 | 2 909.85 | 2 867.86 |
| 194 | php (7.4)| [mezzio](docs.mezzio.dev) (3.2) | 2 805.86 | 2 820.28 | 2 823.14 |
| 195 | r (4.0)| [restrserve](restrserve.org) (0.3) | 2 670.18 | 2 627.03 | 2 535.42 |
| 196 | python (3.8)| [cyclone](cyclone.io) (1.3) | 2 522.50 | 2 521.44 | 2 516.01 |
| 197 | ruby (2.7)| [pakyow](pakyow.com) (1.0) | 2 488.39 | 2 576.16 | 2 527.27 |
| 198 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 817.39 | 1 771.12 | 1 703.66 |
| 199 | python (3.8)| [django-ninja](django-ninja.rest-framework.com) (0.8) | 1 706.97 | 2 459.37 | 2 505.01 |
| 200 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 653.84 | 1 675.39 | 1 645.49 |
| 201 | perl (5.32)| [dancer2](perldancer.org) (0.3) | 1 645.58 | 1 087.12 | 1 242.55 |
| 202 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 592.24 | 1 540.75 | 1 533.96 |
| 203 | php (7.4)| [laminas](getlaminas.org) (3.1) | 1 478.69 | 1 499.39 | 1 537.83 |
| 204 | php (7.4)| [codeigniter4](codeigniter.com) (4.0) | 1 099.64 | 1 123.94 | 1 203.62 |
| 205 | crystal (0.35)| [lucky](luckyframework.org) (0.23) | 649.05 | 672.94 | 662.79 |
| 206 | r (4.0)| [plumber](rplumber.io) (1.0) | 417.61 | 448.43 | 431.24 |
| 207 | php (7.4)| [laravel](laravel.com) (7.27) | 354.96 | 183.46 | 1 631.95 |

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

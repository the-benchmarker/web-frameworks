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

:information_source:  Updated on **2020-11-20** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | go (1.15)| [fiber](https://gofiber.io) (2.2) | 182 799.94 | 195 606.72 | 194 411.58 |
| 2 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 177 613.28 | 183 408.51 | 183 677.23 |
| 3 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 176 796.60 | 190 662.99 | 189 917.74 |
| 4 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.5) | 176 303.14 | 189 880.56 | 189 132.91 |
| 5 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 175 921.38 | 189 098.74 | 188 757.96 |
| 6 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.17) | 175 690.68 | 196 177.27 | 197 634.55 |
| 7 | php (7.4)| [mark](https://github.com/passwalls/mark) (1.1) | 167 802.73 | 200 341.10 | 202 930.79 |
| 8 | java (11)| [jooby](https://jooby.io) (2.8) | 155 655.91 | 204 506.63 | 211 020.48 |
| 9 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 155 020.12 | 192 585.84 | 196 711.91 |
| 10 | php (7.4)| [workerman](https://github.com/walkor/Workerman) (4.0) | 150 214.16 | 181 106.65 | 183 646.69 |
| 11 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 149 731.89 | 196 728.15 | 202 026.21 |
| 12 | crystal (0.35)| [toro](https://github.com/soveran/toro) (0.4) | 148 660.91 | 180 915.62 | 181 622.41 |
| 13 | crystal (0.35)| [grip](https://github.com/grip-framework/grip) (4.0) | 141 982.77 | 171 492.31 | 171 723.29 |
| 14 | crystal (0.35)| [kemal](https://kemalcr.com) (0.26) | 138 611.31 | 168 787.20 | 168 618.44 |
| 15 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 120 957.48 | 122 129.44 | 125 393.30 |
| 16 | go (1.15)| [clevergo](https://clevergo.tech) (0.3) | 120 688.64 | 121 957.14 | 125 271.24 |
| 17 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 119 796.68 | 121 779.81 | 125 256.60 |
| 18 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 119 339.14 | 120 398.76 | 123 705.49 |
| 19 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 117 409.85 | 123 394.30 | 125 683.58 |
| 20 | java (11)| [act](https://actframework.org) (1.9) | 115 875.56 | 148 742.22 | 151 994.95 |
| 21 | go (1.15)| [chi](https://github.com/go-chi/chi) (4.1) | 115 063.04 | 113 779.80 | 117 627.16 |
| 22 | php (7.4)| [simps](https://github.com/simple-swoole/simps) (1.0) | 114 358.69 | 171 573.09 | 191 202.80 |
| 23 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 113 297.34 | 120 385.82 | 123 593.22 |
| 24 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 112 434.57 | 111 581.59 | 115 506.62 |
| 25 | go (1.15)| [violetear](https://violetear.org) (7.0) | 111 680.95 | 110 612.64 | 114 105.98 |
| 26 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 111 613.41 | 110 496.66 | 113 932.44 |
| 27 | fsharp (5.0)| [frank](https://github.com/frank-fs/frank) (6.1) | 111 471.64 | 129 955.48 | 134 294.96 |
| 28 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 111 185.42 | 108 573.59 | 113 037.50 |
| 29 | fsharp (5.0)| [falco](https://github.com/pimbrouwers/Falco) (2.1) | 110 241.60 | 128 736.34 | 132 547.88 |
| 30 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 109 945.71 | 115 423.65 | 116 047.17 |
| 31 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 107 026.87 | 103 745.18 | 107 894.46 |
| 32 | csharp (8.0)| [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (5.0) | 106 118.01 | 121 898.01 | 125 853.56 |
| 33 | go (1.15)| [beego](https://beego.me) (1.12) | 105 278.68 | 109 670.55 | 112 407.20 |
| 34 | go (1.15)| [air](https://github.com/aofei/air) (0.20) | 95 259.13 | 93 728.89 | 97 391.36 |
| 35 | php (7.4)| [nano](https://) (0.0.9) | 95 249.48 | 139 691.67 | 150 857.55 |
| 36 | csharp (8.0)| [carter](https://github.com/CarterCommunity/Carter) (5.2) | 94 720.11 | 104 718.81 | 104 193.55 |
| 37 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.4) | 90 555.04 | 91 225.38 | 95 446.13 |
| 38 | fsharp (5.0)| [saturn](https://saturnframework.org) (0.14) | 85 552.72 | 91 923.99 | 86 727.61 |
| 39 | java (11)| [spring-framework](https://spring.io/projects/spring-framework) (5.3) | 85 225.80 | 91 341.56 | 92 752.84 |
| 40 | go (1.15)| [gf](https://goframe.org) (1.14) | 82 795.23 | 89 643.69 | 91 622.49 |
| 41 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 78 670.81 | 86 662.21 | 97 207.30 |
| 42 | java (11)| [javalin](https://javalin.io) (3.9) | 78 392.08 | 84 142.44 | 85 388.84 |
| 43 | php (7.4)| [one](https://github.com/lizhichao/one) (2.0) | 75 823.01 | 98 451.09 | 101 509.43 |
| 44 | php (7.4)| [hyperf](https://hyperf.io) (2.0) | 75 088.86 | 93 073.62 | 100 860.29 |
| 45 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 74 106.79 | 81 460.79 | 82 427.57 |
| 46 | java (11)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 73 395.50 | 80 963.31 | 81 361.51 |
| 47 | fsharp (5.0)| [websharper](https://websharper.com) (4.6) | 73 170.36 | 82 490.33 | 81 703.92 |
| 48 | java (11)| [restheart](https://restheart.org) (5.1) | 69 792.34 | 90 013.90 | 94 844.22 |
| 49 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 68 389.03 | 69 143.92 | 73 651.79 |
| 50 | rust (1.47)| [actix](https://actix.rs) (3.2) | 68 323.28 | 75 122.21 | 75 975.54 |
| 51 | php (7.4)| [ubiquity](https://ubiquity.kobject.net) (2.3) | 66 724.02 | 69 861.07 | 66 748.13 |
| 52 | php (7.4)| [comet](https://github.com/gotzmann/comet) (0.8) | 65 779.27 | 72 185.90 | 73 601.74 |
| 53 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 63 626.83 | 68 166.56 | 73 148.65 |
| 54 | java (11)| [micronaut](https://micronaut.io) (1.2) | 61 548.66 | 74 773.12 | 75 641.17 |
| 55 | php (7.4)| [chubbyphp-workerman](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 61 049.24 | 65 542.24 | 66 587.70 |
| 56 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 60 564.88 | 65 619.76 | 66 173.19 |
| 57 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 59 510.40 | 61 473.82 | 61 541.47 |
| 58 | rust (1.47)| [nickel](https://nickel-org.github.io) (0.11) | 57 290.64 | 57 599.16 | 58 105.59 |
| 59 | swift (5.3)| [kitura-nio](https://kitura.io) (2.9) | 53 893.71 | 53 384.54 | 53 541.55 |
| 60 | swift (5.3)| [kitura](https://kitura.io) (2.9) | 53 763.97 | 53 778.86 | 53 324.18 |
| 61 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.27) | 53 166.31 | 60 567.07 | 62 130.94 |
| 62 | php (7.4)| [siler-swoole](https://siler.leocavalcante.dev) (1.7) | 51 420.07 | 64 328.73 | 67 326.33 |
| 63 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 50 800.99 | 55 013.49 | 54 993.94 |
| 64 | fsharp (5.0)| [suave](https://suave.io) (2.5) | 49 423.92 | 44 939.58 | 38 093.08 |
| 65 | swift (5.3)| [vapor](https://vapor.codes) (4.35) | 49 318.37 | 52 123.57 | 51 911.46 |
| 66 | python (3.8)| [hug](https://hug.rest) (2.6) | 48 143.33 | 51 666.30 | 51 639.53 |
| 67 | php (7.4)| [imi](https://imiphp.com) (1.2) | 46 257.50 | 50 503.98 | 52 269.78 |
| 68 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.2) | 46 077.00 | 52 335.93 | 53 295.49 |
| 69 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.9) | 45 918.76 | 51 606.38 | 52 034.03 |
| 70 | python (3.8)| [starlette](https://starlette.io) (0.13) | 45 056.99 | 46 695.27 | 48 229.47 |
| 71 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 43 998.41 | 53 544.31 | 54 915.20 |
| 72 | csharp (8.0)| [simplify.web](https://web.simplifynet.dev) (4.2) | 42 043.05 | 46 089.92 | 45 739.37 |
| 73 | rust (1.47)| [gotham](https://gotham.rs) (0.4) | 40 612.58 | 45 164.81 | 46 187.57 |
| 74 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 36 793.80 | 38 215.84 | 38 010.28 |
| 75 | crystal (0.35)| [orion](https://github.com/obsidian/orion) (3.0) | 36 631.60 | 35 077.75 | 32 547.68 |
| 76 | python (3.8)| [emmett](https://emmett.sh) (2.1) | 34 227.51 | 39 674.91 | 40 255.23 |
| 77 | python (3.8)| [index.py](https://index-py.abersheeran.com) (0.12) | 32 980.96 | 37 147.93 | 37 203.65 |
| 78 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 32 870.98 | 31 777.58 | 30 314.06 |
| 79 | r (4.0)| [rserve](https://rforge.net/Rserve/) (1.7) | 32 506.32 | 27 208.45 | 26 470.40 |
| 80 | fsharp (5.0)| [giraffe](https://github.com/giraffe-fsharp/Giraffe) (4.1) | 30 821.22 | 31 132.16 | 30 916.05 |
| 81 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 29 669.84 | 29 858.85 | 28 995.32 |
| 82 | php (7.4)| [chubbyphp-swoole](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 28 339.57 | 34 155.94 | 35 364.09 |
| 83 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.61) | 26 204.00 | 30 014.35 | 30 814.27 |
| 84 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.6) | 26 164.99 | 32 562.77 | 33 397.79 |
| 85 | php (7.4)| [swoft](https://swoft.org) (2.0) | 25 966.68 | 30 504.92 | 30 743.70 |
| 86 | python (3.8)| [responder](https://python-responder.org) (2.0) | 25 664.38 | 32 241.11 | 32 265.95 |
| 87 | php (7.4)| [sw-fw-less](https://github.com/luoxiaojun1992/sw-fw-less) (preview) | 25 193.03 | 29 702.73 | 30 881.27 |
| 88 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 24 779.83 | 26 899.72 | 28 087.63 |
| 89 | php (7.4)| [yii-swoole](https://yiiframework.com) (2.0) | 24 768.00 | 29 144.68 | 29 713.37 |
| 90 | rust (1.47)| [iron](https://ironframework.io) (0.6) | 23 852.89 | 23 321.76 | 23 124.72 |
| 91 | go (1.15)| [macaron](https://go-macaron.com) (1.4) | 23 046.67 | 25 510.29 | 25 368.94 |
| 92 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 22 687.74 | 27 233.11 | 27 096.64 |
| 93 | php (7.4)| [slim-swoole](https://slimframework.com) (4.5) | 21 789.55 | 25 626.52 | 26 223.57 |
| 94 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 21 692.12 | 22 762.21 | 22 596.35 |
| 95 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 20 079.28 | 25 250.09 | 25 111.32 |
| 96 | java (11)| [blade](https://lets-blade.com) (2.0) | 17 837.36 | 21 680.80 | 20 076.48 |
| 97 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 17 054.55 | 17 495.50 | 17 587.50 |
| 98 | php (7.4)| [chubbyphp-roadrunner](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 15 548.07 | 15 940.92 | 16 147.04 |
| 99 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 15 366.63 | 14 935.06 | 14 542.35 |
| 100 | php (7.4)| [slim-roadrunner](https://slimframework.com) (4.5) | 15 029.42 | 15 393.30 | 15 548.63 |
| 101 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 14 737.71 | 15 051.23 | 15 009.52 |
| 102 | php (7.4)| [spiral](https://github.com/spiral/framework) (2.6) | 14 579.16 | 14 764.51 | 14 905.15 |
| 103 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 12 733.33 | 12 782.50 | 12 802.84 |
| 104 | java (11)| [struts2](https://struts.apache.org) (2.5) | 12 639.79 | 13 284.08 | 13 262.17 |
| 105 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.13) | 11 496.22 | 11 114.34 | 10 644.89 |
| 106 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 11 271.04 | 19 949.55 | 18 265.98 |
| 107 | python (3.8)| [tornado](https://tornadoweb.org) (6.1) | 9 811.69 | 10 301.02 | 9 737.04 |
| 108 | php (7.4)| [basicphp](https://github.com/ray-ang/basicphp) (0.9) | 9 703.95 | 9 581.97 | 9 250.34 |
| 109 | php (7.4)| [sunrise-router-roadrunner](https://github.com/sunrise-php/http-router) (2.4) | 9 357.35 | 8 753.59 | 8 444.77 |
| 110 | php (7.4)| [fastsitephp](https://github.com/fastsitephp/fastsitephp) (1.4) | 9 346.83 | 9 216.47 | 8 976.53 |
| 111 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 8 937.29 | 8 911.39 | 8 712.50 |
| 112 | python (3.8)| [django](https://djangoproject.com) (3.1) | 8 325.15 | 8 138.66 | 8 035.47 |
| 113 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 7 618.02 | 7 408.48 | 6 619.48 |
| 114 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 7 437.61 | 8 763.08 | 9 240.19 |
| 115 | php (7.4)| [one-fpm](https://github.com/lizhichao/one) (2.0) | 6 994.77 | 6 887.89 | 6 816.48 |
| 116 | php (7.4)| [phalcon](https://phalcon.io) (4.1) | 6 733.49 | 6 648.91 | 6 580.69 |
| 117 | php (7.4)| [hamlet](https://github.com/vasily-kartashov/hamlet-core) (3.2) | 6 665.72 | 6 584.62 | 6 490.29 |
| 118 | php (7.4)| [siler](https://siler.leocavalcante.dev) (1.7) | 6 535.55 | 6 453.06 | 6 394.58 |
| 119 | php (7.4)| [bearframework](https://github.com/bearframework/bearframework) (1.2) | 5 913.69 | 5 819.48 | 5 789.87 |
| 120 | php (7.4)| [ice](https://iceframework.org) (1.5) | 5 725.12 | 5 649.62 | 5 612.25 |
| 121 | php (7.4)| [sunrise-router](https://github.com/sunrise-php/http-router) (2.4) | 5 664.59 | 5 581.51 | 5 533.95 |
| 122 | php (7.4)| [chubbyphp](https://github.com/chubbyphp/chubbyphp-framework) (3.1) | 5 313.60 | 5 235.20 | 5 229.28 |
| 123 | php (7.4)| [slim](https://slimframework.com) (4.6) | 4 408.34 | 4 384.66 | 4 371.11 |
| 124 | php (7.4)| [nette](https://nette.org/en/) (3.0) | 4 118.65 | 4 083.73 | 4 121.85 |
| 125 | php (7.4)| [yii](https://yiiframework.com) (2.0) | 3 919.86 | 3 915.11 | 3 912.94 |
| 126 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 3 915.42 | 2 482.36 | 2 584.54 |
| 127 | php (7.4)| [lumen](https://lumen.laravel.com) (8.1) | 3 852.03 | 3 855.27 | 3 901.47 |
| 128 | php (7.4)| [sunrise-router-annotations](https://github.com/sunrise-php/http-router) (2.4) | 3 487.73 | 3 497.84 | 3 500.64 |
| 129 | php (7.4)| [symfony](https://symfony.com) (5.1) | 3 091.25 | 3 125.35 | 3 127.56 |
| 130 | php (7.4)| [mezzio](https://docs.mezzio.dev) (3.2) | 2 804.79 | 2 826.20 | 2 826.36 |
| 131 | r (4.0)| [restrserve](https://restrserve.org) (0.3) | 2 799.28 | 2 718.48 | 2 705.73 |
| 132 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 2 502.67 | 2 509.72 | 2 501.79 |
| 133 | python (3.8)| [django-ninja](https://django-ninja.rest-framework.com) (0.8) | 1 854.62 | 2 440.42 | 2 508.83 |
| 134 | r (4.0)| [httpuv](https://github.com/rstudio/httpuv) (1.5) | 1 801.26 | 1 766.17 | 1 682.42 |
| 135 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 652.66 | 1 671.63 | 1 641.97 |
| 136 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 611.41 | 1 551.32 | 1 541.25 |
| 137 | php (7.4)| [laminas](https://getlaminas.org) (3.1) | 1 485.46 | 1 507.28 | 1 542.34 |
| 138 | php (7.4)| [codeigniter4](https://codeigniter.com) (4.0) | 1 104.79 | 1 127.20 | 1 191.76 |
| 139 | crystal (0.35)| [lucky](https://luckyframework.org) (0.23) | 651.04 | 677.06 | 668.33 |
| 140 | r (4.0)| [plumber](https://rplumber.io) (1.0) | 421.52 | 447.72 | 435.80 |
| 141 | php (7.4)| [laravel](https://laravel.com) (7.27) | 312.06 | 174.97 | 2 180.97 |

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

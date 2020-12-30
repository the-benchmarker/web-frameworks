# Which is the fastest?

[![Build Status](https://the-benchmarker.semaphoreci.com/badges/web-frameworks/branches/master.svg)](https://the-benchmarker.semaphoreci.com/projects/web-frameworks)

[![Chat with US](https://img.shields.io/badge/slack-Chat_with_us-blueviolet)](https://thebenchmarker.slack.com)

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

:information_source:  Updated on **2020-12-28** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | go (1.15)| [gearbox](https://gogearbox.com) (1.1) | 120 354.45 | 123 619.56 | 123 675.26 |
| 2 | go (1.15)| [fiber](https://gofiber.io) (2.3) | 120 043.17 | 130 563.82 | 129 530.89 |
| 3 | go (1.15)| [atreugo](https://github.com/savsgio/atreugo/blob/master/docs/README.md) (11.5) | 116 920.66 | 129 216.38 | 128 642.31 |
| 4 | go (1.15)| [fasthttp](https://pkg.go.dev/github.com/valyala/fasthttp) (1.18) | 116 186.80 | 130 249.48 | 130 743.54 |
| 5 | go (1.15)| [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (4.4) | 115 798.69 | 128 484.80 | 128 155.83 |
| 6 | go (1.15)| [router](https://pkg.go.dev/github.com/fasthttp/router) (1.3) | 115 668.41 | 128 958.13 | 128 261.21 |
| 7 | go (1.15)| [webgo](https://github.com/bnkamalesh/webgo) (4.1) | 112 067.82 | 111 290.84 | 114 545.76 |
| 8 | go (1.15)| [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (1.8) | 108 349.78 | 104 960.04 | 109 602.25 |
| 9 | go (1.15)| [rte](https://github.com/jwilner/rte) (0.0) | 81 168.40 | 82 161.42 | 83 955.37 |
| 10 | go (1.15)| [gf](https://goframe.org) (1.14) | 80 800.99 | 88 511.59 | 91 047.36 |
| 11 | go (1.15)| [clevergo](https://clevergo.tech) (0.5) | 80 415.46 | 81 578.10 | 83 797.49 |
| 12 | go (1.15)| [httprouter](https://pkg.go.dev/github.com/julienschmidt/httprouter) (1.3) | 79 851.09 | 80 716.36 | 82 569.94 |
| 13 | go (1.15)| [echo](https://echo.labstack.com) (4.1) | 79 451.85 | 80 627.18 | 82 774.15 |
| 14 | go (1.15)| [gin](https://gin-gonic.com) (1.6) | 78 402.98 | 82 652.88 | 84 145.36 |
| 15 | go (1.15)| [gorouter](https://github.com/vardius/gorouter/wiki) (4.5) | 78 271.88 | 82 946.54 | 84 136.69 |
| 16 | go (1.15)| [chi](https://github.com/go-chi/chi) (1.5) | 77 225.78 | 76 598.41 | 79 171.28 |
| 17 | go (1.15)| [violetear](https://violetear.org) (7.0) | 76 968.78 | 77 124.41 | 79 345.09 |
| 18 | go (1.15)| [kami](https://github.com/guregu/kami) (2.2) | 75 225.30 | 79 216.74 | 79 760.45 |
| 19 | go (1.15)| [aero](https://github.com/aerogo/aero) (1.3) | 75 164.72 | 74 194.21 | 76 462.82 |
| 20 | go (1.15)| [goroute](https://goroute.github.io) (0.0) | 73 730.70 | 71 657.69 | 74 815.79 |
| 21 | python (3.8)| [falcon](https://falconframework.org) (2.0) | 73 403.33 | 80 034.89 | 81 809.51 |
| 22 | go (1.15)| [beego](https://beego.me) (1.12) | 71 640.17 | 74 664.64 | 76 691.05 |
| 23 | go (1.15)| [air](https://github.com/aofei/air) (0.21) | 65 102.37 | 63 616.43 | 66 157.85 |
| 24 | go (1.15)| [goyave](https://github.com/System-Glitch/goyave) (3.6) | 63 067.17 | 63 067.33 | 65 804.64 |
| 25 | rust (1.48)| [actix](https://actix.rs) (3.3) | 58 527.60 | 59 421.53 | 59 583.49 |
| 26 | python (3.8)| [pyramid](https://trypyramid.com) (1.1) | 52 988.81 | 58 028.46 | 58 033.46 |
| 27 | swift (5.3)| [perfect](https://perfect.org) (3.1) | 52 262.90 | 57 074.06 | 64 094.79 |
| 28 | go (1.15)| [mars](https://github.com/roblillack/mars) (1.0) | 46 185.25 | 46 639.52 | 49 595.29 |
| 29 | python (3.8)| [asgineer](https://asgineer.readthedocs.io) (0.8) | 44 916.57 | 50 792.77 | 52 317.56 |
| 30 | clojure (1.1)| [coast](https://coastonclojure.com) (1.0) | 44 806.32 | 45 792.79 | 45 675.09 |
| 31 | haskell (8.8)| [scotty](https://hackage.haskell.org/package/scotty) (0.12) | 44 705.27 | 48 962.35 | 51 121.69 |
| 32 | python (3.8)| [bottle](https://bottlepy.org) (0.12) | 42 542.16 | 45 940.41 | 46 524.29 |
| 33 | swift (5.3)| [vapor](https://vapor.codes) (4.36) | 34 981.20 | 37 498.31 | 37 283.85 |
| 34 | swift (5.3)| [kitura-nio](https://kitura.dev) (2.9) | 34 218.60 | 34 642.50 | 34 621.52 |
| 35 | python (3.8)| [hug](https://hug.rest) (2.6) | 33 642.90 | 35 051.98 | 53 122.38 |
| 36 | dart (2.10)| [aqueduct](https://aqueduct.io) (3.3) | 31 765.71 | 31 582.35 | 31 511.42 |
| 37 | python (3.8)| [apidaora](https://github.com/dutradda/apidaora) (0.28) | 30 231.16 | 35 292.94 | 35 554.47 |
| 38 | python (3.8)| [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (0.3) | 30 179.29 | 33 573.74 | 34 307.81 |
| 39 | python (3.8)| [sanic](https://github.com/huge-success/sanic) (20.9) | 29 593.94 | 33 347.50 | 33 913.44 |
| 40 | python (3.8)| [emmett](https://emmett.sh) (2.1) | 27 896.25 | 36 429.88 | 36 690.64 |
| 41 | rust (1.48)| [salvo](https://github.com/kenorld/salvo) (0.3) | 27 890.39 | 29 583.93 | 29 602.52 |
| 42 | python (3.8)| [starlette](https://starlette.io) (0.14) | 27 429.18 | 31 404.74 | 32 271.87 |
| 43 | python (3.8)| [index.py](https://index-py.abersheeran.com) (0.15) | 24 723.28 | 29 023.89 | 29 409.07 |
| 44 | python (3.8)| [responder](https://python-responder.org) (2.0) | 24 050.03 | 31 612.57 | 31 790.77 |
| 45 | python (3.8)| [clastic](https://github.com/mahmoud/clastic) (19.9) | 23 501.01 | 26 744.87 | 24 414.71 |
| 46 | haskell (8.8)| [servant](https://servant.dev) (0.17) | 23 012.04 | 21 748.37 | 20 721.01 |
| 47 | clojure (1.1)| [luminus](https://luminusweb.com) (1.0) | 22 308.96 | 21 527.54 | 20 989.48 |
| 48 | python (3.8)| [aiohttp](https://aiohttp.readthedocs.io) (3.7) | 17 995.33 | 23 228.85 | 23 805.24 |
| 49 | python (3.8)| [molten](https://moltenframework.com) (1.0) | 17 580.63 | 21 086.82 | 21 047.56 |
| 50 | rust (1.48)| [iron](https://ironframework.io) (0.6) | 17 080.66 | 16 396.86 | 16 570.65 |
| 51 | python (3.8)| [fastapi](https://fastapi.tiangolo.com) (0.62) | 16 529.16 | 20 640.56 | 21 153.51 |
| 52 | go (1.15)| [macaron](https://go-macaron.com) (1.4) | 15 376.18 | 16 957.48 | 16 886.95 |
| 53 | python (3.8)| [masonite](https://masoniteproject.com) (2.3) | 14 937.92 | 20 038.33 | 20 094.12 |
| 54 | python (3.8)| [flask](https://flask.pocoo.org) (1.1) | 14 439.33 | 17 103.89 | 14 787.96 |
| 55 | swift (5.3)| [swifter](https://github.com/httpswift/swifter) (1.5) | 13 879.40 | 13 512.29 | 21 246.87 |
| 56 | dart (2.10)| [start](https://github.com/lvivski/start) (0.4) | 11 398.65 | 11 177.32 | 10 964.61 |
| 57 | go (1.15)| [tango](https://gitea.com/lunny/tango) (0.6) | 11 388.43 | 11 703.78 | 11 746.99 |
| 58 | go (1.15)| [gramework](https://github.com/gramework/gramework) (1.7) | 9 838.21 | 10 025.45 | 10 093.64 |
| 59 | python (3.8)| [cherrypy](https://github.com/cherrypy/cherrypy) (18.6) | 9 800.07 | 10 131.51 | 9 433.71 |
| 60 | python (3.8)| [guillotina](https://guillotina.io) (6.0) | 9 082.62 | 8 789.69 | 8 751.48 |
| 61 | pony (0.38)| [jennet](https://github.com/Theodus/jennet) (0.1) | 8 527.85 | 13 488.26 | 12 457.36 |
| 62 | python (3.8)| [quart](https://pgjones.gitlab.io/quart) (0.14) | 7 497.10 | 7 313.08 | 6 852.41 |
| 63 | python (3.8)| [tornado](https://tornadoweb.org) (6.1) | 7 094.54 | 6 537.79 | 6 400.66 |
| 64 | python (3.8)| [django](https://djangoproject.com) (3.1) | 5 571.18 | 5 441.67 | 5 157.57 |
| 65 | clojure (1.1)| [yada](https://juxt.pro/yada/) (1.2) | 5 524.60 | 6 475.86 | 6 745.36 |
| 66 | python (3.8)| [tonberry](https://github.com/Ayehavgunne/Tonberry) (0.2) | 5 029.15 | 4 936.02 | 4 392.76 |
| 67 | v (0.1)| [vape](https://github.com/exastencil/vape) (0.3) | 3 774.94 | 2 433.05 | 1 602.55 |
| 68 | python (3.8)| [cyclone](https://cyclone.io) (1.3) | 1 651.64 | 1 643.74 | 1 635.13 |
| 69 | python (3.8)| [klein](https://github.com/twisted/klein) (20.6) | 1 628.59 | 1 636.97 | 1 605.35 |
| 70 | python (3.8)| [django-ninja](https://django-ninja.rest-framework.com) (0.9) | 1 304.23 | 1 592.65 | 1 643.28 |
| 71 | python (3.8)| [nameko](https://github.com/nameko/nameko) (2.13) | 1 039.09 | 1 008.87 | 998.74 |

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

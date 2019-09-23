# Which is the fastest?

[![Build Status](https://travis-ci.com/the-benchmarker/web-frameworks.svg?branch=master)](https://travis-ci.com/the-benchmarker/web-frameworks)
[![Join the chat at https://gitter.im/which_is_the_fastest/Lobby](https://badges.gitter.im/which_is_the_fastest/Lobby.svg)](https://gitter.im/which_is_the_fastest/Lobby)

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

:information_source: you need `wrk` **stable**

~~~sh
git clone --branch 4.1.0 https://github.com/wg/wrk
~~~

:warning: `docker` is used for **development** purpose, `production` results will be computed on [DigitalOcean](https://www.digitalocean.com) :warning:

## Usage

+ Install all dependencies

~~~sh
shards install
~~~

+ Build internal tools

~~~sh
shards build
~~~

+ Make framework list

~~~sh
bin/make config
~~~

+ Build containers

> jobs are either languages (example : crystal) or frameworks (example : router.cr)

~~~sh
bin/neph [job1] [job2] [job3] ...
~~~

+ Start the benchmark ....

> tools is a list of language / framework to challenge (example : ruby kemal amber go python)

~~~sh
bin/benchmarker [tools]
~~~

## Results

<!-- Result from here -->
Last update: 2019-09-22
```
OS: Linux (version: 5.2.15-200.fc30.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: flame (ruby)


:four: hanami (ruby)


:five: roda (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 0.10 ms | **0.10** ms | 0.14 ms | 0.20 ms | 4.86 ms | **105.33** | 
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 0.48 ms | **0.39** ms | 0.96 ms | 1.82 ms | 17.17 ms | **413.00** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 5.17 ms | **0.47** ms | 17.69 ms | 45.52 ms | 114.51 ms | **9856.33** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 6.06 ms | **0.57** ms | 20.17 ms | 45.09 ms | 113.93 ms | **10269.67** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.24**) | 2.58 ms | **0.65** ms | 7.49 ms | 16.33 ms | 48.35 ms | **3716.00** | 
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 2.63 ms | **0.77** ms | 7.28 ms | 15.49 ms | 46.31 ms | **3525.00** | 
| `ruby` (`2.6`) | [grape](https://ruby-grape.org) (**1.2**) | 8.65 ms | **0.77** ms | 28.10 ms | 57.29 ms | 122.79 ms | **13586.67** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 7.70 ms | **0.83** ms | 24.45 ms | 55.22 ms | 130.97 ms | **12434.33** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 3.09 ms | **0.86** ms | 8.42 ms | 18.20 ms | 69.05 ms | **4176.00** | 
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 4.01 ms | **0.99** ms | 14.60 ms | 15.11 ms | 1861.52 ms | **28410.67** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 3.65 ms | **1.25** ms | 9.55 ms | 18.53 ms | 48.66 ms | **4389.00** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 31.10 ms | **2.29** ms | 102.79 ms | 282.77 ms | 859.88 ms | **60068.00** | 
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 4.07 ms | **3.64** ms | 7.94 ms | 13.55 ms | 30.64 ms | **2910.00** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 4.31 ms | **3.75** ms | 9.29 ms | 15.88 ms | 39.08 ms | **3689.00** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 4.43 ms | **4.01** ms | 8.68 ms | 14.39 ms | 33.24 ms | **3182.67** | 
| `nim` (`0.20`) | [httpbeast](https://github.com/dom96/httpbeast) (**0.2**) | 4.60 ms | **4.12** ms | 8.03 ms | 13.33 ms | 28.56 ms | **2605.00** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 10.33 ms | **4.25** ms | 29.46 ms | 88.46 ms | 162.95 ms | **18800.00** | 
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 4.73 ms | **4.36** ms | 8.47 ms | 14.05 ms | 28.99 ms | **2778.67** | 
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 5.10 ms | **4.62** ms | 7.94 ms | 14.29 ms | 147.80 ms | **2926.00** | 
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 5.12 ms | **4.64** ms | 7.96 ms | 14.38 ms | 132.86 ms | **2638.33** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 5.18 ms | **4.71** ms | 8.61 ms | 14.33 ms | 33.61 ms | **2615.00** | 
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 5.22 ms | **4.73** ms | 8.10 ms | 14.68 ms | 125.40 ms | **2614.00** | 
| `crystal` (`0.29`) | [toro](https://github.com/soveran/toro) (**0.4**) | 5.61 ms | **4.86** ms | 9.57 ms | 15.51 ms | 31.95 ms | **2991.33** | 
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 5.76 ms | **5.03** ms | 9.68 ms | 15.71 ms | 37.87 ms | **3032.67** | 
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 5.88 ms | **5.07** ms | 9.86 ms | 15.64 ms | 32.46 ms | **3007.00** | 
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 6.06 ms | **5.14** ms | 10.47 ms | 17.06 ms | 36.22 ms | **3352.00** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 9.60 ms | **5.28** ms | 10.67 ms | 139.83 ms | 1053.76 ms | **42566.33** | 
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 6.50 ms | **5.41** ms | 10.63 ms | 18.08 ms | 261.63 ms | **5367.00** | 
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 6.22 ms | **5.42** ms | 10.15 ms | 16.02 ms | 33.89 ms | **3028.33** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.11**) | 8.17 ms | **5.99** ms | 12.72 ms | 62.47 ms | 121.95 ms | **10036.67** | 
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 6.98 ms | **6.03** ms | 11.59 ms | 19.23 ms | 43.33 ms | **3657.00** | 
| `nim` (`0.20`) | [jester](https://github.com/dom96/jester) (**0.4**) | 6.90 ms | **6.36** ms | 10.43 ms | 15.67 ms | 36.86 ms | **2780.67** | 
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 7.56 ms | **6.81** ms | 11.95 ms | 19.88 ms | 45.20 ms | **3596.67** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 7.75 ms | **6.86** ms | 11.44 ms | 18.13 ms | 268.22 ms | **6904.67** | 
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 8.43 ms | **6.97** ms | 13.06 ms | 29.54 ms | 343.31 ms | **9992.00** | 
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 8.44 ms | **7.15** ms | 14.38 ms | 27.12 ms | 73.09 ms | **4983.33** | 
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 9.01 ms | **7.46** ms | 14.30 ms | 31.99 ms | 311.63 ms | **9841.67** | 
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 8.97 ms | **7.54** ms | 14.92 ms | 32.21 ms | 171.27 ms | **6131.00** | 
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 9.23 ms | **7.62** ms | 15.77 ms | 34.41 ms | 167.75 ms | **6458.33** | 
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 9.41 ms | **7.78** ms | 15.62 ms | 34.41 ms | 285.37 ms | **8082.33** | 
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 9.71 ms | **7.97** ms | 16.69 ms | 36.69 ms | 121.80 ms | **6698.00** | 
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 9.52 ms | **8.00** ms | 15.93 ms | 35.40 ms | 98.99 ms | **6266.00** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 232.38 ms | **8.18** ms | 153.22 ms | 5185.71 ms | 7940.74 ms | **896226.33** | 
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 9.15 ms | **8.18** ms | 13.68 ms | 28.38 ms | 122.54 ms | **5176.67** | 
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 10.87 ms | **8.47** ms | 19.54 ms | 45.55 ms | 183.73 ms | **8859.67** | 
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.13**) | 11.04 ms | **8.78** ms | 18.87 ms | 43.70 ms | 334.48 ms | **11023.00** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 10.13 ms | **8.85** ms | 13.84 ms | 24.64 ms | 436.84 ms | **13783.33** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 12.70 ms | **9.45** ms | 27.02 ms | 49.92 ms | 170.55 ms | **10814.67** | 
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 13.12 ms | **9.51** ms | 16.86 ms | 83.32 ms | 589.48 ms | **27390.33** | 
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 12.20 ms | **9.54** ms | 16.85 ms | 55.40 ms | 446.36 ms | **18637.00** | 
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 12.55 ms | **9.71** ms | 17.72 ms | 63.11 ms | 466.72 ms | **18563.33** | 
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 12.89 ms | **9.73** ms | 22.25 ms | 58.26 ms | 281.66 ms | **13761.67** | 
| `go` (`1.13`) | [gf](https://goframe.org) (**1.8**) | 11.75 ms | **9.91** ms | 18.75 ms | 43.08 ms | 292.03 ms | **9091.00** | 
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 10.34 ms | **10.12** ms | 13.41 ms | 17.41 ms | 34.77 ms | **2377.00** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 12.10 ms | **10.40** ms | 22.28 ms | 40.87 ms | 327.28 ms | **11405.00** | 
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 13.98 ms | **10.65** ms | 19.84 ms | 57.71 ms | 575.80 ms | **22933.00** | 
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 15.52 ms | **11.01** ms | 20.24 ms | 108.50 ms | 735.18 ms | **33113.67** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 16.14 ms | **12.31** ms | 32.31 ms | 55.99 ms | 213.76 ms | **11167.00** | 
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 17.50 ms | **12.51** ms | 22.49 ms | 143.19 ms | 785.65 ms | **36063.33** | 
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | 34.58 ms | **12.76** ms | 78.43 ms | 345.72 ms | 1431.87 ms | **81841.67** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 19.64 ms | **13.18** ms | 24.09 ms | 188.32 ms | 882.77 ms | **42233.67** | 
| `php` (`7.3`) | [one](https://github.com/lizhichao/one) (**1.8**) | 13.83 ms | **13.31** ms | 22.07 ms | 32.87 ms | 81.58 ms | **6552.67** | 
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 17.25 ms | **13.41** ms | 23.45 ms | 107.96 ms | 640.65 ms | **28566.67** | 
| `php` (`7.3`) | [basicphp](https://github.com/ray-ang/basicphp) (**0.9**) | 38.75 ms | **13.42** ms | 73.78 ms | 386.82 ms | 762.63 ms | **76974.00** | 
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 15.78 ms | **13.65** ms | 30.47 ms | 54.18 ms | 112.25 ms | **11342.00** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 42.55 ms | **13.65** ms | 83.88 ms | 428.91 ms | 869.71 ms | **85664.67** | 
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 20.54 ms | **13.86** ms | 24.90 ms | 225.24 ms | 848.35 ms | **46097.00** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 44.13 ms | **13.95** ms | 87.86 ms | 452.62 ms | 1010.93 ms | **91237.33** | 
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 19.94 ms | **14.15** ms | 25.16 ms | 172.05 ms | 830.75 ms | **41478.00** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 42.23 ms | **14.23** ms | 76.00 ms | 437.12 ms | 715.02 ms | **86115.33** | 
| `python` (`3.7`) | [asgineer](https://asgineer.readthedocs.io) (**0.7**) | 16.28 ms | **14.71** ms | 26.12 ms | 41.64 ms | 133.85 ms | **7905.33** | 
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 21.91 ms | **14.92** ms | 26.13 ms | 235.27 ms | 973.59 ms | **50925.00** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 47.21 ms | **15.01** ms | 86.09 ms | 504.72 ms | 787.15 ms | **99766.00** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 21.15 ms | **15.53** ms | 47.15 ms | 76.17 ms | 182.04 ms | **16877.33** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**6.0**) | 47.66 ms | **16.26** ms | 85.30 ms | 487.73 ms | 893.10 ms | **96301.00** | 
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 25.06 ms | **16.64** ms | 28.38 ms | 310.63 ms | 1072.67 ms | **59940.00** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 17.17 ms | **16.70** ms | 25.00 ms | 36.13 ms | 159.16 ms | **7003.67** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 20.90 ms | **17.25** ms | 32.92 ms | 68.11 ms | 874.52 ms | **34470.33** | 
| `fsharp` (`7.3`) | [suave](https://https://suave.io) (**2.5**) | 40.49 ms | **17.59** ms | 105.61 ms | 166.43 ms | 898.09 ms | **56844.00** | 
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 25.19 ms | **17.87** ms | 30.11 ms | 246.28 ms | 1246.92 ms | **62043.67** | 
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | 19.27 ms | **18.09** ms | 18.63 ms | 63.85 ms | 687.69 ms | **18616.33** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 19.54 ms | **18.11** ms | 29.45 ms | 43.42 ms | 188.93 ms | **8432.67** | 
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 26.56 ms | **20.45** ms | 26.57 ms | 218.31 ms | 902.74 ms | **47294.33** | 
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 20.88 ms | **21.14** ms | 23.31 ms | 25.17 ms | 141.66 ms | **2524.67** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.0**) | 76.64 ms | **21.16** ms | 149.38 ms | 896.98 ms | 1327.69 ms | **177580.00** | 
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 26.30 ms | **21.49** ms | 24.68 ms | 200.12 ms | 1051.58 ms | **54813.00** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 22.29 ms | **22.21** ms | 27.45 ms | 33.52 ms | 151.17 ms | **4581.33** | 
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 44.50 ms | **26.60** ms | 39.45 ms | 695.61 ms | 1761.70 ms | **113842.00** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 41.58 ms | **26.62** ms | 94.85 ms | 111.56 ms | 257.87 ms | **27294.67** | 
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 51.54 ms | **27.57** ms | 43.30 ms | 798.44 ms | 1739.86 ms | **130964.67** | 
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | 28.49 ms | **27.75** ms | 41.14 ms | 57.75 ms | 98.56 ms | **9984.67** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 43.80 ms | **28.36** ms | 93.84 ms | 149.22 ms | 284.23 ms | **32564.33** | 
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 49.57 ms | **29.17** ms | 44.56 ms | 828.18 ms | 2284.59 ms | **146710.00** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 33.29 ms | **30.09** ms | 47.92 ms | 61.18 ms | 241.05 ms | **12777.67** | 
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 39.56 ms | **38.97** ms | 49.14 ms | 56.11 ms | 279.11 ms | **9369.67** | 
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 42.29 ms | **41.04** ms | 46.79 ms | 53.34 ms | 467.23 ms | **11871.33** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 44.96 ms | **43.63** ms | 69.22 ms | 95.03 ms | 178.41 ms | **18276.00** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 41.96 ms | **45.21** ms | 57.62 ms | 67.14 ms | 151.39 ms | **13332.67** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 47.85 ms | **50.26** ms | 63.35 ms | 97.75 ms | 184.47 ms | **15574.67** | 
| `php` (`7.3`) | [spiral](https://github.com/spiral/framework) (**2.0**) | 56.08 ms | **55.97** ms | 61.82 ms | 64.49 ms | 264.00 ms | **7001.67** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | 80.77 ms | **72.95** ms | 134.71 ms | 173.80 ms | 357.07 ms | **32426.00** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 108.79 ms | **82.47** ms | 248.40 ms | 276.32 ms | 485.02 ms | **62138.33** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 95.90 ms | **93.33** ms | 150.65 ms | 183.70 ms | 283.45 ms | **32823.33** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 148.47 ms | **93.84** ms | 365.80 ms | 444.63 ms | 803.96 ms | **109112.00** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 97.90 ms | **96.02** ms | 128.90 ms | 146.72 ms | 668.48 ms | **31024.33** | 
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | 337.68 ms | **107.12** ms | 760.88 ms | 3492.09 ms | 6975.74 ms | **706369.67** | 
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 206.38 ms | **204.27** ms | 239.72 ms | 262.75 ms | 291.77 ms | **25244.33** | 
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 268.24 ms | **206.20** ms | 210.20 ms | 2779.57 ms | 5187.83 ms | **446212.00** | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (sifrr) (node)


:three: (japronto) (python)


:four: (httpbeast) (nim)


:five: (drogon) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 229783.00 | **132.94** MB |
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 216749.67 | **190.79** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 208150.33 | **249.15** MB |
| `nim` (`0.20`) | [httpbeast](https://github.com/dom96/httpbeast) (**0.2**) | 199918.67 | **284.80** MB |
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 193388.67 | **187.78** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 183265.00 | **329.74** MB |
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 176368.00 | **354.60** MB |
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 176197.33 | **283.98** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 176056.00 | **170.99** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 173983.00 | **452.21** MB |
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 172626.00 | **277.72** MB |
| `crystal` (`0.29`) | [toro](https://github.com/soveran/toro) (**0.4**) | 168006.67 | **158.01** MB |
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 163554.33 | **153.85** MB |
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 161373.67 | **151.79** MB |
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 158278.67 | **168.73** MB |
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 152831.67 | **249.89** MB |
| `nim` (`0.20`) | [jester](https://github.com/dom96/jester) (**0.4**) | 150572.00 | **302.81** MB |
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 146428.67 | **299.80** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 141194.00 | **243.68** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.11**) | 138622.00 | **80.06** MB |
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 137363.33 | **251.63** MB |
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 127723.00 | **208.87** MB |
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 119289.33 | **159.84** MB |
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 116891.00 | **147.43** MB |
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 115204.00 | **295.52** MB |
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 113170.67 | **167.92** MB |
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 112342.33 | **150.42** MB |
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 111203.00 | **148.34** MB |
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 108944.00 | **191.34** MB |
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 107984.67 | **143.41** MB |
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 107229.67 | **188.31** MB |
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 106227.67 | **142.92** MB |
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 105252.33 | **140.57** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 96878.00 | **157.58** MB |
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 96772.33 | **169.84** MB |
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.13**) | 94634.67 | **197.55** MB |
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 90771.00 | **135.97** MB |
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 89496.67 | **134.07** MB |
| `go` (`1.13`) | [gf](https://goframe.org) (**1.8**) | 87815.33 | **149.06** MB |
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 87166.00 | **130.68** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 86351.00 | **202.54** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 85697.67 | **133.57** MB |
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 84622.33 | **111.45** MB |
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 82919.00 | **164.99** MB |
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 79155.67 | **118.51** MB |
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 77765.67 | **116.48** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 76311.00 | **165.06** MB |
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | 71776.33 | **127.98** MB |
| `php` (`7.3`) | [one](https://github.com/lizhichao/one) (**1.8**) | 70990.67 | **163.03** MB |
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 69484.33 | **104.17** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 68380.67 | **51.31** MB |
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 66731.67 | **142.23** MB |
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 66205.00 | **173.90** MB |
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 66006.00 | **138.71** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 64905.33 | **159.97** MB |
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 61654.33 | **130.42** MB |
| `python` (`3.7`) | [asgineer](https://asgineer.readthedocs.io) (**0.7**) | 61120.67 | **109.18** MB |
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 60029.67 | **243.53** MB |
| `php` (`7.3`) | [basicphp](https://github.com/ray-ang/basicphp) (**0.9**) | 59119.33 | **293.70** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 57326.00 | **115.34** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 56626.67 | **281.22** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 55694.00 | **276.80** MB |
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | 55605.67 | **99.73** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 55571.67 | **276.12** MB |
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 53991.33 | **132.31** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 53787.67 | **94.37** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 52666.67 | **261.78** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 52470.67 | **130.18** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 50576.67 | **108.87** MB |
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 49855.00 | **85.23** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.24**) | 49083.33 | **46.90** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**6.0**) | 48683.67 | **253.43** MB |
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 48158.00 | **27.84** MB |
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 47593.33 | **44.76** MB |
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 46209.67 | **85.80** MB |
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 45901.67 | **80.52** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 43796.33 | **115.13** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 41303.67 | **48.77** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.0**) | 38318.33 | **200.28** MB |
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 35406.67 | **91.93** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 34944.33 | **20.16** MB |
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | 34429.67 | **79.08** MB |
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 32562.00 | **55.51** MB |
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 31866.33 | **60.19** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 29965.67 | **64.56** MB |
| `fsharp` (`7.3`) | [suave](https://https://suave.io) (**2.5**) | 28789.33 | **58.10** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 26001.67 | **48.22** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 25577.67 | **62.99** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 24822.33 | **14.31** MB |
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 24807.67 | **30.47** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 23626.00 | **53.48** MB |
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 23316.33 | **21.88** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 22144.67 | **39.46** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 21098.33 | **159.57** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 20806.33 | **40.10** MB |
| `php` (`7.3`) | [spiral](https://github.com/spiral/framework) (**2.0**) | 17364.67 | **30.10** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 16560.00 | **42.98** MB |
| `ruby` (`2.6`) | [grape](https://ruby-grape.org) (**1.2**) | 14879.67 | **8.52** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | 12268.67 | **24.44** MB |
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 11624.00 | **14.88** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 10225.33 | **22.27** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 9906.67 | **29.22** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 9508.00 | **27.51** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 7499.00 | **18.47** MB |
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 4793.67 | **12.36** MB |
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 4770.33 | **5.96** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 4119.33 | **25.88** MB |
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | 601.00 | **1.36** MB |
<!-- Result till here -->

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

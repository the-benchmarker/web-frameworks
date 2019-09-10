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
Last update: 2019-09-10
```
OS: Linux (version: 5.2.11-200.fc30.x86_64, arch: x86_64)
CPU Cores: 12
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: swifter (swift)


:three: iron (rust)


:four: roda (ruby)


:five: syro (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 0.28 ms | **0.24** ms | 0.42 ms | 0.52 ms | 7.83 ms | **125.00** | 
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 5.55 ms | **0.94** ms | 17.43 ms | 21.63 ms | 1903.21 ms | **27664.67** | 
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 1.58 ms | **0.98** ms | 3.97 ms | 7.67 ms | 42.79 ms | **1717.33** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.23**) | 3.24 ms | **1.54** ms | 8.36 ms | 16.76 ms | 52.75 ms | **3878.67** | 
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 3.17 ms | **1.60** ms | 8.17 ms | 16.13 ms | 44.49 ms | **3742.33** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 3.16 ms | **1.72** ms | 7.42 ms | 16.82 ms | 49.45 ms | **3618.67** | 
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 3.35 ms | **2.15** ms | 7.79 ms | 17.31 ms | 49.92 ms | **3733.33** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 3.71 ms | **2.19** ms | 9.40 ms | 18.05 ms | 48.39 ms | **4242.33** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 4.64 ms | **2.67** ms | 11.81 ms | 22.70 ms | 60.19 ms | **5346.00** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 6.43 ms | **3.02** ms | 8.15 ms | 47.40 ms | 720.85 ms | **30698.00** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 36.82 ms | **3.17** ms | 119.77 ms | 320.45 ms | 892.46 ms | **69025.67** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 6.52 ms | **3.55** ms | 16.85 ms | 33.65 ms | 83.71 ms | **7719.67** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.1**) | 7.30 ms | **4.50** ms | 11.36 ms | 83.95 ms | 122.10 ms | **12913.33** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 9.67 ms | **4.92** ms | 23.34 ms | 77.61 ms | 341.84 ms | **16980.33** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 11.02 ms | **6.30** ms | 18.81 ms | 94.41 ms | 209.02 ms | **17793.00** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 9.39 ms | **6.34** ms | 23.02 ms | 45.14 ms | 102.84 ms | **10363.33** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 8.13 ms | **6.95** ms | 16.79 ms | 30.32 ms | 71.06 ms | **6641.67** | 
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 8.67 ms | **7.29** ms | 13.92 ms | 26.80 ms | 263.68 ms | **7706.67** | 
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 8.59 ms | **7.46** ms | 13.24 ms | 27.27 ms | 308.04 ms | **9246.67** | 
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 9.10 ms | **7.54** ms | 14.31 ms | 28.20 ms | 298.75 ms | **9573.33** | 
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 9.35 ms | **7.62** ms | 16.36 ms | 31.90 ms | 295.14 ms | **9167.67** | 
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 10.69 ms | **8.13** ms | 16.39 ms | 46.86 ms | 425.79 ms | **17779.33** | 
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 10.13 ms | **8.67** ms | 18.50 ms | 30.79 ms | 104.33 ms | **6297.00** | 
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 11.20 ms | **9.02** ms | 17.04 ms | 35.70 ms | 398.52 ms | **14077.33** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 10.69 ms | **9.03** ms | 18.32 ms | 33.78 ms | 204.09 ms | **7268.00** | 
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 10.27 ms | **9.19** ms | 15.41 ms | 30.05 ms | 79.39 ms | **4965.33** | 
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 12.03 ms | **9.21** ms | 17.15 ms | 49.46 ms | 477.53 ms | **18469.33** | 
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 10.46 ms | **9.29** ms | 15.65 ms | 30.94 ms | 136.40 ms | **5635.67** | 
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 10.71 ms | **9.58** ms | 16.03 ms | 30.89 ms | 171.96 ms | **5419.67** | 
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 10.14 ms | **9.99** ms | 13.68 ms | 18.70 ms | 73.12 ms | **2980.67** | 
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 14.02 ms | **10.40** ms | 20.87 ms | 53.78 ms | 547.73 ms | **21896.67** | 
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 15.17 ms | **10.67** ms | 20.83 ms | 107.30 ms | 637.67 ms | **29209.00** | 
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 12.32 ms | **10.69** ms | 21.37 ms | 35.47 ms | 248.19 ms | **8280.67** | 
| `crystal` (`0.29`) | [toro](https://github.com/soveran/toro) (**0.4**) | 12.11 ms | **11.09** ms | 20.38 ms | 32.47 ms | 62.43 ms | **6136.00** | 
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 12.38 ms | **11.19** ms | 20.94 ms | 33.21 ms | 71.96 ms | **6312.00** | 
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | 33.00 ms | **11.22** ms | 58.33 ms | 449.97 ms | 1960.95 ms | **95526.67** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 12.03 ms | **11.27** ms | 19.03 ms | 30.45 ms | 77.71 ms | **5530.00** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 172.01 ms | **11.32** ms | 85.13 ms | 3623.25 ms | 6481.61 ms | **650369.00** | 
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 12.81 ms | **11.34** ms | 21.97 ms | 35.10 ms | 68.39 ms | **6701.67** | 
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 13.02 ms | **11.37** ms | 22.72 ms | 36.32 ms | 71.59 ms | **7026.33** | 
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 15.86 ms | **11.80** ms | 21.82 ms | 96.90 ms | 664.64 ms | **28352.00** | 
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 28.24 ms | **11.84** ms | 26.56 ms | 531.66 ms | 2049.27 ms | **100406.33** | 
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 13.50 ms | **11.96** ms | 22.42 ms | 35.11 ms | 77.25 ms | **6561.00** | 
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 17.54 ms | **12.62** ms | 37.60 ms | 74.69 ms | 172.18 ms | **15846.33** | 
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 14.40 ms | **12.80** ms | 22.94 ms | 36.57 ms | 73.61 ms | **6577.67** | 
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 16.10 ms | **12.82** ms | 25.94 ms | 51.69 ms | 411.95 ms | **15235.33** | 
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 15.08 ms | **13.20** ms | 24.43 ms | 40.56 ms | 84.31 ms | **7391.67** | 
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 15.80 ms | **13.67** ms | 25.84 ms | 41.95 ms | 188.22 ms | **8146.67** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 50.99 ms | **14.30** ms | 96.36 ms | 558.91 ms | 986.36 ms | **111479.00** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 47.68 ms | **14.39** ms | 90.91 ms | 499.66 ms | 1091.10 ms | **101371.67** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 48.21 ms | **14.84** ms | 96.92 ms | 495.79 ms | 836.43 ms | **97093.33** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 47.60 ms | **14.90** ms | 91.61 ms | 495.26 ms | 1006.80 ms | **98646.33** | 
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 17.52 ms | **14.91** ms | 25.11 ms | 50.33 ms | 355.93 ms | **14013.00** | 
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 17.91 ms | **15.15** ms | 27.90 ms | 69.05 ms | 229.65 ms | **11699.00** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 51.48 ms | **15.23** ms | 102.98 ms | 533.27 ms | 1064.42 ms | **105752.67** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 22.04 ms | **15.37** ms | 45.40 ms | 80.97 ms | 207.73 ms | **17039.67** | 
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 19.32 ms | **15.85** ms | 31.02 ms | 79.97 ms | 362.01 ms | **14660.33** | 
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 19.53 ms | **15.99** ms | 32.06 ms | 79.28 ms | 203.01 ms | **13513.33** | 
| `nim` (`0.20`) | [jester](https://github.com/dom96/jester) (**0.4**) | 17.50 ms | **16.04** ms | 26.22 ms | 40.38 ms | 107.53 ms | **6846.00** | 
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 20.45 ms | **16.40** ms | 34.11 ms | 86.72 ms | 347.53 ms | **15872.33** | 
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 20.66 ms | **16.49** ms | 34.80 ms | 86.56 ms | 237.73 ms | **15154.67** | 
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 20.19 ms | **16.51** ms | 33.43 ms | 80.03 ms | 197.46 ms | **13707.67** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 17.91 ms | **16.77** ms | 22.29 ms | 46.60 ms | 323.93 ms | **12110.33** | 
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 19.21 ms | **16.85** ms | 29.21 ms | 63.04 ms | 238.21 ms | **10754.67** | 
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 21.64 ms | **16.88** ms | 38.02 ms | 92.56 ms | 225.70 ms | **16520.00** | 
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 20.99 ms | **16.94** ms | 34.56 ms | 88.24 ms | 234.80 ms | **15299.33** | 
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 19.44 ms | **17.06** ms | 29.30 ms | 62.45 ms | 216.45 ms | **10963.00** | 
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 18.82 ms | **17.42** ms | 18.51 ms | 28.12 ms | 732.61 ms | **25413.00** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 19.64 ms | **17.61** ms | 37.11 ms | 64.80 ms | 189.05 ms | **14000.00** | 
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.12**) | 24.40 ms | **18.77** ms | 43.69 ms | 107.81 ms | 271.93 ms | **19458.67** | 
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 25.82 ms | **19.76** ms | 35.51 ms | 118.31 ms | 735.03 ms | **33205.33** | 
| `go` (`1.13`) | [gf](https://goframe.org) (**1.8**) | 24.09 ms | **19.78** ms | 39.91 ms | 95.19 ms | 242.98 ms | **16513.33** | 
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 39.02 ms | **20.99** ms | 36.59 ms | 737.32 ms | 2051.83 ms | **135107.67** | 
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 41.57 ms | **21.44** ms | 46.72 ms | 622.98 ms | 1523.56 ms | **105201.00** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 29.52 ms | **23.11** ms | 49.17 ms | 98.27 ms | 477.48 ms | **21214.67** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.0**) | 99.45 ms | **23.25** ms | 144.50 ms | 1275.29 ms | 1868.34 ms | **247927.33** | 
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 24.59 ms | **24.06** ms | 30.40 ms | 38.85 ms | 75.68 ms | **4737.33** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 40.45 ms | **27.93** ms | 86.00 ms | 134.18 ms | 300.09 ms | **30708.33** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 34.87 ms | **31.19** ms | 57.89 ms | 87.41 ms | 155.73 ms | **17046.67** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 49.79 ms | **33.37** ms | 97.64 ms | 156.83 ms | 343.24 ms | **35112.33** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 44.73 ms | **37.30** ms | 88.21 ms | 166.97 ms | 878.64 ms | **40623.67** | 
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 38.58 ms | **38.43** ms | 42.60 ms | 48.25 ms | 449.25 ms | **13415.67** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 43.71 ms | **42.07** ms | 54.62 ms | 76.84 ms | 149.78 ms | **9834.33** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 47.61 ms | **43.38** ms | 76.53 ms | 110.69 ms | 214.93 ms | **21168.00** | 
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | 52.90 ms | **46.23** ms | 49.78 ms | 290.30 ms | 1730.50 ms | **76363.00** | 
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | 50.97 ms | **48.55** ms | 78.92 ms | 107.17 ms | 230.61 ms | **20670.33** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 55.63 ms | **51.81** ms | 86.49 ms | 130.42 ms | 300.50 ms | **24312.67** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 77.60 ms | **58.22** ms | 145.52 ms | 235.19 ms | 528.41 ms | **49723.67** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 99.94 ms | **78.97** ms | 160.07 ms | 262.66 ms | 579.74 ms | **52354.00** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 94.28 ms | **80.89** ms | 168.00 ms | 279.63 ms | 424.05 ms | **54776.33** | 
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 85.56 ms | **84.31** ms | 97.90 ms | 125.75 ms | 678.20 ms | **29523.67** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 104.29 ms | **91.12** ms | 177.14 ms | 308.85 ms | 446.69 ms | **57028.33** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 93.96 ms | **92.13** ms | 133.39 ms | 166.12 ms | 276.58 ms | **30702.67** | 
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 93.58 ms | **92.76** ms | 114.12 ms | 119.11 ms | 159.70 ms | **13799.00** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | 140.24 ms | **130.57** ms | 216.70 ms | 296.56 ms | 473.65 ms | **52990.67** | 
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | 321.27 ms | **155.69** ms | 584.79 ms | 2916.55 ms | 5080.13 ms | **542258.33** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 221.08 ms | **162.27** ms | 419.31 ms | 639.16 ms | 1362.92 ms | **139865.67** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 226.76 ms | **225.00** ms | 309.40 ms | 366.33 ms | 469.59 ms | **63138.67** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 327.82 ms | **235.12** ms | 698.40 ms | 952.97 ms | 1902.94 ms | **219142.33** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 242.86 ms | **243.24** ms | 330.39 ms | 426.40 ms | 731.15 ms | **75071.00** | 
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 703.66 ms | **701.50** ms | 816.26 ms | 925.28 ms | 1248.77 ms | **93199.33** | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (sifrr) (node)


:three: (kore) (c)


:four: (agoo) (ruby)


:five: (rapidoid) (java)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 365088.67 | **211.26** MB |
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 329425.33 | **290.13** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 254935.00 | **662.65** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.1**) | 192326.33 | **111.16** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 121558.00 | **218.86** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 120835.67 | **144.51** MB |
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 119019.00 | **178.43** MB |
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 116753.67 | **174.97** MB |
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 113543.67 | **170.18** MB |
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 110342.67 | **165.43** MB |
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 109545.00 | **164.30** MB |
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 98929.00 | **254.59** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 97990.00 | **169.00** MB |
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 97242.33 | **91.43** MB |
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 94337.67 | **198.43** MB |
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 90486.33 | **87.85** MB |
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 87336.67 | **175.77** MB |
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 86098.67 | **138.57** MB |
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 84171.33 | **135.32** MB |
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 80225.00 | **169.91** MB |
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 79497.00 | **323.26** MB |
| `crystal` (`0.29`) | [toro](https://github.com/soveran/toro) (**0.4**) | 77677.67 | **73.11** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 77552.67 | **75.27** MB |
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 76339.67 | **71.82** MB |
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 76183.33 | **156.02** MB |
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 74975.33 | **112.41** MB |
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 74347.00 | **69.94** MB |
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 73325.00 | **78.02** MB |
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | 71238.33 | **127.15** MB |
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 70303.67 | **114.96** MB |
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 68396.33 | **117.19** MB |
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 66257.33 | **121.38** MB |
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 65562.67 | **160.72** MB |
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 64941.00 | **138.51** MB |
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 63979.67 | **104.56** MB |
| `nim` (`0.20`) | [jester](https://github.com/dom96/jester) (**0.4**) | 63263.00 | **127.18** MB |
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 60819.33 | **155.34** MB |
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 59856.67 | **34.62** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.23**) | 58679.33 | **56.09** MB |
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 57753.67 | **101.42** MB |
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 56487.33 | **75.46** MB |
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 55853.33 | **103.55** MB |
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 53413.67 | **71.42** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 53335.33 | **86.93** MB |
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 53142.00 | **71.15** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 53078.67 | **263.95** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 52523.33 | **261.27** MB |
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 52520.67 | **66.27** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 52253.67 | **81.51** MB |
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 51719.33 | **78.35** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 51561.33 | **60.95** MB |
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 51524.67 | **68.62** MB |
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 51083.67 | **67.85** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 51053.00 | **265.84** MB |
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 50996.67 | **89.58** MB |
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 50861.67 | **89.34** MB |
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 50502.33 | **88.71** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 49911.67 | **248.17** MB |
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 49784.33 | **66.75** MB |
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 49304.67 | **65.81** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 48461.00 | **113.62** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 48424.67 | **240.82** MB |
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.12**) | 44044.00 | **91.95** MB |
| `go` (`1.13`) | [gf](https://goframe.org) (**1.8**) | 43030.33 | **65.17** MB |
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 42776.00 | **80.85** MB |
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 42499.67 | **73.19** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 41698.00 | **24.10** MB |
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 41235.00 | **82.04** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 39398.33 | **29.26** MB |
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 38670.00 | **100.81** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 35487.00 | **76.76** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.0**) | 32874.67 | **171.73** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 29272.67 | **16.93** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 28647.33 | **57.65** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 26948.00 | **66.40** MB |
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 26693.33 | **25.05** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 24049.33 | **42.19** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 22707.00 | **59.76** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 22557.00 | **170.97** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 21139.00 | **52.41** MB |
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | 20975.67 | **37.66** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 20747.00 | **44.72** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 20667.33 | **53.75** MB |
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | 19486.00 | **44.77** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 18259.33 | **39.51** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 13846.00 | **25.75** MB |
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 13360.33 | **17.10** MB |
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 11327.00 | **13.90** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 10880.33 | **21.06** MB |
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 10482.00 | **13.17** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 10452.67 | **23.71** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 10132.00 | **24.97** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 9659.00 | **17.25** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | 7007.00 | **14.00** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 5378.33 | **33.85** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 4643.00 | **13.48** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 4303.33 | **9.40** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 3964.33 | **11.72** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 3181.00 | **7.85** MB |
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 1365.33 | **3.53** MB |
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | 461.33 | **1.05** MB |
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

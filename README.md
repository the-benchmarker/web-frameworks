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
Last update: 2019-09-17
```
OS: Linux (version: 5.2.13-200.fc30.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: rack-routing (ruby)


:four: hanami (ruby)


:five: roda (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 0.10 ms | **0.10** ms | 0.14 ms | 0.18 ms | 4.46 ms | **52.00** | 
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 0.48 ms | **0.43** ms | 0.87 ms | 1.52 ms | 15.99 ms | **356.00** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 3.73 ms | **0.53** ms | 12.53 ms | 30.99 ms | 85.07 ms | **6802.67** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 6.12 ms | **0.56** ms | 20.95 ms | 46.44 ms | 114.42 ms | **10608.00** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.24**) | 2.64 ms | **0.64** ms | 7.53 ms | 16.04 ms | 47.13 ms | **3662.67** | 
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 2.58 ms | **0.68** ms | 7.30 ms | 16.32 ms | 57.93 ms | **3691.67** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 3.13 ms | **0.69** ms | 9.25 ms | 20.57 ms | 70.03 ms | **4643.00** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 7.85 ms | **1.11** ms | 24.42 ms | 53.36 ms | 126.70 ms | **12191.67** | 
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 5.86 ms | **1.12** ms | 14.87 ms | 16.05 ms | 2539.40 ms | **52663.67** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 5.65 ms | **1.42** ms | 16.01 ms | 36.80 ms | 120.61 ms | **8197.67** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 34.61 ms | **3.26** ms | 111.20 ms | 293.64 ms | 809.57 ms | **62371.33** | 
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 4.12 ms | **3.66** ms | 8.02 ms | 14.00 ms | 43.75 ms | **3032.00** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 4.37 ms | **3.76** ms | 9.38 ms | 15.55 ms | 36.01 ms | **3687.33** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 4.55 ms | **4.13** ms | 8.93 ms | 14.94 ms | 33.86 ms | **3301.00** | 
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 4.78 ms | **4.38** ms | 8.54 ms | 14.19 ms | 31.02 ms | **2817.67** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 5.49 ms | **4.50** ms | 10.38 ms | 21.54 ms | 82.10 ms | **4451.00** | 
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 5.13 ms | **4.65** ms | 8.04 ms | 14.46 ms | 97.30 ms | **2588.67** | 
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 5.22 ms | **4.72** ms | 8.10 ms | 14.69 ms | 101.74 ms | **3308.33** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 5.28 ms | **4.78** ms | 8.80 ms | 14.64 ms | 35.09 ms | **2721.67** | 
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 5.50 ms | **4.90** ms | 8.88 ms | 15.63 ms | 44.93 ms | **2825.33** | 
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 5.76 ms | **4.96** ms | 9.82 ms | 15.97 ms | 41.95 ms | **3104.33** | 
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 5.99 ms | **5.15** ms | 10.16 ms | 16.70 ms | 41.54 ms | **3237.67** | 
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 6.14 ms | **5.15** ms | 10.77 ms | 18.08 ms | 47.40 ms | **3588.00** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 12.62 ms | **5.30** ms | 10.68 ms | 254.85 ms | 1187.27 ms | **65811.67** | 
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 6.41 ms | **5.50** ms | 10.80 ms | 17.84 ms | 37.40 ms | **3445.33** | 
| `crystal` (`0.29`) | [toro](https://github.com/soveran/toro) (**0.4**) | 6.30 ms | **5.50** ms | 10.90 ms | 18.07 ms | 44.65 ms | **3673.67** | 
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 6.57 ms | **5.51** ms | 10.76 ms | 18.27 ms | 214.29 ms | **5245.33** | 
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 6.92 ms | **6.07** ms | 11.10 ms | 18.78 ms | 42.83 ms | **3463.67** | 
| `nim` (`0.20`) | [jester](https://github.com/dom96/jester) (**0.4**) | 6.94 ms | **6.37** ms | 10.49 ms | 16.42 ms | 44.24 ms | **2928.67** | 
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 7.58 ms | **6.67** ms | 12.36 ms | 21.37 ms | 49.36 ms | **3944.33** | 
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 8.46 ms | **7.03** ms | 13.50 ms | 32.13 ms | 285.62 ms | **7580.67** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.11**) | 9.70 ms | **7.10** ms | 16.06 ms | 80.61 ms | 123.28 ms | **12069.00** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 8.05 ms | **7.19** ms | 12.09 ms | 20.29 ms | 196.26 ms | **5865.33** | 
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 8.95 ms | **7.49** ms | 14.85 ms | 33.03 ms | 173.54 ms | **6468.00** | 
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 9.17 ms | **7.60** ms | 16.15 ms | 30.53 ms | 74.22 ms | **5635.67** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 214.33 ms | **7.65** ms | 29.09 ms | 5057.46 ms | 7933.95 ms | **865932.67** | 
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 9.32 ms | **7.73** ms | 15.72 ms | 34.59 ms | 179.15 ms | **6774.00** | 
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 9.47 ms | **7.82** ms | 16.05 ms | 36.36 ms | 180.52 ms | **6881.00** | 
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 9.19 ms | **7.97** ms | 14.68 ms | 31.54 ms | 164.12 ms | **5972.00** | 
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 9.66 ms | **7.97** ms | 16.08 ms | 36.63 ms | 288.45 ms | **7967.00** | 
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 9.71 ms | **8.02** ms | 16.70 ms | 35.89 ms | 127.80 ms | **6631.33** | 
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 10.57 ms | **8.51** ms | 18.70 ms | 41.09 ms | 138.55 ms | **7521.67** | 
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 10.91 ms | **8.57** ms | 19.66 ms | 44.39 ms | 221.36 ms | **8573.33** | 
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 9.97 ms | **8.68** ms | 15.31 ms | 34.13 ms | 86.05 ms | **5695.67** | 
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.13**) | 11.34 ms | **8.91** ms | 19.92 ms | 45.96 ms | 246.53 ms | **10546.67** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 10.83 ms | **8.97** ms | 13.67 ms | 25.50 ms | 643.60 ms | **21489.00** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 12.61 ms | **9.37** ms | 26.52 ms | 51.34 ms | 176.22 ms | **10669.00** | 
| `go` (`1.13`) | [gf](https://goframe.org) (**1.8**) | 11.50 ms | **9.78** ms | 18.51 ms | 41.27 ms | 191.77 ms | **7260.00** | 
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 13.49 ms | **9.79** ms | 17.97 ms | 80.97 ms | 592.19 ms | **25835.33** | 
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 13.87 ms | **9.86** ms | 19.65 ms | 83.30 ms | 589.54 ms | **26751.33** | 
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 13.66 ms | **9.95** ms | 19.59 ms | 71.25 ms | 574.64 ms | **23837.33** | 
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 10.46 ms | **10.13** ms | 13.92 ms | 17.93 ms | 87.87 ms | **2704.33** | 
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | 26.30 ms | **10.34** ms | 56.90 ms | 298.14 ms | 651.64 ms | **53478.67** | 
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 15.39 ms | **10.86** ms | 20.00 ms | 116.08 ms | 702.74 ms | **32975.00** | 
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 20.55 ms | **12.20** ms | 24.55 ms | 275.95 ms | 904.24 ms | **51948.33** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 14.40 ms | **12.23** ms | 26.48 ms | 55.87 ms | 337.68 ms | **15869.33** | 
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 19.83 ms | **12.35** ms | 22.43 ms | 263.10 ms | 909.43 ms | **51521.00** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 17.21 ms | **12.88** ms | 34.55 ms | 63.12 ms | 174.72 ms | **13098.67** | 
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 16.62 ms | **13.38** ms | 34.20 ms | 66.26 ms | 150.51 ms | **13858.00** | 
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 18.63 ms | **13.63** ms | 24.06 ms | 145.27 ms | 794.96 ms | **38311.67** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 17.94 ms | **13.67** ms | 25.45 ms | 84.07 ms | 603.11 ms | **24307.33** | 
| `php` (`7.3`) | [basicphp](https://github.com/ray-ang/basicphp) (**0.9**) | 39.12 ms | **13.69** ms | 72.16 ms | 390.62 ms | 751.72 ms | **77721.33** | 
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 21.13 ms | **14.19** ms | 25.83 ms | 220.25 ms | 898.85 ms | **47304.33** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 42.45 ms | **14.27** ms | 82.93 ms | 432.64 ms | 884.08 ms | **86605.00** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 20.87 ms | **14.34** ms | 47.59 ms | 72.54 ms | 165.71 ms | **15989.33** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 44.96 ms | **14.50** ms | 85.50 ms | 465.24 ms | 979.22 ms | **94506.33** | 
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 23.65 ms | **14.69** ms | 27.98 ms | 298.11 ms | 1027.86 ms | **57411.33** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 43.73 ms | **14.84** ms | 80.35 ms | 455.51 ms | 747.23 ms | **90335.00** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 47.95 ms | **14.90** ms | 98.18 ms | 509.07 ms | 815.69 ms | **100623.00** | 
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 26.23 ms | **15.05** ms | 27.27 ms | 439.57 ms | 1198.50 ms | **75233.00** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**6.0**) | 48.88 ms | **16.12** ms | 90.36 ms | 500.27 ms | 870.67 ms | **98180.67** | 
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 24.11 ms | **16.60** ms | 29.59 ms | 255.83 ms | 1018.66 ms | **53748.33** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 17.56 ms | **16.60** ms | 25.63 ms | 42.03 ms | 127.47 ms | **7849.00** | 
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | 18.27 ms | **17.90** ms | 18.46 ms | 23.67 ms | 517.82 ms | **12937.33** | 
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 26.55 ms | **18.11** ms | 33.02 ms | 254.19 ms | 1418.17 ms | **64817.00** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 26.62 ms | **18.35** ms | 35.00 ms | 288.41 ms | 1451.93 ms | **73601.67** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 19.49 ms | **18.55** ms | 29.37 ms | 39.48 ms | 144.10 ms | **7707.33** | 
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 23.40 ms | **20.17** ms | 26.02 ms | 94.58 ms | 572.77 ms | **23898.00** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.0**) | 77.95 ms | **20.43** ms | 149.45 ms | 921.37 ms | 1481.89 ms | **181788.67** | 
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 20.69 ms | **20.85** ms | 23.28 ms | 25.76 ms | 214.79 ms | **3322.33** | 
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 24.15 ms | **21.64** ms | 25.05 ms | 66.82 ms | 1039.38 ms | **36760.67** | 
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 45.89 ms | **25.33** ms | 42.23 ms | 748.00 ms | 1716.40 ms | **119988.33** | 
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 48.16 ms | **25.48** ms | 42.47 ms | 859.87 ms | 1838.01 ms | **135511.00** | 
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | 30.53 ms | **28.84** ms | 45.69 ms | 66.54 ms | 194.62 ms | **11630.67** | 
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 55.23 ms | **28.85** ms | 46.72 ms | 1004.36 ms | 3078.70 ms | **185584.33** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 30.17 ms | **30.09** ms | 35.04 ms | 41.62 ms | 64.24 ms | **4206.33** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 32.48 ms | **30.48** ms | 49.61 ms | 71.84 ms | 307.90 ms | **13558.00** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 38.33 ms | **31.40** ms | 69.30 ms | 97.59 ms | 358.10 ms | **20153.00** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 46.34 ms | **33.54** ms | 105.86 ms | 117.98 ms | 334.67 ms | **28540.00** | 
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 39.39 ms | **38.20** ms | 48.79 ms | 56.25 ms | 149.79 ms | **6726.00** | 
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 44.59 ms | **41.87** ms | 48.84 ms | 113.23 ms | 643.90 ms | **25649.00** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 48.66 ms | **43.58** ms | 75.95 ms | 116.07 ms | 177.44 ms | **20259.67** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 45.19 ms | **44.07** ms | 71.87 ms | 96.85 ms | 213.21 ms | **20247.33** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 45.76 ms | **44.86** ms | 68.86 ms | 115.34 ms | 202.14 ms | **20726.33** | 
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 53.41 ms | **53.82** ms | 60.72 ms | 63.10 ms | 85.46 ms | **6163.00** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | 89.73 ms | **82.13** ms | 142.33 ms | 182.50 ms | 346.53 ms | **34951.00** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 107.21 ms | **84.55** ms | 201.45 ms | 248.56 ms | 947.19 ms | **57591.00** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 95.27 ms | **91.23** ms | 123.86 ms | 154.71 ms | 309.71 ms | **23214.33** | 
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | 250.89 ms | **93.80** ms | 545.22 ms | 2477.00 ms | 4852.74 ms | **487723.00** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 100.51 ms | **97.56** ms | 131.62 ms | 198.89 ms | 720.76 ms | **32392.67** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 138.83 ms | **121.96** ms | 215.04 ms | 343.29 ms | 891.90 ms | **59037.00** | 
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 205.08 ms | **206.67** ms | 235.47 ms | 255.19 ms | 294.20 ms | **25072.33** | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (sifrr) (node)


:three: (japronto) (python)


:four: (drogon) (cpp)


:five: (atreugo) (go)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 227488.33 | **131.60** MB |
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 213360.33 | **187.89** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 205134.00 | **245.46** MB |
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 191397.33 | **185.82** MB |
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 175276.33 | **352.87** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 174623.33 | **314.29** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 173050.00 | **168.03** MB |
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 172280.00 | **276.95** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 171666.67 | **446.41** MB |
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 164657.00 | **264.88** MB |
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 164189.33 | **154.44** MB |
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 158730.00 | **149.31** MB |
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 156697.00 | **167.08** MB |
| `crystal` (`0.29`) | [toro](https://github.com/soveran/toro) (**0.4**) | 150010.67 | **141.05** MB |
| `nim` (`0.20`) | [jester](https://github.com/dom96/jester) (**0.4**) | 149505.67 | **300.60** MB |
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 148808.00 | **243.28** MB |
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 145852.33 | **298.59** MB |
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 138230.33 | **253.18** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 135303.67 | **233.35** MB |
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 127681.00 | **208.75** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.11**) | 121810.33 | **70.40** MB |
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 117695.67 | **157.70** MB |
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 116502.00 | **147.22** MB |
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 112343.67 | **166.64** MB |
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 111760.67 | **149.67** MB |
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 108003.67 | **143.65** MB |
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 107738.67 | **189.23** MB |
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 107294.00 | **273.72** MB |
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 106580.00 | **143.32** MB |
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 105511.33 | **141.93** MB |
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 104489.67 | **139.55** MB |
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 101005.00 | **134.49** MB |
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 97946.33 | **171.99** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 97139.33 | **157.97** MB |
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 95565.00 | **167.72** MB |
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.13**) | 93303.00 | **194.67** MB |
| `go` (`1.13`) | [gf](https://goframe.org) (**1.8**) | 89073.67 | **134.90** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 86423.67 | **202.65** MB |
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 86396.00 | **129.49** MB |
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 85199.33 | **127.73** MB |
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 84532.00 | **168.16** MB |
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 83708.67 | **125.46** MB |
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 78992.00 | **118.27** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 76914.33 | **166.24** MB |
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | 76745.33 | **136.93** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 74719.67 | **116.39** MB |
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 70531.67 | **105.73** MB |
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 68254.33 | **102.30** MB |
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 66450.00 | **141.72** MB |
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 65240.00 | **137.05** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 65102.67 | **49.25** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 62497.33 | **153.94** MB |
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 60829.33 | **128.85** MB |
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 60793.33 | **162.15** MB |
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 59451.67 | **241.71** MB |
| `php` (`7.3`) | [basicphp](https://github.com/ray-ang/basicphp) (**0.9**) | 58192.33 | **289.19** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 56839.33 | **114.27** MB |
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | 56127.00 | **100.65** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 55250.00 | **274.60** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 54483.67 | **270.80** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 54248.33 | **269.54** MB |
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 53563.67 | **131.25** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 52502.67 | **130.19** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 52351.00 | **260.16** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 50943.00 | **89.37** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 50408.67 | **108.67** MB |
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 49196.00 | **28.44** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.24**) | 47792.67 | **45.67** MB |
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 47780.33 | **44.92** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**6.0**) | 47728.67 | **248.37** MB |
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 47660.00 | **81.47** MB |
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 45807.00 | **80.42** MB |
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 45329.67 | **84.31** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 40794.00 | **48.18** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.0**) | 38278.00 | **200.00** MB |
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 35821.33 | **92.93** MB |
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 34340.33 | **58.80** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 34304.33 | **19.81** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 32524.67 | **85.47** MB |
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | 32298.00 | **74.26** MB |
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 31646.67 | **59.88** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 30691.33 | **66.27** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 26654.67 | **49.55** MB |
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 25129.33 | **30.92** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 22713.33 | **56.01** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 22625.33 | **13.07** MB |
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 22572.00 | **21.17** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 22128.67 | **50.21** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 22036.67 | **39.27** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 20969.67 | **158.54** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 20581.00 | **39.74** MB |
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 18610.33 | **23.35** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 16338.33 | **42.40** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | 11017.33 | **21.96** MB |
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 10740.67 | **13.76** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 10347.00 | **22.55** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 9663.33 | **28.64** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 9539.33 | **27.66** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 7031.00 | **17.34** MB |
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 4821.67 | **12.44** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 3686.00 | **23.20** MB |
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | 664.33 | **1.51** MB |
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

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
Last update: 2019-09-05
```
OS: Linux (version: 5.2.11-200.fc30.x86_64, arch: x86_64)
CPU Cores: 12
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: swifter (swift)


:four: syro (ruby)


:five: agoo-c (c)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 0.07 ms | **0.06** ms | 0.11 ms | 0.13 ms | 3.37 ms | **43.67** | 
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 0.43 ms | **0.33** ms | 0.81 ms | 1.81 ms | 17.57 ms | **414.33** | 
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 4.02 ms | **0.92** ms | 14.67 ms | 15.53 ms | 1533.31 ms | **20384.00** | 
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 3.12 ms | **1.72** ms | 7.98 ms | 15.46 ms | 44.24 ms | **3616.33** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 2.78 ms | **1.83** ms | 6.24 ms | 12.98 ms | 39.61 ms | **2757.33** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.23**) | 3.26 ms | **1.84** ms | 8.26 ms | 15.82 ms | 42.96 ms | **3717.67** | 
| `node` (`12.9`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 3.17 ms | **1.93** ms | 7.43 ms | 16.66 ms | 44.31 ms | **3582.00** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 3.39 ms | **2.38** ms | 8.22 ms | 15.23 ms | 39.46 ms | **3608.00** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 3.90 ms | **2.40** ms | 9.13 ms | 19.86 ms | 45.92 ms | **4283.67** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 4.07 ms | **2.62** ms | 9.44 ms | 20.72 ms | 155.08 ms | **5165.33** | 
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 3.61 ms | **2.81** ms | 6.96 ms | 16.06 ms | 129.13 ms | **3651.00** | 
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 3.60 ms | **2.91** ms | 7.09 ms | 14.14 ms | 32.62 ms | **2949.00** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 12.85 ms | **2.94** ms | 7.76 ms | 370.95 ms | 1275.72 ms | **78826.00** | 
| `go` (`1.12`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 4.03 ms | **3.00** ms | 7.47 ms | 16.70 ms | 106.29 ms | **3783.00** | 
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 4.41 ms | **3.05** ms | 10.19 ms | 21.06 ms | 49.35 ms | **4556.67** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 4.44 ms | **3.09** ms | 11.03 ms | 21.30 ms | 55.37 ms | **4971.33** | 
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 4.15 ms | **3.18** ms | 8.40 ms | 16.41 ms | 33.10 ms | **3374.67** | 
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 3.75 ms | **3.44** ms | 6.49 ms | 12.94 ms | 30.07 ms | **2614.33** | 
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 4.09 ms | **3.46** ms | 7.99 ms | 15.39 ms | 39.74 ms | **3198.00** | 
| `go` (`1.12`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 4.35 ms | **3.48** ms | 7.41 ms | 16.22 ms | 109.18 ms | **3979.00** | 
| `go` (`1.12`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 4.49 ms | **3.70** ms | 7.55 ms | 16.86 ms | 113.94 ms | **3984.67** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 4.64 ms | **3.88** ms | 9.12 ms | 19.61 ms | 97.71 ms | **4021.00** | 
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 4.74 ms | **4.06** ms | 9.16 ms | 17.84 ms | 38.33 ms | **3551.33** | 
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 4.61 ms | **4.25** ms | 8.07 ms | 16.81 ms | 42.20 ms | **3161.67** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 5.59 ms | **4.26** ms | 10.64 ms | 19.70 ms | 183.99 ms | **4855.00** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 6.34 ms | **4.38** ms | 15.78 ms | 30.42 ms | 72.69 ms | **7029.67** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.1**) | 6.43 ms | **4.39** ms | 10.33 ms | 67.14 ms | 113.08 ms | **10630.00** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 6.46 ms | **4.45** ms | 7.31 ms | 92.37 ms | 313.34 ms | **16172.00** | 
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 5.16 ms | **4.70** ms | 8.62 ms | 17.62 ms | 65.83 ms | **3323.00** | 
| `go` (`1.12`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 6.90 ms | **5.04** ms | 12.70 ms | 31.50 ms | 358.97 ms | **10246.00** | 
| `go` (`1.12`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 6.83 ms | **5.28** ms | 13.43 ms | 28.93 ms | 165.66 ms | **6161.33** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 8.58 ms | **5.34** ms | 17.98 ms | 37.83 ms | 266.05 ms | **9050.33** | 
| `node` (`12.9`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 6.98 ms | **5.39** ms | 11.10 ms | 23.05 ms | 276.20 ms | **7948.67** | 
| `go` (`1.12`) | [echo](https://echo.labstack.com) (**4.1**) | 7.67 ms | **5.44** ms | 15.31 ms | 36.41 ms | 272.78 ms | **8388.67** | 
| `go` (`1.12`) | [kami](https://github.com/guregu/kami) (**2.2**) | 7.14 ms | **5.44** ms | 13.31 ms | 30.56 ms | 169.10 ms | **6338.33** | 
| `go` (`1.12`) | [violetear](https://violetear.org) (**7.0**) | 7.48 ms | **5.48** ms | 14.20 ms | 34.51 ms | 240.76 ms | **8087.67** | 
| `node` (`12.9`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 7.08 ms | **5.50** ms | 11.06 ms | 23.29 ms | 275.55 ms | **7772.00** | 
| `node` (`12.9`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 7.05 ms | **5.54** ms | 11.48 ms | 23.00 ms | 243.52 ms | **6775.00** | 
| `go` (`1.12`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 8.21 ms | **5.54** ms | 16.66 ms | 40.16 ms | 369.97 ms | **10986.33** | 
| `go` (`1.12`) | [goroute](https://goroute.github.io) (**0.0**) | 7.74 ms | **5.56** ms | 15.30 ms | 35.87 ms | 237.57 ms | **8418.33** | 
| `go` (`1.12`) | [gin](https://gin-gonic.com) (**1.4**) | 8.00 ms | **5.65** ms | 15.62 ms | 37.26 ms | 286.85 ms | **9750.33** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 210.08 ms | **5.77** ms | 76.83 ms | 5173.30 ms | 7836.94 ms | **864149.67** | 
| `node` (`12.9`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 7.98 ms | **5.83** ms | 11.74 ms | 27.26 ms | 383.11 ms | **13611.00** | 
| `go` (`1.12`) | [beego](https://beego.me) (**1.12**) | 8.14 ms | **6.12** ms | 15.13 ms | 35.00 ms | 222.01 ms | **7490.00** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 7.86 ms | **6.12** ms | 18.83 ms | 35.12 ms | 86.29 ms | **8290.67** | 
| `node` (`12.9`) | [rayo](https://rayo.js.org) (**1.3**) | 7.73 ms | **6.12** ms | 12.29 ms | 24.84 ms | 240.69 ms | **8048.00** | 
| `go` (`1.12`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 8.31 ms | **6.15** ms | 15.00 ms | 36.98 ms | 356.44 ms | **9727.33** | 
| `go` (`1.12`) | [air](https://github.com/aofei/air) (**0.12**) | 9.04 ms | **6.16** ms | 17.94 ms | 43.46 ms | 285.72 ms | **11407.67** | 
| `go` (`1.12`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 8.65 ms | **6.48** ms | 15.55 ms | 37.14 ms | 262.62 ms | **9156.67** | 
| `go` (`1.12`) | [gf](https://goframe.org) (**1.8**) | 9.45 ms | **6.59** ms | 19.91 ms | 44.43 ms | 110.88 ms | **8843.00** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 9.31 ms | **6.80** ms | 22.29 ms | 43.29 ms | 95.54 ms | **9906.33** | 
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 7.30 ms | **7.15** ms | 9.34 ms | 12.12 ms | 78.30 ms | **1763.67** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 9.17 ms | **7.36** ms | 19.38 ms | 37.30 ms | 220.48 ms | **8783.67** | 
| `node` (`12.9`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 9.88 ms | **8.05** ms | 16.29 ms | 32.53 ms | 362.16 ms | **11102.33** | 
| `node` (`12.9`) | [fastify](https://fastify.io) (**2.8**) | 11.14 ms | **8.21** ms | 15.84 ms | 43.12 ms | 507.50 ms | **19981.67** | 
| `node` (`12.9`) | [foxify](https://foxify.js.org) (**0.1**) | 9.65 ms | **8.33** ms | 14.82 ms | 29.74 ms | 335.06 ms | **11323.33** | 
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 9.10 ms | **9.01** ms | 11.55 ms | 14.90 ms | 222.07 ms | **4658.33** | 
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 15.20 ms | **9.01** ms | 37.10 ms | 84.43 ms | 208.96 ms | **17817.67** | 
| `node` (`12.9`) | [koa](https://koajs.com) (**2.8**) | 11.89 ms | **9.11** ms | 17.26 ms | 62.81 ms | 475.71 ms | **19375.00** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 29.27 ms | **9.15** ms | 57.90 ms | 308.86 ms | 506.26 ms | **61424.33** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 32.48 ms | **9.23** ms | 65.05 ms | 354.93 ms | 546.94 ms | **70238.00** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 13.85 ms | **9.24** ms | 30.72 ms | 64.37 ms | 277.37 ms | **13724.33** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 12.63 ms | **9.72** ms | 20.98 ms | 41.09 ms | 405.63 ms | **14486.33** | 
| `node` (`12.9`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 11.74 ms | **9.75** ms | 18.09 ms | 36.42 ms | 412.30 ms | **12871.67** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 32.20 ms | **9.85** ms | 60.90 ms | 346.08 ms | 810.24 ms | **69654.33** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 30.78 ms | **9.87** ms | 60.51 ms | 323.19 ms | 573.67 ms | **63964.67** | 
| `node` (`12.9`) | [express](https://expressjs.com) (**4.17**) | 12.33 ms | **9.91** ms | 18.84 ms | 37.11 ms | 387.08 ms | **12921.33** | 
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 13.95 ms | **10.42** ms | 19.89 ms | 46.11 ms | 739.15 ms | **27190.33** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 33.28 ms | **10.43** ms | 67.34 ms | 348.18 ms | 578.43 ms | **68359.33** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 14.68 ms | **11.36** ms | 27.79 ms | 49.33 ms | 714.51 ms | **22531.67** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 54.90 ms | **11.52** ms | 90.67 ms | 695.11 ms | 1049.78 ms | **138740.00** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 16.41 ms | **11.99** ms | 31.89 ms | 61.17 ms | 302.51 ms | **13594.67** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 13.03 ms | **12.12** ms | 21.11 ms | 34.52 ms | 124.74 ms | **6818.00** | 
| `node` (`12.9`) | [restify](https://restify.com) (**8.4**) | 14.15 ms | **12.31** ms | 21.23 ms | 38.18 ms | 277.63 ms | **8724.00** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 14.34 ms | **13.04** ms | 22.29 ms | 37.92 ms | 156.03 ms | **6980.33** | 
| `node` (`12.9`) | [hapi](https://hapijs.com) (**18.1**) | 23.39 ms | **16.20** ms | 29.95 ms | 220.44 ms | 817.90 ms | **42965.67** | 
| `node` (`12.9`) | [moleculer](https://moleculer.services) (**0.13**) | 23.30 ms | **16.94** ms | 34.63 ms | 140.30 ms | 698.50 ms | **32098.00** | 
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 24.49 ms | **17.82** ms | 30.78 ms | 182.02 ms | 1537.47 ms | **61193.67** | 
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 19.89 ms | **17.84** ms | 19.14 ms | 27.18 ms | 870.98 ms | **32442.33** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 42.95 ms | **18.11** ms | 118.62 ms | 275.10 ms | 752.27 ms | **60330.67** | 
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 21.34 ms | **21.26** ms | 23.61 ms | 27.18 ms | 313.13 ms | **6008.67** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 23.62 ms | **21.48** ms | 37.55 ms | 60.71 ms | 183.15 ms | **11502.67** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 31.07 ms | **21.48** ms | 63.73 ms | 101.20 ms | 256.69 ms | **22310.33** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 23.90 ms | **21.92** ms | 32.98 ms | 56.87 ms | 149.78 ms | **8452.00** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 36.68 ms | **26.23** ms | 76.40 ms | 121.14 ms | 431.35 ms | **29374.00** | 
| `node` (`12.9`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 30.17 ms | **29.57** ms | 32.89 ms | 38.10 ms | 566.71 ms | **15730.67** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.5**) | 33.18 ms | **31.57** ms | 48.71 ms | 61.40 ms | 146.07 ms | **11433.67** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 40.22 ms | **35.62** ms | 71.71 ms | 116.77 ms | 249.61 ms | **23487.67** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 39.14 ms | **35.97** ms | 63.63 ms | 93.51 ms | 390.32 ms | **21546.00** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 85.22 ms | **55.15** ms | 185.25 ms | 273.00 ms | 553.59 ms | **64951.00** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.1**) | 65.50 ms | **59.58** ms | 106.69 ms | 153.11 ms | 273.58 ms | **29887.33** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 85.89 ms | **83.28** ms | 106.99 ms | 186.30 ms | 494.60 ms | **27785.67** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 88.37 ms | **83.87** ms | 120.62 ms | 170.90 ms | 484.56 ms | **30194.67** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 132.84 ms | **103.56** ms | 253.39 ms | 393.61 ms | 790.03 ms | **85157.00** | 
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 164.02 ms | **119.60** ms | 120.72 ms | 1842.88 ms | 2840.50 ms | **286954.33** | 
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 181.83 ms | **183.68** ms | 208.91 ms | 224.62 ms | 268.65 ms | **23172.00** | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (sifrr) (node)


:three: (japronto) (python)


:four: (rapidoid) (java)


:five: (gotham) (rust)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 369305.00 | **213.73** MB |
| `node` (`12.9`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 357717.00 | **314.86** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 287749.00 | **344.55** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 282892.00 | **509.17** MB |
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 279209.00 | **570.96** MB |
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 275568.67 | **259.04** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 264307.33 | **686.85** MB |
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 251582.00 | **244.28** MB |
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 251338.67 | **411.04** MB |
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 244061.00 | **259.20** MB |
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 239986.67 | **225.60** MB |
| `go` (`1.12`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 234913.00 | **472.55** MB |
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 225257.33 | **452.40** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 222877.00 | **384.51** MB |
| `go` (`1.12`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 215319.00 | **345.79** MB |
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 212408.67 | **346.87** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 211894.67 | **205.81** MB |
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 210705.67 | **385.44** MB |
| `go` (`1.12`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 206822.33 | **331.81** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.1**) | 200891.67 | **115.88** MB |
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 196028.00 | **246.73** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 193000.00 | **314.21** MB |
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 176336.67 | **267.24** MB |
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 175870.00 | **349.70** MB |
| `go` (`1.12`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 156453.67 | **209.06** MB |
| `node` (`12.9`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 144654.33 | **216.89** MB |
| `node` (`12.9`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 143189.00 | **214.64** MB |
| `go` (`1.12`) | [kami](https://github.com/guregu/kami) (**2.2**) | 142942.00 | **189.97** MB |
| `go` (`1.12`) | [violetear](https://violetear.org) (**7.0**) | 142916.33 | **189.54** MB |
| `node` (`12.9`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 142572.67 | **213.76** MB |
| `go` (`1.12`) | [echo](https://echo.labstack.com) (**4.1**) | 141630.33 | **248.49** MB |
| `go` (`1.12`) | [goroute](https://goroute.github.io) (**0.0**) | 140614.00 | **246.68** MB |
| `go` (`1.12`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 137961.00 | **184.43** MB |
| `node` (`12.9`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 137391.33 | **205.96** MB |
| `go` (`1.12`) | [gin](https://gin-gonic.com) (**1.4**) | 136019.00 | **238.65** MB |
| `node` (`12.9`) | [rayo](https://rayo.js.org) (**1.3**) | 132040.33 | **197.94** MB |
| `go` (`1.12`) | [beego](https://beego.me) (**1.12**) | 129646.33 | **173.71** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 128930.00 | **302.27** MB |
| `go` (`1.12`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 126737.67 | **168.63** MB |
| `go` (`1.12`) | [air](https://github.com/aofei/air) (**0.12**) | 124526.33 | **259.75** MB |
| `go` (`1.12`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 122073.33 | **163.09** MB |
| `go` (`1.12`) | [gf](https://goframe.org) (**1.8**) | 119601.67 | **181.14** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 119059.33 | **185.60** MB |
| `node` (`12.9`) | [fastify](https://fastify.io) (**2.8**) | 110300.67 | **282.47** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 108868.67 | **235.08** MB |
| `node` (`12.9`) | [foxify](https://foxify.js.org) (**0.1**) | 108019.67 | **227.23** MB |
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 107893.00 | **101.50** MB |
| `node` (`12.9`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 105859.67 | **158.65** MB |
| `go` (`1.12`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 101637.67 | **343.85** MB |
| `node` (`12.9`) | [koa](https://koajs.com) (**2.8**) | 93749.67 | **198.56** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 91398.67 | **68.46** MB |
| `node` (`12.9`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 90371.00 | **367.44** MB |
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 87811.67 | **187.36** MB |
| `node` (`12.9`) | [express](https://expressjs.com) (**4.17**) | 84319.67 | **206.48** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 84114.67 | **207.39** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 82595.33 | **410.78** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 81030.33 | **402.87** MB |
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 80098.67 | **133.96** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 78857.67 | **392.07** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 77128.00 | **383.40** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 75952.33 | **152.84** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 73647.33 | **129.29** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 72166.33 | **375.73** MB |
| `node` (`12.9`) | [restify](https://restify.com) (**8.4**) | 71907.00 | **126.29** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 68501.00 | **147.74** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 66272.00 | **349.08** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 64772.00 | **160.68** MB |
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 60781.33 | **35.18** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.23**) | 58056.00 | **55.50** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 56429.67 | **66.67** MB |
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 54773.33 | **101.53** MB |
| `node` (`12.9`) | [hapi](https://hapijs.com) (**18.1**) | 54318.33 | **141.06** MB |
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 49877.67 | **94.39** MB |
| `node` (`12.9`) | [moleculer](https://moleculer.services) (**0.13**) | 48178.67 | **82.35** MB |
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 45927.33 | **56.28** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 43039.67 | **24.89** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 42222.33 | **91.22** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 40870.00 | **107.57** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 35499.67 | **66.06** MB |
| `node` (`12.9`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 32845.33 | **30.82** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 30198.67 | **17.45** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 29849.67 | **73.57** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.5**) | 29619.33 | **67.21** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 25195.33 | **48.57** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 25173.33 | **44.97** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 24520.00 | **185.90** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 20749.33 | **53.94** MB |
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 17782.00 | **22.80** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.1**) | 15128.00 | **30.22** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 13017.33 | **37.81** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 11301.00 | **33.42** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 11036.33 | **24.09** MB |
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 8272.67 | **10.32** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 7884.33 | **19.38** MB |
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 5386.33 | **13.91** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 4478.33 | **28.22** MB |
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

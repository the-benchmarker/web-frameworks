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
Last update: 2019-07-03
```
OS: Linux (version: 5.1.15-300.fc30.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: cuba (ruby)


:four: rack-routing (ruby)


:five: laravel (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `rust` (`1.35`) | [nickel](https://nickel-org.github.io) (**0.11**) | 0.08 ms | **0.08** ms | 0.12 ms | 0.16 ms | 1.10 ms | **32.33** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.21**) | 4.62 ms | **0.27** ms | 16.35 ms | 36.31 ms | 87.62 ms | **8359.33** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 4.99 ms | **0.30** ms | 17.49 ms | 38.00 ms | 89.61 ms | **8817.33** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 6.11 ms | **0.36** ms | 21.01 ms | 43.41 ms | 100.73 ms | **10343.67** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 105.58 ms | **0.39** ms | 300.32 ms | 1875.73 ms | 6895.62 ms | **362517.00** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 201.43 ms | **0.41** ms | 304.41 ms | 4606.19 ms | 7545.95 ms | **755147.67** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 193.69 ms | **0.41** ms | 291.24 ms | 4327.03 ms | 7060.05 ms | **729037.33** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 173.45 ms | **0.41** ms | 267.82 ms | 4102.34 ms | 7098.02 ms | **675701.33** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**3.12**) | 228.06 ms | **0.42** ms | 326.56 ms | 5000.12 ms | 7553.30 ms | **840957.33** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 171.86 ms | **0.43** ms | 310.37 ms | 3561.18 ms | 7186.93 ms | **612533.67** | 
| `rust` (`1.35`) | [iron](https://ironframework.io) (**0.6**) | 0.48 ms | **0.46** ms | 0.79 ms | 1.22 ms | 17.99 ms | **284.00** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 8.47 ms | **0.58** ms | 26.81 ms | 52.66 ms | 115.30 ms | **12746.00** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 9.93 ms | **0.63** ms | 31.42 ms | 62.88 ms | 154.99 ms | **15202.33** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 10.72 ms | **0.81** ms | 32.09 ms | 62.33 ms | 140.57 ms | **15212.67** | 
| `rust` (`nightly`) | [rocket](https://rocket.rs) (**0.4**) | 83.24 ms | **1.85** ms | 24.00 ms | 2519.22 ms | 6547.44 ms | **442224.67** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 3.06 ms | **2.01** ms | 6.40 ms | 15.01 ms | 50.23 ms | **3227.33** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 3.12 ms | **2.09** ms | 6.67 ms | 14.98 ms | 58.47 ms | **3232.33** | 
| `rust` (`1.35`) | [actix-web](https://actix.rs) (**0.7**) | 3.21 ms | **2.48** ms | 6.30 ms | 13.44 ms | 36.06 ms | **2770.00** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 3.36 ms | **2.60** ms | 6.49 ms | 14.85 ms | 85.89 ms | **2960.00** | 
| `python` (`3.6`) | [vibora](https://vibora.io) (**0.0**) | 3.86 ms | **3.12** ms | 7.97 ms | 14.92 ms | 33.54 ms | **3238.33** | 
| `go` (`1.12`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 3.90 ms | **3.46** ms | 6.13 ms | 12.28 ms | 47.95 ms | **2425.33** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 3.40 ms | **3.49** ms | 5.20 ms | 7.87 ms | 20.33 ms | **1604.00** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.8**) | 8.29 ms | **3.96** ms | 13.54 ms | 91.95 ms | 136.02 ms | **15933.33** | 
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.4**) | 4.47 ms | **4.14** ms | 7.72 ms | 14.34 ms | 30.44 ms | **2701.00** | 
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 4.20 ms | **4.14** ms | 6.73 ms | 13.05 ms | 30.28 ms | **2435.67** | 
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 4.48 ms | **4.35** ms | 7.00 ms | 12.49 ms | 33.76 ms | **2336.00** | 
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.25**) | 4.67 ms | **4.55** ms | 7.27 ms | 12.78 ms | 36.49 ms | **2389.67** | 
| `rust` (`1.35`) | [gotham](https://gotham.rs) (**0.3**) | 5.06 ms | **4.56** ms | 9.01 ms | 19.76 ms | 229.10 ms | **5851.00** | 
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 5.37 ms | **4.78** ms | 8.60 ms | 13.17 ms | 60.21 ms | **2550.67** | 
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.28**) | 5.26 ms | **4.80** ms | 8.03 ms | 13.22 ms | 33.96 ms | **2317.67** | 
| `node` (`12.4`) | [0http](https://github.com/jkyberneees/0http) (**1.0**) | 6.60 ms | **5.09** ms | 9.59 ms | 18.49 ms | 267.53 ms | **8735.00** | 
| `go` (`1.12`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.0**) | 7.43 ms | **5.22** ms | 15.25 ms | 33.29 ms | 176.98 ms | **6996.33** | 
| `go` (`1.12`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 7.55 ms | **5.30** ms | 14.55 ms | 32.20 ms | 248.13 ms | **9440.00** | 
| `node` (`12.4`) | [restana](https://github.com/jkyberneees/ana) (**3.1**) | 7.05 ms | **5.52** ms | 9.97 ms | 20.72 ms | 293.60 ms | **8154.33** | 
| `go` (`1.12`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 8.30 ms | **5.57** ms | 17.70 ms | 37.52 ms | 224.11 ms | **8338.67** | 
| `go` (`1.12`) | [beego](https://beego.me) (**1.12**) | 7.75 ms | **5.64** ms | 15.24 ms | 32.21 ms | 168.94 ms | **6810.00** | 
| `go` (`1.12`) | [echo](https://echo.labstack.com) (**4.1**) | 8.05 ms | **5.65** ms | 16.64 ms | 34.98 ms | 155.18 ms | **7402.00** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**5.2**) | 40.95 ms | **5.71** ms | 133.52 ms | 329.79 ms | 887.44 ms | **71693.33** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 6.75 ms | **5.83** ms | 9.52 ms | 18.58 ms | 264.65 ms | **7725.67** | 
| `go` (`1.12`) | [kami](https://github.com/guregu/kami) (**2.2**) | 7.63 ms | **5.85** ms | 13.55 ms | 30.51 ms | 178.88 ms | **6651.00** | 
| `go` (`1.12`) | [violetear](https://violetear.org) (**7.0**) | 7.75 ms | **5.96** ms | 14.05 ms | 31.27 ms | 178.47 ms | **6622.33** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 6.99 ms | **6.03** ms | 11.32 ms | 23.78 ms | 183.56 ms | **6761.33** | 
| `go` (`1.12`) | [gin](https://gin-gonic.com) (**1.4**) | 8.54 ms | **6.12** ms | 17.32 ms | 37.19 ms | 129.44 ms | **7279.00** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 8.20 ms | **6.52** ms | 16.26 ms | 33.91 ms | 201.71 ms | **7331.00** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 14.06 ms | **6.79** ms | 27.22 ms | 134.25 ms | 656.92 ms | **32648.00** | 
| `go` (`1.12`) | [gf](https://goframe.org) (**1.6**) | 10.91 ms | **7.88** ms | 22.27 ms | 51.03 ms | 173.54 ms | **10263.67** | 
| `node` (`12.4`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 9.10 ms | **7.94** ms | 12.99 ms | 27.59 ms | 379.19 ms | **13969.67** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 10.10 ms | **8.54** ms | 17.91 ms | 38.00 ms | 217.12 ms | **9459.33** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 215.10 ms | **8.55** ms | 128.37 ms | 5037.31 ms | 7934.07 ms | **862182.67** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 12.24 ms | **9.00** ms | 24.11 ms | 44.83 ms | 180.81 ms | **9798.00** | 
| `node` (`12.4`) | [foxify](https://foxify.js.org) (**0.1**) | 193.72 ms | **9.85** ms | 901.31 ms | 1515.63 ms | 1829.47 ms | **402406.00** | 
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 17.52 ms | **11.60** ms | 37.25 ms | 89.58 ms | 337.60 ms | **18598.67** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 12.60 ms | **11.70** ms | 21.13 ms | 36.37 ms | 110.13 ms | **6933.67** | 
| `node` (`12.4`) | [fastify](https://fastify.io) (**2.4**) | 26.55 ms | **11.78** ms | 37.00 ms | 371.17 ms | 896.27 ms | **65806.00** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.5**) | 17.02 ms | **12.14** ms | 33.84 ms | 61.42 ms | 283.89 ms | **13008.67** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 15.46 ms | **13.42** ms | 27.24 ms | 51.18 ms | 511.56 ms | **17932.33** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 16.38 ms | **13.60** ms | 24.00 ms | 53.14 ms | 488.04 ms | **15033.33** | 
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 28.56 ms | **17.25** ms | 37.26 ms | 334.26 ms | 1435.13 ms | **71018.33** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.31**) | 19.24 ms | **17.48** ms | 30.85 ms | 48.09 ms | 170.19 ms | **9018.33** | 
| `node` (`12.4`) | [koa](https://koajs.com) (**2.7**) | 33.09 ms | **17.59** ms | 42.25 ms | 471.97 ms | 1279.60 ms | **83997.33** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.5**) | 23.69 ms | **18.12** ms | 45.56 ms | 63.86 ms | 116.37 ms | **13576.33** | 
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 22.79 ms | **18.45** ms | 39.13 ms | 63.37 ms | 125.31 ms | **11948.67** | 
| `node` (`12.4`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 46.87 ms | **19.42** ms | 59.91 ms | 716.06 ms | 1449.10 ms | **120303.67** | 
| `node` (`12.4`) | [express](https://expressjs.com) (**4.16**) | 35.97 ms | **22.57** ms | 48.95 ms | 408.25 ms | 1202.39 ms | **72713.00** | 
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 26.41 ms | **22.59** ms | 38.42 ms | 56.00 ms | 912.34 ms | **29435.67** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 30.27 ms | **24.16** ms | 49.80 ms | 86.87 ms | 783.59 ms | **28828.00** | 
| `node` (`12.4`) | [rayo](https://rayo.js.org) (**1.3**) | 40.48 ms | **24.33** ms | 65.75 ms | 464.21 ms | 1427.40 ms | **85617.33** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.0**) | 39.15 ms | **25.88** ms | 82.62 ms | 166.02 ms | 424.16 ms | **33090.67** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.17**) | 37.74 ms | **33.02** ms | 61.96 ms | 91.74 ms | 207.59 ms | **16951.00** | 
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 43.95 ms | **39.14** ms | 55.77 ms | 168.63 ms | 1380.20 ms | **49621.00** | 
| `node` (`12.4`) | [restify](https://restify.com) (**8.2**) | 54.14 ms | **39.79** ms | 87.20 ms | 352.61 ms | 1171.31 ms | **66746.33** | 
| `crystal` (`0.28`) | [lucky](https://luckyframework.org) (**0.14**) | 53.84 ms | **40.06** ms | 84.55 ms | 360.12 ms | 525.54 ms | **57775.00** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 53.56 ms | **45.05** ms | 102.83 ms | 181.06 ms | 356.06 ms | **37634.67** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.9**) | 58.99 ms | **46.63** ms | 111.55 ms | 156.89 ms | 230.37 ms | **34146.00** | 
| `node` (`12.4`) | [hapi](https://hapijs.com) (**18.1**) | 219.02 ms | **58.86** ms | 477.40 ms | 2823.52 ms | 4227.12 ms | **530816.67** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 75.95 ms | **68.21** ms | 126.97 ms | 167.84 ms | 236.34 ms | **36562.67** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 82.54 ms | **71.30** ms | 137.28 ms | 201.87 ms | 863.92 ms | **44457.67** | 
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 118.09 ms | **72.79** ms | 161.81 ms | 1408.34 ms | 3486.49 ms | **241904.33** | 
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 86.14 ms | **84.05** ms | 120.71 ms | 159.97 ms | 230.67 ms | **26822.67** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 85.72 ms | **85.97** ms | 107.69 ms | 135.62 ms | 392.40 ms | **21893.33** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 101.91 ms | **92.19** ms | 155.77 ms | 217.34 ms | 909.55 ms | **45187.00** | 
| `node` (`12.4`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 158.89 ms | **149.19** ms | 243.16 ms | 360.10 ms | 985.37 ms | **73126.33** | 
| `python` (`3.7`) | [cyclone](https://cyclone.io) (**0.0**) | 265.35 ms | **202.83** ms | 248.87 ms | 3214.83 ms | 5815.20 ms | **499500.00** | 

### Requests per seconds


#### Ranking (top 5)


:one: (japronto) (python)


:two: (agoo-c) (c)


:three: (actix-web) (rust)


:four: (rapidoid) (java)


:five: (vibora) (python)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 341921.67 | **409.06** MB |
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 341384.67 | **197.62** MB |
| `rust` (`1.35`) | [actix-web](https://actix.rs) (**0.7**) | 300211.00 | **341.08** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 294445.33 | **529.03** MB |
| `python` (`3.6`) | [vibora](https://vibora.io) (**0.0**) | 275443.33 | **312.73** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 256506.00 | **248.91** MB |
| `go` (`1.12`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 238051.67 | **383.66** MB |
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 223681.00 | **210.39** MB |
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.4**) | 217478.00 | **231.69** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.8**) | 214319.00 | **123.99** MB |
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 210005.33 | **197.41** MB |
| `rust` (`1.35`) | [gotham](https://gotham.rs) (**0.3**) | 201157.67 | **411.62** MB |
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.25**) | 199890.33 | **326.90** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 198988.67 | **343.47** MB |
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.28**) | 178346.33 | **326.69** MB |
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 175750.33 | **287.41** MB |
| `node` (`12.4`) | [0http](https://github.com/jkyberneees/0http) (**1.0**) | 154035.00 | **230.94** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 148290.33 | **385.66** MB |
| `go` (`1.12`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.0**) | 146445.33 | **194.86** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 146352.00 | **237.96** MB |
| `go` (`1.12`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 146275.67 | **196.36** MB |
| `node` (`12.4`) | [restana](https://github.com/jkyberneees/ana) (**3.1**) | 143477.67 | **215.06** MB |
| `go` (`1.12`) | [beego](https://beego.me) (**1.12**) | 140049.67 | **187.74** MB |
| `go` (`1.12`) | [kami](https://github.com/guregu/kami) (**2.2**) | 138064.00 | **183.39** MB |
| `go` (`1.12`) | [echo](https://echo.labstack.com) (**4.1**) | 137138.00 | **240.13** MB |
| `go` (`1.12`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 136639.00 | **181.81** MB |
| `go` (`1.12`) | [violetear](https://violetear.org) (**7.0**) | 134595.00 | **179.42** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 130856.33 | **203.83** MB |
| `go` (`1.12`) | [gin](https://gin-gonic.com) (**1.4**) | 128436.33 | **225.07** MB |
| `rust` (`1.35`) | [iron](https://ironframework.io) (**0.6**) | 124726.00 | **157.73** MB |
| `node` (`12.4`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 118506.33 | **177.68** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 104757.67 | **245.45** MB |
| `go` (`1.12`) | [gf](https://goframe.org) (**1.6**) | 103913.67 | **156.77** MB |
| `rust` (`1.35`) | [nickel](https://nickel-org.github.io) (**0.11**) | 101791.33 | **202.47** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 86099.33 | **212.05** MB |
| `node` (`12.4`) | [foxify](https://foxify.js.org) (**0.1**) | 84418.67 | **177.14** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 80789.33 | **174.39** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 73498.00 | **53.44** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 73397.67 | **128.57** MB |
| `node` (`12.4`) | [fastify](https://fastify.io) (**2.4**) | 71692.00 | **187.71** MB |
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 69589.33 | **140.00** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 68498.33 | **146.69** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.5**) | 63731.00 | **157.87** MB |
| `node` (`12.4`) | [koa](https://koajs.com) (**2.7**) | 60022.33 | **127.24** MB |
| `node` (`12.4`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 56573.00 | **84.74** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**3.12**) | 52762.00 | **261.59** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 52349.33 | **259.77** MB |
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 52024.67 | **48.83** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.31**) | 51955.67 | **112.30** MB |
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 51447.33 | **87.32** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 50704.67 | **251.43** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 49276.00 | **244.57** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 49203.00 | **255.23** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.5**) | 45368.67 | **102.83** MB |
| `node` (`12.4`) | [express](https://expressjs.com) (**4.16**) | 45252.67 | **110.73** MB |
| `rust` (`nightly`) | [rocket](https://rocket.rs) (**0.4**) | 44003.67 | **70.92** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 41526.33 | **216.35** MB |
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 38916.33 | **72.15** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 34838.33 | **64.75** MB |
| `node` (`12.4`) | [rayo](https://rayo.js.org) (**1.3**) | 34535.67 | **51.76** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.0**) | 28942.67 | **71.31** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.21**) | 27753.00 | **26.48** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.17**) | 26691.33 | **51.60** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 25593.67 | **30.16** MB |
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 24591.00 | **30.87** MB |
| `crystal` (`0.28`) | [lucky](https://luckyframework.org) (**0.14**) | 23425.33 | **28.76** MB |
| `node` (`12.4`) | [restify](https://restify.com) (**8.2**) | 21153.67 | **37.15** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 20885.00 | **12.05** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 19794.00 | **35.28** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.9**) | 17297.67 | **34.47** MB |
| `node` (`12.4`) | [hapi](https://hapijs.com) (**18.1**) | 17197.00 | **44.13** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 15096.33 | **8.71** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 13135.33 | **28.61** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 12855.67 | **97.22** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 12202.33 | **35.44** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 11914.33 | **30.90** MB |
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 11527.67 | **29.61** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 11433.67 | **33.75** MB |
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 11309.67 | **21.40** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 9541.33 | **23.46** MB |
| `node` (`12.4`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 5900.33 | **5.55** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**5.2**) | 3116.00 | **9.54** MB |
| `python` (`3.7`) | [cyclone](https://cyclone.io) (**0.0**) | 2207.00 | **5.98** MB |
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

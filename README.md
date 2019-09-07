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
Last update: 2019-09-07
```
OS: Linux (version: 5.2.11-200.fc30.x86_64, arch: x86_64)
CPU Cores: 12
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: cuba (ruby)


:four: swifter (swift)


:five: agoo-c (c)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 0.06 ms | **0.05** ms | 0.11 ms | 0.14 ms | 3.64 ms | **46.00** | 
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 0.42 ms | **0.19** ms | 1.02 ms | 2.87 ms | 16.61 ms | **606.67** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 2.95 ms | **0.87** ms | 7.93 ms | 15.18 ms | 35.31 ms | **3616.67** | 
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 4.55 ms | **0.87** ms | 14.58 ms | 15.07 ms | 861.06 ms | **10148.33** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 2.72 ms | **1.36** ms | 6.56 ms | 13.13 ms | 84.03 ms | **3044.67** | 
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 2.67 ms | **1.48** ms | 6.77 ms | 12.66 ms | 28.42 ms | **3016.67** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 3.29 ms | **1.65** ms | 8.02 ms | 18.56 ms | 44.90 ms | **3971.00** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.23**) | 2.78 ms | **1.85** ms | 6.76 ms | 12.43 ms | 28.59 ms | **2965.67** | 
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 2.52 ms | **1.97** ms | 5.41 ms | 11.01 ms | 32.65 ms | **2308.67** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 5.41 ms | **2.14** ms | 9.23 ms | 74.79 ms | 173.82 ms | **12783.33** | 
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 3.12 ms | **2.17** ms | 6.51 ms | 15.60 ms | 45.07 ms | **3217.00** | 
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 3.10 ms | **2.19** ms | 5.67 ms | 13.25 ms | 223.30 ms | **5011.33** | 
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 3.02 ms | **2.23** ms | 5.70 ms | 12.47 ms | 100.08 ms | **2707.33** | 
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 3.25 ms | **2.26** ms | 5.76 ms | 13.86 ms | 172.59 ms | **5799.00** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 33.22 ms | **2.31** ms | 110.29 ms | 303.93 ms | 889.28 ms | **64786.00** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 8.14 ms | **2.69** ms | 6.40 ms | 140.61 ms | 1122.33 ms | **53507.00** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 3.68 ms | **2.78** ms | 7.27 ms | 18.67 ms | 39.55 ms | **3643.67** | 
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 3.79 ms | **2.79** ms | 7.87 ms | 15.90 ms | 36.43 ms | **3288.67** | 
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 3.49 ms | **2.82** ms | 6.33 ms | 15.09 ms | 213.16 ms | **4632.67** | 
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 3.42 ms | **2.83** ms | 6.51 ms | 13.06 ms | 29.79 ms | **2733.67** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 3.66 ms | **2.96** ms | 8.61 ms | 15.69 ms | 36.21 ms | **3736.33** | 
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 3.50 ms | **3.02** ms | 6.31 ms | 13.04 ms | 32.31 ms | **2654.00** | 
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 3.73 ms | **3.21** ms | 6.79 ms | 14.02 ms | 33.31 ms | **2825.33** | 
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 4.28 ms | **3.42** ms | 7.61 ms | 13.17 ms | 193.44 ms | **4703.33** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 5.00 ms | **3.52** ms | 9.89 ms | 18.78 ms | 154.25 ms | **4592.00** | 
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 4.11 ms | **3.56** ms | 7.43 ms | 15.53 ms | 34.53 ms | **3005.67** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.1**) | 6.30 ms | **3.61** ms | 9.10 ms | 82.79 ms | 116.16 ms | **12748.00** | 
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 4.84 ms | **3.70** ms | 8.51 ms | 18.76 ms | 233.91 ms | **7247.00** | 
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 5.25 ms | **3.97** ms | 9.39 ms | 20.25 ms | 350.28 ms | **9012.67** | 
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 4.96 ms | **4.01** ms | 9.25 ms | 19.41 ms | 103.20 ms | **4097.33** | 
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 4.40 ms | **4.13** ms | 7.51 ms | 15.86 ms | 33.61 ms | **2975.67** | 
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 5.24 ms | **4.14** ms | 10.02 ms | 20.83 ms | 203.63 ms | **5032.33** | 
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 5.17 ms | **4.14** ms | 8.89 ms | 19.89 ms | 279.67 ms | **6956.00** | 
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 5.45 ms | **4.16** ms | 10.04 ms | 21.13 ms | 319.32 ms | **8165.67** | 
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 5.30 ms | **4.17** ms | 10.40 ms | 21.26 ms | 140.56 ms | **4488.33** | 
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 5.40 ms | **4.18** ms | 10.11 ms | 21.93 ms | 281.56 ms | **6722.00** | 
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 5.05 ms | **4.24** ms | 8.83 ms | 18.63 ms | 161.09 ms | **4086.00** | 
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 5.49 ms | **4.33** ms | 10.98 ms | 22.05 ms | 102.51 ms | **4485.67** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 6.01 ms | **4.41** ms | 7.12 ms | 67.39 ms | 429.27 ms | **14275.33** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 5.38 ms | **4.47** ms | 12.41 ms | 22.90 ms | 53.44 ms | **5288.67** | 
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.12**) | 6.82 ms | **4.58** ms | 13.68 ms | 29.47 ms | 420.60 ms | **12000.33** | 
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 4.55 ms | **4.60** ms | 6.86 ms | 11.89 ms | 38.19 ms | **2239.33** | 
| `go` (`1.13`) | [gf](https://goframe.org) (**1.8**) | 6.29 ms | **4.80** ms | 11.80 ms | 25.91 ms | 172.38 ms | **6056.33** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 182.82 ms | **4.84** ms | 24.62 ms | 4572.23 ms | 7354.66 ms | **771004.67** | 
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 6.36 ms | **5.03** ms | 9.64 ms | 19.23 ms | 223.98 ms | **5411.33** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 8.39 ms | **5.06** ms | 20.45 ms | 41.19 ms | 104.91 ms | **9172.00** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 7.89 ms | **5.06** ms | 16.68 ms | 34.52 ms | 276.12 ms | **8860.67** | 
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 6.72 ms | **5.20** ms | 9.67 ms | 20.32 ms | 248.78 ms | **7541.33** | 
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 6.95 ms | **5.45** ms | 9.76 ms | 21.22 ms | 280.84 ms | **7571.00** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 6.29 ms | **5.65** ms | 14.12 ms | 25.21 ms | 54.86 ms | **5976.67** | 
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 13.44 ms | **5.91** ms | 35.16 ms | 76.28 ms | 175.47 ms | **16730.00** | 
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 7.56 ms | **6.15** ms | 10.78 ms | 21.44 ms | 306.90 ms | **8928.00** | 
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 8.19 ms | **6.50** ms | 11.07 ms | 27.39 ms | 363.77 ms | **12848.00** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 7.84 ms | **6.90** ms | 17.20 ms | 31.89 ms | 64.88 ms | **7262.00** | 
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 7.07 ms | **6.99** ms | 9.00 ms | 11.26 ms | 89.60 ms | **1610.33** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 9.36 ms | **7.25** ms | 18.09 ms | 36.23 ms | 210.92 ms | **8226.00** | 
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 8.55 ms | **7.38** ms | 12.58 ms | 24.52 ms | 320.40 ms | **9209.67** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 24.44 ms | **7.78** ms | 53.00 ms | 246.89 ms | 496.07 ms | **49924.00** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 25.87 ms | **8.01** ms | 53.76 ms | 265.52 ms | 555.62 ms | **53679.00** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 24.57 ms | **8.02** ms | 48.37 ms | 255.03 ms | 521.14 ms | **51320.33** | 
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 8.00 ms | **8.04** ms | 10.41 ms | 12.33 ms | 76.81 ms | **1987.00** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 9.80 ms | **8.16** ms | 13.30 ms | 31.31 ms | 359.11 ms | **10645.33** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 27.59 ms | **8.20** ms | 54.47 ms | 299.37 ms | 438.19 ms | **59444.00** | 
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 9.09 ms | **8.31** ms | 13.74 ms | 23.15 ms | 300.19 ms | **8671.00** | 
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 10.09 ms | **8.44** ms | 14.18 ms | 27.93 ms | 380.76 ms | **13304.67** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 26.60 ms | **8.57** ms | 54.00 ms | 271.24 ms | 564.66 ms | **55315.00** | 
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 10.61 ms | **8.62** ms | 15.36 ms | 38.71 ms | 375.63 ms | **13939.33** | 
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 10.50 ms | **8.67** ms | 15.13 ms | 30.63 ms | 402.81 ms | **14153.33** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 11.96 ms | **8.81** ms | 19.11 ms | 39.54 ms | 981.49 ms | **31854.00** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 12.51 ms | **8.93** ms | 24.80 ms | 49.83 ms | 323.71 ms | **11676.00** | 
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 11.47 ms | **9.18** ms | 17.36 ms | 30.86 ms | 348.44 ms | **11774.33** | 
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 11.67 ms | **9.18** ms | 16.84 ms | 28.29 ms | 643.62 ms | **23009.67** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 50.09 ms | **9.96** ms | 84.52 ms | 660.02 ms | 1123.38 ms | **130544.33** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 10.75 ms | **10.56** ms | 16.57 ms | 21.94 ms | 94.80 ms | **4475.00** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 11.85 ms | **11.52** ms | 16.48 ms | 21.45 ms | 225.43 ms | **5228.33** | 
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 12.62 ms | **11.83** ms | 15.10 ms | 29.84 ms | 282.38 ms | **8087.00** | 
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 17.27 ms | **13.84** ms | 25.42 ms | 60.02 ms | 460.87 ms | **16271.33** | 
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 25.57 ms | **15.06** ms | 26.11 ms | 366.77 ms | 1833.80 ms | **89127.00** | 
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 19.19 ms | **15.51** ms | 27.96 ms | 92.29 ms | 568.11 ms | **23053.67** | 
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 16.47 ms | **15.61** ms | 16.33 ms | 17.78 ms | 718.33 ms | **21555.33** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 27.29 ms | **17.87** ms | 54.61 ms | 88.32 ms | 464.83 ms | **24042.67** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 26.83 ms | **18.56** ms | 52.79 ms | 90.33 ms | 514.56 ms | **22452.33** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 19.37 ms | **18.81** ms | 24.65 ms | 30.97 ms | 50.23 ms | **3909.33** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 19.10 ms | **18.94** ms | 29.63 ms | 37.67 ms | 62.56 ms | **8197.00** | 
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 20.90 ms | **20.81** ms | 22.05 ms | 23.61 ms | 536.11 ms | **13611.67** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 24.21 ms | **22.63** ms | 36.56 ms | 49.20 ms | 128.82 ms | **9641.00** | 
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 25.75 ms | **25.42** ms | 27.10 ms | 28.81 ms | 490.72 ms | **14174.33** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 29.02 ms | **25.98** ms | 49.57 ms | 80.93 ms | 181.71 ms | **15947.00** | 
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 26.84 ms | **26.68** ms | 32.54 ms | 33.96 ms | 48.10 ms | **3893.67** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 30.80 ms | **28.16** ms | 50.52 ms | 81.08 ms | 385.51 ms | **18878.00** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.1**) | 50.65 ms | **47.24** ms | 82.17 ms | 106.78 ms | 320.16 ms | **23933.00** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 72.06 ms | **55.62** ms | 133.50 ms | 178.67 ms | 527.75 ms | **40026.00** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 62.23 ms | **58.67** ms | 87.14 ms | 107.94 ms | 369.64 ms | **19300.33** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 69.51 ms | **69.59** ms | 80.94 ms | 86.82 ms | 416.05 ms | **16656.67** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 91.13 ms | **70.16** ms | 162.38 ms | 242.64 ms | 563.82 ms | **54026.67** | 
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 182.80 ms | **183.22** ms | 209.01 ms | 234.59 ms | 315.07 ms | **22985.00** | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (sifrr) (node)


:three: (japronto) (python)


:four: (drogon) (cpp)


:five: (rapidoid) (java)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 427943.67 | **247.61** MB |
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 384903.67 | **338.90** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 371873.00 | **445.30** MB |
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 342656.33 | **332.44** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 335004.67 | **602.71** MB |
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 320069.00 | **643.85** MB |
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 318152.00 | **511.23** MB |
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 315045.00 | **505.68** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 308558.33 | **802.15** MB |
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 288280.33 | **590.32** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 288097.00 | **279.05** MB |
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 285815.00 | **268.67** MB |
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 274047.67 | **257.64** MB |
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 273339.00 | **290.75** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 263344.67 | **454.20** MB |
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 260391.67 | **425.36** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.1**) | 241783.67 | **139.93** MB |
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 239211.33 | **437.74** MB |
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 236717.67 | **475.71** MB |
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 227540.67 | **581.34** MB |
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 221289.67 | **361.45** MB |
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 217068.67 | **289.92** MB |
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 205934.67 | **274.81** MB |
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 203673.67 | **272.13** MB |
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 200717.00 | **253.27** MB |
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 198345.00 | **263.27** MB |
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 197133.00 | **345.88** MB |
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 196754.67 | **261.09** MB |
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 196469.00 | **263.28** MB |
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 195836.00 | **343.60** MB |
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 195728.00 | **343.44** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 195190.67 | **318.05** MB |
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 191015.67 | **254.70** MB |
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 181205.67 | **275.56** MB |
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 176199.67 | **350.47** MB |
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.12**) | 170698.33 | **356.30** MB |
| `go` (`1.13`) | [gf](https://goframe.org) (**1.8**) | 166471.33 | **252.08** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 153695.00 | **239.72** MB |
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 151785.33 | **227.56** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 151281.67 | **326.57** MB |
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 149517.67 | **224.16** MB |
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 143805.33 | **215.60** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 142179.33 | **333.33** MB |
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 134222.00 | **201.25** MB |
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 131373.00 | **196.82** MB |
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 120234.33 | **113.11** MB |
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 118697.00 | **177.85** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 115905.00 | **87.79** MB |
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 111178.67 | **289.14** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 110478.00 | **272.14** MB |
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 109857.67 | **234.37** MB |
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 109541.67 | **230.21** MB |
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 100927.33 | **409.77** MB |
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 100662.67 | **213.20** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 100017.00 | **175.55** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 96418.33 | **478.97** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 96207.33 | **478.14** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 94971.67 | **472.34** MB |
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 93101.00 | **156.10** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 92437.67 | **459.49** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 89500.67 | **180.02** MB |
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 88862.33 | **217.78** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 88390.67 | **459.91** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 85915.33 | **213.15** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 80541.67 | **173.66** MB |
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 79263.00 | **139.21** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 76364.00 | **398.77** MB |
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 71289.00 | **41.22** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.23**) | 68470.33 | **65.39** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 64995.00 | **76.82** MB |
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 62791.67 | **116.31** MB |
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 59991.33 | **102.85** MB |
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 59186.67 | **112.01** MB |
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 58009.67 | **150.43** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 52161.67 | **30.15** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 51504.33 | **111.27** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 50087.33 | **131.85** MB |
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 48067.67 | **58.92** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 42000.00 | **78.10** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 40640.00 | **92.22** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 39819.00 | **98.15** MB |
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 38112.67 | **35.70** MB |
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 36628.67 | **45.93** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 35523.33 | **20.53** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 34613.33 | **61.74** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 32991.67 | **63.85** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 30486.00 | **230.77** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 24509.00 | **63.63** MB |
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 20571.33 | **26.41** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.1**) | 19293.33 | **38.54** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 15526.00 | **33.90** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 14123.67 | **41.02** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 13987.00 | **41.40** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 11074.00 | **27.27** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 6023.00 | **37.82** MB |
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 5352.33 | **13.82** MB |
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

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
Last update: 2019-09-25
```
OS: Linux (version: 5.2.15-200.fc30.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: syro (ruby)


:four: hanami (ruby)


:five: roda (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 0.10 ms | **0.09** ms | 0.13 ms | 0.23 ms | 5.84 ms | **95.33** | 
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 0.53 ms | **0.41** ms | 0.92 ms | 2.98 ms | 22.22 ms | **574.67** | 
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 2.65 ms | **0.67** ms | 7.62 ms | 17.81 ms | 71.90 ms | **3990.67** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 6.22 ms | **0.70** ms | 20.58 ms | 45.84 ms | 112.29 ms | **10439.67** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.24**) | 2.74 ms | **0.74** ms | 7.78 ms | 17.50 ms | 60.58 ms | **3927.00** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 3.06 ms | **0.81** ms | 8.80 ms | 20.25 ms | 75.16 ms | **4533.00** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 8.10 ms | **0.82** ms | 25.71 ms | 57.07 ms | 141.66 ms | **12993.00** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 3.63 ms | **0.88** ms | 11.30 ms | 26.44 ms | 79.57 ms | **5911.67** | 
| `ruby` (`2.6`) | [grape](https://ruby-grape.org) (**1.2**) | 9.26 ms | **0.90** ms | 29.24 ms | 60.98 ms | 137.88 ms | **14258.00** | 
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 3.36 ms | **0.91** ms | 14.52 ms | 15.04 ms | 1161.38 ms | **13746.33** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 5.55 ms | **1.47** ms | 16.24 ms | 37.17 ms | 101.15 ms | **8268.00** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 33.06 ms | **3.24** ms | 106.18 ms | 276.26 ms | 793.20 ms | **59349.00** | 
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 4.43 ms | **3.72** ms | 8.89 ms | 16.76 ms | 54.22 ms | **3677.33** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 4.76 ms | **3.75** ms | 10.49 ms | 18.27 ms | 44.31 ms | **4282.00** | 
| `nim` (`1.0`) | [httpbeast](https://github.com/dom96/httpbeast) (**0.2**) | 4.64 ms | **4.15** ms | 8.10 ms | 13.51 ms | 28.39 ms | **2633.33** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 4.87 ms | **4.23** ms | 9.62 ms | 17.52 ms | 54.91 ms | **3885.67** | 
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 5.11 ms | **4.48** ms | 9.45 ms | 16.25 ms | 55.14 ms | **3510.33** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 7.01 ms | **4.51** ms | 10.66 ms | 73.57 ms | 210.61 ms | **12046.67** | 
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 5.60 ms | **4.92** ms | 9.04 ms | 16.41 ms | 106.09 ms | **3214.67** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 5.71 ms | **5.02** ms | 9.56 ms | 16.06 ms | 104.29 ms | **3267.67** | 
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 5.78 ms | **5.06** ms | 9.13 ms | 16.86 ms | 160.60 ms | **4422.33** | 
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 5.79 ms | **5.08** ms | 9.30 ms | 17.25 ms | 51.90 ms | **3098.67** | 
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 6.12 ms | **5.29** ms | 10.34 ms | 17.16 ms | 42.79 ms | **3335.33** | 
| `crystal` (`0.29`) | [toro](https://github.com/soveran/toro) (**0.4**) | 6.14 ms | **5.33** ms | 10.48 ms | 17.34 ms | 48.73 ms | **3488.33** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 15.44 ms | **5.42** ms | 13.49 ms | 359.77 ms | 1323.05 ms | **76123.67** | 
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 6.58 ms | **5.64** ms | 11.53 ms | 19.09 ms | 46.48 ms | **3853.00** | 
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 6.51 ms | **5.68** ms | 11.12 ms | 18.67 ms | 59.80 ms | **3803.67** | 
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 7.40 ms | **5.83** ms | 11.85 ms | 20.79 ms | 365.24 ms | **11638.33** | 
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 6.76 ms | **5.88** ms | 11.37 ms | 18.59 ms | 57.33 ms | **3710.67** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.11**) | 8.83 ms | **6.33** ms | 14.18 ms | 76.02 ms | 126.54 ms | **11300.67** | 
| `nim` (`1.0`) | [jester](https://github.com/dom96/jester) (**0.4**) | 7.06 ms | **6.45** ms | 10.48 ms | 16.13 ms | 51.68 ms | **2825.33** | 
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 7.70 ms | **6.87** ms | 13.01 ms | 21.32 ms | 53.73 ms | **4124.33** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 8.20 ms | **7.06** ms | 11.85 ms | 20.34 ms | 284.83 ms | **9584.00** | 
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 8.54 ms | **7.23** ms | 13.88 ms | 30.84 ms | 129.73 ms | **6003.67** | 
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 8.69 ms | **7.29** ms | 15.07 ms | 28.39 ms | 72.24 ms | **5203.00** | 
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 9.25 ms | **7.72** ms | 15.42 ms | 33.81 ms | 174.68 ms | **6686.33** | 
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 8.74 ms | **7.78** ms | 15.09 ms | 25.75 ms | 56.83 ms | **4940.33** | 
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 9.83 ms | **7.96** ms | 16.72 ms | 38.95 ms | 243.24 ms | **8333.33** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 188.65 ms | **8.09** ms | 25.12 ms | 4743.66 ms | 7915.57 ms | **806014.00** | 
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 9.88 ms | **8.15** ms | 16.60 ms | 37.96 ms | 208.01 ms | **7115.00** | 
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 10.29 ms | **8.23** ms | 18.16 ms | 40.50 ms | 186.62 ms | **7739.67** | 
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 9.69 ms | **8.24** ms | 15.77 ms | 35.10 ms | 178.62 ms | **6687.33** | 
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 10.63 ms | **8.36** ms | 19.07 ms | 43.70 ms | 212.89 ms | **8311.67** | 
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 10.61 ms | **8.49** ms | 18.80 ms | 41.88 ms | 105.11 ms | **7609.67** | 
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 10.99 ms | **8.61** ms | 19.95 ms | 44.52 ms | 143.16 ms | **8436.33** | 
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 10.08 ms | **8.63** ms | 15.34 ms | 33.82 ms | 205.57 ms | **8110.33** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 11.09 ms | **9.03** ms | 13.92 ms | 67.00 ms | 439.98 ms | **19849.67** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 12.46 ms | **9.37** ms | 25.87 ms | 48.12 ms | 165.82 ms | **9833.67** | 
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.13**) | 12.36 ms | **9.49** ms | 23.01 ms | 51.02 ms | 180.55 ms | **9522.00** | 
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 13.70 ms | **9.61** ms | 17.75 ms | 113.87 ms | 633.15 ms | **29957.67** | 
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 13.04 ms | **9.66** ms | 17.20 ms | 97.92 ms | 524.47 ms | **23171.67** | 
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 12.09 ms | **9.81** ms | 17.78 ms | 37.69 ms | 394.84 ms | **13241.00** | 
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | 52.94 ms | **10.15** ms | 81.33 ms | 1176.05 ms | 2754.31 ms | **207025.67** | 
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 10.46 ms | **10.20** ms | 13.68 ms | 17.86 ms | 36.47 ms | **2508.67** | 
| `go` (`1.13`) | [gf](https://goframe.org) (**1.9**) | 12.83 ms | **10.40** ms | 21.92 ms | 51.33 ms | 196.18 ms | **9196.00** | 
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 15.34 ms | **10.92** ms | 19.97 ms | 114.31 ms | 665.87 ms | **31207.00** | 
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 16.72 ms | **11.07** ms | 20.37 ms | 177.25 ms | 778.65 ms | **39260.67** | 
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 17.76 ms | **12.44** ms | 22.28 ms | 154.37 ms | 836.75 ms | **39632.00** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 19.07 ms | **12.73** ms | 40.77 ms | 80.64 ms | 233.61 ms | **16289.00** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 16.57 ms | **13.20** ms | 22.65 ms | 63.63 ms | 586.79 ms | **20801.00** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 15.29 ms | **13.65** ms | 28.23 ms | 53.08 ms | 307.03 ms | **13255.33** | 
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 20.67 ms | **13.65** ms | 24.20 ms | 265.61 ms | 855.45 ms | **49083.33** | 
| `php` (`7.3`) | [one](https://github.com/lizhichao/one) (**1.8**) | 14.85 ms | **13.92** ms | 24.23 ms | 38.86 ms | 98.86 ms | **7656.67** | 
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 22.26 ms | **14.17** ms | 24.66 ms | 297.02 ms | 1045.36 ms | **58464.67** | 
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 21.61 ms | **14.28** ms | 25.25 ms | 235.96 ms | 925.52 ms | **50306.00** | 
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 21.46 ms | **14.99** ms | 26.41 ms | 219.74 ms | 810.62 ms | **43775.00** | 
| `php` (`7.3`) | [basicphp](https://github.com/ray-ang/basicphp) (**0.9**) | 45.10 ms | **15.00** ms | 86.07 ms | 460.96 ms | 913.95 ms | **90162.00** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 45.73 ms | **15.12** ms | 88.54 ms | 471.38 ms | 780.96 ms | **93658.00** | 
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 20.76 ms | **15.13** ms | 46.30 ms | 88.46 ms | 202.27 ms | **19402.67** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 21.35 ms | **15.22** ms | 42.08 ms | 76.01 ms | 197.18 ms | **15565.33** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 46.15 ms | **15.29** ms | 84.31 ms | 475.31 ms | 905.68 ms | **93874.33** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 49.77 ms | **15.58** ms | 93.29 ms | 514.47 ms | 1162.54 ms | **104326.00** | 
| `python` (`3.7`) | [asgineer](https://asgineer.readthedocs.io) (**0.7**) | 17.37 ms | **15.76** ms | 27.74 ms | 44.94 ms | 106.12 ms | **8241.33** | 
| `fsharp` (`7.3`) | [suave](https://suave.io) (**2.5**) | 18.63 ms | **16.07** ms | 26.69 ms | 44.94 ms | 378.77 ms | **13457.00** | 
| `ruby` (`2.6`) | [plezi](https://github.com/boazsegev/plezi) (**0.16**) | 21.28 ms | **16.28** ms | 37.91 ms | 97.32 ms | 219.35 ms | **17467.33** | 
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 22.42 ms | **16.48** ms | 29.09 ms | 182.26 ms | 858.66 ms | **41685.33** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 52.39 ms | **16.72** ms | 95.65 ms | 564.59 ms | 955.25 ms | **112162.00** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 19.91 ms | **17.22** ms | 33.13 ms | 59.56 ms | 814.98 ms | **24835.33** | 
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | 19.32 ms | **17.71** ms | 18.51 ms | 42.33 ms | 750.28 ms | **25439.33** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 19.68 ms | **17.87** ms | 30.40 ms | 56.18 ms | 196.10 ms | **9575.00** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**6.0**) | 52.07 ms | **17.93** ms | 107.71 ms | 528.36 ms | 884.00 ms | **105133.00** | 
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 37.64 ms | **18.03** ms | 31.95 ms | 750.74 ms | 2324.43 ms | **136808.33** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 20.25 ms | **18.97** ms | 31.39 ms | 50.35 ms | 101.12 ms | **9269.33** | 
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 25.37 ms | **20.39** ms | 31.44 ms | 111.29 ms | 746.71 ms | **32455.33** | 
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 20.71 ms | **20.90** ms | 23.15 ms | 24.64 ms | 67.85 ms | **2183.00** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.0**) | 82.05 ms | **21.60** ms | 133.12 ms | 973.52 ms | 1507.45 ms | **187846.67** | 
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 23.36 ms | **22.04** ms | 24.82 ms | 30.68 ms | 745.47 ms | **25769.33** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 22.90 ms | **22.40** ms | 30.09 ms | 44.43 ms | 137.08 ms | **6829.33** | 
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 46.05 ms | **26.41** ms | 38.46 ms | 690.00 ms | 1639.86 ms | **115399.00** | 
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 43.17 ms | **27.48** ms | 41.86 ms | 563.16 ms | 1510.92 ms | **95312.33** | 
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 58.94 ms | **28.51** ms | 44.53 ms | 1138.86 ms | 3095.47 ms | **199557.67** | 
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | 30.76 ms | **29.29** ms | 45.98 ms | 68.07 ms | 132.31 ms | **12137.67** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 42.77 ms | **30.11** ms | 98.04 ms | 116.59 ms | 274.31 ms | **28169.00** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 33.02 ms | **30.46** ms | 49.35 ms | 68.91 ms | 145.82 ms | **12891.67** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 44.86 ms | **33.52** ms | 78.69 ms | 156.35 ms | 364.80 ms | **28426.00** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 41.57 ms | **39.19** ms | 60.50 ms | 76.55 ms | 135.93 ms | **12751.00** | 
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 45.91 ms | **41.17** ms | 47.81 ms | 209.46 ms | 868.32 ms | **40988.67** | 
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 44.50 ms | **42.88** ms | 52.58 ms | 81.48 ms | 181.33 ms | **10225.67** | 
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 43.66 ms | **43.89** ms | 52.80 ms | 59.34 ms | 202.14 ms | **8720.33** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 48.96 ms | **46.46** ms | 74.91 ms | 112.08 ms | 362.76 ms | **22296.67** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 55.01 ms | **48.10** ms | 91.14 ms | 152.02 ms | 268.30 ms | **28391.67** | 
| `php` (`7.3`) | [spiral](https://github.com/spiral/framework) (**2.0**) | 68.61 ms | **67.83** ms | 82.64 ms | 98.61 ms | 345.99 ms | **13059.00** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | 85.61 ms | **77.10** ms | 138.32 ms | 196.72 ms | 289.40 ms | **37731.33** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 105.80 ms | **84.84** ms | 211.63 ms | 241.41 ms | 777.66 ms | **53374.67** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 99.01 ms | **91.05** ms | 146.91 ms | 189.74 ms | 460.30 ms | **33568.67** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 110.14 ms | **104.14** ms | 147.69 ms | 253.43 ms | 750.31 ms | **46011.33** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 141.14 ms | **122.76** ms | 223.10 ms | 314.99 ms | 696.96 ms | **55701.67** | 
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | 332.35 ms | **130.53** ms | 751.27 ms | 3428.04 ms | 7153.92 ms | **641428.67** | 
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 201.53 ms | **200.58** ms | 236.06 ms | 269.17 ms | 367.01 ms | **28649.00** | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (sifrr) (node)


:three: (httpbeast) (nim)


:four: (japronto) (python)


:five: (drogon) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 216128.33 | **125.12** MB |
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 202957.67 | **178.72** MB |
| `nim` (`1.0`) | [httpbeast](https://github.com/dom96/httpbeast) (**0.2**) | 198398.00 | **282.19** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 192910.67 | **231.03** MB |
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 182174.67 | **176.91** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 173958.33 | **313.13** MB |
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 161814.33 | **260.24** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 159694.67 | **155.01** MB |
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 158071.67 | **318.08** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 157677.00 | **409.84** MB |
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 156790.33 | **252.57** MB |
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 154977.00 | **145.72** MB |
| `crystal` (`0.29`) | [toro](https://github.com/soveran/toro) (**0.4**) | 153152.67 | **144.11** MB |
| `nim` (`1.0`) | [jester](https://github.com/dom96/jester) (**0.4**) | 147859.00 | **297.02** MB |
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 145669.00 | **154.67** MB |
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 144541.00 | **135.98** MB |
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 139989.67 | **228.80** MB |
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 136028.33 | **279.06** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 135972.00 | **234.58** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.11**) | 132427.00 | **76.57** MB |
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 124983.67 | **228.84** MB |
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 116582.33 | **156.21** MB |
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 113068.67 | **288.85** MB |
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 111717.67 | **182.40** MB |
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 111083.00 | **164.44** MB |
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 108723.00 | **145.60** MB |
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 107925.33 | **136.04** MB |
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 104577.00 | **139.66** MB |
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 103498.00 | **138.17** MB |
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 103263.33 | **139.08** MB |
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 100859.33 | **177.15** MB |
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 100301.67 | **133.46** MB |
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 97948.67 | **171.85** MB |
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 97700.00 | **171.45** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 96678.67 | **157.28** MB |
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 95555.33 | **126.87** MB |
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 89696.00 | **134.43** MB |
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 89328.67 | **177.58** MB |
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 88599.00 | **132.76** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 87697.00 | **205.49** MB |
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.13**) | 86909.00 | **181.39** MB |
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 85429.00 | **127.99** MB |
| `go` (`1.13`) | [gf](https://goframe.org) (**1.9**) | 82290.00 | **139.79** MB |
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 78595.67 | **117.71** MB |
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | 78306.00 | **139.78** MB |
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 77226.33 | **115.72** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 72866.33 | **157.48** MB |
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 70277.00 | **105.18** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 68563.67 | **51.47** MB |
| `php` (`7.3`) | [one](https://github.com/lizhichao/one) (**1.8**) | 67150.67 | **154.21** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 66797.67 | **104.07** MB |
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 65920.00 | **173.38** MB |
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 65251.67 | **137.06** MB |
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 61207.67 | **129.42** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 59294.33 | **146.04** MB |
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 59291.33 | **240.61** MB |
| `python` (`3.7`) | [asgineer](https://asgineer.readthedocs.io) (**0.7**) | 57723.00 | **102.98** MB |
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 56659.67 | **120.87** MB |
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | 56442.33 | **101.38** MB |
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 53494.00 | **130.82** MB |
| `ruby` (`2.6`) | [plezi](https://github.com/boazsegev/plezi) (**0.16**) | 53110.33 | **113.22** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 53010.00 | **93.02** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 51948.33 | **258.05** MB |
| `php` (`7.3`) | [basicphp](https://github.com/ray-ang/basicphp) (**0.9**) | 51909.00 | **257.95** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 51430.67 | **255.59** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 51300.67 | **103.20** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 49862.33 | **247.82** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 49687.67 | **123.23** MB |
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 49360.00 | **84.34** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 49200.00 | **105.96** MB |
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 48059.00 | **45.09** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 47640.33 | **236.52** MB |
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 47599.33 | **27.54** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.24**) | 46166.00 | **44.09** MB |
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 45012.00 | **83.63** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**6.0**) | 44667.33 | **232.46** MB |
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 43817.67 | **76.88** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 42667.33 | **112.05** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 41581.00 | **49.13** MB |
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 35410.67 | **91.80** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 35210.67 | **20.34** MB |
| `fsharp` (`7.3`) | [suave](https://suave.io) (**2.5**) | 35077.33 | **71.12** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.0**) | 34758.67 | **181.68** MB |
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 32730.00 | **55.62** MB |
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 32153.33 | **60.70** MB |
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | 32138.00 | **73.71** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 30207.67 | **65.22** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 24832.00 | **61.12** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 23915.00 | **54.25** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 23802.33 | **44.25** MB |
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 23090.33 | **21.66** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 23058.00 | **13.32** MB |
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 22597.67 | **27.74** MB |
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 22424.00 | **28.17** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 20498.67 | **155.14** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 20421.33 | **39.40** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 18524.67 | **33.05** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 15741.00 | **40.85** MB |
| `php` (`7.3`) | [spiral](https://github.com/spiral/framework) (**2.0**) | 14390.67 | **24.99** MB |
| `ruby` (`2.6`) | [grape](https://ruby-grape.org) (**1.2**) | 13849.67 | **7.97** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | 11562.00 | **23.09** MB |
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 11462.67 | **14.69** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 9859.00 | **21.49** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 9534.00 | **27.62** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 8881.67 | **26.11** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 6913.33 | **17.04** MB |
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 4903.00 | **12.66** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 3879.33 | **24.34** MB |
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | 633.67 | **1.44** MB |
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

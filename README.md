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
Last update: 2019-09-03
```
OS: Linux (version: 5.2.9-200.fc30.x86_64, arch: x86_64)
CPU Cores: 12
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: swifter (swift)


:four: rocket (rust)


:five: agoo-c (c)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 0.07 ms | **0.06** ms | 0.11 ms | 0.14 ms | 3.34 ms | **38.00** | 
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 0.44 ms | **0.22** ms | 1.03 ms | 2.56 ms | 17.98 ms | **593.33** | 
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 5.19 ms | **0.92** ms | 14.63 ms | 15.16 ms | 1873.59 ms | **26883.67** | 
| `rust` (`nightly`) | [rocket](https://rocket.rs) (**0.4**) | 73.97 ms | **1.45** ms | 3.76 ms | 2349.98 ms | 6592.69 ms | **465437.00** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 2.68 ms | **1.55** ms | 6.22 ms | 13.30 ms | 38.99 ms | **2878.67** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 3.37 ms | **1.74** ms | 8.24 ms | 18.58 ms | 67.14 ms | **4014.00** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 3.65 ms | **1.87** ms | 9.40 ms | 17.74 ms | 45.54 ms | **4238.33** | 
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 2.74 ms | **1.90** ms | 6.71 ms | 12.39 ms | 30.49 ms | **2948.33** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.23**) | 2.80 ms | **2.03** ms | 6.74 ms | 12.32 ms | 36.22 ms | **2945.67** | 
| `node` (`12.9`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 2.58 ms | **2.05** ms | 5.48 ms | 11.23 ms | 31.20 ms | **2339.00** | 
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 3.32 ms | **2.22** ms | 7.15 ms | 16.16 ms | 81.89 ms | **3469.00** | 
| `go` (`1.12`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 3.02 ms | **2.25** ms | 5.73 ms | 12.57 ms | 144.26 ms | **2702.00** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 4.92 ms | **2.27** ms | 9.22 ms | 68.06 ms | 162.30 ms | **11030.67** | 
| `go` (`1.12`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 3.12 ms | **2.27** ms | 5.75 ms | 13.02 ms | 205.53 ms | **4100.67** | 
| `go` (`1.12`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 3.17 ms | **2.38** ms | 5.80 ms | 12.96 ms | 154.96 ms | **3478.33** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 3.09 ms | **2.52** ms | 7.36 ms | 13.41 ms | 31.20 ms | **3203.00** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 33.83 ms | **2.52** ms | 111.69 ms | 306.12 ms | 922.37 ms | **65355.67** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 8.01 ms | **2.73** ms | 7.15 ms | 132.11 ms | 1138.05 ms | **50513.67** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 3.80 ms | **2.85** ms | 7.50 ms | 18.98 ms | 40.69 ms | **3693.67** | 
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 3.91 ms | **2.91** ms | 8.13 ms | 16.10 ms | 34.83 ms | **3349.00** | 
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 3.58 ms | **2.97** ms | 6.17 ms | 14.48 ms | 253.74 ms | **5521.33** | 
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 3.47 ms | **3.00** ms | 6.32 ms | 12.67 ms | 29.12 ms | **2640.33** | 
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 3.71 ms | **3.01** ms | 7.14 ms | 14.45 ms | 33.68 ms | **2975.00** | 
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 3.81 ms | **3.42** ms | 6.71 ms | 13.76 ms | 30.72 ms | **2754.00** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 5.28 ms | **3.60** ms | 10.25 ms | 20.11 ms | 208.53 ms | **6136.67** | 
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 4.19 ms | **3.71** ms | 7.48 ms | 15.48 ms | 35.96 ms | **3003.33** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.1**) | 6.06 ms | **3.73** ms | 9.01 ms | 77.06 ms | 112.13 ms | **11698.33** | 
| `go` (`1.12`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 4.87 ms | **3.88** ms | 8.71 ms | 19.32 ms | 277.03 ms | **5918.33** | 
| `go` (`1.12`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 4.63 ms | **3.99** ms | 9.01 ms | 16.52 ms | 47.19 ms | **3524.33** | 
| `go` (`1.12`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 5.29 ms | **4.12** ms | 10.16 ms | 23.00 ms | 171.83 ms | **5307.67** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 5.39 ms | **4.22** ms | 12.79 ms | 24.01 ms | 54.28 ms | **5544.33** | 
| `go` (`1.12`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 5.37 ms | **4.23** ms | 9.78 ms | 21.38 ms | 233.96 ms | **7176.33** | 
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 4.51 ms | **4.25** ms | 7.64 ms | 16.28 ms | 34.19 ms | **3007.33** | 
| `go` (`1.12`) | [gin](https://gin-gonic.com) (**1.4**) | 5.61 ms | **4.39** ms | 10.70 ms | 23.44 ms | 203.76 ms | **5767.33** | 
| `go` (`1.12`) | [violetear](https://violetear.org) (**7.0**) | 5.46 ms | **4.40** ms | 9.76 ms | 22.29 ms | 178.57 ms | **5415.33** | 
| `go` (`1.12`) | [echo](https://echo.labstack.com) (**4.1**) | 5.57 ms | **4.42** ms | 10.77 ms | 23.33 ms | 113.31 ms | **4756.67** | 
| `go` (`1.12`) | [goroute](https://goroute.github.io) (**0.0**) | 5.69 ms | **4.43** ms | 10.70 ms | 24.18 ms | 270.32 ms | **6660.00** | 
| `go` (`1.12`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 5.81 ms | **4.45** ms | 11.63 ms | 25.57 ms | 150.79 ms | **5195.67** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 5.85 ms | **4.45** ms | 7.30 ms | 58.07 ms | 229.67 ms | **10765.00** | 
| `go` (`1.12`) | [beego](https://beego.me) (**1.12**) | 5.71 ms | **4.45** ms | 10.71 ms | 23.82 ms | 224.78 ms | **5903.67** | 
| `go` (`1.12`) | [kami](https://github.com/guregu/kami) (**2.2**) | 5.34 ms | **4.48** ms | 9.28 ms | 19.86 ms | 99.66 ms | **3707.67** | 
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 4.71 ms | **4.68** ms | 7.05 ms | 12.88 ms | 41.76 ms | **2446.67** | 
| `go` (`1.12`) | [air](https://github.com/aofei/air) (**0.12**) | 6.55 ms | **4.75** ms | 12.72 ms | 28.95 ms | 283.76 ms | **7796.67** | 
| `node` (`12.9`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 5.73 ms | **4.77** ms | 9.32 ms | 17.91 ms | 184.28 ms | **4659.33** | 
| `node` (`12.9`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 6.11 ms | **4.84** ms | 9.53 ms | 19.99 ms | 228.94 ms | **6465.33** | 
| `node` (`12.9`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 6.13 ms | **4.88** ms | 9.50 ms | 19.08 ms | 219.07 ms | **5571.33** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 214.99 ms | **4.95** ms | 29.03 ms | 5389.73 ms | 7548.47 ms | **884429.00** | 
| `go` (`1.12`) | [gf](https://goframe.org) (**1.8**) | 6.69 ms | **4.98** ms | 13.04 ms | 30.01 ms | 117.68 ms | **5928.33** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 7.27 ms | **5.05** ms | 13.67 ms | 26.90 ms | 211.21 ms | **7016.00** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 8.25 ms | **5.09** ms | 19.60 ms | 40.17 ms | 224.51 ms | **9216.00** | 
| `node` (`12.9`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 7.04 ms | **5.30** ms | 10.00 ms | 22.33 ms | 274.59 ms | **9613.67** | 
| `node` (`12.9`) | [rayo](https://rayo.js.org) (**1.3**) | 7.27 ms | **5.41** ms | 10.09 ms | 23.15 ms | 338.81 ms | **10816.67** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 6.33 ms | **5.72** ms | 14.53 ms | 26.04 ms | 62.10 ms | **6212.00** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 7.68 ms | **6.27** ms | 18.08 ms | 34.47 ms | 77.25 ms | **7931.00** | 
| `node` (`12.9`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 7.95 ms | **6.85** ms | 11.70 ms | 23.95 ms | 330.54 ms | **10269.67** | 
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 12.05 ms | **7.02** ms | 28.57 ms | 64.90 ms | 165.37 ms | **13771.67** | 
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 7.53 ms | **7.37** ms | 9.46 ms | 12.18 ms | 42.28 ms | **1659.00** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 9.77 ms | **7.54** ms | 19.33 ms | 40.34 ms | 213.29 ms | **8966.33** | 
| `node` (`12.9`) | [fastify](https://fastify.io) (**2.8**) | 9.00 ms | **7.58** ms | 13.32 ms | 24.26 ms | 284.03 ms | **8493.33** | 
| `node` (`12.9`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 9.99 ms | **7.86** ms | 14.06 ms | 30.51 ms | 405.42 ms | **14654.67** | 
| `node` (`12.9`) | [foxify](https://foxify.js.org) (**0.1**) | 8.06 ms | **7.98** ms | 12.10 ms | 17.52 ms | 173.63 ms | **4542.00** | 
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 7.99 ms | **7.99** ms | 10.41 ms | 12.30 ms | 199.46 ms | **3483.00** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 25.08 ms | **8.00** ms | 51.33 ms | 259.98 ms | 509.21 ms | **51981.00** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 26.25 ms | **8.04** ms | 58.78 ms | 261.01 ms | 782.59 ms | **54393.33** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 25.68 ms | **8.19** ms | 51.60 ms | 268.07 ms | 445.43 ms | **53377.67** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 11.31 ms | **8.29** ms | 14.06 ms | 49.61 ms | 637.65 ms | **23505.67** | 
| `node` (`12.9`) | [koa](https://koajs.com) (**2.8**) | 9.78 ms | **8.50** ms | 14.04 ms | 37.53 ms | 349.47 ms | **13600.33** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 29.49 ms | **8.52** ms | 58.16 ms | 320.26 ms | 577.75 ms | **64268.67** | 
| `node` (`12.9`) | [express](https://expressjs.com) (**4.17**) | 10.23 ms | **8.71** ms | 16.09 ms | 25.02 ms | 245.16 ms | **6845.33** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 26.99 ms | **8.74** ms | 56.47 ms | 277.05 ms | 438.25 ms | **54666.00** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 10.69 ms | **8.94** ms | 19.07 ms | 35.84 ms | 644.84 ms | **14517.00** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 13.51 ms | **9.13** ms | 28.51 ms | 56.95 ms | 354.43 ms | **14149.67** | 
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 12.07 ms | **9.42** ms | 16.83 ms | 30.01 ms | 672.47 ms | **24263.33** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 49.35 ms | **9.97** ms | 91.10 ms | 635.29 ms | 1161.14 ms | **126769.00** | 
| `node` (`12.9`) | [restify](https://restify.com) (**8.4**) | 11.33 ms | **10.86** ms | 13.57 ms | 26.68 ms | 266.71 ms | **7260.00** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 10.80 ms | **11.00** ms | 16.02 ms | 20.35 ms | 115.14 ms | **4151.00** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 12.20 ms | **11.85** ms | 17.93 ms | 24.09 ms | 120.83 ms | **4588.33** | 
| `node` (`12.9`) | [moleculer](https://moleculer.services) (**0.13**) | 16.64 ms | **13.32** ms | 24.87 ms | 54.92 ms | 439.85 ms | **15325.67** | 
| `node` (`12.9`) | [hapi](https://hapijs.com) (**18.1**) | 17.76 ms | **14.83** ms | 25.99 ms | 50.45 ms | 540.40 ms | **20156.67** | 
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 23.20 ms | **15.30** ms | 26.48 ms | 257.56 ms | 1562.00 ms | **69641.00** | 
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 18.49 ms | **15.88** ms | 16.65 ms | 71.32 ms | 1110.92 ms | **40238.00** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 19.75 ms | **17.58** ms | 28.25 ms | 44.21 ms | 392.08 ms | **14842.00** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 25.37 ms | **19.04** ms | 44.19 ms | 74.23 ms | 311.65 ms | **16386.33** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 19.50 ms | **19.07** ms | 24.96 ms | 31.76 ms | 62.78 ms | **4212.00** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 26.38 ms | **19.85** ms | 46.75 ms | 81.41 ms | 388.95 ms | **19089.67** | 
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 21.33 ms | **21.29** ms | 22.87 ms | 24.54 ms | 325.13 ms | **11192.67** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.5**) | 24.23 ms | **24.98** ms | 32.77 ms | 37.91 ms | 131.67 ms | **7747.67** | 
| `node` (`12.9`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 25.97 ms | **25.52** ms | 27.09 ms | 28.99 ms | 489.24 ms | **14437.33** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 29.32 ms | **26.46** ms | 49.61 ms | 79.50 ms | 209.56 ms | **15555.67** | 
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 29.29 ms | **29.30** ms | 33.02 ms | 34.67 ms | 89.71 ms | **3612.67** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 30.77 ms | **29.71** ms | 45.72 ms | 69.51 ms | 291.07 ms | **14637.67** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.1**) | 52.56 ms | **46.64** ms | 83.68 ms | 114.89 ms | 187.94 ms | **20868.00** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 75.75 ms | **56.24** ms | 138.50 ms | 215.02 ms | 516.73 ms | **48900.67** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 63.84 ms | **62.81** ms | 89.56 ms | 120.58 ms | 306.35 ms | **23368.67** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 90.78 ms | **68.68** ms | 163.33 ms | 232.75 ms | 542.80 ms | **52463.67** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 69.93 ms | **73.22** ms | 83.62 ms | 90.24 ms | 296.36 ms | **13868.00** | 
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 185.66 ms | **186.58** ms | 217.39 ms | 235.22 ms | 359.31 ms | **24735.33** | 

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
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 420531.00 | **243.07** MB |
| `node` (`12.9`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 371082.00 | **326.60** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 366913.00 | **439.31** MB |
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 324429.67 | **314.91** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 321537.67 | **578.56** MB |
| `go` (`1.12`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 311639.67 | **501.24** MB |
| `go` (`1.12`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 308040.67 | **619.26** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 301395.00 | **783.51** MB |
| `go` (`1.12`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 300581.33 | **482.14** MB |
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 279810.67 | **572.13** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 277047.00 | **268.71** MB |
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 276129.33 | **259.48** MB |
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 267393.33 | **251.48** MB |
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 263943.00 | **280.95** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 254390.33 | **438.97** MB |
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 251373.00 | **410.62** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.1**) | 234455.00 | **135.66** MB |
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 234005.00 | **427.89** MB |
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 231361.00 | **464.96** MB |
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 214763.33 | **350.84** MB |
| `go` (`1.12`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 212359.33 | **530.06** MB |
| `go` (`1.12`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 206459.33 | **275.84** MB |
| `go` (`1.12`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 198002.00 | **263.58** MB |
| `go` (`1.12`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 194540.33 | **259.90** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 192966.67 | **314.19** MB |
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 192164.33 | **242.44** MB |
| `go` (`1.12`) | [violetear](https://violetear.org) (**7.0**) | 188147.00 | **249.36** MB |
| `go` (`1.12`) | [kami](https://github.com/guregu/kami) (**2.2**) | 186464.00 | **247.75** MB |
| `go` (`1.12`) | [echo](https://echo.labstack.com) (**4.1**) | 186058.33 | **326.77** MB |
| `go` (`1.12`) | [gin](https://gin-gonic.com) (**1.4**) | 185575.67 | **325.68** MB |
| `go` (`1.12`) | [beego](https://beego.me) (**1.12**) | 184343.67 | **247.43** MB |
| `go` (`1.12`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 182809.33 | **243.74** MB |
| `go` (`1.12`) | [goroute](https://goroute.github.io) (**0.0**) | 182195.33 | **319.03** MB |
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 173810.33 | **264.93** MB |
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 167594.33 | **333.52** MB |
| `node` (`12.9`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 165972.00 | **248.79** MB |
| `go` (`1.12`) | [air](https://github.com/aofei/air) (**0.12**) | 164032.33 | **342.20** MB |
| `node` (`12.9`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 162650.67 | **243.72** MB |
| `node` (`12.9`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 159852.33 | **239.39** MB |
| `go` (`1.12`) | [gf](https://goframe.org) (**1.8**) | 159467.00 | **241.45** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 148646.33 | **231.87** MB |
| `node` (`12.9`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 147504.00 | **220.93** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 147032.00 | **317.43** MB |
| `node` (`12.9`) | [rayo](https://rayo.js.org) (**1.3**) | 144577.67 | **216.73** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 138944.67 | **325.66** MB |
| `node` (`12.9`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 130589.00 | **195.75** MB |
| `node` (`12.9`) | [fastify](https://fastify.io) (**2.8**) | 120322.33 | **309.04** MB |
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 119452.67 | **112.39** MB |
| `node` (`12.9`) | [foxify](https://foxify.js.org) (**0.1**) | 117713.67 | **247.52** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 109231.33 | **82.15** MB |
| `node` (`12.9`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 108895.00 | **442.71** MB |
| `node` (`12.9`) | [koa](https://koajs.com) (**2.8**) | 108538.00 | **229.90** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 108263.00 | **266.78** MB |
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 108084.67 | **230.59** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 99915.00 | **175.22** MB |
| `node` (`12.9`) | [express](https://expressjs.com) (**4.17**) | 95685.67 | **234.47** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 94398.00 | **468.51** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 93231.00 | **463.63** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 93202.33 | **462.65** MB |
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 91925.33 | **154.06** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 88866.33 | **178.83** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 87797.00 | **436.46** MB |
| `node` (`12.9`) | [restify](https://restify.com) (**8.4**) | 86757.00 | **152.32** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 85288.67 | **443.81** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 83717.67 | **207.65** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 79242.67 | **170.84** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 74675.67 | **389.91** MB |
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 69210.33 | **40.02** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.23**) | 67921.33 | **64.92** MB |
| `rust` (`nightly`) | [rocket](https://rocket.rs) (**0.4**) | 66964.00 | **105.93** MB |
| `node` (`12.9`) | [moleculer](https://moleculer.services) (**0.13**) | 61922.33 | **106.05** MB |
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 61890.00 | **114.65** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 61831.67 | **73.07** MB |
| `node` (`12.9`) | [hapi](https://hapijs.com) (**18.1**) | 61205.33 | **158.73** MB |
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 58242.00 | **110.19** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 52531.67 | **30.37** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 50831.00 | **109.85** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 49166.00 | **129.40** MB |
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 46713.00 | **57.35** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 41010.00 | **76.27** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.5**) | 40418.00 | **91.68** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 39304.33 | **96.91** MB |
| `node` (`12.9`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 37964.00 | **35.54** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 35591.33 | **20.56** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 34179.00 | **60.98** MB |
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 33622.33 | **42.08** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 32005.67 | **61.94** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 30233.67 | **229.08** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 24964.67 | **64.87** MB |
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 20535.00 | **26.30** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.1**) | 18753.33 | **37.47** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 15282.00 | **33.33** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 13887.67 | **41.12** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 13682.00 | **39.67** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 11127.00 | **27.45** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 5902.33 | **37.09** MB |
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 5282.67 | **13.60** MB |
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

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
Last update: 2019-09-08
```
OS: Linux (version: 5.2.11-200.fc30.x86_64, arch: x86_64)
CPU Cores: 12
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: swifter (swift)


:four: roda (ruby)


:five: agoo-c (c)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 0.06 ms | **0.06** ms | 0.10 ms | 0.12 ms | 3.89 ms | **52.33** | 
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 0.41 ms | **0.18** ms | 1.01 ms | 2.87 ms | 15.52 ms | **599.00** | 
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 4.73 ms | **0.89** ms | 14.61 ms | 15.05 ms | 1548.31 ms | **18113.00** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.23**) | 2.71 ms | **1.31** ms | 7.05 ms | 13.35 ms | 30.45 ms | **3164.67** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 2.72 ms | **1.36** ms | 6.27 ms | 14.14 ms | 154.93 ms | **4211.33** | 
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 2.69 ms | **1.74** ms | 6.67 ms | 12.38 ms | 28.02 ms | **2946.33** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 3.17 ms | **1.79** ms | 7.38 ms | 16.99 ms | 39.37 ms | **3606.67** | 
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 2.53 ms | **1.95** ms | 5.38 ms | 11.39 ms | 29.27 ms | **2335.33** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 3.02 ms | **2.12** ms | 7.34 ms | 13.41 ms | 29.97 ms | **3207.67** | 
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 3.00 ms | **2.18** ms | 5.68 ms | 12.82 ms | 207.66 ms | **3553.00** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 3.61 ms | **2.20** ms | 8.52 ms | 20.06 ms | 106.94 ms | **4587.67** | 
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 3.04 ms | **2.22** ms | 5.71 ms | 13.12 ms | 106.68 ms | **3086.00** | 
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 3.07 ms | **2.23** ms | 5.72 ms | 13.24 ms | 155.96 ms | **3706.00** | 
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 3.08 ms | **2.23** ms | 6.27 ms | 15.05 ms | 54.32 ms | **3108.00** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 33.54 ms | **2.38** ms | 111.37 ms | 308.92 ms | 914.45 ms | **65854.00** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 3.63 ms | **2.44** ms | 8.79 ms | 16.22 ms | 37.90 ms | **3856.00** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 12.73 ms | **2.69** ms | 6.97 ms | 317.15 ms | 1683.88 ms | **82863.67** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 3.76 ms | **2.72** ms | 7.63 ms | 19.04 ms | 41.41 ms | **3782.33** | 
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 3.81 ms | **2.84** ms | 7.88 ms | 15.70 ms | 33.15 ms | **3263.00** | 
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 3.43 ms | **2.86** ms | 6.40 ms | 12.78 ms | 28.41 ms | **2669.00** | 
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 3.47 ms | **2.96** ms | 5.57 ms | 12.86 ms | 273.68 ms | **6441.67** | 
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 3.58 ms | **2.99** ms | 6.71 ms | 13.99 ms | 32.25 ms | **2861.00** | 
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 3.64 ms | **3.35** ms | 6.19 ms | 12.63 ms | 29.81 ms | **2546.67** | 
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 4.25 ms | **3.44** ms | 7.58 ms | 12.96 ms | 184.00 ms | **3985.33** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 5.19 ms | **3.52** ms | 10.11 ms | 19.57 ms | 215.67 ms | **6195.33** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.1**) | 6.56 ms | **3.64** ms | 9.29 ms | 85.00 ms | 117.39 ms | **13333.00** | 
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 4.00 ms | **3.68** ms | 6.84 ms | 14.72 ms | 30.68 ms | **2809.33** | 
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 4.66 ms | **3.72** ms | 8.48 ms | 18.12 ms | 104.67 ms | **4324.00** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 5.25 ms | **3.99** ms | 12.72 ms | 24.08 ms | 52.01 ms | **5579.00** | 
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 5.10 ms | **3.99** ms | 9.09 ms | 19.40 ms | 223.22 ms | **6986.00** | 
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 4.50 ms | **4.00** ms | 8.27 ms | 17.88 ms | 42.13 ms | **3381.00** | 
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 5.20 ms | **4.03** ms | 9.43 ms | 20.09 ms | 232.17 ms | **7258.33** | 
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 5.25 ms | **4.09** ms | 9.86 ms | 20.56 ms | 220.88 ms | **6184.00** | 
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 5.52 ms | **4.18** ms | 10.09 ms | 22.04 ms | 228.11 ms | **8100.00** | 
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 5.33 ms | **4.19** ms | 10.32 ms | 21.31 ms | 116.54 ms | **4952.33** | 
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 5.18 ms | **4.20** ms | 8.76 ms | 19.48 ms | 284.66 ms | **7036.00** | 
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 5.48 ms | **4.22** ms | 10.69 ms | 22.63 ms | 264.36 ms | **6124.67** | 
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 5.25 ms | **4.24** ms | 9.04 ms | 20.08 ms | 225.14 ms | **6228.67** | 
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 5.28 ms | **4.25** ms | 10.07 ms | 20.45 ms | 152.74 ms | **4404.33** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 6.84 ms | **4.37** ms | 7.20 ms | 112.77 ms | 371.71 ms | **19928.33** | 
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 4.57 ms | **4.51** ms | 6.92 ms | 12.15 ms | 54.55 ms | **2401.67** | 
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.12**) | 6.57 ms | **4.59** ms | 13.61 ms | 28.60 ms | 302.29 ms | **8845.00** | 
| `go` (`1.13`) | [gf](https://goframe.org) (**1.8**) | 6.47 ms | **4.79** ms | 11.92 ms | 25.84 ms | 257.82 ms | **8425.67** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 8.11 ms | **4.88** ms | 18.73 ms | 37.07 ms | 221.61 ms | **8777.00** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 8.31 ms | **4.90** ms | 20.35 ms | 40.63 ms | 230.62 ms | **9708.67** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 102.15 ms | **4.97** ms | 13.82 ms | 2809.83 ms | 5547.78 ms | **477731.00** | 
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 6.51 ms | **5.11** ms | 9.65 ms | 18.97 ms | 247.05 ms | **6228.33** | 
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 6.60 ms | **5.15** ms | 9.64 ms | 18.27 ms | 266.38 ms | **7219.67** | 
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 6.95 ms | **5.38** ms | 9.77 ms | 19.75 ms | 282.83 ms | **8003.67** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 6.22 ms | **5.82** ms | 13.82 ms | 24.30 ms | 56.03 ms | **5800.67** | 
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 7.86 ms | **6.18** ms | 10.85 ms | 25.45 ms | 332.21 ms | **11292.00** | 
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 7.84 ms | **6.28** ms | 11.04 ms | 23.69 ms | 339.35 ms | **10721.67** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 7.67 ms | **6.40** ms | 17.78 ms | 33.63 ms | 67.30 ms | **7718.67** | 
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 11.33 ms | **6.77** ms | 26.26 ms | 60.50 ms | 161.37 ms | **12537.00** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 10.26 ms | **6.83** ms | 21.82 ms | 44.20 ms | 282.83 ms | **11089.33** | 
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 7.13 ms | **6.99** ms | 9.11 ms | 11.85 ms | 25.99 ms | **1642.33** | 
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 8.50 ms | **7.54** ms | 12.51 ms | 24.84 ms | 254.90 ms | **7517.67** | 
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 7.92 ms | **7.83** ms | 10.38 ms | 12.23 ms | 105.24 ms | **2914.00** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 25.77 ms | **7.87** ms | 55.61 ms | 261.00 ms | 669.38 ms | **53992.00** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 25.41 ms | **7.87** ms | 54.10 ms | 262.77 ms | 521.73 ms | **52825.00** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 24.35 ms | **7.93** ms | 51.18 ms | 246.77 ms | 539.06 ms | **50069.00** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 28.21 ms | **8.10** ms | 55.30 ms | 308.78 ms | 494.67 ms | **61479.33** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 10.52 ms | **8.20** ms | 15.53 ms | 38.32 ms | 444.46 ms | **13639.67** | 
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 9.82 ms | **8.45** ms | 14.17 ms | 25.36 ms | 366.80 ms | **11610.33** | 
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 9.18 ms | **8.51** ms | 13.89 ms | 23.99 ms | 290.24 ms | **9248.67** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 26.69 ms | **8.54** ms | 56.31 ms | 272.77 ms | 490.18 ms | **54802.00** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 9.98 ms | **8.63** ms | 18.34 ms | 33.55 ms | 260.59 ms | **8476.67** | 
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 9.95 ms | **8.68** ms | 15.18 ms | 25.14 ms | 272.01 ms | **8041.00** | 
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 10.75 ms | **8.73** ms | 14.99 ms | 35.26 ms | 478.07 ms | **17779.67** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 12.10 ms | **8.96** ms | 22.29 ms | 45.20 ms | 287.84 ms | **10570.67** | 
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 11.67 ms | **9.11** ms | 17.13 ms | 38.24 ms | 374.76 ms | **13828.67** | 
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 11.99 ms | **9.41** ms | 16.82 ms | 32.71 ms | 671.70 ms | **24183.33** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 50.62 ms | **9.97** ms | 95.08 ms | 659.83 ms | 1154.32 ms | **131891.33** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 10.75 ms | **10.02** ms | 16.88 ms | 21.83 ms | 45.25 ms | **4680.67** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 12.03 ms | **11.76** ms | 17.91 ms | 24.37 ms | 48.38 ms | **4708.33** | 
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 12.47 ms | **11.82** ms | 15.23 ms | 29.58 ms | 265.55 ms | **7224.00** | 
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 19.69 ms | **13.80** ms | 25.43 ms | 176.09 ms | 624.26 ms | **32880.33** | 
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 18.79 ms | **15.58** ms | 27.94 ms | 67.71 ms | 555.69 ms | **20612.33** | 
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 20.89 ms | **15.72** ms | 27.62 ms | 120.71 ms | 1274.87 ms | **47128.67** | 
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 18.48 ms | **16.43** ms | 17.22 ms | 31.82 ms | 867.75 ms | **33907.67** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 27.71 ms | **17.61** ms | 56.80 ms | 89.06 ms | 312.18 ms | **22729.33** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 26.48 ms | **17.75** ms | 53.12 ms | 100.40 ms | 431.71 ms | **22717.00** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 18.86 ms | **17.91** ms | 25.63 ms | 35.64 ms | 456.49 ms | **11431.33** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 19.07 ms | **18.64** ms | 24.37 ms | 30.55 ms | 87.08 ms | **4060.00** | 
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 20.57 ms | **20.43** ms | 22.67 ms | 24.30 ms | 244.52 ms | **6888.33** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 24.05 ms | **24.40** ms | 34.60 ms | 47.53 ms | 120.97 ms | **9798.67** | 
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 26.54 ms | **25.39** ms | 27.12 ms | 30.11 ms | 780.36 ms | **23178.33** | 
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | 26.30 ms | **25.72** ms | 32.27 ms | 38.30 ms | 162.49 ms | **5162.33** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 28.72 ms | **25.76** ms | 49.06 ms | 80.14 ms | 341.13 ms | **17296.67** | 
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 27.29 ms | **26.01** ms | 35.25 ms | 36.86 ms | 51.24 ms | **4373.00** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 29.68 ms | **27.62** ms | 44.90 ms | 67.66 ms | 320.46 ms | **13894.67** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.1**) | 50.85 ms | **49.63** ms | 76.29 ms | 109.71 ms | 180.15 ms | **21157.67** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 76.10 ms | **51.60** ms | 164.00 ms | 225.32 ms | 542.99 ms | **54910.67** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 62.54 ms | **59.23** ms | 85.55 ms | 103.31 ms | 357.41 ms | **18262.00** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 102.21 ms | **66.73** ms | 236.48 ms | 320.99 ms | 638.60 ms | **78186.00** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 67.94 ms | **67.65** ms | 82.00 ms | 90.31 ms | 456.99 ms | **17226.33** | 
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 182.59 ms | **182.44** ms | 217.25 ms | 246.22 ms | 277.60 ms | **27481.33** | 

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
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 428211.00 | **247.51** MB |
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 384874.00 | **338.57** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 370396.33 | **443.42** MB |
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 337749.33 | **327.95** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 333019.33 | **598.92** MB |
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 319920.00 | **514.44** MB |
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 318313.67 | **640.75** MB |
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 314531.33 | **505.01** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 306759.67 | **797.30** MB |
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 287036.33 | **588.01** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 285828.00 | **277.22** MB |
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 281829.00 | **264.92** MB |
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 274273.67 | **257.75** MB |
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 270987.67 | **288.46** MB |
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 258259.00 | **422.17** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 257894.00 | **445.05** MB |
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 240972.00 | **440.97** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.1**) | 239306.33 | **138.53** MB |
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 236700.00 | **475.08** MB |
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 226875.67 | **578.82** MB |
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 223355.00 | **364.91** MB |
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 217017.00 | **289.77** MB |
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 204841.33 | **273.05** MB |
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 202995.00 | **271.21** MB |
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 202178.33 | **255.11** MB |
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 197312.33 | **345.79** MB |
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 197300.33 | **261.45** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 196786.00 | **320.38** MB |
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 196527.33 | **261.01** MB |
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 195743.00 | **343.11** MB |
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 195731.67 | **262.69** MB |
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 193783.33 | **339.88** MB |
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 191216.67 | **254.77** MB |
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 182194.00 | **277.26** MB |
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 179111.33 | **356.05** MB |
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.12**) | 171014.00 | **356.61** MB |
| `go` (`1.13`) | [gf](https://goframe.org) (**1.8**) | 166204.67 | **251.69** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 153839.33 | **239.95** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 151244.00 | **326.63** MB |
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 150902.67 | **226.26** MB |
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 149223.00 | **223.70** MB |
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 144736.00 | **216.79** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 141250.33 | **331.14** MB |
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 133609.00 | **200.13** MB |
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 131484.33 | **196.91** MB |
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 122065.00 | **114.84** MB |
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 117758.67 | **176.46** MB |
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 111381.33 | **289.94** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 110999.33 | **83.54** MB |
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 110289.67 | **235.31** MB |
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 109774.00 | **230.57** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 109516.33 | **269.79** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 103209.67 | **180.95** MB |
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 101077.67 | **410.99** MB |
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 100789.33 | **213.38** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 96043.33 | **477.19** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 95655.67 | **475.61** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 95586.00 | **475.00** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 92148.67 | **458.25** MB |
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 92087.67 | **154.26** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 90374.67 | **181.51** MB |
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 89223.00 | **218.68** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 88253.33 | **459.32** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 85837.00 | **212.93** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 80479.00 | **173.51** MB |
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 79410.67 | **139.32** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 76395.67 | **398.65** MB |
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 70584.00 | **40.84** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.23**) | 70489.33 | **67.34** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 63231.00 | **74.76** MB |
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 60492.67 | **103.69** MB |
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 60082.67 | **111.30** MB |
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 57989.67 | **150.35** MB |
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 56058.67 | **106.05** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 52652.00 | **30.38** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 52434.67 | **113.33** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 50265.33 | **132.29** MB |
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 48194.00 | **59.05** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 42031.33 | **78.19** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 41101.67 | **93.24** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 40089.67 | **98.81** MB |
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 38362.33 | **35.91** MB |
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | 36513.00 | **84.00** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 36412.00 | **21.05** MB |
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 36121.00 | **45.16** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 34794.33 | **62.09** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 32688.00 | **63.26** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 30779.67 | **233.20** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 25021.67 | **65.01** MB |
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 20181.00 | **25.79** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.1**) | 19505.67 | **38.96** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 15519.00 | **33.87** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 14288.67 | **41.50** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 14194.67 | **42.01** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 11031.00 | **27.06** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 5994.33 | **37.68** MB |
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 5357.00 | **13.84** MB |
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

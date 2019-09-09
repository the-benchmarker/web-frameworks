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
Last update: 2019-09-09
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
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 0.07 ms | **0.06** ms | 0.12 ms | 0.15 ms | 3.86 ms | **56.67** | 
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 0.49 ms | **0.23** ms | 1.20 ms | 2.92 ms | 16.32 ms | **637.00** | 
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 4.73 ms | **0.89** ms | 14.65 ms | 15.34 ms | 2523.30 ms | **34277.67** | 
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 3.11 ms | **0.95** ms | 8.50 ms | 18.05 ms | 64.28 ms | **4147.00** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 2.91 ms | **1.92** ms | 6.47 ms | 12.08 ms | 42.31 ms | **2735.33** | 
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 3.63 ms | **2.01** ms | 8.96 ms | 19.72 ms | 59.90 ms | **4327.67** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 3.97 ms | **2.30** ms | 9.68 ms | 20.05 ms | 46.21 ms | **4432.00** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 3.53 ms | **2.32** ms | 8.81 ms | 16.97 ms | 44.02 ms | **3973.33** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 6.23 ms | **2.56** ms | 10.91 ms | 77.85 ms | 181.20 ms | **13581.67** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 3.86 ms | **2.63** ms | 9.35 ms | 17.26 ms | 39.70 ms | **4104.00** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.23**) | 3.16 ms | **2.64** ms | 7.37 ms | 13.44 ms | 32.46 ms | **3183.00** | 
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 3.72 ms | **2.69** ms | 8.23 ms | 16.95 ms | 41.06 ms | **3664.67** | 
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 3.62 ms | **2.74** ms | 6.44 ms | 14.12 ms | 160.46 ms | **4083.67** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 5.16 ms | **2.97** ms | 7.78 ms | 21.39 ms | 567.21 ms | **19720.67** | 
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 4.04 ms | **3.08** ms | 8.45 ms | 16.68 ms | 37.45 ms | **3487.33** | 
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 3.94 ms | **3.08** ms | 7.15 ms | 15.47 ms | 98.86 ms | **3162.67** | 
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 3.94 ms | **3.09** ms | 7.10 ms | 14.70 ms | 157.31 ms | **3944.67** | 
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 3.85 ms | **3.13** ms | 7.23 ms | 16.16 ms | 149.59 ms | **3794.33** | 
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 4.16 ms | **3.59** ms | 7.79 ms | 15.47 ms | 32.59 ms | **3097.67** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 4.47 ms | **3.79** ms | 8.80 ms | 19.03 ms | 76.93 ms | **3848.33** | 
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 4.69 ms | **3.84** ms | 9.44 ms | 18.34 ms | 52.10 ms | **3828.67** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.1**) | 5.96 ms | **3.89** ms | 9.14 ms | 72.72 ms | 136.78 ms | **10969.67** | 
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 4.72 ms | **4.03** ms | 9.38 ms | 17.68 ms | 41.08 ms | **3715.33** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 5.82 ms | **4.30** ms | 11.03 ms | 20.39 ms | 161.55 ms | **5629.67** | 
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 4.96 ms | **4.35** ms | 9.34 ms | 18.20 ms | 47.65 ms | **3616.00** | 
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 5.00 ms | **4.46** ms | 9.22 ms | 18.55 ms | 37.65 ms | **3590.33** | 
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 6.18 ms | **4.95** ms | 11.38 ms | 27.18 ms | 110.97 ms | **5325.33** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 8.06 ms | **4.96** ms | 8.62 ms | 131.83 ms | 311.85 ms | **21097.67** | 
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 5.71 ms | **4.99** ms | 10.32 ms | 19.76 ms | 66.92 ms | **3890.67** | 
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 5.96 ms | **5.23** ms | 10.73 ms | 20.32 ms | 62.32 ms | **3991.00** | 
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 6.85 ms | **5.24** ms | 13.13 ms | 29.99 ms | 105.01 ms | **5670.00** | 
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 7.03 ms | **5.24** ms | 12.70 ms | 31.21 ms | 347.59 ms | **9604.67** | 
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 6.90 ms | **5.32** ms | 13.11 ms | 30.24 ms | 75.45 ms | **5700.33** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 6.82 ms | **5.34** ms | 15.94 ms | 30.68 ms | 76.16 ms | **6980.67** | 
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 6.94 ms | **5.44** ms | 12.36 ms | 30.04 ms | 133.95 ms | **6060.33** | 
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 7.08 ms | **5.48** ms | 12.42 ms | 30.17 ms | 183.61 ms | **7353.33** | 
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 7.95 ms | **5.60** ms | 16.76 ms | 36.79 ms | 134.53 ms | **7567.67** | 
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 8.04 ms | **5.64** ms | 15.49 ms | 37.55 ms | 354.75 ms | **10858.00** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 210.28 ms | **5.72** ms | 67.61 ms | 5048.72 ms | 7926.40 ms | **863699.00** | 
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 7.78 ms | **5.73** ms | 15.35 ms | 35.37 ms | 134.07 ms | **6746.33** | 
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 8.10 ms | **5.74** ms | 16.47 ms | 38.69 ms | 145.98 ms | **7934.00** | 
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.12**) | 8.84 ms | **5.99** ms | 19.29 ms | 40.72 ms | 141.16 ms | **8215.67** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 7.16 ms | **6.36** ms | 16.28 ms | 29.39 ms | 66.28 ms | **6956.00** | 
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 8.24 ms | **6.71** ms | 12.93 ms | 26.32 ms | 294.09 ms | **9348.67** | 
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 8.40 ms | **6.77** ms | 12.95 ms | 26.35 ms | 349.60 ms | **11392.00** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 9.33 ms | **6.99** ms | 22.48 ms | 44.38 ms | 113.53 ms | **10134.00** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 10.31 ms | **7.09** ms | 21.24 ms | 45.54 ms | 177.49 ms | **9827.00** | 
| `go` (`1.13`) | [gf](https://goframe.org) (**1.8**) | 10.28 ms | **7.30** ms | 20.45 ms | 47.34 ms | 200.11 ms | **9561.00** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 9.47 ms | **7.35** ms | 20.65 ms | 40.98 ms | 212.31 ms | **9717.67** | 
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 7.82 ms | **7.54** ms | 10.09 ms | 13.66 ms | 89.15 ms | **2683.67** | 
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 9.12 ms | **7.59** ms | 14.37 ms | 29.76 ms | 342.35 ms | **11415.33** | 
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 10.57 ms | **8.07** ms | 16.17 ms | 48.34 ms | 443.65 ms | **17502.00** | 
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 12.98 ms | **8.40** ms | 29.54 ms | 65.26 ms | 174.33 ms | **13681.33** | 
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 10.46 ms | **8.42** ms | 16.16 ms | 34.80 ms | 443.43 ms | **15038.33** | 
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 12.61 ms | **8.68** ms | 17.81 ms | 78.20 ms | 617.84 ms | **26821.00** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 31.87 ms | **9.39** ms | 61.88 ms | 349.17 ms | 497.94 ms | **69693.33** | 
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 12.41 ms | **9.56** ms | 18.40 ms | 59.47 ms | 424.99 ms | **17193.33** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 30.66 ms | **9.56** ms | 62.40 ms | 324.73 ms | 543.74 ms | **64753.67** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 31.30 ms | **9.57** ms | 65.79 ms | 325.78 ms | 562.91 ms | **64648.00** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 40.15 ms | **9.74** ms | 119.66 ms | 298.52 ms | 853.44 ms | **64664.67** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 31.13 ms | **9.87** ms | 63.61 ms | 321.80 ms | 624.55 ms | **64593.00** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 13.41 ms | **9.91** ms | 21.07 ms | 47.03 ms | 627.28 ms | **22380.33** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 31.36 ms | **10.10** ms | 61.42 ms | 328.74 ms | 646.55 ms | **66133.00** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 14.97 ms | **10.23** ms | 31.80 ms | 67.88 ms | 211.76 ms | **14146.67** | 
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 12.40 ms | **10.36** ms | 19.58 ms | 37.91 ms | 345.48 ms | **11118.33** | 
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 13.08 ms | **10.45** ms | 20.25 ms | 40.96 ms | 428.10 ms | **14443.00** | 
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 13.85 ms | **10.49** ms | 19.88 ms | 84.35 ms | 489.55 ms | **21131.00** | 
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 14.01 ms | **10.59** ms | 20.30 ms | 41.95 ms | 716.41 ms | **24995.33** | 
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 10.99 ms | **10.80** ms | 14.05 ms | 18.67 ms | 53.29 ms | **2691.00** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 13.43 ms | **11.07** ms | 23.36 ms | 43.84 ms | 639.15 ms | **21200.33** | 
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 13.39 ms | **11.33** ms | 19.90 ms | 38.84 ms | 439.21 ms | **14836.00** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 16.43 ms | **11.85** ms | 34.77 ms | 63.95 ms | 224.43 ms | **13456.67** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 58.93 ms | **12.46** ms | 92.95 ms | 746.72 ms | 1340.82 ms | **148200.67** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 14.18 ms | **12.64** ms | 23.31 ms | 41.38 ms | 86.28 ms | **7586.00** | 
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 14.39 ms | **12.86** ms | 17.97 ms | 38.09 ms | 296.12 ms | **9001.33** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 15.97 ms | **13.80** ms | 27.28 ms | 48.69 ms | 115.80 ms | **9033.67** | 
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 22.46 ms | **17.07** ms | 32.93 ms | 105.31 ms | 672.41 ms | **27028.33** | 
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 19.80 ms | **18.01** ms | 19.30 ms | 30.38 ms | 735.42 ms | **29244.67** | 
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 27.76 ms | **18.49** ms | 35.50 ms | 267.97 ms | 1101.35 ms | **58342.67** | 
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 28.04 ms | **19.59** ms | 34.62 ms | 273.41 ms | 1350.61 ms | **70385.33** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 25.60 ms | **23.14** ms | 41.55 ms | 75.44 ms | 166.84 ms | **13801.00** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 26.09 ms | **23.56** ms | 38.16 ms | 59.85 ms | 98.38 ms | **9198.33** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 35.94 ms | **24.38** ms | 78.77 ms | 127.94 ms | 295.73 ms | **27464.00** | 
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 26.97 ms | **26.62** ms | 31.25 ms | 37.40 ms | 260.13 ms | **6616.33** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 33.96 ms | **26.78** ms | 58.59 ms | 111.99 ms | 340.09 ms | **21906.00** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 32.93 ms | **30.12** ms | 55.42 ms | 93.06 ms | 169.22 ms | **17712.33** | 
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 31.82 ms | **30.76** ms | 35.16 ms | 42.79 ms | 590.12 ms | **15549.67** | 
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | 35.18 ms | **32.03** ms | 56.85 ms | 86.24 ms | 148.42 ms | **16775.67** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 41.45 ms | **35.15** ms | 70.79 ms | 128.51 ms | 233.59 ms | **24081.00** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 41.88 ms | **35.83** ms | 79.41 ms | 129.72 ms | 200.77 ms | **27565.33** | 
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 45.73 ms | **45.39** ms | 50.51 ms | 60.13 ms | 86.05 ms | **4632.00** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.1**) | 63.19 ms | **56.51** ms | 101.54 ms | 174.91 ms | 248.26 ms | **30864.67** | 
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | 261.50 ms | **57.29** ms | 536.95 ms | 3308.34 ms | 5026.34 ms | **647257.67** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 97.57 ms | **71.29** ms | 196.42 ms | 318.26 ms | 670.52 ms | **64360.00** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 76.97 ms | **74.53** ms | 114.70 ms | 165.51 ms | 238.86 ms | **29884.67** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 117.65 ms | **87.14** ms | 242.59 ms | 412.51 ms | 940.65 ms | **77489.00** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 96.18 ms | **88.52** ms | 142.74 ms | 215.67 ms | 402.80 ms | **34825.33** | 
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 163.65 ms | **163.64** ms | 196.42 ms | 219.90 ms | 256.55 ms | **25533.67** | 

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
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 362655.67 | **209.83** MB |
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 333710.67 | **293.85** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 294897.00 | **353.14** MB |
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 283160.33 | **274.95** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 274136.33 | **493.67** MB |
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 261822.00 | **526.87** MB |
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 258823.00 | **529.14** MB |
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 255100.67 | **239.88** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 251702.00 | **654.32** MB |
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 239732.67 | **386.95** MB |
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 235372.00 | **384.26** MB |
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 235065.67 | **379.02** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.1**) | 225790.67 | **130.66** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 222801.00 | **216.33** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 219933.33 | **379.26** MB |
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 215613.00 | **229.93** MB |
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 210212.67 | **197.74** MB |
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 199406.33 | **364.99** MB |
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 199182.00 | **325.74** MB |
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 194741.33 | **391.34** MB |
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 171696.67 | **216.44** MB |
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 170456.33 | **435.01** MB |
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 168606.00 | **224.44** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 168186.00 | **273.89** MB |
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 167705.00 | **254.96** MB |
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 158620.67 | **315.41** MB |
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 155299.00 | **206.03** MB |
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 154653.33 | **207.80** MB |
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 153962.67 | **204.66** MB |
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 149046.67 | **199.13** MB |
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 148627.33 | **197.73** MB |
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 139081.33 | **186.00** MB |
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 138853.00 | **243.70** MB |
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 136637.00 | **239.88** MB |
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 135970.33 | **238.72** MB |
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.12**) | 129006.33 | **269.10** MB |
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 125626.33 | **188.38** MB |
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 124572.33 | **186.64** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 118986.00 | **256.99** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 117972.00 | **183.81** MB |
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 114602.33 | **171.81** MB |
| `go` (`1.13`) | [gf](https://goframe.org) (**1.8**) | 107574.33 | **162.88** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 106588.67 | **250.04** MB |
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 106440.33 | **159.51** MB |
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 103529.33 | **155.13** MB |
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 97706.67 | **146.52** MB |
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 94805.33 | **202.29** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 91473.00 | **67.69** MB |
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 89318.67 | **83.98** MB |
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 87106.67 | **183.21** MB |
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 87098.33 | **228.47** MB |
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 82526.33 | **335.36** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 81380.67 | **404.63** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 81191.67 | **142.41** MB |
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 80671.33 | **170.80** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 79334.00 | **394.58** MB |
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 78506.67 | **131.44** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 78280.67 | **389.11** MB |
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 77323.67 | **189.52** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 77320.67 | **190.60** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 77041.00 | **401.16** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 76657.00 | **381.12** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 70412.33 | **141.70** MB |
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 69626.00 | **122.26** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 66972.67 | **166.13** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 63209.00 | **136.13** MB |
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 61463.00 | **35.56** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 60515.33 | **315.99** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.23**) | 60362.67 | **57.62** MB |
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 54514.67 | **101.05** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 53983.33 | **63.78** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 49323.00 | **28.49** MB |
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 47936.67 | **82.06** MB |
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 47072.33 | **122.20** MB |
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 45533.67 | **86.15** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 39586.67 | **85.54** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 37549.00 | **98.83** MB |
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 36517.00 | **44.81** MB |
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 31121.67 | **29.13** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 30793.67 | **57.32** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 30640.67 | **69.54** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 30246.00 | **74.56** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 28416.33 | **16.43** MB |
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | 28133.67 | **64.66** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 27305.33 | **206.87** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 25106.67 | **44.86** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 24543.33 | **47.49** MB |
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 21550.67 | **26.89** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 20477.00 | **53.25** MB |
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 18534.00 | **23.76** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.1**) | 15866.00 | **31.70** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 12797.33 | **27.95** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 10775.00 | **31.28** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 10031.33 | **29.73** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 8810.00 | **21.74** MB |
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 6027.33 | **15.57** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 4853.67 | **30.50** MB |
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | 886.33 | **2.02** MB |
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

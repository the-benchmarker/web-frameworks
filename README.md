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
Last update: 2019-09-23
```
OS: Linux (version: 5.2.15-200.fc30.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: flame (ruby)


:four: rack-routing (ruby)


:five: hanami (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 0.10 ms | **0.10** ms | 0.14 ms | 0.21 ms | 5.42 ms | **78.00** | 
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 0.55 ms | **0.43** ms | 0.94 ms | 3.30 ms | 31.75 ms | **632.67** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 5.20 ms | **0.48** ms | 17.21 ms | 43.10 ms | 107.80 ms | **9390.00** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 3.79 ms | **0.51** ms | 12.76 ms | 30.93 ms | 82.06 ms | **6830.00** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 6.00 ms | **0.55** ms | 19.86 ms | 43.08 ms | 97.88 ms | **9947.33** | 
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 2.54 ms | **0.59** ms | 7.35 ms | 15.72 ms | 46.82 ms | **3591.33** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 7.42 ms | **0.63** ms | 24.53 ms | 54.13 ms | 134.05 ms | **12411.00** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.24**) | 2.77 ms | **0.71** ms | 8.05 ms | 21.28 ms | 89.62 ms | **4607.33** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 3.06 ms | **0.81** ms | 8.56 ms | 19.25 ms | 75.64 ms | **4379.67** | 
| `ruby` (`2.6`) | [grape](https://ruby-grape.org) (**1.2**) | 9.14 ms | **0.89** ms | 28.25 ms | 58.94 ms | 133.93 ms | **13787.33** | 
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 3.23 ms | **0.97** ms | 14.73 ms | 16.15 ms | 517.45 ms | **6431.33** | 
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 4.10 ms | **3.67** ms | 7.99 ms | 13.55 ms | 30.13 ms | **2893.67** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 4.27 ms | **3.73** ms | 9.01 ms | 15.11 ms | 30.96 ms | **3440.00** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 4.74 ms | **4.15** ms | 9.45 ms | 16.46 ms | 50.33 ms | **3667.33** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 12.26 ms | **4.17** ms | 45.15 ms | 87.28 ms | 228.32 ms | **21164.33** | 
| `nim` (`0.20`) | [httpbeast](https://github.com/dom96/httpbeast) (**0.2**) | 5.25 ms | **4.43** ms | 9.98 ms | 17.71 ms | 52.43 ms | **3841.00** | 
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 5.01 ms | **4.47** ms | 9.14 ms | 15.40 ms | 58.86 ms | **3281.67** | 
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 5.13 ms | **4.68** ms | 7.95 ms | 14.12 ms | 39.32 ms | **2412.33** | 
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 5.20 ms | **4.68** ms | 8.02 ms | 14.31 ms | 100.83 ms | **3364.33** | 
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 5.21 ms | **4.73** ms | 8.13 ms | 14.68 ms | 46.83 ms | **2527.33** | 
| `crystal` (`0.29`) | [toro](https://github.com/soveran/toro) (**0.4**) | 5.83 ms | **5.03** ms | 9.94 ms | 16.47 ms | 46.87 ms | **3262.00** | 
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 5.94 ms | **5.15** ms | 10.20 ms | 16.68 ms | 37.31 ms | **3322.67** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 10.47 ms | **5.17** ms | 10.57 ms | 181.38 ms | 1010.54 ms | **48548.33** | 
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 6.16 ms | **5.29** ms | 10.54 ms | 17.53 ms | 42.89 ms | **3444.67** | 
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 6.31 ms | **5.34** ms | 11.17 ms | 18.55 ms | 39.12 ms | **3725.33** | 
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 6.29 ms | **5.46** ms | 10.36 ms | 16.99 ms | 39.16 ms | **3244.67** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 6.23 ms | **5.54** ms | 10.40 ms | 17.30 ms | 79.87 ms | **3524.33** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 34.50 ms | **5.60** ms | 107.08 ms | 267.32 ms | 745.90 ms | **58156.00** | 
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 6.88 ms | **5.65** ms | 11.57 ms | 19.60 ms | 288.64 ms | **6413.33** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.11**) | 8.89 ms | **6.32** ms | 14.28 ms | 78.30 ms | 128.64 ms | **11560.00** | 
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 7.20 ms | **6.32** ms | 11.91 ms | 19.38 ms | 50.40 ms | **3699.67** | 
| `nim` (`0.20`) | [jester](https://github.com/dom96/jester) (**0.4**) | 6.99 ms | **6.41** ms | 10.50 ms | 16.49 ms | 45.90 ms | **2943.00** | 
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 7.81 ms | **6.64** ms | 13.24 ms | 23.71 ms | 117.93 ms | **4879.00** | 
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 7.69 ms | **6.74** ms | 12.74 ms | 22.13 ms | 46.74 ms | **4111.00** | 
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 8.32 ms | **7.15** ms | 13.22 ms | 28.63 ms | 178.69 ms | **5865.67** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 8.50 ms | **7.38** ms | 12.53 ms | 22.14 ms | 244.62 ms | **8822.33** | 
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 8.81 ms | **7.39** ms | 14.54 ms | 31.62 ms | 221.57 ms | **6393.67** | 
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 8.93 ms | **7.58** ms | 14.61 ms | 31.72 ms | 118.69 ms | **5680.00** | 
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 9.34 ms | **7.60** ms | 15.75 ms | 35.62 ms | 290.46 ms | **8448.00** | 
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 9.65 ms | **7.77** ms | 16.04 ms | 36.72 ms | 277.62 ms | **9612.67** | 
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 9.36 ms | **7.81** ms | 15.76 ms | 33.82 ms | 187.69 ms | **6363.67** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 225.15 ms | **7.85** ms | 155.94 ms | 4983.89 ms | 7803.45 ms | **866046.67** | 
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 9.13 ms | **7.89** ms | 14.56 ms | 32.09 ms | 124.87 ms | **5793.67** | 
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 9.69 ms | **7.95** ms | 16.73 ms | 36.55 ms | 157.80 ms | **6580.00** | 
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 9.76 ms | **8.15** ms | 16.20 ms | 37.03 ms | 179.23 ms | **6982.67** | 
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 9.32 ms | **8.18** ms | 13.97 ms | 31.04 ms | 232.46 ms | **6125.67** | 
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.13**) | 10.94 ms | **8.93** ms | 18.70 ms | 41.88 ms | 188.16 ms | **8270.00** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 13.14 ms | **9.12** ms | 30.48 ms | 54.82 ms | 146.93 ms | **11509.00** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 10.90 ms | **9.15** ms | 13.95 ms | 25.21 ms | 575.89 ms | **19603.00** | 
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 12.75 ms | **9.57** ms | 17.44 ms | 57.54 ms | 549.05 ms | **23309.00** | 
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 13.64 ms | **9.63** ms | 17.48 ms | 110.48 ms | 616.82 ms | **29051.00** | 
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 12.82 ms | **9.91** ms | 17.98 ms | 59.81 ms | 483.80 ms | **19904.67** | 
| `go` (`1.13`) | [gf](https://goframe.org) (**1.9**) | 11.71 ms | **9.95** ms | 18.59 ms | 41.41 ms | 252.51 ms | **8592.00** | 
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 10.39 ms | **10.08** ms | 13.67 ms | 18.11 ms | 107.76 ms | **3072.00** | 
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | 39.28 ms | **10.21** ms | 69.49 ms | 701.96 ms | 2618.57 ms | **144404.67** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 12.46 ms | **10.76** ms | 23.62 ms | 43.05 ms | 241.57 ms | **9890.00** | 
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 14.44 ms | **10.91** ms | 20.01 ms | 67.66 ms | 602.98 ms | **24997.00** | 
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 17.99 ms | **11.17** ms | 20.60 ms | 237.96 ms | 867.74 ms | **47368.00** | 
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 21.16 ms | **12.50** ms | 22.64 ms | 310.51 ms | 948.77 ms | **57616.33** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 18.98 ms | **12.61** ms | 40.85 ms | 78.85 ms | 218.59 ms | **16278.67** | 
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 17.32 ms | **12.88** ms | 37.65 ms | 75.12 ms | 163.33 ms | **15986.00** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 17.59 ms | **13.17** ms | 25.02 ms | 89.59 ms | 648.94 ms | **24988.00** | 
| `php` (`7.3`) | [one](https://github.com/lizhichao/one) (**1.8**) | 14.05 ms | **13.50** ms | 22.61 ms | 32.93 ms | 64.92 ms | **6634.33** | 
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 20.22 ms | **13.63** ms | 24.05 ms | 240.65 ms | 855.39 ms | **46682.33** | 
| `php` (`7.3`) | [basicphp](https://github.com/ray-ang/basicphp) (**0.9**) | 39.18 ms | **13.67** ms | 72.63 ms | 390.52 ms | 719.24 ms | **77542.33** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 44.37 ms | **14.07** ms | 83.52 ms | 455.10 ms | 897.54 ms | **90667.00** | 
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 21.04 ms | **14.16** ms | 24.86 ms | 230.12 ms | 990.46 ms | **51721.33** | 
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 19.21 ms | **14.27** ms | 24.90 ms | 134.03 ms | 784.44 ms | **35846.00** | 
| `fsharp` (`7.3`) | [suave](https://https://suave.io) (**2.5**) | 27.41 ms | **14.42** ms | 66.62 ms | 106.53 ms | 417.81 ms | **28421.00** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 41.57 ms | **14.46** ms | 87.16 ms | 423.78 ms | 695.15 ms | **84307.33** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 42.94 ms | **14.75** ms | 84.64 ms | 443.00 ms | 717.87 ms | **87784.00** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 47.50 ms | **14.98** ms | 86.33 ms | 507.96 ms | 823.70 ms | **100532.00** | 
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 23.61 ms | **15.43** ms | 26.23 ms | 297.99 ms | 989.84 ms | **57740.33** | 
| `python` (`3.7`) | [asgineer](https://asgineer.readthedocs.io) (**0.7**) | 16.17 ms | **15.51** ms | 23.02 ms | 32.77 ms | 284.02 ms | **7202.00** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 22.18 ms | **15.58** ms | 29.76 ms | 159.71 ms | 1771.36 ms | **65219.33** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 20.76 ms | **15.61** ms | 42.39 ms | 71.26 ms | 320.46 ms | **16209.00** | 
| `ruby` (`2.6`) | [plezi](https://github.com/boazsegev/plezi) (**0.16**) | 20.59 ms | **15.69** ms | 37.42 ms | 92.89 ms | 197.83 ms | **17028.67** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**6.0**) | 47.93 ms | **16.06** ms | 86.09 ms | 490.87 ms | 811.09 ms | **96287.67** | 
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 24.27 ms | **17.05** ms | 29.20 ms | 255.33 ms | 1035.82 ms | **54480.33** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 18.48 ms | **17.55** ms | 26.30 ms | 46.77 ms | 129.11 ms | **7467.00** | 
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | 19.56 ms | **17.85** ms | 18.39 ms | 42.97 ms | 1026.80 ms | **32072.00** | 
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 32.52 ms | **18.17** ms | 33.12 ms | 552.88 ms | 1831.05 ms | **106165.00** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 21.79 ms | **19.66** ms | 33.69 ms | 60.01 ms | 165.47 ms | **10599.67** | 
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 25.03 ms | **20.04** ms | 26.30 ms | 149.77 ms | 780.77 ms | **35370.00** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.0**) | 77.82 ms | **20.70** ms | 144.47 ms | 919.70 ms | 1388.65 ms | **179998.67** | 
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 20.58 ms | **20.78** ms | 23.14 ms | 25.18 ms | 100.65 ms | **2493.33** | 
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 24.47 ms | **21.92** ms | 25.31 ms | 56.08 ms | 1169.26 ms | **37238.33** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 22.28 ms | **22.20** ms | 27.92 ms | 35.60 ms | 125.61 ms | **5844.00** | 
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 45.43 ms | **26.43** ms | 42.53 ms | 697.61 ms | 1631.47 ms | **112194.67** | 
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 54.16 ms | **26.78** ms | 41.97 ms | 1003.65 ms | 2135.09 ms | **159837.00** | 
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | 28.49 ms | **27.62** ms | 40.92 ms | 56.35 ms | 134.10 ms | **9711.00** | 
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 35.55 ms | **29.18** ms | 46.06 ms | 209.63 ms | 1256.42 ms | **56262.67** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 46.26 ms | **29.23** ms | 108.20 ms | 130.73 ms | 382.32 ms | **33734.33** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 33.94 ms | **30.04** ms | 55.33 ms | 88.63 ms | 153.35 ms | **16719.33** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 41.94 ms | **33.87** ms | 74.73 ms | 119.32 ms | 295.19 ms | **23723.00** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 42.83 ms | **40.52** ms | 65.64 ms | 91.23 ms | 142.04 ms | **16265.67** | 
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 42.43 ms | **41.25** ms | 47.49 ms | 54.36 ms | 451.81 ms | **12610.33** | 
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 42.18 ms | **42.01** ms | 49.50 ms | 56.69 ms | 246.30 ms | **7755.67** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 47.16 ms | **43.52** ms | 76.66 ms | 114.62 ms | 194.69 ms | **21451.67** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 48.97 ms | **45.64** ms | 72.47 ms | 114.79 ms | 213.91 ms | **19545.67** | 
| `php` (`7.3`) | [spiral](https://github.com/spiral/framework) (**2.0**) | 56.68 ms | **56.22** ms | 62.04 ms | 64.40 ms | 327.61 ms | **7863.33** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | 79.84 ms | **80.86** ms | 106.06 ms | 137.94 ms | 492.65 ms | **25290.67** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 103.55 ms | **86.30** ms | 204.75 ms | 240.48 ms | 761.48 ms | **51377.33** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 94.22 ms | **92.02** ms | 132.13 ms | 161.95 ms | 358.67 ms | **29585.67** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 99.55 ms | **99.39** ms | 117.61 ms | 171.12 ms | 570.81 ms | **26923.00** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 141.01 ms | **115.29** ms | 240.05 ms | 384.14 ms | 1159.70 ms | **80585.67** | 
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | 406.24 ms | **119.18** ms | 869.98 ms | 5183.28 ms | 7005.47 ms | **867148.67** | 
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 184.02 ms | **185.97** ms | 220.88 ms | 253.36 ms | 287.63 ms | **31682.00** | 
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 269.18 ms | **206.07** ms | 212.78 ms | 2757.40 ms | 5040.30 ms | **458491.33** | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (sifrr) (node)


:three: (japronto) (python)


:four: (rapidoid) (java)


:five: (drogon) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 229729.33 | **132.92** MB |
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 214798.67 | **189.10** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 200686.67 | **240.08** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 184521.67 | **332.29** MB |
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 184515.00 | **178.94** MB |
| `nim` (`0.20`) | [httpbeast](https://github.com/dom96/httpbeast) (**0.2**) | 177077.33 | **252.26** MB |
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 176093.00 | **283.74** MB |
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 174600.33 | **351.44** MB |
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 173142.33 | **278.29** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 172756.33 | **449.05** MB |
| `crystal` (`0.29`) | [toro](https://github.com/soveran/toro) (**0.4**) | 161549.00 | **152.01** MB |
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 158226.33 | **148.82** MB |
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 153992.67 | **144.82** MB |
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 151835.00 | **161.73** MB |
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 150919.33 | **246.80** MB |
| `nim` (`0.20`) | [jester](https://github.com/dom96/jester) (**0.4**) | 148500.67 | **298.30** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 147104.67 | **142.80** MB |
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 140878.00 | **288.59** MB |
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 132645.00 | **242.60** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 132473.00 | **228.26** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.11**) | 132444.67 | **76.54** MB |
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 125665.33 | **205.49** MB |
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 123123.33 | **314.64** MB |
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 118149.00 | **158.33** MB |
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 112724.67 | **150.86** MB |
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 111612.33 | **149.56** MB |
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 111562.33 | **164.75** MB |
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 108612.67 | **144.53** MB |
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 108135.00 | **189.92** MB |
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 107336.33 | **188.52** MB |
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 107095.00 | **142.15** MB |
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 106858.00 | **187.70** MB |
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 105580.67 | **133.43** MB |
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 104840.67 | **139.96** MB |
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 104175.33 | **140.25** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 95718.00 | **155.96** MB |
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.13**) | 94028.67 | **196.21** MB |
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 89474.00 | **134.12** MB |
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 88617.33 | **132.83** MB |
| `go` (`1.13`) | [gf](https://goframe.org) (**1.9**) | 87367.00 | **148.08** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 86363.67 | **202.15** MB |
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 85540.33 | **128.17** MB |
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 84666.00 | **168.62** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 81241.00 | **126.56** MB |
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 78543.00 | **117.54** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 78231.33 | **168.96** MB |
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | 77541.00 | **138.43** MB |
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 77189.00 | **115.64** MB |
| `php` (`7.3`) | [one](https://github.com/lizhichao/one) (**1.8**) | 70018.67 | **160.84** MB |
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 69931.00 | **104.69** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 67316.33 | **50.82** MB |
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 67040.33 | **142.95** MB |
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 65987.67 | **173.62** MB |
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 65246.33 | **137.24** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 61708.00 | **152.10** MB |
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 61291.67 | **129.76** MB |
| `python` (`3.7`) | [asgineer](https://asgineer.readthedocs.io) (**0.7**) | 60406.67 | **107.81** MB |
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 59267.33 | **240.91** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 58955.00 | **103.48** MB |
| `php` (`7.3`) | [basicphp](https://github.com/ray-ang/basicphp) (**0.9**) | 58119.00 | **288.60** MB |
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | 56443.33 | **101.25** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 55796.33 | **277.29** MB |
| `ruby` (`2.6`) | [plezi](https://github.com/boazsegev/plezi) (**0.16**) | 54981.33 | **117.23** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 54551.00 | **271.14** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 54446.00 | **270.75** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 53855.33 | **108.37** MB |
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 53089.00 | **130.07** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 52158.67 | **259.18** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 51284.00 | **127.10** MB |
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 49804.33 | **28.78** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**6.0**) | 48253.00 | **251.16** MB |
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 48145.67 | **45.28** MB |
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 48136.67 | **82.17** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 46108.67 | **99.33** MB |
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 45899.33 | **80.60** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.24**) | 45715.00 | **43.67** MB |
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 44862.00 | **83.48** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 43945.67 | **115.47** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 41731.67 | **49.29** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.0**) | 37942.00 | **198.30** MB |
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 35163.00 | **91.27** MB |
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | 34501.67 | **79.23** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 33641.33 | **19.42** MB |
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 33479.67 | **56.87** MB |
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 30735.00 | **58.14** MB |
| `fsharp` (`7.3`) | [suave](https://https://suave.io) (**2.5**) | 30630.67 | **61.98** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 29879.33 | **64.44** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 25089.33 | **46.66** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 24623.67 | **14.20** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 24214.00 | **59.72** MB |
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 23435.67 | **28.79** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 23200.67 | **52.58** MB |
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 23110.00 | **21.67** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 21361.67 | **38.09** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 21294.00 | **161.01** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 20448.67 | **39.44** MB |
| `php` (`7.3`) | [spiral](https://github.com/spiral/framework) (**2.0**) | 17283.33 | **29.97** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 17210.33 | **44.64** MB |
| `ruby` (`2.6`) | [grape](https://ruby-grape.org) (**1.2**) | 14075.33 | **8.07** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | 12326.00 | **24.56** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 10417.33 | **22.68** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 9781.67 | **29.08** MB |
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 9686.67 | **12.42** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 9520.00 | **27.62** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 7027.33 | **17.33** MB |
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 5408.33 | **13.93** MB |
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 4769.00 | **5.96** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 3704.67 | **23.33** MB |
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | 649.00 | **1.47** MB |
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

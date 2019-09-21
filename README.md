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
Last update: 2019-09-21
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
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 0.10 ms | **0.10** ms | 0.13 ms | 0.21 ms | 4.98 ms | **76.33** | 
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 0.51 ms | **0.45** ms | 0.85 ms | 2.07 ms | 37.82 ms | **493.00** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 5.25 ms | **0.47** ms | 17.31 ms | 41.57 ms | 97.64 ms | **9187.67** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 3.70 ms | **0.52** ms | 12.02 ms | 28.02 ms | 71.51 ms | **6251.00** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 6.01 ms | **0.54** ms | 20.16 ms | 43.19 ms | 95.82 ms | **10029.00** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 2.93 ms | **0.61** ms | 8.51 ms | 17.96 ms | 51.61 ms | **4134.00** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 7.34 ms | **0.64** ms | 24.22 ms | 52.88 ms | 121.71 ms | **12152.67** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.24**) | 2.70 ms | **0.70** ms | 7.69 ms | 16.38 ms | 47.66 ms | **3744.33** | 
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 2.56 ms | **0.82** ms | 7.08 ms | 14.29 ms | 35.00 ms | **3324.00** | 
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 3.81 ms | **0.98** ms | 14.59 ms | 15.10 ms | 1854.18 ms | **25607.33** | 
| `ruby` (`2.6`) | [grape](https://ruby-grape.org) (**1.2**) | 9.20 ms | **0.99** ms | 28.02 ms | 57.58 ms | 131.74 ms | **13533.00** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 32.19 ms | **2.68** ms | 105.58 ms | 278.26 ms | 739.43 ms | **59732.33** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 4.12 ms | **3.65** ms | 8.30 ms | 14.05 ms | 37.47 ms | **3132.00** | 
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 4.33 ms | **3.68** ms | 8.64 ms | 15.66 ms | 61.56 ms | **3498.67** | 
| `nim` (`0.20`) | [httpbeast](https://github.com/dom96/httpbeast) (**0.2**) | 4.63 ms | **4.13** ms | 8.32 ms | 13.78 ms | 29.33 ms | **2765.33** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 4.66 ms | **4.15** ms | 9.14 ms | 15.22 ms | 32.73 ms | **3363.67** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 6.23 ms | **4.32** ms | 10.13 ms | 63.28 ms | 126.19 ms | **10109.67** | 
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 5.24 ms | **4.56** ms | 9.86 ms | 16.74 ms | 42.51 ms | **3619.00** | 
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 5.17 ms | **4.66** ms | 7.94 ms | 14.14 ms | 97.71 ms | **3143.67** | 
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 5.14 ms | **4.67** ms | 7.85 ms | 13.77 ms | 98.31 ms | **3046.00** | 
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 5.27 ms | **4.75** ms | 8.04 ms | 14.26 ms | 129.93 ms | **3093.67** | 
| `crystal` (`0.29`) | [toro](https://github.com/soveran/toro) (**0.4**) | 5.72 ms | **4.95** ms | 9.79 ms | 15.89 ms | 39.40 ms | **3117.67** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 5.65 ms | **5.04** ms | 9.36 ms | 15.53 ms | 46.60 ms | **2974.67** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 10.57 ms | **5.22** ms | 11.46 ms | 167.89 ms | 1019.00 ms | **46885.67** | 
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 6.32 ms | **5.50** ms | 10.83 ms | 17.75 ms | 54.97 ms | **3621.00** | 
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 6.55 ms | **5.55** ms | 10.96 ms | 18.08 ms | 168.90 ms | **4486.67** | 
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 6.62 ms | **5.72** ms | 11.16 ms | 18.08 ms | 65.06 ms | **3583.67** | 
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 6.63 ms | **5.78** ms | 11.32 ms | 18.47 ms | 46.28 ms | **3716.67** | 
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 6.93 ms | **6.08** ms | 12.02 ms | 19.77 ms | 50.12 ms | **3976.67** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.11**) | 8.56 ms | **6.27** ms | 13.79 ms | 67.65 ms | 122.82 ms | **10573.00** | 
| `nim` (`0.20`) | [jester](https://github.com/dom96/jester) (**0.4**) | 6.99 ms | **6.38** ms | 10.62 ms | 16.99 ms | 42.25 ms | **3027.00** | 
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 7.91 ms | **6.79** ms | 13.21 ms | 23.43 ms | 60.02 ms | **4229.33** | 
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 7.68 ms | **6.82** ms | 12.64 ms | 21.39 ms | 49.98 ms | **4005.00** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 7.74 ms | **6.84** ms | 11.50 ms | 18.40 ms | 286.15 ms | **7783.67** | 
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 7.71 ms | **6.96** ms | 13.04 ms | 21.01 ms | 43.27 ms | **4094.33** | 
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 8.40 ms | **7.06** ms | 13.32 ms | 31.09 ms | 229.80 ms | **7047.00** | 
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 8.91 ms | **7.50** ms | 14.56 ms | 31.58 ms | 225.26 ms | **6527.33** | 
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 8.96 ms | **7.51** ms | 14.73 ms | 33.07 ms | 123.57 ms | **6244.00** | 
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 9.33 ms | **7.71** ms | 15.82 ms | 34.55 ms | 174.91 ms | **6835.33** | 
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 9.47 ms | **7.74** ms | 15.96 ms | 36.28 ms | 242.93 ms | **7831.67** | 
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 9.19 ms | **7.92** ms | 14.53 ms | 32.81 ms | 132.55 ms | **6048.33** | 
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 9.49 ms | **7.94** ms | 16.02 ms | 34.28 ms | 121.24 ms | **6169.67** | 
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 9.61 ms | **8.00** ms | 16.29 ms | 34.70 ms | 191.01 ms | **6707.33** | 
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 9.53 ms | **8.02** ms | 15.86 ms | 34.93 ms | 170.15 ms | **6468.33** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 194.07 ms | **8.13** ms | 39.38 ms | 4750.80 ms | 7879.81 ms | **804311.00** | 
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 9.21 ms | **8.17** ms | 13.55 ms | 28.11 ms | 239.88 ms | **6890.33** | 
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.13**) | 11.05 ms | **8.95** ms | 18.86 ms | 42.78 ms | 286.65 ms | **9293.67** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 11.80 ms | **9.03** ms | 14.02 ms | 75.35 ms | 930.20 ms | **27758.33** | 
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 11.47 ms | **9.35** ms | 17.60 ms | 36.91 ms | 359.59 ms | **12543.33** | 
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 13.52 ms | **9.49** ms | 18.67 ms | 92.87 ms | 603.36 ms | **27151.00** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 12.67 ms | **9.53** ms | 26.52 ms | 50.21 ms | 118.02 ms | **10287.67** | 
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 13.97 ms | **9.73** ms | 18.98 ms | 113.55 ms | 608.63 ms | **28635.33** | 
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | 47.96 ms | **9.88** ms | 82.28 ms | 829.52 ms | 4489.65 ms | **208513.67** | 
| `go` (`1.13`) | [gf](https://goframe.org) (**1.8**) | 11.81 ms | **9.97** ms | 19.26 ms | 44.47 ms | 173.31 ms | **7632.33** | 
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 10.56 ms | **10.23** ms | 13.95 ms | 18.47 ms | 106.80 ms | **3023.00** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 11.78 ms | **10.31** ms | 21.93 ms | 38.28 ms | 298.65 ms | **10165.67** | 
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 16.01 ms | **10.84** ms | 20.77 ms | 131.79 ms | 737.50 ms | **35347.67** | 
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 15.04 ms | **11.09** ms | 21.06 ms | 76.77 ms | 619.77 ms | **27066.33** | 
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 23.43 ms | **12.40** ms | 23.60 ms | 393.08 ms | 1144.39 ms | **71808.67** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 16.61 ms | **12.81** ms | 32.36 ms | 58.86 ms | 163.56 ms | **11990.00** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 16.81 ms | **12.81** ms | 26.49 ms | 62.76 ms | 547.89 ms | **17277.00** | 
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 16.99 ms | **13.22** ms | 35.83 ms | 68.62 ms | 176.45 ms | **14542.33** | 
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 20.31 ms | **13.24** ms | 25.14 ms | 237.57 ms | 861.11 ms | **46182.33** | 
| `php` (`7.3`) | [basicphp](https://github.com/ray-ang/basicphp) (**0.9**) | 39.53 ms | **13.44** ms | 75.25 ms | 394.11 ms | 734.96 ms | **77836.67** | 
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 24.24 ms | **13.80** ms | 26.84 ms | 341.64 ms | 1059.12 ms | **64447.67** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 41.89 ms | **14.14** ms | 78.40 ms | 427.09 ms | 732.93 ms | **83661.67** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 42.88 ms | **14.40** ms | 76.96 ms | 443.79 ms | 685.06 ms | **87009.00** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 21.13 ms | **14.49** ms | 41.92 ms | 80.99 ms | 263.29 ms | **16435.33** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 44.16 ms | **14.53** ms | 79.09 ms | 457.48 ms | 815.82 ms | **90232.67** | 
| `php` (`7.3`) | [one](https://github.com/lizhichao/one) (**1.8**) | 14.96 ms | **14.62** ms | 20.83 ms | 28.03 ms | 148.36 ms | **4700.33** | 
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 20.54 ms | **14.93** ms | 27.03 ms | 149.90 ms | 795.71 ms | **38668.00** | 
| `python` (`3.7`) | [asgineer](https://asgineer.readthedocs.io) (**0.7**) | 16.17 ms | **15.15** ms | 23.44 ms | 37.76 ms | 96.73 ms | **6386.33** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 48.07 ms | **15.17** ms | 93.79 ms | 511.88 ms | 813.88 ms | **101287.33** | 
| `fsharp` (`7.3`) | [suave](https://https://suave.io) (**2.5**) | 27.68 ms | **15.59** ms | 60.64 ms | 75.47 ms | 341.89 ms | **23273.67** | 
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 28.25 ms | **15.62** ms | 31.48 ms | 395.86 ms | 1142.25 ms | **72669.33** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 17.75 ms | **15.64** ms | 27.79 ms | 52.61 ms | 174.58 ms | **9153.67** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 50.46 ms | **15.83** ms | 31.62 ms | 1185.39 ms | 2899.55 ms | **217922.33** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**6.0**) | 49.56 ms | **16.39** ms | 87.67 ms | 507.80 ms | 1035.39 ms | **101619.00** | 
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 28.20 ms | **16.54** ms | 30.83 ms | 433.00 ms | 1236.50 ms | **75906.33** | 
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 25.80 ms | **17.55** ms | 30.41 ms | 272.95 ms | 1280.65 ms | **64027.33** | 
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | 18.06 ms | **18.00** ms | 18.70 ms | 23.56 ms | 303.37 ms | **5793.00** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 20.42 ms | **18.24** ms | 32.44 ms | 57.83 ms | 179.15 ms | **10452.00** | 
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 28.40 ms | **20.50** ms | 34.19 ms | 236.27 ms | 934.32 ms | **49497.67** | 
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 20.60 ms | **20.84** ms | 22.84 ms | 24.61 ms | 161.46 ms | **2831.67** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.0**) | 79.49 ms | **21.64** ms | 157.15 ms | 929.76 ms | 1645.06 ms | **185966.00** | 
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 23.31 ms | **22.05** ms | 24.52 ms | 39.87 ms | 999.35 ms | **27831.67** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 22.31 ms | **22.20** ms | 27.48 ms | 34.34 ms | 51.80 ms | **4272.00** | 
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 49.14 ms | **25.23** ms | 46.88 ms | 825.06 ms | 1769.61 ms | **131823.67** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 38.95 ms | **25.56** ms | 80.35 ms | 100.64 ms | 243.62 ms | **24401.33** | 
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 50.96 ms | **27.17** ms | 51.60 ms | 794.31 ms | 1731.31 ms | **127745.33** | 
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | 29.11 ms | **28.18** ms | 43.19 ms | 63.22 ms | 108.32 ms | **11304.33** | 
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 40.57 ms | **29.34** ms | 43.36 ms | 459.40 ms | 2014.57 ms | **91679.67** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 44.22 ms | **30.94** ms | 85.02 ms | 168.79 ms | 316.77 ms | **30412.67** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 33.00 ms | **31.29** ms | 49.19 ms | 68.90 ms | 136.45 ms | **12061.00** | 
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 38.71 ms | **37.56** ms | 47.59 ms | 54.96 ms | 214.19 ms | **7400.67** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 42.12 ms | **40.36** ms | 59.23 ms | 75.79 ms | 108.66 ms | **12941.33** | 
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 42.41 ms | **41.14** ms | 46.67 ms | 53.68 ms | 475.95 ms | **15273.67** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 44.87 ms | **43.80** ms | 63.33 ms | 91.31 ms | 156.30 ms | **15011.67** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 48.06 ms | **49.93** ms | 63.29 ms | 101.10 ms | 241.27 ms | **16600.00** | 
| `php` (`7.3`) | [spiral](https://github.com/spiral/framework) (**2.0**) | 56.82 ms | **56.51** ms | 62.32 ms | 64.50 ms | 236.48 ms | **5760.33** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | 87.52 ms | **79.64** ms | 146.81 ms | 187.73 ms | 253.84 ms | **37738.67** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 108.69 ms | **85.87** ms | 211.28 ms | 278.07 ms | 715.47 ms | **59710.00** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 94.81 ms | **87.66** ms | 138.19 ms | 169.65 ms | 278.83 ms | **30105.33** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 97.50 ms | **95.85** ms | 125.40 ms | 150.61 ms | 505.55 ms | **26536.67** | 
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | 401.68 ms | **103.71** ms | 650.61 ms | 5337.47 ms | 6921.85 ms | **988735.33** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 133.99 ms | **114.86** ms | 257.34 ms | 301.27 ms | 825.80 ms | **66808.67** | 
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 206.78 ms | **206.79** ms | 236.50 ms | 261.62 ms | 310.12 ms | **23938.33** | 
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 282.57 ms | **209.95** ms | 233.46 ms | 2596.33 ms | 5317.30 ms | **466468.67** | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (sifrr) (node)


:three: (japronto) (python)


:four: (httpbeast) (nim)


:five: (rapidoid) (java)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 230199.00 | **133.17** MB |
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 205517.33 | **180.88** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 200914.33 | **240.35** MB |
| `nim` (`0.20`) | [httpbeast](https://github.com/dom96/httpbeast) (**0.2**) | 200138.33 | **285.14** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 185174.00 | **333.51** MB |
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 175685.33 | **170.38** MB |
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 175477.33 | **282.71** MB |
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 175415.33 | **353.02** MB |
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 172106.33 | **276.73** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 165743.33 | **430.69** MB |
| `crystal` (`0.29`) | [toro](https://github.com/soveran/toro) (**0.4**) | 164562.00 | **154.82** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 162407.67 | **157.41** MB |
| `nim` (`0.20`) | [jester](https://github.com/dom96/jester) (**0.4**) | 149120.67 | **299.97** MB |
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 148981.33 | **140.17** MB |
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 144987.67 | **296.77** MB |
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 143664.33 | **234.92** MB |
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 142662.33 | **134.18** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 140706.33 | **242.78** MB |
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 137323.67 | **146.76** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.11**) | 133499.00 | **77.17** MB |
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 125913.33 | **205.89** MB |
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 124101.67 | **227.13** MB |
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 122459.67 | **312.84** MB |
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 118930.33 | **159.29** MB |
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 115007.00 | **145.14** MB |
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 112684.00 | **150.84** MB |
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 111829.67 | **149.64** MB |
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 110411.33 | **163.37** MB |
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 108631.67 | **144.52** MB |
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 108154.33 | **143.65** MB |
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 108080.67 | **189.77** MB |
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 107837.67 | **189.34** MB |
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 106477.33 | **187.01** MB |
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 105898.00 | **142.53** MB |
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 104970.67 | **140.24** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 96102.33 | **156.36** MB |
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.13**) | 93844.00 | **195.95** MB |
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 89819.33 | **134.67** MB |
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 88893.33 | **133.24** MB |
| `go` (`1.13`) | [gf](https://goframe.org) (**1.8**) | 87282.33 | **148.29** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 87187.33 | **135.71** MB |
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 87043.67 | **173.31** MB |
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 86522.00 | **129.73** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 86193.00 | **201.94** MB |
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | 79038.33 | **140.93** MB |
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 78068.00 | **117.05** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 77746.00 | **168.21** MB |
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 76704.33 | **114.97** MB |
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 69960.67 | **104.89** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 67703.00 | **50.48** MB |
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 66050.33 | **140.88** MB |
| `php` (`7.3`) | [one](https://github.com/lizhichao/one) (**1.8**) | 65495.00 | **150.39** MB |
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 65124.00 | **170.68** MB |
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 64587.33 | **135.80** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 64376.00 | **158.57** MB |
| `python` (`3.7`) | [asgineer](https://asgineer.readthedocs.io) (**0.7**) | 61222.67 | **109.25** MB |
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 58911.67 | **239.28** MB |
| `php` (`7.3`) | [basicphp](https://github.com/ray-ang/basicphp) (**0.9**) | 57878.33 | **287.41** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 57781.67 | **101.35** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 56903.00 | **114.51** MB |
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | 55819.33 | **100.21** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 55519.33 | **275.77** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 54669.33 | **271.47** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 54396.00 | **270.31** MB |
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 53357.67 | **112.94** MB |
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 52977.00 | **129.59** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 51960.33 | **258.31** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 51664.67 | **128.17** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 49442.33 | **106.56** MB |
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 49405.67 | **84.32** MB |
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 49352.33 | **28.54** MB |
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 48161.67 | **45.24** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**6.0**) | 47828.67 | **248.89** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.24**) | 46935.00 | **44.84** MB |
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 44842.33 | **83.30** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 43975.33 | **115.57** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 43409.67 | **51.26** MB |
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 43088.33 | **75.56** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.0**) | 37318.67 | **195.02** MB |
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 35133.33 | **90.97** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 34532.33 | **19.93** MB |
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | 34062.33 | **78.15** MB |
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 31804.33 | **60.02** MB |
| `fsharp` (`7.3`) | [suave](https://https://suave.io) (**2.5**) | 31606.33 | **64.07** MB |
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 31466.67 | **53.93** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 30184.33 | **65.11** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 26848.67 | **49.97** MB |
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 25483.00 | **31.30** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 24949.33 | **61.41** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 24391.67 | **14.06** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 23634.33 | **53.54** MB |
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 23212.33 | **21.79** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 22130.67 | **39.44** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 21303.00 | **161.03** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 20606.67 | **39.71** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 17424.67 | **45.17** MB |
| `php` (`7.3`) | [spiral](https://github.com/spiral/framework) (**2.0**) | 17215.33 | **29.87** MB |
| `ruby` (`2.6`) | [grape](https://ruby-grape.org) (**1.2**) | 13961.00 | **8.01** MB |
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 11665.00 | **14.94** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | 11321.67 | **22.55** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 10444.67 | **22.74** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 9951.33 | **29.35** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 9435.33 | **27.35** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 7492.33 | **18.42** MB |
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 4788.33 | **12.34** MB |
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 4583.00 | **5.73** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 3975.00 | **24.99** MB |
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | 433.33 | **0.99** MB |
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

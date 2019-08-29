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
Last update: 2019-08-29
```
OS: Linux (version: 5.2.9-200.fc30.x86_64, arch: x86_64)
CPU Cores: 4
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: cuba (ruby)


:three: flame (ruby)


:four: iron (rust)


:five: hanami (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 0.04 ms | **0.04** ms | 0.05 ms | 0.06 ms | 3.64 ms | **35.33** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 1.91 ms | **0.16** ms | 1.60 ms | 35.92 ms | 147.91 ms | **6887.67** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 3.15 ms | **0.21** ms | 6.55 ms | 53.56 ms | 155.65 ms | **10261.00** | 
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 0.30 ms | **0.25** ms | 0.48 ms | 1.08 ms | 45.50 ms | **489.00** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 3.52 ms | **0.34** ms | 10.93 ms | 47.68 ms | 159.72 ms | **9616.67** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 4.33 ms | **0.39** ms | 14.87 ms | 55.06 ms | 173.04 ms | **11170.33** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 2.21 ms | **0.57** ms | 2.95 ms | 36.41 ms | 161.12 ms | **7097.00** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.23**) | 1.74 ms | **0.73** ms | 1.30 ms | 31.46 ms | 175.59 ms | **6201.67** | 
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 5.84 ms | **0.75** ms | 14.37 ms | 14.85 ms | 2277.47 ms | **45233.00** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 17.79 ms | **1.01** ms | 62.53 ms | 156.09 ms | 384.79 ms | **34254.33** | 
| `rust` (`nightly`) | [rocket](https://rocket.rs) (**0.4**) | 117.92 ms | **2.77** ms | 7.48 ms | 3585.12 ms | 6595.01 ms | **607295.33** | 
| `node` (`12.9`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 4.55 ms | **3.82** ms | 8.32 ms | 14.64 ms | 36.24 ms | **3003.00** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 4.45 ms | **4.00** ms | 9.18 ms | 17.00 ms | 43.47 ms | **3726.67** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 4.97 ms | **4.59** ms | 9.58 ms | 16.36 ms | 36.21 ms | **3580.33** | 
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 5.54 ms | **4.84** ms | 9.51 ms | 16.34 ms | 31.72 ms | **3127.67** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 10.19 ms | **4.86** ms | 21.31 ms | 84.22 ms | 176.63 ms | **17720.00** | 
| `go` (`1.12`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 5.86 ms | **5.36** ms | 8.91 ms | 15.17 ms | 98.88 ms | **2782.00** | 
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 412.20 ms | **5.42** ms | 1546.54 ms | 5487.16 ms | 6338.54 ms | **1110034.33** | 
| `go` (`1.12`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 6.50 ms | **5.45** ms | 9.11 ms | 16.34 ms | 369.24 ms | **10917.67** | 
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 6.77 ms | **5.65** ms | 10.37 ms | 18.61 ms | 292.12 ms | **7439.67** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 7.03 ms | **5.74** ms | 11.61 ms | 25.49 ms | 287.45 ms | **8321.00** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 20.37 ms | **5.75** ms | 11.82 ms | 519.52 ms | 1852.66 ms | **99747.33** | 
| `python` (`3.6`) | [vibora](https://vibora.io) (**0.0**) | 6.60 ms | **5.95** ms | 12.07 ms | 19.85 ms | 49.11 ms | **4346.33** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 6.32 ms | **6.00** ms | 9.98 ms | 15.97 ms | 53.79 ms | **3156.67** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.9**) | 9.67 ms | **6.27** ms | 14.65 ms | 88.93 ms | 121.52 ms | **13984.67** | 
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 6.92 ms | **6.28** ms | 11.14 ms | 17.40 ms | 32.90 ms | **3152.33** | 
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 7.17 ms | **6.45** ms | 11.66 ms | 18.26 ms | 35.72 ms | **3313.00** | 
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 7.35 ms | **6.53** ms | 12.26 ms | 19.72 ms | 36.56 ms | **3668.67** | 
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 7.94 ms | **6.96** ms | 13.81 ms | 21.00 ms | 35.23 ms | **4012.33** | 
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 129.02 ms | **7.07** ms | 557.98 ms | 1444.46 ms | 2106.74 ms | **328942.67** | 
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 8.35 ms | **7.61** ms | 12.47 ms | 18.77 ms | 84.81 ms | **3379.67** | 
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 8.75 ms | **8.16** ms | 13.92 ms | 21.68 ms | 43.80 ms | **4025.33** | 
| `go` (`1.12`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 9.27 ms | **8.21** ms | 15.89 ms | 31.02 ms | 120.89 ms | **6223.33** | 
| `go` (`1.12`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 12.66 ms | **8.84** ms | 19.85 ms | 123.71 ms | 277.40 ms | **20342.67** | 
| `node` (`12.9`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 13.70 ms | **8.87** ms | 17.10 ms | 106.72 ms | 799.45 ms | **35285.33** | 
| `node` (`12.9`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 13.31 ms | **8.88** ms | 16.76 ms | 115.67 ms | 664.24 ms | **31536.67** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 11.25 ms | **8.93** ms | 12.70 ms | 47.44 ms | 713.19 ms | **26385.00** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 241.02 ms | **8.99** ms | 284.59 ms | 5261.35 ms | 7941.62 ms | **911869.67** | 
| `node` (`12.9`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 14.72 ms | **9.14** ms | 17.78 ms | 169.02 ms | 769.36 ms | **39263.67** | 
| `go` (`1.12`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.0**) | 15.06 ms | **9.34** ms | 25.89 ms | 145.42 ms | 420.40 ms | **26445.67** | 
| `go` (`1.12`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 15.05 ms | **9.44** ms | 27.00 ms | 135.06 ms | 345.65 ms | **23924.00** | 
| `go` (`1.12`) | [echo](https://echo.labstack.com) (**4.1**) | 15.65 ms | **9.75** ms | 29.48 ms | 136.13 ms | 379.96 ms | **25166.00** | 
| `go` (`1.12`) | [goroute](https://goroute.github.io) (**0.0**) | 15.24 ms | **9.84** ms | 29.48 ms | 119.30 ms | 249.00 ms | **21629.00** | 
| `go` (`1.12`) | [gin](https://gin-gonic.com) (**1.4**) | 14.81 ms | **9.84** ms | 26.08 ms | 131.66 ms | 407.96 ms | **24092.00** | 
| `go` (`1.12`) | [violetear](https://violetear.org) (**7.0**) | 14.17 ms | **10.28** ms | 25.82 ms | 93.31 ms | 284.99 ms | **18217.33** | 
| `go` (`1.12`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 16.42 ms | **10.47** ms | 32.76 ms | 131.96 ms | 294.23 ms | **24259.33** | 
| `go` (`1.12`) | [beego](https://beego.me) (**1.12**) | 19.93 ms | **10.61** ms | 45.85 ms | 176.44 ms | 469.24 ms | **34175.67** | 
| `go` (`1.12`) | [kami](https://github.com/guregu/kami) (**2.2**) | 19.81 ms | **10.69** ms | 38.83 ms | 192.17 ms | 403.70 ms | **35066.00** | 
| `node` (`12.9`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 20.67 ms | **10.90** ms | 24.88 ms | 291.70 ms | 1171.58 ms | **61829.33** | 
| `node` (`12.9`) | [rayo](https://rayo.js.org) (**1.3**) | 24.14 ms | **11.39** ms | 24.10 ms | 413.36 ms | 1362.17 ms | **80128.00** | 
| `go` (`1.12`) | [air](https://github.com/aofei/air) (**0.12**) | 19.65 ms | **11.90** ms | 44.25 ms | 142.02 ms | 559.24 ms | **31688.00** | 
| `go` (`1.12`) | [gf](https://goframe.org) (**1.8**) | 17.92 ms | **11.91** ms | 35.58 ms | 127.91 ms | 605.86 ms | **28401.33** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 60.56 ms | **12.48** ms | 169.98 ms | 624.52 ms | 5003.32 ms | **201126.67** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 59.22 ms | **12.59** ms | 187.97 ms | 593.98 ms | 3577.19 ms | **155062.33** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 73.47 ms | **12.61** ms | 223.73 ms | 777.84 ms | 5452.46 ms | **238145.67** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 76.48 ms | **12.80** ms | 218.60 ms | 1009.08 ms | 5418.37 ms | **252260.00** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 16.68 ms | **13.33** ms | 28.94 ms | 60.23 ms | 343.41 ms | **14828.00** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 73.38 ms | **13.88** ms | 209.51 ms | 853.31 ms | 5070.68 ms | **233754.67** | 
| `node` (`12.9`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 34.96 ms | **14.20** ms | 30.80 ms | 739.88 ms | 1772.06 ms | **123451.67** | 
| `node` (`12.9`) | [foxify](https://foxify.js.org) (**0.1**) | 30.28 ms | **14.49** ms | 37.75 ms | 499.10 ms | 1542.61 ms | **91463.67** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 63.93 ms | **14.56** ms | 28.77 ms | 1824.60 ms | 4754.62 ms | **317274.67** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 16.68 ms | **14.62** ms | 31.77 ms | 67.00 ms | 485.58 ms | **22770.00** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 21.67 ms | **14.90** ms | 35.47 ms | 98.74 ms | 570.17 ms | **25860.00** | 
| `node` (`12.9`) | [fastify](https://fastify.io) (**2.7**) | 41.07 ms | **15.06** ms | 34.54 ms | 775.18 ms | 1571.19 ms | **128923.00** | 
| `node` (`12.9`) | [koa](https://koajs.com) (**2.8**) | 36.39 ms | **16.04** ms | 35.06 ms | 680.77 ms | 1542.04 ms | **111579.33** | 
| `node` (`12.9`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 30.86 ms | **16.16** ms | 36.51 ms | 438.91 ms | 1365.26 ms | **79850.33** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 104.38 ms | **16.25** ms | 356.42 ms | 1042.26 ms | 6244.69 ms | **277767.33** | 
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 15.95 ms | **16.37** ms | 17.41 ms | 19.66 ms | 223.54 ms | **4125.00** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 23.33 ms | **16.92** ms | 39.12 ms | 101.38 ms | 471.87 ms | **24197.67** | 
| `node` (`12.9`) | [restify](https://restify.com) (**8.4**) | 24.22 ms | **17.79** ms | 34.68 ms | 113.66 ms | 793.81 ms | **34793.33** | 
| `node` (`12.9`) | [express](https://expressjs.com) (**4.17**) | 45.33 ms | **18.10** ms | 47.12 ms | 873.15 ms | 1811.63 ms | **138974.67** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 22.02 ms | **18.70** ms | 33.29 ms | 68.34 ms | 764.25 ms | **25756.67** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 31.21 ms | **19.27** ms | 59.57 ms | 126.75 ms | 353.29 ms | **28389.33** | 
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 20.37 ms | **19.40** ms | 36.95 ms | 60.74 ms | 177.10 ms | **13053.67** | 
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 95.13 ms | **20.42** ms | 47.62 ms | 2246.41 ms | 5115.53 ms | **385283.33** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 23.05 ms | **20.78** ms | 34.89 ms | 42.95 ms | 276.22 ms | **10514.67** | 
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 25.24 ms | **25.17** ms | 30.32 ms | 39.66 ms | 327.72 ms | **9802.33** | 
| `node` (`12.9`) | [hapi](https://hapijs.com) (**18.1**) | 79.73 ms | **27.32** ms | 50.93 ms | 1603.15 ms | 3042.92 ms | **261463.67** | 
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 27.20 ms | **27.44** ms | 28.93 ms | 30.84 ms | 394.32 ms | **11939.00** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 42.15 ms | **27.79** ms | 85.65 ms | 154.33 ms | 398.55 ms | **31506.33** | 
| `node` (`12.9`) | [moleculer](https://moleculer.services) (**0.13**) | 94.98 ms | **28.10** ms | 55.85 ms | 1794.54 ms | 3170.74 ms | **302251.00** | 
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 29.39 ms | **29.18** ms | 37.03 ms | 45.04 ms | 64.20 ms | **6065.00** | 
| `node` (`12.9`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 35.21 ms | **30.56** ms | 32.88 ms | 215.62 ms | 982.48 ms | **49255.67** | 
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 95.44 ms | **31.80** ms | 57.86 ms | 1894.55 ms | 3850.46 ms | **330251.67** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.36**) | 35.54 ms | **31.88** ms | 48.40 ms | 55.76 ms | 213.69 ms | **9876.00** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.5**) | 41.83 ms | **40.14** ms | 69.11 ms | 80.61 ms | 167.76 ms | **19587.33** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 50.35 ms | **46.64** ms | 70.79 ms | 82.15 ms | 701.95 ms | **23672.33** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 50.40 ms | **46.78** ms | 67.00 ms | 112.27 ms | 657.59 ms | **27478.67** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 56.14 ms | **56.98** ms | 76.55 ms | 117.21 ms | 526.98 ms | **30054.67** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 66.07 ms | **64.28** ms | 94.37 ms | 130.79 ms | 244.10 ms | **22143.67** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.9**) | 89.78 ms | **83.90** ms | 140.09 ms | 171.30 ms | 249.35 ms | **37912.67** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 108.36 ms | **100.91** ms | 160.30 ms | 183.42 ms | 393.23 ms | **35202.00** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 170.77 ms | **123.93** ms | 172.11 ms | 1779.83 ms | 3102.09 ms | **287963.00** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 140.87 ms | **124.22** ms | 140.71 ms | 1017.54 ms | 2581.78 ms | **165005.33** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 192.36 ms | **151.65** ms | 167.60 ms | 1835.72 ms | 3686.67 ms | **291694.00** | 
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 233.21 ms | **155.08** ms | 170.77 ms | 2714.72 ms | 3659.95 ms | **433485.33** | 

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
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 208866.33 | **120.78** MB |
| `node` (`12.9`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 188877.00 | **166.22** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 185973.33 | **222.68** MB |
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 165359.00 | **160.59** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 158415.67 | **285.05** MB |
| `go` (`1.12`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 154413.33 | **310.71** MB |
| `python` (`3.6`) | [vibora](https://vibora.io) (**0.0**) | 153237.00 | **174.01** MB |
| `go` (`1.12`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 151034.33 | **243.39** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 147265.33 | **142.86** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 143197.00 | **247.00** MB |
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 140899.33 | **288.53** MB |
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 138153.00 | **129.81** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 137351.67 | **356.57** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.9**) | 134501.67 | **77.83** MB |
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 134309.67 | **126.15** MB |
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 131893.67 | **140.33** MB |
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 126682.00 | **254.23** MB |
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 123533.33 | **201.83** MB |
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 118267.00 | **169.85** MB |
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 114463.00 | **209.52** MB |
| `go` (`1.12`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 106392.00 | **265.48** MB |
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 103961.67 | **169.96** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 101953.00 | **166.15** MB |
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 101845.00 | **128.44** MB |
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 101550.33 | **202.04** MB |
| `go` (`1.12`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 100113.67 | **134.01** MB |
| `node` (`12.9`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 94087.00 | **141.08** MB |
| `go` (`1.12`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.0**) | 93699.67 | **124.65** MB |
| `go` (`1.12`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 93551.67 | **125.09** MB |
| `node` (`12.9`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 92975.33 | **139.17** MB |
| `node` (`12.9`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 90729.33 | **135.88** MB |
| `go` (`1.12`) | [gin](https://gin-gonic.com) (**1.4**) | 90532.00 | **158.69** MB |
| `go` (`1.12`) | [violetear](https://violetear.org) (**7.0**) | 90320.00 | **119.54** MB |
| `go` (`1.12`) | [goroute](https://goroute.github.io) (**0.0**) | 89848.67 | **157.47** MB |
| `go` (`1.12`) | [echo](https://echo.labstack.com) (**4.1**) | 89171.33 | **156.26** MB |
| `go` (`1.12`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 86520.33 | **115.28** MB |
| `go` (`1.12`) | [kami](https://github.com/guregu/kami) (**2.2**) | 85355.33 | **113.31** MB |
| `go` (`1.12`) | [beego](https://beego.me) (**1.12**) | 84387.33 | **112.99** MB |
| `go` (`1.12`) | [air](https://github.com/aofei/air) (**0.12**) | 76247.67 | **159.05** MB |
| `go` (`1.12`) | [gf](https://goframe.org) (**1.8**) | 75794.33 | **114.68** MB |
| `node` (`12.9`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 72933.67 | **109.29** MB |
| `node` (`12.9`) | [rayo](https://rayo.js.org) (**1.3**) | 72167.67 | **108.14** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 71999.33 | **155.50** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 67734.00 | **105.44** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 63867.00 | **149.72** MB |
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 61417.00 | **57.73** MB |
| `node` (`12.9`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 61184.67 | **91.58** MB |
| `node` (`12.9`) | [fastify](https://fastify.io) (**2.7**) | 59816.33 | **152.98** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 59099.33 | **103.34** MB |
| `node` (`12.9`) | [foxify](https://foxify.js.org) (**0.1**) | 55709.00 | **116.87** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 55549.33 | **41.77** MB |
| `node` (`12.9`) | [koa](https://koajs.com) (**2.8**) | 52744.67 | **111.52** MB |
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 49975.33 | **106.54** MB |
| `node` (`12.9`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 49436.67 | **200.56** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 48198.00 | **118.58** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 48127.33 | **96.57** MB |
| `node` (`12.9`) | [restify](https://restify.com) (**8.4**) | 46960.00 | **82.21** MB |
| `node` (`12.9`) | [express](https://expressjs.com) (**4.17**) | 44926.00 | **109.78** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 44757.00 | **221.77** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 43243.67 | **93.01** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 43140.00 | **213.81** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 42419.67 | **210.54** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 41279.67 | **204.67** MB |
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 39973.67 | **74.14** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.23**) | 39785.00 | **37.93** MB |
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 39720.67 | **66.25** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 38155.67 | **94.40** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 37656.67 | **195.57** MB |
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 37117.33 | **45.60** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 35359.33 | **41.68** MB |
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 33858.67 | **86.81** MB |
| `node` (`12.9`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 32037.00 | **30.05** MB |
| `node` (`12.9`) | [hapi](https://hapijs.com) (**18.1**) | 31665.33 | **81.97** MB |
| `node` (`12.9`) | [moleculer](https://moleculer.services) (**0.13**) | 31275.33 | **53.49** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 29885.67 | **17.24** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 28508.00 | **148.85** MB |
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 28491.67 | **53.72** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.36**) | 27882.33 | **60.06** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 25658.67 | **67.25** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.5**) | 23767.67 | **53.76** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 20625.67 | **11.89** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 20055.33 | **37.26** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 19732.00 | **48.52** MB |
| `rust` (`nightly`) | [rocket](https://rocket.rs) (**0.4**) | 19468.00 | **29.70** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 18240.00 | **137.88** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 17925.00 | **34.57** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 15003.00 | **26.73** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 14794.67 | **38.38** MB |
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 11612.67 | **14.87** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.9**) | 10999.00 | **21.92** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 9035.67 | **19.66** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 7788.33 | **22.54** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 7274.33 | **21.45** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 6404.00 | **15.74** MB |
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 5982.33 | **7.45** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 3588.33 | **22.55** MB |
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

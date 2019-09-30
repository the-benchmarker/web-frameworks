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
Last update: 2019-09-30
```
OS: Linux (version: 5.2.17-200.fc30.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: syro (ruby)


:four: roda (ruby)


:five: cuba (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `rust` (`1.38`) | [nickel](https://nickel-org.github.io) (**0.11**) | **0.10** ms | 0.11 ms | 0.14 ms | 0.18 ms | 4.65 ms | **86.00** | 
| `rust` (`1.38`) | [iron](https://ironframework.io) (**0.6**) | **0.47** ms | 0.41 ms | 0.79 ms | 2.16 ms | 23.75 ms | **405.00** | 
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | **2.54** ms | 0.60 ms | 7.51 ms | 17.52 ms | 67.23 ms | **3935.33** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.24**) | **2.61** ms | 0.65 ms | 7.40 ms | 15.40 ms | 42.48 ms | **3551.00** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | **2.96** ms | 0.77 ms | 8.19 ms | 16.81 ms | 46.50 ms | **3896.00** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | **3.59** ms | 1.08 ms | 11.68 ms | 28.31 ms | 73.68 ms | **6238.67** | 
| `swift` (`5.1`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | **3.89** ms | 0.84 ms | 14.45 ms | 14.92 ms | 2515.89 ms | **35952.00** | 
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | **3.99** ms | 3.58 ms | 7.68 ms | 13.23 ms | 38.23 ms | **2825.00** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | **4.38** ms | 3.63 ms | 9.35 ms | 15.88 ms | 34.55 ms | **3673.67** | 
| `nim` (`1.0`) | [httpbeast](https://github.com/dom96/httpbeast) (**0.2**) | **4.60** ms | 4.11 ms | 8.03 ms | 13.39 ms | 30.29 ms | **2624.00** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | **4.61** ms | 4.05 ms | 9.12 ms | 15.73 ms | 46.96 ms | **3501.67** | 
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | **4.78** ms | 4.40 ms | 8.42 ms | 13.93 ms | 30.07 ms | **2720.67** | 
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | **5.06** ms | 4.61 ms | 7.86 ms | 13.92 ms | 98.96 ms | **2562.33** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | **5.20** ms | 4.70 ms | 8.70 ms | 14.55 ms | 90.87 ms | **3037.00** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | **5.21** ms | 0.47 ms | 17.14 ms | 43.19 ms | 108.77 ms | **9380.33** | 
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | **5.30** ms | 4.72 ms | 7.99 ms | 14.47 ms | 169.95 ms | **4873.67** | 
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | **5.37** ms | 4.77 ms | 8.61 ms | 15.49 ms | 99.26 ms | **3222.67** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | **5.55** ms | 4.63 ms | 10.37 ms | 20.52 ms | 150.81 ms | **4367.33** | 
| `crystal` (`0.31`) | [toro](https://github.com/soveran/toro) (**0.4**) | **5.57** ms | 4.82 ms | 9.59 ms | 15.49 ms | 31.40 ms | **3032.67** | 
| `crystal` (`0.31`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | **5.59** ms | 4.82 ms | 9.61 ms | 15.59 ms | 30.93 ms | **3036.33** | 
| `crystal` (`0.31`) | [raze](https://razecr.com) (**0.3**) | **5.86** ms | 5.10 ms | 9.69 ms | 15.85 ms | 34.25 ms | **2980.67** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | **5.93** ms | 0.55 ms | 20.45 ms | 45.27 ms | 105.20 ms | **10362.00** | 
| `crystal` (`0.31`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | **6.04** ms | 5.10 ms | 10.52 ms | 17.20 ms | 32.39 ms | **3383.33** | 
| `crystal` (`0.31`) | [kemal](https://kemalcr.com) (**0.28**) | **6.14** ms | 5.32 ms | 10.22 ms | 16.34 ms | 32.02 ms | **3133.67** | 
| `crystal` (`0.31`) | [amber](https://amberframework.org) (**0.3**) | **6.52** ms | 5.68 ms | 10.50 ms | 17.09 ms | 37.83 ms | **3187.67** | 
| `rust` (`1.38`) | [gotham](https://gotham.rs) (**0.4**) | **6.54** ms | 5.48 ms | 10.59 ms | 17.47 ms | 175.10 ms | **5793.67** | 
| `nim` (`1.0`) | [jester](https://github.com/dom96/jester) (**0.4**) | **7.10** ms | 6.48 ms | 10.70 ms | 16.57 ms | 45.78 ms | **3009.67** | 
| `crystal` (`0.31`) | [orion](https://github.com/obsidian/orion) (**1.7**) | **7.14** ms | 6.25 ms | 11.53 ms | 19.53 ms | 41.13 ms | **3598.67** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | **7.51** ms | 0.62 ms | 24.46 ms | 52.98 ms | 110.30 ms | **12206.67** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | **7.85** ms | 6.89 ms | 11.67 ms | 19.60 ms | 282.16 ms | **7173.67** | 
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | **7.91** ms | 6.73 ms | 13.41 ms | 24.07 ms | 62.59 ms | **4423.67** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | **8.15** ms | 5.20 ms | 10.46 ms | 73.68 ms | 742.87 ms | **31426.33** | 
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | **8.26** ms | 7.04 ms | 13.24 ms | 29.48 ms | 171.97 ms | **5904.33** | 
| `ruby` (`2.6`) | [grape](https://ruby-grape.org) (**1.2**) | **8.63** ms | 0.70 ms | 27.99 ms | 57.07 ms | 122.33 ms | **13588.00** | 
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | **8.88** ms | 7.56 ms | 14.54 ms | 31.23 ms | 127.37 ms | **5708.33** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.11**) | **8.96** ms | 5.94 ms | 14.60 ms | 82.80 ms | 123.38 ms | **12466.67** | 
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | **9.31** ms | 8.27 ms | 13.92 ms | 28.60 ms | 158.25 ms | **5511.00** | 
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | **9.32** ms | 7.70 ms | 15.72 ms | 34.46 ms | 205.73 ms | **6945.33** | 
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | **9.32** ms | 7.73 ms | 15.81 ms | 34.46 ms | 153.83 ms | **6415.33** | 
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | **9.41** ms | 8.07 ms | 14.96 ms | 32.88 ms | 199.63 ms | **6592.67** | 
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | **9.48** ms | 7.77 ms | 16.02 ms | 37.06 ms | 144.00 ms | **6897.33** | 
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | **10.19** ms | 8.23 ms | 17.54 ms | 39.63 ms | 236.85 ms | **8130.00** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | **10.25** ms | 9.14 ms | 14.24 ms | 28.04 ms | 292.96 ms | **9249.00** | 
| `rust` (`1.38`) | [actix-web](https://actix.rs) (**1.0**) | **10.29** ms | 10.05 ms | 13.31 ms | 17.21 ms | 95.67 ms | **2918.33** | 
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | **10.82** ms | 8.53 ms | 19.46 ms | 43.71 ms | 139.14 ms | **8245.67** | 
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.13**) | **11.05** ms | 8.88 ms | 19.07 ms | 42.93 ms | 263.88 ms | **9445.67** | 
| `swift` (`5.1`) | [perfect](https://perfect.org) (**3.1**) | **11.67** ms | 11.75 ms | 14.10 ms | 16.01 ms | 141.32 ms | **2227.67** | 
| `go` (`1.13`) | [gf](https://goframe.org) (**1.9**) | **11.76** ms | 9.98 ms | 18.82 ms | 41.65 ms | 202.19 ms | **8581.00** | 
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | **12.02** ms | 8.85 ms | 22.12 ms | 56.87 ms | 225.90 ms | **11071.00** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | **12.14** ms | 9.27 ms | 24.17 ms | 45.19 ms | 146.61 ms | **9158.00** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | **12.61** ms | 10.46 ms | 22.62 ms | 43.72 ms | 451.56 ms | **15844.00** | 
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | **12.88** ms | 9.48 ms | 17.41 ms | 85.79 ms | 511.28 ms | **24189.00** | 
| `php` (`7.3`) | [one](https://github.com/lizhichao/one) (**1.8**) | **13.12** ms | 12.40 ms | 21.80 ms | 34.26 ms | 130.87 ms | **7055.33** | 
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | **14.39** ms | 9.86 ms | 18.19 ms | 114.53 ms | 697.11 ms | **32925.33** | 
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | **14.47** ms | 10.96 ms | 20.09 ms | 83.83 ms | 548.93 ms | **23294.67** | 
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | **14.64** ms | 9.69 ms | 17.84 ms | 151.50 ms | 720.51 ms | **36470.67** | 
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | **15.44** ms | 11.02 ms | 20.10 ms | 122.70 ms | 687.45 ms | **32771.33** | 
| `python` (`3.7`) | [asgineer](https://asgineer.readthedocs.io) (**0.7**) | **15.96** ms | 15.32 ms | 22.65 ms | 31.76 ms | 115.54 ms | **5435.00** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | **16.47** ms | 11.88 ms | 32.49 ms | 57.78 ms | 258.43 ms | **13481.33** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | **16.56** ms | 13.35 ms | 21.05 ms | 56.36 ms | 662.68 ms | **22555.33** | 
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | **16.96** ms | 13.60 ms | 23.48 ms | 73.65 ms | 605.58 ms | **26085.67** | 
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | **18.75** ms | 14.83 ms | 38.81 ms | 74.36 ms | 202.13 ms | **15763.33** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | **19.52** ms | 18.82 ms | 28.34 ms | 40.38 ms | 145.03 ms | **7445.67** | 
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | **19.56** ms | 12.48 ms | 22.49 ms | 255.23 ms | 921.25 ms | **50723.00** | 
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | **19.64** ms | 17.83 ms | 18.35 ms | 30.01 ms | 898.71 ms | **31211.67** | 
| `ruby` (`2.6`) | [plezi](https://github.com/boazsegev/plezi) (**0.16**) | **20.44** ms | 15.42 ms | 37.09 ms | 92.59 ms | 185.54 ms | **17006.67** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | **20.47** ms | 18.34 ms | 33.50 ms | 58.42 ms | 115.69 ms | **10633.67** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | **21.23** ms | 13.92 ms | 47.77 ms | 72.20 ms | 218.04 ms | **16099.33** | 
| `fsharp` (`7.3`) | [suave](https://suave.io) (**2.5**) | **21.98** ms | 10.41 ms | 46.02 ms | 71.62 ms | 365.68 ms | **20197.67** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | **22.43** ms | 22.26 ms | 28.10 ms | 37.24 ms | 102.43 ms | **4984.33** | 
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | **23.39** ms | 15.09 ms | 26.89 ms | 304.33 ms | 1034.22 ms | **58792.33** | 
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | **23.85** ms | 14.07 ms | 25.58 ms | 348.36 ms | 1099.61 ms | **65253.00** | 
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | **23.95** ms | 20.15 ms | 25.84 ms | 125.43 ms | 649.94 ms | **28803.67** | 
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | **24.50** ms | 14.37 ms | 25.81 ms | 379.95 ms | 1116.92 ms | **68290.33** | 
| `swift` (`5.1`) | [vapor](https://vapor.codes) (**3.3**) | **24.56** ms | 17.32 ms | 30.33 ms | 235.50 ms | 1242.84 ms | **60677.00** | 
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | **25.62** ms | 16.55 ms | 29.08 ms | 329.64 ms | 1103.64 ms | **63032.67** | 
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | **26.36** ms | 24.55 ms | 31.37 ms | 79.53 ms | 213.76 ms | **12265.67** | 
| `swift` (`5.1`) | [kitura](https://kitura.io) (**2.7**) | **26.58** ms | 19.92 ms | 21.56 ms | 267.36 ms | 1459.99 ms | **72496.67** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | **30.49** ms | 2.14 ms | 101.89 ms | 277.48 ms | 768.09 ms | **58983.67** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.39**) | **33.17** ms | 30.85 ms | 48.97 ms | 67.85 ms | 169.95 ms | **12009.67** | 
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | **34.68** ms | 11.89 ms | 83.46 ms | 401.57 ms | 919.67 ms | **71218.67** | 
| `crystal` (`0.31`) | [lucky](https://luckyframework.org) (**0.16**) | **37.87** ms | 36.26 ms | 45.92 ms | 52.64 ms | 231.05 ms | **7266.33** | 
| `php` (`7.3`) | [basicphp](https://github.com/ray-ang/basicphp) (**0.9**) | **40.30** ms | 13.92 ms | 74.13 ms | 404.35 ms | 779.51 ms | **80535.33** | 
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | **42.29** ms | 41.12 ms | 47.16 ms | 53.33 ms | 470.54 ms | **13498.00** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | **42.53** ms | 43.63 ms | 57.73 ms | 68.59 ms | 97.92 ms | **11270.67** | 
| `crystal` (`0.31`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | **42.62** ms | 42.64 ms | 48.94 ms | 53.99 ms | 76.72 ms | **5965.33** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | **43.47** ms | 28.48 ms | 100.73 ms | 129.52 ms | 283.89 ms | **29920.00** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | **44.56** ms | 14.72 ms | 89.39 ms | 458.94 ms | 850.55 ms | **92131.00** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | **45.17** ms | 42.59 ms | 67.08 ms | 88.52 ms | 160.30 ms | **15572.33** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | **45.36** ms | 30.54 ms | 98.24 ms | 169.48 ms | 378.33 ms | **34404.67** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | **45.40** ms | 15.50 ms | 87.51 ms | 471.28 ms | 710.08 ms | **93368.33** | 
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | **46.35** ms | 25.68 ms | 42.34 ms | 738.24 ms | 1712.98 ms | **119643.33** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | **47.05** ms | 15.19 ms | 89.55 ms | 485.84 ms | 868.46 ms | **94878.67** | 
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | **47.51** ms | 25.56 ms | 42.94 ms | 773.83 ms | 1715.37 ms | **124743.00** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | **48.62** ms | 46.82 ms | 72.37 ms | 112.71 ms | 192.21 ms | **19424.00** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**6.0**) | **49.83** ms | 16.80 ms | 93.42 ms | 507.50 ms | 872.29 ms | **100279.00** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | **53.67** ms | 16.68 ms | 104.74 ms | 576.74 ms | 967.00 ms | **114237.67** | 
| `swift` (`5.1`) | [kitura-nio](https://kitura.io) (**2.7**) | **53.76** ms | 25.70 ms | 38.84 ms | 1061.19 ms | 3236.77 ms | **195931.67** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | **60.22** ms | 16.38 ms | 33.78 ms | 1269.59 ms | 2668.29 ms | **230514.33** | 
| `php` (`7.3`) | [spiral](https://github.com/spiral/framework) (**2.1**) | **60.39** ms | 59.93 ms | 68.00 ms | 81.42 ms | 298.78 ms | **10248.00** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.0**) | **79.17** ms | 22.49 ms | 149.67 ms | 925.37 ms | 1416.49 ms | **183633.00** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | **83.00** ms | 74.64 ms | 134.93 ms | 176.68 ms | 283.36 ms | **32672.00** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | **94.91** ms | 88.76 ms | 141.76 ms | 175.99 ms | 316.77 ms | **29765.33** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | **105.18** ms | 84.71 ms | 228.72 ms | 282.27 ms | 704.07 ms | **59007.00** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | **107.87** ms | 108.97 ms | 143.61 ms | 204.98 ms | 758.39 ms | **40883.67** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | **134.22** ms | 115.66 ms | 233.87 ms | 290.03 ms | 1048.75 ms | **68415.67** | 
| `crystal` (`0.31`) | [onyx](https://onyxframework.org) (**0.5**) | **209.19** ms | 207.28 ms | 242.75 ms | 270.92 ms | 318.64 ms | **25454.33** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | **215.23** ms | 7.35 ms | 27.63 ms | 5166.68 ms | 7927.55 ms | **874216.00** | 
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | **607.58** ms | 146.49 ms | 2325.82 ms | 6068.42 ms | 6884.61 ms | **1331743.67** | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (sifrr) (node)


:three: (japronto) (python)


:four: (httpbeast) (nim)


:five: (drogon) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 229440.33 | **132.72** MB |
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 217279.67 | **191.29** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 201992.67 | **241.91** MB |
| `nim` (`1.0`) | [httpbeast](https://github.com/dom96/httpbeast) (**0.2**) | 199482.33 | **284.20** MB |
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 190895.00 | **185.32** MB |
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 176647.33 | **284.62** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 174764.00 | **169.74** MB |
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 173340.33 | **278.59** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 173327.67 | **312.00** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 172609.33 | **448.30** MB |
| `crystal` (`0.31`) | [toro](https://github.com/soveran/toro) (**0.4**) | 169158.67 | **159.10** MB |
| `crystal` (`0.31`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 168363.00 | **158.36** MB |
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 167282.33 | **336.70** MB |
| `crystal` (`0.31`) | [raze](https://razecr.com) (**0.3**) | 161428.00 | **151.85** MB |
| `crystal` (`0.31`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 158744.33 | **169.12** MB |
| `crystal` (`0.31`) | [kemal](https://kemalcr.com) (**0.28**) | 154525.00 | **252.63** MB |
| `nim` (`1.0`) | [jester](https://github.com/dom96/jester) (**0.4**) | 147348.67 | **296.36** MB |
| `crystal` (`0.31`) | [amber](https://amberframework.org) (**0.3**) | 146530.00 | **268.16** MB |
| `rust` (`1.38`) | [gotham](https://gotham.rs) (**0.4**) | 146052.00 | **298.87** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.11**) | 139695.00 | **80.66** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 138567.67 | **238.78** MB |
| `crystal` (`0.31`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 134445.33 | **219.84** MB |
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 122229.67 | **312.21** MB |
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 118901.67 | **159.29** MB |
| `rust` (`1.38`) | [iron](https://ironframework.io) (**0.6**) | 116128.00 | **146.24** MB |
| `rust` (`1.38`) | [actix-web](https://actix.rs) (**1.0**) | 113946.33 | **169.02** MB |
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 111792.67 | **149.67** MB |
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 108028.67 | **189.61** MB |
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 108007.33 | **142.94** MB |
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 107775.00 | **189.24** MB |
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 106410.00 | **140.83** MB |
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 105672.33 | **141.03** MB |
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 101232.67 | **135.88** MB |
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 96347.67 | **128.72** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 95084.33 | **154.94** MB |
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.13**) | 94153.33 | **196.57** MB |
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 91731.33 | **161.07** MB |
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 90122.00 | **134.93** MB |
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 89073.00 | **133.52** MB |
| `go` (`1.13`) | [gf](https://goframe.org) (**1.9**) | 87338.00 | **148.16** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 87040.33 | **204.07** MB |
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 86083.67 | **129.05** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 85411.33 | **133.05** MB |
| `swift` (`5.1`) | [perfect](https://perfect.org) (**3.1**) | 82875.33 | **77.99** MB |
| `rust` (`1.38`) | [nickel](https://nickel-org.github.io) (**0.11**) | 81396.67 | **161.74** MB |
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 78475.67 | **117.61** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 78207.33 | **168.88** MB |
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 77433.00 | **116.09** MB |
| `php` (`7.3`) | [one](https://github.com/lizhichao/one) (**1.8**) | 75456.67 | **173.17** MB |
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | 72028.67 | **128.46** MB |
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 70520.00 | **105.69** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 68642.00 | **52.13** MB |
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 65871.00 | **173.14** MB |
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 65424.67 | **137.56** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 64960.00 | **160.17** MB |
| `python` (`3.7`) | [asgineer](https://asgineer.readthedocs.io) (**0.7**) | 61327.33 | **109.51** MB |
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 61129.33 | **129.49** MB |
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 59343.67 | **241.31** MB |
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 59192.00 | **126.23** MB |
| `php` (`7.3`) | [basicphp](https://github.com/ray-ang/basicphp) (**0.9**) | 56932.67 | **282.90** MB |
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | 56504.33 | **101.35** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 55772.33 | **97.87** MB |
| `ruby` (`2.6`) | [plezi](https://github.com/boazsegev/plezi) (**0.16**) | 55594.00 | **118.34** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 53956.33 | **267.92** MB |
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 53742.67 | **131.71** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 51966.33 | **258.43** MB |
| `swift` (`5.1`) | [vapor](https://vapor.codes) (**3.3**) | 51478.67 | **88.30** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 51141.67 | **126.75** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 50507.00 | **250.64** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 50397.00 | **108.65** MB |
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 50010.67 | **28.89** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 49703.00 | **100.02** MB |
| `swift` (`5.1`) | [kitura](https://kitura.io) (**2.7**) | 49161.00 | **91.27** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.24**) | 48393.67 | **46.24** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 46789.67 | **232.43** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**6.0**) | 46650.67 | **242.76** MB |
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 45744.33 | **80.30** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 43580.67 | **114.49** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 42988.33 | **50.78** MB |
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | 38935.00 | **89.52** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.0**) | 36491.00 | **190.70** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 35697.67 | **20.59** MB |
| `swift` (`5.1`) | [kitura-nio](https://kitura.io) (**2.7**) | 35687.67 | **67.48** MB |
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 35682.00 | **92.49** MB |
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 34041.33 | **58.11** MB |
| `fsharp` (`7.3`) | [suave](https://suave.io) (**2.5**) | 32302.67 | **65.37** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.39**) | 29957.33 | **64.56** MB |
| `crystal` (`0.31`) | [lucky](https://luckyframework.org) (**0.16**) | 26138.67 | **32.10** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 24884.33 | **46.32** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 24742.00 | **60.99** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 24607.33 | **14.18** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 23424.33 | **53.05** MB |
| `crystal` (`0.31`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 23353.33 | **29.20** MB |
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 23237.67 | **21.80** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 22011.67 | **39.23** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 21591.33 | **163.11** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 20573.00 | **39.66** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 17012.33 | **44.11** MB |
| `php` (`7.3`) | [spiral](https://github.com/spiral/framework) (**2.1**) | 16239.00 | **28.19** MB |
| `ruby` (`2.6`) | [grape](https://ruby-grape.org) (**1.2**) | 14916.33 | **8.54** MB |
| `swift` (`5.1`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 12281.00 | **15.74** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | 12122.67 | **24.16** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 10332.33 | **22.52** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 9629.67 | **27.93** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 9060.67 | **26.73** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 7491.33 | **18.42** MB |
| `crystal` (`0.31`) | [onyx](https://onyxframework.org) (**0.5**) | 4728.33 | **12.20** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 4209.67 | **26.45** MB |
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | 349.33 | **0.79** MB |
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

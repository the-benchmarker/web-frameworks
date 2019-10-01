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
Last update: 2019-10-01
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


:five: swifter (swift)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `rust` (`1.38`) | [nickel](https://nickel-org.github.io) (**0.11**) | **0.10** ms | 0.09 ms | 0.13 ms | 0.19 ms | 6.34 ms | **86.67** | 
| `rust` (`1.38`) | [iron](https://ironframework.io) (**0.6**) | **0.55** ms | 0.46 ms | 0.90 ms | 3.02 ms | 48.07 ms | **639.00** | 
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | **2.57** ms | 0.63 ms | 7.46 ms | 17.10 ms | 69.66 ms | **3845.67** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.24**) | **2.81** ms | 0.74 ms | 8.12 ms | 19.60 ms | 81.57 ms | **4308.33** | 
| `swift` (`5.1`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | **3.54** ms | 0.92 ms | 14.62 ms | 15.97 ms | 1186.60 ms | **14548.33** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | **3.68** ms | 0.99 ms | 10.06 ms | 20.03 ms | 50.89 ms | **4698.33** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | **4.50** ms | 0.82 ms | 10.33 ms | 62.02 ms | 288.40 ms | **12416.67** | 
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | **4.72** ms | 3.81 ms | 9.98 ms | 18.29 ms | 51.88 ms | **4131.33** | 
| `nim` (`1.0`) | [httpbeast](https://github.com/dom96/httpbeast) (**0.2**) | **4.92** ms | 4.23 ms | 8.91 ms | 15.70 ms | 47.93 ms | **3278.67** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | **4.95** ms | 4.24 ms | 10.00 ms | 17.60 ms | 56.01 ms | **4006.00** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | **5.03** ms | 4.05 ms | 11.08 ms | 19.63 ms | 58.52 ms | **4584.33** | 
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | **5.29** ms | 4.62 ms | 9.84 ms | 16.48 ms | 44.60 ms | **3601.33** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | **5.57** ms | 4.96 ms | 9.19 ms | 15.25 ms | 58.12 ms | **2917.33** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | **5.66** ms | 1.08 ms | 16.84 ms | 39.47 ms | 121.91 ms | **8733.00** | 
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | **5.70** ms | 5.07 ms | 8.56 ms | 16.43 ms | 173.26 ms | **4411.33** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | **5.98** ms | 4.78 ms | 11.20 ms | 25.20 ms | 212.39 ms | **6978.67** | 
| `crystal` (`0.31`) | [raze](https://razecr.com) (**0.3**) | **5.99** ms | 5.22 ms | 10.05 ms | 16.98 ms | 55.57 ms | **3311.33** | 
| `crystal` (`0.31`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | **6.33** ms | 5.52 ms | 10.83 ms | 17.56 ms | 55.06 ms | **3577.67** | 
| `crystal` (`0.31`) | [kemal](https://kemalcr.com) (**0.28**) | **6.34** ms | 5.46 ms | 10.70 ms | 17.52 ms | 54.13 ms | **3467.00** | 
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | **6.38** ms | 5.60 ms | 9.25 ms | 20.41 ms | 169.11 ms | **5128.67** | 
| `crystal` (`0.31`) | [toro](https://github.com/soveran/toro) (**0.4**) | **6.45** ms | 5.48 ms | 11.33 ms | 19.34 ms | 87.91 ms | **4191.67** | 
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | **6.51** ms | 5.55 ms | 9.27 ms | 21.37 ms | 206.75 ms | **6710.00** | 
| `crystal` (`0.31`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | **6.71** ms | 5.82 ms | 11.66 ms | 19.39 ms | 63.55 ms | **4005.00** | 
| `rust` (`1.38`) | [gotham](https://gotham.rs) (**0.4**) | **6.87** ms | 5.77 ms | 11.88 ms | 19.88 ms | 184.56 ms | **4569.33** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | **6.88** ms | 0.53 ms | 25.72 ms | 70.33 ms | 189.98 ms | **15057.33** | 
| `crystal` (`0.31`) | [amber](https://amberframework.org) (**0.3**) | **7.56** ms | 6.86 ms | 12.92 ms | 20.72 ms | 56.78 ms | **4187.00** | 
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | **7.78** ms | 6.59 ms | 13.29 ms | 24.46 ms | 69.96 ms | **4526.00** | 
| `nim` (`1.0`) | [jester](https://github.com/dom96/jester) (**0.4**) | **7.80** ms | 6.99 ms | 12.40 ms | 21.02 ms | 54.50 ms | **3804.00** | 
| `crystal` (`0.31`) | [orion](https://github.com/obsidian/orion) (**1.7**) | **7.93** ms | 7.08 ms | 13.35 ms | 22.60 ms | 52.77 ms | **4311.00** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | **8.46** ms | 0.61 ms | 31.26 ms | 83.81 ms | 205.27 ms | **17942.67** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | **8.91** ms | 7.68 ms | 13.55 ms | 32.20 ms | 192.29 ms | **7922.33** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.11**) | **9.04** ms | 6.01 ms | 14.74 ms | 83.07 ms | 123.15 ms | **12534.33** | 
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | **9.45** ms | 7.83 ms | 16.04 ms | 34.31 ms | 174.69 ms | **6670.00** | 
| `ruby` (`2.6`) | [grape](https://ruby-grape.org) (**1.2**) | **9.91** ms | 0.90 ms | 33.48 ms | 80.41 ms | 211.78 ms | **17995.67** | 
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | **10.16** ms | 8.00 ms | 16.24 ms | 40.74 ms | 314.24 ms | **13287.00** | 
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | **10.24** ms | 8.41 ms | 17.05 ms | 40.86 ms | 289.35 ms | **8455.33** | 
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | **10.36** ms | 8.19 ms | 17.98 ms | 42.44 ms | 274.45 ms | **9306.67** | 
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | **10.61** ms | 8.16 ms | 18.23 ms | 45.26 ms | 326.20 ms | **12710.33** | 
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | **10.65** ms | 9.06 ms | 16.50 ms | 40.09 ms | 189.67 ms | **7780.00** | 
| `rust` (`1.38`) | [actix-web](https://actix.rs) (**1.0**) | **10.74** ms | 10.39 ms | 14.30 ms | 18.96 ms | 99.24 ms | **3221.00** | 
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | **10.80** ms | 8.64 ms | 18.54 ms | 45.83 ms | 190.82 ms | **8735.33** | 
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | **10.90** ms | 8.89 ms | 18.05 ms | 45.44 ms | 258.20 ms | **9003.00** | 
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | **10.98** ms | 8.67 ms | 19.71 ms | 44.50 ms | 214.02 ms | **8467.67** | 
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | **11.35** ms | 8.92 ms | 19.84 ms | 47.64 ms | 214.20 ms | **10191.67** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | **12.01** ms | 10.33 ms | 22.39 ms | 41.04 ms | 259.08 ms | **10607.67** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | **12.63** ms | 5.47 ms | 13.32 ms | 237.81 ms | 1244.62 ms | **59517.00** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | **13.45** ms | 9.45 ms | 28.89 ms | 58.02 ms | 173.62 ms | **11987.00** | 
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | **13.56** ms | 9.71 ms | 18.75 ms | 65.78 ms | 631.40 ms | **27437.33** | 
| `swift` (`5.1`) | [perfect](https://perfect.org) (**3.1**) | **14.11** ms | 13.93 ms | 17.58 ms | 22.53 ms | 115.56 ms | **3906.33** | 
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | **15.42** ms | 10.19 ms | 20.60 ms | 148.56 ms | 695.90 ms | **34035.33** | 
| `go` (`1.13`) | [gf](https://goframe.org) (**1.9**) | **16.24** ms | 12.15 ms | 30.88 ms | 70.75 ms | 292.21 ms | **15099.33** | 
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.13**) | **16.26** ms | 11.91 ms | 30.86 ms | 73.68 ms | 387.03 ms | **17210.33** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | **16.26** ms | 11.60 ms | 30.80 ms | 54.31 ms | 193.57 ms | **11717.67** | 
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | **16.47** ms | 10.46 ms | 20.93 ms | 169.24 ms | 788.37 ms | **40547.00** | 
| `php` (`7.3`) | [one](https://github.com/lizhichao/one) (**1.8**) | **16.73** ms | 14.78 ms | 30.02 ms | 53.37 ms | 146.91 ms | **10889.67** | 
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | **17.35** ms | 11.75 ms | 23.03 ms | 154.32 ms | 702.26 ms | **35759.00** | 
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | **17.42** ms | 12.04 ms | 23.34 ms | 130.12 ms | 754.07 ms | **36285.00** | 
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | **17.49** ms | 13.60 ms | 23.75 ms | 95.98 ms | 661.52 ms | **28741.33** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | **18.57** ms | 17.34 ms | 27.42 ms | 48.71 ms | 155.43 ms | **8229.33** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | **18.84** ms | 11.79 ms | 42.66 ms | 83.51 ms | 593.87 ms | **25435.33** | 
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | **19.67** ms | 19.24 ms | 22.39 ms | 33.62 ms | 293.88 ms | **5680.67** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | **20.37** ms | 14.88 ms | 32.21 ms | 79.88 ms | 610.51 ms | **21631.67** | 
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | **21.26** ms | 16.16 ms | 45.80 ms | 91.76 ms | 249.58 ms | **19660.67** | 
| `python` (`3.7`) | [asgineer](https://asgineer.readthedocs.io) (**0.7**) | **21.85** ms | 18.14 ms | 40.02 ms | 74.99 ms | 211.05 ms | **14909.67** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | **22.01** ms | 19.66 ms | 36.98 ms | 64.59 ms | 168.11 ms | **12405.00** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | **22.07** ms | 16.57 ms | 42.17 ms | 78.23 ms | 209.29 ms | **15538.00** | 
| `ruby` (`2.6`) | [plezi](https://github.com/boazsegev/plezi) (**0.16**) | **22.59** ms | 16.34 ms | 41.94 ms | 101.78 ms | 231.45 ms | **18791.33** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | **23.11** ms | 22.55 ms | 30.80 ms | 44.02 ms | 85.97 ms | **6512.33** | 
| `swift` (`5.1`) | [kitura](https://kitura.io) (**2.7**) | **24.68** ms | 19.67 ms | 22.60 ms | 194.50 ms | 1172.63 ms | **54618.33** | 
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | **25.17** ms | 15.41 ms | 29.34 ms | 310.50 ms | 1016.30 ms | **60235.00** | 
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | **25.71** ms | 14.89 ms | 28.86 ms | 343.67 ms | 1093.16 ms | **65883.00** | 
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | **26.82** ms | 12.78 ms | 25.30 ms | 522.70 ms | 1253.29 ms | **86105.00** | 
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | **27.69** ms | 21.40 ms | 34.06 ms | 168.58 ms | 846.66 ms | **41404.33** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | **31.20** ms | 2.34 ms | 103.83 ms | 277.53 ms | 756.28 ms | **59270.33** | 
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | **31.91** ms | 15.03 ms | 29.83 ms | 619.44 ms | 1439.96 ms | **101171.67** | 
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | **32.05** ms | 17.41 ms | 32.35 ms | 522.30 ms | 1364.47 ms | **89403.33** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | **32.57** ms | 19.98 ms | 39.26 ms | 417.82 ms | 2354.96 ms | **113979.00** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.39**) | **36.27** ms | 32.60 ms | 58.08 ms | 92.90 ms | 253.04 ms | **17339.00** | 
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | **38.93** ms | 28.78 ms | 49.12 ms | 371.05 ms | 821.96 ms | **61753.67** | 
| `crystal` (`0.31`) | [lucky](https://luckyframework.org) (**0.16**) | **40.11** ms | 40.38 ms | 48.13 ms | 54.16 ms | 258.87 ms | **8171.67** | 
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | **42.64** ms | 41.70 ms | 48.06 ms | 54.93 ms | 382.91 ms | **7657.33** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | **42.85** ms | 35.62 ms | 66.91 ms | 115.12 ms | 378.94 ms | **21342.67** | 
| `php` (`7.3`) | [basicphp](https://github.com/ray-ang/basicphp) (**0.9**) | **44.80** ms | 14.82 ms | 92.49 ms | 448.94 ms | 858.67 ms | **88687.00** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | **45.03** ms | 39.56 ms | 71.22 ms | 109.98 ms | 195.79 ms | **19511.33** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | **45.66** ms | 36.02 ms | 82.20 ms | 142.19 ms | 369.74 ms | **25883.67** | 
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | **46.63** ms | 27.06 ms | 48.58 ms | 651.75 ms | 1493.37 ms | **106564.67** | 
| `swift` (`5.1`) | [vapor](https://vapor.codes) (**3.3**) | **49.98** ms | 17.96 ms | 39.11 ms | 958.39 ms | 2464.92 ms | **175161.67** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | **50.81** ms | 47.54 ms | 82.54 ms | 127.99 ms | 207.60 ms | **23860.33** | 
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | **52.62** ms | 25.96 ms | 48.03 ms | 905.35 ms | 1883.23 ms | **144757.67** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | **53.61** ms | 48.41 ms | 88.32 ms | 145.75 ms | 314.07 ms | **27226.00** | 
| `crystal` (`0.31`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | **53.76** ms | 53.74 ms | 60.43 ms | 63.95 ms | 82.40 ms | **5823.33** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**6.0**) | **54.65** ms | 17.85 ms | 102.13 ms | 569.43 ms | 1009.98 ms | **110530.00** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | **54.72** ms | 17.86 ms | 110.00 ms | 566.38 ms | 985.80 ms | **112280.00** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | **54.82** ms | 17.06 ms | 106.28 ms | 593.80 ms | 989.34 ms | **117728.00** | 
| `swift` (`5.1`) | [kitura-nio](https://kitura.io) (**2.7**) | **55.23** ms | 25.95 ms | 41.71 ms | 1029.98 ms | 3224.78 ms | **180322.00** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | **55.98** ms | 18.38 ms | 115.57 ms | 581.04 ms | 1054.68 ms | **116312.33** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | **61.27** ms | 19.24 ms | 116.81 ms | 648.99 ms | 1208.23 ms | **129003.00** | 
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | **65.60** ms | 10.39 ms | 75.65 ms | 1628.78 ms | 2707.77 ms | **262450.00** | 
| `php` (`7.3`) | [spiral](https://github.com/spiral/framework) (**2.1**) | **70.50** ms | 68.59 ms | 89.07 ms | 107.88 ms | 230.22 ms | **14162.33** | 
| `fsharp` (`7.3`) | [suave](https://suave.io) (**2.5**) | **77.21** ms | 29.14 ms | 195.82 ms | 664.34 ms | 1614.76 ms | **140539.67** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.0**) | **84.83** ms | 23.70 ms | 169.36 ms | 987.96 ms | 1483.68 ms | **193169.33** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | **96.45** ms | 93.11 ms | 113.92 ms | 141.99 ms | 746.02 ms | **27325.33** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | **104.87** ms | 92.04 ms | 187.48 ms | 299.99 ms | 504.21 ms | **59933.33** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | **109.03** ms | 95.84 ms | 154.99 ms | 285.13 ms | 825.23 ms | **51452.00** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | **113.69** ms | 106.12 ms | 167.62 ms | 244.04 ms | 354.92 ms | **41635.67** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | **129.64** ms | 120.05 ms | 174.67 ms | 225.18 ms | 972.43 ms | **42154.33** | 
| `crystal` (`0.31`) | [onyx](https://onyxframework.org) (**0.5**) | **192.52** ms | 194.96 ms | 232.34 ms | 254.25 ms | 277.87 ms | **33079.67** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | **204.45** ms | 7.64 ms | 25.60 ms | 5169.22 ms | 7931.34 ms | **858547.00** | 
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | **299.07** ms | 97.09 ms | 665.18 ms | 3522.62 ms | 5179.08 ms | **623368.67** | 

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
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 204026.00 | **118.05** MB |
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 193265.00 | **170.15** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 186541.67 | **223.36** MB |
| `nim` (`1.0`) | [httpbeast](https://github.com/dom96/httpbeast) (**0.2**) | 186178.67 | **264.90** MB |
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 175460.00 | **170.27** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 166168.00 | **299.13** MB |
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 164204.33 | **265.67** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 163589.67 | **158.80** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 160877.00 | **417.83** MB |
| `crystal` (`0.31`) | [raze](https://razecr.com) (**0.3**) | 157061.33 | **147.74** MB |
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 150333.67 | **242.22** MB |
| `crystal` (`0.31`) | [kemal](https://kemalcr.com) (**0.28**) | 149495.67 | **244.42** MB |
| `crystal` (`0.31`) | [toro](https://github.com/soveran/toro) (**0.4**) | 149305.00 | **140.44** MB |
| `crystal` (`0.31`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 149036.67 | **140.21** MB |
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 147490.67 | **296.75** MB |
| `crystal` (`0.31`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 141524.33 | **151.50** MB |
| `rust` (`1.38`) | [gotham](https://gotham.rs) (**0.4**) | 138895.33 | **284.64** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.11**) | 138588.00 | **80.02** MB |
| `nim` (`1.0`) | [jester](https://github.com/dom96/jester) (**0.4**) | 137915.00 | **277.39** MB |
| `crystal` (`0.31`) | [amber](https://amberframework.org) (**0.3**) | 125694.00 | **230.19** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 125650.67 | **216.82** MB |
| `crystal` (`0.31`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 123524.67 | **201.97** MB |
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 123397.33 | **315.56** MB |
| `rust` (`1.38`) | [actix-web](https://actix.rs) (**1.0**) | 107697.00 | **159.11** MB |
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 106814.33 | **187.53** MB |
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 104890.67 | **140.74** MB |
| `rust` (`1.38`) | [iron](https://ironframework.io) (**0.6**) | 104379.33 | **131.76** MB |
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 101085.33 | **177.47** MB |
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 100898.67 | **177.14** MB |
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 100322.00 | **134.55** MB |
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 96905.00 | **127.68** MB |
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 96775.67 | **130.84** MB |
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 96290.00 | **128.39** MB |
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 95931.33 | **126.76** MB |
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 93336.67 | **124.09** MB |
| `rust` (`1.38`) | [nickel](https://nickel-org.github.io) (**0.11**) | 91145.00 | **181.33** MB |
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 87221.00 | **130.77** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 85349.00 | **133.02** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 83825.67 | **196.41** MB |
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 81543.67 | **122.25** MB |
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 80200.33 | **120.22** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 77365.67 | **167.30** MB |
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | 73808.00 | **131.71** MB |
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 72073.67 | **108.03** MB |
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 70344.67 | **105.42** MB |
| `swift` (`5.1`) | [perfect](https://perfect.org) (**3.1**) | 69574.67 | **65.36** MB |
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.13**) | 69497.00 | **144.91** MB |
| `go` (`1.13`) | [gf](https://goframe.org) (**1.9**) | 68369.33 | **116.44** MB |
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 66331.67 | **99.43** MB |
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 64840.00 | **136.26** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 64345.33 | **158.60** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 64255.00 | **104.57** MB |
| `php` (`7.3`) | [one](https://github.com/lizhichao/one) (**1.8**) | 61636.33 | **141.51** MB |
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 60301.67 | **156.92** MB |
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 56955.67 | **120.65** MB |
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 55970.00 | **227.39** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 55411.00 | **41.20** MB |
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 55406.00 | **118.00** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 53587.00 | **107.83** MB |
| `php` (`7.3`) | [basicphp](https://github.com/ray-ang/basicphp) (**0.9**) | 52571.33 | **261.30** MB |
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | 50716.00 | **91.31** MB |
| `ruby` (`2.6`) | [plezi](https://github.com/boazsegev/plezi) (**0.16**) | 50402.00 | **107.43** MB |
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 49669.33 | **121.57** MB |
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 49121.00 | **28.40** MB |
| `swift` (`5.1`) | [kitura](https://kitura.io) (**2.7**) | 48762.67 | **90.59** MB |
| `python` (`3.7`) | [asgineer](https://asgineer.readthedocs.io) (**0.7**) | 48671.00 | **86.95** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 47926.67 | **118.88** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 47067.67 | **82.41** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 46360.67 | **99.88** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 46331.33 | **230.26** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.24**) | 45091.33 | **43.10** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 43716.33 | **217.15** MB |
| `swift` (`5.1`) | [vapor](https://vapor.codes) (**3.3**) | 43584.33 | **73.80** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 42798.33 | **212.69** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 42655.67 | **112.14** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**6.0**) | 42485.67 | **221.15** MB |
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 41489.67 | **72.81** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 40069.33 | **199.11** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 34631.33 | **20.01** MB |
| `swift` (`5.1`) | [kitura-nio](https://kitura.io) (**2.7**) | 34588.67 | **65.41** MB |
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 33816.33 | **87.70** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.0**) | 33810.67 | **176.57** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 33679.00 | **39.74** MB |
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | 32993.33 | **75.85** MB |
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 31820.67 | **54.39** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.39**) | 27627.67 | **59.62** MB |
| `crystal` (`0.31`) | [lucky](https://luckyframework.org) (**0.16**) | 24551.00 | **30.14** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 23341.67 | **57.54** MB |
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 22959.67 | **21.54** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 22952.00 | **42.71** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 22554.00 | **13.04** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 22379.33 | **50.73** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 19823.33 | **35.41** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 18866.33 | **36.46** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 18843.67 | **142.41** MB |
| `crystal` (`0.31`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 18512.00 | **23.14** MB |
| `fsharp` (`7.3`) | [suave](https://suave.io) (**2.5**) | 15410.67 | **31.18** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 15253.00 | **39.53** MB |
| `php` (`7.3`) | [spiral](https://github.com/spiral/framework) (**2.1**) | 13819.00 | **24.00** MB |
| `ruby` (`2.6`) | [grape](https://ruby-grape.org) (**1.2**) | 13061.00 | **7.56** MB |
| `swift` (`5.1`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 10805.67 | **13.84** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 10079.00 | **29.80** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | 9777.67 | **19.51** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 9037.67 | **26.22** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 8783.67 | **19.15** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 7512.33 | **18.51** MB |
| `crystal` (`0.31`) | [onyx](https://onyxframework.org) (**0.5**) | 5168.00 | **13.33** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 4099.00 | **25.78** MB |
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | 789.33 | **1.79** MB |
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

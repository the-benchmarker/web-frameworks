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
Last update: 2019-10-03
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
| `rust` (`1.38`) | [nickel](https://nickel-org.github.io) (**0.11**) | **0.10** ms | 0.10 ms | 0.14 ms | 0.18 ms | 7.53 ms | **76.00** | 
| `rust` (`1.38`) | [iron](https://ironframework.io) (**0.6**) | **0.51** ms | 0.43 ms | 0.92 ms | 1.94 ms | 38.26 ms | **518.00** | 
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | **2.61** ms | 0.71 ms | 7.34 ms | 16.31 ms | 62.26 ms | **3704.67** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.24**) | **2.76** ms | 0.71 ms | 7.88 ms | 19.86 ms | 102.17 ms | **4430.33** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | **2.95** ms | 0.65 ms | 8.44 ms | 17.70 ms | 51.69 ms | **4077.33** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | **3.77** ms | 0.78 ms | 11.11 ms | 25.06 ms | 81.74 ms | **5656.33** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | **4.23** ms | 3.91 ms | 7.48 ms | 13.06 ms | 38.88 ms | **2716.00** | 
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | **4.48** ms | 3.72 ms | 9.13 ms | 17.07 ms | 72.76 ms | **3856.67** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | **4.51** ms | 4.09 ms | 8.94 ms | 14.80 ms | 30.36 ms | **3298.67** | 
| `swift` (`5.1`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | **4.70** ms | 0.87 ms | 14.49 ms | 14.97 ms | 2520.01 ms | **47946.67** | 
| `nim` (`1.0`) | [httpbeast](https://github.com/dom96/httpbeast) (**0.2**) | **4.92** ms | 4.23 ms | 9.02 ms | 15.67 ms | 52.85 ms | **3326.67** | 
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | **5.10** ms | 4.51 ms | 9.35 ms | 15.83 ms | 64.32 ms | **3422.33** | 
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | **5.11** ms | 4.66 ms | 7.95 ms | 13.85 ms | 48.64 ms | **2402.33** | 
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | **5.16** ms | 4.62 ms | 7.88 ms | 14.34 ms | 165.88 ms | **4373.33** | 
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | **5.22** ms | 4.76 ms | 7.98 ms | 14.17 ms | 43.87 ms | **2375.67** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | **5.25** ms | 4.73 ms | 8.81 ms | 14.62 ms | 41.37 ms | **2755.33** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | **5.41** ms | 0.66 ms | 16.81 ms | 41.88 ms | 110.26 ms | **9105.00** | 
| `crystal` (`0.31`) | [toro](https://github.com/soveran/toro) (**0.4**) | **5.74** ms | 5.02 ms | 9.72 ms | 15.94 ms | 45.68 ms | **3140.33** | 
| `crystal` (`0.31`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | **5.97** ms | 5.16 ms | 10.16 ms | 16.82 ms | 60.26 ms | **3366.33** | 
| `crystal` (`0.31`) | [raze](https://razecr.com) (**0.3**) | **6.06** ms | 5.23 ms | 10.41 ms | 17.23 ms | 47.92 ms | **3441.67** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | **6.18** ms | 0.57 ms | 20.84 ms | 44.99 ms | 99.61 ms | **10399.00** | 
| `crystal` (`0.31`) | [kemal](https://kemalcr.com) (**0.28**) | **6.25** ms | 5.35 ms | 10.58 ms | 17.44 ms | 50.29 ms | **3427.67** | 
| `crystal` (`0.31`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | **6.30** ms | 5.38 ms | 11.00 ms | 18.17 ms | 49.65 ms | **3669.67** | 
| `crystal` (`0.31`) | [amber](https://amberframework.org) (**0.3**) | **6.81** ms | 5.85 ms | 11.60 ms | 18.89 ms | 48.69 ms | **3729.67** | 
| `rust` (`1.38`) | [gotham](https://gotham.rs) (**0.4**) | **6.89** ms | 5.70 ms | 11.94 ms | 19.98 ms | 221.74 ms | **5673.00** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | **7.26** ms | 0.63 ms | 24.07 ms | 52.76 ms | 114.11 ms | **12084.33** | 
| `nim` (`1.0`) | [jester](https://github.com/dom96/jester) (**0.4**) | **7.39** ms | 6.63 ms | 11.19 ms | 19.35 ms | 56.80 ms | **3443.67** | 
| `crystal` (`0.31`) | [orion](https://github.com/obsidian/orion) (**1.7**) | **7.65** ms | 6.72 ms | 13.01 ms | 21.55 ms | 52.65 ms | **4177.33** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | **7.75** ms | 6.93 ms | 11.59 ms | 18.33 ms | 213.46 ms | **5691.33** | 
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | **7.77** ms | 6.59 ms | 13.24 ms | 24.47 ms | 96.64 ms | **4582.00** | 
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | **8.24** ms | 6.92 ms | 13.09 ms | 28.97 ms | 281.57 ms | **7345.67** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | **8.70** ms | 4.30 ms | 11.51 ms | 85.66 ms | 225.46 ms | **16689.67** | 
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | **8.76** ms | 7.40 ms | 14.35 ms | 30.97 ms | 226.86 ms | **6372.00** | 
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | **8.81** ms | 7.40 ms | 14.53 ms | 32.20 ms | 182.94 ms | **6423.67** | 
| `ruby` (`2.6`) | [grape](https://ruby-grape.org) (**1.2**) | **9.04** ms | 0.83 ms | 28.84 ms | 59.80 ms | 133.48 ms | **14050.67** | 
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | **9.16** ms | 7.80 ms | 14.63 ms | 33.23 ms | 239.45 ms | **7052.67** | 
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | **9.16** ms | 8.10 ms | 13.46 ms | 29.31 ms | 281.38 ms | **6655.67** | 
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | **9.25** ms | 7.77 ms | 15.67 ms | 32.62 ms | 122.70 ms | **6015.00** | 
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | **9.28** ms | 7.71 ms | 15.68 ms | 34.05 ms | 215.82 ms | **6869.00** | 
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | **9.31** ms | 7.72 ms | 15.72 ms | 34.78 ms | 214.66 ms | **6425.00** | 
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | **9.53** ms | 8.01 ms | 15.80 ms | 35.23 ms | 127.95 ms | **6593.33** | 
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | **9.59** ms | 7.90 ms | 16.46 ms | 35.76 ms | 123.21 ms | **6642.33** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.11**) | **9.61** ms | 6.60 ms | 15.46 ms | 84.80 ms | 122.01 ms | **12914.33** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | **10.39** ms | 8.99 ms | 14.08 ms | 24.05 ms | 505.01 ms | **14456.67** | 
| `rust` (`1.38`) | [actix-web](https://actix.rs) (**1.0**) | **10.46** ms | 10.15 ms | 13.92 ms | 18.15 ms | 33.18 ms | **2635.33** | 
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.13**) | **11.05** ms | 8.84 ms | 18.98 ms | 43.60 ms | 248.31 ms | **10011.67** | 
| `go` (`1.13`) | [gf](https://goframe.org) (**1.9**) | **11.76** ms | 10.08 ms | 18.90 ms | 41.29 ms | 132.98 ms | **7046.67** | 
| `swift` (`5.1`) | [perfect](https://perfect.org) (**3.1**) | **11.84** ms | 11.92 ms | 14.18 ms | 16.19 ms | 108.43 ms | **2768.00** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | **12.05** ms | 10.42 ms | 22.31 ms | 40.48 ms | 245.44 ms | **9693.67** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | **12.72** ms | 9.04 ms | 28.58 ms | 49.96 ms | 107.85 ms | **10718.00** | 
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | **13.02** ms | 9.65 ms | 18.00 ms | 78.86 ms | 523.48 ms | **22742.00** | 
| `php` (`7.3`) | [one](https://github.com/lizhichao/one) (**1.8**) | **13.49** ms | 13.07 ms | 21.71 ms | 30.74 ms | 63.58 ms | **6269.67** | 
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | **13.93** ms | 9.60 ms | 17.84 ms | 104.68 ms | 686.35 ms | **31979.67** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | **14.11** ms | 5.30 ms | 11.96 ms | 301.57 ms | 1184.78 ms | **67608.33** | 
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | **15.88** ms | 10.99 ms | 20.36 ms | 119.35 ms | 705.58 ms | **34880.00** | 
| `python` (`3.7`) | [asgineer](https://asgineer.readthedocs.io) (**0.7**) | **16.03** ms | 15.16 ms | 23.19 ms | 33.66 ms | 180.79 ms | **6452.33** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | **16.31** ms | 11.69 ms | 33.56 ms | 56.52 ms | 178.84 ms | **12036.00** | 
| `ruby` (`2.6`) | [plezi](https://github.com/boazsegev/plezi) (**0.16**) | **16.36** ms | 14.97 ms | 24.63 ms | 37.51 ms | 88.29 ms | **6521.33** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | **16.43** ms | 13.05 ms | 20.74 ms | 73.38 ms | 577.03 ms | **22098.33** | 
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | **16.53** ms | 10.08 ms | 20.49 ms | 216.03 ms | 737.96 ms | **40888.67** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | **17.09** ms | 16.10 ms | 24.92 ms | 34.94 ms | 173.74 ms | **6401.67** | 
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | **17.24** ms | 12.99 ms | 37.98 ms | 70.63 ms | 143.58 ms | **15246.67** | 
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | **17.81** ms | 11.61 ms | 22.14 ms | 202.34 ms | 759.53 ms | **40612.67** | 
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | **18.19** ms | 17.97 ms | 18.54 ms | 23.59 ms | 492.96 ms | **8960.67** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | **19.60** ms | 18.17 ms | 29.73 ms | 45.26 ms | 248.81 ms | **8924.67** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | **20.27** ms | 14.26 ms | 43.21 ms | 66.13 ms | 179.92 ms | **14291.67** | 
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | **20.40** ms | 13.90 ms | 25.89 ms | 195.76 ms | 891.83 ms | **44215.00** | 
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | **20.84** ms | 14.37 ms | 26.92 ms | 184.75 ms | 826.81 ms | **41225.33** | 
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | **21.71** ms | 12.78 ms | 24.47 ms | 315.20 ms | 991.07 ms | **58544.00** | 
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | **22.00** ms | 15.16 ms | 27.04 ms | 217.21 ms | 948.41 ms | **48965.33** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | **22.27** ms | 22.29 ms | 28.15 ms | 36.63 ms | 71.27 ms | **5117.33** | 
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | **22.89** ms | 14.50 ms | 27.26 ms | 278.49 ms | 1032.75 ms | **56448.67** | 
| `swift` (`5.1`) | [vapor](https://vapor.codes) (**3.3**) | **24.00** ms | 17.34 ms | 29.41 ms | 213.71 ms | 1224.89 ms | **57936.67** | 
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | **24.67** ms | 24.12 ms | 28.71 ms | 39.19 ms | 156.81 ms | **6043.33** | 
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | **26.28** ms | 20.10 ms | 28.45 ms | 193.15 ms | 848.27 ms | **43128.33** | 
| `swift` (`5.1`) | [kitura](https://kitura.io) (**2.7**) | **27.34** ms | 19.71 ms | 21.26 ms | 299.32 ms | 1738.30 ms | **81244.33** | 
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | **27.36** ms | 17.38 ms | 31.66 ms | 362.60 ms | 1172.59 ms | **67483.67** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | **32.28** ms | 2.65 ms | 106.03 ms | 284.59 ms | 735.76 ms | **60497.67** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.39**) | **32.94** ms | 31.37 ms | 46.72 ms | 62.44 ms | 424.25 ms | **15441.67** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | **35.85** ms | 15.96 ms | 31.64 ms | 741.88 ms | 2461.68 ms | **151270.67** | 
| `crystal` (`0.31`) | [lucky](https://luckyframework.org) (**0.16**) | **38.08** ms | 36.40 ms | 45.27 ms | 54.95 ms | 62.35 ms | **5496.67** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | **38.88** ms | 26.89 ms | 85.62 ms | 100.58 ms | 236.19 ms | **23757.33** | 
| `php` (`7.3`) | [basicphp](https://github.com/ray-ang/basicphp) (**0.9**) | **39.21** ms | 13.36 ms | 75.18 ms | 388.63 ms | 764.96 ms | **76765.33** | 
| `python` (`3.7`) | [clastic](https://github.com/mahmoud/clastic) (**19.9**) | **39.89** ms | 28.85 ms | 90.82 ms | 125.91 ms | 489.74 ms | **35807.67** | 
| `fsharp` (`7.3`) | [suave](https://suave.io) (**2.5**) | **40.26** ms | 10.97 ms | 86.31 ms | 200.19 ms | 1707.43 ms | **78695.67** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | **41.82** ms | 30.79 ms | 96.66 ms | 108.88 ms | 345.97 ms | **28588.00** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | **42.24** ms | 40.12 ms | 65.20 ms | 73.60 ms | 158.50 ms | **15206.33** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | **42.34** ms | 13.76 ms | 85.78 ms | 430.89 ms | 748.73 ms | **84414.67** | 
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | **42.86** ms | 27.00 ms | 45.44 ms | 571.92 ms | 1452.38 ms | **93877.00** | 
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | **43.62** ms | 42.28 ms | 49.87 ms | 56.33 ms | 379.46 ms | **8128.67** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | **43.74** ms | 14.73 ms | 83.49 ms | 453.54 ms | 739.02 ms | **89643.00** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | **44.62** ms | 42.13 ms | 68.06 ms | 89.29 ms | 188.97 ms | **17635.00** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | **44.71** ms | 14.27 ms | 91.10 ms | 454.30 ms | 922.54 ms | **90115.67** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | **48.32** ms | 44.51 ms | 70.64 ms | 103.24 ms | 280.11 ms | **17314.33** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | **48.54** ms | 14.92 ms | 98.38 ms | 511.64 ms | 876.78 ms | **101269.00** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**6.1**) | **48.62** ms | 16.30 ms | 105.00 ms | 492.84 ms | 861.42 ms | **97220.33** | 
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | **55.10** ms | 11.27 ms | 86.96 ms | 1198.95 ms | 2825.78 ms | **211038.67** | 
| `php` (`7.3`) | [spiral](https://github.com/spiral/framework) (**2.1**) | **56.06** ms | 55.80 ms | 61.70 ms | 63.85 ms | 269.24 ms | **8993.00** | 
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | **56.92** ms | 25.70 ms | 45.88 ms | 1058.48 ms | 2111.83 ms | **169401.33** | 
| `swift` (`5.1`) | [kitura-nio](https://kitura.io) (**2.7**) | **61.82** ms | 24.55 ms | 38.77 ms | 1359.44 ms | 3238.27 ms | **238393.00** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.1**) | **77.18** ms | 21.07 ms | 144.72 ms | 908.69 ms | 1438.77 ms | **178846.67** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | **80.64** ms | 78.58 ms | 124.98 ms | 162.26 ms | 255.71 ms | **29674.33** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | **94.29** ms | 93.24 ms | 134.04 ms | 164.35 ms | 335.44 ms | **26368.67** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | **96.43** ms | 96.76 ms | 112.64 ms | 125.01 ms | 658.50 ms | **22802.33** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | **106.13** ms | 90.25 ms | 203.84 ms | 260.53 ms | 775.39 ms | **57933.67** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | **139.69** ms | 107.25 ms | 249.90 ms | 361.44 ms | 914.03 ms | **84356.67** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | **182.09** ms | 7.49 ms | 22.79 ms | 4640.77 ms | 7936.59 ms | **783950.00** | 
| `crystal` (`0.31`) | [onyx](https://onyxframework.org) (**0.5**) | **188.58** ms | 190.88 ms | 227.41 ms | 260.81 ms | 305.44 ms | **33331.33** | 
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | **314.26** ms | 85.68 ms | 466.80 ms | 4869.84 ms | 6812.34 ms | **845205.67** | 
| `python` (`3.7`) | [cyclone](https://cyclone.io) (**1.3**) | **333.95** ms | 245.14 ms | 306.21 ms | 4040.61 ms | 6910.34 ms | **638856.00** | 
| `crystal` (`0.31`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | **335.39** ms | 218.22 ms | 239.77 ms | 4015.67 ms | 5939.34 ms | **647323.33** | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (sifrr) (node)


:four: (httpbeast) (nim)


:five: (rapidoid) (java)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 220366.00 | **127.46** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 207765.67 | **248.77** MB |
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 202308.00 | **178.12** MB |
| `nim` (`1.0`) | [httpbeast](https://github.com/dom96/httpbeast) (**0.2**) | 188846.67 | **269.02** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 182263.67 | **328.13** MB |
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 180100.33 | **174.76** MB |
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 176650.33 | **284.51** MB |
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 176225.67 | **354.73** MB |
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 173561.00 | **279.05** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 172922.33 | **167.91** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 163621.00 | **425.32** MB |
| `crystal` (`0.31`) | [toro](https://github.com/soveran/toro) (**0.4**) | 163366.67 | **153.67** MB |
| `crystal` (`0.31`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 157664.67 | **148.35** MB |
| `crystal` (`0.31`) | [raze](https://razecr.com) (**0.3**) | 155580.00 | **146.33** MB |
| `crystal` (`0.31`) | [kemal](https://kemalcr.com) (**0.28**) | 151566.33 | **247.88** MB |
| `crystal` (`0.31`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 151362.33 | **161.80** MB |
| `nim` (`1.0`) | [jester](https://github.com/dom96/jester) (**0.4**) | 141937.67 | **285.44** MB |
| `crystal` (`0.31`) | [amber](https://amberframework.org) (**0.3**) | 140035.00 | **256.46** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 139443.00 | **240.56** MB |
| `rust` (`1.38`) | [gotham](https://gotham.rs) (**0.4**) | 138968.00 | **284.16** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.11**) | 129946.33 | **75.07** MB |
| `crystal` (`0.31`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 125953.00 | **205.88** MB |
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 123830.67 | **316.61** MB |
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 119511.33 | **160.04** MB |
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 113183.67 | **151.45** MB |
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 113136.00 | **151.44** MB |
| `rust` (`1.38`) | [actix-web](https://actix.rs) (**1.0**) | 111802.67 | **165.66** MB |
| `rust` (`1.38`) | [iron](https://ironframework.io) (**0.6**) | 110650.67 | **139.67** MB |
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 109200.00 | **145.27** MB |
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 108267.00 | **143.59** MB |
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 108202.33 | **190.04** MB |
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 108184.00 | **189.90** MB |
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 107703.67 | **189.07** MB |
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 106326.00 | **143.08** MB |
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 105513.67 | **141.00** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 96239.33 | **156.51** MB |
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.13**) | 94490.00 | **197.04** MB |
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 89252.67 | **133.83** MB |
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 87921.67 | **131.81** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 87240.00 | **204.54** MB |
| `go` (`1.13`) | [gf](https://goframe.org) (**1.9**) | 86948.00 | **147.56** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 84816.67 | **132.06** MB |
| `rust` (`1.38`) | [nickel](https://nickel-org.github.io) (**0.11**) | 82767.67 | **164.73** MB |
| `swift` (`5.1`) | [perfect](https://perfect.org) (**3.1**) | 81916.33 | **76.96** MB |
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 81808.67 | **122.62** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 79659.33 | **172.31** MB |
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 77666.67 | **116.47** MB |
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | 73990.67 | **131.83** MB |
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 73460.67 | **110.11** MB |
| `php` (`7.3`) | [one](https://github.com/lizhichao/one) (**1.8**) | 72473.00 | **166.47** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 69976.67 | **53.07** MB |
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 66407.00 | **99.50** MB |
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 66260.33 | **141.31** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 65316.00 | **161.01** MB |
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 62863.67 | **163.42** MB |
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 61880.67 | **130.16** MB |
| `python` (`3.7`) | [asgineer](https://asgineer.readthedocs.io) (**0.7**) | 61159.00 | **109.26** MB |
| `ruby` (`2.6`) | [plezi](https://github.com/boazsegev/plezi) (**0.16**) | 60722.33 | **129.58** MB |
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 59206.33 | **125.40** MB |
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 58492.33 | **237.71** MB |
| `php` (`7.3`) | [basicphp](https://github.com/ray-ang/basicphp) (**0.9**) | 58231.00 | **289.18** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 57789.33 | **101.35** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 57322.67 | **115.17** MB |
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | 55847.00 | **100.21** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 55767.67 | **276.93** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 54347.67 | **269.91** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 53883.67 | **267.72** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 52531.00 | **130.29** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 51902.67 | **257.86** MB |
| `swift` (`5.1`) | [vapor](https://vapor.codes) (**3.3**) | 51607.00 | **88.49** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 50650.67 | **109.15** MB |
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 50619.67 | **123.99** MB |
| `swift` (`5.1`) | [kitura](https://kitura.io) (**2.7**) | 49915.33 | **92.64** MB |
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 48582.33 | **28.06** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**6.1**) | 47975.67 | **249.47** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.24**) | 45971.00 | **43.93** MB |
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 45288.33 | **79.42** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 44017.00 | **115.69** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 43151.67 | **50.96** MB |
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | 40317.67 | **92.61** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.1**) | 37780.33 | **197.39** MB |
| `swift` (`5.1`) | [kitura-nio](https://kitura.io) (**2.7**) | 36905.00 | **69.81** MB |
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 35648.00 | **92.44** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 33876.00 | **19.57** MB |
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 32219.00 | **55.28** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.39**) | 30336.33 | **65.41** MB |
| `fsharp` (`7.3`) | [suave](https://suave.io) (**2.5**) | 29600.67 | **59.82** MB |
| `python` (`3.7`) | [clastic](https://github.com/mahmoud/clastic) (**19.9**) | 27275.33 | **67.58** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 27194.33 | **50.61** MB |
| `crystal` (`0.31`) | [lucky](https://luckyframework.org) (**0.16**) | 26032.33 | **32.00** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 25609.00 | **63.05** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 23740.33 | **13.69** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 23554.33 | **53.32** MB |
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 22505.00 | **21.11** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 22289.67 | **39.71** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 20679.67 | **156.32** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 20592.33 | **39.69** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 17571.00 | **45.58** MB |
| `php` (`7.3`) | [spiral](https://github.com/spiral/framework) (**2.1**) | 17431.00 | **30.23** MB |
| `ruby` (`2.6`) | [grape](https://ruby-grape.org) (**1.2**) | 14230.00 | **8.17** MB |
| `swift` (`5.1`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 12536.67 | **16.05** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | 12325.33 | **24.58** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 10410.67 | **22.68** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 10099.33 | **29.86** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 9523.67 | **27.59** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 7485.00 | **18.42** MB |
| `crystal` (`0.31`) | [onyx](https://onyxframework.org) (**0.5**) | 5247.00 | **13.54** MB |
| `crystal` (`0.31`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 4429.33 | **5.53** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 3963.00 | **24.95** MB |
| `python` (`3.7`) | [cyclone](https://cyclone.io) (**1.3**) | 2169.67 | **5.55** MB |
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | 1312.00 | **2.97** MB |
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

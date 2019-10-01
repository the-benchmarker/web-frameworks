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


:five: cuba (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `rust` (`1.38`) | [nickel](https://nickel-org.github.io) (**0.11**) | **0.09** ms | 0.08 ms | 0.13 ms | 0.18 ms | 5.00 ms | **64.33** | 
| `rust` (`1.38`) | [iron](https://ironframework.io) (**0.6**) | **0.50** ms | 0.42 ms | 0.89 ms | 2.05 ms | 27.11 ms | **464.67** | 
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | **2.56** ms | 0.64 ms | 7.29 ms | 16.16 ms | 65.18 ms | **3673.00** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.24**) | **2.66** ms | 0.65 ms | 7.58 ms | 18.68 ms | 76.81 ms | **4141.67** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | **3.14** ms | 0.69 ms | 9.27 ms | 22.31 ms | 95.41 ms | **4970.33** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | **3.62** ms | 0.50 ms | 11.63 ms | 28.21 ms | 71.38 ms | **6221.67** | 
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | **4.05** ms | 3.65 ms | 7.79 ms | 13.32 ms | 38.54 ms | **2848.00** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | **4.38** ms | 3.62 ms | 9.55 ms | 16.73 ms | 94.00 ms | **4062.33** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | **4.58** ms | 4.13 ms | 8.99 ms | 15.14 ms | 36.85 ms | **3351.33** | 
| `nim` (`1.0`) | [httpbeast](https://github.com/dom96/httpbeast) (**0.2**) | **4.71** ms | 4.17 ms | 8.40 ms | 14.00 ms | 32.94 ms | **2817.00** | 
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | **4.82** ms | 4.41 ms | 8.61 ms | 14.12 ms | 31.44 ms | **2855.67** | 
| `swift` (`5.1`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | **5.04** ms | 0.91 ms | 14.55 ms | 15.20 ms | 2514.19 ms | **51856.33** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | **5.24** ms | 0.47 ms | 17.56 ms | 43.39 ms | 107.48 ms | **9504.33** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | **5.26** ms | 4.76 ms | 8.76 ms | 14.72 ms | 42.52 ms | **2738.00** | 
| `crystal` (`0.31`) | [toro](https://github.com/soveran/toro) (**0.4**) | **5.58** ms | 4.86 ms | 9.59 ms | 15.61 ms | 31.82 ms | **3050.00** | 
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | **5.62** ms | 4.97 ms | 9.07 ms | 16.52 ms | 51.48 ms | **2996.00** | 
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | **5.67** ms | 4.93 ms | 9.23 ms | 17.46 ms | 100.22 ms | **3465.00** | 
| `crystal` (`0.31`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | **5.71** ms | 4.94 ms | 9.77 ms | 16.03 ms | 32.64 ms | **3118.00** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | **5.71** ms | 4.67 ms | 10.96 ms | 22.83 ms | 112.56 ms | **4928.33** | 
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | **5.84** ms | 5.07 ms | 9.50 ms | 17.96 ms | 57.57 ms | **3292.33** | 
| `crystal` (`0.31`) | [raze](https://razecr.com) (**0.3**) | **5.86** ms | 5.08 ms | 9.99 ms | 16.20 ms | 33.88 ms | **3165.00** | 
| `crystal` (`0.31`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | **6.04** ms | 5.09 ms | 10.66 ms | 17.67 ms | 38.27 ms | **3517.33** | 
| `crystal` (`0.31`) | [kemal](https://kemalcr.com) (**0.28**) | **6.13** ms | 5.24 ms | 10.41 ms | 17.22 ms | 37.28 ms | **3321.67** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | **6.25** ms | 0.81 ms | 19.68 ms | 44.68 ms | 133.44 ms | **10063.00** | 
| `crystal` (`0.31`) | [amber](https://amberframework.org) (**0.3**) | **6.63** ms | 5.70 ms | 11.07 ms | 18.37 ms | 38.64 ms | **3495.67** | 
| `rust` (`1.38`) | [gotham](https://gotham.rs) (**0.4**) | **6.81** ms | 5.53 ms | 11.11 ms | 19.20 ms | 237.60 ms | **7965.33** | 
| `nim` (`1.0`) | [jester](https://github.com/dom96/jester) (**0.4**) | **7.15** ms | 6.54 ms | 10.67 ms | 16.56 ms | 46.65 ms | **2920.00** | 
| `crystal` (`0.31`) | [orion](https://github.com/obsidian/orion) (**1.7**) | **7.19** ms | 6.21 ms | 12.11 ms | 20.39 ms | 40.29 ms | **3888.67** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | **7.58** ms | 0.65 ms | 24.86 ms | 55.01 ms | 133.38 ms | **12564.67** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | **8.21** ms | 7.34 ms | 12.41 ms | 19.87 ms | 219.22 ms | **5980.67** | 
| `ruby` (`2.6`) | [grape](https://ruby-grape.org) (**1.2**) | **8.73** ms | 0.77 ms | 28.12 ms | 57.23 ms | 115.46 ms | **13625.00** | 
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | **8.78** ms | 7.45 ms | 14.48 ms | 30.81 ms | 216.12 ms | **6084.67** | 
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | **8.97** ms | 7.48 ms | 15.41 ms | 29.82 ms | 130.46 ms | **5619.67** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.11**) | **9.21** ms | 6.07 ms | 15.07 ms | 84.06 ms | 123.42 ms | **12874.33** | 
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | **9.37** ms | 7.77 ms | 15.77 ms | 34.04 ms | 211.43 ms | **7222.00** | 
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | **9.55** ms | 7.84 ms | 16.24 ms | 36.92 ms | 135.41 ms | **7091.00** | 
| `rust` (`1.38`) | [actix-web](https://actix.rs) (**1.0**) | **10.14** ms | 9.84 ms | 13.17 ms | 17.36 ms | 162.10 ms | **4206.00** | 
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | **10.16** ms | 8.14 ms | 17.20 ms | 39.61 ms | 211.93 ms | **9407.67** | 
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | **10.40** ms | 8.83 ms | 16.51 ms | 37.27 ms | 139.31 ms | **6650.00** | 
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | **10.47** ms | 8.46 ms | 18.41 ms | 40.32 ms | 109.45 ms | **7261.00** | 
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | **10.49** ms | 8.58 ms | 17.73 ms | 41.46 ms | 180.58 ms | **7769.33** | 
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | **10.59** ms | 8.28 ms | 18.72 ms | 43.86 ms | 323.04 ms | **9590.67** | 
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | **11.11** ms | 8.83 ms | 19.45 ms | 45.72 ms | 226.82 ms | **8702.00** | 
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | **11.26** ms | 8.78 ms | 20.68 ms | 45.31 ms | 108.10 ms | **8320.33** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | **11.72** ms | 5.24 ms | 10.62 ms | 244.87 ms | 994.59 ms | **55761.67** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | **11.78** ms | 10.11 ms | 21.64 ms | 40.07 ms | 331.28 ms | **11149.33** | 
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.13**) | **11.85** ms | 9.30 ms | 21.23 ms | 49.00 ms | 195.27 ms | **9237.33** | 
| `swift` (`5.1`) | [perfect](https://perfect.org) (**3.1**) | **11.91** ms | 12.12 ms | 14.24 ms | 16.59 ms | 102.27 ms | **2367.00** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | **12.09** ms | 9.52 ms | 24.22 ms | 45.31 ms | 152.64 ms | **9494.33** | 
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | **13.12** ms | 9.58 ms | 17.92 ms | 80.98 ms | 558.77 ms | **24889.00** | 
| `php` (`7.3`) | [one](https://github.com/lizhichao/one) (**1.8**) | **13.16** ms | 12.65 ms | 21.41 ms | 32.31 ms | 172.85 ms | **7007.33** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | **13.22** ms | 9.65 ms | 17.65 ms | 60.77 ms | 723.76 ms | **29184.67** | 
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | **13.52** ms | 9.98 ms | 19.25 ms | 71.28 ms | 543.36 ms | **22273.67** | 
| `go` (`1.13`) | [gf](https://goframe.org) (**1.9**) | **13.73** ms | 10.77 ms | 24.71 ms | 55.09 ms | 289.34 ms | **11106.33** | 
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | **13.77** ms | 9.75 ms | 17.96 ms | 96.27 ms | 621.24 ms | **29091.67** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | **15.39** ms | 12.84 ms | 27.90 ms | 45.63 ms | 189.78 ms | **9531.67** | 
| `python` (`3.7`) | [asgineer](https://asgineer.readthedocs.io) (**0.7**) | **16.22** ms | 15.11 ms | 24.43 ms | 33.23 ms | 86.16 ms | **6453.00** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | **17.17** ms | 16.92 ms | 23.96 ms | 31.56 ms | 78.71 ms | **5602.00** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | **17.66** ms | 13.56 ms | 25.11 ms | 79.50 ms | 621.22 ms | **23509.67** | 
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | **18.01** ms | 11.32 ms | 21.12 ms | 216.57 ms | 867.72 ms | **46308.00** | 
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | **18.03** ms | 11.73 ms | 23.53 ms | 189.63 ms | 738.05 ms | **38781.33** | 
| `ruby` (`2.6`) | [plezi](https://github.com/boazsegev/plezi) (**0.16**) | **18.95** ms | 16.95 ms | 28.76 ms | 48.75 ms | 94.36 ms | **8040.00** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | **19.38** ms | 18.35 ms | 29.45 ms | 38.41 ms | 82.51 ms | **7330.33** | 
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | **19.45** ms | 17.87 ms | 18.39 ms | 23.29 ms | 1249.68 ms | **29221.33** | 
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | **19.72** ms | 12.71 ms | 48.38 ms | 90.28 ms | 199.31 ms | **20036.00** | 
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | **20.04** ms | 13.76 ms | 24.08 ms | 208.57 ms | 835.63 ms | **45681.33** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | **20.57** ms | 15.50 ms | 41.73 ms | 75.33 ms | 260.19 ms | **14683.33** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | **20.77** ms | 17.98 ms | 34.57 ms | 61.16 ms | 949.37 ms | **26649.00** | 
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | **21.85** ms | 16.71 ms | 28.61 ms | 141.50 ms | 829.44 ms | **38313.33** | 
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | **22.28** ms | 12.55 ms | 23.05 ms | 374.47 ms | 1084.07 ms | **66858.67** | 
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | **22.36** ms | 15.15 ms | 27.02 ms | 254.76 ms | 981.92 ms | **51378.67** | 
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | **23.46** ms | 14.56 ms | 26.62 ms | 307.22 ms | 1008.87 ms | **59286.67** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | **24.34** ms | 22.82 ms | 33.53 ms | 55.98 ms | 94.57 ms | **8188.67** | 
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | **24.46** ms | 20.35 ms | 26.82 ms | 115.85 ms | 739.90 ms | **32390.00** | 
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | **26.49** ms | 14.74 ms | 29.23 ms | 413.33 ms | 1098.28 ms | **70562.67** | 
| `swift` (`5.1`) | [kitura](https://kitura.io) (**2.7**) | **28.22** ms | 19.68 ms | 21.28 ms | 355.43 ms | 1703.21 ms | **86435.67** | 
| `fsharp` (`7.3`) | [suave](https://suave.io) (**2.5**) | **28.56** ms | 12.16 ms | 71.45 ms | 114.34 ms | 452.35 ms | **29554.00** | 
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | **28.84** ms | 25.28 ms | 39.61 ms | 94.28 ms | 316.89 ms | **15882.00** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | **32.26** ms | 2.45 ms | 106.86 ms | 287.82 ms | 935.64 ms | **61429.33** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.39**) | **32.59** ms | 32.21 ms | 47.93 ms | 63.45 ms | 203.45 ms | **11867.67** | 
| `swift` (`5.1`) | [kitura-nio](https://kitura.io) (**2.7**) | **33.43** ms | 24.04 ms | 36.84 ms | 356.34 ms | 1429.06 ms | **76311.67** | 
| `crystal` (`0.31`) | [lucky](https://luckyframework.org) (**0.16**) | **38.05** ms | 36.12 ms | 46.85 ms | 54.88 ms | 274.61 ms | **9315.00** | 
| `swift` (`5.1`) | [vapor](https://vapor.codes) (**3.3**) | **39.22** ms | 17.84 ms | 34.13 ms | 798.10 ms | 2526.43 ms | **142837.33** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | **40.33** ms | 24.29 ms | 86.81 ms | 120.14 ms | 294.94 ms | **29218.67** | 
| `python` (`3.7`) | [clastic](https://github.com/mahmoud/clastic) (**19.9**) | **42.52** ms | 25.82 ms | 100.11 ms | 126.02 ms | 247.57 ms | **31056.00** | 
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | **42.84** ms | 25.49 ms | 42.49 ms | 629.27 ms | 1559.75 ms | **102462.00** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | **43.00** ms | 39.61 ms | 66.75 ms | 76.38 ms | 150.86 ms | **14660.33** | 
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | **43.37** ms | 41.50 ms | 48.49 ms | 54.84 ms | 550.70 ms | **16193.00** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | **43.94** ms | 14.42 ms | 81.01 ms | 453.75 ms | 810.97 ms | **90244.00** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | **44.03** ms | 14.73 ms | 90.45 ms | 456.06 ms | 721.35 ms | **90414.00** | 
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | **44.74** ms | 25.63 ms | 42.05 ms | 685.81 ms | 1588.91 ms | **112316.33** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | **45.32** ms | 27.47 ms | 104.03 ms | 138.77 ms | 326.54 ms | **32644.33** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | **46.18** ms | 15.74 ms | 88.26 ms | 475.60 ms | 900.28 ms | **95256.00** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | **48.14** ms | 47.57 ms | 65.03 ms | 99.50 ms | 262.57 ms | **15801.67** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | **48.38** ms | 15.52 ms | 95.37 ms | 516.13 ms | 830.49 ms | **103034.00** | 
| `php` (`7.3`) | [basicphp](https://github.com/ray-ang/basicphp) (**0.9**) | **48.57** ms | 15.75 ms | 89.35 ms | 508.31 ms | 1062.69 ms | **99774.67** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | **49.26** ms | 46.70 ms | 74.51 ms | 111.50 ms | 194.62 ms | **19950.00** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**6.0**) | **53.21** ms | 16.29 ms | 103.64 ms | 537.09 ms | 1035.32 ms | **104909.00** | 
| `crystal` (`0.31`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | **53.99** ms | 53.09 ms | 61.24 ms | 64.55 ms | 95.78 ms | **5729.00** | 
| `php` (`7.3`) | [spiral](https://github.com/spiral/framework) (**2.1**) | **57.25** ms | 56.54 ms | 62.58 ms | 65.71 ms | 476.73 ms | **13822.00** | 
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | **64.11** ms | 9.38 ms | 70.48 ms | 1723.98 ms | 3573.86 ms | **297093.33** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | **81.23** ms | 83.79 ms | 109.40 ms | 142.15 ms | 217.41 ms | **23091.33** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.0**) | **83.41** ms | 23.50 ms | 162.42 ms | 985.63 ms | 1622.07 ms | **194856.33** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | **95.51** ms | 89.12 ms | 152.00 ms | 186.91 ms | 264.74 ms | **32468.00** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | **97.37** ms | 95.86 ms | 119.74 ms | 135.07 ms | 904.67 ms | **32233.00** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | **104.70** ms | 96.33 ms | 163.77 ms | 216.60 ms | 1087.78 ms | **51790.00** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | **134.33** ms | 110.16 ms | 277.23 ms | 298.24 ms | 783.82 ms | **64981.33** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | **203.75** ms | 8.00 ms | 25.37 ms | 5058.29 ms | 7934.62 ms | **854022.00** | 
| `crystal` (`0.31`) | [onyx](https://onyxframework.org) (**0.5**) | **204.71** ms | 204.72 ms | 241.84 ms | 273.04 ms | 310.49 ms | **29291.00** | 
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | **210.89** ms | 74.13 ms | 389.61 ms | 2546.47 ms | 3040.67 ms | **457339.00** | 

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
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 225127.33 | **130.18** MB |
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 214431.67 | **188.73** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 205271.33 | **245.71** MB |
| `nim` (`1.0`) | [httpbeast](https://github.com/dom96/httpbeast) (**0.2**) | 196446.00 | **279.74** MB |
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 189420.00 | **183.94** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 171798.33 | **166.84** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 171435.33 | **445.56** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 168521.00 | **303.22** MB |
| `crystal` (`0.31`) | [toro](https://github.com/soveran/toro) (**0.4**) | 167878.67 | **157.99** MB |
| `crystal` (`0.31`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 164841.00 | **155.13** MB |
| `crystal` (`0.31`) | [raze](https://razecr.com) (**0.3**) | 161373.33 | **151.84** MB |
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 160831.00 | **323.40** MB |
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 160229.67 | **257.05** MB |
| `crystal` (`0.31`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 158643.00 | **169.12** MB |
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 156502.00 | **252.02** MB |
| `crystal` (`0.31`) | [kemal](https://kemalcr.com) (**0.28**) | 155278.00 | **253.90** MB |
| `nim` (`1.0`) | [jester](https://github.com/dom96/jester) (**0.4**) | 145783.00 | **293.12** MB |
| `rust` (`1.38`) | [gotham](https://gotham.rs) (**0.4**) | 144750.67 | **296.47** MB |
| `crystal` (`0.31`) | [amber](https://amberframework.org) (**0.3**) | 143993.33 | **263.81** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.11**) | 137719.00 | **79.60** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 134016.00 | **231.16** MB |
| `crystal` (`0.31`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 133586.00 | **218.47** MB |
| `rust` (`1.38`) | [actix-web](https://actix.rs) (**1.0**) | 113800.67 | **168.28** MB |
| `rust` (`1.38`) | [iron](https://ironframework.io) (**0.6**) | 113662.33 | **143.19** MB |
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 111602.67 | **149.35** MB |
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 110125.00 | **281.29** MB |
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 107241.00 | **188.29** MB |
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 105584.67 | **142.00** MB |
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 101822.33 | **136.41** MB |
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 98648.00 | **173.12** MB |
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 98331.67 | **172.61** MB |
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 97624.00 | **129.78** MB |
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 97383.00 | **129.24** MB |
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 93741.67 | **126.00** MB |
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 93527.33 | **124.71** MB |
| `rust` (`1.38`) | [nickel](https://nickel-org.github.io) (**0.11**) | 91981.00 | **182.85** MB |
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.13**) | 89000.00 | **185.77** MB |
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 88809.33 | **133.04** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 87545.00 | **205.32** MB |
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 87497.33 | **131.15** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 87069.33 | **135.61** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 85932.33 | **140.07** MB |
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 83156.33 | **124.62** MB |
| `swift` (`5.1`) | [perfect](https://perfect.org) (**3.1**) | 81026.33 | **76.23** MB |
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | 79861.00 | **142.15** MB |
| `go` (`1.13`) | [gf](https://goframe.org) (**1.9**) | 77495.33 | **131.01** MB |
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 75922.33 | **113.80** MB |
| `php` (`7.3`) | [one](https://github.com/lizhichao/one) (**1.8**) | 74420.00 | **171.05** MB |
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 71043.33 | **106.45** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 70664.00 | **152.81** MB |
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 69917.67 | **104.82** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 66056.33 | **49.57** MB |
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 65949.33 | **140.50** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 65487.33 | **161.29** MB |
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 64931.67 | **136.55** MB |
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 60869.00 | **159.65** MB |
| `python` (`3.7`) | [asgineer](https://asgineer.readthedocs.io) (**0.7**) | 60610.67 | **108.23** MB |
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 60060.67 | **127.20** MB |
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 58749.33 | **238.91** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 57104.00 | **114.68** MB |
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | 56458.00 | **101.34** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 54275.33 | **269.61** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 53545.00 | **266.06** MB |
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 53232.00 | **130.46** MB |
| `ruby` (`2.6`) | [plezi](https://github.com/boazsegev/plezi) (**0.16**) | 52562.00 | **112.09** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 52339.33 | **129.85** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 51558.00 | **256.37** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 50809.33 | **89.19** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 50787.67 | **252.45** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 50708.33 | **109.12** MB |
| `swift` (`5.1`) | [kitura](https://kitura.io) (**2.7**) | 49711.33 | **92.34** MB |
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 49321.00 | **28.52** MB |
| `php` (`7.3`) | [basicphp](https://github.com/ray-ang/basicphp) (**0.9**) | 48755.67 | **242.38** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.24**) | 47976.00 | **45.80** MB |
| `swift` (`5.1`) | [vapor](https://vapor.codes) (**3.3**) | 47752.00 | **81.61** MB |
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 45746.00 | **80.28** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**6.0**) | 44761.67 | **233.10** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 40769.00 | **48.18** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 40432.33 | **106.37** MB |
| `swift` (`5.1`) | [kitura-nio](https://kitura.io) (**2.7**) | 37582.33 | **71.11** MB |
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | 35812.33 | **82.34** MB |
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 35588.00 | **92.19** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 35382.33 | **20.42** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.0**) | 35129.67 | **183.52** MB |
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 34018.00 | **58.22** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.39**) | 30500.33 | **65.81** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 26931.00 | **50.05** MB |
| `python` (`3.7`) | [clastic](https://github.com/mahmoud/clastic) (**19.9**) | 25941.67 | **64.22** MB |
| `crystal` (`0.31`) | [lucky](https://luckyframework.org) (**0.16**) | 25866.67 | **31.79** MB |
| `fsharp` (`7.3`) | [suave](https://suave.io) (**2.5**) | 24978.33 | **50.45** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 24839.00 | **61.20** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 24419.67 | **14.08** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 23121.67 | **52.33** MB |
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 22798.00 | **21.39** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 20618.00 | **39.76** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 20464.00 | **154.91** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 20434.33 | **36.44** MB |
| `crystal` (`0.31`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 18431.33 | **23.02** MB |
| `php` (`7.3`) | [spiral](https://github.com/spiral/framework) (**2.1**) | 17120.33 | **29.71** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 16865.00 | **43.74** MB |
| `ruby` (`2.6`) | [grape](https://ruby-grape.org) (**1.2**) | 14746.00 | **8.44** MB |
| `swift` (`5.1`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 12364.33 | **15.82** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | 12166.33 | **24.25** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 10305.00 | **22.45** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 10005.33 | **29.58** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 9481.00 | **27.44** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 7429.67 | **18.26** MB |
| `crystal` (`0.31`) | [onyx](https://onyxframework.org) (**0.5**) | 4827.00 | **12.46** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 3970.67 | **24.96** MB |
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | 452.33 | **1.03** MB |
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

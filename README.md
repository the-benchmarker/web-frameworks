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
Last update: 2019-10-02
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
| `rust` (`1.38`) | [nickel](https://nickel-org.github.io) (**0.11**) | **0.11** ms | 0.10 ms | 0.14 ms | 0.25 ms | 6.15 ms | **95.00** | 
| `rust` (`1.38`) | [iron](https://ironframework.io) (**0.6**) | **0.55** ms | 0.38 ms | 1.00 ms | 3.59 ms | 46.90 ms | **730.00** | 
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | **2.67** ms | 0.62 ms | 7.82 ms | 17.14 ms | 50.00 ms | **3879.33** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.24**) | **2.89** ms | 0.89 ms | 8.03 ms | 18.74 ms | 86.42 ms | **4212.33** | 
| `swift` (`5.1`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | **3.11** ms | 0.84 ms | 14.55 ms | 15.82 ms | 860.88 ms | **9539.00** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | **3.21** ms | 0.76 ms | 9.34 ms | 20.16 ms | 67.11 ms | **4596.00** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | **3.99** ms | 0.90 ms | 11.63 ms | 26.82 ms | 90.71 ms | **5991.33** | 
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | **4.61** ms | 3.80 ms | 9.47 ms | 17.15 ms | 52.17 ms | **3867.67** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | **4.68** ms | 4.17 ms | 9.19 ms | 15.22 ms | 38.20 ms | **3429.00** | 
| `nim` (`1.0`) | [httpbeast](https://github.com/dom96/httpbeast) (**0.2**) | **4.77** ms | 4.15 ms | 8.49 ms | 14.87 ms | 45.46 ms | **3009.00** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | **4.94** ms | 4.22 ms | 9.69 ms | 19.99 ms | 89.12 ms | **4550.00** | 
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | **5.42** ms | 4.66 ms | 9.98 ms | 17.91 ms | 67.97 ms | **3902.33** | 
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | **5.44** ms | 4.88 ms | 8.59 ms | 15.13 ms | 104.19 ms | **2877.67** | 
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | **5.46** ms | 4.84 ms | 8.89 ms | 15.73 ms | 46.86 ms | **2881.00** | 
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | **5.56** ms | 4.90 ms | 8.93 ms | 15.87 ms | 152.20 ms | **3489.00** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | **5.80** ms | 4.65 ms | 10.77 ms | 22.68 ms | 223.80 ms | **6099.00** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | **6.08** ms | 5.29 ms | 10.25 ms | 17.79 ms | 122.04 ms | **3760.67** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | **6.11** ms | 2.06 ms | 16.78 ms | 36.67 ms | 109.77 ms | **8230.67** | 
| `crystal` (`0.31`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | **6.20** ms | 5.35 ms | 10.65 ms | 17.88 ms | 63.79 ms | **3652.67** | 
| `crystal` (`0.31`) | [toro](https://github.com/soveran/toro) (**0.4**) | **6.26** ms | 5.51 ms | 10.82 ms | 18.78 ms | 65.21 ms | **3887.67** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | **6.52** ms | 1.44 ms | 19.25 ms | 39.93 ms | 94.24 ms | **9294.00** | 
| `crystal` (`0.31`) | [raze](https://razecr.com) (**0.3**) | **6.74** ms | 5.95 ms | 11.57 ms | 19.96 ms | 66.46 ms | **4114.33** | 
| `crystal` (`0.31`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | **6.90** ms | 6.05 ms | 12.00 ms | 20.28 ms | 57.06 ms | **4153.00** | 
| `crystal` (`0.31`) | [kemal](https://kemalcr.com) (**0.28**) | **7.01** ms | 6.14 ms | 12.13 ms | 20.20 ms | 69.69 ms | **4175.33** | 
| `nim` (`1.0`) | [jester](https://github.com/dom96/jester) (**0.4**) | **7.20** ms | 6.54 ms | 10.80 ms | 17.12 ms | 49.20 ms | **3070.33** | 
| `crystal` (`0.31`) | [amber](https://amberframework.org) (**0.3**) | **7.40** ms | 6.52 ms | 12.84 ms | 20.78 ms | 54.53 ms | **4194.33** | 
| `rust` (`1.38`) | [gotham](https://gotham.rs) (**0.4**) | **7.55** ms | 6.54 ms | 13.24 ms | 22.28 ms | 132.06 ms | **5520.00** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | **8.06** ms | 2.59 ms | 23.05 ms | 49.69 ms | 124.00 ms | **11292.67** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | **8.27** ms | 7.23 ms | 12.51 ms | 24.07 ms | 218.71 ms | **6430.00** | 
| `crystal` (`0.31`) | [orion](https://github.com/obsidian/orion) (**1.7**) | **8.39** ms | 7.54 ms | 14.49 ms | 25.10 ms | 65.19 ms | **4943.67** | 
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | **8.56** ms | 7.26 ms | 14.54 ms | 26.88 ms | 118.51 ms | **4975.00** | 
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | **8.63** ms | 7.39 ms | 13.94 ms | 30.39 ms | 164.27 ms | **5709.67** | 
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | **9.73** ms | 8.48 ms | 14.70 ms | 32.38 ms | 243.25 ms | **7039.67** | 
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | **9.75** ms | 8.08 ms | 16.43 ms | 37.07 ms | 160.05 ms | **6800.33** | 
| `ruby` (`2.6`) | [grape](https://ruby-grape.org) (**1.2**) | **9.78** ms | 1.68 ms | 27.79 ms | 57.73 ms | 148.32 ms | **13378.67** | 
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | **9.81** ms | 8.13 ms | 16.63 ms | 36.74 ms | 154.66 ms | **6631.33** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.11**) | **9.92** ms | 7.30 ms | 16.27 ms | 80.99 ms | 124.32 ms | **12155.00** | 
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | **10.12** ms | 8.56 ms | 16.56 ms | 37.25 ms | 134.58 ms | **6700.33** | 
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | **10.19** ms | 8.36 ms | 17.50 ms | 38.85 ms | 134.24 ms | **7043.33** | 
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | **10.26** ms | 8.24 ms | 17.94 ms | 39.92 ms | 228.87 ms | **7810.67** | 
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | **10.42** ms | 8.39 ms | 18.36 ms | 40.64 ms | 154.96 ms | **7679.00** | 
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | **10.48** ms | 8.48 ms | 18.32 ms | 40.49 ms | 134.00 ms | **7560.00** | 
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | **10.54** ms | 8.49 ms | 18.66 ms | 41.63 ms | 102.38 ms | **7516.00** | 
| `rust` (`1.38`) | [actix-web](https://actix.rs) (**1.0**) | **11.39** ms | 10.89 ms | 15.47 ms | 21.92 ms | 118.69 ms | **4156.67** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | **11.55** ms | 9.30 ms | 14.89 ms | 35.12 ms | 576.14 ms | **22549.00** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | **12.48** ms | 9.61 ms | 23.07 ms | 48.13 ms | 277.30 ms | **11552.67** | 
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.13**) | **12.53** ms | 9.69 ms | 22.89 ms | 50.89 ms | 212.66 ms | **10689.00** | 
| `go` (`1.13`) | [gf](https://goframe.org) (**1.9**) | **12.64** ms | 10.44 ms | 21.50 ms | 48.40 ms | 148.37 ms | **8461.00** | 
| `swift` (`5.1`) | [perfect](https://perfect.org) (**3.1**) | **12.99** ms | 13.05 ms | 15.61 ms | 19.11 ms | 108.95 ms | **3288.33** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | **13.07** ms | 11.27 ms | 24.02 ms | 45.86 ms | 325.36 ms | **12628.67** | 
| `php` (`7.3`) | [one](https://github.com/lizhichao/one) (**1.8**) | **13.48** ms | 12.60 ms | 22.72 ms | 36.89 ms | 152.07 ms | **7529.67** | 
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | **14.32** ms | 10.22 ms | 21.21 ms | 77.20 ms | 605.00 ms | **25789.33** | 
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | **14.42** ms | 11.00 ms | 20.05 ms | 53.32 ms | 577.44 ms | **24465.67** | 
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | **15.16** ms | 10.06 ms | 19.12 ms | 141.90 ms | 689.63 ms | **34693.67** | 
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | **15.71** ms | 10.43 ms | 21.30 ms | 146.84 ms | 671.10 ms | **33783.67** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | **16.45** ms | 5.46 ms | 13.09 ms | 360.08 ms | 1730.80 ms | **83191.33** | 
| `python` (`3.7`) | [asgineer](https://asgineer.readthedocs.io) (**0.7**) | **17.16** ms | 15.72 ms | 25.98 ms | 44.75 ms | 114.07 ms | **7773.00** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | **17.46** ms | 13.16 ms | 24.89 ms | 71.51 ms | 652.64 ms | **25366.00** | 
| `ruby` (`2.6`) | [plezi](https://github.com/boazsegev/plezi) (**0.16**) | **18.00** ms | 16.41 ms | 26.63 ms | 47.93 ms | 113.43 ms | **7901.67** | 
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | **18.08** ms | 13.98 ms | 39.23 ms | 73.93 ms | 163.08 ms | **16078.33** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | **18.54** ms | 17.12 ms | 28.49 ms | 46.98 ms | 140.23 ms | **8243.67** | 
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | **19.31** ms | 18.52 ms | 21.58 ms | 31.31 ms | 447.57 ms | **10552.00** | 
| `fsharp` (`7.3`) | [suave](https://suave.io) (**2.5**) | **19.48** ms | 16.83 ms | 27.66 ms | 54.36 ms | 318.25 ms | **10583.33** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | **19.55** ms | 14.42 ms | 38.24 ms | 76.88 ms | 223.85 ms | **15282.00** | 
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | **20.21** ms | 11.95 ms | 23.79 ms | 277.40 ms | 978.34 ms | **54708.00** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | **20.71** ms | 18.92 ms | 31.90 ms | 52.27 ms | 159.80 ms | **9432.00** | 
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | **21.08** ms | 15.25 ms | 26.87 ms | 155.64 ms | 908.15 ms | **44266.67** | 
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | **21.54** ms | 12.56 ms | 22.79 ms | 348.50 ms | 1082.13 ms | **64223.00** | 
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | **21.77** ms | 14.14 ms | 24.72 ms | 275.90 ms | 1012.45 ms | **55129.33** | 
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | **22.01** ms | 14.96 ms | 29.28 ms | 179.62 ms | 902.33 ms | **44460.00** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | **22.79** ms | 14.62 ms | 53.98 ms | 90.15 ms | 250.47 ms | **19297.67** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | **22.93** ms | 22.33 ms | 30.12 ms | 42.42 ms | 84.63 ms | **6021.33** | 
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | **23.22** ms | 14.22 ms | 27.71 ms | 283.39 ms | 986.90 ms | **56063.00** | 
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | **25.93** ms | 17.02 ms | 29.28 ms | 323.91 ms | 1133.36 ms | **63658.33** | 
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | **26.40** ms | 24.93 ms | 31.20 ms | 63.23 ms | 332.81 ms | **11927.67** | 
| `swift` (`5.1`) | [kitura](https://kitura.io) (**2.7**) | **27.25** ms | 20.29 ms | 23.67 ms | 256.88 ms | 1475.90 ms | **73776.00** | 
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | **27.89** ms | 20.91 ms | 36.23 ms | 177.17 ms | 855.83 ms | **41516.33** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.39**) | **32.87** ms | 30.26 ms | 50.27 ms | 76.81 ms | 286.71 ms | **14909.33** | 
| `swift` (`5.1`) | [vapor](https://vapor.codes) (**3.3**) | **33.61** ms | 17.59 ms | 32.28 ms | 586.95 ms | 1891.30 ms | **113058.00** | 
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | **34.31** ms | 9.99 ms | 61.20 ms | 530.73 ms | 1714.92 ms | **104725.67** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | **36.55** ms | 9.52 ms | 108.08 ms | 261.07 ms | 796.16 ms | **56872.00** | 
| `crystal` (`0.31`) | [lucky](https://luckyframework.org) (**0.16**) | **40.34** ms | 38.57 ms | 50.34 ms | 57.95 ms | 209.91 ms | **8655.33** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | **41.37** ms | 32.19 ms | 89.39 ms | 126.53 ms | 511.19 ms | **31498.67** | 
| `swift` (`5.1`) | [kitura-nio](https://kitura.io) (**2.7**) | **41.68** ms | 25.68 ms | 39.92 ms | 563.52 ms | 1925.29 ms | **112818.67** | 
| `python` (`3.7`) | [clastic](https://github.com/mahmoud/clastic) (**19.9**) | **41.85** ms | 27.37 ms | 97.32 ms | 120.77 ms | 347.07 ms | **29545.67** | 
| `php` (`7.3`) | [basicphp](https://github.com/ray-ang/basicphp) (**0.9**) | **42.00** ms | 14.34 ms | 81.02 ms | 417.53 ms | 767.11 ms | **82380.67** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | **43.00** ms | 44.31 ms | 56.67 ms | 68.59 ms | 122.10 ms | **11503.00** | 
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | **43.07** ms | 41.58 ms | 48.34 ms | 55.81 ms | 473.85 ms | **14819.00** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | **43.63** ms | 14.90 ms | 82.55 ms | 446.51 ms | 735.21 ms | **88327.67** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | **44.79** ms | 15.22 ms | 86.56 ms | 462.87 ms | 703.37 ms | **90934.00** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | **45.40** ms | 43.51 ms | 63.82 ms | 91.39 ms | 156.79 ms | **14514.33** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | **45.51** ms | 15.09 ms | 82.96 ms | 470.69 ms | 738.34 ms | **92568.67** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | **45.53** ms | 33.30 ms | 93.28 ms | 110.82 ms | 259.32 ms | **25355.67** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | **48.82** ms | 44.91 ms | 75.72 ms | 114.85 ms | 240.54 ms | **19113.00** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**6.0**) | **50.46** ms | 17.09 ms | 96.98 ms | 514.73 ms | 825.49 ms | **100833.00** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | **50.52** ms | 17.91 ms | 34.91 ms | 1191.80 ms | 2401.14 ms | **202960.67** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | **50.96** ms | 15.19 ms | 92.41 ms | 543.75 ms | 964.33 ms | **106103.00** | 
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | **56.87** ms | 30.61 ms | 60.49 ms | 873.74 ms | 1832.96 ms | **138277.00** | 
| `php` (`7.3`) | [spiral](https://github.com/spiral/framework) (**2.1**) | **58.84** ms | 58.89 ms | 65.44 ms | 68.25 ms | 339.10 ms | **8865.00** | 
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | **65.35** ms | 26.84 ms | 54.87 ms | 1158.03 ms | 2203.09 ms | **187669.33** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.1**) | **81.75** ms | 22.91 ms | 140.71 ms | 968.60 ms | 1457.19 ms | **189893.67** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | **84.01** ms | 80.74 ms | 123.79 ms | 169.64 ms | 329.48 ms | **32289.00** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | **96.14** ms | 98.90 ms | 113.52 ms | 130.25 ms | 724.46 ms | **26345.00** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | **100.52** ms | 94.28 ms | 146.27 ms | 181.03 ms | 237.21 ms | **30255.67** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | **108.02** ms | 90.00 ms | 200.86 ms | 252.06 ms | 897.46 ms | **57821.33** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | **140.35** ms | 107.80 ms | 301.36 ms | 397.34 ms | 806.05 ms | **79821.33** | 
| `crystal` (`0.31`) | [onyx](https://onyxframework.org) (**0.5**) | **175.70** ms | 175.96 ms | 210.98 ms | 232.72 ms | 253.94 ms | **27506.33** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | **226.60** ms | 8.52 ms | 196.72 ms | 4874.18 ms | 7912.29 ms | **864715.67** | 
| `crystal` (`0.31`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | **292.26** ms | 226.99 ms | 243.18 ms | 2801.87 ms | 4850.24 ms | **450219.33** | 
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | **544.22** ms | 98.39 ms | 2391.73 ms | 3962.59 ms | 6951.10 ms | **1134387.00** | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (sifrr) (node)


:four: (httpbeast) (nim)


:five: (drogon) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 196974.67 | **113.93** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 196542.00 | **235.26** MB |
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 196149.33 | **172.72** MB |
| `nim` (`1.0`) | [httpbeast](https://github.com/dom96/httpbeast) (**0.2**) | 194033.00 | **276.34** MB |
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 170695.00 | **165.75** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 167697.00 | **301.72** MB |
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 165157.00 | **265.75** MB |
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 164670.33 | **331.27** MB |
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 162201.00 | **260.91** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 151587.67 | **393.90** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 151081.00 | **146.69** MB |
| `crystal` (`0.31`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 150765.67 | **141.85** MB |
| `crystal` (`0.31`) | [toro](https://github.com/soveran/toro) (**0.4**) | 148555.67 | **139.51** MB |
| `nim` (`1.0`) | [jester](https://github.com/dom96/jester) (**0.4**) | 145268.67 | **291.97** MB |
| `crystal` (`0.31`) | [raze](https://razecr.com) (**0.3**) | 140117.67 | **131.76** MB |
| `crystal` (`0.31`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 136667.33 | **145.50** MB |
| `crystal` (`0.31`) | [kemal](https://kemalcr.com) (**0.28**) | 134465.67 | **219.87** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 130379.00 | **224.98** MB |
| `crystal` (`0.31`) | [amber](https://amberframework.org) (**0.3**) | 128504.00 | **235.36** MB |
| `rust` (`1.38`) | [gotham](https://gotham.rs) (**0.4**) | 125352.67 | **256.78** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.11**) | 119255.67 | **68.94** MB |
| `crystal` (`0.31`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 114671.00 | **187.48** MB |
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 114063.67 | **152.82** MB |
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 113046.00 | **288.82** MB |
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 103528.67 | **138.54** MB |
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 102957.67 | **136.66** MB |
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 102799.00 | **137.49** MB |
| `rust` (`1.38`) | [actix-web](https://actix.rs) (**1.0**) | 102248.33 | **151.43** MB |
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 100835.00 | **135.56** MB |
| `rust` (`1.38`) | [iron](https://ironframework.io) (**0.6**) | 100655.33 | **127.57** MB |
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 99908.00 | **133.38** MB |
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 99020.00 | **131.80** MB |
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 98631.67 | **173.22** MB |
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 97990.67 | **172.06** MB |
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 97572.67 | **171.29** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 92014.67 | **149.85** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 85276.67 | **199.93** MB |
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.13**) | 84959.67 | **177.37** MB |
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 83864.00 | **125.65** MB |
| `go` (`1.13`) | [gf](https://goframe.org) (**1.9**) | 82694.00 | **140.58** MB |
| `rust` (`1.38`) | [nickel](https://nickel-org.github.io) (**0.11**) | 80932.33 | **160.92** MB |
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 80831.33 | **121.36** MB |
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 80477.67 | **120.56** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 80176.00 | **124.89** MB |
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | 77954.00 | **139.12** MB |
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 77937.00 | **116.82** MB |
| `swift` (`5.1`) | [perfect](https://perfect.org) (**3.1**) | 75255.33 | **70.74** MB |
| `php` (`7.3`) | [one](https://github.com/lizhichao/one) (**1.8**) | 73583.33 | **168.83** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 72759.00 | **157.43** MB |
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 71157.33 | **106.60** MB |
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 70132.67 | **105.03** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 67433.00 | **50.46** MB |
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 65535.33 | **172.95** MB |
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 63242.00 | **134.88** MB |
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 59189.00 | **124.31** MB |
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 58742.67 | **238.72** MB |
| `python` (`3.7`) | [asgineer](https://asgineer.readthedocs.io) (**0.7**) | 58173.67 | **103.87** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 56313.67 | **138.75** MB |
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 55872.67 | **118.31** MB |
| `ruby` (`2.6`) | [plezi](https://github.com/boazsegev/plezi) (**0.16**) | 55812.67 | **119.03** MB |
| `php` (`7.3`) | [basicphp](https://github.com/ray-ang/basicphp) (**0.9**) | 54939.33 | **272.91** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 53635.00 | **107.92** MB |
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 52962.67 | **129.71** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 52893.00 | **262.75** MB |
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | 52614.33 | **94.74** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 51732.67 | **257.17** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 51556.33 | **90.42** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 51529.33 | **256.02** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 50047.00 | **124.08** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 49790.33 | **247.46** MB |
| `swift` (`5.1`) | [vapor](https://vapor.codes) (**3.3**) | 49089.00 | **83.67** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 48215.33 | **103.89** MB |
| `swift` (`5.1`) | [kitura](https://kitura.io) (**2.7**) | 47527.00 | **88.32** MB |
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 47153.00 | **27.27** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**6.0**) | 45288.33 | **235.61** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.24**) | 43871.00 | **41.91** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 42581.00 | **111.79** MB |
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 41534.00 | **72.95** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 39798.00 | **47.01** MB |
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | 38477.00 | **88.46** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.1**) | 35454.67 | **185.22** MB |
| `swift` (`5.1`) | [kitura-nio](https://kitura.io) (**2.7**) | 35173.33 | **66.53** MB |
| `fsharp` (`7.3`) | [suave](https://suave.io) (**2.5**) | 32857.67 | **66.63** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 31790.00 | **18.38** MB |
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 31681.67 | **82.03** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.39**) | 30410.33 | **65.64** MB |
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 27932.33 | **47.77** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 26316.33 | **48.96** MB |
| `python` (`3.7`) | [clastic](https://github.com/mahmoud/clastic) (**19.9**) | 25825.00 | **63.98** MB |
| `crystal` (`0.31`) | [lucky](https://luckyframework.org) (**0.16**) | 24456.33 | **30.01** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 23119.00 | **52.41** MB |
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 22756.67 | **21.35** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 22390.00 | **55.21** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 21931.33 | **39.09** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 20893.67 | **12.08** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 20292.67 | **39.16** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 19568.00 | **148.07** MB |
| `php` (`7.3`) | [spiral](https://github.com/spiral/framework) (**2.1**) | 16557.67 | **28.74** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 15837.33 | **41.09** MB |
| `ruby` (`2.6`) | [grape](https://ruby-grape.org) (**1.2**) | 13109.33 | **7.56** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | 11723.67 | **23.40** MB |
| `swift` (`5.1`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 11117.00 | **14.23** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 10048.00 | **29.73** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 9856.00 | **21.47** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 9252.33 | **26.82** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 7297.33 | **17.99** MB |
| `crystal` (`0.31`) | [onyx](https://onyxframework.org) (**0.5**) | 5640.00 | **14.55** MB |
| `crystal` (`0.31`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 4271.33 | **5.33** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 3501.00 | **22.04** MB |
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | 986.00 | **2.23** MB |
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

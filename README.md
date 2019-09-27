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
Last update: 2019-09-26
```
OS: Linux (version: 5.2.15-200.fc30.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: rack-routing (ruby)


:four: flame (ruby)


:five: cuba (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 0.10 ms | **0.10** ms | 0.13 ms | 0.17 ms | 4.62 ms | **54.00** | 
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 0.49 ms | **0.38** ms | 0.99 ms | 1.81 ms | 18.43 ms | **431.67** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 3.60 ms | **0.42** ms | 12.08 ms | 29.62 ms | 82.98 ms | **6521.67** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 5.45 ms | **0.49** ms | 18.54 ms | 47.06 ms | 113.17 ms | **10224.00** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 2.92 ms | **0.53** ms | 9.07 ms | 23.07 ms | 89.14 ms | **5116.00** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 6.01 ms | **0.55** ms | 20.24 ms | 44.42 ms | 104.97 ms | **10189.33** | 
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 2.52 ms | **0.55** ms | 7.43 ms | 16.73 ms | 69.42 ms | **3790.33** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 7.30 ms | **0.60** ms | 24.32 ms | 53.87 ms | 124.18 ms | **12287.67** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.24**) | 2.64 ms | **0.73** ms | 7.36 ms | 15.19 ms | 43.62 ms | **3506.33** | 
| `ruby` (`2.6`) | [grape](https://ruby-grape.org) (**1.2**) | 8.99 ms | **0.87** ms | 28.44 ms | 58.18 ms | 138.62 ms | **13787.00** | 
| `swift` (`5.1`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 6.18 ms | **1.19** ms | 14.86 ms | 15.42 ms | 2529.49 ms | **52665.33** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 30.34 ms | **2.03** ms | 101.43 ms | 275.67 ms | 822.15 ms | **58818.67** | 
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 4.08 ms | **3.65** ms | 7.87 ms | 13.71 ms | 38.01 ms | **2923.67** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 4.20 ms | **3.79** ms | 8.68 ms | 14.50 ms | 35.59 ms | **3258.67** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 4.49 ms | **4.11** ms | 8.72 ms | 14.46 ms | 36.16 ms | **3166.33** | 
| `nim` (`1.0`) | [httpbeast](https://github.com/dom96/httpbeast) (**0.2**) | 4.66 ms | **4.17** ms | 8.18 ms | 13.67 ms | 32.27 ms | **2702.33** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 5.50 ms | **4.32** ms | 9.95 ms | 20.58 ms | 175.81 ms | **6941.33** | 
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 4.92 ms | **4.39** ms | 9.17 ms | 15.20 ms | 38.12 ms | **3211.33** | 
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 5.13 ms | **4.60** ms | 7.99 ms | 14.43 ms | 99.39 ms | **3568.67** | 
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 5.14 ms | **4.67** ms | 7.98 ms | 14.18 ms | 93.75 ms | **2711.00** | 
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 5.27 ms | **4.74** ms | 8.11 ms | 14.67 ms | 100.21 ms | **3278.67** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 5.22 ms | **4.79** ms | 8.59 ms | 14.01 ms | 33.53 ms | **2561.00** | 
| `crystal` (`0.31`) | [toro](https://github.com/soveran/toro) (**0.4**) | 5.55 ms | **4.83** ms | 9.54 ms | 15.78 ms | 36.15 ms | **3063.67** | 
| `crystal` (`0.31`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 5.64 ms | **4.90** ms | 9.62 ms | 15.85 ms | 31.00 ms | **3050.67** | 
| `crystal` (`0.31`) | [raze](https://razecr.com) (**0.3**) | 5.85 ms | **4.99** ms | 10.16 ms | 16.50 ms | 35.28 ms | **3273.33** | 
| `crystal` (`0.31`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 6.12 ms | **5.13** ms | 10.86 ms | 18.13 ms | 38.41 ms | **3613.33** | 
| `crystal` (`0.31`) | [kemal](https://kemalcr.com) (**0.28**) | 6.08 ms | **5.22** ms | 10.24 ms | 16.84 ms | 38.70 ms | **3249.00** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 13.93 ms | **5.27** ms | 10.94 ms | 314.34 ms | 1454.74 ms | **76591.33** | 
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 6.76 ms | **5.49** ms | 10.76 ms | 18.67 ms | 356.75 ms | **8649.33** | 
| `crystal` (`0.31`) | [amber](https://amberframework.org) (**0.3**) | 6.62 ms | **5.67** ms | 11.09 ms | 18.35 ms | 39.12 ms | **3498.67** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.11**) | 8.37 ms | **6.15** ms | 13.78 ms | 61.15 ms | 122.52 ms | **10117.33** | 
| `crystal` (`0.31`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 7.18 ms | **6.17** ms | 11.91 ms | 20.50 ms | 42.49 ms | **3847.67** | 
| `nim` (`1.0`) | [jester](https://github.com/dom96/jester) (**0.4**) | 7.11 ms | **6.49** ms | 10.64 ms | 16.39 ms | 48.87 ms | **2903.00** | 
| `fsharp` (`7.3`) | [suave](https://suave.io) (**2.5**) | 26.37 ms | **6.59** ms | 65.11 ms | 97.40 ms | 437.20 ms | **28031.67** | 
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 7.99 ms | **6.62** ms | 13.29 ms | 25.45 ms | 231.82 ms | **7728.00** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 7.87 ms | **6.98** ms | 11.85 ms | 18.97 ms | 215.33 ms | **5961.67** | 
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 8.27 ms | **6.99** ms | 13.42 ms | 29.58 ms | 224.85 ms | **6353.00** | 
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 8.77 ms | **7.35** ms | 14.57 ms | 32.00 ms | 214.99 ms | **6206.67** | 
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 8.83 ms | **7.52** ms | 14.60 ms | 30.86 ms | 73.78 ms | **5458.67** | 
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 9.32 ms | **7.64** ms | 15.70 ms | 34.11 ms | 292.68 ms | **8103.00** | 
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 9.38 ms | **7.74** ms | 15.78 ms | 34.12 ms | 224.03 ms | **7436.33** | 
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 9.41 ms | **7.82** ms | 16.06 ms | 34.72 ms | 78.74 ms | **6193.67** | 
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 9.09 ms | **7.90** ms | 14.46 ms | 30.87 ms | 125.73 ms | **5595.00** | 
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 9.68 ms | **7.95** ms | 16.84 ms | 35.43 ms | 130.89 ms | **6535.00** | 
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 9.63 ms | **7.97** ms | 16.24 ms | 36.65 ms | 179.43 ms | **7202.33** | 
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 9.24 ms | **8.14** ms | 13.76 ms | 30.13 ms | 232.69 ms | **6271.33** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 218.32 ms | **8.16** ms | 93.22 ms | 4970.77 ms | 7917.74 ms | **859601.33** | 
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.13**) | 11.03 ms | **8.88** ms | 19.28 ms | 43.74 ms | 179.63 ms | **8092.33** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 12.53 ms | **9.02** ms | 14.13 ms | 125.90 ms | 798.65 ms | **32643.33** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 12.57 ms | **9.53** ms | 25.29 ms | 45.51 ms | 150.10 ms | **9776.67** | 
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 12.62 ms | **9.58** ms | 17.25 ms | 54.00 ms | 536.66 ms | **22347.33** | 
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 12.85 ms | **9.69** ms | 17.60 ms | 60.74 ms | 516.07 ms | **22733.00** | 
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 13.98 ms | **9.93** ms | 18.30 ms | 92.44 ms | 645.60 ms | **28848.33** | 
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 10.25 ms | **9.94** ms | 13.65 ms | 17.63 ms | 35.98 ms | **2572.67** | 
| `go` (`1.13`) | [gf](https://goframe.org) (**1.9**) | 11.79 ms | **10.00** ms | 19.24 ms | 42.81 ms | 94.93 ms | **7305.67** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 12.10 ms | **10.41** ms | 22.83 ms | 42.60 ms | 216.15 ms | **10087.67** | 
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 15.05 ms | **10.89** ms | 20.00 ms | 89.42 ms | 659.51 ms | **29714.33** | 
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 16.69 ms | **11.11** ms | 20.50 ms | 175.89 ms | 773.76 ms | **39125.00** | 
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | 64.82 ms | **11.55** ms | 89.77 ms | 1530.66 ms | 2629.59 ms | **244980.33** | 
| `swift` (`5.1`) | [perfect](https://perfect.org) (**3.1**) | 11.76 ms | **11.91** ms | 14.19 ms | 16.21 ms | 146.55 ms | **2790.33** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 16.75 ms | **11.91** ms | 33.65 ms | 64.06 ms | 240.26 ms | **12902.00** | 
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 18.82 ms | **12.06** ms | 44.03 ms | 83.19 ms | 185.16 ms | **18661.67** | 
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 19.00 ms | **12.47** ms | 22.66 ms | 212.40 ms | 905.37 ms | **46434.33** | 
| `php` (`7.3`) | [one](https://github.com/lizhichao/one) (**1.8**) | 13.93 ms | **13.38** ms | 22.37 ms | 32.46 ms | 149.09 ms | **6629.67** | 
| `php` (`7.3`) | [basicphp](https://github.com/ray-ang/basicphp) (**0.9**) | 38.45 ms | **13.49** ms | 80.03 ms | 385.18 ms | 693.46 ms | **77194.67** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 16.94 ms | **13.56** ms | 22.69 ms | 62.08 ms | 645.35 ms | **22260.00** | 
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 17.71 ms | **13.56** ms | 23.43 ms | 125.31 ms | 706.56 ms | **31104.00** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 43.53 ms | **13.84** ms | 78.56 ms | 451.23 ms | 938.75 ms | **90456.67** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 44.09 ms | **14.03** ms | 80.64 ms | 459.27 ms | 912.12 ms | **91207.67** | 
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 18.52 ms | **14.10** ms | 24.39 ms | 99.43 ms | 721.23 ms | **32605.33** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 41.63 ms | **14.25** ms | 81.36 ms | 426.72 ms | 714.16 ms | **84631.00** | 
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 22.56 ms | **14.31** ms | 25.91 ms | 294.54 ms | 983.18 ms | **55865.67** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 47.19 ms | **14.85** ms | 93.45 ms | 501.45 ms | 827.35 ms | **99357.33** | 
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 25.27 ms | **15.16** ms | 26.80 ms | 381.64 ms | 1128.41 ms | **68576.67** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 19.36 ms | **15.30** ms | 36.82 ms | 57.81 ms | 223.04 ms | **11449.33** | 
| `ruby` (`2.6`) | [plezi](https://github.com/boazsegev/plezi) (**0.16**) | 20.97 ms | **15.30** ms | 38.66 ms | 93.69 ms | 187.05 ms | **17240.00** | 
| `python` (`3.7`) | [asgineer](https://asgineer.readthedocs.io) (**0.7**) | 16.00 ms | **15.35** ms | 22.12 ms | 31.20 ms | 163.79 ms | **5510.33** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 23.56 ms | **15.68** ms | 30.36 ms | 230.74 ms | 1885.54 ms | **74604.33** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**6.0**) | 49.54 ms | **16.09** ms | 92.03 ms | 506.28 ms | 992.02 ms | **101070.00** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 17.43 ms | **16.32** ms | 27.32 ms | 37.03 ms | 86.29 ms | **7135.33** | 
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 25.30 ms | **16.77** ms | 29.02 ms | 317.88 ms | 1076.52 ms | **60357.33** | 
| `swift` (`5.1`) | [vapor](https://vapor.codes) (**3.3**) | 21.81 ms | **17.38** ms | 28.79 ms | 114.59 ms | 910.28 ms | **39132.67** | 
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | 19.99 ms | **17.83** ms | 18.40 ms | 35.02 ms | 953.55 ms | **32463.67** | 
| `swift` (`5.1`) | [kitura](https://kitura.io) (**2.7**) | 20.12 ms | **19.31** ms | 20.91 ms | 24.82 ms | 593.74 ms | **20334.00** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 20.27 ms | **19.32** ms | 28.85 ms | 44.01 ms | 212.17 ms | **8454.33** | 
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 25.22 ms | **20.21** ms | 26.98 ms | 157.87 ms | 770.66 ms | **36634.00** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.0**) | 75.92 ms | **21.16** ms | 138.27 ms | 890.02 ms | 1272.69 ms | **175220.33** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 22.30 ms | **22.13** ms | 28.65 ms | 38.01 ms | 129.77 ms | **6154.67** | 
| `swift` (`5.1`) | [kitura-nio](https://kitura.io) (**2.7**) | 42.84 ms | **24.49** ms | 37.28 ms | 726.86 ms | 2489.07 ms | **133969.33** | 
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 44.05 ms | **25.34** ms | 41.64 ms | 618.96 ms | 1518.97 ms | **103446.33** | 
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 43.12 ms | **25.35** ms | 44.37 ms | 635.28 ms | 1524.39 ms | **103945.00** | 
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | 28.72 ms | **27.59** ms | 41.88 ms | 59.99 ms | 102.30 ms | **10322.00** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 33.13 ms | **31.79** ms | 49.34 ms | 67.37 ms | 194.45 ms | **12559.33** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 40.91 ms | **31.83** ms | 82.93 ms | 101.14 ms | 333.99 ms | **24856.00** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 39.72 ms | **34.47** ms | 64.74 ms | 106.33 ms | 441.11 ms | **21889.67** | 
| `crystal` (`0.31`) | [lucky](https://luckyframework.org) (**0.16**) | 40.13 ms | **40.27** ms | 48.32 ms | 55.47 ms | 350.73 ms | **11940.67** | 
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 42.64 ms | **41.18** ms | 47.81 ms | 54.36 ms | 479.19 ms | **14520.67** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 48.38 ms | **42.59** ms | 77.83 ms | 121.08 ms | 267.11 ms | **21460.67** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 44.72 ms | **44.74** ms | 62.57 ms | 86.69 ms | 202.08 ms | **15545.33** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 47.04 ms | **45.97** ms | 76.14 ms | 91.78 ms | 190.81 ms | **17502.67** | 
| `php` (`7.3`) | [spiral](https://github.com/spiral/framework) (**2.0**) | 56.30 ms | **56.07** ms | 62.15 ms | 64.36 ms | 257.90 ms | **6202.67** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | 80.63 ms | **75.28** ms | 125.50 ms | 165.04 ms | 367.05 ms | **29136.00** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 103.66 ms | **86.91** ms | 182.67 ms | 227.22 ms | 855.02 ms | **50635.33** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 94.96 ms | **93.37** ms | 134.37 ms | 165.22 ms | 343.92 ms | **28157.67** | 
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | 246.13 ms | **95.32** ms | 592.26 ms | 2599.00 ms | 4817.34 ms | **450193.67** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 98.42 ms | **99.69** ms | 115.86 ms | 138.35 ms | 813.36 ms | **27418.67** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 128.42 ms | **116.36** ms | 182.63 ms | 219.72 ms | 781.35 ms | **45414.67** | 
| `crystal` (`0.31`) | [onyx](https://onyxframework.org) (**0.5**) | 205.71 ms | **204.84** ms | 234.68 ms | 258.24 ms | 337.84 ms | **23207.67** | 
| `crystal` (`0.31`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 355.16 ms | **209.10** ms | 224.68 ms | 4576.91 ms | 7246.65 ms | **750066.00** | 

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
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 229247.33 | **132.65** MB |
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 214500.67 | **188.84** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 207010.33 | **247.88** MB |
| `nim` (`1.0`) | [httpbeast](https://github.com/dom96/httpbeast) (**0.2**) | 197461.00 | **281.22** MB |
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 190185.33 | **184.68** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 182498.00 | **328.63** MB |
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 176190.67 | **283.90** MB |
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 175098.00 | **352.38** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 173954.33 | **168.90** MB |
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 172529.00 | **277.55** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 171746.00 | **446.35** MB |
| `crystal` (`0.31`) | [toro](https://github.com/soveran/toro) (**0.4**) | 169396.33 | **159.24** MB |
| `crystal` (`0.31`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 166884.00 | **156.99** MB |
| `crystal` (`0.31`) | [raze](https://razecr.com) (**0.3**) | 162221.33 | **152.52** MB |
| `crystal` (`0.31`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 157448.33 | **167.64** MB |
| `crystal` (`0.31`) | [kemal](https://kemalcr.com) (**0.28**) | 156117.67 | **255.27** MB |
| `nim` (`1.0`) | [jester](https://github.com/dom96/jester) (**0.4**) | 147106.00 | **295.81** MB |
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 144985.67 | **296.83** MB |
| `crystal` (`0.31`) | [amber](https://amberframework.org) (**0.3**) | 144502.00 | **264.43** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 139897.67 | **241.42** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.11**) | 135625.67 | **78.32** MB |
| `crystal` (`0.31`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 134327.00 | **219.56** MB |
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 122768.33 | **313.92** MB |
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 118548.67 | **158.92** MB |
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 115550.33 | **145.60** MB |
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 113217.00 | **167.24** MB |
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 113055.33 | **151.32** MB |
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 111722.33 | **149.58** MB |
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 108762.33 | **144.67** MB |
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 108388.67 | **190.35** MB |
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 108017.33 | **143.34** MB |
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 107410.00 | **188.53** MB |
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 107251.00 | **188.36** MB |
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 105507.33 | **141.89** MB |
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 104849.67 | **139.98** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 95886.67 | **156.19** MB |
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.13**) | 94024.33 | **196.35** MB |
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 89510.00 | **134.20** MB |
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 88272.33 | **132.29** MB |
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 88137.00 | **175.42** MB |
| `go` (`1.13`) | [gf](https://goframe.org) (**1.9**) | 87096.00 | **147.98** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 86225.67 | **134.23** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 86127.67 | **201.98** MB |
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 85554.67 | **128.22** MB |
| `swift` (`5.1`) | [perfect](https://perfect.org) (**3.1**) | 82106.00 | **77.18** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 79637.67 | **172.00** MB |
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 78240.67 | **117.22** MB |
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 76924.33 | **115.27** MB |
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | 72194.67 | **128.84** MB |
| `php` (`7.3`) | [one](https://github.com/lizhichao/one) (**1.8**) | 70150.00 | **160.89** MB |
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 69664.00 | **104.38** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 67323.33 | **50.60** MB |
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 65606.67 | **172.44** MB |
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 65599.67 | **139.91** MB |
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 65195.33 | **137.09** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 64320.67 | **158.53** MB |
| `python` (`3.7`) | [asgineer](https://asgineer.readthedocs.io) (**0.7**) | 60948.00 | **108.86** MB |
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 60721.67 | **128.53** MB |
| `php` (`7.3`) | [basicphp](https://github.com/ray-ang/basicphp) (**0.9**) | 59211.67 | **293.89** MB |
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 59179.67 | **240.41** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 58387.67 | **102.43** MB |
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | 56415.33 | **101.18** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 56412.67 | **113.52** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 55903.00 | **277.71** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 55653.67 | **276.44** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 55110.33 | **273.99** MB |
| `ruby` (`2.6`) | [plezi](https://github.com/boazsegev/plezi) (**0.16**) | 53834.00 | **114.59** MB |
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 53443.00 | **130.87** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 53042.00 | **263.60** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 52306.00 | **129.67** MB |
| `swift` (`5.1`) | [vapor](https://vapor.codes) (**3.3**) | 51374.67 | **88.03** MB |
| `swift` (`5.1`) | [kitura](https://kitura.io) (**2.7**) | 50638.33 | **94.02** MB |
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 50283.33 | **29.08** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 48657.33 | **104.87** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**6.0**) | 48366.67 | **251.66** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.24**) | 47978.00 | **45.82** MB |
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 45676.33 | **80.14** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 44207.00 | **116.02** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 44196.67 | **52.25** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.0**) | 38213.00 | **199.69** MB |
| `swift` (`5.1`) | [kitura-nio](https://kitura.io) (**2.7**) | 36838.67 | **69.63** MB |
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 35640.00 | **92.31** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 35596.33 | **20.54** MB |
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | 34242.33 | **78.53** MB |
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 34210.67 | **58.47** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 30228.67 | **65.21** MB |
| `fsharp` (`7.3`) | [suave](https://suave.io) (**2.5**) | 28873.33 | **58.38** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 25835.00 | **47.93** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 25391.67 | **62.64** MB |
| `crystal` (`0.31`) | [lucky](https://luckyframework.org) (**0.16**) | 24660.00 | **30.29** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 23565.67 | **13.58** MB |
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 23112.67 | **21.68** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 22229.67 | **39.57** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 21291.67 | **160.98** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 21157.67 | **47.91** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 20624.33 | **39.79** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 17519.33 | **45.41** MB |
| `php` (`7.3`) | [spiral](https://github.com/spiral/framework) (**2.0**) | 17273.00 | **29.98** MB |
| `ruby` (`2.6`) | [grape](https://ruby-grape.org) (**1.2**) | 14307.33 | **8.27** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | 12237.00 | **24.37** MB |
| `swift` (`5.1`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 12036.00 | **15.40** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 10339.67 | **22.51** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 9913.33 | **29.26** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 9587.67 | **27.82** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 7529.00 | **18.55** MB |
| `crystal` (`0.31`) | [onyx](https://onyxframework.org) (**0.5**) | 4807.00 | **12.40** MB |
| `crystal` (`0.31`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 4590.33 | **5.73** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 4230.67 | **26.59** MB |
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | 602.33 | **1.37** MB |
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

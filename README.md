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
Last update: 2019-09-10
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


:five: cuba (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `rust` (` 1.37`) | [nickel](https://nickel-org.github.io) (** 0.11**) | 0.07 ms | **0.06** ms | 0.11 ms | 0.13 ms | 3.29 ms | **35.00** | 
| `rust` (` 1.37`) | [iron](https://ironframework.io) (** 0.6**) | 0.43 ms | **0.20** ms | 1.07 ms | 2.87 ms | 17.52 ms | **623.33** | 
| `swift` (` 5.0`) | [swifter](https://github.com/httpswift/swifter) (** 1.4**) | 4.29 ms | **0.87** ms | 14.55 ms | 15.24 ms | 2196.51 ms | **30903.67** | 
| `ruby` (` 2.6`) | [syro](https://github.com/soveran/syro) (** 3.1**) | 3.24 ms | **1.34** ms | 8.60 ms | 17.36 ms | 46.37 ms | **4010.00** | 
| `ruby` (` 2.6`) | [cuba](https://cuba.is) (** 3.9**) | 3.62 ms | **1.60** ms | 9.49 ms | 19.37 ms | 60.51 ms | **4445.67** | 
| `ruby` (` 2.6`) | [roda](https://roda.jeremyevans.net) (** 3.23**) | 3.26 ms | **1.84** ms | 8.25 ms | 16.15 ms | 42.12 ms | **3756.67** | 
| `c` (` 11`) | [agoo-c](https://github.com/ohler55/agoo-c) (** 0.5**) | 3.20 ms | **1.87** ms | 7.38 ms | 16.45 ms | 129.61 ms | **4127.67** | 
| `node` (` 12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (** 0.0**) | 3.14 ms | **2.18** ms | 7.03 ms | 15.61 ms | 46.08 ms | **3337.33** | 
| `ruby` (` 2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (** 0.0**) | 3.76 ms | **2.22** ms | 9.42 ms | 17.66 ms | 41.21 ms | **4206.67** | 
| `python` (` 3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (** 0.1**) | 3.76 ms | **2.29** ms | 8.93 ms | 18.93 ms | 46.98 ms | **4129.00** | 
| `go` (` 1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (** 8.2**) | 3.19 ms | **2.36** ms | 5.86 ms | 13.17 ms | 104.83 ms | **3159.33** | 
| `cpp` (` 14/17`) | [drogon](https://github.com/an-tao/drogon) (** 1.0**) | 3.12 ms | **2.43** ms | 6.13 ms | 13.71 ms | 37.92 ms | **2830.33** | 
| `ruby` (` 2.6`) | [rails](https://rubyonrails.org) (** 6.0**) | 33.93 ms | **2.46** ms | 112.36 ms | 306.05 ms | 984.32 ms | **65411.67** | 
| `go` (` 1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (** 4.1**) | 3.44 ms | **2.60** ms | 6.13 ms | 13.13 ms | 109.25 ms | **3643.00** | 
| `java` (` 8`) | [rapidoid](https://rapidoid.org) (** 5.5**) | 4.15 ms | **2.73** ms | 9.69 ms | 20.96 ms | 107.43 ms | **4600.67** | 
| `go` (` 1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (** 0.1**) | 3.74 ms | **2.90** ms | 6.79 ms | 14.12 ms | 101.55 ms | **2970.00** | 
| `crystal` (` 0.29`) | [spider-gazelle](https://spider-gazelle.net) (** 1.6**) | 3.94 ms | **2.91** ms | 8.22 ms | 16.14 ms | 35.79 ms | **3365.00** | 
| `cpp` (` 11`) | [evhtp](https://criticalstack/libevhtp) (** 1.2**) | 3.83 ms | **2.96** ms | 7.48 ms | 18.61 ms | 40.33 ms | **3609.67** | 
| `rust` (` 1.37`) | [gotham](https://gotham.rs) (** 0.4**) | 3.79 ms | **3.01** ms | 6.29 ms | 14.96 ms | 218.47 ms | **7063.00** | 
| `crystal` (` 0.29`) | [router.cr](https://github.com/tbrand/router.cr) (** 0.2**) | 3.60 ms | **3.02** ms | 6.87 ms | 13.71 ms | 32.34 ms | **2850.33** | 
| `c` (` 99`) | [kore](https://kore.io) (** 3.1**) | 8.79 ms | **3.03** ms | 7.86 ms | 143.61 ms | 1396.90 ms | **55079.33** | 
| `crystal` (` 0.29`) | [toro](https://github.com/soveran/toro) (** 0.4**) | 3.54 ms | **3.04** ms | 6.55 ms | 12.99 ms | 33.27 ms | **2718.67** | 
| `crystal` (` 0.29`) | [raze](https://razecr.com) (** 0.3**) | 3.80 ms | **3.07** ms | 7.46 ms | 15.13 ms | 39.48 ms | **3110.67** | 
| `crystal` (` 0.29`) | [kemal](https://kemalcr.com) (** 0.28**) | 3.87 ms | **3.45** ms | 6.89 ms | 14.02 ms | 31.21 ms | **2812.00** | 
| `crystal` (` 0.29`) | [amber](https://amberframework.org) (** 0.3**) | 4.22 ms | **3.78** ms | 7.40 ms | 15.52 ms | 31.23 ms | **2959.67** | 
| `ruby` (` 2.6`) | [agoo](https://github.com/ohler55/agoo) (** 2.1**) | 5.62 ms | **3.87** ms | 8.69 ms | 69.12 ms | 112.45 ms | **10169.00** | 
| `go` (` 1.13`) | [rte](https://github.com/jwilner/rte) (** 0.0**) | 4.93 ms | **3.92** ms | 9.01 ms | 20.13 ms | 161.68 ms | **4441.67** | 
| `java` (` 8`) | [act](https://actframework.org) (** 1.8**) | 5.45 ms | **4.01** ms | 10.48 ms | 19.34 ms | 192.67 ms | **5328.67** | 
| `crystal` (` 0.29`) | [orion](https://github.com/obsidian/orion) (** 1.7**) | 4.50 ms | **4.27** ms | 7.56 ms | 15.48 ms | 35.53 ms | **2887.67** | 
| `go` (` 1.13`) | [chi](https://github.com/go-chi/chi) (** 4.0**) | 5.57 ms | **4.28** ms | 10.10 ms | 22.65 ms | 285.85 ms | **8067.67** | 
| `ruby` (` 2.6`) | [flame](https://github.com/AlexWayfer/flame) (** 4.18**) | 6.23 ms | **4.36** ms | 15.28 ms | 29.83 ms | 73.13 ms | **6831.67** | 
| `go` (` 1.13`) | [goroute](https://goroute.github.io) (** 0.0**) | 5.59 ms | **4.37** ms | 10.88 ms | 22.92 ms | 182.39 ms | **4923.33** | 
| `go` (` 1.13`) | [echo](https://echo.labstack.com) (** 4.1**) | 5.55 ms | **4.38** ms | 10.47 ms | 21.89 ms | 202.52 ms | **5478.33** | 
| `go` (` 1.13`) | [gin](https://gin-gonic.com) (** 1.4**) | 5.61 ms | **4.40** ms | 10.64 ms | 22.57 ms | 210.00 ms | **5738.00** | 
| `go` (` 1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (** 1.7**) | 6.06 ms | **4.43** ms | 11.87 ms | 25.73 ms | 351.36 ms | **8525.33** | 
| `csharp` (` 7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (** 2.2**) | 6.69 ms | **4.54** ms | 7.57 ms | 83.44 ms | 212.62 ms | **14044.33** | 
| `go` (` 1.13`) | [gramework](https://github.com/gramework/gramework) (** 1.6**) | 5.44 ms | **4.63** ms | 9.88 ms | 18.94 ms | 111.62 ms | **4479.67** | 
| `nim` (` 0.20`) | [jester](https://github.com/dom96/jester) (** 0.4**) | 4.83 ms | **4.74** ms | 7.26 ms | 13.48 ms | 38.03 ms | **2534.67** | 
| `go` (` 1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (** 4.1**) | 5.98 ms | **4.75** ms | 11.17 ms | 25.63 ms | 150.72 ms | **4952.67** | 
| `go` (` 1.13`) | [beego](https://beego.me) (** 1.12**) | 6.57 ms | **4.81** ms | 12.55 ms | 28.94 ms | 273.10 ms | **7887.67** | 
| `go` (` 1.13`) | [kami](https://github.com/guregu/kami) (** 2.2**) | 6.52 ms | **5.05** ms | 11.09 ms | 26.92 ms | 239.09 ms | **8599.67** | 
| `go` (` 1.13`) | [violetear](https://violetear.org) (** 7.0**) | 6.32 ms | **5.10** ms | 11.12 ms | 25.75 ms | 195.73 ms | **5133.00** | 
| `go` (` 1.13`) | [gf](https://goframe.org) (** 1.8**) | 6.88 ms | **5.14** ms | 13.05 ms | 29.36 ms | 219.30 ms | **6488.33** | 
| `scala` (` 2.12`) | [akkahttp](https://akka.io) (** 10.1**) | 69.97 ms | **5.25** ms | 14.24 ms | 1870.64 ms | 4081.51 ms | **332718.33** | 
| `java` (` 8`) | [javalin](https://javalin.io) (** 3.4**) | 14.51 ms | **5.51** ms | 34.84 ms | 131.93 ms | 585.69 ms | **30679.67** | 
| `go` (` 1.13`) | [air](https://github.com/aofei/air) (** 0.12**) | 8.54 ms | **5.73** ms | 18.68 ms | 39.43 ms | 226.96 ms | **8893.00** | 
| `python` (` 3.7`) | [falcon](https://falconframework.org) (** 2.0**) | 8.50 ms | **5.77** ms | 16.90 ms | 35.66 ms | 217.72 ms | **8668.33** | 
| `node` (` 12.1`) | [polkadot](https://github.com/lukeed/polkadot) (** 1.0**) | 7.44 ms | **6.07** ms | 11.47 ms | 22.56 ms | 235.63 ms | **6257.33** | 
| `ruby` (` 2.6`) | [hanami](https://hanamirb.org) (** 1.3**) | 6.63 ms | **6.21** ms | 14.53 ms | 25.64 ms | 59.28 ms | **6063.33** | 
| `node` (` 12.1`) | [restana](https://github.com/jkyberneees/ana) (** 3.3**) | 8.48 ms | **6.68** ms | 12.76 ms | 27.76 ms | 366.98 ms | **12613.67** | 
| `kotlin` (` 1.3`) | [ktor](https://ktor.io) (** 1.2**) | 9.22 ms | **6.81** ms | 20.41 ms | 39.54 ms | 260.06 ms | **10425.33** | 
| `node` (` 12.1`) | [0http](https://github.com/jkyberneees/0http) (** 1.2**) | 8.89 ms | **6.92** ms | 13.48 ms | 29.33 ms | 427.67 ms | **14662.33** | 
| `ruby` (` 2.6`) | [sinatra](https://sinatrarb.com) (** 2.0**) | 8.22 ms | **7.15** ms | 18.33 ms | 34.12 ms | 69.73 ms | **7786.67** | 
| `rust` (` 1.37`) | [actix-web](https://actix.rs) (** 1.0**) | 7.29 ms | **7.15** ms | 9.31 ms | 12.13 ms | 41.11 ms | **1704.67** | 
| `node` (` 12.1`) | [rayo](https://rayo.js.org) (** 1.3**) | 9.00 ms | **7.45** ms | 14.13 ms | 28.23 ms | 328.27 ms | **10480.67** | 
| `node` (` 12.1`) | [polka](https://github.com/lukeed/polka) (** 0.5**) | 9.87 ms | **7.75** ms | 14.14 ms | 37.38 ms | 479.42 ms | **18525.33** | 
| `node` (` 12.1`) | [muneem](https://github.com/node-muneem/muneem) (** 2.4**) | 10.15 ms | **7.98** ms | 13.86 ms | 35.51 ms | 491.82 ms | **18204.00** | 
| `php` (` 7.3`) | [hyperf](https://www.hyperf.io) (** 1.0**) | 12.11 ms | **8.01** ms | 27.48 ms | 58.19 ms | 150.92 ms | **12327.33** | 
| `java` (` 8`) | [spring-boot](https://spring.io/projects/spring-boot) (** 2.1**) | 11.46 ms | **8.27** ms | 16.73 ms | 41.70 ms | 735.87 ms | **21680.33** | 
| `swift` (` 5.0`) | [perfect](https://perfect.org) (** 3.1**) | 8.73 ms | **8.75** ms | 11.17 ms | 14.65 ms | 128.86 ms | **2357.33** | 
| `php` (` 7.3`) | [slim](https://slimframework.com) (** 4.2**) | 29.83 ms | **8.79** ms | 65.63 ms | 295.82 ms | 781.03 ms | **61241.33** | 
| `python` (` 3.7`) | [bottle](https://bottlepy.org) (** 0.12**) | 12.65 ms | **8.80** ms | 27.62 ms | 55.78 ms | 216.54 ms | **11960.67** | 
| `node` (` 12.1`) | [foxify](https://foxify.js.org) (** 0.1**) | 10.47 ms | **8.88** ms | 15.63 ms | 31.39 ms | 335.88 ms | **11258.00** | 
| `scala` (` 2.12`) | [http4s](https://http4s.org) (** 0.18**) | 29.18 ms | **8.98** ms | 19.74 ms | 738.70 ms | 2545.85 ms | **143807.33** | 
| `php` (` 7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (** 3.2**) | 28.59 ms | **9.16** ms | 55.96 ms | 299.45 ms | 614.82 ms | **60621.33** | 
| `php` (` 7.3`) | [symfony](https://symfony.com) (** 4.3**) | 31.62 ms | **9.33** ms | 65.03 ms | 335.26 ms | 809.20 ms | **68486.00** | 
| `node` (` 12.1`) | [fastify](https://fastify.io) (** 2.8**) | 12.41 ms | **9.61** ms | 18.23 ms | 42.75 ms | 515.92 ms | **18397.00** | 
| `node` (` 12.1`) | [koa](https://koajs.com) (** 2.8**) | 11.76 ms | **9.70** ms | 18.09 ms | 34.73 ms | 377.54 ms | **12312.33** | 
| `php` (` 7.3`) | [zend-framework](https://framework.zend.com) (** 3.1**) | 35.13 ms | **9.88** ms | 68.32 ms | 386.16 ms | 765.97 ms | **77090.67** | 
| `swift` (` 5.0`) | [vapor](https://vapor.codes) (** 3.3**) | 14.70 ms | **10.00** ms | 18.98 ms | 121.22 ms | 1006.23 ms | **37483.67** | 
| `node` (` 12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (** 0.0**) | 12.53 ms | **10.18** ms | 18.97 ms | 37.27 ms | 390.91 ms | **13396.67** | 
| `python` (` 3.7`) | [hug](https://hug.rest) (** 2.6**) | 13.83 ms | **10.34** ms | 27.86 ms | 52.44 ms | 181.25 ms | **11166.67** | 
| `php` (` 7.3`) | [lumen](https://lumen.laravel.com) (** 5.8**) | 33.85 ms | **10.87** ms | 66.43 ms | 356.59 ms | 637.65 ms | **71193.67** | 
| `node` (` 12.1`) | [express](https://expressjs.com) (** 4.17**) | 15.26 ms | **11.75** ms | 21.59 ms | 92.18 ms | 534.47 ms | **22448.00** | 
| `php` (` 7.3`) | [laravel](https://laravel.com) (** 5.8**) | 55.72 ms | **11.91** ms | 87.19 ms | 723.29 ms | 1169.55 ms | **142419.00** | 
| `python` (` 3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (** 0.1**) | 13.97 ms | **12.53** ms | 22.53 ms | 39.28 ms | 131.14 ms | **7427.00** | 
| `clojure` (` 1.10`) | [coast](https://coastonclojure.com) (** 1.0**) | 13.01 ms | **12.69** ms | 13.63 ms | 15.97 ms | 359.50 ms | **6420.33** | 
| `node` (` 12.1`) | [restify](https://restify.com) (** 8.4**) | 15.69 ms | **13.70** ms | 21.24 ms | 42.28 ms | 307.14 ms | **10822.33** | 
| `python` (` 3.7`) | [starlette](https://starlette.io) (** 0.12**) | 14.71 ms | **13.93** ms | 23.16 ms | 36.14 ms | 81.55 ms | **6961.00** | 
| `swift` (` 5.0`) | [kitura-nio](https://kitura.io) (** 2.7**) | 23.97 ms | **16.35** ms | 27.94 ms | 240.56 ms | 1533.46 ms | **68890.00** | 
| `swift` (` 5.0`) | [kitura](https://kitura.io) (** 2.7**) | 17.84 ms | **16.35** ms | 17.10 ms | 22.30 ms | 726.19 ms | **26031.33** | 
| `node` (` 12.1`) | [moleculer](https://moleculer.services) (** 0.13**) | 22.16 ms | **17.43** ms | 32.96 ms | 74.57 ms | 586.21 ms | **22612.33** | 
| `python` (` 3.7`) | [fastapi](https://fastapi.tiangolo.com) (** 0.38**) | 20.37 ms | **18.40** ms | 33.66 ms | 44.22 ms | 182.45 ms | **9576.67** | 
| `python` (` 3.7`) | [molten](https://moltenframework.com) (** 0.27**) | 28.18 ms | **19.08** ms | 55.52 ms | 83.97 ms | 301.47 ms | **20654.33** | 
| `node` (` 12.1`) | [hapi](https://hapijs.com) (** 18.1**) | 25.94 ms | **19.39** ms | 33.84 ms | 177.52 ms | 946.59 ms | **45643.67** | 
| `php` (` 7.3`) | [swoft](https://swoft.org) (** 2.0**) | 20.58 ms | **19.84** ms | 26.62 ms | 34.65 ms | 158.47 ms | **4743.33** | 
| `crystal` (` 0.29`) | [lucky](https://luckyframework.org) (** 0.16**) | 20.82 ms | **20.62** ms | 23.10 ms | 24.68 ms | 391.62 ms | **10676.00** | 
| `python` (` 3.7`) | [flask](https://flask.pocoo.org) (** 1.1**) | 33.11 ms | **22.65** ms | 65.34 ms | 112.52 ms | 392.66 ms | **25065.67** | 
| `python` (` 3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (** 3.6**) | 29.96 ms | **27.14** ms | 45.12 ms | 69.81 ms | 141.77 ms | **11671.67** | 
| `node` (` 12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (** 2.0**) | 29.10 ms | **27.72** ms | 31.07 ms | 40.76 ms | 580.57 ms | **22288.00** | 
| `php` (` 7.3`) | [imi](https://imiphp.com) (** 1.0**) | 31.22 ms | **29.43** ms | 50.47 ms | 70.67 ms | 167.60 ms | **13835.33** | 
| `python` (` 3.7`) | [sanic](https://github.com/huge-success/sanic) (** 19.6**) | 32.92 ms | **30.03** ms | 55.82 ms | 89.26 ms | 191.29 ms | **17624.67** | 
| `python` (` 3.7`) | [bocadillo](https://bocadilloproject.github.io) (** 0.18**) | 37.89 ms | **34.86** ms | 61.55 ms | 103.85 ms | 182.93 ms | **19479.00** | 
| `perl` (` 5.3`) | [dancer2](https://perldancer.org) (** 2.0**) | 324.28 ms | **49.44** ms | 647.14 ms | 4594.41 ms | 6693.18 ms | **890142.33** | 
| `python` (` 3.7`) | [quart](https://pgjones.gitlab.io/quart) (** 0.10**) | 59.52 ms | **53.46** ms | 100.42 ms | 157.56 ms | 249.96 ms | **30797.00** | 
| `python` (` 3.7`) | [django](https://djangoproject.com) (** 2.2**) | 88.14 ms | **56.34** ms | 197.63 ms | 272.96 ms | 585.30 ms | **68375.00** | 
| `python` (` 3.7`) | [responder](https://python-responder.org) (** 1.3**) | 84.44 ms | **79.64** ms | 128.30 ms | 188.94 ms | 387.98 ms | **34750.00** | 
| `python` (` 3.7`) | [tornado](https://tornadoweb.org) (** 5.1**) | 86.44 ms | **85.81** ms | 105.38 ms | 160.00 ms | 390.23 ms | **21367.67** | 
| `python` (` 3.7`) | [masonite](https://masoniteproject.com) (** 2.2**) | 118.55 ms | **86.92** ms | 218.30 ms | 372.97 ms | 842.43 ms | **74601.67** | 
| `crystal` (` 0.29`) | [athena](https://github.com/blacksmoke16/athena) (** 0.7**) | 142.64 ms | **117.91** ms | 122.37 ms | 1145.04 ms | 2281.63 ms | **189099.33** | 
| `crystal` (` 0.29`) | [onyx](https://onyxframework.org) (** 0.5**) | 190.70 ms | **189.93** ms | 225.35 ms | 256.62 ms | 334.11 ms | **27018.00** | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (sifrr) (node)


:three: (drogon) (cpp)


:four: (japronto) (python)


:five: (atreugo) (go)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| `c` (` 11`) | [agoo-c](https://github.com/ohler55/agoo-c) (** 0.5**) | 350939.00 | **202.87** MB |
| `node` (` 12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (** 0.0**) | 337920.67 | **297.60** MB |
| `cpp` (` 14/17`) | [drogon](https://github.com/an-tao/drogon) (** 1.0**) | 316618.33 | **307.46** MB |
| `python` (` 3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (** 0.1**) | 302375.33 | **362.09** MB |
| `go` (` 1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (** 8.2**) | 298345.67 | **600.20** MB |
| `go` (` 1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (** 4.1**) | 276973.67 | **444.02** MB |
| `rust` (` 1.37`) | [gotham](https://gotham.rs) (** 0.4**) | 272657.67 | **558.31** MB |
| `java` (` 8`) | [rapidoid](https://rapidoid.org) (** 5.5**) | 272232.33 | **490.30** MB |
| `crystal` (` 0.29`) | [toro](https://github.com/soveran/toro) (** 0.4**) | 272167.33 | **255.80** MB |
| `cpp` (` 11`) | [evhtp](https://criticalstack/libevhtp) (** 1.2**) | 271540.67 | **263.45** MB |
| `crystal` (` 0.29`) | [router.cr](https://github.com/tbrand/router.cr) (** 0.2**) | 270192.67 | **254.08** MB |
| `crystal` (` 0.29`) | [raze](https://razecr.com) (** 0.3**) | 262555.00 | **246.87** MB |
| `crystal` (` 0.29`) | [spider-gazelle](https://spider-gazelle.net) (** 1.6**) | 262060.00 | **278.59** MB |
| `c` (` 99`) | [kore](https://kore.io) (** 3.1**) | 258263.00 | **671.32** MB |
| `go` (` 1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (** 0.1**) | 250515.00 | **401.06** MB |
| `crystal` (` 0.29`) | [kemal](https://kemalcr.com) (** 0.28**) | 247532.00 | **404.74** MB |
| `crystal` (` 0.29`) | [amber](https://amberframework.org) (** 0.3**) | 230133.00 | **421.03** MB |
| `ruby` (` 2.6`) | [agoo](https://github.com/ohler55/agoo) (** 2.1**) | 229273.67 | **132.49** MB |
| `nim` (` 0.20`) | [jester](https://github.com/dom96/jester) (** 0.4**) | 227068.33 | **455.69** MB |
| `java` (` 8`) | [act](https://actframework.org) (** 1.8**) | 225471.33 | **389.06** MB |
| `crystal` (` 0.29`) | [orion](https://github.com/obsidian/orion) (** 1.7**) | 212442.00 | **347.48** MB |
| `go` (` 1.13`) | [rte](https://github.com/jwilner/rte) (** 0.0**) | 203956.00 | **272.62** MB |
| `rust` (` 1.37`) | [iron](https://ironframework.io) (** 0.6**) | 194666.33 | **245.50** MB |
| `go` (` 1.13`) | [chi](https://github.com/go-chi/chi) (** 4.0**) | 188967.33 | **252.09** MB |
| `csharp` (` 7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (** 2.2**) | 188792.67 | **307.21** MB |
| `go` (` 1.13`) | [goroute](https://goroute.github.io) (** 0.0**) | 185832.00 | **326.17** MB |
| `go` (` 1.13`) | [gin](https://gin-gonic.com) (** 1.4**) | 184756.33 | **323.48** MB |
| `go` (` 1.13`) | [echo](https://echo.labstack.com) (** 4.1**) | 184296.00 | **323.48** MB |
| `go` (` 1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (** 1.7**) | 179779.00 | **239.44** MB |
| `go` (` 1.13`) | [gramework](https://github.com/gramework/gramework) (** 1.6**) | 177744.33 | **453.97** MB |
| `rust` (` 1.37`) | [actix-web](https://actix.rs) (** 1.0**) | 175450.33 | **266.56** MB |
| `go` (` 1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (** 4.1**) | 172976.67 | **230.48** MB |
| `rust` (` 1.37`) | [nickel](https://nickel-org.github.io) (** 0.11**) | 168755.33 | **336.26** MB |
| `go` (` 1.13`) | [beego](https://beego.me) (** 1.12**) | 166645.67 | **222.24** MB |
| `go` (` 1.13`) | [kami](https://github.com/guregu/kami) (** 2.2**) | 161627.33 | **214.91** MB |
| `go` (` 1.13`) | [violetear](https://violetear.org) (** 7.0**) | 161410.00 | **213.98** MB |
| `go` (` 1.13`) | [gf](https://goframe.org) (** 1.8**) | 153056.00 | **232.09** MB |
| `java` (` 8`) | [javalin](https://javalin.io) (** 3.4**) | 142211.67 | **253.93** MB |
| `scala` (` 2.12`) | [akkahttp](https://akka.io) (** 10.1**) | 140794.67 | **304.34** MB |
| `go` (` 1.13`) | [air](https://github.com/aofei/air) (** 0.12**) | 133068.00 | **277.67** MB |
| `node` (` 12.1`) | [polkadot](https://github.com/lukeed/polkadot) (** 1.0**) | 132056.67 | **197.98** MB |
| `node` (` 12.1`) | [restana](https://github.com/jkyberneees/ana) (** 3.3**) | 127106.00 | **190.48** MB |
| `python` (` 3.7`) | [falcon](https://falconframework.org) (** 2.0**) | 126499.00 | **296.72** MB |
| `kotlin` (` 1.3`) | [ktor](https://ktor.io) (** 1.2**) | 124700.00 | **194.36** MB |
| `node` (` 12.1`) | [0http](https://github.com/jkyberneees/0http) (** 1.2**) | 121901.33 | **182.73** MB |
| `node` (` 12.1`) | [rayo](https://rayo.js.org) (** 1.3**) | 114429.33 | **171.59** MB |
| `node` (` 12.1`) | [polka](https://github.com/lukeed/polka) (** 0.5**) | 113857.00 | **170.70** MB |
| `node` (` 12.1`) | [muneem](https://github.com/node-muneem/muneem) (** 2.4**) | 110248.67 | **165.29** MB |
| `swift` (` 5.0`) | [perfect](https://perfect.org) (** 3.1**) | 110146.67 | **103.60** MB |
| `java` (` 8`) | [spring-boot](https://spring.io/projects/spring-boot) (** 2.1**) | 104517.00 | **79.04** MB |
| `php` (` 7.3`) | [hyperf](https://www.hyperf.io) (** 1.0**) | 99622.67 | **212.52** MB |
| `scala` (` 2.12`) | [http4s](https://http4s.org) (** 0.18**) | 98478.33 | **172.89** MB |
| `node` (` 12.1`) | [foxify](https://foxify.js.org) (** 0.1**) | 97578.33 | **205.21** MB |
| `node` (` 12.1`) | [fastify](https://fastify.io) (** 2.8**) | 93809.67 | **244.29** MB |
| `python` (` 3.7`) | [bottle](https://bottlepy.org) (** 0.12**) | 88356.00 | **217.84** MB |
| `node` (` 12.1`) | [koa](https://koajs.com) (** 2.8**) | 86727.33 | **183.67** MB |
| `node` (` 12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (** 0.0**) | 84604.33 | **344.04** MB |
| `php` (` 7.3`) | [slim](https://slimframework.com) (** 4.2**) | 84252.67 | **418.61** MB |
| `php` (` 7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (** 3.2**) | 84148.33 | **417.98** MB |
| `swift` (` 5.0`) | [vapor](https://vapor.codes) (** 3.3**) | 83759.33 | **140.24** MB |
| `php` (` 7.3`) | [symfony](https://symfony.com) (** 4.3**) | 80375.33 | **399.73** MB |
| `clojure` (` 1.10`) | [coast](https://coastonclojure.com) (** 1.0**) | 78264.33 | **140.19** MB |
| `python` (` 3.7`) | [hug](https://hug.rest) (** 2.6**) | 76817.00 | **190.62** MB |
| `php` (` 7.3`) | [zend-framework](https://framework.zend.com) (** 3.1**) | 76262.33 | **379.28** MB |
| `node` (` 12.1`) | [express](https://expressjs.com) (** 4.17**) | 73077.33 | **179.07** MB |
| `python` (` 3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (** 0.1**) | 70635.67 | **142.19** MB |
| `php` (` 7.3`) | [lumen](https://lumen.laravel.com) (** 5.8**) | 70501.33 | **367.09** MB |
| `python` (` 3.7`) | [starlette](https://starlette.io) (** 0.12**) | 67063.00 | **144.60** MB |
| `php` (` 7.3`) | [laravel](https://laravel.com) (** 5.8**) | 65247.00 | **340.77** MB |
| `node` (` 12.1`) | [restify](https://restify.com) (** 8.4**) | 63851.00 | **112.11** MB |
| `swift` (` 5.0`) | [kitura](https://kitura.io) (** 2.7**) | 60131.00 | **111.43** MB |
| `ruby` (` 2.6`) | [syro](https://github.com/soveran/syro) (** 3.1**) | 58803.00 | **34.01** MB |
| `ruby` (` 2.6`) | [roda](https://roda.jeremyevans.net) (** 3.23**) | 58177.33 | **55.58** MB |
| `swift` (` 5.0`) | [kitura-nio](https://kitura.io) (** 2.7**) | 54420.00 | **102.95** MB |
| `ruby` (` 2.6`) | [cuba](https://cuba.is) (** 3.9**) | 52603.00 | **62.15** MB |
| `ruby` (` 2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (** 0.0**) | 51001.33 | **29.46** MB |
| `python` (` 3.7`) | [fastapi](https://fastapi.tiangolo.com) (** 0.38**) | 48013.00 | **103.79** MB |
| `crystal` (` 0.29`) | [lucky](https://luckyframework.org) (** 0.16**) | 47804.00 | **58.63** MB |
| `node` (` 12.1`) | [moleculer](https://moleculer.services) (** 0.13**) | 47068.00 | **80.56** MB |
| `php` (` 7.3`) | [swoft](https://swoft.org) (** 2.0**) | 46675.67 | **122.86** MB |
| `node` (` 12.1`) | [hapi](https://hapijs.com) (** 18.1**) | 46180.33 | **119.77** MB |
| `python` (` 3.7`) | [molten](https://moltenframework.com) (** 0.27**) | 38758.67 | **72.10** MB |
| `node` (` 12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (** 2.0**) | 34954.33 | **32.78** MB |
| `python` (` 3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (** 3.6**) | 33275.00 | **75.42** MB |
| `python` (` 3.7`) | [flask](https://flask.pocoo.org) (** 1.1**) | 32442.67 | **79.99** MB |
| `php` (` 7.3`) | [imi](https://imiphp.com) (** 1.0**) | 31585.67 | **72.62** MB |
| `ruby` (` 2.6`) | [flame](https://github.com/AlexWayfer/flame) (** 4.18**) | 30642.67 | **17.72** MB |
| `python` (` 3.7`) | [sanic](https://github.com/huge-success/sanic) (** 19.6**) | 30549.67 | **54.49** MB |
| `ruby` (` 2.6`) | [hanami](https://hanamirb.org) (** 1.3**) | 28927.33 | **218.98** MB |
| `python` (` 3.7`) | [bocadillo](https://bocadilloproject.github.io) (** 0.18**) | 26764.33 | **51.81** MB |
| `ruby` (` 2.6`) | [sinatra](https://sinatrarb.com) (** 2.0**) | 23389.33 | **60.72** MB |
| `swift` (` 5.0`) | [swifter](https://github.com/httpswift/swifter) (** 1.4**) | 19612.00 | **25.08** MB |
| `python` (` 3.7`) | [quart](https://pgjones.gitlab.io/quart) (** 0.10**) | 16830.33 | **33.58** MB |
| `python` (` 3.7`) | [django](https://djangoproject.com) (** 2.2**) | 12833.33 | **37.21** MB |
| `python` (` 3.7`) | [responder](https://python-responder.org) (** 1.3**) | 11685.33 | **25.52** MB |
| `python` (` 3.7`) | [tornado](https://tornadoweb.org) (** 5.1**) | 11207.67 | **33.14** MB |
| `python` (` 3.7`) | [masonite](https://masoniteproject.com) (** 2.2**) | 8620.00 | **21.26** MB |
| `crystal` (` 0.29`) | [athena](https://github.com/blacksmoke16/athena) (** 0.7**) | 8270.00 | **10.33** MB |
| `ruby` (` 2.6`) | [rails](https://rubyonrails.org) (** 6.0**) | 5893.33 | **37.10** MB |
| `crystal` (` 0.29`) | [onyx](https://onyxframework.org) (** 0.5**) | 5131.00 | **13.25** MB |
| `perl` (` 5.3`) | [dancer2](https://perldancer.org) (** 2.0**) | 1315.00 | **2.99** MB |
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

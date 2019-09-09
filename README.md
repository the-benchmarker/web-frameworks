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
Last update: 2019-09-09
```
OS: Linux (version: 5.2.11-200.fc30.x86_64, arch: x86_64)
CPU Cores: 12
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: swifter (swift)


:four: roda (ruby)


:five: agoo-c (c)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `rust` (` 1.37`) | [nickel](https://nickel-org.github.io) (** 0.11**) | 0.07 ms | **0.06** ms | 0.11 ms | 0.13 ms | 3.64 ms | **46.67** | 
| `rust` (` 1.37`) | [iron](https://ironframework.io) (** 0.6**) | 0.42 ms | **0.20** ms | 1.05 ms | 2.74 ms | 15.05 ms | **585.33** | 
| `swift` (` 5.0`) | [swifter](https://github.com/httpswift/swifter) (** 1.4**) | 5.12 ms | **0.92** ms | 14.63 ms | 15.09 ms | 1856.76 ms | **25620.67** | 
| `ruby` (` 2.6`) | [roda](https://roda.jeremyevans.net) (** 3.23**) | 2.76 ms | **1.27** ms | 7.08 ms | 13.30 ms | 30.22 ms | **3167.33** | 
| `c` (` 11`) | [agoo-c](https://github.com/ohler55/agoo-c) (** 0.5**) | 2.66 ms | **1.61** ms | 6.25 ms | 13.94 ms | 37.47 ms | **2936.33** | 
| `python` (` 3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (** 0.1**) | 3.11 ms | **1.93** ms | 7.02 ms | 16.07 ms | 35.20 ms | **3395.00** | 
| `node` (` 12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (** 0.0**) | 2.57 ms | **2.00** ms | 5.46 ms | 11.08 ms | 30.69 ms | **2334.00** | 
| `ruby` (` 2.6`) | [syro](https://github.com/soveran/syro) (** 3.1**) | 2.75 ms | **2.13** ms | 6.61 ms | 12.12 ms | 27.28 ms | **2885.67** | 
| `ruby` (` 2.6`) | [cuba](https://cuba.is) (** 3.9**) | 3.09 ms | **2.15** ms | 7.35 ms | 13.33 ms | 29.51 ms | **3181.67** | 
| `cpp` (` 14/17`) | [drogon](https://github.com/an-tao/drogon) (** 1.0**) | 3.26 ms | **2.17** ms | 7.08 ms | 16.32 ms | 47.49 ms | **3422.33** | 
| `java` (` 8`) | [rapidoid](https://rapidoid.org) (** 5.5**) | 5.13 ms | **2.26** ms | 8.96 ms | 71.64 ms | 225.48 ms | **12154.33** | 
| `go` (` 1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (** 0.1**) | 3.08 ms | **2.27** ms | 5.73 ms | 12.77 ms | 154.93 ms | **3405.33** | 
| `go` (` 1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (** 8.2**) | 3.06 ms | **2.29** ms | 5.71 ms | 11.98 ms | 98.88 ms | **2600.67** | 
| `go` (` 1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (** 4.1**) | 3.13 ms | **2.33** ms | 5.75 ms | 12.53 ms | 153.03 ms | **3383.33** | 
| `ruby` (` 2.6`) | [rails](https://rubyonrails.org) (** 6.0**) | 33.81 ms | **2.42** ms | 112.41 ms | 310.26 ms | 968.26 ms | **66163.33** | 
| `c` (` 99`) | [kore](https://kore.io) (** 3.1**) | 8.84 ms | **2.67** ms | 7.77 ms | 175.18 ms | 1010.34 ms | **52852.33** | 
| `cpp` (` 11`) | [evhtp](https://criticalstack/libevhtp) (** 1.2**) | 3.73 ms | **2.84** ms | 7.21 ms | 18.45 ms | 96.57 ms | **3806.33** | 
| `crystal` (` 0.29`) | [spider-gazelle](https://spider-gazelle.net) (** 1.6**) | 3.87 ms | **2.84** ms | 8.06 ms | 15.81 ms | 35.44 ms | **3300.33** | 
| `rust` (` 1.37`) | [gotham](https://gotham.rs) (** 0.4**) | 3.51 ms | **2.86** ms | 6.56 ms | 14.77 ms | 154.87 ms | **4172.00** | 
| `crystal` (` 0.29`) | [toro](https://github.com/soveran/toro) (** 0.4**) | 3.49 ms | **2.90** ms | 6.67 ms | 13.12 ms | 28.32 ms | **2764.33** | 
| `crystal` (` 0.29`) | [router.cr](https://github.com/tbrand/router.cr) (** 0.2**) | 3.56 ms | **2.92** ms | 6.84 ms | 13.47 ms | 29.86 ms | **2823.33** | 
| `crystal` (` 0.29`) | [raze](https://razecr.com) (** 0.3**) | 3.64 ms | **3.08** ms | 6.83 ms | 13.77 ms | 34.18 ms | **2827.67** | 
| `crystal` (` 0.29`) | [kemal](https://kemalcr.com) (** 0.28**) | 3.93 ms | **3.19** ms | 7.63 ms | 15.36 ms | 36.39 ms | **3123.00** | 
| `ruby` (` 2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (** 0.0**) | 3.75 ms | **3.32** ms | 8.61 ms | 15.47 ms | 35.31 ms | **3689.67** | 
| `go` (` 1.13`) | [gramework](https://github.com/gramework/gramework) (** 1.6**) | 4.29 ms | **3.54** ms | 7.76 ms | 13.18 ms | 151.07 ms | **3128.33** | 
| `java` (` 8`) | [act](https://actframework.org) (** 1.8**) | 5.33 ms | **3.62** ms | 10.23 ms | 19.67 ms | 280.85 ms | **7312.33** | 
| `ruby` (` 2.6`) | [agoo](https://github.com/ohler55/agoo) (** 2.1**) | 6.39 ms | **3.72** ms | 9.47 ms | 81.85 ms | 113.48 ms | **12666.00** | 
| `crystal` (` 0.29`) | [amber](https://amberframework.org) (** 0.3**) | 4.09 ms | **3.74** ms | 7.11 ms | 14.55 ms | 31.07 ms | **2817.67** | 
| `go` (` 1.13`) | [rte](https://github.com/jwilner/rte) (** 0.0**) | 4.79 ms | **3.82** ms | 8.72 ms | 19.51 ms | 155.94 ms | **4621.67** | 
| `ruby` (` 2.6`) | [hanami](https://hanamirb.org) (** 1.3**) | 30.98 ms | **3.99** ms | 17.01 ms | 908.70 ms | 5598.27 ms | **250918.33** | 
| `go` (` 1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (** 4.1**) | 5.15 ms | **4.02** ms | 9.29 ms | 20.05 ms | 286.69 ms | **7136.33** | 
| `go` (` 1.13`) | [chi](https://github.com/go-chi/chi) (** 4.0**) | 5.24 ms | **4.16** ms | 9.81 ms | 20.92 ms | 170.36 ms | **5164.33** | 
| `crystal` (` 0.29`) | [orion](https://github.com/obsidian/orion) (** 1.7**) | 4.49 ms | **4.19** ms | 7.80 ms | 16.57 ms | 32.83 ms | **3079.33** | 
| `go` (` 1.13`) | [echo](https://echo.labstack.com) (** 4.1**) | 5.58 ms | **4.25** ms | 10.25 ms | 22.06 ms | 319.13 ms | **8353.67** | 
| `go` (` 1.13`) | [beego](https://beego.me) (** 1.12**) | 5.65 ms | **4.26** ms | 10.66 ms | 23.37 ms | 232.42 ms | **7520.00** | 
| `go` (` 1.13`) | [goroute](https://goroute.github.io) (** 0.0**) | 5.41 ms | **4.28** ms | 10.61 ms | 21.56 ms | 75.97 ms | **4295.33** | 
| `go` (` 1.13`) | [kami](https://github.com/guregu/kami) (** 2.2**) | 5.29 ms | **4.30** ms | 9.14 ms | 20.52 ms | 238.34 ms | **5848.33** | 
| `go` (` 1.13`) | [violetear](https://violetear.org) (** 7.0**) | 5.31 ms | **4.32** ms | 8.99 ms | 19.56 ms | 282.38 ms | **6681.33** | 
| `go` (` 1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (** 1.7**) | 5.82 ms | **4.33** ms | 11.31 ms | 24.52 ms | 338.89 ms | **7875.33** | 
| `go` (` 1.13`) | [gin](https://gin-gonic.com) (** 1.4**) | 5.56 ms | **4.33** ms | 10.44 ms | 21.99 ms | 235.02 ms | **6944.33** | 
| `csharp` (` 7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (** 2.2**) | 5.92 ms | **4.43** ms | 7.19 ms | 54.63 ms | 354.85 ms | **13232.67** | 
| `ruby` (` 2.6`) | [flame](https://github.com/AlexWayfer/flame) (** 4.18**) | 5.44 ms | **4.56** ms | 12.48 ms | 22.94 ms | 56.25 ms | **5297.33** | 
| `go` (` 1.13`) | [air](https://github.com/aofei/air) (** 0.12**) | 6.68 ms | **4.65** ms | 14.18 ms | 29.41 ms | 336.44 ms | **7798.33** | 
| `nim` (` 0.20`) | [jester](https://github.com/dom96/jester) (** 0.4**) | 4.71 ms | **4.67** ms | 7.13 ms | 12.85 ms | 35.35 ms | **2418.67** | 
| `python` (` 3.7`) | [falcon](https://falconframework.org) (** 2.0**) | 8.17 ms | **4.89** ms | 18.49 ms | 37.80 ms | 209.44 ms | **8494.00** | 
| `go` (` 1.13`) | [gf](https://goframe.org) (** 1.8**) | 6.54 ms | **4.91** ms | 12.36 ms | 26.91 ms | 179.26 ms | **6416.33** | 
| `scala` (` 2.12`) | [akkahttp](https://akka.io) (** 10.1**) | 153.64 ms | **4.98** ms | 20.34 ms | 3985.18 ms | 6242.68 ms | **651683.67** | 
| `kotlin` (` 1.3`) | [ktor](https://ktor.io) (** 1.2**) | 8.63 ms | **5.20** ms | 19.73 ms | 40.88 ms | 434.29 ms | **12996.67** | 
| `node` (` 12.1`) | [polkadot](https://github.com/lukeed/polkadot) (** 1.0**) | 6.46 ms | **5.23** ms | 9.72 ms | 18.03 ms | 172.40 ms | **3750.67** | 
| `node` (` 12.1`) | [0http](https://github.com/jkyberneees/0http) (** 1.2**) | 6.72 ms | **5.39** ms | 9.74 ms | 19.28 ms | 204.57 ms | **5281.00** | 
| `node` (` 12.1`) | [restana](https://github.com/jkyberneees/ana) (** 3.3**) | 7.59 ms | **5.99** ms | 10.26 ms | 23.70 ms | 330.21 ms | **10790.00** | 
| `ruby` (` 2.6`) | [sinatra](https://sinatrarb.com) (** 2.0**) | 7.82 ms | **6.49** ms | 18.19 ms | 34.37 ms | 73.38 ms | **7903.33** | 
| `node` (` 12.1`) | [polka](https://github.com/lukeed/polka) (** 0.5**) | 7.87 ms | **6.68** ms | 11.21 ms | 22.56 ms | 322.31 ms | **9420.33** | 
| `node` (` 12.1`) | [rayo](https://rayo.js.org) (** 1.3**) | 7.61 ms | **6.72** ms | 11.34 ms | 20.77 ms | 229.84 ms | **5657.67** | 
| `php` (` 7.3`) | [hyperf](https://www.hyperf.io) (** 1.0**) | 13.55 ms | **6.96** ms | 35.14 ms | 73.85 ms | 185.49 ms | **16390.67** | 
| `rust` (` 1.37`) | [actix-web](https://actix.rs) (** 1.0**) | 7.20 ms | **7.00** ms | 9.27 ms | 12.51 ms | 105.17 ms | **2279.67** | 
| `python` (` 3.7`) | [bottle](https://bottlepy.org) (** 0.12**) | 10.02 ms | **7.44** ms | 20.15 ms | 39.85 ms | 281.50 ms | **10054.00** | 
| `node` (` 12.1`) | [muneem](https://github.com/node-muneem/muneem) (** 2.4**) | 9.76 ms | **7.69** ms | 12.92 ms | 54.34 ms | 407.65 ms | **16315.67** | 
| `php` (` 7.3`) | [slim](https://slimframework.com) (** 4.2**) | 24.76 ms | **7.96** ms | 51.52 ms | 252.41 ms | 518.43 ms | **50783.67** | 
| `swift` (` 5.0`) | [perfect](https://perfect.org) (** 3.1**) | 8.05 ms | **7.97** ms | 10.45 ms | 12.38 ms | 151.36 ms | **2792.00** | 
| `php` (` 7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (** 3.2**) | 25.33 ms | **8.08** ms | 52.63 ms | 261.51 ms | 476.29 ms | **52622.33** | 
| `php` (` 7.3`) | [symfony](https://symfony.com) (** 4.3**) | 25.75 ms | **8.19** ms | 51.36 ms | 270.19 ms | 437.19 ms | **53916.33** | 
| `php` (` 7.3`) | [zend-framework](https://framework.zend.com) (** 3.1**) | 28.49 ms | **8.28** ms | 56.20 ms | 309.39 ms | 460.44 ms | **61400.67** | 
| `node` (` 12.1`) | [fastify](https://fastify.io) (** 2.8**) | 11.32 ms | **8.51** ms | 15.14 ms | 51.16 ms | 504.61 ms | **20997.33** | 
| `node` (` 12.1`) | [foxify](https://foxify.js.org) (** 0.1**) | 9.68 ms | **8.58** ms | 14.35 ms | 27.08 ms | 342.70 ms | **10949.33** | 
| `php` (` 7.3`) | [lumen](https://lumen.laravel.com) (** 5.8**) | 27.80 ms | **8.72** ms | 60.26 ms | 280.32 ms | 619.27 ms | **57825.67** | 
| `node` (` 12.1`) | [koa](https://koajs.com) (** 2.8**) | 10.70 ms | **8.78** ms | 16.02 ms | 33.07 ms | 353.91 ms | **12105.67** | 
| `java` (` 8`) | [spring-boot](https://spring.io/projects/spring-boot) (** 2.1**) | 10.70 ms | **8.80** ms | 14.36 ms | 33.86 ms | 504.00 ms | **14224.67** | 
| `node` (` 12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (** 0.0**) | 10.83 ms | **8.82** ms | 15.98 ms | 32.31 ms | 406.50 ms | **14517.00** | 
| `scala` (` 2.12`) | [http4s](https://http4s.org) (** 0.18**) | 11.56 ms | **8.99** ms | 19.42 ms | 38.28 ms | 901.20 ms | **24857.67** | 
| `swift` (` 5.0`) | [vapor](https://vapor.codes) (** 3.3**) | 12.11 ms | **9.44** ms | 16.86 ms | 33.69 ms | 709.99 ms | **25417.67** | 
| `python` (` 3.7`) | [hug](https://hug.rest) (** 2.6**) | 12.99 ms | **9.52** ms | 26.32 ms | 52.12 ms | 282.95 ms | **11507.00** | 
| `node` (` 12.1`) | [express](https://expressjs.com) (** 4.17**) | 11.97 ms | **9.95** ms | 18.24 ms | 31.41 ms | 361.20 ms | **11445.33** | 
| `php` (` 7.3`) | [laravel](https://laravel.com) (** 5.8**) | 51.51 ms | **10.10** ms | 77.30 ms | 686.58 ms | 1110.78 ms | **133882.67** | 
| `python` (` 3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (** 0.1**) | 11.06 ms | **10.45** ms | 17.22 ms | 25.33 ms | 109.14 ms | **4920.33** | 
| `node` (` 12.1`) | [restify](https://restify.com) (** 8.4**) | 12.83 ms | **12.05** ms | 15.54 ms | 27.32 ms | 244.04 ms | **6122.67** | 
| `python` (` 3.7`) | [starlette](https://starlette.io) (** 0.12**) | 12.34 ms | **12.06** ms | 17.83 ms | 22.46 ms | 224.67 ms | **5940.67** | 
| `clojure` (` 1.10`) | [coast](https://coastonclojure.com) (** 1.0**) | 14.20 ms | **12.38** ms | 13.19 ms | 15.62 ms | 1013.68 ms | **33113.67** | 
| `node` (` 12.1`) | [moleculer](https://moleculer.services) (** 0.13**) | 19.02 ms | **14.33** ms | 26.17 ms | 104.02 ms | 629.95 ms | **26853.00** | 
| `swift` (` 5.0`) | [kitura-nio](https://kitura.io) (** 2.7**) | 23.92 ms | **15.22** ms | 26.69 ms | 301.91 ms | 1496.05 ms | **69772.33** | 
| `node` (` 12.1`) | [hapi](https://hapijs.com) (** 18.1**) | 20.25 ms | **15.89** ms | 28.55 ms | 89.42 ms | 627.39 ms | **27116.00** | 
| `swift` (` 5.0`) | [kitura](https://kitura.io) (** 2.7**) | 16.22 ms | **15.98** ms | 16.68 ms | 17.57 ms | 687.37 ms | **14132.67** | 
| `python` (` 3.7`) | [flask](https://flask.pocoo.org) (** 1.1**) | 28.80 ms | **18.66** ms | 61.12 ms | 94.68 ms | 317.64 ms | **22581.33** | 
| `python` (` 3.7`) | [molten](https://moltenframework.com) (** 0.27**) | 26.72 ms | **18.91** ms | 48.46 ms | 78.30 ms | 388.38 ms | **21401.33** | 
| `python` (` 3.7`) | [fastapi](https://fastapi.tiangolo.com) (** 0.38**) | 19.38 ms | **18.98** ms | 26.65 ms | 34.47 ms | 308.36 ms | **8245.67** | 
| `php` (` 7.3`) | [swoft](https://swoft.org) (** 2.0**) | 20.07 ms | **19.49** ms | 25.53 ms | 32.86 ms | 110.45 ms | **4482.33** | 
| `crystal` (` 0.29`) | [lucky](https://luckyframework.org) (** 0.16**) | 20.30 ms | **19.58** ms | 22.68 ms | 24.25 ms | 530.58 ms | **14288.00** | 
| `python` (` 3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (** 3.6**) | 24.44 ms | **24.90** ms | 33.22 ms | 38.55 ms | 175.86 ms | **7366.67** | 
| `php` (` 7.3`) | [imi](https://imiphp.com) (** 1.0**) | 27.76 ms | **26.62** ms | 44.45 ms | 63.95 ms | 247.91 ms | **12576.00** | 
| `python` (` 3.7`) | [sanic](https://github.com/huge-success/sanic) (** 19.6**) | 29.35 ms | **26.66** ms | 49.12 ms | 78.35 ms | 158.10 ms | **15086.00** | 
| `node` (` 12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (** 2.0**) | 27.17 ms | **26.87** ms | 29.23 ms | 31.77 ms | 504.77 ms | **12364.00** | 
| `python` (` 3.7`) | [bocadillo](https://bocadilloproject.github.io) (** 0.18**) | 31.54 ms | **28.82** ms | 50.60 ms | 75.51 ms | 189.91 ms | **14960.33** | 
| `perl` (` 5.3`) | [dancer2](https://perldancer.org) (** 2.0**) | 308.26 ms | **44.53** ms | 754.70 ms | 4794.42 ms | 6677.48 ms | **816832.00** | 
| `python` (` 3.7`) | [quart](https://pgjones.gitlab.io/quart) (** 0.10**) | 52.49 ms | **50.89** ms | 83.14 ms | 110.69 ms | 270.16 ms | **24387.67** | 
| `python` (` 3.7`) | [django](https://djangoproject.com) (** 2.2**) | 75.92 ms | **52.91** ms | 148.88 ms | 208.25 ms | 444.81 ms | **49369.67** | 
| `python` (` 3.7`) | [responder](https://python-responder.org) (** 1.3**) | 64.88 ms | **61.50** ms | 87.20 ms | 107.88 ms | 184.57 ms | **16439.67** | 
| `python` (` 3.7`) | [masonite](https://masoniteproject.com) (** 2.2**) | 96.00 ms | **66.84** ms | 182.68 ms | 267.73 ms | 553.37 ms | **61793.67** | 
| `python` (` 3.7`) | [tornado](https://tornadoweb.org) (** 5.1**) | 71.16 ms | **70.65** ms | 84.64 ms | 97.28 ms | 455.17 ms | **15197.33** | 
| `crystal` (` 0.29`) | [athena](https://github.com/blacksmoke16/athena) (** 0.7**) | 208.09 ms | **109.30** ms | 116.56 ms | 2721.13 ms | 5452.39 ms | **504059.33** | 
| `crystal` (` 0.29`) | [onyx](https://onyxframework.org) (** 0.5**) | 184.12 ms | **184.64** ms | 207.84 ms | 226.02 ms | 297.52 ms | **19715.33** | 

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
| `c` (` 11`) | [agoo-c](https://github.com/ohler55/agoo-c) (** 0.5**) | 423613.33 | **245.00** MB |
| `node` (` 12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (** 0.0**) | 375202.67 | **330.33** MB |
| `python` (` 3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (** 0.1**) | 360234.67 | **431.33** MB |
| `cpp` (` 14/17`) | [drogon](https://github.com/an-tao/drogon) (** 1.0**) | 333943.33 | **324.09** MB |
| `java` (` 8`) | [rapidoid](https://rapidoid.org) (** 5.5**) | 322958.33 | **581.31** MB |
| `go` (` 1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (** 8.2**) | 310190.33 | **624.06** MB |
| `go` (` 1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (** 0.1**) | 310027.67 | **499.16** MB |
| `c` (` 99`) | [kore](https://kore.io) (** 3.1**) | 305854.00 | **795.02** MB |
| `go` (` 1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (** 4.1**) | 305060.33 | **490.01** MB |
| `rust` (` 1.37`) | [gotham](https://gotham.rs) (** 0.4**) | 284592.67 | **582.67** MB |
| `cpp` (` 11`) | [evhtp](https://criticalstack/libevhtp) (** 1.2**) | 280877.33 | **272.37** MB |
| `crystal` (` 0.29`) | [toro](https://github.com/soveran/toro) (** 0.4**) | 279343.00 | **262.67** MB |
| `crystal` (` 0.29`) | [router.cr](https://github.com/tbrand/router.cr) (** 0.2**) | 275367.67 | **259.01** MB |
| `crystal` (` 0.29`) | [raze](https://razecr.com) (** 0.3**) | 268625.00 | **252.51** MB |
| `crystal` (` 0.29`) | [spider-gazelle](https://spider-gazelle.net) (** 1.6**) | 267344.33 | **284.62** MB |
| `crystal` (` 0.29`) | [kemal](https://kemalcr.com) (** 0.28**) | 253720.67 | **414.62** MB |
| `java` (` 8`) | [act](https://actframework.org) (** 1.8**) | 251776.33 | **434.37** MB |
| `ruby` (` 2.6`) | [agoo](https://github.com/ohler55/agoo) (** 2.1**) | 236207.67 | **136.54** MB |
| `crystal` (` 0.29`) | [amber](https://amberframework.org) (** 0.3**) | 235162.33 | **430.50** MB |
| `nim` (` 0.20`) | [jester](https://github.com/dom96/jester) (** 0.4**) | 230933.33 | **463.95** MB |
| `go` (` 1.13`) | [gramework](https://github.com/gramework/gramework) (** 1.6**) | 221129.67 | **564.28** MB |
| `crystal` (` 0.29`) | [orion](https://github.com/obsidian/orion) (** 1.7**) | 217001.00 | **354.86** MB |
| `go` (` 1.13`) | [rte](https://github.com/jwilner/rte) (** 0.0**) | 211749.00 | **282.49** MB |
| `go` (` 1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (** 4.1**) | 201660.00 | **268.90** MB |
| `go` (` 1.13`) | [chi](https://github.com/go-chi/chi) (** 4.0**) | 198204.00 | **264.75** MB |
| `rust` (` 1.37`) | [iron](https://ironframework.io) (** 0.6**) | 197969.00 | **249.53** MB |
| `csharp` (` 7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (** 2.2**) | 194635.33 | **316.94** MB |
| `go` (` 1.13`) | [kami](https://github.com/guregu/kami) (** 2.2**) | 193079.67 | **256.27** MB |
| `go` (` 1.13`) | [goroute](https://goroute.github.io) (** 0.0**) | 192805.67 | **338.22** MB |
| `go` (` 1.13`) | [violetear](https://violetear.org) (** 7.0**) | 192101.67 | **254.78** MB |
| `go` (` 1.13`) | [echo](https://echo.labstack.com) (** 4.1**) | 191730.00 | **336.25** MB |
| `go` (` 1.13`) | [beego](https://beego.me) (** 1.12**) | 190899.00 | **256.14** MB |
| `go` (` 1.13`) | [gin](https://gin-gonic.com) (** 1.4**) | 190002.33 | **333.17** MB |
| `go` (` 1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (** 1.7**) | 185782.33 | **247.37** MB |
| `rust` (` 1.37`) | [actix-web](https://actix.rs) (** 1.0**) | 179863.00 | **273.37** MB |
| `rust` (` 1.37`) | [nickel](https://nickel-org.github.io) (** 0.11**) | 173416.33 | **345.64** MB |
| `go` (` 1.13`) | [air](https://github.com/aofei/air) (** 0.12**) | 166961.00 | **348.24** MB |
| `go` (` 1.13`) | [gf](https://goframe.org) (** 1.8**) | 162344.33 | **246.10** MB |
| `node` (` 12.1`) | [polkadot](https://github.com/lukeed/polkadot) (** 1.0**) | 146075.67 | **218.85** MB |
| `kotlin` (` 1.3`) | [ktor](https://ktor.io) (** 1.2**) | 145950.67 | **227.67** MB |
| `scala` (` 2.12`) | [akkahttp](https://akka.io) (** 10.1**) | 145174.00 | **313.77** MB |
| `node` (` 12.1`) | [0http](https://github.com/jkyberneees/0http) (** 1.2**) | 144702.33 | **216.71** MB |
| `node` (` 12.1`) | [restana](https://github.com/jkyberneees/ana) (** 3.3**) | 138153.00 | **207.02** MB |
| `python` (` 3.7`) | [falcon](https://falconframework.org) (** 2.0**) | 137890.00 | **323.41** MB |
| `node` (` 12.1`) | [polka](https://github.com/lukeed/polka) (** 0.5**) | 130023.00 | **194.81** MB |
| `node` (` 12.1`) | [rayo](https://rayo.js.org) (** 1.3**) | 127441.67 | **190.89** MB |
| `swift` (` 5.0`) | [perfect](https://perfect.org) (** 3.1**) | 118438.67 | **111.42** MB |
| `node` (` 12.1`) | [muneem](https://github.com/node-muneem/muneem) (** 2.4**) | 114334.00 | **171.26** MB |
| `java` (` 8`) | [spring-boot](https://spring.io/projects/spring-boot) (** 2.1**) | 108946.67 | **82.39** MB |
| `node` (` 12.1`) | [fastify](https://fastify.io) (** 2.8**) | 107795.00 | **279.42** MB |
| `php` (` 7.3`) | [hyperf](https://www.hyperf.io) (** 1.0**) | 106693.33 | **227.61** MB |
| `node` (` 12.1`) | [foxify](https://foxify.js.org) (** 0.1**) | 106294.00 | **223.33** MB |
| `python` (` 3.7`) | [bottle](https://bottlepy.org) (** 0.12**) | 106161.33 | **261.64** MB |
| `scala` (` 2.12`) | [http4s](https://http4s.org) (** 0.18**) | 99561.00 | **174.56** MB |
| `node` (` 12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (** 0.0**) | 99254.00 | **403.02** MB |
| `node` (` 12.1`) | [koa](https://koajs.com) (** 2.8**) | 96554.00 | **204.42** MB |
| `php` (` 7.3`) | [slim](https://slimframework.com) (** 4.2**) | 94888.67 | **471.52** MB |
| `php` (` 7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (** 3.2**) | 93210.67 | **463.16** MB |
| `php` (` 7.3`) | [symfony](https://symfony.com) (** 4.3**) | 92690.33 | **460.82** MB |
| `swift` (` 5.0`) | [vapor](https://vapor.codes) (** 3.3**) | 91610.33 | **153.56** MB |
| `php` (` 7.3`) | [zend-framework](https://framework.zend.com) (** 3.1**) | 90872.33 | **451.91** MB |
| `python` (` 3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (** 0.1**) | 87853.00 | **176.56** MB |
| `php` (` 7.3`) | [lumen](https://lumen.laravel.com) (** 5.8**) | 87174.33 | **453.88** MB |
| `node` (` 12.1`) | [express](https://expressjs.com) (** 4.17**) | 84949.00 | **208.18** MB |
| `python` (` 3.7`) | [hug](https://hug.rest) (** 2.6**) | 83012.00 | **205.88** MB |
| `clojure` (` 1.10`) | [coast](https://coastonclojure.com) (** 1.0**) | 80190.00 | **143.92** MB |
| `python` (` 3.7`) | [starlette](https://starlette.io) (** 0.12**) | 77884.67 | **167.55** MB |
| `node` (` 12.1`) | [restify](https://restify.com) (** 8.4**) | 76686.67 | **134.62** MB |
| `php` (` 7.3`) | [laravel](https://laravel.com) (** 5.8**) | 75065.33 | **391.62** MB |
| `ruby` (` 2.6`) | [syro](https://github.com/soveran/syro) (** 3.1**) | 69176.00 | **39.99** MB |
| `ruby` (` 2.6`) | [roda](https://roda.jeremyevans.net) (** 3.23**) | 68697.00 | **65.61** MB |
| `ruby` (` 2.6`) | [cuba](https://cuba.is) (** 3.9**) | 61844.33 | **72.97** MB |
| `swift` (` 5.0`) | [kitura](https://kitura.io) (** 2.7**) | 61653.33 | **114.23** MB |
| `swift` (` 5.0`) | [kitura-nio](https://kitura.io) (** 2.7**) | 58465.67 | **110.60** MB |
| `node` (` 12.1`) | [moleculer](https://moleculer.services) (** 0.13**) | 58259.00 | **99.74** MB |
| `node` (` 12.1`) | [hapi](https://hapijs.com) (** 18.1**) | 55870.00 | **144.89** MB |
| `ruby` (` 2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (** 0.0**) | 51038.00 | **29.50** MB |
| `python` (` 3.7`) | [fastapi](https://fastapi.tiangolo.com) (** 0.38**) | 50096.00 | **108.27** MB |
| `crystal` (` 0.29`) | [lucky](https://luckyframework.org) (** 0.16**) | 49524.67 | **60.72** MB |
| `php` (` 7.3`) | [swoft](https://swoft.org) (** 2.0**) | 48374.67 | **127.33** MB |
| `python` (` 3.7`) | [molten](https://moltenframework.com) (** 0.27**) | 40519.00 | **75.38** MB |
| `python` (` 3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (** 3.6**) | 39694.33 | **90.07** MB |
| `python` (` 3.7`) | [flask](https://flask.pocoo.org) (** 1.1**) | 38253.00 | **94.31** MB |
| `node` (` 12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (** 2.0**) | 36313.67 | **33.99** MB |
| `ruby` (` 2.6`) | [flame](https://github.com/AlexWayfer/flame) (** 4.18**) | 35147.00 | **20.32** MB |
| `php` (` 7.3`) | [imi](https://imiphp.com) (** 1.0**) | 34953.67 | **80.40** MB |
| `python` (` 3.7`) | [sanic](https://github.com/huge-success/sanic) (** 19.6**) | 33969.33 | **60.62** MB |
| `python` (` 3.7`) | [bocadillo](https://bocadilloproject.github.io) (** 0.18**) | 31583.67 | **61.10** MB |
| `ruby` (` 2.6`) | [hanami](https://hanamirb.org) (** 1.3**) | 28017.33 | **212.25** MB |
| `ruby` (` 2.6`) | [sinatra](https://sinatrarb.com) (** 2.0**) | 24549.33 | **63.78** MB |
| `swift` (` 5.0`) | [swifter](https://github.com/httpswift/swifter) (** 1.4**) | 20274.00 | **25.98** MB |
| `python` (` 3.7`) | [quart](https://pgjones.gitlab.io/quart) (** 0.10**) | 18699.67 | **37.35** MB |
| `python` (` 3.7`) | [responder](https://python-responder.org) (** 1.3**) | 15018.00 | **32.78** MB |
| `python` (` 3.7`) | [django](https://djangoproject.com) (** 2.2**) | 13727.67 | **39.83** MB |
| `python` (` 3.7`) | [tornado](https://tornadoweb.org) (** 5.1**) | 13585.67 | **40.22** MB |
| `python` (` 3.7`) | [masonite](https://masoniteproject.com) (** 2.2**) | 10769.00 | **26.56** MB |
| `crystal` (` 0.29`) | [athena](https://github.com/blacksmoke16/athena) (** 0.7**) | 8749.67 | **10.94** MB |
| `ruby` (` 2.6`) | [rails](https://rubyonrails.org) (** 6.0**) | 5929.67 | **37.40** MB |
| `crystal` (` 0.29`) | [onyx](https://onyxframework.org) (** 0.5**) | 5324.67 | **13.71** MB |
| `perl` (` 5.3`) | [dancer2](https://perldancer.org) (** 2.0**) | 1508.33 | **3.42** MB |
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

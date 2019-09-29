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
Last update: 2019-09-29
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
| `rust` (`1.38`) | [nickel](https://nickel-org.github.io) (**0.11**) | **0.10** ms | 0.09 ms | 0.13 ms | 0.18 ms | 4.53 ms | **54.00** | 
| `rust` (`1.38`) | [iron](https://ironframework.io) (**0.6**) | **0.49** ms | 0.39 ms | 0.92 ms | 1.71 ms | 20.80 ms | **411.67** | 
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | **2.60** ms | 0.69 ms | 7.25 ms | 15.51 ms | 49.74 ms | **3545.33** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.24**) | **2.69** ms | 0.95 ms | 7.12 ms | 14.21 ms | 38.42 ms | **3313.00** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | **2.94** ms | 0.81 ms | 8.24 ms | 17.13 ms | 47.87 ms | **3944.33** | 
| `swift` (`5.1`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | **3.57** ms | 0.93 ms | 14.54 ms | 15.02 ms | 1552.74 ms | **20753.33** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | **3.72** ms | 0.80 ms | 10.90 ms | 24.28 ms | 63.06 ms | **5488.33** | 
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | **4.06** ms | 3.63 ms | 7.88 ms | 13.50 ms | 34.99 ms | **2878.33** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | **4.53** ms | 4.10 ms | 8.94 ms | 15.17 ms | 33.04 ms | **3316.67** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | **4.56** ms | 3.66 ms | 10.22 ms | 17.62 ms | 40.64 ms | **4098.00** | 
| `nim` (`1.0`) | [httpbeast](https://github.com/dom96/httpbeast) (**0.2**) | **4.59** ms | 4.11 ms | 8.10 ms | 13.52 ms | 30.23 ms | **2655.00** | 
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | **4.80** ms | 4.36 ms | 8.68 ms | 14.32 ms | 32.40 ms | **2903.00** | 
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | **5.07** ms | 4.61 ms | 7.96 ms | 14.20 ms | 104.99 ms | **2692.00** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | **5.21** ms | 0.47 ms | 17.40 ms | 43.17 ms | 101.94 ms | **9435.00** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | **5.26** ms | 4.76 ms | 8.68 ms | 14.40 ms | 97.31 ms | **2834.67** | 
| `crystal` (`0.31`) | [toro](https://github.com/soveran/toro) (**0.4**) | **5.58** ms | 4.86 ms | 9.45 ms | 15.60 ms | 34.69 ms | **2979.33** | 
| `crystal` (`0.31`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | **5.64** ms | 4.89 ms | 9.53 ms | 15.72 ms | 31.63 ms | **2987.00** | 
| `crystal` (`0.31`) | [raze](https://razecr.com) (**0.3**) | **5.83** ms | 5.00 ms | 9.95 ms | 16.30 ms | 33.00 ms | **3164.00** | 
| `crystal` (`0.31`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | **6.02** ms | 5.09 ms | 10.49 ms | 17.19 ms | 33.95 ms | **3393.00** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | **6.05** ms | 0.56 ms | 20.32 ms | 45.42 ms | 110.68 ms | **10327.67** | 
| `crystal` (`0.31`) | [kemal](https://kemalcr.com) (**0.28**) | **6.13** ms | 5.33 ms | 10.11 ms | 16.45 ms | 40.48 ms | **3134.33** | 
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | **6.25** ms | 5.46 ms | 9.50 ms | 19.15 ms | 172.07 ms | **4695.00** | 
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | **6.36** ms | 5.44 ms | 9.38 ms | 19.23 ms | 188.94 ms | **7064.33** | 
| `rust` (`1.38`) | [gotham](https://gotham.rs) (**0.4**) | **6.48** ms | 5.53 ms | 10.59 ms | 17.30 ms | 157.74 ms | **4062.33** | 
| `crystal` (`0.31`) | [amber](https://amberframework.org) (**0.3**) | **6.57** ms | 5.68 ms | 10.65 ms | 17.58 ms | 38.52 ms | **3276.67** | 
| `nim` (`1.0`) | [jester](https://github.com/dom96/jester) (**0.4**) | **7.06** ms | 6.43 ms | 10.49 ms | 17.35 ms | 51.49 ms | **3059.67** | 
| `crystal` (`0.31`) | [orion](https://github.com/obsidian/orion) (**1.7**) | **7.15** ms | 6.37 ms | 11.16 ms | 18.79 ms | 41.87 ms | **3395.33** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | **7.36** ms | 0.58 ms | 24.39 ms | 54.00 ms | 117.23 ms | **12325.00** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | **7.83** ms | 7.01 ms | 11.84 ms | 19.10 ms | 116.05 ms | **4840.33** | 
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | **7.96** ms | 6.57 ms | 13.22 ms | 25.05 ms | 244.02 ms | **7854.67** | 
| `ruby` (`2.6`) | [grape](https://ruby-grape.org) (**1.2**) | **8.66** ms | 0.63 ms | 28.77 ms | 59.62 ms | 133.18 ms | **14091.00** | 
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | **8.72** ms | 7.43 ms | 14.36 ms | 30.62 ms | 119.69 ms | **5504.67** | 
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | **8.87** ms | 7.59 ms | 14.44 ms | 31.46 ms | 79.18 ms | **5510.00** | 
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | **8.93** ms | 7.51 ms | 14.90 ms | 32.04 ms | 211.78 ms | **6084.00** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.11**) | **9.02** ms | 5.91 ms | 14.68 ms | 83.53 ms | 123.06 ms | **12656.00** | 
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | **9.37** ms | 7.77 ms | 16.03 ms | 34.65 ms | 124.56 ms | **6284.67** | 
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | **9.45** ms | 7.79 ms | 15.72 ms | 33.83 ms | 243.90 ms | **8343.33** | 
| `rust` (`1.38`) | [actix-web](https://actix.rs) (**1.0**) | **10.00** ms | 9.79 ms | 13.09 ms | 16.90 ms | 32.83 ms | **2365.67** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | **10.44** ms | 4.19 ms | 32.72 ms | 84.80 ms | 126.51 ms | **18752.00** | 
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | **11.22** ms | 9.08 ms | 19.49 ms | 44.97 ms | 146.98 ms | **8069.67** | 
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | **11.38** ms | 9.16 ms | 19.53 ms | 45.96 ms | 154.25 ms | **8527.00** | 
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | **11.84** ms | 9.01 ms | 21.97 ms | 50.94 ms | 197.65 ms | **9826.33** | 
| `swift` (`5.1`) | [perfect](https://perfect.org) (**3.1**) | **11.93** ms | 12.04 ms | 14.34 ms | 16.37 ms | 42.78 ms | **1972.33** | 
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.13**) | **12.07** ms | 9.40 ms | 21.84 ms | 49.69 ms | 140.81 ms | **9192.33** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | **12.08** ms | 10.20 ms | 21.87 ms | 41.25 ms | 387.94 ms | **14109.67** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | **12.19** ms | 9.35 ms | 25.87 ms | 46.74 ms | 138.70 ms | **9121.00** | 
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | **12.99** ms | 9.64 ms | 17.36 ms | 61.32 ms | 602.39 ms | **25432.00** | 
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | **13.24** ms | 10.70 ms | 21.44 ms | 54.67 ms | 369.27 ms | **12507.00** | 
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | **13.48** ms | 9.94 ms | 25.21 ms | 60.57 ms | 338.83 ms | **13360.33** | 
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | **13.49** ms | 9.82 ms | 18.97 ms | 75.01 ms | 556.50 ms | **24204.67** | 
| `go` (`1.13`) | [gf](https://goframe.org) (**1.9**) | **13.64** ms | 11.03 ms | 23.81 ms | 54.35 ms | 183.71 ms | **9710.00** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | **14.96** ms | 5.17 ms | 11.13 ms | 350.04 ms | 1456.04 ms | **83578.33** | 
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | **15.14** ms | 9.89 ms | 18.03 ms | 177.02 ms | 680.75 ms | **35938.67** | 
| `php` (`7.3`) | [one](https://github.com/lizhichao/one) (**1.8**) | **15.49** ms | 14.04 ms | 27.23 ms | 45.58 ms | 146.63 ms | **9364.33** | 
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | **16.69** ms | 10.86 ms | 20.23 ms | 197.18 ms | 752.37 ms | **39928.67** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | **16.74** ms | 11.28 ms | 36.92 ms | 62.74 ms | 141.35 ms | **13115.67** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | **17.28** ms | 15.94 ms | 26.36 ms | 37.00 ms | 137.86 ms | **6863.33** | 
| `python` (`3.7`) | [asgineer](https://asgineer.readthedocs.io) (**0.7**) | **17.68** ms | 16.08 ms | 27.17 ms | 44.17 ms | 157.56 ms | **7959.33** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | **18.09** ms | 13.35 ms | 22.18 ms | 102.59 ms | 749.81 ms | **33341.00** | 
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | **18.77** ms | 14.03 ms | 24.24 ms | 126.73 ms | 744.93 ms | **34170.67** | 
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | **19.25** ms | 11.52 ms | 22.10 ms | 255.72 ms | 783.52 ms | **47880.67** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | **19.29** ms | 10.31 ms | 27.39 ms | 268.81 ms | 1030.03 ms | **56454.33** | 
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | **19.57** ms | 12.38 ms | 22.39 ms | 247.45 ms | 925.14 ms | **50841.00** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | **19.61** ms | 15.50 ms | 35.01 ms | 60.10 ms | 334.84 ms | **14927.00** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | **19.70** ms | 18.47 ms | 30.55 ms | 48.54 ms | 140.83 ms | **8961.33** | 
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | **20.17** ms | 18.14 ms | 18.64 ms | 25.42 ms | 1041.25 ms | **33781.67** | 
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | **20.25** ms | 13.67 ms | 24.07 ms | 242.97 ms | 874.30 ms | **47659.67** | 
| `ruby` (`2.6`) | [plezi](https://github.com/boazsegev/plezi) (**0.16**) | **21.37** ms | 16.15 ms | 38.90 ms | 98.12 ms | 209.55 ms | **17864.67** | 
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | **22.14** ms | 15.16 ms | 26.74 ms | 245.89 ms | 948.48 ms | **50452.67** | 
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | **23.65** ms | 16.38 ms | 53.66 ms | 116.37 ms | 325.34 ms | **24840.00** | 
| `swift` (`5.1`) | [kitura](https://kitura.io) (**2.7**) | **24.73** ms | 19.71 ms | 21.28 ms | 196.75 ms | 1335.38 ms | **60343.33** | 
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | **25.58** ms | 20.40 ms | 26.35 ms | 184.94 ms | 803.32 ms | **39704.33** | 
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | **25.73** ms | 14.60 ms | 27.60 ms | 396.09 ms | 1149.22 ms | **71258.00** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | **26.69** ms | 16.22 ms | 31.51 ms | 335.60 ms | 2028.09 ms | **95539.67** | 
| `fsharp` (`7.3`) | [suave](https://suave.io) (**2.5**) | **26.83** ms | 3.81 ms | 71.19 ms | 114.35 ms | 435.95 ms | **32907.00** | 
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | **28.14** ms | 16.72 ms | 29.40 ms | 447.65 ms | 1269.01 ms | **78250.67** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | **29.22** ms | 26.46 ms | 43.89 ms | 74.63 ms | 147.27 ms | **12403.67** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | **30.49** ms | 2.11 ms | 101.84 ms | 275.07 ms | 794.84 ms | **58630.67** | 
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | **30.71** ms | 10.94 ms | 72.70 ms | 347.29 ms | 1074.66 ms | **66053.00** | 
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | **31.34** ms | 26.43 ms | 46.74 ms | 103.69 ms | 355.44 ms | **19834.33** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | **33.61** ms | 31.49 ms | 48.46 ms | 70.16 ms | 214.72 ms | **12019.67** | 
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | **34.42** ms | 25.13 ms | 38.80 ms | 331.78 ms | 1114.15 ms | **60999.67** | 
| `swift` (`5.1`) | [vapor](https://vapor.codes) (**3.3**) | **37.23** ms | 17.58 ms | 31.25 ms | 791.65 ms | 2209.32 ms | **139608.00** | 
| `crystal` (`0.31`) | [lucky](https://luckyframework.org) (**0.16**) | **38.28** ms | 37.44 ms | 45.75 ms | 53.10 ms | 78.36 ms | **5804.33** | 
| `php` (`7.3`) | [basicphp](https://github.com/ray-ang/basicphp) (**0.9**) | **40.67** ms | 13.40 ms | 79.99 ms | 403.07 ms | 862.01 ms | **79923.33** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | **42.18** ms | 32.95 ms | 84.99 ms | 112.26 ms | 501.89 ms | **30582.00** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | **42.74** ms | 32.07 ms | 88.55 ms | 106.94 ms | 414.17 ms | **25730.33** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | **43.65** ms | 43.16 ms | 65.57 ms | 84.67 ms | 114.41 ms | **15632.33** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | **45.24** ms | 44.19 ms | 61.26 ms | 84.61 ms | 141.54 ms | **12935.00** | 
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | **45.61** ms | 41.03 ms | 46.43 ms | 213.50 ms | 869.38 ms | **43866.67** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | **47.31** ms | 15.73 ms | 100.53 ms | 489.96 ms | 893.33 ms | **98028.00** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | **48.75** ms | 45.47 ms | 75.01 ms | 113.22 ms | 310.55 ms | **21963.00** | 
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | **54.26** ms | 26.20 ms | 48.23 ms | 916.81 ms | 1890.66 ms | **147412.00** | 
| `crystal` (`0.31`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | **54.49** ms | 54.49 ms | 60.28 ms | 64.79 ms | 89.45 ms | **5270.00** | 
| `swift` (`5.1`) | [kitura-nio](https://kitura.io) (**2.7**) | **57.33** ms | 24.35 ms | 38.07 ms | 1266.98 ms | 3726.67 ms | **222709.00** | 
| `php` (`7.3`) | [spiral](https://github.com/spiral/framework) (**2.1**) | **57.86** ms | 57.36 ms | 64.28 ms | 72.75 ms | 264.11 ms | **8034.00** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | **60.29** ms | 18.80 ms | 120.55 ms | 627.26 ms | 1167.20 ms | **124434.00** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**6.0**) | **65.71** ms | 21.56 ms | 130.42 ms | 680.20 ms | 1159.75 ms | **135326.67** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | **67.99** ms | 21.14 ms | 126.35 ms | 744.04 ms | 1463.15 ms | **148395.33** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | **70.25** ms | 21.36 ms | 141.97 ms | 781.08 ms | 1688.31 ms | **154688.00** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | **81.02** ms | 80.32 ms | 119.14 ms | 155.32 ms | 267.64 ms | **29344.67** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.0**) | **96.60** ms | 23.75 ms | 189.42 ms | 1130.83 ms | 2017.06 ms | **219032.67** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | **105.54** ms | 100.01 ms | 156.19 ms | 201.62 ms | 422.49 ms | **35830.67** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | **105.80** ms | 89.68 ms | 192.55 ms | 219.59 ms | 739.06 ms | **50274.67** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | **107.70** ms | 101.58 ms | 136.03 ms | 158.20 ms | 839.64 ms | **36716.00** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | **131.09** ms | 107.15 ms | 248.07 ms | 283.39 ms | 775.82 ms | **61894.67** | 
| `crystal` (`0.31`) | [onyx](https://onyxframework.org) (**0.5**) | **211.21** ms | 206.86 ms | 253.15 ms | 285.51 ms | 363.65 ms | **28994.33** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | **219.21** ms | 7.66 ms | 147.81 ms | 4894.90 ms | 7879.03 ms | **843074.67** | 
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | **304.04** ms | 109.97 ms | 613.74 ms | 2938.13 ms | 4944.42 ms | **606855.67** | 

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
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 227243.33 | **131.48** MB |
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 215342.00 | **189.57** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 207335.67 | **248.22** MB |
| `nim` (`1.0`) | [httpbeast](https://github.com/dom96/httpbeast) (**0.2**) | 200752.00 | **286.02** MB |
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 191346.33 | **185.79** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 185802.67 | **334.46** MB |
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 176413.67 | **284.34** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 173426.67 | **168.36** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 172721.67 | **448.66** MB |
| `crystal` (`0.31`) | [toro](https://github.com/soveran/toro) (**0.4**) | 168279.67 | **158.32** MB |
| `crystal` (`0.31`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 167020.67 | **157.11** MB |
| `crystal` (`0.31`) | [raze](https://razecr.com) (**0.3**) | 162725.33 | **153.06** MB |
| `crystal` (`0.31`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 159130.00 | **169.61** MB |
| `crystal` (`0.31`) | [kemal](https://kemalcr.com) (**0.28**) | 154772.33 | **252.87** MB |
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 152816.33 | **242.72** MB |
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 150267.67 | **302.48** MB |
| `nim` (`1.0`) | [jester](https://github.com/dom96/jester) (**0.4**) | 148673.33 | **298.94** MB |
| `rust` (`1.38`) | [gotham](https://gotham.rs) (**0.4**) | 146784.33 | **300.53** MB |
| `crystal` (`0.31`) | [amber](https://amberframework.org) (**0.3**) | 145308.67 | **266.16** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.11**) | 139885.67 | **80.83** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 138239.67 | **238.61** MB |
| `crystal` (`0.31`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 133969.00 | **219.12** MB |
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 123344.33 | **315.29** MB |
| `rust` (`1.38`) | [iron](https://ironframework.io) (**0.6**) | 115983.00 | **145.79** MB |
| `rust` (`1.38`) | [actix-web](https://actix.rs) (**1.0**) | 114946.00 | **170.12** MB |
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 112505.00 | **150.69** MB |
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 111949.33 | **150.86** MB |
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 111047.67 | **148.70** MB |
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 107708.33 | **189.09** MB |
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 106975.67 | **187.88** MB |
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 93248.67 | **124.40** MB |
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 91116.00 | **160.02** MB |
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 90388.33 | **121.10** MB |
| `rust` (`1.38`) | [nickel](https://nickel-org.github.io) (**0.11**) | 90038.00 | **178.57** MB |
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 88917.00 | **133.32** MB |
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.13**) | 87940.00 | **183.59** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 87365.67 | **136.13** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 87161.00 | **204.37** MB |
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 86123.33 | **129.13** MB |
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 85097.33 | **127.56** MB |
| `swift` (`5.1`) | [perfect](https://perfect.org) (**3.1**) | 81531.00 | **76.68** MB |
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 81237.33 | **108.62** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 80720.33 | **131.56** MB |
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 80031.33 | **105.83** MB |
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 78597.00 | **117.84** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 78413.00 | **169.59** MB |
| `go` (`1.13`) | [gf](https://goframe.org) (**1.9**) | 77342.67 | **132.09** MB |
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | 74867.33 | **133.65** MB |
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 73128.67 | **109.63** MB |
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 70428.33 | **105.60** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 69177.33 | **51.55** MB |
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 65923.33 | **173.17** MB |
| `php` (`7.3`) | [one](https://github.com/lizhichao/one) (**1.8**) | 65705.00 | **151.02** MB |
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 65232.67 | **137.19** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 65202.00 | **160.73** MB |
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 59431.67 | **241.68** MB |
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 58793.33 | **124.53** MB |
| `php` (`7.3`) | [basicphp](https://github.com/ray-ang/basicphp) (**0.9**) | 57553.67 | **285.92** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 57027.67 | **114.65** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 56761.67 | **99.59** MB |
| `python` (`3.7`) | [asgineer](https://asgineer.readthedocs.io) (**0.7**) | 56426.67 | **100.80** MB |
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | 55443.67 | **99.47** MB |
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 53577.00 | **131.28** MB |
| `ruby` (`2.6`) | [plezi](https://github.com/boazsegev/plezi) (**0.16**) | 53304.00 | **113.54** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 52806.67 | **131.03** MB |
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 52696.33 | **112.38** MB |
| `swift` (`5.1`) | [vapor](https://vapor.codes) (**3.3**) | 51225.67 | **87.77** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 50542.33 | **251.37** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 50512.33 | **108.72** MB |
| `swift` (`5.1`) | [kitura](https://kitura.io) (**2.7**) | 49596.67 | **92.11** MB |
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 48671.00 | **28.15** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.24**) | 47116.67 | **44.93** MB |
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 45710.67 | **80.22** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 43335.33 | **51.15** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 40185.33 | **199.75** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 38999.00 | **193.63** MB |
| `swift` (`5.1`) | [kitura-nio](https://kitura.io) (**2.7**) | 37447.00 | **70.84** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 36258.67 | **180.19** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**6.0**) | 36057.00 | **187.38** MB |
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 35942.67 | **93.18** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 34345.00 | **90.24** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 34311.00 | **19.80** MB |
| `fsharp` (`7.3`) | [suave](https://suave.io) (**2.5**) | 33296.67 | **67.35** MB |
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | 33256.67 | **76.44** MB |
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 32988.67 | **56.00** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.0**) | 30702.67 | **160.46** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 29570.00 | **63.72** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 25835.33 | **48.04** MB |
| `crystal` (`0.31`) | [lucky](https://luckyframework.org) (**0.16**) | 25770.67 | **31.61** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 24596.00 | **14.18** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 24409.00 | **60.02** MB |
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 23438.67 | **21.98** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 22780.00 | **51.59** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 21992.67 | **39.15** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 21097.33 | **159.55** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 20503.67 | **39.53** MB |
| `crystal` (`0.31`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 18247.67 | **22.84** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 17389.67 | **45.09** MB |
| `php` (`7.3`) | [spiral](https://github.com/spiral/framework) (**2.1**) | 16858.33 | **29.24** MB |
| `ruby` (`2.6`) | [grape](https://ruby-grape.org) (**1.2**) | 14862.00 | **8.51** MB |
| `swift` (`5.1`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 12435.00 | **15.92** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | 12201.33 | **24.31** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 9496.33 | **27.53** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 9300.00 | **20.26** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 9110.00 | **26.94** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 7545.00 | **18.59** MB |
| `crystal` (`0.31`) | [onyx](https://onyxframework.org) (**0.5**) | 4684.33 | **12.07** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 4198.00 | **26.39** MB |
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | 500.33 | **1.14** MB |
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

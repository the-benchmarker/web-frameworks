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
Last update: 2019-08-27
```
OS: Linux (version: 5.1.20-300.fc30.x86_64, arch: x86_64)
CPU Cores: 4
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: hanami (ruby)


:four: rack-routing (ruby)


:five: swifter (swift)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 0.05 ms | **0.04** ms | 0.05 ms | 0.09 ms | 7.65 ms | **103.33** | 
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 0.31 ms | **0.26** ms | 0.51 ms | 1.15 ms | 43.42 ms | **445.33** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 4.84 ms | **0.35** ms | 11.43 ms | 79.39 ms | 245.98 ms | **15280.33** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 2.98 ms | **0.80** ms | 2.45 ms | 57.65 ms | 257.00 ms | **11069.00** | 
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 6.55 ms | **0.84** ms | 14.44 ms | 14.93 ms | 2823.81 ms | **57641.00** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 2.56 ms | **0.88** ms | 2.19 ms | 50.28 ms | 272.14 ms | **10011.67** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 22.72 ms | **1.02** ms | 77.54 ms | 210.46 ms | 558.99 ms | **45220.00** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.23**) | 2.39 ms | **1.11** ms | 2.09 ms | 45.98 ms | 291.43 ms | **9678.00** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 4.09 ms | **1.62** ms | 4.64 ms | 67.14 ms | 292.16 ms | **12719.67** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 5.15 ms | **2.30** ms | 5.28 ms | 79.34 ms | 328.94 ms | **14525.67** | 
| `rust` (`nightly`) | [rocket](https://rocket.rs) (**0.4**) | 135.97 ms | **3.02** ms | 51.85 ms | 3860.97 ms | 6595.47 ms | **658579.00** | 
| `node` (`12.9`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 5.06 ms | **3.91** ms | 8.39 ms | 13.85 ms | 36.73 ms | **3032.00** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 5.19 ms | **4.15** ms | 11.39 ms | 19.55 ms | 39.53 ms | **4514.67** | 
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 6.18 ms | **5.27** ms | 10.62 ms | 17.29 ms | 36.80 ms | **3690.67** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 5.71 ms | **5.29** ms | 11.13 ms | 19.12 ms | 40.23 ms | **4285.67** | 
| `go` (`1.12`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 6.41 ms | **5.46** ms | 9.25 ms | 16.08 ms | 359.32 ms | **9077.00** | 
| `go` (`1.12`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 6.08 ms | **5.48** ms | 9.17 ms | 15.75 ms | 213.43 ms | **4543.33** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 6.71 ms | **5.75** ms | 11.34 ms | 20.80 ms | 177.73 ms | **7673.00** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 13.05 ms | **5.83** ms | 12.13 ms | 256.88 ms | 1066.62 ms | **57390.00** | 
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 7.67 ms | **6.39** ms | 12.50 ms | 21.45 ms | 303.78 ms | **10128.67** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 8.17 ms | **6.76** ms | 13.94 ms | 34.45 ms | 210.72 ms | **8356.00** | 
| `python` (`3.6`) | [vibora](https://vibora.io) (**0.0**) | 7.43 ms | **6.84** ms | 13.13 ms | 20.80 ms | 48.16 ms | **4673.67** | 
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 8.03 ms | **7.42** ms | 12.60 ms | 19.17 ms | 36.44 ms | **3521.33** | 
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 8.10 ms | **7.43** ms | 12.78 ms | 19.58 ms | 35.66 ms | **3595.33** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 7.73 ms | **7.44** ms | 11.15 ms | 16.69 ms | 51.89 ms | **2802.33** | 
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 8.29 ms | **7.68** ms | 13.11 ms | 20.82 ms | 42.18 ms | **3832.00** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.9**) | 11.56 ms | **7.69** ms | 19.53 ms | 92.28 ms | 133.10 ms | **15102.33** | 
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 8.38 ms | **7.87** ms | 12.21 ms | 17.50 ms | 56.02 ms | **2739.67** | 
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 8.62 ms | **7.99** ms | 13.34 ms | 19.76 ms | 34.18 ms | **3605.00** | 
| `go` (`1.12`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 9.32 ms | **8.07** ms | 15.61 ms | 29.93 ms | 275.53 ms | **9039.33** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 202.45 ms | **8.55** ms | 47.70 ms | 4729.55 ms | 7926.79 ms | **819767.67** | 
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 9.50 ms | **8.87** ms | 14.28 ms | 21.53 ms | 39.87 ms | **3793.67** | 
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 9.57 ms | **8.91** ms | 15.30 ms | 23.32 ms | 55.15 ms | **4560.00** | 
| `go` (`1.12`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 12.74 ms | **8.94** ms | 19.93 ms | 101.62 ms | 468.48 ms | **23712.00** | 
| `go` (`1.12`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.0**) | 12.50 ms | **9.51** ms | 22.63 ms | 75.92 ms | 218.86 ms | **13884.33** | 
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 9.96 ms | **9.52** ms | 14.69 ms | 21.74 ms | 36.11 ms | **3787.67** | 
| `go` (`1.12`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 14.99 ms | **9.65** ms | 26.50 ms | 131.87 ms | 354.49 ms | **23220.67** | 
| `go` (`1.12`) | [gin](https://gin-gonic.com) (**1.4**) | 13.11 ms | **9.73** ms | 23.56 ms | 79.27 ms | 427.83 ms | **17613.33** | 
| `go` (`1.12`) | [echo](https://echo.labstack.com) (**4.1**) | 12.79 ms | **9.77** ms | 22.82 ms | 76.03 ms | 352.86 ms | **15521.33** | 
| `go` (`1.12`) | [goroute](https://goroute.github.io) (**0.0**) | 12.32 ms | **9.84** ms | 22.61 ms | 64.50 ms | 234.35 ms | **12070.00** | 
| `go` (`1.12`) | [violetear](https://violetear.org) (**7.0**) | 12.66 ms | **10.06** ms | 20.95 ms | 83.58 ms | 312.21 ms | **15292.33** | 
| `go` (`1.12`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 15.47 ms | **10.27** ms | 30.11 ms | 118.02 ms | 376.02 ms | **23072.33** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 25.64 ms | **10.40** ms | 16.39 ms | 550.71 ms | 1853.58 ms | **109786.00** | 
| `go` (`1.12`) | [beego](https://beego.me) (**1.12**) | 16.45 ms | **10.71** ms | 32.18 ms | 129.11 ms | 364.15 ms | **24941.00** | 
| `go` (`1.12`) | [kami](https://github.com/guregu/kami) (**2.2**) | 17.77 ms | **10.88** ms | 37.17 ms | 143.52 ms | 320.02 ms | **26674.00** | 
| `node` (`12.9`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 15.49 ms | **10.97** ms | 19.57 ms | 141.99 ms | 813.37 ms | **37865.00** | 
| `node` (`12.9`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 21.22 ms | **11.01** ms | 20.41 ms | 385.16 ms | 1150.06 ms | **70323.67** | 
| `node` (`12.9`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 17.82 ms | **11.50** ms | 19.98 ms | 245.99 ms | 925.31 ms | **49736.67** | 
| `go` (`1.12`) | [gf](https://goframe.org) (**1.8**) | 17.25 ms | **11.95** ms | 35.17 ms | 120.59 ms | 330.04 ms | **22069.00** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 96.64 ms | **12.23** ms | 220.79 ms | 1992.23 ms | 7050.87 ms | **380960.33** | 
| `go` (`1.12`) | [air](https://github.com/aofei/air) (**0.12**) | 20.78 ms | **12.27** ms | 46.72 ms | 164.71 ms | 486.92 ms | **32780.00** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 63.58 ms | **12.86** ms | 191.20 ms | 648.18 ms | 4177.11 ms | **190699.33** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 63.26 ms | **12.99** ms | 192.21 ms | 624.01 ms | 5113.11 ms | **184033.67** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 71.85 ms | **13.00** ms | 222.92 ms | 743.59 ms | 5872.07 ms | **223474.67** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 85.02 ms | **13.45** ms | 228.03 ms | 1214.13 ms | 7190.35 ms | **302554.00** | 
| `node` (`12.9`) | [rayo](https://rayo.js.org) (**1.3**) | 36.32 ms | **13.58** ms | 27.34 ms | 780.27 ms | 1912.62 ms | **133771.33** | 
| `node` (`12.9`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 40.36 ms | **13.79** ms | 27.58 ms | 945.24 ms | 1996.97 ms | **152267.33** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 16.17 ms | **14.86** ms | 31.40 ms | 58.60 ms | 511.15 ms | **17707.00** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 17.76 ms | **15.57** ms | 27.99 ms | 45.01 ms | 248.71 ms | **9447.00** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 89.24 ms | **15.84** ms | 30.75 ms | 2645.07 ms | 4603.81 ms | **427557.00** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 111.38 ms | **16.73** ms | 343.02 ms | 1215.13 ms | 7276.46 ms | **331686.67** | 
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 16.58 ms | **16.92** ms | 17.86 ms | 19.68 ms | 216.81 ms | **2762.00** | 
| `node` (`12.9`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 53.72 ms | **17.63** ms | 36.99 ms | 1105.95 ms | 2013.16 ms | **179677.67** | 
| `node` (`12.9`) | [fastify](https://fastify.io) (**2.7**) | 65.53 ms | **18.13** ms | 42.40 ms | 1342.13 ms | 2475.39 ms | **222539.33** | 
| `node` (`12.9`) | [koa](https://koajs.com) (**2.8**) | 40.06 ms | **18.71** ms | 41.86 ms | 700.78 ms | 1804.36 ms | **117677.33** | 
| `node` (`12.9`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 50.90 ms | **19.13** ms | 45.17 ms | 1012.49 ms | 2234.49 ms | **166353.33** | 
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 22.86 ms | **19.49** ms | 46.61 ms | 78.13 ms | 149.35 ms | **16552.00** | 
| `node` (`12.9`) | [foxify](https://foxify.js.org) (**0.1**) | 58.42 ms | **20.07** ms | 47.90 ms | 1112.40 ms | 2196.89 ms | **184038.00** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 27.32 ms | **20.65** ms | 42.06 ms | 114.29 ms | 829.90 ms | **40426.33** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 24.83 ms | **21.55** ms | 41.58 ms | 68.07 ms | 348.90 ms | **16281.67** | 
| `node` (`12.9`) | [express](https://expressjs.com) (**4.17**) | 69.88 ms | **21.90** ms | 54.36 ms | 1372.91 ms | 2807.03 ms | **229324.67** | 
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 70.50 ms | **22.77** ms | 44.14 ms | 1575.99 ms | 3550.60 ms | **270490.00** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.5**) | 30.14 ms | **24.63** ms | 49.74 ms | 103.01 ms | 266.40 ms | **18798.00** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 27.46 ms | **24.82** ms | 45.87 ms | 65.01 ms | 235.83 ms | **13291.67** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 41.75 ms | **25.07** ms | 81.04 ms | 134.91 ms | 386.76 ms | **28383.00** | 
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 25.52 ms | **25.70** ms | 30.45 ms | 39.16 ms | 387.07 ms | **11085.00** | 
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 25.49 ms | **25.91** ms | 28.28 ms | 29.67 ms | 255.00 ms | **5453.00** | 
| `node` (`12.9`) | [restify](https://restify.com) (**8.4**) | 36.27 ms | **26.48** ms | 48.47 ms | 327.86 ms | 1191.55 ms | **62931.33** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 31.22 ms | **27.67** ms | 52.33 ms | 70.75 ms | 194.74 ms | **14235.00** | 
| `node` (`12.9`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 31.89 ms | **30.74** ms | 32.93 ms | 35.71 ms | 732.26 ms | **20383.67** | 
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 43.30 ms | **30.97** ms | 52.85 ms | 457.36 ms | 1737.01 ms | **97430.00** | 
| `node` (`12.9`) | [hapi](https://hapijs.com) (**18.1**) | 177.85 ms | **32.99** ms | 191.00 ms | 3008.59 ms | 4878.55 ms | **537366.33** | 
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 35.03 ms | **33.35** ms | 49.11 ms | 80.61 ms | 151.53 ms | **12574.67** | 
| `node` (`12.9`) | [moleculer](https://moleculer.services) (**0.13**) | 142.93 ms | **35.56** ms | 90.77 ms | 2536.98 ms | 4286.23 ms | **439354.67** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.36**) | 48.52 ms | **43.22** ms | 80.21 ms | 111.85 ms | 242.98 ms | **22326.33** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 51.78 ms | **44.08** ms | 74.34 ms | 126.37 ms | 690.89 ms | **31398.67** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.5**) | 45.49 ms | **46.42** ms | 64.23 ms | 86.16 ms | 220.25 ms | **15762.33** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 51.86 ms | **48.61** ms | 68.48 ms | 104.95 ms | 773.99 ms | **30775.00** | 
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 664.63 ms | **50.73** ms | 2398.66 ms | 5524.75 ms | 7103.50 ms | **1230948.33** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 75.50 ms | **69.80** ms | 120.91 ms | 209.49 ms | 579.21 ms | **41303.33** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 94.73 ms | **91.83** ms | 136.58 ms | 203.28 ms | 574.50 ms | **37688.33** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 127.21 ms | **116.34** ms | 162.05 ms | 435.70 ms | 1566.90 ms | **82931.33** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.9**) | 118.99 ms | **118.17** ms | 195.09 ms | 260.97 ms | 609.85 ms | **57842.33** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 143.50 ms | **140.79** ms | 196.09 ms | 242.37 ms | 659.14 ms | **42408.00** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 197.09 ms | **141.16** ms | 169.68 ms | 1800.52 ms | 3653.38 ms | **330776.00** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 291.36 ms | **187.04** ms | 294.24 ms | 3185.63 ms | 5956.56 ms | **528291.33** | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (sifrr) (node)


:three: (japronto) (python)


:four: (atreugo) (go)


:five: (fasthttprouter) (go)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 180195.67 | **104.24** MB |
| `node` (`12.9`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 166852.00 | **146.82** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 164196.00 | **196.35** MB |
| `go` (`1.12`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 148896.67 | **299.36** MB |
| `go` (`1.12`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 148308.67 | **238.67** MB |
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 145921.33 | **141.53** MB |
| `python` (`3.6`) | [vibora](https://vibora.io) (**0.0**) | 141282.67 | **160.14** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 139444.67 | **251.08** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 129319.67 | **336.28** MB |
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 126118.67 | **257.58** MB |
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 124503.00 | **250.12** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 122495.67 | **118.81** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 122238.67 | **210.85** MB |
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 120070.67 | **112.71** MB |
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 118744.00 | **111.60** MB |
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 116742.00 | **124.47** MB |
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 111978.33 | **182.74** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.9**) | 111175.67 | **64.22** MB |
| `go` (`1.12`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 105438.67 | **263.33** MB |
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 104432.00 | **149.19** MB |
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 102516.00 | **187.62** MB |
| `go` (`1.12`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 98832.33 | **132.02** MB |
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 98324.33 | **123.83** MB |
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 98235.67 | **160.38** MB |
| `go` (`1.12`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.0**) | 93508.33 | **124.43** MB |
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 93084.33 | **184.84** MB |
| `go` (`1.12`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 91645.33 | **122.19** MB |
| `go` (`1.12`) | [echo](https://echo.labstack.com) (**4.1**) | 90786.67 | **159.38** MB |
| `go` (`1.12`) | [goroute](https://goroute.github.io) (**0.0**) | 90621.33 | **159.07** MB |
| `go` (`1.12`) | [gin](https://gin-gonic.com) (**1.4**) | 90412.67 | **158.42** MB |
| `go` (`1.12`) | [violetear](https://violetear.org) (**7.0**) | 89898.33 | **119.07** MB |
| `go` (`1.12`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 86656.33 | **115.44** MB |
| `go` (`1.12`) | [beego](https://beego.me) (**1.12**) | 85205.67 | **114.41** MB |
| `go` (`1.12`) | [kami](https://github.com/guregu/kami) (**2.2**) | 85022.33 | **113.24** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 84928.00 | **138.34** MB |
| `node` (`12.9`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 81983.67 | **122.75** MB |
| `node` (`12.9`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 81215.33 | **121.48** MB |
| `node` (`12.9`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 78566.00 | **117.80** MB |
| `go` (`1.12`) | [gf](https://goframe.org) (**1.8**) | 75248.00 | **113.97** MB |
| `go` (`1.12`) | [air](https://github.com/aofei/air) (**0.12**) | 75119.33 | **156.53** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 72929.00 | **157.44** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 67135.00 | **104.44** MB |
| `node` (`12.9`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 62519.67 | **93.70** MB |
| `node` (`12.9`) | [rayo](https://rayo.js.org) (**1.3**) | 61469.67 | **92.13** MB |
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 59441.67 | **55.83** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 56430.33 | **98.82** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 56385.67 | **132.12** MB |
| `node` (`12.9`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 52040.67 | **78.00** MB |
| `node` (`12.9`) | [fastify](https://fastify.io) (**2.7**) | 51219.67 | **126.12** MB |
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 46815.33 | **99.65** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 45712.67 | **33.68** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 42942.00 | **212.75** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 42696.67 | **211.34** MB |
| `node` (`12.9`) | [koa](https://koajs.com) (**2.8**) | 42560.00 | **90.08** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 41920.00 | **207.85** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 41225.67 | **101.59** MB |
| `node` (`12.9`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 40864.67 | **165.96** MB |
| `node` (`12.9`) | [foxify](https://foxify.js.org) (**0.1**) | 40631.33 | **85.31** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 40058.33 | **198.43** MB |
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 39305.67 | **72.93** MB |
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 38787.33 | **47.64** MB |
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 38201.67 | **63.73** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 37509.33 | **194.72** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 36552.00 | **73.49** MB |
| `node` (`12.9`) | [express](https://expressjs.com) (**4.17**) | 35129.00 | **85.98** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.5**) | 34575.00 | **85.70** MB |
| `node` (`12.9`) | [restify](https://restify.com) (**8.4**) | 32952.00 | **57.74** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.23**) | 32527.67 | **31.02** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 32039.33 | **68.94** MB |
| `node` (`12.9`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 31857.67 | **29.89** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 29728.67 | **35.04** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 28936.33 | **151.08** MB |
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 28488.33 | **53.71** MB |
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 27966.67 | **71.83** MB |
| `rust` (`nightly`) | [rocket](https://rocket.rs) (**0.4**) | 27153.00 | **42.60** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 25196.67 | **66.09** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 24470.67 | **14.13** MB |
| `node` (`12.9`) | [hapi](https://hapijs.com) (**18.1**) | 23641.33 | **61.13** MB |
| `node` (`12.9`) | [moleculer](https://moleculer.services) (**0.13**) | 22988.67 | **39.26** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.5**) | 21886.00 | **49.55** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.36**) | 20754.00 | **44.75** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 19859.67 | **36.83** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 19380.33 | **47.64** MB |
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 17145.33 | **21.51** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 16555.67 | **9.55** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 13534.67 | **102.37** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 13365.67 | **25.80** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 12745.33 | **33.08** MB |
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 11316.33 | **14.48** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 10427.33 | **18.58** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.9**) | 8277.00 | **16.50** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 7783.67 | **22.53** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 6866.00 | **20.23** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 6766.00 | **14.76** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 4725.67 | **11.62** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 2804.00 | **17.64** MB |
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

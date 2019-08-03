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
Last update: 2019-08-03
```
OS: Linux (version: 5.1.20-300.fc30.x86_64, arch: x86_64)
CPU Cores: 4
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: rack-routing (ruby)


:three: roda (ruby)


:four: cuba (ruby)


:five: flame (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `rust` (`1.36`) | [nickel](https://nickel-org.github.io) (**0.11**) | 0.05 ms | **0.05** ms | 0.05 ms | 0.06 ms | 4.23 ms | **28.33** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 2.23 ms | **0.16** ms | 1.30 ms | 43.08 ms | 141.66 ms | **8184.00** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.22**) | 1.74 ms | **0.25** ms | 1.12 ms | 34.15 ms | 160.67 ms | **6572.33** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 1.87 ms | **0.26** ms | 1.29 ms | 35.21 ms | 148.97 ms | **6763.33** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 3.10 ms | **0.27** ms | 6.82 ms | 50.65 ms | 163.59 ms | **9691.67** | 
| `rust` (`1.36`) | [iron](https://ironframework.io) (**0.6**) | 0.32 ms | **0.27** ms | 0.50 ms | 1.08 ms | 39.93 ms | **380.33** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 3.46 ms | **0.31** ms | 10.03 ms | 47.51 ms | 141.48 ms | **9508.33** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 4.19 ms | **0.37** ms | 15.38 ms | 53.96 ms | 147.42 ms | **10958.33** | 
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 5.19 ms | **0.74** ms | 14.32 ms | 14.73 ms | 1912.35 ms | **28240.67** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**5.2**) | 16.20 ms | **0.92** ms | 58.01 ms | 151.98 ms | 372.96 ms | **32822.00** | 
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 278.43 ms | **1.83** ms | 1101.16 ms | 3228.88 ms | 4553.56 ms | **693704.33** | 
| `rust` (`nightly`) | [rocket](https://rocket.rs) (**0.4**) | 153.16 ms | **2.79** ms | 7.29 ms | 3867.96 ms | 6593.26 ms | **690899.67** | 
| `node` (`12.7`) | [nanoexpress](https://nanoexpress.js.org) (**0.12**) | 4.51 ms | **3.79** ms | 8.11 ms | 13.93 ms | 32.23 ms | **2867.33** | 
| `node` (`12.7`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 4.60 ms | **3.86** ms | 8.45 ms | 15.19 ms | 33.51 ms | **3111.67** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 4.56 ms | **4.02** ms | 9.53 ms | 17.61 ms | 37.45 ms | **3857.67** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 4.94 ms | **4.80** ms | 9.15 ms | 15.08 ms | 31.63 ms | **3254.67** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 8.51 ms | **4.94** ms | 12.30 ms | 80.20 ms | 131.02 ms | **14103.33** | 
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.25**) | 160.32 ms | **5.03** ms | 544.64 ms | 2277.94 ms | 2709.39 ms | **433116.67** | 
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 5.74 ms | **5.07** ms | 9.60 ms | 15.92 ms | 38.75 ms | **3037.33** | 
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.28**) | 152.46 ms | **5.14** ms | 541.16 ms | 1817.93 ms | 2145.85 ms | **375516.33** | 
| `go` (`1.12`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 5.90 ms | **5.37** ms | 9.15 ms | 15.40 ms | 111.18 ms | **3590.33** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 32.54 ms | **5.95** ms | 13.35 ms | 935.86 ms | 2598.44 ms | **169432.67** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 7.40 ms | **5.96** ms | 11.99 ms | 27.62 ms | 363.91 ms | **9406.67** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 6.38 ms | **6.03** ms | 10.31 ms | 16.93 ms | 138.93 ms | **3971.33** | 
| `rust` (`1.36`) | [gotham](https://gotham.rs) (**0.4**) | 7.06 ms | **6.14** ms | 11.72 ms | 20.00 ms | 159.25 ms | **4993.67** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.9**) | 10.13 ms | **6.21** ms | 16.38 ms | 92.65 ms | 153.69 ms | **15265.33** | 
| `python` (`3.6`) | [vibora](https://vibora.io) (**0.0**) | 6.96 ms | **6.28** ms | 12.35 ms | 19.30 ms | 48.21 ms | **4347.67** | 
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.4**) | 7.78 ms | **6.78** ms | 13.47 ms | 21.71 ms | 39.12 ms | **4139.67** | 
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 7.57 ms | **6.81** ms | 12.43 ms | 19.59 ms | 34.09 ms | **3571.33** | 
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 7.92 ms | **7.13** ms | 13.05 ms | 20.68 ms | 40.46 ms | **3757.00** | 
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 8.03 ms | **7.63** ms | 11.50 ms | 15.84 ms | 57.91 ms | **2606.33** | 
| `rust` (`1.36`) | [actix-web](https://actix.rs) (**1.0**) | 8.69 ms | **8.13** ms | 13.45 ms | 19.95 ms | 164.00 ms | **5257.67** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 217.10 ms | **8.92** ms | 133.73 ms | 4929.32 ms | 7924.38 ms | **856256.67** | 
| `node` (`12.7`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 16.34 ms | **8.93** ms | 17.00 ms | 234.55 ms | 879.05 ms | **48428.00** | 
| `node` (`12.7`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 13.66 ms | **9.02** ms | 17.45 ms | 106.99 ms | 686.86 ms | **33049.67** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 12.30 ms | **9.17** ms | 12.87 ms | 67.71 ms | 861.54 ms | **36548.67** | 
| `go` (`1.12`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.0**) | 13.86 ms | **9.30** ms | 24.53 ms | 120.82 ms | 302.17 ms | **20907.67** | 
| `node` (`12.7`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 17.82 ms | **9.47** ms | 18.55 ms | 259.58 ms | 1027.50 ms | **55895.67** | 
| `go` (`1.12`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 14.49 ms | **9.58** ms | 25.35 ms | 130.88 ms | 318.01 ms | **23165.33** | 
| `go` (`1.12`) | [violetear](https://violetear.org) (**7.0**) | 12.11 ms | **9.86** ms | 19.35 ms | 73.19 ms | 215.56 ms | **13373.33** | 
| `go` (`1.12`) | [echo](https://echo.labstack.com) (**4.1**) | 15.30 ms | **9.87** ms | 29.30 ms | 120.29 ms | 300.95 ms | **22141.67** | 
| `go` (`1.12`) | [gin](https://gin-gonic.com) (**1.4**) | 15.59 ms | **9.93** ms | 29.16 ms | 125.38 ms | 351.39 ms | **23687.33** | 
| `go` (`1.12`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 16.84 ms | **10.38** ms | 34.60 ms | 136.30 ms | 410.92 ms | **25713.00** | 
| `go` (`1.12`) | [kami](https://github.com/guregu/kami) (**2.2**) | 19.28 ms | **10.61** ms | 38.16 ms | 183.87 ms | 378.98 ms | **33954.00** | 
| `go` (`1.12`) | [beego](https://beego.me) (**1.12**) | 19.41 ms | **10.69** ms | 43.59 ms | 164.17 ms | 346.33 ms | **31267.33** | 
| `node` (`12.7`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 19.64 ms | **11.96** ms | 23.81 ms | 255.60 ms | 903.44 ms | **50380.00** | 
| `node` (`12.7`) | [rayo](https://rayo.js.org) (**1.3**) | 22.98 ms | **12.04** ms | 25.09 ms | 360.61 ms | 1125.85 ms | **66445.33** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.0**) | 63.37 ms | **12.49** ms | 185.69 ms | 716.21 ms | 5037.24 ms | **196312.33** | 
| `go` (`1.12`) | [gf](https://goframe.org) (**1.8**) | 19.27 ms | **12.60** ms | 42.63 ms | 135.44 ms | 309.58 ms | **25743.33** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 64.92 ms | **12.63** ms | 187.09 ms | 623.04 ms | 5086.62 ms | **218418.67** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 61.52 ms | **12.69** ms | 188.26 ms | 618.75 ms | 5167.81 ms | **172679.00** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 14.51 ms | **12.77** ms | 29.44 ms | 55.33 ms | 402.14 ms | **16157.33** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 85.32 ms | **13.00** ms | 241.30 ms | 1145.60 ms | 6050.35 ms | **295679.67** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 77.09 ms | **13.55** ms | 209.38 ms | 942.56 ms | 5406.26 ms | **254233.67** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 17.15 ms | **13.56** ms | 30.16 ms | 55.07 ms | 277.98 ms | **12406.67** | 
| `node` (`12.7`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 30.38 ms | **14.53** ms | 30.03 ms | 555.32 ms | 1457.57 ms | **92894.67** | 
| `node` (`12.7`) | [foxify](https://foxify.js.org) (**0.1**) | 28.31 ms | **14.85** ms | 34.07 ms | 439.52 ms | 1365.48 ms | **79969.67** | 
| `node` (`12.7`) | [koa](https://koajs.com) (**2.7**) | 34.11 ms | **15.56** ms | 36.98 ms | 577.88 ms | 1451.84 ms | **97764.00** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 45.31 ms | **15.68** ms | 29.21 ms | 1055.68 ms | 4044.04 ms | **231481.33** | 
| `node` (`12.7`) | [fastify](https://fastify.io) (**2.7**) | 44.97 ms | **15.80** ms | 32.02 ms | 970.77 ms | 1908.85 ms | **156573.67** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 22.74 ms | **16.32** ms | 36.49 ms | 84.99 ms | 813.72 ms | **30314.67** | 
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 15.94 ms | **16.42** ms | 17.40 ms | 19.42 ms | 107.11 ms | **2135.33** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 109.82 ms | **16.54** ms | 318.11 ms | 1365.63 ms | 6316.06 ms | **352932.33** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 23.59 ms | **16.62** ms | 41.79 ms | 125.50 ms | 425.31 ms | **23510.67** | 
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 19.79 ms | **18.96** ms | 34.32 ms | 54.53 ms | 162.77 ms | **11095.67** | 
| `node` (`12.7`) | [restify](https://restify.com) (**8.4**) | 27.74 ms | **19.26** ms | 33.47 ms | 272.92 ms | 1042.86 ms | **56076.67** | 
| `node` (`12.7`) | [express](https://expressjs.com) (**4.17**) | 48.06 ms | **20.66** ms | 39.31 ms | 917.28 ms | 2063.22 ms | **152873.00** | 
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 110.99 ms | **20.71** ms | 52.18 ms | 2351.28 ms | 4002.78 ms | **395872.67** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.5**) | 26.84 ms | **20.81** ms | 44.36 ms | 66.49 ms | 333.19 ms | **16930.67** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 22.74 ms | **21.08** ms | 34.87 ms | 42.87 ms | 183.35 ms | **8761.67** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 40.96 ms | **24.51** ms | 80.63 ms | 139.05 ms | 326.46 ms | **28238.00** | 
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 34.92 ms | **25.59** ms | 30.48 ms | 375.97 ms | 1803.50 ms | **87495.33** | 
| `crystal` (`0.28`) | [lucky](https://luckyframework.org) (**0.14**) | 28.06 ms | **27.61** ms | 32.23 ms | 34.23 ms | 181.29 ms | **7488.00** | 
| `node` (`12.7`) | [moleculer](https://moleculer.services) (**0.13**) | 81.95 ms | **28.55** ms | 55.23 ms | 1542.58 ms | 2857.41 ms | **257190.67** | 
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 29.73 ms | **29.46** ms | 37.79 ms | 45.85 ms | 62.95 ms | **6302.67** | 
| `node` (`12.7`) | [hapi](https://hapijs.com) (**18.1**) | 94.84 ms | **30.31** ms | 50.50 ms | 1802.97 ms | 3218.00 ms | **300295.33** | 
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 54.68 ms | **30.63** ms | 53.86 ms | 927.25 ms | 2564.78 ms | **161143.67** | 
| `node` (`12.7`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 31.75 ms | **31.48** ms | 33.97 ms | 36.38 ms | 158.14 ms | **4019.00** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.33**) | 35.02 ms | **32.04** ms | 52.98 ms | 63.73 ms | 486.43 ms | **16779.00** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.5**) | 41.98 ms | **39.28** ms | 57.06 ms | 68.76 ms | 232.38 ms | **12460.00** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 51.51 ms | **42.74** ms | 79.03 ms | 157.81 ms | 598.71 ms | **35784.33** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 51.37 ms | **46.04** ms | 67.91 ms | 127.42 ms | 780.80 ms | **33283.33** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 56.19 ms | **48.77** ms | 84.01 ms | 135.96 ms | 808.20 ms | **35617.00** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 68.21 ms | **64.48** ms | 90.41 ms | 239.59 ms | 754.40 ms | **47937.00** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 74.24 ms | **67.35** ms | 115.11 ms | 238.62 ms | 931.74 ms | **48687.67** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.9**) | 89.53 ms | **92.23** ms | 121.96 ms | 135.54 ms | 502.39 ms | **28170.67** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 108.85 ms | **104.31** ms | 152.83 ms | 178.10 ms | 408.45 ms | **33056.67** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 145.44 ms | **118.68** ms | 144.29 ms | 1132.50 ms | 2318.63 ms | **185107.00** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 232.92 ms | **126.53** ms | 188.21 ms | 3092.81 ms | 4714.74 ms | **511889.67** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 178.35 ms | **140.91** ms | 196.08 ms | 1314.77 ms | 3519.37 ms | **211777.67** | 
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 245.71 ms | **162.54** ms | 194.40 ms | 2769.99 ms | 4030.17 ms | **443049.00** | 
| `python` (`3.7`) | [cyclone](https://cyclone.io) (**0.0**) | 272.59 ms | **180.08** ms | 227.92 ms | 3757.32 ms | 5974.29 ms | **597843.00** | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (sifrr) (node)


:three: (nanoexpress) (node)


:four: (japronto) (python)


:five: (drogon) (cpp)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 210232.67 | **121.65** MB |
| `node` (`12.7`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 188853.33 | **166.09** MB |
| `node` (`12.7`) | [nanoexpress](https://nanoexpress.js.org) (**0.12**) | 188567.33 | **165.88** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 184693.00 | **221.00** MB |
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 161026.00 | **156.23** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 156709.33 | **281.95** MB |
| `go` (`1.12`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 152153.00 | **244.67** MB |
| `python` (`3.6`) | [vibora](https://vibora.io) (**0.0**) | 147427.00 | **167.33** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 147079.67 | **381.83** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 143610.67 | **139.34** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 138553.33 | **239.05** MB |
| `rust` (`1.36`) | [gotham](https://gotham.rs) (**0.4**) | 134880.33 | **275.96** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.9**) | 134718.33 | **77.86** MB |
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 128333.67 | **120.53** MB |
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 128162.33 | **257.39** MB |
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.4**) | 126346.67 | **134.85** MB |
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 123602.67 | **115.93** MB |
| `rust` (`1.36`) | [actix-web](https://actix.rs) (**1.0**) | 117264.33 | **167.55** MB |
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.25**) | 113228.33 | **184.68** MB |
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.28**) | 110978.33 | **202.93** MB |
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 102861.33 | **168.05** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 99010.67 | **161.20** MB |
| `rust` (`1.36`) | [iron](https://ironframework.io) (**0.6**) | 97639.00 | **122.92** MB |
| `rust` (`1.36`) | [nickel](https://nickel-org.github.io) (**0.11**) | 95667.33 | **190.09** MB |
| `go` (`1.12`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.0**) | 94715.67 | **126.25** MB |
| `go` (`1.12`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 93363.33 | **124.95** MB |
| `go` (`1.12`) | [violetear](https://violetear.org) (**7.0**) | 92980.00 | **123.31** MB |
| `node` (`12.7`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 92694.00 | **138.69** MB |
| `node` (`12.7`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 91621.00 | **137.23** MB |
| `go` (`1.12`) | [echo](https://echo.labstack.com) (**4.1**) | 90448.67 | **158.70** MB |
| `go` (`1.12`) | [gin](https://gin-gonic.com) (**1.4**) | 89289.33 | **156.55** MB |
| `node` (`12.7`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 87913.67 | **131.52** MB |
| `go` (`1.12`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 86102.67 | **114.97** MB |
| `go` (`1.12`) | [kami](https://github.com/guregu/kami) (**2.2**) | 85594.00 | **113.84** MB |
| `go` (`1.12`) | [beego](https://beego.me) (**1.12**) | 84779.00 | **113.85** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 76701.67 | **119.38** MB |
| `go` (`1.12`) | [gf](https://goframe.org) (**1.8**) | 73825.33 | **111.79** MB |
| `node` (`12.7`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 71712.33 | **107.41** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 69337.33 | **148.69** MB |
| `node` (`12.7`) | [rayo](https://rayo.js.org) (**1.3**) | 68606.00 | **102.77** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 61321.67 | **143.61** MB |
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 61218.67 | **57.52** MB |
| `node` (`12.7`) | [fastify](https://fastify.io) (**2.7**) | 58939.33 | **150.60** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 58450.00 | **102.29** MB |
| `node` (`12.7`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 57957.33 | **86.70** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 54261.33 | **40.63** MB |
| `node` (`12.7`) | [foxify](https://foxify.js.org) (**0.1**) | 54000.00 | **113.24** MB |
| `node` (`12.7`) | [koa](https://koajs.com) (**2.7**) | 50927.67 | **107.51** MB |
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 50233.33 | **107.04** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 48899.00 | **120.20** MB |
| `node` (`12.7`) | [restify](https://restify.com) (**8.4**) | 45800.67 | **80.15** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 43592.67 | **93.65** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 42985.33 | **212.98** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.0**) | 42926.00 | **212.73** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 42580.67 | **211.26** MB |
| `node` (`12.7`) | [express](https://expressjs.com) (**4.17**) | 41963.00 | **102.49** MB |
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 41462.33 | **68.85** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 40581.67 | **201.21** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.22**) | 40399.33 | **38.47** MB |
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 39318.00 | **72.93** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 38148.00 | **198.06** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.5**) | 37589.00 | **93.02** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 36484.00 | **42.99** MB |
| `crystal` (`0.28`) | [lucky](https://luckyframework.org) (**0.14**) | 35659.67 | **43.94** MB |
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 33487.33 | **85.81** MB |
| `node` (`12.7`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 31179.67 | **29.27** MB |
| `node` (`12.7`) | [moleculer](https://moleculer.services) (**0.13**) | 30686.33 | **52.38** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 30037.33 | **17.31** MB |
| `node` (`12.7`) | [hapi](https://hapijs.com) (**18.1**) | 29807.33 | **77.10** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 29317.00 | **153.12** MB |
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 29097.67 | **54.85** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.33**) | 28302.33 | **60.95** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 25877.33 | **67.86** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.5**) | 23608.00 | **53.40** MB |
| `rust` (`nightly`) | [rocket](https://rocket.rs) (**0.4**) | 21502.67 | **33.61** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 20918.00 | **12.07** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 20074.67 | **37.28** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 19657.00 | **48.31** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 18553.00 | **140.28** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 18054.67 | **34.82** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 15235.67 | **39.51** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 15114.33 | **26.92** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 12748.00 | **25.63** MB |
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 11260.67 | **14.40** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.9**) | 10994.00 | **21.90** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 9011.33 | **19.61** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 7917.67 | **22.91** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 7332.33 | **21.62** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 6443.00 | **15.84** MB |
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 5907.00 | **7.38** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**5.2**) | 3943.33 | **12.08** MB |
| `python` (`3.7`) | [cyclone](https://cyclone.io) (**0.0**) | 2962.33 | **8.05** MB |
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

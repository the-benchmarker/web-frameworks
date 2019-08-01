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
Last update: 2019-08-01
```
OS: Linux (version: 5.1.20-300.fc30.x86_64, arch: x86_64)
CPU Cores: 4
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: cuba (ruby)


:three: rack-routing (ruby)


:four: roda (ruby)


:five: iron (rust)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `rust` (`1.36`) | [nickel](https://nickel-org.github.io) (**0.11**) | 0.04 ms | **0.04** ms | 0.05 ms | 0.06 ms | 3.96 ms | **35.00** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 1.88 ms | **0.14** ms | 0.97 ms | 37.19 ms | 128.22 ms | **7056.00** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 2.23 ms | **0.19** ms | 2.89 ms | 39.94 ms | 144.25 ms | **7700.67** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.22**) | 1.75 ms | **0.22** ms | 1.06 ms | 34.72 ms | 152.21 ms | **6642.67** | 
| `rust` (`1.36`) | [iron](https://ironframework.io) (**0.6**) | 0.32 ms | **0.27** ms | 0.52 ms | 1.13 ms | 41.00 ms | **485.67** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 3.50 ms | **0.29** ms | 12.43 ms | 47.84 ms | 155.25 ms | **9783.33** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 3.13 ms | **0.35** ms | 5.10 ms | 52.20 ms | 173.29 ms | **9977.67** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 4.25 ms | **0.38** ms | 14.93 ms | 55.02 ms | 155.17 ms | **11190.67** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**5.2**) | 16.38 ms | **0.92** ms | 58.89 ms | 154.13 ms | 428.14 ms | **33427.33** | 
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 6.85 ms | **0.94** ms | 14.58 ms | 15.16 ms | 2589.63 ms | **56557.67** | 
| `rust` (`nightly`) | [rocket](https://rocket.rs) (**0.4**) | 144.21 ms | **2.77** ms | 6.81 ms | 4281.23 ms | 6595.27 ms | **699658.33** | 
| `node` (`12.7`) | [nanoexpress](https://nanoexpress.js.org) (**0.12**) | 4.51 ms | **3.68** ms | 8.36 ms | 15.42 ms | 34.29 ms | **3183.67** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 4.44 ms | **3.93** ms | 8.95 ms | 16.73 ms | 41.98 ms | **3599.67** | 
| `node` (`12.7`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 5.33 ms | **4.04** ms | 10.66 ms | 20.71 ms | 73.00 ms | **4486.67** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 5.27 ms | **4.52** ms | 9.94 ms | 18.46 ms | 58.56 ms | **3978.67** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 12.24 ms | **4.81** ms | 41.54 ms | 86.50 ms | 178.26 ms | **20537.00** | 
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 5.68 ms | **4.82** ms | 9.78 ms | 16.98 ms | 119.86 ms | **3687.00** | 
| `go` (`1.12`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 5.91 ms | **5.38** ms | 8.90 ms | 14.92 ms | 149.79 ms | **3602.33** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 28.50 ms | **5.93** ms | 13.79 ms | 735.61 ms | 2122.22 ms | **145361.33** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 7.09 ms | **5.96** ms | 11.97 ms | 23.74 ms | 194.91 ms | **5735.33** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 6.35 ms | **6.00** ms | 10.28 ms | 16.68 ms | 78.34 ms | **3440.67** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.9**) | 10.38 ms | **6.20** ms | 15.95 ms | 94.10 ms | 122.91 ms | **15772.67** | 
| `rust` (`1.36`) | [gotham](https://gotham.rs) (**0.4**) | 7.21 ms | **6.33** ms | 12.12 ms | 20.22 ms | 227.96 ms | **6802.00** | 
| `python` (`3.6`) | [vibora](https://vibora.io) (**0.0**) | 7.15 ms | **6.50** ms | 13.08 ms | 20.62 ms | 52.62 ms | **4637.67** | 
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 8.21 ms | **7.48** ms | 13.12 ms | 21.15 ms | 52.83 ms | **3903.00** | 
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.25**) | 8.48 ms | **7.55** ms | 14.38 ms | 22.22 ms | 38.56 ms | **4172.67** | 
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 9.66 ms | **7.72** ms | 14.61 ms | 61.75 ms | 192.13 ms | **11607.00** | 
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.4**) | 8.65 ms | **7.80** ms | 14.20 ms | 22.89 ms | 51.93 ms | **4359.00** | 
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 8.70 ms | **7.83** ms | 13.56 ms | 21.47 ms | 55.39 ms | **3847.67** | 
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 261.69 ms | **8.44** ms | 994.79 ms | 2673.43 ms | 3011.87 ms | **587368.00** | 
| `rust` (`1.36`) | [actix-web](https://actix.rs) (**1.0**) | 9.11 ms | **8.49** ms | 14.45 ms | 22.18 ms | 103.61 ms | **5188.67** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 215.84 ms | **8.83** ms | 138.45 ms | 4946.90 ms | 7924.46 ms | **855389.33** | 
| `node` (`12.7`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 14.79 ms | **8.96** ms | 17.12 ms | 187.89 ms | 784.65 ms | **40649.33** | 
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.28**) | 12.48 ms | **9.02** ms | 17.30 ms | 104.73 ms | 254.67 ms | **18251.33** | 
| `node` (`12.7`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 18.99 ms | **9.07** ms | 17.93 ms | 303.20 ms | 1010.62 ms | **62062.67** | 
| `go` (`1.12`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.0**) | 12.68 ms | **9.24** ms | 22.02 ms | 92.49 ms | 288.71 ms | **16814.00** | 
| `go` (`1.12`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 13.44 ms | **9.40** ms | 24.13 ms | 104.37 ms | 353.30 ms | **19378.67** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 11.95 ms | **9.49** ms | 15.67 ms | 57.85 ms | 580.98 ms | **22187.67** | 
| `go` (`1.12`) | [violetear](https://violetear.org) (**7.0**) | 12.79 ms | **9.97** ms | 21.65 ms | 78.62 ms | 336.14 ms | **15191.00** | 
| `go` (`1.12`) | [gin](https://gin-gonic.com) (**1.4**) | 15.31 ms | **10.07** ms | 28.96 ms | 121.42 ms | 377.14 ms | **22415.33** | 
| `go` (`1.12`) | [kami](https://github.com/guregu/kami) (**2.2**) | 17.75 ms | **10.20** ms | 33.66 ms | 174.63 ms | 394.25 ms | **31327.67** | 
| `go` (`1.12`) | [beego](https://beego.me) (**1.12**) | 15.77 ms | **10.28** ms | 28.28 ms | 132.66 ms | 454.86 ms | **25169.67** | 
| `go` (`1.12`) | [echo](https://echo.labstack.com) (**4.1**) | 12.86 ms | **10.45** ms | 23.69 ms | 57.11 ms | 169.52 ms | **10936.33** | 
| `go` (`1.12`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 16.06 ms | **10.46** ms | 33.01 ms | 118.90 ms | 349.49 ms | **23276.67** | 
| `node` (`12.7`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 18.00 ms | **10.50** ms | 20.25 ms | 253.02 ms | 868.83 ms | **49702.67** | 
| `go` (`1.12`) | [gf](https://goframe.org) (**1.8**) | 18.89 ms | **12.27** ms | 43.37 ms | 125.30 ms | 291.37 ms | **24538.67** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**3.12**) | 61.15 ms | **12.36** ms | 184.87 ms | 575.34 ms | 4754.56 ms | **185802.33** | 
| `node` (`12.7`) | [rayo](https://rayo.js.org) (**1.3**) | 24.30 ms | **12.37** ms | 24.25 ms | 426.49 ms | 1236.69 ms | **75633.67** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 67.44 ms | **12.53** ms | 210.03 ms | 760.11 ms | 5350.30 ms | **195079.00** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 71.80 ms | **12.59** ms | 203.51 ms | 871.03 ms | 6006.50 ms | **250868.33** | 
| `node` (`12.7`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 27.52 ms | **12.88** ms | 25.52 ms | 497.38 ms | 1318.21 ms | **86921.33** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 71.06 ms | **13.18** ms | 225.17 ms | 744.19 ms | 3975.33 ms | **212668.67** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 17.81 ms | **13.27** ms | 32.53 ms | 54.58 ms | 238.13 ms | **15147.33** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 72.20 ms | **13.98** ms | 221.53 ms | 790.33 ms | 5326.31 ms | **222787.33** | 
| `node` (`12.7`) | [foxify](https://foxify.js.org) (**0.1**) | 28.16 ms | **14.28** ms | 36.85 ms | 415.96 ms | 1365.98 ms | **76084.67** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 15.57 ms | **14.32** ms | 30.25 ms | 55.86 ms | 335.21 ms | **13628.00** | 
| `node` (`12.7`) | [fastify](https://fastify.io) (**2.7**) | 40.60 ms | **14.85** ms | 35.38 ms | 820.39 ms | 1868.70 ms | **137154.00** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 22.75 ms | **15.32** ms | 37.63 ms | 83.47 ms | 815.84 ms | **32030.00** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 57.22 ms | **15.84** ms | 30.82 ms | 1365.41 ms | 4841.37 ms | **265261.67** | 
| `node` (`12.7`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 41.89 ms | **16.32** ms | 34.26 ms | 820.95 ms | 1672.56 ms | **134378.33** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 99.47 ms | **16.34** ms | 326.91 ms | 1007.60 ms | 4945.70 ms | **255418.33** | 
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 16.12 ms | **16.59** ms | 17.52 ms | 19.61 ms | 160.53 ms | **4525.00** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 23.69 ms | **17.32** ms | 42.26 ms | 113.50 ms | 332.92 ms | **20776.33** | 
| `node` (`12.7`) | [koa](https://koajs.com) (**2.7**) | 47.54 ms | **18.05** ms | 37.18 ms | 969.31 ms | 1980.98 ms | **156703.33** | 
| `node` (`12.7`) | [restify](https://restify.com) (**8.4**) | 26.88 ms | **18.86** ms | 35.84 ms | 218.11 ms | 1004.46 ms | **49359.33** | 
| `node` (`12.7`) | [express](https://expressjs.com) (**4.17**) | 47.79 ms | **19.16** ms | 40.73 ms | 893.23 ms | 1885.20 ms | **144989.00** | 
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 20.40 ms | **20.26** ms | 34.04 ms | 55.62 ms | 121.82 ms | **11336.33** | 
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 117.03 ms | **20.82** ms | 60.36 ms | 2500.49 ms | 4709.56 ms | **430349.67** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 23.48 ms | **21.39** ms | 33.70 ms | 45.90 ms | 329.80 ms | **11130.33** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.5**) | 27.30 ms | **21.57** ms | 39.12 ms | 91.41 ms | 427.61 ms | **19008.33** | 
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 27.86 ms | **25.39** ms | 30.75 ms | 104.13 ms | 997.75 ms | **38116.00** | 
| `crystal` (`0.28`) | [lucky](https://luckyframework.org) (**0.14**) | 27.68 ms | **26.51** ms | 33.20 ms | 36.59 ms | 318.20 ms | **7621.67** | 
| `node` (`12.7`) | [moleculer](https://moleculer.services) (**0.13**) | 73.24 ms | **28.38** ms | 53.65 ms | 1425.82 ms | 2771.89 ms | **233676.33** | 
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 29.50 ms | **29.21** ms | 37.21 ms | 45.30 ms | 63.88 ms | **5993.33** | 
| `node` (`12.7`) | [hapi](https://hapijs.com) (**18.1**) | 99.68 ms | **30.52** ms | 51.67 ms | 1906.84 ms | 3430.76 ms | **321175.67** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 41.88 ms | **30.53** ms | 76.12 ms | 134.09 ms | 290.88 ms | **25688.00** | 
| `node` (`12.7`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 30.92 ms | **30.87** ms | 33.07 ms | 35.40 ms | 284.54 ms | **6768.33** | 
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 35.60 ms | **31.20** ms | 49.99 ms | 138.47 ms | 977.35 ms | **39779.67** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.33**) | 35.79 ms | **34.58** ms | 50.80 ms | 60.93 ms | 585.30 ms | **22823.00** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.5**) | 41.76 ms | **39.22** ms | 61.83 ms | 75.67 ms | 371.48 ms | **20457.67** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 51.59 ms | **44.60** ms | 78.02 ms | 119.78 ms | 777.52 ms | **32234.33** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 51.83 ms | **47.69** ms | 66.00 ms | 134.05 ms | 787.36 ms | **34791.33** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 67.64 ms | **59.72** ms | 117.48 ms | 170.16 ms | 514.50 ms | **40069.33** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 68.30 ms | **61.91** ms | 105.67 ms | 176.23 ms | 412.88 ms | **33661.67** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.9**) | 91.88 ms | **71.85** ms | 170.89 ms | 208.42 ms | 402.44 ms | **54297.33** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 119.64 ms | **78.68** ms | 152.77 ms | 1589.46 ms | 2638.37 ms | **237230.33** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 126.18 ms | **119.38** ms | 184.68 ms | 269.95 ms | 456.51 ms | **46784.00** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 149.38 ms | **121.52** ms | 153.39 ms | 1030.47 ms | 2242.74 ms | **173056.00** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 191.58 ms | **135.10** ms | 168.88 ms | 2418.32 ms | 4214.75 ms | **374949.00** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 209.73 ms | **150.34** ms | 173.76 ms | 2345.34 ms | 3778.85 ms | **365346.33** | 
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 301.36 ms | **165.11** ms | 180.44 ms | 3945.45 ms | 6680.68 ms | **685534.33** | 
| `python` (`3.7`) | [cyclone](https://cyclone.io) (**0.0**) | 289.04 ms | **203.28** ms | 257.54 ms | 3781.68 ms | 6646.29 ms | **604742.00** | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (nanoexpress) (node)


:three: (japronto) (python)


:four: (drogon) (cpp)


:five: (rapidoid) (java)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 206401.67 | **119.37** MB |
| `node` (`12.7`) | [nanoexpress](https://nanoexpress.js.org) (**0.12**) | 191206.33 | **168.15** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 162242.00 | **194.24** MB |
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 162237.33 | **157.29** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 158476.67 | **285.36** MB |
| `node` (`12.7`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 155658.33 | **136.93** MB |
| `go` (`1.12`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 152167.67 | **245.14** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 149690.67 | **388.40** MB |
| `python` (`3.6`) | [vibora](https://vibora.io) (**0.0**) | 144839.00 | **164.44** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 143832.33 | **139.54** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 139708.33 | **240.69** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.9**) | 135087.33 | **78.18** MB |
| `rust` (`1.36`) | [gotham](https://gotham.rs) (**0.4**) | 133216.67 | **272.66** MB |
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 118191.33 | **237.59** MB |
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 116415.33 | **109.52** MB |
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.25**) | 116184.67 | **189.70** MB |
| `rust` (`1.36`) | [actix-web](https://actix.rs) (**1.0**) | 114911.00 | **165.02** MB |
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 111775.67 | **105.10** MB |
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.4**) | 111425.67 | **118.55** MB |
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 102600.33 | **167.75** MB |
| `rust` (`1.36`) | [nickel](https://nickel-org.github.io) (**0.11**) | 99301.67 | **197.60** MB |
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.28**) | 95869.67 | **175.59** MB |
| `go` (`1.12`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.0**) | 94985.33 | **126.59** MB |
| `rust` (`1.36`) | [iron](https://ironframework.io) (**0.6**) | 94468.67 | **119.29** MB |
| `node` (`12.7`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 92225.67 | **138.19** MB |
| `go` (`1.12`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 92163.67 | **123.04** MB |
| `node` (`12.7`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 91162.67 | **136.47** MB |
| `go` (`1.12`) | [violetear](https://violetear.org) (**7.0**) | 90989.67 | **120.88** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 90217.00 | **146.56** MB |
| `go` (`1.12`) | [gin](https://gin-gonic.com) (**1.4**) | 88953.33 | **155.81** MB |
| `go` (`1.12`) | [beego](https://beego.me) (**1.12**) | 87491.67 | **117.45** MB |
| `go` (`1.12`) | [kami](https://github.com/guregu/kami) (**2.2**) | 87269.00 | **116.21** MB |
| `go` (`1.12`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 86111.00 | **114.72** MB |
| `go` (`1.12`) | [echo](https://echo.labstack.com) (**4.1**) | 83263.00 | **146.17** MB |
| `node` (`12.7`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 80810.33 | **120.85** MB |
| `go` (`1.12`) | [gf](https://goframe.org) (**1.8**) | 74752.67 | **113.31** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 70228.00 | **150.75** MB |
| `node` (`12.7`) | [rayo](https://rayo.js.org) (**1.3**) | 68931.00 | **103.23** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 67074.00 | **104.53** MB |
| `node` (`12.7`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 65874.67 | **98.70** MB |
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 60714.00 | **56.98** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 59631.33 | **139.60** MB |
| `node` (`12.7`) | [fastify](https://fastify.io) (**2.7**) | 58380.00 | **150.33** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 57037.67 | **99.80** MB |
| `node` (`12.7`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 55402.33 | **82.99** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 54546.33 | **40.58** MB |
| `node` (`12.7`) | [foxify](https://foxify.js.org) (**0.1**) | 54484.33 | **114.20** MB |
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 49676.33 | **105.80** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 46987.67 | **115.70** MB |
| `node` (`12.7`) | [koa](https://koajs.com) (**2.7**) | 46466.67 | **97.98** MB |
| `node` (`12.7`) | [restify](https://restify.com) (**8.4**) | 44709.33 | **78.28** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**3.12**) | 43751.33 | **216.83** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 42395.67 | **91.10** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 42374.33 | **210.27** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 42207.33 | **209.14** MB |
| `node` (`12.7`) | [express](https://expressjs.com) (**4.17**) | 42034.33 | **102.72** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 39864.67 | **197.39** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.22**) | 39831.00 | **37.96** MB |
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 39699.33 | **66.11** MB |
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 39660.00 | **73.56** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.5**) | 38065.67 | **94.25** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 37611.67 | **195.30** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 36313.67 | **42.80** MB |
| `crystal` (`0.28`) | [lucky](https://luckyframework.org) (**0.14**) | 35800.33 | **44.14** MB |
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 33729.00 | **86.46** MB |
| `node` (`12.7`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 31614.67 | **29.66** MB |
| `node` (`12.7`) | [moleculer](https://moleculer.services) (**0.13**) | 30592.33 | **52.25** MB |
| `node` (`12.7`) | [hapi](https://hapijs.com) (**18.1**) | 29849.67 | **77.23** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 29719.67 | **17.14** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 29386.00 | **153.45** MB |
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 29027.33 | **54.73** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.33**) | 27776.33 | **59.79** MB |
| `rust` (`nightly`) | [rocket](https://rocket.rs) (**0.4**) | 25131.00 | **38.23** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 24712.33 | **64.87** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.5**) | 23672.67 | **53.54** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 20875.67 | **12.04** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 19912.33 | **36.98** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 19432.00 | **47.78** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 18292.33 | **138.37** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 15063.67 | **26.84** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 15024.33 | **38.98** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 14989.00 | **29.01** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 11570.67 | **23.31** MB |
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 11033.67 | **14.12** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.9**) | 10903.67 | **21.72** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 7785.33 | **16.98** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 7725.33 | **22.37** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 7039.33 | **20.69** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 6294.33 | **15.48** MB |
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 5799.67 | **7.24** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**5.2**) | 3895.67 | **11.94** MB |
| `python` (`3.7`) | [cyclone](https://cyclone.io) (**0.0**) | 2828.67 | **7.67** MB |
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

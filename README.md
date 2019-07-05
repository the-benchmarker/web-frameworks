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
Last update: 2019-07-05
```
OS: Linux (version: 5.1.15-300.fc30.x86_64, arch: x86_64)
CPU Cores: 8
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: roda (ruby)


:three: cuba (ruby)


:four: rack-routing (ruby)


:five: iron (rust)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `rust` (`1.35`) | [nickel](https://nickel-org.github.io) (**0.11**) | 0.09 ms | **0.08** ms | 0.13 ms | 0.18 ms | 2.04 ms | **35.00** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.21**) | 5.16 ms | **0.31** ms | 18.42 ms | 41.17 ms | 101.10 ms | **9457.67** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 5.66 ms | **0.34** ms | 19.73 ms | 42.53 ms | 104.08 ms | **9948.67** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 6.58 ms | **0.37** ms | 23.03 ms | 48.09 ms | 118.20 ms | **11414.33** | 
| `rust` (`1.35`) | [iron](https://ironframework.io) (**0.6**) | 0.53 ms | **0.50** ms | 0.90 ms | 1.41 ms | 11.81 ms | **319.33** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 165.43 ms | **0.55** ms | 387.68 ms | 3168.42 ms | 7265.31 ms | **564056.00** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 194.26 ms | **0.56** ms | 308.61 ms | 4714.67 ms | 7574.04 ms | **751311.67** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 188.41 ms | **0.57** ms | 355.52 ms | 4217.61 ms | 6993.85 ms | **695797.67** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 202.82 ms | **0.57** ms | 347.04 ms | 4652.17 ms | 7907.95 ms | **761595.33** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**3.12**) | 199.10 ms | **0.58** ms | 328.50 ms | 4707.63 ms | 7577.78 ms | **759422.00** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 9.24 ms | **0.61** ms | 29.42 ms | 58.63 ms | 131.80 ms | **14080.33** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 170.30 ms | **0.62** ms | 354.39 ms | 3377.08 ms | 7885.14 ms | **622160.00** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 10.66 ms | **0.65** ms | 34.43 ms | 68.86 ms | 165.23 ms | **16680.33** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 11.62 ms | **0.86** ms | 35.10 ms | 68.75 ms | 156.11 ms | **16788.67** | 
| `rust` (`nightly`) | [rocket](https://rocket.rs) (**0.4**) | 128.19 ms | **2.17** ms | 44.79 ms | 3364.56 ms | 6594.53 ms | **617098.33** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 3.29 ms | **2.29** ms | 6.96 ms | 14.13 ms | 60.58 ms | **3084.33** | 
| `rust` (`1.35`) | [actix-web](https://actix.rs) (**0.7**) | 4.03 ms | **3.05** ms | 8.24 ms | 17.95 ms | 69.93 ms | **3667.33** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 4.12 ms | **3.22** ms | 8.96 ms | 18.31 ms | 38.67 ms | **3962.33** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 7.03 ms | **3.29** ms | 9.32 ms | 41.31 ms | 864.05 ms | **32273.67** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 4.49 ms | **3.97** ms | 8.53 ms | 17.39 ms | 110.36 ms | **3564.33** | 
| `python` (`3.6`) | [vibora](https://vibora.io) (**0.0**) | 5.28 ms | **4.20** ms | 11.01 ms | 19.28 ms | 44.40 ms | **4227.67** | 
| `go` (`1.12`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 4.76 ms | **4.57** ms | 7.31 ms | 15.06 ms | 106.98 ms | **3264.67** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 4.45 ms | **4.79** ms | 6.38 ms | 11.13 ms | 29.38 ms | **2200.33** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.8**) | 7.45 ms | **4.83** ms | 12.80 ms | 72.72 ms | 123.38 ms | **11062.67** | 
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 5.27 ms | **5.04** ms | 8.59 ms | 15.10 ms | 34.59 ms | **2810.00** | 
| `rust` (`1.35`) | [gotham](https://gotham.rs) (**0.3**) | 5.65 ms | **5.09** ms | 10.18 ms | 20.30 ms | 166.85 ms | **4975.00** | 
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.4**) | 6.04 ms | **5.28** ms | 10.47 ms | 19.22 ms | 43.00 ms | **3597.00** | 
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 5.63 ms | **5.41** ms | 9.34 ms | 15.90 ms | 41.40 ms | **3031.67** | 
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 5.96 ms | **5.45** ms | 9.65 ms | 15.72 ms | 41.64 ms | **2833.67** | 
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.25**) | 6.06 ms | **5.48** ms | 9.83 ms | 15.82 ms | 42.94 ms | **2881.00** | 
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.28**) | 6.66 ms | **5.79** ms | 10.43 ms | 16.53 ms | 44.20 ms | **2993.67** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**5.2**) | 43.73 ms | **6.14** ms | 142.27 ms | 354.63 ms | 869.87 ms | **77043.00** | 
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 7.35 ms | **6.49** ms | 11.15 ms | 18.03 ms | 51.14 ms | **3350.00** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 7.96 ms | **6.84** ms | 10.69 ms | 22.04 ms | 366.39 ms | **12245.00** | 
| `go` (`1.12`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.0**) | 9.91 ms | **7.09** ms | 20.31 ms | 45.29 ms | 187.41 ms | **9163.67** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 7.79 ms | **7.17** ms | 12.60 ms | 23.63 ms | 180.33 ms | **5867.00** | 
| `go` (`1.12`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 9.57 ms | **7.20** ms | 18.70 ms | 40.83 ms | 193.96 ms | **8425.33** | 
| `node` (`12.5`) | [0http](https://github.com/jkyberneees/0http) (**1.0**) | 9.21 ms | **7.62** ms | 13.66 ms | 29.87 ms | 409.45 ms | **15225.00** | 
| `go` (`1.12`) | [echo](https://echo.labstack.com) (**4.1**) | 10.42 ms | **7.63** ms | 21.40 ms | 45.80 ms | 182.00 ms | **9298.33** | 
| `go` (`1.12`) | [gin](https://gin-gonic.com) (**1.4**) | 10.51 ms | **7.66** ms | 21.39 ms | 46.42 ms | 243.26 ms | **9963.67** | 
| `go` (`1.12`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 11.08 ms | **7.68** ms | 23.05 ms | 50.55 ms | 416.97 ms | **13228.67** | 
| `go` (`1.12`) | [beego](https://beego.me) (**1.12**) | 10.36 ms | **7.90** ms | 20.21 ms | 44.66 ms | 226.78 ms | **8938.33** | 
| `go` (`1.12`) | [kami](https://github.com/guregu/kami) (**2.2**) | 9.65 ms | **7.99** ms | 16.95 ms | 38.15 ms | 86.57 ms | **6911.33** | 
| `go` (`1.12`) | [violetear](https://violetear.org) (**7.0**) | 10.36 ms | **8.41** ms | 18.02 ms | 41.03 ms | 255.11 ms | **10133.67** | 
| `node` (`12.5`) | [restana](https://github.com/jkyberneees/ana) (**3.1**) | 10.79 ms | **8.55** ms | 14.65 ms | 70.55 ms | 522.71 ms | **22236.33** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 10.02 ms | **8.76** ms | 19.64 ms | 39.00 ms | 292.69 ms | **9303.67** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 127.54 ms | **8.87** ms | 26.95 ms | 3107.91 ms | 6219.80 ms | **545635.00** | 
| `node` (`12.5`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 12.37 ms | **9.52** ms | 16.44 ms | 73.71 ms | 534.85 ms | **23508.33** | 
| `node` (`12.5`) | [rayo](https://rayo.js.org) (**1.3**) | 15.08 ms | **9.89** ms | 17.60 ms | 164.69 ms | 789.43 ms | **40534.00** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 12.52 ms | **9.90** ms | 22.09 ms | 39.59 ms | 285.39 ms | **9813.00** | 
| `go` (`1.12`) | [gf](https://goframe.org) (**1.6**) | 13.95 ms | **9.94** ms | 28.21 ms | 62.93 ms | 237.73 ms | **14087.00** | 
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 11.16 ms | **11.08** ms | 14.12 ms | 16.49 ms | 153.08 ms | **3152.67** | 
| `node` (`12.5`) | [foxify](https://foxify.js.org) (**0.1**) | 15.55 ms | **12.37** ms | 21.58 ms | 59.66 ms | 549.28 ms | **21993.67** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 15.15 ms | **12.37** ms | 26.81 ms | 47.76 ms | 218.35 ms | **10066.00** | 
| `node` (`12.5`) | [fastify](https://fastify.io) (**2.4**) | 20.91 ms | **14.41** ms | 25.74 ms | 215.09 ms | 905.92 ms | **46643.00** | 
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 21.19 ms | **14.82** ms | 25.05 ms | 190.14 ms | 1084.53 ms | **51934.67** | 
| `node` (`12.5`) | [koa](https://koajs.com) (**2.7**) | 21.94 ms | **15.27** ms | 26.93 ms | 216.04 ms | 973.75 ms | **48536.67** | 
| `node` (`12.5`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 24.61 ms | **15.43** ms | 27.30 ms | 312.22 ms | 994.51 ms | **58374.00** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 18.22 ms | **15.80** ms | 32.54 ms | 57.81 ms | 453.13 ms | **18173.00** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 18.03 ms | **15.83** ms | 32.76 ms | 53.62 ms | 111.54 ms | **10852.33** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 19.50 ms | **16.24** ms | 27.13 ms | 72.81 ms | 701.76 ms | **21126.33** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.5**) | 20.11 ms | **16.54** ms | 32.20 ms | 54.69 ms | 310.28 ms | **11762.67** | 
| `node` (`12.5`) | [restify](https://restify.com) (**8.2**) | 21.10 ms | **17.36** ms | 29.31 ms | 78.47 ms | 614.22 ms | **26049.33** | 
| `node` (`12.5`) | [express](https://expressjs.com) (**4.16**) | 23.92 ms | **18.67** ms | 29.79 ms | 173.36 ms | 776.72 ms | **36684.00** | 
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 34.51 ms | **25.69** ms | 39.27 ms | 335.79 ms | 1702.76 ms | **69072.33** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.5**) | 30.01 ms | **26.76** ms | 52.24 ms | 72.28 ms | 108.07 ms | **15856.00** | 
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 38.38 ms | **27.15** ms | 49.50 ms | 393.98 ms | 1392.19 ms | **76994.33** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.31**) | 31.78 ms | **27.83** ms | 55.43 ms | 89.91 ms | 177.77 ms | **18135.67** | 
| `node` (`12.5`) | [hapi](https://hapijs.com) (**18.1**) | 67.37 ms | **31.68** ms | 50.47 ms | 1119.59 ms | 2165.95 ms | **182993.00** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 39.52 ms | **35.28** ms | 55.27 ms | 94.91 ms | 648.13 ms | **21086.33** | 
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 36.25 ms | **35.88** ms | 43.98 ms | 51.90 ms | 66.93 ms | **6168.33** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 43.15 ms | **38.21** ms | 68.48 ms | 93.17 ms | 271.08 ms | **18403.33** | 
| `crystal` (`0.28`) | [lucky](https://luckyframework.org) (**0.14**) | 45.05 ms | **41.85** ms | 53.71 ms | 92.39 ms | 498.54 ms | **19953.00** | 
| `node` (`12.5`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 50.07 ms | **45.31** ms | 54.29 ms | 194.86 ms | 884.95 ms | **42210.00** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.17**) | 50.74 ms | **46.44** ms | 82.84 ms | 115.91 ms | 225.70 ms | **23204.00** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 71.55 ms | **63.24** ms | 126.21 ms | 202.86 ms | 390.44 ms | **41528.33** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.9**) | 81.10 ms | **71.10** ms | 151.47 ms | 211.97 ms | 307.14 ms | **44798.00** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 105.64 ms | **94.84** ms | 170.70 ms | 238.57 ms | 905.00 ms | **48512.00** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 105.52 ms | **101.71** ms | 163.17 ms | 214.56 ms | 301.09 ms | **41966.00** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 108.57 ms | **109.18** ms | 134.12 ms | 154.16 ms | 604.47 ms | **27155.00** | 
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 115.22 ms | **112.41** ms | 158.94 ms | 206.23 ms | 321.81 ms | **33638.00** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 127.07 ms | **119.26** ms | 186.34 ms | 259.48 ms | 821.23 ms | **48921.00** | 
| `python` (`3.7`) | [cyclone](https://cyclone.io) (**0.0**) | 386.82 ms | **271.21** ms | 354.22 ms | 4774.87 ms | 7355.14 ms | **751068.33** | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (actix-web) (rust)


:four: (kore) (c)


:five: (rapidoid) (java)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 303445.00 | **175.52** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 255944.67 | **306.33** MB |
| `rust` (`1.35`) | [actix-web](https://actix.rs) (**0.7**) | 247085.67 | **280.93** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 223508.67 | **581.13** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 216631.00 | **390.13** MB |
| `python` (`3.6`) | [vibora](https://vibora.io) (**0.0**) | 205836.33 | **233.69** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 202228.33 | **196.36** MB |
| `go` (`1.12`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 196276.00 | **315.35** MB |
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 184830.33 | **371.63** MB |
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 177637.00 | **167.09** MB |
| `rust` (`1.35`) | [gotham](https://gotham.rs) (**0.3**) | 172988.00 | **354.49** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.8**) | 169724.00 | **98.18** MB |
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.4**) | 161655.67 | **172.56** MB |
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 157418.33 | **148.07** MB |
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.25**) | 155254.33 | **253.78** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 154859.33 | **267.33** MB |
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.28**) | 142276.67 | **260.56** MB |
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 130524.00 | **213.43** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 124028.33 | **202.09** MB |
| `node` (`12.5`) | [0http](https://github.com/jkyberneees/0http) (**1.0**) | 116965.67 | **175.30** MB |
| `rust` (`1.35`) | [iron](https://ironframework.io) (**0.6**) | 113863.00 | **143.43** MB |
| `go` (`1.12`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 111584.67 | **149.14** MB |
| `go` (`1.12`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.0**) | 111323.00 | **148.21** MB |
| `node` (`12.5`) | [restana](https://github.com/jkyberneees/ana) (**3.1**) | 108857.33 | **163.18** MB |
| `go` (`1.12`) | [kami](https://github.com/guregu/kami) (**2.2**) | 108488.00 | **144.14** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 105034.67 | **163.49** MB |
| `go` (`1.12`) | [echo](https://echo.labstack.com) (**4.1**) | 105006.33 | **184.17** MB |
| `go` (`1.12`) | [gin](https://gin-gonic.com) (**1.4**) | 104977.67 | **184.00** MB |
| `go` (`1.12`) | [beego](https://beego.me) (**1.12**) | 104224.67 | **140.48** MB |
| `go` (`1.12`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 103413.00 | **138.57** MB |
| `go` (`1.12`) | [violetear](https://violetear.org) (**7.0**) | 103001.67 | **136.15** MB |
| `rust` (`1.35`) | [nickel](https://nickel-org.github.io) (**0.11**) | 97712.00 | **194.13** MB |
| `node` (`12.5`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 93975.67 | **140.85** MB |
| `node` (`12.5`) | [rayo](https://rayo.js.org) (**1.3**) | 89891.00 | **134.72** MB |
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 86002.67 | **80.83** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 82079.33 | **192.48** MB |
| `go` (`1.12`) | [gf](https://goframe.org) (**1.6**) | 81836.00 | **124.02** MB |
| `node` (`12.5`) | [foxify](https://foxify.js.org) (**0.1**) | 70695.67 | **148.61** MB |
| `node` (`12.5`) | [fastify](https://fastify.io) (**2.4**) | 70680.33 | **173.04** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 68382.33 | **168.55** MB |
| `node` (`12.5`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 62866.00 | **94.08** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 62136.33 | **44.88** MB |
| `node` (`12.5`) | [koa](https://koajs.com) (**2.7**) | 60186.67 | **127.11** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 59324.33 | **103.97** MB |
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 58849.33 | **98.20** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 57481.00 | **124.08** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 56856.67 | **122.01** MB |
| `node` (`12.5`) | [restify](https://restify.com) (**8.2**) | 52822.33 | **92.59** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.5**) | 49729.00 | **123.29** MB |
| `node` (`12.5`) | [express](https://expressjs.com) (**4.16**) | 49589.67 | **121.29** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**3.12**) | 41308.00 | **204.65** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 40697.00 | **201.72** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 40441.00 | **200.88** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 39230.33 | **194.64** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 38066.00 | **197.76** MB |
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 35437.33 | **65.72** MB |
| `rust` (`nightly`) | [rocket](https://rocket.rs) (**0.4**) | 34828.00 | **56.41** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.5**) | 33944.33 | **76.99** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 33689.33 | **175.53** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.31**) | 33061.33 | **71.47** MB |
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 31532.33 | **59.41** MB |
| `node` (`12.5`) | [hapi](https://hapijs.com) (**18.1**) | 29530.33 | **76.33** MB |
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 27365.33 | **34.38** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 25780.00 | **47.94** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.21**) | 24766.33 | **23.62** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 22840.00 | **56.24** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 22511.67 | **26.55** MB |
| `crystal` (`0.28`) | [lucky](https://luckyframework.org) (**0.14**) | 22158.33 | **27.24** MB |
| `node` (`12.5`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 20905.33 | **19.61** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.17**) | 19923.00 | **38.50** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 19395.33 | **11.20** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 14305.00 | **25.51** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 13814.67 | **7.98** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.9**) | 12594.00 | **25.08** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 12035.00 | **90.97** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 11017.00 | **28.60** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 9402.00 | **20.48** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 9308.00 | **27.03** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 8857.00 | **26.11** MB |
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 8614.00 | **22.17** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 7745.67 | **19.07** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**5.2**) | 2914.00 | **8.93** MB |
| `python` (`3.7`) | [cyclone](https://cyclone.io) (**0.0**) | 1871.33 | **5.08** MB |
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

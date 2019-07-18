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
Last update: 2019-07-18
```
OS: Linux (version: 5.1.17-300.fc30.x86_64, arch: x86_64)
CPU Cores: 4
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: rack-routing (ruby)


:three: cuba (ruby)


:four: flame (ruby)


:five: iron (rust)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `rust` (`1.36`) | [nickel](https://nickel-org.github.io) (**0.11**) | 0.05 ms | **0.05** ms | 0.05 ms | 0.06 ms | 1.62 ms | **14.67** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 2.25 ms | **0.19** ms | 3.98 ms | 38.64 ms | 142.07 ms | **7506.33** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 1.88 ms | **0.20** ms | 1.76 ms | 35.07 ms | 150.79 ms | **6707.33** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 3.17 ms | **0.26** ms | 9.88 ms | 46.93 ms | 148.02 ms | **9228.00** | 
| `rust` (`1.36`) | [iron](https://ironframework.io) (**0.6**) | 0.31 ms | **0.26** ms | 0.48 ms | 1.08 ms | 43.75 ms | **505.67** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 4.41 ms | **0.34** ms | 16.34 ms | 51.28 ms | 138.48 ms | **10633.33** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.22**) | 1.74 ms | **0.43** ms | 1.44 ms | 31.78 ms | 158.08 ms | **6287.67** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 3.56 ms | **0.51** ms | 10.08 ms | 46.34 ms | 171.23 ms | **9268.00** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**5.2**) | 17.89 ms | **1.00** ms | 63.06 ms | 165.21 ms | 472.53 ms | **35723.67** | 
| `rust` (`nightly`) | [rocket](https://rocket.rs) (**0.4**) | 67.45 ms | **2.79** ms | 5.58 ms | 2143.67 ms | 6594.23 ms | **417565.67** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 4.57 ms | **3.58** ms | 9.78 ms | 17.95 ms | 36.15 ms | **3973.67** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 5.42 ms | **4.62** ms | 10.67 ms | 19.50 ms | 47.91 ms | **4236.00** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 7.11 ms | **5.29** ms | 10.48 ms | 24.42 ms | 302.68 ms | **14762.67** | 
| `go` (`1.12`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 5.96 ms | **5.47** ms | 9.05 ms | 15.26 ms | 104.47 ms | **3542.67** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 23.52 ms | **5.56** ms | 12.38 ms | 599.49 ms | 1676.14 ms | **115753.33** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.8**) | 8.63 ms | **6.03** ms | 13.93 ms | 65.42 ms | 92.22 ms | **10686.67** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 6.41 ms | **6.05** ms | 9.80 ms | 15.63 ms | 117.77 ms | **3561.67** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 7.22 ms | **6.08** ms | 11.86 ms | 27.22 ms | 152.00 ms | **5771.00** | 
| `rust` (`1.36`) | [gotham](https://gotham.rs) (**0.4**) | 7.18 ms | **6.30** ms | 12.10 ms | 19.58 ms | 99.77 ms | **4597.33** | 
| `python` (`3.6`) | [vibora](https://vibora.io) (**0.0**) | 7.37 ms | **6.92** ms | 13.40 ms | 21.79 ms | 52.06 ms | **4865.67** | 
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.4**) | 7.86 ms | **7.06** ms | 13.05 ms | 20.77 ms | 41.64 ms | **3908.67** | 
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 7.93 ms | **7.12** ms | 13.29 ms | 20.75 ms | 37.29 ms | **3901.00** | 
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.25**) | 40.90 ms | **7.69** ms | 17.94 ms | 792.48 ms | 947.25 ms | **137926.33** | 
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 8.45 ms | **7.96** ms | 12.37 ms | 19.65 ms | 115.61 ms | **3551.00** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 230.63 ms | **8.10** ms | 130.74 ms | 5323.58 ms | 7929.98 ms | **905324.00** | 
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 8.74 ms | **8.19** ms | 13.35 ms | 19.77 ms | 52.29 ms | **3598.33** | 
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 513.33 ms | **8.20** ms | 2272.77 ms | 4133.82 ms | 4471.71 ms | **1048698.00** | 
| `rust` (`1.36`) | [actix-web](https://actix.rs) (**1.0**) | 9.15 ms | **8.67** ms | 14.25 ms | 20.95 ms | 42.27 ms | **3919.33** | 
| `node` (`12.6`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 19.79 ms | **9.21** ms | 18.49 ms | 378.54 ms | 1024.06 ms | **68160.33** | 
| `node` (`12.6`) | [0http](https://github.com/jkyberneees/0http) (**1.0**) | 14.40 ms | **9.29** ms | 17.60 ms | 140.71 ms | 790.26 ms | **36947.67** | 
| `go` (`1.12`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 12.91 ms | **9.49** ms | 21.31 ms | 92.30 ms | 379.32 ms | **19466.00** | 
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.28**) | 10.12 ms | **9.51** ms | 15.34 ms | 22.93 ms | 41.85 ms | **4091.67** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 12.00 ms | **9.51** ms | 14.24 ms | 62.69 ms | 581.15 ms | **25412.33** | 
| `go` (`1.12`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.0**) | 12.27 ms | **9.62** ms | 21.41 ms | 69.41 ms | 504.32 ms | **16205.00** | 
| `go` (`1.12`) | [echo](https://echo.labstack.com) (**4.1**) | 13.38 ms | **9.99** ms | 23.66 ms | 94.03 ms | 385.25 ms | **18190.33** | 
| `go` (`1.12`) | [gin](https://gin-gonic.com) (**1.4**) | 13.07 ms | **10.05** ms | 24.07 ms | 77.17 ms | 328.45 ms | **15394.00** | 
| `go` (`1.12`) | [violetear](https://violetear.org) (**7.0**) | 11.23 ms | **10.14** ms | 17.48 ms | 43.04 ms | 174.40 ms | **8424.67** | 
| `go` (`1.12`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 14.28 ms | **10.39** ms | 26.97 ms | 95.69 ms | 273.01 ms | **18190.33** | 
| `go` (`1.12`) | [kami](https://github.com/guregu/kami) (**2.2**) | 15.58 ms | **10.65** ms | 27.43 ms | 126.50 ms | 319.63 ms | **22872.00** | 
| `go` (`1.12`) | [beego](https://beego.me) (**1.12**) | 16.49 ms | **10.94** ms | 32.93 ms | 125.17 ms | 359.17 ms | **23783.67** | 
| `node` (`12.6`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 18.45 ms | **11.27** ms | 21.15 ms | 261.19 ms | 901.38 ms | **49934.00** | 
| `node` (`12.6`) | [rayo](https://rayo.js.org) (**1.3**) | 29.50 ms | **11.93** ms | 26.67 ms | 609.13 ms | 1487.23 ms | **101945.00** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**3.12**) | 91.10 ms | **12.21** ms | 214.27 ms | 1747.50 ms | 5998.55 ms | **351297.67** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 69.72 ms | **12.30** ms | 205.96 ms | 853.69 ms | 5173.83 ms | **219778.00** | 
| `go` (`1.12`) | [gf](https://goframe.org) (**1.8**) | 16.33 ms | **12.49** ms | 31.36 ms | 96.03 ms | 291.06 ms | **18957.67** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 85.26 ms | **12.65** ms | 234.79 ms | 1191.12 ms | 7034.48 ms | **304040.00** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 66.28 ms | **12.68** ms | 199.69 ms | 666.34 ms | 4385.67 ms | **195991.33** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 14.53 ms | **12.91** ms | 29.01 ms | 54.02 ms | 296.94 ms | **14877.33** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 67.42 ms | **13.76** ms | 211.53 ms | 659.55 ms | 5963.11 ms | **180988.67** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 17.73 ms | **13.87** ms | 31.84 ms | 56.53 ms | 262.49 ms | **14625.33** | 
| `node` (`12.6`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 24.78 ms | **14.11** ms | 25.81 ms | 416.47 ms | 1209.86 ms | **73341.67** | 
| `node` (`12.6`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 36.94 ms | **14.47** ms | 32.43 ms | 746.56 ms | 1692.59 ms | **123051.33** | 
| `node` (`12.6`) | [fastify](https://fastify.io) (**2.6**) | 32.04 ms | **15.56** ms | 31.80 ms | 554.30 ms | 1402.31 ms | **93096.33** | 
| `node` (`12.6`) | [koa](https://koajs.com) (**2.7**) | 40.45 ms | **15.99** ms | 36.77 ms | 831.20 ms | 1917.79 ms | **136242.00** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 22.89 ms | **16.02** ms | 37.79 ms | 79.71 ms | 564.82 ms | **23183.33** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 42.24 ms | **16.28** ms | 30.97 ms | 884.31 ms | 3062.14 ms | **179606.33** | 
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 16.29 ms | **16.75** ms | 17.78 ms | 20.06 ms | 291.54 ms | **4697.00** | 
| `node` (`12.6`) | [foxify](https://foxify.js.org) (**0.1**) | 44.82 ms | **16.90** ms | 33.90 ms | 950.92 ms | 2038.11 ms | **155047.00** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 94.39 ms | **16.91** ms | 340.52 ms | 849.06 ms | 6257.03 ms | **230157.00** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 23.02 ms | **18.70** ms | 40.30 ms | 75.18 ms | 205.49 ms | **14820.00** | 
| `node` (`12.6`) | [restify](https://restify.com) (**8.2**) | 27.10 ms | **18.79** ms | 38.28 ms | 194.76 ms | 983.55 ms | **46944.33** | 
| `node` (`12.6`) | [express](https://expressjs.com) (**4.17**) | 39.53 ms | **19.44** ms | 42.63 ms | 658.47 ms | 1810.62 ms | **113781.67** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 23.03 ms | **20.74** ms | 37.20 ms | 58.81 ms | 100.69 ms | **10753.33** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 26.44 ms | **20.91** ms | 33.06 ms | 189.91 ms | 690.90 ms | **41587.33** | 
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 116.16 ms | **22.15** ms | 51.62 ms | 2484.23 ms | 4852.32 ms | **443831.33** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.5**) | 29.02 ms | **22.17** ms | 46.99 ms | 95.89 ms | 522.65 ms | **23071.00** | 
| `crystal` (`0.28`) | [lucky](https://luckyframework.org) (**0.14**) | 27.34 ms | **25.50** ms | 34.95 ms | 40.53 ms | 111.09 ms | **5451.67** | 
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 26.08 ms | **25.90** ms | 28.99 ms | 35.82 ms | 405.91 ms | **13283.33** | 
| `node` (`12.6`) | [hapi](https://hapijs.com) (**18.1**) | 89.91 ms | **29.73** ms | 57.53 ms | 1698.66 ms | 3259.45 ms | **285430.00** | 
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 30.37 ms | **30.13** ms | 38.45 ms | 47.56 ms | 64.16 ms | **6385.33** | 
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 54.57 ms | **31.77** ms | 55.98 ms | 812.72 ms | 2288.72 ms | **145963.67** | 
| `node` (`12.6`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 36.72 ms | **32.38** ms | 34.81 ms | 228.55 ms | 740.86 ms | **42368.33** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.33**) | 39.21 ms | **35.50** ms | 53.53 ms | 162.47 ms | 805.67 ms | **40054.67** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 52.18 ms | **42.50** ms | 75.56 ms | 142.97 ms | 814.65 ms | **39487.33** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 52.89 ms | **45.06** ms | 79.55 ms | 145.50 ms | 684.74 ms | **37258.00** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.5**) | 54.91 ms | **53.75** ms | 80.32 ms | 95.76 ms | 116.13 ms | **16242.00** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 57.43 ms | **54.63** ms | 84.38 ms | 124.67 ms | 382.05 ms | **23313.33** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 67.53 ms | **62.05** ms | 107.10 ms | 153.40 ms | 503.30 ms | **37953.33** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.9**) | 93.19 ms | **88.63** ms | 130.62 ms | 156.28 ms | 283.20 ms | **27491.67** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 155.49 ms | **111.25** ms | 200.39 ms | 1196.07 ms | 2304.25 ms | **203037.67** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 133.26 ms | **120.06** ms | 194.49 ms | 278.73 ms | 557.15 ms | **47868.00** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 252.13 ms | **136.98** ms | 170.62 ms | 3439.24 ms | 5971.61 ms | **589485.00** | 
| `python` (`3.7`) | [cyclone](https://cyclone.io) (**0.0**) | 252.41 ms | **152.77** ms | 193.75 ms | 4189.98 ms | 7107.43 ms | **663632.33** | 
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 190.02 ms | **160.72** ms | 164.73 ms | 1762.03 ms | 3405.64 ms | **280548.00** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 246.30 ms | **178.75** ms | 320.25 ms | 1787.23 ms | 3591.19 ms | **314129.67** | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (kore) (c)


:four: (rapidoid) (java)


:five: (fasthttprouter) (go)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 203266.67 | **117.51** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 157988.00 | **189.06** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 150243.67 | **390.35** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 150114.00 | **270.21** MB |
| `go` (`1.12`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 149756.33 | **241.08** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 142253.67 | **138.05** MB |
| `python` (`3.6`) | [vibora](https://vibora.io) (**0.0**) | 139876.00 | **158.53** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 136915.33 | **236.22** MB |
| `rust` (`1.36`) | [gotham](https://gotham.rs) (**0.4**) | 132682.67 | **271.65** MB |
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 124719.33 | **250.64** MB |
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.4**) | 123482.33 | **132.13** MB |
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 122526.33 | **115.01** MB |
| `rust` (`1.36`) | [actix-web](https://actix.rs) (**1.0**) | 112751.00 | **161.52** MB |
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.25**) | 112292.67 | **183.60** MB |
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 109367.00 | **102.69** MB |
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 102538.00 | **167.57** MB |
| `rust` (`1.36`) | [iron](https://ironframework.io) (**0.6**) | 99525.33 | **125.07** MB |
| `rust` (`1.36`) | [nickel](https://nickel-org.github.io) (**0.11**) | 95606.33 | **189.92** MB |
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.28**) | 95518.33 | **174.75** MB |
| `go` (`1.12`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 93095.00 | **123.98** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 93023.33 | **151.18** MB |
| `go` (`1.12`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.0**) | 92207.00 | **122.70** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.8**) | 91497.33 | **53.15** MB |
| `go` (`1.12`) | [violetear](https://violetear.org) (**7.0**) | 90957.00 | **120.81** MB |
| `node` (`12.6`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 90145.33 | **135.14** MB |
| `node` (`12.6`) | [0http](https://github.com/jkyberneees/0http) (**1.0**) | 89225.33 | **133.79** MB |
| `go` (`1.12`) | [echo](https://echo.labstack.com) (**4.1**) | 89120.67 | **156.11** MB |
| `go` (`1.12`) | [gin](https://gin-gonic.com) (**1.4**) | 88983.00 | **155.96** MB |
| `go` (`1.12`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 86366.67 | **115.08** MB |
| `go` (`1.12`) | [kami](https://github.com/guregu/kami) (**2.2**) | 85738.67 | **114.01** MB |
| `go` (`1.12`) | [beego](https://beego.me) (**1.12**) | 84336.33 | **113.07** MB |
| `node` (`12.6`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 76267.00 | **113.95** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 74996.33 | **116.89** MB |
| `go` (`1.12`) | [gf](https://goframe.org) (**1.8**) | 73791.00 | **111.55** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 70864.00 | **151.96** MB |
| `node` (`12.6`) | [rayo](https://rayo.js.org) (**1.3**) | 70272.00 | **105.26** MB |
| `node` (`12.6`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 61691.00 | **92.36** MB |
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 60372.33 | **56.65** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 59751.33 | **140.07** MB |
| `node` (`12.6`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 58534.67 | **87.63** MB |
| `node` (`12.6`) | [fastify](https://fastify.io) (**2.6**) | 56888.33 | **146.27** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 55527.00 | **97.15** MB |
| `node` (`12.6`) | [koa](https://koajs.com) (**2.7**) | 51346.67 | **108.60** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 51179.00 | **38.58** MB |
| `node` (`12.6`) | [foxify](https://foxify.js.org) (**0.1**) | 49763.00 | **104.57** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 45512.67 | **112.01** MB |
| `node` (`12.6`) | [restify](https://restify.com) (**8.2**) | 44231.33 | **77.46** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**3.12**) | 43788.33 | **216.97** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 43704.67 | **93.94** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 43641.33 | **89.75** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 42757.00 | **211.79** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 42328.67 | **209.72** MB |
| `node` (`12.6`) | [express](https://expressjs.com) (**4.17**) | 41816.00 | **102.19** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 40081.00 | **198.62** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.22**) | 39468.00 | **37.59** MB |
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 39400.33 | **73.15** MB |
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 38624.33 | **64.42** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 37490.67 | **194.73** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.5**) | 36618.33 | **90.60** MB |
| `crystal` (`0.28`) | [lucky](https://luckyframework.org) (**0.14**) | 36264.33 | **44.59** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 36231.00 | **42.70** MB |
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 32737.67 | **84.01** MB |
| `node` (`12.6`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 30499.00 | **28.64** MB |
| `rust` (`nightly`) | [rocket](https://rocket.rs) (**0.4**) | 30178.00 | **47.61** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 29417.33 | **16.96** MB |
| `node` (`12.6`) | [hapi](https://hapijs.com) (**18.1**) | 29287.67 | **75.76** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 28282.67 | **147.61** MB |
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 28023.67 | **52.82** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.33**) | 27124.33 | **58.40** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 20266.67 | **11.70** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 20024.67 | **37.14** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 19273.67 | **47.38** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.5**) | 19207.33 | **43.51** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 18039.67 | **136.42** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 17315.33 | **33.40** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 14865.00 | **26.47** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 14499.67 | **37.60** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.9**) | 10581.00 | **21.10** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 7569.33 | **21.91** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 7407.33 | **16.18** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 6988.67 | **20.58** MB |
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 6167.00 | **7.71** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 4904.33 | **12.08** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**5.2**) | 3565.33 | **10.92** MB |
| `python` (`3.7`) | [cyclone](https://cyclone.io) (**0.0**) | 2869.33 | **7.79** MB |
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

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
Last update: 2019-07-22
```
OS: Linux (version: 5.1.18-300.fc30.x86_64, arch: x86_64)
CPU Cores: 4
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: cuba (ruby)


:three: flame (ruby)


:four: roda (ruby)


:five: iron (rust)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `rust` (`1.36`) | [nickel](https://nickel-org.github.io) (**0.11**) | 0.05 ms | **0.05** ms | 0.05 ms | 0.08 ms | 5.29 ms | **56.67** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 1.91 ms | **0.13** ms | 1.05 ms | 37.75 ms | 135.36 ms | **7167.00** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 3.25 ms | **0.20** ms | 10.21 ms | 49.13 ms | 169.48 ms | **9774.00** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.22**) | 1.82 ms | **0.22** ms | 1.02 ms | 37.56 ms | 177.46 ms | **7239.33** | 
| `rust` (`1.36`) | [iron](https://ironframework.io) (**0.6**) | 0.31 ms | **0.27** ms | 0.50 ms | 1.15 ms | 35.58 ms | **454.67** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 3.59 ms | **0.32** ms | 12.22 ms | 48.07 ms | 153.27 ms | **9771.33** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 4.32 ms | **0.36** ms | 15.64 ms | 52.10 ms | 152.64 ms | **10681.67** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 2.22 ms | **0.37** ms | 3.34 ms | 37.05 ms | 161.37 ms | **7227.67** | 
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 6.08 ms | **0.79** ms | 14.43 ms | 14.98 ms | 3420.25 ms | **57534.67** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**5.2**) | 17.45 ms | **1.00** ms | 61.63 ms | 157.75 ms | 423.60 ms | **34428.67** | 
| `rust` (`nightly`) | [rocket](https://rocket.rs) (**0.4**) | 99.59 ms | **3.28** ms | 5.59 ms | 2616.45 ms | 4468.94 ms | **451341.67** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 4.62 ms | **3.71** ms | 9.99 ms | 18.17 ms | 38.68 ms | **4043.67** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 4.94 ms | **4.55** ms | 8.99 ms | 15.19 ms | 32.94 ms | **3225.00** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 7.21 ms | **5.14** ms | 10.54 ms | 68.76 ms | 206.65 ms | **11146.67** | 
| `go` (`1.12`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 6.09 ms | **5.59** ms | 9.18 ms | 14.93 ms | 217.51 ms | **3628.67** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 25.20 ms | **5.84** ms | 14.35 ms | 638.53 ms | 2085.07 ms | **128655.33** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 6.43 ms | **6.04** ms | 10.04 ms | 16.54 ms | 109.39 ms | **3715.67** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 7.03 ms | **6.06** ms | 11.67 ms | 23.06 ms | 172.56 ms | **4865.67** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.8**) | 9.95 ms | **6.45** ms | 16.30 ms | 89.26 ms | 124.77 ms | **14178.00** | 
| `python` (`3.6`) | [vibora](https://vibora.io) (**0.0**) | 7.30 ms | **6.63** ms | 13.29 ms | 21.80 ms | 53.28 ms | **4847.00** | 
| `rust` (`1.36`) | [gotham](https://gotham.rs) (**0.4**) | 7.44 ms | **6.73** ms | 12.68 ms | 20.74 ms | 99.79 ms | **4835.00** | 
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 8.06 ms | **7.29** ms | 13.30 ms | 20.99 ms | 35.15 ms | **3881.67** | 
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.4**) | 8.34 ms | **7.33** ms | 14.18 ms | 23.46 ms | 60.60 ms | **4600.33** | 
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 13.34 ms | **7.42** ms | 15.08 ms | 172.21 ms | 259.08 ms | **28492.33** | 
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.25**) | 10.16 ms | **7.63** ms | 15.22 ms | 95.04 ms | 182.58 ms | **15294.00** | 
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 8.57 ms | **7.95** ms | 12.41 ms | 21.23 ms | 110.09 ms | **3674.00** | 
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 373.43 ms | **8.08** ms | 1553.61 ms | 3308.57 ms | 3705.02 ms | **786728.00** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 198.98 ms | **8.20** ms | 24.18 ms | 4643.33 ms | 7924.42 ms | **813688.67** | 
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.28**) | 65.15 ms | **8.33** ms | 17.90 ms | 1402.55 ms | 1765.98 ms | **240658.00** | 
| `rust` (`1.36`) | [actix-web](https://actix.rs) (**1.0**) | 9.23 ms | **8.67** ms | 14.54 ms | 22.06 ms | 49.33 ms | **4122.67** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 11.27 ms | **9.06** ms | 12.56 ms | 33.37 ms | 852.50 ms | **27702.00** | 
| `node` (`12.6`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 15.27 ms | **9.12** ms | 18.04 ms | 167.96 ms | 900.19 ms | **43351.67** | 
| `go` (`1.12`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 11.24 ms | **9.28** ms | 19.41 ms | 56.82 ms | 289.01 ms | **11859.67** | 
| `node` (`12.6`) | [0http](https://github.com/jkyberneees/0http) (**1.0**) | 14.37 ms | **9.29** ms | 17.32 ms | 150.61 ms | 722.75 ms | **35795.00** | 
| `node` (`12.6`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 15.08 ms | **9.79** ms | 17.83 ms | 163.30 ms | 795.85 ms | **38886.33** | 
| `go` (`1.12`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.0**) | 12.23 ms | **9.85** ms | 22.20 ms | 56.69 ms | 284.76 ms | **11619.33** | 
| `go` (`1.12`) | [echo](https://echo.labstack.com) (**4.1**) | 14.03 ms | **9.86** ms | 23.94 ms | 116.07 ms | 442.37 ms | **22638.67** | 
| `go` (`1.12`) | [gin](https://gin-gonic.com) (**1.4**) | 13.48 ms | **10.04** ms | 23.90 ms | 88.44 ms | 385.67 ms | **19036.00** | 
| `go` (`1.12`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 13.15 ms | **10.29** ms | 24.99 ms | 73.29 ms | 248.44 ms | **13903.00** | 
| `go` (`1.12`) | [violetear](https://violetear.org) (**7.0**) | 11.43 ms | **10.32** ms | 18.29 ms | 44.42 ms | 147.34 ms | **7938.67** | 
| `go` (`1.12`) | [kami](https://github.com/guregu/kami) (**2.2**) | 14.29 ms | **10.35** ms | 21.97 ms | 121.64 ms | 279.94 ms | **21273.33** | 
| `go` (`1.12`) | [beego](https://beego.me) (**1.12**) | 16.59 ms | **10.87** ms | 29.88 ms | 152.90 ms | 341.00 ms | **25655.00** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**3.12**) | 93.38 ms | **11.52** ms | 206.59 ms | 1744.24 ms | 7036.72 ms | **383679.00** | 
| `node` (`12.6`) | [rayo](https://rayo.js.org) (**1.3**) | 22.08 ms | **11.87** ms | 23.24 ms | 344.57 ms | 1255.19 ms | **67217.00** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 85.40 ms | **12.02** ms | 226.09 ms | 1354.83 ms | 7221.47 ms | **314832.33** | 
| `node` (`12.6`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 24.08 ms | **12.19** ms | 23.71 ms | 454.03 ms | 1179.77 ms | **77264.33** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 69.23 ms | **12.25** ms | 199.16 ms | 741.94 ms | 5580.73 ms | **228574.00** | 
| `go` (`1.12`) | [gf](https://goframe.org) (**1.8**) | 15.88 ms | **12.59** ms | 31.43 ms | 84.77 ms | 232.06 ms | **16215.67** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 69.58 ms | **12.65** ms | 215.68 ms | 675.09 ms | 5941.91 ms | **210759.33** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 14.74 ms | **13.19** ms | 29.72 ms | 55.85 ms | 286.60 ms | **15063.33** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 75.79 ms | **13.51** ms | 223.12 ms | 787.64 ms | 5951.28 ms | **251663.67** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 17.50 ms | **13.80** ms | 28.34 ms | 68.39 ms | 407.08 ms | **17512.00** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 22.01 ms | **14.52** ms | 34.42 ms | 118.36 ms | 656.94 ms | **30365.33** | 
| `node` (`12.6`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 31.74 ms | **14.80** ms | 30.71 ms | 600.84 ms | 1494.64 ms | **98801.00** | 
| `node` (`12.6`) | [foxify](https://foxify.js.org) (**0.1**) | 32.09 ms | **15.07** ms | 34.77 ms | 557.35 ms | 1557.95 ms | **97034.00** | 
| `node` (`12.6`) | [fastify](https://fastify.io) (**2.6**) | 35.83 ms | **15.63** ms | 33.03 ms | 710.88 ms | 1619.02 ms | **114592.33** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 36.77 ms | **15.92** ms | 30.38 ms | 755.76 ms | 3588.76 ms | **156971.67** | 
| `node` (`12.6`) | [koa](https://koajs.com) (**2.7**) | 38.17 ms | **16.14** ms | 39.53 ms | 720.83 ms | 1770.19 ms | **120707.33** | 
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 16.04 ms | **16.49** ms | 17.51 ms | 19.62 ms | 225.47 ms | **4345.67** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 94.47 ms | **16.60** ms | 308.04 ms | 953.71 ms | 5177.04 ms | **232558.00** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 22.37 ms | **17.13** ms | 36.77 ms | 87.89 ms | 343.50 ms | **17081.33** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 19.66 ms | **18.63** ms | 27.33 ms | 42.23 ms | 94.55 ms | **6430.33** | 
| `node` (`12.6`) | [restify](https://restify.com) (**8.2**) | 34.34 ms | **19.23** ms | 39.84 ms | 494.94 ms | 1372.02 ms | **87070.67** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.5**) | 28.62 ms | **19.99** ms | 47.76 ms | 110.00 ms | 354.10 ms | **20523.67** | 
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 127.33 ms | **20.92** ms | 58.50 ms | 2746.61 ms | 4546.04 ms | **466130.33** | 
| `node` (`12.6`) | [express](https://expressjs.com) (**4.17**) | 52.93 ms | **21.11** ms | 41.04 ms | 1045.06 ms | 2210.91 ms | **169965.33** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 23.59 ms | **23.52** ms | 32.30 ms | 44.58 ms | 118.05 ms | **7429.67** | 
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 31.41 ms | **26.09** ms | 32.44 ms | 243.34 ms | 1533.13 ms | **53239.67** | 
| `crystal` (`0.28`) | [lucky](https://luckyframework.org) (**0.14**) | 29.01 ms | **28.62** ms | 33.45 ms | 35.61 ms | 174.56 ms | **4691.67** | 
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 30.32 ms | **29.99** ms | 38.78 ms | 48.74 ms | 88.56 ms | **6913.67** | 
| `node` (`12.6`) | [hapi](https://hapijs.com) (**18.1**) | 117.74 ms | **31.13** ms | 69.67 ms | 2067.69 ms | 3600.93 ms | **361216.00** | 
| `node` (`12.6`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 31.48 ms | **31.33** ms | 33.39 ms | 35.76 ms | 437.18 ms | **13701.33** | 
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 57.08 ms | **32.66** ms | 55.28 ms | 847.55 ms | 2335.62 ms | **161046.00** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.33**) | 37.28 ms | **35.62** ms | 49.53 ms | 60.02 ms | 641.36 ms | **20748.00** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.5**) | 42.87 ms | **41.47** ms | 62.60 ms | 74.19 ms | 145.12 ms | **14630.67** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 51.39 ms | **44.15** ms | 79.02 ms | 112.16 ms | 639.73 ms | **28826.67** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 51.82 ms | **46.12** ms | 77.75 ms | 120.78 ms | 516.80 ms | **27440.67** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 56.55 ms | **53.27** ms | 71.39 ms | 108.22 ms | 476.04 ms | **20428.33** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 64.51 ms | **57.85** ms | 85.81 ms | 87.53 ms | 148.86 ms | **20407.67** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 65.99 ms | **62.82** ms | 97.65 ms | 139.90 ms | 364.56 ms | **25781.00** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.9**) | 92.28 ms | **77.65** ms | 142.66 ms | 168.51 ms | 355.42 ms | **40951.67** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 112.56 ms | **116.45** ms | 156.12 ms | 180.47 ms | 330.59 ms | **35615.33** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 138.80 ms | **120.16** ms | 173.22 ms | 722.64 ms | 1979.35 ms | **123818.67** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 217.18 ms | **128.59** ms | 176.81 ms | 2797.86 ms | 4724.84 ms | **459302.67** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 197.07 ms | **139.32** ms | 220.00 ms | 1754.23 ms | 4292.55 ms | **290488.33** | 
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 396.41 ms | **147.34** ms | 238.15 ms | 6335.90 ms | 7969.30 ms | **1038423.67** | 
| `python` (`3.7`) | [cyclone](https://cyclone.io) (**0.0**) | 289.70 ms | **212.30** ms | 261.91 ms | 3699.43 ms | 6115.36 ms | **588181.33** | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (rapidoid) (java)


:four: (fasthttprouter) (go)


:five: (kore) (c)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 203568.67 | **117.69** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 177144.33 | **211.88** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 153223.67 | **275.85** MB |
| `go` (`1.12`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 146994.67 | **237.10** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 146034.00 | **379.33** MB |
| `python` (`3.6`) | [vibora](https://vibora.io) (**0.0**) | 142967.33 | **162.10** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 141126.00 | **136.94** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 136959.00 | **236.22** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.8**) | 132071.33 | **76.43** MB |
| `rust` (`1.36`) | [gotham](https://gotham.rs) (**0.4**) | 127938.00 | **262.10** MB |
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 123436.67 | **248.11** MB |
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 121360.33 | **114.20** MB |
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 118244.33 | **111.03** MB |
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.4**) | 117681.33 | **125.61** MB |
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.25**) | 113829.00 | **186.04** MB |
| `rust` (`1.36`) | [actix-web](https://actix.rs) (**1.0**) | 112368.67 | **161.34** MB |
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.28**) | 103291.33 | **188.93** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 101112.67 | **164.69** MB |
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 99781.33 | **163.02** MB |
| `rust` (`1.36`) | [iron](https://ironframework.io) (**0.6**) | 96873.00 | **122.35** MB |
| `go` (`1.12`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 93501.00 | **125.06** MB |
| `rust` (`1.36`) | [nickel](https://nickel-org.github.io) (**0.11**) | 90452.67 | **179.57** MB |
| `node` (`12.6`) | [0http](https://github.com/jkyberneees/0http) (**1.0**) | 89967.67 | **134.80** MB |
| `go` (`1.12`) | [violetear](https://violetear.org) (**7.0**) | 89718.33 | **118.97** MB |
| `go` (`1.12`) | [echo](https://echo.labstack.com) (**4.1**) | 89622.00 | **157.18** MB |
| `node` (`12.6`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 89279.00 | **133.81** MB |
| `go` (`1.12`) | [gin](https://gin-gonic.com) (**1.4**) | 88354.00 | **154.87** MB |
| `go` (`1.12`) | [kami](https://github.com/guregu/kami) (**2.2**) | 88144.33 | **116.95** MB |
| `go` (`1.12`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.0**) | 86987.33 | **115.00** MB |
| `node` (`12.6`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 86817.00 | **130.09** MB |
| `go` (`1.12`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 86437.00 | **115.15** MB |
| `go` (`1.12`) | [beego](https://beego.me) (**1.12**) | 84542.67 | **113.63** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 74580.33 | **116.02** MB |
| `go` (`1.12`) | [gf](https://goframe.org) (**1.8**) | 72934.67 | **110.35** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 70643.33 | **151.46** MB |
| `node` (`12.6`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 69866.00 | **104.67** MB |
| `node` (`12.6`) | [rayo](https://rayo.js.org) (**1.3**) | 68778.00 | **103.05** MB |
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 61251.00 | **57.55** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 60982.67 | **142.81** MB |
| `node` (`12.6`) | [fastify](https://fastify.io) (**2.6**) | 58510.67 | **149.99** MB |
| `node` (`12.6`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 58067.33 | **86.99** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 56436.67 | **98.76** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 55765.00 | **41.94** MB |
| `node` (`12.6`) | [foxify](https://foxify.js.org) (**0.1**) | 53863.33 | **113.07** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 51025.33 | **104.80** MB |
| `node` (`12.6`) | [koa](https://koajs.com) (**2.7**) | 50569.33 | **106.85** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 47741.00 | **117.40** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**3.12**) | 44209.00 | **218.95** MB |
| `node` (`12.6`) | [restify](https://restify.com) (**8.2**) | 44076.33 | **77.16** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 43026.67 | **213.61** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 42864.33 | **212.34** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 42264.67 | **90.81** MB |
| `node` (`12.6`) | [express](https://expressjs.com) (**4.17**) | 40833.33 | **99.73** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 40436.33 | **200.43** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.22**) | 39322.00 | **37.45** MB |
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 38727.67 | **64.55** MB |
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 38090.00 | **70.74** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 37764.00 | **196.07** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.5**) | 37091.33 | **91.71** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 35490.67 | **41.81** MB |
| `crystal` (`0.28`) | [lucky](https://luckyframework.org) (**0.14**) | 34078.33 | **42.03** MB |
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 32803.00 | **84.18** MB |
| `node` (`12.6`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 31155.00 | **29.23** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 29700.00 | **17.12** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 29263.67 | **152.77** MB |
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 27655.33 | **52.16** MB |
| `node` (`12.6`) | [hapi](https://hapijs.com) (**18.1**) | 27603.33 | **71.23** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.33**) | 26951.00 | **58.04** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.5**) | 23116.00 | **52.33** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 19920.33 | **11.49** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 19651.00 | **36.48** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 19416.67 | **47.74** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 17880.00 | **135.19** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 17492.33 | **33.75** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 15267.67 | **40.08** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 14972.33 | **26.67** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 14794.00 | **38.37** MB |
| `rust` (`nightly`) | [rocket](https://rocket.rs) (**0.4**) | 13383.67 | **22.26** MB |
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 11138.00 | **14.26** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.9**) | 10639.67 | **21.20** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 8724.67 | **19.01** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 7586.33 | **21.95** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 7046.67 | **20.76** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 6235.33 | **15.33** MB |
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 6171.33 | **7.72** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**5.2**) | 3657.67 | **11.21** MB |
| `python` (`3.7`) | [cyclone](https://cyclone.io) (**0.0**) | 2831.33 | **7.69** MB |
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

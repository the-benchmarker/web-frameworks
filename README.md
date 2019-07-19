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
Last update: 2019-07-19
```
OS: Linux (version: 5.1.17-300.fc30.x86_64, arch: x86_64)
CPU Cores: 4
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: cuba (ruby)


:three: rack-routing (ruby)


:four: iron (rust)


:five: flame (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `rust` (`1.36`) | [nickel](https://nickel-org.github.io) (**0.11**) | 0.04 ms | **0.04** ms | 0.05 ms | 0.08 ms | 4.39 ms | **52.00** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 1.92 ms | **0.19** ms | 2.14 ms | 35.15 ms | 153.81 ms | **6759.67** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 2.24 ms | **0.26** ms | 3.32 ms | 38.50 ms | 169.88 ms | **7459.67** | 
| `rust` (`1.36`) | [iron](https://ironframework.io) (**0.6**) | 0.30 ms | **0.26** ms | 0.51 ms | 1.15 ms | 49.22 ms | **431.00** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 3.20 ms | **0.26** ms | 7.44 ms | 51.06 ms | 177.58 ms | **9906.67** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 3.50 ms | **0.32** ms | 11.74 ms | 43.92 ms | 128.87 ms | **8935.33** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 4.29 ms | **0.40** ms | 14.51 ms | 53.39 ms | 158.33 ms | **10869.33** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.22**) | 1.77 ms | **0.48** ms | 1.35 ms | 31.79 ms | 174.80 ms | **6293.67** | 
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 6.08 ms | **0.80** ms | 14.43 ms | 14.99 ms | 4067.98 ms | **63435.67** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**5.2**) | 17.32 ms | **0.98** ms | 61.49 ms | 160.82 ms | 467.20 ms | **34781.67** | 
| `rust` (`nightly`) | [rocket](https://rocket.rs) (**0.4**) | 179.88 ms | **2.98** ms | 131.12 ms | 4212.03 ms | 6594.64 ms | **758362.67** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 4.58 ms | **3.61** ms | 9.86 ms | 18.00 ms | 37.19 ms | **3959.33** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 4.92 ms | **4.69** ms | 8.85 ms | 15.25 ms | 34.23 ms | **3227.00** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 9.43 ms | **5.12** ms | 12.39 ms | 81.93 ms | 125.39 ms | **15702.33** | 
| `go` (`1.12`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 6.19 ms | **5.58** ms | 9.12 ms | 15.62 ms | 229.67 ms | **5935.33** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 7.21 ms | **5.97** ms | 12.10 ms | 26.43 ms | 281.54 ms | **7162.33** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 6.41 ms | **5.99** ms | 9.90 ms | 15.66 ms | 152.95 ms | **3798.33** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 47.90 ms | **6.05** ms | 15.17 ms | 1304.45 ms | 2624.46 ms | **228528.33** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.8**) | 10.29 ms | **6.33** ms | 15.93 ms | 92.76 ms | 126.36 ms | **15249.33** | 
| `python` (`3.6`) | [vibora](https://vibora.io) (**0.0**) | 7.08 ms | **6.35** ms | 12.76 ms | 20.85 ms | 46.60 ms | **4547.00** | 
| `rust` (`1.36`) | [gotham](https://gotham.rs) (**0.4**) | 7.30 ms | **6.51** ms | 12.36 ms | 20.06 ms | 165.48 ms | **5591.67** | 
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 7.84 ms | **7.22** ms | 12.49 ms | 19.13 ms | 33.01 ms | **3484.67** | 
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 7.95 ms | **7.22** ms | 12.98 ms | 20.21 ms | 37.16 ms | **3748.33** | 
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.4**) | 8.11 ms | **7.25** ms | 13.51 ms | 21.85 ms | 40.52 ms | **4036.33** | 
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.25**) | 321.05 ms | **7.63** ms | 1384.33 ms | 2243.02 ms | 2419.82 ms | **603297.67** | 
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 8.59 ms | **7.83** ms | 12.49 ms | 22.90 ms | 136.60 ms | **4371.00** | 
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 691.31 ms | **7.90** ms | 2650.45 ms | 5701.70 ms | 6313.12 ms | **1356596.67** | 
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.28**) | 31.38 ms | **8.27** ms | 43.90 ms | 585.20 ms | 819.44 ms | **97367.00** | 
| `rust` (`1.36`) | [actix-web](https://actix.rs) (**1.0**) | 8.90 ms | **8.34** ms | 14.14 ms | 21.38 ms | 43.58 ms | **4005.00** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 190.80 ms | **8.55** ms | 23.63 ms | 4827.47 ms | 7932.26 ms | **812257.67** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 10.39 ms | **8.96** ms | 12.62 ms | 28.57 ms | 577.87 ms | **18953.67** | 
| `node` (`12.6`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 15.91 ms | **9.29** ms | 17.69 ms | 214.89 ms | 883.01 ms | **46408.00** | 
| `node` (`12.6`) | [0http](https://github.com/jkyberneees/0http) (**1.0**) | 14.72 ms | **9.40** ms | 17.25 ms | 145.80 ms | 806.17 ms | **38133.67** | 
| `node` (`12.6`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 17.57 ms | **9.56** ms | 18.07 ms | 258.15 ms | 1016.02 ms | **54806.33** | 
| `go` (`1.12`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.0**) | 12.61 ms | **9.71** ms | 23.01 ms | 75.59 ms | 205.35 ms | **13978.67** | 
| `go` (`1.12`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 13.43 ms | **9.94** ms | 23.68 ms | 94.28 ms | 379.39 ms | **18021.33** | 
| `go` (`1.12`) | [gin](https://gin-gonic.com) (**1.4**) | 14.47 ms | **10.03** ms | 24.90 ms | 114.01 ms | 521.74 ms | **24025.33** | 
| `go` (`1.12`) | [echo](https://echo.labstack.com) (**4.1**) | 13.81 ms | **10.27** ms | 25.55 ms | 85.86 ms | 333.33 ms | **17645.67** | 
| `go` (`1.12`) | [violetear](https://violetear.org) (**7.0**) | 12.16 ms | **10.49** ms | 19.96 ms | 55.50 ms | 176.62 ms | **10601.67** | 
| `go` (`1.12`) | [kami](https://github.com/guregu/kami) (**2.2**) | 15.47 ms | **10.62** ms | 26.20 ms | 132.41 ms | 396.82 ms | **23634.33** | 
| `go` (`1.12`) | [beego](https://beego.me) (**1.12**) | 16.61 ms | **10.63** ms | 32.52 ms | 129.19 ms | 529.21 ms | **27347.00** | 
| `go` (`1.12`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 15.75 ms | **10.87** ms | 29.24 ms | 117.74 ms | 366.73 ms | **23181.00** | 
| `node` (`12.6`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 20.55 ms | **11.61** ms | 22.68 ms | 297.82 ms | 1113.97 ms | **59994.33** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 79.94 ms | **11.82** ms | 213.91 ms | 1179.28 ms | 7259.71 ms | **309229.33** | 
| `node` (`12.6`) | [rayo](https://rayo.js.org) (**1.3**) | 20.52 ms | **12.18** ms | 24.27 ms | 269.56 ms | 985.00 ms | **52659.67** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**3.12**) | 73.90 ms | **12.22** ms | 194.25 ms | 1146.94 ms | 5893.72 ms | **260158.67** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 64.85 ms | **12.42** ms | 192.00 ms | 707.38 ms | 5140.47 ms | **207960.33** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 17.49 ms | **12.49** ms | 31.14 ms | 57.02 ms | 176.18 ms | **11803.67** | 
| `go` (`1.12`) | [gf](https://goframe.org) (**1.8**) | 16.75 ms | **12.49** ms | 33.56 ms | 104.05 ms | 383.84 ms | **19737.67** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 14.21 ms | **12.60** ms | 29.11 ms | 53.88 ms | 317.61 ms | **13950.33** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 117.65 ms | **12.88** ms | 254.89 ms | 2223.56 ms | 7176.18 ms | **422484.33** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 72.51 ms | **13.80** ms | 211.02 ms | 700.58 ms | 5393.04 ms | **223409.67** | 
| `node` (`12.6`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 39.31 ms | **14.52** ms | 31.20 ms | 867.47 ms | 1829.95 ms | **138508.67** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 23.49 ms | **14.56** ms | 41.48 ms | 121.59 ms | 202.90 ms | **21842.00** | 
| `node` (`12.6`) | [fastify](https://fastify.io) (**2.6**) | 39.40 ms | **15.21** ms | 32.44 ms | 816.83 ms | 1741.38 ms | **131155.33** | 
| `node` (`12.6`) | [foxify](https://foxify.js.org) (**0.1**) | 33.14 ms | **15.42** ms | 32.54 ms | 633.60 ms | 1795.29 ms | **112873.00** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 20.04 ms | **15.43** ms | 28.64 ms | 68.60 ms | 1483.34 ms | **47020.67** | 
| `node` (`12.6`) | [koa](https://koajs.com) (**2.7**) | 40.98 ms | **15.62** ms | 40.59 ms | 817.83 ms | 1844.25 ms | **134221.33** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 101.86 ms | **16.18** ms | 333.06 ms | 996.25 ms | 6227.08 ms | **267878.33** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 22.53 ms | **16.64** ms | 37.90 ms | 77.51 ms | 548.28 ms | **20797.33** | 
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 16.55 ms | **16.74** ms | 17.86 ms | 21.46 ms | 229.65 ms | **5048.33** | 
| `node` (`12.6`) | [restify](https://restify.com) (**8.2**) | 28.42 ms | **18.77** ms | 35.38 ms | 290.44 ms | 1014.93 ms | **56814.33** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 19.93 ms | **18.83** ms | 27.26 ms | 38.62 ms | 75.33 ms | **5602.33** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.5**) | 27.54 ms | **19.66** ms | 46.67 ms | 86.94 ms | 312.10 ms | **19542.00** | 
| `node` (`12.6`) | [express](https://expressjs.com) (**4.17**) | 43.13 ms | **20.63** ms | 40.46 ms | 759.50 ms | 1830.97 ms | **126302.33** | 
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 154.02 ms | **20.97** ms | 72.70 ms | 3505.68 ms | 6626.03 ms | **583396.67** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 24.67 ms | **22.45** ms | 33.20 ms | 48.10 ms | 623.76 ms | **22880.33** | 
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 26.30 ms | **26.04** ms | 28.82 ms | 36.11 ms | 608.20 ms | **16260.00** | 
| `crystal` (`0.28`) | [lucky](https://luckyframework.org) (**0.14**) | 29.13 ms | **27.76** ms | 34.04 ms | 35.89 ms | 247.74 ms | **6152.00** | 
| `node` (`12.6`) | [hapi](https://hapijs.com) (**18.1**) | 102.56 ms | **29.45** ms | 59.91 ms | 1973.12 ms | 3526.20 ms | **331651.33** | 
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 30.46 ms | **30.17** ms | 38.79 ms | 47.87 ms | 65.61 ms | **6545.00** | 
| `node` (`12.6`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 32.02 ms | **31.44** ms | 33.58 ms | 35.89 ms | 447.10 ms | **13153.67** | 
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 42.09 ms | **32.53** ms | 52.25 ms | 322.43 ms | 1772.58 ms | **81779.33** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.33**) | 36.85 ms | **33.39** ms | 50.27 ms | 64.27 ms | 818.88 ms | **28146.33** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 53.18 ms | **40.29** ms | 85.46 ms | 158.87 ms | 426.52 ms | **31803.00** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 52.60 ms | **41.08** ms | 92.89 ms | 110.57 ms | 522.15 ms | **31290.33** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.5**) | 41.36 ms | **41.42** ms | 58.36 ms | 70.64 ms | 165.48 ms | **15141.67** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 57.22 ms | **54.72** ms | 78.54 ms | 115.09 ms | 340.71 ms | **21684.67** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 66.29 ms | **63.38** ms | 98.31 ms | 134.62 ms | 357.06 ms | **25074.67** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.9**) | 91.33 ms | **83.74** ms | 140.63 ms | 173.76 ms | 238.35 ms | **34900.33** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 110.35 ms | **96.58** ms | 167.60 ms | 204.47 ms | 949.22 ms | **51111.00** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 157.34 ms | **124.45** ms | 153.67 ms | 1390.62 ms | 2978.74 ms | **226865.67** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 177.91 ms | **130.88** ms | 169.66 ms | 1869.80 ms | 3397.27 ms | **295648.67** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 210.95 ms | **148.81** ms | 193.56 ms | 2316.37 ms | 3814.14 ms | **358953.67** | 
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 233.84 ms | **154.90** ms | 190.56 ms | 2512.70 ms | 3715.97 ms | **415604.67** | 
| `python` (`3.7`) | [cyclone](https://cyclone.io) (**0.0**) | 307.96 ms | **221.01** ms | 278.49 ms | 3832.09 ms | 7175.95 ms | **618534.67** | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (rapidoid) (java)


:four: (fasthttprouter) (go)


:five: (vibora) (python)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 199670.00 | **115.49** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 177634.00 | **212.57** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 153167.00 | **275.56** MB |
| `go` (`1.12`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 146986.67 | **236.54** MB |
| `python` (`3.6`) | [vibora](https://vibora.io) (**0.0**) | 144171.33 | **163.66** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 142837.67 | **370.30** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 141011.33 | **136.87** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 137266.33 | **236.87** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.8**) | 132990.67 | **76.84** MB |
| `rust` (`1.36`) | [gotham](https://gotham.rs) (**0.4**) | 130192.33 | **266.35** MB |
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 123911.33 | **116.40** MB |
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 123719.00 | **248.70** MB |
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 122186.33 | **114.87** MB |
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.4**) | 120138.00 | **128.27** MB |
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.25**) | 114424.33 | **187.01** MB |
| `rust` (`1.36`) | [actix-web](https://actix.rs) (**1.0**) | 114286.67 | **163.27** MB |
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.28**) | 105704.33 | **193.62** MB |
| `rust` (`1.36`) | [nickel](https://nickel-org.github.io) (**0.11**) | 102067.33 | **202.98** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 100367.67 | **163.49** MB |
| `rust` (`1.36`) | [iron](https://ironframework.io) (**0.6**) | 99692.33 | **125.66** MB |
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 95829.67 | **156.64** MB |
| `go` (`1.12`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.0**) | 91250.00 | **121.51** MB |
| `node` (`12.6`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 90498.00 | **135.64** MB |
| `go` (`1.12`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 89401.33 | **119.52** MB |
| `node` (`12.6`) | [0http](https://github.com/jkyberneees/0http) (**1.0**) | 88940.67 | **133.26** MB |
| `go` (`1.12`) | [gin](https://gin-gonic.com) (**1.4**) | 88229.00 | **154.70** MB |
| `go` (`1.12`) | [violetear](https://violetear.org) (**7.0**) | 87979.00 | **116.68** MB |
| `node` (`12.6`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 87181.67 | **130.61** MB |
| `go` (`1.12`) | [echo](https://echo.labstack.com) (**4.1**) | 86944.33 | **152.32** MB |
| `go` (`1.12`) | [kami](https://github.com/guregu/kami) (**2.2**) | 85569.00 | **113.85** MB |
| `go` (`1.12`) | [beego](https://beego.me) (**1.12**) | 83531.33 | **112.24** MB |
| `go` (`1.12`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 83107.67 | **110.75** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 76189.67 | **118.63** MB |
| `go` (`1.12`) | [gf](https://goframe.org) (**1.8**) | 73145.67 | **110.94** MB |
| `node` (`12.6`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 71049.67 | **106.46** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 69006.00 | **147.87** MB |
| `node` (`12.6`) | [rayo](https://rayo.js.org) (**1.3**) | 68066.67 | **102.05** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 60301.00 | **141.34** MB |
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 60239.67 | **56.60** MB |
| `node` (`12.6`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 58723.67 | **87.98** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 58317.33 | **102.08** MB |
| `node` (`12.6`) | [fastify](https://fastify.io) (**2.6**) | 58029.33 | **147.94** MB |
| `node` (`12.6`) | [foxify](https://foxify.js.org) (**0.1**) | 53745.00 | **112.74** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 52174.00 | **39.04** MB |
| `node` (`12.6`) | [koa](https://koajs.com) (**2.7**) | 50993.67 | **107.80** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 50145.67 | **102.88** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 49704.67 | **122.36** MB |
| `node` (`12.6`) | [restify](https://restify.com) (**8.2**) | 45617.00 | **79.87** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**3.12**) | 43847.33 | **217.11** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 42281.00 | **209.34** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 42019.67 | **208.50** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 41710.67 | **89.72** MB |
| `node` (`12.6`) | [express](https://expressjs.com) (**4.17**) | 41081.33 | **100.31** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 39650.00 | **196.61** MB |
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 39061.33 | **72.50** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.22**) | 38720.67 | **36.90** MB |
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 38685.00 | **64.54** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.5**) | 37882.33 | **93.84** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 37822.00 | **196.49** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 35257.00 | **41.55** MB |
| `crystal` (`0.28`) | [lucky](https://luckyframework.org) (**0.14**) | 33885.00 | **41.73** MB |
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 32685.67 | **83.76** MB |
| `node` (`12.6`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 31139.67 | **29.22** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 29710.33 | **17.13** MB |
| `node` (`12.6`) | [hapi](https://hapijs.com) (**18.1**) | 29298.33 | **75.73** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 28941.33 | **151.14** MB |
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 28372.00 | **53.53** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.33**) | 27457.67 | **59.12** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.5**) | 23169.00 | **52.40** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 20298.33 | **11.70** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 19610.00 | **36.40** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 19088.33 | **46.94** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 18242.67 | **137.89** MB |
| `rust` (`nightly`) | [rocket](https://rocket.rs) (**0.4**) | 17988.33 | **27.86** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 17408.33 | **33.59** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 14934.00 | **26.61** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 14907.33 | **38.66** MB |
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 11351.67 | **14.52** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.9**) | 10817.67 | **21.55** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 8826.33 | **19.23** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 7604.67 | **22.02** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 7089.33 | **20.92** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 6298.67 | **15.47** MB |
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 6247.67 | **7.81** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**5.2**) | 3680.33 | **11.28** MB |
| `python` (`3.7`) | [cyclone](https://cyclone.io) (**0.0**) | 2889.33 | **7.85** MB |
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

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
Last update: 2019-07-29
```
OS: Linux (version: 5.1.19-300.fc30.x86_64, arch: x86_64)
CPU Cores: 4
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: flame (ruby)


:four: sinatra (ruby)


:five: cuba (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `rust` (`1.36`) | [nickel](https://nickel-org.github.io) (**0.11**) | 0.05 ms | **0.05** ms | 0.05 ms | 0.18 ms | 7.26 ms | **97.00** | 
| `rust` (`1.36`) | [iron](https://ironframework.io) (**0.6**) | 0.32 ms | **0.26** ms | 0.50 ms | 1.96 ms | 22.25 ms | **455.33** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 3.73 ms | **0.40** ms | 11.20 ms | 47.06 ms | 167.84 ms | **9481.67** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 5.14 ms | **0.68** ms | 16.91 ms | 56.28 ms | 196.44 ms | **11639.67** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 2.31 ms | **0.74** ms | 3.94 ms | 32.07 ms | 146.95 ms | **6392.67** | 
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 4.08 ms | **0.80** ms | 14.60 ms | 16.49 ms | 1775.77 ms | **21754.00** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.22**) | 2.09 ms | **1.09** ms | 3.95 ms | 23.01 ms | 224.38 ms | **4961.00** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**5.2**) | 18.81 ms | **1.11** ms | 65.68 ms | 165.49 ms | 468.01 ms | **36143.33** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 3.32 ms | **1.15** ms | 8.45 ms | 33.70 ms | 181.03 ms | **6996.00** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 5.68 ms | **2.03** ms | 14.72 ms | 53.80 ms | 228.84 ms | **10754.67** | 
| `rust` (`nightly`) | [rocket](https://rocket.rs) (**0.4**) | 87.12 ms | **3.22** ms | 8.84 ms | 1974.10 ms | 4426.79 ms | **362882.33** | 
| `node` (`12.7`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 4.63 ms | **3.94** ms | 8.64 ms | 15.22 ms | 37.42 ms | **3223.00** | 
| `node` (`12.7`) | [nanoexpress](https://nanoexpress.js.org) (**0.12**) | 6.10 ms | **4.71** ms | 12.24 ms | 24.88 ms | 83.29 ms | **5346.00** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 5.19 ms | **4.89** ms | 10.54 ms | 17.94 ms | 39.97 ms | **4214.33** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 5.86 ms | **5.03** ms | 11.73 ms | 22.11 ms | 87.30 ms | **4957.00** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 24.97 ms | **5.58** ms | 11.72 ms | 666.61 ms | 1831.71 ms | **127342.00** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 6.90 ms | **5.66** ms | 13.06 ms | 26.12 ms | 69.81 ms | **5400.00** | 
| `go` (`1.12`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 7.23 ms | **6.28** ms | 12.09 ms | 23.22 ms | 112.91 ms | **5140.00** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 7.70 ms | **6.38** ms | 13.47 ms | 29.33 ms | 137.81 ms | **6599.33** | 
| `python` (`3.6`) | [vibora](https://vibora.io) (**0.0**) | 8.14 ms | **7.18** ms | 15.30 ms | 27.21 ms | 77.48 ms | **5930.67** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 7.91 ms | **7.37** ms | 13.22 ms | 23.45 ms | 56.07 ms | **4629.00** | 
| `rust` (`1.36`) | [gotham](https://gotham.rs) (**0.4**) | 8.58 ms | **7.46** ms | 14.83 ms | 31.45 ms | 88.28 ms | **6141.67** | 
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.4**) | 8.36 ms | **7.56** ms | 13.61 ms | 21.80 ms | 41.25 ms | **4034.00** | 
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 8.37 ms | **7.76** ms | 13.09 ms | 20.51 ms | 45.99 ms | **3763.00** | 
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 8.62 ms | **7.89** ms | 13.64 ms | 21.90 ms | 62.95 ms | **4118.00** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.9**) | 15.13 ms | **8.00** ms | 35.88 ms | 103.38 ms | 138.94 ms | **21254.67** | 
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.25**) | 8.80 ms | **8.16** ms | 13.56 ms | 20.96 ms | 35.57 ms | **3695.33** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 190.99 ms | **8.55** ms | 23.33 ms | 4826.83 ms | 7933.53 ms | **816502.00** | 
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 9.82 ms | **8.85** ms | 15.38 ms | 26.06 ms | 158.98 ms | **5476.67** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 11.03 ms | **9.05** ms | 12.82 ms | 39.74 ms | 712.28 ms | **23442.33** | 
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 10.22 ms | **9.20** ms | 16.30 ms | 27.15 ms | 48.62 ms | **4873.00** | 
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.28**) | 10.64 ms | **9.69** ms | 16.83 ms | 27.71 ms | 67.92 ms | **5097.00** | 
| `node` (`12.7`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 19.86 ms | **10.08** ms | 19.09 ms | 336.02 ms | 1097.71 ms | **64431.67** | 
| `rust` (`1.36`) | [actix-web](https://actix.rs) (**1.0**) | 11.31 ms | **10.41** ms | 18.34 ms | 30.47 ms | 67.25 ms | **5728.00** | 
| `go` (`1.12`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 13.44 ms | **11.29** ms | 24.55 ms | 48.94 ms | 141.39 ms | **9355.33** | 
| `go` (`1.12`) | [gin](https://gin-gonic.com) (**1.4**) | 13.63 ms | **11.54** ms | 24.65 ms | 49.56 ms | 119.93 ms | **9594.33** | 
| `node` (`12.7`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 16.42 ms | **11.56** ms | 21.98 ms | 149.56 ms | 765.09 ms | **35625.33** | 
| `go` (`1.12`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.0**) | 13.95 ms | **11.75** ms | 25.35 ms | 49.73 ms | 113.13 ms | **9524.67** | 
| `node` (`12.7`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 26.96 ms | **12.08** ms | 22.99 ms | 491.98 ms | 1313.37 ms | **89669.33** | 
| `go` (`1.12`) | [beego](https://beego.me) (**1.12**) | 15.15 ms | **12.46** ms | 27.51 ms | 60.50 ms | 224.56 ms | **12647.33** | 
| `node` (`12.7`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 30.21 ms | **12.47** ms | 24.01 ms | 652.00 ms | 1507.35 ms | **106469.00** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 79.45 ms | **12.56** ms | 222.20 ms | 894.31 ms | 7155.60 ms | **285844.67** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**3.12**) | 85.52 ms | **12.68** ms | 211.73 ms | 998.14 ms | 7284.58 ms | **357313.67** | 
| `go` (`1.12`) | [kami](https://github.com/guregu/kami) (**2.2**) | 15.46 ms | **12.96** ms | 27.71 ms | 55.22 ms | 108.89 ms | **10381.67** | 
| `go` (`1.12`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 16.04 ms | **13.03** ms | 30.69 ms | 60.11 ms | 149.34 ms | **11944.00** | 
| `go` (`1.12`) | [echo](https://echo.labstack.com) (**4.1**) | 16.33 ms | **13.11** ms | 31.84 ms | 58.46 ms | 113.48 ms | **11809.67** | 
| `go` (`1.12`) | [violetear](https://violetear.org) (**7.0**) | 16.47 ms | **13.80** ms | 30.41 ms | 57.83 ms | 143.30 ms | **11329.00** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 75.97 ms | **13.90** ms | 220.68 ms | 795.30 ms | 6063.46 ms | **236137.00** | 
| `go` (`1.12`) | [gf](https://goframe.org) (**1.8**) | 16.17 ms | **13.90** ms | 29.44 ms | 60.37 ms | 169.10 ms | **11492.00** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 19.45 ms | **14.18** ms | 38.27 ms | 68.27 ms | 193.64 ms | **15199.00** | 
| `node` (`12.7`) | [rayo](https://rayo.js.org) (**1.3**) | 32.29 ms | **14.40** ms | 27.99 ms | 646.10 ms | 1433.64 ms | **104353.00** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 16.34 ms | **14.65** ms | 30.46 ms | 64.26 ms | 328.36 ms | **14370.00** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 84.62 ms | **15.27** ms | 254.60 ms | 920.83 ms | 6179.28 ms | **254653.00** | 
| `node` (`12.7`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 45.98 ms | **15.56** ms | 32.50 ms | 987.98 ms | 1895.36 ms | **158950.67** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 87.18 ms | **16.69** ms | 271.34 ms | 935.47 ms | 6202.35 ms | **237094.67** | 
| `node` (`12.7`) | [koa](https://koajs.com) (**2.7**) | 43.62 ms | **16.72** ms | 36.09 ms | 927.28 ms | 1960.27 ms | **150074.33** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 24.21 ms | **17.01** ms | 40.92 ms | 119.65 ms | 264.32 ms | **20808.00** | 
| `node` (`12.7`) | [foxify](https://foxify.js.org) (**0.1**) | 46.80 ms | **18.32** ms | 35.96 ms | 823.48 ms | 1703.17 ms | **140903.67** | 
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 19.00 ms | **18.52** ms | 23.16 ms | 29.24 ms | 120.38 ms | **4253.67** | 
| `node` (`12.7`) | [fastify](https://fastify.io) (**2.6**) | 57.77 ms | **18.95** ms | 38.93 ms | 1006.17 ms | 1843.64 ms | **173221.67** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 21.09 ms | **19.63** ms | 30.80 ms | 49.94 ms | 97.34 ms | **8275.00** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 25.20 ms | **19.71** ms | 41.14 ms | 81.66 ms | 498.08 ms | **22510.67** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 113.27 ms | **21.05** ms | 388.31 ms | 1143.11 ms | 6340.06 ms | **270313.00** | 
| `node` (`12.7`) | [restify](https://restify.com) (**8.2**) | 33.47 ms | **21.15** ms | 38.95 ms | 430.30 ms | 1231.38 ms | **74536.67** | 
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 26.30 ms | **21.48** ms | 52.31 ms | 88.45 ms | 181.79 ms | **19659.67** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 25.95 ms | **21.82** ms | 40.66 ms | 117.99 ms | 957.75 ms | **34280.00** | 
| `node` (`12.7`) | [express](https://expressjs.com) (**4.17**) | 55.23 ms | **21.97** ms | 46.33 ms | 1056.93 ms | 2230.56 ms | **172646.33** | 
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 138.01 ms | **23.82** ms | 52.75 ms | 3022.27 ms | 4975.52 ms | **509877.33** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 25.74 ms | **24.29** ms | 37.79 ms | 57.52 ms | 168.22 ms | **10091.00** | 
| `crystal` (`0.28`) | [lucky](https://luckyframework.org) (**0.14**) | 28.58 ms | **26.80** ms | 34.11 ms | 36.96 ms | 357.69 ms | **11550.67** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.5**) | 32.03 ms | **27.93** ms | 53.59 ms | 88.56 ms | 273.72 ms | **18052.00** | 
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 33.99 ms | **28.24** ms | 34.22 ms | 252.68 ms | 1279.43 ms | **60014.67** | 
| `node` (`12.7`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 31.52 ms | **30.94** ms | 33.05 ms | 36.59 ms | 521.74 ms | **13708.67** | 
| `node` (`12.7`) | [moleculer](https://moleculer.services) (**0.13**) | 84.80 ms | **31.19** ms | 59.11 ms | 1638.49 ms | 3222.17 ms | **270596.33** | 
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 33.68 ms | **33.24** ms | 43.06 ms | 55.40 ms | 82.65 ms | **7577.67** | 
| `node` (`12.7`) | [hapi](https://hapijs.com) (**18.1**) | 206.33 ms | **36.12** ms | 347.89 ms | 3186.95 ms | 4818.15 ms | **593631.67** | 
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 59.20 ms | **37.95** ms | 68.60 ms | 756.97 ms | 2801.60 ms | **151435.33** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.33**) | 44.58 ms | **41.83** ms | 64.08 ms | 101.37 ms | 220.78 ms | **17020.00** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 49.78 ms | **47.97** ms | 67.32 ms | 101.63 ms | 241.34 ms | **15227.00** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 60.01 ms | **50.87** ms | 98.60 ms | 165.95 ms | 539.32 ms | **32955.33** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.5**) | 51.70 ms | **51.51** ms | 77.06 ms | 117.30 ms | 170.28 ms | **21293.33** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 65.75 ms | **53.26** ms | 112.78 ms | 191.28 ms | 511.92 ms | **38090.33** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 58.73 ms | **57.09** ms | 83.51 ms | 130.62 ms | 517.36 ms | **26618.00** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 68.42 ms | **63.37** ms | 109.77 ms | 153.06 ms | 294.83 ms | **29952.00** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.9**) | 115.13 ms | **111.38** ms | 174.05 ms | 253.81 ms | 349.45 ms | **46275.00** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 175.02 ms | **134.14** ms | 212.42 ms | 1260.87 ms | 2468.73 ms | **209841.33** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 147.37 ms | **134.57** ms | 210.46 ms | 304.94 ms | 483.81 ms | **47320.33** | 
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 321.51 ms | **158.70** ms | 198.75 ms | 4604.86 ms | 7058.22 ms | **764980.00** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 266.06 ms | **163.06** ms | 283.97 ms | 3020.75 ms | 5002.33 ms | **485102.00** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 252.38 ms | **170.00** ms | 281.81 ms | 2415.38 ms | 3863.86 ms | **378685.00** | 
| `python` (`3.7`) | [cyclone](https://cyclone.io) (**0.0**) | 308.46 ms | **194.29** ms | 253.12 ms | 4410.81 ms | 6982.56 ms | **696555.00** | 

### Requests per seconds


#### Ranking (top 5)


:one: (sifrr) (node)


:two: (agoo-c) (c)


:three: (japronto) (python)


:four: (kore) (c)


:five: (nanoexpress) (node)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| `node` (`12.7`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 176261.33 | **155.18** MB |
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 175676.67 | **101.52** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 148777.67 | **178.12** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 132825.33 | **345.06** MB |
| `node` (`12.7`) | [nanoexpress](https://nanoexpress.js.org) (**0.12**) | 129591.67 | **113.83** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 122192.67 | **219.99** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 120318.33 | **207.67** MB |
| `go` (`1.12`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 115837.67 | **185.27** MB |
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.4**) | 115194.33 | **122.83** MB |
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 113167.00 | **106.29** MB |
| `python` (`3.6`) | [vibora](https://vibora.io) (**0.0**) | 112713.33 | **127.91** MB |
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.25**) | 110274.00 | **180.07** MB |
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 109444.67 | **102.84** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 106665.33 | **103.45** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 99493.00 | **162.05** MB |
| `rust` (`1.36`) | [gotham](https://gotham.rs) (**0.4**) | 99189.67 | **203.75** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.9**) | 98121.00 | **56.78** MB |
| `rust` (`1.36`) | [actix-web](https://actix.rs) (**1.0**) | 97041.67 | **141.80** MB |
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 96687.33 | **157.83** MB |
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 96456.67 | **193.92** MB |
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.28**) | 89218.67 | **163.20** MB |
| `rust` (`1.36`) | [iron](https://ironframework.io) (**0.6**) | 87434.33 | **110.24** MB |
| `node` (`12.7`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 84907.67 | **127.28** MB |
| `rust` (`1.36`) | [nickel](https://nickel-org.github.io) (**0.11**) | 83491.33 | **165.90** MB |
| `go` (`1.12`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 73923.33 | **99.96** MB |
| `go` (`1.12`) | [gin](https://gin-gonic.com) (**1.4**) | 73818.00 | **129.54** MB |
| `node` (`12.7`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 73439.67 | **109.94** MB |
| `node` (`12.7`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 71442.33 | **107.00** MB |
| `go` (`1.12`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.0**) | 69752.67 | **93.60** MB |
| `node` (`12.7`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 68904.00 | **103.23** MB |
| `go` (`1.12`) | [beego](https://beego.me) (**1.12**) | 66384.67 | **90.18** MB |
| `go` (`1.12`) | [gf](https://goframe.org) (**1.8**) | 64162.33 | **97.35** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 63333.00 | **98.61** MB |
| `go` (`1.12`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 62950.33 | **83.54** MB |
| `go` (`1.12`) | [kami](https://github.com/guregu/kami) (**2.2**) | 62296.00 | **83.16** MB |
| `go` (`1.12`) | [echo](https://echo.labstack.com) (**4.1**) | 58978.33 | **103.51** MB |
| `node` (`12.7`) | [rayo](https://rayo.js.org) (**1.3**) | 58824.33 | **88.14** MB |
| `go` (`1.12`) | [violetear](https://violetear.org) (**7.0**) | 56617.67 | **75.07** MB |
| `node` (`12.7`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 55873.67 | **83.72** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 55848.67 | **130.94** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 53963.33 | **115.96** MB |
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 51626.67 | **48.56** MB |
| `node` (`12.7`) | [koa](https://koajs.com) (**2.7**) | 49637.33 | **104.99** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 47589.67 | **97.80** MB |
| `node` (`12.7`) | [foxify](https://foxify.js.org) (**0.1**) | 47108.00 | **99.03** MB |
| `node` (`12.7`) | [fastify](https://fastify.io) (**2.6**) | 46928.33 | **122.69** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 46202.00 | **113.66** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 44558.00 | **33.77** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 42349.33 | **74.23** MB |
| `node` (`12.7`) | [restify](https://restify.com) (**8.2**) | 41363.67 | **72.51** MB |
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 41041.67 | **87.50** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**3.12**) | 39008.67 | **193.63** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 38957.67 | **193.41** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 38675.67 | **83.23** MB |
| `node` (`12.7`) | [express](https://expressjs.com) (**4.17**) | 38172.67 | **93.33** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 36522.00 | **181.19** MB |
| `crystal` (`0.28`) | [lucky](https://luckyframework.org) (**0.14**) | 35290.00 | **43.53** MB |
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 34863.00 | **58.41** MB |
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 34507.33 | **64.11** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 32545.33 | **169.27** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 32490.33 | **161.17** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.5**) | 31784.00 | **78.76** MB |
| `node` (`12.7`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 31658.33 | **29.71** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.22**) | 30804.33 | **29.41** MB |
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 29471.33 | **75.65** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 28967.00 | **34.18** MB |
| `node` (`12.7`) | [moleculer](https://moleculer.services) (**0.13**) | 27904.00 | **47.65** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 23966.67 | **125.47** MB |
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 23813.33 | **45.03** MB |
| `node` (`12.7`) | [hapi](https://hapijs.com) (**18.1**) | 23640.67 | **61.27** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.33**) | 22491.67 | **48.52** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 19723.00 | **51.82** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.5**) | 19451.00 | **44.11** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 19269.67 | **11.12** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 17332.67 | **32.23** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 17183.00 | **9.92** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 16887.33 | **32.57** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 15451.00 | **38.09** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 14568.00 | **25.95** MB |
| `rust` (`nightly`) | [rocket](https://rocket.rs) (**0.4**) | 13646.33 | **23.55** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 12369.00 | **32.12** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 11420.33 | **86.54** MB |
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 9082.33 | **11.63** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.9**) | 8623.00 | **17.21** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 6684.67 | **14.60** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 6582.33 | **19.08** MB |
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 5869.00 | **7.35** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 5271.67 | **15.49** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 5016.67 | **12.34** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**5.2**) | 3393.00 | **10.40** MB |
| `python` (`3.7`) | [cyclone](https://cyclone.io) (**0.0**) | 2691.67 | **7.31** MB |
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

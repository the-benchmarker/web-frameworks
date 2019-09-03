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
Last update: 2019-09-03
```
OS: Linux (version: 5.2.9-200.fc30.x86_64, arch: x86_64)
CPU Cores: 12
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: iron (rust)


:three: swifter (swift)


:four: rocket (rust)


:five: cuba (ruby)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 0.07 ms | **0.06** ms | 0.12 ms | 0.15 ms | 3.30 ms | **43.00** | 
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 0.44 ms | **0.24** ms | 1.07 ms | 2.77 ms | 16.18 ms | **588.67** | 
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 4.05 ms | **0.77** ms | 14.45 ms | 15.47 ms | 2534.78 ms | **38610.33** | 
| `rust` (`nightly`) | [rocket](https://rocket.rs) (**0.4**) | 91.59 ms | **1.47** ms | 4.17 ms | 3000.24 ms | 6593.07 ms | **533968.67** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 3.46 ms | **1.66** ms | 9.05 ms | 18.47 ms | 54.01 ms | **4235.33** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.23**) | 2.99 ms | **1.78** ms | 7.47 ms | 14.52 ms | 41.02 ms | **3380.33** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 3.31 ms | **1.85** ms | 7.80 ms | 17.66 ms | 39.93 ms | **3771.00** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 2.95 ms | **1.85** ms | 6.79 ms | 14.21 ms | 43.79 ms | **3186.33** | 
| `node` (`12.9`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 3.58 ms | **2.05** ms | 8.85 ms | 18.89 ms | 46.95 ms | **4138.33** | 
| `go` (`1.12`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 3.21 ms | **2.33** ms | 5.77 ms | 12.44 ms | 224.25 ms | **4674.33** | 
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 3.16 ms | **2.37** ms | 6.25 ms | 14.57 ms | 40.10 ms | **3017.33** | 
| `go` (`1.12`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 3.19 ms | **2.38** ms | 5.82 ms | 12.68 ms | 105.61 ms | **3364.00** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 4.23 ms | **2.41** ms | 10.82 ms | 21.25 ms | 59.87 ms | **4970.00** | 
| `go` (`1.12`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 3.26 ms | **2.42** ms | 5.82 ms | 13.06 ms | 210.49 ms | **4493.67** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 4.05 ms | **2.65** ms | 9.27 ms | 19.30 ms | 142.40 ms | **5214.00** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 6.11 ms | **2.78** ms | 7.42 ms | 76.65 ms | 866.14 ms | **30530.67** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 3.81 ms | **2.85** ms | 7.51 ms | 18.80 ms | 43.48 ms | **3689.33** | 
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 3.99 ms | **2.99** ms | 8.20 ms | 16.27 ms | 42.62 ms | **3371.33** | 
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 3.73 ms | **3.03** ms | 7.26 ms | 14.43 ms | 30.82 ms | **2990.00** | 
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 3.44 ms | **3.04** ms | 6.14 ms | 12.07 ms | 29.00 ms | **2523.33** | 
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 4.04 ms | **3.23** ms | 7.42 ms | 15.68 ms | 202.65 ms | **5949.67** | 
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 3.81 ms | **3.49** ms | 6.56 ms | 13.36 ms | 36.17 ms | **2685.67** | 
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 4.13 ms | **3.86** ms | 6.94 ms | 14.48 ms | 31.01 ms | **2771.67** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 8.06 ms | **3.89** ms | 21.22 ms | 43.34 ms | 113.21 ms | **9896.67** | 
| `go` (`1.12`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 4.97 ms | **3.99** ms | 9.00 ms | 19.93 ms | 274.51 ms | **5280.33** | 
| `go` (`1.12`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 4.92 ms | **4.06** ms | 9.28 ms | 17.63 ms | 223.45 ms | **6597.33** | 
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 4.65 ms | **4.27** ms | 8.15 ms | 17.25 ms | 40.44 ms | **3209.33** | 
| `go` (`1.12`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 5.50 ms | **4.31** ms | 10.51 ms | 23.70 ms | 118.64 ms | **5119.67** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 5.68 ms | **4.32** ms | 10.63 ms | 19.40 ms | 211.70 ms | **5583.00** | 
| `go` (`1.12`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 5.46 ms | **4.33** ms | 10.14 ms | 22.23 ms | 192.96 ms | **5701.00** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.1**) | 6.14 ms | **4.33** ms | 9.77 ms | 69.67 ms | 111.77 ms | **10204.33** | 
| `go` (`1.12`) | [gin](https://gin-gonic.com) (**1.4**) | 5.61 ms | **4.41** ms | 10.86 ms | 23.88 ms | 146.23 ms | **5355.67** | 
| `go` (`1.12`) | [violetear](https://violetear.org) (**7.0**) | 5.56 ms | **4.43** ms | 10.02 ms | 23.78 ms | 216.91 ms | **5876.67** | 
| `go` (`1.12`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 5.83 ms | **4.44** ms | 11.44 ms | 25.37 ms | 217.74 ms | **6173.00** | 
| `go` (`1.12`) | [goroute](https://goroute.github.io) (**0.0**) | 5.58 ms | **4.45** ms | 10.65 ms | 23.13 ms | 133.79 ms | **4979.00** | 
| `go` (`1.12`) | [echo](https://echo.labstack.com) (**4.1**) | 5.71 ms | **4.51** ms | 11.03 ms | 24.15 ms | 112.80 ms | **4727.33** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 6.30 ms | **4.51** ms | 7.49 ms | 70.47 ms | 231.75 ms | **13387.33** | 
| `go` (`1.12`) | [kami](https://github.com/guregu/kami) (**2.2**) | 5.66 ms | **4.51** ms | 9.56 ms | 22.25 ms | 180.55 ms | **7038.67** | 
| `go` (`1.12`) | [beego](https://beego.me) (**1.12**) | 5.89 ms | **4.52** ms | 11.26 ms | 24.93 ms | 171.32 ms | **5977.33** | 
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 4.73 ms | **4.69** ms | 7.14 ms | 12.66 ms | 38.38 ms | **2437.67** | 
| `go` (`1.12`) | [air](https://github.com/aofei/air) (**0.12**) | 6.89 ms | **4.91** ms | 13.44 ms | 30.33 ms | 194.40 ms | **8569.67** | 
| `go` (`1.12`) | [gf](https://goframe.org) (**1.8**) | 7.04 ms | **5.04** ms | 13.74 ms | 31.86 ms | 192.13 ms | **8047.00** | 
| `node` (`12.9`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 6.50 ms | **5.12** ms | 10.13 ms | 21.32 ms | 227.88 ms | **5305.67** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 170.10 ms | **5.20** ms | 18.20 ms | 4495.57 ms | 7380.65 ms | **753266.33** | 
| `node` (`12.9`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 7.01 ms | **5.46** ms | 11.59 ms | 22.91 ms | 244.72 ms | **6845.33** | 
| `node` (`12.9`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 7.91 ms | **5.76** ms | 11.22 ms | 29.06 ms | 371.56 ms | **13854.33** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 8.67 ms | **5.89** ms | 16.87 ms | 36.75 ms | 255.81 ms | **9044.67** | 
| `node` (`12.9`) | [rayo](https://rayo.js.org) (**1.3**) | 7.54 ms | **5.97** ms | 11.81 ms | 23.79 ms | 257.22 ms | **7276.67** | 
| `node` (`12.9`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 8.12 ms | **6.41** ms | 13.21 ms | 27.76 ms | 311.32 ms | **9376.00** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 8.49 ms | **6.81** ms | 17.68 ms | 33.58 ms | 220.93 ms | **8029.00** | 
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 7.60 ms | **6.92** ms | 10.58 ms | 15.50 ms | 74.70 ms | **2445.33** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 7.41 ms | **6.93** ms | 15.99 ms | 28.18 ms | 64.19 ms | **6617.67** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 8.91 ms | **7.11** ms | 20.87 ms | 40.37 ms | 116.39 ms | **9259.33** | 
| `node` (`12.9`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 10.65 ms | **7.54** ms | 14.19 ms | 62.84 ms | 567.81 ms | **25031.33** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 9.86 ms | **7.78** ms | 18.56 ms | 36.51 ms | 287.13 ms | **9366.33** | 
| `node` (`12.9`) | [foxify](https://foxify.js.org) (**0.1**) | 10.12 ms | **8.26** ms | 14.73 ms | 35.23 ms | 430.09 ms | **15861.33** | 
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 11.46 ms | **8.74** ms | 24.12 ms | 47.06 ms | 110.97 ms | **9968.00** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 10.43 ms | **8.87** ms | 18.83 ms | 34.37 ms | 424.68 ms | **11500.33** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 28.13 ms | **8.89** ms | 57.19 ms | 294.18 ms | 504.35 ms | **58363.67** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 29.58 ms | **9.05** ms | 61.73 ms | 304.21 ms | 651.77 ms | **61600.67** | 
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 9.21 ms | **9.13** ms | 11.67 ms | 15.28 ms | 132.35 ms | **3571.67** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 29.33 ms | **9.14** ms | 60.07 ms | 300.91 ms | 670.11 ms | **60632.00** | 
| `node` (`12.9`) | [fastify](https://fastify.io) (**2.8**) | 12.66 ms | **9.21** ms | 18.66 ms | 59.31 ms | 597.88 ms | **23846.67** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 11.24 ms | **9.26** ms | 17.73 ms | 33.89 ms | 329.19 ms | **9947.00** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 30.05 ms | **9.53** ms | 61.64 ms | 307.17 ms | 596.18 ms | **61433.00** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 12.90 ms | **9.56** ms | 25.12 ms | 45.43 ms | 284.99 ms | **11407.00** | 
| `node` (`12.9`) | [koa](https://koajs.com) (**2.8**) | 11.75 ms | **9.63** ms | 18.56 ms | 36.86 ms | 344.51 ms | **10867.00** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 32.75 ms | **9.63** ms | 62.76 ms | 357.46 ms | 549.25 ms | **71087.00** | 
| `node` (`12.9`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 13.44 ms | **10.16** ms | 19.51 ms | 78.96 ms | 488.79 ms | **21055.00** | 
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 15.91 ms | **10.45** ms | 19.87 ms | 138.98 ms | 1038.47 ms | **41577.00** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 11.51 ms | **11.09** ms | 17.19 ms | 26.34 ms | 78.35 ms | **4774.00** | 
| `node` (`12.9`) | [express](https://expressjs.com) (**4.17**) | 15.37 ms | **11.51** ms | 22.04 ms | 96.54 ms | 538.12 ms | **24496.67** | 
| `node` (`12.9`) | [restify](https://restify.com) (**8.4**) | 12.14 ms | **11.53** ms | 15.48 ms | 28.89 ms | 213.58 ms | **6713.33** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 56.93 ms | **11.97** ms | 106.08 ms | 730.72 ms | 1253.26 ms | **145620.00** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 12.78 ms | **12.38** ms | 18.29 ms | 25.22 ms | 198.50 ms | **5162.67** | 
| `node` (`12.9`) | [moleculer](https://moleculer.services) (**0.13**) | 17.18 ms | **13.88** ms | 25.94 ms | 53.70 ms | 404.96 ms | **13768.33** | 
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 19.48 ms | **17.35** ms | 18.58 ms | 37.85 ms | 868.63 ms | **33693.33** | 
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 28.67 ms | **17.95** ms | 30.65 ms | 392.59 ms | 1575.53 ms | **87135.00** | 
| `node` (`12.9`) | [hapi](https://hapijs.com) (**18.1**) | 25.23 ms | **18.61** ms | 36.15 ms | 141.59 ms | 785.96 ms | **36155.67** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 44.85 ms | **18.85** ms | 123.94 ms | 289.03 ms | 747.84 ms | **63011.33** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 21.87 ms | **19.35** ms | 36.76 ms | 57.51 ms | 155.52 ms | **11442.33** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 26.69 ms | **21.17** ms | 43.38 ms | 73.37 ms | 471.25 ms | **18255.33** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 22.80 ms | **21.20** ms | 30.71 ms | 48.77 ms | 83.27 ms | **6818.67** | 
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 21.33 ms | **21.68** ms | 23.30 ms | 24.88 ms | 172.67 ms | **2485.00** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 31.98 ms | **22.48** ms | 67.14 ms | 106.27 ms | 276.15 ms | **22873.67** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.5**) | 25.39 ms | **24.83** ms | 34.85 ms | 41.14 ms | 131.18 ms | **7329.00** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 29.71 ms | **26.96** ms | 50.06 ms | 78.88 ms | 130.70 ms | **15423.00** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 31.99 ms | **29.21** ms | 53.25 ms | 80.63 ms | 258.47 ms | **16934.67** | 
| `node` (`12.9`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 34.75 ms | **33.46** ms | 37.93 ms | 72.71 ms | 595.14 ms | **21657.67** | 
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 48.93 ms | **49.02** ms | 52.00 ms | 53.73 ms | 129.27 ms | **3811.00** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 79.14 ms | **56.72** ms | 159.37 ms | 232.44 ms | 530.30 ms | **53567.67** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.1**) | 63.37 ms | **56.99** ms | 105.87 ms | 165.24 ms | 304.55 ms | **31386.33** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 64.91 ms | **61.19** ms | 90.76 ms | 110.73 ms | 260.76 ms | **19224.67** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 94.79 ms | **71.68** ms | 175.89 ms | 271.38 ms | 571.85 ms | **57738.00** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 89.56 ms | **85.52** ms | 113.18 ms | 200.91 ms | 719.21 ms | **33988.00** | 
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 187.66 ms | **186.53** ms | 216.90 ms | 241.44 ms | 306.25 ms | **23430.00** | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (japronto) (python)


:three: (sifrr) (node)


:four: (drogon) (cpp)


:five: (atreugo) (go)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 381889.33 | **220.85** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 353488.33 | **423.25** MB |
| `node` (`12.9`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 328818.67 | **289.45** MB |
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 321614.00 | **311.99** MB |
| `go` (`1.12`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 302647.00 | **608.70** MB |
| `go` (`1.12`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 300084.00 | **482.84** MB |
| `go` (`1.12`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 295165.67 | **473.42** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 276355.00 | **718.21** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 275970.67 | **497.03** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 274990.33 | **266.77** MB |
| `crystal` (`0.29`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 273513.67 | **257.07** MB |
| `crystal` (`0.29`) | [raze](https://razecr.com) (**0.3**) | 265147.67 | **249.25** MB |
| `crystal` (`0.29`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 258050.00 | **274.28** MB |
| `rust` (`1.37`) | [gotham](https://gotham.rs) (**0.4**) | 248619.67 | **508.39** MB |
| `crystal` (`0.29`) | [kemal](https://kemalcr.com) (**0.28**) | 247544.00 | **404.74** MB |
| `crystal` (`0.29`) | [amber](https://amberframework.org) (**0.3**) | 231083.67 | **423.01** MB |
| `nim` (`0.2`) | [jester](https://github.com/dom96/jester) (**0.4**) | 229367.33 | **461.18** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 217253.33 | **374.89** MB |
| `crystal` (`0.29`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 210717.33 | **344.64** MB |
| `go` (`1.12`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 206870.67 | **515.84** MB |
| `go` (`1.12`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 203059.33 | **271.66** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.1**) | 202779.00 | **117.34** MB |
| `go` (`1.12`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 191250.67 | **254.68** MB |
| `rust` (`1.37`) | [iron](https://ironframework.io) (**0.6**) | 190952.00 | **241.04** MB |
| `go` (`1.12`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 189682.33 | **253.58** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 188008.67 | **306.28** MB |
| `go` (`1.12`) | [goroute](https://goroute.github.io) (**0.0**) | 184640.33 | **323.65** MB |
| `go` (`1.12`) | [violetear](https://violetear.org) (**7.0**) | 184125.67 | **244.04** MB |
| `go` (`1.12`) | [gin](https://gin-gonic.com) (**1.4**) | 183241.33 | **321.22** MB |
| `go` (`1.12`) | [echo](https://echo.labstack.com) (**4.1**) | 183225.67 | **321.25** MB |
| `go` (`1.12`) | [kami](https://github.com/guregu/kami) (**2.2**) | 182665.00 | **242.56** MB |
| `go` (`1.12`) | [beego](https://beego.me) (**1.12**) | 180175.33 | **241.84** MB |
| `go` (`1.12`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 179620.67 | **239.75** MB |
| `rust` (`1.37`) | [actix-web](https://actix.rs) (**1.0**) | 178648.00 | **271.60** MB |
| `go` (`1.12`) | [air](https://github.com/aofei/air) (**0.12**) | 160276.33 | **334.23** MB |
| `rust` (`1.37`) | [nickel](https://nickel-org.github.io) (**0.11**) | 160040.00 | **318.81** MB |
| `go` (`1.12`) | [gf](https://goframe.org) (**1.8**) | 155822.67 | **235.85** MB |
| `node` (`12.9`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 150105.33 | **224.92** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 144591.33 | **312.06** MB |
| `node` (`12.9`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 144512.00 | **216.66** MB |
| `node` (`12.9`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 140138.00 | **209.94** MB |
| `node` (`12.9`) | [rayo](https://rayo.js.org) (**1.3**) | 132922.67 | **199.22** MB |
| `node` (`12.9`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 127831.33 | **191.55** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 126450.00 | **197.02** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 123756.33 | **290.14** MB |
| `node` (`12.9`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 117290.67 | **175.86** MB |
| `node` (`12.9`) | [foxify](https://foxify.js.org) (**0.1**) | 108070.00 | **227.31** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 105682.33 | **260.52** MB |
| `swift` (`5.0`) | [perfect](https://perfect.org) (**3.1**) | 105235.67 | **99.00** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 99453.00 | **74.89** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 98990.67 | **173.49** MB |
| `node` (`12.9`) | [fastify](https://fastify.io) (**2.8**) | 96229.67 | **254.46** MB |
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 96005.00 | **204.87** MB |
| `node` (`12.9`) | [koa](https://koajs.com) (**2.8**) | 86386.33 | **182.99** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 85025.33 | **171.02** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 84848.00 | **422.00** MB |
| `node` (`12.9`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 84386.00 | **343.04** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 83973.33 | **417.21** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 82366.33 | **409.24** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 81730.33 | **202.77** MB |
| `node` (`12.9`) | [restify](https://restify.com) (**8.4**) | 80654.33 | **141.64** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 80191.00 | **417.56** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 80072.33 | **397.70** MB |
| `swift` (`5.0`) | [vapor](https://vapor.codes) (**3.3**) | 79661.33 | **133.17** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 75187.33 | **162.11** MB |
| `node` (`12.9`) | [express](https://expressjs.com) (**4.17**) | 73665.33 | **180.49** MB |
| `rust` (`nightly`) | [rocket](https://rocket.rs) (**0.4**) | 64971.67 | **102.57** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 64917.67 | **338.92** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.23**) | 63274.33 | **60.46** MB |
| `node` (`12.9`) | [moleculer](https://moleculer.services) (**0.13**) | 59373.33 | **101.68** MB |
| `swift` (`5.0`) | [kitura](https://kitura.io) (**2.7**) | 56635.33 | **105.02** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 55140.33 | **65.17** MB |
| `swift` (`5.0`) | [kitura-nio](https://kitura.io) (**2.7**) | 49825.00 | **94.23** MB |
| `node` (`12.9`) | [hapi](https://hapijs.com) (**18.1**) | 46356.33 | **120.29** MB |
| `crystal` (`0.29`) | [lucky](https://luckyframework.org) (**0.16**) | 46229.33 | **56.75** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.38**) | 46113.00 | **99.76** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 45273.67 | **26.18** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 42813.33 | **112.69** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.5**) | 38540.00 | **87.44** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 37921.00 | **93.50** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 34625.33 | **64.41** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 33797.33 | **60.34** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 31296.33 | **60.56** MB |
| `node` (`12.9`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 28617.33 | **26.83** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 25932.67 | **196.34** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 23730.67 | **13.73** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 21542.67 | **56.01** MB |
| `crystal` (`0.29`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 20076.67 | **25.09** MB |
| `swift` (`5.0`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 18254.67 | **23.39** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.1**) | 15887.67 | **31.73** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 15030.67 | **32.81** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 13270.00 | **38.48** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 10829.33 | **32.14** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 10803.33 | **26.65** MB |
| `crystal` (`0.29`) | [onyx](https://onyxframework.org) (**0.5**) | 5211.67 | **13.46** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 4357.33 | **27.54** MB |
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

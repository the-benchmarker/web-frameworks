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
Last update: 2019-10-02
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
| `rust` (`1.38`) | [nickel](https://nickel-org.github.io) (**0.11**) | **0.11** ms | 0.11 ms | 0.15 ms | 0.24 ms | 6.47 ms | **100.00** | 
| `rust` (`1.38`) | [iron](https://ironframework.io) (**0.6**) | **0.51** ms | 0.45 ms | 0.86 ms | 1.91 ms | 27.02 ms | **460.33** | 
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | **2.71** ms | 0.71 ms | 7.67 ms | 17.32 ms | 58.82 ms | **3881.67** | 
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.24**) | **2.76** ms | 0.78 ms | 7.76 ms | 17.16 ms | 58.84 ms | **3867.67** | 
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | **3.04** ms | 0.78 ms | 8.56 ms | 18.08 ms | 56.41 ms | **4149.67** | 
| `swift` (`5.1`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | **3.82** ms | 0.87 ms | 14.53 ms | 15.30 ms | 1847.24 ms | **27770.00** | 
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | **3.90** ms | 0.95 ms | 11.18 ms | 26.37 ms | 100.04 ms | **5875.00** | 
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | **4.04** ms | 3.60 ms | 7.84 ms | 13.48 ms | 35.21 ms | **2883.67** | 
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | **4.22** ms | 3.70 ms | 8.97 ms | 15.11 ms | 33.15 ms | **3441.33** | 
| `nim` (`1.0`) | [httpbeast](https://github.com/dom96/httpbeast) (**0.2**) | **4.64** ms | 4.10 ms | 8.34 ms | 13.81 ms | 30.35 ms | **2772.67** | 
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | **4.95** ms | 4.26 ms | 9.83 ms | 17.84 ms | 61.15 ms | **4005.00** | 
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | **5.32** ms | 0.51 ms | 17.29 ms | 44.27 ms | 114.60 ms | **9561.67** | 
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | **5.43** ms | 4.76 ms | 10.08 ms | 17.99 ms | 65.22 ms | **3930.33** | 
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | **5.44** ms | 4.89 ms | 8.53 ms | 15.31 ms | 49.00 ms | **2689.33** | 
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | **5.56** ms | 4.79 ms | 8.65 ms | 16.25 ms | 254.09 ms | **6002.00** | 
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | **5.63** ms | 4.99 ms | 9.06 ms | 16.17 ms | 99.86 ms | **3126.67** | 
| `crystal` (`0.31`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | **5.69** ms | 4.91 ms | 9.78 ms | 15.99 ms | 37.16 ms | **3135.67** | 
| `crystal` (`0.31`) | [raze](https://razecr.com) (**0.3**) | **6.10** ms | 5.26 ms | 10.35 ms | 17.09 ms | 41.44 ms | **3339.67** | 
| `crystal` (`0.31`) | [toro](https://github.com/soveran/toro) (**0.4**) | **6.11** ms | 5.28 ms | 10.49 ms | 18.13 ms | 54.63 ms | **3663.67** | 
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | **6.18** ms | 5.43 ms | 10.37 ms | 17.43 ms | 57.70 ms | **3542.00** | 
| `crystal` (`0.31`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | **6.21** ms | 5.24 ms | 10.92 ms | 18.01 ms | 43.80 ms | **3610.00** | 
| `crystal` (`0.31`) | [kemal](https://kemalcr.com) (**0.28**) | **6.29** ms | 5.39 ms | 10.60 ms | 17.42 ms | 49.38 ms | **3387.33** | 
| `crystal` (`0.31`) | [amber](https://amberframework.org) (**0.3**) | **6.69** ms | 5.79 ms | 11.02 ms | 18.08 ms | 41.26 ms | **3437.67** | 
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | **6.93** ms | 1.59 ms | 19.38 ms | 40.93 ms | 130.18 ms | **9426.67** | 
| `nim` (`1.0`) | [jester](https://github.com/dom96/jester) (**0.4**) | **7.09** ms | 6.48 ms | 10.65 ms | 16.49 ms | 39.73 ms | **2911.00** | 
| `crystal` (`0.31`) | [orion](https://github.com/obsidian/orion) (**1.7**) | **7.66** ms | 6.71 ms | 13.01 ms | 21.81 ms | 55.04 ms | **4211.33** | 
| `rust` (`1.38`) | [gotham](https://gotham.rs) (**0.4**) | **7.77** ms | 7.08 ms | 13.26 ms | 21.64 ms | 165.41 ms | **5416.33** | 
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | **8.08** ms | 6.84 ms | 13.59 ms | 25.41 ms | 119.61 ms | **5010.00** | 
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | **8.73** ms | 7.73 ms | 13.29 ms | 26.74 ms | 153.16 ms | **6403.67** | 
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | **8.76** ms | 1.69 ms | 25.94 ms | 57.21 ms | 146.88 ms | **12907.33** | 
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | **9.09** ms | 4.73 ms | 12.86 ms | 87.73 ms | 232.50 ms | **16903.67** | 
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.11**) | **9.27** ms | 6.32 ms | 15.30 ms | 83.08 ms | 122.61 ms | **12523.67** | 
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | **9.28** ms | 8.02 ms | 14.85 ms | 33.06 ms | 90.48 ms | **5715.67** | 
| `ruby` (`2.6`) | [grape](https://ruby-grape.org) (**1.2**) | **9.33** ms | 1.10 ms | 27.96 ms | 58.40 ms | 151.41 ms | **13617.67** | 
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | **9.52** ms | 7.84 ms | 15.80 ms | 36.99 ms | 181.34 ms | **7616.00** | 
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | **9.88** ms | 8.03 ms | 17.14 ms | 37.84 ms | 178.56 ms | **7147.00** | 
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | **9.92** ms | 8.64 ms | 15.22 ms | 33.31 ms | 155.54 ms | **5938.33** | 
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | **10.12** ms | 8.29 ms | 17.30 ms | 40.24 ms | 91.25 ms | **7061.67** | 
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | **10.30** ms | 8.38 ms | 17.63 ms | 40.27 ms | 182.37 ms | **7983.67** | 
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | **10.38** ms | 8.33 ms | 18.27 ms | 41.08 ms | 182.18 ms | **7608.33** | 
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | **10.39** ms | 8.38 ms | 18.17 ms | 40.46 ms | 141.11 ms | **7803.33** | 
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | **10.39** ms | 8.31 ms | 18.41 ms | 41.01 ms | 169.34 ms | **7487.00** | 
| `rust` (`1.38`) | [actix-web](https://actix.rs) (**1.0**) | **10.61** ms | 10.27 ms | 13.99 ms | 18.99 ms | 43.52 ms | **2746.00** | 
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | **10.91** ms | 9.06 ms | 14.11 ms | 32.02 ms | 512.34 ms | **18824.00** | 
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | **11.17** ms | 8.78 ms | 20.09 ms | 46.06 ms | 144.41 ms | **8337.33** | 
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | **11.87** ms | 9.72 ms | 17.38 ms | 35.09 ms | 402.09 ms | **13080.00** | 
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.13**) | **12.19** ms | 9.57 ms | 21.95 ms | 50.11 ms | 138.08 ms | **9155.33** | 
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | **12.27** ms | 9.49 ms | 16.94 ms | 59.17 ms | 504.52 ms | **20083.00** | 
| `swift` (`5.1`) | [perfect](https://perfect.org) (**3.1**) | **12.71** ms | 12.78 ms | 15.13 ms | 18.67 ms | 49.07 ms | **2267.00** | 
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | **12.93** ms | 9.55 ms | 17.56 ms | 73.82 ms | 567.04 ms | **24312.67** | 
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | **13.19** ms | 11.75 ms | 24.50 ms | 42.96 ms | 312.03 ms | **10880.67** | 
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | **14.00** ms | 9.76 ms | 30.25 ms | 57.17 ms | 178.09 ms | **11992.33** | 
| `go` (`1.13`) | [gf](https://goframe.org) (**1.9**) | **14.02** ms | 11.06 ms | 24.66 ms | 58.07 ms | 289.21 ms | **11563.67** | 
| `php` (`7.3`) | [one](https://github.com/lizhichao/one) (**1.8**) | **14.27** ms | 13.25 ms | 23.71 ms | 40.21 ms | 115.91 ms | **7981.00** | 
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | **15.21** ms | 11.08 ms | 20.25 ms | 109.68 ms | 656.82 ms | **28578.00** | 
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | **16.32** ms | 5.24 ms | 10.53 ms | 433.15 ms | 1465.00 ms | **90864.00** | 
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | **16.44** ms | 11.04 ms | 20.36 ms | 154.75 ms | 775.43 ms | **39290.00** | 
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | **17.54** ms | 12.44 ms | 22.23 ms | 146.77 ms | 806.60 ms | **38449.67** | 
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | **17.77** ms | 17.92 ms | 18.53 ms | 22.64 ms | 373.71 ms | **7745.33** | 
| `ruby` (`2.6`) | [plezi](https://github.com/boazsegev/plezi) (**0.16**) | **17.79** ms | 15.20 ms | 27.75 ms | 50.34 ms | 114.43 ms | **8541.00** | 
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | **17.96** ms | 13.52 ms | 23.78 ms | 107.21 ms | 756.40 ms | **33762.33** | 
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | **18.04** ms | 13.06 ms | 36.84 ms | 68.41 ms | 200.53 ms | **13923.67** | 
| `python` (`3.7`) | [asgineer](https://asgineer.readthedocs.io) (**0.7**) | **18.40** ms | 16.86 ms | 27.91 ms | 51.70 ms | 104.49 ms | **8750.00** | 
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | **19.36** ms | 17.68 ms | 29.93 ms | 51.59 ms | 121.10 ms | **8912.33** | 
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | **19.41** ms | 14.14 ms | 31.12 ms | 73.18 ms | 680.35 ms | **24291.00** | 
| `fsharp` (`7.3`) | [suave](https://suave.io) (**2.5**) | **20.27** ms | 16.92 ms | 29.01 ms | 55.00 ms | 404.77 ms | **13227.00** | 
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | **21.00** ms | 13.84 ms | 25.35 ms | 235.21 ms | 914.53 ms | **48954.00** | 
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | **21.20** ms | 16.62 ms | 32.08 ms | 70.56 ms | 1309.03 ms | **46303.00** | 
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | **21.29** ms | 15.09 ms | 26.57 ms | 189.06 ms | 926.69 ms | **45814.33** | 
| `swift` (`5.1`) | [kitura](https://kitura.io) (**2.7**) | **21.52** ms | 19.61 ms | 21.30 ms | 25.65 ms | 881.76 ms | **34817.67** | 
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | **21.75** ms | 15.64 ms | 42.91 ms | 76.00 ms | 295.44 ms | **15659.33** | 
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | **22.06** ms | 14.24 ms | 25.81 ms | 282.21 ms | 929.74 ms | **52780.00** | 
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | **22.41** ms | 13.51 ms | 56.01 ms | 111.09 ms | 269.45 ms | **24794.00** | 
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | **22.75** ms | 20.12 ms | 37.42 ms | 63.27 ms | 133.01 ms | **11609.00** | 
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | **22.83** ms | 16.68 ms | 28.35 ms | 212.92 ms | 872.69 ms | **44958.00** | 
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | **23.53** ms | 20.02 ms | 25.51 ms | 129.35 ms | 625.07 ms | **27270.33** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | **24.13** ms | 22.91 ms | 32.12 ms | 51.66 ms | 94.86 ms | **7255.67** | 
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | **26.11** ms | 24.46 ms | 28.25 ms | 82.55 ms | 382.81 ms | **16153.00** | 
| `swift` (`5.1`) | [vapor](https://vapor.codes) (**3.3**) | **27.46** ms | 17.46 ms | 31.97 ms | 352.18 ms | 1677.42 ms | **77850.67** | 
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | **31.76** ms | 10.40 ms | 54.51 ms | 450.61 ms | 2027.26 ms | **100351.33** | 
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | **34.89** ms | 6.82 ms | 107.73 ms | 264.09 ms | 763.79 ms | **57617.67** | 
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.39**) | **35.08** ms | 31.80 ms | 55.97 ms | 87.23 ms | 215.86 ms | **16448.67** | 
| `crystal` (`0.31`) | [lucky](https://luckyframework.org) (**0.16**) | **37.80** ms | 35.83 ms | 45.80 ms | 54.94 ms | 275.35 ms | **10137.67** | 
| `php` (`7.3`) | [basicphp](https://github.com/ray-ang/basicphp) (**0.9**) | **42.54** ms | 14.67 ms | 80.13 ms | 426.71 ms | 696.36 ms | **84139.33** | 
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | **42.82** ms | 41.04 ms | 47.65 ms | 53.23 ms | 612.11 ms | **17486.67** | 
| `python` (`3.7`) | [clastic](https://github.com/mahmoud/clastic) (**19.9**) | **43.27** ms | 30.01 ms | 90.32 ms | 143.63 ms | 335.27 ms | **30832.33** | 
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | **43.54** ms | 27.12 ms | 42.84 ms | 552.30 ms | 1490.72 ms | **97428.33** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | **44.05** ms | 14.61 ms | 84.62 ms | 455.62 ms | 734.20 ms | **90202.33** | 
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | **44.53** ms | 30.28 ms | 92.81 ms | 146.23 ms | 356.98 ms | **29651.33** | 
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | **44.70** ms | 40.96 ms | 67.21 ms | 99.21 ms | 158.32 ms | **17162.33** | 
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | **45.82** ms | 28.94 ms | 99.21 ms | 153.12 ms | 393.38 ms | **32806.00** | 
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | **47.44** ms | 26.68 ms | 39.11 ms | 731.75 ms | 1662.36 ms | **120900.67** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**6.0**) | **49.15** ms | 16.41 ms | 89.04 ms | 504.25 ms | 818.14 ms | **98773.33** | 
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | **49.40** ms | 47.06 ms | 74.19 ms | 117.91 ms | 184.87 ms | **20736.00** | 
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | **50.73** ms | 17.05 ms | 97.02 ms | 524.80 ms | 877.47 ms | **103527.33** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | **51.49** ms | 16.07 ms | 97.00 ms | 552.30 ms | 1053.95 ms | **111172.67** | 
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | **52.79** ms | 48.72 ms | 85.79 ms | 141.54 ms | 283.94 ms | **26556.33** | 
| `swift` (`5.1`) | [kitura-nio](https://kitura.io) (**2.7**) | **57.96** ms | 25.27 ms | 39.54 ms | 1211.04 ms | 2793.02 ms | **209059.33** | 
| `php` (`7.3`) | [spiral](https://github.com/spiral/framework) (**2.1**) | **59.27** ms | 58.78 ms | 65.94 ms | 75.16 ms | 268.28 ms | **7329.33** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | **60.82** ms | 16.78 ms | 109.44 ms | 667.05 ms | 1743.55 ms | **136481.00** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.1**) | **84.25** ms | 23.97 ms | 165.85 ms | 985.64 ms | 1451.86 ms | **195353.67** | 
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | **88.85** ms | 84.03 ms | 131.59 ms | 197.33 ms | 334.58 ms | **33677.67** | 
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | **105.18** ms | 97.03 ms | 160.76 ms | 214.08 ms | 313.27 ms | **35015.67** | 
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | **108.65** ms | 99.63 ms | 147.26 ms | 253.01 ms | 1106.49 ms | **47207.67** | 
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | **114.10** ms | 105.59 ms | 154.56 ms | 348.84 ms | 1160.80 ms | **66783.67** | 
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | **143.75** ms | 120.16 ms | 239.36 ms | 360.16 ms | 928.07 ms | **71561.00** | 
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | **173.65** ms | 8.36 ms | 30.62 ms | 4227.74 ms | 7584.74 ms | **726546.67** | 
| `crystal` (`0.31`) | [onyx](https://onyxframework.org) (**0.5**) | **188.60** ms | 187.56 ms | 231.07 ms | 265.52 ms | 288.52 ms | **33124.67** | 
| `crystal` (`0.31`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | **336.33** ms | 214.69 ms | 239.15 ms | 4088.74 ms | 5762.86 ms | **650318.67** | 
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | **345.73** ms | 94.73 ms | 671.14 ms | 4486.24 ms | 7152.21 ms | **791670.67** | 

### Requests per seconds


#### Ranking (top 5)


:one: (agoo-c) (c)


:two: (sifrr) (node)


:three: (httpbeast) (nim)


:four: (japronto) (python)


:five: (kore) (c)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| `c` (`11`) | [agoo-c](https://github.com/ohler55/agoo-c) (**0.5**) | 230191.00 | **133.12** MB |
| `node` (`12.1`) | [sifrr](https://sifrr.github.io/sifrr/#/./packages/server/sifrr-server/) (**0.0**) | 217284.67 | **191.22** MB |
| `nim` (`1.0`) | [httpbeast](https://github.com/dom96/httpbeast) (**0.2**) | 199151.33 | **283.53** MB |
| `python` (`3.7`) | [japronto](https://github.com/squeaky-pl/japronto) (**0.1**) | 189440.33 | **226.86** MB |
| `c` (`99`) | [kore](https://kore.io) (**3.1**) | 173322.67 | **450.52** MB |
| `cpp` (`14/17`) | [drogon](https://github.com/an-tao/drogon) (**1.0**) | 171074.33 | **166.02** MB |
| `go` (`1.13`) | [gorouter-fasthttp](https://github.com/vardius/gorouter/wiki) (**4.1**) | 166665.67 | **268.91** MB |
| `go` (`1.13`) | [fasthttprouter](https://godoc.org/github.com/buaazp/fasthttprouter) (**0.1**) | 166039.33 | **268.03** MB |
| `crystal` (`0.31`) | [router.cr](https://github.com/tbrand/router.cr) (**0.2**) | 165377.00 | **155.54** MB |
| `java` (`8`) | [rapidoid](https://rapidoid.org) (**5.5**) | 164499.67 | **296.08** MB |
| `go` (`1.13`) | [atreugo](https://github.com/savsgio/atreugo/tree/master/docs) (**8.2**) | 160364.67 | **322.81** MB |
| `crystal` (`0.31`) | [raze](https://razecr.com) (**0.3**) | 155311.00 | **146.03** MB |
| `crystal` (`0.31`) | [spider-gazelle](https://spider-gazelle.net) (**1.6**) | 154433.33 | **164.76** MB |
| `crystal` (`0.31`) | [toro](https://github.com/soveran/toro) (**0.4**) | 154022.33 | **144.83** MB |
| `crystal` (`0.31`) | [kemal](https://kemalcr.com) (**0.28**) | 151141.00 | **246.93** MB |
| `cpp` (`11`) | [evhtp](https://criticalstack/libevhtp) (**1.2**) | 150279.00 | **145.90** MB |
| `nim` (`1.0`) | [jester](https://github.com/dom96/jester) (**0.4**) | 147292.33 | **296.12** MB |
| `crystal` (`0.31`) | [amber](https://amberframework.org) (**0.3**) | 142296.00 | **260.53** MB |
| `ruby` (`2.6`) | [agoo](https://github.com/ohler55/agoo) (**2.11**) | 133863.00 | **77.29** MB |
| `crystal` (`0.31`) | [orion](https://github.com/obsidian/orion) (**1.7**) | 125655.00 | **205.32** MB |
| `java` (`8`) | [act](https://actframework.org) (**1.8**) | 124508.33 | **214.93** MB |
| `rust` (`1.38`) | [gotham](https://gotham.rs) (**0.4**) | 122101.33 | **248.50** MB |
| `go` (`1.13`) | [gramework](https://github.com/gramework/gramework) (**1.6**) | 120067.67 | **307.19** MB |
| `rust` (`1.38`) | [iron](https://ironframework.io) (**0.6**) | 111330.67 | **140.45** MB |
| `rust` (`1.38`) | [actix-web](https://actix.rs) (**1.0**) | 109255.00 | **161.81** MB |
| `go` (`1.13`) | [kami](https://github.com/guregu/kami) (**2.2**) | 107411.67 | **142.93** MB |
| `go` (`1.13`) | [goroute](https://goroute.github.io) (**0.0**) | 106408.00 | **186.74** MB |
| `go` (`1.13`) | [echo](https://echo.labstack.com) (**4.1**) | 102971.00 | **180.70** MB |
| `go` (`1.13`) | [rte](https://github.com/jwilner/rte) (**0.0**) | 101265.67 | **135.81** MB |
| `go` (`1.13`) | [violetear](https://violetear.org) (**7.0**) | 101013.67 | **134.36** MB |
| `go` (`1.13`) | [chi](https://github.com/go-chi/chi) (**4.0**) | 99560.33 | **131.18** MB |
| `go` (`1.13`) | [gorilla-mux](https://www.gorillatoolkit.org/pkg/mux) (**1.7**) | 99528.67 | **133.12** MB |
| `go` (`1.13`) | [beego](https://beego.me) (**1.12**) | 99275.33 | **133.63** MB |
| `go` (`1.13`) | [gorouter](https://github.com/vardius/gorouter/wiki) (**4.1**) | 98337.67 | **131.42** MB |
| `csharp` (`7.3`) | [aspnetcore](https://docs.microsoft.com/en-us/aspnet/index) (**2.2**) | 95856.00 | **155.99** MB |
| `go` (`1.13`) | [gin](https://gin-gonic.com) (**1.4**) | 93794.00 | **164.57** MB |
| `node` (`12.1`) | [polkadot](https://github.com/lukeed/polkadot) (**1.0**) | 90133.00 | **135.05** MB |
| `node` (`12.1`) | [0http](https://github.com/jkyberneees/0http) (**1.2**) | 89233.00 | **133.70** MB |
| `go` (`1.13`) | [air](https://github.com/aofei/air) (**0.13**) | 87047.00 | **181.56** MB |
| `node` (`12.1`) | [restana](https://github.com/jkyberneees/ana) (**3.3**) | 86755.33 | **129.98** MB |
| `python` (`3.7`) | [falcon](https://falconframework.org) (**2.0**) | 80570.33 | **188.94** MB |
| `node` (`12.1`) | [polka](https://github.com/lukeed/polka) (**0.5**) | 78580.67 | **117.72** MB |
| `kotlin` (`1.3`) | [ktor](https://ktor.io) (**1.2**) | 77825.00 | **121.22** MB |
| `node` (`12.1`) | [rayo](https://rayo.js.org) (**1.3**) | 76882.33 | **115.18** MB |
| `go` (`1.13`) | [gf](https://goframe.org) (**1.9**) | 76237.67 | **130.41** MB |
| `swift` (`5.1`) | [perfect](https://perfect.org) (**3.1**) | 76056.33 | **71.40** MB |
| `java` (`8`) | [javalin](https://javalin.io) (**3.4**) | 75663.67 | **134.93** MB |
| `rust` (`1.38`) | [nickel](https://nickel-org.github.io) (**0.11**) | 74510.33 | **148.02** MB |
| `scala` (`2.12`) | [akkahttp](https://akka.io) (**10.1**) | 70937.67 | **153.48** MB |
| `php` (`7.3`) | [one](https://github.com/lizhichao/one) (**1.8**) | 70462.67 | **161.78** MB |
| `node` (`12.1`) | [muneem](https://github.com/node-muneem/muneem) (**2.4**) | 70461.33 | **105.56** MB |
| `node` (`12.1`) | [fastify](https://fastify.io) (**2.8**) | 66048.00 | **173.59** MB |
| `node` (`12.1`) | [foxify](https://foxify.js.org) (**0.1**) | 65353.33 | **137.39** MB |
| `node` (`12.1`) | [koa](https://koajs.com) (**2.8**) | 61146.33 | **129.29** MB |
| `python` (`3.7`) | [bottle](https://bottlepy.org) (**0.12**) | 60650.67 | **149.38** MB |
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 60252.00 | **128.56** MB |
| `java` (`8`) | [spring-boot](https://spring.io/projects/spring-boot) (**2.1**) | 60197.00 | **44.57** MB |
| `node` (`12.1`) | [iotjs-express](https://github.com/SamsungInternet/iotjs-express) (**0.0**) | 59195.00 | **240.41** MB |
| `ruby` (`2.6`) | [plezi](https://github.com/boazsegev/plezi) (**0.16**) | 57040.67 | **121.54** MB |
| `clojure` (`1.10`) | [coast](https://coastonclojure.com) (**1.0**) | 56147.67 | **100.73** MB |
| `scala` (`2.12`) | [http4s](https://http4s.org) (**0.18**) | 55227.67 | **96.90** MB |
| `python` (`3.7`) | [asgineer](https://asgineer.readthedocs.io) (**0.7**) | 54560.67 | **97.46** MB |
| `node` (`12.1`) | [express](https://expressjs.com) (**4.17**) | 53616.33 | **131.21** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 53449.67 | **265.55** MB |
| `php` (`7.3`) | [basicphp](https://github.com/ray-ang/basicphp) (**0.9**) | 53113.00 | **263.80** MB |
| `python` (`3.7`) | [blacksheep](https://github.com/RobertoPrevato/BlackSheep) (**0.1**) | 51700.33 | **104.02** MB |
| `swift` (`5.1`) | [kitura](https://kitura.io) (**2.7**) | 49736.67 | **92.36** MB |
| `swift` (`5.1`) | [vapor](https://vapor.codes) (**3.3**) | 49609.00 | **84.62** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 49282.00 | **244.90** MB |
| `python` (`3.7`) | [hug](https://hug.rest) (**2.6**) | 49055.67 | **121.58** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**4.2**) | 47228.00 | **234.75** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**6.0**) | 47202.67 | **245.62** MB |
| `ruby` (`2.6`) | [syro](https://github.com/soveran/syro) (**3.1**) | 46690.67 | **27.01** MB |
| `node` (`12.1`) | [restify](https://restify.com) (**8.4**) | 46264.33 | **81.08** MB |
| `ruby` (`2.6`) | [roda](https://roda.jeremyevans.net) (**3.24**) | 45819.00 | **43.76** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 44609.67 | **221.84** MB |
| `python` (`3.7`) | [starlette](https://starlette.io) (**0.12**) | 44552.33 | **95.94** MB |
| `ruby` (`2.6`) | [cuba](https://cuba.is) (**3.9**) | 41736.33 | **49.31** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 40694.33 | **107.00** MB |
| `php` (`7.3`) | [imi](https://imiphp.com) (**1.0**) | 39953.67 | **91.89** MB |
| `swift` (`5.1`) | [kitura-nio](https://kitura.io) (**2.7**) | 35303.33 | **66.76** MB |
| `node` (`12.1`) | [hapi](https://hapijs.com) (**18.1**) | 35194.33 | **91.25** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**6.1**) | 34599.67 | **180.84** MB |
| `ruby` (`2.6`) | [rack-routing](https://github.com/georgeu2000/rack-routing) (**0.0**) | 32730.33 | **18.93** MB |
| `node` (`12.1`) | [moleculer](https://moleculer.services) (**0.13**) | 32284.00 | **55.63** MB |
| `fsharp` (`7.3`) | [suave](https://suave.io) (**2.5**) | 31255.67 | **63.40** MB |
| `python` (`3.7`) | [fastapi](https://fastapi.tiangolo.com) (**0.39**) | 28776.00 | **62.20** MB |
| `crystal` (`0.31`) | [lucky](https://luckyframework.org) (**0.16**) | 26185.67 | **32.17** MB |
| `python` (`3.7`) | [clastic](https://github.com/mahmoud/clastic) (**19.9**) | 24900.33 | **61.78** MB |
| `python` (`3.7`) | [molten](https://moltenframework.com) (**0.27**) | 24626.00 | **45.81** MB |
| `ruby` (`2.6`) | [flame](https://github.com/AlexWayfer/flame) (**4.18**) | 24116.67 | **13.91** MB |
| `python` (`3.7`) | [flask](https://flask.pocoo.org) (**1.1**) | 23735.33 | **58.53** MB |
| `node` (`12.1`) | [turbo_polka](https://github.com/mafintosh/turbo-http) (**2.0**) | 23249.67 | **21.81** MB |
| `python` (`3.7`) | [aiohttp](https://aiohttp.readthedocs.io) (**3.6**) | 22542.67 | **51.12** MB |
| `python` (`3.7`) | [sanic](https://github.com/huge-success/sanic) (**19.6**) | 20295.00 | **36.24** MB |
| `python` (`3.7`) | [bocadillo](https://bocadilloproject.github.io) (**0.18**) | 19219.33 | **37.10** MB |
| `ruby` (`2.6`) | [hanami](https://hanamirb.org) (**1.3**) | 18365.00 | **139.22** MB |
| `php` (`7.3`) | [spiral](https://github.com/spiral/framework) (**2.1**) | 16562.33 | **28.73** MB |
| `ruby` (`2.6`) | [sinatra](https://sinatrarb.com) (**2.0**) | 14746.67 | **38.29** MB |
| `ruby` (`2.6`) | [grape](https://ruby-grape.org) (**1.2**) | 13775.00 | **7.90** MB |
| `swift` (`5.1`) | [swifter](https://github.com/httpswift/swifter) (**1.4**) | 11638.00 | **14.89** MB |
| `python` (`3.7`) | [quart](https://pgjones.gitlab.io/quart) (**0.10**) | 11210.00 | **22.39** MB |
| `python` (`3.7`) | [responder](https://python-responder.org) (**1.3**) | 9397.33 | **20.51** MB |
| `python` (`3.7`) | [django](https://djangoproject.com) (**2.2**) | 8992.33 | **26.11** MB |
| `python` (`3.7`) | [tornado](https://tornadoweb.org) (**5.1**) | 8772.33 | **25.91** MB |
| `python` (`3.7`) | [masonite](https://masoniteproject.com) (**2.2**) | 6848.67 | **16.89** MB |
| `crystal` (`0.31`) | [onyx](https://onyxframework.org) (**0.5**) | 5249.67 | **13.54** MB |
| `crystal` (`0.31`) | [athena](https://github.com/blacksmoke16/athena) (**0.7**) | 4460.33 | **5.57** MB |
| `ruby` (`2.6`) | [rails](https://rubyonrails.org) (**6.0**) | 3676.00 | **23.08** MB |
| `perl` (`5.3`) | [dancer2](https://perldancer.org) (**2.0**) | 964.00 | **2.18** MB |
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

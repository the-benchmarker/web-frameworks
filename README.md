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

+ Initialize `sqlite` database

~~~sh
bin/db init
~~~

+ Make configuration

~~~sh
bin/make config
~~~

+ Build containers

> jobs are either languages (example : crystal) or frameworks (example : router.cr)

~~~sh
bin/neph [job1] [job2] [job3] ...
~~~

+ Export all results readme

~~~sh
bin/db to_readme
~~~

## Results

| Language | Framework | Average | 50th percentile | 90th percentile | Standard deviation | Requests / s | Throughput |
|----|----|--------:|------------:|--------:|---------:|-------:|----|
| rust (1.38)| nickel (0.11) | **0.27** ms | 0.22 ms | 0.44 ms | 224.00 | 37193.67 | 4.91 Mb |
| ruby (2.6)| syro (3.1) | **2.65** ms | 0.51 ms | 8.20 ms | 4596.00 | 47878.33 | 1.84 Mb |
| ruby (2.6)| roda (3.25) | **2.75** ms | 0.63 ms | 8.21 ms | 4450.33 | 46308.33 | 2.93 Mb |
| rust (1.38)| iron (0.6) | **3.09** ms | 2.96 ms | 4.53 ms | 1386.00 | 21169.33 | 1.75 Mb |
| ruby (2.6)| cuba (3.9) | **3.10** ms | 0.60 ms | 9.50 ms | 5080.33 | 41428.67 | 3.24 Mb |
| ruby (2.6)| rack-routing (0.0) | **3.84** ms | 0.58 ms | 12.23 ms | 6364.33 | 33380.67 | 1.28 Mb |
| ruby (2.6)| camping (2.1) | **4.68** ms | 0.43 ms | 16.31 ms | 8871.00 | 27627.67 | 1.75 Mb |
| c (11)| agoo-c (0.7) | **4.73** ms | 4.37 ms | 9.53 ms | 3676.67 | 207911.33 | 7.97 Mb |
| node (12.11)| sifrr (0.0) | **4.81** ms | 4.32 ms | 9.88 ms | 4021.00 | 203341.33 | 11.86 Mb |
| swift (5.1)| swifter (1.4) | **4.82** ms | 0.86 ms | 14.55 ms | 72648.33 | 11942.00 | 1.02 Mb |
| nim (1.0)| httpbeast (0.2) | **5.04** ms | 4.51 ms | 9.76 ms | 3715.67 | 193122.67 | 18.22 Mb |
| python (3.7)| japronto (0.1) | **5.09** ms | 4.58 ms | 10.00 ms | 3830.00 | 191858.33 | 15.22 Mb |
| ruby (2.6)| flame (4.18) | **5.51** ms | 0.49 ms | 19.28 ms | 10889.67 | 23385.33 | 0.90 Mb |
| cpp (11)| drogon (1.0) | **5.60** ms | 4.87 ms | 10.10 ms | 5087.33 | 177211.33 | 11.40 Mb |
| go (1.13)| atreugo (8.2) | **5.77** ms | 5.05 ms | 9.06 ms | 4273.33 | 163349.00 | 21.78 Mb |
| cpp (11)| evhtp (1.2) | **5.88** ms | 5.22 ms | 9.56 ms | 2867.33 | 161255.33 | 10.37 Mb |
| go (1.13)| gorouter-fasthttp (4.1) | **5.96** ms | 5.28 ms | 9.32 ms | 3010.67 | 159275.00 | 16.97 Mb |
| go (1.13)| fasthttprouter (0.1) | **6.17** ms | 5.16 ms | 9.18 ms | 7504.33 | 161768.67 | 17.26 Mb |
| ruby (2.6)| hanami (1.3) | **6.32** ms | 0.85 ms | 20.66 ms | 10459.67 | 20313.33 | 10.20 Mb |
| java (8)| rapidoid (5.5) | **6.36** ms | 4.91 ms | 10.83 ms | 9484.00 | 167757.67 | 20.01 Mb |
| crystal (0.31)| toro (0.4) | **6.48** ms | 5.64 ms | 10.73 ms | 3360.00 | 149164.33 | 9.30 Mb |
| crystal (0.31)| router.cr (0.2) | **6.60** ms | 5.73 ms | 10.86 ms | 3361.00 | 146728.67 | 9.15 Mb |
| crystal (0.31)| raze (0.3) | **6.73** ms | 5.90 ms | 11.02 ms | 3402.33 | 144067.33 | 8.98 Mb |
| crystal (0.31)| kemal (0.28) | **7.17** ms | 6.39 ms | 11.29 ms | 3367.00 | 134723.67 | 14.59 Mb |
| nim (1.0)| jester (0.4) | **7.34** ms | 6.63 ms | 11.93 ms | 3902.00 | 145128.00 | 19.33 Mb |
| crystal (0.31)| amber (0.3) | **7.54** ms | 6.80 ms | 12.13 ms | 3653.67 | 128955.00 | 15.65 Mb |
| ruby (2.6)| sinatra (2.0) | **7.89** ms | 0.67 ms | 26.50 ms | 13556.00 | 16263.33 | 2.80 Mb |
| crystal (0.31)| orion (1.7) | **8.57** ms | 8.11 ms | 13.47 ms | 3967.67 | 114071.00 | 12.36 Mb |
| java (8)| act (1.8) | **8.74** ms | 7.70 ms | 13.04 ms | 5897.00 | 125353.67 | 14.34 Mb |
| go (1.13)| rte (0.0) | **9.44** ms | 7.82 ms | 15.77 ms | 7167.00 | 108734.67 | 9.66 Mb |
| ruby (2.6)| grape (1.2) | **9.66** ms | 0.81 ms | 31.83 ms | 15639.00 | 13342.00 | 0.51 Mb |
| go (1.13)| gorouter (4.1) | **9.98** ms | 8.11 ms | 17.26 ms | 7947.33 | 103208.33 | 9.15 Mb |
| rust (1.38)| actix-web (1.0) | **10.11** ms | 9.63 ms | 13.58 ms | 2855.67 | 107540.67 | 10.31 Mb |
| go (1.13)| chi (4.0) | **10.11** ms | 8.27 ms | 17.62 ms | 7420.33 | 101521.00 | 9.01 Mb |
| go (1.13)| kami (2.2) | **10.41** ms | 8.53 ms | 17.28 ms | 9284.67 | 99353.00 | 8.75 Mb |
| c (11)| kore (3.3) | **10.62** ms | 5.96 ms | 15.18 ms | 31960.00 | 163204.67 | 29.41 Mb |
| go (1.13)| gin (1.4) | **10.67** ms | 8.52 ms | 18.94 ms | 8631.33 | 97576.67 | 11.35 Mb |
| go (1.13)| beego (1.12) | **10.75** ms | 8.65 ms | 18.92 ms | 8330.67 | 97071.00 | 8.66 Mb |
| go (1.13)| goroute (0.0) | **10.79** ms | 8.39 ms | 19.27 ms | 10691.00 | 98247.00 | 11.43 Mb |
| go (1.13)| echo (4.1) | **10.81** ms | 8.51 ms | 19.27 ms | 9729.33 | 97354.00 | 11.33 Mb |
| go (1.13)| violetear (7.0) | **11.25** ms | 8.93 ms | 16.29 ms | 16865.33 | 97533.00 | 8.58 Mb |
| go (1.13)| gorilla-mux (1.7) | **11.68** ms | 8.66 ms | 20.32 ms | 15945.67 | 94883.33 | 8.39 Mb |
| python (3.7)| falcon (2.0) | **12.68** ms | 10.39 ms | 22.63 ms | 8170.33 | 80810.67 | 12.55 Mb |
| go (1.13)| air (0.13) | **12.87** ms | 9.66 ms | 23.91 ms | 12488.33 | 84626.33 | 11.71 Mb |
| swift (5.1)| perfect (3.1) | **12.98** ms | 13.03 ms | 15.31 ms | 2222.00 | 75330.00 | 4.70 Mb |
| node (12.11)| polkadot (1.0) | **13.20** ms | 9.26 ms | 17.45 ms | 30375.33 | 93550.33 | 9.29 Mb |
| go (1.13)| gf (1.9) | **13.48** ms | 10.80 ms | 23.06 ms | 12277.33 | 78613.67 | 8.85 Mb |
| php (7.3)| one (1.8) | **13.76** ms | 12.51 ms | 23.78 ms | 8060.00 | 73308.00 | 11.17 Mb |
| node (12.11)| restana (3.3) | **14.61** ms | 9.59 ms | 18.21 ms | 36153.00 | 88740.67 | 8.81 Mb |
| node (12.11)| 0http (1.2) | **14.91** ms | 9.37 ms | 17.93 ms | 40363.00 | 91677.67 | 9.10 Mb |
| ruby (2.6)| agoo (2.11) | **15.03** ms | 14.86 ms | 16.44 ms | 2830.67 | 65894.33 | 2.53 Mb |
| python (3.7)| bottle (0.12) | **16.62** ms | 14.17 ms | 25.54 ms | 9051.67 | 60624.67 | 9.90 Mb |
| node (12.11)| polka (0.5) | **17.15** ms | 10.21 ms | 19.87 ms | 47920.67 | 82505.00 | 8.19 Mb |
| rust (1.38)| gotham (0.4) | **17.15** ms | 16.91 ms | 25.62 ms | 13116.00 | 59046.33 | 7.95 Mb |
| node (12.11)| rayo (1.3) | **17.19** ms | 10.37 ms | 20.21 ms | 45913.67 | 80146.67 | 7.96 Mb |
| php (7.3)| hyperf (1.0) | **17.30** ms | 14.25 ms | 34.47 ms | 13144.33 | 61865.67 | 8.74 Mb |
| python (3.7)| asgineer (0.7) | **17.54** ms | 16.29 ms | 27.22 ms | 8198.00 | 56770.33 | 6.72 Mb |
| ruby (2.6)| plezi (0.16) | **17.59** ms | 16.46 ms | 22.52 ms | 9224.33 | 56193.67 | 7.94 Mb |
| python (3.7)| blacksheep (0.2) | **19.24** ms | 17.16 ms | 31.74 ms | 10069.00 | 52681.67 | 7.02 Mb |
| kotlin (1.3)| ktor (1.2) | **19.32** ms | 12.30 ms | 29.11 ms | 44983.00 | 71436.33 | 7.38 Mb |
| node (12.11)| muneem (2.4) | **20.58** ms | 11.68 ms | 22.81 ms | 58022.33 | 71351.33 | 7.09 Mb |
| python (3.7)| hug (2.6) | **20.86** ms | 16.24 ms | 38.20 ms | 12515.33 | 49486.33 | 8.13 Mb |
| python (3.7)| starlette (0.12) | **21.30** ms | 19.28 ms | 33.13 ms | 9275.67 | 46825.67 | 6.69 Mb |
| node (12.11)| foxify (0.1) | **22.63** ms | 12.49 ms | 23.61 ms | 66874.67 | 68437.33 | 9.53 Mb |
| swift (5.1)| kitura-nio (2.8) | **23.05** ms | 20.19 ms | 23.13 ms | 41441.00 | 47335.00 | 5.84 Mb |
| node (12.11)| koa (2.8) | **23.77** ms | 13.87 ms | 26.07 ms | 61285.67 | 61368.00 | 8.61 Mb |
| php (7.3)| swoft (2.0) | **24.47** ms | 23.62 ms | 31.79 ms | 6420.00 | 40016.67 | 6.98 Mb |
| node (12.11)| express (4.17) | **26.69** ms | 15.70 ms | 28.98 ms | 69657.67 | 54189.67 | 8.80 Mb |
| swift (5.1)| vapor (3.3) | **27.31** ms | 17.80 ms | 31.91 ms | 74122.00 | 48645.67 | 5.50 Mb |
| swift (5.1)| kitura (2.8) | **27.50** ms | 20.43 ms | 23.42 ms | 69260.00 | 47142.00 | 5.81 Mb |
| node (12.11)| fastify (2.8) | **28.08** ms | 14.95 ms | 28.09 ms | 82385.00 | 59824.00 | 10.49 Mb |
| node (12.11)| restify (8.4) | **28.30** ms | 19.12 ms | 31.45 ms | 56964.00 | 45956.33 | 5.35 Mb |
| csharp (7.3)| aspnetcore (2.2) | **29.04** ms | 9.94 ms | 17.56 ms | 108366.00 | 86333.33 | 9.32 Mb |
| php (7.3)| imi (1.0) | **29.33** ms | 28.20 ms | 37.61 ms | 6888.33 | 33642.33 | 5.12 Mb |
| node (12.11)| iotjs-express (0.0) | **30.70** ms | 14.39 ms | 27.58 ms | 95457.33 | 59548.33 | 16.04 Mb |
| java (8)| javalin (3.5) | **34.46** ms | 11.71 ms | 51.93 ms | 101842.33 | 67139.00 | 7.94 Mb |
| ruby (2.6)| rails (6.0) | **34.52** ms | 2.75 ms | 114.21 ms | 63871.00 | 3712.00 | 1.55 Mb |
| python (3.7)| fastapi (0.42) | **36.32** ms | 32.42 ms | 59.90 ms | 17708.00 | 27748.67 | 3.97 Mb |
| python (3.7)| clastic (19.9) | **38.73** ms | 33.54 ms | 58.55 ms | 15666.67 | 25664.33 | 4.22 Mb |
| crystal (0.31)| spider-gazelle (1.6) | **38.76** ms | 36.37 ms | 46.03 ms | 28463.67 | 25994.33 | 1.84 Mb |
| python (3.7)| molten (0.27) | **40.12** ms | 33.97 ms | 63.54 ms | 17159.67 | 25411.67 | 3.13 Mb |
| java (8)| spring-boot (2.1) | **42.37** ms | 15.75 ms | 38.29 ms | 150025.67 | 48612.00 | 2.59 Mb |
| python (3.7)| aiohttp (3.6) | **43.01** ms | 40.00 ms | 63.82 ms | 15471.33 | 23079.00 | 3.47 Mb |
| php (7.3)| basicphp (0.9) | **43.74** ms | 15.10 ms | 87.96 ms | 93399.33 | 53807.67 | 17.72 Mb |
| node (12.11)| turbo_polka (2.0) | **44.10** ms | 41.81 ms | 48.92 ms | 24944.00 | 22399.33 | 1.40 Mb |
| crystal (0.31)| lucky (0.18) | **44.34** ms | 41.92 ms | 54.18 ms | 19795.67 | 22248.00 | 1.82 Mb |
| python (3.7)| flask (1.1) | **46.01** ms | 38.39 ms | 74.54 ms | 20679.67 | 21496.00 | 3.51 Mb |
| php (7.3)| slim (4.3) | **48.30** ms | 16.00 ms | 93.44 ms | 105752.00 | 51204.00 | 16.86 Mb |
| php (7.3)| lumen (6.2) | **48.35** ms | 15.92 ms | 97.58 ms | 105499.33 | 50832.33 | 16.75 Mb |
| php (7.3)| zend-expressive (3.2) | **49.46** ms | 16.04 ms | 96.08 ms | 109344.67 | 50667.67 | 16.69 Mb |
| php (7.3)| symfony (4.3) | **50.11** ms | 16.53 ms | 102.99 ms | 109238.00 | 49756.33 | 16.39 Mb |
| python (3.7)| sanic (19.9) | **51.14** ms | 45.66 ms | 93.33 ms | 27984.67 | 19754.00 | 2.34 Mb |
| python (3.7)| bocadillo (0.18) | **51.49** ms | 45.64 ms | 88.70 ms | 26613.33 | 19828.00 | 2.54 Mb |
| fsharp (7.3)| suave (2.5) | **54.58** ms | 19.22 ms | 151.17 ms | 71582.67 | 19115.33 | 2.57 Mb |
| php (7.3)| zend-framework (3.1) | **55.75** ms | 16.63 ms | 96.58 ms | 131458.67 | 48335.67 | 15.92 Mb |
| php (7.3)| spiral (2.1) | **59.01** ms | 59.09 ms | 65.52 ms | 7324.33 | 16470.00 | 1.90 Mb |
| scala (2.12)| http4s (0.18) | **60.56** ms | 19.92 ms | 42.76 ms | 222309.33 | 44275.00 | 5.15 Mb |
| node (12.11)| hapi (18.1) | **62.99** ms | 24.50 ms | 46.74 ms | 191886.33 | 34529.67 | 5.94 Mb |
| clojure (1.10)| coast (1.0) | **64.67** ms | 19.46 ms | 23.15 ms | 232096.00 | 48432.67 | 5.78 Mb |
| crystal (0.31)| athena (0.7) | **66.97** ms | 49.38 ms | 180.74 ms | 83334.00 | 24013.67 | 2.00 Mb |
| node (12.11)| moleculer (0.13) | **67.50** ms | 27.79 ms | 56.98 ms | 192012.00 | 30095.00 | 3.43 Mb |
| php (7.3)| laravel (6.3) | **74.64** ms | 19.72 ms | 135.57 ms | 180423.33 | 41703.67 | 13.80 Mb |
| python (3.7)| quart (0.10) | **86.45** ms | 79.37 ms | 128.20 ms | 31986.67 | 11449.33 | 1.52 Mb |
| go (1.13)| gramework (1.6) | **97.23** ms | 98.40 ms | 102.64 ms | 18028.67 | 10068.00 | 1.71 Mb |
| python (3.7)| tornado (5.1) | **100.94** ms | 97.74 ms | 126.03 ms | 24710.33 | 9618.67 | 1.89 Mb |
| python (3.7)| responder (1.3) | **104.06** ms | 92.71 ms | 156.68 ms | 34376.33 | 9448.33 | 1.37 Mb |
| python (3.7)| django (2.2) | **106.90** ms | 96.45 ms | 152.76 ms | 30267.33 | 9179.00 | 1.77 Mb |
| python (3.7)| masonite (2.2) | **136.43** ms | 121.49 ms | 202.16 ms | 42475.00 | 7147.67 | 1.17 Mb |
| crystal (0.31)| onyx (0.5) | **197.81** ms | 198.06 ms | 231.88 ms | 30057.67 | 4962.33 | 0.85 Mb |
| scala (2.12)| akkahttp (10.1) | **228.12** ms | 8.61 ms | 313.55 ms | 840552.00 | 66210.67 | 9.49 Mb |
| perl (5.3)| dancer2 (2.0) | **291.22** ms | 75.19 ms | 444.13 ms | 707841.33 | 1284.67 | 0.19 Mb |
| python (3.7)| cyclone (1.3) | **334.41** ms | 309.00 ms | 394.91 ms | 325892.33 | 2242.00 | 0.38 Mb |

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

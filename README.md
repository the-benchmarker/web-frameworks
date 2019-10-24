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
| rust (1.38)| nickel (0.11) | **0.26** ms | 0.22 ms | 0.43 ms | 207.33 | 35680.33 | 4.71 Mb |
| ruby (2.6)| syro (3.1) | **2.62** ms | 0.58 ms | 7.78 ms | 3999.33 | 48236.33 | 1.85 Mb |
| ruby (2.6)| roda (3.25) | **2.76** ms | 0.73 ms | 7.85 ms | 3883.67 | 45956.67 | 2.91 Mb |
| ruby (2.6)| cuba (3.9) | **3.12** ms | 0.55 ms | 9.67 ms | 4996.67 | 40815.33 | 3.20 Mb |
| rust (1.38)| iron (0.6) | **3.16** ms | 3.04 ms | 4.58 ms | 1356.33 | 20760.00 | 1.71 Mb |
| ruby (2.6)| rack-routing (0.0) | **3.74** ms | 0.59 ms | 11.83 ms | 6144.00 | 34060.67 | 1.31 Mb |
| ruby (2.6)| camping (2.1) | **4.60** ms | 0.46 ms | 16.18 ms | 8650.33 | 27993.00 | 1.77 Mb |
| c (11)| agoo-c (0.7) | **4.81** ms | 4.32 ms | 9.42 ms | 3795.67 | 203752.33 | 7.81 Mb |
| node (12.11)| sifrr (0.0) | **4.81** ms | 4.22 ms | 10.00 ms | 4287.00 | 206160.67 | 12.03 Mb |
| nim (1.0)| httpbeast (0.2) | **5.09** ms | 4.47 ms | 9.80 ms | 3920.33 | 193631.33 | 18.27 Mb |
| swift (5.1)| swifter (1.4) | **5.11** ms | 0.82 ms | 14.48 ms | 66267.67 | 12188.67 | 1.04 Mb |
| python (3.7)| japronto (0.1) | **5.20** ms | 4.59 ms | 10.20 ms | 4236.00 | 187841.33 | 14.90 Mb |
| cpp (11)| drogon (1.0) | **5.47** ms | 4.79 ms | 10.12 ms | 3977.33 | 178842.33 | 11.50 Mb |
| ruby (2.6)| flame (4.18) | **5.61** ms | 1.71 ms | 16.86 ms | 8691.67 | 22762.67 | 0.87 Mb |
| cpp (11)| evhtp (1.2) | **5.83** ms | 5.15 ms | 9.50 ms | 3587.33 | 162050.67 | 10.42 Mb |
| ruby (2.6)| hanami (1.3) | **6.29** ms | 0.59 ms | 20.84 ms | 10702.00 | 20338.33 | 10.22 Mb |
| go (1.13)| atreugo (8.2) | **6.34** ms | 4.97 ms | 9.00 ms | 11919.67 | 165534.67 | 22.07 Mb |
| crystal (0.31)| router.cr (0.2) | **6.41** ms | 5.59 ms | 10.46 ms | 3229.33 | 150464.33 | 9.38 Mb |
| go (1.13)| gorouter-fasthttp (4.2) | **6.46** ms | 5.09 ms | 9.04 ms | 11690.67 | 162899.00 | 17.36 Mb |
| crystal (0.31)| toro (0.4) | **6.48** ms | 5.66 ms | 10.63 ms | 3288.67 | 148657.67 | 9.27 Mb |
| go (1.13)| fasthttprouter (0.1) | **6.53** ms | 5.43 ms | 9.95 ms | 7678.00 | 151306.67 | 16.15 Mb |
| crystal (0.31)| raze (0.3) | **6.64** ms | 5.77 ms | 10.80 ms | 3332.00 | 145464.00 | 9.07 Mb |
| crystal (0.31)| kemal (0.28) | **7.00** ms | 6.05 ms | 11.50 ms | 3494.33 | 138443.33 | 15.00 Mb |
| java (8)| rapidoid (5.5) | **7.09** ms | 5.00 ms | 11.04 ms | 14561.00 | 164262.33 | 19.60 Mb |
| crystal (0.31)| amber (0.3) | **7.36** ms | 6.61 ms | 11.69 ms | 3460.33 | 131205.33 | 15.92 Mb |
| nim (1.0)| jester (0.4) | **7.69** ms | 6.63 ms | 13.22 ms | 5219.00 | 141072.00 | 18.79 Mb |
| ruby (2.6)| sinatra (2.0) | **7.77** ms | 2.36 ms | 23.69 ms | 11746.67 | 16477.33 | 2.84 Mb |
| crystal (0.31)| orion (1.7) | **8.28** ms | 7.61 ms | 13.32 ms | 4015.33 | 118157.67 | 12.80 Mb |
| java (8)| act (1.8) | **8.77** ms | 7.41 ms | 12.86 ms | 9094.00 | 126536.67 | 14.48 Mb |
| ruby (2.6)| grape (1.2) | **9.44** ms | 0.81 ms | 31.03 ms | 15096.67 | 13648.67 | 0.52 Mb |
| rust (1.38)| actix-web (1.0) | **9.91** ms | 9.49 ms | 13.29 ms | 2874.33 | 107919.33 | 10.32 Mb |
| go (1.13)| gorouter (4.2) | **10.02** ms | 7.92 ms | 16.41 ms | 12381.33 | 104418.00 | 9.23 Mb |
| go (1.13)| kami (2.2) | **10.20** ms | 8.56 ms | 16.27 ms | 8551.33 | 101464.00 | 8.94 Mb |
| go (1.13)| chi (4.0) | **10.46** ms | 8.28 ms | 17.82 ms | 10385.67 | 101845.67 | 9.02 Mb |
| go (1.13)| echo (4.1) | **10.76** ms | 8.30 ms | 17.83 ms | 14234.67 | 101296.33 | 11.78 Mb |
| go (1.13)| goroute (0.0) | **11.08** ms | 8.51 ms | 18.91 ms | 13331.00 | 97263.33 | 11.32 Mb |
| go (1.13)| gin (1.4) | **11.32** ms | 8.72 ms | 20.15 ms | 10886.33 | 94555.00 | 11.00 Mb |
| go (1.13)| violetear (7.0) | **11.55** ms | 8.68 ms | 15.36 ms | 22693.33 | 100502.00 | 8.83 Mb |
| go (1.13)| rte (0.0) | **11.99** ms | 9.18 ms | 21.57 ms | 10383.33 | 90383.00 | 7.93 Mb |
| go (1.13)| gorilla-mux (1.7) | **12.15** ms | 8.42 ms | 18.92 ms | 25013.33 | 98672.33 | 8.74 Mb |
| go (1.13)| beego (1.12) | **12.19** ms | 8.89 ms | 20.10 ms | 18710.00 | 93034.33 | 8.18 Mb |
| swift (5.1)| perfect (3.1) | **12.63** ms | 12.77 ms | 14.88 ms | 3788.33 | 76702.33 | 4.78 Mb |
| go (1.13)| webgo (3.0) | **13.35** ms | 9.89 ms | 22.00 ms | 20037.33 | 83559.00 | 7.39 Mb |
| go (1.13)| gf (1.9) | **13.39** ms | 10.78 ms | 22.87 ms | 11332.00 | 79176.00 | 8.89 Mb |
| go (1.13)| air (0.13) | **13.69** ms | 9.60 ms | 22.95 ms | 23420.00 | 85025.00 | 11.76 Mb |
| python (3.7)| falcon (2.0) | **13.78** ms | 11.65 ms | 22.71 ms | 8178.67 | 73300.67 | 11.39 Mb |
| c (11)| kore (3.3) | **13.90** ms | 5.55 ms | 18.78 ms | 42314.00 | 167450.33 | 30.18 Mb |
| ruby (2.6)| agoo (2.11) | **14.32** ms | 14.26 ms | 15.28 ms | 2474.33 | 68696.67 | 2.63 Mb |
| php (7.3)| one (1.8) | **14.56** ms | 13.09 ms | 25.22 ms | 8766.33 | 69494.00 | 10.59 Mb |
| node (12.11)| polkadot (1.0) | **15.87** ms | 9.28 ms | 19.11 ms | 45352.00 | 91337.33 | 9.07 Mb |
| python (3.7)| bottle (0.12) | **16.00** ms | 14.11 ms | 25.02 ms | 8326.33 | 62799.67 | 10.26 Mb |
| python (3.7)| asgineer (0.7) | **17.33** ms | 15.83 ms | 26.91 ms | 7697.67 | 57176.00 | 6.77 Mb |
| rust (1.38)| gotham (0.4) | **17.35** ms | 16.86 ms | 25.94 ms | 13147.67 | 58565.00 | 7.87 Mb |
| ruby (2.6)| plezi (0.16) | **17.51** ms | 16.31 ms | 22.87 ms | 9556.67 | 56490.33 | 7.98 Mb |
| php (7.3)| hyperf (1.0) | **18.78** ms | 15.52 ms | 36.93 ms | 14133.67 | 56825.67 | 8.03 Mb |
| node (12.11)| rayo (1.3) | **18.95** ms | 10.55 ms | 21.17 ms | 55360.33 | 78787.67 | 7.82 Mb |
| node (12.11)| 0http (1.2) | **19.61** ms | 9.85 ms | 21.16 ms | 64223.67 | 85667.00 | 8.51 Mb |
| node (12.11)| polka (0.5) | **19.85** ms | 10.12 ms | 19.91 ms | 60971.33 | 82536.00 | 8.20 Mb |
| python (3.7)| blacksheep (0.2) | **20.30** ms | 17.86 ms | 33.12 ms | 10339.00 | 50254.33 | 6.70 Mb |
| python (3.7)| hug (2.6) | **21.09** ms | 18.45 ms | 31.32 ms | 9955.33 | 47208.67 | 7.76 Mb |
| node (12.11)| restana (3.3) | **21.97** ms | 9.54 ms | 19.64 ms | 76433.33 | 89163.67 | 8.85 Mb |
| node (12.11)| muneem (2.4) | **22.81** ms | 12.17 ms | 23.98 ms | 67741.33 | 69046.00 | 6.86 Mb |
| php (7.3)| swoft (2.0) | **22.94** ms | 22.45 ms | 28.65 ms | 5039.33 | 42397.33 | 7.39 Mb |
| swift (5.1)| kitura (2.8) | **24.27** ms | 20.32 ms | 22.59 ms | 51520.00 | 47730.67 | 5.89 Mb |
| java (8)| spring-boot (2.1) | **24.67** ms | 14.03 ms | 31.73 ms | 65027.33 | 54942.00 | 2.89 Mb |
| python (3.7)| starlette (0.12) | **25.31** ms | 21.14 ms | 44.73 ms | 16403.00 | 41260.67 | 5.89 Mb |
| csharp (7.3)| aspnetcore (2.2) | **26.24** ms | 9.42 ms | 15.86 ms | 101837.00 | 91977.33 | 9.93 Mb |
| node (12.11)| restify (8.4) | **26.56** ms | 18.50 ms | 28.05 ms | 53383.67 | 48152.33 | 5.60 Mb |
| fsharp (7.3)| suave (2.5) | **27.60** ms | 19.32 ms | 48.04 ms | 43230.67 | 31116.33 | 4.18 Mb |
| php (7.3)| imi (1.0) | **28.33** ms | 27.58 ms | 35.42 ms | 6008.00 | 34318.33 | 5.23 Mb |
| swift (5.1)| kitura-nio (2.8) | **28.33** ms | 20.11 ms | 22.37 ms | 82289.67 | 48216.33 | 5.95 Mb |
| node (12.11)| iotjs-express (0.0) | **30.15** ms | 14.79 ms | 29.60 ms | 87982.67 | 56166.33 | 15.13 Mb |
| node (12.11)| express (4.17) | **30.74** ms | 15.66 ms | 29.16 ms | 87201.67 | 54179.00 | 8.79 Mb |
| node (12.11)| fastify (2.8) | **31.52** ms | 14.77 ms | 27.98 ms | 99897.00 | 60101.33 | 10.64 Mb |
| node (12.11)| foxify (0.1) | **32.43** ms | 13.95 ms | 29.34 ms | 104290.67 | 60256.67 | 8.39 Mb |
| ruby (2.6)| rails (6.0) | **33.73** ms | 2.66 ms | 111.31 ms | 64009.33 | 3799.00 | 1.59 Mb |
| scala (2.12)| http4s (0.18) | **35.65** ms | 18.18 ms | 42.96 ms | 96388.00 | 49215.67 | 5.72 Mb |
| python (3.7)| fastapi (0.42) | **35.98** ms | 30.19 ms | 63.71 ms | 20847.67 | 28632.33 | 4.10 Mb |
| swift (5.1)| vapor (3.3) | **36.36** ms | 17.57 ms | 31.46 ms | 131995.33 | 49849.67 | 5.66 Mb |
| crystal (0.31)| spider-gazelle (1.6) | **37.72** ms | 36.09 ms | 45.30 ms | 16979.33 | 25889.33 | 1.83 Mb |
| python (3.7)| clastic (19.9) | **38.84** ms | 32.65 ms | 61.92 ms | 16817.33 | 25455.33 | 4.18 Mb |
| python (3.7)| responder (2.0) | **39.30** ms | 34.89 ms | 63.71 ms | 19312.67 | 25543.67 | 3.70 Mb |
| kotlin (1.3)| ktor (1.2) | **39.58** ms | 11.65 ms | 28.75 ms | 152053.00 | 75478.67 | 7.80 Mb |
| python (3.7)| aiohttp (3.6) | **42.50** ms | 41.20 ms | 64.10 ms | 16298.33 | 23298.67 | 3.50 Mb |
| php (7.3)| basicphp (0.9) | **43.24** ms | 14.83 ms | 86.78 ms | 91489.67 | 54248.67 | 17.86 Mb |
| python (3.7)| flask (1.1) | **44.11** ms | 38.01 ms | 71.78 ms | 19708.67 | 22306.33 | 3.64 Mb |
| php (7.3)| slim (4.3) | **46.26** ms | 15.80 ms | 96.28 ms | 97753.67 | 51977.67 | 17.12 Mb |
| php (7.3)| lumen (6.2) | **46.29** ms | 15.36 ms | 94.81 ms | 97091.67 | 51957.67 | 17.12 Mb |
| crystal (0.31)| lucky (0.18) | **46.62** ms | 43.60 ms | 54.53 ms | 38392.00 | 22286.33 | 1.82 Mb |
| node (12.11)| koa (2.8) | **46.66** ms | 14.42 ms | 31.80 ms | 156773.33 | 56586.67 | 7.94 Mb |
| python (3.7)| molten (0.27) | **47.70** ms | 39.79 ms | 77.19 ms | 26420.33 | 21225.67 | 2.62 Mb |
| python (3.7)| sanic (19.9) | **49.84** ms | 44.10 ms | 88.25 ms | 28339.00 | 20060.33 | 2.37 Mb |
| php (7.3)| symfony (4.3) | **50.64** ms | 16.79 ms | 101.29 ms | 107774.00 | 47851.00 | 15.77 Mb |
| node (12.11)| turbo_polka (2.0) | **50.97** ms | 41.93 ms | 50.35 ms | 71832.33 | 22069.67 | 1.38 Mb |
| clojure (1.10)| coast (1.0) | **51.94** ms | 18.91 ms | 21.20 ms | 201227.00 | 49024.33 | 5.85 Mb |
| php (7.3)| zend-expressive (3.2) | **53.62** ms | 17.65 ms | 108.55 ms | 116273.33 | 45999.67 | 15.15 Mb |
| node (12.11)| hapi (18.4) | **55.52** ms | 23.46 ms | 42.24 ms | 168027.00 | 36910.67 | 6.35 Mb |
| php (7.3)| zend-framework (3.1) | **57.63** ms | 17.52 ms | 110.93 ms | 128718.33 | 45165.33 | 14.88 Mb |
| python (3.7)| bocadillo (0.18) | **59.15** ms | 51.71 ms | 98.13 ms | 30732.33 | 17159.00 | 2.20 Mb |
| php (7.3)| spiral (2.1) | **59.15** ms | 58.52 ms | 66.80 ms | 12565.33 | 16415.67 | 1.89 Mb |
| crystal (0.31)| athena (0.7) | **65.63** ms | 47.87 ms | 176.04 ms | 82096.00 | 24568.00 | 2.05 Mb |
| node (12.11)| moleculer (0.13) | **68.23** ms | 26.76 ms | 55.75 ms | 197002.33 | 30897.33 | 3.53 Mb |
| php (7.3)| laravel (6.4) | **76.63** ms | 20.54 ms | 161.52 ms | 180840.00 | 39719.67 | 13.14 Mb |
| python (3.7)| quart (0.10) | **82.15** ms | 82.77 ms | 121.40 ms | 31316.00 | 11869.33 | 1.57 Mb |
| python (3.7)| cherrypy (18.3) | **87.92** ms | 73.02 ms | 78.44 ms | 220258.00 | 1387.00 | 0.21 Mb |
| java (8)| javalin (3.5) | **91.71** ms | 9.84 ms | 160.35 ms | 294738.33 | 67800.67 | 8.02 Mb |
| go (1.13)| gramework (1.6) | **95.17** ms | 96.72 ms | 101.85 ms | 18894.67 | 10227.67 | 1.74 Mb |
| python (3.7)| tornado (5.1) | **99.95** ms | 96.23 ms | 123.59 ms | 44236.67 | 9764.00 | 1.91 Mb |
| python (3.7)| django (2.2) | **103.41** ms | 96.16 ms | 147.89 ms | 34463.33 | 9385.00 | 1.81 Mb |
| python (3.7)| masonite (2.2) | **133.61** ms | 117.78 ms | 217.33 ms | 55988.33 | 7287.67 | 1.19 Mb |
| crystal (0.31)| onyx (0.5) | **195.52** ms | 195.76 ms | 227.98 ms | 29253.33 | 5027.67 | 0.86 Mb |
| scala (2.12)| akkahttp (10.1) | **226.75** ms | 8.14 ms | 166.05 ms | 867456.67 | 68834.33 | 9.87 Mb |
| perl (5.3)| dancer2 (2.0) | **313.30** ms | 100.12 ms | 707.46 ms | 652468.00 | 1325.33 | 0.20 Mb |
| python (3.7)| cyclone (1.3) | **355.79** ms | 283.80 ms | 360.38 ms | 574044.33 | 2292.00 | 0.39 Mb |
| python (3.7)| nameko (2.12) | **626.16** ms | 547.80 ms | 664.70 ms | 654206.33 | 1237.67 | 0.17 Mb |

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

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
| rust (1.38)| nickel (0.11) | **0.10** ms | 0.09 ms | 0.14 ms | 68.33 | 130950.67 | 17.30 Mb |
| rust (1.38)| iron (0.6) | **1.94** ms | 1.86 ms | 2.97 ms | 1007.33 | 50152.33 | 4.13 Mb |
| ruby (2.6)| roda (3.25) | **2.76** ms | 2.27 ms | 6.51 ms | 2796.67 | 68865.33 | 4.36 Mb |
| ruby (2.6)| syro (3.1) | **2.76** ms | 1.67 ms | 7.00 ms | 3185.33 | 68988.00 | 2.64 Mb |
| ruby (2.6)| cuba (3.9) | **3.08** ms | 2.38 ms | 7.46 ms | 3300.33 | 61641.00 | 4.83 Mb |
| nim (1.0)| httpbeast (0.2) | **3.44** ms | 2.00 ms | 8.30 ms | 3973.00 | 364987.33 | 34.44 Mb |
| go (1.13)| atreugo (9.0) | **3.49** ms | 2.54 ms | 6.13 ms | 5548.33 | 286181.00 | 38.15 Mb |
| ruby (2.6)| rack-routing (0.0) | **3.58** ms | 2.61 ms | 8.88 ms | 3911.67 | 53644.67 | 2.06 Mb |
| javascript (12.13)| sifrr (0.0) | **3.60** ms | 1.94 ms | 8.94 ms | 4290.67 | 349655.67 | 20.40 Mb |
| javascript (12.13)| nanoexpress (1.1) | **3.73** ms | 2.22 ms | 8.95 ms | 4245.67 | 334726.33 | 19.50 Mb |
| go (1.13)| fasthttprouter (0.1) | **3.78** ms | 2.83 ms | 6.73 ms | 5015.67 | 262756.00 | 28.01 Mb |
| java (8)| rapidoid (5.5) | **3.86** ms | 2.05 ms | 9.23 ms | 5410.67 | 332150.67 | 39.62 Mb |
| crystal (0.31)| toro (0.4) | **3.87** ms | 3.03 ms | 7.68 ms | 3172.00 | 266042.33 | 16.58 Mb |
| go (1.13)| fasthttp (1.5) | **3.88** ms | 2.42 ms | 6.01 ms | 11823.00 | 303273.67 | 32.39 Mb |
| c (11)| agoo-c (0.7) | **3.94** ms | 2.79 ms | 8.61 ms | 4178.00 | 273343.67 | 10.47 Mb |
| nim (1.0)| jester (0.4) | **4.41** ms | 3.44 ms | 8.39 ms | 3562.67 | 266610.67 | 35.51 Mb |
| go (1.13)| gorouter-fasthttp (4.2) | **4.57** ms | 2.92 ms | 6.85 ms | 13426.00 | 255990.00 | 27.21 Mb |
| c (11)| kore (3.3) | **4.58** ms | 3.91 ms | 9.29 ms | 3697.00 | 225696.33 | 40.69 Mb |
| crystal (0.31)| router.cr (0.2) | **4.60** ms | 3.91 ms | 9.02 ms | 3668.67 | 221642.67 | 13.81 Mb |
| crystal (0.31)| raze (0.3) | **4.80** ms | 4.27 ms | 9.19 ms | 3644.00 | 209491.67 | 13.05 Mb |
| crystal (0.31)| amber (0.3) | **4.81** ms | 4.08 ms | 9.07 ms | 3638.33 | 214771.33 | 26.05 Mb |
| crystal (0.31)| kemal (0.28) | **4.91** ms | 4.22 ms | 9.62 ms | 3839.00 | 207222.00 | 22.45 Mb |
| ruby (2.6)| camping (2.1) | **4.92** ms | 3.11 ms | 12.29 ms | 5643.67 | 38866.00 | 2.46 Mb |
| javascript (12.13)| nanoexpress-pro (1.4) | **4.97** ms | 3.23 ms | 11.02 ms | 5366.33 | 254831.67 | 14.87 Mb |
| crystal (0.31)| spider-gazelle (2.0) | **4.98** ms | 4.21 ms | 9.86 ms | 4021.67 | 205981.00 | 14.55 Mb |
| crystal (0.31)| orion (1.7) | **5.23** ms | 4.50 ms | 9.58 ms | 3850.33 | 195154.00 | 21.14 Mb |
| ruby (2.6)| flame (4.18) | **5.24** ms | 4.20 ms | 12.22 ms | 5248.67 | 36490.00 | 1.40 Mb |
| go (1.13)| rte (0.0) | **5.41** ms | 4.05 ms | 9.99 ms | 7708.67 | 202183.67 | 17.90 Mb |
| java (8)| act (1.8) | **5.43** ms | 3.54 ms | 10.66 ms | 7447.67 | 251061.00 | 28.72 Mb |
| cpp (11)| drogon (1.0) | **5.46** ms | 5.09 ms | 7.77 ms | 3312.00 | 195734.67 | 12.58 Mb |
| ruby (2.6)| agoo (2.11) | **5.81** ms | 3.84 ms | 9.18 ms | 10511.00 | 233761.33 | 8.96 Mb |
| javascript (12.13)| polkadot (1.0) | **6.03** ms | 4.97 ms | 9.41 ms | 7305.00 | 165204.33 | 16.41 Mb |
| go (1.13)| gorilla-mux (1.7) | **6.50** ms | 4.47 ms | 14.60 ms | 7774.67 | 179196.67 | 15.86 Mb |
| javascript (12.13)| restana (3.3) | **6.56** ms | 5.17 ms | 9.61 ms | 8534.00 | 156387.00 | 15.53 Mb |
| go (1.13)| kami (2.2) | **6.71** ms | 4.53 ms | 10.50 ms | 16018.00 | 185333.67 | 16.33 Mb |
| ruby (2.6)| hanami (1.3) | **6.73** ms | 5.33 ms | 15.96 ms | 7059.33 | 28466.00 | 14.30 Mb |
| swift (5.1)| perfect (3.1) | **6.77** ms | 6.59 ms | 9.46 ms | 2931.33 | 142431.00 | 8.88 Mb |
| go (1.13)| chi (4.0) | **6.77** ms | 4.64 ms | 14.27 ms | 9296.00 | 173184.33 | 15.33 Mb |
| javascript (12.13)| polka (0.5) | **6.92** ms | 5.24 ms | 9.93 ms | 10229.33 | 150332.00 | 14.93 Mb |
| javascript (12.13)| rayo (1.3) | **7.07** ms | 5.38 ms | 10.21 ms | 9512.00 | 146850.67 | 14.58 Mb |
| go (1.13)| gorouter (4.2) | **7.27** ms | 4.95 ms | 15.05 ms | 8936.00 | 165477.33 | 14.35 Mb |
| go (1.13)| gf (1.9) | **7.37** ms | 5.08 ms | 14.50 ms | 10606.33 | 154706.33 | 17.40 Mb |
| rust (1.38)| gotham (0.4) | **7.43** ms | 6.73 ms | 11.48 ms | 6447.33 | 137796.33 | 18.54 Mb |
| swift (5.1)| swifter (1.4) | **7.43** ms | 0.88 ms | 14.57 ms | 87092.00 | 22220.33 | 1.89 Mb |
| javascript (12.13)| 0http (1.2) | **7.62** ms | 5.74 ms | 10.96 ms | 12968.00 | 142164.67 | 14.12 Mb |
| javascript (12.13)| muneem (2.4) | **7.79** ms | 6.71 ms | 11.31 ms | 9590.33 | 132685.00 | 13.18 Mb |
| ruby (2.6)| sinatra (2.0) | **7.90** ms | 5.66 ms | 19.21 ms | 8640.67 | 24793.00 | 4.27 Mb |
| python (3.7)| falcon (2.0) | **8.07** ms | 5.79 ms | 15.54 ms | 8192.33 | 131568.67 | 20.44 Mb |
| csharp (7.3)| aspnetcore (3.0) | **8.35** ms | 4.45 ms | 7.98 ms | 35623.67 | 193103.00 | 20.85 Mb |
| go (1.13)| echo (4.1) | **8.50** ms | 4.31 ms | 12.41 ms | 32340.00 | 188595.33 | 21.94 Mb |
| javascript (12.13)| foxify (0.1) | **8.74** ms | 7.27 ms | 11.82 ms | 14333.33 | 122836.67 | 17.11 Mb |
| cpp (11)| evhtp (1.2) | **8.87** ms | 8.66 ms | 11.87 ms | 2821.33 | 111427.67 | 7.17 Mb |
| go (1.13)| beego (1.12) | **9.08** ms | 5.73 ms | 21.04 ms | 10450.00 | 132637.00 | 11.80 Mb |
| go (1.13)| violetear (7.0) | **9.11** ms | 6.36 ms | 18.93 ms | 9059.00 | 123932.67 | 10.89 Mb |
| ruby (2.6)| grape (1.2) | **9.59** ms | 7.55 ms | 23.12 ms | 10167.00 | 20197.67 | 0.76 Mb |
| python (3.7)| bottle (0.12) | **9.60** ms | 8.06 ms | 17.15 ms | 10495.00 | 109264.33 | 17.84 Mb |
| go (1.13)| gin (1.5) | **9.94** ms | 6.15 ms | 22.78 ms | 12692.67 | 120003.00 | 13.96 Mb |
| rust (1.38)| actix-web (1.0) | **10.07** ms | 8.28 ms | 16.98 ms | 9557.00 | 149043.00 | 14.93 Mb |
| go (1.13)| goroute (0.0) | **10.11** ms | 4.61 ms | 16.39 ms | 38619.67 | 168948.00 | 19.65 Mb |
| python (3.7)| asgineer (0.7) | **10.18** ms | 9.13 ms | 18.11 ms | 5533.33 | 97769.00 | 11.57 Mb |
| javascript (12.13)| iotjs-express (0.0) | **10.23** ms | 7.94 ms | 14.03 ms | 17047.33 | 109564.00 | 29.51 Mb |
| javascript (12.13)| fastify (2.1) | **10.38** ms | 8.52 ms | 15.01 ms | 13654.00 | 109114.67 | 18.73 Mb |
| go (1.13)| webgo (3.0) | **10.59** ms | 6.62 ms | 23.50 ms | 14621.33 | 116100.67 | 10.24 Mb |
| python (3.7)| blacksheep (0.2) | **10.71** ms | 9.72 ms | 16.98 ms | 4529.33 | 91459.67 | 12.19 Mb |
| ruby (2.6)| plezi (0.16) | **11.54** ms | 10.59 ms | 16.10 ms | 5248.67 | 84731.67 | 11.97 Mb |
| javascript (12.13)| express (4.17) | **11.58** ms | 8.53 ms | 15.96 ms | 21703.33 | 96545.67 | 15.67 Mb |
| php (7.3)| one (1.8) | **11.61** ms | 10.57 ms | 20.50 ms | 6762.67 | 86102.00 | 13.12 Mb |
| javascript (12.13)| koa (2.11) | **11.95** ms | 9.34 ms | 17.29 ms | 18234.33 | 92981.00 | 13.05 Mb |
| python (3.7)| hug (2.6) | **12.07** ms | 9.66 ms | 20.62 ms | 9196.00 | 84735.67 | 13.92 Mb |
| javascript (12.13)| restify (8.4) | **12.09** ms | 11.41 ms | 14.63 ms | 9823.00 | 84131.33 | 9.79 Mb |
| python (3.7)| starlette (0.13) | **12.22** ms | 11.27 ms | 18.99 ms | 5506.67 | 81185.33 | 11.60 Mb |
| go (1.13)| air (0.13) | **12.29** ms | 6.84 ms | 30.42 ms | 17252.00 | 106623.33 | 14.75 Mb |
| kotlin (1.3)| ktor (1.2) | **12.40** ms | 8.89 ms | 24.09 ms | 21693.67 | 95645.67 | 9.88 Mb |
| php (7.3)| hyperf (1.0) | **12.47** ms | 9.61 ms | 25.41 ms | 9911.00 | 87339.00 | 12.35 Mb |
| go (1.13)| mars (1.0) | **13.36** ms | 5.70 ms | 36.88 ms | 18112.33 | 115284.67 | 17.21 Mb |
| php (7.3)| sw-fw-less (preview) | **13.45** ms | 12.16 ms | 24.26 ms | 7875.00 | 74084.67 | 11.29 Mb |
| php (7.3)| imi (1.0) | **15.59** ms | 15.19 ms | 19.98 ms | 4440.67 | 62364.33 | 9.50 Mb |
| swift (5.1)| vapor (3.3) | **16.68** ms | 8.54 ms | 15.81 ms | 70435.00 | 100653.67 | 11.16 Mb |
| swift (5.1)| kitura (2.8) | **16.95** ms | 14.41 ms | 15.14 ms | 38878.33 | 68106.67 | 8.40 Mb |
| php (7.3)| swoft (2.0) | **17.56** ms | 16.39 ms | 23.62 ms | 6483.33 | 55776.33 | 9.72 Mb |
| fsharp (7.3)| suave (2.5) | **19.01** ms | 14.06 ms | 23.90 ms | 50495.67 | 68270.00 | 9.17 Mb |
| python (3.7)| responder (2.0) | **19.67** ms | 19.08 ms | 29.09 ms | 7717.33 | 50245.33 | 7.27 Mb |
| swift (5.1)| kitura-nio (2.8) | **20.11** ms | 14.10 ms | 14.79 ms | 65842.00 | 69221.67 | 8.54 Mb |
| python (3.7)| fastapi (0.42) | **20.35** ms | 19.13 ms | 37.29 ms | 11173.33 | 49495.33 | 7.09 Mb |
| javascript (12.13)| moleculer (0.13) | **20.96** ms | 14.79 ms | 30.24 ms | 31749.00 | 55027.00 | 6.28 Mb |
| javascript (12.13)| hapi (18.4) | **22.83** ms | 16.66 ms | 29.64 ms | 39920.00 | 54959.67 | 9.47 Mb |
| java (8)| micronaut (1.2) | **23.48** ms | 9.71 ms | 46.90 ms | 58224.33 | 76340.00 | 10.28 Mb |
| python (3.7)| flask (1.1) | **23.63** ms | 19.92 ms | 35.84 ms | 12710.00 | 42066.33 | 6.87 Mb |
| crystal (0.31)| lucky (0.18) | **23.98** ms | 22.48 ms | 27.97 ms | 13238.67 | 41496.00 | 3.39 Mb |
| python (3.7)| clastic (19.9) | **25.41** ms | 20.49 ms | 43.72 ms | 14521.00 | 39885.67 | 6.55 Mb |
| python (3.7)| molten (0.27) | **26.86** ms | 21.49 ms | 44.81 ms | 16268.33 | 39128.00 | 4.82 Mb |
| php (7.3)| slim (4.3) | **27.63** ms | 8.34 ms | 49.87 ms | 66815.00 | 94866.33 | 28.41 Mb |
| java (8)| spring-boot (2.1) | **27.73** ms | 7.53 ms | 11.92 ms | 116224.67 | 117724.00 | 6.04 Mb |
| python (3.7)| aiohttp (3.6) | **27.80** ms | 27.74 ms | 47.63 ms | 13699.67 | 36182.33 | 5.44 Mb |
| php (7.3)| basicphp (0.9) | **27.86** ms | 8.28 ms | 50.18 ms | 66074.00 | 94278.33 | 28.31 Mb |
| php (7.3)| zend-expressive (3.2) | **28.92** ms | 8.23 ms | 51.42 ms | 74103.00 | 95291.33 | 28.54 Mb |
| javascript (12.13)| turbo_polka (0.3) | **29.29** ms | 26.92 ms | 29.37 ms | 36333.33 | 35967.33 | 2.24 Mb |
| php (7.3)| spiral (2.3) | **29.70** ms | 29.15 ms | 32.49 ms | 6133.67 | 33287.67 | 3.84 Mb |
| crystal (0.31)| athena (0.7) | **29.75** ms | 25.42 ms | 73.76 ms | 33708.00 | 46320.67 | 3.86 Mb |
| python (3.7)| sanic (19.9) | **29.98** ms | 24.86 ms | 56.80 ms | 20092.67 | 34871.33 | 4.13 Mb |
| php (7.3)| zend-framework (3.1) | **31.47** ms | 8.89 ms | 56.25 ms | 74463.33 | 89735.00 | 26.88 Mb |
| php (7.3)| lumen (6.2) | **35.13** ms | 9.93 ms | 67.98 ms | 83962.33 | 78196.00 | 23.43 Mb |
| php (7.3)| symfony (4.3) | **35.74** ms | 9.96 ms | 63.01 ms | 90226.33 | 78791.00 | 23.61 Mb |
| python (3.7)| bocadillo (0.18) | **38.14** ms | 34.23 ms | 64.94 ms | 22522.00 | 26942.67 | 3.45 Mb |
| go (1.13)| gramework (1.7) | **42.14** ms | 43.10 ms | 44.88 ms | 6136.33 | 23035.67 | 3.91 Mb |
| java (8)| javalin (3.5) | **45.40** ms | 4.76 ms | 37.61 ms | 228553.67 | 149847.33 | 17.73 Mb |
| clojure (1.10)| coast (1.0) | **47.10** ms | 14.96 ms | 18.24 ms | 200288.00 | 61969.00 | 7.39 Mb |
| ruby (2.6)| rails (6.0) | **48.62** ms | 21.32 ms | 133.78 ms | 66672.67 | 4050.33 | 1.69 Mb |
| scala (2.12)| http4s (0.18) | **50.59** ms | 6.66 ms | 20.74 ms | 256451.00 | 103866.33 | 12.08 Mb |
| python (3.7)| quart (0.10) | **52.97** ms | 49.27 ms | 86.24 ms | 24793.00 | 18572.00 | 2.46 Mb |
| php (7.3)| laravel (6.5) | **57.11** ms | 11.34 ms | 73.30 ms | 153047.00 | 68156.33 | 20.49 Mb |
| python (3.7)| tornado (5.1) | **71.34** ms | 72.37 ms | 82.40 ms | 29171.33 | 13621.33 | 2.67 Mb |
| python (3.7)| cherrypy (18.3) | **75.49** ms | 61.48 ms | 135.43 ms | 39693.33 | 13593.00 | 2.41 Mb |
| python (3.7)| django (2.2) | **76.56** ms | 63.48 ms | 122.74 ms | 39465.00 | 13071.00 | 2.51 Mb |
| python (3.7)| masonite (2.2) | **108.23** ms | 103.25 ms | 150.25 ms | 37949.00 | 8960.00 | 1.46 Mb |
| crystal (0.31)| onyx (0.5) | **160.53** ms | 158.90 ms | 188.66 ms | 23007.67 | 6127.00 | 1.05 Mb |
| scala (2.12)| akkahttp (10.1) | **193.07** ms | 4.94 ms | 242.16 ms | 724006.67 | 142791.33 | 20.47 Mb |
| perl (5.3)| dancer2 (2.0) | **211.70** ms | 31.13 ms | 511.49 ms | 667639.67 | 2449.67 | 0.37 Mb |
| python (3.7)| cyclone (1.3) | **274.43** ms | 224.60 ms | 272.36 ms | 438543.00 | 3427.67 | 0.58 Mb |
| julia (1.3)| merly (0.2) | **320.20** ms | 98.39 ms | 399.06 ms | 877345.67 | 7415.67 | 0.59 Mb |
| python (3.7)| klein (19.6) | **446.01** ms | 342.90 ms | 416.59 ms | 681564.33 | 2260.00 | 0.33 Mb |
| python (3.7)| nameko (2.12) | **460.00** ms | 368.18 ms | 396.19 ms | 694145.33 | 2074.33 | 0.29 Mb |

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

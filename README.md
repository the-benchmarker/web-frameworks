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
| rust (1.38)| nickel (0.11) | **0.24** ms | 0.20 ms | 0.39 ms | 199.33 | 37636.67 | 4.97 Mb |
| ruby (2.6)| syro (3.1) | **2.70** ms | 0.63 ms | 7.94 ms | 4143.67 | 46844.33 | 1.80 Mb |
| ruby (2.6)| roda (3.25) | **2.79** ms | 0.67 ms | 8.10 ms | 4188.67 | 45494.67 | 2.88 Mb |
| rust (1.38)| iron (0.6) | **3.04** ms | 2.93 ms | 4.49 ms | 1368.67 | 21289.67 | 1.76 Mb |
| ruby (2.6)| cuba (3.9) | **3.06** ms | 0.57 ms | 9.41 ms | 5064.33 | 41756.67 | 3.27 Mb |
| ruby (2.6)| rack-routing (0.0) | **3.84** ms | 0.73 ms | 11.38 ms | 5596.00 | 33197.00 | 1.27 Mb |
| c (11)| agoo-c (0.7) | **4.59** ms | 4.28 ms | 8.86 ms | 3377.00 | 209196.00 | 8.02 Mb |
| ruby (2.6)| camping (2.1) | **4.67** ms | 0.49 ms | 16.08 ms | 8578.67 | 27517.67 | 1.74 Mb |
| node (12.11)| sifrr (0.0) | **4.79** ms | 4.32 ms | 9.82 ms | 4054.33 | 203003.67 | 11.84 Mb |
| nim (1.0)| httpbeast (0.2) | **5.02** ms | 4.54 ms | 9.61 ms | 3651.00 | 192873.33 | 18.20 Mb |
| python (3.7)| japronto (0.1) | **5.13** ms | 4.63 ms | 9.96 ms | 3909.00 | 189149.67 | 15.00 Mb |
| cpp (11)| drogon (1.0) | **5.42** ms | 4.90 ms | 9.91 ms | 3546.00 | 177889.33 | 11.44 Mb |
| ruby (2.6)| flame (4.18) | **5.47** ms | 0.49 ms | 19.16 ms | 10784.33 | 23469.67 | 0.90 Mb |
| swift (5.1)| swifter (1.4) | **5.56** ms | 0.85 ms | 14.54 ms | 86445.00 | 11871.00 | 1.01 Mb |
| cpp (11)| evhtp (1.2) | **5.90** ms | 5.24 ms | 9.56 ms | 2929.33 | 160195.67 | 10.30 Mb |
| go (1.13)| gorouter-fasthttp (4.2) | **6.04** ms | 5.38 ms | 9.31 ms | 3625.67 | 156380.00 | 16.64 Mb |
| go (1.13)| fasthttprouter (0.1) | **6.18** ms | 5.11 ms | 9.14 ms | 9601.00 | 161432.00 | 17.23 Mb |
| go (1.13)| atreugo (8.2) | **6.26** ms | 5.19 ms | 9.27 ms | 8758.00 | 159818.33 | 21.31 Mb |
| go (1.13)| fasthttp (1.5) | **6.33** ms | 4.87 ms | 8.99 ms | 12920.33 | 168733.33 | 18.05 Mb |
| crystal (0.31)| router.cr (0.2) | **6.46** ms | 5.66 ms | 10.55 ms | 3220.00 | 149292.33 | 9.31 Mb |
| crystal (0.31)| toro (0.4) | **6.47** ms | 5.67 ms | 10.55 ms | 3222.00 | 148845.33 | 9.28 Mb |
| ruby (2.6)| hanami (1.3) | **6.66** ms | 0.60 ms | 22.99 ms | 11912.33 | 19246.00 | 9.67 Mb |
| crystal (0.31)| raze (0.3) | **6.75** ms | 5.91 ms | 10.91 ms | 3312.00 | 143044.33 | 8.91 Mb |
| java (8)| rapidoid (5.5) | **6.80** ms | 5.09 ms | 11.01 ms | 13146.00 | 163718.00 | 19.53 Mb |
| crystal (0.31)| kemal (0.28) | **7.09** ms | 6.44 ms | 10.91 ms | 3221.67 | 135783.67 | 14.71 Mb |
| nim (1.0)| jester (0.4) | **7.29** ms | 6.65 ms | 11.72 ms | 3980.00 | 145089.00 | 19.33 Mb |
| c (11)| kore (3.3) | **7.41** ms | 5.96 ms | 13.15 ms | 9455.67 | 161905.67 | 29.17 Mb |
| crystal (0.31)| amber (0.3) | **7.67** ms | 7.04 ms | 12.12 ms | 3559.67 | 126369.67 | 15.33 Mb |
| ruby (2.6)| sinatra (2.0) | **7.97** ms | 0.68 ms | 26.40 ms | 13397.67 | 16038.67 | 2.76 Mb |
| crystal (0.31)| orion (1.7) | **8.35** ms | 7.80 ms | 13.19 ms | 3888.33 | 116803.00 | 12.65 Mb |
| ruby (2.6)| grape (1.2) | **9.51** ms | 0.80 ms | 31.25 ms | 15583.67 | 13576.00 | 0.51 Mb |
| java (8)| act (1.8) | **9.73** ms | 7.65 ms | 13.12 ms | 15954.67 | 121682.33 | 13.92 Mb |
| go (1.13)| gorouter (4.2) | **9.88** ms | 8.05 ms | 16.18 ms | 9871.67 | 105554.67 | 9.31 Mb |
| go (1.13)| rte (0.0) | **9.89** ms | 7.84 ms | 15.73 ms | 13656.33 | 107851.67 | 9.58 Mb |
| rust (1.38)| actix-web (1.0) | **10.23** ms | 9.78 ms | 13.79 ms | 3258.67 | 105115.33 | 10.09 Mb |
| go (1.13)| echo (4.1) | **10.69** ms | 8.56 ms | 19.02 ms | 8175.67 | 96770.67 | 11.26 Mb |
| go (1.13)| violetear (7.0) | **10.72** ms | 8.90 ms | 16.17 ms | 12167.00 | 97186.00 | 8.55 Mb |
| go (1.13)| gin (1.4) | **10.99** ms | 8.60 ms | 19.20 ms | 10553.00 | 96706.33 | 11.25 Mb |
| go (1.13)| goroute (0.0) | **11.04** ms | 8.56 ms | 19.05 ms | 13101.00 | 96836.00 | 11.27 Mb |
| go (1.13)| chi (4.0) | **11.16** ms | 8.27 ms | 18.36 ms | 17216.00 | 101013.67 | 8.96 Mb |
| go (1.13)| beego (1.12) | **11.22** ms | 8.77 ms | 19.63 ms | 10964.00 | 95757.67 | 8.54 Mb |
| go (1.13)| kami (2.2) | **11.31** ms | 8.63 ms | 17.37 ms | 18533.00 | 98168.67 | 8.66 Mb |
| go (1.13)| webgo (3.0) | **11.62** ms | 9.32 ms | 18.84 ms | 12303.33 | 91533.00 | 8.08 Mb |
| python (3.7)| falcon (2.0) | **12.45** ms | 10.15 ms | 20.53 ms | 7317.67 | 80978.00 | 12.58 Mb |
| go (1.13)| air (0.13) | **12.70** ms | 9.58 ms | 23.38 ms | 12702.00 | 84610.00 | 11.70 Mb |
| swift (5.1)| perfect (3.1) | **12.99** ms | 13.07 ms | 15.50 ms | 4827.33 | 74517.00 | 4.64 Mb |
| go (1.13)| gorilla-mux (1.7) | **13.51** ms | 8.56 ms | 19.96 ms | 33361.33 | 95175.67 | 8.43 Mb |
| csharp (7.3)| aspnetcore (2.2) | **13.52** ms | 9.69 ms | 16.21 ms | 33059.33 | 87579.33 | 9.45 Mb |
| node (12.11)| polkadot (1.0) | **13.54** ms | 9.24 ms | 17.63 ms | 33076.33 | 93298.67 | 9.27 Mb |
| go (1.13)| gf (1.9) | **13.58** ms | 10.90 ms | 23.38 ms | 11764.67 | 77603.00 | 8.73 Mb |
| php (7.3)| one (1.8) | **13.74** ms | 12.47 ms | 23.63 ms | 8067.67 | 73012.00 | 11.12 Mb |
| ruby (2.6)| agoo (2.11) | **13.96** ms | 13.56 ms | 18.36 ms | 3473.67 | 70218.33 | 2.69 Mb |
| node (12.11)| 0http (1.2) | **15.87** ms | 9.40 ms | 17.70 ms | 45955.33 | 91692.00 | 9.11 Mb |
| php (7.3)| hyperf (1.0) | **16.97** ms | 14.55 ms | 32.28 ms | 11795.33 | 61592.33 | 8.70 Mb |
| rust (1.38)| gotham (0.4) | **17.32** ms | 16.37 ms | 24.25 ms | 18398.00 | 59471.00 | 8.00 Mb |
| node (12.11)| rayo (1.3) | **17.50** ms | 10.49 ms | 20.18 ms | 48826.33 | 80132.33 | 7.96 Mb |
| node (12.11)| polka (0.5) | **17.51** ms | 10.14 ms | 19.82 ms | 50286.67 | 82259.33 | 8.17 Mb |
| ruby (2.6)| plezi (0.16) | **17.71** ms | 16.45 ms | 23.31 ms | 8987.67 | 55487.33 | 7.84 Mb |
| python (3.7)| bottle (0.12) | **18.49** ms | 15.86 ms | 30.21 ms | 10116.67 | 54975.33 | 8.98 Mb |
| php (7.3)| sw-fw-less (preview) | **19.53** ms | 17.97 ms | 31.21 ms | 9924.00 | 50967.00 | 7.76 Mb |
| python (3.7)| blacksheep (0.2) | **19.53** ms | 17.34 ms | 32.78 ms | 10283.33 | 51572.67 | 6.88 Mb |
| kotlin (1.3)| ktor (1.2) | **19.86** ms | 12.19 ms | 29.10 ms | 52312.67 | 72213.00 | 7.46 Mb |
| python (3.7)| asgineer (0.7) | **20.12** ms | 17.88 ms | 33.06 ms | 10311.67 | 49954.00 | 5.91 Mb |
| node (12.11)| restana (3.3) | **20.18** ms | 9.67 ms | 19.11 ms | 67392.00 | 88524.00 | 8.79 Mb |
| node (12.11)| muneem (2.4) | **21.71** ms | 11.72 ms | 22.32 ms | 65248.00 | 72124.00 | 7.16 Mb |
| python (3.7)| hug (2.6) | **21.91** ms | 17.88 ms | 35.48 ms | 13931.67 | 46597.00 | 7.66 Mb |
| python (3.7)| starlette (0.12) | **22.08** ms | 18.90 ms | 37.56 ms | 11508.33 | 45531.33 | 6.50 Mb |
| node (12.11)| foxify (0.1) | **22.35** ms | 12.59 ms | 23.78 ms | 62906.67 | 68189.00 | 9.50 Mb |
| clojure (1.10)| coast (1.0) | **22.53** ms | 19.66 ms | 21.77 ms | 36208.00 | 48332.00 | 5.76 Mb |
| php (7.3)| swoft (2.0) | **23.21** ms | 22.53 ms | 30.76 ms | 6650.33 | 41957.67 | 7.31 Mb |
| node (12.11)| iotjs-express (0.0) | **24.19** ms | 14.39 ms | 26.72 ms | 64464.67 | 59753.67 | 16.09 Mb |
| swift (5.1)| kitura-nio (2.8) | **25.57** ms | 20.16 ms | 23.38 ms | 61899.00 | 47177.00 | 5.82 Mb |
| php (7.3)| imi (1.0) | **26.63** ms | 25.65 ms | 33.72 ms | 6709.67 | 36652.00 | 5.58 Mb |
| swift (5.1)| kitura (2.8) | **27.10** ms | 20.72 ms | 23.75 ms | 66177.67 | 46502.67 | 5.73 Mb |
| node (12.11)| restify (8.4) | **28.43** ms | 19.08 ms | 31.25 ms | 59532.00 | 45832.33 | 5.33 Mb |
| node (12.11)| koa (2.8) | **28.76** ms | 13.84 ms | 26.71 ms | 85051.00 | 60916.33 | 8.55 Mb |
| node (12.11)| express (4.17) | **29.69** ms | 15.76 ms | 29.73 ms | 82691.33 | 53886.00 | 8.75 Mb |
| java (8)| spring-boot (2.1) | **29.70** ms | 16.10 ms | 36.25 ms | 86239.67 | 47422.33 | 2.52 Mb |
| node (12.11)| fastify (2.8) | **32.84** ms | 15.00 ms | 27.96 ms | 105969.00 | 60039.33 | 10.56 Mb |
| ruby (2.6)| rails (6.0) | **33.29** ms | 2.49 ms | 110.27 ms | 63071.67 | 3850.33 | 1.61 Mb |
| python (3.7)| fastapi (0.42) | **36.39** ms | 32.15 ms | 60.51 ms | 19152.00 | 27792.00 | 3.98 Mb |
| python (3.7)| responder (2.0) | **36.72** ms | 34.41 ms | 58.42 ms | 16339.33 | 27054.33 | 3.91 Mb |
| crystal (0.31)| spider-gazelle (1.6) | **37.63** ms | 35.73 ms | 45.36 ms | 15535.33 | 26014.67 | 1.84 Mb |
| python (3.7)| clastic (19.9) | **40.15** ms | 33.18 ms | 65.65 ms | 19619.00 | 24875.67 | 4.09 Mb |
| python (3.7)| molten (0.27) | **40.30** ms | 33.85 ms | 66.82 ms | 19592.00 | 25365.67 | 3.13 Mb |
| fsharp (7.3)| suave (2.5) | **40.58** ms | 24.44 ms | 100.56 ms | 50407.67 | 24596.33 | 3.30 Mb |
| python (3.7)| flask (1.1) | **41.74** ms | 36.29 ms | 63.43 ms | 16853.67 | 23598.00 | 3.85 Mb |
| crystal (0.31)| lucky (0.18) | **42.86** ms | 40.21 ms | 52.32 ms | 14370.33 | 22844.33 | 1.87 Mb |
| node (12.11)| turbo_polka (2.0) | **44.09** ms | 42.05 ms | 48.93 ms | 22248.33 | 22322.00 | 1.39 Mb |
| python (3.7)| aiohttp (3.6) | **44.17** ms | 42.06 ms | 69.09 ms | 19059.33 | 22607.00 | 3.40 Mb |
| python (3.7)| bocadillo (0.18) | **52.16** ms | 45.99 ms | 87.88 ms | 30216.00 | 19473.67 | 2.50 Mb |
| java (8)| micronaut (1.2) | **52.78** ms | 23.20 ms | 97.08 ms | 128239.67 | 24704.67 | 3.43 Mb |
| swift (5.1)| vapor (3.3) | **53.36** ms | 17.34 ms | 32.92 ms | 219012.33 | 48897.33 | 5.54 Mb |
| php (7.3)| lumen (6.2) | **53.83** ms | 18.27 ms | 114.41 ms | 112798.67 | 43774.33 | 14.42 Mb |
| php (7.3)| slim (4.3) | **54.73** ms | 18.51 ms | 112.98 ms | 117455.33 | 43835.00 | 14.43 Mb |
| php (7.3)| zend-expressive (3.2) | **55.56** ms | 18.58 ms | 123.67 ms | 115767.33 | 43418.33 | 14.30 Mb |
| python (3.7)| sanic (19.9) | **57.11** ms | 51.21 ms | 96.65 ms | 38019.00 | 18091.00 | 2.14 Mb |
| php (7.3)| basicphp (0.9) | **58.93** ms | 19.67 ms | 119.40 ms | 123055.33 | 40573.00 | 13.40 Mb |
| php (7.3)| spiral (2.2) | **59.60** ms | 59.64 ms | 66.25 ms | 8035.67 | 16327.33 | 1.88 Mb |
| php (7.3)| symfony (4.3) | **59.85** ms | 19.16 ms | 119.08 ms | 133181.33 | 40957.33 | 13.49 Mb |
| php (7.3)| zend-framework (3.1) | **59.94** ms | 19.09 ms | 127.72 ms | 129446.67 | 41951.33 | 13.82 Mb |
| scala (2.12)| http4s (0.18) | **65.22** ms | 19.31 ms | 45.01 ms | 257133.67 | 45286.00 | 5.27 Mb |
| node (12.11)| hapi (18.4) | **66.34** ms | 24.26 ms | 46.49 ms | 204446.67 | 35301.33 | 6.07 Mb |
| crystal (0.31)| athena (0.7) | **67.28** ms | 48.80 ms | 180.47 ms | 84270.67 | 24144.67 | 2.01 Mb |
| php (7.3)| laravel (6.4) | **81.27** ms | 22.88 ms | 171.46 ms | 188420.33 | 35942.33 | 11.89 Mb |
| node (12.11)| moleculer (0.13) | **85.99** ms | 27.18 ms | 60.05 ms | 254224.67 | 30295.33 | 3.46 Mb |
| python (3.7)| quart (0.10) | **88.90** ms | 75.34 ms | 156.00 ms | 46756.33 | 11209.00 | 1.48 Mb |
| python (3.7)| cherrypy (18.3) | **89.78** ms | 73.66 ms | 79.81 ms | 233050.00 | 1373.67 | 0.21 Mb |
| go (1.13)| gramework (1.6) | **96.01** ms | 97.62 ms | 102.01 ms | 18890.33 | 10148.00 | 1.72 Mb |
| python (3.7)| tornado (5.1) | **101.51** ms | 100.19 ms | 126.70 ms | 34740.67 | 9525.33 | 1.87 Mb |
| python (3.7)| django (2.2) | **105.60** ms | 93.93 ms | 163.05 ms | 36832.33 | 9189.33 | 1.77 Mb |
| java (8)| javalin (3.5) | **125.41** ms | 11.66 ms | 290.35 ms | 362928.33 | 56370.67 | 6.67 Mb |
| python (3.7)| masonite (2.2) | **138.54** ms | 129.50 ms | 179.52 ms | 53997.67 | 6988.67 | 1.14 Mb |
| perl (5.3)| dancer2 (2.0) | **162.32** ms | 58.98 ms | 364.69 ms | 338036.67 | 1492.00 | 0.22 Mb |
| crystal (0.31)| onyx (0.5) | **193.90** ms | 193.25 ms | 226.42 ms | 28283.67 | 5066.00 | 0.87 Mb |
| scala (2.12)| akkahttp (10.1) | **220.51** ms | 7.35 ms | 96.23 ms | 873321.67 | 65406.00 | 9.38 Mb |
| python (3.7)| cyclone (1.3) | **399.23** ms | 351.41 ms | 445.11 ms | 460804.67 | 2202.67 | 0.37 Mb |
| python (3.7)| nameko (2.12) | **655.34** ms | 551.15 ms | 613.68 ms | 770037.67 | 1278.00 | 0.18 Mb |

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

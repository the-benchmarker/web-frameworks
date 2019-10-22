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
| rust (1.38)| nickel (0.11) | **0.26** ms | 0.22 ms | 0.43 ms | 204.33 | 38186.00 | 5.04 Mb |
| ruby (2.6)| roda (3.25) | **2.65** ms | 0.57 ms | 8.02 ms | 4278.33 | 47927.00 | 3.03 Mb |
| ruby (2.6)| syro (3.1) | **2.65** ms | 0.55 ms | 7.98 ms | 4203.33 | 47855.67 | 1.83 Mb |
| ruby (2.6)| cuba (3.9) | **3.11** ms | 0.60 ms | 9.35 ms | 4758.33 | 40953.00 | 3.21 Mb |
| rust (1.38)| iron (0.6) | **3.15** ms | 3.05 ms | 4.55 ms | 1335.33 | 20760.33 | 1.71 Mb |
| ruby (2.6)| rack-routing (0.0) | **3.67** ms | 0.47 ms | 11.81 ms | 6376.00 | 34856.33 | 1.34 Mb |
| ruby (2.6)| camping (2.1) | **4.64** ms | 0.72 ms | 15.38 ms | 8051.00 | 27648.33 | 1.75 Mb |
| c (11)| agoo-c (0.7) | **4.76** ms | 4.42 ms | 9.58 ms | 3827.33 | 208969.00 | 8.01 Mb |
| node (12.11)| sifrr (0.0) | **4.84** ms | 4.27 ms | 10.02 ms | 4194.67 | 204100.00 | 11.91 Mb |
| python (3.7)| japronto (0.1) | **5.00** ms | 4.44 ms | 9.89 ms | 3933.67 | 196106.00 | 15.56 Mb |
| nim (1.0)| httpbeast (0.2) | **5.20** ms | 4.57 ms | 10.14 ms | 4019.00 | 189405.33 | 17.87 Mb |
| ruby (2.6)| flame (4.18) | **5.35** ms | 0.48 ms | 18.67 ms | 10537.00 | 24020.67 | 0.92 Mb |
| cpp (11)| drogon (1.0) | **5.47** ms | 4.83 ms | 10.14 ms | 3693.33 | 177308.33 | 11.40 Mb |
| cpp (11)| evhtp (1.2) | **5.88** ms | 5.17 ms | 9.55 ms | 2982.33 | 162363.67 | 10.44 Mb |
| go (1.13)| atreugo (8.2) | **6.05** ms | 5.02 ms | 8.94 ms | 8578.33 | 164940.33 | 21.99 Mb |
| ruby (2.6)| hanami (1.3) | **6.11** ms | 0.71 ms | 19.94 ms | 10268.00 | 20912.67 | 10.51 Mb |
| go (1.13)| gorouter-fasthttp (4.2) | **6.12** ms | 5.14 ms | 9.19 ms | 6989.00 | 161200.33 | 17.15 Mb |
| swift (5.1)| swifter (1.4) | **6.13** ms | 1.06 ms | 14.81 ms | 71474.00 | 12188.00 | 1.04 Mb |
| java (8)| rapidoid (5.5) | **6.27** ms | 4.83 ms | 10.73 ms | 9373.67 | 171537.00 | 20.46 Mb |
| crystal (0.31)| toro (0.4) | **6.43** ms | 5.66 ms | 10.40 ms | 3154.00 | 149787.67 | 9.34 Mb |
| crystal (0.31)| router.cr (0.2) | **6.45** ms | 5.61 ms | 10.64 ms | 3287.33 | 149822.00 | 9.34 Mb |
| c (11)| kore (3.3) | **6.56** ms | 5.93 ms | 12.28 ms | 4618.67 | 165073.33 | 29.75 Mb |
| crystal (0.31)| raze (0.3) | **6.59** ms | 5.87 ms | 10.34 ms | 3054.33 | 145730.67 | 9.08 Mb |
| go (1.13)| fasthttprouter (0.1) | **6.74** ms | 5.09 ms | 9.04 ms | 14574.67 | 163461.33 | 17.44 Mb |
| crystal (0.31)| kemal (0.28) | **7.10** ms | 6.29 ms | 11.29 ms | 3380.67 | 136308.33 | 14.77 Mb |
| nim (1.0)| jester (0.4) | **7.47** ms | 6.67 ms | 12.25 ms | 4208.67 | 142282.67 | 18.95 Mb |
| crystal (0.31)| amber (0.3) | **7.59** ms | 6.94 ms | 11.90 ms | 3518.00 | 127914.67 | 15.52 Mb |
| ruby (2.6)| sinatra (2.0) | **7.76** ms | 0.65 ms | 25.76 ms | 12982.67 | 16466.33 | 2.84 Mb |
| crystal (0.31)| orion (1.7) | **8.39** ms | 7.93 ms | 13.06 ms | 3751.67 | 116288.33 | 12.60 Mb |
| ruby (2.6)| grape (1.2) | **9.53** ms | 0.85 ms | 30.82 ms | 14913.00 | 13517.67 | 0.51 Mb |
| go (1.13)| rte (0.0) | **9.66** ms | 7.57 ms | 15.34 ms | 13373.67 | 111454.33 | 9.90 Mb |
| go (1.13)| gorouter (4.2) | **9.79** ms | 7.86 ms | 15.76 ms | 11335.33 | 108697.67 | 9.59 Mb |
| go (1.13)| chi (4.0) | **9.82** ms | 8.08 ms | 16.83 ms | 7067.67 | 104626.00 | 9.28 Mb |
| rust (1.38)| actix-web (1.0) | **10.02** ms | 9.54 ms | 13.30 ms | 2870.33 | 108414.00 | 10.39 Mb |
| go (1.13)| goroute (0.0) | **10.33** ms | 8.24 ms | 18.14 ms | 9079.00 | 100857.33 | 11.73 Mb |
| go (1.13)| violetear (7.0) | **10.33** ms | 8.73 ms | 15.52 ms | 9712.67 | 100305.33 | 8.83 Mb |
| go (1.13)| beego (1.12) | **10.50** ms | 8.45 ms | 18.21 ms | 8880.67 | 99230.67 | 8.85 Mb |
| go (1.13)| echo (4.1) | **10.53** ms | 8.27 ms | 18.28 ms | 10527.33 | 100345.33 | 11.67 Mb |
| java (8)| act (1.8) | **10.74** ms | 7.30 ms | 12.54 ms | 29279.67 | 129284.00 | 14.79 Mb |
| go (1.13)| gin (1.4) | **10.81** ms | 8.31 ms | 18.06 ms | 14608.00 | 99656.33 | 11.59 Mb |
| go (1.13)| gorilla-mux (1.7) | **10.87** ms | 8.37 ms | 19.71 ms | 10512.00 | 98112.33 | 8.68 Mb |
| go (1.13)| kami (2.2) | **12.20** ms | 8.42 ms | 16.68 ms | 28841.67 | 101725.00 | 8.97 Mb |
| python (3.7)| falcon (2.0) | **12.31** ms | 9.95 ms | 20.39 ms | 7719.33 | 82047.33 | 12.74 Mb |
| swift (5.1)| perfect (3.1) | **12.33** ms | 12.39 ms | 14.69 ms | 4003.33 | 79191.33 | 4.94 Mb |
| go (1.13)| gf (1.9) | **12.79** ms | 10.56 ms | 21.55 ms | 9361.67 | 80606.67 | 9.08 Mb |
| go (1.13)| air (0.13) | **13.14** ms | 9.41 ms | 22.22 ms | 20232.33 | 87424.00 | 12.09 Mb |
| node (12.11)| 0http (1.2) | **13.55** ms | 9.34 ms | 17.96 ms | 30234.67 | 90932.00 | 9.03 Mb |
| ruby (2.6)| agoo (2.11) | **13.68** ms | 13.27 ms | 16.99 ms | 3530.33 | 72104.33 | 2.76 Mb |
| php (7.3)| one (1.8) | **13.79** ms | 12.56 ms | 23.66 ms | 8045.67 | 73544.00 | 11.20 Mb |
| csharp (7.3)| aspnetcore (2.2) | **15.42** ms | 9.51 ms | 15.95 ms | 45597.67 | 90192.33 | 9.74 Mb |
| python (3.7)| bottle (0.12) | **16.09** ms | 13.32 ms | 25.74 ms | 9305.67 | 62433.33 | 10.20 Mb |
| node (12.11)| restana (3.3) | **16.38** ms | 9.72 ms | 19.08 ms | 47209.00 | 87476.33 | 8.69 Mb |
| python (3.7)| asgineer (0.7) | **17.10** ms | 15.79 ms | 25.27 ms | 7313.00 | 58318.00 | 6.90 Mb |
| php (7.3)| hyperf (1.0) | **17.34** ms | 15.10 ms | 32.01 ms | 11470.67 | 59848.67 | 8.46 Mb |
| rust (1.38)| gotham (0.4) | **18.31** ms | 17.44 ms | 24.51 ms | 20751.33 | 57728.67 | 7.76 Mb |
| ruby (2.6)| plezi (0.16) | **18.31** ms | 16.82 ms | 23.36 ms | 12612.00 | 54544.00 | 7.71 Mb |
| python (3.7)| blacksheep (0.2) | **18.72** ms | 17.20 ms | 30.89 ms | 9574.00 | 53785.67 | 7.17 Mb |
| node (12.11)| polkadot (1.0) | **19.08** ms | 9.30 ms | 18.11 ms | 63079.33 | 92804.00 | 9.22 Mb |
| node (12.11)| rayo (1.3) | **19.31** ms | 10.40 ms | 20.10 ms | 57810.00 | 79914.33 | 7.94 Mb |
| python (3.7)| hug (2.6) | **20.19** ms | 17.41 ms | 32.34 ms | 10743.33 | 50144.67 | 8.24 Mb |
| kotlin (1.3)| ktor (1.2) | **20.45** ms | 11.82 ms | 27.92 ms | 54411.00 | 73606.33 | 7.61 Mb |
| node (12.11)| polka (0.5) | **21.10** ms | 10.50 ms | 20.63 ms | 67727.67 | 80376.33 | 7.98 Mb |
| node (12.11)| foxify (0.1) | **21.18** ms | 12.67 ms | 23.72 ms | 54775.67 | 67399.33 | 9.39 Mb |
| python (3.7)| starlette (0.12) | **22.10** ms | 20.08 ms | 35.96 ms | 9883.67 | 44906.00 | 6.41 Mb |
| php (7.3)| swoft (2.0) | **23.12** ms | 22.56 ms | 29.71 ms | 6027.67 | 42309.67 | 7.38 Mb |
| swift (5.1)| kitura-nio (2.8) | **23.83** ms | 19.90 ms | 22.09 ms | 47942.00 | 48730.00 | 6.01 Mb |
| node (12.11)| muneem (2.4) | **25.32** ms | 11.61 ms | 22.67 ms | 84798.00 | 72368.67 | 7.19 Mb |
| node (12.11)| fastify (2.8) | **26.08** ms | 15.14 ms | 28.29 ms | 69677.33 | 58746.33 | 10.36 Mb |
| node (12.11)| restify (8.4) | **26.82** ms | 19.02 ms | 31.28 ms | 48614.33 | 45682.00 | 5.31 Mb |
| swift (5.1)| kitura (2.8) | **26.86** ms | 20.22 ms | 22.44 ms | 76954.33 | 47958.00 | 5.91 Mb |
| node (12.11)| express (4.17) | **27.75** ms | 15.77 ms | 29.52 ms | 72832.33 | 53803.67 | 8.73 Mb |
| node (12.11)| iotjs-express (0.0) | **28.03** ms | 14.59 ms | 27.77 ms | 83679.00 | 58504.67 | 15.76 Mb |
| php (7.3)| imi (1.0) | **28.69** ms | 27.85 ms | 33.75 ms | 6401.00 | 34085.67 | 5.19 Mb |
| swift (5.1)| vapor (3.3) | **29.07** ms | 17.25 ms | 30.79 ms | 90645.00 | 50413.33 | 5.72 Mb |
| node (12.11)| koa (2.8) | **29.26** ms | 13.86 ms | 26.87 ms | 89358.33 | 61016.00 | 8.56 Mb |
| ruby (2.6)| rails (6.0) | **33.03** ms | 2.51 ms | 109.53 ms | 62672.67 | 3872.33 | 1.62 Mb |
| fsharp (7.3)| suave (2.5) | **33.43** ms | 21.36 ms | 56.19 ms | 53951.33 | 31266.00 | 4.20 Mb |
| python (3.7)| responder (2.0) | **35.56** ms | 32.80 ms | 56.83 ms | 16579.00 | 28073.33 | 4.06 Mb |
| python (3.7)| fastapi (0.42) | **35.77** ms | 31.22 ms | 60.95 ms | 20013.67 | 28603.67 | 4.09 Mb |
| crystal (0.31)| spider-gazelle (1.6) | **37.23** ms | 35.55 ms | 44.88 ms | 15304.67 | 26196.33 | 1.85 Mb |
| python (3.7)| clastic (19.9) | **37.94** ms | 34.00 ms | 55.19 ms | 14882.00 | 25915.00 | 4.26 Mb |
| python (3.7)| molten (0.27) | **39.12** ms | 33.24 ms | 62.78 ms | 17942.00 | 25875.33 | 3.19 Mb |
| python (3.7)| flask (1.1) | **40.73** ms | 36.87 ms | 57.39 ms | 17265.67 | 24186.33 | 3.95 Mb |
| python (3.7)| aiohttp (3.6) | **41.99** ms | 40.01 ms | 61.98 ms | 14151.67 | 23612.33 | 3.55 Mb |
| php (7.3)| basicphp (0.9) | **42.53** ms | 14.65 ms | 86.02 ms | 89071.67 | 54753.33 | 18.03 Mb |
| crystal (0.31)| lucky (0.18) | **44.70** ms | 40.96 ms | 53.14 ms | 34426.33 | 22411.33 | 1.83 Mb |
| php (7.3)| lumen (6.2) | **46.32** ms | 15.31 ms | 90.70 ms | 99470.67 | 51968.67 | 17.12 Mb |
| php (7.3)| slim (4.3) | **46.70** ms | 15.20 ms | 85.32 ms | 103351.67 | 52091.67 | 17.15 Mb |
| php (7.3)| symfony (4.3) | **48.34** ms | 16.05 ms | 96.66 ms | 106020.33 | 51411.00 | 16.94 Mb |
| python (3.7)| bocadillo (0.18) | **49.66** ms | 45.67 ms | 79.00 ms | 23281.00 | 20089.33 | 2.57 Mb |
| php (7.3)| zend-expressive (3.2) | **49.85** ms | 16.11 ms | 100.22 ms | 109528.00 | 49895.00 | 16.43 Mb |
| python (3.7)| sanic (19.9) | **51.07** ms | 39.25 ms | 102.40 ms | 31357.33 | 20401.67 | 2.41 Mb |
| node (12.11)| turbo_polka (2.0) | **55.59** ms | 41.81 ms | 49.02 ms | 103253.00 | 22320.00 | 1.39 Mb |
| java (8)| spring-boot (2.1) | **56.74** ms | 14.10 ms | 31.30 ms | 224746.67 | 56435.00 | 2.95 Mb |
| php (7.3)| spiral (2.1) | **57.11** ms | 57.15 ms | 63.25 ms | 6870.33 | 16930.67 | 1.95 Mb |
| php (7.3)| zend-framework (3.1) | **58.04** ms | 18.02 ms | 115.24 ms | 130207.00 | 44974.67 | 14.81 Mb |
| node (12.11)| hapi (18.4) | **63.58** ms | 24.06 ms | 46.01 ms | 192817.00 | 35354.00 | 6.08 Mb |
| scala (2.12)| http4s (0.18) | **66.46** ms | 16.98 ms | 40.66 ms | 252826.00 | 49491.33 | 5.76 Mb |
| php (7.3)| laravel (6.3) | **71.69** ms | 19.36 ms | 142.24 ms | 170052.67 | 42301.33 | 14.00 Mb |
| node (12.11)| moleculer (0.13) | **77.74** ms | 27.20 ms | 58.54 ms | 229193.00 | 30334.00 | 3.46 Mb |
| clojure (1.10)| coast (1.0) | **78.28** ms | 18.52 ms | 21.42 ms | 287772.67 | 48851.00 | 5.83 Mb |
| python (3.7)| quart (0.10) | **82.28** ms | 78.48 ms | 116.69 ms | 30168.33 | 11853.33 | 1.57 Mb |
| crystal (0.31)| athena (0.7) | **84.44** ms | 50.57 ms | 224.99 ms | 110040.33 | 22486.67 | 1.87 Mb |
| go (1.13)| gramework (1.6) | **95.31** ms | 96.66 ms | 100.70 ms | 15521.00 | 10236.67 | 1.74 Mb |
| python (3.7)| tornado (5.1) | **97.91** ms | 99.17 ms | 117.12 ms | 32939.67 | 9710.33 | 1.90 Mb |
| python (3.7)| django (2.2) | **102.97** ms | 89.68 ms | 179.81 ms | 41869.33 | 9450.00 | 1.82 Mb |
| java (8)| javalin (3.5) | **116.53** ms | 10.18 ms | 127.66 ms | 468637.00 | 69363.33 | 8.21 Mb |
| python (3.7)| masonite (2.2) | **131.79** ms | 116.04 ms | 218.40 ms | 49370.33 | 7337.33 | 1.20 Mb |
| crystal (0.31)| onyx (0.5) | **198.30** ms | 199.34 ms | 231.23 ms | 29878.33 | 4950.00 | 0.85 Mb |
| scala (2.12)| akkahttp (10.1) | **234.23** ms | 6.84 ms | 119.37 ms | 922027.00 | 70500.33 | 10.11 Mb |
| perl (5.3)| dancer2 (2.0) | **340.64** ms | 98.31 ms | 617.12 ms | 802407.67 | 1168.00 | 0.18 Mb |
| python (3.7)| cyclone (1.3) | **419.42** ms | 339.81 ms | 437.23 ms | 587256.33 | 2260.00 | 0.38 Mb |
| python (3.7)| nameko (2.12) | **606.39** ms | 527.01 ms | 563.63 ms | 705671.67 | 1304.67 | 0.18 Mb |

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

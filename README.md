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
| rust (1.38)| nickel (0.11) | **0.26** ms | 0.23 ms | 0.43 ms | 222.67 | 37288.33 | 4.93 Mb |
| ruby (2.6)| syro (3.1) | **2.63** ms | 0.52 ms | 8.09 ms | 4478.00 | 47908.33 | 1.84 Mb |
| ruby (2.6)| roda (3.25) | **2.72** ms | 0.58 ms | 8.34 ms | 4656.67 | 46653.67 | 2.95 Mb |
| ruby (2.6)| cuba (3.9) | **2.98** ms | 0.53 ms | 9.33 ms | 4966.67 | 42811.67 | 3.35 Mb |
| rust (1.38)| iron (0.6) | **3.17** ms | 3.03 ms | 4.64 ms | 1442.67 | 20642.67 | 1.70 Mb |
| ruby (2.6)| rack-routing (0.0) | **3.85** ms | 0.66 ms | 12.01 ms | 6159.33 | 33227.33 | 1.27 Mb |
| c (11)| agoo-c (0.7) | **4.62** ms | 4.28 ms | 9.13 ms | 3404.67 | 210275.00 | 8.06 Mb |
| javascript (12.11)| sifrr (0.0) | **4.76** ms | 4.29 ms | 9.77 ms | 3979.33 | 203548.00 | 11.87 Mb |
| python (3.7)| japronto (0.1) | **5.01** ms | 4.61 ms | 9.81 ms | 3744.33 | 192618.00 | 15.28 Mb |
| nim (1.0)| httpbeast (0.2) | **5.07** ms | 4.47 ms | 9.81 ms | 3947.33 | 193501.00 | 18.26 Mb |
| ruby (2.6)| camping (2.1) | **5.35** ms | 1.26 ms | 15.78 ms | 8459.33 | 23942.33 | 1.52 Mb |
| go (1.13)| fasthttp (1.5) | **5.61** ms | 4.84 ms | 8.88 ms | 4564.67 | 169194.33 | 18.10 Mb |
| ruby (2.6)| flame (4.18) | **5.72** ms | 1.75 ms | 17.46 ms | 9181.67 | 22440.00 | 0.86 Mb |
| go (1.13)| fasthttprouter (0.1) | **5.86** ms | 5.10 ms | 9.16 ms | 4732.00 | 161656.67 | 17.26 Mb |
| swift (5.1)| swifter (1.4) | **5.93** ms | 0.80 ms | 14.48 ms | 93960.33 | 11940.00 | 1.01 Mb |
| cpp (11)| evhtp (1.2) | **6.13** ms | 5.38 ms | 10.07 ms | 3406.67 | 154234.33 | 9.92 Mb |
| java (8)| rapidoid (5.5) | **6.13** ms | 4.95 ms | 10.98 ms | 6551.67 | 166667.33 | 19.88 Mb |
| go (1.13)| atreugo (8.2) | **6.15** ms | 5.15 ms | 9.28 ms | 7323.33 | 160399.67 | 21.39 Mb |
| ruby (2.6)| hanami (1.3) | **6.17** ms | 2.02 ms | 19.15 ms | 9648.67 | 20712.00 | 10.41 Mb |
| crystal (0.31)| toro (0.4) | **6.49** ms | 5.65 ms | 10.83 ms | 3407.00 | 148954.00 | 9.28 Mb |
| go (1.13)| gorouter-fasthttp (4.2) | **6.50** ms | 5.50 ms | 10.02 ms | 6559.33 | 150479.00 | 15.99 Mb |
| c (11)| kore (3.3) | **6.55** ms | 5.96 ms | 12.16 ms | 4544.00 | 165231.33 | 29.78 Mb |
| crystal (0.31)| router.cr (0.2) | **6.60** ms | 5.75 ms | 10.96 ms | 3452.00 | 146371.00 | 9.12 Mb |
| crystal (0.31)| raze (0.3) | **6.80** ms | 5.94 ms | 11.04 ms | 3406.00 | 142283.33 | 8.87 Mb |
| crystal (0.31)| spider-gazelle (2.0) | **6.83** ms | 6.02 ms | 11.24 ms | 3492.67 | 141444.00 | 9.98 Mb |
| crystal (0.31)| kemal (0.28) | **7.18** ms | 6.27 ms | 12.04 ms | 3708.33 | 134760.67 | 14.60 Mb |
| ruby (2.6)| sinatra (2.0) | **7.38** ms | 2.31 ms | 21.56 ms | 10595.67 | 17273.67 | 2.98 Mb |
| nim (1.0)| jester (0.4) | **7.45** ms | 6.72 ms | 11.96 ms | 4289.67 | 143953.33 | 19.18 Mb |
| crystal (0.31)| amber (0.3) | **7.64** ms | 6.88 ms | 12.23 ms | 3701.33 | 127121.00 | 15.42 Mb |
| crystal (0.31)| orion (1.7) | **8.47** ms | 7.83 ms | 13.65 ms | 4122.00 | 115770.67 | 12.54 Mb |
| ruby (2.6)| grape (1.2) | **9.70** ms | 0.84 ms | 31.33 ms | 15266.67 | 13300.33 | 0.51 Mb |
| go (1.13)| gorouter (4.2) | **9.78** ms | 7.97 ms | 16.31 ms | 9452.00 | 105566.33 | 9.31 Mb |
| rust (1.38)| actix-web (1.0) | **10.24** ms | 9.83 ms | 13.72 ms | 2960.00 | 105794.67 | 10.16 Mb |
| java (8)| act (1.8) | **10.33** ms | 7.77 ms | 13.40 ms | 21370.33 | 121834.00 | 13.94 Mb |
| go (1.13)| chi (4.0) | **10.55** ms | 8.08 ms | 17.76 ms | 14481.33 | 102120.67 | 9.06 Mb |
| go (1.13)| kami (2.2) | **10.61** ms | 8.66 ms | 17.42 ms | 10024.00 | 99073.67 | 8.73 Mb |
| go (1.13)| echo (4.1) | **10.90** ms | 8.50 ms | 19.58 ms | 10073.00 | 97108.00 | 11.30 Mb |
| go (1.13)| rte (0.0) | **10.90** ms | 8.36 ms | 18.25 ms | 14418.00 | 99775.67 | 8.78 Mb |
| go (1.13)| gin (1.4) | **10.95** ms | 8.63 ms | 19.30 ms | 10163.67 | 96114.67 | 11.18 Mb |
| go (1.13)| webgo (3.0) | **11.69** ms | 9.24 ms | 18.71 ms | 14356.00 | 91532.00 | 8.08 Mb |
| go (1.13)| violetear (7.0) | **12.07** ms | 9.36 ms | 18.66 ms | 17394.67 | 91181.00 | 7.98 Mb |
| go (1.13)| beego (1.12) | **12.45** ms | 9.33 ms | 22.76 ms | 12934.67 | 87253.33 | 7.80 Mb |
| python (3.7)| falcon (2.0) | **12.63** ms | 10.30 ms | 21.05 ms | 7832.00 | 80593.33 | 12.52 Mb |
| swift (5.1)| perfect (3.1) | **12.71** ms | 12.86 ms | 15.17 ms | 3125.00 | 75752.00 | 4.72 Mb |
| csharp (7.3)| aspnetcore (2.2) | **12.75** ms | 10.34 ms | 18.51 ms | 16640.00 | 81130.00 | 8.76 Mb |
| go (1.13)| air (0.13) | **12.82** ms | 9.53 ms | 23.60 ms | 14377.00 | 84344.00 | 11.67 Mb |
| php (7.3)| one (1.8) | **13.84** ms | 12.44 ms | 24.09 ms | 8347.00 | 73148.67 | 11.14 Mb |
| go (1.13)| goroute (0.0) | **14.15** ms | 8.49 ms | 19.81 ms | 37937.00 | 97563.67 | 11.35 Mb |
| go (1.13)| gorilla-mux (1.7) | **14.24** ms | 8.58 ms | 20.82 ms | 38513.00 | 94343.00 | 8.35 Mb |
| javascript (12.11)| polkadot (1.0) | **14.56** ms | 9.13 ms | 17.73 ms | 38793.00 | 93332.00 | 9.27 Mb |
| ruby (2.6)| agoo (2.11) | **14.67** ms | 14.56 ms | 16.27 ms | 3311.67 | 67534.33 | 2.59 Mb |
| javascript (12.11)| 0http (1.2) | **15.67** ms | 9.38 ms | 17.73 ms | 45399.00 | 91542.67 | 9.09 Mb |
| go (1.13)| gf (1.9) | **15.82** ms | 11.85 ms | 29.68 ms | 14720.67 | 69036.67 | 7.75 Mb |
| javascript (12.11)| rayo (1.3) | **16.08** ms | 10.33 ms | 20.01 ms | 37930.67 | 80164.00 | 7.96 Mb |
| javascript (12.11)| restana (3.3) | **16.48** ms | 9.59 ms | 18.95 ms | 46882.00 | 88276.33 | 8.77 Mb |
| python (3.7)| bottle (0.12) | **16.70** ms | 14.00 ms | 27.72 ms | 9641.67 | 60781.33 | 9.93 Mb |
| ruby (2.6)| plezi (0.16) | **16.97** ms | 16.00 ms | 21.01 ms | 8015.33 | 57860.00 | 8.18 Mb |
| php (7.3)| hyperf (1.0) | **17.18** ms | 14.59 ms | 32.77 ms | 12207.33 | 61325.33 | 8.67 Mb |
| rust (1.38)| gotham (0.4) | **17.62** ms | 17.51 ms | 26.31 ms | 14211.00 | 57530.33 | 7.74 Mb |
| javascript (12.11)| polka (0.5) | **18.01** ms | 10.17 ms | 19.91 ms | 50295.67 | 82198.67 | 8.16 Mb |
| python (3.7)| asgineer (0.7) | **18.44** ms | 16.25 ms | 31.77 ms | 9951.00 | 54640.67 | 6.47 Mb |
| php (7.3)| sw-fw-less (preview) | **18.46** ms | 17.29 ms | 29.11 ms | 8817.00 | 53609.33 | 8.17 Mb |
| cpp (11)| drogon (1.0) | **19.47** ms | 15.87 ms | 21.53 ms | 37302.00 | 60340.67 | 3.88 Mb |
| go (1.13)| mars (1.0) | **19.74** ms | 12.53 ms | 44.95 ms | 26811.00 | 62786.67 | 9.38 Mb |
| javascript (12.11)| muneem (2.4) | **19.80** ms | 11.58 ms | 22.11 ms | 54744.00 | 72513.00 | 7.20 Mb |
| python (3.7)| blacksheep (0.2) | **20.17** ms | 18.19 ms | 32.48 ms | 10188.33 | 50129.00 | 6.68 Mb |
| python (3.7)| hug (2.6) | **20.52** ms | 16.88 ms | 34.36 ms | 11031.67 | 48972.33 | 8.05 Mb |
| python (3.7)| starlette (0.12) | **22.25** ms | 20.09 ms | 36.41 ms | 11216.33 | 45223.33 | 6.46 Mb |
| javascript (12.11)| foxify (0.1) | **22.85** ms | 12.63 ms | 23.80 ms | 64935.33 | 67876.67 | 9.46 Mb |
| php (7.3)| swoft (2.0) | **23.99** ms | 22.80 ms | 32.08 ms | 7222.00 | 40823.67 | 7.12 Mb |
| swift (5.1)| kitura (2.8) | **24.64** ms | 20.19 ms | 23.13 ms | 54868.00 | 47573.00 | 5.87 Mb |
| swift (5.1)| kitura-nio (2.8) | **24.90** ms | 20.55 ms | 23.35 ms | 52745.67 | 46942.00 | 5.79 Mb |
| javascript (12.11)| restify (8.4) | **25.61** ms | 19.04 ms | 30.15 ms | 42103.33 | 45917.67 | 5.34 Mb |
| php (7.3)| imi (1.0) | **27.16** ms | 25.59 ms | 35.77 ms | 8215.33 | 36137.00 | 5.50 Mb |
| javascript (12.11)| fastify (2.8) | **27.47** ms | 14.98 ms | 27.57 ms | 79376.33 | 60310.00 | 10.61 Mb |
| swift (5.1)| vapor (3.3) | **28.14** ms | 17.70 ms | 31.68 ms | 82533.00 | 48794.33 | 5.53 Mb |
| javascript (12.11)| express (4.17) | **28.20** ms | 15.54 ms | 29.39 ms | 78233.00 | 53953.00 | 8.76 Mb |
| javascript (12.11)| iotjs-express (0.0) | **29.07** ms | 14.19 ms | 27.21 ms | 89119.00 | 59707.00 | 16.08 Mb |
| javascript (12.11)| koa (2.8) | **30.48** ms | 13.95 ms | 26.47 ms | 94364.67 | 61022.33 | 8.56 Mb |
| java (8)| javalin (3.5) | **32.27** ms | 11.41 ms | 52.74 ms | 94747.00 | 67836.33 | 8.03 Mb |
| ruby (2.6)| rails (6.0) | **33.06** ms | 2.47 ms | 110.13 ms | 62137.00 | 3866.67 | 1.62 Mb |
| kotlin (1.3)| ktor (1.2) | **34.87** ms | 11.97 ms | 29.78 ms | 123245.67 | 72458.00 | 7.49 Mb |
| python (3.7)| responder (2.0) | **36.18** ms | 33.50 ms | 58.97 ms | 16665.00 | 27434.67 | 3.97 Mb |
| python (3.7)| fastapi (0.42) | **36.57** ms | 32.94 ms | 60.68 ms | 18651.67 | 27623.67 | 3.95 Mb |
| fsharp (7.3)| suave (2.5) | **36.77** ms | 29.84 ms | 80.62 ms | 32372.00 | 26805.00 | 3.60 Mb |
| python (3.7)| clastic (19.9) | **38.79** ms | 33.18 ms | 60.87 ms | 16610.67 | 25403.33 | 4.17 Mb |
| java (8)| micronaut (1.2) | **39.01** ms | 20.48 ms | 87.97 ms | 59954.67 | 26473.00 | 3.63 Mb |
| python (3.7)| molten (0.27) | **39.81** ms | 34.95 ms | 57.91 ms | 16158.67 | 25302.00 | 3.12 Mb |
| java (8)| spring-boot (2.1) | **41.32** ms | 16.29 ms | 40.33 ms | 138275.33 | 46067.00 | 2.40 Mb |
| python (3.7)| flask (1.1) | **41.67** ms | 34.88 ms | 69.18 ms | 19150.67 | 23677.33 | 3.87 Mb |
| python (3.7)| aiohttp (3.6) | **43.20** ms | 40.26 ms | 67.96 ms | 17410.67 | 23064.67 | 3.47 Mb |
| crystal (0.31)| lucky (0.18) | **44.06** ms | 41.17 ms | 53.45 ms | 24154.00 | 22482.00 | 1.84 Mb |
| javascript (12.11)| turbo_polka (2.0) | **48.78** ms | 41.35 ms | 48.39 ms | 68103.00 | 22699.67 | 1.42 Mb |
| python (3.7)| sanic (19.9) | **49.81** ms | 48.05 ms | 78.25 ms | 23229.67 | 19661.33 | 2.33 Mb |
| python (3.7)| bocadillo (0.18) | **52.23** ms | 44.47 ms | 85.79 ms | 27181.67 | 19452.33 | 2.49 Mb |
| php (7.3)| basicphp (0.9) | **52.74** ms | 17.95 ms | 104.58 ms | 109613.33 | 44494.33 | 14.69 Mb |
| php (7.3)| slim (4.3) | **54.11** ms | 18.29 ms | 119.34 ms | 113154.33 | 43905.00 | 14.46 Mb |
| php (7.3)| zend-expressive (3.2) | **55.59** ms | 18.57 ms | 116.08 ms | 118794.33 | 43515.67 | 14.33 Mb |
| scala (2.12)| http4s (0.18) | **55.63** ms | 17.83 ms | 41.13 ms | 214380.33 | 46942.67 | 5.46 Mb |
| php (7.3)| lumen (6.2) | **56.15** ms | 18.07 ms | 108.96 ms | 122583.33 | 43421.00 | 14.31 Mb |
| php (7.3)| symfony (4.3) | **57.10** ms | 18.39 ms | 114.14 ms | 123939.67 | 42927.00 | 14.14 Mb |
| php (7.3)| spiral (2.3) | **57.22** ms | 57.39 ms | 64.00 ms | 9107.33 | 16948.00 | 1.95 Mb |
| javascript (12.11)| hapi (18.4) | **59.26** ms | 24.27 ms | 45.96 ms | 175066.67 | 35422.00 | 6.09 Mb |
| php (7.3)| zend-framework (3.1) | **61.06** ms | 19.25 ms | 121.66 ms | 134005.67 | 41652.00 | 13.72 Mb |
| crystal (0.31)| athena (0.7) | **66.54** ms | 48.65 ms | 178.44 ms | 82933.00 | 24202.33 | 2.02 Mb |
| javascript (12.11)| moleculer (0.13) | **71.66** ms | 26.68 ms | 56.19 ms | 207966.00 | 30854.67 | 3.52 Mb |
| clojure (1.10)| coast (1.0) | **81.50** ms | 19.18 ms | 22.04 ms | 300148.33 | 48564.33 | 5.79 Mb |
| php (7.3)| laravel (6.4) | **81.60** ms | 21.55 ms | 152.51 ms | 191975.00 | 36208.33 | 11.98 Mb |
| python (3.7)| cherrypy (18.3) | **86.47** ms | 72.60 ms | 76.39 ms | 219694.33 | 1402.33 | 0.22 Mb |
| python (3.7)| quart (0.10) | **87.28** ms | 85.76 ms | 130.01 ms | 34905.67 | 11227.00 | 1.49 Mb |
| go (1.13)| gramework (1.6) | **96.14** ms | 97.88 ms | 101.95 ms | 18702.00 | 10112.00 | 1.72 Mb |
| python (3.7)| tornado (5.1) | **100.68** ms | 98.84 ms | 126.68 ms | 29726.33 | 9539.33 | 1.87 Mb |
| python (3.7)| django (2.2) | **105.73** ms | 91.93 ms | 163.07 ms | 39833.67 | 9177.00 | 1.77 Mb |
| python (3.7)| masonite (2.2) | **136.65** ms | 120.27 ms | 220.36 ms | 49283.33 | 7084.33 | 1.16 Mb |
| crystal (0.31)| onyx (0.5) | **194.66** ms | 195.34 ms | 223.30 ms | 27352.00 | 5040.33 | 0.86 Mb |
| scala (2.12)| akkahttp (10.1) | **219.55** ms | 7.03 ms | 62.08 ms | 877928.33 | 69915.00 | 10.02 Mb |
| perl (5.3)| dancer2 (2.0) | **225.68** ms | 58.22 ms | 343.50 ms | 578518.33 | 1189.67 | 0.18 Mb |
| python (3.7)| cyclone (1.3) | **362.07** ms | 299.31 ms | 378.18 ms | 502837.67 | 2243.33 | 0.38 Mb |
| julia (1.3)| merly (0.2) | **457.17** ms | 129.98 ms | 1006.83 ms | 1131069.33 | 2776.67 | 0.22 Mb |
| python (3.7)| klein (19.6) | **617.89** ms | 536.29 ms | 605.48 ms | 730811.67 | 1292.00 | 0.18 Mb |
| python (3.7)| nameko (2.12) | **658.62** ms | 604.86 ms | 663.61 ms | 648792.00 | 1320.33 | 0.19 Mb |

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

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
| rust (1.38)| nickel (0.11) | **0.10** ms | 0.09 ms | 0.15 ms | 79.67 | 129976.67 | 17.17 Mb |
| rust (1.38)| iron (0.6) | **2.00** ms | 1.93 ms | 3.06 ms | 991.00 | 47949.67 | 3.95 Mb |
| javascript (12.13)| sifrr (0.0) | **3.34** ms | 1.92 ms | 7.95 ms | 3778.67 | 354445.33 | 20.67 Mb |
| javascript (12.13)| nanoexpress (1.1) | **3.59** ms | 2.09 ms | 8.46 ms | 4038.33 | 340587.00 | 19.85 Mb |
| javascript (12.13)| nanoexpress-pro (1.6) | **3.80** ms | 1.66 ms | 9.93 ms | 4792.67 | 367277.00 | 21.42 Mb |
| java (8)| rapidoid (5.5) | **3.90** ms | 2.27 ms | 9.29 ms | 4706.67 | 314318.67 | 37.50 Mb |
| nim (1.0)| httpbeast (0.2) | **4.03** ms | 2.93 ms | 9.45 ms | 3995.67 | 271670.67 | 25.63 Mb |
| c (11)| agoo-c (0.7) | **4.05** ms | 3.42 ms | 8.37 ms | 3712.67 | 247700.00 | 9.49 Mb |
| go (1.13)| fasthttprouter (0.1) | **4.40** ms | 3.79 ms | 6.87 ms | 4474.67 | 215806.33 | 23.02 Mb |
| c (11)| kore (3.3) | **4.78** ms | 4.28 ms | 8.92 ms | 3488.67 | 205588.67 | 37.06 Mb |
| go (1.13)| fasthttp (1.5) | **4.86** ms | 3.61 ms | 6.80 ms | 12081.00 | 222647.33 | 23.80 Mb |
| crystal (0.31)| kemal (0.28) | **5.03** ms | 4.13 ms | 9.69 ms | 3617.33 | 199635.33 | 21.63 Mb |
| crystal (0.31)| toro (0.4) | **5.03** ms | 4.10 ms | 10.06 ms | 3928.67 | 200352.33 | 12.49 Mb |
| go (1.13)| gorouter-fasthttp (4.2) | **5.06** ms | 3.88 ms | 7.03 ms | 12166.00 | 210110.00 | 22.36 Mb |
| crystal (0.31)| router.cr (0.2) | **5.11** ms | 4.17 ms | 10.10 ms | 3877.00 | 195768.67 | 12.20 Mb |
| nim (1.0)| jester (0.4) | **5.24** ms | 4.28 ms | 10.30 ms | 3760.33 | 212243.67 | 28.27 Mb |
| go (1.13)| atreugo (9.0) | **5.30** ms | 3.80 ms | 6.94 ms | 14121.67 | 214377.00 | 28.58 Mb |
| crystal (0.31)| raze (0.3) | **5.30** ms | 4.48 ms | 10.03 ms | 3745.33 | 184597.33 | 11.50 Mb |
| crystal (0.31)| spider-gazelle (2.1) | **5.38** ms | 4.44 ms | 10.44 ms | 3907.33 | 184295.67 | 12.99 Mb |
| java (8)| act (1.8) | **5.41** ms | 3.68 ms | 10.76 ms | 5489.33 | 238630.00 | 27.31 Mb |
| crystal (0.31)| orion (1.7) | **5.56** ms | 4.78 ms | 9.77 ms | 3378.33 | 175494.67 | 19.01 Mb |
| ruby (2.6)| agoo (2.11) | **5.89** ms | 3.90 ms | 9.42 ms | 10632.67 | 229001.00 | 8.78 Mb |
| crystal (0.31)| amber (0.3) | **5.95** ms | 4.91 ms | 10.97 ms | 3892.33 | 164857.67 | 20.00 Mb |
| javascript (12.13)| polkadot (1.0) | **6.41** ms | 5.10 ms | 9.65 ms | 8457.33 | 159460.33 | 15.84 Mb |
| javascript (12.13)| restana (3.3) | **6.63** ms | 5.29 ms | 9.82 ms | 6718.33 | 150761.00 | 14.97 Mb |
| php (7.3)| workerman (3.5) | **6.72** ms | 5.53 ms | 11.74 ms | 4542.00 | 148883.00 | 18.96 Mb |
| cpp (11)| drogon (1.0) | **6.73** ms | 6.07 ms | 10.26 ms | 4652.00 | 155247.67 | 9.98 Mb |
| swift (5.1)| swifter (1.4) | **6.82** ms | 0.81 ms | 14.53 ms | 76059.67 | 21861.67 | 1.86 Mb |
| swift (5.1)| perfect (3.1) | **6.95** ms | 6.81 ms | 9.54 ms | 3126.67 | 138809.00 | 8.65 Mb |
| rust (1.38)| actix-web (1.0) | **7.06** ms | 6.58 ms | 9.95 ms | 2862.00 | 167203.00 | 16.40 Mb |
| javascript (12.13)| rayo (1.3) | **7.19** ms | 5.56 ms | 10.44 ms | 8383.00 | 141564.00 | 14.06 Mb |
| javascript (12.13)| 0http (1.2) | **7.66** ms | 6.00 ms | 11.72 ms | 9611.67 | 132807.33 | 13.19 Mb |
| python (3.8)| falcon (2.0) | **8.12** ms | 5.66 ms | 15.47 ms | 7304.67 | 128822.33 | 20.01 Mb |
| javascript (12.13)| polka (0.5) | **8.20** ms | 5.48 ms | 10.39 ms | 17051.33 | 144447.33 | 14.34 Mb |
| javascript (12.13)| muneem (2.4) | **8.98** ms | 6.92 ms | 12.46 ms | 18240.00 | 127091.33 | 12.62 Mb |
| javascript (12.13)| foxify (0.1) | **9.04** ms | 7.77 ms | 13.12 ms | 13051.00 | 117533.67 | 16.37 Mb |
| csharp (7.3)| aspnetcore (3.0) | **9.40** ms | 4.41 ms | 7.60 ms | 41343.33 | 197645.00 | 21.34 Mb |
| cpp (11)| evhtp (1.2) | **9.48** ms | 9.29 ms | 12.84 ms | 3909.00 | 104942.00 | 6.75 Mb |
| python (3.8)| bottle (0.12) | **9.64** ms | 8.18 ms | 17.90 ms | 8023.67 | 108224.33 | 17.67 Mb |
| javascript (12.13)| iotjs-express (0.0) | **9.87** ms | 8.16 ms | 14.65 ms | 11025.00 | 105065.33 | 28.30 Mb |
| rust (1.38)| gotham (0.4) | **9.89** ms | 7.19 ms | 11.76 ms | 29310.33 | 131455.00 | 17.68 Mb |
| go (1.13)| gorouter (4.2) | **9.94** ms | 6.00 ms | 23.68 ms | 12488.33 | 126284.00 | 11.15 Mb |
| go (1.13)| kami (2.2) | **10.02** ms | 6.47 ms | 22.25 ms | 12278.00 | 121953.33 | 10.76 Mb |
| ruby (2.6)| plezi (0.16) | **10.18** ms | 9.65 ms | 13.52 ms | 4484.00 | 95710.33 | 13.52 Mb |
| go (1.13)| rte (0.0) | **10.25** ms | 5.89 ms | 23.44 ms | 17670.67 | 129173.33 | 11.45 Mb |
| javascript (12.13)| koa (2.11) | **10.36** ms | 8.71 ms | 14.93 ms | 13882.33 | 103122.67 | 14.47 Mb |
| python (3.8)| asgineer (0.7) | **10.38** ms | 9.99 ms | 15.18 ms | 3604.67 | 93845.33 | 11.10 Mb |
| python (3.8)| blacksheep (0.2) | **11.19** ms | 11.08 ms | 16.80 ms | 4416.00 | 87428.00 | 11.66 Mb |
| javascript (12.13)| fastify (2.1) | **11.48** ms | 8.82 ms | 15.89 ms | 20061.67 | 104535.00 | 17.93 Mb |
| go (1.13)| chi (4.0) | **11.78** ms | 5.90 ms | 28.06 ms | 23771.67 | 122973.33 | 10.90 Mb |
| javascript (12.13)| express (4.17) | **11.82** ms | 8.99 ms | 16.74 ms | 17710.33 | 91955.00 | 14.93 Mb |
| go (1.13)| goroute (0.0) | **11.99** ms | 5.70 ms | 32.02 ms | 18756.33 | 119716.33 | 13.93 Mb |
| go (1.13)| gin (1.5) | **12.06** ms | 5.94 ms | 30.80 ms | 20591.33 | 119360.33 | 13.89 Mb |
| php (7.3)| one (1.8) | **12.11** ms | 11.10 ms | 21.00 ms | 6797.67 | 81773.33 | 12.46 Mb |
| go (1.13)| beego (1.12) | **12.15** ms | 6.47 ms | 32.81 ms | 13821.67 | 112612.00 | 10.02 Mb |
| swift (5.1)| vapor (3.3) | **12.21** ms | 8.92 ms | 16.08 ms | 32287.00 | 97385.00 | 10.80 Mb |
| go (1.13)| echo (4.1) | **12.24** ms | 5.73 ms | 31.87 ms | 21403.67 | 120364.00 | 14.00 Mb |
| python (3.8)| hug (2.6) | **12.47** ms | 10.96 ms | 20.10 ms | 9581.00 | 81132.67 | 13.33 Mb |
| go (1.13)| gorilla-mux (1.7) | **12.83** ms | 5.70 ms | 35.09 ms | 19565.33 | 116516.33 | 10.31 Mb |
| go (1.13)| air (0.13) | **12.98** ms | 6.14 ms | 36.68 ms | 15123.33 | 111351.67 | 15.40 Mb |
| php (7.3)| hyperf (1.0) | **13.19** ms | 10.87 ms | 25.63 ms | 9578.67 | 79052.67 | 11.17 Mb |
| go (1.13)| aero (1.3) | **13.28** ms | 7.65 ms | 30.95 ms | 23240.00 | 103582.33 | 9.19 Mb |
| go (1.13)| violetear (7.0) | **13.58** ms | 7.61 ms | 34.29 ms | 18365.33 | 101043.67 | 8.91 Mb |
| python (3.8)| starlette (0.13) | **13.72** ms | 12.95 ms | 20.19 ms | 5729.33 | 71826.67 | 10.26 Mb |
| php (7.3)| sw-fw-less (preview) | **14.58** ms | 13.62 ms | 24.86 ms | 7419.67 | 67158.33 | 10.23 Mb |
| go (1.13)| webgo (3.0) | **14.96** ms | 7.34 ms | 40.52 ms | 21170.67 | 98561.00 | 8.71 Mb |
| fsharp (7.3)| suave (2.5) | **16.01** ms | 14.73 ms | 21.17 ms | 15471.00 | 62874.33 | 8.44 Mb |
| swift (5.1)| kitura (2.8) | **16.08** ms | 14.70 ms | 15.37 ms | 26657.33 | 67091.67 | 8.27 Mb |
| javascript (12.13)| restify (8.5) | **17.08** ms | 12.23 ms | 23.06 ms | 31594.00 | 68422.00 | 7.96 Mb |
| php (7.3)| imi (1.0) | **17.30** ms | 16.78 ms | 22.11 ms | 4338.00 | 56057.67 | 8.54 Mb |
| php (7.3)| swoft (2.0) | **18.21** ms | 17.43 ms | 23.81 ms | 4908.33 | 53572.67 | 9.34 Mb |
| java (8)| spring-boot (2.1) | **18.96** ms | 7.61 ms | 12.60 ms | 88262.00 | 113593.33 | 5.84 Mb |
| go (1.13)| gf (1.1) | **18.99** ms | 8.59 ms | 52.06 ms | 27577.00 | 80949.00 | 9.12 Mb |
| go (1.13)| mars (1.0) | **19.25** ms | 7.25 ms | 54.47 ms | 24642.33 | 84056.00 | 12.55 Mb |
| python (3.8)| fastapi (0.44) | **19.41** ms | 18.26 ms | 31.35 ms | 8683.00 | 51120.67 | 7.31 Mb |
| swift (5.1)| kitura-nio (2.8) | **19.61** ms | 14.27 ms | 14.95 ms | 60739.33 | 68974.00 | 8.51 Mb |
| python (3.8)| responder (2.0) | **20.34** ms | 18.70 ms | 34.05 ms | 10595.00 | 49674.33 | 7.19 Mb |
| clojure (1.10)| coast (1.0) | **20.70** ms | 13.01 ms | 13.95 ms | 66237.33 | 75747.00 | 9.04 Mb |
| ruby (2.6)| roda (3.26) | **20.71** ms | 4.03 ms | 44.81 ms | 170601.33 | 59826.33 | 3.79 Mb |
| javascript (12.13)| moleculer (0.13) | **21.00** ms | 15.24 ms | 31.69 ms | 26192.00 | 52560.00 | 6.00 Mb |
| javascript (12.13)| hapi (18.4) | **22.96** ms | 17.48 ms | 30.58 ms | 36715.00 | 52538.33 | 9.05 Mb |
| kotlin (1.3)| ktor (1.2) | **22.96** ms | 7.63 ms | 22.95 ms | 90770.00 | 111627.67 | 11.54 Mb |
| python (3.8)| molten (0.27) | **23.21** ms | 19.93 ms | 36.31 ms | 11764.33 | 44170.00 | 5.44 Mb |
| ruby (2.6)| cuba (3.9) | **23.28** ms | 4.28 ms | 47.98 ms | 181733.67 | 55867.67 | 4.38 Mb |
| python (3.8)| aiohttp (3.6) | **23.32** ms | 22.38 ms | 33.45 ms | 10186.67 | 42984.67 | 6.46 Mb |
| python (3.8)| flask (1.1) | **24.96** ms | 19.75 ms | 40.38 ms | 14252.33 | 40737.33 | 6.65 Mb |
| php (7.3)| ubiquity (2.3) | **26.90** ms | 8.11 ms | 60.32 ms | 59221.67 | 94385.00 | 28.26 Mb |
| python (3.8)| clastic (19.9) | **27.34** ms | 21.03 ms | 48.12 ms | 16787.00 | 37518.67 | 6.16 Mb |
| ruby (2.6)| syro (3.1) | **27.37** ms | 5.07 ms | 68.06 ms | 111040.67 | 53194.33 | 2.04 Mb |
| php (7.3)| one-fpm (1.8) | **28.09** ms | 9.17 ms | 50.47 ms | 61578.33 | 86112.67 | 25.78 Mb |
| crystal (0.31)| lucky (0.18) | **28.54** ms | 23.30 ms | 28.22 ms | 46299.00 | 39991.33 | 3.26 Mb |
| ruby (2.6)| rack-routing (0.0) | **28.73** ms | 6.36 ms | 80.25 ms | 132292.00 | 39130.00 | 1.50 Mb |
| ruby (2.6)| camping (2.1) | **29.11** ms | 6.41 ms | 77.98 ms | 139966.67 | 35394.00 | 2.24 Mb |
| javascript (12.13)| turbo_polka (0.3) | **29.51** ms | 27.37 ms | 29.67 ms | 34975.00 | 35473.67 | 2.21 Mb |
| python (3.8)| sanic (19.9) | **30.21** ms | 27.66 ms | 50.72 ms | 15412.33 | 33362.67 | 3.95 Mb |
| python (3.8)| bocadillo (0.18) | **30.24** ms | 27.38 ms | 49.26 ms | 14726.33 | 33330.00 | 4.27 Mb |
| java (8)| micronaut (1.2) | **31.44** ms | 9.58 ms | 45.87 ms | 107985.33 | 74554.33 | 10.01 Mb |
| ruby (2.6)| flame (4.18) | **32.90** ms | 8.55 ms | 101.33 ms | 103352.33 | 27660.67 | 1.06 Mb |
| php (7.3)| zend-framework (3.1) | **34.81** ms | 9.81 ms | 74.62 ms | 78974.00 | 79976.33 | 23.95 Mb |
| php (7.3)| slim (4.3) | **36.60** ms | 9.88 ms | 84.05 ms | 86096.00 | 78483.67 | 23.50 Mb |
| php (7.3)| zend-expressive (3.2) | **37.68** ms | 9.37 ms | 88.85 ms | 90868.00 | 82761.33 | 24.78 Mb |
| crystal (0.31)| athena (0.7) | **37.79** ms | 32.09 ms | 94.98 ms | 45377.00 | 38064.33 | 3.17 Mb |
| php (7.3)| lumen (6.2) | **38.04** ms | 9.27 ms | 103.75 ms | 88355.00 | 82305.00 | 24.66 Mb |
| php (7.3)| basicphp (0.9) | **38.41** ms | 9.84 ms | 87.01 ms | 93807.00 | 78246.33 | 23.50 Mb |
| php (7.3)| spiral (2.3) | **39.40** ms | 39.35 ms | 46.65 ms | 10297.33 | 24695.67 | 2.85 Mb |
| php (7.3)| symfony (4.3) | **44.46** ms | 9.35 ms | 128.35 ms | 104539.67 | 82096.33 | 24.60 Mb |
| go (1.13)| gramework (1.7) | **46.01** ms | 47.43 ms | 49.86 ms | 10056.33 | 20939.67 | 3.56 Mb |
| java (8)| javalin (3.5) | **50.25** ms | 4.99 ms | 40.49 ms | 260762.00 | 145386.00 | 17.20 Mb |
| scala (2.12)| http4s (0.18) | **51.63** ms | 7.29 ms | 23.00 ms | 231557.00 | 94221.33 | 10.96 Mb |
| ruby (2.6)| hanami (1.3) | **51.81** ms | 11.83 ms | 159.85 ms | 140870.00 | 21031.00 | 10.56 Mb |
| php (7.3)| laravel (6.6) | **52.65** ms | 10.95 ms | 68.84 ms | 141367.00 | 71279.00 | 21.43 Mb |
| ruby (2.6)| grape (1.2) | **55.40** ms | 11.83 ms | 145.77 ms | 232906.67 | 19470.33 | 0.74 Mb |
| python (3.8)| quart (0.10) | **56.24** ms | 51.72 ms | 96.07 ms | 28954.33 | 17983.33 | 2.38 Mb |
| ruby (2.6)| sinatra (2.0) | **58.27** ms | 13.76 ms | 181.25 ms | 141539.33 | 18514.00 | 3.19 Mb |
| python (3.8)| tornado (6.0) | **63.67** ms | 62.93 ms | 87.42 ms | 20942.33 | 15293.67 | 3.00 Mb |
| python (3.8)| django (3.0) | **68.70** ms | 50.76 ms | 136.49 ms | 46057.33 | 15475.00 | 3.39 Mb |
| java (8)| spring-framework (5.2) | **72.41** ms | 10.39 ms | 152.36 ms | 242295.33 | 84113.33 | 10.23 Mb |
| python (3.8)| cherrypy (18.5) | **78.17** ms | 63.27 ms | 141.55 ms | 41678.67 | 13048.67 | 2.31 Mb |
| python (3.8)| masonite (2.2) | **103.36** ms | 76.34 ms | 202.69 ms | 68049.00 | 10205.67 | 1.67 Mb |
| crystal (0.31)| onyx (0.5) | **128.35** ms | 129.72 ms | 148.64 ms | 17757.00 | 7671.00 | 1.31 Mb |
| ruby (2.6)| rails (6.0) | **152.38** ms | 37.73 ms | 509.25 ms | 247689.67 | 4091.00 | 1.71 Mb |
| scala (2.12)| akkahttp (10.1) | **224.45** ms | 5.06 ms | 91.59 ms | 902224.33 | 135967.33 | 19.49 Mb |
| python (3.8)| cyclone (1.3) | **256.00** ms | 227.33 ms | 270.80 ms | 324114.67 | 3418.67 | 0.58 Mb |
| perl (5.3)| dancer2 (2.0) | **332.79** ms | 39.35 ms | 923.66 ms | 901939.67 | 2051.00 | 0.31 Mb |
| julia (1.3)| merly (0.2) | **338.92** ms | 120.09 ms | 492.39 ms | 871653.67 | 6210.33 | 0.49 Mb |
| python (3.8)| klein (19.6) | **397.20** ms | 317.51 ms | 344.09 ms | 654239.67 | 2258.00 | 0.33 Mb |
| python (3.8)| nameko (2.12) | **447.38** ms | 359.76 ms | 390.38 ms | 677808.67 | 2129.67 | 0.30 Mb |

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

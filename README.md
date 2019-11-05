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
| rust (1.38)| nickel (0.11) | **0.27** ms | 0.23 ms | 0.44 ms | 226.67 | 36908.67 | 4.87 Mb |
| ruby (2.6)| syro (3.1) | **2.65** ms | 0.54 ms | 8.02 ms | 4302.00 | 47663.67 | 1.83 Mb |
| ruby (2.6)| roda (3.25) | **2.78** ms | 0.65 ms | 8.07 ms | 3998.33 | 45522.00 | 2.88 Mb |
| ruby (2.6)| cuba (3.9) | **3.07** ms | 0.60 ms | 9.32 ms | 4912.00 | 41650.00 | 3.26 Mb |
| rust (1.38)| iron (0.6) | **3.19** ms | 3.05 ms | 4.66 ms | 1412.33 | 20548.67 | 1.69 Mb |
| ruby (2.6)| rack-routing (0.0) | **3.83** ms | 0.84 ms | 11.51 ms | 5804.00 | 33300.00 | 1.28 Mb |
| c (11)| agoo-c (0.7) | **4.70** ms | 4.29 ms | 9.44 ms | 3660.00 | 206384.33 | 7.91 Mb |
| ruby (2.6)| camping (2.1) | **4.75** ms | 0.69 ms | 15.78 ms | 8360.33 | 27031.67 | 1.71 Mb |
| python (3.7)| japronto (0.1) | **5.01** ms | 4.57 ms | 9.79 ms | 3724.67 | 192343.67 | 15.26 Mb |
| nim (1.0)| httpbeast (0.2) | **5.16** ms | 4.58 ms | 9.97 ms | 3953.00 | 189643.33 | 17.90 Mb |
| ruby (2.6)| flame (4.18) | **5.57** ms | 0.72 ms | 17.42 ms | 9116.67 | 22957.00 | 0.88 Mb |
| go (1.13)| fasthttp (1.5) | **5.67** ms | 4.90 ms | 8.96 ms | 4502.00 | 168189.67 | 18.00 Mb |
| cpp (11)| evhtp (1.2) | **5.86** ms | 5.27 ms | 9.42 ms | 2731.67 | 160410.33 | 10.32 Mb |
| go (1.13)| fasthttprouter (0.1) | **6.01** ms | 5.16 ms | 9.17 ms | 5865.00 | 162047.00 | 17.30 Mb |
| go (1.13)| atreugo (8.2) | **6.08** ms | 5.14 ms | 9.17 ms | 7324.67 | 161141.33 | 21.49 Mb |
| go (1.13)| gorouter-fasthttp (4.2) | **6.15** ms | 5.32 ms | 9.25 ms | 5847.33 | 157617.67 | 16.78 Mb |
| ruby (2.6)| hanami (1.3) | **6.28** ms | 0.56 ms | 21.91 ms | 11447.33 | 20418.00 | 10.26 Mb |
| crystal (0.31)| router.cr (0.2) | **6.50** ms | 5.70 ms | 10.55 ms | 3187.67 | 148264.00 | 9.24 Mb |
| crystal (0.31)| toro (0.4) | **6.54** ms | 5.72 ms | 10.58 ms | 3209.33 | 147411.00 | 9.19 Mb |
| java (8)| rapidoid (5.5) | **6.59** ms | 5.45 ms | 12.11 ms | 7150.67 | 155095.33 | 18.50 Mb |
| crystal (0.31)| spider-gazelle (2.0) | **6.69** ms | 5.91 ms | 10.77 ms | 3232.33 | 144360.33 | 10.19 Mb |
| crystal (0.31)| raze (0.3) | **6.76** ms | 5.97 ms | 10.86 ms | 3259.67 | 142889.67 | 8.90 Mb |
| c (11)| kore (3.3) | **6.88** ms | 6.01 ms | 12.76 ms | 5664.33 | 159235.33 | 28.69 Mb |
| crystal (0.31)| kemal (0.28) | **7.06** ms | 6.32 ms | 11.02 ms | 3212.67 | 136588.33 | 14.80 Mb |
| javascript (12.11)| sifrr (0.0) | **7.29** ms | 6.14 ms | 15.21 ms | 6279.00 | 138116.33 | 8.06 Mb |
| nim (1.0)| jester (0.4) | **7.39** ms | 6.69 ms | 11.89 ms | 4091.33 | 144448.33 | 19.24 Mb |
| crystal (0.31)| amber (0.3) | **7.63** ms | 7.06 ms | 11.89 ms | 3475.00 | 127020.33 | 15.41 Mb |
| ruby (2.6)| sinatra (2.0) | **7.81** ms | 0.66 ms | 26.34 ms | 13463.33 | 16386.33 | 2.82 Mb |
| crystal (0.31)| orion (1.7) | **8.65** ms | 8.15 ms | 13.59 ms | 4008.67 | 113037.33 | 12.24 Mb |
| java (8)| act (1.8) | **9.30** ms | 7.74 ms | 13.31 ms | 11992.00 | 122485.33 | 14.02 Mb |
| ruby (2.6)| grape (1.2) | **9.37** ms | 0.81 ms | 30.90 ms | 15171.33 | 13757.33 | 0.52 Mb |
| go (1.13)| rte (0.0) | **9.71** ms | 7.64 ms | 15.52 ms | 13366.33 | 108852.33 | 9.67 Mb |
| go (1.13)| gorouter (4.2) | **9.83** ms | 8.03 ms | 16.42 ms | 9273.33 | 105240.00 | 9.28 Mb |
| go (1.13)| chi (4.0) | **10.17** ms | 8.19 ms | 17.71 ms | 8550.67 | 101304.67 | 8.98 Mb |
| rust (1.38)| actix-web (1.0) | **10.24** ms | 9.76 ms | 13.63 ms | 2768.00 | 105907.00 | 10.15 Mb |
| go (1.13)| violetear (7.0) | **10.65** ms | 8.98 ms | 16.19 ms | 10324.00 | 97143.00 | 8.55 Mb |
| go (1.13)| goroute (0.0) | **10.72** ms | 8.42 ms | 18.67 ms | 10911.33 | 97987.33 | 11.40 Mb |
| swift (5.1)| swifter (1.4) | **10.80** ms | 3.98 ms | 21.86 ms | 91564.67 | 6968.67 | 0.59 Mb |
| go (1.13)| echo (4.1) | **10.83** ms | 8.55 ms | 18.85 ms | 10660.67 | 97394.00 | 11.33 Mb |
| go (1.13)| beego (1.12) | **11.02** ms | 8.66 ms | 18.90 ms | 11661.33 | 96024.00 | 8.56 Mb |
| go (1.13)| kami (2.2) | **11.29** ms | 8.56 ms | 17.60 ms | 18265.00 | 98597.33 | 8.68 Mb |
| go (1.13)| webgo (3.0) | **11.67** ms | 9.27 ms | 18.98 ms | 13272.67 | 91828.67 | 8.11 Mb |
| go (1.13)| gin (1.4) | **12.12** ms | 8.60 ms | 19.10 ms | 22670.67 | 96620.00 | 11.24 Mb |
| go (1.13)| gorilla-mux (1.7) | **12.13** ms | 8.79 ms | 20.44 ms | 18718.67 | 93560.00 | 8.28 Mb |
| python (3.7)| falcon (2.0) | **12.43** ms | 10.30 ms | 20.00 ms | 7735.00 | 81051.00 | 12.59 Mb |
| go (1.13)| air (0.13) | **12.89** ms | 9.54 ms | 23.53 ms | 15854.33 | 85193.00 | 11.78 Mb |
| ruby (2.6)| agoo (2.11) | **13.57** ms | 12.68 ms | 19.86 ms | 5718.67 | 72109.33 | 2.76 Mb |
| javascript (12.11)| 0http (1.2) | **13.70** ms | 9.42 ms | 19.04 ms | 30032.00 | 88709.67 | 8.81 Mb |
| php (7.3)| one (1.8) | **13.73** ms | 12.44 ms | 23.59 ms | 8042.00 | 73071.00 | 11.13 Mb |
| go (1.13)| gf (1.9) | **14.73** ms | 10.81 ms | 23.02 ms | 25846.33 | 77948.33 | 8.77 Mb |
| javascript (12.11)| polkadot (1.0) | **15.84** ms | 9.59 ms | 20.08 ms | 42925.33 | 87849.67 | 8.72 Mb |
| csharp (7.3)| aspnetcore (2.2) | **15.92** ms | 9.93 ms | 16.68 ms | 42463.00 | 86560.33 | 9.35 Mb |
| python (3.7)| bottle (0.12) | **16.30** ms | 14.06 ms | 24.83 ms | 8618.33 | 61312.67 | 10.01 Mb |
| ruby (2.6)| plezi (0.16) | **17.77** ms | 16.62 ms | 22.40 ms | 9890.33 | 55707.00 | 7.87 Mb |
| rust (1.38)| gotham (0.4) | **17.86** ms | 17.29 ms | 23.95 ms | 18728.67 | 58936.00 | 7.93 Mb |
| php (7.3)| hyperf (1.0) | **17.97** ms | 14.52 ms | 36.07 ms | 13878.00 | 60210.33 | 8.51 Mb |
| cpp (11)| drogon (1.0) | **18.26** ms | 15.47 ms | 20.83 ms | 30668.00 | 61898.67 | 3.98 Mb |
| php (7.3)| sw-fw-less (preview) | **18.37** ms | 17.23 ms | 28.52 ms | 8695.67 | 53371.00 | 8.13 Mb |
| go (1.13)| mars (1.0) | **18.42** ms | 12.58 ms | 41.96 ms | 17932.33 | 63027.67 | 9.41 Mb |
| python (3.7)| asgineer (0.7) | **19.70** ms | 18.48 ms | 30.44 ms | 8253.33 | 50178.00 | 5.94 Mb |
| javascript (12.11)| foxify (0.1) | **20.30** ms | 12.56 ms | 24.33 ms | 48659.33 | 66347.67 | 9.24 Mb |
| swift (5.1)| perfect (3.1) | **20.31** ms | 19.08 ms | 28.49 ms | 6097.33 | 51940.00 | 3.24 Mb |
| python (3.7)| hug (2.6) | **20.48** ms | 17.57 ms | 31.69 ms | 9992.67 | 48754.33 | 8.01 Mb |
| python (3.7)| blacksheep (0.2) | **21.46** ms | 19.73 ms | 34.35 ms | 10327.00 | 46548.00 | 6.21 Mb |
| python (3.7)| starlette (0.12) | **22.07** ms | 20.57 ms | 33.21 ms | 9007.67 | 44994.67 | 6.43 Mb |
| javascript (12.11)| rayo (1.3) | **22.62** ms | 10.99 ms | 22.32 ms | 72156.67 | 76270.67 | 7.57 Mb |
| javascript (12.11)| restana (3.3) | **23.25** ms | 11.32 ms | 26.19 ms | 69906.00 | 72863.00 | 7.24 Mb |
| php (7.3)| swoft (2.0) | **23.42** ms | 22.58 ms | 30.41 ms | 6379.67 | 41880.33 | 7.30 Mb |
| javascript (12.11)| polka (0.5) | **25.29** ms | 12.36 ms | 26.82 ms | 77831.67 | 67714.00 | 6.72 Mb |
| javascript (12.11)| fastify (2.8) | **25.33** ms | 16.00 ms | 31.31 ms | 56717.67 | 54601.67 | 9.62 Mb |
| kotlin (1.3)| ktor (1.2) | **26.22** ms | 12.76 ms | 31.03 ms | 81205.33 | 69460.67 | 7.18 Mb |
| php (7.3)| imi (1.0) | **26.39** ms | 25.42 ms | 33.54 ms | 6530.33 | 37082.00 | 5.65 Mb |
| javascript (12.11)| muneem (2.4) | **26.54** ms | 11.83 ms | 23.78 ms | 87076.00 | 70312.67 | 6.98 Mb |
| fsharp (7.3)| suave (2.5) | **27.94** ms | 20.07 ms | 44.13 ms | 31761.33 | 28487.00 | 3.83 Mb |
| javascript (12.11)| iotjs-express (0.0) | **29.14** ms | 16.11 ms | 33.42 ms | 75754.00 | 52282.00 | 14.08 Mb |
| javascript (12.11)| express (4.17) | **29.54** ms | 16.68 ms | 33.69 ms | 71598.67 | 49167.67 | 7.98 Mb |
| java (8)| spring-boot (2.1) | **30.59** ms | 15.77 ms | 36.94 ms | 83659.33 | 48895.33 | 2.55 Mb |
| ruby (2.6)| rails (6.0) | **33.05** ms | 2.40 ms | 109.34 ms | 62770.00 | 3874.33 | 1.62 Mb |
| javascript (12.11)| koa (2.11) | **34.33** ms | 15.62 ms | 33.29 ms | 102720.00 | 52232.00 | 7.33 Mb |
| python (3.7)| responder (2.0) | **36.48** ms | 31.57 ms | 63.57 ms | 20048.67 | 27865.00 | 4.03 Mb |
| python (3.7)| fastapi (0.42) | **37.13** ms | 34.14 ms | 58.08 ms | 16582.67 | 26880.33 | 3.85 Mb |
| swift (5.1)| kitura-nio (2.8) | **37.76** ms | 34.20 ms | 52.78 ms | 49350.33 | 30808.33 | 3.80 Mb |
| python (3.7)| clastic (19.9) | **38.56** ms | 33.78 ms | 57.41 ms | 16052.00 | 25498.33 | 4.19 Mb |
| python (3.7)| molten (0.27) | **39.80** ms | 34.41 ms | 62.12 ms | 17394.67 | 25374.33 | 3.13 Mb |
| swift (5.1)| kitura (2.8) | **41.03** ms | 35.43 ms | 60.80 ms | 34559.33 | 24086.67 | 2.97 Mb |
| python (3.7)| flask (1.1) | **41.65** ms | 37.40 ms | 55.98 ms | 22687.00 | 23792.00 | 3.89 Mb |
| python (3.7)| aiohttp (3.6) | **43.90** ms | 41.23 ms | 74.61 ms | 20896.33 | 22864.33 | 3.44 Mb |
| crystal (0.31)| lucky (0.18) | **45.53** ms | 44.02 ms | 53.34 ms | 27654.00 | 21964.00 | 1.79 Mb |
| javascript (12.11)| restify (8.4) | **51.51** ms | 29.12 ms | 61.13 ms | 114858.33 | 30255.00 | 3.52 Mb |
| php (7.3)| basicphp (0.9) | **53.01** ms | 17.93 ms | 108.15 ms | 110249.00 | 44682.33 | 14.75 Mb |
| python (3.7)| bocadillo (0.18) | **53.05** ms | 47.08 ms | 95.42 ms | 30522.33 | 19385.33 | 2.48 Mb |
| python (3.7)| sanic (19.9) | **53.50** ms | 46.27 ms | 98.23 ms | 30798.00 | 19255.67 | 2.28 Mb |
| php (7.3)| slim (4.3) | **54.13** ms | 18.36 ms | 115.14 ms | 113259.67 | 44071.33 | 14.51 Mb |
| php (7.3)| lumen (6.2) | **54.22** ms | 18.35 ms | 116.97 ms | 113648.33 | 43917.67 | 14.47 Mb |
| clojure (1.10)| coast (1.0) | **55.11** ms | 19.24 ms | 22.04 ms | 212267.33 | 46425.00 | 5.54 Mb |
| php (7.3)| zend-expressive (3.2) | **55.38** ms | 18.55 ms | 115.46 ms | 120499.67 | 43571.33 | 14.35 Mb |
| scala (2.12)| http4s (0.18) | **56.66** ms | 17.87 ms | 42.53 ms | 225166.67 | 45154.67 | 5.25 Mb |
| php (7.3)| spiral (2.3) | **56.82** ms | 57.28 ms | 63.87 ms | 8296.00 | 16943.67 | 1.95 Mb |
| php (7.3)| symfony (4.3) | **58.84** ms | 18.10 ms | 111.46 ms | 135337.00 | 43240.00 | 14.25 Mb |
| javascript (12.11)| turbo_polka (2.0) | **59.17** ms | 43.08 ms | 51.63 ms | 110422.67 | 21553.33 | 1.34 Mb |
| php (7.3)| zend-framework (3.1) | **60.80** ms | 18.92 ms | 123.52 ms | 133576.33 | 42105.00 | 13.87 Mb |
| swift (5.1)| vapor (3.3) | **63.41** ms | 25.13 ms | 59.71 ms | 211311.33 | 31386.33 | 3.49 Mb |
| java (8)| micronaut (1.2) | **64.35** ms | 24.33 ms | 98.90 ms | 186090.33 | 22066.33 | 3.09 Mb |
| javascript (12.11)| hapi (18.4) | **70.41** ms | 25.16 ms | 50.69 ms | 217225.67 | 33081.33 | 5.69 Mb |
| crystal (0.31)| athena (0.7) | **71.64** ms | 50.05 ms | 195.20 ms | 90464.67 | 23597.33 | 1.96 Mb |
| javascript (12.11)| moleculer (0.13) | **75.57** ms | 28.60 ms | 61.07 ms | 206514.00 | 28697.33 | 3.28 Mb |
| php (7.3)| laravel (6.4) | **81.26** ms | 22.23 ms | 149.23 ms | 193577.67 | 36388.33 | 12.04 Mb |
| java (8)| javalin (3.5) | **82.22** ms | 12.05 ms | 108.30 ms | 265977.33 | 64244.67 | 7.60 Mb |
| python (3.7)| cherrypy (18.3) | **86.37** ms | 72.49 ms | 76.98 ms | 212017.67 | 1402.67 | 0.22 Mb |
| go (1.13)| gramework (1.6) | **94.46** ms | 97.14 ms | 101.38 ms | 18074.67 | 10167.00 | 1.73 Mb |
| python (3.7)| quart (0.10) | **97.34** ms | 84.39 ms | 163.46 ms | 46698.67 | 10118.00 | 1.34 Mb |
| python (3.7)| tornado (5.1) | **101.84** ms | 99.12 ms | 123.90 ms | 30587.67 | 9398.00 | 1.84 Mb |
| python (3.7)| django (2.2) | **105.73** ms | 93.37 ms | 162.35 ms | 38551.33 | 9191.67 | 1.77 Mb |
| python (3.7)| masonite (2.2) | **135.92** ms | 121.45 ms | 211.70 ms | 53373.33 | 7086.33 | 1.16 Mb |
| crystal (0.31)| onyx (0.5) | **194.12** ms | 193.41 ms | 229.33 ms | 29723.67 | 5059.67 | 0.87 Mb |
| perl (5.3)| dancer2 (2.0) | **218.37** ms | 76.34 ms | 410.69 ms | 490077.67 | 1640.67 | 0.25 Mb |
| scala (2.12)| akkahttp (10.1) | **238.83** ms | 7.74 ms | 169.13 ms | 913264.00 | 66068.67 | 9.47 Mb |
| python (3.7)| cyclone (1.3) | **350.63** ms | 293.28 ms | 410.38 ms | 461340.00 | 2241.67 | 0.38 Mb |
| julia (1.3)| merly (0.2) | **416.71** ms | 130.56 ms | 901.15 ms | 1012070.33 | 3306.00 | 0.26 Mb |
| python (3.7)| klein (19.6) | **518.78** ms | 434.66 ms | 534.62 ms | 555188.33 | 1448.67 | 0.21 Mb |
| python (3.7)| nameko (2.12) | **577.99** ms | 505.82 ms | 559.04 ms | 655869.67 | 1331.67 | 0.19 Mb |

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

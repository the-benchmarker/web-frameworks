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

| Language | Framework | Average | 50th percentile | 75th percentile | 90th percentile | Standard deviation | Requests / s | Throughput |
|----|----|---------|-------------|---------|----------|----------|--------|----|
| rust | nickel | **0.26** ms | 0.21 ms | 0.31 ms | 0.43 ms | 217.00 | 37468.00 | 4.95 Mb |
| ruby | syro | **2.61** ms | 0.52 ms | 3.37 ms | 7.95 ms | 4307.00 | 48644.33 | 1.86 Mb |
| ruby | roda | **2.68** ms | 0.54 ms | 3.19 ms | 8.28 ms | 4608.33 | 47552.33 | 3.01 Mb |
| rust | iron | **3.14** ms | 3.01 ms | 3.77 ms | 4.56 ms | 1384.00 | 20864.00 | 1.72 Mb |
| ruby | cuba | **3.20** ms | 0.82 ms | 4.14 ms | 9.49 ms | 5121.67 | 39746.33 | 3.11 Mb |
| ruby | rack-routing | **3.81** ms | 0.53 ms | 4.92 ms | 12.28 ms | 6489.67 | 33756.33 | 1.29 Mb |
| c | agoo-c | **4.36** ms | 3.92 ms | 5.81 ms | 8.55 ms | 3085.67 | 220381.67 | 8.45 Mb |
| ruby | camping | **4.94** ms | 0.56 ms | 6.63 ms | 16.37 ms | 8487.00 | 26115.33 | 1.65 Mb |
| swift | swifter | **4.98** ms | 0.74 ms | 0.86 ms | 14.42 ms | 79160.00 | 11151.33 | 0.95 Mb |
| node | sifrr | **5.00** ms | 4.33 ms | 7.16 ms | 10.24 ms | 4361.33 | 198939.33 | 11.61 Mb |
| python | japronto | **5.07** ms | 4.53 ms | 7.30 ms | 9.93 ms | 3889.33 | 193314.00 | 15.33 Mb |
| nim | httpbeast | **5.44** ms | 4.82 ms | 7.58 ms | 10.51 ms | 4346.33 | 179604.00 | 16.95 Mb |
| cpp | drogon | **5.55** ms | 4.79 ms | 7.56 ms | 10.21 ms | 4810.67 | 178608.33 | 11.49 Mb |
| ruby | flame | **5.64** ms | 0.60 ms | 7.32 ms | 18.35 ms | 9884.67 | 22711.00 | 0.87 Mb |
| cpp | evhtp | **5.88** ms | 5.24 ms | 7.30 ms | 9.51 ms | 2889.33 | 161408.33 | 10.38 Mb |
| go | atreugo | **6.20** ms | 5.32 ms | 7.32 ms | 10.05 ms | 3828.00 | 153634.00 | 20.48 Mb |
| crystal | toro | **6.41** ms | 5.62 ms | 8.27 ms | 10.53 ms | 3200.67 | 150525.33 | 9.38 Mb |
| ruby | hanami | **6.46** ms | 0.58 ms | 8.65 ms | 22.29 ms | 11596.67 | 19944.33 | 10.02 Mb |
| crystal | router.cr | **6.47** ms | 5.73 ms | 8.23 ms | 10.36 ms | 3107.33 | 148690.00 | 9.27 Mb |
| java | rapidoid | **6.59** ms | 5.17 ms | 8.35 ms | 11.59 ms | 9240.33 | 162038.33 | 19.33 Mb |
| go | gorouter-fasthttp | **6.66** ms | 5.57 ms | 7.83 ms | 10.72 ms | 5953.00 | 145720.67 | 15.50 Mb |
| crystal | raze | **6.71** ms | 5.85 ms | 8.48 ms | 10.89 ms | 3351.67 | 144409.33 | 9.00 Mb |
| crystal | kemal | **7.04** ms | 6.26 ms | 8.87 ms | 11.20 ms | 3357.00 | 137223.67 | 14.86 Mb |
| c | kore | **7.06** ms | 5.71 ms | 8.69 ms | 13.49 ms | 7984.00 | 170159.33 | 30.66 Mb |
| ruby | sinatra | **7.17** ms | 0.66 ms | 9.83 ms | 24.27 ms | 12565.00 | 17865.33 | 3.08 Mb |
| go | fasthttprouter | **7.44** ms | 5.39 ms | 7.47 ms | 10.33 ms | 16194.00 | 150359.33 | 16.00 Mb |
| crystal | amber | **7.64** ms | 6.94 ms | 9.37 ms | 12.16 ms | 3583.67 | 127612.67 | 15.48 Mb |
| nim | jester | **8.01** ms | 7.01 ms | 10.13 ms | 13.57 ms | 5267.00 | 135744.33 | 18.08 Mb |
| crystal | orion | **8.48** ms | 7.97 ms | 10.13 ms | 13.28 ms | 3911.67 | 115305.67 | 12.49 Mb |
| java | act | **8.68** ms | 7.39 ms | 10.09 ms | 12.68 ms | 8439.67 | 128201.33 | 14.67 Mb |
| ruby | grape | **9.35** ms | 0.81 ms | 15.04 ms | 30.65 ms | 15070.33 | 13765.33 | 0.52 Mb |
| rust | actix-web | **10.10** ms | 9.62 ms | 11.53 ms | 13.51 ms | 4088.67 | 107775.33 | 10.34 Mb |
| go | rte | **10.74** ms | 8.57 ms | 11.94 ms | 18.97 ms | 8190.00 | 96696.67 | 8.61 Mb |
| go | kami | **11.02** ms | 8.92 ms | 12.01 ms | 18.50 ms | 9302.00 | 95010.67 | 8.35 Mb |
| go | gorouter | **11.08** ms | 8.30 ms | 11.62 ms | 18.65 ms | 14931.00 | 99827.00 | 8.82 Mb |
| go | chi | **11.15** ms | 8.16 ms | 11.22 ms | 17.78 ms | 19075.00 | 102467.00 | 9.06 Mb |
| go | goroute | **11.18** ms | 8.64 ms | 12.56 ms | 20.14 ms | 9919.00 | 95198.00 | 11.07 Mb |
| go | violetear | **11.42** ms | 9.23 ms | 12.24 ms | 18.19 ms | 10571.67 | 92757.33 | 8.16 Mb |
| go | echo | **11.77** ms | 9.00 ms | 13.42 ms | 21.82 ms | 9557.33 | 91067.67 | 10.59 Mb |
| go | beego | **12.36** ms | 9.21 ms | 13.22 ms | 21.04 ms | 16211.67 | 89522.00 | 8.04 Mb |
| go | gorilla-mux | **12.41** ms | 9.35 ms | 14.34 ms | 23.41 ms | 10067.33 | 86846.33 | 7.64 Mb |
| python | falcon | **12.56** ms | 10.50 ms | 15.31 ms | 20.27 ms | 7488.67 | 80494.67 | 12.50 Mb |
| go | gin | **13.41** ms | 9.30 ms | 14.36 ms | 24.13 ms | 19915.67 | 86143.33 | 10.02 Mb |
| swift | perfect | **13.55** ms | 13.53 ms | 14.77 ms | 16.12 ms | 2403.00 | 72606.00 | 4.53 Mb |
| php | one | **13.81** ms | 12.47 ms | 17.74 ms | 23.80 ms | 8328.00 | 72527.67 | 11.05 Mb |
| go | air | **13.98** ms | 10.24 ms | 15.90 ms | 26.88 ms | 13229.00 | 78783.00 | 10.90 Mb |
| ruby | agoo | **14.19** ms | 13.74 ms | 15.23 ms | 18.84 ms | 5105.33 | 69738.67 | 2.67 Mb |
| go | gf | **15.19** ms | 11.38 ms | 17.04 ms | 28.08 ms | 15429.00 | 71863.00 | 8.09 Mb |
| csharp | aspnetcore | **16.02** ms | 11.27 ms | 14.65 ms | 25.29 ms | 27980.33 | 73411.00 | 7.92 Mb |
| node | rayo | **16.37** ms | 10.75 ms | 15.59 ms | 21.41 ms | 38208.33 | 77346.33 | 7.68 Mb |
| python | bottle | **16.69** ms | 14.06 ms | 20.76 ms | 26.29 ms | 9305.00 | 60558.00 | 9.89 Mb |
| php | hyperf | **16.86** ms | 14.43 ms | 22.58 ms | 32.10 ms | 11801.33 | 62370.33 | 8.81 Mb |
| rust | gotham | **16.87** ms | 17.09 ms | 21.12 ms | 24.58 ms | 8948.33 | 59452.67 | 7.99 Mb |
| node | polkadot | **17.25** ms | 9.73 ms | 14.31 ms | 20.63 ms | 52779.67 | 87260.33 | 8.67 Mb |
| python | asgineer | **17.56** ms | 15.54 ms | 21.42 ms | 28.54 ms | 8990.33 | 57333.33 | 6.78 Mb |
| ruby | plezi | **17.91** ms | 16.66 ms | 19.30 ms | 22.86 ms | 9697.67 | 55634.67 | 7.86 Mb |
| python | blacksheep | **18.57** ms | 16.09 ms | 22.37 ms | 30.24 ms | 10045.67 | 54909.33 | 7.32 Mb |
| node | 0http | **18.88** ms | 9.87 ms | 14.58 ms | 21.56 ms | 58541.00 | 85114.67 | 8.45 Mb |
| node | polka | **20.05** ms | 10.91 ms | 16.15 ms | 22.48 ms | 60470.00 | 76074.33 | 7.55 Mb |
| node | restana | **20.26** ms | 10.67 ms | 15.89 ms | 22.98 ms | 59054.67 | 78957.67 | 7.84 Mb |
| kotlin | ktor | **20.48** ms | 12.37 ms | 18.94 ms | 28.41 ms | 55165.33 | 72114.67 | 7.45 Mb |
| python | hug | **20.92** ms | 17.74 ms | 24.27 ms | 32.07 ms | 10970.67 | 48223.00 | 7.92 Mb |
| node | foxify | **22.49** ms | 12.71 ms | 17.66 ms | 24.65 ms | 61333.00 | 65961.00 | 9.19 Mb |
| swift | kitura | **22.63** ms | 20.18 ms | 21.43 ms | 23.28 ms | 33696.00 | 47401.00 | 5.85 Mb |
| python | starlette | **23.04** ms | 19.15 ms | 28.31 ms | 40.76 ms | 14600.67 | 45186.00 | 6.45 Mb |
| php | swoft | **24.90** ms | 23.09 ms | 27.67 ms | 34.82 ms | 8977.67 | 39519.00 | 6.89 Mb |
| fsharp | suave | **26.34** ms | 20.88 ms | 29.89 ms | 43.66 ms | 21037.67 | 29067.33 | 3.90 Mb |
| swift | kitura-nio | **27.94** ms | 20.00 ms | 21.19 ms | 22.68 ms | 76022.00 | 48040.67 | 5.92 Mb |
| node | restify | **28.02** ms | 19.06 ms | 23.81 ms | 35.02 ms | 51637.33 | 44564.33 | 5.18 Mb |
| node | iotjs-express | **29.65** ms | 15.09 ms | 20.78 ms | 29.87 ms | 84514.67 | 55899.00 | 15.05 Mb |
| php | imi | **30.64** ms | 28.72 ms | 33.02 ms | 40.76 ms | 8550.00 | 32264.00 | 4.91 Mb |
| node | muneem | **32.81** ms | 12.62 ms | 18.57 ms | 27.11 ms | 114031.33 | 66130.00 | 6.57 Mb |
| ruby | rails | **33.72** ms | 2.67 ms | 37.11 ms | 111.15 ms | 63598.00 | 3799.00 | 1.59 Mb |
| swift | vapor | **33.74** ms | 17.41 ms | 21.29 ms | 33.77 ms | 107440.67 | 47910.33 | 5.41 Mb |
| node | koa | **34.69** ms | 14.37 ms | 20.50 ms | 30.38 ms | 108417.67 | 57311.67 | 8.04 Mb |
| python | fastapi | **36.71** ms | 33.27 ms | 45.82 ms | 61.11 ms | 18294.33 | 27541.00 | 3.94 Mb |
| crystal | spider-gazelle | **37.54** ms | 35.99 ms | 41.01 ms | 45.12 ms | 11830.00 | 25945.00 | 1.83 Mb |
| node | fastify | **38.28** ms | 15.66 ms | 21.24 ms | 31.35 ms | 127342.67 | 56438.67 | 9.97 Mb |
| node | express | **38.52** ms | 16.70 ms | 23.73 ms | 34.47 ms | 119003.67 | 48996.33 | 7.95 Mb |
| python | clastic | **38.98** ms | 31.23 ms | 39.68 ms | 72.94 ms | 20341.67 | 25798.67 | 4.24 Mb |
| java | spring-boot | **39.56** ms | 15.91 ms | 25.31 ms | 39.59 ms | 129415.00 | 47084.33 | 2.49 Mb |
| python | flask | **39.61** ms | 33.72 ms | 43.14 ms | 61.47 ms | 16179.00 | 25036.33 | 4.09 Mb |
| python | molten | **39.88** ms | 34.68 ms | 41.53 ms | 60.60 ms | 16842.00 | 25464.00 | 3.14 Mb |
| python | aiohttp | **42.10** ms | 38.32 ms | 52.09 ms | 65.81 ms | 17240.00 | 23721.67 | 3.57 Mb |
| crystal | lucky | **44.29** ms | 40.60 ms | 47.91 ms | 53.42 ms | 31218.33 | 22583.67 | 1.84 Mb |
| php | symfony | **47.56** ms | 15.51 ms | 22.40 ms | 93.36 ms | 106939.33 | 52468.00 | 17.29 Mb |
| php | basicphp | **48.09** ms | 16.44 ms | 24.53 ms | 94.00 ms | 101685.67 | 49310.67 | 16.23 Mb |
| php | zend-expressive | **48.49** ms | 15.27 ms | 23.03 ms | 91.91 ms | 109752.00 | 52154.33 | 17.18 Mb |
| php | slim | **49.66** ms | 16.18 ms | 23.97 ms | 98.56 ms | 110377.67 | 50152.00 | 16.51 Mb |
| php | lumen | **51.23** ms | 16.64 ms | 26.27 ms | 96.86 ms | 116670.33 | 48707.67 | 16.05 Mb |
| python | sanic | **51.33** ms | 44.56 ms | 66.72 ms | 91.21 ms | 28962.67 | 19749.00 | 2.34 Mb |
| python | bocadillo | **51.35** ms | 44.43 ms | 66.18 ms | 87.29 ms | 28777.67 | 19835.67 | 2.54 Mb |
| php | zend-framework | **52.15** ms | 16.07 ms | 24.90 ms | 97.98 ms | 116076.67 | 49871.00 | 16.43 Mb |
| node | turbo_polka | **52.73** ms | 42.07 ms | 45.32 ms | 49.07 ms | 78962.67 | 22209.67 | 1.38 Mb |
| scala | http4s | **55.74** ms | 16.56 ms | 24.00 ms | 36.72 ms | 218929.67 | 50890.00 | 5.92 Mb |
| php | spiral | **57.83** ms | 57.46 ms | 61.34 ms | 64.01 ms | 5964.00 | 16902.00 | 1.95 Mb |
| clojure | coast | **58.74** ms | 18.03 ms | 18.49 ms | 21.16 ms | 215789.33 | 49451.67 | 5.90 Mb |
| node | hapi | **65.85** ms | 24.98 ms | 31.80 ms | 49.71 ms | 193978.00 | 33073.00 | 5.69 Mb |
| crystal | athena | **70.69** ms | 49.41 ms | 112.17 ms | 191.12 ms | 89671.33 | 23884.33 | 1.99 Mb |
| php | laravel | **77.83** ms | 20.13 ms | 34.27 ms | 136.49 ms | 190190.00 | 39966.33 | 13.22 Mb |
| python | quart | **83.65** ms | 79.00 ms | 99.40 ms | 127.93 ms | 32599.67 | 11818.00 | 1.56 Mb |
| node | moleculer | **86.30** ms | 30.68 ms | 43.73 ms | 66.78 ms | 239846.00 | 27394.67 | 3.14 Mb |
| go | gramework | **95.95** ms | 97.25 ms | 100.23 ms | 103.49 ms | 16089.00 | 10123.67 | 1.72 Mb |
| python | tornado | **98.45** ms | 98.48 ms | 110.92 ms | 124.50 ms | 24670.33 | 9856.33 | 1.93 Mb |
| python | responder | **101.85** ms | 88.19 ms | 139.68 ms | 166.91 ms | 45812.33 | 9703.00 | 1.40 Mb |
| python | django | **105.65** ms | 91.98 ms | 112.78 ms | 173.11 ms | 40420.67 | 9208.67 | 1.77 Mb |
| python | masonite | **134.14** ms | 125.91 ms | 136.55 ms | 186.93 ms | 45385.67 | 7255.67 | 1.19 Mb |
| java | javalin | **150.10** ms | 11.20 ms | 28.66 ms | 323.77 ms | 516762.00 | 67530.67 | 7.99 Mb |
| scala | akkahttp | **181.64** ms | 7.59 ms | 11.04 ms | 27.62 ms | 732017.33 | 64137.67 | 9.19 Mb |
| crystal | onyx | **196.11** ms | 197.68 ms | 214.49 ms | 230.28 ms | 32188.67 | 5004.33 | 0.86 Mb |
| perl | dancer2 | **383.26** ms | 69.10 ms | 158.28 ms | 450.39 ms | 1064193.00 | 1357.33 | 0.20 Mb |
| python | cyclone | **385.77** ms | 318.06 ms | 362.70 ms | 412.83 ms | 498250.00 | 2248.33 | 0.38 Mb |

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

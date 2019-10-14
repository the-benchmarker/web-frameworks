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
| rust (1.38)| nickel (0.11) | **0.25** ms | 0.21 ms | 0.41 ms | 210.00 | 37771.00 | 4.99 Mb |
| ruby (2.6)| syro (3.1) | **2.64** ms | 0.54 ms | 7.97 ms | 4257.33 | 47947.33 | 1.84 Mb |
| ruby (2.6)| roda (3.25) | **2.72** ms | 0.61 ms | 8.13 ms | 4312.00 | 46641.33 | 2.95 Mb |
| ruby (2.6)| cuba (3.9) | **3.05** ms | 0.54 ms | 9.52 ms | 5106.33 | 41811.33 | 3.27 Mb |
| rust (1.38)| iron (0.6) | **3.12** ms | 2.99 ms | 4.52 ms | 1378.67 | 20717.00 | 1.71 Mb |
| ruby (2.6)| rack-routing (0.0) | **3.75** ms | 0.56 ms | 11.96 ms | 6301.00 | 33999.00 | 1.30 Mb |
| c (11)| agoo-c (0.7) | **4.46** ms | 4.27 ms | 8.01 ms | 2888.33 | 213745.67 | 8.19 Mb |
| ruby (2.6)| camping (2.1) | **4.78** ms | 0.57 ms | 15.98 ms | 8286.33 | 26842.33 | 1.70 Mb |
| node (12.11)| sifrr (0.0) | **4.81** ms | 4.31 ms | 9.92 ms | 4053.67 | 203445.00 | 11.87 Mb |
| python (3.7)| japronto (0.1) | **4.96** ms | 4.49 ms | 9.81 ms | 3841.67 | 194795.67 | 15.45 Mb |
| nim (1.0)| httpbeast (0.2) | **5.03** ms | 4.43 ms | 9.72 ms | 3911.67 | 195364.33 | 18.43 Mb |
| cpp (11)| drogon (1.0) | **5.46** ms | 4.84 ms | 10.08 ms | 3656.00 | 177853.67 | 11.44 Mb |
| ruby (2.6)| flame (4.18) | **5.48** ms | 0.57 ms | 18.24 ms | 9942.00 | 23378.00 | 0.90 Mb |
| go (1.13)| fasthttp (1.5) | **5.75** ms | 4.85 ms | 8.92 ms | 7164.67 | 168713.67 | 18.05 Mb |
| cpp (11)| evhtp (1.2) | **5.82** ms | 5.24 ms | 9.35 ms | 2610.33 | 161951.33 | 10.42 Mb |
| swift (5.1)| swifter (1.4) | **6.17** ms | 0.84 ms | 14.50 ms | 94075.00 | 12298.33 | 1.05 Mb |
| ruby (2.6)| hanami (1.3) | **6.19** ms | 0.56 ms | 21.64 ms | 11136.33 | 20675.33 | 10.39 Mb |
| crystal (0.31)| toro (0.4) | **6.47** ms | 5.71 ms | 10.50 ms | 3155.33 | 148912.33 | 9.28 Mb |
| crystal (0.31)| router.cr (0.2) | **6.47** ms | 5.73 ms | 10.38 ms | 3104.67 | 148535.33 | 9.26 Mb |
| c (11)| kore (3.3) | **6.56** ms | 6.00 ms | 11.88 ms | 4461.67 | 162995.00 | 29.37 Mb |
| go (1.13)| gorouter-fasthttp (4.2) | **6.58** ms | 5.32 ms | 9.26 ms | 11146.00 | 157576.33 | 16.77 Mb |
| crystal (0.31)| raze (0.3) | **6.66** ms | 5.91 ms | 10.53 ms | 3132.33 | 144841.00 | 9.03 Mb |
| go (1.13)| atreugo (8.2) | **6.75** ms | 5.19 ms | 9.19 ms | 13961.33 | 161003.33 | 21.47 Mb |
| go (1.13)| fasthttprouter (0.1) | **6.84** ms | 5.16 ms | 9.22 ms | 14553.00 | 161528.00 | 17.25 Mb |
| crystal (0.31)| kemal (0.28) | **7.08** ms | 6.32 ms | 11.24 ms | 3353.00 | 136497.00 | 14.79 Mb |
| java (8)| rapidoid (5.5) | **7.16** ms | 4.88 ms | 11.04 ms | 13690.33 | 169077.67 | 20.17 Mb |
| nim (1.0)| jester (0.4) | **7.46** ms | 6.66 ms | 12.31 ms | 4583.67 | 142882.33 | 19.03 Mb |
| crystal (0.31)| amber (0.3) | **7.58** ms | 7.04 ms | 11.62 ms | 3364.00 | 127707.67 | 15.49 Mb |
| ruby (2.6)| sinatra (2.0) | **7.83** ms | 0.67 ms | 26.48 ms | 13598.33 | 16391.33 | 2.82 Mb |
| crystal (0.31)| orion (1.7) | **8.29** ms | 7.91 ms | 12.76 ms | 3619.00 | 117253.00 | 12.70 Mb |
| ruby (2.6)| grape (1.2) | **9.32** ms | 0.80 ms | 31.08 ms | 15254.00 | 13823.33 | 0.53 Mb |
| go (1.13)| rte (0.0) | **9.33** ms | 7.77 ms | 15.50 ms | 7100.00 | 109052.67 | 9.68 Mb |
| java (8)| act (1.8) | **9.68** ms | 7.45 ms | 12.90 ms | 16949.33 | 126414.67 | 14.46 Mb |
| go (1.13)| gorouter (4.2) | **9.73** ms | 7.92 ms | 16.18 ms | 9259.00 | 106651.67 | 9.41 Mb |
| go (1.13)| violetear (7.0) | **10.16** ms | 8.81 ms | 15.61 ms | 7091.00 | 98842.00 | 8.69 Mb |
| go (1.13)| chi (4.0) | **10.20** ms | 8.26 ms | 17.80 ms | 8284.00 | 101873.00 | 9.03 Mb |
| rust (1.38)| actix-web (1.0) | **10.22** ms | 9.75 ms | 13.62 ms | 4037.00 | 106276.00 | 10.21 Mb |
| go (1.13)| kami (2.2) | **10.48** ms | 8.55 ms | 17.34 ms | 9901.67 | 98618.33 | 8.69 Mb |
| go (1.13)| echo (4.1) | **11.11** ms | 8.37 ms | 19.31 ms | 15006.33 | 97876.67 | 11.39 Mb |
| go (1.13)| goroute (0.0) | **11.19** ms | 8.46 ms | 18.63 ms | 16283.00 | 97973.33 | 11.40 Mb |
| go (1.13)| gorilla-mux (1.7) | **11.22** ms | 8.50 ms | 20.51 ms | 11955.33 | 95108.33 | 8.42 Mb |
| go (1.13)| gin (1.4) | **11.42** ms | 8.47 ms | 18.50 ms | 18945.67 | 97044.33 | 11.29 Mb |
| go (1.13)| beego (1.12) | **11.97** ms | 8.41 ms | 18.28 ms | 24825.67 | 99396.33 | 8.86 Mb |
| python (3.7)| falcon (2.0) | **12.12** ms | 10.12 ms | 19.60 ms | 7016.67 | 82635.33 | 12.84 Mb |
| go (1.13)| webgo (3.0) | **12.38** ms | 9.24 ms | 18.60 ms | 20383.33 | 91761.67 | 8.10 Mb |
| swift (5.1)| perfect (3.1) | **12.55** ms | 12.60 ms | 14.94 ms | 2456.67 | 77554.00 | 4.83 Mb |
| go (1.13)| gf (1.9) | **13.19** ms | 10.62 ms | 22.47 ms | 11799.00 | 79884.00 | 9.00 Mb |
| php (7.3)| one (1.8) | **13.32** ms | 12.16 ms | 22.77 ms | 7759.67 | 74363.00 | 11.33 Mb |
| go (1.13)| air (0.13) | **13.50** ms | 9.47 ms | 23.69 ms | 20899.00 | 85102.00 | 11.77 Mb |
| node (12.11)| polkadot (1.0) | **13.51** ms | 9.17 ms | 17.31 ms | 33305.00 | 94266.67 | 9.36 Mb |
| node (12.11)| restana (3.3) | **14.03** ms | 9.65 ms | 19.59 ms | 30089.67 | 86412.00 | 8.58 Mb |
| ruby (2.6)| agoo (2.11) | **14.66** ms | 14.50 ms | 16.28 ms | 2435.33 | 66658.33 | 2.56 Mb |
| kotlin (1.3)| ktor (1.2) | **15.22** ms | 11.80 ms | 27.08 ms | 20668.00 | 74757.00 | 7.73 Mb |
| csharp (7.3)| aspnetcore (2.2) | **15.69** ms | 9.66 ms | 16.47 ms | 42207.33 | 89106.00 | 9.62 Mb |
| node (12.11)| 0http (1.2) | **15.76** ms | 9.57 ms | 19.25 ms | 42610.67 | 88521.33 | 8.79 Mb |
| node (12.11)| rayo (1.3) | **16.26** ms | 10.45 ms | 19.92 ms | 39693.67 | 80157.33 | 7.96 Mb |
| python (3.7)| bottle (0.12) | **16.26** ms | 13.38 ms | 27.38 ms | 9145.67 | 62153.00 | 10.15 Mb |
| php (7.3)| hyperf (1.0) | **16.81** ms | 14.42 ms | 32.40 ms | 12140.67 | 62802.67 | 8.88 Mb |
| ruby (2.6)| plezi (0.16) | **17.35** ms | 16.47 ms | 21.79 ms | 9684.67 | 56742.33 | 8.02 Mb |
| rust (1.38)| gotham (0.4) | **17.66** ms | 17.47 ms | 25.20 ms | 14600.67 | 57964.67 | 7.79 Mb |
| python (3.7)| asgineer (0.7) | **17.91** ms | 16.17 ms | 28.35 ms | 8037.67 | 55508.67 | 6.57 Mb |
| php (7.3)| sw-fw-less (preview) | **18.09** ms | 17.11 ms | 28.11 ms | 8045.00 | 54456.67 | 8.30 Mb |
| python (3.7)| blacksheep (0.2) | **18.99** ms | 17.82 ms | 29.22 ms | 7967.00 | 52113.67 | 6.95 Mb |
| python (3.7)| hug (2.6) | **19.69** ms | 17.16 ms | 30.01 ms | 9556.67 | 50475.00 | 8.29 Mb |
| python (3.7)| starlette (0.12) | **22.27** ms | 19.71 ms | 37.89 ms | 11299.00 | 45323.00 | 6.47 Mb |
| node (12.11)| polka (0.5) | **22.36** ms | 10.94 ms | 22.76 ms | 68279.67 | 75172.33 | 7.46 Mb |
| php (7.3)| swoft (2.0) | **22.91** ms | 22.35 ms | 28.94 ms | 5915.67 | 42291.00 | 7.37 Mb |
| swift (5.1)| kitura-nio (2.8) | **23.56** ms | 20.65 ms | 23.19 ms | 39707.33 | 46590.67 | 5.75 Mb |
| node (12.11)| koa (2.8) | **24.61** ms | 13.85 ms | 26.37 ms | 66124.33 | 60688.67 | 8.52 Mb |
| node (12.11)| restify (8.4) | **24.99** ms | 18.83 ms | 30.23 ms | 36712.00 | 46061.00 | 5.36 Mb |
| swift (5.1)| kitura (2.8) | **25.62** ms | 21.33 ms | 27.20 ms | 47811.00 | 44354.00 | 5.47 Mb |
| php (7.3)| imi (1.0) | **25.89** ms | 25.01 ms | 32.26 ms | 6164.33 | 37680.00 | 5.74 Mb |
| node (12.11)| foxify (0.1) | **27.26** ms | 14.21 ms | 29.16 ms | 75001.33 | 58339.67 | 8.13 Mb |
| node (12.11)| fastify (2.8) | **30.40** ms | 15.25 ms | 28.99 ms | 91497.00 | 58204.00 | 10.32 Mb |
| clojure (1.10)| coast (1.0) | **32.17** ms | 19.33 ms | 21.89 ms | 81615.00 | 49168.00 | 5.86 Mb |
| ruby (2.6)| rails (6.0) | **32.62** ms | 2.35 ms | 108.54 ms | 62155.00 | 3934.00 | 1.65 Mb |
| node (12.11)| muneem (2.4) | **34.07** ms | 12.88 ms | 26.70 ms | 119069.33 | 65520.00 | 6.51 Mb |
| node (12.11)| express (4.17) | **35.04** ms | 16.22 ms | 31.96 ms | 106016.00 | 51230.00 | 8.32 Mb |
| python (3.7)| responder (2.0) | **35.89** ms | 31.25 ms | 63.19 ms | 19984.00 | 28201.00 | 4.08 Mb |
| python (3.7)| fastapi (0.42) | **36.17** ms | 33.25 ms | 59.92 ms | 17936.67 | 27759.67 | 3.97 Mb |
| crystal (0.31)| spider-gazelle (1.6) | **37.22** ms | 35.27 ms | 44.81 ms | 15217.00 | 26408.67 | 1.87 Mb |
| node (12.11)| iotjs-express (0.0) | **37.81** ms | 15.91 ms | 33.62 ms | 119730.00 | 52544.67 | 14.15 Mb |
| python (3.7)| molten (0.27) | **39.87** ms | 35.36 ms | 55.83 ms | 19147.67 | 25430.00 | 3.13 Mb |
| python (3.7)| flask (1.1) | **40.75** ms | 35.49 ms | 61.15 ms | 15772.67 | 24216.00 | 3.96 Mb |
| swift (5.1)| vapor (3.3) | **41.45** ms | 17.36 ms | 32.04 ms | 158716.00 | 50052.67 | 5.68 Mb |
| python (3.7)| aiohttp (3.6) | **41.66** ms | 39.15 ms | 63.51 ms | 15687.33 | 23774.33 | 3.57 Mb |
| python (3.7)| clastic (19.9) | **42.40** ms | 35.65 ms | 64.58 ms | 21821.00 | 23378.00 | 3.84 Mb |
| crystal (0.31)| lucky (0.18) | **43.83** ms | 42.99 ms | 53.38 ms | 17941.67 | 22405.00 | 1.83 Mb |
| fsharp (7.3)| suave (2.5) | **44.89** ms | 39.11 ms | 109.35 ms | 48124.00 | 24494.00 | 3.29 Mb |
| node (12.11)| turbo_polka (2.0) | **49.43** ms | 42.10 ms | 49.43 ms | 58020.67 | 22301.33 | 1.39 Mb |
| python (3.7)| sanic (19.9) | **49.74** ms | 42.72 ms | 87.38 ms | 25132.33 | 20159.67 | 2.39 Mb |
| python (3.7)| bocadillo (0.18) | **51.17** ms | 43.73 ms | 84.88 ms | 26919.67 | 19728.67 | 2.53 Mb |
| php (7.3)| basicphp (0.9) | **51.30** ms | 17.05 ms | 107.58 ms | 107374.00 | 46185.33 | 15.25 Mb |
| php (7.3)| lumen (6.2) | **51.94** ms | 17.60 ms | 110.24 ms | 107039.33 | 45627.00 | 15.03 Mb |
| php (7.3)| slim (4.3) | **52.10** ms | 17.47 ms | 108.22 ms | 110717.67 | 45641.00 | 15.03 Mb |
| java (8)| micronaut (1.2) | **52.41** ms | 23.01 ms | 91.31 ms | 127027.33 | 25789.67 | 3.48 Mb |
| php (7.3)| zend-expressive (3.2) | **53.09** ms | 17.63 ms | 110.37 ms | 112267.33 | 45289.67 | 14.91 Mb |
| php (7.3)| symfony (4.3) | **54.32** ms | 17.88 ms | 105.86 ms | 116022.00 | 44849.33 | 14.78 Mb |
| php (7.3)| spiral (2.3) | **56.43** ms | 56.65 ms | 62.91 ms | 8215.33 | 17187.33 | 1.98 Mb |
| php (7.3)| zend-framework (3.1) | **58.22** ms | 18.54 ms | 119.39 ms | 125606.67 | 43438.33 | 14.31 Mb |
| scala (2.12)| http4s (0.18) | **60.44** ms | 16.34 ms | 35.30 ms | 281292.33 | 47049.67 | 5.47 Mb |
| node (12.11)| moleculer (0.13) | **68.38** ms | 27.37 ms | 56.94 ms | 184399.67 | 30060.67 | 3.43 Mb |
| node (12.11)| hapi (18.4) | **69.92** ms | 23.97 ms | 45.83 ms | 218351.33 | 35601.67 | 6.12 Mb |
| java (8)| spring-boot (2.1) | **70.59** ms | 15.10 ms | 37.73 ms | 284403.33 | 50304.67 | 2.65 Mb |
| crystal (0.31)| athena (0.7) | **74.59** ms | 50.46 ms | 202.40 ms | 94961.33 | 23147.67 | 1.92 Mb |
| php (7.3)| laravel (6.4) | **79.99** ms | 21.84 ms | 167.78 ms | 189241.67 | 37704.00 | 12.48 Mb |
| python (3.7)| quart (0.10) | **86.25** ms | 82.42 ms | 145.80 ms | 39095.33 | 11403.33 | 1.51 Mb |
| python (3.7)| cherrypy (18.3) | **87.86** ms | 72.30 ms | 74.40 ms | 234676.67 | 1413.67 | 0.22 Mb |
| go (1.13)| gramework (1.6) | **95.56** ms | 96.95 ms | 100.85 ms | 17632.33 | 10191.67 | 1.73 Mb |
| python (3.7)| tornado (5.1) | **99.51** ms | 98.52 ms | 116.16 ms | 35245.67 | 9758.00 | 1.91 Mb |
| python (3.7)| django (2.2) | **102.85** ms | 90.64 ms | 165.29 ms | 41207.67 | 9447.33 | 1.82 Mb |
| python (3.7)| masonite (2.2) | **134.64** ms | 127.81 ms | 179.30 ms | 51366.67 | 7202.00 | 1.18 Mb |
| java (8)| javalin (3.5) | **145.07** ms | 12.34 ms | 462.04 ms | 398448.67 | 61092.67 | 7.23 Mb |
| crystal (0.31)| onyx (0.5) | **194.51** ms | 195.02 ms | 229.20 ms | 30570.67 | 5046.00 | 0.86 Mb |
| scala (2.12)| akkahttp (10.1) | **243.07** ms | 7.18 ms | 181.80 ms | 937799.33 | 67849.00 | 9.73 Mb |
| perl (5.3)| dancer2 (2.0) | **372.43** ms | 58.88 ms | 424.60 ms | 1155837.00 | 1258.67 | 0.19 Mb |
| python (3.7)| cyclone (1.3) | **400.09** ms | 305.62 ms | 377.24 ms | 630897.67 | 2275.00 | 0.39 Mb |
| python (3.7)| klein (19.6) | **516.85** ms | 488.09 ms | 558.39 ms | 452764.33 | 1454.00 | 0.21 Mb |
| python (3.7)| nameko (2.12) | **572.78** ms | 498.18 ms | 541.16 ms | 648399.67 | 1344.33 | 0.19 Mb |

## Can I contribute ?

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

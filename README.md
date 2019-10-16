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
| rust (1.38)| nickel (0.11) | **0.22** ms | 0.18 ms | 0.34 ms | 189.33 | 39675.33 | 5.24 Mb |
| ruby (2.6)| syro (3.1) | **2.71** ms | 0.64 ms | 7.95 ms | 4670.00 | 46794.33 | 1.79 Mb |
| ruby (2.6)| roda (3.24) | **2.85** ms | 0.84 ms | 8.00 ms | 5072.33 | 44906.67 | 2.84 Mb |
| rust (1.38)| iron (0.6) | **3.08** ms | 2.94 ms | 4.47 ms | 1404.00 | 21073.33 | 1.73 Mb |
| ruby (2.6)| cuba (3.9) | **3.14** ms | 0.56 ms | 9.73 ms | 5328.67 | 40786.33 | 3.19 Mb |
| ruby (2.6)| rack-routing (0.0) | **3.98** ms | 0.76 ms | 12.11 ms | 6651.33 | 32092.67 | 1.23 Mb |
| swift (5.1)| swifter (1.4) | **4.63** ms | 0.87 ms | 14.65 ms | 66116.00 | 11252.67 | 0.96 Mb |
| c (11)| agoo-c (0.7) | **4.76** ms | 4.36 ms | 9.79 ms | 3809.00 | 208426.67 | 7.99 Mb |
| node (12.11)| sifrr (0.0) | **4.80** ms | 4.29 ms | 9.85 ms | 4000.33 | 204250.67 | 11.92 Mb |
| ruby (2.6)| camping (2.1) | **4.90** ms | 1.06 ms | 14.15 ms | 7071.33 | 26285.00 | 1.66 Mb |
| python (3.7)| japronto (0.1) | **4.98** ms | 4.54 ms | 9.73 ms | 3755.00 | 193173.67 | 15.32 Mb |
| nim (1.0)| httpbeast (0.2) | **5.00** ms | 4.50 ms | 9.59 ms | 3633.33 | 193508.67 | 18.26 Mb |
| cpp (11)| drogon (1.0) | **5.52** ms | 4.92 ms | 9.93 ms | 4769.67 | 178533.33 | 11.48 Mb |
| ruby (2.6)| flame (4.18) | **5.66** ms | 0.92 ms | 17.20 ms | 9165.00 | 22657.33 | 0.87 Mb |
| cpp (11)| evhtp (1.2) | **5.86** ms | 5.18 ms | 9.59 ms | 2851.33 | 162567.67 | 10.46 Mb |
| go (1.13)| fasthttprouter (0.1) | **5.93** ms | 5.14 ms | 9.13 ms | 5801.67 | 161955.33 | 17.27 Mb |
| crystal (0.31)| toro (0.4) | **6.40** ms | 5.56 ms | 10.63 ms | 3317.00 | 151526.67 | 9.44 Mb |
| crystal (0.31)| router.cr (0.2) | **6.43** ms | 5.59 ms | 10.62 ms | 3284.67 | 150328.67 | 9.37 Mb |
| go (1.13)| gorouter-fasthttp (4.1) | **6.47** ms | 5.27 ms | 9.30 ms | 10427.00 | 158892.67 | 16.93 Mb |
| ruby (2.6)| hanami (1.3) | **6.49** ms | 0.65 ms | 21.84 ms | 11355.67 | 19764.67 | 9.93 Mb |
| crystal (0.31)| raze (0.3) | **6.70** ms | 5.90 ms | 10.76 ms | 3235.33 | 144234.67 | 8.99 Mb |
| go (1.13)| atreugo (8.2) | **6.73** ms | 5.13 ms | 9.19 ms | 14161.33 | 162138.00 | 21.62 Mb |
| crystal (0.31)| kemal (0.28) | **7.07** ms | 6.29 ms | 11.31 ms | 3387.33 | 136473.00 | 14.78 Mb |
| nim (1.0)| jester (0.4) | **7.39** ms | 6.56 ms | 12.39 ms | 4223.33 | 145424.33 | 19.37 Mb |
| crystal (0.31)| amber (0.3) | **7.75** ms | 7.21 ms | 12.09 ms | 3514.33 | 125356.67 | 15.21 Mb |
| java (8)| rapidoid (5.5) | **7.77** ms | 5.78 ms | 12.88 ms | 13285.33 | 149404.67 | 17.82 Mb |
| crystal (0.31)| orion (1.7) | **8.27** ms | 7.67 ms | 13.16 ms | 3886.00 | 118322.33 | 12.82 Mb |
| ruby (2.6)| sinatra (2.0) | **8.40** ms | 1.38 ms | 25.63 ms | 12992.00 | 15232.67 | 2.63 Mb |
| java (8)| act (1.8) | **9.57** ms | 8.22 ms | 14.20 ms | 9102.67 | 117684.33 | 13.46 Mb |
| go (1.13)| rte (0.0) | **9.57** ms | 7.75 ms | 15.76 ms | 9888.67 | 108995.33 | 9.68 Mb |
| go (1.13)| gorouter (4.1) | **9.99** ms | 8.13 ms | 17.23 ms | 8168.33 | 102486.67 | 9.09 Mb |
| ruby (2.6)| grape (1.2) | **10.09** ms | 1.34 ms | 31.22 ms | 15532.00 | 12783.00 | 0.48 Mb |
| go (1.13)| chi (4.0) | **10.41** ms | 8.20 ms | 17.96 ms | 10154.67 | 102204.67 | 9.07 Mb |
| rust (1.38)| actix-web (1.0) | **10.44** ms | 9.89 ms | 14.09 ms | 3099.33 | 106057.67 | 10.21 Mb |
| go (1.13)| kami (2.2) | **10.63** ms | 8.58 ms | 17.31 ms | 11991.67 | 99217.33 | 8.75 Mb |
| go (1.13)| violetear (7.0) | **10.70** ms | 8.92 ms | 16.28 ms | 11635.00 | 97403.67 | 8.58 Mb |
| c (11)| kore (3.3) | **10.71** ms | 5.83 ms | 15.08 ms | 29769.67 | 164466.00 | 29.64 Mb |
| go (1.13)| gin (1.4) | **10.76** ms | 8.54 ms | 19.11 ms | 9142.00 | 97552.33 | 11.35 Mb |
| go (1.13)| echo (4.1) | **10.78** ms | 8.50 ms | 19.09 ms | 9914.33 | 97412.00 | 11.33 Mb |
| go (1.13)| goroute (0.0) | **10.83** ms | 8.44 ms | 19.14 ms | 11410.67 | 98315.00 | 11.44 Mb |
| go (1.13)| gorilla-mux (1.7) | **11.19** ms | 8.65 ms | 20.46 ms | 10328.33 | 95399.67 | 8.44 Mb |
| go (1.13)| beego (1.12) | **11.43** ms | 8.64 ms | 19.53 ms | 15093.00 | 96066.67 | 8.56 Mb |
| python (3.7)| falcon (2.0) | **12.53** ms | 10.16 ms | 21.24 ms | 8486.00 | 81226.33 | 12.62 Mb |
| go (1.13)| air (0.13) | **12.75** ms | 9.60 ms | 23.37 ms | 13356.67 | 85008.00 | 11.76 Mb |
| swift (5.1)| perfect (3.1) | **13.07** ms | 13.17 ms | 15.43 ms | 2126.33 | 74802.67 | 4.66 Mb |
| go (1.13)| gf (1.9) | **13.55** ms | 10.83 ms | 22.76 ms | 13521.33 | 78389.33 | 8.82 Mb |
| csharp (7.3)| aspnetcore (2.2) | **13.62** ms | 9.94 ms | 16.78 ms | 29288.00 | 86730.33 | 9.36 Mb |
| php (7.3)| one (1.8) | **13.78** ms | 12.48 ms | 23.75 ms | 8208.33 | 72867.00 | 11.10 Mb |
| ruby (2.6)| agoo (2.11) | **15.16** ms | 14.92 ms | 16.88 ms | 1875.00 | 64760.67 | 2.48 Mb |
| node (12.11)| 0http (1.2) | **16.08** ms | 9.38 ms | 17.77 ms | 48496.67 | 92183.00 | 9.15 Mb |
| node (12.11)| restana (3.3) | **16.26** ms | 9.51 ms | 18.15 ms | 48566.33 | 90002.67 | 8.94 Mb |
| python (3.7)| bottle (0.12) | **16.68** ms | 13.97 ms | 26.26 ms | 9326.00 | 60705.33 | 9.91 Mb |
| php (7.3)| hyperf (1.0) | **16.99** ms | 14.75 ms | 32.27 ms | 11801.33 | 61269.00 | 8.66 Mb |
| rust (1.38)| gotham (0.4) | **17.05** ms | 17.12 ms | 23.93 ms | 11286.33 | 58989.67 | 7.95 Mb |
| python (3.7)| asgineer (0.7) | **18.01** ms | 15.73 ms | 29.50 ms | 9519.33 | 56541.00 | 6.69 Mb |
| node (12.11)| rayo (1.3) | **18.82** ms | 10.19 ms | 19.85 ms | 54262.00 | 81216.67 | 8.07 Mb |
| python (3.7)| blacksheep (0.2) | **19.37** ms | 16.65 ms | 32.26 ms | 10745.33 | 52791.67 | 7.04 Mb |
| node (12.11)| polkadot (1.0) | **19.56** ms | 9.29 ms | 17.75 ms | 66767.00 | 93895.00 | 9.32 Mb |
| ruby (2.6)| plezi (0.16) | **20.39** ms | 17.76 ms | 29.97 ms | 14803.67 | 49195.00 | 6.95 Mb |
| python (3.7)| hug (2.6) | **20.53** ms | 16.39 ms | 33.95 ms | 11666.00 | 49413.33 | 8.12 Mb |
| node (12.11)| polka (0.5) | **20.98** ms | 10.16 ms | 20.08 ms | 68739.33 | 82916.33 | 8.23 Mb |
| python (3.7)| starlette (0.12) | **21.48** ms | 19.59 ms | 33.38 ms | 9682.67 | 46519.33 | 6.64 Mb |
| node (12.11)| foxify (0.1) | **21.64** ms | 12.28 ms | 23.50 ms | 59114.33 | 68628.67 | 9.56 Mb |
| php (7.3)| swoft (2.0) | **23.38** ms | 22.57 ms | 30.89 ms | 7271.33 | 41851.00 | 7.30 Mb |
| node (12.11)| iotjs-express (0.0) | **24.25** ms | 14.02 ms | 26.54 ms | 62922.67 | 60481.00 | 16.29 Mb |
| node (12.11)| muneem (2.4) | **24.35** ms | 12.11 ms | 23.81 ms | 76698.67 | 70094.00 | 6.96 Mb |
| node (12.11)| fastify (2.8) | **24.70** ms | 14.77 ms | 27.36 ms | 64257.00 | 60229.67 | 10.61 Mb |
| swift (5.1)| kitura-nio (2.8) | **25.97** ms | 24.06 ms | 31.34 ms | 20852.00 | 38460.67 | 4.74 Mb |
| node (12.11)| restify (8.4) | **26.60** ms | 19.08 ms | 31.67 ms | 46033.67 | 45869.00 | 5.34 Mb |
| java (8)| spring-boot (2.1) | **27.97** ms | 15.56 ms | 37.75 ms | 75502.67 | 48307.33 | 2.54 Mb |
| swift (5.1)| kitura (2.8) | **28.09** ms | 21.01 ms | 24.89 ms | 74241.00 | 45449.00 | 5.61 Mb |
| node (12.11)| koa (2.8) | **28.53** ms | 13.74 ms | 26.36 ms | 85627.00 | 61602.67 | 8.64 Mb |
| kotlin (1.3)| ktor (1.2) | **28.77** ms | 12.33 ms | 29.09 ms | 97089.00 | 73222.33 | 7.57 Mb |
| php (7.3)| imi (1.0) | **29.39** ms | 28.33 ms | 36.36 ms | 6572.33 | 33223.33 | 5.06 Mb |
| node (12.11)| express (4.17) | **29.58** ms | 15.38 ms | 29.23 ms | 85044.00 | 54904.00 | 8.91 Mb |
| swift (5.1)| vapor (3.3) | **29.97** ms | 17.58 ms | 33.60 ms | 78975.67 | 47033.00 | 5.31 Mb |
| ruby (2.6)| rails (6.0) | **32.42** ms | 2.36 ms | 107.20 ms | 61689.67 | 3958.33 | 1.66 Mb |
| python (3.7)| fastapi (0.42) | **36.76** ms | 31.52 ms | 62.74 ms | 19291.67 | 27828.00 | 3.98 Mb |
| crystal (0.31)| spider-gazelle (1.6) | **37.49** ms | 34.85 ms | 44.17 ms | 25508.67 | 26755.00 | 1.89 Mb |
| python (3.7)| clastic (19.9) | **38.78** ms | 33.49 ms | 60.69 ms | 18116.00 | 25508.00 | 4.19 Mb |
| python (3.7)| molten (0.27) | **40.31** ms | 33.96 ms | 64.55 ms | 17438.67 | 25262.67 | 3.11 Mb |
| python (3.7)| flask (1.1) | **41.58** ms | 34.65 ms | 67.32 ms | 18794.67 | 24112.33 | 3.94 Mb |
| python (3.7)| aiohttp (3.6) | **42.69** ms | 40.33 ms | 62.50 ms | 15119.33 | 23198.33 | 3.49 Mb |
| php (7.3)| basicphp (0.9) | **43.01** ms | 14.85 ms | 89.55 ms | 90179.00 | 54472.67 | 17.93 Mb |
| crystal (0.31)| lucky (0.18) | **43.83** ms | 43.86 ms | 54.37 ms | 14106.00 | 22064.67 | 1.80 Mb |
| fsharp (7.3)| suave (2.5) | **45.78** ms | 21.63 ms | 111.04 ms | 49442.67 | 25562.00 | 3.43 Mb |
| php (7.3)| slim (4.3) | **47.49** ms | 15.59 ms | 89.20 ms | 105213.67 | 51769.00 | 17.05 Mb |
| php (7.3)| lumen (6.1) | **48.03** ms | 15.46 ms | 85.80 ms | 107399.00 | 51888.33 | 17.10 Mb |
| node (12.11)| turbo_polka (2.0) | **48.70** ms | 41.77 ms | 48.86 ms | 59444.33 | 22391.33 | 1.40 Mb |
| php (7.3)| zend-expressive (3.2) | **48.93** ms | 15.70 ms | 93.37 ms | 111435.67 | 51354.33 | 16.91 Mb |
| php (7.3)| symfony (4.3) | **50.46** ms | 15.71 ms | 95.39 ms | 117086.67 | 50630.00 | 16.68 Mb |
| python (3.7)| bocadillo (0.18) | **51.29** ms | 46.18 ms | 82.10 ms | 24607.33 | 19577.33 | 2.51 Mb |
| python (3.7)| sanic (19.9) | **51.31** ms | 41.75 ms | 94.83 ms | 32901.00 | 19983.33 | 2.36 Mb |
| php (7.3)| zend-framework (3.1) | **55.21** ms | 16.74 ms | 104.67 ms | 126144.33 | 48471.33 | 15.97 Mb |
| clojure (1.10)| coast (1.0) | **56.89** ms | 18.99 ms | 21.83 ms | 205075.33 | 49225.67 | 5.87 Mb |
| java (8)| javalin (3.5) | **58.22** ms | 11.53 ms | 51.98 ms | 251070.67 | 64754.00 | 7.66 Mb |
| php (7.3)| spiral (2.1) | **59.40** ms | 58.96 ms | 65.34 ms | 12172.33 | 16530.00 | 1.91 Mb |
| node (12.11)| moleculer (0.13) | **60.19** ms | 26.99 ms | 54.49 ms | 161907.67 | 30762.33 | 3.51 Mb |
| scala (2.12)| http4s (0.18) | **60.92** ms | 17.35 ms | 36.87 ms | 238616.33 | 47708.00 | 5.55 Mb |
| node (12.11)| hapi (18.1) | **66.77** ms | 24.56 ms | 45.55 ms | 202341.67 | 35455.33 | 6.10 Mb |
| crystal (0.31)| athena (0.7) | **66.97** ms | 48.91 ms | 180.10 ms | 83600.67 | 24114.67 | 2.00 Mb |
| php (7.3)| laravel (6.2) | **73.89** ms | 19.12 ms | 134.96 ms | 181880.67 | 42441.67 | 14.04 Mb |
| python (3.7)| quart (0.10) | **87.46** ms | 84.94 ms | 137.49 ms | 37474.67 | 11241.67 | 1.49 Mb |
| go (1.13)| gramework (1.6) | **96.11** ms | 98.02 ms | 101.90 ms | 16333.33 | 10090.00 | 1.71 Mb |
| python (3.7)| tornado (5.1) | **100.84** ms | 100.45 ms | 123.59 ms | 24621.00 | 9556.33 | 1.87 Mb |
| python (3.7)| responder (1.3) | **105.30** ms | 91.54 ms | 176.71 ms | 49092.00 | 9369.33 | 1.36 Mb |
| python (3.7)| django (2.2) | **105.92** ms | 91.40 ms | 178.39 ms | 43114.67 | 9234.00 | 1.78 Mb |
| python (3.7)| masonite (2.2) | **138.31** ms | 120.39 ms | 226.65 ms | 50841.00 | 7007.00 | 1.14 Mb |
| crystal (0.31)| onyx (0.5) | **195.24** ms | 194.89 ms | 229.03 ms | 30482.33 | 5032.67 | 0.86 Mb |
| scala (2.12)| akkahttp (10.1) | **209.80** ms | 7.69 ms | 89.87 ms | 831744.33 | 67306.67 | 9.65 Mb |
| perl (5.3)| dancer2 (2.0) | **339.73** ms | 109.33 ms | 424.50 ms | 776220.67 | 1004.67 | 0.15 Mb |
| python (3.7)| cyclone (1.3) | **380.75** ms | 341.13 ms | 421.44 ms | 449679.00 | 2252.00 | 0.38 Mb |

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

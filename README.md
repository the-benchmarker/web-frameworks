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
| rust (1.38)| nickel (0.11) | **0.25** ms | 0.19 ms | 0.43 ms | 276.67 | 38972.33 | 5.15 Mb |
| rust (1.38)| iron (0.6) | **3.06** ms | 2.86 ms | 4.92 ms | 1757.67 | 21445.33 | 1.77 Mb |
| ruby (2.6)| syro (3.1) | **3.19** ms | 0.63 ms | 8.57 ms | 7472.33 | 41738.00 | 1.60 Mb |
| ruby (2.6)| roda (3.25) | **3.33** ms | 0.64 ms | 9.67 ms | 7109.33 | 39245.33 | 2.48 Mb |
| ruby (2.6)| cuba (3.9) | **3.83** ms | 0.60 ms | 10.64 ms | 9560.00 | 35822.00 | 2.81 Mb |
| ruby (2.6)| rack-routing (0.0) | **4.57** ms | 0.58 ms | 14.74 ms | 10363.67 | 28898.67 | 1.11 Mb |
| c (11)| agoo-c (0.7) | **4.58** ms | 4.26 ms | 8.92 ms | 3442.67 | 210924.67 | 8.09 Mb |
| ruby (2.6)| camping (2.1) | **4.68** ms | 0.51 ms | 15.71 ms | 8362.00 | 27592.00 | 1.75 Mb |
| javascript (12.11)| sifrr (0.0) | **4.82** ms | 4.32 ms | 9.81 ms | 4013.00 | 202147.33 | 11.79 Mb |
| nim (1.0)| httpbeast (0.2) | **5.00** ms | 4.48 ms | 9.56 ms | 3783.67 | 194038.67 | 18.31 Mb |
| python (3.7)| japronto (0.1) | **5.04** ms | 4.59 ms | 9.86 ms | 3823.00 | 191790.00 | 15.21 Mb |
| go (1.13)| fasthttp (1.5) | **5.72** ms | 4.81 ms | 8.87 ms | 7101.00 | 169071.67 | 18.09 Mb |
| ruby (2.6)| flame (4.18) | **5.85** ms | 0.59 ms | 19.67 ms | 11057.00 | 21838.33 | 0.84 Mb |
| crystal (0.31)| toro (0.4) | **6.30** ms | 5.49 ms | 10.43 ms | 3219.00 | 152896.00 | 9.53 Mb |
| swift (5.1)| swifter (1.4) | **6.39** ms | 0.92 ms | 14.82 ms | 98398.00 | 10756.67 | 0.92 Mb |
| java (8)| rapidoid (5.5) | **6.41** ms | 4.96 ms | 10.61 ms | 11245.33 | 168303.00 | 20.08 Mb |
| crystal (0.31)| router.cr (0.2) | **6.41** ms | 5.62 ms | 10.42 ms | 3194.00 | 150236.67 | 9.36 Mb |
| go (1.13)| atreugo (8.2) | **6.50** ms | 5.11 ms | 9.16 ms | 12266.67 | 161112.67 | 21.48 Mb |
| crystal (0.31)| raze (0.3) | **6.70** ms | 5.81 ms | 10.92 ms | 3403.67 | 144590.00 | 9.01 Mb |
| go (1.13)| gorouter-fasthttp (4.2) | **6.72** ms | 5.27 ms | 9.37 ms | 13289.33 | 157040.33 | 16.71 Mb |
| crystal (0.31)| spider-gazelle (2.0) | **6.80** ms | 5.75 ms | 11.63 ms | 3782.33 | 143742.33 | 10.17 Mb |
| crystal (0.31)| kemal (0.28) | **6.94** ms | 5.99 ms | 11.39 ms | 3509.00 | 139337.00 | 15.09 Mb |
| ruby (2.6)| hanami (1.3) | **6.98** ms | 1.04 ms | 21.91 ms | 12121.00 | 18541.67 | 9.31 Mb |
| c (11)| kore (3.3) | **7.19** ms | 5.89 ms | 13.39 ms | 7708.00 | 164436.00 | 29.63 Mb |
| nim (1.0)| jester (0.4) | **7.34** ms | 6.66 ms | 11.89 ms | 4027.67 | 145140.00 | 19.33 Mb |
| cpp (11)| evhtp (1.2) | **7.41** ms | 5.79 ms | 13.14 ms | 7408.00 | 137409.33 | 8.84 Mb |
| go (1.13)| fasthttprouter (0.1) | **7.46** ms | 6.38 ms | 11.01 ms | 7890.33 | 129988.00 | 13.87 Mb |
| crystal (0.31)| amber (0.3) | **7.59** ms | 6.83 ms | 12.21 ms | 3633.00 | 127913.67 | 15.52 Mb |
| crystal (0.31)| orion (1.7) | **8.24** ms | 7.53 ms | 13.48 ms | 4127.67 | 118720.00 | 12.86 Mb |
| ruby (2.6)| sinatra (2.0) | **9.41** ms | 0.94 ms | 30.02 ms | 19039.00 | 13930.00 | 2.40 Mb |
| go (1.13)| rte (0.0) | **9.41** ms | 7.73 ms | 15.73 ms | 8021.33 | 108744.00 | 9.65 Mb |
| java (8)| act (1.8) | **9.77** ms | 7.43 ms | 12.74 ms | 20100.67 | 125946.00 | 14.41 Mb |
| go (1.13)| gorouter (4.2) | **9.83** ms | 8.04 ms | 16.63 ms | 8385.33 | 105145.33 | 9.27 Mb |
| rust (1.38)| actix-web (1.0) | **10.11** ms | 9.54 ms | 13.41 ms | 4812.33 | 108764.33 | 10.45 Mb |
| go (1.13)| chi (4.0) | **10.68** ms | 8.26 ms | 17.84 ms | 13389.33 | 101624.00 | 9.02 Mb |
| go (1.13)| violetear (7.0) | **10.77** ms | 8.96 ms | 16.52 ms | 11103.67 | 96428.33 | 8.48 Mb |
| go (1.13)| kami (2.2) | **10.77** ms | 8.61 ms | 17.48 ms | 11905.67 | 98807.00 | 8.71 Mb |
| ruby (2.6)| grape (1.2) | **10.83** ms | 0.85 ms | 39.44 ms | 21199.00 | 11906.00 | 0.45 Mb |
| go (1.13)| goroute (0.0) | **10.97** ms | 8.50 ms | 19.43 ms | 11072.00 | 97205.33 | 11.31 Mb |
| csharp (7.3)| aspnetcore (2.2) | **11.31** ms | 9.94 ms | 16.70 ms | 7128.00 | 85707.67 | 9.25 Mb |
| go (1.13)| beego (1.12) | **11.36** ms | 8.66 ms | 19.15 ms | 15240.33 | 95817.33 | 8.55 Mb |
| swift (5.1)| perfect (3.1) | **12.76** ms | 12.88 ms | 15.15 ms | 3742.33 | 75721.00 | 4.72 Mb |
| python (3.7)| falcon (2.0) | **12.80** ms | 10.04 ms | 22.88 ms | 9113.67 | 80558.33 | 12.51 Mb |
| go (1.13)| air (0.13) | **12.82** ms | 9.67 ms | 23.78 ms | 12420.67 | 84419.00 | 11.68 Mb |
| go (1.13)| gf (1.9) | **13.87** ms | 10.87 ms | 24.65 ms | 13036.67 | 77313.33 | 8.71 Mb |
| php (7.3)| one (1.8) | **13.93** ms | 12.50 ms | 24.17 ms | 8413.00 | 72623.33 | 11.06 Mb |
| go (1.13)| echo (4.1) | **14.24** ms | 10.27 ms | 25.63 ms | 18672.33 | 78686.00 | 9.15 Mb |
| go (1.13)| gorilla-mux (1.7) | **14.80** ms | 8.65 ms | 21.21 ms | 40662.67 | 93928.33 | 8.31 Mb |
| go (1.13)| gin (1.4) | **15.08** ms | 10.67 ms | 25.73 ms | 22570.33 | 77490.00 | 9.01 Mb |
| javascript (12.11)| restana (3.3) | **15.64** ms | 9.81 ms | 19.72 ms | 40830.33 | 85704.33 | 8.51 Mb |
| javascript (12.11)| rayo (1.3) | **16.11** ms | 10.33 ms | 20.29 ms | 38225.33 | 79867.33 | 7.93 Mb |
| ruby (2.6)| agoo (2.11) | **16.34** ms | 15.95 ms | 18.69 ms | 4269.33 | 60526.33 | 2.32 Mb |
| python (3.7)| bottle (0.12) | **16.45** ms | 14.12 ms | 26.26 ms | 8737.33 | 61264.33 | 10.00 Mb |
| php (7.3)| hyperf (1.0) | **16.80** ms | 14.71 ms | 31.02 ms | 10959.00 | 61318.33 | 8.67 Mb |
| javascript (12.11)| 0http (1.2) | **17.10** ms | 9.42 ms | 18.58 ms | 53781.33 | 90730.33 | 9.01 Mb |
| go (1.13)| webgo (3.0) | **17.26** ms | 12.16 ms | 32.67 ms | 17692.00 | 66452.00 | 5.81 Mb |
| ruby (2.6)| plezi (0.16) | **17.54** ms | 16.65 ms | 22.24 ms | 8886.00 | 55993.33 | 7.91 Mb |
| javascript (12.11)| polkadot (1.0) | **17.56** ms | 9.29 ms | 18.34 ms | 56911.00 | 92703.00 | 9.21 Mb |
| rust (1.38)| gotham (0.4) | **17.62** ms | 16.78 ms | 26.74 ms | 13873.33 | 57807.00 | 7.77 Mb |
| python (3.7)| asgineer (0.7) | **18.10** ms | 16.89 ms | 28.78 ms | 8163.00 | 54897.33 | 6.50 Mb |
| php (7.3)| sw-fw-less (preview) | **18.37** ms | 17.25 ms | 29.19 ms | 8726.67 | 53481.33 | 8.15 Mb |
| go (1.13)| mars (1.0) | **18.60** ms | 12.62 ms | 42.77 ms | 18676.00 | 62363.00 | 9.31 Mb |
| cpp (11)| drogon (1.0) | **18.64** ms | 15.44 ms | 20.85 ms | 34163.67 | 62024.67 | 3.99 Mb |
| javascript (12.11)| polka (0.5) | **19.62** ms | 10.15 ms | 20.28 ms | 60975.33 | 81607.00 | 8.10 Mb |
| python (3.7)| starlette (0.12) | **21.81** ms | 19.59 ms | 35.98 ms | 10458.00 | 45626.00 | 6.52 Mb |
| python (3.7)| hug (2.6) | **22.01** ms | 18.59 ms | 34.24 ms | 11149.67 | 45443.33 | 7.47 Mb |
| python (3.7)| blacksheep (0.2) | **22.15** ms | 20.27 ms | 34.50 ms | 10783.67 | 45292.67 | 6.04 Mb |
| swift (5.1)| kitura (2.8) | **22.33** ms | 21.11 ms | 25.30 ms | 22739.33 | 44951.67 | 5.54 Mb |
| swift (5.1)| kitura-nio (2.8) | **23.46** ms | 21.05 ms | 24.84 ms | 31787.33 | 45369.67 | 5.60 Mb |
| php (7.3)| swoft (2.0) | **23.77** ms | 22.65 ms | 30.53 ms | 6374.00 | 41416.67 | 7.22 Mb |
| javascript (12.11)| foxify (0.1) | **23.84** ms | 12.54 ms | 23.74 ms | 69544.67 | 67531.67 | 9.41 Mb |
| javascript (12.11)| koa (2.8) | **26.25** ms | 13.92 ms | 26.49 ms | 71340.33 | 60354.00 | 8.47 Mb |
| php (7.3)| imi (1.0) | **26.54** ms | 25.47 ms | 33.38 ms | 7332.33 | 36863.33 | 5.62 Mb |
| javascript (12.11)| fastify (2.1) | **26.73** ms | 15.03 ms | 27.99 ms | 73327.00 | 59075.00 | 10.39 Mb |
| javascript (12.11)| muneem (2.4) | **27.54** ms | 11.77 ms | 23.25 ms | 93830.67 | 71552.67 | 7.11 Mb |
| kotlin (1.3)| ktor (1.2) | **27.78** ms | 12.29 ms | 29.09 ms | 91290.33 | 72166.00 | 7.46 Mb |
| javascript (12.11)| restify (8.4) | **27.88** ms | 19.13 ms | 31.92 ms | 55664.00 | 45529.00 | 5.30 Mb |
| javascript (12.11)| iotjs-express (0.0) | **28.60** ms | 14.55 ms | 27.32 ms | 84974.33 | 59030.00 | 15.90 Mb |
| javascript (12.11)| express (4.17) | **29.90** ms | 15.73 ms | 29.63 ms | 84621.00 | 53479.33 | 8.68 Mb |
| swift (5.1)| vapor (3.3) | **32.95** ms | 17.39 ms | 32.12 ms | 108491.33 | 49197.67 | 5.57 Mb |
| python (3.7)| responder (2.0) | **36.65** ms | 34.34 ms | 60.75 ms | 17641.00 | 27075.00 | 3.92 Mb |
| python (3.7)| fastapi (0.42) | **36.76** ms | 32.17 ms | 60.59 ms | 18671.67 | 27287.33 | 3.90 Mb |
| ruby (2.6)| rails (6.0) | **38.95** ms | 2.80 ms | 129.81 ms | 70465.67 | 3257.00 | 1.36 Mb |
| python (3.7)| clastic (19.9) | **38.96** ms | 33.66 ms | 59.31 ms | 16127.33 | 25214.67 | 4.14 Mb |
| python (3.7)| molten (0.27) | **39.99** ms | 35.16 ms | 58.98 ms | 15655.00 | 25237.67 | 3.11 Mb |
| fsharp (7.3)| suave (2.5) | **39.99** ms | 31.92 ms | 85.32 ms | 41178.67 | 25269.67 | 3.40 Mb |
| python (3.7)| flask (1.1) | **42.35** ms | 34.22 ms | 74.83 ms | 19914.00 | 23492.00 | 3.84 Mb |
| java (8)| spring-boot (2.1) | **42.63** ms | 15.31 ms | 38.25 ms | 164921.33 | 48841.33 | 2.56 Mb |
| crystal (0.31)| lucky (0.18) | **43.46** ms | 40.06 ms | 52.73 ms | 24103.00 | 22816.00 | 1.86 Mb |
| python (3.7)| aiohttp (3.6) | **44.01** ms | 41.10 ms | 63.46 ms | 14277.33 | 22518.67 | 3.38 Mb |
| python (3.7)| sanic (19.9) | **49.80** ms | 47.69 ms | 76.31 ms | 23870.00 | 19695.67 | 2.33 Mb |
| javascript (12.11)| turbo_polka (2.0) | **51.41** ms | 41.68 ms | 48.82 ms | 75503.33 | 22401.33 | 1.40 Mb |
| python (3.7)| bocadillo (0.18) | **52.44** ms | 47.32 ms | 83.17 ms | 24339.00 | 19099.67 | 2.45 Mb |
| php (7.3)| lumen (6.2) | **54.04** ms | 18.37 ms | 113.75 ms | 111790.00 | 43741.33 | 14.41 Mb |
| php (7.3)| basicphp (0.9) | **54.70** ms | 17.98 ms | 107.97 ms | 120014.67 | 44131.33 | 14.57 Mb |
| clojure (1.10)| coast (1.0) | **55.05** ms | 18.73 ms | 21.76 ms | 205597.33 | 47922.67 | 5.72 Mb |
| php (7.3)| slim (4.3) | **55.37** ms | 18.40 ms | 114.11 ms | 119272.33 | 43841.67 | 14.44 Mb |
| php (7.3)| zend-expressive (3.2) | **56.45** ms | 19.05 ms | 113.88 ms | 119504.00 | 42806.33 | 14.10 Mb |
| php (7.3)| symfony (4.3) | **56.79** ms | 18.65 ms | 116.39 ms | 121088.33 | 42768.00 | 14.09 Mb |
| php (7.3)| spiral (2.3) | **58.57** ms | 58.45 ms | 65.22 ms | 6362.33 | 16656.33 | 1.92 Mb |
| php (7.3)| zend-framework (3.1) | **60.92** ms | 19.50 ms | 126.57 ms | 132034.00 | 41533.00 | 13.68 Mb |
| javascript (12.11)| hapi (18.4) | **63.16** ms | 24.27 ms | 46.16 ms | 185315.00 | 34990.67 | 6.02 Mb |
| java (8)| micronaut (1.2) | **64.71** ms | 22.25 ms | 101.38 ms | 195549.00 | 24182.00 | 3.35 Mb |
| crystal (0.31)| athena (0.7) | **65.70** ms | 48.03 ms | 176.47 ms | 82036.33 | 24555.00 | 2.04 Mb |
| javascript (12.11)| moleculer (0.13) | **71.91** ms | 27.56 ms | 58.01 ms | 207550.00 | 29844.00 | 3.40 Mb |
| php (7.3)| laravel (6.4) | **80.03** ms | 22.53 ms | 160.33 ms | 185126.67 | 36192.00 | 11.98 Mb |
| scala (2.12)| http4s (0.18) | **82.19** ms | 22.33 ms | 55.78 ms | 341870.00 | 38909.33 | 4.53 Mb |
| python (3.7)| quart (0.10) | **87.79** ms | 84.02 ms | 138.98 ms | 39514.00 | 11112.00 | 1.47 Mb |
| python (3.7)| cherrypy (18.3) | **89.09** ms | 72.88 ms | 75.47 ms | 230511.67 | 1401.00 | 0.22 Mb |
| java (8)| javalin (3.5) | **98.15** ms | 10.48 ms | 155.75 ms | 344832.33 | 63650.33 | 7.53 Mb |
| go (1.13)| gramework (1.6) | **101.07** ms | 102.77 ms | 109.98 ms | 20844.33 | 9629.67 | 1.63 Mb |
| python (3.7)| tornado (5.1) | **101.46** ms | 98.46 ms | 122.14 ms | 40214.33 | 9467.33 | 1.86 Mb |
| python (3.7)| django (2.2) | **107.27** ms | 100.72 ms | 138.79 ms | 35691.33 | 9077.00 | 1.75 Mb |
| python (3.7)| masonite (2.2) | **136.50** ms | 119.19 ms | 225.87 ms | 51294.33 | 7082.00 | 1.16 Mb |
| crystal (0.31)| onyx (0.5) | **194.26** ms | 193.42 ms | 236.31 ms | 33715.67 | 5057.00 | 0.87 Mb |
| perl (5.3)| dancer2 (2.0) | **198.18** ms | 56.96 ms | 299.89 ms | 497499.67 | 1416.33 | 0.21 Mb |
| scala (2.12)| akkahttp (10.1) | **226.68** ms | 8.61 ms | 34.02 ms | 874131.67 | 53833.33 | 7.72 Mb |
| julia (1.3)| merly (0.2) | **312.74** ms | 91.84 ms | 269.14 ms | 904290.67 | 3801.33 | 0.30 Mb |
| python (3.7)| cyclone (1.3) | **391.91** ms | 336.11 ms | 439.66 ms | 482420.67 | 2237.67 | 0.38 Mb |
| python (3.7)| klein (19.6) | **513.26** ms | 434.12 ms | 549.62 ms | 573163.33 | 1445.00 | 0.21 Mb |
| python (3.7)| nameko (2.12) | **606.43** ms | 535.91 ms | 589.60 ms | 657269.33 | 1313.33 | 0.18 Mb |

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

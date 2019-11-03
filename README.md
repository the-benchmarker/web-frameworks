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
| rust (1.38)| nickel (0.11) | **0.26** ms | 0.22 ms | 0.42 ms | 244.67 | 37149.33 | 4.91 Mb |
| ruby (2.6)| syro (3.1) | **2.66** ms | 0.58 ms | 7.89 ms | 4131.00 | 47338.33 | 1.81 Mb |
| ruby (2.6)| roda (3.25) | **2.72** ms | 0.60 ms | 8.16 ms | 4293.33 | 46431.67 | 2.94 Mb |
| rust (1.38)| iron (0.6) | **3.04** ms | 2.89 ms | 4.63 ms | 1496.00 | 21075.67 | 1.74 Mb |
| ruby (2.6)| cuba (3.9) | **3.19** ms | 1.32 ms | 8.68 ms | 4255.33 | 39877.67 | 3.12 Mb |
| ruby (2.6)| rack-routing (0.0) | **3.78** ms | 0.54 ms | 12.28 ms | 6524.33 | 33798.67 | 1.29 Mb |
| ruby (2.6)| camping (2.1) | **4.72** ms | 0.76 ms | 14.86 ms | 7460.33 | 27125.00 | 1.72 Mb |
| python (3.7)| japronto (0.1) | **4.86** ms | 4.39 ms | 9.63 ms | 3798.67 | 201134.33 | 15.95 Mb |
| c (11)| agoo-c (0.7) | **4.90** ms | 4.57 ms | 9.02 ms | 3578.00 | 200556.33 | 7.69 Mb |
| nim (1.0)| httpbeast (0.2) | **5.10** ms | 4.49 ms | 9.79 ms | 4035.00 | 192134.33 | 18.13 Mb |
| javascript (12.11)| sifrr (0.0) | **5.20** ms | 4.43 ms | 10.62 ms | 4957.67 | 194341.67 | 11.34 Mb |
| cpp (11)| drogon (1.0) | **5.42** ms | 4.87 ms | 9.96 ms | 3643.67 | 178149.33 | 11.46 Mb |
| go (1.13)| fasthttp (1.5) | **5.53** ms | 4.79 ms | 8.75 ms | 4240.00 | 172157.33 | 18.42 Mb |
| ruby (2.6)| flame (4.18) | **5.65** ms | 1.62 ms | 17.87 ms | 9558.67 | 22720.67 | 0.87 Mb |
| go (1.13)| atreugo (8.2) | **5.80** ms | 5.07 ms | 9.00 ms | 4244.00 | 163750.33 | 21.83 Mb |
| cpp (11)| evhtp (1.2) | **5.84** ms | 5.23 ms | 9.44 ms | 2880.00 | 160988.67 | 10.36 Mb |
| go (1.13)| gorouter-fasthttp (4.2) | **5.93** ms | 5.06 ms | 8.63 ms | 7623.67 | 165974.67 | 17.68 Mb |
| ruby (2.6)| hanami (1.3) | **6.24** ms | 0.56 ms | 21.92 ms | 11360.00 | 20449.67 | 10.27 Mb |
| swift (5.1)| swifter (1.4) | **6.36** ms | 0.90 ms | 14.58 ms | 93117.67 | 12196.33 | 1.04 Mb |
| crystal (0.31)| router.cr (0.2) | **6.43** ms | 5.62 ms | 10.67 ms | 3331.33 | 150196.67 | 9.36 Mb |
| crystal (0.31)| toro (0.4) | **6.46** ms | 5.67 ms | 10.56 ms | 3204.33 | 149025.33 | 9.29 Mb |
| go (1.13)| fasthttprouter (0.1) | **6.55** ms | 5.12 ms | 9.19 ms | 12309.00 | 162188.67 | 17.31 Mb |
| crystal (0.31)| raze (0.3) | **6.80** ms | 5.94 ms | 11.03 ms | 3363.00 | 142158.67 | 8.86 Mb |
| crystal (0.31)| kemal (0.28) | **7.02** ms | 6.18 ms | 11.34 ms | 3414.00 | 137742.00 | 14.92 Mb |
| ruby (2.6)| sinatra (2.0) | **7.17** ms | 0.65 ms | 24.27 ms | 12448.00 | 17851.67 | 3.08 Mb |
| java (8)| rapidoid (5.5) | **7.43** ms | 4.66 ms | 10.71 ms | 15805.33 | 174614.00 | 20.83 Mb |
| nim (1.0)| jester (0.4) | **7.52** ms | 6.75 ms | 12.39 ms | 4274.00 | 141333.00 | 18.83 Mb |
| crystal (0.31)| amber (0.3) | **7.60** ms | 7.05 ms | 11.66 ms | 3341.00 | 127245.33 | 15.44 Mb |
| c (11)| kore (3.3) | **7.81** ms | 6.12 ms | 14.22 ms | 9629.00 | 155701.33 | 28.06 Mb |
| java (8)| act (1.8) | **8.39** ms | 7.20 ms | 12.33 ms | 6877.00 | 129957.33 | 14.87 Mb |
| crystal (0.31)| orion (1.7) | **8.45** ms | 7.96 ms | 13.37 ms | 3939.33 | 115434.67 | 12.50 Mb |
| go (1.13)| rte (0.0) | **8.64** ms | 7.42 ms | 13.62 ms | 5702.00 | 116861.33 | 10.37 Mb |
| ruby (2.6)| grape (1.2) | **9.79** ms | 0.81 ms | 31.83 ms | 15556.33 | 13156.33 | 0.50 Mb |
| rust (1.38)| actix-web (1.0) | **10.25** ms | 9.73 ms | 13.68 ms | 4580.00 | 106243.33 | 10.18 Mb |
| go (1.13)| violetear (7.0) | **10.32** ms | 8.48 ms | 13.95 ms | 15698.00 | 104861.33 | 9.23 Mb |
| go (1.13)| kami (2.2) | **10.39** ms | 8.46 ms | 16.66 ms | 10676.00 | 101831.33 | 8.98 Mb |
| go (1.13)| gorouter (4.2) | **10.62** ms | 7.80 ms | 15.98 ms | 20023.33 | 107889.33 | 9.54 Mb |
| go (1.13)| gorilla-mux (1.7) | **10.63** ms | 8.48 ms | 18.65 ms | 9231.67 | 98232.33 | 8.68 Mb |
| go (1.13)| gin (1.4) | **10.64** ms | 8.52 ms | 18.66 ms | 9450.67 | 96280.67 | 11.20 Mb |
| go (1.13)| beego (1.12) | **10.89** ms | 8.21 ms | 16.31 ms | 20665.33 | 103102.00 | 9.19 Mb |
| go (1.13)| goroute (0.0) | **10.97** ms | 8.54 ms | 19.75 ms | 9785.00 | 96164.67 | 11.19 Mb |
| go (1.13)| echo (4.1) | **11.51** ms | 8.40 ms | 19.14 ms | 19102.67 | 97679.67 | 11.36 Mb |
| python (3.7)| falcon (2.0) | **11.65** ms | 9.86 ms | 18.96 ms | 6517.33 | 86035.67 | 13.36 Mb |
| csharp (7.3)| aspnetcore (2.2) | **12.05** ms | 9.28 ms | 14.82 ms | 24453.00 | 94061.33 | 10.15 Mb |
| go (1.13)| air (0.13) | **12.25** ms | 9.34 ms | 22.94 ms | 10528.00 | 87835.67 | 12.15 Mb |
| swift (5.1)| perfect (3.1) | **13.00** ms | 12.97 ms | 15.44 ms | 2791.00 | 75016.67 | 4.68 Mb |
| php (7.3)| one (1.8) | **13.18** ms | 12.28 ms | 22.05 ms | 7607.00 | 75458.00 | 11.49 Mb |
| javascript (12.11)| polkadot (1.0) | **13.28** ms | 9.15 ms | 18.13 ms | 30446.33 | 93323.33 | 9.27 Mb |
| go (1.13)| gf (1.9) | **13.39** ms | 10.35 ms | 19.85 ms | 20553.67 | 84034.67 | 9.47 Mb |
| go (1.13)| chi (4.0) | **13.74** ms | 9.79 ms | 23.83 ms | 21405.67 | 84547.00 | 7.57 Mb |
| ruby (2.6)| agoo (2.11) | **14.05** ms | 13.65 ms | 19.58 ms | 5582.67 | 70620.67 | 2.71 Mb |
| kotlin (1.3)| ktor (1.2) | **14.51** ms | 11.11 ms | 25.79 ms | 18114.33 | 77570.00 | 8.02 Mb |
| javascript (12.11)| restana (3.3) | **14.99** ms | 9.17 ms | 16.94 ms | 43014.33 | 94638.67 | 9.40 Mb |
| go (1.13)| webgo (3.0) | **15.15** ms | 9.22 ms | 18.74 ms | 41613.33 | 91989.33 | 8.12 Mb |
| python (3.7)| bottle (0.12) | **15.63** ms | 14.03 ms | 23.02 ms | 7424.67 | 63757.33 | 10.41 Mb |
| php (7.3)| hyperf (1.0) | **16.34** ms | 13.90 ms | 32.68 ms | 12427.33 | 65215.67 | 9.22 Mb |
| javascript (12.11)| polka (0.5) | **16.54** ms | 9.74 ms | 18.38 ms | 47526.00 | 87146.33 | 8.65 Mb |
| javascript (12.11)| 0http (1.2) | **17.41** ms | 9.92 ms | 20.40 ms | 49777.00 | 85160.33 | 8.46 Mb |
| php (7.3)| sw-fw-less (preview) | **17.53** ms | 16.71 ms | 26.73 ms | 7348.67 | 56119.00 | 8.55 Mb |
| rust (1.38)| gotham (0.4) | **17.64** ms | 16.57 ms | 25.77 ms | 19627.33 | 59696.00 | 8.03 Mb |
| python (3.7)| asgineer (0.7) | **17.66** ms | 15.54 ms | 28.32 ms | 9806.33 | 57352.33 | 6.79 Mb |
| ruby (2.6)| plezi (0.16) | **17.87** ms | 16.46 ms | 22.97 ms | 11922.33 | 55945.33 | 7.91 Mb |
| python (3.7)| blacksheep (0.2) | **18.78** ms | 17.34 ms | 28.65 ms | 8710.00 | 52929.33 | 7.06 Mb |
| python (3.7)| hug (2.6) | **19.42** ms | 15.91 ms | 31.25 ms | 10606.00 | 51776.33 | 8.51 Mb |
| javascript (12.11)| rayo (1.3) | **19.77** ms | 10.59 ms | 21.05 ms | 58379.00 | 78414.33 | 7.79 Mb |
| javascript (12.11)| foxify (0.1) | **20.09** ms | 11.81 ms | 22.32 ms | 56111.00 | 72094.33 | 10.04 Mb |
| go (1.13)| mars (1.0) | **20.27** ms | 13.46 ms | 45.89 ms | 22895.00 | 58821.67 | 8.78 Mb |
| python (3.7)| starlette (0.12) | **20.34** ms | 19.30 ms | 29.88 ms | 7674.33 | 48604.00 | 6.94 Mb |
| php (7.3)| swoft (2.0) | **22.54** ms | 22.29 ms | 27.27 ms | 3962.33 | 43511.00 | 7.59 Mb |
| javascript (12.11)| muneem (2.4) | **22.94** ms | 10.96 ms | 20.89 ms | 77130.33 | 77481.00 | 7.69 Mb |
| swift (5.1)| kitura (2.8) | **23.60** ms | 20.17 ms | 22.36 ms | 46969.00 | 48067.33 | 5.93 Mb |
| javascript (12.11)| koa (2.8) | **24.18** ms | 14.10 ms | 27.30 ms | 61242.33 | 59331.33 | 8.33 Mb |
| javascript (12.11)| express (4.17) | **24.23** ms | 14.91 ms | 27.20 ms | 58955.67 | 57311.00 | 9.30 Mb |
| swift (5.1)| vapor (3.3) | **25.32** ms | 17.26 ms | 29.95 ms | 65735.00 | 50284.00 | 5.71 Mb |
| php (7.3)| imi (1.0) | **25.40** ms | 25.05 ms | 30.07 ms | 4100.33 | 38687.33 | 5.89 Mb |
| javascript (12.11)| iotjs-express (0.0) | **26.81** ms | 13.65 ms | 26.05 ms | 82574.00 | 63114.33 | 17.00 Mb |
| javascript (12.11)| fastify (2.8) | **27.03** ms | 14.27 ms | 26.15 ms | 79293.00 | 63613.00 | 11.15 Mb |
| java (8)| spring-boot (2.1) | **30.14** ms | 14.12 ms | 29.57 ms | 105456.67 | 56059.33 | 2.94 Mb |
| javascript (12.11)| restify (8.4) | **30.88** ms | 20.24 ms | 38.33 ms | 60835.67 | 41590.67 | 4.84 Mb |
| scala (2.12)| http4s (0.18) | **34.19** ms | 18.32 ms | 38.95 ms | 103118.33 | 47927.33 | 5.57 Mb |
| swift (5.1)| kitura-nio (2.8) | **34.33** ms | 20.34 ms | 22.48 ms | 108266.33 | 47764.33 | 5.89 Mb |
| python (3.7)| responder (2.0) | **34.83** ms | 32.27 ms | 54.52 ms | 16751.33 | 28492.33 | 4.12 Mb |
| ruby (2.6)| rails (6.0) | **34.87** ms | 2.87 ms | 114.52 ms | 65358.67 | 3674.33 | 1.53 Mb |
| python (3.7)| fastapi (0.42) | **35.33** ms | 30.50 ms | 56.59 ms | 16030.33 | 28336.67 | 4.05 Mb |
| python (3.7)| clastic (19.9) | **36.61** ms | 32.02 ms | 58.04 ms | 14360.00 | 26865.33 | 4.41 Mb |
| fsharp (7.3)| suave (2.5) | **37.29** ms | 27.08 ms | 84.41 ms | 53107.67 | 27346.67 | 3.67 Mb |
| crystal (0.31)| spider-gazelle (1.6) | **37.65** ms | 35.34 ms | 44.65 ms | 20364.67 | 26349.67 | 1.86 Mb |
| python (3.7)| molten (0.27) | **39.55** ms | 34.57 ms | 59.56 ms | 17146.33 | 25487.33 | 3.14 Mb |
| python (3.7)| flask (1.1) | **39.96** ms | 36.89 ms | 51.90 ms | 14210.33 | 24577.67 | 4.01 Mb |
| python (3.7)| aiohttp (3.6) | **41.94** ms | 36.26 ms | 70.47 ms | 18551.00 | 24005.67 | 3.61 Mb |
| javascript (12.11)| turbo_polka (2.0) | **45.27** ms | 41.65 ms | 48.96 ms | 39306.00 | 22417.67 | 1.40 Mb |
| crystal (0.31)| lucky (0.18) | **46.84** ms | 44.06 ms | 55.06 ms | 39666.33 | 22185.33 | 1.81 Mb |
| python (3.7)| sanic (19.9) | **47.60** ms | 43.04 ms | 75.80 ms | 28257.00 | 20835.33 | 2.47 Mb |
| python (3.7)| bocadillo (0.18) | **49.38** ms | 47.89 ms | 71.56 ms | 18848.33 | 20104.00 | 2.57 Mb |
| php (7.3)| basicphp (0.9) | **51.36** ms | 17.01 ms | 98.43 ms | 112766.33 | 47075.67 | 15.54 Mb |
| php (7.3)| lumen (6.2) | **51.48** ms | 17.20 ms | 106.98 ms | 107090.00 | 46039.00 | 15.17 Mb |
| php (7.3)| slim (4.3) | **51.73** ms | 17.13 ms | 100.85 ms | 108530.00 | 46381.00 | 15.27 Mb |
| php (7.3)| zend-expressive (3.2) | **52.99** ms | 17.33 ms | 109.95 ms | 116465.33 | 45964.33 | 15.14 Mb |
| java (8)| micronaut (1.2) | **53.23** ms | 29.03 ms | 97.54 ms | 105528.33 | 23667.00 | 3.18 Mb |
| clojure (1.10)| coast (1.0) | **53.50** ms | 17.77 ms | 20.66 ms | 204040.67 | 49221.33 | 5.87 Mb |
| php (7.3)| spiral (2.3) | **54.02** ms | 53.79 ms | 59.58 ms | 9046.67 | 18009.67 | 2.08 Mb |
| php (7.3)| symfony (4.3) | **55.37** ms | 17.40 ms | 104.30 ms | 125376.00 | 45390.00 | 14.95 Mb |
| php (7.3)| zend-framework (3.1) | **58.14** ms | 18.32 ms | 113.95 ms | 129344.00 | 44123.67 | 14.53 Mb |
| javascript (12.11)| moleculer (0.13) | **62.03** ms | 27.04 ms | 56.40 ms | 169718.33 | 30485.67 | 3.48 Mb |
| crystal (0.31)| athena (0.7) | **72.09** ms | 50.03 ms | 195.39 ms | 91590.67 | 23579.67 | 1.96 Mb |
| php (7.3)| laravel (6.4) | **77.54** ms | 21.21 ms | 151.51 ms | 182594.00 | 38199.33 | 12.64 Mb |
| python (3.7)| quart (0.10) | **83.51** ms | 79.45 ms | 139.13 ms | 38185.67 | 11830.33 | 1.57 Mb |
| javascript (12.11)| hapi (18.4) | **86.88** ms | 26.27 ms | 59.72 ms | 264424.67 | 31128.67 | 5.36 Mb |
| python (3.7)| cherrypy (18.3) | **89.87** ms | 75.35 ms | 76.31 ms | 230917.67 | 1361.67 | 0.21 Mb |
| go (1.13)| gramework (1.6) | **95.15** ms | 97.28 ms | 101.59 ms | 18567.33 | 10162.67 | 1.73 Mb |
| python (3.7)| tornado (5.1) | **97.56** ms | 94.89 ms | 119.61 ms | 40669.67 | 9998.00 | 1.96 Mb |
| python (3.7)| django (2.2) | **100.44** ms | 88.94 ms | 175.67 ms | 38222.33 | 9654.33 | 1.86 Mb |
| python (3.7)| masonite (2.2) | **129.28** ms | 118.44 ms | 196.35 ms | 42357.00 | 7482.67 | 1.22 Mb |
| java (8)| javalin (3.5) | **173.73** ms | 11.39 ms | 633.07 ms | 471772.33 | 60889.67 | 7.21 Mb |
| scala (2.12)| akkahttp (10.1) | **191.56** ms | 7.02 ms | 20.05 ms | 832934.00 | 67181.33 | 9.63 Mb |
| crystal (0.31)| onyx (0.5) | **195.55** ms | 197.18 ms | 225.99 ms | 28366.33 | 5019.00 | 0.86 Mb |
| perl (5.3)| dancer2 (2.0) | **336.18** ms | 93.02 ms | 750.95 ms | 761405.67 | 1381.33 | 0.21 Mb |
| python (3.7)| cyclone (1.3) | **361.28** ms | 318.37 ms | 396.98 ms | 405069.00 | 2314.67 | 0.39 Mb |
| python (3.7)| klein (19.6) | **508.19** ms | 447.31 ms | 503.18 ms | 636182.00 | 1493.67 | 0.21 Mb |
| python (3.7)| nameko (2.12) | **625.89** ms | 557.20 ms | 611.19 ms | 626940.00 | 1363.33 | 0.19 Mb |

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

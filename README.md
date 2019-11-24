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
| rust (1.38)| nickel (0.11) | **0.11** ms | 0.09 ms | 0.16 ms | 106.33 | 129684.33 | 17.12 Mb |
| rust (1.38)| iron (0.6) | **1.85** ms | 1.83 ms | 2.79 ms | 820.00 | 52571.00 | 4.33 Mb |
| ruby (2.6)| syro (3.1) | **2.66** ms | 2.01 ms | 6.43 ms | 2792.33 | 71570.67 | 2.74 Mb |
| ruby (2.6)| roda (3.25) | **2.79** ms | 1.91 ms | 6.89 ms | 3067.00 | 68264.00 | 4.32 Mb |
| c (11)| agoo-c (0.7) | **2.94** ms | 2.16 ms | 6.02 ms | 3479.00 | 346152.67 | 13.26 Mb |
| ruby (2.6)| cuba (3.9) | **3.06** ms | 2.00 ms | 7.70 ms | 3453.67 | 62127.67 | 4.87 Mb |
| javascript (12.13)| sifrr (0.0) | **3.21** ms | 1.80 ms | 7.64 ms | 3673.00 | 374515.67 | 21.85 Mb |
| go (1.13)| fasthttp (1.5) | **3.29** ms | 2.32 ms | 5.71 ms | 7556.67 | 319722.00 | 34.16 Mb |
| go (1.13)| fasthttprouter (0.1) | **3.37** ms | 2.44 ms | 5.81 ms | 5888.00 | 303746.33 | 32.37 Mb |
| go (1.13)| gorouter-fasthttp (4.2) | **3.40** ms | 2.52 ms | 5.95 ms | 5166.00 | 292931.33 | 31.11 Mb |
| nim (1.0)| httpbeast (0.2) | **3.46** ms | 1.92 ms | 8.47 ms | 4064.00 | 370934.00 | 35.00 Mb |
| javascript (12.13)| nanoexpress (1.1) | **3.50** ms | 1.91 ms | 8.43 ms | 4036.33 | 358833.33 | 20.91 Mb |
| ruby (2.6)| rack-routing (0.0) | **3.51** ms | 2.39 ms | 8.64 ms | 3795.33 | 54780.00 | 2.10 Mb |
| c (11)| kore (3.3) | **3.59** ms | 3.16 ms | 6.86 ms | 2840.33 | 281806.33 | 50.80 Mb |
| go (1.13)| atreugo (9.0) | **3.83** ms | 2.81 ms | 6.64 ms | 6266.67 | 262730.00 | 35.03 Mb |
| crystal (0.31)| router.cr (0.2) | **3.92** ms | 2.90 ms | 7.78 ms | 3266.00 | 267398.33 | 16.66 Mb |
| java (8)| rapidoid (5.5) | **3.97** ms | 2.13 ms | 8.35 ms | 10048.67 | 333666.67 | 39.80 Mb |
| crystal (0.31)| kemal (0.28) | **4.20** ms | 3.28 ms | 8.18 ms | 3374.67 | 247241.67 | 26.78 Mb |
| crystal (0.31)| toro (0.4) | **4.26** ms | 3.45 ms | 8.43 ms | 3553.00 | 242021.67 | 15.08 Mb |
| crystal (0.31)| spider-gazelle (2.0) | **4.35** ms | 3.99 ms | 8.04 ms | 3230.67 | 227841.67 | 16.05 Mb |
| crystal (0.31)| raze (0.3) | **4.48** ms | 3.58 ms | 8.90 ms | 3706.00 | 234002.67 | 14.58 Mb |
| nim (1.0)| jester (0.4) | **4.50** ms | 3.33 ms | 8.81 ms | 3811.67 | 272051.67 | 36.24 Mb |
| ruby (2.6)| camping (2.1) | **4.63** ms | 3.76 ms | 11.00 ms | 4787.33 | 41261.00 | 2.61 Mb |
| crystal (0.31)| orion (1.7) | **4.77** ms | 4.32 ms | 8.14 ms | 3228.33 | 209908.33 | 22.74 Mb |
| javascript (12.13)| nanoexpress-pro (1.4) | **5.01** ms | 3.20 ms | 11.15 ms | 5987.33 | 257511.00 | 15.03 Mb |
| ruby (2.6)| flame (4.18) | **5.01** ms | 2.85 ms | 12.89 ms | 5827.67 | 38284.33 | 1.47 Mb |
| crystal (0.31)| amber (0.3) | **5.15** ms | 4.46 ms | 9.68 ms | 3873.67 | 198337.33 | 24.06 Mb |
| java (8)| act (1.8) | **5.25** ms | 3.41 ms | 10.41 ms | 7021.00 | 251327.67 | 28.77 Mb |
| cpp (11)| drogon (1.0) | **5.31** ms | 5.03 ms | 7.54 ms | 2695.00 | 199499.67 | 12.82 Mb |
| go (1.13)| kami (2.2) | **5.43** ms | 4.43 ms | 9.74 ms | 4576.67 | 191234.00 | 16.84 Mb |
| go (1.13)| gorouter (4.2) | **5.51** ms | 4.07 ms | 9.94 ms | 9616.33 | 204091.00 | 17.98 Mb |
| go (1.13)| goroute (0.0) | **5.98** ms | 4.30 ms | 12.59 ms | 7462.33 | 188845.33 | 21.97 Mb |
| javascript (12.13)| 0http (1.2) | **5.98** ms | 5.06 ms | 9.33 ms | 4859.33 | 162015.67 | 16.09 Mb |
| go (1.13)| violetear (7.0) | **5.99** ms | 4.45 ms | 9.35 ms | 13142.33 | 189910.33 | 16.71 Mb |
| go (1.13)| gin (1.4) | **6.07** ms | 4.33 ms | 12.08 ms | 9226.67 | 187249.00 | 21.78 Mb |
| javascript (12.13)| polkadot (1.0) | **6.17** ms | 5.01 ms | 9.36 ms | 8282.67 | 166136.33 | 16.50 Mb |
| ruby (2.6)| agoo (2.11) | **6.33** ms | 4.17 ms | 10.15 ms | 11059.33 | 210563.33 | 8.07 Mb |
| go (1.13)| rte (0.0) | **6.37** ms | 4.29 ms | 12.01 ms | 13569.00 | 187254.67 | 16.60 Mb |
| swift (5.1)| swifter (1.4) | **6.61** ms | 0.81 ms | 14.50 ms | 66244.67 | 22398.67 | 1.91 Mb |
| javascript (12.13)| restana (3.3) | **6.68** ms | 5.21 ms | 9.59 ms | 9906.00 | 156472.67 | 15.54 Mb |
| swift (5.1)| perfect (3.1) | **6.71** ms | 6.60 ms | 9.40 ms | 2712.00 | 142859.00 | 8.90 Mb |
| go (1.13)| webgo (3.0) | **6.71** ms | 4.56 ms | 10.98 ms | 15969.00 | 181104.33 | 15.96 Mb |
| go (1.13)| beego (1.12) | **6.86** ms | 4.36 ms | 12.81 ms | 16054.33 | 187993.33 | 16.72 Mb |
| ruby (2.6)| hanami (1.3) | **6.86** ms | 5.36 ms | 16.41 ms | 7275.33 | 28056.33 | 14.09 Mb |
| go (1.13)| echo (4.1) | **6.88** ms | 4.30 ms | 12.38 ms | 17712.00 | 188393.67 | 21.92 Mb |
| go (1.13)| chi (4.0) | **6.93** ms | 4.18 ms | 11.06 ms | 21494.33 | 197139.33 | 17.45 Mb |
| javascript (12.13)| polka (0.5) | **7.04** ms | 5.28 ms | 9.96 ms | 11190.33 | 151434.67 | 15.04 Mb |
| csharp (7.3)| aspnetcore (3.0) | **7.12** ms | 4.94 ms | 9.36 ms | 21502.00 | 171131.67 | 18.48 Mb |
| rust (1.38)| actix-web (1.0) | **7.17** ms | 6.65 ms | 9.88 ms | 3697.67 | 173972.67 | 17.23 Mb |
| javascript (12.13)| rayo (1.3) | **7.17** ms | 5.37 ms | 10.10 ms | 11158.33 | 148005.67 | 14.70 Mb |
| ruby (2.6)| sinatra (2.0) | **7.28** ms | 5.45 ms | 17.72 ms | 7794.00 | 26405.33 | 4.55 Mb |
| go (1.13)| air (0.13) | **7.32** ms | 4.62 ms | 16.55 ms | 11880.00 | 168121.67 | 23.25 Mb |
| go (1.13)| gorilla-mux (1.7) | **7.33** ms | 4.70 ms | 17.26 ms | 9459.67 | 166546.67 | 14.73 Mb |
| rust (1.38)| gotham (0.4) | **7.43** ms | 6.52 ms | 10.29 ms | 11808.00 | 144747.67 | 19.46 Mb |
| javascript (12.13)| muneem (2.4) | **8.26** ms | 6.35 ms | 11.55 ms | 14625.33 | 133200.33 | 13.23 Mb |
| python (3.7)| falcon (2.0) | **8.29** ms | 5.72 ms | 16.73 ms | 7122.00 | 128875.00 | 20.02 Mb |
| go (1.13)| gf (1.9) | **8.44** ms | 5.43 ms | 18.77 ms | 10945.00 | 141384.67 | 15.90 Mb |
| javascript (12.13)| foxify (0.1) | **8.68** ms | 7.39 ms | 12.11 ms | 13499.33 | 123608.67 | 17.22 Mb |
| cpp (11)| evhtp (1.2) | **9.13** ms | 8.78 ms | 12.31 ms | 5131.67 | 109277.67 | 7.03 Mb |
| javascript (12.13)| koa (2.11) | **9.59** ms | 8.38 ms | 13.96 ms | 12074.00 | 108910.33 | 15.28 Mb |
| python (3.7)| bottle (0.12) | **9.60** ms | 8.14 ms | 17.55 ms | 7829.00 | 108113.33 | 17.65 Mb |
| javascript (12.13)| iotjs-express (0.0) | **9.85** ms | 7.78 ms | 13.90 ms | 14708.67 | 110450.00 | 29.75 Mb |
| python (3.7)| asgineer (0.7) | **10.45** ms | 9.47 ms | 16.67 ms | 4829.00 | 94322.33 | 11.16 Mb |
| javascript (12.13)| fastify (2.1) | **10.57** ms | 8.48 ms | 15.05 ms | 15774.00 | 109475.67 | 18.79 Mb |
| php (7.3)| one (1.8) | **10.63** ms | 9.44 ms | 19.52 ms | 6520.00 | 93908.33 | 14.30 Mb |
| ruby (2.6)| grape (1.2) | **10.92** ms | 8.32 ms | 26.51 ms | 11435.67 | 17767.00 | 0.67 Mb |
| swift (5.1)| vapor (3.3) | **10.94** ms | 8.45 ms | 15.28 ms | 23015.67 | 101248.33 | 11.22 Mb |
| ruby (2.6)| plezi (0.16) | **10.96** ms | 10.04 ms | 14.92 ms | 6750.00 | 89792.33 | 12.69 Mb |
| javascript (12.13)| express (4.17) | **11.04** ms | 8.63 ms | 16.00 ms | 14999.67 | 96470.00 | 15.66 Mb |
| python (3.7)| blacksheep (0.2) | **11.17** ms | 10.35 ms | 17.92 ms | 5063.67 | 88148.67 | 11.75 Mb |
| php (7.3)| hyperf (1.0) | **11.56** ms | 9.30 ms | 23.65 ms | 8880.33 | 92642.33 | 13.09 Mb |
| go (1.13)| mars (1.0) | **11.95** ms | 5.24 ms | 33.52 ms | 14371.67 | 126562.00 | 18.90 Mb |
| javascript (12.13)| restify (8.4) | **12.03** ms | 11.30 ms | 14.69 ms | 9679.67 | 84400.00 | 9.82 Mb |
| python (3.7)| hug (2.6) | **12.10** ms | 10.45 ms | 20.18 ms | 7752.67 | 83268.33 | 13.68 Mb |
| java (8)| spring-boot (2.1) | **12.35** ms | 7.67 ms | 12.75 ms | 41616.00 | 112067.33 | 5.81 Mb |
| php (7.3)| sw-fw-less (preview) | **13.34** ms | 11.93 ms | 24.36 ms | 7881.33 | 74074.33 | 11.28 Mb |
| fsharp (7.3)| suave (2.5) | **14.05** ms | 12.80 ms | 18.96 ms | 13779.67 | 73557.33 | 9.88 Mb |
| python (3.7)| starlette (0.13) | **14.85** ms | 12.93 ms | 25.97 ms | 8302.33 | 68198.33 | 9.74 Mb |
| php (7.3)| imi (1.0) | **15.21** ms | 15.01 ms | 19.56 ms | 3964.67 | 63882.67 | 9.73 Mb |
| php (7.3)| swoft (2.0) | **15.62** ms | 15.10 ms | 20.61 ms | 4469.67 | 62411.33 | 10.88 Mb |
| swift (5.1)| kitura (2.8) | **15.63** ms | 14.30 ms | 15.11 ms | 26236.67 | 68630.00 | 8.47 Mb |
| swift (5.1)| kitura-nio (2.8) | **15.97** ms | 13.92 ms | 14.62 ms | 32892.33 | 70284.00 | 8.67 Mb |
| python (3.7)| responder (2.0) | **20.15** ms | 19.34 ms | 31.25 ms | 8373.00 | 49135.33 | 7.11 Mb |
| javascript (12.13)| moleculer (0.13) | **20.24** ms | 14.79 ms | 30.24 ms | 26263.67 | 54307.33 | 6.20 Mb |
| python (3.7)| fastapi (0.42) | **20.63** ms | 18.43 ms | 32.97 ms | 9400.67 | 48578.33 | 6.96 Mb |
| python (3.7)| clastic (19.9) | **24.13** ms | 20.10 ms | 38.97 ms | 13049.00 | 41212.00 | 6.77 Mb |
| python (3.7)| molten (0.27) | **24.42** ms | 19.93 ms | 39.67 ms | 19457.33 | 42928.00 | 5.29 Mb |
| javascript (12.13)| hapi (18.4) | **24.56** ms | 16.85 ms | 30.48 ms | 49739.33 | 55023.00 | 9.48 Mb |
| python (3.7)| flask (1.1) | **25.45** ms | 19.86 ms | 44.25 ms | 16418.67 | 40636.67 | 6.64 Mb |
| crystal (0.31)| lucky (0.18) | **25.65** ms | 21.08 ms | 25.53 ms | 40124.33 | 44193.33 | 3.60 Mb |
| kotlin (1.3)| ktor (1.2) | **26.91** ms | 5.15 ms | 22.90 ms | 111029.00 | 139475.00 | 14.41 Mb |
| php (7.3)| basicphp (0.9) | **27.08** ms | 8.20 ms | 48.25 ms | 62373.00 | 96253.33 | 28.90 Mb |
| php (7.3)| slim (4.3) | **27.19** ms | 8.15 ms | 50.95 ms | 66550.33 | 97292.67 | 29.13 Mb |
| php (7.3)| lumen (6.2) | **27.30** ms | 8.55 ms | 52.20 ms | 60826.67 | 92872.00 | 27.83 Mb |
| python (3.7)| aiohttp (3.6) | **28.41** ms | 25.84 ms | 46.43 ms | 12904.67 | 35445.00 | 5.33 Mb |
| php (7.3)| zend-expressive (3.2) | **28.53** ms | 8.07 ms | 53.81 ms | 73696.00 | 97506.67 | 29.20 Mb |
| php (7.3)| spiral (2.3) | **28.78** ms | 28.47 ms | 31.76 ms | 8889.67 | 34023.00 | 3.92 Mb |
| crystal (0.31)| athena (0.7) | **29.04** ms | 24.27 ms | 71.63 ms | 32449.33 | 46597.67 | 3.87 Mb |
| php (7.3)| symfony (4.3) | **29.21** ms | 8.63 ms | 54.52 ms | 68885.33 | 91255.67 | 27.34 Mb |
| javascript (12.13)| turbo_polka (0.3) | **30.43** ms | 26.76 ms | 28.90 ms | 46947.67 | 36104.00 | 2.25 Mb |
| python (3.7)| sanic (19.9) | **30.72** ms | 26.64 ms | 56.44 ms | 19921.67 | 33803.00 | 4.00 Mb |
| php (7.3)| zend-framework (3.1) | **31.11** ms | 8.57 ms | 55.20 ms | 77009.33 | 92357.67 | 27.66 Mb |
| python (3.7)| bocadillo (0.18) | **32.68** ms | 29.87 ms | 52.27 ms | 16644.67 | 31183.33 | 4.00 Mb |
| ruby (2.6)| rails (6.0) | **35.08** ms | 2.57 ms | 114.44 ms | 65662.67 | 5638.00 | 2.36 Mb |
| java (8)| micronaut (1.2) | **36.38** ms | 9.50 ms | 45.05 ms | 148025.00 | 76794.33 | 10.36 Mb |
| go (1.13)| gramework (1.7) | **41.92** ms | 43.03 ms | 44.54 ms | 7110.33 | 23061.67 | 3.92 Mb |
| scala (2.12)| http4s (0.18) | **42.67** ms | 6.94 ms | 21.97 ms | 206351.00 | 99378.33 | 11.56 Mb |
| php (7.3)| laravel (6.5) | **45.86** ms | 8.94 ms | 74.00 ms | 123348.00 | 86311.00 | 25.95 Mb |
| clojure (1.10)| coast (1.0) | **49.75** ms | 11.50 ms | 12.53 ms | 213250.33 | 83565.67 | 9.97 Mb |
| python (3.7)| quart (0.10) | **55.03** ms | 48.92 ms | 92.22 ms | 27708.67 | 17943.67 | 2.37 Mb |
| java (8)| javalin (3.5) | **56.28** ms | 4.75 ms | 36.53 ms | 264492.67 | 156423.33 | 18.51 Mb |
| python (3.7)| django (2.2) | **70.06** ms | 58.77 ms | 113.33 ms | 33588.00 | 14195.00 | 2.73 Mb |
| python (3.7)| cherrypy (18.3) | **75.69** ms | 65.25 ms | 123.65 ms | 31149.33 | 12954.00 | 2.30 Mb |
| python (3.7)| tornado (5.1) | **76.02** ms | 74.50 ms | 93.37 ms | 31998.33 | 12770.00 | 2.50 Mb |
| python (3.7)| masonite (2.2) | **95.15** ms | 77.75 ms | 166.46 ms | 44411.67 | 10269.33 | 1.68 Mb |
| crystal (0.31)| onyx (0.5) | **163.28** ms | 163.45 ms | 186.42 ms | 21386.67 | 6034.33 | 1.03 Mb |
| scala (2.12)| akkahttp (10.1) | **176.28** ms | 4.37 ms | 21.00 ms | 731364.00 | 144620.00 | 20.73 Mb |
| perl (5.3)| dancer2 (2.0) | **208.64** ms | 27.14 ms | 256.54 ms | 642018.00 | 1971.67 | 0.30 Mb |
| julia (1.3)| merly (0.2) | **288.77** ms | 108.34 ms | 319.92 ms | 762685.67 | 7370.67 | 0.59 Mb |
| python (3.7)| cyclone (1.3) | **297.15** ms | 235.52 ms | 290.51 ms | 496982.33 | 3316.00 | 0.56 Mb |
| python (3.7)| klein (19.6) | **406.39** ms | 342.25 ms | 396.45 ms | 559844.00 | 2206.67 | 0.32 Mb |
| python (3.7)| nameko (2.12) | **491.48** ms | 396.63 ms | 432.41 ms | 719762.67 | 2026.00 | 0.28 Mb |

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

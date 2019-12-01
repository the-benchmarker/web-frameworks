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
| rust (1.38)| nickel (0.11) | **0.10** ms | 0.09 ms | 0.15 ms | 78.67 | 132618.67 | 17.51 Mb |
| rust (1.38)| iron (0.6) | **1.89** ms | 1.87 ms | 2.87 ms | 812.33 | 51864.00 | 4.27 Mb |
| ruby (2.6)| roda (3.25) | **2.65** ms | 1.52 ms | 6.86 ms | 3081.67 | 71534.67 | 4.53 Mb |
| ruby (2.6)| syro (3.1) | **2.67** ms | 2.03 ms | 6.50 ms | 2839.00 | 71136.67 | 2.73 Mb |
| ruby (2.6)| cuba (3.9) | **2.93** ms | 1.75 ms | 7.45 ms | 3315.33 | 65276.67 | 5.11 Mb |
| c (11)| agoo-c (0.7) | **3.02** ms | 2.07 ms | 6.33 ms | 3793.00 | 363620.33 | 13.93 Mb |
| go (1.13)| fasthttprouter (0.1) | **3.22** ms | 2.48 ms | 5.89 ms | 3096.33 | 297910.00 | 31.75 Mb |
| nim (1.0)| httpbeast (0.2) | **3.37** ms | 2.08 ms | 7.97 ms | 3784.67 | 359413.33 | 33.91 Mb |
| javascript (12.13)| sifrr (0.0) | **3.51** ms | 1.80 ms | 8.74 ms | 4166.33 | 360178.00 | 21.01 Mb |
| c (11)| kore (3.3) | **3.53** ms | 3.11 ms | 6.95 ms | 2570.00 | 291551.33 | 52.55 Mb |
| ruby (2.6)| rack-routing (0.0) | **3.54** ms | 2.76 ms | 8.58 ms | 3739.33 | 53991.00 | 2.07 Mb |
| go (1.13)| atreugo (9.0) | **3.57** ms | 2.52 ms | 5.94 ms | 7174.00 | 294669.33 | 39.29 Mb |
| javascript (12.13)| nanoexpress (1.1) | **3.62** ms | 2.04 ms | 8.62 ms | 4147.33 | 342102.67 | 19.94 Mb |
| go (1.13)| fasthttp (1.5) | **3.66** ms | 2.75 ms | 6.68 ms | 3974.00 | 267290.67 | 28.56 Mb |
| crystal (0.31)| router.cr (0.2) | **3.94** ms | 2.98 ms | 7.90 ms | 3256.00 | 264668.00 | 16.50 Mb |
| crystal (0.31)| toro (0.4) | **3.95** ms | 2.90 ms | 8.05 ms | 3335.33 | 266132.67 | 16.59 Mb |
| crystal (0.31)| raze (0.3) | **3.96** ms | 3.28 ms | 7.44 ms | 3041.33 | 255219.67 | 15.90 Mb |
| crystal (0.31)| spider-gazelle (2.1) | **4.08** ms | 3.20 ms | 7.95 ms | 3282.33 | 253077.33 | 17.83 Mb |
| go (1.13)| gorouter-fasthttp (4.2) | **4.15** ms | 2.55 ms | 5.99 ms | 13513.67 | 289312.00 | 30.73 Mb |
| java (8)| rapidoid (5.5) | **4.17** ms | 2.20 ms | 9.56 ms | 7648.00 | 316162.67 | 37.72 Mb |
| crystal (0.31)| kemal (0.28) | **4.21** ms | 3.39 ms | 8.12 ms | 3298.33 | 243558.67 | 26.38 Mb |
| ruby (2.6)| camping (2.1) | **4.37** ms | 3.57 ms | 10.45 ms | 4493.00 | 44067.00 | 2.79 Mb |
| nim (1.0)| jester (0.4) | **4.38** ms | 3.49 ms | 8.20 ms | 3357.67 | 262984.67 | 35.03 Mb |
| crystal (0.31)| amber (0.3) | **4.57** ms | 3.79 ms | 8.51 ms | 3464.00 | 225234.67 | 27.33 Mb |
| crystal (0.31)| orion (1.7) | **4.86** ms | 4.34 ms | 8.47 ms | 3392.33 | 207652.33 | 22.49 Mb |
| javascript (12.13)| nanoexpress-pro (1.4) | **5.02** ms | 3.57 ms | 10.88 ms | 5603.00 | 244360.00 | 14.26 Mb |
| ruby (2.6)| flame (4.18) | **5.19** ms | 4.15 ms | 12.16 ms | 5194.33 | 37010.33 | 1.42 Mb |
| go (1.13)| aero (1.3) | **5.22** ms | 4.37 ms | 8.98 ms | 4443.00 | 193598.00 | 17.16 Mb |
| go (1.13)| rte (0.0) | **5.27** ms | 4.14 ms | 9.92 ms | 5019.33 | 202377.33 | 17.94 Mb |
| ruby (2.6)| agoo (2.11) | **5.42** ms | 3.88 ms | 8.47 ms | 9442.00 | 233379.33 | 8.95 Mb |
| cpp (11)| drogon (1.0) | **5.45** ms | 5.09 ms | 7.76 ms | 3370.00 | 195273.67 | 12.55 Mb |
| go (1.13)| violetear (7.0) | **5.46** ms | 4.49 ms | 9.43 ms | 5595.33 | 187408.67 | 16.48 Mb |
| java (8)| act (1.8) | **5.76** ms | 3.67 ms | 10.72 ms | 10805.33 | 240083.00 | 27.48 Mb |
| go (1.13)| echo (4.1) | **6.09** ms | 4.42 ms | 12.59 ms | 7828.33 | 185427.33 | 21.57 Mb |
| javascript (12.13)| 0http (1.2) | **6.21** ms | 5.12 ms | 9.59 ms | 6044.00 | 157919.33 | 15.68 Mb |
| go (1.13)| kami (2.2) | **6.31** ms | 4.49 ms | 10.45 ms | 13454.67 | 185862.00 | 16.37 Mb |
| javascript (12.13)| polkadot (1.0) | **6.32** ms | 5.08 ms | 9.62 ms | 7653.33 | 160646.67 | 15.95 Mb |
| go (1.13)| webgo (3.0) | **6.55** ms | 4.80 ms | 13.12 ms | 6856.67 | 167223.67 | 14.77 Mb |
| javascript (12.13)| restana (3.3) | **6.62** ms | 5.25 ms | 9.76 ms | 7930.00 | 152156.00 | 15.11 Mb |
| ruby (2.6)| hanami (1.3) | **6.71** ms | 4.92 ms | 16.36 ms | 7335.67 | 28612.00 | 14.37 Mb |
| go (1.13)| gorouter (4.2) | **6.88** ms | 4.55 ms | 13.61 ms | 13082.67 | 177399.33 | 15.64 Mb |
| go (1.13)| gin (1.5) | **6.98** ms | 4.38 ms | 12.61 ms | 18403.67 | 184850.00 | 21.50 Mb |
| rust (1.38)| actix-web (1.0) | **7.10** ms | 6.61 ms | 9.89 ms | 3177.00 | 171049.00 | 16.88 Mb |
| go (1.13)| gorilla-mux (1.7) | **7.20** ms | 4.41 ms | 14.61 ms | 16233.33 | 181183.00 | 16.01 Mb |
| go (1.13)| beego (1.12) | **7.21** ms | 4.83 ms | 16.19 ms | 8334.00 | 163353.00 | 14.53 Mb |
| rust (1.38)| gotham (0.4) | **7.23** ms | 6.62 ms | 11.04 ms | 7806.33 | 141493.67 | 19.03 Mb |
| javascript (12.13)| rayo (1.3) | **7.26** ms | 5.49 ms | 10.37 ms | 10454.00 | 144337.00 | 14.33 Mb |
| go (1.13)| chi (4.0) | **7.28** ms | 4.49 ms | 13.89 ms | 17381.67 | 178228.33 | 15.69 Mb |
| ruby (2.6)| sinatra (2.0) | **7.38** ms | 5.80 ms | 17.52 ms | 7633.33 | 26090.33 | 4.50 Mb |
| javascript (12.13)| polka (0.5) | **7.38** ms | 5.41 ms | 10.28 ms | 12524.33 | 146840.33 | 14.58 Mb |
| swift (5.1)| perfect (3.1) | **7.46** ms | 7.47 ms | 10.09 ms | 2240.33 | 128707.00 | 8.02 Mb |
| swift (5.1)| swifter (1.4) | **7.50** ms | 0.86 ms | 14.55 ms | 90325.33 | 21765.00 | 1.85 Mb |
| python (3.7)| falcon (2.0) | **7.74** ms | 5.87 ms | 13.55 ms | 5564.33 | 129842.00 | 20.17 Mb |
| go (1.13)| goroute (0.0) | **8.01** ms | 4.34 ms | 13.27 ms | 26886.67 | 186241.33 | 21.67 Mb |
| javascript (12.13)| muneem (2.4) | **8.24** ms | 7.08 ms | 11.85 ms | 12134.67 | 129073.67 | 12.82 Mb |
| javascript (12.13)| foxify (0.1) | **8.49** ms | 7.41 ms | 12.62 ms | 9901.33 | 119999.67 | 16.72 Mb |
| go (1.13)| air (0.13) | **8.70** ms | 4.83 ms | 19.55 ms | 18161.67 | 157956.33 | 21.85 Mb |
| csharp (7.3)| aspnetcore (3.0) | **8.95** ms | 4.58 ms | 8.35 ms | 37493.67 | 184671.00 | 19.94 Mb |
| go (1.13)| gf (1.9) | **9.01** ms | 5.12 ms | 15.26 ms | 26855.00 | 153732.33 | 17.29 Mb |
| cpp (11)| evhtp (1.2) | **9.11** ms | 8.68 ms | 11.92 ms | 6176.00 | 110993.33 | 7.14 Mb |
| ruby (2.6)| grape (1.2) | **9.17** ms | 5.88 ms | 22.35 ms | 9763.33 | 21192.00 | 0.80 Mb |
| python (3.7)| bottle (0.12) | **9.51** ms | 8.24 ms | 16.37 ms | 6901.67 | 107071.33 | 17.48 Mb |
| javascript (12.13)| koa (2.11) | **9.78** ms | 8.50 ms | 14.30 ms | 11829.00 | 105495.33 | 14.80 Mb |
| ruby (2.6)| plezi (0.16) | **10.01** ms | 9.50 ms | 13.80 ms | 4500.67 | 97621.67 | 13.80 Mb |
| javascript (12.13)| iotjs-express (0.0) | **10.05** ms | 8.05 ms | 14.61 ms | 13025.67 | 106623.00 | 28.72 Mb |
| python (3.7)| asgineer (0.7) | **10.33** ms | 9.40 ms | 17.10 ms | 4834.33 | 94855.67 | 11.22 Mb |
| php (7.3)| one (1.8) | **10.66** ms | 9.50 ms | 19.49 ms | 6639.67 | 93567.67 | 14.25 Mb |
| javascript (12.13)| fastify (2.1) | **10.97** ms | 8.67 ms | 15.60 ms | 15993.67 | 105790.00 | 18.15 Mb |
| python (3.7)| blacksheep (0.2) | **11.02** ms | 10.30 ms | 16.48 ms | 4459.00 | 89138.00 | 11.88 Mb |
| javascript (12.13)| express (4.17) | **11.35** ms | 8.79 ms | 16.29 ms | 15402.33 | 94017.00 | 15.26 Mb |
| java (8)| spring-boot (2.1) | **11.86** ms | 8.09 ms | 13.73 ms | 31523.33 | 103065.67 | 5.39 Mb |
| go (1.13)| mars (1.0) | **11.96** ms | 5.31 ms | 33.38 ms | 14773.67 | 125281.33 | 18.71 Mb |
| python (3.7)| hug (2.6) | **12.36** ms | 10.59 ms | 22.25 ms | 8754.33 | 82339.00 | 13.53 Mb |
| javascript (12.13)| restify (8.4) | **12.36** ms | 11.48 ms | 15.39 ms | 9404.67 | 82162.00 | 9.56 Mb |
| php (7.3)| sw-fw-less (preview) | **13.10** ms | 11.71 ms | 24.06 ms | 7572.00 | 75711.67 | 11.53 Mb |
| php (7.3)| hyperf (1.0) | **13.18** ms | 9.07 ms | 28.65 ms | 13070.67 | 90930.33 | 12.85 Mb |
| python (3.7)| starlette (0.13) | **13.71** ms | 13.34 ms | 21.71 ms | 6392.33 | 71914.67 | 10.27 Mb |
| clojure (1.10)| coast (1.0) | **14.07** ms | 11.73 ms | 12.89 ms | 34464.00 | 83103.67 | 9.91 Mb |
| swift (5.1)| vapor (3.3) | **14.21** ms | 8.88 ms | 16.44 ms | 48030.00 | 97028.00 | 10.76 Mb |
| fsharp (7.3)| suave (2.5) | **15.08** ms | 13.56 ms | 21.00 ms | 17058.00 | 67942.67 | 9.12 Mb |
| php (7.3)| swoft (2.0) | **15.40** ms | 15.11 ms | 19.93 ms | 3623.33 | 63367.00 | 11.05 Mb |
| php (7.3)| imi (1.0) | **15.40** ms | 15.14 ms | 19.89 ms | 3813.00 | 63010.67 | 9.60 Mb |
| swift (5.1)| kitura-nio (2.8) | **17.78** ms | 14.90 ms | 16.43 ms | 40675.00 | 65027.67 | 8.02 Mb |
| python (3.7)| responder (2.0) | **20.32** ms | 19.11 ms | 30.87 ms | 8011.33 | 48641.33 | 7.04 Mb |
| python (3.7)| fastapi (0.42) | **20.56** ms | 17.70 ms | 34.55 ms | 9251.33 | 48668.67 | 6.97 Mb |
| javascript (12.13)| moleculer (0.13) | **20.89** ms | 15.08 ms | 30.98 ms | 29241.67 | 53611.00 | 6.12 Mb |
| crystal (0.31)| lucky (0.18) | **23.10** ms | 21.98 ms | 26.51 ms | 12743.00 | 42653.67 | 3.48 Mb |
| python (3.7)| clastic (19.9) | **23.38** ms | 18.92 ms | 40.80 ms | 13093.33 | 42933.33 | 7.05 Mb |
| java (8)| micronaut (1.2) | **23.53** ms | 9.59 ms | 46.15 ms | 59561.33 | 72298.33 | 9.74 Mb |
| python (3.7)| molten (0.27) | **24.33** ms | 19.18 ms | 42.52 ms | 14809.00 | 42956.00 | 5.29 Mb |
| python (3.7)| flask (1.1) | **24.59** ms | 19.92 ms | 41.51 ms | 13672.67 | 40863.33 | 6.67 Mb |
| javascript (12.13)| hapi (18.4) | **26.03** ms | 17.31 ms | 31.46 ms | 56187.00 | 53616.00 | 9.24 Mb |
| kotlin (1.3)| ktor (1.2) | **26.73** ms | 5.59 ms | 22.30 ms | 111033.33 | 135604.33 | 14.01 Mb |
| php (7.3)| lumen (6.2) | **27.04** ms | 8.27 ms | 49.30 ms | 63628.33 | 96179.00 | 28.82 Mb |
| php (7.3)| basicphp (0.9) | **27.89** ms | 8.30 ms | 51.65 ms | 65127.33 | 94309.67 | 28.32 Mb |
| python (3.7)| aiohttp (3.6) | **28.65** ms | 26.13 ms | 43.60 ms | 12476.00 | 35161.00 | 5.29 Mb |
| php (7.3)| zend-expressive (3.2) | **29.01** ms | 8.31 ms | 54.68 ms | 73300.00 | 93923.67 | 28.13 Mb |
| php (7.3)| spiral (2.3) | **29.08** ms | 28.80 ms | 32.21 ms | 4017.00 | 33650.67 | 3.88 Mb |
| php (7.3)| slim (4.3) | **29.68** ms | 8.78 ms | 58.58 ms | 70022.33 | 89827.00 | 26.90 Mb |
| javascript (12.13)| turbo_polka (0.3) | **29.71** ms | 27.13 ms | 29.34 ms | 36319.33 | 35681.67 | 2.22 Mb |
| php (7.3)| symfony (4.3) | **29.79** ms | 8.46 ms | 61.87 ms | 72511.00 | 92723.67 | 27.78 Mb |
| python (3.7)| sanic (19.9) | **30.69** ms | 25.49 ms | 58.04 ms | 20615.00 | 34105.33 | 4.04 Mb |
| scala (2.12)| http4s (0.18) | **30.69** ms | 7.84 ms | 23.94 ms | 155570.33 | 96589.00 | 11.23 Mb |
| python (3.7)| bocadillo (0.18) | **31.14** ms | 28.07 ms | 51.17 ms | 14742.67 | 32392.33 | 4.15 Mb |
| crystal (0.31)| athena (0.7) | **31.17** ms | 26.65 ms | 76.89 ms | 36613.00 | 45170.67 | 3.75 Mb |
| swift (5.1)| kitura (2.8) | **31.91** ms | 14.99 ms | 16.67 ms | 128624.33 | 64548.67 | 7.96 Mb |
| php (7.3)| zend-framework (3.1) | **34.76** ms | 9.27 ms | 60.03 ms | 85976.67 | 84540.33 | 25.32 Mb |
| ruby (2.6)| rails (6.0) | **35.57** ms | 2.68 ms | 116.35 ms | 66636.00 | 5567.00 | 2.33 Mb |
| go (1.13)| gramework (1.7) | **42.62** ms | 43.35 ms | 44.85 ms | 6368.67 | 22856.33 | 3.88 Mb |
| php (7.3)| laravel (6.5) | **45.66** ms | 9.22 ms | 63.43 ms | 124574.67 | 83850.67 | 25.21 Mb |
| java (8)| javalin (3.5) | **49.97** ms | 5.29 ms | 44.28 ms | 218279.67 | 145789.67 | 17.25 Mb |
| python (3.7)| quart (0.10) | **53.97** ms | 51.32 ms | 89.33 ms | 25083.00 | 18259.33 | 2.42 Mb |
| python (3.7)| django (2.2) | **67.75** ms | 59.29 ms | 113.33 ms | 30259.33 | 14459.67 | 2.78 Mb |
| python (3.7)| tornado (5.1) | **72.19** ms | 70.24 ms | 86.25 ms | 31325.00 | 13543.00 | 2.66 Mb |
| python (3.7)| cherrypy (18.3) | **75.59** ms | 64.28 ms | 121.92 ms | 30564.33 | 13021.33 | 2.31 Mb |
| python (3.7)| masonite (2.2) | **92.38** ms | 75.97 ms | 148.31 ms | 46177.33 | 10757.67 | 1.76 Mb |
| perl (5.3)| dancer2 (2.0) | **118.99** ms | 26.56 ms | 281.17 ms | 339733.67 | 2310.33 | 0.35 Mb |
| scala (2.12)| akkahttp (10.1) | **149.75** ms | 5.22 ms | 43.68 ms | 610972.67 | 113145.33 | 16.22 Mb |
| julia (1.3)| merly (0.2) | **280.48** ms | 113.86 ms | 335.27 ms | 699225.33 | 7194.33 | 0.57 Mb |
| python (3.7)| cyclone (1.3) | **324.97** ms | 224.77 ms | 275.61 ms | 670687.00 | 3382.00 | 0.57 Mb |
| python (3.7)| klein (19.6) | **391.14** ms | 325.09 ms | 368.45 ms | 551849.33 | 2227.33 | 0.32 Mb |
| python (3.7)| nameko (2.12) | **473.66** ms | 388.90 ms | 425.89 ms | 677082.00 | 2039.33 | 0.29 Mb |

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

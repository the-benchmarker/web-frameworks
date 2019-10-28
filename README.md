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
| rust (1.38)| nickel (0.11) | **0.27** ms | 0.23 ms | 0.45 ms | 226.33 | 36076.33 | 4.77 Mb |
| ruby (2.6)| syro (3.1) | **2.56** ms | 0.52 ms | 7.80 ms | 4281.33 | 49467.33 | 1.90 Mb |
| ruby (2.6)| roda (3.25) | **2.70** ms | 0.62 ms | 8.02 ms | 4165.67 | 46819.67 | 2.96 Mb |
| ruby (2.6)| cuba (3.9) | **3.11** ms | 0.53 ms | 9.64 ms | 5189.00 | 41205.33 | 3.23 Mb |
| rust (1.38)| iron (0.6) | **3.11** ms | 2.98 ms | 4.57 ms | 1409.67 | 20907.33 | 1.72 Mb |
| ruby (2.6)| rack-routing (0.0) | **3.90** ms | 0.66 ms | 12.61 ms | 6775.00 | 32829.00 | 1.26 Mb |
| c (11)| agoo-c (0.7) | **4.58** ms | 4.17 ms | 8.62 ms | 3828.33 | 211105.67 | 8.09 Mb |
| node (12.11)| sifrr (0.0) | **4.88** ms | 4.31 ms | 10.11 ms | 4179.67 | 201412.67 | 11.75 Mb |
| python (3.7)| japronto (0.1) | **4.95** ms | 4.55 ms | 9.71 ms | 3710.33 | 194167.00 | 15.40 Mb |
| ruby (2.6)| camping (2.1) | **5.06** ms | 0.87 ms | 15.18 ms | 7716.33 | 25225.33 | 1.60 Mb |
| nim (1.0)| httpbeast (0.2) | **5.07** ms | 4.53 ms | 9.70 ms | 3789.00 | 191666.00 | 18.08 Mb |
| swift (5.1)| swifter (1.4) | **5.23** ms | 0.87 ms | 14.80 ms | 63964.67 | 10396.33 | 0.89 Mb |
| ruby (2.6)| flame (4.18) | **5.55** ms | 0.97 ms | 17.89 ms | 9527.33 | 23045.67 | 0.88 Mb |
| cpp (11)| drogon (1.0) | **5.56** ms | 4.84 ms | 10.29 ms | 4146.67 | 175883.33 | 11.31 Mb |
| go (1.13)| fasthttp (1.5) | **5.77** ms | 4.90 ms | 8.94 ms | 6027.33 | 168740.33 | 18.05 Mb |
| cpp (11)| evhtp (1.2) | **5.92** ms | 5.25 ms | 9.60 ms | 3010.67 | 159621.67 | 10.27 Mb |
| go (1.13)| fasthttprouter (0.1) | **5.92** ms | 5.12 ms | 9.07 ms | 5801.00 | 162778.67 | 17.38 Mb |
| ruby (2.6)| hanami (1.3) | **6.09** ms | 0.57 ms | 20.66 ms | 10620.00 | 20964.67 | 10.53 Mb |
| go (1.13)| gorouter-fasthttp (4.2) | **6.21** ms | 5.31 ms | 9.18 ms | 6662.67 | 158174.67 | 16.84 Mb |
| crystal (0.31)| router.cr (0.2) | **6.67** ms | 5.74 ms | 11.39 ms | 3661.33 | 145518.67 | 9.07 Mb |
| go (1.13)| atreugo (8.2) | **6.83** ms | 5.19 ms | 9.05 ms | 14573.00 | 161214.00 | 21.49 Mb |
| crystal (0.31)| raze (0.3) | **6.84** ms | 5.93 ms | 11.35 ms | 3540.67 | 141641.33 | 8.83 Mb |
| crystal (0.31)| toro (0.4) | **6.96** ms | 6.13 ms | 11.62 ms | 3976.33 | 138899.00 | 8.66 Mb |
| crystal (0.31)| kemal (0.28) | **7.13** ms | 6.29 ms | 11.66 ms | 3626.33 | 135786.67 | 14.71 Mb |
| c (11)| kore (3.3) | **7.31** ms | 5.62 ms | 14.36 ms | 7540.33 | 163524.67 | 29.47 Mb |
| java (8)| rapidoid (5.5) | **7.51** ms | 4.82 ms | 11.04 ms | 15532.33 | 168940.33 | 20.15 Mb |
| nim (1.0)| jester (0.4) | **7.61** ms | 6.75 ms | 12.65 ms | 4478.33 | 141561.67 | 18.86 Mb |
| crystal (0.31)| amber (0.3) | **7.72** ms | 7.00 ms | 12.40 ms | 3674.67 | 125989.00 | 15.29 Mb |
| ruby (2.6)| sinatra (2.0) | **8.10** ms | 0.96 ms | 25.67 ms | 13084.00 | 15771.33 | 2.72 Mb |
| crystal (0.31)| orion (1.7) | **8.91** ms | 8.14 ms | 14.86 ms | 4692.00 | 110267.00 | 11.94 Mb |
| java (8)| act (1.8) | **9.22** ms | 7.92 ms | 13.91 ms | 8627.33 | 117994.00 | 13.50 Mb |
| go (1.13)| rte (0.0) | **9.52** ms | 7.71 ms | 15.57 ms | 10244.00 | 108676.00 | 9.64 Mb |
| go (1.13)| gorouter (4.2) | **9.79** ms | 7.90 ms | 16.29 ms | 9850.00 | 106078.00 | 9.36 Mb |
| go (1.13)| kami (2.2) | **10.28** ms | 8.59 ms | 17.13 ms | 7828.33 | 98875.00 | 8.71 Mb |
| rust (1.38)| actix-web (1.0) | **10.31** ms | 9.74 ms | 13.85 ms | 4283.67 | 105803.33 | 10.15 Mb |
| ruby (2.6)| grape (1.2) | **10.56** ms | 1.50 ms | 31.65 ms | 15401.67 | 12185.67 | 0.47 Mb |
| go (1.13)| gin (1.4) | **10.66** ms | 8.59 ms | 18.56 ms | 8788.33 | 96860.67 | 11.27 Mb |
| go (1.13)| chi (4.0) | **10.67** ms | 8.15 ms | 18.02 ms | 14402.33 | 102553.33 | 9.10 Mb |
| go (1.13)| goroute (0.0) | **10.85** ms | 8.43 ms | 19.26 ms | 10888.67 | 97094.00 | 11.30 Mb |
| go (1.13)| beego (1.12) | **10.85** ms | 8.58 ms | 18.71 ms | 10547.00 | 97394.67 | 8.68 Mb |
| go (1.13)| gorilla-mux (1.7) | **10.92** ms | 8.62 ms | 19.88 ms | 8381.67 | 95730.00 | 8.48 Mb |
| go (1.13)| echo (4.1) | **10.95** ms | 8.43 ms | 19.43 ms | 11635.67 | 97643.67 | 11.36 Mb |
| go (1.13)| webgo (3.0) | **12.03** ms | 9.25 ms | 18.52 ms | 19058.67 | 91808.00 | 8.11 Mb |
| go (1.13)| air (0.13) | **12.62** ms | 9.51 ms | 22.75 ms | 13593.67 | 85288.00 | 11.80 Mb |
| go (1.13)| violetear (7.0) | **12.76** ms | 8.78 ms | 16.23 ms | 31899.67 | 98816.67 | 8.69 Mb |
| python (3.7)| falcon (2.0) | **13.28** ms | 11.00 ms | 22.70 ms | 8350.67 | 77016.33 | 11.96 Mb |
| go (1.13)| gf (1.9) | **13.52** ms | 10.91 ms | 22.79 ms | 12324.33 | 78299.67 | 8.81 Mb |
| php (7.3)| one (1.8) | **13.86** ms | 12.60 ms | 23.56 ms | 8367.00 | 72255.00 | 11.01 Mb |
| swift (5.1)| perfect (3.1) | **14.25** ms | 14.26 ms | 17.21 ms | 2919.00 | 67586.67 | 4.21 Mb |
| node (12.11)| restana (3.3) | **15.25** ms | 9.70 ms | 18.89 ms | 39517.33 | 86962.33 | 8.64 Mb |
| python (3.7)| bottle (0.12) | **15.68** ms | 13.81 ms | 24.00 ms | 7671.00 | 63699.33 | 10.40 Mb |
| ruby (2.6)| agoo (2.11) | **15.82** ms | 15.64 ms | 16.71 ms | 3642.00 | 63029.00 | 2.41 Mb |
| node (12.11)| polkadot (1.0) | **16.47** ms | 9.31 ms | 18.51 ms | 48971.00 | 91752.33 | 9.11 Mb |
| php (7.3)| hyperf (1.0) | **17.20** ms | 14.84 ms | 32.38 ms | 11836.00 | 60867.33 | 8.60 Mb |
| node (12.11)| rayo (1.3) | **17.44** ms | 10.57 ms | 20.48 ms | 46893.67 | 78525.33 | 7.80 Mb |
| rust (1.38)| gotham (0.4) | **17.46** ms | 17.36 ms | 24.89 ms | 11907.00 | 57682.67 | 7.76 Mb |
| node (12.11)| 0http (1.2) | **17.51** ms | 9.44 ms | 19.00 ms | 55448.33 | 90037.67 | 8.94 Mb |
| python (3.7)| asgineer (0.7) | **17.82** ms | 16.54 ms | 27.48 ms | 8592.67 | 55812.00 | 6.60 Mb |
| php (7.3)| sw-fw-less (preview) | **18.46** ms | 17.30 ms | 28.86 ms | 8753.00 | 53637.33 | 8.17 Mb |
| ruby (2.6)| plezi (0.16) | **18.80** ms | 16.53 ms | 27.78 ms | 11535.00 | 52748.67 | 7.45 Mb |
| python (3.7)| blacksheep (0.2) | **19.24** ms | 17.17 ms | 32.29 ms | 10140.67 | 52612.67 | 7.01 Mb |
| node (12.11)| polka (0.5) | **20.18** ms | 10.35 ms | 20.32 ms | 64044.33 | 81134.67 | 8.06 Mb |
| python (3.7)| hug (2.6) | **20.28** ms | 17.23 ms | 31.86 ms | 10451.00 | 49039.67 | 8.06 Mb |
| python (3.7)| starlette (0.12) | **22.19** ms | 19.09 ms | 38.28 ms | 12946.67 | 46165.67 | 6.59 Mb |
| node (12.11)| foxify (0.1) | **23.48** ms | 12.76 ms | 24.39 ms | 67797.33 | 66945.67 | 9.33 Mb |
| php (7.3)| swoft (2.0) | **24.56** ms | 22.82 ms | 33.64 ms | 8785.67 | 39655.33 | 6.91 Mb |
| node (12.11)| muneem (2.4) | **26.38** ms | 11.83 ms | 23.03 ms | 84610.00 | 71131.00 | 7.06 Mb |
| php (7.3)| imi (1.0) | **26.62** ms | 25.68 ms | 34.35 ms | 7681.67 | 36746.67 | 5.60 Mb |
| kotlin (1.3)| ktor (1.2) | **26.97** ms | 10.98 ms | 25.99 ms | 94259.67 | 78227.33 | 8.08 Mb |
| swift (5.1)| kitura-nio (2.8) | **27.44** ms | 22.60 ms | 27.67 ms | 52898.33 | 41955.00 | 5.17 Mb |
| node (12.11)| koa (2.8) | **27.63** ms | 14.15 ms | 27.16 ms | 81302.00 | 60111.00 | 8.43 Mb |
| node (12.11)| iotjs-express (0.0) | **27.75** ms | 14.53 ms | 27.60 ms | 79824.00 | 58659.00 | 15.80 Mb |
| csharp (7.3)| aspnetcore (2.2) | **27.87** ms | 9.81 ms | 17.61 ms | 106236.67 | 86907.33 | 9.38 Mb |
| fsharp (7.3)| suave (2.5) | **28.49** ms | 21.69 ms | 42.12 ms | 46882.33 | 28370.67 | 3.81 Mb |
| node (12.11)| restify (8.4) | **28.74** ms | 19.18 ms | 31.93 ms | 60036.00 | 45317.00 | 5.27 Mb |
| node (12.11)| fastify (2.8) | **30.88** ms | 15.22 ms | 28.45 ms | 94999.33 | 59257.33 | 10.40 Mb |
| python (3.7)| fastapi (0.42) | **34.25** ms | 31.23 ms | 56.73 ms | 17254.00 | 29147.67 | 4.17 Mb |
| swift (5.1)| kitura (2.8) | **34.68** ms | 22.49 ms | 27.91 ms | 102393.00 | 42126.33 | 5.20 Mb |
| clojure (1.10)| coast (1.0) | **35.22** ms | 19.60 ms | 25.59 ms | 119498.00 | 45928.33 | 5.48 Mb |
| node (12.11)| express (4.17) | **35.29** ms | 15.92 ms | 30.29 ms | 106910.00 | 52947.67 | 8.59 Mb |
| ruby (2.6)| rails (6.0) | **35.56** ms | 6.04 ms | 109.48 ms | 58626.33 | 3582.00 | 1.50 Mb |
| java (8)| spring-boot (2.1) | **37.33** ms | 16.72 ms | 40.62 ms | 113469.67 | 45486.00 | 2.40 Mb |
| python (3.7)| responder (2.0) | **37.71** ms | 32.28 ms | 64.22 ms | 20326.33 | 26994.67 | 3.90 Mb |
| swift (5.1)| vapor (3.3) | **38.54** ms | 17.93 ms | 36.10 ms | 125197.33 | 45104.00 | 5.08 Mb |
| crystal (0.31)| spider-gazelle (1.6) | **39.23** ms | 37.02 ms | 47.54 ms | 22789.67 | 25267.67 | 1.78 Mb |
| python (3.7)| clastic (19.9) | **40.90** ms | 35.38 ms | 59.87 ms | 21997.00 | 24341.67 | 4.00 Mb |
| python (3.7)| molten (0.27) | **43.43** ms | 36.31 ms | 65.52 ms | 19391.67 | 23340.00 | 2.88 Mb |
| crystal (0.31)| lucky (0.18) | **44.90** ms | 42.14 ms | 55.95 ms | 18573.33 | 21864.33 | 1.79 Mb |
| python (3.7)| flask (1.1) | **45.03** ms | 38.91 ms | 65.09 ms | 18600.67 | 21962.00 | 3.59 Mb |
| scala (2.12)| http4s (0.18) | **46.80** ms | 20.26 ms | 44.92 ms | 172611.33 | 44420.00 | 5.17 Mb |
| node (12.11)| turbo_polka (2.0) | **46.99** ms | 42.16 ms | 48.77 ms | 44315.33 | 22262.00 | 1.39 Mb |
| python (3.7)| aiohttp (3.6) | **47.67** ms | 43.73 ms | 73.36 ms | 21406.00 | 21096.00 | 3.17 Mb |
| python (3.7)| sanic (19.9) | **47.94** ms | 40.98 ms | 86.52 ms | 23515.33 | 21183.67 | 2.51 Mb |
| python (3.7)| bocadillo (0.18) | **48.74** ms | 46.51 ms | 78.46 ms | 19418.00 | 20581.33 | 2.64 Mb |
| java (8)| micronaut (1.2) | **53.64** ms | 25.12 ms | 91.50 ms | 134622.33 | 26358.33 | 3.57 Mb |
| php (7.3)| zend-expressive (3.2) | **55.84** ms | 18.37 ms | 113.58 ms | 119634.00 | 43571.67 | 14.35 Mb |
| php (7.3)| slim (4.3) | **57.81** ms | 19.61 ms | 126.02 ms | 120645.33 | 41172.00 | 13.56 Mb |
| php (7.3)| basicphp (0.9) | **58.78** ms | 20.12 ms | 120.79 ms | 124992.33 | 40654.67 | 13.42 Mb |
| php (7.3)| lumen (6.2) | **58.98** ms | 19.89 ms | 122.51 ms | 124284.33 | 40604.00 | 13.38 Mb |
| php (7.3)| zend-framework (3.1) | **60.89** ms | 19.45 ms | 127.28 ms | 133744.67 | 41687.33 | 13.73 Mb |
| php (7.3)| symfony (4.3) | **62.34** ms | 19.74 ms | 123.39 ms | 141268.67 | 40232.67 | 13.26 Mb |
| php (7.3)| spiral (2.2) | **64.28** ms | 63.59 ms | 74.77 ms | 10765.00 | 15181.00 | 1.75 Mb |
| node (12.11)| hapi (18.4) | **70.72** ms | 24.38 ms | 47.00 ms | 218429.00 | 34997.00 | 6.02 Mb |
| node (12.11)| moleculer (0.13) | **73.65** ms | 27.43 ms | 57.06 ms | 213732.00 | 30288.00 | 3.46 Mb |
| php (7.3)| laravel (6.4) | **85.42** ms | 23.69 ms | 157.92 ms | 203995.33 | 34394.00 | 11.38 Mb |
| python (3.7)| cherrypy (18.3) | **85.80** ms | 72.31 ms | 77.66 ms | 206124.67 | 1404.00 | 0.22 Mb |
| python (3.7)| quart (0.10) | **86.50** ms | 77.39 ms | 139.93 ms | 39295.00 | 11382.33 | 1.51 Mb |
| crystal (0.31)| athena (0.7) | **87.80** ms | 52.43 ms | 234.55 ms | 114439.33 | 21638.67 | 1.80 Mb |
| go (1.13)| gramework (1.6) | **96.33** ms | 97.90 ms | 102.17 ms | 19330.00 | 10111.33 | 1.72 Mb |
| python (3.7)| django (2.2) | **100.07** ms | 94.37 ms | 141.35 ms | 31582.67 | 9699.33 | 1.87 Mb |
| perl (5.3)| dancer2 (2.0) | **104.62** ms | 52.79 ms | 193.80 ms | 215959.67 | 1181.33 | 0.18 Mb |
| python (3.7)| tornado (5.1) | **105.39** ms | 104.12 ms | 123.60 ms | 29558.67 | 9039.00 | 1.77 Mb |
| python (3.7)| masonite (2.2) | **128.71** ms | 116.33 ms | 208.90 ms | 47128.67 | 7514.33 | 1.23 Mb |
| java (8)| javalin (3.5) | **133.02** ms | 12.42 ms | 304.34 ms | 430168.33 | 61883.33 | 7.32 Mb |
| crystal (0.31)| onyx (0.5) | **190.99** ms | 192.37 ms | 223.75 ms | 30173.00 | 5142.00 | 0.88 Mb |
| scala (2.12)| akkahttp (10.1) | **203.90** ms | 7.33 ms | 37.20 ms | 843230.67 | 68814.00 | 9.86 Mb |
| python (3.7)| cyclone (1.3) | **366.60** ms | 315.97 ms | 405.85 ms | 461264.33 | 2299.67 | 0.39 Mb |
| python (3.7)| nameko (2.12) | **659.37** ms | 572.43 ms | 642.90 ms | 712035.67 | 1315.33 | 0.18 Mb |

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

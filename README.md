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
| rust (1.38)| nickel (0.11) | **0.10** ms | 0.08 ms | 0.15 ms | 65.00 | 136468.00 | 18.02 Mb |
| rust (1.38)| iron (0.6) | **1.85** ms | 1.83 ms | 2.78 ms | 796.67 | 52481.00 | 4.32 Mb |
| c (11)| agoo-c (0.7) | **2.78** ms | 2.06 ms | 5.71 ms | 2607.00 | 366509.33 | 14.04 Mb |
| go (1.13)| fasthttprouter (0.1) | **3.22** ms | 2.46 ms | 5.89 ms | 3093.33 | 299857.33 | 31.95 Mb |
| go (1.13)| router (1.6) | **3.27** ms | 2.47 ms | 5.86 ms | 3901.67 | 300137.00 | 31.94 Mb |
| nim (1.0)| httpbeast (0.2) | **3.35** ms | 1.98 ms | 7.97 ms | 3786.33 | 368038.00 | 34.73 Mb |
| go (1.13)| atreugo (9.0) | **3.35** ms | 2.49 ms | 5.88 ms | 4731.33 | 296156.00 | 39.48 Mb |
| c (11)| kore (3.3) | **3.43** ms | 3.13 ms | 6.34 ms | 2389.33 | 293798.67 | 52.96 Mb |
| go (1.13)| gorouter-fasthttp (4.2) | **3.54** ms | 2.53 ms | 5.98 ms | 6505.00 | 290389.00 | 30.84 Mb |
| javascript (12.13)| nanoexpress-pro (1.6) | **3.77** ms | 1.58 ms | 9.97 ms | 4839.33 | 380743.67 | 22.20 Mb |
| javascript (12.13)| nanoexpress (1.1) | **3.85** ms | 2.03 ms | 9.50 ms | 4635.00 | 341675.33 | 19.91 Mb |
| javascript (12.13)| sifrr (0.0) | **3.87** ms | 1.74 ms | 10.05 ms | 4915.67 | 357227.67 | 20.84 Mb |
| crystal (0.31)| toro (0.4) | **3.88** ms | 2.85 ms | 7.81 ms | 3263.33 | 270998.67 | 16.89 Mb |
| crystal (0.31)| router.cr (0.2) | **3.90** ms | 3.03 ms | 7.66 ms | 3150.00 | 263745.67 | 16.44 Mb |
| crystal (0.31)| spider-gazelle (2.1) | **3.98** ms | 3.09 ms | 7.74 ms | 3197.67 | 259524.67 | 18.28 Mb |
| crystal (0.31)| raze (0.3) | **4.01** ms | 3.20 ms | 7.67 ms | 3170.00 | 255957.67 | 15.95 Mb |
| go (1.13)| fasthttp (1.5) | **4.09** ms | 2.61 ms | 6.49 ms | 11862.67 | 279246.00 | 29.84 Mb |
| crystal (0.31)| kemal (0.28) | **4.20** ms | 3.51 ms | 7.76 ms | 3210.00 | 242202.33 | 26.24 Mb |
| crystal (0.31)| amber (0.3) | **4.45** ms | 3.78 ms | 8.10 ms | 3328.33 | 230254.00 | 27.94 Mb |
| nim (1.0)| jester (0.4) | **4.48** ms | 3.36 ms | 8.71 ms | 3671.33 | 271771.67 | 36.20 Mb |
| crystal (0.31)| orion (1.7) | **4.82** ms | 4.32 ms | 8.36 ms | 3370.00 | 209693.00 | 22.71 Mb |
| php (7.4)| workerman (3.5) | **5.04** ms | 4.74 ms | 7.77 ms | 3132.67 | 198955.67 | 25.33 Mb |
| go (1.13)| aero (1.3) | **5.23** ms | 4.38 ms | 8.86 ms | 4959.67 | 196432.33 | 17.41 Mb |
| go (1.13)| rte (0.0) | **5.33** ms | 4.09 ms | 9.71 ms | 7290.67 | 206799.33 | 18.32 Mb |
| cpp (11)| drogon (1.0) | **5.44** ms | 5.12 ms | 7.73 ms | 3054.67 | 196767.00 | 12.64 Mb |
| java (8)| act (1.8) | **5.54** ms | 3.57 ms | 10.69 ms | 8171.33 | 247309.67 | 28.29 Mb |
| go (1.13)| kami (2.2) | **5.56** ms | 4.45 ms | 10.01 ms | 5592.00 | 189754.33 | 16.72 Mb |
| go (1.13)| echo (4.1) | **5.74** ms | 4.24 ms | 11.86 ms | 5964.33 | 189281.33 | 22.02 Mb |
| go (1.13)| violetear (7.0) | **5.84** ms | 4.46 ms | 9.31 ms | 10984.00 | 190498.67 | 16.76 Mb |
| go (1.13)| goroute (0.0) | **5.89** ms | 4.33 ms | 12.70 ms | 5686.67 | 189829.67 | 22.08 Mb |
| javascript (12.13)| polkadot (1.0) | **5.92** ms | 4.95 ms | 9.44 ms | 6289.33 | 164850.67 | 16.37 Mb |
| javascript (12.13)| restana (3.4) | **6.39** ms | 5.18 ms | 9.64 ms | 6646.67 | 155936.67 | 15.49 Mb |
| go (1.13)| gorilla-mux (1.7) | **6.40** ms | 4.32 ms | 13.84 ms | 9432.67 | 183759.67 | 16.25 Mb |
| ruby (2.6)| agoo (2.11) | **6.51** ms | 3.65 ms | 9.56 ms | 13157.00 | 240971.00 | 9.24 Mb |
| java (8)| rapidoid (5.5) | **6.60** ms | 2.09 ms | 10.85 ms | 15770.00 | 321124.00 | 38.31 Mb |
| go (1.13)| gin (1.5) | **6.65** ms | 4.39 ms | 11.98 ms | 15234.00 | 187589.67 | 21.82 Mb |
| javascript (12.13)| rayo (1.3) | **6.69** ms | 5.30 ms | 10.06 ms | 6377.33 | 147029.00 | 14.60 Mb |
| go (1.13)| gorouter (4.2) | **6.80** ms | 4.46 ms | 12.35 ms | 15253.00 | 184330.67 | 16.25 Mb |
| swift (5.1)| perfect (3.1) | **6.85** ms | 6.75 ms | 9.45 ms | 2025.33 | 140825.33 | 8.78 Mb |
| go (1.13)| beego (1.12) | **6.85** ms | 4.67 ms | 15.02 ms | 8414.67 | 170318.00 | 15.15 Mb |
| javascript (12.13)| polka (0.5) | **6.96** ms | 5.30 ms | 9.91 ms | 10072.33 | 150466.33 | 14.94 Mb |
| javascript (12.13)| 0http (1.2) | **7.08** ms | 5.40 ms | 10.32 ms | 10449.67 | 147709.00 | 14.67 Mb |
| rust (1.38)| actix-web (1.0) | **7.11** ms | 6.68 ms | 9.76 ms | 2493.00 | 178952.33 | 17.83 Mb |
| rust (1.38)| gotham (0.4) | **7.12** ms | 6.36 ms | 10.46 ms | 9762.00 | 147987.67 | 19.89 Mb |
| go (1.13)| air (0.13) | **7.26** ms | 4.55 ms | 16.90 ms | 11228.67 | 168295.00 | 23.28 Mb |
| go (1.13)| webgo (3.0) | **7.47** ms | 4.52 ms | 11.04 ms | 23850.67 | 181074.00 | 15.96 Mb |
| swift (5.1)| swifter (1.4) | **7.51** ms | 0.84 ms | 14.52 ms | 93358.00 | 22112.33 | 1.88 Mb |
| go (1.13)| gf (1.1) | **7.54** ms | 5.07 ms | 14.95 ms | 12550.33 | 157825.67 | 17.72 Mb |
| python (3.8)| falcon (2.0) | **7.69** ms | 5.55 ms | 13.69 ms | 6820.00 | 132317.00 | 20.55 Mb |
| javascript (12.13)| muneem (2.4) | **7.70** ms | 6.57 ms | 11.53 ms | 8341.67 | 131992.00 | 13.11 Mb |
| go (1.13)| chi (4.0) | **8.42** ms | 4.40 ms | 13.05 ms | 30884.00 | 183314.67 | 16.16 Mb |
| javascript (12.13)| foxify (0.1) | **8.60** ms | 7.09 ms | 12.21 ms | 12419.33 | 122493.33 | 17.06 Mb |
| cpp (11)| evhtp (1.2) | **8.98** ms | 8.71 ms | 11.87 ms | 4094.67 | 110782.33 | 7.13 Mb |
| ruby (2.6)| plezi (0.16) | **9.68** ms | 9.37 ms | 13.59 ms | 4416.33 | 100532.67 | 14.21 Mb |
| python (3.8)| bottle (0.12) | **9.74** ms | 7.81 ms | 19.34 ms | 8575.33 | 110854.00 | 18.10 Mb |
| javascript (12.13)| iotjs-express (0.0) | **9.77** ms | 7.95 ms | 14.12 ms | 12360.67 | 109502.00 | 29.49 Mb |
| php (7.4)| one (1.8) | **10.55** ms | 9.44 ms | 19.00 ms | 6435.33 | 93213.33 | 14.20 Mb |
| javascript (12.13)| koa (2.11) | **10.59** ms | 8.61 ms | 14.55 ms | 17871.67 | 104927.00 | 14.72 Mb |
| javascript (12.13)| express (4.17) | **10.86** ms | 8.58 ms | 16.01 ms | 13481.00 | 96011.00 | 15.58 Mb |
| python (3.8)| blacksheep (0.2) | **10.87** ms | 10.97 ms | 17.16 ms | 4516.33 | 90028.33 | 12.00 Mb |
| python (3.8)| asgineer (0.7) | **10.96** ms | 10.94 ms | 17.06 ms | 4938.67 | 89722.00 | 10.62 Mb |
| go (1.13)| mars (1.0) | **11.93** ms | 5.36 ms | 33.05 ms | 15191.00 | 126635.00 | 18.91 Mb |
| javascript (12.13)| fastify (2.11) | **12.00** ms | 8.56 ms | 15.21 ms | 27200.00 | 109011.00 | 18.76 Mb |
| python (3.8)| hug (2.6) | **12.27** ms | 9.65 ms | 21.40 ms | 9569.33 | 84232.00 | 13.84 Mb |
| php (7.4)| hyperf (1.0) | **12.30** ms | 8.56 ms | 26.60 ms | 11698.67 | 94197.33 | 13.31 Mb |
| swift (5.1)| vapor (3.3) | **12.62** ms | 8.69 ms | 15.87 ms | 36249.00 | 99559.00 | 11.04 Mb |
| php (7.4)| sw-fw-less (preview) | **12.95** ms | 11.91 ms | 23.02 ms | 7153.67 | 76292.33 | 11.62 Mb |
| clojure (1.10)| coast (1.0) | **13.25** ms | 11.66 ms | 12.66 ms | 28601.67 | 84626.67 | 10.10 Mb |
| python (3.8)| starlette (0.13) | **13.26** ms | 12.02 ms | 21.96 ms | 6316.00 | 74989.00 | 10.71 Mb |
| javascript (12.13)| restify (8.5) | **14.57** ms | 11.44 ms | 20.09 ms | 23290.00 | 75426.67 | 8.77 Mb |
| php (7.4)| swoft (2.0) | **14.91** ms | 14.74 ms | 19.34 ms | 3969.33 | 64666.67 | 11.27 Mb |
| php (7.4)| imi (1.0) | **15.12** ms | 14.84 ms | 19.68 ms | 4658.00 | 64077.00 | 9.76 Mb |
| csharp (7.3)| aspnetcore (3.0) | **15.39** ms | 4.38 ms | 7.82 ms | 77742.33 | 196243.33 | 21.18 Mb |
| swift (5.1)| kitura-nio (2.8) | **16.46** ms | 14.30 ms | 14.90 ms | 38628.00 | 68833.67 | 8.49 Mb |
| fsharp (7.3)| suave (2.5) | **17.47** ms | 13.02 ms | 26.33 ms | 35735.67 | 70317.67 | 9.44 Mb |
| kotlin (1.3)| ktor (1.2) | **18.16** ms | 5.33 ms | 21.99 ms | 76974.67 | 138844.00 | 14.35 Mb |
| python (3.8)| fastapi (0.45) | **18.62** ms | 18.17 ms | 28.90 ms | 7115.67 | 53021.00 | 7.58 Mb |
| python (3.8)| responder (2.0) | **18.80** ms | 18.62 ms | 29.20 ms | 7687.33 | 52258.33 | 7.57 Mb |
| swift (5.1)| kitura (2.8) | **19.32** ms | 14.67 ms | 15.29 ms | 47665.00 | 67092.00 | 8.28 Mb |
| javascript (12.13)| moleculer (0.13) | **20.03** ms | 14.53 ms | 30.41 ms | 25372.00 | 54818.00 | 6.25 Mb |
| ruby (2.6)| cuba (3.9) | **21.22** ms | 3.91 ms | 40.02 ms | 176344.33 | 59171.33 | 4.63 Mb |
| python (3.8)| aiohttp (3.6) | **22.26** ms | 20.13 ms | 37.09 ms | 9710.00 | 45190.33 | 6.79 Mb |
| python (3.8)| molten (0.27) | **22.53** ms | 19.42 ms | 34.50 ms | 11737.33 | 45357.33 | 5.59 Mb |
| crystal (0.31)| lucky (0.18) | **22.82** ms | 20.64 ms | 25.54 ms | 21219.00 | 45004.67 | 3.67 Mb |
| python (3.8)| flask (1.1) | **23.62** ms | 20.72 ms | 36.02 ms | 14291.33 | 42168.00 | 6.89 Mb |
| php (7.4)| ubiquity (2.3) | **23.80** ms | 7.34 ms | 44.55 ms | 56867.67 | 105954.67 | 31.72 Mb |
| javascript (12.13)| hapi (18.4) | **24.80** ms | 16.93 ms | 30.60 ms | 51243.67 | 54911.33 | 9.46 Mb |
| python (3.8)| clastic (19.9) | **24.91** ms | 20.66 ms | 40.11 ms | 13869.33 | 40247.33 | 6.61 Mb |
| ruby (2.6)| flame (4.18) | **25.62** ms | 6.29 ms | 67.69 ms | 124028.00 | 35176.00 | 1.35 Mb |
| php (7.4)| one-fpm (1.8) | **26.52** ms | 8.37 ms | 52.82 ms | 61421.33 | 93455.00 | 27.98 Mb |
| php (7.4)| lumen (6.2) | **26.72** ms | 8.25 ms | 50.09 ms | 62268.00 | 96874.67 | 29.02 Mb |
| ruby (2.6)| syro (3.1) | **27.08** ms | 3.69 ms | 40.55 ms | 226246.33 | 64499.67 | 2.47 Mb |
| ruby (2.6)| rack-routing (0.0) | **27.08** ms | 4.86 ms | 55.70 ms | 175948.33 | 49146.67 | 1.88 Mb |
| python (3.8)| sanic (19.9) | **29.10** ms | 26.89 ms | 50.26 ms | 16493.67 | 34677.33 | 4.10 Mb |
| python (3.8)| bocadillo (0.18) | **29.72** ms | 26.78 ms | 51.01 ms | 16512.00 | 34427.00 | 4.41 Mb |
| ruby (2.6)| camping (2.1) | **30.46** ms | 5.67 ms | 60.62 ms | 226279.00 | 40801.00 | 2.58 Mb |
| javascript (12.13)| turbo_polka (0.3) | **30.70** ms | 26.21 ms | 28.28 ms | 51441.67 | 36964.67 | 2.30 Mb |
| crystal (0.31)| athena (0.7) | **30.72** ms | 24.46 ms | 73.59 ms | 38602.33 | 45941.00 | 3.81 Mb |
| php (7.4)| symfony (4.3) | **31.93** ms | 8.46 ms | 80.77 ms | 74484.33 | 92969.33 | 27.86 Mb |
| php (7.4)| spiral (2.4) | **33.31** ms | 32.06 ms | 39.23 ms | 14834.00 | 30229.67 | 3.49 Mb |
| ruby (2.6)| roda (3.26) | **34.38** ms | 3.93 ms | 48.13 ms | 259588.00 | 63975.00 | 4.05 Mb |
| ruby (2.6)| sinatra (2.0) | **34.82** ms | 8.99 ms | 97.64 ms | 159581.67 | 24641.33 | 4.25 Mb |
| php (7.4)| zend-expressive (3.2) | **34.93** ms | 8.35 ms | 89.14 ms | 85272.00 | 92578.33 | 27.72 Mb |
| php (7.4)| zend-framework (3.1) | **36.06** ms | 8.62 ms | 84.02 ms | 87220.00 | 90498.33 | 27.10 Mb |
| php (7.4)| slim (4.3) | **38.15** ms | 8.25 ms | 95.76 ms | 96340.33 | 92214.00 | 27.61 Mb |
| ruby (2.6)| hanami (1.3) | **40.15** ms | 8.13 ms | 98.85 ms | 206271.33 | 27537.00 | 13.83 Mb |
| php (7.4)| basicphp (0.9) | **41.56** ms | 8.74 ms | 114.98 ms | 104252.67 | 86136.33 | 25.87 Mb |
| scala (2.12)| http4s (0.18) | **43.20** ms | 6.98 ms | 21.35 ms | 203834.00 | 101038.33 | 11.75 Mb |
| go (1.13)| gramework (1.7) | **43.41** ms | 42.90 ms | 44.44 ms | 14911.33 | 22861.00 | 3.89 Mb |
| php (7.4)| laravel (6.7) | **45.15** ms | 9.07 ms | 70.52 ms | 123768.67 | 85138.00 | 25.60 Mb |
| java (8)| micronaut (1.2) | **47.02** ms | 9.52 ms | 52.61 ms | 193376.00 | 78852.67 | 10.59 Mb |
| python (3.8)| quart (0.10) | **53.02** ms | 45.72 ms | 91.52 ms | 25884.33 | 18859.67 | 2.50 Mb |
| java (8)| spring-boot (2.1) | **55.82** ms | 7.67 ms | 14.45 ms | 238090.33 | 111758.33 | 5.76 Mb |
| java (8)| javalin (3.5) | **59.75** ms | 5.15 ms | 54.54 ms | 275362.33 | 145414.33 | 17.21 Mb |
| python (3.8)| tornado (6.0) | **61.22** ms | 58.97 ms | 79.05 ms | 17489.67 | 15859.33 | 3.11 Mb |
| java (8)| spring-framework (5.2) | **61.63** ms | 9.38 ms | 81.17 ms | 231552.67 | 89100.00 | 10.75 Mb |
| ruby (2.6)| grape (1.2) | **62.26** ms | 11.71 ms | 167.61 ms | 221870.00 | 20486.33 | 0.77 Mb |
| python (3.8)| django (3.0) | **70.33** ms | 55.18 ms | 125.28 ms | 40351.00 | 14642.67 | 3.21 Mb |
| python (3.8)| cherrypy (18.5) | **83.95** ms | 66.34 ms | 140.74 ms | 47274.33 | 12166.33 | 2.16 Mb |
| python (3.8)| masonite (2.2) | **102.77** ms | 84.41 ms | 166.81 ms | 52486.00 | 9768.67 | 1.60 Mb |
| ruby (2.6)| rails (6.0) | **114.20** ms | 13.15 ms | 410.16 ms | 189480.00 | 5566.00 | 2.33 Mb |
| crystal (0.31)| onyx (0.5) | **164.07** ms | 163.65 ms | 192.43 ms | 23825.67 | 6007.00 | 1.03 Mb |
| scala (2.12)| akkahttp (10.1) | **213.57** ms | 4.84 ms | 20.83 ms | 903642.33 | 138807.67 | 19.90 Mb |
| perl (5.3)| dancer2 (2.0) | **261.77** ms | 31.66 ms | 482.32 ms | 840817.33 | 2293.00 | 0.34 Mb |
| julia (1.3)| merly (0.2) | **270.68** ms | 100.40 ms | 279.31 ms | 717679.00 | 7447.33 | 0.59 Mb |
| python (3.8)| cyclone (1.3) | **309.61** ms | 199.11 ms | 238.83 ms | 683522.33 | 3606.67 | 0.61 Mb |
| python (3.8)| klein (19.6) | **389.64** ms | 308.11 ms | 335.13 ms | 641219.33 | 2344.00 | 0.34 Mb |
| python (3.8)| nameko (2.12) | **480.16** ms | 393.24 ms | 429.65 ms | 712420.33 | 2167.33 | 0.30 Mb |

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

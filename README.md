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
| rust (1.38)| nickel (0.11) | **0.27** ms | 0.23 ms | 0.44 ms | 184.00 | 36801.67 | 4.86 Mb |
| ruby (2.6)| syro (3.1) | **2.70** ms | 0.80 ms | 7.56 ms | 3574.33 | 46793.67 | 1.79 Mb |
| ruby (2.6)| roda (3.25) | **2.76** ms | 0.68 ms | 7.91 ms | 3765.67 | 45861.67 | 2.90 Mb |
| ruby (2.6)| cuba (3.9) | **3.17** ms | 0.67 ms | 9.22 ms | 4413.67 | 40092.00 | 3.14 Mb |
| rust (1.38)| iron (0.6) | **3.24** ms | 3.17 ms | 4.77 ms | 1243.67 | 20221.33 | 1.67 Mb |
| ruby (2.6)| rack-routing (0.0) | **3.87** ms | 1.61 ms | 10.22 ms | 4680.67 | 32857.67 | 1.26 Mb |
| ruby (2.6)| camping (2.1) | **4.78** ms | 1.21 ms | 13.04 ms | 6019.67 | 26796.33 | 1.70 Mb |
| c (11)| agoo-c (0.7) | **5.14** ms | 4.38 ms | 9.97 ms | 4250.33 | 190111.00 | 7.29 Mb |
| node (12.11)| sifrr (0.0) | **5.15** ms | 4.37 ms | 10.37 ms | 4287.00 | 192485.67 | 11.23 Mb |
| nim (1.0)| httpbeast (0.2) | **5.47** ms | 4.69 ms | 10.45 ms | 4138.33 | 180152.00 | 17.00 Mb |
| cpp (11)| drogon (1.0) | **5.51** ms | 4.79 ms | 10.05 ms | 3617.33 | 177806.00 | 11.44 Mb |
| ruby (2.6)| flame (4.18) | **5.53** ms | 2.04 ms | 15.07 ms | 7154.00 | 23113.67 | 0.89 Mb |
| python (3.7)| japronto (0.1) | **5.62** ms | 4.74 ms | 11.07 ms | 4474.00 | 175839.33 | 13.95 Mb |
| cpp (11)| evhtp (1.2) | **5.95** ms | 5.29 ms | 9.66 ms | 3049.33 | 159785.67 | 10.28 Mb |
| swift (5.1)| swifter (1.4) | **6.05** ms | 0.90 ms | 14.58 ms | 92284.00 | 12134.33 | 1.03 Mb |
| go (1.13)| gorouter-fasthttp (4.2) | **6.22** ms | 5.51 ms | 9.01 ms | 5348.67 | 156122.67 | 16.62 Mb |
| go (1.13)| fasthttprouter (0.1) | **6.26** ms | 5.53 ms | 9.07 ms | 5495.33 | 155849.33 | 16.61 Mb |
| go (1.13)| atreugo (8.2) | **6.36** ms | 5.45 ms | 9.01 ms | 7401.67 | 157327.33 | 20.98 Mb |
| crystal (0.31)| toro (0.4) | **6.48** ms | 5.62 ms | 10.97 ms | 3606.33 | 150149.67 | 9.36 Mb |
| crystal (0.31)| router.cr (0.2) | **6.56** ms | 5.76 ms | 10.99 ms | 3587.00 | 148010.00 | 9.22 Mb |
| ruby (2.6)| hanami (1.3) | **6.56** ms | 1.97 ms | 18.55 ms | 8980.33 | 19546.00 | 9.82 Mb |
| java (8)| rapidoid (5.5) | **6.60** ms | 5.12 ms | 12.00 ms | 7537.67 | 157000.67 | 18.73 Mb |
| crystal (0.31)| raze (0.3) | **6.68** ms | 5.86 ms | 11.06 ms | 3585.67 | 145282.00 | 9.05 Mb |
| crystal (0.31)| kemal (0.28) | **7.07** ms | 6.30 ms | 11.44 ms | 3573.33 | 137216.67 | 14.86 Mb |
| ruby (2.6)| sinatra (2.0) | **7.25** ms | 0.79 ms | 21.48 ms | 10431.67 | 17643.00 | 3.04 Mb |
| crystal (0.31)| amber (0.3) | **7.54** ms | 6.90 ms | 11.95 ms | 3558.33 | 128838.00 | 15.63 Mb |
| c (11)| kore (3.3) | **8.02** ms | 6.31 ms | 15.69 ms | 8410.33 | 150388.33 | 27.10 Mb |
| crystal (0.31)| orion (1.7) | **8.20** ms | 7.55 ms | 13.22 ms | 4027.67 | 119171.33 | 12.91 Mb |
| nim (1.0)| jester (0.4) | **8.50** ms | 7.38 ms | 14.64 ms | 5556.33 | 127894.00 | 17.04 Mb |
| java (8)| act (1.8) | **9.00** ms | 7.36 ms | 13.35 ms | 11554.00 | 122001.00 | 13.96 Mb |
| ruby (2.6)| grape (1.2) | **9.41** ms | 0.89 ms | 28.29 ms | 13464.67 | 13700.00 | 0.52 Mb |
| go (1.13)| gorouter (4.2) | **9.65** ms | 8.18 ms | 15.33 ms | 7700.33 | 106521.67 | 9.40 Mb |
| go (1.13)| rte (0.0) | **9.68** ms | 7.84 ms | 14.73 ms | 12530.67 | 109493.00 | 9.71 Mb |
| go (1.13)| violetear (7.0) | **10.06** ms | 8.94 ms | 15.13 ms | 6059.00 | 99188.33 | 8.72 Mb |
| go (1.13)| chi (4.0) | **10.13** ms | 8.21 ms | 16.26 ms | 11024.67 | 103573.00 | 9.19 Mb |
| go (1.13)| echo (4.1) | **10.24** ms | 8.44 ms | 17.24 ms | 7949.67 | 99450.00 | 11.57 Mb |
| rust (1.38)| actix-web (1.0) | **10.41** ms | 10.14 ms | 13.65 ms | 2737.33 | 104935.67 | 10.11 Mb |
| go (1.13)| kami (2.2) | **10.52** ms | 8.61 ms | 15.99 ms | 13084.67 | 100017.33 | 8.81 Mb |
| go (1.13)| gorilla-mux (1.7) | **10.91** ms | 8.67 ms | 18.75 ms | 9729.67 | 97127.00 | 8.60 Mb |
| go (1.13)| beego (1.12) | **10.93** ms | 8.80 ms | 17.78 ms | 11342.67 | 97399.00 | 8.68 Mb |
| go (1.13)| goroute (0.0) | **10.95** ms | 8.51 ms | 17.32 ms | 15464.67 | 99078.67 | 11.53 Mb |
| csharp (7.3)| aspnetcore (2.2) | **11.05** ms | 9.73 ms | 15.09 ms | 11110.67 | 90257.00 | 9.74 Mb |
| go (1.13)| webgo (3.0) | **11.46** ms | 9.27 ms | 17.05 ms | 15686.67 | 93382.67 | 8.23 Mb |
| go (1.13)| air (0.13) | **12.23** ms | 9.76 ms | 21.61 ms | 9409.67 | 86606.33 | 11.98 Mb |
| go (1.13)| gin (1.4) | **12.46** ms | 8.68 ms | 17.97 ms | 26976.67 | 98074.33 | 11.41 Mb |
| swift (5.1)| perfect (3.1) | **12.89** ms | 13.04 ms | 15.18 ms | 3477.33 | 74950.67 | 4.67 Mb |
| python (3.7)| falcon (2.0) | **13.03** ms | 10.67 ms | 22.45 ms | 8010.00 | 78252.00 | 12.16 Mb |
| go (1.13)| gf (1.9) | **13.22** ms | 10.96 ms | 20.93 ms | 12967.67 | 79635.67 | 8.97 Mb |
| php (7.3)| one (1.8) | **14.52** ms | 13.93 ms | 22.98 ms | 6812.67 | 67795.67 | 10.33 Mb |
| ruby (2.6)| agoo (2.11) | **15.18** ms | 14.89 ms | 15.98 ms | 3053.00 | 65657.67 | 2.52 Mb |
| node (12.11)| polkadot (1.0) | **16.40** ms | 9.80 ms | 20.45 ms | 45343.67 | 85660.67 | 8.51 Mb |
| php (7.3)| hyperf (1.0) | **16.83** ms | 14.16 ms | 32.19 ms | 11917.00 | 62467.33 | 8.83 Mb |
| python (3.7)| bottle (0.12) | **17.06** ms | 14.82 ms | 27.12 ms | 9484.67 | 59203.67 | 9.67 Mb |
| ruby (2.6)| plezi (0.16) | **17.50** ms | 16.03 ms | 22.16 ms | 12749.67 | 56876.67 | 8.04 Mb |
| python (3.7)| asgineer (0.7) | **17.54** ms | 15.66 ms | 29.01 ms | 9786.33 | 57229.33 | 6.77 Mb |
| rust (1.38)| gotham (0.4) | **17.68** ms | 17.53 ms | 25.72 ms | 11158.00 | 57205.33 | 7.69 Mb |
| node (12.11)| 0http (1.2) | **18.50** ms | 9.76 ms | 19.86 ms | 58465.33 | 86863.33 | 8.63 Mb |
| node (12.11)| restana (3.3) | **19.32** ms | 10.57 ms | 22.68 ms | 56616.00 | 78385.67 | 7.78 Mb |
| python (3.7)| hug (2.6) | **20.78** ms | 17.69 ms | 30.23 ms | 17943.33 | 49338.00 | 8.11 Mb |
| kotlin (1.3)| ktor (1.2) | **21.04** ms | 13.44 ms | 35.20 ms | 43233.33 | 63766.00 | 6.59 Mb |
| python (3.7)| starlette (0.12) | **23.40** ms | 20.24 ms | 39.38 ms | 13104.00 | 43580.00 | 6.22 Mb |
| php (7.3)| swoft (2.0) | **23.44** ms | 22.94 ms | 28.20 ms | 4838.67 | 41668.00 | 7.26 Mb |
| node (12.11)| polka (0.5) | **23.98** ms | 11.92 ms | 24.22 ms | 73975.00 | 71050.67 | 7.06 Mb |
| python (3.7)| blacksheep (0.2) | **24.21** ms | 20.90 ms | 42.72 ms | 14136.33 | 42524.33 | 5.67 Mb |
| swift (5.1)| kitura-nio (2.8) | **24.62** ms | 20.93 ms | 23.28 ms | 51187.33 | 46337.33 | 5.71 Mb |
| swift (5.1)| kitura (2.8) | **26.05** ms | 22.19 ms | 24.52 ms | 48429.33 | 43760.33 | 5.40 Mb |
| node (12.11)| rayo (1.3) | **26.61** ms | 11.58 ms | 23.94 ms | 88720.33 | 72234.67 | 7.17 Mb |
| php (7.3)| imi (1.0) | **29.20** ms | 28.53 ms | 34.16 ms | 5297.00 | 33482.33 | 5.10 Mb |
| node (12.11)| restify (8.4) | **30.82** ms | 20.40 ms | 36.82 ms | 63580.33 | 41783.00 | 4.86 Mb |
| node (12.11)| fastify (2.8) | **31.76** ms | 16.21 ms | 31.08 ms | 92351.00 | 53942.33 | 9.64 Mb |
| node (12.11)| foxify (0.1) | **32.41** ms | 13.36 ms | 27.03 ms | 107636.67 | 62874.00 | 8.76 Mb |
| clojure (1.10)| coast (1.0) | **32.71** ms | 20.87 ms | 26.59 ms | 80690.33 | 43979.67 | 5.25 Mb |
| node (12.11)| iotjs-express (0.0) | **33.18** ms | 15.18 ms | 30.18 ms | 104705.00 | 55714.67 | 15.01 Mb |
| ruby (2.6)| rails (6.0) | **33.90** ms | 3.60 ms | 109.33 ms | 60104.33 | 3772.00 | 1.58 Mb |
| node (12.11)| koa (2.8) | **35.20** ms | 14.56 ms | 29.16 ms | 113757.67 | 57215.33 | 8.03 Mb |
| node (12.11)| muneem (2.4) | **35.48** ms | 13.52 ms | 28.36 ms | 120693.00 | 62107.00 | 6.17 Mb |
| python (3.7)| responder (2.0) | **35.84** ms | 33.97 ms | 56.19 ms | 16886.67 | 27758.33 | 4.02 Mb |
| crystal (0.31)| spider-gazelle (1.6) | **36.49** ms | 34.64 ms | 43.85 ms | 16441.00 | 26940.00 | 1.90 Mb |
| python (3.7)| fastapi (0.42) | **37.56** ms | 33.35 ms | 60.24 ms | 17909.00 | 26722.00 | 3.82 Mb |
| node (12.11)| express (4.17) | **40.45** ms | 17.68 ms | 34.09 ms | 124127.00 | 48139.67 | 7.81 Mb |
| python (3.7)| molten (0.27) | **40.94** ms | 36.44 ms | 58.80 ms | 16905.67 | 24559.33 | 3.03 Mb |
| python (3.7)| clastic (19.9) | **41.45** ms | 36.31 ms | 57.84 ms | 17280.67 | 23793.00 | 3.91 Mb |
| python (3.7)| flask (1.1) | **42.74** ms | 38.80 ms | 58.46 ms | 15093.33 | 22930.00 | 3.75 Mb |
| php (7.3)| basicphp (0.9) | **44.65** ms | 15.87 ms | 89.46 ms | 91378.00 | 51287.67 | 16.89 Mb |
| crystal (0.31)| lucky (0.18) | **44.70** ms | 44.19 ms | 52.84 ms | 28428.00 | 22432.00 | 1.83 Mb |
| fsharp (7.3)| suave (2.5) | **44.83** ms | 19.02 ms | 108.37 ms | 73521.33 | 25104.00 | 3.37 Mb |
| python (3.7)| aiohttp (3.6) | **45.22** ms | 41.97 ms | 66.11 ms | 15634.33 | 22024.00 | 3.31 Mb |
| php (7.3)| zend-expressive (3.2) | **48.58** ms | 16.80 ms | 102.19 ms | 100731.67 | 48651.00 | 16.02 Mb |
| php (7.3)| slim (4.3) | **48.60** ms | 16.51 ms | 96.12 ms | 102960.00 | 49236.67 | 16.21 Mb |
| php (7.3)| lumen (6.2) | **50.01** ms | 16.83 ms | 99.89 ms | 110198.33 | 48829.33 | 16.09 Mb |
| php (7.3)| symfony (4.3) | **51.74** ms | 17.01 ms | 96.76 ms | 115451.33 | 48126.33 | 15.86 Mb |
| python (3.7)| sanic (19.9) | **53.48** ms | 48.91 ms | 88.64 ms | 26822.00 | 18664.00 | 2.21 Mb |
| python (3.7)| bocadillo (0.18) | **54.55** ms | 46.84 ms | 100.97 ms | 34027.67 | 19040.00 | 2.44 Mb |
| php (7.3)| zend-framework (3.1) | **55.02** ms | 17.59 ms | 107.70 ms | 121536.67 | 46387.33 | 15.28 Mb |
| scala (2.12)| http4s (0.18) | **57.63** ms | 17.95 ms | 38.25 ms | 226862.33 | 47241.67 | 5.49 Mb |
| php (7.3)| spiral (2.2) | **59.95** ms | 58.89 ms | 65.00 ms | 10093.67 | 16382.67 | 1.89 Mb |
| node (12.11)| turbo_polka (2.0) | **63.21** ms | 42.87 ms | 51.35 ms | 125998.33 | 21717.33 | 1.35 Mb |
| swift (5.1)| vapor (3.3) | **66.99** ms | 20.15 ms | 42.21 ms | 256405.67 | 39310.33 | 4.42 Mb |
| php (7.3)| laravel (6.4) | **74.14** ms | 20.69 ms | 150.50 ms | 172589.67 | 39724.33 | 13.15 Mb |
| node (12.11)| hapi (18.4) | **75.38** ms | 26.04 ms | 51.73 ms | 223890.00 | 32238.33 | 5.55 Mb |
| java (8)| spring-boot (2.1) | **76.29** ms | 16.57 ms | 43.34 ms | 269156.33 | 45987.67 | 2.43 Mb |
| python (3.7)| quart (0.10) | **85.26** ms | 71.95 ms | 155.06 ms | 41884.67 | 11644.33 | 1.54 Mb |
| crystal (0.31)| athena (0.7) | **85.29** ms | 50.15 ms | 227.46 ms | 111336.33 | 22480.67 | 1.87 Mb |
| python (3.7)| cherrypy (18.3) | **86.88** ms | 72.08 ms | 72.92 ms | 231588.33 | 1422.33 | 0.22 Mb |
| node (12.11)| moleculer (0.13) | **90.58** ms | 29.90 ms | 65.89 ms | 264313.33 | 27768.67 | 3.16 Mb |
| go (1.13)| gramework (1.6) | **96.23** ms | 97.46 ms | 101.93 ms | 18590.00 | 10155.33 | 1.73 Mb |
| python (3.7)| tornado (5.1) | **104.40** ms | 102.39 ms | 133.62 ms | 35026.67 | 9133.00 | 1.79 Mb |
| java (8)| javalin (3.5) | **105.43** ms | 11.41 ms | 240.81 ms | 321255.00 | 59009.00 | 6.98 Mb |
| python (3.7)| django (2.2) | **113.49** ms | 100.35 ms | 183.36 ms | 45760.33 | 8563.67 | 1.65 Mb |
| python (3.7)| masonite (2.2) | **139.80** ms | 133.31 ms | 182.05 ms | 50708.67 | 6879.00 | 1.12 Mb |
| crystal (0.31)| onyx (0.5) | **188.43** ms | 192.47 ms | 217.46 ms | 29513.67 | 5211.00 | 0.89 Mb |
| perl (5.3)| dancer2 (2.0) | **188.60** ms | 59.20 ms | 272.76 ms | 460211.67 | 1039.33 | 0.16 Mb |
| scala (2.12)| akkahttp (10.1) | **250.89** ms | 7.38 ms | 201.78 ms | 944911.00 | 70320.67 | 10.08 Mb |
| python (3.7)| cyclone (1.3) | **418.95** ms | 322.16 ms | 432.46 ms | 619144.00 | 2228.33 | 0.38 Mb |
| python (3.7)| nameko (2.12) | **643.35** ms | 602.35 ms | 654.86 ms | 623467.00 | 1284.00 | 0.18 Mb |

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

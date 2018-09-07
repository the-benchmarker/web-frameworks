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

+ Helping decide beetween languages, depending on use case
+ Learning languages, best practices, devops culture ...
+ Having fun :heart:

## Requirements

+ [Crystal](https://crystal-lang.org) as `built-in` tools are made in this language
+ [Docker](https://www.docker.com) as **frameworks** are `isolated` into _containers_

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

+ Build containers

> job is either a language (example : crystal) or a framework (example : router.cr)

~~~sh
bin/neph [job]
~~~

+ Start the benchmark ....

> tools is a list of language / framework to challenge (example : ruby kemal amber go python)

~~~sh
bin/benchmarker [tools]
~~~

## Results

<!-- Result from here -->
Last update: 2018-09-08
```
OS: Linux (version: 4.17.19-200.fc28.x86_64, arch: x86_64)
CPU Cores: 4
```

### Latency


#### Ranking (top 5)


:one: nickel (rust)


:two: rocket (rust)


:three: laravel (php)


:four: symfony (php)


:five: iron (rust)


#### Full table

| Language (Runtime)        | Framework (Middleware)    |         Average |  50% percentile |  90% percentile |  99% percentile | 99.9% percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust                      | nickel                    | 0.04 ms | 0.04 ms | 0.05 ms | 0.05 ms | 0.74 ms | 6.00 | 
| rust                      | rocket                    | 0.06 ms | 0.06 ms | 0.07 ms | 0.08 ms | 0.78 ms | 11.00 | 
| php                       | laravel                   | 45.21 ms | 0.22 ms | 16.06 ms | 1596.47 ms | 7289.77 ms | 325906.00 | 
| php                       | symfony                   | 33.25 ms | 0.23 ms | 15.72 ms | 1086.61 ms | 6968.74 ms | 245825.67 | 
| rust                      | iron                      | 0.31 ms | 0.26 ms | 0.45 ms | 1.12 ms | 59.78 ms | 669.00 | 
| ruby                      | rack-routing              | 2.05 ms | 0.72 ms | 4.19 ms | 27.93 ms | 106.90 ms | 5411.33 | 
| ruby                      | roda                      | 1.51 ms | 0.93 ms | 2.04 ms | 17.31 ms | 131.35 ms | 4128.33 | 
| ruby                      | flame                     | 3.29 ms | 1.00 ms | 8.91 ms | 30.78 ms | 87.20 ms | 6052.67 | 
| ruby                      | rails                     | 15.13 ms | 1.70 ms | 54.35 ms | 99.85 ms | 184.87 ms | 24394.33 | 
| ruby                      | hanami                    | 3.25 ms | 2.54 ms | 3.73 ms | 43.05 ms | 126.69 ms | 7283.00 | 
| ruby                      | sinatra                   | 3.80 ms | 2.81 ms | 5.03 ms | 45.95 ms | 126.91 ms | 7453.67 | 
| rust                      | actix-web                 | 4.53 ms | 4.20 ms | 8.90 ms | 15.95 ms | 36.81 ms | 3445.00 | 
| go                        | fasthttprouter            | 5.53 ms | 5.23 ms | 8.52 ms | 15.06 ms | 282.24 ms | 3694.00 | 
| cpp                       | evhtp                     | 5.60 ms | 5.25 ms | 9.23 ms | 13.88 ms | 184.89 ms | 2951.33 | 
| java                      | act                       | 6.23 ms | 5.41 ms | 10.95 ms | 21.06 ms | 191.32 ms | 4801.00 | 
| python                    | vibora                    | 6.37 ms | 5.64 ms | 12.06 ms | 19.36 ms | 48.99 ms | 4349.33 | 
| crystal                   | spider-gazelle            | 6.92 ms | 6.48 ms | 10.39 ms | 17.24 ms | 35.91 ms | 2988.67 | 
| scala                     | akkahttp                  | 30.57 ms | 6.77 ms | 12.37 ms | 104.92 ms | 7842.04 ms | 302882.67 | 
| csharp                    | aspnetcore                | 8.32 ms | 7.96 ms | 11.47 ms | 15.29 ms | 298.07 ms | 4123.00 | 
| go                        | iris                      | 10.91 ms | 9.38 ms | 20.60 ms | 58.76 ms | 783.49 ms | 19268.00 | 
| go                        | echo                      | 10.68 ms | 9.54 ms | 19.92 ms | 54.63 ms | 588.69 ms | 15057.67 | 
| go                        | gorilla-mux               | 11.58 ms | 9.86 ms | 24.78 ms | 70.78 ms | 689.52 ms | 18613.33 | 
| nim                       | mofuw                     | 14.68 ms | 11.84 ms | 15.82 ms | 121.40 ms | 529.41 ms | 24612.67 | 
| python                    | japronto                  | 12.13 ms | 12.28 ms | 12.42 ms | 12.54 ms | 655.17 ms | 7956.67 | 
| swift                     | perfect                   | 15.04 ms | 15.71 ms | 16.50 ms | 17.69 ms | 158.94 ms | 2939.33 | 
| python                    | bottle                    | 21.69 ms | 16.91 ms | 42.65 ms | 55.29 ms | 655.53 ms | 17570.00 | 
| node                      | polka                     | 22.39 ms | 17.08 ms | 34.46 ms | 67.88 ms | 1913.69 ms | 47650.00 | 
| node                      | rayo                      | 22.02 ms | 17.22 ms | 35.27 ms | 64.67 ms | 1758.09 ms | 40176.67 | 
| swift                     | vapor                     | 30.57 ms | 19.69 ms | 28.56 ms | 52.60 ms | 4635.84 ms | 148842.00 | 
| node                      | fastify                   | 26.75 ms | 20.13 ms | 37.35 ms | 72.50 ms | 2267.80 ms | 66840.00 | 
| crystal                   | router.cr                 | 20.45 ms | 20.21 ms | 24.39 ms | 25.00 ms | 163.88 ms | 3338.00 | 
| crystal                   | raze                      | 20.44 ms | 20.63 ms | 25.13 ms | 25.68 ms | 306.78 ms | 4516.67 | 
| node                      | koa                       | 26.72 ms | 20.69 ms | 37.03 ms | 68.68 ms | 2390.75 ms | 64391.00 | 
| crystal                   | prism                     | 21.22 ms | 21.10 ms | 24.20 ms | 28.26 ms | 97.98 ms | 3892.00 | 
| crystal                   | kemal                     | 23.90 ms | 21.60 ms | 29.96 ms | 31.27 ms | 36.98 ms | 4178.67 | 
| crystal                   | raze                      | 20.44 ms | 20.63 ms | 25.13 ms | 25.68 ms | 306.78 ms | 4516.67 | 
| swift                     | kitura                    | 22.73 ms | 22.93 ms | 28.03 ms | 38.27 ms | 613.49 ms | 10865.00 | 
| crystal                   | lucky                     | 23.42 ms | 23.17 ms | 27.84 ms | 28.47 ms | 164.46 ms | 4023.33 | 
| node                      | restify                   | 27.88 ms | 23.39 ms | 45.98 ms | 76.61 ms | 1394.33 ms | 32354.67 | 
| go                        | gin                       | 29.11 ms | 23.46 ms | 76.43 ms | 160.30 ms | 451.78 ms | 36781.00 | 
| node                      | express                   | 31.41 ms | 25.03 ms | 43.35 ms | 72.94 ms | 2729.24 ms | 76069.67 | 
| crystal                   | amber                     | 26.21 ms | 28.96 ms | 30.01 ms | 31.07 ms | 35.41 ms | 4327.67 | 
| node                      | hapi                      | 41.31 ms | 30.95 ms | 54.83 ms | 91.36 ms | 3484.24 ms | 116688.00 | 
| python                    | aiohttp                   | 36.38 ms | 33.68 ms | 53.87 ms | 59.86 ms | 669.02 ms | 17789.00 | 
| python                    | flask                     | 53.88 ms | 40.93 ms | 99.81 ms | 114.13 ms | 745.96 ms | 30858.00 | 
| python                    | sanic                     | 69.83 ms | 53.62 ms | 121.45 ms | 149.56 ms | 505.97 ms | 43364.33 | 
| python                    | django                    | 87.90 ms | 77.12 ms | 133.97 ms | 149.52 ms | 1015.53 ms | 39544.33 | 
| python                    | tornado                   | 137.19 ms | 124.30 ms | 176.97 ms | 191.60 ms | 4020.56 ms | 165465.67 | 
| python                    | quart                     | 169.71 ms | 167.09 ms | 206.32 ms | 239.76 ms | 2140.20 ms | 71674.67 | 

#### Ranking (top 5)


:one: actix-web (rust)


:two: fasthttprouter (go)


:three: evhtp (cpp)


:four: vibora (python)


:five: act (java)


#### Full table

| Language (Runtime)        | Framework (Middleware)    |    Requests / s |      Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| rust                      | actix-web                 | 184603.67 | 209.73 MB |
| go                        | fasthttprouter            | 158651.67 | 255.46 MB |
| cpp                       | evhtp                     | 157318.00 | 152.74 MB |
| python                    | vibora                    | 155970.67 | 177.03 MB |
| java                      | act                       | 143975.67 | 245.90 MB |
| crystal                   | spider-gazelle            | 137383.67 | 147.01 MB |
| csharp                    | aspnetcore                | 116370.67 | 189.58 MB |
| rust                      | rocket                    | 108494.33 | 171.83 MB |
| rust                      | nickel                    | 102399.00 | 203.86 MB |
| rust                      | iron                      | 99904.00 | 126.36 MB |
| go                        | iris                      | 90818.67 | 121.46 MB |
| go                        | echo                      | 89956.00 | 157.93 MB |
| go                        | gorilla-mux               | 85115.67 | 113.55 MB |
| python                    | japronto                  | 82451.67 | 98.55 MB |
| scala                     | akkahttp                  | 75336.67 | 161.72 MB |
| nim                       | mofuw                     | 71478.00 | 125.36 MB |
| swift                     | perfect                   | 64976.33 | 61.08 MB |
| crystal                   | raze                      | 48253.00 | 45.34 MB |
| crystal                   | router.cr                 | 48805.00 | 45.87 MB |
| crystal                   | raze                      | 48253.00 | 45.34 MB |
| crystal                   | prism                     | 48077.00 | 53.38 MB |
| node                      | polka                     | 47123.33 | 70.58 MB |
| node                      | rayo                      | 46328.00 | 69.40 MB |
| python                    | bottle                    | 45999.00 | 113.22 MB |
| swift                     | vapor                     | 43995.67 | 74.27 MB |
| crystal                   | lucky                     | 43258.67 | 53.33 MB |
| swift                     | kitura                    | 43150.67 | 80.20 MB |
| crystal                   | kemal                     | 42296.00 | 69.08 MB |
| node                      | fastify                   | 42236.00 | 101.27 MB |
| ruby                      | roda                      | 42002.00 | 40.13 MB |
| node                      | koa                       | 40569.00 | 85.89 MB |
| crystal                   | amber                     | 38520.00 | 55.93 MB |
| node                      | restify                   | 36126.33 | 63.41 MB |
| php                       | symfony                   | 34209.67 | 170.28 MB |
| node                      | express                   | 34055.67 | 83.42 MB |
| go                        | gin                       | 33681.33 | 59.14 MB |
| php                       | laravel                   | 32511.00 | 162.01 MB |
| ruby                      | rack-routing              | 31157.33 | 18.02 MB |
| node                      | hapi                      | 27693.00 | 60.44 MB |
| python                    | aiohttp                   | 27382.00 | 62.09 MB |
| ruby                      | hanami                    | 19605.67 | 148.56 MB |
| ruby                      | flame                     | 19416.00 | 11.22 MB |
| python                    | flask                     | 18251.33 | 44.97 MB |
| ruby                      | sinatra                   | 16790.33 | 43.64 MB |
| python                    | sanic                     | 14255.33 | 25.45 MB |
| python                    | django                    | 11162.00 | 32.39 MB |
| python                    | tornado                   | 7069.33 | 18.78 MB |
| python                    | quart                     | 5677.33 | 11.68 MB |
| ruby                      | rails                     | 4218.33 | 12.92 MB |
| rust                      | actix-web                 | 184603.67 | 209.73 MB |
| go                        | fasthttprouter            | 158651.67 | 255.46 MB |
| cpp                       | evhtp                     | 157318.00 | 152.74 MB |
| python                    | vibora                    | 155970.67 | 177.03 MB |
| java                      | act                       | 143975.67 | 245.90 MB |
| crystal                   | spider-gazelle            | 137383.67 | 147.01 MB |
| csharp                    | aspnetcore                | 116370.67 | 189.58 MB |
| rust                      | rocket                    | 108494.33 | 171.83 MB |
| rust                      | nickel                    | 102399.00 | 203.86 MB |
| rust                      | iron                      | 99904.00 | 126.36 MB |
| go                        | iris                      | 90818.67 | 121.46 MB |
| go                        | echo                      | 89956.00 | 157.93 MB |
| go                        | gorilla-mux               | 85115.67 | 113.55 MB |
| python                    | japronto                  | 82451.67 | 98.55 MB |
| scala                     | akkahttp                  | 75336.67 | 161.72 MB |
| nim                       | mofuw                     | 71478.00 | 125.36 MB |
| swift                     | perfect                   | 64976.33 | 61.08 MB |
| crystal                   | raze                      | 48253.00 | 45.34 MB |
| crystal                   | router.cr                 | 48805.00 | 45.87 MB |
| crystal                   | raze                      | 48253.00 | 45.34 MB |
| crystal                   | prism                     | 48077.00 | 53.38 MB |
| node                      | polka                     | 47123.33 | 70.58 MB |
| node                      | rayo                      | 46328.00 | 69.40 MB |
| python                    | bottle                    | 45999.00 | 113.22 MB |
| swift                     | vapor                     | 43995.67 | 74.27 MB |
| crystal                   | lucky                     | 43258.67 | 53.33 MB |
| swift                     | kitura                    | 43150.67 | 80.20 MB |
| crystal                   | kemal                     | 42296.00 | 69.08 MB |
| node                      | fastify                   | 42236.00 | 101.27 MB |
| ruby                      | roda                      | 42002.00 | 40.13 MB |
| node                      | koa                       | 40569.00 | 85.89 MB |
| crystal                   | amber                     | 38520.00 | 55.93 MB |
| node                      | restify                   | 36126.33 | 63.41 MB |
| php                       | symfony                   | 34209.67 | 170.28 MB |
| node                      | express                   | 34055.67 | 83.42 MB |
| go                        | gin                       | 33681.33 | 59.14 MB |
| php                       | laravel                   | 32511.00 | 162.01 MB |
| ruby                      | rack-routing              | 31157.33 | 18.02 MB |
| node                      | hapi                      | 27693.00 | 60.44 MB |
| python                    | aiohttp                   | 27382.00 | 62.09 MB |
| ruby                      | hanami                    | 19605.67 | 148.56 MB |
| ruby                      | flame                     | 19416.00 | 11.22 MB |
| python                    | flask                     | 18251.33 | 44.97 MB |
| ruby                      | sinatra                   | 16790.33 | 43.64 MB |
| python                    | sanic                     | 14255.33 | 25.45 MB |
| python                    | django                    | 11162.00 | 32.39 MB |
| python                    | tornado                   | 7069.33 | 18.78 MB |
| python                    | quart                     | 5677.33 | 11.68 MB |
| ruby                      | rails                     | 4218.33 | 12.92 MB |
<!-- Result till here -->

## How to contribute ?

In any way you want ...

+ Request a framework addition
+ Report a bug (on any implementation)
+ Suggest an idea
+ ...

Any kind of idea is :heart:

## Contributors

- [Taichiro Suzuki](https://github.com/tbrand) - Author, maintainer
- [OvermindDL1](https://github.com/OvermindDL1) - Maintainer
- [Marwan Rabbâa](https://github.com/waghanza) - Mainainer

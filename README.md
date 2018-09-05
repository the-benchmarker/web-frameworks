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
Last update: 2018-09-05
```
OS: Linux (version: 4.16.11-100.fc26.x86_64, arch: x86_64)
CPU Cores: 8
```

<details open><summary>Ranked by latency (ordered by 50th percentile - lowest is better)</summary> 

| Language (Runtime)        | Framework (Middleware)    |         Average |  50% percentile |  90% percentile |  99% percentile | 99.9% percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust                      | nickel                    | 0.11 ms | 0.11 ms | 0.15 ms | 0.34 ms | 7.90 ms | 116.00 | 
| rust                      | rocket                    | 0.14 ms | 0.12 ms | 0.20 ms | 0.61 ms | 11.22 ms | 173.00 | 
| rust                      | iron                      | 0.60 ms | 0.46 ms | 1.06 ms | 3.36 ms | 25.19 ms | 699.00 | 
| ruby                      | roda                      | 2.76 ms | 1.89 ms | 5.45 ms | 17.68 ms | 90.21 ms | 3574.33 | 
| ruby                      | rack-routing              | 3.81 ms | 2.49 ms | 8.60 ms | 20.92 ms | 119.31 ms | 4329.67 | 
| ruby                      | hanami                    | 6.51 ms | 2.92 ms | 16.85 ms | 48.96 ms | 148.78 ms | 9883.67 | 
| php                       | symfony                   | 171.50 ms | 3.58 ms | 440.48 ms | 2319.56 ms | 7117.26 ms | 525533.67 | 
| ruby                      | flame                     | 6.77 ms | 3.66 ms | 16.78 ms | 39.90 ms | 119.48 ms | 8390.67 | 
| rust                      | actix-web                 | 4.85 ms | 4.22 ms | 9.02 ms | 15.06 ms | 36.06 ms | 3154.67 | 
| ruby                      | sinatra                   | 7.89 ms | 4.67 ms | 19.19 ms | 44.36 ms | 151.84 ms | 9370.67 | 
| cpp                       | evhtp                     | 5.74 ms | 4.87 ms | 9.95 ms | 17.52 ms | 46.31 ms | 3400.33 | 
| php                       | laravel                   | 241.44 ms | 4.91 ms | 558.83 ms | 4142.46 ms | 6324.56 ms | 779896.00 | 
| go                        | fasthttprouter            | 6.45 ms | 5.78 ms | 9.62 ms | 18.81 ms | 145.81 ms | 3806.33 | 
| crystal                   | spider-gazelle            | 6.96 ms | 5.87 ms | 12.29 ms | 21.46 ms | 58.73 ms | 4234.00 | 
| python                    | vibora                    | 7.21 ms | 6.09 ms | 13.71 ms | 23.51 ms | 50.33 ms | 4945.33 | 
| ruby                      | rails                     | 29.43 ms | 7.28 ms | 93.69 ms | 180.60 ms | 358.94 ms | 42912.67 | 
| java                      | act                       | 8.52 ms | 7.43 ms | 12.76 ms | 24.00 ms | 187.95 ms | 5942.00 | 
| scala                     | akkahttp                  | 198.02 ms | 9.08 ms | 27.31 ms | 4587.25 ms | 7923.44 ms | 810321.33 | 
| go                        | iris                      | 10.41 ms | 9.21 ms | 15.63 ms | 29.89 ms | 232.12 ms | 6981.33 | 
| csharp                    | aspnetcore                | 10.77 ms | 9.30 ms | 17.46 ms | 28.99 ms | 229.24 ms | 6745.00 | 
| go                        | echo                      | 10.67 ms | 9.36 ms | 16.56 ms | 32.29 ms | 179.06 ms | 6781.00 | 
| go                        | gorilla-mux               | 12.07 ms | 10.22 ms | 18.68 ms | 39.03 ms | 432.12 ms | 12434.33 | 
| nim                       | mofuw                     | 28.14 ms | 10.23 ms | 51.84 ms | 290.03 ms | 516.09 ms | 57266.33 | 
| swift                     | perfect                   | 15.89 ms | 15.90 ms | 18.25 ms | 21.70 ms | 244.59 ms | 4870.67 | 
| node                      | rayo                      | 24.51 ms | 16.16 ms | 39.94 ms | 161.95 ms | 820.25 ms | 40619.00 | 
| swift                     | vapor                     | 26.34 ms | 16.31 ms | 29.84 ms | 343.55 ms | 1710.22 ms | 75399.67 | 
| node                      | fastify                   | 28.21 ms | 16.86 ms | 37.68 ms | 327.13 ms | 1154.89 ms | 63601.67 | 
| node                      | polka                     | 38.07 ms | 18.58 ms | 48.73 ms | 605.50 ms | 1486.48 ms | 101655.33 | 
| python                    | japronto                  | 21.74 ms | 20.49 ms | 24.07 ms | 31.27 ms | 480.91 ms | 13492.00 | 
| node                      | restify                   | 30.89 ms | 22.71 ms | 47.62 ms | 157.11 ms | 807.06 ms | 39556.67 | 
| node                      | koa                       | 34.57 ms | 22.73 ms | 49.03 ms | 356.28 ms | 1169.25 ms | 65912.33 | 
| node                      | express                   | 46.09 ms | 25.32 ms | 58.12 ms | 665.21 ms | 1650.80 ms | 110891.00 | 
| crystal                   | raze                      | 29.98 ms | 27.09 ms | 39.59 ms | 48.44 ms | 166.45 ms | 6787.00 | 
| swift                     | kitura                    | 27.96 ms | 27.46 ms | 33.35 ms | 42.28 ms | 307.96 ms | 8321.67 | 
| crystal                   | router.cr                 | 30.82 ms | 27.67 ms | 41.30 ms | 49.56 ms | 300.59 ms | 9609.00 | 
| go                        | gin                       | 53.29 ms | 28.13 ms | 144.58 ms | 304.11 ms | 590.79 ms | 65684.67 | 
| crystal                   | prism                     | 32.54 ms | 29.87 ms | 42.18 ms | 52.47 ms | 261.23 ms | 9175.00 | 
| crystal                   | raze                      | 29.98 ms | 27.09 ms | 39.59 ms | 48.44 ms | 166.45 ms | 6787.00 | 
| node                      | hapi                      | 56.43 ms | 30.95 ms | 64.48 ms | 821.59 ms | 1753.35 ms | 131329.33 | 
| crystal                   | kemal                     | 38.42 ms | 34.28 ms | 49.95 ms | 61.61 ms | 627.04 ms | 23094.67 | 
| crystal                   | amber                     | 37.51 ms | 35.74 ms | 51.33 ms | 60.09 ms | 428.58 ms | 16716.67 | 
| crystal                   | lucky                     | 41.76 ms | 41.67 ms | 49.95 ms | 56.68 ms | 186.45 ms | 7907.00 | 
| python                    | flask                     | 57.45 ms | 42.27 ms | 107.73 ms | 196.96 ms | 488.25 ms | 38553.00 | 
| python                    | sanic                     | 58.04 ms | 53.62 ms | 99.53 ms | 156.45 ms | 283.37 ms | 31247.33 | 
| python                    | django                    | 84.49 ms | 61.29 ms | 176.63 ms | 231.90 ms | 671.95 ms | 54885.33 | 
| python                    | tornado                   | 121.61 ms | 110.59 ms | 162.37 ms | 499.78 ms | 1236.50 ms | 83021.00 | 

</details>


<details><summary>Ranked by requests (ordered by number or requests per sencond - highest is better)</summary>

| Language (Runtime)        | Framework (Middleware)    |    Requests / s |      Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| rust                      | actix-web                 | 190682.67 | 216.81 MB |
| cpp                       | evhtp                     | 163378.00 | 158.60 MB |
| python                    | vibora                    | 148099.00 | 168.17 MB |
| go                        | fasthttprouter            | 146274.00 | 235.79 MB |
| crystal                   | spider-gazelle            | 140155.00 | 149.91 MB |
| java                      | act                       | 127374.00 | 217.87 MB |
| rust                      | iron                      | 97527.33 | 122.74 MB |
| go                        | iris                      | 94731.00 | 126.53 MB |
| rust                      | rocket                    | 92835.67 | 147.51 MB |
| go                        | echo                      | 92132.33 | 161.60 MB |
| csharp                    | aspnetcore                | 91806.67 | 149.68 MB |
| go                        | gorilla-mux               | 84693.33 | 113.03 MB |
| nim                       | mofuw                     | 77491.67 | 135.89 MB |
| rust                      | nickel                    | 72626.33 | 144.43 MB |
| swift                     | perfect                   | 61614.00 | 57.99 MB |
| scala                     | akkahttp                  | 58212.67 | 124.99 MB |
| php                       | laravel                   | 55068.67 | 273.93 MB |
| swift                     | vapor                     | 52489.33 | 89.98 MB |
| node                      | fastify                   | 49858.33 | 118.37 MB |
| node                      | rayo                      | 49598.33 | 74.35 MB |
| python                    | japronto                  | 46762.00 | 55.75 MB |
| ruby                      | roda                      | 46149.33 | 44.07 MB |
| php                       | symfony                   | 45856.00 | 228.14 MB |
| node                      | polka                     | 43892.33 | 65.82 MB |
| node                      | koa                       | 37223.67 | 78.74 MB |
| node                      | restify                   | 36944.33 | 64.83 MB |
| swift                     | kitura                    | 35396.33 | 65.69 MB |
| ruby                      | rack-routing              | 33271.67 | 19.24 MB |
| node                      | express                   | 33026.67 | 80.85 MB |
| crystal                   | raze                      | 33012.00 | 30.96 MB |
| crystal                   | router.cr                 | 32061.67 | 30.05 MB |
| crystal                   | raze                      | 33012.00 | 30.96 MB |
| crystal                   | prism                     | 30840.00 | 34.20 MB |
| node                      | hapi                      | 28283.67 | 61.83 MB |
| go                        | gin                       | 27834.67 | 48.76 MB |
| crystal                   | amber                     | 26785.33 | 38.79 MB |
| crystal                   | kemal                     | 26369.00 | 42.95 MB |
| crystal                   | lucky                     | 23567.67 | 29.00 MB |
| ruby                      | hanami                    | 19667.67 | 148.72 MB |
| ruby                      | flame                     | 19101.67 | 11.03 MB |
| python                    | flask                     | 18737.00 | 46.01 MB |
| python                    | sanic                     | 17475.00 | 31.12 MB |
| ruby                      | sinatra                   | 16191.33 | 42.06 MB |
| python                    | django                    | 12351.00 | 35.77 MB |
| python                    | tornado                   | 8291.67 | 22.05 MB |
| ruby                      | rails                     | 4371.00 | 13.29 MB |

</details>

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

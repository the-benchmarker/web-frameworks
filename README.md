# Which is the fastest?

[![Build Status](https://travis-ci.com/tbrand/which_is_the_fastest.svg?branch=master)](https://travis-ci.com/tbrand/which_is_the_fastest)
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
Last update: 2018-07-25
```
OS: Linux (version: 4.17.7-200.fc28.x86_64, arch: x86_64)
CPU Cores: 4
```

<details open><summary>Ranked by latency</summary>

| Language (Runtime)        | Framework (Middleware)    |         Average |  50% percentile |  90% percentile |  99% percentile | 99.9% percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| rust                      | nickel                    | 0.04 ms | 0.03 ms | 0.04 ms | 0.09 ms | 6.11 ms | 89.00 | 
| rust                      | rocket                    | 0.07 ms | 0.06 ms | 0.09 ms | 0.20 ms | 12.53 ms | 167.00 | 
| rust                      | iron                      | 0.40 ms | 0.27 ms | 0.68 ms | 2.73 ms | 38.44 ms | 642.00 | 
| ruby                      | roda                      | 2.17 ms | 0.10 ms | 3.78 ms | 46.57 ms | 162.75 ms | 8375.00 | 
| ruby                      | rack-routing              | 2.17 ms | 0.17 ms | 5.31 ms | 34.47 ms | 150.60 ms | 6607.00 | 
| ruby                      | flame                     | 3.95 ms | 0.39 ms | 11.55 ms | 45.09 ms | 129.88 ms | 8782.00 | 
| ruby                      | hanami                    | 4.48 ms | 0.63 ms | 9.12 ms | 68.89 ms | 221.53 ms | 12671.00 | 
| ruby                      | sinatra                   | 5.43 ms | 0.71 ms | 15.05 ms | 65.31 ms | 194.47 ms | 12340.00 | 
| rust                      | actix-web                 | 5.82 ms | 5.19 ms | 11.20 ms | 18.38 ms | 206.37 ms | 4436.00 | 
| cpp                       | evhtp                     | 5.69 ms | 5.18 ms | 9.07 ms | 14.00 ms | 46.30 ms | 2640.00 | 
| python                    | vibora                    | 5.00 ms | 4.78 ms | 9.67 ms | 14.66 ms | 32.74 ms | 3387.00 | 
| nim                       | jester                    | 4.66 ms | 4.27 ms | 8.38 ms | 13.76 ms | 23.87 ms | 2786.00 | 
| java                      | act                       | 6.83 ms | 4.92 ms | 11.02 ms | 30.14 ms | 235.64 ms | 12263.00 | 
| csharp                    | aspnetcore                | 7.51 ms | 7.22 ms | 10.07 ms | 13.65 ms | 214.80 ms | 3166.00 | 
| go                        | fasthttprouter            | 7.31 ms | 6.82 ms | 11.16 ms | 18.31 ms | 214.43 ms | 4181.00 | 
| nim                       | mofuw                     | 10.95 ms | 10.08 ms | 13.94 ms | 29.59 ms | 59.92 ms | 3911.00 | 
| go                        | echo                      | 14.84 ms | 10.47 ms | 25.82 ms | 126.93 ms | 377.19 ms | 22503.00 | 
| go                        | iris                      | 13.82 ms | 11.15 ms | 23.41 ms | 96.37 ms | 372.83 ms | 18128.00 | 
| swift                     | perfect                   | 13.29 ms | 12.83 ms | 14.26 ms | 17.18 ms | 435.57 ms | 13719.00 | 
| go                        | gorilla-mux               | 14.43 ms | 10.41 ms | 25.16 ms | 118.71 ms | 449.63 ms | 22848.00 | 
| crystal                   | router.cr                 | 20.20 ms | 20.75 ms | 24.01 ms | 26.59 ms | 44.52 ms | 3221.00 | 
| crystal                   | lucky                     | 20.58 ms | 18.80 ms | 25.71 ms | 30.77 ms | 35.09 ms | 3649.00 | 
| crystal                   | spider-gazelle            | 21.22 ms | 22.59 ms | 24.73 ms | 27.94 ms | 33.07 ms | 3296.00 | 
| swift                     | kitura                    | 20.32 ms | 20.30 ms | 25.94 ms | 35.52 ms | 77.33 ms | 4957.00 | 
| crystal                   | amber                     | 19.89 ms | 18.53 ms | 25.14 ms | 28.26 ms | 34.64 ms | 3566.00 | 
| ruby                      | rails                     | 23.73 ms | 2.56 ms | 79.12 ms | 160.45 ms | 320.17 ms | 37550.00 | 
| crystal                   | kemal                     | 23.99 ms | 24.09 ms | 27.56 ms | 29.82 ms | 234.26 ms | 5535.00 | 
| node                      | polka                     | 32.70 ms | 14.91 ms | 30.42 ms | 667.03 ms | 1701.41 ms | 111728.00 | 
| node                      | restify                   | 39.86 ms | 23.12 ms | 51.44 ms | 536.35 ms | 1573.14 ms | 95956.00 | 
| node                      | rayo                      | 42.84 ms | 17.81 ms | 37.66 ms | 899.44 ms | 1884.33 ms | 142002.00 | 
| elixir                    | phoenix                   | 49.45 ms | 28.63 ms | 89.16 ms | 764.60 ms | 2089.14 ms | 139816.00 | 
| go                        | gin                       | 48.22 ms | 42.61 ms | 108.67 ms | 206.58 ms | 490.95 ms | 47477.00 | 
| python                    | flask                     | 49.38 ms | 47.22 ms | 59.68 ms | 70.38 ms | 676.85 ms | 26934.00 | 
| swift                     | vapor                     | 82.50 ms | 18.56 ms | 32.66 ms | 1926.73 ms | 3925.69 ms | 340386.00 | 
| python                    | sanic                     | 63.40 ms | 54.34 ms | 108.18 ms | 149.03 ms | 467.47 ms | 31007.00 | 
| node                      | fastify                   | 39.40 ms | 15.55 ms | 41.01 ms | 765.46 ms | 1777.40 ms | 124687.00 | 
| python                    | django                    | 75.57 ms | 69.79 ms | 99.33 ms | 118.95 ms | 1015.01 ms | 38245.00 | 
| node                      | koa                       | 108.47 ms | 23.44 ms | 63.01 ms | 2182.75 ms | 3191.92 ms | 364083.00 | 
| node                      | express                   | 112.26 ms | 27.32 ms | 72.22 ms | 2231.05 ms | 3666.91 ms | 372057.00 | 
| scala                     | akkahttp                  | 147.07 ms | 9.03 ms | 27.54 ms | 3957.54 ms | 7912.57 ms | 679564.00 | 
| php                       | symfony                   | 172.53 ms | 0.63 ms | 539.03 ms | 2187.55 ms | 6962.41 ms | 504933.00 | 
| python                    | tornado                   | 363.56 ms | 116.32 ms | 182.29 ms | 6313.69 ms | 7959.94 ms | 1032220.00 | 
| php                       | laravel                   | 222.39 ms | 0.34 ms | 618.55 ms | 3110.88 ms | 6994.28 ms | 632250.00 | 

</details>


<details><summary>Ranked by requests</summary>

| Language (Runtime)        | Framework (Middleware)    |    Requests / s |      Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| rust                      | actix-web                 | 149440.00 | 169.11 MB |
| python                    | vibora                    | 187304.00 | 212.00 MB |
| cpp                       | evhtp                     | 165003.00 | 158.83 MB |
| nim                       | jester                    | 191498.00 | 383.72 MB |
| java                      | act                       | 163536.00 | 278.73 MB |
| csharp                    | aspnetcore                | 126315.00 | 202.18 MB |
| go                        | fasthttprouter            | 125242.00 | 175.41 MB |
| rust                      | nickel                    | 114533.00 | 224.26 MB |
| rust                      | rocket                    | 104431.00 | 143.07 MB |
| nim                       | mofuw                     | 92005.00 | 161.12 MB |
| go                        | echo                      | 87792.00 | 153.76 MB |
| rust                      | iron                      | 78990.00 | 89.24 MB |
| go                        | iris                      | 81413.00 | 92.09 MB |
| go                        | gorilla-mux               | 90657.00 | 102.31 MB |
| swift                     | perfect                   | 76958.00 | 71.87 MB |
| scala                     | akkahttp                  | 60627.00 | 129.51 MB |
| node                      | polka                     | 58620.00 | 87.43 MB |
| crystal                   | router.cr                 | 48912.00 | 45.72 MB |
| node                      | rayo                      | 48225.00 | 71.85 MB |
| swift                     | vapor                     | 51106.00 | 57.61 MB |
| crystal                   | lucky                     | 48353.00 | 56.80 MB |
| crystal                   | spider-gazelle            | 46648.00 | 43.58 MB |
| crystal                   | amber                     | 49375.00 | 71.27 MB |
| swift                     | kitura                    | 47342.00 | 87.63 MB |
| node                      | fastify                   | 50723.00 | 75.58 MB |
| elixir                    | phoenix                   | 40497.00 | 87.18 MB |
| crystal                   | kemal                     | 40867.00 | 66.46 MB |
| node                      | restify                   | 35615.00 | 62.20 MB |
| ruby                      | roda                      | 32460.00 | 30.79 MB |
| node                      | koa                       | 35061.00 | 73.87 MB |
| php                       | symfony                   | 32259.00 | 160.66 MB |
| php                       | laravel                   | 31721.00 | 157.52 MB |
| node                      | express                   | 30776.00 | 74.98 MB |
| ruby                      | rack-routing              | 30342.00 | 17.36 MB |
| go                        | gin                       | 24244.00 | 42.35 MB |
| python                    | flask                     | 20186.00 | 49.50 MB |
| ruby                      | flame                     | 16120.00 | 9.22 MB |
| python                    | sanic                     | 15629.00 | 27.76 MB |
| ruby                      | hanami                    | 14351.00 | 108.50 MB |
| python                    | django                    | 13151.00 | 37.92 MB |
| ruby                      | sinatra                   | 11726.00 | 30.37 MB |
| python                    | tornado                   | 7987.00 | 23.07 MB |
| ruby                      | rails                     | 2683.00 | 7.07 MB |

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

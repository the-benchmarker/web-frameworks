# Which is the fastest?

[![Build Status](https://the-benchmarker.semaphoreci.com/badges/web-frameworks/branches/master.svg)](https://the-benchmarker.semaphoreci.com/projects/web-frameworks)

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

+ [Ruby](https://ruby-lang.org) as `built-in` tools are made in this language
+ [Docker](https://www.docker.com) as **frameworks** are `isolated` into _containers_
+ [wrk](https://github.com/wg/wrk) as benchmarking tool, `>= 4.1.0`
+ [postgresql](https://www.postgresql.org) to store data, `>= 10`

:information_source::information_source::information_source::information_source::information_source:

:warning: On `OSX` you need `docker-machine` to use `docker` containerization

~~~
brew install docker-machine
docker-machine create default
eval $(docker-machine env default)
~~~

:information_source::information_source::information_source::information_source::information_source:

## Usage

... to be documented ...

feel free to create an issue if you want to try this project

## Results

:information_source:  Updated on **2020-12-05** :information_source:

> Benchmarking with [wrk](https://github.com/wg/wrk)
   + Threads : 8
   + Timeout : 8
   + Duration : 15s (seconds)

:information_source: Sorted by max `req/s` on concurrency **64** :information_source:

|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) |
|----|----------|-----------|-----------:|------------:|------------:|
| 1 | java (11)| [jooby](https://jooby.io) (2.8) | 111 061.29 | 140 339.31 | 144 452.81 |
| 2 | java (11)| [rapidoid](https://rapidoid.org) (5.5) | 110 176.00 | 129 441.01 | 139 001.68 |
| 3 | java (11)| [light-4j](https://doc.networknt.com) (2.0) | 107 112.05 | 133 804.26 | 138 606.62 |
| 4 | java (11)| [act](https://actframework.org) (1.9) | 94 891.89 | 117 796.82 | 120 179.22 |
| 5 | java (11)| [restheart](https://restheart.org) (5.1) | 69 883.68 | 69 746.55 | 65 821.38 |
| 6 | java (11)| [spring-boot](https://spring.io/projects/spring-boot) (2.3) | 57 488.20 | 61 192.81 | 61 598.62 |
| 7 | java (11)| [javalin](https://javalin.io) (3.9) | 54 101.19 | 57 302.30 | 58 255.04 |
| 8 | java (11)| [spring-framework](https://spring.io/projects/spring-framework) (5.3) | 46 668.54 | 49 872.45 | 51 067.82 |
| 9 | java (11)| [micronaut](https://micronaut.io) (1.2) | 44 984.51 | 50 590.85 | 51 191.25 |
| 10 | java (11)| [quarkus](https://quarkus.io) (1.1) | 43 466.28 | 54 062.54 | 54 808.58 |
| 11 | java (11)| [struts2](https://struts.apache.org) (2.5) | 13 925.68 | 14 234.78 | 14 275.05 |
| 12 | java (11)| [blade](https://lets-blade.com) (2.0) | 12 987.31 | 16 141.92 | 14 701.63 |

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

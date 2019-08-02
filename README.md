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

+ Make framework list

~~~sh
bin/make config
~~~

+ Build containers

> jobs are either languages (example : crystal) or frameworks (example : router.cr)

~~~sh
bin/neph [job1] [job2] [job3] ...
~~~

+ Start the benchmark ....

> tools is a list of language / framework to challenge (example : ruby kemal amber go python)

~~~sh
bin/benchmarker [tools]
~~~

## Results

<!-- Result from here -->
Last update: 2019-08-02
```
OS: Linux (version: 5.1.20-300.fc30.x86_64, arch: x86_64)
CPU Cores: 4
```

### Latency


#### Ranking (top 5)


:one: slim (php)


:two: zend-expressive (php)


:three: symfony (php)


:four: zend-framework (php)


:five: lumen (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Average | 50th percentile | 90th percentile | 99th percentile | 99.9th percentile | Standard deviation |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| `php` (`7.3`) | [slim](https://slimframework.com) (**3.12**) | 73.64 ms | **11.40** ms | 192.72 ms | 879.47 ms | 6990.41 ms | **286646.00** | 
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 85.62 ms | **12.21** ms | 205.35 ms | 1567.75 ms | 5927.39 ms | **321051.67** | 
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 74.04 ms | **12.39** ms | 218.13 ms | 988.23 ms | 6022.82 ms | **241029.00** | 
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 73.25 ms | **12.99** ms | 223.49 ms | 827.80 ms | 5448.65 ms | **214155.00** | 
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 73.17 ms | **13.27** ms | 215.32 ms | 847.05 ms | 5327.47 ms | **219323.67** | 
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 100.27 ms | **16.42** ms | 329.37 ms | 1007.26 ms | 5171.60 ms | **261463.33** | 
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 20.80 ms | **19.10** ms | 35.69 ms | 63.31 ms | 162.45 ms | **12882.67** | 
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 43.11 ms | **43.08** ms | 60.13 ms | 81.59 ms | 133.84 ms | **15309.00** | 

### Requests per seconds


#### Ranking (top 5)


:one: (hyperf) (php)


:two: (slim) (php)


:three: (zend-expressive) (php)


:four: (symfony) (php)


:five: (zend-framework) (php)


#### Full table

| Language (Runtime) | Framework (Middleware) | Requests / s | Throughput |
|---------------------------|---------------------------|----------------:|---------:|
| `php` (`7.3`) | [hyperf](https://www.hyperf.io) (**1.0**) | 49142.67 | **104.75** MB |
| `php` (`7.3`) | [slim](https://slimframework.com) (**3.12**) | 44203.00 | **219.11** MB |
| `php` (`7.3`) | [zend-expressive](https://zendframework.github.io/zend-expressive) (**3.2**) | 42667.67 | **211.36** MB |
| `php` (`7.3`) | [symfony](https://symfony.com) (**4.3**) | 41622.67 | **206.33** MB |
| `php` (`7.3`) | [zend-framework](https://framework.zend.com) (**3.1**) | 39838.67 | **197.47** MB |
| `php` (`7.3`) | [lumen](https://lumen.laravel.com) (**5.8**) | 37273.67 | **193.67** MB |
| `php` (`7.3`) | [laravel](https://laravel.com) (**5.8**) | 29385.33 | **153.32** MB |
| `php` (`7.3`) | [swoft](https://swoft.org) (**2.0**) | 22944.00 | **60.16** MB |
<!-- Result till here -->

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

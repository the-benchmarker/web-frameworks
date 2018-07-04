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
Last update: 2018-07-05
```
OS: Linux (version: 4.16.11-100.fc26.x86_64, arch: x86_64)
CPU Cores: 8
```

### Ranking (Framework)

1. [actix-web](https://github.com/actix/actix-web) (rust)
2. [evhtp](https://github.com/criticalstack/libevhtp) (cpp)
3. [fasthttprouter](https://github.com/buaazp/fasthttprouter) (go)
4. [vibora](https://github.com/vibora-io/vibora) (python)
5. [mofuw](https://github.com/2vg/mofuw) (nim)
6. [act](https://github.com/actframework/actframework) (java)
7. [iris](https://github.com/kataras/iris) (go)
8. [iron](https://github.com/iron/iron) (rust)
9. [aspnetcore](https://github.com/aspnet/Home) (csharp)
10. [rocket](https://github.com/SergioBenitez/Rocket) (rust)
11. [echo](https://github.com/labstack/echo) (go)
12. [gorilla-mux](https://github.com/gorilla/mux) (go)
13. [polka](https://github.com/lukeed/polka) (node)
14. [rayo](https://github.com/GetRayo/rayo.js) (node)
15. [nickel](https://github.com/nickel-org/nickel.rs) (rust)
16. [fastify](https://github.com/fastify/fastify) (node)
17. [akkahttp](https://github.com/akka/akka-http) (scala)
18. [koa](https://github.com/koajs/koa) (node)
19. [perfect](https://github.com/PerfectlySoft/Perfect) (swift)
20. [restify](https://github.com/restify/node-restify) (node)
21. [vapor](https://github.com/vapor/vapor) (swift)
22. [express](https://github.com/expressjs/express) (node)
23. [symfony](https://github.com/symfony/symfony) (php)
24. [plug](https://github.com/elixir-lang/plug) (elixir)
25. [roda](https://github.com/jeremyevans/roda) (ruby)
26. [phoenix](https://github.com/phoenixframework/phoenix) (elixir)
27. [laravel](https://github.com/laravel/framework) (php)
28. [kitura](https://github.com/IBM-Swift/Kitura) (swift)
29. [rack-routing](https://github.com/georgeu2000/rack-routing) (ruby)
30. [router.cr](https://github.com/tbrand/router.cr) (crystal)
31. [spider-gazelle](https://github.com/spider-gazelle/spider-gazelle) (crystal)
32. [gin](https://github.com/gin-gonic/gin) (go)
33. [lucky](https://github.com/luckyframework/lucky) (crystal)
34. [kemal](https://github.com/kemalcr/kemal) (crystal)
35. [amber](https://github.com/amberframework/amber) (crystal)
36. [flame](https://github.com/AlexWayfer/flame) (ruby)
37. [flask](https://github.com/pallets/flask) (python)
38. [hanami](https://github.com/hanami/hanami) (ruby)
39. [sanic](https://github.com/channelcat/sanic) (python)
40. [sinatra](https://github.com/sinatra/sinatra) (ruby)
41. [jester](https://github.com/dom96/jester) (nim)
42. [django](https://github.com/django/django) (python)
43. [tornado](https://github.com/tornadoweb/tornado) (python)
44. [rails](https://github.com/rails/rails) (ruby)

### Ranking (Language)

1. rust ([actix-web](https://github.com/actix/actix-web))
2. cpp ([evhtp](https://github.com/criticalstack/libevhtp))
3. go ([fasthttprouter](https://github.com/buaazp/fasthttprouter))
4. python ([vibora](https://github.com/vibora-io/vibora))
5. nim ([mofuw](https://github.com/2vg/mofuw))
6. java ([act](https://github.com/actframework/actframework))
7. csharp ([aspnetcore](https://github.com/aspnet/Home))
8. node ([polka](https://github.com/lukeed/polka))
9. scala ([akkahttp](https://github.com/akka/akka-http))
10. swift ([perfect](https://github.com/PerfectlySoft/Perfect))
11. php ([symfony](https://github.com/symfony/symfony))
12. elixir ([plug](https://github.com/elixir-lang/plug))
13. ruby ([roda](https://github.com/jeremyevans/roda))
14. crystal ([router.cr](https://github.com/tbrand/router.cr))

### All frameworks

| Language (Runtime)        | Framework (Middleware)    |    Requests / s |         Latency |   99 percentile |      Throughput |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|-----------:|
| ruby                      | rails                     | 4929.00 | 26016.67 | 158555.33 | 4.52 MB |
| ruby                      | sinatra                   | 16960.67 | 7519.67 | 42064.67 | 14.24 MB |
| ruby                      | roda                      | 47429.00 | 2646.00 | 12115.00 | 14.64 MB |
| ruby                      | rack-routing              | 35453.00 | 3694.67 | 28968.33 | 6.64 MB |
| ruby                      | flame                     | 21039.00 | 6116.00 | 43857.33 | 3.87 MB |
| ruby                      | hanami                    | 19916.67 | 6450.00 | 43367.33 | 47.11 MB |
| crystal                   | kemal                     | 26241.67 | 38333.00 | 53639.00 | 11.97 MB |
| crystal                   | router.cr                 | 32949.33 | 30163.67 | 48114.67 | 8.83 MB |
| crystal                   | amber                     | 25937.67 | 41523.33 | 191221.67 | 11.03 MB |
| crystal                   | lucky                     | 28037.00 | 35525.67 | 54591.00 | 10.06 MB |
| crystal                   | spider-gazelle            | 32191.33 | 31266.67 | 50261.00 | 8.27 MB |
| go                        | echo                      | 97116.67 | 11351.33 | 50338.67 | 55.86 MB |
| go                        | gorilla-mux               | 91919.00 | 10792.00 | 33209.33 | 34.45 MB |
| go                        | iris                      | 104709.00 | 9592.33 | 27661.00 | 39.20 MB |
| go                        | fasthttprouter            | 169550.00 | 5936.00 | 17381.33 | 77.99 MB |
| go                        | gin                       | 28369.67 | 56173.00 | 313997.00 | 16.50 MB |
| rust                      | actix-web                 | 192814.33 | 4801.00 | 14455.33 | 72.33 MB |
| rust                      | iron                      | 104620.00 | 561.67 | 2507.00 | 39.56 MB |
| rust                      | nickel                    | 81319.67 | 103.00 | 297.33 | 48.74 MB |
| rust                      | rocket                    | 97964.33 | 139.67 | 829.67 | 41.30 MB |
| node                      | express                   | 55013.67 | 23975.67 | 264864.33 | 46.00 MB |
| node                      | fastify                   | 75240.00 | 17954.00 | 192346.00 | 75.39 MB |
| node                      | polka                     | 90443.33 | 14183.67 | 132408.33 | 45.32 MB |
| node                      | rayo                      | 87775.67 | 14052.33 | 113420.00 | 43.90 MB |
| node                      | koa                       | 61574.67 | 23661.00 | 342473.67 | 44.97 MB |
| node                      | restify                   | 58498.33 | 19266.67 | 127451.00 | 35.07 MB |
| elixir                    | plug                      | 49210.33 | 24896.33 | 142760.67 | 31.30 MB |
| elixir                    | phoenix                   | 45529.33 | 28456.67 | 211058.00 | 30.67 MB |
| swift                     | vapor                     | 55761.67 | 27291.00 | 441861.00 | 20.00 MB |
| swift                     | perfect                   | 59168.33 | 16478.67 | 20715.00 | 16.88 MB |
| swift                     | kitura                    | 35671.33 | 29004.00 | 85609.33 | 20.35 MB |
| scala                     | akkahttp                  | 64432.33 | 206881.33 | 4339488.67 | 49.93 MB |
| csharp                    | aspnetcore                | 99514.33 | 10163.67 | 30573.33 | 52.31 MB |
| python                    | sanic                     | 18267.00 | 56052.00 | 159447.33 | 10.71 MB |
| python                    | flask                     | 20657.67 | 52338.33 | 148850.67 | 17.11 MB |
| python                    | django                    | 12434.67 | 82708.33 | 198759.00 | 11.58 MB |
| python                    | tornado                   | 8666.67 | 112379.33 | 170080.00 | 6.16 MB |
| python                    | vibora                    | 144485.00 | 8425.67 | 22648.00 | 67.71 MB |
| nim                       | jester                    | 16642.33 | 240892.33 | 4590197.00 | 6.24 MB |
| nim                       | mofuw                     | 125784.67 | 9016.33 | 56692.00 | 71.40 MB |
| java                      | act                       | 125644.00 | 10037.67 | 68708.33 | 46.15 MB |
| cpp                       | evhtp                     | 170223.33 | 5423.33 | 14555.33 | 54.59 MB |
| php                       | symfony                   | 49262.33 | 180582.00 | 2693203.00 | 77.60 MB |
| php                       | laravel                   | 45492.67 | 256662.67 | 4052371.33 | 70.55 MB |
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

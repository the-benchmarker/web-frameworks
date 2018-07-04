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
Last update: 2018-07-04
```
OS: Linux (version: 4.17.3-200.fc28.x86_64, arch: x86_64)
CPU Cores: 4
```

### Ranking (Framework)

1. [actix-web](https://github.com/actix/actix-web) (rust)
2. [fasthttprouter](https://github.com/buaazp/fasthttprouter) (go)
3. [evhtp](https://github.com/criticalstack/libevhtp) (cpp)
4. [act](https://github.com/actframework/actframework) (java)
5. [aspnetcore](https://github.com/aspnet/Home) (csharp)
6. [mofuw](https://github.com/2vg/mofuw) (nim)
7. [rocket](https://github.com/SergioBenitez/Rocket) (rust)
8. [nickel](https://github.com/nickel-org/nickel.rs) (rust)
9. [iris](https://github.com/kataras/iris) (go)
10. [echo](https://github.com/labstack/echo) (go)
11. [gorilla-mux](https://github.com/gorilla/mux) (go)
12. [iron](https://github.com/iron/iron) (rust)
13. [japronto](https://github.com/squeaky-pl/japronto) (python)
14. [polka](https://github.com/lukeed/polka) (node)
15. [rayo](https://github.com/GetRayo/rayo.js) (node)
16. [akkahttp](https://github.com/akka/akka-http) (scala)
17. [fastify](https://github.com/fastify/fastify) (node)
18. [perfect](https://github.com/PerfectlySoft/Perfect) (swift)
19. [restify](https://github.com/restify/node-restify) (node)
20. [koa](https://github.com/koajs/koa) (node)
21. [router.cr](https://github.com/tbrand/router.cr) (crystal)
22. [express](https://github.com/expressjs/express) (node)
23. [spider-gazelle](https://github.com/spider-gazelle/spider-gazelle) (crystal)
24. [lucky](https://github.com/luckyframework/lucky) (crystal)
25. [vapor](https://github.com/vapor/vapor) (swift)
26. [plug](https://github.com/elixir-lang/plug) (elixir)
27. [kitura](https://github.com/IBM-Swift/Kitura) (swift)
28. [amber](https://github.com/amberframework/amber) (crystal)
29. [kemal](https://github.com/kemalcr/kemal) (crystal)
30. [phoenix](https://github.com/phoenixframework/phoenix) (elixir)
31. [roda](https://github.com/jeremyevans/roda) (ruby)
32. [gin](https://github.com/gin-gonic/gin) (go)
33. [symfony](https://github.com/symfony/symfony) (php)
34. [laravel](https://github.com/laravel/framework) (php)
35. [jester](https://github.com/dom96/jester) (nim)
36. [rack-routing](https://github.com/georgeu2000/rack-routing) (ruby)
37. [vibora](https://github.com/vibora-io/vibora) (python)
38. [hanami](https://github.com/hanami/hanami) (ruby)
39. [flask](https://github.com/pallets/flask) (python)
40. [flame](https://github.com/AlexWayfer/flame) (ruby)
41. [sinatra](https://github.com/sinatra/sinatra) (ruby)
42. [sanic](https://github.com/channelcat/sanic) (python)
43. [django](https://github.com/django/django) (python)
44. [tornado](https://github.com/tornadoweb/tornado) (python)
45. [rails](https://github.com/rails/rails) (ruby)

### Ranking (Language)

1. rust ([actix-web](https://github.com/actix/actix-web))
2. go ([fasthttprouter](https://github.com/buaazp/fasthttprouter))
3. cpp ([evhtp](https://github.com/criticalstack/libevhtp))
4. java ([act](https://github.com/actframework/actframework))
5. csharp ([aspnetcore](https://github.com/aspnet/Home))
6. nim ([mofuw](https://github.com/2vg/mofuw))
7. python ([japronto](https://github.com/squeaky-pl/japronto))
8. node ([polka](https://github.com/lukeed/polka))
9. scala ([akkahttp](https://github.com/akka/akka-http))
10. swift ([perfect](https://github.com/PerfectlySoft/Perfect))
11. crystal ([router.cr](https://github.com/tbrand/router.cr))
12. elixir ([plug](https://github.com/elixir-lang/plug))
13. ruby ([roda](https://github.com/jeremyevans/roda))
14. php ([symfony](https://github.com/symfony/symfony))

### All frameworks

| Language (Runtime)        | Framework (Middleware)    |    Requests / s |         Latency |   99 percentile |      Throughput |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|-----------:|
| ruby                      | rails                     | 3897.33 | 16534.67 | 106263.33 | 3.09 MB |
| ruby                      | sinatra                   | 14916.00 | 4291.67 | 46616.00 | 12.63 MB |
| ruby                      | roda                      | 36602.67 | 1832.33 | 29363.00 | 11.86 MB |
| ruby                      | rack-routing              | 28413.00 | 2313.00 | 36260.00 | 5.00 MB |
| ruby                      | flame                     | 17715.33 | 3633.67 | 43025.67 | 3.41 MB |
| ruby                      | hanami                    | 18842.33 | 3393.67 | 43576.67 | 44.56 MB |
| crystal                   | kemal                     | 41926.00 | 23992.67 | 27337.67 | 19.55 MB |
| crystal                   | router.cr                 | 52502.00 | 19016.33 | 25307.00 | 14.52 MB |
| crystal                   | amber                     | 42250.67 | 23734.33 | 31710.00 | 16.82 MB |
| crystal                   | lucky                     | 45144.67 | 21964.33 | 30965.67 | 16.91 MB |
| crystal                   | spider-gazelle            | 45683.33 | 22070.67 | 30138.33 | 12.40 MB |
| go                        | echo                      | 91679.33 | 14884.00 | 130063.00 | 52.89 MB |
| go                        | gorilla-mux               | 88578.33 | 15630.33 | 128265.33 | 33.25 MB |
| go                        | iris                      | 96490.67 | 14569.00 | 146986.00 | 36.57 MB |
| go                        | fasthttprouter            | 163260.00 | 5460.00 | 13554.33 | 75.39 MB |
| go                        | gin                       | 35908.00 | 34265.33 | 156813.67 | 18.90 MB |
| rust                      | actix-web                 | 181606.00 | 4728.00 | 14981.00 | 70.86 MB |
| rust                      | iron                      | 87743.33 | 349.33 | 1728.67 | 35.01 MB |
| rust                      | nickel                    | 103994.33 | 42.67 | 51.33 | 68.41 MB |
| rust                      | rocket                    | 105134.00 | 70.00 | 189.00 | 48.76 MB |
| node                      | express                   | 46936.67 | 48406.00 | 887460.33 | 38.92 MB |
| node                      | fastify                   | 65194.33 | 28248.67 | 534193.00 | 66.23 MB |
| node                      | polka                     | 78936.00 | 20722.67 | 349554.00 | 37.99 MB |
| node                      | rayo                      | 76292.67 | 19695.33 | 291678.00 | 37.52 MB |
| node                      | koa                       | 53626.33 | 39260.67 | 717946.00 | 38.31 MB |
| node                      | restify                   | 58412.00 | 21364.67 | 170594.67 | 34.25 MB |
| elixir                    | plug                      | 43322.33 | 51003.67 | 697155.67 | 27.79 MB |
| elixir                    | phoenix                   | 39391.67 | 40366.33 | 339262.00 | 26.50 MB |
| swift                     | vapor                     | 45001.67 | 61975.00 | 1368733.67 | 16.37 MB |
| swift                     | perfect                   | 63277.33 | 15729.00 | 19385.33 | 16.96 MB |
| swift                     | kitura                    | 42841.33 | 24383.67 | 100690.67 | 26.31 MB |
| scala                     | akkahttp                  | 67532.67 | 253080.67 | 4929728.00 | 50.81 MB |
| csharp                    | aspnetcore                | 116722.33 | 8890.67 | 17008.67 | 60.83 MB |
| python                    | sanic                     | 14774.33 | 67164.67 | 125040.67 | 8.81 MB |
| python                    | japronto                  | 82049.67 | 12384.33 | 15956.33 | 32.21 MB |
| python                    | flask                     | 17787.00 | 59817.67 | 191245.00 | 15.38 MB |
| python                    | django                    | 11619.00 | 86205.00 | 197110.33 | 11.18 MB |
| python                    | tornado                   | 6978.33 | 228052.00 | 2947652.33 | 4.83 MB |
| python                    | vibora                    | 28263.00 | 51878.33 | 103748.00 | 20.07 MB |
| nim                       | jester                    | 29081.00 | 76547.00 | 1686765.67 | 10.50 MB |
| nim                       | mofuw                     | 115072.00 | 9772.00 | 46172.67 | 64.39 MB |
| java                      | act                       | 138152.33 | 9633.33 | 116304.00 | 74.13 MB |
| cpp                       | evhtp                     | 156938.67 | 5784.33 | 14014.33 | 50.92 MB |
| php                       | symfony                   | 32745.33 | 192165.67 | 2652773.33 | 52.94 MB |
| php                       | laravel                   | 30293.67 | 261373.00 | 3585675.67 | 47.73 MB |
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

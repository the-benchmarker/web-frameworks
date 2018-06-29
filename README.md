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
Last update: 2018-06-29
```
OS: Linux (version: 4.16.11-100.fc26.x86_64, arch: x86_64)
CPU Cores: 8
```

### Ranking (Framework)

1. [actix-web](https://github.com/actix/actix-web) (rust)
2. [fasthttprouter](https://github.com/buaazp/fasthttprouter) (go)
3. [evhtp](https://github.com/criticalstack/libevhtp) (cpp)
4. [act](https://github.com/actframework/actframework) (java)
5. [iris](https://github.com/kataras/iris) (go)
6. [mofuw](https://github.com/2vg/mofuw) (nim)
7. [echo](https://github.com/labstack/echo) (go)
8. [gorilla-mux](https://github.com/gorilla/mux) (go)
9. [iron](https://github.com/iron/iron) (rust)
10. [aspnetcore](https://github.com/aspnet/Home) (csharp)
11. [rocket](https://github.com/SergioBenitez/Rocket) (rust)
12. [polka](https://github.com/lukeed/polka) (node)
13. [rayo](https://github.com/GetRayo/rayo.js) (node)
14. [nickel](https://github.com/nickel-org/nickel.rs) (rust)
15. [fastify](https://github.com/fastify/fastify) (node)
16. [koa](https://github.com/koajs/koa) (node)
17. [restify](https://github.com/restify/node-restify) (node)
18. [perfect](https://github.com/PerfectlySoft/Perfect) (swift)
19. [akkahttp](https://github.com/akka/akka-http) (scala)
20. [plug](https://github.com/elixir-lang/plug) (elixir)
21. [vapor](https://github.com/vapor/vapor) (swift)
22. [express](https://github.com/expressjs/express) (node)
23. [japronto](https://github.com/squeaky-pl/japronto) (python)
24. [symfony](https://github.com/symfony/symfony) (php)
25. [laravel](https://github.com/laravel/framework) (php)
26. [phoenix](https://github.com/phoenixframework/phoenix) (elixir)
27. [roda](https://github.com/jeremyevans/roda) (ruby)
28. [kitura](https://github.com/IBM-Swift/Kitura) (swift)
29. [router.cr](https://github.com/tbrand/router.cr) (crystal)
30. [rack-routing](https://github.com/georgeu2000/rack-routing) (ruby)
31. [spider-gazelle](https://github.com/spider-gazelle/spider-gazelle) (crystal)
32. [gin](https://github.com/gin-gonic/gin) (go)
33. [kemal](https://github.com/kemalcr/kemal) (crystal)
34. [lucky](https://github.com/luckyframework/lucky) (crystal)
35. [amber](https://github.com/amberframework/amber) (crystal)
36. [flask](https://github.com/pallets/flask) (python)
37. [sinatra](https://github.com/sinatra/sinatra) (ruby)
38. [flame](https://github.com/AlexWayfer/flame) (ruby)
39. [sanic](https://github.com/channelcat/sanic) (python)
40. [hanami](https://github.com/hanami/hanami) (ruby)
41. [jester](https://github.com/dom96/jester) (nim)
42. [django](https://github.com/django/django) (python)
43. [rails](https://github.com/rails/rails) (ruby)
44. [tornado](https://github.com/tornadoweb/tornado) (python)

### Ranking (Language)

1. rust ([actix-web](https://github.com/actix/actix-web))
2. go ([fasthttprouter](https://github.com/buaazp/fasthttprouter))
3. cpp ([evhtp](https://github.com/criticalstack/libevhtp))
4. java ([act](https://github.com/actframework/actframework))
5. nim ([mofuw](https://github.com/2vg/mofuw))
6. csharp ([aspnetcore](https://github.com/aspnet/Home))
7. node ([polka](https://github.com/lukeed/polka))
8. swift ([perfect](https://github.com/PerfectlySoft/Perfect))
9. scala ([akkahttp](https://github.com/akka/akka-http))
10. elixir ([plug](https://github.com/elixir-lang/plug))
11. python ([japronto](https://github.com/squeaky-pl/japronto))
12. php ([symfony](https://github.com/symfony/symfony))
13. ruby ([roda](https://github.com/jeremyevans/roda))
14. crystal ([router.cr](https://github.com/tbrand/router.cr))

### All frameworks

| Language (Runtime)        | Framework (Middleware)    |    Requests / s |         Latency |   99 percentile |      Throughput |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|-----------:|
| ruby                      | rails                     | 4778.67 | 26821.00 | 164288.67 | 4.38 MB |
| ruby                      | sinatra                   | 17548.67 | 7313.00 | 46293.00 | 13.91 MB |
| ruby                      | roda                      | 38444.00 | 3424.00 | 26214.00 | 12.15 MB |
| ruby                      | rack-routing              | 28732.67 | 4610.67 | 39270.00 | 5.47 MB |
| ruby                      | flame                     | 17140.00 | 7500.00 | 58117.00 | 3.25 MB |
| ruby                      | hanami                    | 16721.67 | 7782.00 | 66469.00 | 36.23 MB |
| crystal                   | kemal                     | 25841.33 | 38456.67 | 57598.67 | 11.98 MB |
| crystal                   | router.cr                 | 29928.00 | 33320.67 | 48460.00 | 7.88 MB |
| crystal                   | amber                     | 24124.33 | 41460.67 | 56444.33 | 9.88 MB |
| crystal                   | lucky                     | 24758.67 | 45379.67 | 280317.33 | 8.00 MB |
| crystal                   | spider-gazelle            | 26664.67 | 37092.67 | 52950.00 | 7.44 MB |
| go                        | echo                      | 96059.00 | 10726.00 | 32985.33 | 55.95 MB |
| go                        | gorilla-mux               | 87399.00 | 12239.33 | 41149.00 | 34.49 MB |
| go                        | iris                      | 107148.00 | 9256.00 | 27514.33 | 37.18 MB |
| go                        | fasthttprouter            | 161418.00 | 6012.67 | 17740.33 | 77.09 MB |
| go                        | gin                       | 25868.33 | 52520.67 | 235061.33 | 15.20 MB |
| rust                      | actix-web                 | 162924.00 | 5786.67 | 18794.33 | 60.76 MB |
| rust                      | iron                      | 86538.00 | 685.67 | 5064.67 | 33.96 MB |
| rust                      | nickel                    | 73094.00 | 120.33 | 713.00 | 42.87 MB |
| rust                      | rocket                    | 79408.33 | 184.33 | 1829.00 | 34.22 MB |
| node                      | express                   | 47287.67 | 32237.00 | 485855.33 | 41.39 MB |
| node                      | fastify                   | 71667.67 | 19827.67 | 238551.00 | 69.34 MB |
| node                      | polka                     | 77670.33 | 17336.67 | 191144.33 | 38.59 MB |
| node                      | rayo                      | 74175.00 | 18304.67 | 217529.67 | 36.55 MB |
| node                      | koa                       | 62014.33 | 24622.67 | 354717.33 | 45.27 MB |
| node                      | restify                   | 57886.00 | 21700.00 | 207967.33 | 34.64 MB |
| elixir                    | plug                      | 49185.00 | 23794.67 | 123746.00 | 31.37 MB |
| elixir                    | phoenix                   | 38582.33 | 34195.00 | 339589.67 | 26.34 MB |
| swift                     | vapor                     | 49036.67 | 27874.33 | 391829.67 | 17.75 MB |
| swift                     | perfect                   | 53096.67 | 18467.00 | 24376.00 | 15.10 MB |
| swift                     | kitura                    | 30893.00 | 34033.67 | 101858.00 | 17.43 MB |
| scala                     | akkahttp                  | 51161.33 | 221500.00 | 4325494.67 | 39.99 MB |
| csharp                    | aspnetcore                | 82491.00 | 15392.00 | 148221.33 | 42.94 MB |
| python                    | sanic                     | 16817.00 | 61676.00 | 187775.00 | 9.85 MB |
| python                    | japronto                  | 45846.00 | 21982.00 | 30042.33 | 18.70 MB |
| python                    | flask                     | 17697.67 | 56331.33 | 115950.67 | 14.39 MB |
| python                    | django                    | 9852.33 | 101829.00 | 245963.00 | 9.23 MB |
| python                    | tornado                   | 1308.33 | 766829.00 | 5012888.33 | 0.89 MB |
| nim                       | jester                    | 16074.00 | 242181.33 | 4648949.33 | 5.90 MB |
| nim                       | mofuw                     | 101243.00 | 14912.67 | 128445.67 | 57.16 MB |
| java                      | act                       | 108475.67 | 10534.33 | 47621.33 | 42.92 MB |
| cpp                       | evhtp                     | 154440.67 | 6030.33 | 18452.00 | 46.85 MB |
| php                       | symfony                   | 42104.00 | 217780.67 | 3430583.33 | 66.54 MB |
| php                       | laravel                   | 39466.67 | 283421.33 | 4087190.33 | 61.15 MB |
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

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
OS: Linux (version: 4.16.11-100.fc26.x86_64, arch: x86_64)
CPU Cores: 8
```

### Ranking (Framework)

1. [actix-web](https://github.com/actix/actix-web) (rust)
2. [evhtp](https://github.com/criticalstack/libevhtp) (cpp)
3. [fasthttprouter](https://github.com/buaazp/fasthttprouter) (go)
4. [act](https://github.com/actframework/actframework) (java)
5. [mofuw](https://github.com/2vg/mofuw) (nim)
6. [iris](https://github.com/kataras/iris) (go)
7. [iron](https://github.com/iron/iron) (rust)
8. [aspnetcore](https://github.com/aspnet/Home) (csharp)
9. [echo](https://github.com/labstack/echo) (go)
10. [gorilla-mux](https://github.com/gorilla/mux) (go)
11. [polka](https://github.com/lukeed/polka) (node)
12. [rayo](https://github.com/GetRayo/rayo.js) (node)
13. [rocket](https://github.com/SergioBenitez/Rocket) (rust)
14. [fastify](https://github.com/fastify/fastify) (node)
15. [nickel](https://github.com/nickel-org/nickel.rs) (rust)
16. [akkahttp](https://github.com/akka/akka-http) (scala)
17. [koa](https://github.com/koajs/koa) (node)
18. [perfect](https://github.com/PerfectlySoft/Perfect) (swift)
19. [restify](https://github.com/restify/node-restify) (node)
20. [vapor](https://github.com/vapor/vapor) (swift)
21. [express](https://github.com/expressjs/express) (node)
22. [plug](https://github.com/elixir-lang/plug) (elixir)
23. [japronto](https://github.com/squeaky-pl/japronto) (python)
24. [symfony](https://github.com/symfony/symfony) (php)
25. [roda](https://github.com/jeremyevans/roda) (ruby)
26. [laravel](https://github.com/laravel/framework) (php)
27. [phoenix](https://github.com/phoenixframework/phoenix) (elixir)
28. [kitura](https://github.com/IBM-Swift/Kitura) (swift)
29. [rack-routing](https://github.com/georgeu2000/rack-routing) (ruby)
30. [router.cr](https://github.com/tbrand/router.cr) (crystal)
31. [spider-gazelle](https://github.com/spider-gazelle/spider-gazelle) (crystal)
32. [lucky](https://github.com/luckyframework/lucky) (crystal)
33. [gin](https://github.com/gin-gonic/gin) (go)
34. [amber](https://github.com/amberframework/amber) (crystal)
35. [kemal](https://github.com/kemalcr/kemal) (crystal)
36. [hanami](https://github.com/hanami/hanami) (ruby)
37. [vibora](https://github.com/vibora-io/vibora) (python)
38. [flask](https://github.com/pallets/flask) (python)
39. [flame](https://github.com/AlexWayfer/flame) (ruby)
40. [sanic](https://github.com/channelcat/sanic) (python)
41. [sinatra](https://github.com/sinatra/sinatra) (ruby)
42. [jester](https://github.com/dom96/jester) (nim)
43. [django](https://github.com/django/django) (python)
44. [tornado](https://github.com/tornadoweb/tornado) (python)
45. [rails](https://github.com/rails/rails) (ruby)

### Ranking (Language)

1. rust ([actix-web](https://github.com/actix/actix-web))
2. cpp ([evhtp](https://github.com/criticalstack/libevhtp))
3. go ([fasthttprouter](https://github.com/buaazp/fasthttprouter))
4. java ([act](https://github.com/actframework/actframework))
5. nim ([mofuw](https://github.com/2vg/mofuw))
6. csharp ([aspnetcore](https://github.com/aspnet/Home))
7. node ([polka](https://github.com/lukeed/polka))
8. scala ([akkahttp](https://github.com/akka/akka-http))
9. swift ([perfect](https://github.com/PerfectlySoft/Perfect))
10. elixir ([plug](https://github.com/elixir-lang/plug))
11. python ([japronto](https://github.com/squeaky-pl/japronto))
12. php ([symfony](https://github.com/symfony/symfony))
13. ruby ([roda](https://github.com/jeremyevans/roda))
14. crystal ([router.cr](https://github.com/tbrand/router.cr))

### All frameworks

| Language (Runtime)        | Framework (Middleware)    |    Requests / s |         Latency |   99 percentile |      Throughput |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|-----------:|
| ruby                      | rails                     | 4761.00 | 26919.67 | 162862.67 | 4.38 MB |
| ruby                      | sinatra                   | 17283.00 | 7399.33 | 56004.00 | 15.21 MB |
| ruby                      | roda                      | 46700.00 | 2770.67 | 19497.00 | 15.24 MB |
| ruby                      | rack-routing              | 34916.67 | 3635.00 | 19003.00 | 6.48 MB |
| ruby                      | flame                     | 20518.33 | 6234.00 | 28251.00 | 3.89 MB |
| ruby                      | hanami                    | 22345.00 | 5770.67 | 43889.67 | 51.21 MB |
| crystal                   | kemal                     | 25200.00 | 43311.33 | 234106.67 | 12.07 MB |
| crystal                   | router.cr                 | 33093.67 | 30103.00 | 46789.67 | 8.82 MB |
| crystal                   | amber                     | 27303.33 | 37140.33 | 99846.67 | 12.55 MB |
| crystal                   | lucky                     | 29143.67 | 34123.33 | 49553.00 | 9.72 MB |
| crystal                   | spider-gazelle            | 30593.67 | 32559.00 | 50359.67 | 8.25 MB |
| go                        | echo                      | 97339.33 | 11602.67 | 71843.00 | 56.07 MB |
| go                        | gorilla-mux               | 92514.33 | 10994.67 | 34318.67 | 34.67 MB |
| go                        | iris                      | 112012.67 | 9006.33 | 26323.33 | 43.51 MB |
| go                        | fasthttprouter            | 169950.67 | 5454.67 | 15925.33 | 78.12 MB |
| go                        | gin                       | 28922.33 | 52454.33 | 231539.00 | 16.77 MB |
| rust                      | actix-web                 | 194316.33 | 4767.67 | 14369.00 | 72.55 MB |
| rust                      | iron                      | 105054.33 | 560.00 | 2488.33 | 39.78 MB |
| rust                      | nickel                    | 78394.33 | 104.33 | 225.00 | 52.50 MB |
| rust                      | rocket                    | 82891.00 | 153.33 | 637.00 | 41.44 MB |
| node                      | express                   | 55345.00 | 29060.00 | 486954.33 | 46.56 MB |
| node                      | fastify                   | 79699.33 | 19169.33 | 286130.67 | 80.91 MB |
| node                      | polka                     | 90225.00 | 14501.67 | 150340.00 | 45.46 MB |
| node                      | rayo                      | 88797.67 | 13874.00 | 114112.00 | 44.60 MB |
| node                      | koa                       | 62054.33 | 24214.33 | 340715.33 | 45.39 MB |
| node                      | restify                   | 58492.33 | 20345.00 | 167351.33 | 34.91 MB |
| elixir                    | plug                      | 51159.00 | 21783.00 | 102740.00 | 34.00 MB |
| elixir                    | phoenix                   | 44914.33 | 29444.33 | 303656.00 | 30.46 MB |
| swift                     | vapor                     | 55961.00 | 32165.33 | 654217.00 | 20.07 MB |
| swift                     | perfect                   | 61050.00 | 16163.67 | 20522.67 | 17.37 MB |
| swift                     | kitura                    | 36429.00 | 26424.00 | 31915.33 | 20.63 MB |
| scala                     | akkahttp                  | 63011.33 | 241451.00 | 4826857.00 | 48.75 MB |
| csharp                    | aspnetcore                | 99944.00 | 11140.00 | 75752.00 | 52.93 MB |
| python                    | sanic                     | 18337.33 | 55557.00 | 158447.67 | 10.73 MB |
| python                    | japronto                  | 50401.00 | 22917.33 | 118136.67 | 20.03 MB |
| python                    | flask                     | 20605.33 | 49153.67 | 134906.67 | 16.69 MB |
| python                    | django                    | 12942.67 | 79532.67 | 200013.00 | 12.33 MB |
| python                    | tornado                   | 8676.33 | 112013.67 | 202921.67 | 6.21 MB |
| python                    | vibora                    | 21077.00 | 94541.00 | 287450.67 | 13.42 MB |
| nim                       | jester                    | 17260.67 | 229040.00 | 4512341.33 | 6.29 MB |
| nim                       | mofuw                     | 118914.00 | 12810.00 | 107716.00 | 67.20 MB |
| java                      | act                       | 128895.33 | 9148.00 | 39106.00 | 47.11 MB |
| cpp                       | evhtp                     | 170421.00 | 5425.33 | 15044.67 | 54.48 MB |
| php                       | symfony                   | 49477.33 | 175734.33 | 2729404.33 | 77.60 MB |
| php                       | laravel                   | 46615.33 | 240620.33 | 3593686.33 | 71.65 MB |
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

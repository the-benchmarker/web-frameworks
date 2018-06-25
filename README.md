# Which is the fastest?

Measuring response times (routing times) for each framework (middleware).
Each framework has to have two features; routing and parsing path parameters.

Any idea is :heart:, let discuss about it on [![Join the chat at https://gitter.im/which_is_the_fastest/Lobby](https://badges.gitter.im/which_is_the_fastest/Lobby.svg)](https://gitter.im/which_is_the_fastest/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Result

<!-- Result from here -->
Last update: 2018-06-25
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
6. [iron](https://github.com/iron/iron) (rust)
7. [rocket](https://github.com/SergioBenitez/Rocket) (rust)
8. [iris](https://github.com/kataras/iris) (go)
9. [echo](https://github.com/labstack/echo) (go)
10. [aspnetcore](https://github.com/aspnet/Home) (csharp)
11. [polka](https://github.com/lukeed/polka) (node)
12. [gorilla-mux](https://github.com/gorilla/mux) (go)
13. [nickel](https://github.com/nickel-org/nickel.rs) (rust)
14. [rayo](https://github.com/GetRayo/rayo.js) (node)
15. [fastify](https://github.com/fastify/fastify) (node)
16. [akkahttp](https://github.com/akka/akka-http) (scala)
17. [perfect](https://github.com/PerfectlySoft/Perfect) (swift)
18. [express](https://github.com/expressjs/express) (node)
19. [koa](https://github.com/koajs/koa) (node)
20. [restify](https://github.com/restify/node-restify) (node)
21. [symfony](https://github.com/symfony/symfony) (php)
22. [vapor](https://github.com/vapor/vapor) (swift)
23. [japronto](https://github.com/squeaky-pl/japronto) (python)
24. [laravel](https://github.com/laravel/framework) (php)
25. [plug](https://github.com/elixir-lang/plug) (elixir)
26. [roda](https://github.com/jeremyevans/roda) (ruby)
27. [phoenix](https://github.com/phoenixframework/phoenix) (elixir)
28. [rack-routing](https://github.com/georgeu2000/rack-routing) (ruby)
29. [kitura](https://github.com/IBM-Swift/Kitura) (swift)
30. [router.cr](https://github.com/tbrand/router.cr) (crystal)
31. [spider-gazelle](https://github.com/spider-gazelle/spider-gazelle) (crystal)
32. [amber](https://github.com/amberframework/amber) (crystal)
33. [gin](https://github.com/gin-gonic/gin) (go)
34. [lucky](https://github.com/luckyframework/lucky) (crystal)
35. [kemal](https://github.com/kemalcr/kemal) (crystal)
36. [flame](https://github.com/AlexWayfer/flame) (ruby)
37. [hanami](https://github.com/hanami/hanami) (ruby)
38. [sinatra](https://github.com/sinatra/sinatra) (ruby)
39. [flask](https://github.com/pallets/flask) (python)
40. [sanic](https://github.com/channelcat/sanic) (python)
41. [jester](https://github.com/dom96/jester) (nim)
42. [django](https://github.com/django/django) (python)
43. [rails](https://github.com/rails/rails) (ruby)
44. [tornado](https://github.com/tornadoweb/tornado) (python)

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
10. php ([symfony](https://github.com/symfony/symfony))
11. python ([japronto](https://github.com/squeaky-pl/japronto))
12. elixir ([plug](https://github.com/elixir-lang/plug))
13. ruby ([roda](https://github.com/jeremyevans/roda))
14. crystal ([router.cr](https://github.com/tbrand/router.cr))

### All frameworks

| Language (Runtime)        | Framework (Middleware)    |    Requests / s |         Latency |   99 percentile |      Throughput |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|-----------:|
| ruby                      | rails                     | 5341.33 | 24009.00 | 148917.33 | 4.92 MB |
| ruby                      | sinatra                   | 18576.33 | 6888.33 | 37867.67 | 15.65 MB |
| ruby                      | roda                      | 45817.00 | 2750.67 | 13510.33 | 15.28 MB |
| ruby                      | rack-routing              | 36130.00 | 3529.33 | 18580.33 | 6.41 MB |
| ruby                      | flame                     | 21630.67 | 5980.33 | 34563.00 | 3.78 MB |
| ruby                      | hanami                    | 19777.67 | 6495.33 | 50932.67 | 49.86 MB |
| crystal                   | kemal                     | 26464.00 | 38453.67 | 97378.67 | 14.46 MB |
| crystal                   | router.cr                 | 33809.67 | 31632.33 | 127451.67 | 9.22 MB |
| crystal                   | amber                     | 28919.67 | 33765.67 | 53829.00 | 12.91 MB |
| crystal                   | lucky                     | 26826.67 | 36802.67 | 54024.33 | 10.48 MB |
| crystal                   | spider-gazelle            | 31686.67 | 31566.33 | 48026.33 | 8.50 MB |
| go                        | echo                      | 102749.00 | 9788.67 | 30439.33 | 58.67 MB |
| go                        | gorilla-mux               | 93085.67 | 11484.67 | 38809.00 | 34.96 MB |
| go                        | iris                      | 107121.67 | 9188.67 | 26071.00 | 37.97 MB |
| go                        | fasthttprouter            | 173023.33 | 5294.33 | 15832.00 | 81.41 MB |
| go                        | gin                       | 28370.00 | 55352.67 | 243286.67 | 16.35 MB |
| rust                      | actix-web                 | 187197.00 | 4964.67 | 16960.67 | 68.44 MB |
| rust                      | iron                      | 109887.33 | 535.67 | 1787.00 | 42.34 MB |
| rust                      | nickel                    | 90944.00 | 94.67 | 154.67 | 48.64 MB |
| rust                      | rocket                    | 108408.00 | 126.00 | 258.67 | 49.09 MB |
| node                      | express                   | 57646.33 | 24613.00 | 324796.67 | 47.84 MB |
| node                      | fastify                   | 78772.33 | 17032.67 | 171012.67 | 79.21 MB |
| node                      | polka                     | 93896.33 | 14283.67 | 152223.33 | 47.39 MB |
| node                      | rayo                      | 82281.67 | 14530.00 | 108914.33 | 41.89 MB |
| node                      | koa                       | 57094.00 | 28845.33 | 469860.67 | 41.14 MB |
| node                      | restify                   | 53997.67 | 22747.33 | 190929.00 | 31.75 MB |
| elixir                    | plug                      | 46006.00 | 22831.00 | 66607.33 | 29.64 MB |
| elixir                    | phoenix                   | 39535.00 | 29230.33 | 162903.67 | 26.10 MB |
| swift                     | vapor                     | 52026.33 | 33374.33 | 641201.33 | 18.76 MB |
| swift                     | perfect                   | 60997.67 | 16129.67 | 23422.67 | 16.99 MB |
| swift                     | kitura                    | 36085.67 | 27688.67 | 39228.67 | 20.32 MB |
| scala                     | akkahttp                  | 64107.00 | 207638.33 | 4364330.00 | 52.22 MB |
| csharp                    | aspnetcore                | 99635.33 | 11286.00 | 70700.67 | 49.43 MB |
| python                    | sanic                     | 17175.67 | 59193.33 | 163777.67 | 10.04 MB |
| python                    | japronto                  | 48902.33 | 20567.00 | 32187.67 | 19.65 MB |
| python                    | flask                     | 17735.00 | 58812.33 | 180922.67 | 13.96 MB |
| python                    | django                    | 11212.33 | 91561.00 | 263200.00 | 10.32 MB |
| python                    | tornado                   | 1356.00 | 680218.67 | 3066278.00 | 0.95 MB |
| nim                       | jester                    | 16922.00 | 235762.67 | 4730227.67 | 6.32 MB |
| nim                       | mofuw                     | 112493.67 | 13666.00 | 120175.33 | 67.53 MB |
| java                      | act                       | 130696.00 | 8442.00 | 25991.33 | 47.85 MB |
| cpp                       | evhtp                     | 174950.33 | 5390.33 | 16729.67 | 56.15 MB |
| php                       | symfony                   | 53528.67 | 259307.67 | 4103460.33 | 85.64 MB |
| php                       | laravel                   | 48366.00 | 233432.00 | 3286005.33 | 78.21 MB |
<!-- Result till here -->

## The rule

We want to know the response time (routing time), not a usability. So full-stack framework is at a disadvantage.
 - Each server has no special logics.
 - Each server's executable is named as `server_[Lauguage]_[Framework]`. (For example, `server_ruby_sinatra`)
 - There are only 3 routes
   - GET  '/'         return status code 200 with empty body
   - GET  '/user/:id' return status code 200 with the id
   - POST '/user'     return status code 200 with empty body

## Installation

Required environment -> See **Current target frameworks(middlewares)**

### By using Neph

[Neph](https://github.com/tbrand/neph) is a modern command line job processor that can be substitute for `make` command.

To compile servers and benchmarker,
```
> neph
```
For each language,
```
> neph ruby
```
For each framework,
```
> neph rails
```

See [neph.yaml](https://github.com/tbrand/which_is_the_fastest/blob/master/neph.yaml)

### By using make

To compile servers and benchmarker,
```
> make
```
For each language,
```
> make ruby
```
For each framework,
```
> make rails
```

## Usage

You can take a benchmark by
```bash
> bin/benchmarker
```

For each language
```bash
> bin/benchmarker ruby
```

For each framework
```bash
> bin/benchmarker rails
```

For comparison (Comparing rails, kemal and router.cr in this example)
```bash
> bin/benchmarker rails crystal
```

If you take it manually, you can run each server by
```bash
> bin/server_[Language]_[Framework]
```

and run client by
```bash
> time bin/client
```

You can set # of threads and # of the loops of the request(there are 3 requests in a loop) by
```bash
> time bin/client -t 16 -r 1000
```
In the above example, 16 threads requests 1000 * 3 times.
So 48000 requests are sent in total.

## Using Docker
Setup servers by using `docker` is under **WIP**. Currently, crystal and ruby servers are supported. For example
```bash
docker-compose up rails
```

Then you can run your client by
```bash
time ./bin/client
```
## Development
 - **Give me PR when you want to add other web frameworks**
 - **Give me PR when you can tuning each framework (under the rule)**

### Where should I modify when adding new framework
 - `/[language]/[framework]/[codes]` <- Project itself
 - `benchmarker/benchmarker.cr` <- Adding it as a target to
 - `README.md` <- Adding it as a target framework of the list
 - `Makefile`
 - `neph.yaml` (optional)

Anyway, you don't have to care about details since maintainer can fix them after merging it. The result will be updated by maintainer.

## Contributing

1. Fork it (https://github.com/tbrand/which_is_the_fastest/fork)
2. Create your feature branch (git checkout -b my-new-feature)
3. Commit your changes (git commit -am 'Add some feature')
4. Push to the branch (git push origin my-new-feature)
5. Create a new Pull Request

## Contributors

- [tbrand](https://github.com/tbrand) Taichiro Suzuki - creator, maintainer
- [OvermindDL1](https://github.com/OvermindDL1) OvermindDL1 - maintainer
- [waghanza](https://github.com/waghanza) Marwan Rabbâa - maintainer

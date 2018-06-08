# Which is the fastest?

Measuring response times (routing times) for each framework (middleware).
Each framework has to have two features; routing and parsing path parameters.

Any idea is :heart:, let discuss about it on [![Join the chat at https://gitter.im/which_is_the_fastest/Lobby](https://badges.gitter.im/which_is_the_fastest/Lobby.svg)](https://gitter.im/which_is_the_fastest/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Result

<!-- Result from here -->
Last update: 2018-06-09
```
OS: Linux (version: 4.16.13-300.fc28.x86_64, arch: x86_64)
CPU Cores: 4
```

### Ranking (Framework)

1. [actix-web](https://github.com/actix/actix-web) (rust)
2. [fasthttprouter](https://github.com/buaazp/fasthttprouter) (go)
3. [evhtp](https://github.com/criticalstack/libevhtp) (cpp)
4. [act](https://github.com/actframework/actframework) (java)
5. [mofuw](https://github.com/2vg/mofuw) (nim)
6. [aspnetcore](https://github.com/aspnet/Home) (csharp)
7. [rocket](https://github.com/SergioBenitez/Rocket) (rust)
8. [nickel](https://github.com/nickel-org/nickel.rs) (rust)
9. [iris](https://github.com/kataras/iris) (go)
10. [iron](https://github.com/iron/iron) (rust)
11. [gorilla-mux](https://github.com/gorilla/mux) (go)
12. [echo](https://github.com/labstack/echo) (go)
13. [rayo](https://github.com/GetRayo/rayo.js) (node)
14. [polka](https://github.com/lukeed/polka) (node)
15. [japronto](https://github.com/squeaky-pl/japronto) (python)
16. [fastify](https://github.com/fastify/fastify) (node)
17. [perfect](https://github.com/PerfectlySoft/Perfect) (swift)
18. [akkahttp](https://github.com/akka/akka-http) (scala)
19. [router.cr](https://github.com/tbrand/router.cr) (crystal)
20. [raze](https://github.com/samueleaton/raze) (crystal)
21. [spider-gazelle](https://github.com/spider-gazelle/spider-gazelle) (crystal)
22. [vapor](https://github.com/vapor/vapor) (swift)
23. [lucky](https://github.com/luckyframework/lucky) (crystal)
24. [amber](https://github.com/amberframework/amber) (crystal)
25. [kemal](https://github.com/kemalcr/kemal) (crystal)
26. [sanic](https://github.com/channelcat/sanic) (python)
27. [plug](https://github.com/elixir-lang/plug) (elixir)
28. [express](https://github.com/expressjs/express) (node)
29. [gin](https://github.com/gin-gonic/gin) (go)
30. [kitura](https://github.com/IBM-Swift/Kitura) (swift)
31. [roda](https://github.com/jeremyevans/roda) (ruby)
32. [phoenix](https://github.com/phoenixframework/phoenix) (elixir)
33. [rack-routing](https://github.com/iAmPlus/rack-routing) (ruby)
34. [jester](https://github.com/dom96/jester) (nim)
35. [flame](https://github.com/AlexWayfer/flame) (ruby)
36. [flask](https://github.com/pallets/flask) (python)
37. [sinatra](https://github.com/sinatra/sinatra) (ruby)
38. [django](https://github.com/django/django) (python)
39. [rails](https://github.com/rails/rails) (ruby)
40. [tornado](https://github.com/tornadoweb/tornado) (python)

### Ranking (Language)

1. rust ([actix-web](https://github.com/actix/actix-web))
2. go ([fasthttprouter](https://github.com/buaazp/fasthttprouter))
3. cpp ([evhtp](https://github.com/criticalstack/libevhtp))
4. java ([act](https://github.com/actframework/actframework))
5. nim ([mofuw](https://github.com/2vg/mofuw))
6. csharp ([aspnetcore](https://github.com/aspnet/Home))
7. node ([rayo](https://github.com/GetRayo/rayo.js))
8. python ([japronto](https://github.com/squeaky-pl/japronto))
9. swift ([perfect](https://github.com/PerfectlySoft/Perfect))
10. scala ([akkahttp](https://github.com/akka/akka-http))
11. crystal ([router.cr](https://github.com/tbrand/router.cr))
12. elixir ([plug](https://github.com/elixir-lang/plug))
13. ruby ([roda](https://github.com/jeremyevans/roda))

### All frameworks

| Language (Runtime)        | Framework (Middleware)    |    Requests / s |         Latency |   99 percentile |      Throughput |
|---------------------------|---------------------------|-----------------|-----------------|-----------------|------------|
| ruby                      | rails                     | 4671.00 | 13635.00 | 4671.00 | 12294975.00 |
| ruby                      | sinatra                   | 16360.00 | 3895.00 | 16360.00 | 42332812.00 |
| ruby                      | roda                      | 41646.00 | 1564.00 | 41646.00 | 39497535.00 |
| ruby                      | rack-routing              | 35602.00 | 1844.00 | 35602.00 | 20332166.00 |
| ruby                      | flame                     | 21293.00 | 2996.00 | 21293.00 | 12160798.00 |
| crystal                   | kemal                     | 49671.00 | 25229.00 | 49671.00 | 80745876.00 |
| crystal                   | router.cr                 | 58447.00 | 22666.00 | 58447.00 | 54521808.00 |
| crystal                   | raze                      | 56666.00 | 16379.00 | 56666.00 | 52833238.00 |
| crystal                   | lucky                     | 51648.00 | 18086.00 | 51648.00 | 60642972.00 |
| crystal                   | amber                     | 51092.00 | 25468.00 | 51092.00 | 73752768.00 |
| crystal                   | spider-gazelle            | 55877.00 | 23902.00 | 55877.00 | 52121168.00 |
| go                        | echo                      | 94024.00 | 14954.00 | 94024.00 | 164513520.00 |
| go                        | gorilla-mux               | 94646.00 | 13784.00 | 94646.00 | 106942500.00 |
| go                        | iris                      | 98817.00 | 14056.00 | 98817.00 | 111828300.00 |
| go                        | fasthttprouter            | 169565.00 | 5559.00 | 169565.00 | 238026804.00 |
| go                        | gin                       | 47558.00 | 25434.00 | 47558.00 | 82946960.00 |
| rust                      | actix-web                 | 193879.00 | 4191.00 | 193879.00 | 219329775.00 |
| rust                      | iron                      | 95837.00 | 332.00 | 95837.00 | 108278100.00 |
| rust                      | nickel                    | 102210.00 | 43.00 | 102210.00 | 200015270.00 |
| rust                      | rocket                    | 122714.00 | 59.00 | 122714.00 | 167994372.00 |
| node                      | express                   | 47784.00 | 54772.00 | 47784.00 | 116402508.00 |
| node                      | fastify                   | 79264.00 | 25406.00 | 79264.00 | 118486467.00 |
| node                      | polka                     | 86485.00 | 25475.00 | 86485.00 | 129264399.00 |
| node                      | rayo                      | 87524.00 | 17997.00 | 87524.00 | 130716531.00 |
| elixir                    | plug                      | 48058.00 | 31029.00 | 48058.00 | 103417314.00 |
| elixir                    | phoenix                   | 40106.00 | 33881.00 | 40106.00 | 86289918.00 |
| swift                     | vapor                     | 52345.00 | 23773.00 | 52345.00 | 59041500.00 |
| swift                     | perfect                   | 75254.00 | 12607.00 | 75254.00 | 70296778.00 |
| swift                     | kitura                    | 46715.00 | 20010.00 | 46715.00 | 86426565.00 |
| scala                     | akkahttp                  | 64771.00 | 211582.00 | 64771.00 | 138398028.00 |
| csharp                    | aspnetcore                | 124906.00 | 7263.00 | 124906.00 | 199594502.00 |
| python                    | sanic                     | 48279.00 | 19313.00 | 48279.00 | 86582615.00 |
| python                    | japronto                  | 80969.00 | 11445.00 | 80969.00 | 96254864.00 |
| python                    | flask                     | 17241.00 | 60640.00 | 17241.00 | 42363537.00 |
| python                    | django                    | 12039.00 | 80415.00 | 12039.00 | 34764480.00 |
| python                    | tornado                   | 2098.00 | 466380.00 | 2098.00 | 6122446.00 |
| nim                       | jester                    | 30245.00 | 56324.00 | 30245.00 | 35036232.00 |
| nim                       | mofuw                     | 139689.00 | 7925.00 | 139689.00 | 250877228.00 |
| java                      | act                       | 154129.00 | 9661.00 | 154129.00 | 262951678.00 |
| cpp                       | evhtp                     | 165218.00 | 5296.00 | 165218.00 | 159617920.00 |
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

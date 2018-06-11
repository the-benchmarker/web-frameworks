# Which is the fastest?

Measuring response times (routing times) for each framework (middleware).
Each framework has to have two features; routing and parsing path parameters.

Any idea is :heart:, let discuss about it on [![Join the chat at https://gitter.im/which_is_the_fastest/Lobby](https://badges.gitter.im/which_is_the_fastest/Lobby.svg)](https://gitter.im/which_is_the_fastest/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Result

<!-- Result from here -->
Last update: 2018-06-11
```
OS: Linux (version: 4.16.11-100.fc26.x86_64, arch: x86_64)
CPU Cores: 8
threads: 9, requests: 100000.0
```

### Ranking (Framework)

1. [actix-web](https://github.com/actix/actix-web) (rust)
2. [fasthttprouter](https://github.com/buaazp/fasthttprouter) (go)
3. [evhtp](https://github.com/criticalstack/libevhtp) (cpp)
4. [mofuw](https://github.com/2vg/mofuw) (nim)
5. [act](https://github.com/actframework/actframework) (java)
6. [iris](https://github.com/kataras/iris) (go)
7. [echo](https://github.com/labstack/echo) (go)
8. [gorilla-mux](https://github.com/gorilla/mux) (go)
9. [iron](https://github.com/iron/iron) (rust)
10. [rocket](https://github.com/SergioBenitez/Rocket) (rust)
11. [aspnetcore](https://github.com/aspnet/Home) (csharp)
12. [polka](https://github.com/lukeed/polka) (node)
13. [nickel](https://github.com/nickel-org/nickel.rs) (rust)
14. [fastify](https://github.com/fastify/fastify) (node)
15. [perfect](https://github.com/PerfectlySoft/Perfect) (swift)
16. [roda](https://github.com/jeremyevans/roda) (ruby)
17. [vapor](https://github.com/vapor/vapor) (swift)
18. [plug](https://github.com/elixir-lang/plug) (elixir)
19. [japronto](https://github.com/squeaky-pl/japronto) (python)
20. [express](https://github.com/expressjs/express) (node)
21. [sanic](https://github.com/channelcat/sanic) (python)
22. [router.cr](https://github.com/tbrand/router.cr) (crystal)
23. [akkahttp](https://github.com/akka/akka-http) (scala)
24. [rack-routing](https://github.com/iAmPlus/rack-routing) (ruby)
25. [phoenix](https://github.com/phoenixframework/phoenix) (elixir)
26. [raze](https://github.com/samueleaton/raze) (crystal)
27. [kitura](https://github.com/IBM-Swift/Kitura) (swift)
28. [spider-gazelle](https://github.com/spider-gazelle/spider-gazelle) (crystal)
29. [gin](https://github.com/gin-gonic/gin) (go)
30. [lucky](https://github.com/luckyframework/lucky) (crystal)
31. [kemal](https://github.com/kemalcr/kemal) (crystal)
32. [amber](https://github.com/amberframework/amber) (crystal)
33. [flame](https://github.com/AlexWayfer/flame) (ruby)
34. [sinatra](https://github.com/sinatra/sinatra) (ruby)
35. [jester](https://github.com/dom96/jester) (nim)
36. [flask](https://github.com/pallets/flask) (python)
37. [django](https://github.com/django/django) (python)
38. [rails](https://github.com/rails/rails) (ruby)
39. [tornado](https://github.com/tornadoweb/tornado) (python)

### Ranking (Language)

1. rust ([actix-web](https://github.com/actix/actix-web))
2. go ([fasthttprouter](https://github.com/buaazp/fasthttprouter))
3. cpp ([evhtp](https://github.com/criticalstack/libevhtp))
4. nim ([mofuw](https://github.com/2vg/mofuw))
5. java ([act](https://github.com/actframework/actframework))
6. csharp ([aspnetcore](https://github.com/aspnet/Home))
7. node ([polka](https://github.com/lukeed/polka))
8. swift ([perfect](https://github.com/PerfectlySoft/Perfect))
9. ruby ([roda](https://github.com/jeremyevans/roda))
10. elixir ([plug](https://github.com/elixir-lang/plug))
11. python ([japronto](https://github.com/squeaky-pl/japronto))
12. crystal ([router.cr](https://github.com/tbrand/router.cr))
13. scala ([akkahttp](https://github.com/akka/akka-http))

### All frameworks

| Language (Runtime)        | Framework (Middleware)    |    Requests / s |         Latency |   99 percentile |      Throughput |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|-----------:|
| ruby                      | rails                     | 4867.00 | 26323.33 | 162669.00 | 4.48 MB |
| ruby                      | sinatra                   | 16715.00 | 7633.67 | 36309.33 | 14.19 MB |
| ruby                      | roda                      | 49740.67 | 2526.00 | 11710.33 | 15.90 MB |
| ruby                      | rack-routing              | 34272.00 | 3728.33 | 23486.00 | 6.39 MB |
| ruby                      | flame                     | 20926.33 | 6095.33 | 27686.00 | 4.02 MB |
| crystal                   | kemal                     | 27425.00 | 36444.00 | 57842.33 | 14.02 MB |
| crystal                   | router.cr                 | 36517.67 | 28225.33 | 82786.00 | 10.72 MB |
| crystal                   | raze                      | 32326.33 | 30781.33 | 48521.00 | 9.09 MB |
| crystal                   | lucky                     | 27515.00 | 36204.33 | 56917.33 | 9.98 MB |
| crystal                   | amber                     | 24926.33 | 40997.67 | 57698.00 | 10.52 MB |
| crystal                   | spider-gazelle            | 30584.00 | 36200.00 | 173178.00 | 8.38 MB |
| go                        | echo                      | 96220.67 | 10498.33 | 31915.00 | 55.34 MB |
| go                        | gorilla-mux               | 92431.33 | 11152.33 | 36022.33 | 34.73 MB |
| go                        | iris                      | 104717.67 | 9712.33 | 28285.00 | 39.37 MB |
| go                        | fasthttprouter            | 168926.67 | 5657.00 | 16365.33 | 78.00 MB |
| go                        | gin                       | 29324.67 | 52773.00 | 225567.67 | 16.74 MB |
| rust                      | actix-web                 | 193921.33 | 4728.33 | 14374.67 | 72.90 MB |
| rust                      | iron                      | 90255.00 | 663.00 | 3873.00 | 30.55 MB |
| rust                      | nickel                    | 69629.33 | 123.33 | 652.00 | 47.40 MB |
| rust                      | rocket                    | 81630.00 | 172.00 | 1159.00 | 33.37 MB |
| node                      | express                   | 44856.67 | 34392.67 | 524501.67 | 37.87 MB |
| node                      | fastify                   | 61676.67 | 26339.33 | 431968.67 | 61.60 MB |
| node                      | polka                     | 77936.67 | 19126.67 | 291491.00 | 40.83 MB |
| elixir                    | plug                      | 48104.67 | 25085.33 | 161449.33 | 31.40 MB |
| elixir                    | phoenix                   | 32512.00 | 42085.00 | 522119.67 | 22.71 MB |
| swift                     | vapor                     | 48879.33 | 28451.33 | 405902.67 | 17.59 MB |
| swift                     | perfect                   | 51061.33 | 19425.67 | 24855.00 | 14.39 MB |
| swift                     | kitura                    | 31047.33 | 31437.67 | 43421.00 | 17.40 MB |
| scala                     | akkahttp                  | 35226.00 | 214843.67 | 4444267.33 | 31.40 MB |
| csharp                    | aspnetcore                | 79716.67 | 13068.00 | 45945.00 | 42.23 MB |
| python                    | sanic                     | 38882.33 | 26593.33 | 82150.67 | 24.53 MB |
| python                    | japronto                  | 45671.33 | 21940.00 | 28894.33 | 18.32 MB |
| python                    | flask                     | 15657.67 | 65211.00 | 204498.33 | 13.06 MB |
| python                    | django                    | 9283.67 | 107230.33 | 273896.67 | 8.92 MB |
| python                    | tornado                   | 1201.00 | 781124.67 | 4114950.33 | 0.86 MB |
| nim                       | jester                    | 15816.33 | 227326.67 | 4441944.33 | 5.86 MB |
| nim                       | mofuw                     | 112033.33 | 16236.33 | 149419.67 | 66.88 MB |
| java                      | act                       | 105662.00 | 11537.67 | 61959.00 | 37.24 MB |
| cpp                       | evhtp                     | 152686.67 | 6143.33 | 18806.00 | 51.09 MB |
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

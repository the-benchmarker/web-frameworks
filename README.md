# Which is the fastest?

Measuring response times (routing times) for each framework (middleware).
Each framework has to have two features; routing and parsing path parameters.

Any idea is :heart:, let discuss about it on [![Join the chat at https://gitter.im/which_is_the_fastest/Lobby](https://badges.gitter.im/which_is_the_fastest/Lobby.svg)](https://gitter.im/which_is_the_fastest/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Result

<!-- Result from here -->
Last update: 2018-06-15
```
OS: Linux (version: 4.16.11-100.fc26.x86_64, arch: x86_64)
CPU Cores: 8
```

### Ranking (Framework)

1. [actix-web](https://github.com/actix/actix-web) (rust)
2. [fasthttprouter](https://github.com/buaazp/fasthttprouter) (go)
3. [evhtp](https://github.com/criticalstack/libevhtp) (cpp)
4. [mofuw](https://github.com/2vg/mofuw) (nim)
5. [act](https://github.com/actframework/actframework) (java)
6. [iron](https://github.com/iron/iron) (rust)
7. [iris](https://github.com/kataras/iris) (go)
8. [aspnetcore](https://github.com/aspnet/Home) (csharp)
9. [rocket](https://github.com/SergioBenitez/Rocket) (rust)
10. [echo](https://github.com/labstack/echo) (go)
11. [polka](https://github.com/lukeed/polka) (node)
12. [gorilla-mux](https://github.com/gorilla/mux) (go)
13. [rayo](https://github.com/GetRayo/rayo.js) (node)
14. [nickel](https://github.com/nickel-org/nickel.rs) (rust)
15. [fastify](https://github.com/fastify/fastify) (node)
16. [akkahttp](https://github.com/akka/akka-http) (scala)
17. [perfect](https://github.com/PerfectlySoft/Perfect) (swift)
18. [vapor](https://github.com/vapor/vapor) (swift)
19. [express](https://github.com/expressjs/express) (node)
20. [japronto](https://github.com/squeaky-pl/japronto) (python)
21. [roda](https://github.com/jeremyevans/roda) (ruby)
22. [sanic](https://github.com/channelcat/sanic) (python)
23. [plug](https://github.com/elixir-lang/plug) (elixir)
24. [phoenix](https://github.com/phoenixframework/phoenix) (elixir)
25. [rack-routing](https://github.com/iAmPlus/rack-routing) (ruby)
26. [kitura](https://github.com/IBM-Swift/Kitura) (swift)
27. [router.cr](https://github.com/tbrand/router.cr) (crystal)
28. [raze](https://github.com/samueleaton/raze) (crystal)
29. [spider-gazelle](https://github.com/spider-gazelle/spider-gazelle) (crystal)
30. [gin](https://github.com/gin-gonic/gin) (go)
31. [kemal](https://github.com/kemalcr/kemal) (crystal)
32. [lucky](https://github.com/luckyframework/lucky) (crystal)
33. [amber](https://github.com/amberframework/amber) (crystal)
34. [flame](https://github.com/AlexWayfer/flame) (ruby)
35. [flask](https://github.com/pallets/flask) (python)
36. [jester](https://github.com/dom96/jester) (nim)
37. [sinatra](https://github.com/sinatra/sinatra) (ruby)
38. [django](https://github.com/django/django) (python)
39. [rails](https://github.com/rails/rails) (ruby)
40. [tornado](https://github.com/tornadoweb/tornado) (python)

### Ranking (Language)

1. rust ([actix-web](https://github.com/actix/actix-web))
2. go ([fasthttprouter](https://github.com/buaazp/fasthttprouter))
3. cpp ([evhtp](https://github.com/criticalstack/libevhtp))
4. nim ([mofuw](https://github.com/2vg/mofuw))
5. java ([act](https://github.com/actframework/actframework))
6. csharp ([aspnetcore](https://github.com/aspnet/Home))
7. node ([polka](https://github.com/lukeed/polka))
8. scala ([akkahttp](https://github.com/akka/akka-http))
9. swift ([perfect](https://github.com/PerfectlySoft/Perfect))
10. python ([japronto](https://github.com/squeaky-pl/japronto))
11. ruby ([roda](https://github.com/jeremyevans/roda))
12. elixir ([plug](https://github.com/elixir-lang/plug))
13. crystal ([router.cr](https://github.com/tbrand/router.cr))

### All frameworks

| Language (Runtime)        | Framework (Middleware)    |    Requests / s |         Latency |   99 percentile |      Throughput |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|-----------:|
| ruby                      | rails                     | 4700.00 | 27297.33 | 168675.33 | 4.36 MB |
| ruby                      | sinatra                   | 15812.00 | 8102.67 | 67657.67 | 13.83 MB |
| ruby                      | roda                      | 42907.67 | 2948.00 | 17695.00 | 13.91 MB |
| ruby                      | rack-routing              | 33971.67 | 3757.00 | 20575.67 | 5.93 MB |
| ruby                      | flame                     | 20543.00 | 6325.33 | 49648.67 | 3.75 MB |
| crystal                   | kemal                     | 26949.67 | 40503.00 | 216682.33 | 13.41 MB |
| crystal                   | router.cr                 | 32651.33 | 31229.33 | 74960.33 | 9.22 MB |
| crystal                   | raze                      | 32260.00 | 32438.00 | 82230.00 | 8.38 MB |
| crystal                   | lucky                     | 25298.67 | 39936.33 | 56458.00 | 8.36 MB |
| crystal                   | amber                     | 24269.67 | 41516.67 | 57406.67 | 9.73 MB |
| crystal                   | spider-gazelle            | 30358.00 | 32698.33 | 50282.67 | 8.27 MB |
| go                        | echo                      | 90097.00 | 11351.00 | 38948.67 | 43.45 MB |
| go                        | gorilla-mux               | 80796.67 | 12907.67 | 45094.00 | 31.72 MB |
| go                        | iris                      | 97219.00 | 10390.33 | 31541.33 | 38.80 MB |
| go                        | fasthttprouter            | 170640.00 | 5713.00 | 17252.00 | 78.62 MB |
| go                        | gin                       | 29554.33 | 51859.00 | 224478.67 | 17.23 MB |
| rust                      | actix-web                 | 181245.67 | 5199.33 | 16907.00 | 68.76 MB |
| rust                      | iron                      | 98689.00 | 591.33 | 3017.33 | 37.68 MB |
| rust                      | nickel                    | 77911.67 | 108.67 | 406.00 | 46.62 MB |
| rust                      | rocket                    | 96148.00 | 141.00 | 483.00 | 50.95 MB |
| node                      | express                   | 49950.33 | 32980.33 | 544891.33 | 41.35 MB |
| node                      | fastify                   | 69439.00 | 19285.67 | 212247.33 | 72.82 MB |
| node                      | polka                     | 80856.00 | 16565.00 | 198517.67 | 40.45 MB |
| node                      | rayo                      | 78804.33 | 15648.00 | 119027.67 | 39.36 MB |
| elixir                    | plug                      | 38529.33 | 28759.33 | 154774.67 | 27.46 MB |
| elixir                    | phoenix                   | 37133.00 | 32630.33 | 258123.33 | 23.55 MB |
| swift                     | vapor                     | 51576.00 | 41949.67 | 940764.00 | 19.58 MB |
| swift                     | perfect                   | 56951.00 | 17329.67 | 24624.33 | 16.87 MB |
| swift                     | kitura                    | 33607.33 | 30888.00 | 92812.00 | 19.33 MB |
| scala                     | akkahttp                  | 59853.33 | 210753.00 | 4777700.33 | 47.02 MB |
| csharp                    | aspnetcore                | 96442.00 | 12482.00 | 95651.33 | 50.48 MB |
| python                    | sanic                     | 41849.00 | 25016.67 | 84516.33 | 24.32 MB |
| python                    | japronto                  | 46809.00 | 21408.00 | 30999.67 | 19.14 MB |
| python                    | flask                     | 18577.33 | 58890.33 | 169235.00 | 15.09 MB |
| python                    | django                    | 10693.00 | 93285.00 | 227041.67 | 9.35 MB |
| python                    | tornado                   | 1349.00 | 733741.33 | 4699986.00 | 0.95 MB |
| nim                       | jester                    | 16756.33 | 235403.33 | 4529816.67 | 5.98 MB |
| nim                       | mofuw                     | 121368.33 | 15125.67 | 137293.00 | 71.99 MB |
| java                      | act                       | 119728.00 | 9470.33 | 29921.67 | 44.70 MB |
| cpp                       | evhtp                     | 164458.33 | 5810.33 | 18420.33 | 52.83 MB |
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

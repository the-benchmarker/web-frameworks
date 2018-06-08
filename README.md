# Which is the fastest?

Measuring response times (routing times) for each framework (middleware).
Each framework has to have two features; routing and parsing path parameters.

Any idea is :heart:, let discuss about it on [![Join the chat at https://gitter.im/which_is_the_fastest/Lobby](https://badges.gitter.im/which_is_the_fastest/Lobby.svg)](https://gitter.im/which_is_the_fastest/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Result

<!-- Result from here -->
Last update: 2018-06-08
```
OS: Linux (version: 4.16.11-100.fc26.x86_64, arch: x86_64)
CPU Cores: 8
```

### Ranking (Framework)

1. [tornado](https://github.com/tornadoweb/tornado) (python)
2. [akkahttp](https://github.com/akka/akka-http) (scala)
3. [rails](https://github.com/rails/rails) (ruby)
4. [django](https://github.com/django/django) (python)
5. [sinatra](https://github.com/sinatra/sinatra) (ruby)
6. [flask](https://github.com/pallets/flask) (python)
7. [flame](https://github.com/AlexWayfer/flame) (ruby)
8. [express](https://github.com/expressjs/express) (node)
9. [jester](https://github.com/dom96/jester) (nim)
10. [act](https://github.com/actframework/actframework) (java)
11. [sanic](https://github.com/channelcat/sanic) (python)
12. [amber](https://github.com/amberframework/amber) (crystal)
13. [fastify](https://github.com/fastify/fastify) (node)
14. [gin](https://github.com/gin-gonic/gin) (go)
15. [kemal](https://github.com/kemalcr/kemal) (crystal)
16. [lucky](https://github.com/luckyframework/lucky) (crystal)
17. [polka](https://github.com/lukeed/polka) (node)
18. [spider-gazelle](https://github.com/spider-gazelle/spider-gazelle) (crystal)
19. [raze](https://github.com/samueleaton/raze) (crystal)
20. [rack-routing](https://github.com/iAmPlus/rack-routing) (ruby)
21. [router.cr](https://github.com/tbrand/router.cr) (crystal)
22. [kitura](https://github.com/IBM-Swift/Kitura) (swift)
23. [aspnetcore](https://github.com/aspnet/Home) (csharp)
24. [phoenix](https://github.com/phoenixframework/phoenix) (elixir)
25. [roda](https://github.com/jeremyevans/roda) (ruby)
26. [vapor](https://github.com/vapor/vapor) (swift)
27. [plug](https://github.com/elixir-lang/plug) (elixir)
28. [rocket](https://github.com/SergioBenitez/Rocket) (rust)
29. [nickel](https://github.com/nickel-org/nickel.rs) (rust)
30. [iron](https://github.com/iron/iron) (rust)
31. [echo](https://github.com/labstack/echo) (go)
32. [gorilla-mux](https://github.com/gorilla/mux) (go)
33. [iris](https://github.com/kataras/iris) (go)
34. [mofuw](https://github.com/2vg/mofuw) (nim)
35. [perfect](https://github.com/PerfectlySoft/Perfect) (swift)
36. [actix-web](https://github.com/actix/actix-web) (rust)
37. [japronto](https://github.com/squeaky-pl/japronto) (python)
38. [evhtp](https://github.com/criticalstack/libevhtp) (cpp)
39. [fasthttprouter](https://github.com/buaazp/fasthttprouter) (go)

### Ranking (Language)

1. python ([tornado](https://github.com/tornadoweb/tornado))
2. scala ([akkahttp](https://github.com/akka/akka-http))
3. ruby ([rails](https://github.com/rails/rails))
4. node ([express](https://github.com/expressjs/express))
5. nim ([jester](https://github.com/dom96/jester))
6. java ([act](https://github.com/actframework/actframework))
7. crystal ([amber](https://github.com/amberframework/amber))
8. go ([gin](https://github.com/gin-gonic/gin))
9. swift ([kitura](https://github.com/IBM-Swift/Kitura))
10. csharp ([aspnetcore](https://github.com/aspnet/Home))
11. elixir ([phoenix](https://github.com/phoenixframework/phoenix))
12. rust ([rocket](https://github.com/SergioBenitez/Rocket))
13. cpp ([evhtp](https://github.com/criticalstack/libevhtp))

### All frameworks

| Language (Runtime)        | Framework (Middleware)    |          Errors |    Requests / s |      Throughput |
|---------------------------|---------------------------|-----------------|-----------------|-----------------|
| ruby                      | rails                     |               0 |         4698.26 |        802.93KB |
| ruby                      | sinatra                   |               0 |        15667.12 |          2.57MB |
| ruby                      | roda                      |               0 |        54040.28 |          3.25MB |
| ruby                      | rack-routing              |               0 |        40663.68 |          1.47MB |
| ruby                      | flame                     |               0 |        19446.71 |        721.66KB |
| crystal                   | kemal                     |               0 |        35102.26 |          3.62MB |
| crystal                   | router.cr                 |               0 |        43350.97 |          2.56MB |
| crystal                   | raze                      |               0 |        39309.05 |          2.32MB |
| crystal                   | lucky                     |               0 |        35279.23 |          2.62MB |
| crystal                   | amber                     |               0 |        31209.24 |          2.86MB |
| crystal                   | spider-gazelle            |               0 |         38913.9 |          2.30MB |
| go                        | echo                      |               0 |        63605.81 |          7.04MB |
| go                        | gorilla-mux               |               0 |        64775.51 |          4.63MB |
| go                        | iris                      |               0 |        65065.73 |          4.65MB |
| go                        | fasthttprouter            |               0 |        73971.65 |          6.56MB |
| go                        | gin                       |               0 |        33294.65 |          3.68MB |
| rust                      | actix-web                 |               0 |         68460.3 |          4.90MB |
| rust                      | iron                      |               0 |        62739.15 |          4.49MB |
| rust                      | nickel                    |               0 |        62119.73 |          7.70MB |
| rust                      | rocket                    |               0 |        58565.76 |          5.08MB |
| node                      | express                   |               0 |        22114.64 |          3.42MB |
| node                      | fastify                   |               0 |        31516.64 |          2.98MB |
| node                      | polka                     |               0 |         38555.0 |          3.64MB |
| elixir                    | plug                      |               0 |        57877.82 |          7.89MB |
| elixir                    | phoenix                   |               0 |        52874.33 |          7.21MB |
| swift                     | vapor                     |               0 |         57035.2 |          4.08MB |
| swift                     | perfect                   |               0 |        68101.94 |          4.03MB |
| swift                     | kitura                    |               0 |        45687.22 |          5.36MB |
| scala                     | akkahttp                  |               0 |         2498.24 |        346.44KB |
| csharp                    | aspnetcore                |               0 |        48488.55 |          4.90MB |
| python                    | sanic                     |               0 |        28817.52 |          3.27MB |
| python                    | japronto                  |               0 |        68802.77 |          5.18MB |
| python                    | flask                     |               0 |        18879.86 |          2.93MB |
| python                    | django                    |               0 |         12289.3 |          2.25MB |
| python                    | tornado                   |               0 |         1441.31 |        273.06KB |
| nim                       | jester                    |               0 |        22920.28 |          1.68MB |
| nim                       | mofuw                     |               0 |        65430.19 |          7.43MB |
| java                      | act                       |               0 |        24037.43 |          2.59MB |
| cpp                       | evhtp                     |               0 |        70038.91 |          4.27MB |
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

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

1. [evhtp](https://github.com/criticalstack/libevhtp) (cpp)
2. [actix-web](https://github.com/actix/actix-web) (rust)
3. [fasthttprouter](https://github.com/buaazp/fasthttprouter) (go)
4. [mofuw](https://github.com/2vg/mofuw) (nim)
5. [iron](https://github.com/iron/iron) (rust)
6. [gorilla-mux](https://github.com/gorilla/mux) (go)
7. [aspnetcore](https://github.com/aspnet/Home) (csharp)
8. [echo](https://github.com/labstack/echo) (go)
9. [rocket](https://github.com/SergioBenitez/Rocket) (rust)
10. [iris](https://github.com/kataras/iris) (go)
11. [act](https://github.com/actframework/actframework) (java)
12. [polka](https://github.com/lukeed/polka) (node)
13. [nickel](https://github.com/nickel-org/nickel.rs) (rust)
14. [perfect](https://github.com/PerfectlySoft/Perfect) (swift)
15. [japronto](https://github.com/squeaky-pl/japronto) (python)
16. [vapor](https://github.com/vapor/vapor) (swift)
17. [fastify](https://github.com/fastify/fastify) (node)
18. [roda](https://github.com/jeremyevans/roda) (ruby)
19. [sanic](https://github.com/channelcat/sanic) (python)
20. [plug](https://github.com/elixir-lang/plug) (elixir)
21. [phoenix](https://github.com/phoenixframework/phoenix) (elixir)
22. [rack-routing](https://github.com/iAmPlus/rack-routing) (ruby)
23. [spider-gazelle](https://github.com/spider-gazelle/spider-gazelle) (crystal)
24. [kitura](https://github.com/IBM-Swift/Kitura) (swift)
25. [router.cr](https://github.com/tbrand/router.cr) (crystal)
26. [raze](https://github.com/samueleaton/raze) (crystal)
27. [kemal](https://github.com/kemalcr/kemal) (crystal)
28. [amber](https://github.com/amberframework/amber) (crystal)
29. [lucky](https://github.com/luckyframework/lucky) (crystal)
30. [gin](https://github.com/gin-gonic/gin) (go)
31. [express](https://github.com/expressjs/express) (node)
32. [flame](https://github.com/AlexWayfer/flame) (ruby)
33. [flask](https://github.com/pallets/flask) (python)
34. [jester](https://github.com/dom96/jester) (nim)
35. [sinatra](https://github.com/sinatra/sinatra) (ruby)
36. [django](https://github.com/django/django) (python)
37. [akkahttp](https://github.com/akka/akka-http) (scala)
38. [rails](https://github.com/rails/rails) (ruby)
39. [tornado](https://github.com/tornadoweb/tornado) (python)

### Ranking (Language)

1. cpp ([evhtp](https://github.com/criticalstack/libevhtp))
2. rust ([actix-web](https://github.com/actix/actix-web))
3. go ([fasthttprouter](https://github.com/buaazp/fasthttprouter))
4. nim ([mofuw](https://github.com/2vg/mofuw))
5. csharp ([aspnetcore](https://github.com/aspnet/Home))
6. java ([act](https://github.com/actframework/actframework))
7. node ([polka](https://github.com/lukeed/polka))
8. swift ([perfect](https://github.com/PerfectlySoft/Perfect))
9. python ([japronto](https://github.com/squeaky-pl/japronto))
10. ruby ([roda](https://github.com/jeremyevans/roda))
11. elixir ([plug](https://github.com/elixir-lang/plug))
12. crystal ([spider-gazelle](https://github.com/spider-gazelle/spider-gazelle))
13. scala ([akkahttp](https://github.com/akka/akka-http))

### All frameworks

| Language (Runtime)        | Framework (Middleware)    |          Errors |    Requests / s |      Throughput |
|---------------------------|---------------------------|-----------------|-----------------|-----------------|
| ruby                      | rails                     |               0 |          5244.0 |          0.88MB |
| ruby                      | sinatra                   |               0 |        18319.21 |          3.00MB |
| ruby                      | roda                      |               0 |        51746.05 |          3.11MB |
| ruby                      | rack-routing              |               0 |        38970.56 |          1.41MB |
| ruby                      | flame                     |               0 |        20051.54 |        744.10KB |
| crystal                   | kemal                     |               0 |        30853.43 |          3.18MB |
| crystal                   | router.cr                 |               0 |        36430.32 |          2.15MB |
| crystal                   | raze                      |               0 |        33763.82 |          2.00MB |
| crystal                   | lucky                     |               0 |        29881.06 |          2.22MB |
| crystal                   | amber                     |               0 |        30195.57 |          2.76MB |
| crystal                   | spider-gazelle            |               0 |        37697.55 |          2.23MB |
| go                        | echo                      |               0 |        96148.61 |         10.64MB |
| go                        | gorilla-mux               |               0 |       104014.08 |          7.44MB |
| go                        | iris                      |               0 |         88321.3 |          6.32MB |
| go                        | fasthttprouter            |               0 |       155974.14 |         13.83MB |
| go                        | gin                       |               0 |        28294.89 |          3.13MB |
| rust                      | actix-web                 |               0 |       182784.28 |         13.07MB |
| rust                      | iron                      |               0 |       104671.44 |          7.49MB |
| rust                      | nickel                    |               0 |        75911.41 |          9.41MB |
| rust                      | rocket                    |               0 |         92901.0 |          8.06MB |
| node                      | express                   |               0 |        24262.72 |          3.75MB |
| node                      | fastify                   |               0 |        53698.26 |          5.07MB |
| node                      | polka                     |               0 |        76523.19 |          7.22MB |
| elixir                    | plug                      |               0 |        47695.39 |          6.50MB |
| elixir                    | phoenix                   |               0 |        41442.85 |          5.65MB |
| swift                     | vapor                     |               0 |        54414.23 |          3.89MB |
| swift                     | perfect                   |               0 |        73990.89 |          4.37MB |
| swift                     | kitura                    |               0 |        36446.95 |          4.28MB |
| scala                     | akkahttp                  |               0 |         9815.91 |          1.33MB |
| csharp                    | aspnetcore                |               0 |        103493.5 |         10.46MB |
| python                    | sanic                     |               0 |        50452.43 |          5.73MB |
| python                    | japronto                  |               0 |        55134.46 |          4.15MB |
| python                    | flask                     |               0 |        19839.14 |          3.08MB |
| python                    | django                    |               0 |        12533.96 |          2.30MB |
| python                    | tornado                   |               0 |         1309.27 |        248.04KB |
| nim                       | jester                    |               0 |        19409.12 |          1.43MB |
| nim                       | mofuw                     |               0 |       114043.75 |         12.94MB |
| java                      | act                       |               0 |        77115.44 |          8.31MB |
| cpp                       | evhtp                     |               0 |       185815.53 |         11.34MB |
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

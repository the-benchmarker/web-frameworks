# Which is the fastest?

Measuring response times (routing times) for each framework (middleware).
Each framework has to have two features; routing and parsing path parameters.

Any idea is :heart:, let discuss about it on [![Join the chat at https://gitter.im/which_is_the_fastest/Lobby](https://badges.gitter.im/which_is_the_fastest/Lobby.svg)](https://gitter.im/which_is_the_fastest/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Result

<!-- Result from here -->
Last update: 2018-05-12
```
OS: Linux (version: 4.16.7-200.fc27.x86_64, arch: x86_64)
CPU Cores: 4
```

### Ranking (Framework)

1. [router.cr](https://github.com/tbrand/router.cr) (crystal)
2. [raze](https://github.com/samueleaton/raze) (crystal)
3. [spider-gazelle](https://github.com/spider-gazelle/spider-gazelle) (crystal)
4. [lucky](https://github.com/luckyframework/lucky) (crystal)
5. [japronto](https://github.com/squeaky-pl/japronto) (python)
6. [amber](https://github.com/amberframework/amber) (crystal)
7. [actix-web](https://github.com/actix/actix-web) (rust)
8. [kemal](https://github.com/kemalcr/kemal) (crystal)
9. [fasthttprouter](https://github.com/buaazp/fasthttprouter) (go)
10. [nickel](https://github.com/nickel-org/nickel.rs) (rust)
11. [iron](https://github.com/iron/iron) (rust)
12. [rocket](https://github.com/SergioBenitez/Rocket) (rust)
13. [iris](https://github.com/kataras/iris) (go)
14. [echo](https://github.com/labstack/echo) (go)
15. [gorilla-mux](https://github.com/gorilla/mux) (go)
16. [polka](https://github.com/lukeed/polka) (node)
17. [fastify](https://github.com/fastify/fastify) (node)
18. [aspnetcore](https://github.com/aspnet/Home) (csharp)
19. [perfect](https://github.com/PerfectlySoft/Perfect) (swift)
20. [express](https://github.com/expressjs/express) (node)
21. [vapor](https://github.com/vapor/vapor) (swift)
22. [akkahttp](https://github.com/akka/akka-http) (scala)
23. [gin](https://github.com/gin-gonic/gin) (go)
24. [kitura](https://github.com/IBM-Swift/Kitura) (swift)
25. [roda](https://github.com/jeremyevans/roda) (ruby)
26. [rack-routing](https://github.com/iAmPlus/rack-routing) (ruby)
27. [sanic](https://github.com/channelcat/sanic) (python)
28. [mofuw](https://github.com/2vg/mofuw) (nim)
29. [plug](https://github.com/elixir-lang/plug) (elixir)
30. [phoenix](https://github.com/phoenixframework/phoenix) (elixir)
31. [flask](https://github.com/pallets/flask) (python)
32. [flame](https://github.com/AlexWayfer/flame) (ruby)
33. [sinatra](https://github.com/sinatra/sinatra) (ruby)
34. [jester](https://github.com/dom96/jester) (nim)
35. [django](https://github.com/django/django) (python)
36. [tornado](https://github.com/tornadoweb/tornado) (python)
37. [rails](https://github.com/rails/rails) (ruby)

### Ranking (Language)

1. crystal ([router.cr](https://github.com/tbrand/router.cr))
2. python ([japronto](https://github.com/squeaky-pl/japronto))
3. rust ([actix-web](https://github.com/actix/actix-web))
4. go ([fasthttprouter](https://github.com/buaazp/fasthttprouter))
5. node ([polka](https://github.com/lukeed/polka))
6. csharp ([aspnetcore](https://github.com/aspnet/Home))
7. swift ([perfect](https://github.com/PerfectlySoft/Perfect))
8. scala ([akkahttp](https://github.com/akka/akka-http))
9. ruby ([roda](https://github.com/jeremyevans/roda))
10. nim ([mofuw](https://github.com/2vg/mofuw))
11. elixir ([plug](https://github.com/elixir-lang/plug))

### All frameworks

| Language (Runtime)        | Framework (Middleware)    |       Max [sec] |       Min [sec] |       Ave [sec] |
|---------------------------|---------------------------|-----------------|-----------------|-----------------|
| ruby                      | rails                     |     1138.717029 |      596.705515 |      793.880213 |
| ruby                      | sinatra                   |      220.587592 |      155.617026 |      192.097094 |
| ruby                      | roda                      |      105.715563 |       71.388148 |       83.228420 |
| ruby                      | rack-routing              |      115.894582 |       83.406358 |       98.019375 |
| ruby                      | flame                     |      227.469019 |      125.581070 |      181.891574 |
| crystal                   | kemal                     |       30.740479 |       29.321029 |       30.060208 |
| crystal                   | router.cr                 |       25.867453 |       25.617570 |       25.781999 |
| crystal                   | raze                      |       26.655143 |       25.705600 |       26.071092 |
| crystal                   | lucky                     |       28.044076 |       27.428675 |       27.821724 |
| crystal                   | amber                     |       28.453804 |       28.067356 |       28.264912 |
| crystal                   | spider-gazelle            |       26.823136 |       26.209777 |       26.620977 |
| go                        | echo                      |       39.374395 |       38.996778 |       39.165814 |
| go                        | gorilla-mux               |       39.564056 |       39.280296 |       39.478078 |
| go                        | iris                      |       39.124244 |       38.538335 |       38.825708 |
| go                        | fasthttprouter            |       31.302753 |       31.031693 |       31.140474 |
| go                        | gin                       |       80.046432 |       79.116341 |       79.705716 |
| rust                      | actix-web                 |       30.083140 |       29.882826 |       29.980387 |
| rust                      | iron                      |       35.874337 |       35.691640 |       35.788559 |
| rust                      | nickel                    |       32.189398 |       32.084676 |       32.116358 |
| rust                      | rocket                    |       37.214242 |       36.793590 |       37.005314 |
| node                      | express                   |       55.809856 |       55.508365 |       55.700875 |
| node                      | fastify                   |       51.434494 |       46.757252 |       47.956960 |
| node                      | polka                     |       42.706277 |       42.024216 |       42.409195 |
| elixir                    | plug                      |      124.661604 |      122.949471 |      123.807926 |
| elixir                    | phoenix                   |      146.712690 |      138.679688 |      143.023028 |
| swift                     | vapor                     |       59.177873 |       58.909018 |       59.048115 |
| swift                     | perfect                   |       55.277174 |       52.570073 |       54.623487 |
| swift                     | kitura                    |      100.965082 |       67.449515 |       80.560688 |
| scala                     | akkahttp                  |       64.114466 |       62.593498 |       63.369870 |
| csharp                    | aspnetcore                |       51.437905 |       50.897969 |       51.071856 |
| python                    | sanic                     |      118.590948 |       79.125047 |      101.660033 |
| python                    | japronto                  |       28.014322 |       27.667655 |       27.879403 |
| python                    | flask                     |      219.014852 |      114.585625 |      145.897795 |
| python                    | django                    |      478.203936 |      194.889338 |      271.425040 |
| python                    | tornado                   |      726.349087 |      706.362335 |      718.087107 |
| nim                       | jester                    |      246.613563 |      246.253752 |      246.379956 |
| nim                       | mofuw                     |      147.627614 |       83.375723 |      108.593340 |
<!-- Result till here -->

## Current target frameworks (middlewares)

 - Ruby
   - [Rails](https://github.com/rails/rails)
   - [Sinatra](https://github.com/sinatra/sinatra)
   - [Roda](https://github.com/jeremyevans/roda)
   - [Rack-Routing](https://github.com/georgeu2000/rack-routing)
   - [Flame](https://github.com/AlexWayfer/flame)
 - Crystal
   - [Kemal](https://github.com/kemalcr/kemal)
   - [raze](https://github.com/samueleaton/raze)
   - [router.cr](https://github.com/tbrand/router.cr)
 - Go
   - [Echo](https://github.com/labstack/echo)
   - [gorilla-mux](https://github.com/gorilla/mux)
   - [iris](https://github.com/kataras/iris)
   - [fasthttprouter](https://github.com/buaazp/fasthttprouter)
   - [Gin](https://github.com/gin-gonic/gin)
 - Rust
   - [IRON](https://github.com/iron/iron)
   - [nickel.rs](https://github.com/nickel-org/nickel.rs)
   - [Rocket](https://rocket.rs) (nightly)
 - node
   - [express](https://github.com/expressjs/express)
   - [polka](https://github.com/lukeed/polka)
 - Elixir
   - [Plug](http://github.com/elixir-lang/plug)
   - [Phoenix](http://github.com/phoenixframework/phoenix)
 - Swift
   - [Vapor](https://vapor.codes)
   - [Perfect](https://www.perfect.org)
   - [Kitura](http://www.kitura.io)
 - Scala
   - [Akka-Http (Routing DSL)](http://doc.akka.io/docs/akka-http/current/scala/http/introduction.html#routing-dsl-for-http-servers)
 - C#
   - [ASP.NET Core](https://www.microsoft.com/net/core)
 - Python
   - [sanic](https://github.com/channelcat/sanic)
   - [japronto](https://github.com/squeaky-pl/japronto)
   - [flask](https://github.com/pallets/flask)
   - [tornado](https://github.com/tornadoweb/tornado)
 - Nim
   - [Jester](https://github.com/dom96/jester)
   - [mofuw](https://github.com/2vg/mofuw)
 - Objective-C
   - [Criollo](https://criollo.io/)

See Development section when you want to add new languages or frameworks.

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

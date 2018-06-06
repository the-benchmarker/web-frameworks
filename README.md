# Which is the fastest?

Measuring response times (routing times) for each framework (middleware).
Each framework has to have two features; routing and parsing path parameters.

Any idea is :heart:, let discuss about it on [![Join the chat at https://gitter.im/which_is_the_fastest/Lobby](https://badges.gitter.im/which_is_the_fastest/Lobby.svg)](https://gitter.im/which_is_the_fastest/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Result

<!-- Result from here -->
Last update: 2018-06-06
```
OS: Linux (version: 4.16.11-100.fc26.x86_64, arch: x86_64)
CPU Cores: 8
```

### Ranking (Framework)

1. [fasthttprouter](https://github.com/buaazp/fasthttprouter) (go)
2. [rocket](https://github.com/SergioBenitez/Rocket) (rust)
3. [actix-web](https://github.com/actix/actix-web) (rust)
4. [evhtp](https://github.com/criticalstack/libevhtp) (cpp)
5. [nickel](https://github.com/nickel-org/nickel.rs) (rust)
6. [japronto](https://github.com/squeaky-pl/japronto) (python)
7. [iron](https://github.com/iron/iron) (rust)
8. [echo](https://github.com/labstack/echo) (go)
9. [iris](https://github.com/kataras/iris) (go)
10. [gorilla-mux](https://github.com/gorilla/mux) (go)
11. [vapor](https://github.com/vapor/vapor) (swift)
12. [router.cr](https://github.com/tbrand/router.cr) (crystal)
13. [fastify](https://github.com/fastify/fastify) (node)
14. [raze](https://github.com/samueleaton/raze) (crystal)
15. [polka](https://github.com/lukeed/polka) (node)
16. [plug](https://github.com/elixir-lang/plug) (elixir)
17. [spider-gazelle](https://github.com/spider-gazelle/spider-gazelle) (crystal)
18. [act](https://github.com/actframework/actframework) (java)
19. [amber](https://github.com/amberframework/amber) (crystal)
20. [roda](https://github.com/jeremyevans/roda) (ruby)
21. [express](https://github.com/expressjs/express) (node)
22. [lucky](https://github.com/luckyframework/lucky) (crystal)
23. [kemal](https://github.com/kemalcr/kemal) (crystal)
24. [perfect](https://github.com/PerfectlySoft/Perfect) (swift)
25. [phoenix](https://github.com/phoenixframework/phoenix) (elixir)
26. [kitura](https://github.com/IBM-Swift/Kitura) (swift)
27. [aspnetcore](https://github.com/aspnet/Home) (csharp)
28. [rack-routing](https://github.com/iAmPlus/rack-routing) (ruby)
29. [sanic](https://github.com/channelcat/sanic) (python)
30. [flame](https://github.com/AlexWayfer/flame) (ruby)
31. [akkahttp](https://github.com/akka/akka-http) (scala)
32. [flask](https://github.com/pallets/flask) (python)
33. [sinatra](https://github.com/sinatra/sinatra) (ruby)
34. [django](https://github.com/django/django) (python)
35. [jester](https://github.com/dom96/jester) (nim)
36. [mofuw](https://github.com/2vg/mofuw) (nim)
37. [rails](https://github.com/rails/rails) (ruby)
38. [tornado](https://github.com/tornadoweb/tornado) (python)
39. [gin](https://github.com/gin-gonic/gin) (go)

### Ranking (Language)

1. go ([fasthttprouter](https://github.com/buaazp/fasthttprouter))
2. rust ([rocket](https://github.com/SergioBenitez/Rocket))
3. cpp ([evhtp](https://github.com/criticalstack/libevhtp))
4. python ([japronto](https://github.com/squeaky-pl/japronto))
5. swift ([vapor](https://github.com/vapor/vapor))
6. crystal ([router.cr](https://github.com/tbrand/router.cr))
7. node ([fastify](https://github.com/fastify/fastify))
8. elixir ([plug](https://github.com/elixir-lang/plug))
9. java ([act](https://github.com/actframework/actframework))
10. ruby ([roda](https://github.com/jeremyevans/roda))
11. csharp ([aspnetcore](https://github.com/aspnet/Home))
12. scala ([akkahttp](https://github.com/akka/akka-http))
13. nim ([jester](https://github.com/dom96/jester))

### All frameworks

| Language (Runtime)        | Framework (Middleware)    |         Minimum |         Maximum |         Average |
|---------------------------|---------------------------|-----------------|-----------------|-----------------|
| ruby                      | rails                     |             674 |           60534 |            2405 |
| ruby                      | sinatra                   |             204 |           27742 |             819 |
| ruby                      | roda                      |              73 |           18256 |             282 |
| ruby                      | rack-routing              |              90 |           21269 |             340 |
| ruby                      | flame                     |             141 |           42077 |             647 |
| crystal                   | kemal                     |              29 |            5698 |             285 |
| crystal                   | router.cr                 |              27 |            8791 |             246 |
| crystal                   | raze                      |              27 |            9522 |             257 |
| crystal                   | lucky                     |              28 |            7530 |             285 |
| crystal                   | amber                     |              28 |            6487 |             276 |
| crystal                   | spider-gazelle            |              26 |            8943 |             269 |
| go                        | echo                      |              35 |           15384 |             190 |
| go                        | gorilla-mux               |              33 |           19486 |             222 |
| go                        | iris                      |              33 |           20959 |             204 |
| go                        | fasthttprouter            |              23 |           15676 |             120 |
| go                        | gin                       |              46 |          251860 |            8248 |
| rust                      | actix-web                 |              26 |           16757 |             163 |
| rust                      | iron                      |              43 |           15754 |             176 |
| rust                      | nickel                    |              35 |           16226 |             173 |
| rust                      | rocket                    |              37 |           16426 |             141 |
| node                      | express                   |              52 |           29245 |             284 |
| node                      | fastify                   |              35 |           23115 |             250 |
| node                      | polka                     |              33 |           30454 |             260 |
| elixir                    | plug                      |              52 |           10370 |             263 |
| elixir                    | phoenix                   |              62 |           16288 |             297 |
| swift                     | vapor                     |              64 |           23145 |             244 |
| swift                     | perfect                   |              36 |           21161 |             287 |
| swift                     | kitura                    |              92 |           15020 |             312 |
| scala                     | akkahttp                  |              51 |          365932 |             652 |
| csharp                    | aspnetcore                |              47 |          241092 |             317 |
| python                    | sanic                     |             120 |           17025 |             363 |
| python                    | japronto                  |              26 |            7221 |             175 |
| python                    | flask                     |             262 |           18447 |             763 |
| python                    | django                    |             392 |           29087 |            1124 |
| python                    | tornado                   |            1100 |           21074 |            6916 |
| nim                       | jester                    |              56 |           32982 |            1239 |
| nim                       | mofuw                     |              31 |           88467 |            2339 |
| java                      | act                       |              28 |          129573 |             275 |
| cpp                       | evhtp                     |              28 |           16019 |             165 |
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

# Which is the fastest?

Measuring response times (routing times) for each framework (middleware).
Each framework has to have two features; routing and parsing path parameters.

## Result

<!-- Result from here -->
Last update: 2018-03-15
```
OS: Darwin (version: 17.3.0, arch: x86_64)
CPU Cores: 8
```

### Ranking (Framework)

1. [nickel](https://github.com/nickel-org/nickel.rs) (rust)
2. [japronto](https://github.com/squeaky-pl/japronto) (python)
3. [iron](https://github.com/iron/iron) (rust)
4. [router_cr](https://github.com/tbrand/router.cr) (crystal)
5. [spider-gazelle](https://github.com/spider-gazelle/spider-gazelle) (crystal)
6. [raze](https://github.com/samueleaton/raze) (crystal)
7. [actix](https://github.com/actix/actix-web) (rust)
8. [fasthttprouter](https://github.com/buaazp/fasthttprouter) (go)
9. [amber](https://github.com/amberframework/amber) (crystal)
10. [lucky](https://github.com/luckyframework/lucky) (crystal)
11. [rocket](https://github.com/SergioBenitez/Rocket) (rust)
12. [kemal](https://github.com/kemalcr/kemal) (crystal)
13. [clusterpolka](https://github.com/lukeed/polka) (node)
14. [iris](https://github.com/kataras/iris) (go)
15. [gorilla_mux](https://github.com/gorilla/mux) (go)
16. [echo](https://github.com/labstack/echo) (go)
17. [gin](https://github.com/gin-gonic/gin) (go)
18. [vapor](https://github.com/vapor/vapor) (swift)
19. [aspnetcore](https://github.com/aspnet/Home) (csharp)
20. [perfect](https://github.com/PerfectlySoft/Perfect) (swift)
21. [sanic](https://github.com/channelcat/sanic) (python)
22. [polka](https://github.com/lukeed/polka) (node)
23. [kitura](https://github.com/IBM-Swift/Kitura) (swift)
24. [clusterexpress](https://github.com/LearnBoost/cluster) (node)
25. [plug](https://github.com/elixir-lang/plug) (elixir)
26. [phoenix](https://github.com/phoenixframework/phoenix) (elixir)
27. [akkahttp](https://github.com/akka/akka-http) (scala)
28. [express](https://github.com/expressjs/express) (node)
29. [roda](https://github.com/jeremyevans/roda) (ruby)
30. [rack-routing](https://github.com/iAmPlus/rack-routing) (ruby)
31. [criollo](https://github.com/thecatalinstan/criollo) (objc)
32. [jester](https://github.com/dom96/jester) (nim)
33. [sinatra](https://github.com/sinatra/sinatra) (ruby)
34. [tornado](https://github.com/tornadoweb/tornado) (python)
35. [rails](https://github.com/rails/rails) (ruby)

### Ranking (Language)

1. rust ([nickel](https://github.com/nickel-org/nickel.rs))
2. python ([japronto](https://github.com/squeaky-pl/japronto))
3. crystal ([router_cr](https://github.com/tbrand/router.cr))
4. go ([fasthttprouter](https://github.com/buaazp/fasthttprouter))
5. node ([clusterpolka](https://github.com/lukeed/polka))
6. swift ([vapor](https://github.com/vapor/vapor))
7. csharp ([aspnetcore](https://github.com/aspnet/Home))
8. elixir ([plug](https://github.com/elixir-lang/plug))
9. scala ([akkahttp](https://github.com/akka/akka-http))
10. ruby ([roda](https://github.com/jeremyevans/roda))
11. objc ([criollo](https://github.com/thecatalinstan/criollo))
12. nim ([jester](https://github.com/dom96/jester))

### All frameworks

| Language (Runtime)        | Framework (Middleware)    |       Max [sec] |       Min [sec] |       Ave [sec] |
|---------------------------|---------------------------|-----------------|-----------------|-----------------|
| ruby                      | rails                     |      183.065177 |      182.594138 |      182.784460 |
| ruby                      | sinatra                   |       54.599611 |       54.235050 |       54.360554 |
| ruby                      | roda                      |       19.539005 |       19.362119 |       19.428262 |
| ruby                      | rack-routing              |       24.912685 |       24.787798 |       24.857921 |
| crystal                   | kemal                     |        5.239213 |        4.743690 |        4.857822 |
| crystal                   | router_cr                 |        4.066965 |        3.929231 |        4.015780 |
| crystal                   | raze                      |        4.274440 |        3.952904 |        4.073834 |
| crystal                   | lucky                     |        4.407409 |        4.320964 |        4.377559 |
| crystal                   | amber                     |        4.422603 |        4.302611 |        4.371277 |
| crystal                   | spider-gazelle            |        4.111804 |        3.961091 |        4.038874 |
| go                        | echo                      |        5.706486 |        5.195444 |        5.458082 |
| go                        | gorilla_mux               |        5.688501 |        5.039164 |        5.442471 |
| go                        | iris                      |        5.540159 |        5.014120 |        5.355044 |
| go                        | fasthttprouter            |        4.363762 |        4.202933 |        4.299051 |
| go                        | gin                       |        6.022577 |        5.461489 |        5.817580 |
| rust                      | actix                     |        4.239260 |        4.058780 |        4.103622 |
| rust                      | iron                      |        3.980654 |        3.918856 |        3.944900 |
| rust                      | nickel                    |        3.922851 |        3.836264 |        3.886912 |
| rust                      | rocket                    |        4.922653 |        4.449431 |        4.709064 |
| node                      | express                   |       15.727199 |       14.595424 |       14.948420 |
| node                      | clusterexpress            |       10.672635 |        8.388129 |        9.267956 |
| node                      | polka                     |        8.788398 |        8.311747 |        8.478669 |
| node                      | clusterpolka              |        5.425973 |        5.079739 |        5.266841 |
| elixir                    | plug                      |       10.217389 |        9.375499 |        9.780671 |
| elixir                    | phoenix                   |       10.176699 |        9.456586 |       10.015052 |
| swift                     | vapor                     |        6.805480 |        6.259005 |        6.561916 |
| swift                     | perfect                   |        7.729230 |        7.527969 |        7.624530 |
| swift                     | kitura                    |        9.018302 |        8.751379 |        8.857953 |
| scala                     | akkahttp                  |       11.285840 |        9.558254 |       10.281555 |
| csharp                    | aspnetcore                |        7.064339 |        6.611566 |        6.810423 |
| python                    | sanic                     |        8.930118 |        7.300009 |        8.129179 |
| python                    | japronto                  |        4.255003 |        3.793259 |        3.926455 |
| python                    | tornado                   |       99.925639 |       98.832093 |       99.228150 |
| nim                       | jester                    |       35.604124 |       34.916220 |       35.241652 |
| objc                      | criollo                   |       32.341102 |       31.504481 |       31.896720 |
<!-- Result till here -->

## Current target frameworks (middlewares)

 - Ruby
   - [Rails](https://github.com/rails/rails)
   - [Sinatra](https://github.com/sinatra/sinatra)
   - [Roda](https://github.com/jeremyevans/roda)
   - [Rack-Routing](https://github.com/georgeu2000/rack-routing)
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
   - [express/cluster](https://github.com/LearnBoost/cluster)
   - [polka](https://github.com/lukeed/polka)
   - [polka/cluster](https://github.com/lukeed/polka)
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

## Donate

```
1AE9P6TUVik1rJGQhaSqGWRk1oAQ3DJnmo
```

![QRCode](https://user-images.githubusercontent.com/3483230/30004198-ccfeb086-9105-11e7-821c-927e4aa7af70.png)

# Which is the fastest?

Measuring response times (routing times) for each framework (middleware).
Each framework has to have two features; routing and parsing path parameters.

Any idea is :heart:, let discuss about it on [![Join the chat at https://gitter.im/which_is_the_fastest/Lobby](https://badges.gitter.im/which_is_the_fastest/Lobby.svg)](https://gitter.im/which_is_the_fastest/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Result

<!-- Result from here -->
Last update: 2018-04-10
```
OS: Darwin (version: 17.3.0, arch: x86_64)
CPU Cores: 8
```

### Ranking (Framework)

1. [router_cr](https://github.com/tbrand/router.cr) (crystal)
2. [iron](https://github.com/iron/iron) (rust)
3. [raze](https://github.com/samueleaton/raze) (crystal)
4. [nickel](https://github.com/nickel-org/nickel.rs) (rust)
5. [japronto](https://github.com/squeaky-pl/japronto) (python)
6. [actix](https://github.com/actix/actix-web) (rust)
7. [spider-gazelle](https://github.com/spider-gazelle/spider-gazelle) (crystal)
8. [fasthttprouter](https://github.com/buaazp/fasthttprouter) (go)
9. [mofuw](https://github.com/2vg/mofuw) (nim)
10. [lucky](https://github.com/luckyframework/lucky) (crystal)
11. [amber](https://github.com/amberframework/amber) (crystal)
12. [kemal](https://github.com/kemalcr/kemal) (crystal)
13. [rocket](https://github.com/SergioBenitez/Rocket) (rust)
14. [clusterpolka](https://github.com/lukeed/polka) (node)
15. [iris](https://github.com/kataras/iris) (go)
16. [gorilla_mux](https://github.com/gorilla/mux) (go)
17. [echo](https://github.com/labstack/echo) (go)
18. [phoenix](https://github.com/phoenixframework/phoenix) (elixir)
19. [gin](https://github.com/gin-gonic/gin) (go)
20. [plug](https://github.com/elixir-lang/plug) (elixir)
21. [aspnetcore](https://github.com/aspnet/Home) (csharp)
22. [vapor](https://github.com/vapor/vapor) (swift)
23. [sanic](https://github.com/channelcat/sanic) (python)
24. [perfect](https://github.com/PerfectlySoft/Perfect) (swift)
25. [kitura](https://github.com/IBM-Swift/Kitura) (swift)
26. [jester](https://github.com/dom96/jester) (nim)
27. [polka](https://github.com/lukeed/polka) (node)
28. [clusterexpress](https://github.com/LearnBoost/cluster) (node)
29. [akkahttp](https://github.com/akka/akka-http) (scala)
30. [express](https://github.com/expressjs/express) (node)
31. [roda](https://github.com/jeremyevans/roda) (ruby)
32. [rack-routing](https://github.com/iAmPlus/rack-routing) (ruby)
33. [criollo](https://github.com/thecatalinstan/criollo) (objc)
34. [sinatra](https://github.com/sinatra/sinatra) (ruby)
35. [tornado](https://github.com/tornadoweb/tornado) (python)
36. [rails](https://github.com/rails/rails) (ruby)

### Ranking (Language)

1. crystal ([router_cr](https://github.com/tbrand/router.cr))
2. rust ([iron](https://github.com/iron/iron))
3. python ([japronto](https://github.com/squeaky-pl/japronto))
4. go ([fasthttprouter](https://github.com/buaazp/fasthttprouter))
5. nim ([mofuw](https://github.com/2vg/mofuw))
6. node ([clusterpolka](https://github.com/lukeed/polka))
7. elixir ([phoenix](https://github.com/phoenixframework/phoenix))
8. csharp ([aspnetcore](https://github.com/aspnet/Home))
9. swift ([vapor](https://github.com/vapor/vapor))
10. scala ([akkahttp](https://github.com/akka/akka-http))
11. ruby ([roda](https://github.com/jeremyevans/roda))
12. objc ([criollo](https://github.com/thecatalinstan/criollo))

### All frameworks

| Language (Runtime)        | Framework (Middleware)    |       Max [sec] |       Min [sec] |       Ave [sec] |
|---------------------------|---------------------------|-----------------|-----------------|-----------------|
| ruby                      | rails                     |      350.161733 |      349.403240 |      349.758695 |
| ruby                      | sinatra                   |      100.885575 |      100.654648 |      100.776388 |
| ruby                      | roda                      |       35.200353 |       34.684047 |       34.891702 |
| ruby                      | rack-routing              |       45.339112 |       44.963142 |       45.080912 |
| crystal                   | kemal                     |        8.239321 |        7.847715 |        8.080067 |
| crystal                   | router_cr                 |        6.643287 |        6.410330 |        6.555974 |
| crystal                   | raze                      |        6.931865 |        6.664825 |        6.848396 |
| crystal                   | lucky                     |        8.145836 |        7.735874 |        8.010866 |
| crystal                   | amber                     |        8.090061 |        7.831198 |        8.011325 |
| crystal                   | spider-gazelle            |        7.240015 |        7.046256 |        7.192485 |
| go                        | echo                      |       10.205904 |        9.653988 |        9.989534 |
| go                        | gorilla_mux               |        9.971818 |        9.486147 |        9.743084 |
| go                        | iris                      |        9.732393 |        9.387682 |        9.565645 |
| go                        | fasthttprouter            |        7.592036 |        7.326633 |        7.452470 |
| go                        | gin                       |       10.973279 |       10.560217 |       10.811344 |
| rust                      | actix                     |        7.199203 |        6.997528 |        7.099141 |
| rust                      | iron                      |        6.801253 |        6.679035 |        6.710507 |
| rust                      | nickel                    |        6.936677 |        6.786194 |        6.876099 |
| rust                      | rocket                    |        8.657137 |        8.349306 |        8.504091 |
| node                      | express                   |       28.081658 |       26.611051 |       27.016107 |
| node                      | clusterexpress            |       17.409648 |       15.773684 |       16.309491 |
| node                      | polka                     |       15.867429 |       15.066904 |       15.401819 |
| node                      | clusterpolka              |        9.363300 |        9.197916 |        9.309864 |
| elixir                    | plug                      |       11.253845 |       10.597757 |       10.876218 |
| elixir                    | phoenix                   |       11.220435 |       10.087917 |       10.668334 |
| swift                     | vapor                     |       12.498265 |       12.178270 |       12.336104 |
| swift                     | perfect                   |       12.900839 |       12.444516 |       12.684239 |
| swift                     | kitura                    |       13.263198 |       12.493619 |       12.940476 |
| scala                     | akkahttp                  |       18.595948 |       16.553332 |       17.078369 |
| csharp                    | aspnetcore                |       10.932268 |       10.829186 |       10.887798 |
| python                    | sanic                     |       12.986084 |       11.706889 |       12.437696 |
| python                    | japronto                  |        7.116580 |        7.002742 |        7.055527 |
| python                    | tornado                   |      176.956387 |      176.746874 |      176.855119 |
| nim                       | jester                    |       14.122242 |       14.050893 |       14.082330 |
| nim                       | mofuw                     |        8.012292 |        7.701051 |        7.814884 |
| objc                      | criollo                   |       57.233161 |       55.853540 |       56.561862 |
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

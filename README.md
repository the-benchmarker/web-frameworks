# Which is the fastest?

Measuring response times (routing times) for each framework (middleware).
Each framework has to have two features; routing and parsing path parameters.

## Result

<!-- Result from here -->
Last update: 2018-02-12
```
OS: Darwin (version: 17.3.0, arch: x86_64)
CPU Cores: 8
```

### Ranking (Framework)

1. [router_cr](https://github.com/tbrand/router.cr) (crystal)
2. [actix](https://github.com/actix/actix-web) (rust)
3. [nickel](https://github.com/nickel-org/nickel.rs) (rust)
4. [iron](https://github.com/iron/iron) (rust)
5. [raze](https://github.com/samueleaton/raze) (crystal)
6. [japronto](https://github.com/squeaky-pl/japronto) (python)
7. [fasthttprouter](https://github.com/buaazp/fasthttprouter) (go)
8. [kemal](https://github.com/kemalcr/kemal) (crystal)
9. [rocket](https://github.com/SergioBenitez/Rocket) (rust)
10. [clusterpolka](https://github.com/lukeed/polka) (node)
11. [echo](https://github.com/labstack/echo) (go)
12. [iris](https://github.com/kataras/iris) (go)
13. [gorilla_mux](https://github.com/gorilla/mux) (go)
14. [aspnetcore](https://github.com/aspnet/Home) (csharp)
15. [plug](https://github.com/elixir-lang/plug) (elixir)
16. [gin](https://github.com/gin-gonic/gin) (go)
17. [phoenix](https://github.com/phoenixframework/phoenix) (elixir)
18. [vapor](https://github.com/vapor/vapor) (swift)
19. [perfect](https://github.com/PerfectlySoft/Perfect) (swift)
20. [sanic](https://github.com/channelcat/sanic) (python)
21. [polka](https://github.com/lukeed/polka) (node)
22. [kitura](https://github.com/IBM-Swift/Kitura) (swift)
23. [clusterexpress](https://github.com/LearnBoost/cluster) (node)
24. [akkahttp](https://github.com/akka/akka-http) (scala)
25. [express](https://github.com/expressjs/express) (node)
26. [roda](https://github.com/jeremyevans/roda) (ruby)
27. [jester](https://github.com/dom96/jester) (nim)
28. [criollo](https://github.com/thecatalinstan/criollo) (objc)
29. [sinatra](https://github.com/sinatra/sinatra) (ruby)
30. [rails](https://github.com/rails/rails) (ruby)

### Ranking (Language)

1. crystal ([router_cr](https://github.com/tbrand/router.cr))
2. rust ([actix](https://github.com/actix/actix-web))
3. python ([japronto](https://github.com/squeaky-pl/japronto))
4. go ([fasthttprouter](https://github.com/buaazp/fasthttprouter))
5. node ([clusterpolka](https://github.com/lukeed/polka))
6. csharp ([aspnetcore](https://github.com/aspnet/Home))
7. elixir ([plug](https://github.com/elixir-lang/plug))
8. swift ([vapor](https://github.com/vapor/vapor))
9. scala ([akkahttp](https://github.com/akka/akka-http))
10. ruby ([roda](https://github.com/jeremyevans/roda))
11. nim ([jester](https://github.com/dom96/jester))
12. objc ([criollo](https://github.com/thecatalinstan/criollo))

### All frameworks

| Language (Runtime)        | Framework (Middleware)    |       Max [sec] |       Min [sec] |       Ave [sec] |
|---------------------------|---------------------------|-----------------|-----------------|-----------------|
| ruby                      | rails                     |      170.367628 |      169.936902 |      170.126046 |
| ruby                      | sinatra                   |       48.778433 |       48.624297 |       48.669990 |
| ruby                      | roda                      |       18.932657 |       18.894802 |       18.912907 |
| crystal                   | kemal                     |        4.106407 |        3.966922 |        4.064647 |
| crystal                   | router_cr                 |        3.405136 |        3.237196 |        3.341510 |
| crystal                   | raze                      |        3.560398 |        3.404760 |        3.518680 |
| go                        | echo                      |        5.191288 |        4.834103 |        4.988960 |
| go                        | gorilla_mux               |        5.275145 |        5.093143 |        5.208652 |
| go                        | iris                      |        5.282357 |        4.888284 |        5.075264 |
| go                        | fasthttprouter            |        3.736463 |        3.618410 |        3.686607 |
| go                        | gin                       |        5.641557 |        5.135107 |        5.393527 |
| rust                      | actix                     |        3.387631 |        3.303975 |        3.342585 |
| rust                      | iron                      |        3.554686 |        3.451469 |        3.501088 |
| rust                      | nickel                    |        3.570879 |        3.397856 |        3.477659 |
| rust                      | rocket                    |        4.254425 |        4.146578 |        4.195467 |
| node                      | express                   |       14.019920 |       12.905849 |       13.208341 |
| node                      | clusterexpress            |        8.666874 |        7.384296 |        7.777996 |
| node                      | polka                     |        7.687397 |        7.284888 |        7.389873 |
| node                      | clusterpolka              |        4.595137 |        4.201701 |        4.309777 |
| elixir                    | plug                      |        5.528723 |        5.223662 |        5.355764 |
| elixir                    | phoenix                   |        5.858134 |        5.154151 |        5.482702 |
| swift                     | vapor                     |        6.498268 |        5.940431 |        6.122171 |
| swift                     | perfect                   |        6.644757 |        6.035297 |        6.239225 |
| swift                     | kitura                    |        8.151191 |        7.400613 |        7.721501 |
| scala                     | akkahttp                  |        9.505543 |        8.379722 |        8.877244 |
| csharp                    | aspnetcore                |        5.478113 |        4.890794 |        5.235464 |
| python                    | sanic                     |        7.562353 |        6.305690 |        6.971293 |
| python                    | japronto                  |        3.569385 |        3.470750 |        3.523741 |
| nim                       | jester                    |       27.408656 |       27.176336 |       27.255200 |
| objc                      | criollo                   |       32.316814 |       27.295537 |       29.580028 |
<!-- Result till here -->

## Current target frameworks (middlewares)

 - Ruby
   - [Rails](https://github.com/rails/rails)
   - [Sinatra](https://github.com/sinatra/sinatra)
   - [Roda](https://github.com/jeremyevans/roda)
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

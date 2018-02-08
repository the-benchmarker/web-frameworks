# Which is the fastest?

Measuring response times (routing times) for each framework (middleware).
Each framework has to have two features; routing and parsing path parameters.

## Result

<!-- Result from here -->
Last update: 2018-01-18
```
OS: Darwin (version: 17.3.0, arch: x86_64)
CPU Cores: 8
```

### Ranking (Framework)

1. [iron](https://github.com/iron/iron) (rust)
2. [router_cr](https://github.com/tbrand/router.cr) (crystal)
3. [nickel](https://github.com/nickel-org/nickel.rs) (rust)
4. [raze](https://github.com/samueleaton/raze) (crystal)
5. [japronto](https://github.com/squeaky-pl/japronto) (python)
6. [fasthttprouter](https://github.com/buaazp/fasthttprouter) (go)
7. [kemal](https://github.com/kemalcr/kemal) (crystal)
8. [rocket](https://github.com/SergioBenitez/Rocket) (rust)
9. [iris](https://github.com/kataras/iris) (go)
10. [echo](https://github.com/labstack/echo) (go)
11. [aspnetcore](https://github.com/aspnet/Home) (csharp)
12. [gorilla_mux](https://github.com/gorilla/mux) (go)
13. [plug](https://github.com/elixir-lang/plug) (elixir)
14. [gin](https://github.com/gin-gonic/gin) (go)
15. [phoenix](https://github.com/phoenixframework/phoenix) (elixir)
16. [vapor](https://github.com/vapor/vapor) (swift)
17. [perfect](https://github.com/PerfectlySoft/Perfect) (swift)
18. [sanic](https://github.com/channelcat/sanic) (python)
19. [kitura](https://github.com/IBM-Swift/Kitura) (swift)
20. [clusterexpress](https://github.com/LearnBoost/cluster) (node)
21. [akkahttp](https://github.com/akka/akka-http) (scala)
22. [express](https://github.com/expressjs/express) (node)
23. [roda](https://github.com/jeremyevans/roda) (ruby)
24. [jester](https://github.com/dom96/jester) (nim)
25. [criollo](https://github.com/thecatalinstan/criollo) (objc)
26. [sinatra](https://github.com/sinatra/sinatra) (ruby)
27. [rails](https://github.com/rails/rails) (ruby)

### Ranking (Language)

1. rust ([iron](https://github.com/iron/iron))
2. crystal ([router_cr](https://github.com/tbrand/router.cr))
3. python ([japronto](https://github.com/squeaky-pl/japronto))
4. go ([fasthttprouter](https://github.com/buaazp/fasthttprouter))
5. csharp ([aspnetcore](https://github.com/aspnet/Home))
6. elixir ([plug](https://github.com/elixir-lang/plug))
7. swift ([vapor](https://github.com/vapor/vapor))
8. node ([clusterexpress](https://github.com/LearnBoost/cluster))
9. scala ([akkahttp](https://github.com/akka/akka-http))
10. ruby ([roda](https://github.com/jeremyevans/roda))
11. nim ([jester](https://github.com/dom96/jester))
12. objc ([criollo](https://github.com/thecatalinstan/criollo))

### All frameworks

| Language (Runtime)        | Framework (Middleware)    |       Max [sec] |       Min [sec] |       Ave [sec] |
|---------------------------|---------------------------|-----------------|-----------------|-----------------|
| ruby                      | rails                     |      169.990758 |      169.245815 |      169.500696 |
| ruby                      | sinatra                   |       48.785529 |       48.654910 |       48.711666 |
| ruby                      | roda                      |       18.727357 |       18.608974 |       18.661612 |
| crystal                   | kemal                     |        4.178043 |        4.054511 |        4.129273 |
| crystal                   | router_cr                 |        3.455170 |        3.376436 |        3.427589 |
| crystal                   | raze                      |        3.562387 |        3.444175 |        3.536099 |
| go                        | echo                      |        5.256643 |        4.904162 |        5.048713 |
| go                        | gorilla_mux               |        5.390430 |        4.980591 |        5.142020 |
| go                        | iris                      |        5.244560 |        4.795147 |        4.989660 |
| go                        | fasthttprouter            |        3.715726 |        3.595522 |        3.662270 |
| go                        | gin                       |        5.682742 |        5.357033 |        5.502924 |
| rust                      | iron                      |        3.485740 |        3.357875 |        3.412701 |
| rust                      | nickel                    |        3.485643 |        3.416737 |        3.464607 |
| rust                      | rocket                    |        4.260527 |        4.139049 |        4.211128 |
| node                      | express                   |       13.895781 |       13.413687 |       13.619828 |
| node                      | clusterexpress            |        9.048196 |        7.710249 |        8.209995 |
| elixir                    | plug                      |        5.599585 |        5.259777 |        5.456209 |
| elixir                    | phoenix                   |        5.693897 |        5.393461 |        5.618488 |
| swift                     | vapor                     |        6.296311 |        5.985685 |        6.120673 |
| swift                     | perfect                   |        6.570743 |        6.186247 |        6.362126 |
| swift                     | kitura                    |        7.643707 |        7.318455 |        7.467837 |
| scala                     | akkahttp                  |        9.675075 |        7.849877 |        8.536331 |
| csharp                    | aspnetcore                |        5.405356 |        4.861280 |        5.128910 |
| python                    | sanic                     |        7.291824 |        6.076758 |        6.445577 |
| python                    | japronto                  |        3.587456 |        3.509572 |        3.544309 |
| nim                       | jester                    |       27.304759 |       27.177061 |       27.240023 |
| objc                      | criollo                   |       28.879014 |       27.740031 |       28.389459 |
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

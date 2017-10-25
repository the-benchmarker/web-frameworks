# Which is the fastest?

Measuring response times for each framework (middleware).
Each framework has to have two features; routing and parsing path parameters.

## Result

<!-- Result from here -->
Last update: 2017-10-04
```
OS: Darwin (version: 17.0.0, arch: x86_64)
CPU Cores: 8
```

### Ranking (Framework)

1. [router_cr](https://github.com/tbrand/router.cr) (crystal)
2. [raze](https://github.com/samueleaton/raze) (crystal)
3. [japronto](https://github.com/squeaky-pl/japronto) (python)
4. [fasthttprouter](https://github.com/buaazp/fasthttprouter) (go)
5. [iron](https://github.com/iron/iron) (rust)
6. [nickel](https://github.com/nickel-org/nickel.rs) (rust)
7. [rocket](https://github.com/SergioBenitez/Rocket) (rust)
8. [kemal](https://github.com/kemalcr/kemal) (crystal)
9. [echo](https://github.com/labstack/echo) (go)
10. [gorilla_mux](https://github.com/gorilla/mux) (go)
11. [iris](https://github.com/kataras/iris) (go)
12. [aspnetcore](https://github.com/aspnet/Home) (csharp)
13. [plug](https://github.com/elixir-lang/plug) (elixir)
14. [phoenix](https://github.com/phoenixframework/phoenix) (elixir)
15. [vapor](https://github.com/vapor/vapor) (swift)
16. [perfect](https://github.com/PerfectlySoft/Perfect) (swift)
17. [kitura](https://github.com/IBM-Swift/Kitura) (swift)
18. [sanic](https://github.com/channelcat/sanic) (python)
19. [akkahttp](https://github.com/akka/akka-http) (scala)
20. [clusterexpress](https://github.com/LearnBoost/cluster) (node)
21. [express](https://github.com/expressjs/express) (node)
22. [roda](https://github.com/jeremyevans/roda) (ruby)
23. [sinatra](https://github.com/sinatra/sinatra) (ruby)
24. [rails](https://github.com/rails/rails) (ruby)

### Ranking (Language)

1. crystal ([router_cr](https://github.com/tbrand/router.cr))
2. python ([japronto](https://github.com/squeaky-pl/japronto))
3. go ([fasthttprouter](https://github.com/buaazp/fasthttprouter))
4. rust ([iron](https://github.com/iron/iron))
5. csharp ([aspnetcore](https://github.com/aspnet/Home))
6. elixir ([plug](https://github.com/elixir-lang/plug))
7. swift ([vapor](https://github.com/vapor/vapor))
8. scala ([akkahttp](https://github.com/akka/akka-http))
9. node ([clusterexpress](https://github.com/LearnBoost/cluster))
10. ruby ([roda](https://github.com/jeremyevans/roda))

### All frameworks

| Language (Runtime)        | Framework (Middleware)    |       Max [sec] |       Min [sec] |       Ave [sec] |
|---------------------------|---------------------------|-----------------|-----------------|-----------------|
| ruby                      | rails                     |      151.196547 |      150.960227 |      151.073975 |
| ruby                      | sinatra                   |       43.854565 |       43.581662 |       43.664177 |
| ruby                      | roda                      |       15.892388 |       15.811082 |       15.844165 |
| crystal                   | kemal                     |        3.547369 |        3.075486 |        3.444593 |
| crystal                   | router_cr                 |        2.486733 |        2.284032 |        2.344394 |
| crystal                   | raze                      |        2.490715 |        2.399881 |        2.456708 |
| go                        | echo                      |        3.785530 |        3.511024 |        3.647536 |
| go                        | gorilla_mux               |        3.970627 |        3.522344 |        3.686079 |
| go                        | iris                      |        3.938069 |        3.583784 |        3.776195 |
| go                        | fasthttprouter            |        2.824961 |        2.686712 |        2.731345 |
| rust                      | iron                      |        2.865915 |        2.743227 |        2.812815 |
| rust                      | nickel                    |        2.877018 |        2.823066 |        2.858988 |
| rust                      | rocket                    |        3.276701 |        3.145324 |        3.204920 |
| node                      | express                   |       13.961471 |       13.668522 |       13.791606 |
| node                      | clusterexpress            |        8.187491 |        7.431857 |        7.656550 |
| elixir                    | plug                      |        4.895296 |        4.368241 |        4.727784 |
| elixir                    | phoenix                   |        5.307217 |        4.877070 |        5.116376 |
| swift                     | vapor                     |        5.613291 |        5.250472 |        5.479300 |
| swift                     | perfect                   |        5.703425 |        5.575321 |        5.648020 |
| swift                     | kitura                    |        6.678699 |        6.379810 |        6.503248 |
| scala                     | akkahttp                  |        8.330310 |        7.047381 |        7.584030 |
| csharp                    | aspnetcore                |        3.959679 |        3.799744 |        3.860993 |
| python                    | sanic                     |        7.359875 |        6.208559 |        6.735841 |
| python                    | japronto                  |        2.639756 |        2.590362 |        2.610999 |
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
 - Rust
   - [IRON](https://github.com/iron/iron)
   - [nickel.rs](https://github.com/nickel-org/nickel.rs)
   - [Rocket](https://rocket.rs) (nightly)
 - node
   - [express](https://github.com/expressjs/express)
   - [express/cluster](https://github.com/LearnBoost/cluster)
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
 - Objective-C
   - [Criollo](https://criollo.io/)

See Development section when you want to add new languages or frameworks.

## The rule

We want to know the response time, not a usability. So full-stack framework is at a disadvantage.
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
> neph -j ruby
```
For each framework,
```
> neph -j rails
```

See [neph.yml](https://github.com/tbrand/which_is_the_fastest/blob/master/neph.yml)

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
 - `/[language]/[framework]/[codes]` <- Project iteself
 - `benchmarker/benchmarker.cr` <- Adding it as a target to
 - `README.md` <- Adding it as a target framework of the list
 - `Makefile`
 - `neph.yml` (optional)

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

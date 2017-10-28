# Which is the fastest?

Measuring response times (routing times) for each framework (middleware).
Each framework has to have two features; routing and parsing path parameters.

## Result

<!-- Result from here -->
Last update: 2017-10-28
```
OS: Darwin (version: 17.0.0, arch: x86_64)
CPU Cores: 8
```

### Ranking (Framework)

1. [router_cr](https://github.com/tbrand/router.cr) (crystal)
2. [raze](https://github.com/samueleaton/raze) (crystal)
3. [japronto](https://github.com/squeaky-pl/japronto) (python)
4. [nickel](https://github.com/nickel-org/nickel.rs) (rust)
5. [fasthttprouter](https://github.com/buaazp/fasthttprouter) (go)
6. [iron](https://github.com/iron/iron) (rust)
7. [rocket](https://github.com/SergioBenitez/Rocket) (rust)
8. [kemal](https://github.com/kemalcr/kemal) (crystal)
9. [iris](https://github.com/kataras/iris) (go)
10. [gorilla_mux](https://github.com/gorilla/mux) (go)
11. [echo](https://github.com/labstack/echo) (go)
12. [aspnetcore](https://github.com/aspnet/Home) (csharp)
13. [plug](https://github.com/elixir-lang/plug) (elixir)
14. [phoenix](https://github.com/phoenixframework/phoenix) (elixir)
15. [vapor](https://github.com/vapor/vapor) (swift)
16. [sanic](https://github.com/channelcat/sanic) (python)
17. [perfect](https://github.com/PerfectlySoft/Perfect) (swift)
18. [kitura](https://github.com/IBM-Swift/Kitura) (swift)
19. [clusterexpress](https://github.com/LearnBoost/cluster) (node)
20. [akkahttp](https://github.com/akka/akka-http) (scala)
21. [express](https://github.com/expressjs/express) (node)
22. [roda](https://github.com/jeremyevans/roda) (ruby)
23. [criollo](https://github.com/thecatalinstan/criollo) (objc)
24. [sinatra](https://github.com/sinatra/sinatra) (ruby)
25. [rails](https://github.com/rails/rails) (ruby)

### Ranking (Language)

1. crystal ([router_cr](https://github.com/tbrand/router.cr))
2. python ([japronto](https://github.com/squeaky-pl/japronto))
3. rust ([nickel](https://github.com/nickel-org/nickel.rs))
4. go ([fasthttprouter](https://github.com/buaazp/fasthttprouter))
5. csharp ([aspnetcore](https://github.com/aspnet/Home))
6. elixir ([plug](https://github.com/elixir-lang/plug))
7. swift ([vapor](https://github.com/vapor/vapor))
8. node ([clusterexpress](https://github.com/LearnBoost/cluster))
9. scala ([akkahttp](https://github.com/akka/akka-http))
10. ruby ([roda](https://github.com/jeremyevans/roda))
11. objc ([criollo](https://github.com/thecatalinstan/criollo))

### All frameworks

| Language (Runtime)        | Framework (Middleware)    |       Max [sec] |       Min [sec] |       Ave [sec] |
|---------------------------|---------------------------|-----------------|-----------------|-----------------|
| ruby                      | rails                     |      158.252949 |      152.902179 |      155.655884 |
| ruby                      | sinatra                   |       50.694360 |       43.014136 |       47.820369 |
| ruby                      | roda                      |       18.021737 |       16.183754 |       17.532603 |
| crystal                   | kemal                     |        3.849068 |        3.183558 |        3.467156 |
| crystal                   | router_cr                 |        2.636737 |        2.432839 |        2.492161 |
| crystal                   | raze                      |        2.541773 |        2.506448 |        2.526932 |
| go                        | echo                      |        4.762311 |        3.949864 |        4.438283 |
| go                        | gorilla_mux               |        4.596907 |        4.098254 |        4.354651 |
| go                        | iris                      |        4.445337 |        3.879196 |        4.138097 |
| go                        | fasthttprouter            |        3.063812 |        2.892952 |        2.964931 |
| rust                      | iron                      |        3.500870 |        2.850328 |        3.111342 |
| rust                      | nickel                    |        3.056766 |        2.857153 |        2.942337 |
| rust                      | rocket                    |        3.564244 |        3.261656 |        3.375485 |
| node                      | express                   |       14.330640 |       14.023177 |       14.115579 |
| node                      | clusterexpress            |        8.468612 |        7.671729 |        7.898184 |
| elixir                    | plug                      |        5.822447 |        4.320761 |        5.261752 |
| elixir                    | phoenix                   |        6.393529 |        4.796465 |        5.914619 |
| swift                     | vapor                     |        6.776044 |        5.411954 |        6.354711 |
| swift                     | perfect                   |        7.103620 |        5.809812 |        6.627695 |
| swift                     | kitura                    |        8.353631 |        6.686213 |        7.891552 |
| scala                     | akkahttp                  |        9.341465 |        8.380177 |        8.819683 |
| csharp                    | aspnetcore                |        5.501347 |        4.215834 |        5.035063 |
| python                    | sanic                     |        7.135462 |        4.771837 |        6.409820 |
| python                    | japronto                  |        2.910558 |        2.688990 |        2.755750 |
| objc                      | criollo                   |       32.403479 |       26.284483 |       30.581767 |
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

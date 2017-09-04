# Which is the fastest?

Measuring response times for each framework (middleware).
Each framework has to have two features; routing and parsing path parameters.

## Result

<!-- Result from here -->
Last update: 2017-09-04

### Ranking (Framework)

1. [router_cr](https://github.com/tbrand/router.cr) (crystal)
2. [japronto](https://github.com/squeaky-pl/japronto) (python)
3. [raze](https://github.com/samueleaton/raze) (crystal)
4. [iron](https://github.com/iron/iron) (rust)
5. [nickel](https://github.com/nickel-org/nickel.rs) (rust)
6. [fasthttprouter](https://github.com/buaazp/fasthttprouter) (go)
7. [kemal](https://github.com/kemalcr/kemal) (crystal)
8. [rocket](https://github.com/SergioBenitez/Rocket) (rust)
9. [gorilla_mux](https://github.com/gorilla/mux) (go)
10. [echo](https://github.com/labstack/echo) (go)
11. [iris](https://github.com/kataras/iris) (go)
12. [aspnetcore](https://github.com/aspnet/Home) (csharp)
13. [plug](https://github.com/elixir-lang/plug) (elixir)
14. [vapor](https://github.com/vapor/vapor) (swift)
15. [phoenix](https://github.com/phoenixframework/phoenix) (elixir)
16. [sanic](https://github.com/channelcat/sanic) (python)
17. [perfect](https://github.com/PerfectlySoft/Perfect) (swift)
18. [akkahttp](https://github.com/akka/akka-http) (scala)
19. [clusterexpress](https://github.com/LearnBoost/cluster) (node)
20. [express](https://github.com/expressjs/express) (node)
21. [roda](https://github.com/jeremyevans/roda) (ruby)
22. [kitura](https://github.com/IBM-Swift/Kitura) (swift)
23. [sinatra](https://github.com/sinatra/sinatra) (ruby)
24. [rails](https://github.com/rails/rails) (ruby)

### Ranking (Language)

1. crystal ([router_cr](https://github.com/tbrand/router.cr))
2. python ([japronto](https://github.com/squeaky-pl/japronto))
3. rust ([iron](https://github.com/iron/iron))
4. go ([fasthttprouter](https://github.com/buaazp/fasthttprouter))
5. csharp ([aspnetcore](https://github.com/aspnet/Home))
6. elixir ([plug](https://github.com/elixir-lang/plug))
7. swift ([vapor](https://github.com/vapor/vapor))
8. scala ([akkahttp](https://github.com/akka/akka-http))
9. node ([clusterexpress](https://github.com/LearnBoost/cluster))
10. ruby ([roda](https://github.com/jeremyevans/roda))

### All frameworks

| Language (Runtime)        | Framework (Middleware)    |       Max [sec] |       Min [sec] |       Ave [sec] |
|---------------------------|---------------------------|-----------------|-----------------|-----------------|
| ruby                      | rails                     |      710.670133 |      701.146349 |      705.275191 |
| ruby                      | sinatra                   |       47.459479 |       45.941944 |       47.004057 |
| ruby                      | roda                      |       17.654319 |       17.167874 |       17.369515 |
| crystal                   | kemal                     |        3.776265 |        3.489867 |        3.644438 |
| crystal                   | router_cr                 |        2.936218 |        2.773093 |        2.858738 |
| crystal                   | raze                      |        3.085805 |        2.931826 |        2.997011 |
| go                        | echo                      |        4.628243 |        4.362947 |        4.466652 |
| go                        | gorilla_mux               |        4.524554 |        4.057697 |        4.336543 |
| go                        | iris                      |        4.573527 |        4.239478 |        4.474351 |
| go                        | fasthttprouter            |        3.258863 |        3.156528 |        3.217419 |
| rust                      | iron                      |        3.173863 |        3.076442 |        3.119547 |
| rust                      | nickel                    |        3.270208 |        3.136764 |        3.190911 |
| rust                      | rocket                    |        3.813496 |        3.613802 |        3.713490 |
| node                      | express                   |       15.901570 |       15.393989 |       15.658762 |
| node                      | clusterexpress            |        9.449904 |        8.807977 |        9.043189 |
| elixir                    | plug                      |        5.352757 |        4.955110 |        5.154404 |
| elixir                    | phoenix                   |        5.932869 |        5.470591 |        5.657195 |
| swift                     | vapor                     |        5.715677 |        5.256360 |        5.471084 |
| swift                     | perfect                   |        6.960515 |        6.750418 |        6.877417 |
| swift                     | kitura                    |       19.766912 |       18.392527 |       19.013893 |
| scala                     | akkahttp                  |        9.830623 |        7.925446 |        8.886830 |
| csharp                    | aspnetcore                |        5.171439 |        4.466433 |        4.724765 |
| python                    | sanic                     |        6.784137 |        5.388289 |        5.868620 |
| python                    | japronto                  |        3.075319 |        2.934478 |        2.985214 |
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

# Which is the fastest?

Measuring response times for each framework (middleware).
Each framework has to have two features; routing and parsing path parameters.

## Result

<!-- Result from here -->
Last update: 2017-09-04

### Ranking (Framework)

1. [router_cr](https://github.com/tbrand/router.cr)
2. [raze](https://github.com/samueleaton/raze)
3. [fasthttprouter](https://github.com/buaazp/fasthttprouter)
4. [iron](https://github.com/iron/iron)
5. [japronto](https://github.com/squeaky-pl/japronto)
6. [nickel](https://github.com/nickel-org/nickel.rs)
7. [kemal](https://github.com/kemalcr/kemal)
8. [rocket](https://github.com/SergioBenitez/Rocket)
9. [gorilla_mux](https://github.com/gorilla/mux)
10. [echo](https://github.com/labstack/echo)
11. [iris](https://github.com/kataras/iris)
12. [plug](https://github.com/elixir-lang/plug)
13. [vapor](https://github.com/vapor/vapor)
14. [phoenix](https://github.com/phoenixframework/phoenix)
15. [aspnetcore](https://github.com/aspnet/Home)
16. [sanic](https://github.com/channelcat/sanic)
17. [perfect](https://github.com/PerfectlySoft/Perfect)
18. [akkahttp](https://github.com/akka/akka-http)
19. [clusterexpress](https://github.com/LearnBoost/cluster)
20. [express](https://github.com/expressjs/express)
21. [roda](https://github.com/jeremyevans/roda)
22. [kitura](https://github.com/IBM-Swift/Kitura)
23. [sinatra](https://github.com/sinatra/sinatra)
24. [rails](https://github.com/rails/rails)

### Ranking (Language)

1. crystal ([router_cr](https://github.com/tbrand/router.cr))
2. go ([fasthttprouter](https://github.com/buaazp/fasthttprouter))
3. rust ([iron](https://github.com/iron/iron))
4. python ([japronto](https://github.com/squeaky-pl/japronto))
5. elixir ([plug](https://github.com/elixir-lang/plug))
6. swift ([vapor](https://github.com/vapor/vapor))
7. csharp ([aspnetcore](https://github.com/aspnet/Home))
8. scala ([akkahttp](https://github.com/akka/akka-http))
9. node ([clusterexpress](https://github.com/LearnBoost/cluster))
10. ruby ([roda](https://github.com/jeremyevans/roda))

### All frameworks

| Language (Runtime)        | Framework (Middleware)    |       Max [sec] |       Min [sec] |       Ave [sec] |
|---------------------------|---------------------------|-----------------|-----------------|-----------------|
| ruby                      | rails                     |      754.063778 |      708.440793 |      736.451491 |
| ruby                      | sinatra                   |       50.935940 |       46.196863 |       48.511964 |
| ruby                      | roda                      |       17.978084 |       17.067853 |       17.600964 |
| crystal                   | kemal                     |        4.005528 |        3.876987 |        3.913506 |
| crystal                   | router_cr                 |        3.022270 |        2.776481 |        2.865556 |
| crystal                   | raze                      |        3.527649 |        2.899719 |        3.052073 |
| go                        | echo                      |        4.565017 |        4.332744 |        4.458838 |
| go                        | gorilla_mux               |        4.783134 |        4.195671 |        4.391958 |
| go                        | iris                      |        4.870891 |        4.431526 |        4.719329 |
| go                        | fasthttprouter            |        3.426500 |        2.982375 |        3.178191 |
| rust                      | iron                      |        3.616533 |        3.052602 |        3.307757 |
| rust                      | nickel                    |        3.437849 |        3.268217 |        3.353121 |
| rust                      | rocket                    |        4.486677 |        4.151858 |        4.390674 |
| node                      | express                   |       16.297716 |       15.355201 |       15.641189 |
| node                      | clusterexpress            |       10.272147 |        8.916303 |        9.505865 |
| elixir                    | plug                      |        5.617337 |        5.169207 |        5.361000 |
| elixir                    | phoenix                   |        5.738947 |        5.306468 |        5.575212 |
| swift                     | vapor                     |        5.659526 |        5.240323 |        5.475122 |
| swift                     | perfect                   |        6.973852 |        6.734707 |        6.826379 |
| swift                     | kitura                    |       19.664970 |       17.669080 |       18.878266 |
| scala                     | akkahttp                  |       10.572538 |        8.800646 |        9.409009 |
| csharp                    | aspnetcore                |        5.886491 |        5.307668 |        5.608353 |
| python                    | sanic                     |        6.955599 |        5.493192 |        6.356439 |
| python                    | japronto                  |        3.375258 |        3.138797 |        3.315764 |
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

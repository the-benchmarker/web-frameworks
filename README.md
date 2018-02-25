# Which is the fastest?

Measuring response times (routing times) for each framework (middleware).
Each framework has to have two features; routing and parsing path parameters.

## Result

<!-- Result from here -->
Last update: 2018-02-26
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
8. [lucky](https://github.com/luckyframework/lucky) (crystal)
9. [amber](https://github.com/amberframework/amber) (crystal)
10. [kemal](https://github.com/kemalcr/kemal) (crystal)
11. [rocket](https://github.com/SergioBenitez/Rocket) (rust)
12. [clusterpolka](https://github.com/lukeed/polka) (node)
13. [echo](https://github.com/labstack/echo) (go)
14. [gorilla_mux](https://github.com/gorilla/mux) (go)
15. [aspnetcore](https://github.com/aspnet/Home) (csharp)
16. [iris](https://github.com/kataras/iris) (go)
17. [phoenix](https://github.com/phoenixframework/phoenix) (elixir)
18. [plug](https://github.com/elixir-lang/plug) (elixir)
19. [gin](https://github.com/gin-gonic/gin) (go)
20. [vapor](https://github.com/vapor/vapor) (swift)
21. [perfect](https://github.com/PerfectlySoft/Perfect) (swift)
22. [sanic](https://github.com/channelcat/sanic) (python)
23. [polka](https://github.com/lukeed/polka) (node)
24. [clusterexpress](https://github.com/LearnBoost/cluster) (node)
25. [kitura](https://github.com/IBM-Swift/Kitura) (swift)
26. [akkahttp](https://github.com/akka/akka-http) (scala)
27. [express](https://github.com/expressjs/express) (node)
28. [roda](https://github.com/jeremyevans/roda) (ruby)
29. [jester](https://github.com/dom96/jester) (nim)
30. [criollo](https://github.com/thecatalinstan/criollo) (objc)
31. [sinatra](https://github.com/sinatra/sinatra) (ruby)
32. [tornado](https://github.com/tornadoweb/tornado) (python)
33. [rack-routing](https://github.com/georgeu2000/rack-routing) (ruby)
34. [rails](https://github.com/rails/rails) (ruby)

### Ranking (Language)

1. crystal ([router_cr](https://github.com/tbrand/router.cr))
2. rust ([actix](https://github.com/actix/actix-web))
3. python ([japronto](https://github.com/squeaky-pl/japronto))
4. go ([fasthttprouter](https://github.com/buaazp/fasthttprouter))
5. node ([clusterpolka](https://github.com/lukeed/polka))
6. csharp ([aspnetcore](https://github.com/aspnet/Home))
7. elixir ([phoenix](https://github.com/phoenixframework/phoenix))
8. swift ([vapor](https://github.com/vapor/vapor))
9. scala ([akkahttp](https://github.com/akka/akka-http))
10. ruby ([roda](https://github.com/jeremyevans/roda))
11. nim ([jester](https://github.com/dom96/jester))
12. objc ([criollo](https://github.com/thecatalinstan/criollo))

### All frameworks

| Language (Runtime)        | Framework (Middleware)    |       Max [sec] |       Min [sec] |       Ave [sec] |
|---------------------------|---------------------------|-----------------|-----------------|-----------------|
| ruby                      | rails                     |      330.695639 |      329.748766 |      330.283498 |
| ruby                      | sinatra                   |       97.614918 |       97.473684 |       97.549466 |
| ruby                      | roda                      |       37.567316 |       37.482821 |       37.537436 |
| ruby                      | rack-routing              |      293.084913 |      291.885714 |      292.707013 |
| crystal                   | kemal                     |        8.471980 |        7.895288 |        8.240809 |
| crystal                   | router_cr                 |        6.663747 |        6.398756 |        6.580617 |
| crystal                   | raze                      |        7.046766 |        6.681810 |        6.931381 |
| crystal                   | lucky                     |        7.853848 |        7.483846 |        7.716000 |
| crystal                   | amber                     |        8.180518 |        7.714918 |        8.021653 |
| go                        | echo                      |       10.407790 |        9.657063 |       10.030704 |
| go                        | gorilla_mux               |       10.162614 |        9.867718 |       10.041358 |
| go                        | iris                      |       10.584802 |       10.062414 |       10.369381 |
| go                        | fasthttprouter            |        7.383870 |        7.237157 |        7.309896 |
| go                        | gin                       |       11.390456 |       10.660009 |       10.951984 |
| rust                      | actix                     |        6.837484 |        6.533378 |        6.661213 |
| rust                      | iron                      |        7.106496 |        6.702638 |        6.840316 |
| rust                      | nickel                    |        6.893016 |        6.765759 |        6.833456 |
| rust                      | rocket                    |        8.471154 |        8.249096 |        8.374040 |
| node                      | express                   |       27.553285 |       26.094557 |       26.461259 |
| node                      | clusterexpress            |       15.340291 |       14.423945 |       14.746395 |
| node                      | polka                     |       15.204288 |       14.529034 |       14.746328 |
| node                      | clusterpolka              |        9.005489 |        8.700110 |        8.890766 |
| elixir                    | plug                      |       11.180962 |       10.546574 |       10.919743 |
| elixir                    | phoenix                   |       10.930012 |       10.564129 |       10.792129 |
| swift                     | vapor                     |       12.312772 |       11.998519 |       12.125449 |
| swift                     | perfect                   |       12.949930 |       12.451148 |       12.665174 |
| swift                     | kitura                    |       15.190245 |       14.873937 |       15.024665 |
| scala                     | akkahttp                  |       18.091491 |       16.309630 |       16.807849 |
| csharp                    | aspnetcore                |       10.464090 |        9.795961 |       10.130911 |
| python                    | sanic                     |       15.118685 |       12.626734 |       13.913008 |
| python                    | japronto                  |        7.219928 |        6.947663 |        7.123151 |
| python                    | tornado                   |      174.423872 |      173.858005 |      174.095675 |
| nim                       | jester                    |       54.692087 |       54.458567 |       54.575013 |
| objc                      | criollo                   |       61.117013 |       56.024134 |       57.618709 |
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

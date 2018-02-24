# Which is the fastest?

Measuring response times (routing times) for each framework (middleware).
Each framework has to have two features; routing and parsing path parameters.

## Result

<!-- Result from here -->
Last update: 2018-02-17
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
17. [plug](https://github.com/elixir-lang/plug) (elixir)
18. [gin](https://github.com/gin-gonic/gin) (go)
19. [phoenix](https://github.com/phoenixframework/phoenix) (elixir)
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
32. [rails](https://github.com/rails/rails) (ruby)

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
| ruby                      | rails                     |      335.395009 |      333.636317 |      334.396840 |
| ruby                      | sinatra                   |       99.141004 |       98.975714 |       99.026594 |
| ruby                      | roda                      |       37.466029 |       37.374301 |       37.419190 |
| crystal                   | kemal                     |        8.325529 |        7.946342 |        8.221634 |
| crystal                   | router_cr                 |        6.699849 |        6.328368 |        6.554750 |
| crystal                   | raze                      |        7.168540 |        6.609037 |        6.972206 |
| crystal                   | lucky                     |        8.039500 |        7.378015 |        7.611963 |
| crystal                   | amber                     |        7.962421 |        7.630477 |        7.883198 |
| go                        | echo                      |       10.680238 |        9.922064 |       10.253166 |
| go                        | gorilla_mux               |       10.603140 |       10.141003 |       10.381136 |
| go                        | iris                      |       11.089684 |       10.042793 |       10.546684 |
| go                        | fasthttprouter            |        7.458703 |        7.251910 |        7.346479 |
| go                        | gin                       |       11.514139 |       10.806855 |       11.177951 |
| rust                      | actix                     |        6.867163 |        6.533883 |        6.656760 |
| rust                      | iron                      |        7.088215 |        6.742639 |        6.881779 |
| rust                      | nickel                    |        6.907404 |        6.733162 |        6.836242 |
| rust                      | rocket                    |        8.828674 |        8.322669 |        8.560235 |
| node                      | express                   |       27.026919 |       25.636237 |       26.024945 |
| node                      | clusterexpress            |       16.060312 |       14.749847 |       15.140425 |
| node                      | polka                     |       15.263102 |       14.424546 |       14.690798 |
| node                      | clusterpolka              |        9.132165 |        8.292221 |        8.726003 |
| elixir                    | plug                      |       11.684682 |       10.443505 |       11.025091 |
| elixir                    | phoenix                   |       11.795545 |       10.572868 |       11.283478 |
| swift                     | vapor                     |       12.750298 |       12.005783 |       12.446656 |
| swift                     | perfect                   |       13.272732 |       11.944508 |       12.715364 |
| swift                     | kitura                    |       16.144335 |       14.695239 |       15.612439 |
| scala                     | akkahttp                  |       17.806122 |       16.206246 |       16.854295 |
| csharp                    | aspnetcore                |       10.915338 |       10.031779 |       10.503702 |
| python                    | sanic                     |       14.016670 |       11.820920 |       12.900015 |
| python                    | japronto                  |        7.208292 |        7.011206 |        7.124510 |
| nim                       | jester                    |       54.473501 |       54.306489 |       54.402050 |
| objc                      | criollo                   |       57.432404 |       56.113433 |       56.738384 |
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

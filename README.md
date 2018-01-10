# Which is the fastest?

Measuring response times (routing times) for each framework (middleware).
Each framework has to have two features; routing and parsing path parameters.

## Result

<!-- Result from here -->
Last update: 2018-01-10
```
OS: Darwin (version: 17.2.0, arch: x86_64)
CPU Cores: 8
```

### Ranking (Framework)

1. [router_cr](https://github.com/tbrand/router.cr) (crystal)
2. [raze](https://github.com/samueleaton/raze) (crystal)
3. [nickel](https://github.com/nickel-org/nickel.rs) (rust)
4. [japronto](https://github.com/squeaky-pl/japronto) (python)
5. [iron](https://github.com/iron/iron) (rust)
6. [fasthttprouter](https://github.com/buaazp/fasthttprouter) (go)
7. [kemal](https://github.com/kemalcr/kemal) (crystal)
8. [rocket](https://github.com/SergioBenitez/Rocket) (rust)
9. [aspnetcore](https://github.com/aspnet/Home) (csharp)
10. [gorilla_mux](https://github.com/gorilla/mux) (go)
11. [iris](https://github.com/kataras/iris) (go)
12. [echo](https://github.com/labstack/echo) (go)
13. [plug](https://github.com/elixir-lang/plug) (elixir)
14. [phoenix](https://github.com/phoenixframework/phoenix) (elixir)
15. [vapor](https://github.com/vapor/vapor) (swift)
16. [perfect](https://github.com/PerfectlySoft/Perfect) (swift)
17. [sanic](https://github.com/channelcat/sanic) (python)
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
2. rust ([nickel](https://github.com/nickel-org/nickel.rs))
3. python ([japronto](https://github.com/squeaky-pl/japronto))
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
| ruby                      | rails                     |      161.079991 |      160.341164 |      160.664641 |
| ruby                      | sinatra                   |       45.534552 |       45.326070 |       45.378119 |
| ruby                      | roda                      |       16.244110 |       16.204136 |       16.220350 |
| crystal                   | kemal                     |        3.292682 |        3.157299 |        3.252980 |
| crystal                   | router_cr                 |        2.552623 |        2.471256 |        2.516766 |
| crystal                   | raze                      |        2.777100 |        2.665155 |        2.735574 |
| go                        | echo                      |        4.159959 |        4.011258 |        4.077308 |
| go                        | gorilla_mux               |        4.170352 |        3.909323 |        4.036364 |
| go                        | iris                      |        4.348133 |        3.864387 |        4.073871 |
| go                        | fasthttprouter            |        3.016682 |        2.886777 |        2.962532 |
| rust                      | iron                      |        3.023135 |        2.837906 |        2.918141 |
| rust                      | nickel                    |        2.818330 |        2.780301 |        2.803500 |
| rust                      | rocket                    |        3.556692 |        3.363295 |        3.426910 |
| node                      | express                   |       12.429907 |       11.482538 |       11.783619 |
| node                      | clusterexpress            |        7.884909 |        6.594599 |        6.911125 |
| elixir                    | plug                      |        4.654937 |        4.269527 |        4.546351 |
| elixir                    | phoenix                   |        4.933707 |        4.788846 |        4.854297 |
| swift                     | vapor                     |        5.561565 |        5.325290 |        5.412418 |
| swift                     | perfect                   |        5.634706 |        5.352592 |        5.545727 |
| swift                     | kitura                    |        6.652122 |        6.514098 |        6.582017 |
| scala                     | akkahttp                  |        9.114887 |        7.183424 |        7.819570 |
| csharp                    | aspnetcore                |        4.099906 |        3.747440 |        3.849951 |
| python                    | sanic                     |        6.438378 |        5.505323 |        5.748179 |
| python                    | japronto                  |        2.857492 |        2.797253 |        2.827124 |
| objc                      | criollo                   |       34.878167 |       23.799794 |       26.548463 |
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

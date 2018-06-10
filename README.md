# Which is the fastest?

Measuring response times (routing times) for each framework (middleware).
Each framework has to have two features; routing and parsing path parameters.

Any idea is :heart:, let discuss about it on [![Join the chat at https://gitter.im/which_is_the_fastest/Lobby](https://badges.gitter.im/which_is_the_fastest/Lobby.svg)](https://gitter.im/which_is_the_fastest/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Result

<!-- Result from here -->
Last update: 2018-06-10
```
OS: Linux (version: 4.16.14-300.fc28.x86_64, arch: x86_64)
CPU Cores: 4
```

### Ranking (Framework)

1. [actix-web](https://github.com/actix/actix-web) (rust)
2. [fasthttprouter](https://github.com/buaazp/fasthttprouter) (go)
3. [evhtp](https://github.com/criticalstack/libevhtp) (cpp)
4. [act](https://github.com/actframework/actframework) (java)
5. [mofuw](https://github.com/2vg/mofuw) (nim)
6. [aspnetcore](https://github.com/aspnet/Home) (csharp)
7. [rocket](https://github.com/SergioBenitez/Rocket) (rust)
8. [nickel](https://github.com/nickel-org/nickel.rs) (rust)
9. [iris](https://github.com/kataras/iris) (go)
10. [iron](https://github.com/iron/iron) (rust)
11. [echo](https://github.com/labstack/echo) (go)
12. [gorilla-mux](https://github.com/gorilla/mux) (go)
13. [japronto](https://github.com/squeaky-pl/japronto) (python)
14. [polka](https://github.com/lukeed/polka) (node)
15. [rayo](https://github.com/GetRayo/rayo.js) (node)
16. [akkahttp](https://github.com/akka/akka-http) (scala)
17. [fastify](https://github.com/fastify/fastify) (node)
18. [perfect](https://github.com/PerfectlySoft/Perfect) (swift)
19. [raze](https://github.com/samueleaton/raze) (crystal)
20. [router.cr](https://github.com/tbrand/router.cr) (crystal)
21. [sanic](https://github.com/channelcat/sanic) (python)
22. [express](https://github.com/expressjs/express) (node)
23. [spider-gazelle](https://github.com/spider-gazelle/spider-gazelle) (crystal)
24. [vapor](https://github.com/vapor/vapor) (swift)
25. [lucky](https://github.com/luckyframework/lucky) (crystal)
26. [plug](https://github.com/elixir-lang/plug) (elixir)
27. [kitura](https://github.com/IBM-Swift/Kitura) (swift)
28. [amber](https://github.com/amberframework/amber) (crystal)
29. [kemal](https://github.com/kemalcr/kemal) (crystal)
30. [roda](https://github.com/jeremyevans/roda) (ruby)
31. [phoenix](https://github.com/phoenixframework/phoenix) (elixir)
32. [gin](https://github.com/gin-gonic/gin) (go)
33. [rack-routing](https://github.com/iAmPlus/rack-routing) (ruby)
34. [jester](https://github.com/dom96/jester) (nim)
35. [flask](https://github.com/pallets/flask) (python)
36. [flame](https://github.com/AlexWayfer/flame) (ruby)
37. [sinatra](https://github.com/sinatra/sinatra) (ruby)
38. [django](https://github.com/django/django) (python)
39. [rails](https://github.com/rails/rails) (ruby)
40. [tornado](https://github.com/tornadoweb/tornado) (python)

### Ranking (Language)

1. rust ([actix-web](https://github.com/actix/actix-web))
2. go ([fasthttprouter](https://github.com/buaazp/fasthttprouter))
3. cpp ([evhtp](https://github.com/criticalstack/libevhtp))
4. java ([act](https://github.com/actframework/actframework))
5. nim ([mofuw](https://github.com/2vg/mofuw))
6. csharp ([aspnetcore](https://github.com/aspnet/Home))
7. python ([japronto](https://github.com/squeaky-pl/japronto))
8. node ([polka](https://github.com/lukeed/polka))
9. scala ([akkahttp](https://github.com/akka/akka-http))
10. swift ([perfect](https://github.com/PerfectlySoft/Perfect))
11. crystal ([raze](https://github.com/samueleaton/raze))
12. elixir ([plug](https://github.com/elixir-lang/plug))
13. ruby ([roda](https://github.com/jeremyevans/roda))

### All frameworks

| Language (Runtime)        | Framework (Middleware)    |    Requests / s |         Latency |   99 percentile |      Throughput |
|---------------------------|---------------------------|----------------:|:----------------:|----------------:|-----------:|
| ruby                      | rails                     | 4372.00 | 14636.33 | 96833.67 | 3979380.33 |
| ruby                      | sinatra                   | 16246.67 | 3922.00 | 45670.33 | 13920038.67 |
| ruby                      | roda                      | 38623.67 | 1715.33 | 30815.67 | 12687448.67 |
| ruby                      | rack-routing              | 29904.67 | 2278.33 | 37652.00 | 5907377.33 |
| ruby                      | flame                     | 17468.00 | 3701.33 | 47760.33 | 3340251.33 |
| crystal                   | kemal                     | 40110.67 | 23705.67 | 33775.00 | 19304178.67 |
| crystal                   | router.cr                 | 51950.67 | 18357.33 | 24785.00 | 14798754.00 |
| crystal                   | raze                      | 54512.67 | 20082.33 | 125983.67 | 16769547.33 |
| crystal                   | lucky                     | 44838.00 | 21376.67 | 26056.67 | 15899976.00 |
| crystal                   | amber                     | 41827.33 | 25088.67 | 95390.33 | 16906595.33 |
| crystal                   | spider-gazelle            | 46736.67 | 20296.67 | 25062.00 | 12927898.00 |
| go                        | echo                      | 90506.00 | 14453.00 | 129697.33 | 52181620.67 |
| go                        | gorilla-mux               | 86871.67 | 14802.00 | 125746.00 | 32838546.67 |
| go                        | iris                      | 96267.00 | 15078.00 | 174130.00 | 35769067.00 |
| go                        | fasthttprouter            | 160652.33 | 6156.33 | 53915.67 | 73558143.33 |
| go                        | gin                       | 36927.67 | 31137.33 | 148275.67 | 20709597.00 |
| rust                      | actix-web                 | 188240.67 | 4367.33 | 15139.00 | 70870040.67 |
| rust                      | iron                      | 95394.33 | 321.67 | 1094.67 | 35548869.33 |
| rust                      | nickel                    | 104670.00 | 42.33 | 50.33 | 68611160.00 |
| rust                      | rocket                    | 107608.67 | 68.33 | 159.00 | 49859189.33 |
| node                      | express                   | 47823.00 | 37230.33 | 730944.33 | 39938433.00 |
| node                      | fastify                   | 66100.67 | 27589.33 | 534516.33 | 66311172.00 |
| node                      | polka                     | 77566.33 | 17744.67 | 232930.33 | 38042086.33 |
| node                      | rayo                      | 76513.67 | 18590.00 | 263289.00 | 37589230.67 |
| elixir                    | plug                      | 43648.67 | 33837.00 | 243430.67 | 29272801.00 |
| elixir                    | phoenix                   | 38188.33 | 36290.33 | 232460.67 | 25718128.33 |
| swift                     | vapor                     | 46053.33 | 51436.67 | 1166803.00 | 16522653.33 |
| swift                     | perfect                   | 65855.67 | 14834.67 | 17608.67 | 16999461.00 |
| swift                     | kitura                    | 42926.00 | 22127.67 | 37097.67 | 26409534.00 |
| scala                     | akkahttp                  | 69245.00 | 230129.33 | 5289308.67 | 53127119.00 |
| csharp                    | aspnetcore                | 117176.00 | 8602.67 | 17974.67 | 61599543.33 |
| python                    | sanic                     | 48795.00 | 19507.00 | 36879.67 | 29155520.67 |
| python                    | japronto                  | 86765.67 | 11057.00 | 11265.33 | 35608694.33 |
| python                    | flask                     | 18775.00 | 50920.33 | 101835.33 | 15377015.33 |
| python                    | django                    | 11025.67 | 89173.00 | 263761.00 | 10375313.67 |
| python                    | tornado                   | 1910.67 | 490938.33 | 2724463.00 | 1349414.67 |
| nim                       | jester                    | 28754.33 | 66472.00 | 1262444.67 | 10469903.00 |
| nim                       | mofuw                     | 131842.67 | 8788.33 | 51147.67 | 80210274.67 |
| java                      | act                       | 138188.00 | 8296.67 | 53235.33 | 73474924.33 |
| cpp                       | evhtp                     | 158545.00 | 5397.67 | 13185.67 | 50781478.33 |
<!-- Result till here -->

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

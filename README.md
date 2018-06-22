# Which is the fastest?

Measuring response times (routing times) for each framework (middleware).
Each framework has to have two features; routing and parsing path parameters.

Any idea is :heart:, let discuss about it on [![Join the chat at https://gitter.im/which_is_the_fastest/Lobby](https://badges.gitter.im/which_is_the_fastest/Lobby.svg)](https://gitter.im/which_is_the_fastest/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Result

<!-- Result from here -->
Last update: 2018-06-22
```
OS: Linux (version: 4.16.11-100.fc26.x86_64, arch: x86_64)
CPU Cores: 8
```

### Ranking (Framework)

1. [actix-web](https://github.com/actix/actix-web) (rust)
2. [fasthttprouter](https://github.com/buaazp/fasthttprouter) (go)
3. [evhtp](https://github.com/criticalstack/libevhtp) (cpp)
4. [act](https://github.com/actframework/actframework) (java)
5. [mofuw](https://github.com/2vg/mofuw) (nim)
6. [iris](https://github.com/kataras/iris) (go)
7. [iron](https://github.com/iron/iron) (rust)
8. [aspnetcore](https://github.com/aspnet/Home) (csharp)
9. [gorilla-mux](https://github.com/gorilla/mux) (go)
10. [echo](https://github.com/labstack/echo) (go)
11. [rocket](https://github.com/SergioBenitez/Rocket) (rust)
12. [polka](https://github.com/lukeed/polka) (node)
13. [rayo](https://github.com/GetRayo/rayo.js) (node)
14. [nickel](https://github.com/nickel-org/nickel.rs) (rust)
15. [fastify](https://github.com/fastify/fastify) (node)
16. [symfony](https://github.com/symfony/symfony) (php)
17. [perfect](https://github.com/PerfectlySoft/Perfect) (swift)
18. [akkahttp](https://github.com/akka/akka-http) (scala)
19. [vapor](https://github.com/vapor/vapor) (swift)
20. [express](https://github.com/expressjs/express) (node)
21. [japronto](https://github.com/squeaky-pl/japronto) (python)
22. [plug](https://github.com/elixir-lang/plug) (elixir)
23. [laravel](https://github.com/laravel/framework) (php)
24. [roda](https://github.com/jeremyevans/roda) (ruby)
25. [phoenix](https://github.com/phoenixframework/phoenix) (elixir)
26. [rack-routing](https://github.com/georgeu2000/rack-routing) (ruby)
27. [router.cr](https://github.com/tbrand/router.cr) (crystal)
28. [kitura](https://github.com/IBM-Swift/Kitura) (swift)
29. [spider-gazelle](https://github.com/spider-gazelle/spider-gazelle) (crystal)
30. [gin](https://github.com/gin-gonic/gin) (go)
31. [lucky](https://github.com/luckyframework/lucky) (crystal)
32. [kemal](https://github.com/kemalcr/kemal) (crystal)
33. [amber](https://github.com/amberframework/amber) (crystal)
34. [flame](https://github.com/AlexWayfer/flame) (ruby)
35. [flask](https://github.com/pallets/flask) (python)
36. [sanic](https://github.com/channelcat/sanic) (python)
37. [jester](https://github.com/dom96/jester) (nim)
38. [sinatra](https://github.com/sinatra/sinatra) (ruby)
39. [django](https://github.com/django/django) (python)
40. [rails](https://github.com/rails/rails) (ruby)
41. [tornado](https://github.com/tornadoweb/tornado) (python)

### Ranking (Language)

1. rust ([actix-web](https://github.com/actix/actix-web))
2. go ([fasthttprouter](https://github.com/buaazp/fasthttprouter))
3. cpp ([evhtp](https://github.com/criticalstack/libevhtp))
4. java ([act](https://github.com/actframework/actframework))
5. nim ([mofuw](https://github.com/2vg/mofuw))
6. csharp ([aspnetcore](https://github.com/aspnet/Home))
7. node ([polka](https://github.com/lukeed/polka))
8. php ([symfony](https://github.com/symfony/symfony))
9. swift ([perfect](https://github.com/PerfectlySoft/Perfect))
10. scala ([akkahttp](https://github.com/akka/akka-http))
11. python ([japronto](https://github.com/squeaky-pl/japronto))
12. elixir ([plug](https://github.com/elixir-lang/plug))
13. ruby ([roda](https://github.com/jeremyevans/roda))
14. crystal ([router.cr](https://github.com/tbrand/router.cr))

### All frameworks

| Language (Runtime)        | Framework (Middleware)    |    Requests / s |         Latency |   99 percentile |      Throughput |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|-----------:|
| ruby                      | rails                     | 4611.33 | 27861.00 | 174566.67 | 4.34 MB |
| ruby                      | sinatra                   | 16768.67 | 7643.67 | 55288.67 | 14.87 MB |
| ruby                      | roda                      | 45638.33 | 2788.33 | 16947.33 | 14.92 MB |
| ruby                      | rack-routing              | 34894.33 | 3681.33 | 23820.33 | 6.31 MB |
| ruby                      | flame                     | 21713.33 | 5906.33 | 27577.33 | 4.24 MB |
| crystal                   | kemal                     | 28708.67 | 34784.00 | 56094.67 | 15.25 MB |
| crystal                   | router.cr                 | 34852.00 | 28777.00 | 45224.67 | 9.42 MB |
| crystal                   | amber                     | 27261.33 | 36681.67 | 53126.67 | 11.51 MB |
| crystal                   | lucky                     | 29036.67 | 33977.33 | 51791.00 | 11.04 MB |
| crystal                   | spider-gazelle            | 30971.67 | 32338.00 | 51724.33 | 8.28 MB |
| go                        | echo                      | 92976.67 | 11770.33 | 54057.33 | 55.23 MB |
| go                        | gorilla-mux               | 98665.67 | 10745.67 | 38066.67 | 36.59 MB |
| go                        | iris                      | 111486.00 | 9012.00 | 26889.67 | 41.78 MB |
| go                        | fasthttprouter            | 178029.67 | 5292.67 | 15793.33 | 81.36 MB |
| go                        | gin                       | 30366.00 | 52257.00 | 224732.67 | 17.48 MB |
| rust                      | actix-web                 | 201039.67 | 4602.00 | 15214.67 | 75.45 MB |
| rust                      | iron                      | 104908.00 | 561.00 | 2395.67 | 38.96 MB |
| rust                      | nickel                    | 86370.00 | 98.67 | 203.00 | 61.02 MB |
| rust                      | rocket                    | 92141.00 | 139.00 | 326.00 | 45.65 MB |
| node                      | express                   | 54475.00 | 30560.67 | 465009.67 | 46.16 MB |
| node                      | fastify                   | 72711.33 | 18919.33 | 184683.67 | 65.66 MB |
| node                      | polka                     | 87658.67 | 14799.33 | 123607.67 | 45.73 MB |
| node                      | rayo                      | 86668.33 | 14938.00 | 140620.00 | 43.47 MB |
| elixir                    | plug                      | 49394.00 | 22549.33 | 75481.67 | 33.34 MB |
| elixir                    | phoenix                   | 41079.33 | 28381.00 | 204433.00 | 27.68 MB |
| swift                     | vapor                     | 54830.67 | 57292.00 | 1365021.33 | 20.45 MB |
| swift                     | perfect                   | 60535.33 | 16177.33 | 22070.00 | 16.69 MB |
| swift                     | kitura                    | 34545.33 | 31251.00 | 120427.33 | 19.17 MB |
| scala                     | akkahttp                  | 59872.00 | 178780.67 | 4032980.33 | 47.29 MB |
| csharp                    | aspnetcore                | 103547.00 | 11672.33 | 84094.00 | 51.09 MB |
| python                    | sanic                     | 17647.00 | 57105.00 | 149797.67 | 9.94 MB |
| python                    | japronto                  | 52478.67 | 19220.33 | 30166.33 | 20.99 MB |
| python                    | flask                     | 17737.00 | 61064.00 | 204945.00 | 14.25 MB |
| python                    | django                    | 11140.00 | 92266.33 | 240016.00 | 9.84 MB |
| python                    | tornado                   | 1370.67 | 746251.67 | 5150188.67 | 0.91 MB |
| nim                       | jester                    | 17297.67 | 230566.00 | 4488995.00 | 6.47 MB |
| nim                       | mofuw                     | 118821.33 | 14601.67 | 131776.67 | 65.92 MB |
| java                      | act                       | 121048.00 | 9351.00 | 34167.33 | 46.18 MB |
| cpp                       | evhtp                     | 172703.00 | 5387.00 | 16571.00 | 56.81 MB |
| php                       | symfony                   | 62136.00 | 128755.00 | 2109841.67 | 100.30 MB |
| php                       | laravel                   | 46680.00 | 264386.67 | 4136509.00 | 78.59 MB |
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

# Which is the fastest?

Measuring response times (routing times) for each framework (middleware).
Each framework has to have two features; routing and parsing path parameters.

Any idea is :heart:, let discuss about it on [![Join the chat at https://gitter.im/which_is_the_fastest/Lobby](https://badges.gitter.im/which_is_the_fastest/Lobby.svg)](https://gitter.im/which_is_the_fastest/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Result

<!-- Result from here -->
Last update: 2018-06-21
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
6. [iron](https://github.com/iron/iron) (rust)
7. [iris](https://github.com/kataras/iris) (go)
8. [aspnetcore](https://github.com/aspnet/Home) (csharp)
9. [echo](https://github.com/labstack/echo) (go)
10. [rocket](https://github.com/SergioBenitez/Rocket) (rust)
11. [polka](https://github.com/lukeed/polka) (node)
12. [gorilla-mux](https://github.com/gorilla/mux) (go)
13. [rayo](https://github.com/GetRayo/rayo.js) (node)
14. [nickel](https://github.com/nickel-org/nickel.rs) (rust)
15. [fastify](https://github.com/fastify/fastify) (node)
16. [akkahttp](https://github.com/akka/akka-http) (scala)
17. [symfony](https://github.com/symfony/symfony) (php)
18. [vapor](https://github.com/vapor/vapor) (swift)
19. [perfect](https://github.com/PerfectlySoft/Perfect) (swift)
20. [express](https://github.com/expressjs/express) (node)
21. [japronto](https://github.com/squeaky-pl/japronto) (python)
22. [laravel](https://github.com/laravel/framework) (php)
23. [plug](https://github.com/elixir-lang/plug) (elixir)
24. [roda](https://github.com/jeremyevans/roda) (ruby)
25. [phoenix](https://github.com/phoenixframework/phoenix) (elixir)
26. [kitura](https://github.com/IBM-Swift/Kitura) (swift)
27. [router.cr](https://github.com/tbrand/router.cr) (crystal)
28. [rack-routing](https://github.com/georgeu2000/rack-routing) (ruby)
29. [spider-gazelle](https://github.com/spider-gazelle/spider-gazelle) (crystal)
30. [lucky](https://github.com/luckyframework/lucky) (crystal)
31. [gin](https://github.com/gin-gonic/gin) (go)
32. [kemal](https://github.com/kemalcr/kemal) (crystal)
33. [flame](https://github.com/AlexWayfer/flame) (ruby)
34. [jester](https://github.com/dom96/jester) (nim)
35. [flask](https://github.com/pallets/flask) (python)
36. [sanic](https://github.com/channelcat/sanic) (python)
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
7. node ([polka](https://github.com/lukeed/polka))
8. scala ([akkahttp](https://github.com/akka/akka-http))
9. php ([symfony](https://github.com/symfony/symfony))
10. swift ([vapor](https://github.com/vapor/vapor))
11. python ([japronto](https://github.com/squeaky-pl/japronto))
12. elixir ([plug](https://github.com/elixir-lang/plug))
13. ruby ([roda](https://github.com/jeremyevans/roda))
14. crystal ([router.cr](https://github.com/tbrand/router.cr))

### All frameworks

| Language (Runtime)        | Framework (Middleware)    |    Requests / s |         Latency |   99 percentile |      Throughput |
|---------------------------|---------------------------|----------------:|----------------:|----------------:|-----------:|
| ruby                      | rails                     | 4584.33 | 28134.33 | 175211.00 | 4.54 MB |
| ruby                      | sinatra                   | 15149.00 | 8502.33 | 76521.67 | 12.09 MB |
| ruby                      | roda                      | 40198.33 | 3166.00 | 19912.33 | 12.81 MB |
| ruby                      | rack-routing              | 30784.67 | 4309.67 | 32581.33 | 5.41 MB |
| ruby                      | flame                     | 21223.67 | 6061.33 | 28145.33 | 3.64 MB |
| crystal                   | kemal                     | 25187.00 | 39450.67 | 61651.67 | 13.79 MB |
| crystal                   | router.cr                 | 32568.00 | 30387.33 | 46888.00 | 8.96 MB |
| crystal                   | lucky                     | 28279.00 | 34858.33 | 53256.33 | 15.68 MB |
| crystal                   | spider-gazelle            | 30563.33 | 32832.67 | 51282.33 | 8.06 MB |
| go                        | echo                      | 88765.33 | 11815.67 | 39381.33 | 51.14 MB |
| go                        | gorilla-mux               | 77444.33 | 13273.33 | 44930.00 | 30.71 MB |
| go                        | iris                      | 96147.00 | 10314.33 | 30569.33 | 36.90 MB |
| go                        | fasthttprouter            | 159574.33 | 6617.00 | 20105.33 | 76.40 MB |
| go                        | gin                       | 27944.00 | 52889.67 | 232624.33 | 16.14 MB |
| rust                      | actix-web                 | 180317.00 | 5174.67 | 16760.33 | 66.92 MB |
| rust                      | iron                      | 97570.67 | 602.00 | 3225.33 | 37.02 MB |
| rust                      | nickel                    | 74563.33 | 109.67 | 270.00 | 48.19 MB |
| rust                      | rocket                    | 84534.67 | 158.33 | 940.00 | 36.32 MB |
| node                      | express                   | 46723.33 | 31400.00 | 436759.00 | 40.70 MB |
| node                      | fastify                   | 63789.00 | 22733.00 | 273767.33 | 64.20 MB |
| node                      | polka                     | 79973.33 | 16785.33 | 193096.00 | 42.11 MB |
| node                      | rayo                      | 75764.00 | 17965.33 | 222321.00 | 38.38 MB |
| elixir                    | plug                      | 42440.67 | 27076.00 | 144232.67 | 28.84 MB |
| elixir                    | phoenix                   | 38049.67 | 28159.00 | 87569.33 | 25.46 MB |
| swift                     | vapor                     | 53046.00 | 39657.33 | 921786.67 | 19.15 MB |
| swift                     | perfect                   | 52902.67 | 18624.33 | 26933.33 | 15.35 MB |
| swift                     | kitura                    | 35359.67 | 28841.00 | 56772.00 | 19.71 MB |
| scala                     | akkahttp                  | 60269.33 | 188475.67 | 4392002.67 | 44.96 MB |
| csharp                    | aspnetcore                | 95735.33 | 10750.00 | 34029.00 | 52.33 MB |
| python                    | sanic                     | 16309.67 | 61710.00 | 153582.67 | 9.61 MB |
| python                    | japronto                  | 46634.33 | 21209.33 | 30130.67 | 18.93 MB |
| python                    | flask                     | 17002.00 | 57426.33 | 123760.33 | 14.28 MB |
| python                    | django                    | 10506.33 | 95948.33 | 254193.67 | 10.50 MB |
| python                    | tornado                   | 1226.67 | 728607.33 | 4317583.00 | 0.83 MB |
| nim                       | jester                    | 17015.00 | 227399.00 | 4767049.33 | 6.21 MB |
| nim                       | mofuw                     | 117208.33 | 16010.33 | 148326.67 | 64.54 MB |
| java                      | act                       | 128324.67 | 9627.00 | 56026.33 | 47.86 MB |
| cpp                       | evhtp                     | 159498.33 | 5878.33 | 17491.67 | 52.93 MB |
| php                       | symfony                   | 55700.00 | 171170.67 | 3004957.33 | 88.11 MB |
| php                       | laravel                   | 45378.00 | 221249.67 | 3586468.00 | 67.40 MB |
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

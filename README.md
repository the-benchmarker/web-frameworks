# Which is the fastest? (Rails vs Crystal)

## Regulation and Rule
 - Each server has no logics
 - There are only 3 routes; GET '/', GET '/user/:id' and POST '/user'.

## Result

result here

### How to run server
```bash
bin/server_rails # for rails
bin/server_kemal # for Kemal
bin/server_route_cr # for route.cr
```

### How to run client
```bash
time bin/client -t 16 -r 1000 # 48000 requests in total
time bin/client -t 16 -r 2000 # 96000 requests in total
time bin/client -t 16 -r 3000 # 144000 requests in total
time bin/client -t 16 -r 4000 # 192000 requests in total
time bin/client -t 16 -r 5000 # 240000 requests in total
```

## Installation

You need
 - crystal(0.21.1)
 - rails

To compile servers,
```
> make
```

## Usage

To run server
```bash
./bin/server_rails
./bin/server_kemal
./bin/server_route_cr
```

To run client
```bash
time ./bin/client
```

You can set # of threads and # of request loops(each loop requests 3 times) by
```bash
time ./bin/client -t 16 -r 1000
```
In the above example, 16 threads requests 1000 * 3 times.
So 48000 requests are sent in total.

## Thanks for
 - [Rails](https://github.com/rails/rails)
 - [Kemal](https://github.com/kemalcr/kemal)
 - [route.cr](https://github.com/tbrand/route.cr)

## Development
 - **Give me PR when you want to add other web frameworks**
 - **Give me PR when you can turning each framework**

## Contributing

1. Fork it ( https://github.com/tbrand/which_is_the_fastest/fork )
2. Create your feature branch (git checkout -b my-new-feature)
3. Commit your changes (git commit -am 'Add some feature')
4. Push to the branch (git push origin my-new-feature)
5. Create a new Pull Request

## Contributors

- [tbrand](https://github.com/tbrand) Taichiro Suzuki - creator, maintainer

# Which is the fastest?

## Result

<img src="https://cloud.githubusercontent.com/assets/3483230/24584152/89b2beb6-179e-11e7-8340-abf785e380fd.png" width="400"/>
<img src="https://cloud.githubusercontent.com/assets/3483230/24584153/8b0debe6-179e-11e7-92fa-fb2c7da739f7.png" width="400"/>

**Current target frameworks**
 - Ruby
   - [Rails](https://github.com/rails/rails)
   - [Sinatra](https://github.com/sinatra/sinatra)
   - [Roda](https://github.com/jeremyevans/roda)
 - Crystal
   - [Kemal](https://github.com/kemalcr/kemal)
   - [route.cr](https://github.com/tbrand/route.cr)
 - Go
   - [Echo](https://github.com/labstack/echo)

### How to run server
```bash
bin/server_ruby_rails   # for Rails
bin/server_ruby_sinatra # for Sinatra
bin/server_ruby_roda    # for Roda
bin/server_kemal        # for Kemal
bin/server_route_cr     # for route.cr
bin/server_go_echo      # for Echo
```

### How to run client
```bash
time bin/client -t 16 -r 1000 # 48000  requests in total
time bin/client -t 16 -r 2000 # 96000  requests in total
time bin/client -t 16 -r 3000 # 144000 requests in total
time bin/client -t 16 -r 4000 # 192000 requests in total
time bin/client -t 16 -r 5000 # 240000 requests in total
```

## Regulation and Rule
I want to know a response time, not a usability. So full-stack framework is at a disadvantage.
 - Each server has no logics
 - Each server executable is `server_[Lauguage]_[Framework]`
 - There are only 3 routes
   - GET  '/'         return status code 200 with empty body
   - GET  '/user/:id' return status code 200 with the id
   - POST '/user'     return status code 200 with empty body

## Installation

You need
 - Ruby(bundler)
 - crystal
 - Go(glide)

To compile servers,
```
> make
```

## Usage

To run server
```bash
bin/server_[Language]_[Framework]
```

To run client
```bash
time bin/client
```

You can set # of threads and # of request loops(each loop requests 3 times) by
```bash
time bin/client -t 16 -r 1000
```
In the above example, 16 threads requests 1000 * 3 times.
So 48000 requests are sent in total.

## Development
 - **Give me PR when you want to add other web frameworks**
 - **Give me PR when you can tuning each framework (under the regulation)**

## Contributing

1. Fork it ( https://github.com/tbrand/which_is_the_fastest/fork )
2. Create your feature branch (git checkout -b my-new-feature)
3. Commit your changes (git commit -am 'Add some feature')
4. Push to the branch (git push origin my-new-feature)
5. Create a new Pull Request

## Contributors

- [tbrand](https://github.com/tbrand) Taichiro Suzuki - creator, maintainer

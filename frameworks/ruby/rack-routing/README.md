## Rack Routing Demo
----
This app shows how to use the [Rack Routing gem](https://github.com/iAmPlus/rack-routing).

### Route http requests to Ruby methods:

1. `GET /` => `get_root`
1. `GET /foos/:id` => `get_foo` (with `@url_params`)
1. `POST /foos` => `post_foos`

### Examples:

Request: `POST /foos, { "bar":"baz" }`  
Ruby:   

    def post_foos
      Foo.create @params
      Rack::Response.new( 'Foo was created.', 200 )
    end

### To run locally:
    rackup

This will start the server on port 9292.

### To run specs:
    rspec

or

    guard
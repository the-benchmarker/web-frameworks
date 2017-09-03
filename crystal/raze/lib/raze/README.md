<br>
<br>
<p align="center">
<img width="250" title="raze" alt="raze" src="https://raw.githubusercontent.com/samueleaton/design/master/raze-hero.png">
</p>
<br>
<br>

# Raze

> Raze for days.

## Installation

Add this to your application's `shard.yml`:

```yaml
dependencies:
  raze:
    github: samueleaton/raze
```

## Usage

```ruby
require "raze"

get "/hello" do |ctx|
  "hello, world!"
end

Raze.run
```


Raze takes a modular-first approach to middlewares

```ruby
require "raze"

# Define middlewares
class Authenticator < Raze::Handler
  def call(ctx, done)
    puts "Authenticate here..."
    done.call
  end
end

class DDoSBlocker < Raze::Handler
  def call(ctx, done)
    puts "Prevent DDoS attack here..."
    done.call
  end
end

class UserFetcher < Raze::Handler
  def call(ctx, done)
    # Fetch user record from DB here...
    ctx.state["user_name"] = "Sam"
    done.call
  end
end

# Define routes, attach middlewares
get "/api/**", [Authenticator.new, DDoSBlocker.new]

get "/api/user/:user_id", UserFetcher.new do |ctx|
  "hello, #{ctx.state["user_name"]}!"
end

Raze.run
```

## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create a new Pull Request

## Contributors

- [Sam Eaton](https://github.com/samueleaton) Sam Eaton - creator, maintainer

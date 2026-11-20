module Benchmark
  class Routes < Hanami::Routes
    root to: "index.empty"

    get "/user/:id", to: "user.get"

    post "/user", to: "index.empty"
  end
end

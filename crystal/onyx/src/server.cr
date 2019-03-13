require "onyx/http"

struct IDEndpoint
  include Onyx::HTTP::Endpoint

  params do
    path do
      type id : Int32
    end
  end

  def call
    context.response << params.path.id
  end
end

Onyx.get "/" { }
Onyx.get "/user/:id", IDEndpoint
Onyx.post "/user" { }

(System.cpu_count - 1).times do |i|
  Process.fork do
    Onyx.listen("0.0.0.0", 3000, reuse_port: true)
  end
end

Onyx.listen("0.0.0.0", 3000, reuse_port: true)

require "onyx/http"

struct EmptyEndpoint
  include Onyx::HTTP::Endpoint

  def call
    context.response << String.new
  end
end

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

Onyx::HTTP.get "/", EmptyEndpoint

Onyx::HTTP.on "/user" do |r|
  r.get "/:id", IDEndpoint
  r.post "/", EmptyEndpoint
end

(System.cpu_count - 1).times do |i|
  Process.fork do
    Onyx::HTTP.listen("0.0.0.0", 3000, reuse_port: true)
  end
end

Onyx::HTTP.listen("0.0.0.0", 3000, reuse_port: true)

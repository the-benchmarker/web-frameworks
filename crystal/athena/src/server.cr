require "athena/routing"

class BenchmarkController < Athena::Routing::Controller
  @[Athena::Routing::Get(path: "/")]
  def root_get : Nil
  end

  @[Athena::Routing::Post(path: "/user")]
  def root_post(body : String?) : Nil
  end

  @[Athena::Routing::Get(path: "/user/:id", constraints: {id: /\d+/})]
  def user(id : Int32) : Int32
    id
  end
end

Athena::Routing.run(3000)

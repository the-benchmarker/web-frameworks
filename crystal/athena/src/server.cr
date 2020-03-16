require "athena"

class BenchmarkController < ART::Controller
  @[ART::Get("/")]
  def root_get : Nil
  end

  @[ART::Post("/user")]
  def root_post : Nil
  end

  @[ART::Get("/user/:id", constraints: {id: /\d+/})]
  def user(id : Int32) : Int32
    id
  end
end

System.cpu_count.times do |i|
  Process.fork do
    ART.run reuse_port: true
  end
end

sleep

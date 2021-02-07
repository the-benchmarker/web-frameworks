require "athena"

Log.setup :none

class BenchmarkController < ART::Controller
  @[ARTA::Get("/")]
  def root_get : String
    ""
  end

  @[ARTA::Post("/user")]
  def root_post : String
    ""
  end

  @[ARTA::Get("/user/:id", constraints: {id: /\d+/})]
  def user(id : Int32) : Int32
    id
  end
end

System.cpu_count.times do
  Process.fork do
    ART.run reuse_port: true
  end
end

sleep

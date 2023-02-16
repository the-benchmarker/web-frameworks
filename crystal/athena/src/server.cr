require "athena"

Log.setup :none

class BenchmarkController < ATH::Controller
  @[ARTA::Get("/")]
  def root_get : Nil
  end

  @[ARTA::Post("/user")]
  def root_post : Nil
  end

  @[ARTA::Get("/user/{id<\\d+>}")]
  def user(id : Int32) : Int32
    id
  end
end

System.cpu_count.times do
  Process.fork do
    ATH.run reuse_port: true
  end
end

sleep

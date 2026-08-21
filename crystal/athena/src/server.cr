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

Fiber::ExecutionContext.default.resize(maximum: Fiber::ExecutionContext.default_workers_count)
ATH.run reuse_port: true

require "./project"

Fiber::ExecutionContext.default.resize(
  maximum: Fiber::ExecutionContext.default_workers_count,
)
Marten.start

require "./project"

Fiber::ExecutionContext.default.resize(maximum: System.cpu_count)
Marten.start

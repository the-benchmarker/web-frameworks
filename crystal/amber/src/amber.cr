require "../config/application"

Fiber::ExecutionContext.default.resize(maximum: Fiber::ExecutionContext.default_workers_count)
Amber::Server.start

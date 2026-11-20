require "../config/application"

Fiber::ExecutionContext.default.resize(maximum: System.cpu_count)
Amber::Server.start

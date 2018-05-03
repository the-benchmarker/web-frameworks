import Vapor

var services = Services.default()

var commands = CommandConfig()
commands.use(StartFrameworks(), as: "start")
commands.use(WrkBenchmark(), as: "wrk")
services.register(commands)

let app = try Application(config: .default(), environment: .detect(), services: services)
try app.run()

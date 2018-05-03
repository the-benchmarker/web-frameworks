import Vapor
import FluentSQLite

var services = Services.default()

try services.register(FluentSQLiteProvider())
let db = try SQLiteDatabase(storage: .file(path: "results.db"))
services.register(db)

var migrations = MigrationConfig()
services.register(migrations)

var commands = CommandConfig()
commands.use(StartFrameworks(), as: "start")
commands.use(WrkBenchmark(), as: "wrk")
services.register(commands)

let app = try Application(config: .default(), environment: .detect(), services: services)
try app.run()

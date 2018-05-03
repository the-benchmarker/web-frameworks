import Vapor

final class StartFrameworks: Command {
    let arguments: [CommandArgument] = []
    let options: [CommandOption] = []
    let help: [String] = []

    func run(using context: CommandContext) throws -> EventLoopFuture<Void> {
        for (i, framework) in try Framework.readAll().enumerated() {
            let port = 8000 + i
            let res = try Process.execute("docker", "run", "-d", "-p", "3000:\(port.description)", framework.name)
            context.console.print("\(framework): \(res)")
        }
        return .done(on: context.container)
    }
}

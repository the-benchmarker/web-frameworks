import Vapor

final class WrkBenchmark: Command {
    var arguments: [CommandArgument] { return [] }
    var options: [CommandOption] { return [] }
    var help: [String] { return [] }

    func run(using context: CommandContext) throws -> EventLoopFuture<Void> {
        context.console.success("wrk done")
        return .done(on: context.container)
    }
}

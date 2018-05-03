import Vapor

final class StartFrameworks: Command {

    var arguments: [CommandArgument] { return [] }
    var options: [CommandOption] { return [] }
    var help: [String] { return [] }

    func run(using context: CommandContext) throws -> EventLoopFuture<Void> {
        context.console.success("start done")
        return .done(on: context.container)
    }
}

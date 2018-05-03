import Vapor

final class WrkBenchmark: Command {
    var arguments: [CommandArgument] { return [] }
    var options: [CommandOption] { return [] }
    var help: [String] { return [] }

    func run(using context: CommandContext) throws -> EventLoopFuture<Void> {
        var runs: [WrkRun] = []

        for framework in try Framework.readAll() {
            var results: [WrkResult] = []

            context.console.print("\(framework) ", newLine: false)
            for _ in 1...2 {
                context.console.print(".", newLine: false)
                let res = try Process.execute("wrk", "-s", "wrk-report.lua", "-t", "4", "-c", "128", "-d", "1", "http://localhost:8080/ping")
                let parts = res.split(separator: "$", maxSplits: 1).map(String.init)
                guard parts.count == 2 else {
                    throw "no $ found in wrk output"
                }
                let result = try JSONDecoder().decode(WrkResult.self, from: Data(parts[1].utf8))
                results.append(result)
            }
            context.console.print("")

            let run = WrkRun(framework: framework, results: results)
            runs.append(run)
        }

        context.console.report(\.requests, title: "total requests", suffix: " reqs", runs: runs)
        context.console.report(\.latency.mean, title: "average latency", suffix: "µs", runs: runs)
        return .done(on: context.container)
    }
}

extension Console {
    func report(_ stat: KeyPath<WrkResult, Int>, title: String, suffix: String, runs: [WrkRun]) {
        let sorted = runs.sorted {
            $0.results.map { $0[keyPath: stat] }.average > $1.results.map { $0[keyPath: stat] }.average
        }
        output("\(title):".consoleText(.success))
        for (i, run) in sorted.enumerated() {
            let avg = run.results.map { $0[keyPath: stat] }.average
            print("#\(i + 1) \(run.framework.description): \(avg)\(suffix)")
        }
    }
}

struct WrkRun {
    var framework: Framework
    var results: [WrkResult]

    var average: WrkResult {
        let latency = WrkResult.Latency(
            min: results.map { $0.latency.min }.average,
            max: results.map { $0.latency.max }.average,
            mean: results.map { $0.latency.mean }.average,
            stdev: results.map { $0.latency.stdev }.average
        )
        let errors = WrkResult.Errors(
            connect: results.map { $0.errors.connect }.average,
            read: results.map { $0.errors.read }.average,
            write: results.map { $0.errors.write }.average,
            status: results.map { $0.errors.status }.average,
            timeout: results.map { $0.errors.timeout }.average
        )
        return WrkResult(
            latency: latency,
            requests: results.map { $0.requests }.average,
            bytes: results.map { $0.bytes }.average,
            duration: results.map { $0.duration }.average,
            errors: errors
        )
    }
}

extension Array where Element == Int {
    /// Returns the average of all elements in the array
    var average: Int {
        switch count {
        case 0: return 0
        case 1: return self[0]
        default:
            var total = 0
            for el in self {
                total += el
            }
            return total / count
        }
    }
}


struct WrkResult: Codable {
    struct Latency: Codable {
        var min: Int
        var max: Int
        var mean: Int
        var stdev: Int
    }

    struct Errors: Codable {
        var connect: Int
        var read: Int
        var write: Int
        var status: Int
        var timeout: Int
    }

    var latency: Latency
    var requests: Int
    var bytes: Int
    var duration: Int
    var errors: Errors
}

import Vapor

final class WrkBenchmark: Command {
    let arguments: [CommandArgument] = [.argument(name: "hostname")]
    let options: [CommandOption] = [
        .value(name: "runs", short: "r"),
        .value(name: "threads", short: "t"),
        .value(name: "connections", short: "c"),
        .value(name: "duration", short: "d"),
    ]
    let help: [String] = []

    func run(using context: CommandContext) throws -> EventLoopFuture<Void> {
        var runs: [WrkRun] = []
        let hostname = try context.argument("hostname")
        let runCount = context.options["runs"].flatMap(Int.init) ?? 1
        let duration = context.options["duration"] ?? "5"
        let threads = context.options["threads"] ?? "4"
        let connections = context.options["connections"] ?? "128"

        let frameworks = try Framework.readAll()
        let client = try context.container.make(Client.self)

        context.console.output("testing...".consoleText(.success))
        return frameworks.enumerated().map { (i, framework) -> () -> Future<Void> in
            return {
                return client.get("http://\(hostname):\(framework.port)/").map { res -> Response in
                    context.console.print("[\(i+1)/\(frameworks.count)] \(framework)")
                    return res
                }.map { res -> Void in
                    if res.http.status != .ok {
                        context.console.print("⚠️  invalid response status")
                    }
                    if res.http.headers.firstValue(name: .date) == nil {
                        context.console.print("⚠️  missing date header")
                    }
                    if res.http.body.count != 0 {
                        context.console.print("⚠️  body size should be zero")
                    }
                }.mapIfError { error in
                    context.console.print("❌  could not connect: \(error)")
                }
            }
        }.syncFlatten(on: context.container).map {
            context.console.print("\(runCount) runs: wrk -t \(threads) -c \(connections) -d \(duration)")
            context.console.output("benchmarking...".consoleText(.success))
            for (i, framework) in frameworks.enumerated() {
                var results: [WrkResult] = []

                context.console.print("[\(i+1)/\(frameworks.count)] \(framework) ", newLine: false)
                for _ in 1...runCount {
                    do {
                        let result = try self.wrk(hostname: hostname, port: framework.port, threads: threads, connections: connections, duration: duration)
                        results.append(result)
                        context.console.print(".", newLine: false)
                    } catch {
                        context.console.print("x", newLine: false)
                    }
                }
                context.console.print("")

                let run = WrkRun(framework: framework, results: results)
                runs.append(run)
            }

            context.console.report(\.requests, title: "total requests", suffix: " reqs", op: >, runs: runs)
            context.console.report(\.latency.mean, title: "average latency", suffix: "µs", op: <, runs: runs)
        }
    }

    func wrk(hostname: String, port: Int, threads: String, connections: String, duration: String) throws -> WrkResult {
        let res = try Process.execute("wrk", "-s", "wrk-report.lua", "-t", threads, "-c", connections, "-d", duration, "http://\(hostname):\(port)/ping")
        let parts = res.split(separator: "$", maxSplits: 1).map(String.init)
        guard parts.count == 2 else {
            throw "no $ found in wrk output"
        }
        return try JSONDecoder().decode(WrkResult.self, from: Data(parts[1].utf8))
    }
}

// MARK: Report

extension Console {
    func report(_ stat: KeyPath<WrkResult, Int>, title: String, suffix: String, op: (Int, Int) -> Bool, runs: [WrkRun]) {
        let sorted = runs.sorted {
            let l = $0.results.map { $0[keyPath: stat] }.average
            let r = $1.results.map { $0[keyPath: stat] }.average
            return op(l, r)
        }
        output("\(title):".consoleText(.success))
        for (i, run) in sorted.enumerated() {
            let avg = run.results.map { $0[keyPath: stat] }.average
            print("#\(i + 1) \(run.framework.description): \(avg)\(suffix)")
        }
    }
}


// MARK: Run

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

// MARK: Result

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

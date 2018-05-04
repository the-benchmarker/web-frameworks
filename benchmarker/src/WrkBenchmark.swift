import Vapor

final class WrkBenchmark: Command {
    let arguments: [CommandArgument] = [.argument(name: "hostname")]
    let options: [CommandOption] = [.value(name: "runs", short: "r")]
    let help: [String] = []

    func run(using context: CommandContext) throws -> EventLoopFuture<Void> {
        var runs: [WrkRun] = []
        let hostname = try context.argument("hostname")
        let runCount = context.options["runs"].flatMap(Int.init) ?? 1

        let frameworks = try Framework.readAll()

        let client = try context.container.make(Client.self)

        context.console.output("testing...".consoleText(.success))
        return frameworks.enumerated().map { (i, framework) -> Future<Void> in
            context.console.print("[\(i+1)/\(frameworks.count)] \(framework) ", newLine: false)
            return client.get("http://\(hostname):\(framework.port)/").map { res -> Void in
                context.console.output(res.description.consoleText())
                if res.http.contentType != .plainText {
                    context.console.output("⚠️ wrong content type")
                }
                if res.http.headers.firstValue(name: .date) == nil {
                    context.console.output("⚠️ missing date header")
                }
                if res.http.body.count != 4 {
                    context.console.output("⚠️ wrong body size")
                }
            }
        }.flatten(on: context.container).map {
            context.console.output("benchmarking...".consoleText(.success))
            for (i, framework) in frameworks.enumerated() {
                var results: [WrkResult] = []

                context.console.print("[\(i+1)/\(frameworks.count)] \(framework) ", newLine: false)
                for _ in 1...runCount {
                    do {
                        let result = try self.wrk(hostname: hostname, port: framework.port)
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

            context.console.report(\.requests, title: "total requests", suffix: " reqs", runs: runs)
            context.console.report(\.latency.mean, title: "average latency", suffix: "µs", runs: runs)
        }
    }

    func wrk(hostname: String, port: Int) throws -> WrkResult {
        let res = try Process.execute("wrk", "-s", "wrk-report.lua", "-t", "4", "-c", "128", "-d", "1", "http://\(hostname):\(port)/ping")
        let parts = res.split(separator: "$", maxSplits: 1).map(String.init)
        guard parts.count == 2 else {
            throw "no $ found in wrk output"
        }
        return try JSONDecoder().decode(WrkResult.self, from: Data(parts[1].utf8))
    }
}

// MARK: Report

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

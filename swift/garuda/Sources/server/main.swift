import Garuda

#if canImport(Glibc)
import Glibc
#else
import Darwin
#endif

// The three routes the suite asks for: an empty 200 at the root, the id echoed
// back from /user/:id, and an empty 200 for POST /user.
//
// `withParameter` lends the id's bytes rather than copying them into a String,
// which is the ordinary way to write this in Garuda and not a special case for
// a benchmark.
let app = Application()

app.get("/") { _, response in
    response.send(status: 200)
}

app.get("/user/:id") { request, response in
    request.withParameter(0) { response.send($0) }
}

app.post("/user") { _, response in
    response.send(status: 200)
}

// The image sets SERVER_HOSTNAME and SERVER_PORT and starts the binary with no
// arguments, so they are read here and handed to the same parser the `garuda`
// command line uses. The array is a full argv: Garuda 2 skips its first
// element as the program name.
//
// No --workers: Garuda 2 sizes its shards from the CPUs the process may run
// on (the cpuset SERVER_CPUS gives the container), one shard per last-level
// cache domain. A count from sysconf would be the host's, not the cpuset's.
func environment(_ name: String, _ fallback: String) -> String {
    guard let value = getenv(name), let string = String(validatingCString: value),
          !string.isEmpty else { return fallback }
    return string
}

app.run(arguments: [
    CommandLine.arguments[0],
    "--host", environment("SERVER_HOSTNAME", "0.0.0.0"),
    "--port", environment("SERVER_PORT", "3000"),
    "--log-level", "error",
])

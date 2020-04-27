// swift-tools-version:5.0

import PackageDescription

let package = Package(
    name: "server",
    dependencies: [
        .package(url: "https://github.com/PerfectlySoft/Perfect-HTTPServer.git", .upToNextMinor(from: "3.0.23"))
    ],
    targets: [
        .target(name: "server", dependencies: ["PerfectHTTPServer"])
    ]
)

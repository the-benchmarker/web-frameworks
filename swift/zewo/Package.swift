// swift-tools-version:4.0
import PackageDescription

let package = Package(
    name: "server",
    dependencies: [
        .package(url: "https://github.com/Zewo/HTTPServer.git", .upToNextMinor(from: "0.14.0"))
    ],
    targets: [
        .target(name: "server", dependencies: ["HTTPServer"], path: ".")
    ]
)

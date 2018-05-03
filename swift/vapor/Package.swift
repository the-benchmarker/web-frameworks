// swift-tools-version:4.0
import PackageDescription

let package = Package(
    name: "Benchmark",
    dependencies: [
        .package(url: "https://github.com/vapor/vapor.git", .branch("gm")),
    ],
    targets: [
        .target(name: "main", dependencies: ["Vapor"], path: "."),
    ]
)

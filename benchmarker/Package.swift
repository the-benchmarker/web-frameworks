// swift-tools-version:4.0
import PackageDescription

let package = Package(
    name: "which_is_the_fastest",
    dependencies: [
        .package(url: "https://github.com/vapor/vapor.git", .branch("gm")),
    ],
    targets: [
        .target(name: "benchmarker", dependencies: ["Vapor"], path: "src"),
    ]
)

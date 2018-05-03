// swift-tools-version:4.0
import PackageDescription

let package = Package(
    name: "which_is_the_fastest",
    dependencies: [
        .package(url: "https://github.com/vapor/vapor.git", .branch("gm")),
        .package(url: "https://github.com/vapor/fluent-sqlite.git", from: "3.0.0-rc"),
    ],
    targets: [
        .target(name: "benchmarker", dependencies: ["FluentSQLite", "Vapor"], path: "."),
    ]
)

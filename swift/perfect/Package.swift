// swift-tools-version:4.0
import PackageDescription

let package = Package(
    name: "server",
    dependencies: [
        .package(url: "https://github.com/PerfectlySoft/Perfect", "3.1.0"..<"3.2.0")
    ],
    targets: [
        .target(name: "server", dependencies: ["Perfect"], path: ".")
    ]
)

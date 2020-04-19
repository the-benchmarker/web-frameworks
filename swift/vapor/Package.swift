// swift-tools-version:5.2

import PackageDescription

let package = Package(
    name: "server",
    dependencies: [
        .package(url: "https://github.com/vapor/vapor.git", .upToNextMinor(from: "4.3.0"))
    ],
    targets: [
        .target(name: "App", dependencies: [
            .product(name: "Vapor", package: "vapor")
        ]),
        .target(name: "Run", dependencies: ["App"]),
    ]
)

// swift-tools-version:6.0
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "server",
    platforms: [.macOS(.v14)], // This is for development on macOS
    dependencies: [
        .package(url: "https://github.com/hummingbird-project/hummingbird.git", from: "2.16.0"),
    ],
    targets: [
        .executableTarget(
            name: "server",
            dependencies: [
                .product(name: "Hummingbird", package: "hummingbird"),
            ],
        ),
    ]
)

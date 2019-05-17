// swift-tools-version:4.0

import PackageDescription

let package = Package(
    name: "server",
    dependencies: [
        .package(url: "https://github.com/IBM-Swift/Kitura", .upToNextMinor(from: "2.7.0"))
    ],
    targets: [
       .target(name: "server", dependencies: ["Kitura"], path: ".")
    ]
)

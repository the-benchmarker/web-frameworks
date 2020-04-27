// swift-tools-version:5.1

import PackageDescription

let package = Package(
    name: "server",
    dependencies: [
        .package(url: "https://github.com/IBM-Swift/Kitura", .upToNextMinor(from: "2.9.1"))
    ],
    targets: [
       .target(name: "server", dependencies: ["Kitura"])
    ]
)

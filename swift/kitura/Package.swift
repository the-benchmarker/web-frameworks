// swift-tools-version:3.1

import PackageDescription

let package = Package(
    name: "server_swift_kitura",
    dependencies: [
        .Package(url: "https://github.com/IBM-Swift/Kitura.git", majorVersion: 1, minor: 7)
    ]
)

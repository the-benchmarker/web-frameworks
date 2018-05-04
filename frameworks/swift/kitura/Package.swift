// swift-tools-version:3.1

import PackageDescription

let package = Package(
    name: "server",
    dependencies: [
        .Package(url: "https://github.com/IBM-Swift/Kitura.git", majorVersion: 2, minor: 2)
    ]
)

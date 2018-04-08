// swift-tools-version:3.1

import PackageDescription

let package = Package(
    name: "server_swift_vapor",
    dependencies: [
        .Package(url: "https://github.com/vapor/vapor.git",  majorVersion: 2, minor: 4)
    ],
    exclude: [
        "Config",
        "Database",
        "Localization",
        "Public",
        "Resources",
    ]
)


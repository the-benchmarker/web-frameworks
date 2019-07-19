// swift-tools-version:5.0

import PackageDescription

let package = Package(
    name: "MyServer",
    dependencies: [
        .package(url: "https://github.com/httpswift/swifter.git", .upToNextMajor(from: "1.4.7"))
    ],
    targets: [
        .target(name: "server", dependencies: ["Swifter"], path: ".")
    ]
)

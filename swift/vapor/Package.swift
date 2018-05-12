// swift-tools-version:4.0
import PackageDescription

let package = Package(
    name: "which_is_the_fastest",
    dependencies: [
        .package(url: "https://github.com/vapor/vapor.git", from: "3.0.0"),
    ],
    targets: [
        .target(name: "server", dependencies: ["Vapor"], path: "."),
    ]
)

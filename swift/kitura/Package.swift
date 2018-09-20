// swift-tools-version:4.0
import PackageDescription

let package = Package(
    name: "server",
    dependencies: [
        .package(url: "https://github.com/IBM-Swift/Kitura.git", from: "2.5.2"),
    ],
    targets: [
        .target(name: "server", dependencies: ["Kitura"], path: "."),
    ]
)

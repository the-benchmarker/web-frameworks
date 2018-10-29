// swift-tools-version:4.0
import PackageDescription

let package = Package(
    name: "server",
    dependencies: [
        .package(url: "https://github.com/IBM-Swift/Kitura.git", "2.5.0"..<"2.6.0")
    ],
    targets: [
       .target(name: "server", dependencies: ["Kitura"], path: ".")
    ]
)

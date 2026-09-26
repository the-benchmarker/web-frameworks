// swift-tools-version:6.2
// 6.2 is Garuda's floor: a borrowed request body is a `Span`.

import PackageDescription

let package = Package(
    name: "server",
    platforms: [.macOS(.v15)],
    dependencies: [
        .package(url: "https://github.com/grepjava/garuda.git", from: "1.0.1"),
    ],
    targets: [
        .executableTarget(
            name: "server",
            dependencies: [
                .product(name: "Garuda", package: "garuda"),
            ]
        ),
    ]
)

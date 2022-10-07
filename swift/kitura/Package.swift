// swift-tools-version:5.1

import PackageDescription

let package = Package(
    name: "server",
    dependencies: [
        .package(url: "https://github.com/Kitura/Kitura", .upToNextMinor(from: "3.0.1"))
    ],
    targets: [
       .target(
           name: "server", 
           dependencies: ["Kitura"],
           swiftSettings: [
                // Enable better optimizations when building in Release configuration. Despite the use of
                // the `.unsafeFlags` construct required by SwiftPM, this flag is recommended for Release
                // builds. See <https://github.com/swift-server/guides#building-for-production> for details.
                .unsafeFlags(["-cross-module-optimization"], .when(configuration: .release))
            ]
           )
    ]
)

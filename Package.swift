// swift-tools-version: 6.0
import PackageDescription

let package = Package(
  name: "LearnCompilers",
  platforms: [
    .macOS(.v13),
    .iOS(.v16),
    .tvOS(.v16),
    .watchOS(.v9),
  ],
  products: [
    .library(
      name: "LearnCompilers",
      targets: ["LearnCompilers"]),
    .executable(
      name: "Compile",
      targets: ["Compile"]),
    .executable(
      name: "Interpret",
      targets: ["Interpret"]),
  ],
  dependencies: [
    // Add the local dependency
    .package(path: "../learn-parsers")
  ],
  targets: [
    .target(
      name: "LearnCompilers",
      dependencies: [
        // Make LearnCompilers depend on LearnParsers
        .product(name: "LearnParsers", package: "learn-parsers")
      ]
    ),
    .executableTarget(
      name: "Compile",
      dependencies: ["LearnCompilers"]
    ),
    .executableTarget(
      name: "Interpret",
      dependencies: ["LearnCompilers"]
    ),
    .testTarget(
      name: "LearnCompilersTests",
      dependencies: ["LearnCompilers"]
    ),
  ]
)

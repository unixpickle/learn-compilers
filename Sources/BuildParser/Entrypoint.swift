/// Export the parser as a file.

import Foundation
import LearnCompilers
import LearnParsers

@main
struct CompileApp {
  static func main() {
    let args = CommandLine.arguments

    guard args.count == 2 else {
      print("Usage: BuildParser <output-file>")
      exit(1)
    }

    let outputPath = args[1]

    do {
      let parser = try LR1Parser(grammar: SimpleGrammar())
      let encoder = PropertyListEncoder()
      encoder.outputFormat = .binary
      let data = try encoder.encode(parser)
      try data.write(to: URL(fileURLWithPath: outputPath))
    } catch {
      print("Error building parser:")
      print(error)
      exit(1)
    }
  }
}

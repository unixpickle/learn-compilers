import Foundation
import LearnCompilers

@main
struct CompileApp {
  static func main() {
    let args = CommandLine.arguments

    guard args.count == 3 else {
      print("Usage: Compile <input-file> <output-file>")
      exit(1)
    }

    let inputPath = args[1]
    let outputPath = args[2]

    do {
      let inputData = try String(contentsOfFile: inputPath, encoding: .utf8)

      let match: ASTMatch
      do {
        match = try Parser.parse(inputData)
      } catch {
        print("Compilation failed with error:")
        print(formatParseError(filename: inputPath, error))
        exit(1)
      }
      var ast = AST(match: match)

      var table = ScopeTable()
      table.addStandardLibrary()
      var errors: [ASTDecorationError]
      (ast, errors) = ast.decorated(table: &table, fileID: inputPath)

      if !errors.isEmpty {
        print("Compilation failed with error(s):")
        for error in errors {
          print("\(error)")
        }
        exit(1)
      }

      var cfg = CFG(ast: ast)
      cfg.add(ast: StandardLibrary.ast, omitUnused: true)
      cfg.insertPhiAndNumberVars()
      cfg.performBasicOptimizations(fnReduction: BuiltInFunction.reduce)
      cfg.inlineSingleCalls()
      cfg.performBasicOptimizations(fnReduction: BuiltInFunction.reduce)
      try cfg.checkMissingReturns()

      let outputString =
        if outputPath.hasSuffix(".cfg.txt") {
          cfg.description
        } else if outputPath.hasSuffix(".ll") {
          try BackendLLVM().compileAssembly(cfg: cfg)
        } else {
          try BackendAArch64().compileAssembly(cfg: cfg)
        }
      try outputString.write(toFile: outputPath, atomically: true, encoding: .utf8)
    } catch {
      print("Compilation failed with error:")
      print(error)
      exit(1)
    }
  }
}

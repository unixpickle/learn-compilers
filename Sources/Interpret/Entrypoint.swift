import Foundation
import LearnCompilers

@main
struct CompileApp {
  static func main() {
    let args = CommandLine.arguments

    guard args.count == 2 else {
      print("Usage: Interpret <input-file>")
      exit(1)
    }

    let inputPath = args[1]

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

      guard
        let mainFunction = cfg.functions.keys.compactMap({ key in
          key.name == "main" && key.signature.args.isEmpty ? key : nil
        })
        .first
      else {
        print("Error: no main function with no arguments is defined")
        exit(1)
      }
      let interp = try Interpreter(cfg: cfg, entrypoint: mainFunction, arguments: [])
      if case .integer(let x) = interp.run() {
        exit(Int32(x))
      }
    } catch {
      print("Compilation failed with error:")
      print(error)
      exit(1)
    }
  }
}

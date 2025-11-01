import LearnParsers
import Testing

@testable import LearnCompilers

@Test func testInterpreterFibonacci() throws {
  for code in FibonacciImplementations {
    let cfg = try codeToCFG(code)
    try checkFibonacciImplementation(cfg: cfg)
  }
}

private func codeToCFG(_ code: String) throws -> CFG {
  let match = try Parser.parse(code)
  var ast = AST(match: match)
  #expect(ast.codeString == code)

  var table = ScopeTable()
  table.addBuiltIns()
  var errors: [ASTDecorationError]
  (ast, errors) = ast.decorated(table: &table, fileID: "stdin")

  #expect(errors.isEmpty)
  if !errors.isEmpty {
    throw errors.first!
  }

  var cfg = CFG(ast: ast)
  try cfg.insertPhiAndNumberVars()
  return cfg
}

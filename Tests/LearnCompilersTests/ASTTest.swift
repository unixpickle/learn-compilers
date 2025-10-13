import LearnParsers
import Testing

@testable import LearnCompilers

@Test func testASTRoundTrip() async throws {
  let parser = try LR1Parser(grammar: SimpleGrammar())

  func parseCode(_ code: String) async throws -> ASTMatch {
    var p = parser
    return try await p.read(StringParserReader(code))
  }

  var code = "fn main() -> int {\n  x: int = 3\n  y: int = add(x, 4)\n  print(y)\n}"
  var match = try await parseCode(code)
  var ast = astNodeFor(match: match)
  #expect(ast.codeString == code)

  code = "fn main() -> int {\n  x: int = 3\n  ?if (eq(x, 3)) {\n    print(3)\n  }\n  print(0)\n}"
  match = try await parseCode(code)
  ast = astNodeFor(match: match)
  #expect(ast.codeString == code)

  code = "fn main(z: int) -> int {\n  print(0)\n}"
  match = try await parseCode(code)
  ast = astNodeFor(match: match)
  #expect(ast.codeString == code)

  code = "fn main(z: int) {\n  print(0)\n}"
  match = try await parseCode(code)
  ast = astNodeFor(match: match)
  #expect(ast.codeString == code)

  code = "fn main (z :int ){\n  print(0)\n}"
  match = try await parseCode(code)
  ast = astNodeFor(match: match)
  #expect(ast.codeString == code)

  code = "fn main (z :int , x : str ){\n  print(0)\n}"
  match = try await parseCode(code)
  ast = astNodeFor(match: match)
  #expect(ast.codeString == code)

  code = "\nfn main (z: int, x: str )->str{\n  print(0)\n}\n\nfn foo(x: int) -> str {}"
  match = try await parseCode(code)
  ast = astNodeFor(match: match)
  #expect(ast.codeString == code)
}

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
  var ast = AST(match: match)
  #expect(ast.codeString == code)
  ensureSetContentsDoesntChange(node: ast)

  code = "fn main() -> int {\n  x: int = 3\n  if? (eq(x, 3)) {\n    print(3)\n  }\n  print(0)\n}"
  match = try await parseCode(code)
  ast = AST(match: match)
  #expect(ast.codeString == code)
  ensureSetContentsDoesntChange(node: ast)

  code = "fn main(z: int) -> int {\n  print(0)\n}"
  match = try await parseCode(code)
  ast = AST(match: match)
  #expect(ast.codeString == code)
  ensureSetContentsDoesntChange(node: ast)

  code = "fn main(z: int) {\n  print(0)\n}"
  match = try await parseCode(code)
  ast = AST(match: match)
  #expect(ast.codeString == code)
  ensureSetContentsDoesntChange(node: ast)

  code = "fn main (z :int ){\n  print(0)\n}"
  match = try await parseCode(code)
  ast = AST(match: match)
  #expect(ast.codeString == code)
  ensureSetContentsDoesntChange(node: ast)

  code = "fn main (z :int , x : str ){\n  print(0)\n}"
  match = try await parseCode(code)
  ast = AST(match: match)
  #expect(ast.codeString == code)
  ensureSetContentsDoesntChange(node: ast)

  code = "\nfn main (z: int, x: str )->str{\n  print(0)\n}\n\nfn foo(x: int) -> str {}"
  match = try await parseCode(code)
  ast = AST(match: match)
  #expect(ast.codeString == code)
  ensureSetContentsDoesntChange(node: ast)

  code =
    "fn main(x: int) -> int {\n  x: int = 3\n  while? (lt(x, 5)) {\n    print(x)\n  x = add(x, 1)\n  break!()\n  }\n  print(x)\n}"
  match = try await parseCode(code)
  ast = AST(match: match)
  #expect(ast.codeString == code)
  ensureSetContentsDoesntChange(node: ast)

  code = "fn main(x: int) -> int {\n  x: int = 3\n  return!(add(x, 3))\n}"
  match = try await parseCode(code)
  ast = AST(match: match)
  #expect(ast.codeString == code)
  ensureSetContentsDoesntChange(node: ast)

  code = "fn main(x: int) {\n  x: int = 3\n  return!()\n}"
  match = try await parseCode(code)
  ast = AST(match: match)
  #expect(ast.codeString == code)
  ensureSetContentsDoesntChange(node: ast)
}

@discardableResult
func ensureSetContentsDoesntChange(node: ASTNode) -> ASTNode {
  var newNode = node
  switch newNode.contents {
  case .code(let code):
    newNode.contents = .code(code)
  case .children(let children):
    newNode.contents = .children(children.map { ensureSetContentsDoesntChange(node: $0) })
  }
  #expect(newNode.codeString == node.codeString)
  return newNode
}

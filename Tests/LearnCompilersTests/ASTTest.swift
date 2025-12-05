import LearnParsers
import Testing

@testable import LearnCompilers

@Test func testASTRoundTrip() throws {
  var code =
    "fn main() -> int {\n  // declare\n  x: int = 3 //here we go!   \n  y: int = add(x, 4)\n  print(y)\n}"
  var match = try Parser.parse(code)
  var ast = AST(match: match)
  #expect(ast.codeString == code)
  ensureSetContentsDoesntChange(node: ast)

  code = "fn main() -> int {\n  x: int = 3\n  if? (eq(x, 3)) {\n    print(3)\n  }\n  print(0)\n}"
  match = try Parser.parse(code)
  ast = AST(match: match)
  #expect(ast.codeString == code)
  ensureSetContentsDoesntChange(node: ast)

  code = "fn main(z: int) -> int {\n  print(0)\n}"
  match = try Parser.parse(code)
  ast = AST(match: match)
  #expect(ast.codeString == code)
  ensureSetContentsDoesntChange(node: ast)

  code = "fn main(z: int) {\n  print(0)\n}"
  match = try Parser.parse(code)
  ast = AST(match: match)
  #expect(ast.codeString == code)
  ensureSetContentsDoesntChange(node: ast)

  code = "fn main (z :int ){\n  print(0)\n}"
  match = try Parser.parse(code)
  ast = AST(match: match)
  #expect(ast.codeString == code)
  ensureSetContentsDoesntChange(node: ast)

  code = "fn main (z :int , x : str ){\n  print(0)\n}"
  match = try Parser.parse(code)
  ast = AST(match: match)
  #expect(ast.codeString == code)
  ensureSetContentsDoesntChange(node: ast)

  code = "\nfn main (z: int, x: str )->str{\n  print(0)\n}\n\nfn foo(x: int) -> str {}"
  match = try Parser.parse(code)
  ast = AST(match: match)
  #expect(ast.codeString == code)
  ensureSetContentsDoesntChange(node: ast)

  code =
    "fn main(x: int) -> int {\n  x: int = 3\n  while? (lt(x, 5)) {\n    print(x)\n  x = add(x, 1)\n  break!()\n  }\n  print(x)\n}"
  match = try Parser.parse(code)
  ast = AST(match: match)
  #expect(ast.codeString == code)
  ensureSetContentsDoesntChange(node: ast)

  code = """
    fn main(x: int) -> int {
      x: int = -1
      while? (lt(x, 5)) {
        x = add(x, 1)
        if? (lt(x, 3)) {
          continue!()
        }
        print(x)
        break!()
      }
      print(x)
    }
    """
  match = try Parser.parse(code)
  ast = AST(match: match)
  #expect(ast.codeString == code)
  ensureSetContentsDoesntChange(node: ast)

  code = "fn main(x: int) -> int {\n  x: int = 3\n  return!(add(x, 3))\n}"
  match = try Parser.parse(code)
  ast = AST(match: match)
  #expect(ast.codeString == code)
  ensureSetContentsDoesntChange(node: ast)

  code = "fn main(x: int) {\n  x: int = 3\n  return!()\n}"
  match = try Parser.parse(code)
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
